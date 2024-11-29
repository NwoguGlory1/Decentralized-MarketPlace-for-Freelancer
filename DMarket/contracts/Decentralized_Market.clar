;; Freelancer Marketplace Contract
;; Allows clients to post jobs, freelancers to bid, and handles escrow payments

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-unauthorized (err u105))

;; Job Status: 1-Open, 2-In Progress, 3-Completed, 4-Cancelled
(define-data-var next-job-id uint u1)

;; Job Details Map
(define-map jobs
    uint
    {
        client: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        budget: uint,
        freelancer: (optional principal),
        status: uint,
        deadline: uint,
        created-at: uint
    }
)

;; Bids mapping: job-id -> freelancer -> bid amount
(define-map bids
    {job-id: uint, freelancer: principal}
    {
        amount: uint,
        proposal: (string-ascii 500)
    }
)

;; Escrow balances
(define-map escrow-balance
    uint
    uint
)

;; Read-only functions

(define-read-only (get-job (job-id uint))
    (map-get? jobs job-id)
)

(define-read-only (get-bid (job-id uint) (freelancer principal))
    (map-get? bids {job-id: job-id, freelancer: freelancer})
)

(define-read-only (get-escrow-balance (job-id uint))
    (default-to u0 (map-get? escrow-balance job-id))
)

;; Public functions

;; Post a new job
(define-public (post-job (title (string-ascii 100)) (description (string-ascii 500)) (budget uint) (deadline uint))
    (let
        (
            (job-id (var-get next-job-id))
        )
        (asserts! (> budget u0) err-invalid-status)
        (asserts! (> deadline block-height) err-invalid-status)
        
        (map-set jobs job-id {
            client: tx-sender,
            title: title,
            description: description,
            budget: budget,
            freelancer: none,
            status: u1,
            deadline: deadline,
            created-at: block-height
        })
        
        (var-set next-job-id (+ job-id u1))
        (ok job-id)
    )
)

;; Submit a bid for a job
(define-public (submit-bid (job-id uint) (amount uint) (proposal (string-ascii 500)))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
        )
        (asserts! (is-eq (get status job) u1) err-invalid-status)
        (asserts! (<= amount (get budget job)) err-invalid-status)
        (asserts! (not (is-eq tx-sender (get client job))) err-unauthorized)
        
        (map-set bids {job-id: job-id, freelancer: tx-sender} {
            amount: amount,
            proposal: proposal
        })
        (ok true)
    )
)

;; Accept a bid and fund escrow
(define-public (accept-bid (job-id uint) (freelancer principal))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
            (bid (unwrap! (map-get? bids {job-id: job-id, freelancer: freelancer}) err-not-found))
        )
        (asserts! (is-eq tx-sender (get client job)) err-unauthorized)
        (asserts! (is-eq (get status job) u1) err-invalid-status)
        
        ;; Transfer funds to escrow
        (try! (stx-transfer? (get amount bid) tx-sender (as-contract tx-sender)))
        
        ;; Update job status and freelancer
        (map-set jobs job-id (merge job {
            status: u2,
            freelancer: (some freelancer)
        }))
        
        ;; Set escrow balance
        (map-set escrow-balance job-id (get amount bid))
        (ok true)
    )
)

;; Mark job as complete (by client)
(define-public (complete-job (job-id uint))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
            (escrow-amount (unwrap! (map-get? escrow-balance job-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get client job)) err-unauthorized)
        (asserts! (is-eq (get status job) u2) err-invalid-status)
        
        ;; Transfer funds to freelancer
        (try! (as-contract (stx-transfer? escrow-amount tx-sender (unwrap! (get freelancer job) err-not-found))))
        
        ;; Update job status
        (map-set jobs job-id (merge job {status: u3}))
        
        ;; Clear escrow
        (map-delete escrow-balance job-id)
        (ok true)
    )
)

;; Cancel job (only if not started)
(define-public (cancel-job (job-id uint))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get client job)) err-unauthorized)
        (asserts! (is-eq (get status job) u1) err-invalid-status)
        
        (map-set jobs job-id (merge job {status: u4}))
        (ok true)
    )
)


;; Initialize contract
(define-public (initialize)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok true)
    )
)

;; Dispute Resolution Mechanism
(define-map disputes
    uint
    {
        job-id: uint,
        disputant: principal,
        reason: (string-ascii 500),
        status: uint,  ;; 1-Pending, 2-Resolved, 3-Closed
        arbitrator: (optional principal)
    }
)


;; Open a dispute for a job
(define-public (open-dispute (job-id uint) (reason (string-ascii 500)))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
            (dispute-id (var-get next-dispute-id))
        )
        (asserts! (or 
            (is-eq tx-sender (get client job))
            (is-eq tx-sender (unwrap! (get freelancer job) err-not-found))
        ) err-unauthorized)
        
        (map-set disputes dispute-id {
            job-id: job-id,
            disputant: tx-sender,
            reason: reason,
            status: u1,
            arbitrator: none
        })
        
        (var-set next-dispute-id (+ dispute-id u1))
        (ok dispute-id)
    )
)
