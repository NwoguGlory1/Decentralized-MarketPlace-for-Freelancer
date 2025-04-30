;; Freelancer Marketplace Contract - Updated
;; Allows clients to post jobs, freelancers to bid, and handles escrow payments

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-past-deadline (err u107))
(define-constant err-invalid-rating (err u108))
(define-constant err-already-rated (err u109))
(define-constant err-invalid-team (err u110))
(define-constant err-invalid-percentage (err u111))
(define-constant err-verification-required (err u112))
(define-constant err-invalid-invitation (err u113))
(define-constant minimum-bid-time u720)

;; Job Status: 1-Open, 2-In Progress, 3-Completed, 4-Cancelled
(define-data-var next-job-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var next-team-id uint u1)
(define-data-var next-invitation-id uint u1)
(define-data-var next-verification-id uint u1)

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
        created-at: uint,
        is-featured: bool
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

;; Subscription system for featured job listings
(define-map subscriptions
    principal
    {
        level: uint,  ;; 1-Basic, 2-Premium, 3-Enterprise
        expiry: uint,
        featured-jobs-remaining: uint
    }
)

;; Direct Job Invitations
(define-map job-invitations
    uint  ;; invitation-id
    {
        job-id: uint,
        client: principal,
        freelancer: principal,
        message: (string-ascii 200),
        status: uint  ;; 1-Pending, 2-Accepted, 3-Declined
    }
)

;; Skill Verification System
(define-map verified-skills
    uint  ;; verification-id
    {
        freelancer: principal,
        skill: (string-ascii 50),
        verifier: principal,
        verified-at: uint,
        level: uint  ;; 1-Beginner, 2-Intermediate, 3-Expert, 4-Master
    }
)

;; Freelancer Teams/Collaboration
(define-map teams
    uint  ;; team-id
    {
        name: (string-ascii 50),
        leader: principal,
        members: (list 10 principal),
        created-at: uint
    }
)

;; Team Assignments for Jobs
(define-map team-assignments
    uint  ;; job-id
    {
        team-id: uint,
        payment-splits: (list 10 {member: principal, percentage: uint})
    }
)

;; Referral System
(define-map referrals
    {referrer: principal, referee: principal}
    {
        created-at: uint,
        status: uint,  ;; 1-Pending, 2-Completed
        reward: uint
    }
)

;; Smart Deadlines
(define-map smart-deadlines
    uint  ;; job-id
    {
        original-deadline: uint,
        extensions: (list 5 {reason: (string-ascii 100), length: uint}),
        current-deadline: uint
    }
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

(define-read-only (get-subscription (user principal))
    (map-get? subscriptions user)
)

(define-read-only (get-invitation (invitation-id uint))
    (map-get? job-invitations invitation-id)
)

(define-read-only (get-verified-skill (verification-id uint))
    (map-get? verified-skills verification-id)
)

(define-read-only (get-team (team-id uint))
    (map-get? teams team-id)
)

(define-read-only (get-team-assignment (job-id uint))
    (map-get? team-assignments job-id)
)

(define-read-only (get-referral (referrer principal) (referee principal))
    (map-get? referrals {referrer: referrer, referee: referee})
)

(define-read-only (get-smart-deadline (job-id uint))
    (map-get? smart-deadlines job-id)
)

;; Public functions

;; Post a new job
(define-public (post-job (title (string-ascii 100)) (description (string-ascii 500)) (budget uint) (deadline uint))
    (let
        (
            (job-id (var-get next-job-id))
        )
        (asserts! (> budget u0) err-invalid-amount)
        (asserts! (> deadline block-height) err-past-deadline)
        
        (map-set jobs job-id {
            client: tx-sender,
            title: title,
            description: description,
            budget: budget,
            freelancer: none,
            status: u1,
            deadline: deadline,
            created-at: block-height,
            is-featured: false
        })
        
        ;; Initialize smart deadline
        (map-set smart-deadlines job-id {
            original-deadline: deadline,
            extensions: (list),
            current-deadline: deadline
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
        (asserts! (<= amount (get budget job)) err-invalid-amount)
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
        
        ;; Check if job was assigned to a team
        (match (map-get? team-assignments job-id)
            team-assignment (try!
                (release-payment-to-team job-id escrow-amount team-assignment))
            ;; If not a team assignment, pay the individual freelancer
            (try! (as-contract (stx-transfer? 
                escrow-amount 
                tx-sender 
                (unwrap! (get freelancer job) err-not-found)
            )))
        )
        
        ;; Update job status
        (map-set jobs job-id (merge job {status: u3}))
        
        ;; Clear escrow
        (map-delete escrow-balance job-id)
        (ok true)
    )
)

;; Private helper to release payment to team members
(define-private (release-payment-to-team (job-id uint) (amount uint) (assignment {team-id: uint, payment-splits: (list 10 {member: principal, percentage: uint})}))
    (fold release-payment-to-member (get payment-splits assignment) (ok amount))
)

;; Helper to pay each team member
(define-private (release-payment-to-member 
    (split {member: principal, percentage: uint}) 
    (result (response uint uint))
)
    (match result
        amount (let
            (
                (member-amount (/ (* amount (get percentage split)) u100))
            )
            (if (> member-amount u0)
                (begin
                    (try! (as-contract (stx-transfer? member-amount tx-sender (get member split))))
                    (ok (- amount member-amount))
                )
                (ok amount)
            )
        )
        error (err error)
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

;; Resolve dispute by contract owner or designated arbitrator
(define-public (resolve-dispute (dispute-id uint) (resolution-amount uint))
    (let
        (
            (dispute (unwrap! (map-get? disputes dispute-id) err-not-found))
            (job (unwrap! (map-get? jobs (get job-id dispute)) err-not-found))
            (escrow-amount (unwrap! (map-get? escrow-balance (get job-id dispute)) err-not-found))
        )
        (asserts! (or 
            (is-eq tx-sender contract-owner)
            (is-eq (some tx-sender) (get arbitrator dispute))
        ) err-unauthorized)
        
        ;; Transfer resolved amount from escrow
        (try! (as-contract (stx-transfer? resolution-amount tx-sender 
            (if (is-eq resolution-amount escrow-amount)
                (unwrap! (get freelancer job) err-not-found)
                (get client job)
            )
        )))
        
        ;; Update dispute and job status
        (map-set disputes dispute-id (merge dispute {status: u2}))
        (map-set jobs (get job-id dispute) (merge job {status: u3}))
        
        ;; Clear escrow
        (map-delete escrow-balance (get job-id dispute))
        (ok true)
    )
)

;; Rating and Reputation System
(define-map user-ratings
    principal
    {
        total-jobs: uint,
        completed-jobs: uint,
        average-rating: uint,
        ratings-count: uint
    }
)

;; Rate a completed job
(define-public (rate-job (job-id uint) (rating uint))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
            (rater tx-sender)
        )
        (asserts! (is-eq (get status job) u3) err-invalid-status)
        (asserts! (or 
            (is-eq rater (get client job))
            (is-eq rater (unwrap! (get freelancer job) err-not-found))
        ) err-unauthorized)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        
        (let
            (
                (target (if (is-eq rater (get client job)) 
                    (unwrap! (get freelancer job) err-not-found)
                    (get client job)))
                (current-rating (default-to 
                    {total-jobs: u0, completed-jobs: u0, average-rating: u0, ratings-count: u0} 
                    (map-get? user-ratings target)))
            )
            (map-set user-ratings target {
                total-jobs: (+ (get total-jobs current-rating) u1),
                completed-jobs: (+ (get completed-jobs current-rating) u1),
                average-rating: (/ 
                    (+ (* (get average-rating current-rating) (get ratings-count current-rating)) rating)
                    (+ (get ratings-count current-rating) u1)
                ),
                ratings-count: (+ (get ratings-count current-rating) u1)
            })
        )
        (ok true)
    )
)

;; Get user rating
(define-read-only (get-user-rating (user principal))
    (map-get? user-ratings user)
)

;; Additional Data Maps
(define-map job-ratings
    {job-id: uint, rater: principal}
    {rating: uint, comment: (string-ascii 200)}
)

(define-map freelancer-skills
    principal
    (list 10 (string-ascii 50))
)

(define-map user-profiles
    principal
    {
        name: (string-ascii 50),
        bio: (string-ascii 500),
        contact: (string-ascii 100),
        hourly-rate: uint,
        total-earnings: uint
    }
)

(define-map milestone-tracking
    {job-id: uint, milestone-id: uint}
    {
        description: (string-ascii 200),
        amount: uint,
        status: uint,  ;; 1-Pending, 2-Completed, 3-Paid
        deadline: uint
    }
)

;; Create or update user profile
(define-public (update-profile 
    (name (string-ascii 50)) 
    (bio (string-ascii 500))
    (contact (string-ascii 100))
    (hourly-rate uint)
)
    (let
        (
            (existing-profile (map-get? user-profiles tx-sender))
            (current-earnings (match existing-profile
                profile (get total-earnings profile)
                u0
            ))
        )
        (ok (map-set user-profiles tx-sender {
            name: name,
            bio: bio,
            contact: contact,
            hourly-rate: hourly-rate,
            total-earnings: current-earnings
        }))
    )
)

;; Read-only function to verify profile
(define-read-only (get-profile (user principal))
    (default-to
        {
            name: "", 
            bio: "", 
            contact: "", 
            hourly-rate: u0, 
            total-earnings: u0
        }
        (map-get? user-profiles user)
    )
)

;; Update freelancer skills
(define-public (update-skills (skills (list 10 (string-ascii 50))))
    (ok (map-set freelancer-skills tx-sender skills))
)

;; Helper function to get amount from milestone
(define-private (get-amount (milestone {description: (string-ascii 200), amount: uint, status: uint, deadline: uint}))
    (get amount milestone)
)

;; Helper function to build milestone list - needed by other functions
(define-private (get-milestone-list 
    (job-id uint) 
    (current-id uint) 
    (acc (list 5 {milestone-id: uint, description: (string-ascii 200), amount: uint, status: uint, deadline: uint}))
)
    (match (map-get? milestone-tracking {job-id: job-id, milestone-id: current-id})
        milestone (if (< (len acc) u5)
            (get-milestone-list 
                job-id 
                (+ current-id u1)
                (unwrap! (as-max-len? 
                    (append 
                        acc
                        {
                            milestone-id: current-id,
                            description: (get description milestone),
                            amount: (get amount milestone),
                            status: (get status milestone),
                            deadline: (get deadline milestone)
                        }
                    )
                    u5
                ) 
                acc)
            )
            acc
        )
        acc
    )
)

;; Get all milestones for a job
(define-read-only (get-job-milestones (job-id uint))
    (let
        (
            (milestone-list (list))
        )
        (ok (get-milestone-list job-id u0 milestone-list))
    )
)

;; Helper function to count completed milestones
(define-private (get-completed-milestone-count (job-id uint) (current-id uint) (count uint))
    (match (map-get? milestone-tracking {job-id: job-id, milestone-id: current-id})
        milestone (if (is-eq (get status milestone) u3)
            (get-completed-milestone-count job-id (+ current-id u1) (+ count u1))
            (get-completed-milestone-count job-id (+ current-id u1) count)
        )
        count
    )
)

;; Helper function to check if all milestones are completed
(define-private (all-milestones-completed (job-id uint))
    (let
        (
            (job (unwrap! (map-get? jobs job-id) err-not-found))
            (total-milestones (len (unwrap! (get-job-milestones job-id) false)))
            (completed-milestones (get-completed-milestone-count job-id u0 u0))
        )
        (is-eq total-milestones completed-milestones)
    )
)

;; Helper function to create milestones recursively
(define-private (create-milestones (job-id uint) (milestones (list 5 {description: (string-ascii 200), amount: uint, deadline: uint})) (milestone-id uint))
    (match (element-at milestones milestone-id)
        milestone (begin
            (map-set milestone-tracking 
                {job-id: job-id, milestone-id: milestone-id}
                {
                    description: (get description milestone),
                    amount: (get amount milestone),
                    status: u1,
                    deadline: (get deadline milestone)
                }
            )
            (if (< (+ milestone-id u1) (len milestones))
                (create-milestones job-id milestones (+ milestone-id u1))
                (ok true)
            )
        )
        (ok true)
    )
)

;; Post job with milestones
(define-public (post-job-with-milestones 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (budget uint) 
    (deadline uint)
    (milestone)))