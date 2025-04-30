
import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v0.14.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

// Helper function to assert success responses
function assertSuccess(receipt: any) {
  assertEquals(receipt.result.expectOk(), true);
}

// Helper function to assert error responses
function assertError(receipt: any, errorCode: number) {
  assertEquals(receipt.result.expectErr().expectUint(), errorCode);
}

Clarinet.test({
  name: "Ensure that job posting works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const client = accounts.get('wallet_1')!;
    const futureBlock = chain.blockHeight + 100;

    // Post a new job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Test Job"),
          types.ascii("This is a test job description"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);

    // Assert successful post
    assertEquals(block.receipts.length, 1);
    assertSuccess(block.receipts[0]);
    
    // The job ID should be 1 (first job)
    assertEquals(block.receipts[0].result.expectOk().expectUint(), 1);
    
    // Verify job details using get-job
    const jobQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-job",
      [types.uint(1)],
      client.address
    );
    
    const jobData = jobQuery.result.expectSome().expectTuple();
    assertEquals(jobData.client, client.address);
    assertEquals(jobData.title, types.ascii("Test Job"));
    assertEquals(jobData.budget, types.uint(1000000));
    assertEquals(jobData.status, types.uint(1)); // Status 1 = Open
  },
});

Clarinet.test({
  name: "Ensure that job posting fails with invalid parameters",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const pastBlock = chain.blockHeight - 100;
    
    // Try to post a job with 0 budget
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Test Job"),
          types.ascii("This is a test job description"),
          types.uint(0), // 0 STX - should fail
          types.uint(chain.blockHeight + 100)
        ],
        client.address
      )
    ]);
    
    // Assert failure with err-invalid-amount (u106)
    assertError(block.receipts[0], 106);
    
    // Try to post a job with past deadline
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Test Job"),
          types.ascii("This is a test job description"),
          types.uint(1000000),
          types.uint(pastBlock) // Past deadline - should fail
        ],
        client.address
      )
    ]);
    
    // Assert failure with err-past-deadline (u107)
    assertError(block.receipts[0], 107);
  },
});

Clarinet.test({
  name: "Ensure that bidding works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job first
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Test Job"),
          types.ascii("This is a test job description"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit a bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("I can complete this job efficiently")
        ],
        freelancer.address
      )
    ]);
    
    // Assert successful bid
    assertSuccess(block.receipts[0]);
    
    // Verify bid details
    const bidQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-bid",
      [types.uint(jobId), types.principal(freelancer.address)],
      freelancer.address
    );
    
    const bidData = bidQuery.result.expectSome().expectTuple();
    assertEquals(bidData.amount, types.uint(800000));
    assertEquals(bidData.proposal, types.ascii("I can complete this job efficiently"));
  },
});

Clarinet.test({
  name: "Ensure that bidding fails with invalid parameters",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job first
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Another Job"),
          types.ascii("This is another test job"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Try to bid above budget
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(1200000), // 1.2 STX - above budget
          types.ascii("Expensive proposal")
        ],
        freelancer.address
      )
    ]);
    
    // Assert failure with err-invalid-amount (u106)
    assertError(block.receipts[0], 106);
    
    // Try to bid as client (should fail)
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000),
          types.ascii("Client bidding on own job")
        ],
        client.address
      )
    ]);
    
    // Assert failure with err-unauthorized (u105)
    assertError(block.receipts[0], 105);
  },
});

Clarinet.test({
  name: "Ensure that accepting a bid works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Job for Acceptance"),
          types.ascii("Testing bid acceptance"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit a bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("I can do this job")
        ],
        freelancer.address
      )
    ]);
    
    // Accept the bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      )
    ]);
    
    // Assert successful acceptance
    assertSuccess(block.receipts[0]);
    
    // Verify job status is now "In Progress" (2)
    const jobQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-job",
      [types.uint(jobId)],
      client.address
    );
    
    const jobData = jobQuery.result.expectSome().expectTuple();
    assertEquals(jobData.status, types.uint(2)); // Status 2 = In Progress
    assertEquals(jobData.freelancer.expectSome(), freelancer.address);
    
    // Verify escrow balance
    const escrowQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-escrow-balance",
      [types.uint(jobId)],
      client.address
    );
    
    assertEquals(escrowQuery.result.expectUint(), 800000);
  },
});

Clarinet.test({
  name: "Ensure that accepting a bid fails with invalid parameters",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const otherUser = accounts.get('wallet_3')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Another Job"),
          types.ascii("Testing bid acceptance failures"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit a bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("I can do this job")
        ],
        freelancer.address
      )
    ]);
    
    // Try to accept bid as non-client
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        otherUser.address
      )
    ]);
    
    // Assert failure with err-unauthorized (u105)
    assertError(block.receipts[0], 105);
    
    // Accept bid properly
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      )
    ]);
    
    // Try to accept bid again (job already in progress)
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      )
    ]);
    
    // Assert failure with err-invalid-status (u103)
    assertError(block.receipts[0], 103);
  },
});

Clarinet.test({
  name: "Ensure that completing a job works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Completion Test Job"),
          types.ascii("Testing job completion"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit and accept bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("Ready to work")
        ],
        freelancer.address
      ),
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      )
    ]);
    
    // Check freelancer's initial balance
    const initialBalance = freelancer.balance;
    
    // Complete the job
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "complete-job",
        [types.uint(jobId)],
        client.address
      )
    ]);
    
    // Assert successful completion
    assertSuccess(block.receipts[0]);
    
    // Verify job status is now "Completed" (3)
    const jobQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-job",
      [types.uint(jobId)],
      client.address
    );
    
    const jobData = jobQuery.result.expectSome().expectTuple();
    assertEquals(jobData.status, types.uint(3)); // Status 3 = Completed
    
    // Escrow should be cleared
    const escrowQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-escrow-balance",
      [types.uint(jobId)],
      client.address
    );
    
    assertEquals(escrowQuery.result.expectUint(), 0);
    
    // Check freelancer's new balance - increased by bid amount
    const newBalance = chain.getAssetsMaps().assets[".STX"][freelancer.address];
    assertEquals(newBalance, initialBalance + 800000);
  },
});

Clarinet.test({
  name: "Ensure that job cancellation works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Job to Cancel"),
          types.ascii("Testing job cancellation"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Cancel the job
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "cancel-job",
        [types.uint(jobId)],
        client.address
      )
    ]);
    
    // Assert successful cancellation
    assertSuccess(block.receipts[0]);
    
    // Verify job status is now "Cancelled" (4)
    const jobQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-job",
      [types.uint(jobId)],
      client.address
    );
    
    const jobData = jobQuery.result.expectSome().expectTuple();
    assertEquals(jobData.status, types.uint(4)); // Status 4 = Cancelled
  },
});

Clarinet.test({
  name: "Ensure that dispute resolution works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!; // Contract owner
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post a job
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Dispute Test Job"),
          types.ascii("Testing dispute resolution"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit and accept bid
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("Ready to work")
        ],
        freelancer.address
      ),
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      )
    ]);
    
    // Open a dispute by client
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "open-dispute",
        [
          types.uint(jobId),
          types.ascii("Work not completed as agreed")
        ],
        client.address
      )
    ]);
    
    // Assert successful dispute creation
    assertSuccess(block.receipts[0]);
    const disputeId = block.receipts[0].result.expectOk().expectUint();
    
    // Check freelancer's initial balance
    const initialBalance = freelancer.balance;
    
    // Resolve dispute in freelancer's favor (by contract owner)
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "resolve-dispute",
        [
          types.uint(disputeId),
          types.uint(800000) // Full amount to freelancer
        ],
        deployer.address
      )
    ]);
    
    // Assert successful resolution
    assertSuccess(block.receipts[0]);
    
    // Verify job status is now "Completed" (3)
    const jobQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-job",
      [types.uint(jobId)],
      client.address
    );
    
    const jobData = jobQuery.result.expectSome().expectTuple();
    assertEquals(jobData.status, types.uint(3)); // Status 3 = Completed
    
    // Check freelancer's new balance - increased by resolved amount
    const newBalance = chain.getAssetsMaps().assets[".STX"][freelancer.address];
    assertEquals(newBalance, initialBalance + 800000);
  },
});

Clarinet.test({
  name: "Ensure that rating system works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const client = accounts.get('wallet_1')!;
    const freelancer = accounts.get('wallet_2')!;
    const futureBlock = chain.blockHeight + 100;
    
    // Post and complete a job first
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "post-job",
        [
          types.ascii("Rating Test Job"),
          types.ascii("Testing rating system"),
          types.uint(1000000), // 1 STX
          types.uint(futureBlock)
        ],
        client.address
      )
    ]);
    const jobId = block.receipts[0].result.expectOk().expectUint();
    
    // Submit and accept bid, then complete job
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "submit-bid",
        [
          types.uint(jobId),
          types.uint(800000), // 0.8 STX
          types.ascii("Ready to work")
        ],
        freelancer.address
      ),
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "accept-bid",
        [
          types.uint(jobId),
          types.principal(freelancer.address)
        ],
        client.address
      ),
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "complete-job",
        [types.uint(jobId)],
        client.address
      )
    ]);
    
    // Rate the freelancer (client rating freelancer)
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "rate-job",
        [
          types.uint(jobId),
          types.uint(5) // 5-star rating
        ],
        client.address
      )
    ]);
    
    // Assert successful rating
    assertSuccess(block.receipts[0]);
    
    // Verify freelancer rating
    const ratingQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-user-rating",
      [types.principal(freelancer.address)],
      client.address
    );
    
    const ratingData = ratingQuery.result.expectSome().expectTuple();
    assertEquals(ratingData['average-rating'], types.uint(5));
    assertEquals(ratingData['ratings-count'], types.uint(1));
    assertEquals(ratingData['completed-jobs'], types.uint(1));
    
    // Now let freelancer rate client
    block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "rate-job",
        [
          types.uint(jobId),
          types.uint(4) // 4-star rating
        ],
        freelancer.address
      )
    ]);
    
    // Assert successful rating
    assertSuccess(block.receipts[0]);
    
    // Verify client rating
    const clientRatingQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-user-rating",
      [types.principal(client.address)],
      freelancer.address
    );
    
    const clientRatingData = clientRatingQuery.result.expectSome().expectTuple();
    assertEquals(clientRatingData['average-rating'], types.uint(4));
    assertEquals(clientRatingData['ratings-count'], types.uint(1));
  },
});

Clarinet.test({
  name: "Ensure that user profiles work correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const freelancer = accounts.get('wallet_2')!;
    
    // Update user profile
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "update-profile",
        [
          types.ascii("John Doe"),
          types.ascii("Experienced web developer with 5 years of experience"),
          types.ascii("john@example.com"),
          types.uint(50000) // 0.05 STX per hour
        ],
        freelancer.address
      )
    ]);
    
    // Assert successful profile update
    assertSuccess(block.receipts[0]);
    
    // Verify profile details
    const profileQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-profile",
      [types.principal(freelancer.address)],
      freelancer.address
    );
    
    const profileData = profileQuery.result.expectTuple();
    assertEquals(profileData.name, types.ascii("John Doe"));
    assertEquals(profileData.bio, types.ascii("Experienced web developer with 5 years of experience"));
    assertEquals(profileData.contact, types.ascii("john@example.com"));
    assertEquals(profileData['hourly-rate'], types.uint(50000));
    assertEquals(profileData['total-earnings'], types.uint(0));
  },
});

Clarinet.test({
  name: "Ensure that skills update works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const freelancer = accounts.get('wallet_2')!;
    
    // Update skills
    let block = chain.mineBlock([
      Tx.contractCall(
        "decentralized-marketplace-for-freelancers",
        "update-skills",
        [
          types.list([
            types.ascii("JavaScript"),
            types.ascii("HTML"),
            types.ascii("CSS"),
            types.ascii("React")
          ])
        ],
        freelancer.address
      )
    ]);
    
    // Assert successful skills update
    assertSuccess(block.receipts[0]);
    
    // Get skills (would need a get-skills function which is not in the contract)
    // This is just to demonstrate the test would work if such a function existed
    /*
    const skillsQuery = chain.callReadOnlyFn(
      "decentralized-marketplace-for-freelancers",
      "get-skills",
      [types.principal(freelancer.address)],
      freelancer.address
    );
    
    const skills = skillsQuery.result.expectList();
    assertEquals(skills.length, 4);
    assertEquals(skills[0], types.ascii("JavaScript"));
    */
  },
});

// More tests can be added for:
// - Team creation and assignment
// - Invitation system
// - Skill verification
// - Smart deadlines
// - Referrals
// - etc.