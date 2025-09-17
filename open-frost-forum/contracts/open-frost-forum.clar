;; OpenFrostForum DAO Governance Contract
;; A progressive trust architecture for decentralized governance

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-MEMBER (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-TIER (err u105))
(define-constant ERR-PROPOSAL-EXPIRED (err u106))
(define-constant ERR-INVALID-PARAMETERS (err u107))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PROPOSAL-DURATION u144000) ;; ~1000 blocks (about 1 week)
(define-constant MIN-CONTRIBUTION-SCORE u100)
(define-constant VOTING-WEIGHT-MULTIPLIER u10)

;; Member tiers
(define-constant TIER-OBSERVER u1)
(define-constant TIER-CONTRIBUTOR u2)
(define-constant TIER-GOVERNOR u3)

;; Data variables
(define-data-var proposal-counter uint u0)
(define-data-var total-treasury uint u0)

;; Member data structure
(define-map members 
    principal 
    {
        tier: uint,
        contribution-score: uint,
        expertise-domains: (list 5 (string-ascii 20)),
        last-activity: uint,
        votes-cast: uint,
        proposals-created: uint
    })

;; Proposal data structure
(define-map proposals 
    uint 
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        proposer: principal,
        created-at: uint,
        expires-at: uint,
        for-votes: uint,
        against-votes: uint,
        status: (string-ascii 10),
        required-tier: uint,
        domain: (string-ascii 20)
    })

;; Vote tracking
(define-map votes 
    {proposal-id: uint, voter: principal} 
    {
        vote: bool, ;; true for yes, false for no
        weight: uint,
        timestamp: uint
    })

;; Expertise domains for competency scoring
(define-map domain-expertise 
    {member: principal, domain: (string-ascii 20)} 
    uint)

;; Read-only functions

;; Get member information
(define-read-only (get-member (member principal))
    (map-get? members member))

;; Get proposal information
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

;; Get vote information
(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter}))

;; Calculate voting weight based on tier and expertise
(define-read-only (calculate-voting-weight (voter principal) (domain (string-ascii 20)))
    (let (
        (member-data (unwrap! (map-get? members voter) u0))
        (base-weight (get tier member-data))
        (expertise-bonus (default-to u0 (map-get? domain-expertise {member: voter, domain: domain})))
        (activity-bonus (if (> (get votes-cast member-data) u10) u2 u1))
    )
    (* (+ base-weight expertise-bonus activity-bonus) VOTING-WEIGHT-MULTIPLIER)))

;; Check if proposal is active
(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and 
            (is-eq (get status proposal) "active")
            (<= stacks-block-height (get expires-at proposal)))
        false))

;; Get current proposal counter
(define-read-only (get-proposal-counter)
    (var-get proposal-counter))

;; Public functions

;; Register as a new member (starts as Observer)
(define-public (register-member)
    (let (
        (caller tx-sender)
    )
    (asserts! (is-none (map-get? members caller)) ERR-INVALID-MEMBER)
    (map-set members caller {
        tier: TIER-OBSERVER,
        contribution-score: u0,
        expertise-domains: (list),
        last-activity: stacks-block-height,
        votes-cast: u0,
        proposals-created: u0
    })
    (ok true)))

;; Upgrade member tier based on contribution score
(define-public (upgrade-member-tier (member principal))
    (let (
        (member-data (unwrap! (map-get? members member) ERR-INVALID-MEMBER))
        (current-score (get contribution-score member-data))
        (current-tier (get tier member-data))
        (new-tier (if (>= current-score u1000) TIER-GOVERNOR
                     (if (>= current-score u300) TIER-CONTRIBUTOR
                        TIER-OBSERVER)))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (if (> new-tier current-tier)
        (begin
            (map-set members member (merge member-data {tier: new-tier}))
            (ok true))
        (ok false))))

;; Add contribution score to member
(define-public (add-contribution-score (member principal) (score uint) (domain (string-ascii 20)))
    (let (
        (member-data (unwrap! (map-get? members member) ERR-INVALID-MEMBER))
        (current-score (get contribution-score member-data))
        (current-expertise (default-to u0 (map-get? domain-expertise {member: member, domain: domain})))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Update member contribution score
    (map-set members member (merge member-data {
        contribution-score: (+ current-score score),
        last-activity: stacks-block-height
    }))
    
    ;; Update domain expertise
    (map-set domain-expertise {member: member, domain: domain} (+ current-expertise (/ score u10)))
    
    (ok true)))

;; Create a new proposal
(define-public (create-proposal 
    (title (string-ascii 50)) 
    (description (string-ascii 200))
    (required-tier uint)
    (domain (string-ascii 20)))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR-INVALID-MEMBER))
        (proposal-id (+ (var-get proposal-counter) u1))
    )
    (asserts! (>= (get tier member-data) TIER-CONTRIBUTOR) ERR-INSUFFICIENT-TIER)
    (asserts! (and (>= required-tier TIER-OBSERVER) (<= required-tier TIER-GOVERNOR)) ERR-INVALID-PARAMETERS)
    
    ;; Create proposal
    (map-set proposals proposal-id {
        title: title,
        description: description,
        proposer: caller,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height PROPOSAL-DURATION),
        for-votes: u0,
        against-votes: u0,
        status: "active",
        required-tier: required-tier,
        domain: domain
    })
    
    ;; Update member data
    (map-set members caller (merge member-data {
        proposals-created: (+ (get proposals-created member-data) u1),
        last-activity: stacks-block-height
    }))
    
    ;; Increment proposal counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR-INVALID-MEMBER))
        (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voting-weight (calculate-voting-weight caller (get domain proposal-data)))
        (existing-vote (map-get? votes {proposal-id: proposal-id, voter: caller}))
    )
    
    ;; Validation checks
    (asserts! (>= (get tier member-data) (get required-tier proposal-data)) ERR-INSUFFICIENT-TIER)
    (asserts! (is-proposal-active proposal-id) ERR-VOTING-CLOSED)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    
    ;; Record vote
    (map-set votes {proposal-id: proposal-id, voter: caller} {
        vote: vote-for,
        weight: voting-weight,
        timestamp: stacks-block-height
    })
    
    ;; Update proposal vote counts
    (if vote-for
        (map-set proposals proposal-id (merge proposal-data {
            for-votes: (+ (get for-votes proposal-data) voting-weight)
        }))
        (map-set proposals proposal-id (merge proposal-data {
            against-votes: (+ (get against-votes proposal-data) voting-weight)
        })))
    
    ;; Update member activity
    (map-set members caller (merge member-data {
        votes-cast: (+ (get votes-cast member-data) u1),
        last-activity: stacks-block-height
    }))
    
    (ok true)))

;; Finalize proposal (close voting)
(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (for-votes (get for-votes proposal-data))
        (against-votes (get against-votes proposal-data))
        (total-votes (+ for-votes against-votes))
        (passed (and (> total-votes u0) (> for-votes against-votes)))
    )
    
    ;; Check if proposal has expired or caller is authorized
    (asserts! (or 
        (> stacks-block-height (get expires-at proposal-data))
        (is-eq tx-sender CONTRACT-OWNER)
        (is-eq tx-sender (get proposer proposal-data))) 
        ERR-NOT-AUTHORIZED)
    
    ;; Update proposal status
    (map-set proposals proposal-id (merge proposal-data {
        status: (if passed "passed" "rejected")
    }))
    
    (ok passed)))

;; Administrative function to update treasury
(define-public (update-treasury (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set total-treasury amount)
        (ok true)))

;; Get treasury balance
(define-read-only (get-treasury-balance)
    (var-get total-treasury))

;; Emergency pause function
(define-public (emergency-pause-proposal (proposal-id uint))
    (let (
        (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set proposals proposal-id (merge proposal-data {status: "paused"}))
    (ok true)))

;; Initialize contract
(begin
    (map-set members CONTRACT-OWNER {
        tier: TIER-GOVERNOR,
        contribution-score: u1000,
        expertise-domains: (list "governance" "technical"),
        last-activity: stacks-block-height,
        votes-cast: u0,
        proposals-created: u0
    }))