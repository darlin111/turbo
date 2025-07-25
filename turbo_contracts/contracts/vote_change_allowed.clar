;; Enhanced Voting Contract with Advanced Features
;; Comprehensive voting system with multiple voting types, delegation, and advanced security

;; Error constants
(define-constant err-voting-ended (err u100))
(define-constant err-voting-not-started (err u101))
(define-constant err-invalid-candidate (err u102))
(define-constant err-not-authorized (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-invalid-proposal (err u105))
(define-constant err-insufficient-stake (err u106))
(define-constant err-blacklisted (err u107))
(define-constant err-invalid-delegation (err u108))
(define-constant err-voting-paused (err u109))
(define-constant err-invalid-weight (err u110))
(define-constant err-max-candidates-reached (err u111))
(define-constant err-invalid-timelock (err u112))

;; Contract constants
(define-constant contract-owner tx-sender)
(define-constant max-candidates u50)
(define-constant min-voting-period u144) ;; ~24 hours in blocks
(define-constant max-voting-period u4320) ;; ~30 days in blocks
(define-constant min-stake-amount u1000000) ;; 1 STX in micro-STX

;; Contract state variables
(define-data-var voting-active bool false)
(define-data-var voting-paused bool false)
(define-data-var voting-start-block uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var total-votes uint u0)
(define-data-var total-weight uint u0)
(define-data-var next-candidate-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var voting-type (string-ascii 20) "simple")
(define-data-var quorum-required uint u0)
(define-data-var vote-change-fee uint u0)
(define-data-var emergency-stop bool false)
(define-data-var timelock-period uint u0)

;; Enhanced candidate structure
(define-map candidates 
  { candidate-id: uint } 
  { 
    name: (string-ascii 50), 
    description: (string-ascii 200),
    vote-count: uint, 
    weighted-votes: uint,
    active: bool,
    created-by: principal,
    creation-block: uint,
    metadata: (string-ascii 100)
  })

;; Voter enhanced tracking
(define-map voter-info
  { voter: principal }
  {
    total-votes-cast: uint,
    voting-power: uint,
    reputation-score: uint,
    stake-amount: uint,
    is-blacklisted: bool,
    last-vote-block: uint,
    delegate: (optional principal)
  })

;; Vote delegation system
(define-map delegations
  { delegator: principal }
  { delegate: principal, weight: uint, active: bool })

(define-map delegate-power
  { delegate: principal }
  { total-delegated-power: uint, delegator-count: uint })

;; Enhanced voting choices with weights
(define-map voter-choices 
  { voter: principal } 
  { 
    candidate-id: uint, 
    voted-at-block: uint,
    vote-weight: uint,
    is-delegated: bool,
    original-voter: principal
  })

;; Proposals system (for governance)
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    status: (string-ascii 20),
    created-at: uint,
    voting-ends-at: uint,
    execution-delay: uint,
    proposal-type: (string-ascii 30)
  })

;; Proposal votes
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: (string-ascii 10), weight: uint, timestamp: uint })

;; Staking system for voting power
(define-map voter-stakes
  { voter: principal }
  { amount: uint, locked-until: uint, multiplier: uint })

;; Vote history with enhanced tracking
(define-map vote-history
  { voter: principal, vote-number: uint }
  { 
    candidate-id: uint, 
    timestamp: uint,
    vote-type: (string-ascii 20),
    weight: uint,
    fee-paid: uint
  })

;; Blacklist system
(define-map blacklist
  { address: principal }
  { reason: (string-ascii 100), blacklisted-at: uint, blacklisted-by: principal })

;; Admin roles
(define-map admins
  { admin: principal }
  { permissions: uint, added-by: principal, added-at: uint })

;; Voting statistics
(define-map voting-stats
  { period: uint }
  {
    total-participants: uint,
    total-votes: uint,
    total-weight: uint,
    winner-candidate: uint,
    participation-rate: uint
  })

;; Read-only functions

(define-read-only (is-voting-active)
  (and 
    (var-get voting-active) 
    (>= block-height (var-get voting-start-block))
    (< block-height (var-get voting-end-block))
    (not (var-get voting-paused))
    (not (var-get emergency-stop))))

(define-read-only (get-candidate (candidate-id uint))
  (map-get? candidates { candidate-id: candidate-id }))

(define-read-only (get-voter-info (voter principal))
  (map-get? voter-info { voter: voter }))

(define-read-only (get-voter-choice (voter principal))
  (map-get? voter-choices { voter: voter }))

(define-read-only (get-voting-power (voter principal))
  (let ((voter-data (map-get? voter-info { voter: voter }))
        (stake-data (map-get? voter-stakes { voter: voter })))
    (match voter-data
      data (+ (get voting-power data) 
              (match stake-data
                stake (* (get amount stake) (get multiplier stake))
                u0))
      u1))) ;; Default voting power

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations { delegator: delegator }))

(define-read-only (get-delegate-power (delegate principal))
  (map-get? delegate-power { delegate: delegate }))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id }))

(define-read-only (is-blacklisted (address principal))
  (is-some (map-get? blacklist { address: address })))

(define-read-only (get-quorum-met)
  (>= (var-get total-weight) (var-get quorum-required)))

(define-read-only (get-voting-stats (period uint))
  (map-get? voting-stats { period: period }))

(define-read-only (calculate-results)
  (let ((candidate-1-votes (get-candidate-votes u1))
        (candidate-2-votes (get-candidate-votes u2))
        (candidate-3-votes (get-candidate-votes u3)))
    {
      candidate-1: candidate-1-votes,
      candidate-2: candidate-2-votes,
      candidate-3: candidate-3-votes,
      total: (+ candidate-1-votes candidate-2-votes candidate-3-votes),
      quorum-met: (get-quorum-met)
    }))

(define-read-only (get-candidate-votes (candidate-id uint))
  (match (map-get? candidates { candidate-id: candidate-id })
    candidate-data (get weighted-votes candidate-data)
    u0))

;; Administrative functions

(define-public (add-admin (admin principal) (permissions uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set admins
      { admin: admin }
      { permissions: permissions, added-by: tx-sender, added-at: block-height })
    (ok true)))

(define-public (blacklist-address (address principal) (reason (string-ascii 100)))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-some (map-get? admins { admin: tx-sender }))) err-not-authorized)
    (map-set blacklist
      { address: address }
      { reason: reason, blacklisted-at: block-height, blacklisted-by: tx-sender })
    (map-set voter-info
      { voter: address }
      (merge (default-to 
               { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                 stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                 delegate: none }
               (map-get? voter-info { voter: address }))
             { is-blacklisted: true }))
    (ok true)))

(define-public (remove-blacklist (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-delete blacklist { address: address })
    (map-set voter-info
      { voter: address }
      (merge (default-to 
               { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                 stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                 delegate: none }
               (map-get? voter-info { voter: address }))
             { is-blacklisted: false }))
    (ok true)))

(define-public (pause-voting)
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-some (map-get? admins { admin: tx-sender }))) err-not-authorized)
    (var-set voting-paused true)
    (ok true)))

(define-public (resume-voting)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set voting-paused false)
    (ok true)))

(define-public (emergency-stop-toggle)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set emergency-stop (not (var-get emergency-stop)))
    (ok (var-get emergency-stop))))

(define-public (add-candidate (name (string-ascii 50)) (description (string-ascii 200)) (metadata (string-ascii 100)))
  (let ((candidate-id (var-get next-candidate-id)))
    (begin
      (asserts! (< candidate-id max-candidates) err-max-candidates-reached)
      (asserts! (or (is-eq tx-sender contract-owner) 
                    (is-some (map-get? admins { admin: tx-sender }))) err-not-authorized)
      (map-set candidates 
        { candidate-id: candidate-id }
        { 
          name: name, 
          description: description,
          vote-count: u0, 
          weighted-votes: u0,
          active: true,
          created-by: tx-sender,
          creation-block: block-height,
          metadata: metadata
        })
      (var-set next-candidate-id (+ candidate-id u1))
      (ok candidate-id))))

(define-public (deactivate-candidate (candidate-id uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-some (map-get? admins { admin: tx-sender }))) err-not-authorized)
    (match (map-get? candidates { candidate-id: candidate-id })
      candidate-data
      (begin
        (map-set candidates
          { candidate-id: candidate-id }
          (merge candidate-data { active: false }))
        (ok true))
      err-invalid-candidate)))

(define-public (start-voting (duration-blocks uint) (voting-type-str (string-ascii 20)) (quorum uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (>= duration-blocks min-voting-period) 
                   (<= duration-blocks max-voting-period)) err-invalid-timelock)
    (var-set voting-active true)
    (var-set voting-start-block block-height)
    (var-set voting-end-block (+ block-height duration-blocks))
    (var-set voting-type voting-type-str)
    (var-set quorum-required quorum)
    (var-set total-votes u0)
    (var-set total-weight u0)
    (ok true)))

;; Staking system
(define-public (stake-tokens (amount uint) (lock-period uint))
  (begin
    (asserts! (>= amount min-stake-amount) err-insufficient-stake)
    (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
    (let ((multiplier (if (>= lock-period u4320) u3 ;; 30 days = 3x multiplier
                         (if (>= lock-period u1440) u2 ;; 10 days = 2x multiplier
                             u1)))) ;; default = 1x multiplier
      (map-set voter-stakes
        { voter: tx-sender }
        { 
          amount: amount, 
          locked-until: (+ block-height lock-period),
          multiplier: multiplier
        })
      ;; Transfer STX to contract (simplified - in real implementation use STX transfer)
      (ok multiplier))))

(define-public (unstake-tokens)
  (match (map-get? voter-stakes { voter: tx-sender })
    stake-data
    (begin
      (asserts! (>= block-height (get locked-until stake-data)) err-invalid-timelock)
      (map-delete voter-stakes { voter: tx-sender })
      ;; Return STX to voter (simplified)
      (ok (get amount stake-data)))
    (ok u0)))

;; Delegation system
(define-public (delegate-vote (delegate-to principal) (weight uint))
  (begin
    (asserts! (not (is-eq tx-sender delegate-to)) err-invalid-delegation)
    (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
    (asserts! (not (is-blacklisted delegate-to)) err-blacklisted)
    (asserts! (> weight u0) err-invalid-weight)
    
    ;; Update delegation
    (map-set delegations
      { delegator: tx-sender }
      { delegate: delegate-to, weight: weight, active: true })
    
    ;; Update delegate power
    (let ((current-power (default-to 
                          { total-delegated-power: u0, delegator-count: u0 }
                          (map-get? delegate-power { delegate: delegate-to }))))
      (map-set delegate-power
        { delegate: delegate-to }
        { 
          total-delegated-power: (+ (get total-delegated-power current-power) weight),
          delegator-count: (+ (get delegator-count current-power) u1)
        }))
    
    ;; Update voter info
    (map-set voter-info
      { voter: tx-sender }
      (merge (default-to 
              { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                delegate: none }
              (map-get? voter-info { voter: tx-sender }))
             { delegate: (some delegate-to) }))
    
    (ok true)))

(define-public (revoke-delegation)
  (match (map-get? delegations { delegator: tx-sender })
    delegation-data
    (let ((delegate-addr (get delegate delegation-data))
          (weight (get weight delegation-data)))
      (begin
        ;; Remove delegation
        (map-delete delegations { delegator: tx-sender })
        
        ;; Update delegate power
        (match (map-get? delegate-power { delegate: delegate-addr })
          power-data
          (map-set delegate-power
            { delegate: delegate-addr }
            { 
              total-delegated-power: (- (get total-delegated-power power-data) weight),
              delegator-count: (- (get delegator-count power-data) u1)
            })
          true)
        
        ;; Update voter info
        (map-set voter-info
          { voter: tx-sender }
          (merge (default-to 
                  { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                    stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                    delegate: none }
                  (map-get? voter-info { voter: tx-sender }))
                 { delegate: none }))
        
        (ok true)))
    (ok false)))

;; Enhanced voting function
(define-public (vote (candidate-id uint))
  (let ((current-choice (map-get? voter-choices { voter: tx-sender }))
        (candidate-data (map-get? candidates { candidate-id: candidate-id }))
        (voter-data (map-get? voter-info { voter: tx-sender }))
        (voting-power (get-voting-power tx-sender))
        (fee (var-get vote-change-fee)))
    (begin
      ;; Security checks
      (asserts! (is-voting-active) err-voting-ended)
      (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
      (asserts! (is-some candidate-data) err-invalid-candidate)
      (asserts! (get active (unwrap-panic candidate-data)) err-invalid-candidate)
      
      ;; Handle vote change with fee
      (match current-choice
        existing-choice
        (let ((old-candidate-id (get candidate-id existing-choice))
              (old-candidate-data (unwrap-panic (map-get? candidates { candidate-id: old-candidate-id }))))
          (if (not (is-eq old-candidate-id candidate-id))
            (begin
              ;; Charge fee for vote change (if applicable)
              (if (> fee u0)
                (unwrap-panic (stx-transfer? fee tx-sender contract-owner))
                true)
              
              ;; Update old candidate
              (map-set candidates
                { candidate-id: old-candidate-id }
                (merge old-candidate-data { 
                  vote-count: (- (get vote-count old-candidate-data) u1),
                  weighted-votes: (- (get weighted-votes old-candidate-data) voting-power)
                }))
              
              ;; Update new candidate
              (map-set candidates
                { candidate-id: candidate-id }
                (merge (unwrap-panic candidate-data) { 
                  vote-count: (+ (get vote-count (unwrap-panic candidate-data)) u1),
                  weighted-votes: (+ (get weighted-votes (unwrap-panic candidate-data)) voting-power)
                }))
              
              ;; Update voter choice
              (map-set voter-choices
                { voter: tx-sender }
                { 
                  candidate-id: candidate-id, 
                  voted-at-block: block-height,
                  vote-weight: voting-power,
                  is-delegated: false,
                  original-voter: tx-sender
                })
              
              ;; Update vote history
              (let ((vote-count (get total-votes-cast (default-to 
                                  { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                                    stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                                    delegate: none }
                                  voter-data))))
                (map-set vote-history
                  { voter: tx-sender, vote-number: (+ vote-count u1) }
                  { 
                    candidate-id: candidate-id, 
                    timestamp: block-height,
                    vote-type: "change",
                    weight: voting-power,
                    fee-paid: fee
                  }))
              
              (ok "vote-changed"))
            (ok "same-candidate")))
        
        ;; First time voting
        (begin
          ;; Update candidate
          (map-set candidates
            { candidate-id: candidate-id }
            (merge (unwrap-panic candidate-data) { 
              vote-count: (+ (get vote-count (unwrap-panic candidate-data)) u1),
              weighted-votes: (+ (get weighted-votes (unwrap-panic candidate-data)) voting-power)
            }))
          
          ;; Record voter choice
          (map-set voter-choices
            { voter: tx-sender }
            { 
              candidate-id: candidate-id, 
              voted-at-block: block-height,
              vote-weight: voting-power,
              is-delegated: false,
              original-voter: tx-sender
            })
          
          ;; Update totals
          (var-set total-votes (+ (var-get total-votes) u1))
          (var-set total-weight (+ (var-get total-weight) voting-power))
          
          ;; Update voter info
          (map-set voter-info
            { voter: tx-sender }
            (merge (default-to 
                    { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                      stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                      delegate: none }
                    voter-data)
                   { 
                     total-votes-cast: (+ (get total-votes-cast (default-to 
                                           { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                                             stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                                             delegate: none }
                                           voter-data)) u1),
                     last-vote-block: block-height,
                     reputation-score: (+ (get reputation-score (default-to 
                                           { total-votes-cast: u0, voting-power: u1, reputation-score: u0, 
                                             stake-amount: u0, is-blacklisted: false, last-vote-block: u0, 
                                             delegate: none }
                                           voter-data)) u1)
                   }))
          
          ;; Record vote history
          (map-set vote-history
            { voter: tx-sender, vote-number: u1 }
            { 
              candidate-id: candidate-id, 
              timestamp: block-height,
              vote-type: "initial",
              weight: voting-power,
              fee-paid: u0
            })
          
          (ok "vote-cast"))))))

;; Proposal system
(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) 
                               (proposal-type (string-ascii 30)) (execution-delay uint))
  (let ((proposal-id (var-get next-proposal-id)))
    (begin
      (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
      (asserts! (>= (get-voting-power tx-sender) min-stake-amount) err-insufficient-stake)
      
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description: description,
          proposer: tx-sender,
          yes-votes: u0,
          no-votes: u0,
          abstain-votes: u0,
          status: "active",
          created-at: block-height,
          voting-ends-at: (+ block-height u1440), ;; 10 days
          execution-delay: execution-delay,
          proposal-type: proposal-type
        })
      
      (var-set next-proposal-id (+ proposal-id u1))
      (ok proposal-id))))

(define-public (vote-on-proposal (proposal-id uint) (choice (string-ascii 10)))
  (let ((proposal-data (map-get? proposals { proposal-id: proposal-id }))
        (voting-power (get-voting-power tx-sender)))
    (begin
      (asserts! (is-some proposal-data) err-invalid-proposal)
      (asserts! (not (is-blacklisted tx-sender)) err-blacklisted)
      (asserts! (< block-height (get voting-ends-at (unwrap-panic proposal-data))) err-voting-ended)
      (asserts! (is-eq (get status (unwrap-panic proposal-data)) "active") err-voting-ended)
      
      ;; Record vote
      (map-set proposal-votes
        { proposal-id: proposal-id, voter: tx-sender }
        { vote: choice, weight: voting-power, timestamp: block-height })
      
      ;; Update proposal counts
      (let ((updated-proposal (if (is-eq choice "yes")
                                (merge (unwrap-panic proposal-data) 
                                       { yes-votes: (+ (get yes-votes (unwrap-panic proposal-data)) voting-power) })
                                (if (is-eq choice "no")
                                  (merge (unwrap-panic proposal-data) 
                                         { no-votes: (+ (get no-votes (unwrap-panic proposal-data)) voting-power) })
                                  (merge (unwrap-panic proposal-data) 
                                         { abstain-votes: (+ (get abstain-votes (unwrap-panic proposal-data)) voting-power) })))))
        (map-set proposals { proposal-id: proposal-id } updated-proposal))
      
      (ok true))))

;; Set various parameters
(define-public (set-vote-change-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set vote-change-fee fee)
    (ok true)))

(define-public (set-timelock-period (period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set timelock-period period)
    (ok true)))

;; Batch operations
(define-public (batch-add-candidates (candidates-list (list 10 { name: (string-ascii 50), description: (string-ascii 200), metadata: (string-ascii 100) })))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map add-candidate-batch candidates-list))))

(define-private (add-candidate-batch (candidate-data { name: (string-ascii 50), description: (string-ascii 200), metadata: (string-ascii 100) }))
  (add-candidate (get name candidate-data) (get description candidate-data) (get metadata candidate-data)))

;; Initialize with enhanced setup
(define-public (initialize-enhanced)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (unwrap-panic (add-candidate "Alice Johnson" "Experienced leader with 10 years in governance" "progressive"))
    (unwrap-panic (add-candidate "Bob Smith" "Technology expert focusing on innovation" "tech-focused"))
    (unwrap-panic (add-candidate "Charlie Davis" "Community advocate for social justice" "social-justice"))
    (unwrap-panic (add-candidate "Diana Wong" "Environmental policy specialist" "environmental"))
    (var-set quorum-required u1000)
    (var-set vote-change-fee u100000) ;; 0.1 STX
    (ok true)))