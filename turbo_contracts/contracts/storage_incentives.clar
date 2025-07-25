;; Storage Incentives Contract
;; Rewards long-term storage commitments with bonus payments

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-FILE-NOT-FOUND (err u1001))
(define-constant ERR-COMMITMENT-NOT-FOUND (err u1002))
(define-constant ERR-COMMITMENT-ACTIVE (err u1003))
(define-constant ERR-COMMITMENT-EXPIRED (err u1004))
(define-constant ERR-INVALID-DURATION (err u1005))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1006))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u1007))
(define-constant ERR-VERIFICATION-FAILED (err u1008))
(define-constant ERR-INVALID-INPUT (err u1009))
(define-constant ERR-SIZE-TOO-LARGE (err u1010))
(define-constant ERR-DURATION-TOO-LONG (err u1011))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Minimum storage duration (in blocks) - approximately 1 month
(define-constant MIN-STORAGE-DURATION u4320)

;; Maximum storage duration (in blocks) - approximately 5 years to prevent overflow
(define-constant MAX-STORAGE-DURATION u21600000)

;; Maximum file size in MB to prevent overflow attacks
(define-constant MAX-FILE-SIZE-MB u100000)

;; Base reward rate (micro-STX per block per MB)
(define-constant BASE-REWARD-RATE u10)

;; Maximum reward amount to prevent overflow
(define-constant MAX-REWARD-AMOUNT u1000000000000)

;; Bonus multipliers for different commitment periods
(define-constant BONUS-3-MONTHS u150) ;; 1.5x multiplier
(define-constant BONUS-6-MONTHS u200) ;; 2.0x multiplier
(define-constant BONUS-12-MONTHS u300) ;; 3.0x multiplier

;; File storage record
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    size-mb: uint,
    created-at: uint,
    is-active: bool
  }
)

;; Storage commitments
(define-map commitments
  { file-id: (string-ascii 64), commitment-id: uint }
  {
    storage-provider: principal,
    duration-blocks: uint,
    start-block: uint,
    end-block: uint,
    bonus-multiplier: uint,
    reward-amount: uint,
    is-verified: bool,
    is-claimed: bool
  }
)

;; Track commitment IDs per file
(define-map file-commitment-counter
  { file-id: (string-ascii 64) }
  { counter: uint }
)

;; Storage provider balances
(define-map provider-balances
  { provider: principal }
  { balance: uint }
)

;; Contract balance for rewards
(define-data-var contract-balance uint u0)

;; Input validation helper
(define-private (is-valid-file-id (file-id (string-ascii 64)))
  (and 
    (> (len file-id) u0)
    (<= (len file-id) u64)
  )
)

;; Register a new file for storage incentives
(define-public (register-file (file-id (string-ascii 64)) (size-mb uint))
  (let ((file-data {
    owner: tx-sender,
    size-mb: size-mb,
    created-at: block-height,
    is-active: true
  }))
    ;; Input validation
    (asserts! (is-valid-file-id file-id) ERR-INVALID-INPUT)
    (asserts! (and (> size-mb u0) (<= size-mb MAX-FILE-SIZE-MB)) ERR-SIZE-TOO-LARGE)
    (asserts! (is-none (map-get? files { file-id: file-id })) ERR-FILE-NOT-FOUND)
    
    (map-set files { file-id: file-id } file-data)
    (map-set file-commitment-counter { file-id: file-id } { counter: u0 })
    (ok file-id)
  )
)

;; Create a storage commitment
(define-public (create-commitment 
  (file-id (string-ascii 64)) 
  (duration-blocks uint)
)
  (let (
    (file-data (unwrap! (map-get? files { file-id: file-id }) ERR-FILE-NOT-FOUND))
    (commitment-counter (default-to { counter: u0 } (map-get? file-commitment-counter { file-id: file-id })))
    (commitment-id (+ (get counter commitment-counter) u1))
    (bonus-multiplier (calculate-bonus-multiplier duration-blocks))
    (reward-amount (calculate-reward-amount (get size-mb file-data) duration-blocks bonus-multiplier))
    (end-block (+ block-height duration-blocks))
  )
    ;; Input validation
    (asserts! (is-valid-file-id file-id) ERR-INVALID-INPUT)
    (asserts! (and (>= duration-blocks MIN-STORAGE-DURATION) (<= duration-blocks MAX-STORAGE-DURATION)) ERR-DURATION-TOO-LONG)
    (asserts! (get is-active file-data) ERR-FILE-NOT-FOUND)
    (asserts! (<= reward-amount MAX-REWARD-AMOUNT) ERR-INVALID-INPUT)
    
    ;; Prevent integer overflow in end-block calculation
    (asserts! (>= end-block block-height) ERR-INVALID-INPUT)
    
    ;; Create commitment record
    (map-set commitments 
      { file-id: file-id, commitment-id: commitment-id }
      {
        storage-provider: tx-sender,
        duration-blocks: duration-blocks,
        start-block: block-height,
        end-block: end-block,
        bonus-multiplier: bonus-multiplier,
        reward-amount: reward-amount,
        is-verified: false,
        is-claimed: false
      }
    )
    
    ;; Update commitment counter
    (map-set file-commitment-counter { file-id: file-id } { counter: commitment-id })
    
    (ok commitment-id)
  )
)

;; Verify storage commitment (called by contract owner or authorized verifier)
(define-public (verify-storage-commitment 
  (file-id (string-ascii 64)) 
  (commitment-id uint)
)
  (let (
    (commitment-key { file-id: file-id, commitment-id: commitment-id })
    (commitment-data (unwrap! (map-get? commitments commitment-key) ERR-COMMITMENT-NOT-FOUND))
  )
    ;; Input validation
    (asserts! (is-valid-file-id file-id) ERR-INVALID-INPUT)
    (asserts! (> commitment-id u0) ERR-INVALID-INPUT)
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= block-height (get end-block commitment-data)) ERR-COMMITMENT-ACTIVE)
    (asserts! (not (get is-verified commitment-data)) ERR-VERIFICATION-FAILED)
    
    ;; Mark as verified
    (map-set commitments commitment-key
      (merge commitment-data { is-verified: true })
    )
    
    (ok true)
  )
)

;; Claim storage reward
(define-public (claim-reward 
  (file-id (string-ascii 64)) 
  (commitment-id uint)
)
  (let (
    (commitment-key { file-id: file-id, commitment-id: commitment-id })
    (commitment-data (unwrap! (map-get? commitments commitment-key) ERR-COMMITMENT-NOT-FOUND))
    (reward-amount (get reward-amount commitment-data))
    (provider-balance (default-to { balance: u0 } (map-get? provider-balances { provider: tx-sender })))
  )
    ;; Input validation
    (asserts! (is-valid-file-id file-id) ERR-INVALID-INPUT)
    (asserts! (> commitment-id u0) ERR-INVALID-INPUT)
    (asserts! (is-eq tx-sender (get storage-provider commitment-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-verified commitment-data) ERR-VERIFICATION-FAILED)
    (asserts! (not (get is-claimed commitment-data)) ERR-REWARD-ALREADY-CLAIMED)
    (asserts! (>= (var-get contract-balance) reward-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Prevent overflow in balance addition
    (asserts! (<= reward-amount (- u340282366920938463463374607431768211455 (get balance provider-balance))) ERR-INVALID-INPUT)
    
    ;; Mark as claimed
    (map-set commitments commitment-key
      (merge commitment-data { is-claimed: true })
    )
    
    ;; Update provider balance
    (map-set provider-balances 
      { provider: tx-sender }
      { balance: (+ (get balance provider-balance) reward-amount) }
    )
    
    ;; Update contract balance
    (var-set contract-balance (- (var-get contract-balance) reward-amount))
    
    (ok reward-amount)
  )
)

;; Withdraw earned rewards
(define-public (withdraw-rewards (amount uint))
  (let (
    (provider-balance (default-to { balance: u0 } (map-get? provider-balances { provider: tx-sender })))
  )
    ;; Input validation
    (asserts! (> amount u0) ERR-INVALID-INPUT)
    (asserts! (>= (get balance provider-balance) amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update provider balance
    (map-set provider-balances 
      { provider: tx-sender }
      { balance: (- (get balance provider-balance) amount) }
    )
    
    ;; Transfer STX to provider
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    
    (ok amount)
  )
)

;; Fund the contract (only owner)
(define-public (fund-contract (amount uint))
  (begin
    ;; Input validation
    (asserts! (> amount u0) ERR-INVALID-INPUT)
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Prevent overflow in contract balance
    (asserts! (<= amount (- u340282366920938463463374607431768211455 (var-get contract-balance))) ERR-INVALID-INPUT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok amount)
  )
)

;; Calculate bonus multiplier based on commitment duration
(define-private (calculate-bonus-multiplier (duration-blocks uint))
  (if (>= duration-blocks (* MIN-STORAGE-DURATION u12)) ;; 12 months
    BONUS-12-MONTHS
    (if (>= duration-blocks (* MIN-STORAGE-DURATION u6)) ;; 6 months
      BONUS-6-MONTHS
      (if (>= duration-blocks (* MIN-STORAGE-DURATION u3)) ;; 3 months
        BONUS-3-MONTHS
        u100 ;; 1.0x multiplier (no bonus)
      )
    )
  )
)

;; Calculate reward amount with overflow protection
(define-private (calculate-reward-amount (size-mb uint) (duration-blocks uint) (bonus-multiplier uint))
  (let (
    ;; Use safe multiplication to prevent overflow
    (base-calc (/ (* size-mb BASE-REWARD-RATE) u1000)) ;; Reduce precision to prevent overflow
    (duration-calc (/ duration-blocks u1000)) ;; Reduce precision
    (base-reward (* base-calc duration-calc))
    (bonus-reward (/ (* base-reward bonus-multiplier) u100))
  )
    ;; Cap the reward at maximum allowed
    (if (> bonus-reward MAX-REWARD-AMOUNT)
      MAX-REWARD-AMOUNT
      bonus-reward
    )
  )
)

;; Read-only functions

;; Get file information
(define-read-only (get-file-info (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

;; Get commitment information
(define-read-only (get-commitment-info (file-id (string-ascii 64)) (commitment-id uint))
  (map-get? commitments { file-id: file-id, commitment-id: commitment-id })
)

;; Get provider balance
(define-read-only (get-provider-balance (provider principal))
  (default-to { balance: u0 } (map-get? provider-balances { provider: provider }))
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

;; Get commitment counter for file
(define-read-only (get-file-commitment-counter (file-id (string-ascii 64)))
  (default-to { counter: u0 } (map-get? file-commitment-counter { file-id: file-id }))
)

;; Check if commitment is ready for verification
(define-read-only (is-commitment-ready-for-verification (file-id (string-ascii 64)) (commitment-id uint))
  (match (map-get? commitments { file-id: file-id, commitment-id: commitment-id })
    commitment-data 
      (and 
        (>= block-height (get end-block commitment-data))
        (not (get is-verified commitment-data))
      )
    false
  )
)

;; Check if reward is claimable
(define-read-only (is-reward-claimable (file-id (string-ascii 64)) (commitment-id uint))
  (match (map-get? commitments { file-id: file-id, commitment-id: commitment-id })
    commitment-data 
      (and 
        (get is-verified commitment-data)
        (not (get is-claimed commitment-data))
      )
    false
  )
)