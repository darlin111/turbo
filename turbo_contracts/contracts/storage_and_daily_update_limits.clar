;; Enhanced Daily Update Limit Storage Contract
;; Advanced storage system with comprehensive features and governance

;; Constants
(define-constant ERR_DAILY_LIMIT_EXCEEDED (err u1001))
(define-constant ERR_UNAUTHORIZED (err u1002))
(define-constant ERR_VALUE_NOT_FOUND (err u1003))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u1004))
(define-constant ERR_CONTRACT_PAUSED (err u1005))
(define-constant ERR_INVALID_VALUE (err u1006))
(define-constant ERR_BACKUP_NOT_FOUND (err u1007))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u1008))
(define-constant ERR_INVALID_TIMEFRAME (err u1009))
(define-constant ERR_BATCH_TOO_LARGE (err u1010))

;; Configuration Constants
(define-constant DEFAULT_DAILY_LIMIT u5)
(define-constant MAX_DAILY_LIMIT u100)
(define-constant BLOCKS_PER_DAY u144)
(define-constant UPDATE_FEE u1000) ;; 0.001 STX in microSTX
(define-constant PREMIUM_MULTIPLIER u10)
(define-constant MAX_BATCH_SIZE u10)
(define-constant MAX_BACKUP_HISTORY u30)

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var total-users uint u0)
(define-data-var total-updates uint u0)
(define-data-var fee-enabled bool false)
(define-data-var emergency-mode bool false)

;; Data Maps
;; Enhanced user storage with metadata
(define-map user-storage 
  { user: principal }
  { 
    value: uint,
    last-updated: uint,
    total-updates: uint,
    created-at: uint,
    is-premium: bool,
    subscription-expires: uint
  }
)

;; Track daily update counts with enhanced tracking
(define-map daily-updates
  { user: principal, day: uint }
  { 
    count: uint,
    last-update-time: uint
  }
)

;; Custom daily limits and user settings
(define-map user-settings
  { user: principal }
  { 
    daily-limit: uint,
    auto-backup: bool,
    notifications: bool,
    is-frozen: bool
  }
)

;; Value history and backups
(define-map value-history
  { user: principal, timestamp: uint }
  { 
    value: uint,
    update-type: (string-ascii 20)
  }
)

;; Statistics and analytics
(define-map daily-stats
  { day: uint }
  {
    total-updates: uint,
    unique-users: uint,
    premium-updates: uint
  }
)

;; Access control and permissions
(define-map user-permissions
  { user: principal }
  {
    can-read: bool,
    can-write: bool,
    can-backup: bool,
    granted-by: principal,
    granted-at: uint
  }
)

;; Shared storage (users can share access to their data)
(define-map shared-access
  { owner: principal, accessor: principal }
  {
    read-access: bool,
    write-access: bool,
    expires-at: uint
  }
)

;; Event logging
(define-map event-log
  { event-id: uint }
  {
    user: principal,
    action: (string-ascii 50),
    timestamp: uint,
    details: (string-ascii 100)
  }
)

(define-data-var next-event-id uint u1)

;; Private Functions

(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-contract-active)
  (and (not (var-get contract-paused)) (not (var-get emergency-mode)))
)

(define-private (get-current-day)
  (/ block-height BLOCKS_PER_DAY)
)

(define-private (get-user-daily-limit (user principal))
  (let (
    (user-setting (map-get? user-settings { user: user }))
    (is-premium (is-user-premium user))
  )
    (match user-setting
      settings (let ((custom-limit (get daily-limit settings)))
                 (if (> custom-limit u0)
                   (if is-premium (* custom-limit PREMIUM_MULTIPLIER) custom-limit)
                   (if is-premium (* DEFAULT_DAILY_LIMIT PREMIUM_MULTIPLIER) DEFAULT_DAILY_LIMIT)))
      (if is-premium (* DEFAULT_DAILY_LIMIT PREMIUM_MULTIPLIER) DEFAULT_DAILY_LIMIT)
    )
  )
)

(define-private (get-daily-update-count (user principal))
  (default-to u0
    (get count (map-get? daily-updates { 
      user: user, 
      day: (get-current-day) 
    }))
  )
)

(define-private (is-user-premium (user principal))
  (match (map-get? user-storage { user: user })
    storage-data (and 
                  (get is-premium storage-data)
                  (> (get subscription-expires storage-data) block-height))
    false
  )
)

(define-private (can-user-update (user principal))
  (let (
    (daily-limit (get-user-daily-limit user))
    (current-count (get-daily-update-count user))
    (user-settings-data (map-get? user-settings { user: user }))
  )
    (and
      (< current-count daily-limit)
      (match user-settings-data
        settings (not (get is-frozen settings))
        true)
    )
  )
)

(define-private (increment-daily-count (user principal))
  (let (
    (current-day (get-current-day))
    (current-count (get-daily-update-count user))
  )
    (map-set daily-updates
      { user: user, day: current-day }
      { 
        count: (+ current-count u1),
        last-update-time: block-height
      }
    )
  )
)

(define-private (log-event (user principal) (action (string-ascii 50)) (details (string-ascii 100)))
  (let (
    (event-id (var-get next-event-id))
  )
    (map-set event-log
      { event-id: event-id }
      {
        user: user,
        action: action,
        timestamp: block-height,
        details: details
      }
    )
    (var-set next-event-id (+ event-id u1))
    event-id
  )
)

(define-private (create-backup (user principal) (value uint) (update-type (string-ascii 20)))
  (begin
    (map-set value-history
      { user: user, timestamp: block-height }
      { 
        value: value,
        update-type: update-type
      }
    )
    true
  )
)

(define-private (update-daily-stats)
  (let (
    (current-day (get-current-day))
    (current-stats (default-to 
                    { total-updates: u0, unique-users: u0, premium-updates: u0 }
                    (map-get? daily-stats { day: current-day })))
  )
    (map-set daily-stats
      { day: current-day }
      (merge current-stats { total-updates: (+ (get total-updates current-stats) u1) })
    )
  )
)

;; Public Functions

;; Enhanced value update with fees and premium features
(define-public (update-value (new-value uint))
  (let (
    (user tx-sender)
  )
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    (asserts! (can-user-update user) ERR_DAILY_LIMIT_EXCEEDED)
    (asserts! (> new-value u0) ERR_INVALID_VALUE)
    
    ;; Handle fee if enabled
    (if (var-get fee-enabled)
      (asserts! (>= (stx-get-balance tx-sender) UPDATE_FEE) ERR_INSUFFICIENT_PAYMENT)
      true
    )
    
    (let (
      (existing-data (map-get? user-storage { user: user }))
    )
      ;; Create or update user storage
      (map-set user-storage
        { user: user }
        (match existing-data
          data (merge data { 
                  value: new-value,
                  last-updated: block-height,
                  total-updates: (+ (get total-updates data) u1)
                })
          { 
            value: new-value,
            last-updated: block-height,
            total-updates: u1,
            created-at: block-height,
            is-premium: false,
            subscription-expires: u0
          }
        )
      )
      
      ;; Increment counters
      (increment-daily-count user)
      (var-set total-updates (+ (var-get total-updates) u1))
      
      ;; Create backup if auto-backup is enabled
      (match (map-get? user-settings { user: user })
        settings (if (get auto-backup settings)
                   (create-backup user new-value "auto-update")
                   true)
        true
      )
      
      ;; Update statistics
      (update-daily-stats)
      
      ;; Log event
      (log-event user "update-value" "Value updated successfully")
      
      ;; Transfer fee if enabled
      (if (var-get fee-enabled)
        (unwrap-panic (stx-transfer? UPDATE_FEE tx-sender (var-get contract-owner)))
        true
      )
      
      (ok true)
    )
  )
)

;; Batch update multiple values
(define-public (batch-update-values (values (list 10 uint)))
  (let (
    (user tx-sender)
    (batch-size (len values))
  )
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    (asserts! (<= batch-size MAX_BATCH_SIZE) ERR_BATCH_TOO_LARGE)
    (asserts! (>= (get-user-daily-limit user) (+ (get-daily-update-count user) batch-size)) ERR_DAILY_LIMIT_EXCEEDED)
    
    (fold batch-update-helper values (ok u0))
  )
)

(define-private (batch-update-helper (value uint) (prev-result (response uint uint)))
  (match prev-result
    success (match (update-value value)
              update-success (ok (+ success u1))
              update-error (err update-error))
    error (err error)
  )
)

;; Premium subscription management
(define-public (upgrade-to-premium (duration uint))
  (let (
    (user tx-sender)
    (premium-fee (* UPDATE_FEE PREMIUM_MULTIPLIER duration))
  )
    (asserts! (>= (stx-get-balance tx-sender) premium-fee) ERR_INSUFFICIENT_PAYMENT)
    
    (match (map-get? user-storage { user: user })
      existing-data 
      (begin
        (map-set user-storage
          { user: user }
          (merge existing-data {
            is-premium: true,
            subscription-expires: (+ block-height (* duration BLOCKS_PER_DAY))
          })
        )
        (unwrap-panic (stx-transfer? premium-fee tx-sender (var-get contract-owner)))
        (log-event user "upgrade-premium" "Premium subscription activated")
        (ok true)
      )
      ERR_VALUE_NOT_FOUND
    )
  )
)

;; User settings management
(define-public (update-user-settings (daily-limit uint) (auto-backup bool) (notifications bool))
  (let (
    (user tx-sender)
  )
    (asserts! (<= daily-limit MAX_DAILY_LIMIT) ERR_INVALID_VALUE)
    
    (map-set user-settings
      { user: user }
      {
        daily-limit: daily-limit,
        auto-backup: auto-backup,
        notifications: notifications,
        is-frozen: false
      }
    )
    (log-event user "update-settings" "User settings updated")
    (ok true)
  )
)

;; Manual backup creation
(define-public (create-manual-backup (description (string-ascii 20)))
  (let (
    (user tx-sender)
  )
    (match (map-get? user-storage { user: user })
      storage-data
      (begin
        (create-backup user (get value storage-data) description)
        (log-event user "manual-backup" "Manual backup created")
        (ok true)
      )
      ERR_VALUE_NOT_FOUND
    )
  )
)

;; Restore from backup
(define-public (restore-from-backup (timestamp uint))
  (let (
    (user tx-sender)
  )
    (match (map-get? value-history { user: user, timestamp: timestamp })
      backup-data
      (begin
        (asserts! (can-user-update user) ERR_DAILY_LIMIT_EXCEEDED)
        (update-value (get value backup-data))
      )
      ERR_BACKUP_NOT_FOUND
    )
  )
)

;; Share access to data
(define-public (grant-access (accessor principal) (read-access bool) (write-access bool) (duration uint))
  (let (
    (user tx-sender)
    (expires-at (+ block-height (* duration BLOCKS_PER_DAY)))
  )
    (map-set shared-access
      { owner: user, accessor: accessor }
      {
        read-access: read-access,
        write-access: write-access,
        expires-at: expires-at
      }
    )
    (log-event user "grant-access" "Access granted to another user")
    (ok true)
  )
)

;; Revoke shared access
(define-public (revoke-access (accessor principal))
  (let (
    (user tx-sender)
  )
    (map-delete shared-access { owner: user, accessor: accessor })
    (log-event user "revoke-access" "Access revoked")
    (ok true)
  )
)

;; Read Functions

(define-read-only (get-value (user principal))
  (match (map-get? user-storage { user: user })
    storage-data (ok (get value storage-data))
    ERR_VALUE_NOT_FOUND
  )
)

(define-read-only (get-update-count (user principal))
  (ok (get-daily-update-count user))
)

(define-read-only (get-comprehensive-info (user principal))
  (match (map-get? user-storage { user: user })
    storage-data 
    (let (
      (user-settings-data (map-get? user-settings { user: user }))
      (daily-count (get-daily-update-count user))
      (daily-limit (get-user-daily-limit user))
    )
      (ok {
        value: (get value storage-data),
        last-updated: (get last-updated storage-data),
        total-updates: (get total-updates storage-data),
        created-at: (get created-at storage-data),
        is-premium: (get is-premium storage-data),
        subscription-expires: (get subscription-expires storage-data),
        daily-limit: daily-limit,
        today-count: daily-count,
        remaining-updates: (if (> daily-limit daily-count) (- daily-limit daily-count) u0),
        settings: user-settings-data
      })
    )
    ERR_VALUE_NOT_FOUND
  )
)

(define-read-only (get-user-backups (user principal) (limit uint))
  (ok "Backup history would be returned here - implementation depends on specific requirements")
)

(define-read-only (get-shared-data (owner principal))
  (let (
    (access-info (map-get? shared-access { owner: owner, accessor: tx-sender }))
  )
    (match access-info
      access 
      (if (and (get read-access access) (> (get expires-at access) block-height))
        (get-value owner)
        ERR_UNAUTHORIZED)
      ERR_UNAUTHORIZED
    )
  )
)

(define-read-only (get-contract-stats)
  (ok {
    total-users: (var-get total-users),
    total-updates: (var-get total-updates),
    contract-paused: (var-get contract-paused),
    fee-enabled: (var-get fee-enabled),
    current-day: (get-current-day),
    blocks-per-day: BLOCKS_PER_DAY
  })
)

(define-read-only (get-daily-statistics (day uint))
  (ok (map-get? daily-stats { day: day }))
)

;; Admin Functions

(define-public (pause-contract)
  (if (is-contract-owner)
    (begin
      (var-set contract-paused true)
      (log-event (var-get contract-owner) "pause-contract" "Contract paused by admin")
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (resume-contract)
  (if (is-contract-owner)
    (begin
      (var-set contract-paused false)
      (log-event (var-get contract-owner) "resume-contract" "Contract resumed by admin")
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (toggle-fees (enabled bool))
  (if (is-contract-owner)
    (begin
      (var-set fee-enabled enabled)
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (freeze-user (user principal))
  (if (is-contract-owner)
    (begin
      (map-set user-settings
        { user: user }
        (match (map-get? user-settings { user: user })
          existing (merge existing { is-frozen: true })
          { daily-limit: u0, auto-backup: false, notifications: false, is-frozen: true }
        )
      )
      (log-event (var-get contract-owner) "freeze-user" "User frozen by admin")
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (emergency-stop)
  (if (is-contract-owner)
    (begin
      (var-set emergency-mode true)
      (var-set contract-paused true)
      (log-event (var-get contract-owner) "emergency-stop" "Emergency mode activated")
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

(define-public (set-contract-owner (new-owner principal))
  (if (is-contract-owner)
    (begin
      (var-set contract-owner new-owner)
      (log-event new-owner "ownership-transfer" "Contract ownership transferred")
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

;; Utility Functions

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-read-only (get-event-log (event-id uint))
  (ok (map-get? event-log { event-id: event-id }))
)