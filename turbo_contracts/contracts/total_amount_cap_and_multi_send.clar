;; Multi-Send with Total Amount Cap Contract
;; This contract allows sending STX to multiple recipients with a total amount limit

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-amount-cap-exceeded (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-transfer-failed (err u104))
(define-constant err-empty-recipients (err u105))
(define-constant err-invalid-recipient (err u106))

;; Data Variables
(define-data-var max-total-amount uint u1000000) ;; Default cap: 1,000,000 microSTX (1,000 STX)
(define-data-var contract-enabled bool true)

;; Data Maps
(define-map user-daily-limits principal uint)
(define-map user-daily-spent principal uint)

;; Read-only functions

;; Get the current maximum total amount cap
(define-read-only (get-max-total-amount)
  (var-get max-total-amount)
)

;; Get contract status
(define-read-only (is-contract-enabled)
  (var-get contract-enabled)
)

;; Get user's daily limit
(define-read-only (get-user-daily-limit (user principal))
  (default-to (var-get max-total-amount) (map-get? user-daily-limits user))
)

;; Get user's daily spent amount
(define-read-only (get-user-daily-spent (user principal))
  (default-to u0 (map-get? user-daily-spent user))
)

;; Calculate total amount from recipients list
(define-read-only (calculate-total-amount (recipients (list 50 {recipient: principal, amount: uint})))
  (fold sum-amounts recipients u0)
)

;; Helper function for folding amounts
(define-private (sum-amounts (recipient-data {recipient: principal, amount: uint}) (total uint))
  (+ total (get amount recipient-data))
)

;; Validate recipient data
(define-private (validate-recipient (recipient-data {recipient: principal, amount: uint}))
  (and 
    (> (get amount recipient-data) u0)
    (not (is-eq (get recipient recipient-data) tx-sender))
  )
)

;; Validate all recipients
(define-private (validate-all-recipients (recipients (list 50 {recipient: principal, amount: uint})))
  (fold validate-recipients-fold recipients true)
)

;; Helper function for validating recipients
(define-private (validate-recipients-fold (recipient-data {recipient: principal, amount: uint}) (valid bool))
  (and valid (validate-recipient recipient-data))
)

;; Private functions

;; Execute single transfer
(define-private (execute-transfer (recipient-data {recipient: principal, amount: uint}) (success bool))
  (if success
    (match (stx-transfer? (get amount recipient-data) tx-sender (get recipient recipient-data))
      transfer-success true
      transfer-error false
    )
    false
  )
)

;; Update user's daily spent amount
(define-private (update-daily-spent (user principal) (amount uint))
  (let ((current-spent (get-user-daily-spent user)))
    (map-set user-daily-spent user (+ current-spent amount))
  )
)

;; Public functions

;; Set maximum total amount (owner only)
(define-public (set-max-total-amount (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-amount u0) err-invalid-amount)
    (var-set max-total-amount new-amount)
    (ok true)
  )
)

;; Enable/disable contract (owner only)
(define-public (set-contract-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-enabled enabled)
    (ok true)
  )
)

;; Set user-specific daily limit (owner only)
(define-public (set-user-daily-limit (user principal) (limit uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> limit u0) err-invalid-amount)
    (map-set user-daily-limits user limit)
    (ok true)
  )
)

;; Reset user's daily spent amount (owner only - for testing or manual reset)
(define-public (reset-user-daily-spent (user principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete user-daily-spent user)
    (ok true)
  )
)

;; Main multi-send function
(define-public (multi-send (recipients (list 50 {recipient: principal, amount: uint})))
  (let (
    (total-amount (calculate-total-amount recipients))
    (user-limit (get-user-daily-limit tx-sender))
    (user-spent (get-user-daily-spent tx-sender))
    (sender-balance (stx-get-balance tx-sender))
  )
    ;; Validate contract is enabled
    (asserts! (var-get contract-enabled) err-invalid-amount)
    
    ;; Validate recipients list is not empty
    (asserts! (> (len recipients) u0) err-empty-recipients)
    
    ;; Validate all recipients
    (asserts! (validate-all-recipients recipients) err-invalid-recipient)
    
    ;; Validate total amount is positive
    (asserts! (> total-amount u0) err-invalid-amount)
    
    ;; Check if total amount exceeds the cap
    (asserts! (<= total-amount user-limit) err-amount-cap-exceeded)
    
    ;; Check if user hasn't exceeded daily spending limit
    (asserts! (<= (+ user-spent total-amount) user-limit) err-amount-cap-exceeded)
    
    ;; Check if sender has sufficient balance
    (asserts! (>= sender-balance total-amount) err-insufficient-balance)
    
    ;; Execute all transfers
    (asserts! (fold execute-transfer recipients true) err-transfer-failed)
    
    ;; Update user's daily spent amount
    (update-daily-spent tx-sender total-amount)
    
    (ok {
      total-sent: total-amount,
      recipients-count: (len recipients),
      remaining-daily-limit: (- user-limit (+ user-spent total-amount))
    })
  )
)

;; Emergency function to withdraw contract balance (owner only)
(define-public (emergency-withdraw)
  (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> contract-balance u0) err-insufficient-balance)
    (as-contract (stx-transfer? contract-balance tx-sender contract-owner))
  )
)