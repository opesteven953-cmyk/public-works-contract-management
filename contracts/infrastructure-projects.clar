;; Public Works Contract Management
;; Handles contractor qualification, bid processing, project monitoring, and payment coordination

;; Data Variables
(define-data-var department-admin principal tx-sender)
(define-data-var next-contractor-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-bid-id uint u1)
(define-data-var next-payment-id uint u1)

;; Data Maps
(define-map contractors
  { contractor-id: uint }
  {
    name: (string-ascii 100),
    license-number: (string-ascii 50),
    qualifications: (string-ascii 200),
    insurance-valid: bool,
    rating: uint,
    approved: bool,
    registered-at: uint
  }
)

(define-map projects
  { project-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 200),
    budget: uint,
    deadline: uint,
    status: (string-ascii 20),
    managing-officer: principal,
    created-at: uint
  }
)

(define-map bids
  { bid-id: uint }
  {
    project-id: uint,
    contractor-id: uint,
    bid-amount: uint,
    bid-date: uint,
    status: (string-ascii 20)
  }
)

(define-map project-assignments
  { project-id: uint }
  {
    contractor-id: uint,
    assigned-date: uint,
    start-date: (optional uint),
    completion-date: (optional uint)
  }
)

(define-map progress-updates
  { project-id: uint, update-id: uint }
  {
    update-date: uint,
    progress-percent: uint,
    notes: (string-ascii 200)
  }
)

(define-map payments
  { payment-id: uint }
  {
    project-id: uint,
    contractor-id: uint,
    amount: uint,
    payment-date: uint,
    milestone: (string-ascii 50),
    approved: bool
  }
)

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CONTRACTOR-NOT-FOUND (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-BID-NOT-FOUND (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-NOT-APPROVED (err u105))

;; Public Functions
(define-public (register-contractor (name (string-ascii 100)) (license-number (string-ascii 50)) (qualifications (string-ascii 200)) (insurance-valid bool))
  (let ((contractor-id (var-get next-contractor-id)))
    (map-set contractors
      { contractor-id: contractor-id }
      {
        name: name,
        license-number: license-number,
        qualifications: qualifications,
        insurance-valid: insurance-valid,
        rating: u0,
        approved: false,
        registered-at: stacks-block-height
      }
    )
    (var-set next-contractor-id (+ contractor-id u1))
    (ok contractor-id)
  )
)

(define-public (approve-contractor (contractor-id uint) (approved bool) (rating uint))
  (let ((contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-CONTRACTOR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (map-set contractors
      { contractor-id: contractor-id }
      (merge contractor { approved: approved, rating: rating })
    )
    (ok approved)
  )
)

(define-public (create-project (title (string-ascii 100)) (description (string-ascii 200)) (budget uint) (deadline uint))
  (let ((project-id (var-get next-project-id)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (> budget u0) ERR-INVALID-AMOUNT)
    (asserts! (> deadline stacks-block-height) ERR-INVALID-AMOUNT)
    
    (map-set projects
      { project-id: project-id }
      {
        title: title,
        description: description,
        budget: budget,
        deadline: deadline,
        status: "bidding",
        managing-officer: tx-sender,
        created-at: stacks-block-height
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (submit-bid (project-id uint) (contractor-id uint) (bid-amount uint))
  (let ((bid-id (var-get next-bid-id))
        (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (contractor (unwrap! (map-get? contractors { contractor-id: contractor-id }) ERR-CONTRACTOR-NOT-FOUND)))
    (asserts! (is-eq (get status project) "bidding") ERR-INVALID-AMOUNT)
    (asserts! (get approved contractor) ERR-NOT-APPROVED)
    (asserts! (> bid-amount u0) ERR-INVALID-AMOUNT)

    (map-set bids
      { bid-id: bid-id }
      {
        project-id: project-id,
        contractor-id: contractor-id,
        bid-amount: bid-amount,
        bid-date: stacks-block-height,
        status: "submitted"
      }
    )
    
    (var-set next-bid-id (+ bid-id u1))
    (ok bid-id)
  )
)

(define-public (award-project (project-id uint) (bid-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (bid (unwrap! (map-get? bids { bid-id: bid-id }) ERR-BID-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get project-id bid) project-id) ERR-BID-NOT-FOUND)
    (asserts! (is-eq (get status project) "bidding") ERR-INVALID-AMOUNT)

    (map-set project-assignments
      { project-id: project-id }
      {
        contractor-id: (get contractor-id bid),
        assigned-date: stacks-block-height,
        start-date: none,
        completion-date: none
      }
    )

    (map-set projects
      { project-id: project-id }
      (merge project { status: "awarded" })
    )
    (ok true)
  )
)

(define-public (start-project (project-id uint))
  (let ((assignment (unwrap! (map-get? project-assignments { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (map-set project-assignments
      { project-id: project-id }
      (merge assignment { start-date: (some stacks-block-height) })
    )
    (ok true)
  )
)

(define-public (update-progress (project-id uint) (update-id uint) (progress-percent uint) (notes (string-ascii 200)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (<= progress-percent u100) ERR-INVALID-AMOUNT)
    (map-set progress-updates
      { project-id: project-id, update-id: update-id }
      {
        update-date: stacks-block-height,
        progress-percent: progress-percent,
        notes: notes
      }
    )
    (ok true)
  )
)

(define-public (approve-payment (project-id uint) (contractor-id uint) (amount uint) (milestone (string-ascii 50)))
  (let ((payment-id (var-get next-payment-id)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (map-set payments
      { payment-id: payment-id }
      {
        project-id: project-id,
        contractor-id: contractor-id,
        amount: amount,
        payment-date: stacks-block-height,
        milestone: milestone,
        approved: true
      }
    )
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

(define-public (complete-project (project-id uint))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
        (assignment (unwrap! (map-get? project-assignments { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get department-admin)) ERR-NOT-AUTHORIZED)
    (map-set project-assignments
      { project-id: project-id }
      (merge assignment { completion-date: (some stacks-block-height) })
    )
    (map-set projects
      { project-id: project-id }
      (merge project { status: "completed" })
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-contractor (contractor-id uint))
  (map-get? contractors { contractor-id: contractor-id })
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-bid (bid-id uint))
  (map-get? bids { bid-id: bid-id })
)

(define-read-only (get-assignment (project-id uint))
  (map-get? project-assignments { project-id: project-id })
)

(define-read-only (get-progress (project-id uint) (update-id uint))
  (map-get? progress-updates { project-id: project-id, update-id: update-id })
)

(define-read-only (get-payment (payment-id uint))
  (map-get? payments { payment-id: payment-id })
)


;; title: infrastructure-projects
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

