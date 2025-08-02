(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-ACTIVE-ESCROW (err u102))
(define-constant ERR-ALREADY-APPROVED (err u103))
(define-constant ERR-NOT-COMPLETED (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-DISPUTE-ALREADY-RAISED (err u106))
(define-constant ERR-NO-DISPUTE (err u107))
(define-constant ERR-DISPUTE-RESOLVED (err u108))
(define-constant ERR-MILESTONE-NOT-FOUND (err u109))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u110))
(define-constant ERR-INVALID-MILESTONE-PERCENTAGE (err u111))
(define-constant ERR-MILESTONES-EXCEED-100-PERCENT (err u112))

(define-data-var contract-owner principal tx-sender)

(define-map escrows
    { escrow-id: uint }
    {
        client: principal,
        freelancer: principal,
        amount: uint,
        client-approved: bool,
        freelancer-approved: bool,
        is-active: bool,
        completed: bool,
        dispute-raised: bool,
        dispute-resolved: bool,
        arbitrator: (optional principal),
        milestones-enabled: bool,
        total-milestones: uint,
        completed-milestones: uint,
        released-amount: uint,
    }
)

(define-data-var escrow-counter uint u0)

(define-map milestones
    {
        escrow-id: uint,
        milestone-id: uint,
    }
    {
        description: (string-ascii 256),
        percentage: uint,
        completed: bool,
        client-approved: bool,
    }
)

(define-read-only (get-escrow (escrow-id uint))
    (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-escrow-count)
    (var-get escrow-counter)
)

(define-read-only (get-milestone
        (escrow-id uint)
        (milestone-id uint)
    )
    (map-get? milestones {
        escrow-id: escrow-id,
        milestone-id: milestone-id,
    })
)

(define-public (create-escrow
        (freelancer principal)
        (amount uint)
    )
    (let ((escrow-id (+ (var-get escrow-counter) u1)))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set escrows { escrow-id: escrow-id } {
            client: tx-sender,
            freelancer: freelancer,
            amount: amount,
            client-approved: false,
            freelancer-approved: false,
            is-active: true,
            completed: false,
            dispute-raised: false,
            dispute-resolved: false,
            arbitrator: none,
            milestones-enabled: false,
            total-milestones: u0,
            completed-milestones: u0,
            released-amount: u0,
        })
        (var-set escrow-counter escrow-id)
        (ok escrow-id)
    )
)

(define-public (approve-work (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (not (get client-approved escrow)) ERR-ALREADY-APPROVED)
        (map-set escrows { escrow-id: escrow-id }
            (merge escrow { client-approved: true })
        )
        (ok true)
    )
)

(define-public (approve-completion (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get freelancer escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (not (get freelancer-approved escrow)) ERR-ALREADY-APPROVED)
        (map-set escrows { escrow-id: escrow-id }
            (merge escrow { freelancer-approved: true })
        )
        (ok true)
    )
)

(define-public (release-payment (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts!
            (and (get client-approved escrow) (get freelancer-approved escrow))
            ERR-NOT-COMPLETED
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get freelancer escrow))))
        (map-set escrows { escrow-id: escrow-id }
            (merge escrow {
                is-active: false,
                completed: true,
            })
        )
        (ok true)
    )
)

(define-public (raise-dispute
        (escrow-id uint)
        (arbitrator principal)
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (not (get dispute-raised escrow)) ERR-DISPUTE-ALREADY-RAISED)
        (asserts!
            (or (is-eq (get client escrow) tx-sender) (is-eq (get freelancer escrow) tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (map-set escrows { escrow-id: escrow-id }
            (merge escrow {
                dispute-raised: true,
                arbitrator: (some arbitrator),
            })
        )
        (ok true)
    )
)

(define-public (resolve-dispute
        (escrow-id uint)
        (award-to-client bool)
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (get dispute-raised escrow) ERR-NO-DISPUTE)
        (asserts! (not (get dispute-resolved escrow)) ERR-DISPUTE-RESOLVED)
        (asserts!
            (is-eq (unwrap! (get arbitrator escrow) ERR-NO-DISPUTE) tx-sender)
            ERR-NOT-AUTHORIZED
        )
        (if award-to-client
            (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get client escrow))))
            (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get freelancer escrow))))
        )
        (map-set escrows { escrow-id: escrow-id }
            (merge escrow {
                is-active: false,
                completed: true,
                dispute-resolved: true,
            })
        )
        (ok true)
    )
)

(define-read-only (get-dispute-status (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (ok {
            dispute-raised: (get dispute-raised escrow),
            dispute-resolved: (get dispute-resolved escrow),
            arbitrator: (get arbitrator escrow),
        })
    )
)

(define-public (add-milestone
        (escrow-id uint)
        (milestone-id uint)
        (description (string-ascii 256))
        (percentage uint)
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (and (> percentage u0) (<= percentage u100))
            ERR-INVALID-MILESTONE-PERCENTAGE
        )
        (asserts! (is-none (get-milestone escrow-id milestone-id))
            ERR-ALREADY-EXISTS
        )
        (let ((total-percentage (+ (calculate-total-percentage escrow-id) percentage)))
            (asserts! (<= total-percentage u100)
                ERR-MILESTONES-EXCEED-100-PERCENT
            )
            (map-set milestones {
                escrow-id: escrow-id,
                milestone-id: milestone-id,
            } {
                description: description,
                percentage: percentage,
                completed: false,
                client-approved: false,
            })
            (map-set escrows { escrow-id: escrow-id }
                (merge escrow {
                    milestones-enabled: true,
                    total-milestones: (+ (get total-milestones escrow) u1),
                })
            )
            (ok true)
        )
    )
)

(define-public (complete-milestone
        (escrow-id uint)
        (milestone-id uint)
    )
    (let (
            (escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW))
            (milestone (unwrap! (get-milestone escrow-id milestone-id)
                ERR-MILESTONE-NOT-FOUND
            ))
        )
        (asserts! (is-eq (get freelancer escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
        (map-set milestones {
            escrow-id: escrow-id,
            milestone-id: milestone-id,
        }
            (merge milestone { completed: true })
        )
        (ok true)
    )
)

(define-public (approve-milestone
        (escrow-id uint)
        (milestone-id uint)
    )
    (let (
            (escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW))
            (milestone (unwrap! (get-milestone escrow-id milestone-id)
                ERR-MILESTONE-NOT-FOUND
            ))
        )
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (get completed milestone) ERR-NOT-COMPLETED)
        (asserts! (not (get client-approved milestone)) ERR-ALREADY-APPROVED)
        (let ((payment-amount (/ (* (get amount escrow) (get percentage milestone)) u100)))
            (try! (as-contract (stx-transfer? payment-amount tx-sender (get freelancer escrow))))
            (map-set milestones {
                escrow-id: escrow-id,
                milestone-id: milestone-id,
            }
                (merge milestone { client-approved: true })
            )
            (map-set escrows { escrow-id: escrow-id }
                (merge escrow {
                    completed-milestones: (+ (get completed-milestones escrow) u1),
                    released-amount: (+ (get released-amount escrow) payment-amount),
                })
            )
            (ok true)
        )
    )
)

(define-private (calculate-total-percentage (escrow-id uint))
    (fold + (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) u0)
)

(define-read-only (get-milestone-progress (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (ok {
            milestones-enabled: (get milestones-enabled escrow),
            total-milestones: (get total-milestones escrow),
            completed-milestones: (get completed-milestones escrow),
            released-amount: (get released-amount escrow),
            remaining-amount: (- (get amount escrow) (get released-amount escrow)),
        })
    )
)
