(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-ACTIVE-ESCROW (err u102))
(define-constant ERR-ALREADY-APPROVED (err u103))
(define-constant ERR-NOT-COMPLETED (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-DISPUTE-ALREADY-RAISED (err u106))
(define-constant ERR-NO-DISPUTE (err u107))
(define-constant ERR-DISPUTE-RESOLVED (err u108))

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
    }
)

(define-data-var escrow-counter uint u0)

(define-read-only (get-escrow (escrow-id uint))
    (map-get? escrows { escrow-id: escrow-id })
)

(define-read-only (get-escrow-count)
    (var-get escrow-counter)
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

(define-public (raise-dispute (escrow-id uint) (arbitrator principal))
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

(define-public (resolve-dispute (escrow-id uint) (award-to-client bool))
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
