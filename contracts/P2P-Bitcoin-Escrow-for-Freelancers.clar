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
(define-constant ERR-DEADLINE-PASSED (err u113))
(define-constant ERR-INVALID-DEADLINE (err u114))
(define-constant ERR-DEADLINE-NOT-PASSED (err u115))
(define-constant ERR-INVALID-RATING (err u116))
(define-constant ERR-RATING-ALREADY-EXISTS (err u117))
(define-constant ERR-CANNOT-RATE-SELF (err u118))
(define-constant ERR-INSUFFICIENT-INTERACTIONS (err u119))

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
        deadline: uint,
        created-at: uint,
    }
)

(define-data-var escrow-counter uint u0)

;; Reputation System Data Structures
(define-map user-reputation
    { user: principal }
    {
        total-escrows-as-client: uint,
        total-escrows-as-freelancer: uint,
        completed-escrows-as-client: uint,
        completed-escrows-as-freelancer: uint,
        total-rating-points: uint,
        total-ratings-received: uint,
        reputation-score: uint,
        last-updated: uint,
    }
)

(define-map user-ratings
    {
        rater: principal,
        rated: principal,
        escrow-id: uint,
    }
    {
        rating: uint,
        comment: (string-ascii 500),
        timestamp: uint,
    }
)

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

;; Reputation System Read-Only Functions
(define-read-only (get-user-reputation (user principal))
    (default-to {
        total-escrows-as-client: u0,
        total-escrows-as-freelancer: u0,
        completed-escrows-as-client: u0,
        completed-escrows-as-freelancer: u0,
        total-rating-points: u0,
        total-ratings-received: u0,
        reputation-score: u0,
        last-updated: u0,
    }
        (map-get? user-reputation { user: user })
    )
)

(define-read-only (get-user-rating
        (rater principal)
        (rated principal)
        (escrow-id uint)
    )
    (map-get? user-ratings {
        rater: rater,
        rated: rated,
        escrow-id: escrow-id,
    })
)

(define-read-only (calculate-reputation-score
        (total-rating-points uint)
        (total-ratings-received uint)
        (completion-rate uint)
    )
    (if (is-eq total-ratings-received u0)
        u0
        (let (
                (average-rating (/ total-rating-points total-ratings-received))
                (weighted-score (+ (* average-rating u70) (* completion-rate u30)))
            )
            (/ weighted-score u100)
        )
    )
)

(define-read-only (get-user-completion-rate (user principal))
    (let ((reputation (get-user-reputation user)))
        (let (
                (total-as-client (get total-escrows-as-client reputation))
                (total-as-freelancer (get total-escrows-as-freelancer reputation))
                (completed-as-client (get completed-escrows-as-client reputation))
                (completed-as-freelancer (get completed-escrows-as-freelancer reputation))
                (total-escrows (+ total-as-client total-as-freelancer))
                (total-completed (+ completed-as-client completed-as-freelancer))
            )
            (if (is-eq total-escrows u0)
                u0
                (/ (* total-completed u100) total-escrows)
            )
        )
    )
)

(define-public (create-escrow
        (freelancer principal)
        (amount uint)
        (deadline-blocks uint)
    )
    (let (
            (escrow-id (+ (var-get escrow-counter) u1))
            (current-block stacks-block-height)
            (deadline (+ current-block deadline-blocks))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> deadline-blocks u0) ERR-INVALID-DEADLINE)
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
            deadline: deadline,
            created-at: current-block,
        })
        (var-set escrow-counter escrow-id)
        (begin
            ;; Update reputation counters for both client and freelancer
            (increment-escrow-counter tx-sender true false)
            (increment-escrow-counter freelancer false false)
            (ok escrow-id)
        )
    )
)

(define-public (approve-work (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (< stacks-block-height (get deadline escrow))
            ERR-DEADLINE-PASSED
        )
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
        (asserts! (< stacks-block-height (get deadline escrow))
            ERR-DEADLINE-PASSED
        )
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
        (begin
            ;; Update completion statistics for both parties
            (increment-escrow-counter (get client escrow) true true)
            (increment-escrow-counter (get freelancer escrow) false true)
            (ok true)
        )
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
        (begin
            ;; Update completion statistics - mark as completed for dispute resolution
            (increment-escrow-counter (get client escrow) true true)
            (increment-escrow-counter (get freelancer escrow) false true)
            (ok true)
        )
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

(define-read-only (get-deadline-info (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (ok {
            deadline: (get deadline escrow),
            created-at: (get created-at escrow),
            blocks-remaining: (if (> (get deadline escrow) stacks-block-height)
                (- (get deadline escrow) stacks-block-height)
                u0
            ),
            is-expired: (>= stacks-block-height (get deadline escrow)),
        })
    )
)

(define-read-only (is-escrow-expired (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (ok (>= stacks-block-height (get deadline escrow)))
    )
)

(define-public (claim-timeout-refund (escrow-id uint))
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (>= stacks-block-height (get deadline escrow))
            ERR-DEADLINE-NOT-PASSED
        )
        (asserts!
            (not (and (get client-approved escrow) (get freelancer-approved escrow)))
            ERR-NOT-COMPLETED
        )
        (let ((refund-amount (- (get amount escrow) (get released-amount escrow))))
            (try! (as-contract (stx-transfer? refund-amount tx-sender (get client escrow))))
            (map-set escrows { escrow-id: escrow-id }
                (merge escrow {
                    is-active: false,
                    completed: true,
                })
            )
            (begin
                ;; Update completion statistics - timeout doesn't count as successful completion
                ;; Only client gets completion credit for claiming refund
                (increment-escrow-counter (get client escrow) true true)
                (ok refund-amount)
            )
        )
    )
)

(define-public (extend-deadline
        (escrow-id uint)
        (additional-blocks uint)
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (> additional-blocks u0) ERR-INVALID-DEADLINE)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts!
            (or (is-eq (get client escrow) tx-sender) (is-eq (get freelancer escrow) tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (let ((new-deadline (+ (get deadline escrow) additional-blocks)))
            (map-set escrows { escrow-id: escrow-id }
                (merge escrow { deadline: new-deadline })
            )
            (ok new-deadline)
        )
    )
)

;; Reputation System Public Functions
(define-public (rate-user
        (escrow-id uint)
        (rated-user principal)
        (rating uint)
        (comment (string-ascii 500))
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (get completed escrow) ERR-NOT-COMPLETED)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (not (is-eq tx-sender rated-user)) ERR-CANNOT-RATE-SELF)
        (asserts!
            (or (is-eq (get client escrow) tx-sender) (is-eq (get freelancer escrow) tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (asserts!
            (or (is-eq (get client escrow) rated-user) (is-eq (get freelancer escrow) rated-user))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (is-none (get-user-rating tx-sender rated-user escrow-id))
            ERR-RATING-ALREADY-EXISTS
        )
        ;; Store the rating
        (map-set user-ratings {
            rater: tx-sender,
            rated: rated-user,
            escrow-id: escrow-id,
        } {
            rating: rating,
            comment: comment,
            timestamp: stacks-block-height,
        })
        ;; Update rated user's reputation
        (update-user-reputation rated-user rating)
        (ok true)
    )
)

(define-private (update-user-reputation
        (user principal)
        (new-rating uint)
    )
    (let ((current-rep (get-user-reputation user)))
        (let (
                (new-total-points (+ (get total-rating-points current-rep) new-rating))
                (new-total-ratings (+ (get total-ratings-received current-rep) u1))
                (completion-rate (get-user-completion-rate user))
                (new-score (calculate-reputation-score new-total-points new-total-ratings
                    completion-rate
                ))
            )
            (map-set user-reputation { user: user }
                (merge current-rep {
                    total-rating-points: new-total-points,
                    total-ratings-received: new-total-ratings,
                    reputation-score: new-score,
                    last-updated: stacks-block-height,
                })
            )
        )
    )
)

(define-private (increment-escrow-counter
        (user principal)
        (as-client bool)
        (completed bool)
    )
    (let ((current-rep (get-user-reputation user)))
        (if as-client
            (map-set user-reputation { user: user }
                (merge current-rep {
                    total-escrows-as-client: (+ (get total-escrows-as-client current-rep) u1),
                    completed-escrows-as-client: (if completed
                        (+ (get completed-escrows-as-client current-rep) u1)
                        (get completed-escrows-as-client current-rep)
                    ),
                    last-updated: stacks-block-height,
                })
            )
            (map-set user-reputation { user: user }
                (merge current-rep {
                    total-escrows-as-freelancer: (+ (get total-escrows-as-freelancer current-rep) u1),
                    completed-escrows-as-freelancer: (if completed
                        (+ (get completed-escrows-as-freelancer current-rep) u1)
                        (get completed-escrows-as-freelancer current-rep)
                    ),
                    last-updated: stacks-block-height,
                })
            )
        )
    )
)

(define-public (top-up-escrow
        (escrow-id uint)
        (additional uint)
    )
    (let ((escrow (unwrap! (get-escrow escrow-id) ERR-NO-ACTIVE-ESCROW)))
        (asserts! (is-eq (get client escrow) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active escrow) ERR-NO-ACTIVE-ESCROW)
        (asserts! (< stacks-block-height (get deadline escrow))
            ERR-DEADLINE-PASSED
        )
        (asserts! (> additional u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? additional tx-sender (as-contract tx-sender)))
        (let ((new-amount (+ (get amount escrow) additional)))
            (map-set escrows { escrow-id: escrow-id }
                (merge escrow { amount: new-amount })
            )
            (ok new-amount)
        )
    )
)
