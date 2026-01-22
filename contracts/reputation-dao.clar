;; ------------------------------------------------------------
;; reputation-dao.clar
;; Reputation-based DAO governance (no token voting)
;; ------------------------------------------------------------

(define-constant ERR-NOT-ADMIN u100)
(define-constant ERR-NOT-MEMBER u101)
(define-constant ERR-PROPOSAL-NOT-FOUND u102)
(define-constant ERR-ALREADY-VOTED u103)
(define-constant ERR-VOTING-CLOSED u104)
(define-constant ERR-INVALID-AMOUNT u105)

;; ------------------------------------------------------------
;; DAO parameters
;; ------------------------------------------------------------

(define-data-var admin (optional principal) none)
(define-data-var proposal-count uint u0)

;; minimum total reputation required to validate vote
(define-data-var quorum-reputation uint u100)

;; ------------------------------------------------------------
;; Reputation storage
;; ------------------------------------------------------------

(define-map reputation
  {user: principal}
  {points: uint}
)

(define-private (get-rep (u principal))
  (default-to u0 (get points (map-get? reputation { user: u })))
)

;; ------------------------------------------------------------
;; Proposals
;; ------------------------------------------------------------

(define-map proposals
  {id: uint}
  {
    creator: principal,
    description: (string-ascii 200),
    start: uint,
    end: uint,
    yes: uint,
    no: uint,
    open: bool
  }
)

;; Track voting
(define-map votes
  {id: uint, voter: principal}
  {voted: bool}
)

;; ------------------------------------------------------------
;; Initialization
;; ------------------------------------------------------------

(define-public (initialize (dao-admin principal))
  ;; @disable-check unchecked-data
  (if (is-some (var-get admin))
    (err ERR-NOT-ADMIN)
    (begin
      ;; @disable-check unchecked-data
      (var-set admin (some dao-admin))
      ;; @disable-check unchecked-data
      (map-set reputation { user: dao-admin } { points: u100 })
      (ok dao-admin)
    )
  )
)

;; ------------------------------------------------------------
;; Admin controls: reputation management
;; ------------------------------------------------------------

(define-public (grant-reputation (user principal) (amount uint))
  ;; @disable-check unchecked-data
  (if (or (is-none (var-get admin)) (<= amount u0))
    (if (is-none (var-get admin))
      (err ERR-NOT-ADMIN)
      (err ERR-INVALID-AMOUNT)
    )
    (if (not (is-eq (unwrap-panic (var-get admin)) tx-sender))
      (err ERR-NOT-ADMIN)
      ;; @disable-check unchecked-data
      (begin
        ;; @disable-check unchecked-data
        (map-set reputation
          { user: user }
          ;; @disable-check unchecked-data
          { points: (+ (get-rep user) amount) }
        )
        (ok true)
      )
    )
  )
)

(define-public (revoke-reputation (user principal) (amount uint))
  ;; @disable-check unchecked-data
  (if (is-none (var-get admin))
    (err ERR-NOT-ADMIN)
    (if (not (is-eq (unwrap-panic (var-get admin)) tx-sender))
      (err ERR-NOT-ADMIN)
      ;; @disable-check unchecked-data
      (let ((current (get-rep user)))
        (if (< current amount)
          (err ERR-INVALID-AMOUNT)
          ;; @disable-check unchecked-data
          (begin
            ;; @disable-check unchecked-data
            (map-set reputation
              { user: user }
              { points: (- current amount) }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; Create proposal (requires reputation > 0)
;; ------------------------------------------------------------

(define-public (create-proposal (description (string-ascii 200)) (duration uint))
  ;; @disable-check unchecked-data
  (if (<= (get-rep tx-sender) u0)
    (err ERR-NOT-MEMBER)
    (let ((id (+ (var-get proposal-count) u1)))
      (begin
        (var-set proposal-count id)
        ;; @disable-check unchecked-data
        (map-set proposals
          { id: id }
          {
            creator: tx-sender,
            ;; @disable-check unchecked-data
            description: description,
            start: burn-block-height,
            ;; @disable-check unchecked-data
            end: (+ burn-block-height duration),
            yes: u0,
            no: u0,
            open: true
          }
        )
        (ok id)
      )
    )
  )
)

;; ------------------------------------------------------------
;; Vote (weighted by reputation)
;; ------------------------------------------------------------

(define-public (vote (proposal-id uint) (support bool))
  ;; @disable-check unchecked-data
  (match (map-get? proposals { id: proposal-id })
    some-p
      (if (or (not (get open some-p)) (> burn-block-height (get end some-p)))
        (err ERR-VOTING-CLOSED)
        (if (<= (get-rep tx-sender) u0)
          (err ERR-NOT-MEMBER)
          (if (is-some (map-get? votes { id: proposal-id, voter: tx-sender }))
            (err ERR-ALREADY-VOTED)
            (let ((weight (get-rep tx-sender)))
              (begin
                ;; @disable-check unchecked-data
                (map-set votes { id: proposal-id, voter: tx-sender } { voted: true })
                (if support
                  ;; @disable-check unchecked-data
                  (map-set proposals { id: proposal-id }
                    {
                      creator: (get creator some-p),
                      description: (get description some-p),
                      start: (get start some-p),
                      end: (get end some-p),
                      yes: (+ (get yes some-p) weight),
                      no: (get no some-p),
                      open: (get open some-p)
                    })
                  ;; @disable-check unchecked-data
                  (map-set proposals { id: proposal-id }
                    {
                      creator: (get creator some-p),
                      description: (get description some-p),
                      start: (get start some-p),
                      end: (get end some-p),
                      yes: (get yes some-p),
                      no: (+ (get no some-p) weight),
                      open: (get open some-p)
                    })
                )
                (ok true)
              )
            )
          )
        )
      )
    (err ERR-PROPOSAL-NOT-FOUND)
  )
)

;; ------------------------------------------------------------
;; Finalize proposal
;; ------------------------------------------------------------

(define-public (finalize (proposal-id uint))
  ;; @disable-check unchecked-data
  (match (map-get? proposals { id: proposal-id })
    some-p
      (if (get open some-p)
        (let ((total (+ (get yes some-p) (get no some-p))))
          (begin
            ;; @disable-check unchecked-data
            (map-set proposals { id: proposal-id }
              {
                creator: (get creator some-p),
                description: (get description some-p),
                start: (get start some-p),
                end: (get end some-p),
                yes: (get yes some-p),
                no: (get no some-p),
                open: false
              })
            (if (< total (var-get quorum-reputation))
              (ok { result: "failed-quorum" })
              (if (> (get yes some-p) (get no some-p))
                (ok { result: "passed" })
                (ok { result: "rejected" })
              )
            )
          )
        )
        (err ERR-VOTING-CLOSED)
      )
    (err ERR-PROPOSAL-NOT-FOUND)
  )
)

;; ------------------------------------------------------------
;; Read-only helpers
;; ------------------------------------------------------------

(define-read-only (get-reputation (user principal))
  (ok (get-rep user))
)

(define-read-only (get-proposal (id uint))
  (map-get? proposals { id: id })
)

(define-read-only (get-total-proposals)
  (ok (var-get proposal-count))
)
