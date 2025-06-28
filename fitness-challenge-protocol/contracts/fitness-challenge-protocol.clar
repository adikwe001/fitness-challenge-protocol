;; Fitness Challenge Protocol
;; A decentralized fitness challenge platform on Stacks

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-challenge-ended (err u104))
(define-constant err-already-participant (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-challenge-not-ended (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-target-not-met (err u109))

;; Data Variables
(define-data-var next-challenge-id uint u1)
(define-data-var protocol-fee-rate uint u250) ;; 2.5% in basis points

;; Data Maps
(define-map challenges 
  { challenge-id: uint }
  {
    creator: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    target-value: uint,
    entry-fee: uint,
    start-block: uint,
    end-block: uint,
    total-pool: uint,
    participants-count: uint,
    is-active: bool
  }
)

(define-map participants
  { challenge-id: uint, participant: principal }
  {
    current-progress: uint,
    has-paid: bool,
    reward-claimed: bool,
    join-block: uint
  }
)

(define-map challenge-participants
  { challenge-id: uint }
  { participant-list: (list 100 principal) }
)

(define-map user-stats
  { user: principal }
  {
    challenges-joined: uint,
    challenges-completed: uint,
    total-rewards: uint
  }
)

;; Private Functions
(define-private (is-challenge-active (challenge-id uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) false)))
    (and 
      (get is-active challenge)
      (>= block-height (get start-block challenge))
      (< block-height (get end-block challenge))
    )
  )
)

(define-private (is-challenge-ended (challenge-id uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) false)))
    (>= block-height (get end-block challenge))
  )
)

(define-private (calculate-protocol-fee (amount uint))
  (/ (* amount (var-get protocol-fee-rate)) u10000)
)

(define-private (update-user-stats (user principal) (increment-joined bool) (increment-completed bool) (reward-amount uint))
  (let ((current-stats (default-to 
                         {challenges-joined: u0, challenges-completed: u0, total-rewards: u0}
                         (map-get? user-stats {user: user}))))
    (map-set user-stats {user: user}
      {
        challenges-joined: (if increment-joined 
                            (+ (get challenges-joined current-stats) u1)
                            (get challenges-joined current-stats)),
        challenges-completed: (if increment-completed
                               (+ (get challenges-completed current-stats) u1)
                               (get challenges-completed current-stats)),
        total-rewards: (+ (get total-rewards current-stats) reward-amount)
      }
    )
  )
)

;; Public Functions

;; Create a new fitness challenge
(define-public (create-challenge (title (string-ascii 64)) 
                                (description (string-ascii 256))
                                (target-value uint)
                                (entry-fee uint)
                                (duration-blocks uint))
  (let ((challenge-id (var-get next-challenge-id))
        (start-block (+ block-height u10))
        (end-block (+ start-block duration-blocks)))
    
    (asserts! (> target-value u0) err-invalid-amount)
    (asserts! (> duration-blocks u144) err-invalid-amount) ;; Minimum 1 day
    
    (map-set challenges {challenge-id: challenge-id}
      {
        creator: tx-sender,
        title: title,
        description: description,
        target-value: target-value,
        entry-fee: entry-fee,
        start-block: start-block,
        end-block: end-block,
        total-pool: u0,
        participants-count: u0,
        is-active: true
      }
    )
    
    (map-set challenge-participants {challenge-id: challenge-id}
      {participant-list: (list)}
    )
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

;; Join a fitness challenge
(define-public (join-challenge (challenge-id uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) err-not-found))
        (entry-fee (get entry-fee challenge))
        (participants-list (default-to (list) 
                             (get participant-list 
                               (map-get? challenge-participants {challenge-id: challenge-id})))))
    
    (asserts! (get is-active challenge) err-challenge-ended)
    (asserts! (< block-height (get start-block challenge)) err-challenge-ended)
    (asserts! (is-none (map-get? participants {challenge-id: challenge-id, participant: tx-sender})) 
              err-already-participant)
    (asserts! (< (len participants-list) u100) err-invalid-amount)
    
    ;; Transfer entry fee if required
    (if (> entry-fee u0)
      (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))
      true
    )
    
    ;; Add participant
    (map-set participants {challenge-id: challenge-id, participant: tx-sender}
      {
        current-progress: u0,
        has-paid: (> entry-fee u0),
        reward-claimed: false,
        join-block: block-height
      }
    )
    
    ;; Update challenge data
    (map-set challenges {challenge-id: challenge-id}
      (merge challenge {
        total-pool: (+ (get total-pool challenge) entry-fee),
        participants-count: (+ (get participants-count challenge) u1)
      })
    )
    
    ;; Update participants list
    (map-set challenge-participants {challenge-id: challenge-id}
      {participant-list: (unwrap! (as-max-len? (append participants-list tx-sender) u100) err-invalid-amount)}
    )
    
    ;; Update user stats
    (update-user-stats tx-sender true false u0)
    (ok true)
  )
)

;; Update fitness progress
(define-public (update-progress (challenge-id uint) (new-progress uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) err-not-found))
        (participant-data (unwrap! (map-get? participants {challenge-id: challenge-id, participant: tx-sender}) 
                                  err-unauthorized)))
    
    (asserts! (is-challenge-active challenge-id) err-challenge-ended)
    (asserts! (>= new-progress (get current-progress participant-data)) err-invalid-amount)
    
    (map-set participants {challenge-id: challenge-id, participant: tx-sender}
      (merge participant-data {current-progress: new-progress})
    )
    
    (ok true)
  )
)

;; Claim reward for completed challenge
(define-public (claim-reward (challenge-id uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) err-not-found))
        (participant-data (unwrap! (map-get? participants {challenge-id: challenge-id, participant: tx-sender}) 
                                  err-unauthorized))
        (total-pool (get total-pool challenge))
        (participants-count (get participants-count challenge)))
    
    (asserts! (is-challenge-ended challenge-id) err-challenge-not-ended)
    (asserts! (not (get reward-claimed participant-data)) err-already-claimed)
    (asserts! (>= (get current-progress participant-data) (get target-value challenge)) err-target-not-met)
    
    ;; Calculate reward (equal split among successful participants)
    (let ((protocol-fee (calculate-protocol-fee total-pool))
          (reward-pool (- total-pool protocol-fee))
          (successful-participants (count-successful-participants challenge-id))
          (individual-reward (if (> successful-participants u0) 
                              (/ reward-pool successful-participants) 
                              u0)))
      
      (asserts! (> individual-reward u0) err-invalid-amount)
      
      ;; Mark reward as claimed
      (map-set participants {challenge-id: challenge-id, participant: tx-sender}
        (merge participant-data {reward-claimed: true})
      )
      
      ;; Transfer reward
      (try! (as-contract (stx-transfer? individual-reward tx-sender tx-sender)))
      
      ;; Update user stats
      (update-user-stats tx-sender false true individual-reward)
      (ok individual-reward)
    )
  )
)

;; Helper function to count successful participants
(define-private (count-successful-participants (challenge-id uint))
  (let ((challenge (unwrap! (map-get? challenges {challenge-id: challenge-id}) u0))
        (target (get target-value challenge))
        (participants-list (default-to (list) 
                             (get participant-list 
                               (map-get? challenge-participants {challenge-id: challenge-id})))))
    (fold count-if-successful participants-list {target: target, challenge-id: challenge-id, count: u0})
  )
)

(define-private (count-if-successful (participant principal) (data {target: uint, challenge-id: uint, count: uint}))
  (let ((participant-data (map-get? participants 
                            {challenge-id: (get challenge-id data), participant: participant})))
    (if (and (is-some participant-data)
             (>= (get current-progress (unwrap-panic participant-data)) (get target data)))
      {target: (get target data), challenge-id: (get challenge-id data), count: (+ (get count data) u1)}
      data
    )
  )
)

;; Admin function to set protocol fee
(define-public (set-protocol-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set protocol-fee-rate new-fee-rate)
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges {challenge-id: challenge-id})
)

(define-read-only (get-participant-data (challenge-id uint) (participant principal))
  (map-get? participants {challenge-id: challenge-id, participant: participant})
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats {user: user})
)

(define-read-only (get-protocol-fee-rate)
  (var-get protocol-fee-rate)
)

(define-read-only (get-next-challenge-id)
  (var-get next-challenge-id)
)