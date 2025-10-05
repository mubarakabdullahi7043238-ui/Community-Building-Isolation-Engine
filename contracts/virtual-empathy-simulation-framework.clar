;; Virtual Empathy Simulation Framework
;; Generates authentic emotional responses using crowd-sourced feeling databases

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_EMOTION (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_VALIDATION (err u104))
(define-constant MIN_VALIDATION_SCORE u50)
(define-constant MAX_EMOTION_INTENSITY u100)

;; Data Variables
(define-data-var total-emotions uint u0)
(define-data-var total-responses uint u0)
(define-data-var validation-threshold uint u3)

;; Data Maps
(define-map emotions
    { emotion-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        intensity: uint,
        creator: principal,
        validation-score: uint,
        created-at: uint,
        is-active: bool
    }
)

(define-map emotion-responses
    { emotion-id: uint, response-id: uint }
    {
        response-text: (string-ascii 500),
        creator: principal,
        validation-count: uint,
        authenticity-score: uint,
        usage-count: uint,
        created-at: uint
    }
)

(define-map user-validations
    { user: principal, emotion-id: uint, response-id: uint }
    {
        score: uint,
        comment: (string-ascii 200),
        validated-at: uint
    }
)

(define-map user-stats
    { user: principal }
    {
        emotions-created: uint,
        responses-created: uint,
        validations-given: uint,
        reputation-score: uint
    }
)

;; Public Functions

;; Create a new emotion entry in the database
(define-public (create-emotion (name (string-ascii 50)) (description (string-ascii 200)) (intensity uint))
    (let
        (
            (emotion-id (+ (var-get total-emotions) u1))
            (current-block burn-block-height)
        )
        (asserts! (<= intensity MAX_EMOTION_INTENSITY) ERR_INVALID_EMOTION)
        (asserts! (> (len name) u0) ERR_INVALID_EMOTION)
        
        ;; Store emotion data
        (map-set emotions
            { emotion-id: emotion-id }
            {
                name: name,
                description: description,
                intensity: intensity,
                creator: tx-sender,
                validation-score: u0,
                created-at: current-block,
                is-active: true
            }
        )
        
        ;; Update counters and user stats
        (var-set total-emotions emotion-id)
        (update-user-emotion-count tx-sender)
        
        (ok emotion-id)
    )
)

;; Add a response to an existing emotion
(define-public (add-emotion-response (emotion-id uint) (response-text (string-ascii 500)))
    (let
        (
            (emotion-data (unwrap! (map-get? emotions { emotion-id: emotion-id }) ERR_NOT_FOUND))
            (response-id (+ (var-get total-responses) u1))
            (current-block burn-block-height)
        )
        (asserts! (get is-active emotion-data) ERR_NOT_FOUND)
        (asserts! (> (len response-text) u0) ERR_INVALID_EMOTION)
        
        ;; Store response data
        (map-set emotion-responses
            { emotion-id: emotion-id, response-id: response-id }
            {
                response-text: response-text,
                creator: tx-sender,
                validation-count: u0,
                authenticity-score: u0,
                usage-count: u0,
                created-at: current-block
            }
        )
        
        ;; Update counters and user stats
        (var-set total-responses response-id)
        (update-user-response-count tx-sender)
        
        (ok response-id)
    )
)

;; Validate a response for authenticity
(define-public (validate-response (emotion-id uint) (response-id uint) (score uint) (comment (string-ascii 200)))
    (let
        (
            (response-data (unwrap! (map-get? emotion-responses { emotion-id: emotion-id, response-id: response-id }) ERR_NOT_FOUND))
            (current-block burn-block-height)
        )
        (asserts! (<= score MAX_EMOTION_INTENSITY) ERR_INVALID_EMOTION)
        (asserts! (is-none (map-get? user-validations { user: tx-sender, emotion-id: emotion-id, response-id: response-id })) ERR_ALREADY_EXISTS)
        
        ;; Store validation
        (map-set user-validations
            { user: tx-sender, emotion-id: emotion-id, response-id: response-id }
            {
                score: score,
                comment: comment,
                validated-at: current-block
            }
        )
        
        ;; Update response validation data
        (map-set emotion-responses
            { emotion-id: emotion-id, response-id: response-id }
            (merge response-data {
                validation-count: (+ (get validation-count response-data) u1),
                authenticity-score: (calculate-authenticity-score emotion-id response-id score)
            })
        )
        
        ;; Update user stats
        (update-user-validation-count tx-sender)
        
        (ok true)
    )
)

;; Generate an empathetic response based on emotion and context
(define-public (generate-empathy-response (emotion-id uint) (context (string-ascii 200)))
    (let
        (
            (emotion-data (unwrap! (map-get? emotions { emotion-id: emotion-id }) ERR_NOT_FOUND))
            (best-response (get-best-validated-response emotion-id))
        )
        (asserts! (get is-active emotion-data) ERR_NOT_FOUND)
        
        ;; Increment usage count for the selected response
        (match best-response
            response-id (increment-response-usage emotion-id response-id)
            false
        )
        
        (ok best-response)
    )
)

;; Update emotion activation status (admin function)
(define-public (set-emotion-status (emotion-id uint) (is-active bool))
    (let
        (
            (emotion-data (unwrap! (map-get? emotions { emotion-id: emotion-id }) ERR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set emotions
            { emotion-id: emotion-id }
            (merge emotion-data { is-active: is-active })
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get emotion details
(define-read-only (get-emotion (emotion-id uint))
    (map-get? emotions { emotion-id: emotion-id })
)

;; Get response details
(define-read-only (get-emotion-response (emotion-id uint) (response-id uint))
    (map-get? emotion-responses { emotion-id: emotion-id, response-id: response-id })
)

;; Get user validation
(define-read-only (get-user-validation (user principal) (emotion-id uint) (response-id uint))
    (map-get? user-validations { user: user, emotion-id: emotion-id, response-id: response-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (default-to
        { emotions-created: u0, responses-created: u0, validations-given: u0, reputation-score: u0 }
        (map-get? user-stats { user: user })
    )
)

;; Get total counts
(define-read-only (get-total-emotions)
    (var-get total-emotions)
)

(define-read-only (get-total-responses)
    (var-get total-responses)
)

(define-read-only (get-validation-threshold)
    (var-get validation-threshold)
)

;; Private Functions

;; Simple min function
(define-private (min (a uint) (b uint))
    (if (<= a b) a b)
)

;; Simple max function
(define-private (max (a uint) (b uint))
    (if (>= a b) a b)
)

;; Calculate authenticity score based on validations
(define-private (calculate-authenticity-score (emotion-id uint) (response-id uint) (new-score uint))
    (let
        (
            (response-data (unwrap-panic (map-get? emotion-responses { emotion-id: emotion-id, response-id: response-id })))
            (current-score (get authenticity-score response-data))
            (validation-count (get validation-count response-data))
        )
        (if (is-eq validation-count u0)
            new-score
            (/ (+ (* current-score validation-count) new-score) (+ validation-count u1))
        )
    )
)

;; Get the best validated response for an emotion
(define-private (get-best-validated-response (emotion-id uint))
    ;; Simplified: returns response-id 1 if it exists and meets validation threshold
    (match (map-get? emotion-responses { emotion-id: emotion-id, response-id: u1 })
        response-data (if (>= (get authenticity-score response-data) MIN_VALIDATION_SCORE) (some u1) none)
        none
    )
)

;; Increment response usage count
(define-private (increment-response-usage (emotion-id uint) (response-id uint))
    (match (map-get? emotion-responses { emotion-id: emotion-id, response-id: response-id })
        response-data (map-set emotion-responses
            { emotion-id: emotion-id, response-id: response-id }
            (merge response-data { usage-count: (+ (get usage-count response-data) u1) })
        )
        false
    )
)

;; Update user emotion creation count
(define-private (update-user-emotion-count (user principal))
    (let
        (
            (current-stats (get-user-stats user))
        )
        (map-set user-stats
            { user: user }
            (merge current-stats {
                emotions-created: (+ (get emotions-created current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u5)
            })
        )
    )
)

;; Update user response creation count
(define-private (update-user-response-count (user principal))
    (let
        (
            (current-stats (get-user-stats user))
        )
        (map-set user-stats
            { user: user }
            (merge current-stats {
                responses-created: (+ (get responses-created current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u3)
            })
        )
    )
)

;; Update user validation count
(define-private (update-user-validation-count (user principal))
    (let
        (
            (current-stats (get-user-stats user))
        )
        (map-set user-stats
            { user: user }
            (merge current-stats {
                validations-given: (+ (get validations-given current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u2)
            })
        )
    )
)

