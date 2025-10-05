;; Loneliness Prevention Algorithm
;; Prevents social isolation by scheduling mandatory fun and automated friendship maintenance

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_INVALID_SCHEDULE (err u202))
(define-constant ERR_ALREADY_EXISTS (err u203))
(define-constant ERR_ISOLATION_RISK (err u204))
(define-constant MAX_ISOLATION_SCORE u100)
(define-constant CRITICAL_ISOLATION_THRESHOLD u80)
(define-constant INTERACTION_FREQUENCY_DAYS u7)
(define-constant FRIENDSHIP_DECAY_RATE u5)

;; Data Variables
(define-data-var total-users uint u0)
(define-data-var total-interactions uint u0)
(define-data-var total-scheduled-activities uint u0)
(define-data-var global-loneliness-index uint u0)

;; Data Maps
(define-map user-profiles
    { user: principal }
    {
        username: (string-ascii 50),
        isolation-score: uint,
        last-interaction: uint,
        friendship-count: uint,
        scheduled-activities: uint,
        engagement-level: uint,
        risk-status: (string-ascii 20),
        joined-at: uint,
        is-active: bool
    }
)

(define-map friendships
    { user1: principal, user2: principal }
    {
        friendship-strength: uint,
        last-maintenance: uint,
        interaction-count: uint,
        decay-rate: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-map scheduled-activities
    { activity-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        activity-type: (string-ascii 30),
        scheduled-for: uint,
        participants: (list 50 principal),
        creator: principal,
        is-mandatory: bool,
        engagement-score: uint,
        status: (string-ascii 20)
    }
)

(define-map user-interactions
    { user: principal, interaction-id: uint }
    {
        interaction-type: (string-ascii 50),
        partner: (optional principal),
        activity-id: (optional uint),
        engagement-score: uint,
        timestamp: uint,
        duration: uint
    }
)

(define-map isolation-alerts
    { user: principal, alert-id: uint }
    {
        alert-type: (string-ascii 50),
        severity: uint,
        message: (string-ascii 200),
        triggered-at: uint,
        resolved: bool,
        resolution-actions: (string-ascii 300)
    }
)

;; Public Functions

;; Register a new user in the system
(define-public (register-user (username (string-ascii 50)))
    (let
        (
            (current-block burn-block-height)
            (user-count (+ (var-get total-users) u1))
        )
        (asserts! (> (len username) u0) ERR_INVALID_SCHEDULE)
        (asserts! (is-none (map-get? user-profiles { user: tx-sender })) ERR_ALREADY_EXISTS)
        
        ;; Create user profile
        (map-set user-profiles
            { user: tx-sender }
            {
                username: username,
                isolation-score: u20,
                last-interaction: current-block,
                friendship-count: u0,
                scheduled-activities: u0,
                engagement-level: u50,
                risk-status: "normal",
                joined-at: current-block,
                is-active: true
            }
        )
        
        ;; Update global counters
        (var-set total-users user-count)
        
        (ok user-count)
    )
)

;; Create a friendship between two users
(define-public (create-friendship (friend principal))
    (let
        (
            (current-block burn-block-height)
            (user-profile (unwrap! (map-get? user-profiles { user: tx-sender }) ERR_NOT_FOUND))
            (friend-profile (unwrap! (map-get? user-profiles { user: friend }) ERR_NOT_FOUND))
        )
        (asserts! (not (is-eq tx-sender friend)) ERR_INVALID_SCHEDULE)
        (asserts! (get is-active user-profile) ERR_NOT_FOUND)
        (asserts! (get is-active friend-profile) ERR_NOT_FOUND)
        
        ;; Create bidirectional friendship
        (map-set friendships
            { user1: tx-sender, user2: friend }
            {
                friendship-strength: u50,
                last-maintenance: current-block,
                interaction-count: u0,
                decay-rate: FRIENDSHIP_DECAY_RATE,
                status: "active",
                created-at: current-block
            }
        )
        
        (map-set friendships
            { user1: friend, user2: tx-sender }
            {
                friendship-strength: u50,
                last-maintenance: current-block,
                interaction-count: u0,
                decay-rate: FRIENDSHIP_DECAY_RATE,
                status: "active",
                created-at: current-block
            }
        )
        
        ;; Update friendship counts
        (update-friendship-count tx-sender true)
        (update-friendship-count friend true)
        
        (ok true)
    )
)

;; Schedule a mandatory fun activity
(define-public (schedule-activity (title (string-ascii 100)) (description (string-ascii 300)) (activity-type (string-ascii 30)) (scheduled-for uint) (is-mandatory bool))
    (let
        (
            (activity-id (+ (var-get total-scheduled-activities) u1))
            (current-block burn-block-height)
        )
        (asserts! (> (len title) u0) ERR_INVALID_SCHEDULE)
        (asserts! (> scheduled-for current-block) ERR_INVALID_SCHEDULE)
        
        ;; Store activity
        (map-set scheduled-activities
            { activity-id: activity-id }
            {
                title: title,
                description: description,
                activity-type: activity-type,
                scheduled-for: scheduled-for,
                participants: (list tx-sender),
                creator: tx-sender,
                is-mandatory: is-mandatory,
                engagement-score: u0,
                status: "scheduled"
            }
        )
        
        ;; Update counters
        (var-set total-scheduled-activities activity-id)
        
        (ok activity-id)
    )
)

;; Join a scheduled activity
(define-public (join-activity (activity-id uint))
    (let
        (
            (activity-data (unwrap! (map-get? scheduled-activities { activity-id: activity-id }) ERR_NOT_FOUND))
            (current-participants (get participants activity-data))
        )
        (asserts! (is-eq (get status activity-data) "scheduled") ERR_INVALID_SCHEDULE)
        (asserts! (is-none (index-of current-participants tx-sender)) ERR_ALREADY_EXISTS)
        
        ;; Add participant to activity
        (map-set scheduled-activities
            { activity-id: activity-id }
            (merge activity-data {
                participants: (unwrap! (as-max-len? (append current-participants tx-sender) u50) ERR_INVALID_SCHEDULE)
            })
        )
        
        ;; Update user activity count
        (update-user-activity-count tx-sender)
        
        (ok true)
    )
)

;; Record a social interaction
(define-public (record-interaction (interaction-type (string-ascii 50)) (partner (optional principal)) (activity-id (optional uint)) (duration uint))
    (let
        (
            (interaction-id (+ (var-get total-interactions) u1))
            (current-block burn-block-height)
            (engagement-score (calculate-engagement-score interaction-type duration))
        )
        (asserts! (> (len interaction-type) u0) ERR_INVALID_SCHEDULE)
        (asserts! (> duration u0) ERR_INVALID_SCHEDULE)
        
        ;; Store interaction
        (map-set user-interactions
            { user: tx-sender, interaction-id: interaction-id }
            {
                interaction-type: interaction-type,
                partner: partner,
                activity-id: activity-id,
                engagement-score: engagement-score,
                timestamp: current-block,
                duration: duration
            }
        )
        
        ;; Update user profile
        (update-user-interaction tx-sender current-block engagement-score)
        
        ;; Update friendship if partner specified
        (match partner
            friend-principal (maintain-friendship tx-sender friend-principal current-block)
            true
        )
        
        ;; Update global counter
        (var-set total-interactions interaction-id)
        
        (ok interaction-id)
    )
)

;; Calculate and update isolation risk
(define-public (assess-isolation-risk (user principal))
    (let
        (
            (user-profile (unwrap! (map-get? user-profiles { user: user }) ERR_NOT_FOUND))
            (current-block burn-block-height)
            (isolation-score (calculate-isolation-score user current-block))
        )
        ;; Update isolation score
        (map-set user-profiles
            { user: user }
            (merge user-profile {
                isolation-score: isolation-score,
                risk-status: (determine-risk-status isolation-score)
            })
        )
        
        ;; Create alert if critical
        (if (>= isolation-score CRITICAL_ISOLATION_THRESHOLD)
            (create-isolation-alert user isolation-score)
            true
        )
        
        (ok isolation-score)
    )
)

;; Emergency intervention for high-risk users
(define-public (trigger-intervention (user principal) (intervention-type (string-ascii 50)))
    (let
        (
            (user-profile (unwrap! (map-get? user-profiles { user: user }) ERR_NOT_FOUND))
            (isolation-score (get isolation-score user-profile))
        )
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender user)) ERR_UNAUTHORIZED)
        (asserts! (>= isolation-score CRITICAL_ISOLATION_THRESHOLD) ERR_ISOLATION_RISK)
        
        ;; Schedule mandatory interaction
        (unwrap-panic (schedule-activity
            "Emergency Social Interaction"
            "Automated intervention to prevent isolation"
            "intervention"
            (+ burn-block-height u144) ;; 24 hours later
            true
        ))
        
        ;; Reduce isolation score temporarily
        (map-set user-profiles
            { user: user }
            (merge user-profile {
                isolation-score: (- isolation-score u20),
                risk-status: "intervention"
            })
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get user profile
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles { user: user })
)

;; Get friendship details
(define-read-only (get-friendship (user1 principal) (user2 principal))
    (map-get? friendships { user1: user1, user2: user2 })
)

;; Get scheduled activity
(define-read-only (get-activity (activity-id uint))
    (map-get? scheduled-activities { activity-id: activity-id })
)

;; Get user interaction
(define-read-only (get-user-interaction (user principal) (interaction-id uint))
    (map-get? user-interactions { user: user, interaction-id: interaction-id })
)

;; Get isolation alert
(define-read-only (get-isolation-alert (user principal) (alert-id uint))
    (map-get? isolation-alerts { user: user, alert-id: alert-id })
)

;; Get system statistics
(define-read-only (get-system-stats)
    {
        total-users: (var-get total-users),
        total-interactions: (var-get total-interactions),
        total-activities: (var-get total-scheduled-activities),
        global-loneliness-index: (var-get global-loneliness-index)
    }
)

;; Check if user needs intervention
(define-read-only (needs-intervention (user principal))
    (match (map-get? user-profiles { user: user })
        profile (>= (get isolation-score profile) CRITICAL_ISOLATION_THRESHOLD)
        false
    )
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

;; Calculate engagement score based on interaction type and duration
(define-private (calculate-engagement-score (interaction-type (string-ascii 50)) (duration uint))
    (let
        (
            (base-score (if (is-eq interaction-type "video-call")
                u20
                (if (is-eq interaction-type "text-chat")
                    u10
                    (if (is-eq interaction-type "group-activity")
                        u30
                        u15
                    )
                )
            ))
            (duration-bonus (min (/ duration u10) u20))
        )
        (+ base-score duration-bonus)
    )
)

;; Calculate isolation score based on recent activity
(define-private (calculate-isolation-score (user principal) (current-block uint))
    (let
        (
            (user-profile (unwrap-panic (map-get? user-profiles { user: user })))
            (last-interaction (get last-interaction user-profile))
            (friendship-count (get friendship-count user-profile))
            (engagement-level (get engagement-level user-profile))
            (days-since-interaction (/ (- current-block last-interaction) u144)) ;; blocks per day
        )
        (let
            (
                (time-penalty (min (* days-since-interaction u10) u50))
                (friendship-bonus (min (* friendship-count u5) u30))
                (engagement-bonus (/ engagement-level u2))
            )
            (max u0 (min MAX_ISOLATION_SCORE (- (+ u50 time-penalty) friendship-bonus engagement-bonus)))
        )
    )
)

;; Determine risk status based on isolation score
(define-private (determine-risk-status (isolation-score uint))
    (if (>= isolation-score CRITICAL_ISOLATION_THRESHOLD)
        "critical"
        (if (>= isolation-score u60)
            "high"
            (if (>= isolation-score u40)
                "medium"
                "low"
            )
        )
    )
)

;; Create isolation alert
(define-private (create-isolation-alert (user principal) (isolation-score uint))
    (let
        (
            (alert-id u1) ;; Simplified alert ID
            (current-block burn-block-height)
        )
        (map-set isolation-alerts
            { user: user, alert-id: alert-id }
            {
                alert-type: "isolation-risk",
                severity: isolation-score,
                message: "High isolation risk detected - intervention recommended",
                triggered-at: current-block,
                resolved: false,
                resolution-actions: "Schedule mandatory social activities"
            }
        )
    )
)

;; Update user interaction data
(define-private (update-user-interaction (user principal) (current-block uint) (engagement-score uint))
    (let
        (
            (user-profile (unwrap-panic (map-get? user-profiles { user: user })))
            (new-engagement (min u100 (+ (get engagement-level user-profile) (/ engagement-score u2))))
        )
        (map-set user-profiles
            { user: user }
            (merge user-profile {
                last-interaction: current-block,
                engagement-level: new-engagement
            })
        )
    )
)

;; Maintain friendship strength
(define-private (maintain-friendship (user1 principal) (user2 principal) (current-block uint))
    (match (map-get? friendships { user1: user1, user2: user2 })
        friendship-data (map-set friendships
            { user1: user1, user2: user2 }
            (merge friendship-data {
                last-maintenance: current-block,
                interaction-count: (+ (get interaction-count friendship-data) u1),
                friendship-strength: (min u100 (+ (get friendship-strength friendship-data) u5))
            })
        )
        false
    )
)

;; Update friendship count
(define-private (update-friendship-count (user principal) (increment bool))
    (let
        (
            (user-profile (unwrap-panic (map-get? user-profiles { user: user })))
            (current-count (get friendship-count user-profile))
            (new-count (if increment (+ current-count u1) (- current-count u1)))
        )
        (map-set user-profiles
            { user: user }
            (merge user-profile { friendship-count: new-count })
        )
    )
)

;; Update user activity count
(define-private (update-user-activity-count (user principal))
    (let
        (
            (user-profile (unwrap-panic (map-get? user-profiles { user: user })))
        )
        (map-set user-profiles
            { user: user }
            (merge user-profile {
                scheduled-activities: (+ (get scheduled-activities user-profile) u1),
                engagement-level: (min u100 (+ (get engagement-level user-profile) u10))
            })
        )
    )
)

