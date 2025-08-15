;; Group Events Contract
;; Manages kite flying group events and attendee coordination

(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-DATA (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-EVENT-FULL (err u103))

(define-data-var next-event-id uint u1)

(define-map events
  { event-id: uint }
  {
    organizer: principal,
    location: (string-ascii 50),
    scheduled-time: uint,
    min-wind-speed: uint,
    max-wind-speed: uint,
    max-attendees: uint,
    current-attendees: uint,
    status: (string-ascii 10),
    description: (string-ascii 200)
  }
)

(define-map event-attendees
  { event-id: uint, attendee: principal }
  { joined-at: uint, kite-type: (string-ascii 20) }
)

(define-map user-events
  { user: principal, event-id: uint }
  { role: (string-ascii 10) }
)

(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-attendee-info (event-id uint) (attendee principal))
  (map-get? event-attendees { event-id: event-id, attendee: attendee })
)

(define-read-only (get-next-event-id)
  (var-get next-event-id)
)

(define-public (create-event
  (location (string-ascii 50))
  (scheduled-time uint)
  (min-wind-speed uint)
  (max-wind-speed uint)
  (max-attendees uint)
  (description (string-ascii 200))
)
  (let ((event-id (var-get next-event-id)))
    (map-set events
      { event-id: event-id }
      {
        organizer: tx-sender,
        location: location,
        scheduled-time: scheduled-time,
        min-wind-speed: min-wind-speed,
        max-wind-speed: max-wind-speed,
        max-attendees: max-attendees,
        current-attendees: u0,
        status: "scheduled",
        description: description
      }
    )
    (map-set user-events
      { user: tx-sender, event-id: event-id }
      { role: "organizer" }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (join-event (event-id uint) (kite-type (string-ascii 20)))
  (let ((event-data (map-get? events { event-id: event-id })))
    (if (is-some event-data)
      (let ((event (unwrap-panic event-data)))
        (if (< (get current-attendees event) (get max-attendees event))
          (begin
            (map-set event-attendees
              { event-id: event-id, attendee: tx-sender }
              { joined-at: stacks-block-height, kite-type: kite-type }
            )
            (map-set user-events
              { user: tx-sender, event-id: event-id }
              { role: "attendee" }
            )
            (map-set events
              { event-id: event-id }
              (merge event { current-attendees: (+ (get current-attendees event) u1) })
            )
            (ok true)
          )
          ERR-EVENT-FULL
        )
      )
      ERR-NOT-FOUND
    )
  )
)

(define-public (update-event-status (event-id uint) (new-status (string-ascii 10)))
  (let ((event-data (map-get? events { event-id: event-id })))
    (if (is-some event-data)
      (let ((event (unwrap-panic event-data)))
        (if (is-eq (get organizer event) tx-sender)
          (begin
            (map-set events
              { event-id: event-id }
              (merge event { status: new-status })
            )
            (ok true)
          )
          ERR-UNAUTHORIZED
        )
      )
      ERR-NOT-FOUND
    )
  )
)
