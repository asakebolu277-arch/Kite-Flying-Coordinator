;; Wind Conditions Contract
;; Manages wind reports and kite type recommendations

(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-DATA (err u101))
(define-constant ERR-NOT-FOUND (err u102))

(define-map wind-reports
  { location: (string-ascii 50), reporter: principal }
  {
    wind-speed: uint,
    wind-direction: (string-ascii 10),
    conditions: (string-ascii 20),
    kite-types: (list 3 (string-ascii 20)),
    timestamp: uint,
    active: bool
  }
)

(define-map location-stats
  { location: (string-ascii 50) }
  { report-count: uint, last-updated: uint }
)

(define-read-only (get-wind-report (location (string-ascii 50)) (reporter principal))
  (map-get? wind-reports { location: location, reporter: reporter })
)

(define-read-only (get-location-stats (location (string-ascii 50)))
  (map-get? location-stats { location: location })
)

(define-read-only (recommend-kites (wind-speed uint))
  (if (<= wind-speed u5)
    (list "ultralight" "indoor")
    (if (<= wind-speed u15)
      (list "delta" "diamond" "box")
      (if (<= wind-speed u25)
        (list "sport" "stunt")
        (list "power")
      )
    )
  )
)

(define-public (submit-wind-report
  (location (string-ascii 50))
  (wind-speed uint)
  (wind-direction (string-ascii 10))
  (conditions (string-ascii 20))
)
  (let ((kite-recommendations (recommend-kites wind-speed)))
    (map-set wind-reports
      { location: location, reporter: tx-sender }
      {
        wind-speed: wind-speed,
        wind-direction: wind-direction,
        conditions: conditions,
        kite-types: kite-recommendations,
        timestamp: stacks-block-height,
        active: true
      }
    )
    (map-set location-stats
      { location: location }
      {
        report-count: (+ (default-to u0 (get report-count (map-get? location-stats { location: location }))) u1),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (deactivate-report (location (string-ascii 50)))
  (let ((existing-report (map-get? wind-reports { location: location, reporter: tx-sender })))
    (if (is-some existing-report)
      (begin
        (map-set wind-reports
          { location: location, reporter: tx-sender }
          (merge (unwrap-panic existing-report) { active: false })
        )
        (ok true)
      )
      ERR-NOT-FOUND
    )
  )
)
