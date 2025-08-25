;; Customer Analytics Contract
;; Handles behavior analysis and segmentation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-INPUT (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))

;; Data Variables
(define-data-var total-customers uint u0)
(define-data-var next-interaction-id uint u1)

;; Data Maps
(define-map customers
  { customer-id: (string-ascii 50) }
  {
    first-purchase: uint,
    last-purchase: uint,
    total-purchases: uint,
    total-spent: uint,
    avg-order-value: uint,
    purchase-frequency: uint,
    segment: (string-ascii 20)
  }
)

(define-map customer-interactions
  { interaction-id: uint }
  {
    customer-id: (string-ascii 50),
    interaction-type: (string-ascii 20),
    product-id: (optional (string-ascii 50)),
    value: uint,
    timestamp: uint,
    channel: (string-ascii 20)
  }
)

(define-map customer-segments
  { segment: (string-ascii 20) }
  {
    min-spent: uint,
    min-frequency: uint,
    customer-count: uint,
    avg-value: uint,
    description: (string-ascii 100)
  }
)

(define-map behavioral-analytics
  { customer-id: (string-ascii 50) }
  {
    lifetime-value: uint,
    churn-risk: uint,
    next-purchase-prediction: uint,
    preferred-categories: (string-ascii 100),
    last-analysis: uint
  }
)

;; Public Functions

;; Register new customer
(define-public (register-customer (customer-id (string-ascii 50)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (> (len customer-id) u0) ERR-INVALID-INPUT)
    (asserts! (is-none (map-get? customers { customer-id: customer-id })) ERR-ALREADY-EXISTS)

    (map-set customers
      { customer-id: customer-id }
      {
        first-purchase: u0,
        last-purchase: u0,
        total-purchases: u0,
        total-spent: u0,
        avg-order-value: u0,
        purchase-frequency: u0,
        segment: "NEW"
      }
    )

    (var-set total-customers (+ (var-get total-customers) u1))
    (ok true)
  )
)

;; Record customer interaction
(define-public (record-interaction (customer-id (string-ascii 50)) (interaction-type (string-ascii 20)) (product-id (optional (string-ascii 50))) (value uint) (channel (string-ascii 20)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (interaction-id (var-get next-interaction-id))
      (customer (unwrap! (map-get? customers { customer-id: customer-id }) ERR-NOT-FOUND))
    )
    (asserts! (> (len interaction-type) u0) ERR-INVALID-INPUT)
    (asserts! (> (len channel) u0) ERR-INVALID-INPUT)

    ;; Record interaction
    (map-set customer-interactions
      { interaction-id: interaction-id }
      {
        customer-id: customer-id,
        interaction-type: interaction-type,
        product-id: product-id,
        value: value,
        timestamp: current-time,
        channel: channel
      }
    )

    ;; Update customer data if it's a purchase
    (if (is-eq interaction-type "PURCHASE")
      (let
        (
          (new-total-spent (+ (get total-spent customer) value))
          (new-total-purchases (+ (get total-purchases customer) u1))
          (new-avg-order (/ new-total-spent new-total-purchases))
          (first-purchase-time (if (is-eq (get first-purchase customer) u0) current-time (get first-purchase customer)))
          (time-span (- current-time first-purchase-time))
          (frequency (if (> time-span u0) (/ new-total-purchases (/ time-span u86400)) u0))
        )
        (map-set customers
          { customer-id: customer-id }
          {
            first-purchase: first-purchase-time,
            last-purchase: current-time,
            total-purchases: new-total-purchases,
            total-spent: new-total-spent,
            avg-order-value: new-avg-order,
            purchase-frequency: frequency,
            segment: (calculate-segment new-total-spent frequency)
          }
        )
      )
      true
    )

    (var-set next-interaction-id (+ interaction-id u1))
    (ok interaction-id)
  )
)

;; Calculate customer segment
(define-private (calculate-segment (total-spent uint) (frequency uint))
  (if (and (> total-spent u1000) (> frequency u10))
    "VIP"
    (if (and (> total-spent u500) (> frequency u5))
      "LOYAL"
      (if (> total-spent u100)
        "REGULAR"
        "CASUAL"
      )
    )
  )
)

;; Analyze customer behavior
(define-public (analyze-customer (customer-id (string-ascii 50)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (customer (unwrap! (map-get? customers { customer-id: customer-id }) ERR-NOT-FOUND))
      (days-since-last (if (> (get last-purchase customer) u0)
                         (/ (- current-time (get last-purchase customer)) u86400)
                         u999))
      (lifetime-value (get total-spent customer))
      (churn-risk (if (> days-since-last u90) u80 (if (> days-since-last u30) u40 u10)))
      (next-purchase-days (if (> (get purchase-frequency customer) u0)
                            (/ u30 (get purchase-frequency customer))
                            u60))
    )

    (map-set behavioral-analytics
      { customer-id: customer-id }
      {
        lifetime-value: lifetime-value,
        churn-risk: churn-risk,
        next-purchase-prediction: (+ current-time (* next-purchase-days u86400)),
        preferred-categories: "GENERAL",
        last-analysis: current-time
      }
    )

    (ok {
      lifetime-value: lifetime-value,
      churn-risk: churn-risk,
      segment: (get segment customer),
      next-purchase-days: next-purchase-days
    })
  )
)

;; Initialize customer segments
(define-public (initialize-segments)
  (begin
    (map-set customer-segments
      { segment: "VIP" }
      {
        min-spent: u1000,
        min-frequency: u10,
        customer-count: u0,
        avg-value: u0,
        description: "High-value frequent customers"
      }
    )

    (map-set customer-segments
      { segment: "LOYAL" }
      {
        min-spent: u500,
        min-frequency: u5,
        customer-count: u0,
        avg-value: u0,
        description: "Regular repeat customers"
      }
    )

    (map-set customer-segments
      { segment: "REGULAR" }
      {
        min-spent: u100,
        min-frequency: u1,
        customer-count: u0,
        avg-value: u0,
        description: "Occasional customers"
      }
    )

    (map-set customer-segments
      { segment: "CASUAL" }
      {
        min-spent: u0,
        min-frequency: u0,
        customer-count: u0,
        avg-value: u0,
        description: "Infrequent or new customers"
      }
    )

    (ok true)
  )
)

;; Read Functions

;; Get customer data
(define-read-only (get-customer (customer-id (string-ascii 50)))
  (map-get? customers { customer-id: customer-id })
)

;; Get customer interaction
(define-read-only (get-interaction (interaction-id uint))
  (map-get? customer-interactions { interaction-id: interaction-id })
)

;; Get customer segment info
(define-read-only (get-segment-info (segment (string-ascii 20)))
  (map-get? customer-segments { segment: segment })
)

;; Get behavioral analytics
(define-read-only (get-behavioral-analytics (customer-id (string-ascii 50)))
  (map-get? behavioral-analytics { customer-id: customer-id })
)

;; Get total customers
(define-read-only (get-total-customers)
  (var-get total-customers)
)
