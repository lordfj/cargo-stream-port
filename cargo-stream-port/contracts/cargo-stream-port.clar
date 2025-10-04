;; CargoStream - Autonomous Supply Chain Orchestration Platform
;; A comprehensive smart contract for managing autonomous shipments with automated payments

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-parameters (err u106))

;; Shipment Status Types
(define-constant status-created u0)
(define-constant status-in-transit u1)
(define-constant status-customs u2)
(define-constant status-delivered u3)
(define-constant status-disputed u4)
(define-constant status-cancelled u5)

;; Data Variables
(define-data-var shipment-nonce uint u0)
(define-data-var route-nonce uint u0)

;; Data Maps
(define-map shipments
    uint
    {
        shipper: principal,
        receiver: principal,
        carrier: (optional principal),
        origin: (string-ascii 100),
        destination: (string-ascii 100),
        status: uint,
        escrow-amount: uint,
        insurance-premium: uint,
        carbon-credits: uint,
        created-at: uint,
        delivered-at: (optional uint),
        current-route: uint
    }
)

(define-map routes
    uint
    {
        shipment-id: uint,
        route-name: (string-ascii 100),
        estimated-duration: uint,
        cost: uint,
        carbon-footprint: uint,
        risk-score: uint,
        is-active: bool,
        activated-at: (optional uint)
    }
)

(define-map compliance-zones
    (string-ascii 50)
    {
        region: (string-ascii 100),
        requirements: (string-ascii 500),
        last-updated: uint,
        is-active: bool
    }
)

(define-map authorized-carriers
    principal
    {
        name: (string-ascii 100),
        rating: uint,
        total-deliveries: uint,
        is-active: bool
    }
)

(define-map quality-assessments
    uint
    {
        shipment-id: uint,
        assessor: principal,
        quality-score: uint,
        temperature-compliance: bool,
        damage-reported: bool,
        assessed-at: uint
    }
)

;; Private Functions
(define-private (is-shipment-owner (shipment-id uint) (user principal))
    (match (map-get? shipments shipment-id)
        shipment (or (is-eq (get shipper shipment) user)
                    (is-eq (get receiver shipment) user))
        false
    )
)

(define-private (is-authorized-carrier (carrier principal))
    (match (map-get? authorized-carriers carrier)
        carrier-data (get is-active carrier-data)
        false
    )
)

;; Public Functions

;; Register a new carrier
(define-public (register-carrier (name (string-ascii 100)))
    (begin
        (asserts! (is-none (map-get? authorized-carriers tx-sender)) err-already-exists)
        (ok (map-set authorized-carriers tx-sender {
            name: name,
            rating: u100,
            total-deliveries: u0,
            is-active: true
        }))
    )
)

;; Create a new shipment
(define-public (create-shipment 
    (receiver principal)
    (origin (string-ascii 100))
    (destination (string-ascii 100))
    (escrow-amount uint)
    (insurance-premium uint))
    (let
        (
            (shipment-id (+ (var-get shipment-nonce) u1))
            (carbon-credits (/ escrow-amount u1000))
        )
        (asserts! (> escrow-amount u0) err-invalid-parameters)
        (try! (stx-transfer? (+ escrow-amount insurance-premium) tx-sender (as-contract tx-sender)))
        
        (map-set shipments shipment-id {
            shipper: tx-sender,
            receiver: receiver,
            carrier: none,
            origin: origin,
            destination: destination,
            status: status-created,
            escrow-amount: escrow-amount,
            insurance-premium: insurance-premium,
            carbon-credits: carbon-credits,
            created-at: block-height,
            delivered-at: none,
            current-route: u0
        })
        
        (var-set shipment-nonce shipment-id)
        (ok shipment-id)
    )
)

;; Assign a carrier to a shipment
(define-public (assign-carrier (shipment-id uint) (carrier principal))
    (let
        (
            (shipment (unwrap! (map-get? shipments shipment-id) err-not-found))
        )
        (asserts! (is-eq (get shipper shipment) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status shipment) status-created) err-invalid-status)
        (asserts! (is-authorized-carrier carrier) err-unauthorized)
        
        (ok (map-set shipments shipment-id
            (merge shipment { carrier: (some carrier), status: status-in-transit })
        ))
    )
)

;; Create an alternative route
(define-public (create-route
    (shipment-id uint)
    (route-name (string-ascii 100))
    (estimated-duration uint)
    (cost uint)
    (carbon-footprint uint)
    (risk-score uint))
    (let
        (
            (route-id (+ (var-get route-nonce) u1))
            (shipment (unwrap! (map-get? shipments shipment-id) err-not-found))
        )
        (asserts! (is-shipment-owner shipment-id tx-sender) err-unauthorized)
        
        (map-set routes route-id {
            shipment-id: shipment-id,
            route-name: route-name,
            estimated-duration: estimated-duration,
            cost: cost,
            carbon-footprint: carbon-footprint,
            risk-score: risk-score,
            is-active: false,
            activated-at: none
        })
        
        (var-set route-nonce route-id)
        (ok route-id)
    )
)

;; Activate an alternative route
(define-public (activate-route (route-id uint))
    (let
        (
            (route (unwrap! (map-get? routes route-id) err-not-found))
            (shipment (unwrap! (map-get? shipments (get shipment-id route)) err-not-found))
        )
        (asserts! (is-shipment-owner (get shipment-id route) tx-sender) err-unauthorized)
        
        (map-set routes route-id
            (merge route { is-active: true, activated-at: (some block-height) })
        )
        
        (ok (map-set shipments (get shipment-id route)
            (merge shipment { current-route: route-id })
        ))
    )
)

;; Update shipment status
(define-public (update-status (shipment-id uint) (new-status uint))
    (let
        (
            (shipment (unwrap! (map-get? shipments shipment-id) err-not-found))
        )
        (asserts! (or (is-eq tx-sender (get shipper shipment))
                     (is-eq (some tx-sender) (get carrier shipment)))
                 err-unauthorized)
        (asserts! (<= new-status status-cancelled) err-invalid-parameters)
        
        (ok (map-set shipments shipment-id
            (merge shipment { status: new-status })
        ))
    )
)

;; Submit quality assessment
(define-public (submit-quality-assessment
    (shipment-id uint)
    (quality-score uint)
    (temperature-compliance bool)
    (damage-reported bool))
    (let
        (
            (shipment (unwrap! (map-get? shipments shipment-id) err-not-found))
        )
        (asserts! (is-eq (get receiver shipment) tx-sender) err-unauthorized)
        (asserts! (<= quality-score u100) err-invalid-parameters)
        
        (map-set quality-assessments shipment-id {
            shipment-id: shipment-id,
            assessor: tx-sender,
            quality-score: quality-score,
            temperature-compliance: temperature-compliance,
            damage-reported: damage-reported,
            assessed-at: block-height
        })
        
        (ok true)
    )
)

;; Complete delivery and release payment
(define-public (complete-delivery (shipment-id uint))
    (let
        (
            (shipment (unwrap! (map-get? shipments shipment-id) err-not-found))
            (carrier-principal (unwrap! (get carrier shipment) err-not-found))
            (quality-check (map-get? quality-assessments shipment-id))
        )
        (asserts! (is-eq (some tx-sender) (get carrier shipment)) err-unauthorized)
        (asserts! (is-eq (get status shipment) status-in-transit) err-invalid-status)
        
        ;; Calculate payment based on quality assessment
        (let
            (
                (payment-amount (match quality-check
                    assessment (if (and (>= (get quality-score assessment) u80)
                                       (not (get damage-reported assessment)))
                                  (get escrow-amount shipment)
                                  (/ (* (get escrow-amount shipment) u90) u100))
                    (get escrow-amount shipment)
                ))
            )
            ;; Transfer payment to carrier
            (try! (as-contract (stx-transfer? payment-amount tx-sender carrier-principal)))
            
            ;; Return insurance premium to shipper if quality is good
            (match quality-check
                assessment (if (>= (get quality-score assessment) u90)
                              (try! (as-contract (stx-transfer? (get insurance-premium shipment) 
                                                                tx-sender 
                                                                (get shipper shipment))))
                              true)
                true
            )
            
            ;; Update carrier stats
            (match (map-get? authorized-carriers carrier-principal)
                carrier-data (map-set authorized-carriers carrier-principal
                    (merge carrier-data { 
                        total-deliveries: (+ (get total-deliveries carrier-data) u1)
                    }))
                false
            )
            
            ;; Update shipment
            (map-set shipments shipment-id
                (merge shipment { 
                    status: status-delivered,
                    delivered-at: (some block-height)
                })
            )
            
            (ok payment-amount)
        )
    )
)

;; Add or update compliance zone
(define-public (update-compliance-zone
    (zone-id (string-ascii 50))
    (region (string-ascii 100))
    (requirements (string-ascii 500)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (ok (map-set compliance-zones zone-id {
            region: region,
            requirements: requirements,
            last-updated: block-height,
            is-active: true
        }))
    )
)

;; Read-only functions

(define-read-only (get-shipment (shipment-id uint))
    (ok (map-get? shipments shipment-id))
)

(define-read-only (get-route (route-id uint))
    (ok (map-get? routes route-id))
)

(define-read-only (get-compliance-zone (zone-id (string-ascii 50)))
    (ok (map-get? compliance-zones zone-id))
)

(define-read-only (get-carrier-info (carrier principal))
    (ok (map-get? authorized-carriers carrier))
)

(define-read-only (get-quality-assessment (shipment-id uint))
    (ok (map-get? quality-assessments shipment-id))
)

(define-read-only (get-shipment-nonce)
    (ok (var-get shipment-nonce))
)

(define-read-only (get-route-nonce)
    (ok (var-get route-nonce))
)
