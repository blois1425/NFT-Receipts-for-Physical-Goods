(define-non-fungible-token receipt-nft uint)

(define-data-var last-token-id uint u0)
(define-data-var contract-owner principal tx-sender)

(define-map receipts
    { token-id: uint }
    {
        retailer: principal,
        customer: principal,
        product-name: (string-ascii 100),
        product-category: (string-ascii 50),
        purchase-price: uint,
        purchase-date: uint,
        warranty-period: uint,
        product-serial: (string-ascii 50),
        is-transferable: bool,
        metadata-uri: (string-ascii 200),
    }
)

(define-map authorized-retailers
    principal
    bool
)

(define-map retailer-profiles
    { retailer: principal }
    {
        business-name: (string-ascii 100),
        verification-status: bool,
        total-receipts-issued: uint,
        reputation-score: uint,
    }
)

(define-map warranty-claims
    {
        token-id: uint,
        claim-id: uint,
    }
    {
        claimant: principal,
        claim-date: uint,
        claim-type: (string-ascii 50),
        status: (string-ascii 20),
        resolution-date: (optional uint),
    }
)

(define-data-var last-claim-id uint u0)

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-NOT-OWNER (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-WARRANTY-EXPIRED (err u104))
(define-constant ERR-NOT-TRANSFERABLE (err u105))
(define-constant ERR-ALREADY-CLAIMED (err u106))

(define-read-only (get-last-token-id)
    (var-get last-token-id)
)

(define-read-only (get-receipt-data (token-id uint))
    (map-get? receipts { token-id: token-id })
)

(define-read-only (get-retailer-profile (retailer principal))
    (map-get? retailer-profiles { retailer: retailer })
)

(define-read-only (is-authorized-retailer (retailer principal))
    (default-to false (map-get? authorized-retailers retailer))
)

(define-read-only (get-warranty-status (token-id uint))
    (match (map-get? receipts { token-id: token-id })
        receipt-data (let ((expiry-block (+ (get purchase-date receipt-data)
                (get warranty-period receipt-data)
            )))
            (ok {
                is-valid: (< stacks-block-height expiry-block),
                expiry-block: expiry-block,
                blocks-remaining: (if (< stacks-block-height expiry-block)
                    (- expiry-block stacks-block-height)
                    u0
                ),
            })
        )
        ERR-NOT-FOUND
    )
)

(define-read-only (get-warranty-claim
        (token-id uint)
        (claim-id uint)
    )
    (map-get? warranty-claims {
        token-id: token-id,
        claim-id: claim-id,
    })
)

(define-public (authorize-retailer
        (retailer principal)
        (business-name (string-ascii 100))
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> (len business-name) u0) ERR-INVALID-INPUT)
        (map-set authorized-retailers retailer true)
        (map-set retailer-profiles { retailer: retailer } {
            business-name: business-name,
            verification-status: true,
            total-receipts-issued: u0,
            reputation-score: u100,
        })
        (ok true)
    )
)

(define-public (revoke-retailer-authorization (retailer principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-delete authorized-retailers retailer)
        (match (map-get? retailer-profiles { retailer: retailer })
            profile (map-set retailer-profiles { retailer: retailer }
                (merge profile { verification-status: false })
            )
            true
        )
        (ok true)
    )
)

(define-public (issue-receipt
        (customer principal)
        (product-name (string-ascii 100))
        (product-category (string-ascii 50))
        (purchase-price uint)
        (warranty-blocks uint)
        (product-serial (string-ascii 50))
        (is-transferable bool)
        (metadata-uri (string-ascii 200))
    )
    (let ((token-id (+ (var-get last-token-id) u1)))
        (asserts! (is-authorized-retailer tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> (len product-name) u0) ERR-INVALID-INPUT)
        (asserts! (> (len product-category) u0) ERR-INVALID-INPUT)
        (asserts! (> purchase-price u0) ERR-INVALID-INPUT)
        (asserts! (> warranty-blocks u0) ERR-INVALID-INPUT)
        (try! (nft-mint? receipt-nft token-id customer))
        (map-set receipts { token-id: token-id } {
            retailer: tx-sender,
            customer: customer,
            product-name: product-name,
            product-category: product-category,
            purchase-price: purchase-price,
            purchase-date: stacks-block-height,
            warranty-period: warranty-blocks,
            product-serial: product-serial,
            is-transferable: is-transferable,
            metadata-uri: metadata-uri,
        })
        (var-set last-token-id token-id)
        (match (map-get? retailer-profiles { retailer: tx-sender })
            profile (map-set retailer-profiles { retailer: tx-sender }
                (merge profile { total-receipts-issued: (+ (get total-receipts-issued profile) u1) })
            )
            true
        )
        (ok token-id)
    )
)

(define-public (transfer-receipt
        (token-id uint)
        (sender principal)
        (recipient principal)
    )
    (match (map-get? receipts { token-id: token-id })
        receipt-data (begin
            (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
            (asserts! (get is-transferable receipt-data) ERR-NOT-TRANSFERABLE)
            (try! (nft-transfer? receipt-nft token-id sender recipient))
            (map-set receipts { token-id: token-id }
                (merge receipt-data { customer: recipient })
            )
            (ok true)
        )
        ERR-NOT-FOUND
    )
)

(define-public (file-warranty-claim
        (token-id uint)
        (claim-type (string-ascii 50))
    )
    (let ((claim-id (+ (var-get last-claim-id) u1)))
        (match (map-get? receipts { token-id: token-id })
            receipt-data (begin
                (asserts! (is-eq tx-sender (get customer receipt-data))
                    ERR-NOT-OWNER
                )
                (asserts! (> (len claim-type) u0) ERR-INVALID-INPUT)
                (match (get-warranty-status token-id)
                    warranty-status (begin
                        (asserts! (get is-valid warranty-status)
                            ERR-WARRANTY-EXPIRED
                        )
                        (map-set warranty-claims {
                            token-id: token-id,
                            claim-id: claim-id,
                        } {
                            claimant: tx-sender,
                            claim-date: stacks-block-height,
                            claim-type: claim-type,
                            status: "pending",
                            resolution-date: none,
                        })
                        (var-set last-claim-id claim-id)
                        (ok claim-id)
                    )
                    err-code (err err-code)
                )
            )
            ERR-NOT-FOUND
        )
    )
)
(define-public (resolve-warranty-claim
        (token-id uint)
        (claim-id uint)
        (resolution-status (string-ascii 20))
    )
    (match (map-get? receipts { token-id: token-id })
        receipt-data (match (map-get? warranty-claims {
            token-id: token-id,
            claim-id: claim-id,
        })
            claim-data (begin
                (asserts! (is-eq tx-sender (get retailer receipt-data))
                    ERR-NOT-AUTHORIZED
                )
                (asserts! (> (len resolution-status) u0) ERR-INVALID-INPUT)
                (map-set warranty-claims {
                    token-id: token-id,
                    claim-id: claim-id,
                }
                    (merge claim-data {
                        status: resolution-status,
                        resolution-date: (some stacks-block-height),
                    })
                )
                (ok true)
            )
            ERR-NOT-FOUND
        )
        ERR-NOT-FOUND
    )
)

(define-public (update-retailer-reputation
        (retailer principal)
        (new-score uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-score u100) ERR-INVALID-INPUT)
        (match (map-get? retailer-profiles { retailer: retailer })
            profile (begin
                (map-set retailer-profiles { retailer: retailer }
                    (merge profile { reputation-score: new-score })
                )
                (ok true)
            )
            ERR-NOT-FOUND
        )
    )
)

(define-read-only (get-receipts-by-customer (customer principal))
    (ok customer)
)

(define-read-only (validate-receipt-authenticity (token-id uint))
    (match (map-get? receipts { token-id: token-id })
        receipt-data (ok {
            is-authentic: true,
            retailer: (get retailer receipt-data),
            issue-date: (get purchase-date receipt-data),
            verified: (is-authorized-retailer (get retailer receipt-data)),
        })
        ERR-NOT-FOUND
    )
)
