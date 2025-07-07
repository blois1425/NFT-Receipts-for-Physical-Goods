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
(define-public (issue-bulk-receipts
        (customers (list 20 principal))
        (product-names (list 20 (string-ascii 100)))
        (product-categories (list 20 (string-ascii 50)))
        (purchase-prices (list 20 uint))
        (warranty-blocks-list (list 20 uint))
        (product-serials (list 20 (string-ascii 50)))
        (transferable-flags (list 20 bool))
        (metadata-uris (list 20 (string-ascii 200)))
    )
    (let (
            (customers-len (len customers))
            (names-len (len product-names))
            (categories-len (len product-categories))
            (prices-len (len purchase-prices))
            (warranty-len (len warranty-blocks-list))
            (serials-len (len product-serials))
            (flags-len (len transferable-flags))
            (uris-len (len metadata-uris))
        )
        (asserts! (is-authorized-retailer tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq customers-len names-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len categories-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len prices-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len warranty-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len serials-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len flags-len) ERR-INVALID-INPUT)
        (asserts! (is-eq customers-len uris-len) ERR-INVALID-INPUT)
        (asserts! (> customers-len u0) ERR-INVALID-INPUT)
        (fold process-bulk-receipt
            (map create-receipt-data customers product-names product-categories
                purchase-prices warranty-blocks-list product-serials
                transferable-flags metadata-uris
            )
            (ok (list))
        )
    )
)

(define-private (create-receipt-data
        (customer principal)
        (product-name (string-ascii 100))
        (product-category (string-ascii 50))
        (purchase-price uint)
        (warranty-blocks uint)
        (product-serial (string-ascii 50))
        (is-transferable bool)
        (metadata-uri (string-ascii 200))
    )
    {
        customer: customer,
        product-name: product-name,
        product-category: product-category,
        purchase-price: purchase-price,
        warranty-blocks: warranty-blocks,
        product-serial: product-serial,
        is-transferable: is-transferable,
        metadata-uri: metadata-uri,
    }
)

(define-private (process-bulk-receipt
        (receipt-data {
            customer: principal,
            product-name: (string-ascii 100),
            product-category: (string-ascii 50),
            purchase-price: uint,
            warranty-blocks: uint,
            product-serial: (string-ascii 50),
            is-transferable: bool,
            metadata-uri: (string-ascii 200),
        })
        (acc (response (list 20 uint) uint))
    )
    (match acc
        success-list (let ((token-id (+ (var-get last-token-id) u1)))
            (asserts! (> (len (get product-name receipt-data)) u0)
                ERR-INVALID-INPUT
            )
            (asserts! (> (len (get product-category receipt-data)) u0)
                ERR-INVALID-INPUT
            )
            (asserts! (> (get purchase-price receipt-data) u0) ERR-INVALID-INPUT)
            (asserts! (> (get warranty-blocks receipt-data) u0) ERR-INVALID-INPUT)
            (try! (nft-mint? receipt-nft token-id (get customer receipt-data)))
            (map-set receipts { token-id: token-id } {
                retailer: tx-sender,
                customer: (get customer receipt-data),
                product-name: (get product-name receipt-data),
                product-category: (get product-category receipt-data),
                purchase-price: (get purchase-price receipt-data),
                purchase-date: stacks-block-height,
                warranty-period: (get warranty-blocks receipt-data),
                product-serial: (get product-serial receipt-data),
                is-transferable: (get is-transferable receipt-data),
                metadata-uri: (get metadata-uri receipt-data),
            })
            (var-set last-token-id token-id)
            (match (map-get? retailer-profiles { retailer: tx-sender })
                profile (map-set retailer-profiles { retailer: tx-sender }
                    (merge profile { total-receipts-issued: (+ (get total-receipts-issued profile) u1) })
                )
                true
            )
            (ok (unwrap-panic (as-max-len? (append success-list token-id) u20)))
        )
        error-code (err error-code)
    )
)

(define-public (bulk-transfer-receipts
        (token-ids (list 10 uint))
        (recipients (list 10 principal))
    )
    (let (
            (token-ids-len (len token-ids))
            (recipients-len (len recipients))
        )
        (asserts! (is-eq token-ids-len recipients-len) ERR-INVALID-INPUT)
        (asserts! (> token-ids-len u0) ERR-INVALID-INPUT)
        (fold process-bulk-transfer
            (map create-transfer-data token-ids recipients) (ok true)
        )
    )
)

(define-private (create-transfer-data
        (token-id uint)
        (recipient principal)
    )
    {
        token-id: token-id,
        recipient: recipient,
    }
)

(define-private (process-bulk-transfer
        (transfer-data {
            token-id: uint,
            recipient: principal,
        })
        (acc (response bool uint))
    )
    (match acc
        success (match (map-get? receipts { token-id: (get token-id transfer-data) })
            receipt-data (begin
                (asserts! (is-eq tx-sender (get customer receipt-data))
                    ERR-NOT-AUTHORIZED
                )
                (asserts! (get is-transferable receipt-data) ERR-NOT-TRANSFERABLE)
                (try! (nft-transfer? receipt-nft (get token-id transfer-data) tx-sender
                    (get recipient transfer-data)
                ))
                (map-set receipts { token-id: (get token-id transfer-data) }
                    (merge receipt-data { customer: (get recipient transfer-data) })
                )
                (ok true)
            )
            ERR-NOT-FOUND
        )
        error-code (err error-code)
    )
)
(define-map marketplace-listings
    { token-id: uint }
    {
        seller: principal,
        price: uint,
        listed-at: uint,
        is-active: bool,
    }
)

(define-map escrow-transactions
    { transaction-id: uint }
    {
        token-id: uint,
        seller: principal,
        buyer: principal,
        price: uint,
        created-at: uint,
        status: (string-ascii 20),
        seller-confirmed: bool,
        buyer-confirmed: bool,
    }
)

(define-data-var last-transaction-id uint u0)
(define-data-var marketplace-fee-rate uint u250)

(define-constant ERR-ALREADY-LISTED (err u107))
(define-constant ERR-NOT-LISTED (err u108))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u109))
(define-constant ERR-TRANSACTION-NOT-FOUND (err u110))
(define-constant ERR-INVALID-STATUS (err u111))

(define-public (list-receipt-for-sale
        (token-id uint)
        (price uint)
    )
    (match (map-get? receipts { token-id: token-id })
        receipt-data (begin
            (asserts! (is-eq tx-sender (get customer receipt-data)) ERR-NOT-OWNER)
            (asserts! (get is-transferable receipt-data) ERR-NOT-TRANSFERABLE)
            (asserts! (> price u0) ERR-INVALID-INPUT)
            (asserts!
                (is-none (map-get? marketplace-listings { token-id: token-id }))
                ERR-ALREADY-LISTED
            )
            (map-set marketplace-listings { token-id: token-id } {
                seller: tx-sender,
                price: price,
                listed-at: stacks-block-height,
                is-active: true,
            })
            (ok true)
        )
        ERR-NOT-FOUND
    )
)

(define-public (remove-listing (token-id uint))
    (match (map-get? marketplace-listings { token-id: token-id })
        listing-data (begin
            (asserts! (is-eq tx-sender (get seller listing-data))
                ERR-NOT-AUTHORIZED
            )
            (asserts! (get is-active listing-data) ERR-NOT-LISTED)
            (map-set marketplace-listings { token-id: token-id }
                (merge listing-data { is-active: false })
            )
            (ok true)
        )
        ERR-NOT-FOUND
    )
)

(define-public (initiate-purchase (token-id uint))
    (let ((transaction-id (+ (var-get last-transaction-id) u1)))
        (match (map-get? marketplace-listings { token-id: token-id })
            listing-data (match (map-get? receipts { token-id: token-id })
                receipt-data (begin
                    (asserts! (get is-active listing-data) ERR-NOT-LISTED)
                    (asserts! (not (is-eq tx-sender (get seller listing-data)))
                        ERR-NOT-AUTHORIZED
                    )
                    (asserts!
                        (>= (stx-get-balance tx-sender) (get price listing-data))
                        ERR-INSUFFICIENT-PAYMENT
                    )
                    (try! (stx-transfer? (get price listing-data) tx-sender
                        (as-contract tx-sender)
                    ))
                    (map-set escrow-transactions { transaction-id: transaction-id } {
                        token-id: token-id,
                        seller: (get seller listing-data),
                        buyer: tx-sender,
                        price: (get price listing-data),
                        created-at: stacks-block-height,
                        status: "pending",
                        seller-confirmed: false,
                        buyer-confirmed: false,
                    })
                    (map-set marketplace-listings { token-id: token-id }
                        (merge listing-data { is-active: false })
                    )
                    (var-set last-transaction-id transaction-id)
                    (ok transaction-id)
                )
                ERR-NOT-FOUND
            )
            ERR-NOT-FOUND
        )
    )
)

(define-public (confirm-transaction
        (transaction-id uint)
        (is-seller bool)
    )
    (match (map-get? escrow-transactions { transaction-id: transaction-id })
        transaction-data (let (
                (caller-is-seller (is-eq tx-sender (get seller transaction-data)))
                (caller-is-buyer (is-eq tx-sender (get buyer transaction-data)))
            )
            (asserts! (or caller-is-seller caller-is-buyer) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status transaction-data) "pending")
                ERR-INVALID-STATUS
            )
            (asserts! (is-eq is-seller caller-is-seller) ERR-NOT-AUTHORIZED)
            (let ((updated-transaction (if is-seller
                    (merge transaction-data { seller-confirmed: true })
                    (merge transaction-data { buyer-confirmed: true })
                )))
                (map-set escrow-transactions { transaction-id: transaction-id }
                    updated-transaction
                )
                (if (and (get seller-confirmed updated-transaction) (get buyer-confirmed updated-transaction))
                    (complete-transaction transaction-id)
                    (ok true)
                )
            )
        )
        ERR-TRANSACTION-NOT-FOUND
    )
)

(define-private (complete-transaction (transaction-id uint))
    (match (map-get? escrow-transactions { transaction-id: transaction-id })
        transaction-data (let (
                (marketplace-fee (/
                    (* (get price transaction-data)
                        (var-get marketplace-fee-rate)
                    )
                    u10000
                ))
                (seller-amount (- (get price transaction-data) marketplace-fee))
            )
            (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller transaction-data))))
            (try! (nft-transfer? receipt-nft (get token-id transaction-data)
                (get seller transaction-data) (get buyer transaction-data)
            ))
            (match (map-get? receipts { token-id: (get token-id transaction-data) })
                receipt-data (map-set receipts { token-id: (get token-id transaction-data) }
                    (merge receipt-data { customer: (get buyer transaction-data) })
                )
                true
            )
            (map-set escrow-transactions { transaction-id: transaction-id }
                (merge transaction-data { status: "completed" })
            )
            (ok true)
        )
        ERR-TRANSACTION-NOT-FOUND
    )
)

(define-public (cancel-transaction (transaction-id uint))
    (match (map-get? escrow-transactions { transaction-id: transaction-id })
        transaction-data (begin
            (asserts!
                (or
                    (is-eq tx-sender (get seller transaction-data))
                    (is-eq tx-sender (get buyer transaction-data))
                )
                ERR-NOT-AUTHORIZED
            )
            (asserts! (is-eq (get status transaction-data) "pending")
                ERR-INVALID-STATUS
            )
            (try! (as-contract (stx-transfer? (get price transaction-data) tx-sender
                (get buyer transaction-data)
            )))
            (map-set escrow-transactions { transaction-id: transaction-id }
                (merge transaction-data { status: "cancelled" })
            )
            (map-set marketplace-listings { token-id: (get token-id transaction-data) } {
                seller: (get seller transaction-data),
                price: (get price transaction-data),
                listed-at: (get created-at transaction-data),
                is-active: true,
            })
            (ok true)
        )
        ERR-TRANSACTION-NOT-FOUND
    )
)
