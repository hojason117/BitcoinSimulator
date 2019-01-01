# BitcoinSimulator

Implemented a cryptocurrency following a simplified version of Bitcoin protocol.

Features including:

* Blockchain
  * Distributed transaction ledger
* Block
  * Previous block hash
  * Merkle tree root
  * Nonce
  * Timestamp
* Transaction
  * Signature (ECDSA)
  * Multiple input and output
  * Transaction fee
* Mining
  * Hash-based Proof-of-Work system
  * Difficulty
  * Mempool (unconfirmed transactions)
  * Coinbase transaction
* Wallet
  * Address (hashed public key)
    * Public key
    * Private key
* Concensus
  * Block verification
  * Transaction verification
* Peer-to-Peer Network
  * Peers join and leave arbitrarily
  * Periodically exchange neighbors
  * Controlled message flooding
* Simulation
  * Web UI
    * Dashboard
    * Parameter configurations

## Web UI Simulator

Defaults to 100 peers (80 traders and 10 miners), and each trader creates a transaction every 2~4 seconds.

To start the web UI simulator:

* Install dependencies with `mix deps.get`
* Install Node.js dependencies with `cd assets && npm install`
* Start Phoenix endpoint with `mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

## CLI Sample Output

``` sh
...
[info] Peer joined [id: 4548]
[info] Peer joined [id: 9809]
...
[info] Block mined [transaction count: 1]
[info] Block mined [transaction count: 59]
...
```

## Testing

To run available tests:

* `mix test`

To run tests with detailed reporting:

* `mix test --trace`

To see test coverage:

* `mix test --cover`
