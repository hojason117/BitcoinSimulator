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
  * Coinbase transaction (**_Not yet implemented_**)
* Wallet
  * Address (hashed public key)
    * Public key
    * Private key
* Concensus
  * Block verification (**_Not yet implemented_**)
  * Transaction verification (**_Not yet implemented_**)
* Peer-to-Peer Network
  * Peers join and leave arbitrarily
  * Periodically exchange neighbors
  * Controlled Message flooding
* Simulation
  * Web UI (**_Not yet implemented_**)

## Web UI Simulator

Defaults to 100 peers (80 traders and 10 miners), and each trader creates a transaction every 3~5 seconds.

To start the web UI simulator:

* Install dependencies with `mix deps.get`
* Install Node.js dependencies with `cd assets && npm install`
* Start Phoenix endpoint with `mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

## CLI Sample Output

`...`  
`[info] Peer joined [id: 4548]`  
`[info] Peer joined [id: 9809]`  
`...`  
`[info] Block mined [transaction count: 0,hash: <<0, 0, 249, 244, 148, 208, 121, 200, 56, 134, 124, 60, 254, 179, 170, 165, 226, 41, 82, 46, 16, 238, 49, 82, 151, 128, 86, 217, 183, 63, 127, 152>>]`  
`[info] Block mined [transaction count: 59,hash: <<0, 0, 58, 180, 181, 14, 221, 95, 218, 246, 155, 28, 170, 112, 171, 195, 155, 220, 1, 229, 221, 17, 107, 190, 107, 20, 204, 99, 125, 227, 13, 108>>]`  
`...`

## Testing

To run available tests:

* `mix test`

To run tests with detailed reporting:

* `mix test --trace`

To see test coverage:

* `mix test --cover`

### Test Detail

* Monitor
  * "did start peers": Make sure all the peers are started.

* Network
  * "message seen?": Controlled flooding.
  * "saw message": Controlled flooding.

* Blockchain
  * "transaction hash": Test the correctness of a transaction hash.
  * "merkle root": Test the correctness of a merkle root hash.
  * "merkle root - empty transactions": No transactions for current block.
  * "double hash": Test the correctness of double SHA256 hash.

* Mining
  * "get top unconfirmed transactions": Get transactions to put in block.
  * "get top unconfirmed transactions - mempool larger than max transaction per block": Maximum transaction count per block.
  * "get top unconfirmed transactions - order": Check the order of unconfirmed transactions.
  * "add unconfirmed transaction": Add transactions to mempool.

* Peer
  * "exchange neighbors": Test exchange neighbor mechanism.
  * "exchange neighbors result doesn't contain self id": Neighbors should  not contain self.

* TradeCenter
  * "get random trade partner": Get random peers to start transaction.
  * "get random trade partner - not enough partner": Get random peers to start transaction.

<!-- * Create and migrate your database with `mix ecto.setup` -->
