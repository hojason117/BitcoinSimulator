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

### Test Detail

* Blockchain
  * "get new blockchain": Get an empty blockchain with only genesis block and check its correctness.
  * "block header hash": Check the correctness of the block hash.
  * "transaction hash": Check the correctness of the transaction hash.
  * "merkle root": Check the correctness of a merkle root hash.
  * "update unspent txout": Upon receiving a block, a peer should update its copy of unspent txouts currently in the network(i.e. remove spent txouts and add new txouts appeared in this block). Check the correctness of this behaviour.
* Network
  * "message seen?": Controlled flooding, check if this message has been received before. Check the correctness of the message record.
  * "saw message": Controlled flooding, mark the message as seen. Make sure the message record has been updated.
* Wallet
  * "update address detail": Upon receiving a block, a peer should go through all the txouts in the block and add value to its wallet if the address in the txout belongs to it.  Check the correctness of this behaviour.
* Mining
  * "add unconfirmed transaction": Upon receiving a transaction, a peer should add this transaction to the mempool if verified. Make sure the transaction is added to the mempool.
  * "get top unconfirmed transactions": Get transactions to be put into a block. Make sure it returns all availble transactions in the mempool, if not exceeding the maximum block size.
  * "get top unconfirmed transactions - mempool larger than max transaction per block": Make sure the number of resulting transactions does not exceed the maximum transaction count per block.
  * "get top unconfirmed transactions - order": Check the order of the resulting transactions, which should be in timestamp ascending order.
  * "get block template": Get an emply block with only previous block hash to be populated with transactions. Check the correctness of the empty block.
  * "match leading zeros?": Check if a given block hash meets the mining difficulty. Check the correctness of this verification.
* Node
  * "exchange neighbors": Test exchange neighbor mechanism. Make sure the neighbor set is different after exchanging neighbors.
  * "exchange neighbors result doesn't contain self id": Make sure the neighbor set does not contain self.
  * "random transaction value": Generate a random transaction value, including transaction value, transaction fee and remaining value. Make sure transaction value, transaction fee and remaining value sums up to the original value, so no bitcoins are lost in the process.
* TradeCenter
  * "get random trade partner": Get random peers to start transaction. The resulting peers should not contain self.
