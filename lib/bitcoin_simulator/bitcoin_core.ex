defmodule BitcoinSimulator.BitcoinCore do

  alias BitcoinSimulator.BitcoinCore.{Blockchain, Mining, Network, RawTransaction, Wallet}

  # Block Chain

  def get_new_blockchain, do: Blockchain.get_new_blockchain()

  def get_best_block_hash(blockchain), do: Blockchain.get_best_block_hash(blockchain)

  def block_header_hash(header), do: Blockchain.block_header_hash(header)

  def transaction_hash(tx), do: Blockchain.transaction_hash(tx)

  def verify_block(blockchain, block), do: Blockchain.verify_block(blockchain, block)

  def verify_transaction(blockchain, tx), do: Blockchain.verify_transaction(blockchain, tx)

  def add_block(block, blockchain, wallet, mempool, mining_process \\ nil, mining_txs \\ nil) do
    Blockchain.add_block(block, blockchain, wallet, mempool, mining_process, mining_txs)
  end

  # Mining

  def get_new_mempool, do: Mining.get_new_mempool()

  def get_top_unconfirmed_transactions(mempool), do: Mining.get_top_unconfirmed_transactions(mempool)

  def get_block_template(prev_hash, txs), do: Mining.get_block_template(prev_hash, txs)

  def mine(block, coinbase_addr, self_id), do: Mining.mine(block, coinbase_addr, self_id)

  def add_unconfirmed_tx(mempool, tx, tx_hash), do: Mining.add_unconfirmed_tx(mempool, tx, tx_hash)

  def calc_cainbase_value(blockchain, txs), do: Mining.calc_cainbase_value(blockchain, txs)

  # Network

  def get_new_message_record, do: %Network.MessageRecord{}

  def get_initial_neighbors(id), do: Network.get_initial_neighbors(id)

  def get_initial_blockchain(neighbors), do: Network.get_initial_blockchain(neighbors)

  def exchange_neighbors(neighbors), do: Network.exchange_neighbors(neighbors)

  def mix_neighbors(neighbors, self_id), do: Network.mix_neighbors(neighbors, self_id)

  def message_seen?(record, type, hash), do: Network.message_seen?(record, type, hash)

  def saw_message(record, type, hash), do: Network.saw_message(record, type, hash)

  def clean_message_record(record), do: Network.clean_message_record(record)

  def broadcast_message(type, message, neighbors, sender), do: Network.broadcast_message(type, message, neighbors, sender)

  # Raw Transaction

  def create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value) do
    RawTransaction.create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value)
  end

  def create_coinbase_transaction(out_addresses, out_values), do: RawTransaction.create_coinbase_transaction(out_addresses, out_values)

  # Wallet

  def get_new_wallet, do: Wallet.get_new_wallet()

  def get_new_address(wallet), do: Wallet.get_new_address(wallet)

  def combine_unspent_addresses(wallet, target_value), do: Wallet.combine_unspent_addresses(wallet, target_value)

  def spend_address(wallet, address), do: Wallet.spend_address(wallet, address)

  def import_address(wallet, address), do: Wallet.import_address(wallet, address)

end
