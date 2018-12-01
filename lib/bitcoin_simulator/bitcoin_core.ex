defmodule BitcoinSimulator.BitcoinCore do

  alias BitcoinSimulator.BitcoinCore.{Blockchain, Mining, Network, RawTransaction, Wallet}

  # Block Chain

  def get_new_blockchain, do: Blockchain.get_new_blockchain()

  def get_best_block_hash(blockchain), do: Blockchain.get_best_block_hash(blockchain)

  def block_header_hash(header), do: Blockchain.block_header_hash(header)

  def transaction_hash(tx), do: Blockchain.transaction_hash(tx)

  def verify_block?(block), do: Blockchain.verify_block?(block)

  def verify_transaction?(tx), do: Blockchain.verify_transaction?(tx)

  def merkle_root(transactions), do: Blockchain.merkle_root(transactions)

  # Mining

  def get_new_mempool, do: Mining.get_new_mempool()

  def get_top_unconfirmed_transactions(mempool), do: Mining.get_top_unconfirmed_transactions(mempool)

  def get_block_template(prev_hash, txs), do: Mining.get_block_template(prev_hash, txs)

  def mine(block, self_id), do: Mining.mine(block, self_id)

  def add_unconfirmed_tx(mempool, tx, tx_hash), do: Mining.add_unconfirmed_tx(mempool, tx, tx_hash)

  # Network

  def get_new_message_record, do: %Network.MessageRecord{}

  def get_initial_neighbors(id), do: Network.get_initial_neighbors(id)

  def exchange_neighbors(neighbors), do: Network.exchange_neighbors(neighbors)

  def mix_neighbors(neighbors, self_id), do: Network.mix_neighbors(neighbors, self_id)

  def messageSeen?(record, type, hash), do: Network.messageSeen?(record, type, hash)

  def sawMessage(record, type, hash), do: Network.sawMessage(record, type, hash)

  def cleanMessageRecord(record), do: Network.cleanMessageRecord(record)

  def broadcast_message(type, message, neighbors), do: Network.broadcast_message(type, message, neighbors)

  # Raw Transaction

  def create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value) do
    RawTransaction.create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value)
  end

  # Wallet

  def get_new_wallet, do: Wallet.get_new_wallet()

  def get_new_address(wallet), do: Wallet.get_new_address(wallet)

  def combine_unspent_addresses(wallet, target_value), do: Wallet.combine_unspent_addresses(wallet, target_value)

  def spend_address(wallet, address), do: Wallet.spend_address(wallet, address)

end
