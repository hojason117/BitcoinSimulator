defmodule BitcoinSimulator.Const do
  values = [
    hash_func: :sha256,
    hash_digest: 256,
    max_total_peer: 10000,
    peer_id_range: 100000,
    neighbor_count: 10,
    default_peer_count: 100,
    default_trader_percentage: 80,
    default_miner_percentage: 10,
    exchange_neighbors_interval: 5000,
    peer_initiate_auto_trading_after: 5000,
    peer_initiate_mining_after: 10000,
    peer_auto_trading_interval_range_min: 1000,
    peer_auto_trading_interval_range_max: 2000,
    txout_count_range: 3,
    transaction_value_precision: 3,
    transaction_fee_percentage_range: 20,
    max_transaction_per_block: 300,
    target_difficulty_bits: 16,
    block_reward: 25.0,
    network_message_record_ttl: 10000,
    confirmation_count: 6
  ]

  for {key, value} <- values do
    # def encode(unquote(value)), do: unquote(key)
    def decode(unquote(key)),   do: unquote(value)
  end
end
