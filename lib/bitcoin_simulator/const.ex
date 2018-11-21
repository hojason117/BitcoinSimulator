defmodule BitcoinSimulator.Const do
  values = [
    hash_func: :sha256,
    hash_digest: 256,
    max_total_peer: 100000,
    neighbor_count: 10,
    initial_peer_count: 100,
    initial_miner_percentage: 10
  ]

  for {key, value} <- values do
    # def encode(unquote(value)), do: unquote(key)
    def decode(unquote(key)),   do: unquote(value)
  end
end
