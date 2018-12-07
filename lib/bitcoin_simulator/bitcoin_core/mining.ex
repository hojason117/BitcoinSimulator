defmodule BitcoinSimulator.BitcoinCore.Mining do
  use Timex

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Simulation.Param
  alias BitcoinSimulator.Const

  defmodule MemPool do
    defstruct [
      unconfirmed_txs: Map.new()
    ]
  end

  # APIs

  def get_new_mempool, do: %MemPool{}

  def get_top_unconfirmed_transactions(mempool) do
    transactions = Map.values(mempool.unconfirmed_txs)
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    sorted_txs = sort_unconfirmed_transactions(transactions)
    Enum.take(sorted_txs, max_transaction_per_block)
  end

  def get_block_template(prev_hash, txs) do
    %Blockchain.Block{
      header: %Blockchain.BlockHeader{
        previous_block_hash: prev_hash,
        merkle_root_hash: Blockchain.merkle_root(txs),
        n_bits: GenServer.call(Param, {:get_param, :target_difficulty_bits}),
      },
      transactions: txs
    }
  end

  def mine(block, coinbase_addr, self_id) do
    Process.flag(:priority, :low)
    mined_block_header = mine_helper(block.header, 0)
    mined_block = %{block | header: mined_block_header}
    GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{self_id}"}}, {:block_mined, mined_block, coinbase_addr})
  end

  def add_unconfirmed_tx(mempool, tx, tx_hash) do
    new_unconfirmed_txs = Map.put(mempool.unconfirmed_txs, tx_hash, tx)
    %{mempool | unconfirmed_txs: new_unconfirmed_txs}
  end

  def calc_cainbase_value(blockchain, txs) do
    fee = Enum.reduce(txs, 0.0, fn(x, acc) -> acc + calc_transaction_fee(blockchain, x) end)
    (fee + Const.decode(:block_reward)) |> Float.round(Const.decode(:transaction_value_precision))
  end

  # Aux

  def match_leading_zeros?(hash, difficulty) do
    remain = Const.decode(:hash_digest) - difficulty
    <<n::size(difficulty), _::size(remain)>> = hash
    n == 0
  end

  def clean_unconfirmed_txs(tx_hashes, mempool) do
    new_unconfirmed_txs = Enum.reduce(MapSet.to_list(tx_hashes), mempool.unconfirmed_txs, fn(x, acc) -> Map.delete(acc, x) end)
    %{mempool | unconfirmed_txs: new_unconfirmed_txs}
  end

  defp sort_unconfirmed_transactions(txs), do: Enum.sort(txs, fn(a, b) -> Timex.compare(a.time, b.time) == -1 end)

  defp mine_helper(header, nonce) do
    filled_header = %{header | time: Timex.now(), nonce: nonce}
    hash = Blockchain.block_header_hash(filled_header)
    if match_leading_zeros?(hash, GenServer.call(Param, {:get_param, :target_difficulty_bits})), do: filled_header, else: mine_helper(header, nonce + 1)
  end

  defp calc_transaction_fee(blockchain, tx) do
    total_in = Enum.reduce(tx.tx_in, 0.0, fn(x, acc) -> acc + blockchain.unspent_txout[x.previous_output].value end) |> Float.round(Const.decode(:transaction_value_precision))
    total_out = Enum.reduce(tx.tx_out, 0.0, fn(x, acc) -> acc + x.value end) |> Float.round(Const.decode(:transaction_value_precision))
    (total_in - total_out) |> Float.round(Const.decode(:transaction_value_precision))
  end

end
