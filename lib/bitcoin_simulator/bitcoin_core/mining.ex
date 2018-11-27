defmodule BitcoinSimulator.BitcoinCore.Mining do
  use Timex

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Const

  defmodule MemPool do
    defstruct [
      unconfirmed_txs: %{}
    ]
  end

  # APIs

  def submitBlock(block) do
    if Blockchain.verify_block?(block) do
      IO.inspect(Blockchain.block_header_hash(block.header))
      IO.inspect(block)
    else

    end
  end

  # Aux

  def add_unconfirmed_tx(mempool, tx, tx_hash) do
    new_unconfirmed_txs = Map.put(mempool.unconfirmed_txs, tx_hash, tx)
    %{mempool | unconfirmed_txs: new_unconfirmed_txs}
  end

  def get_top_unconfirmed_transactions(transactions) do
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    sorted_txs = sort_unconfirmed_transactions(transactions)
    Enum.take(sorted_txs, max_transaction_per_block)
  end

  def mine(block, self_id) do
    mined_block_header = mine_helper(block.header, 0)
    mined_block = %{block | header: mined_block_header}
    GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{self_id}"}}, {:block_mined, mined_block})
  end

  defp sort_unconfirmed_transactions(txs), do: Enum.sort(txs, fn(a, b) -> Timex.compare(a.time, b.time) == -1 end)

  defp mine_helper(header, nonce) do
    filled_header = %{header | time: Timex.now(), nonce: nonce}
    hash = Blockchain.block_header_hash(filled_header)
    if match_leading_zeros?(hash, Const.decode(:target_difficulty)), do: filled_header, else: mine_helper(header, nonce + 1)
  end

  defp match_leading_zeros?(hash, difficulty) do
    leading_zeros = difficulty * 8
    binary_part(hash, 0, difficulty) == <<0::size(leading_zeros)>>
  end

end
