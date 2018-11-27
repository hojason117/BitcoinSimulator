defmodule BitcoinSimulator.BitcoinCore.MiningTest do
  use ExUnit.Case, async: true
  use Timex

  alias BitcoinSimulator.BitcoinCore.BlockchainServer
  alias BitcoinSimulator.BitcoinCore.Mining
  alias BitcoinSimulator.Const

  test "add unconfirmed transaction" do
    mempool = %Mining.MemPool{}
    assert Map.has_key?(mempool.unconfirmed_txs, :crypto.hash(:sha256, "test")) == false
    mempool = Mining.add_unconfirmed_tx(mempool, "test", :crypto.hash(:sha256, "test"))
    assert Map.has_key?(mempool.unconfirmed_txs, :crypto.hash(:sha256, "test")) == true
  end

  test "get top unconfirmed transactions" do
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    mempool_size = max_transaction_per_block - 100
    txs = Enum.reduce(1..mempool_size, [], fn(_x, acc) -> [%BlockchainServer.Transaction{time: Timex.now()} | acc] end)
    top_txs = Mining.get_top_unconfirmed_transactions(txs)
    assert length(top_txs) == mempool_size
  end

  test "get top unconfirmed transactions - mempool larger than max transaction per block" do
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    mempool_size = max_transaction_per_block + 100
    txs = Enum.reduce(1..mempool_size, [], fn(_x, acc) -> [%BlockchainServer.Transaction{time: Timex.now()} | acc] end)
    top_txs = Mining.get_top_unconfirmed_transactions(txs)
    assert length(top_txs) == max_transaction_per_block
  end

  test "get top unconfirmed transactions - order" do
    txs = Enum.reduce(3..1, [], fn(x, acc) -> acc ++ [%BlockchainServer.Transaction{time: Timex.now() |> Timex.shift(seconds: x)}] end)
    assert Timex.compare(Enum.at(txs, 0).time, Enum.at(txs, 2).time)== 1
    top_txs = Mining.get_top_unconfirmed_transactions(txs)
    assert Timex.compare(Enum.at(top_txs, 0).time, Enum.at(top_txs, 2).time) == -1
  end

end
