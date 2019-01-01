defmodule BitcoinSimulator.BitcoinCore.MiningTest do
  use ExUnit.Case, async: true
  use Timex

  alias BitcoinSimulator.BitcoinCore.{Blockchain, Mining}
  alias BitcoinSimulator.Const

  test "get new mempool" do
    assert Mining.get_new_mempool().unconfirmed_txs == %{}
  end

  test "add unconfirmed tx" do
    mempool = %Mining.MemPool{}
    assert Map.has_key?(mempool.unconfirmed_txs, :crypto.hash(:sha256, "test")) == false
    mempool = Mining.add_unconfirmed_tx(mempool, "test", :crypto.hash(:sha256, "test"))
    assert Map.has_key?(mempool.unconfirmed_txs, :crypto.hash(:sha256, "test")) == true
  end

  test "get top unconfirmed transactions" do
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    mempool_size = max_transaction_per_block - 100
    mempool = %Mining.MemPool{}
    mempool = Enum.reduce(1..mempool_size, mempool, fn(x, acc) -> Mining.add_unconfirmed_tx(acc, %Blockchain.Transaction{time: Timex.now()}, <<x::32>>) end)
    top_txs = Mining.get_top_unconfirmed_transactions(mempool)
    assert length(top_txs) == mempool_size
  end

  test "get top unconfirmed transactions - mempool larger than max transaction per block" do
    max_transaction_per_block = Const.decode(:max_transaction_per_block)
    mempool_size = max_transaction_per_block + 100
    mempool = %Mining.MemPool{}
    mempool = Enum.reduce(1..mempool_size, mempool, fn(x, acc) -> Mining.add_unconfirmed_tx(acc, %Blockchain.Transaction{time: Timex.now()}, <<x::32>>) end)
    top_txs = Mining.get_top_unconfirmed_transactions(mempool)
    assert length(top_txs) == max_transaction_per_block
  end

  test "get top unconfirmed transactions - order" do
    txs = Enum.reduce(3..1, [], fn(x, acc) -> acc ++ [%Blockchain.Transaction{time: Timex.now() |> Timex.shift(seconds: x)}] end)
    mempool = %Mining.MemPool{}
    mempool = Mining.add_unconfirmed_tx(mempool, Enum.at(txs, 0), <<0, 0, 0, 0>>)
    mempool = Mining.add_unconfirmed_tx(mempool, Enum.at(txs, 1), <<1, 1, 1, 1>>)
    mempool = Mining.add_unconfirmed_tx(mempool, Enum.at(txs, 2), <<2, 2, 2, 2>>)

    assert Timex.compare(Enum.at(txs, 0).time, Enum.at(txs, 2).time) == 1

    top_txs = Mining.get_top_unconfirmed_transactions(mempool)

    assert Timex.compare(Enum.at(top_txs, 0).time, Enum.at(top_txs, 2).time) == -1
  end

  test "get block template" do
    prev_hash = <<227, 176, 196, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36,
      39, 174, 65, 228, 100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85>>
    block = %Blockchain.Block{
      header: %Blockchain.BlockHeader{
        previous_block_hash: prev_hash,
        merkle_root_hash: Blockchain.merkle_root([]),
        n_bits: Const.decode(:target_difficulty_bits),
      },
      transactions: []
    }

    assert Mining.get_block_template(prev_hash, []) == block
  end

  test "match leading zeros?" do
    fail = <<227, 176, 196, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36,
      39, 174, 65, 228, 100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85>>
    success = <<0, 0, 15, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36, 39,
      174, 65, 228, 100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85>>
    assert Mining.match_leading_zeros?(fail, 20) == false
    assert Mining.match_leading_zeros?(success, 20) == true
  end

end
