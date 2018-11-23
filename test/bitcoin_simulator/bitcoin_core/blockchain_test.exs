defmodule BitcoinSimulator.BitcoinCore.BlockchainTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.BitcoinCore.BlockchainServer
  alias BitcoinSimulator.Const

  test "double hash" do
    hash_func = Const.decode(:hash_func)
    assert Blockchain.double_hash("test") == :crypto.hash(hash_func, :crypto.hash(hash_func, "test"))
  end

  test "transaction hash" do
    tx_in = %BlockchainServer.Txin{
      previous_output: %{
        hash: :crypto.hash(Const.decode(:hash_func), "test_in"),
        index: 0
      }
    }

    tx_out = %BlockchainServer.Txout{
      value: 10,
      address: :crypto.hash(Const.decode(:hash_func), "test_out"),
    }

    signature = :crypto.hash(Const.decode(:hash_func), "test_sig")

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = Time.new(0, 0, 0, 0)
    transaction = %BlockchainServer.Transaction{
      in_count: 1,
      tx_in: [tx_in],
      out_count: 1,
      tx_out: [tx_out],
      time: time,
      signatures: [signature],
      public_keys: [public_key]
    }

    assert Blockchain.transaction_hash(transaction) == <<96, 21, 111, 148, 103, 136, 12, 139, 20, 182, 118, 63, 191, 83, 192, 59, 126,
      176, 177, 210, 255, 150, 120, 165, 81, 160, 149, 189, 228, 247, 174, 110>>
  end

end
