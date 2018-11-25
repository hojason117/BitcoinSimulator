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

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = Time.new(0, 0, 0, 0)
    transaction = %BlockchainServer.Transaction{
      in_count: 1,
      tx_in: [tx_in],
      out_count: 1,
      tx_out: [tx_out],
      time: time,
      public_keys: [public_key]
    }

    assert Blockchain.transaction_hash(transaction) == <<233, 90, 145, 137, 101, 238, 65, 202, 18, 176, 73, 95, 98, 252, 140, 155, 156,
      34, 193, 53, 174, 28, 169, 37, 143, 252, 33, 192, 219, 90, 116, 48>>
  end

end
