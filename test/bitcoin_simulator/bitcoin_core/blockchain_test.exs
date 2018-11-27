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
      value: 10.0,
      address: :crypto.hash(Const.decode(:hash_func), "test_out"),
    }

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = DateTime.from_unix(1_464_096_368)
    transaction = %BlockchainServer.Transaction{
      in_count: 1,
      tx_in: [tx_in],
      out_count: 1,
      tx_out: [tx_out],
      time: time,
      public_keys: [public_key]
    }

    assert Blockchain.transaction_hash(transaction) == <<32, 241, 165, 87, 37, 152, 91, 4, 231, 22, 207, 55, 15, 167, 229, 221, 177,
      145, 240, 54, 42, 226, 15, 147, 34, 114, 125, 192, 94, 187, 251, 143>>
  end

  test "merkle root" do
    tx_in = %BlockchainServer.Txin{
      previous_output: %{
        hash: :crypto.hash(Const.decode(:hash_func), "test_in"),
        index: 0
      }
    }

    tx_out = %BlockchainServer.Txout{
      value: 10.0,
      address: :crypto.hash(Const.decode(:hash_func), "test_out"),
    }

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = DateTime.from_unix(1_464_096_368)
    tx = %BlockchainServer.Transaction{
      in_count: 1,
      tx_in: [tx_in],
      out_count: 1,
      tx_out: [tx_out],
      time: time,
      public_keys: [public_key]
    }

    assert Blockchain.merkle_root([tx, tx]) == <<187, 228, 202, 127, 96, 177, 204, 104, 6, 242, 229, 9, 153, 67, 106, 65, 96,
      69, 152, 249, 177, 5, 60, 162, 25, 52, 220, 41, 68, 249, 107, 223>>
  end

  test "merkle root - empty transactions" do
    assert Blockchain.merkle_root([]) == <<93, 246, 224, 226, 118, 19, 89, 211, 10, 130, 117, 5, 142, 41, 159, 204, 3,
      129, 83, 69, 69, 245, 92, 244, 62, 65, 152, 63, 93, 76, 148, 86>>
  end

end
