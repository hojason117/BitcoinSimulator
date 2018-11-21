defmodule BitcoinSimulator.BitcoinCore.BlockchainTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Const

  test "double hash" do
    hash_func = Const.decode(:hash_func)
    assert Blockchain.double_hash("test") == :crypto.hash(hash_func, :crypto.hash(hash_func, "test"))
  end

end
