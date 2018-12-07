defmodule BitcoinSimulator.BitcoinCore.BlockchainTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Const

  test "get new blockchain" do
    hash_digest = Const.decode(:hash_digest)
    {:ok, datetime, 0} = DateTime.from_iso8601("2018-11-20T00:00:00Z")
    genesis_block = %Blockchain.Block{
      header: %Blockchain.BlockHeader{
        previous_block_hash: <<0::size(hash_digest)>>,
        merkle_root_hash: <<93, 246, 224, 226, 118, 19, 89, 211, 10, 130, 117, 5, 142, 41, 159, 204, 3,
          129, 83, 69, 69, 245, 92, 244, 62, 65, 152, 63, 93, 76, 148, 86>>,
        time: datetime,
        n_bits: 0,
        nonce: 0
      },
      transactions: []
    }

    blockchain = %{
      blocks: Map.new(),
      transactions: Map.new(),
      block_count: 0,
      genesis_block: nil,
      tip: nil
    }

    blockchain = %{blockchain |
      blocks: Map.put(blockchain.blocks, Blockchain.block_header_hash(genesis_block.header), genesis_block),
      transactions: Map.new(),
      block_count: 1,
      genesis_block: genesis_block,
      tip: genesis_block
    }

    target = Blockchain.get_new_blockchain()
    assert Map.size(target.blocks) == Map.size(blockchain.blocks)
    assert target.block_count == 1
    assert target.genesis_block == target.tip
    assert target.genesis_block == genesis_block
  end

  test "get best block hash" do
    # Blockchain.get_best_block_hash()
  end

  test "block header hash" do
    hash_digest = Const.decode(:hash_digest)
    {:ok, datetime, 0} = DateTime.from_iso8601("2018-11-20T00:00:00Z")
    header = %Blockchain.BlockHeader{
      previous_block_hash: <<0::size(hash_digest)>>,
      merkle_root_hash: <<93, 246, 224, 226, 118, 19, 89, 211, 10, 130, 117, 5, 142, 41, 159, 204, 3,
        129, 83, 69, 69, 245, 92, 244, 62, 65, 152, 63, 93, 76, 148, 86>>,
      time: datetime,
      n_bits: 0,
      nonce: 0
    }

    assert Blockchain.block_header_hash(header) == <<130, 240, 173, 63, 120, 29, 97, 134, 5, 49, 147, 152, 31, 23, 120, 112, 195,
      223, 124, 184, 58, 15, 135, 23, 52, 78, 212, 40, 63, 92, 66, 91>>
  end

  test "transaction hash" do
    tx_in = %Blockchain.Txin{
      previous_output: %{
        hash: :crypto.hash(Const.decode(:hash_func), "test_in"),
        index: 0
      }
    }

    tx_out = %Blockchain.Txout{
      value: 10.0,
      address: :crypto.hash(Const.decode(:hash_func), "test_out"),
    }

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = DateTime.from_unix(1_464_096_368)
    transaction = %Blockchain.Transaction{
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
    tx_in = %Blockchain.Txin{
      previous_output: %{
        hash: :crypto.hash(Const.decode(:hash_func), "test_in"),
        index: 0
      }
    }

    tx_out = %Blockchain.Txout{
      value: 10.0,
      address: :crypto.hash(Const.decode(:hash_func), "test_out"),
    }

    public_key = :crypto.hash(Const.decode(:hash_func), "test_key")

    {:ok, time} = DateTime.from_unix(1_464_096_368)
    tx = %Blockchain.Transaction{
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

  test "update unspent txout" do
    unspent_txout = %{
      %{ hash: <<100::256>>, index: 2 } => %{ value: 3.679, address: <<200::256>> }
    }

    assert unspent_txout |> Map.to_list |> Enum.at(0) == {%{ hash: <<100::256>>, index: 2 }, %{ value: 3.679, address: <<200::256>> }}

    {:ok, time} = DateTime.from_unix(1_464_096_368)
    txs = [
      %Blockchain.Transaction{
        in_count: 1,
        tx_in: [%{ previous_output: %{ hash: <<100::256>>, index: 2 } }],
        out_count: 1,
        tx_out: [%{ value: 3.679, address: <<300::256>> }],
        time: time,
        signatures: [<<400::256>>],
        public_keys: [<<500::256>>]
      }
    ]

    unspent_txout = Blockchain.update_unspent_txout(txs, unspent_txout)
    tx_hash = <<132, 69, 140, 191, 129, 44, 195, 97, 137, 247, 222, 121, 187, 149, 83, 109, 79, 127, 95, 211, 163, 216, 226, 250, 246, 252, 4, 141, 120, 122, 54, 20>>
    assert unspent_txout |> Map.to_list |> Enum.at(0) == {%{ hash: tx_hash, index: 0 }, %{ value: 3.679, address: <<300::256>> }}
  end

end
