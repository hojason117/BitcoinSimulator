defmodule BitcoinSimulator.BitcoinCore.RawTransaction do
  use Timex

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Const

  # APIs

  def create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value) do
    txin_count = length(in_addresses)
    txin = Enum.reduce(in_addresses, [], fn(x, acc) ->
      acc ++ [%Blockchain.Txin{ previous_output: x.outpoint }]
    end)

    txout = Enum.reduce(0..length(out_values) - 1, [], fn(x, acc) ->
      acc ++ [%Blockchain.Txout{ value: Enum.at(out_values, x), address: Enum.at(out_addresses, x) }]
    end)
    txout =
      if change_address != nil do
        txout ++ [%Blockchain.Txout{ value: change_value, address: change_address }]
      else
        txout
      end
    txout_count = length(txout)

    public_keys = Enum.reduce(in_addresses, [], fn(x, acc) ->
      acc ++ [x.public_key]
    end)

    tx = %Blockchain.Transaction{
      in_count: txin_count,
      tx_in: txin,
      out_count: txout_count,
      tx_out: txout,
      time: Timex.now(),
      public_keys: public_keys
    }

    tx_hash = Blockchain.transaction_hash(tx)
    signatures = Enum.reduce(in_addresses, [], fn(x, acc) ->
      acc ++ [:crypto.sign(:ecdsa, Const.decode(:hash_func), tx_hash, [x.private_Key, :secp256k1])]
    end)

    %{tx | signatures: signatures}
  end

  def create_coinbase_transaction(out_addresses, out_values) do
    hash_digest = Const.decode(:hash_digest)
    txin = [%Blockchain.Txin{ previous_output: %{ hash: <<0::size(hash_digest)>>, index: -1 } }]
    txout = [%Blockchain.Txout{ value: out_values, address: out_addresses }]
    %Blockchain.Transaction{
      in_count: 1,
      tx_in: txin,
      out_count: 1,
      tx_out: txout,
      time: Timex.now(),
    }
  end

  # Aux

end
