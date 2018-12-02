defmodule BitcoinSimulator.BitcoinCore.Blockchain do

  alias BitcoinSimulator.BitcoinCore.{Blockchain, Mining}
  alias BitcoinSimulator.Simulation.Param
  alias BitcoinSimulator.Const

  defmodule Blockchain do
    defstruct [
      blocks: Map.new(),
      unspent_txout: Map.new(),
      block_count: 0,
      genesis_block: nil,
      tip: nil
    ]
  end

  defmodule BlockHeader do
    defstruct [
      previous_block_hash: nil,
      merkle_root_hash: nil,
      time: nil,
      n_bits: nil,
      nonce: nil
    ]
  end

  defmodule Block do
    defstruct [
      header: %BlockHeader{},
      transactions: []
    ]
  end

  defmodule Transaction do
    defstruct [
      in_count: 0,
      tx_in: [],
      out_count: 0,
      tx_out: [],
      time: nil,
      signatures: [],
      public_keys: []
    ]
  end

  defmodule Txin do
    defstruct [
      previous_output: %{
        hash: nil,
        index: 0
      }
    ]
  end

  defmodule Txout do
    defstruct [
      value: 0.0,
      address: nil
    ]
  end

  # APIs

  def get_new_blockchain do
    hash_digest = Const.decode(:hash_digest)
    {:ok, datetime, 0} = DateTime.from_iso8601("2018-11-20T00:00:00Z")
    genesis_block = %Block{
      header: %BlockHeader{
        previous_block_hash: <<0::size(hash_digest)>>,
        merkle_root_hash: merkle_root([]),
        time: datetime,
        n_bits: 0,
        nonce: 0
      },
      transactions: []
    }

    %Blockchain{
      blocks: Map.new([{block_header_hash(genesis_block.header), genesis_block}]),
      block_count: 1,
      genesis_block: genesis_block,
      tip: genesis_block
    }
  end

  def get_best_block_hash(blockchain), do: block_header_hash(blockchain.tip.header)

  def block_header_hash(header) do
    header = header.previous_block_hash <> header.merkle_root_hash <> DateTime.to_string(header.time) <> Integer.to_string(header.n_bits) <> Integer.to_string(header.nonce)
    double_hash(header)
  end

  def transaction_hash(tx) do
    in_count = Integer.to_string(tx.in_count)
    tx_in = txin_hash(tx.tx_in)
    out_count = Integer.to_string(tx.out_count)
    tx_out = txout_hash(tx.tx_out)
    time = Time.to_string(tx.time)
    public_keys = public_keys_hash(tx.public_keys)

    input = in_count <> tx_in <> out_count <> tx_out <> time <> public_keys
    double_hash(input)
  end

  def verify_block?(blockchain, block) do
    cond do
      # Transaction list must be non-empty
      length(block.transactions) == 0 ->
        false
      # Block hash must satisfy claimed nBits proof of work
      not Mining.match_leading_zeros?(block_header_hash(block.header), GenServer.call(Param, {:get_param, :target_difficulty_bits})) ->
        false
      # First transaction must be coinbase
      not coinbase_transaction?(Enum.at(block.transactions, 0)) ->
        false
      # Check all transactions
      not verify_txs?(blockchain, block.transactions, 0, length(block.transactions)) ->
        false
      # Verify Merkle hash
      merkle_root(block.transactions) != block.header.merkle_root_hash ->
        false
      # prev hash

      true ->
        true
    end
  end

  def verify_transaction?(blockchain, tx) do
    cond do
      # Neither in or out lists are empty
      length(tx.tx_in) == 0 or length(tx.tx_out) == 0 ->
        false
      # Skip verification for coinbase transactions
      coinbase_transaction?(tx) ->
        true
      # Each input's referenced output must exist and has not already been spent(double spending)
      not verify_tx_input?(blockchain, tx.tx_in, 0) ->
        false
      # Sum of input values >= sum of output values
      not verify_tx_sum?(blockchain, tx) ->
        false
      # Check signature and public key
      not verify_tx_sig?(blockchain, tx, 0) ->
        false

      true ->
        true
    end
  end

  def merkle_root(transactions) do
    unless length(transactions) == 0 do
      hashes = Enum.reduce(transactions, [], fn(x, acc) -> [transaction_hash(x) | acc] end)
      hashes |> Enum.reverse() |> merkle_tree_hash_level() |> Enum.at(0)
    else
      double_hash("")
    end
  end

  # Aux

  defp double_hash(input) do
    hash_func = Const.decode(:hash_func)
    :crypto.hash(hash_func, :crypto.hash(hash_func, input))
  end

  defp merkle_tree_hash_level(hashes) when length(hashes) == 1, do: hashes

  defp merkle_tree_hash_level(hashes) do
    {new_hashes, temp, odd} =
      Enum.reduce(hashes, {[], nil, false}, fn(x, acc) ->
        {new_hashes, temp, odd} = acc
        if odd do
          {[double_hash(temp <> x) | new_hashes], nil, false}
        else
          {new_hashes, x, true}
        end
      end)

    new_hashes = if odd, do: [double_hash(temp <> temp) | new_hashes], else: new_hashes

    merkle_tree_hash_level(Enum.reverse(new_hashes))
  end

  defp txin_hash(txin) do
    Enum.reduce(txin, "", fn(x, acc) ->
      acc <> x.previous_output.hash <> Integer.to_string(x.previous_output.index)
    end)
  end

  defp txout_hash(txout) do
    Enum.reduce(txout, "", fn(x, acc) ->
      acc <> Float.to_string(x.value) <> x.address
    end)
  end

  defp public_keys_hash(keys) do
    Enum.reduce(keys, "", fn(x, acc) -> acc <> x end)
  end

  defp verify_txs?(blockchain, txs, index, len) do
    if index == len do
      true
    else
      if verify_transaction?(blockchain, Enum.at(txs, index)), do: verify_txs?(blockchain, txs, index + 1, len), else: false
    end
  end

  defp coinbase_transaction?(tx) do
    hash_digest = Const.decode(:hash_digest)
    length(tx.tx_in) == 1
    and Enum.at(tx.tx_in, 0).previous_output.hash == <<0::size(hash_digest)>>
    and Enum.at(tx.tx_in, 0).previous_output.index == -1
  end

  defp verify_tx_input?(blockchain, txin, index) do
    if index == length(txin) do
      true
    else
      if Map.has_key?(blockchain.unspent_txout, Enum.at(txin, index).previous_output) do
        verify_tx_input?(blockchain, txin, index + 1)
      else
        false
      end
    end
  end

  defp verify_tx_sum?(blockchain, tx) do
    total_in = Enum.reduce(tx.tx_in, 0.0, fn(x, acc) -> acc + blockchain.unspent_txout[x.previous_output].value end) |> Float.round(Const.decode(:transaction_value_precision))
    total_out = Enum.reduce(tx.tx_out, 0.0, fn(x, acc) -> acc + x.value end) |> Float.round(Const.decode(:transaction_value_precision))
    total_in >= total_out
  end

  defp verify_tx_sig?(blockchain, tx, index) do
    if index == length(tx.signatures) do
      true
    else
      prev_addr = blockchain.unspent_txout[Enum.at(tx.tx_in, index).previous_output].address
      pk_match? = prev_addr == :crypto.hash(:ripemd160, :crypto.hash(:sha256, Enum.at(tx.public_keys, index)))
      sig_valid? = :crypto.verify(
        :ecdsa,
        Const.decode(:hash_func),
        transaction_hash(tx),
        Enum.at(tx.signatures, index),
        [Enum.at(tx.public_keys, index), :secp256k1])

      if pk_match? and sig_valid? do
        verify_tx_sig?(blockchain, tx, index + 1)
      else
        false
      end
    end
  end

end
