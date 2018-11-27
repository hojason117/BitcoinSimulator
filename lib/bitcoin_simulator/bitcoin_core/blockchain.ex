defmodule BitcoinSimulator.BitcoinCore.Blockchain do

  alias BitcoinSimulator.BitcoinCore.BlockchainServer
  alias BitcoinSimulator.Const

  # APIs

  def getBestBlockHash, do: GenServer.call(BlockchainServer, :best_block_header) |> block_header_hash()

  def getBlock(hash), do: GenServer.call(BlockchainServer, {:get_block, hash})

  def getBlockChainInfo, do: GenServer.call(BlockchainServer, :blockchain_info)

  def getBlockCount, do: GenServer.call(BlockchainServer, :block_count)

  def getBlockHash(height), do: GenServer.call(BlockchainServer, {:block_hash, height})

  def getBlockHeader(hash), do: GenServer.call(BlockchainServer, {:block_header, hash})

  def getDifficulty, do: GenServer.call(BlockchainServer, :difficulty)

  # Aux

  def block_header_hash(header) do
    header = header.previous_block_hash <> header.merkle_root_hash <> Time.to_string(header.time) <> Integer.to_string(header.n_bits) <> Integer.to_string(header.nonce)
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

  def merkle_root(transactions) do
    unless length(transactions) == 0 do
      hashes = Enum.reduce(transactions, [], fn(x, acc) -> [transaction_hash(x) | acc] end)
      hashes |> Enum.reverse() |> merkle_tree_hash_level() |> Enum.at(0)
    else
      double_hash("")
    end
  end

  def double_hash(input) do
    hash_func = Const.decode(:hash_func)
    :crypto.hash(hash_func, :crypto.hash(hash_func, input))
  end

  def verify_block?(_block) do
    # TODO
    true
  end

  def verify_transaction?(_tx) do
    # TODO
    true
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
    Enum.reduce(keys, fn(x, acc) -> acc <> x end)
  end

end
