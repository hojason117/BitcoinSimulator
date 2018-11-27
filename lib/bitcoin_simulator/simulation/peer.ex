defmodule BitcoinSimulator.Simulation.Peer do
  use GenServer
  use Timex

  alias BitcoinSimulator.BitcoinCore.BlockchainServer
  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Simulation.Tracker
  alias BitcoinSimulator.Simulation.TradeCenter
  alias BitcoinSimulator.BitcoinCore.Network
  alias BitcoinSimulator.BitcoinCore.Mining
  alias BitcoinSimulator.BitcoinCore.Wallet
  alias BitcoinSimulator.Const

  # Client

  def start_link(arg) do
    name = {:via, Registry, {BitcoinSimulator.Registry, "peer_#{arg}"}}
    GenServer.start_link(__MODULE__, arg, name: name)
  end

  # Server (callbacks)

  def init(arg) do
    GenServer.cast(TradeCenter, {:peer_join, arg})
    neighbors = GenServer.call(Tracker, {:peer_join, arg})

    address = Wallet.getNewAddress()
    address = %{address |
      value: 10.0,
      outpoint: %{
        hash: :crypto.hash(:sha256, ""),
        index: 0
      }
    }
    state = %{
      id: arg,
      name: "peer_#{arg}",
      neighbors: neighbors,
      roles: MapSet.new(),
      wallet: %Wallet.Wallet{
        unspent_addresses: Map.new([{address.address, address}]),
        unspent_balance: 10.0
      },
      mempool: %Mining.MemPool{},
      messageRecord: %Network.MessageRecord{}
    }

    # state = %{
    #   id: arg,
    #   name: "peer_#{arg}",
    #   neighbors: neighbors,
    #   roles: MapSet.new(),
    #   wallet: %Wallet.Wallet{},
    #   mempool: %Mining.MemPool{},
    #   messageRecord: %Network.MessageRecord{}
    # }

    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))
    Process.send_after(self(), :initiate_trade, Const.decode(:peer_initiate_auto_trading_after))
    Process.send_after(self(), :initiate_mine, 5000)

    {:ok, state}
  end

  def handle_call(:new_address, _from, state) do
    address = Wallet.getNewAddress()
    new_wallet = add_new_address(state.wallet, address)
    {:reply, address.address, Map.put(state, :wallet, new_wallet)}
  end

  def handle_cast({:modify_role, role, action}, state) do
    new_state =
      case action do
        :add ->
          Map.put(state, :roles, MapSet.put(state.roles, role))
        :remove ->
          Map.put(state, :roles, MapSet.delete(state.roles, role))
      end
    {:noreply, new_state}
  end

  def handle_cast({:exchange_neighbors, peer_neighbors}, state) do
    {:noreply, Map.put(state, :neighbors, Network.mix_neighbors(MapSet.union(state.neighbors, peer_neighbors) |> MapSet.delete(state.id)))}
  end

  def handle_cast({:spend_addresses, addresses}, state) do
    new_wallet = move_addresses(state.wallet, addresses)
    {:noreply, Map.put(state, :wallet, new_wallet)}
  end

  def handle_cast({:transaction, transaction}, state) do
    tx_hash = Blockchain.transaction_hash(transaction)
    unless Network.messageSeen?(state.messageRecord, :transaction, tx_hash) do
      new_state =
        if Blockchain.verify_transaction?(transaction) do
          tx_add_mempool_broadcast(transaction, tx_hash, state)
        else
          state
        end
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  def handle_cast({:transaction_assembled, tx}, state) do
    tx_hash = Blockchain.transaction_hash(tx)
    new_state =
      if Blockchain.verify_transaction?(tx) do
        tx_add_mempool_broadcast(tx, tx_hash, state)
      else
        state
      end
    {:noreply, new_state}
  end

  def handle_cast({:block_mined, block}, state) do
    Mining.submitBlock(block)
    Process.send_after(self(), :initiate_mine, 1000)
    {:noreply, state}
  end

  def handle_info(:exchange_neighbors, state) do
    spawn_link(Network, :exchange_neighbors, [state.neighbors])
    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))
    {:noreply, state}
  end

  def handle_info(:initiate_trade, state) do
    if MapSet.member?(state.roles, :trader), do: spawn_link(fn -> trade(state) end)
    trading_interval_min = Const.decode(:peer_auto_trading_interval_range_min)
    trading_interval_max = Const.decode(:peer_auto_trading_interval_range_max)
    Process.send_after(self(), :initiate_trade, Enum.random(trading_interval_min..trading_interval_max))
    {:noreply, state}
  end

  def handle_info(:initiate_mine, state) do
    if MapSet.member?(state.roles, :miner) do
      txs = Mining.get_top_unconfirmed_transactions(Map.values(state.mempool.unconfirmed_txs))
      block =
        %BlockchainServer.Block{
          header: %BlockchainServer.BlockHeader{
            previous_block_hash: Blockchain.getBestBlockHash(),
            merkle_root_hash: Blockchain.merkle_root(txs),
            n_bits: Const.decode(:target_difficulty) * 8,
          },
          transactions: txs
        }
      spawn_link(Mining, :mine, [block, state.id])
    else
      Process.send_after(self(), :initiate_mine, 5000)
    end
    {:noreply, state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

  defp add_new_address(wallet, address) do
    %Wallet.Wallet{
      spent_addresses: wallet.spent_addresses,
      unspent_addresses: Map.put(wallet.unspent_addresses, address.address, address),
      unspent_balance: wallet.unspent_balance
    }
  end

  defp trade(state) do
    trading_partner_count = Enum.random(1..Const.decode(:txout_count_range))
    partners = GenServer.call(TradeCenter, {:get_random_trade_partner, state.id, trading_partner_count})
    {transaction_value, transaction_fee, _remain_value} =
      if state.wallet.unspent_balance < 0.1 do
        {0.0, 0.0, state.wallet.unspent_balance}
      else
        random_transaction_value(state.wallet.unspent_balance)
      end

    unless partners == :not_enough_partner or transaction_value == 0.0 do
      out_addresses = Enum.reduce(MapSet.to_list(partners), [], fn(x, acc) ->
        [GenServer.call({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, :new_address) | acc]
      end)

      sorted_addresses = state.wallet.unspent_addresses |> Map.values() |> sort_addresses_by_value()
      {in_addresses, sum} = combine_unspent_addresses(sorted_addresses, transaction_value + transaction_fee, [], 0.0, 0)
      {change, change_value} =
        if sum == transaction_value + transaction_fee do
          {false, 0.0}
        else
          {true, sum - transaction_value - transaction_fee}
        end

      txin_count = length(in_addresses)
      txin = Enum.reduce(in_addresses, [], fn(x, acc) ->
        acc ++ [%BlockchainServer.Txin{ previous_output: x.outpoint }]
      end)

      out_values = split_out_value(transaction_value, length(out_addresses), [], 0.0)
      txout = Enum.reduce(0..length(out_values) - 1, [], fn(x, acc) ->
        acc ++ [%BlockchainServer.Txout{ value: Enum.at(out_values, x), address: Enum.at(out_addresses, x) }]
      end)
      txout =
        if change do
          change_address = GenServer.call({:via, Registry, {BitcoinSimulator.Registry, "peer_#{state.id}"}}, :new_address)
          txout ++ [%BlockchainServer.Txout{ value: change_value, address: change_address }]
        else
          txout
        end
      txout_count = length(txout)

      public_keys = Enum.reduce(in_addresses, [], fn(x, acc) ->
        acc ++ [x.public_key]
      end)

      tx = %BlockchainServer.Transaction{
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

      tx = %{tx | signatures: signatures}

      GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{state.id}"}}, {:spend_addresses, in_addresses})
      GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{state.id}"}}, {:transaction_assembled, tx})
    end
  end

  defp random_transaction_value(balance) do
    transaction_value_precision = Const.decode(:transaction_value_precision)
    precision_helper = :math.pow(10, transaction_value_precision)
    transaction_value = (balance * precision_helper |> trunc() |> :rand.uniform()) / precision_helper |> Float.round(transaction_value_precision)
    transaction_fee = transaction_value * Enum.random(0..Const.decode(:transaction_fee_percentage_range)) / 100.0 |> Float.round(transaction_value_precision)
    transaction_fee = if transaction_value + transaction_fee > balance, do: balance - transaction_value, else: transaction_fee
    remain_value = balance - transaction_value - transaction_fee |> Float.round(transaction_value_precision)
    {transaction_value, transaction_fee, remain_value}
  end

  defp combine_unspent_addresses(addresses, target_value, result, result_sum, current_index) do
    if result_sum >= target_value do
      {result, result_sum}
    else
      current = Enum.at(addresses, current_index)
      unless current.value == 0.0 do
        combine_unspent_addresses(addresses, target_value, [current | result], result_sum + current.value, current_index + 1)
      else
        combine_unspent_addresses(addresses, target_value, result, result_sum, current_index + 1)
      end
    end
  end

  defp sort_addresses_by_value(addresses), do: Enum.sort(addresses, fn(a, b) -> a.value < b.value end)

  defp split_out_value(sum, pieces, result, current_sum) do
    cond do
      length(result) == pieces ->
        if Enum.any?(result, fn(x) -> x == 0.0 end) do
          split_out_value(sum, pieces, [], 0.0)
        else
          result
        end
      length(result) == pieces - 1 ->
        transaction_value_precision = Const.decode(:transaction_value_precision)
        split_out_value(sum, pieces, [Float.round(sum - current_sum, transaction_value_precision) | result], sum)
      true ->
        transaction_value_precision = Const.decode(:transaction_value_precision)
        precision_helper = :math.pow(10, transaction_value_precision)
        difference = sum - current_sum
        new_value =
          unless difference == 0.0 do
            (difference * precision_helper |> trunc() |> :rand.uniform()) / precision_helper |> Float.round(transaction_value_precision)
          else
            0.0
          end
        split_out_value(sum, pieces, [new_value | result], current_sum + new_value)
    end
  end

  defp move_addresses(wallet, addresses) do
    {new_unspent_addresses, new_unspent_balance} =
      Enum.reduce(addresses, {wallet.unspent_addresses, wallet.unspent_balance}, fn(x, acc) ->
        {unspent_addresses, unspent_balance} = acc
        {Map.delete(unspent_addresses, x.address), unspent_balance - x.value}
      end)

    %Wallet.Wallet{
      spent_addresses: wallet.spent_addresses ++ addresses,
      unspent_addresses: new_unspent_addresses,
      unspent_balance: new_unspent_balance
    }
  end

  defp tx_add_mempool_broadcast(transaction, tx_hash, state) do
    new_mempool = Mining.add_unconfirmed_tx(state.mempool, transaction, tx_hash)
    Enum.each(MapSet.to_list(state.neighbors), fn(x) ->
      GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:transaction, transaction})
    end)
    new_message_record = Network.sawMessage(state.messageRecord, :transaction, tx_hash)
    Map.merge(state, %{mempool: new_mempool, messageRecord: new_message_record})
  end

end
