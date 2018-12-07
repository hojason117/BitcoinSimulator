defmodule BitcoinSimulator.Simulation.Peer do
  defmacro __using__(_) do
    quote do
      use GenServer
      require Logger

      alias BitcoinSimulator.BitcoinCore
      alias BitcoinSimulator.Simulation.{TradeCenter, Tracker, Param}
      alias BitcoinSimulator.Const

      # Client

      def start_link(arg) do
        name = {:via, Registry, {BitcoinSimulator.Registry, "peer_#{arg}"}}
        GenServer.start_link(__MODULE__, arg, name: name)
      end

      # Server (callbacks)

      def handle_call(:new_address, _from, state) do
        {address, new_wallet} = BitcoinCore.get_new_address(state.wallet)
        {:reply, address.address, %{state | wallet: new_wallet}}
      end

      def handle_call(:request_blockchain, _from, state), do: {:reply, state.blockchain, state}

      def handle_cast({:modify_role, role, action}, state) do
        new_state =
          case action do
            :add ->
              %{state | roles: MapSet.put(state.roles, role)}
            :remove ->
              %{state | roles: MapSet.delete(state.roles, role)}
          end
        {:noreply, new_state}
      end

      def handle_cast({:exchange_neighbors, peer_neighbors}, state) do
        {:noreply, %{state | neighbors: BitcoinCore.mix_neighbors(MapSet.union(state.neighbors, peer_neighbors), state.id)}}
      end

      # Implement:
      # handle_cast({:transaction, transaction, sender}, state)

      # Implement:
      #  handle_cast({:transaction_assembled, transaction, new_wallet}, state)

      # Implement:
      # handle_cast({:block, block, sender}, state)

      # Implement:
      # handle_cast({:block_mined, block, coinbase_addr}, state)

      def handle_cast(:terminate, state) do
        if state.mining_process != nil, do: Process.exit(state.mining_process, :kill)
        GenServer.cast(Tracker, {:peer_leave, state.id})
        new_state = %{state | mining_process: nil, mining_txs: nil}
        {:stop, :normal, new_state}
      end

      def handle_info(:exchange_neighbors, state) do
        BitcoinCore.exchange_neighbors(state.neighbors)
        Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))
        {:noreply, state}
      end

      def handle_info(:initiate_trade, state) do
        if MapSet.member?(state.roles, :trader), do: spawn_link(fn -> assemble_transaction(state.id, state.wallet) end)
        trading_interval_min = GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_min})
        trading_interval_max = GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_max})
        Process.send_after(self(), :initiate_trade, Enum.random(trading_interval_min..trading_interval_max))
        {:noreply, state}
      end

      def handle_info(:initiate_mine, state) do
        if MapSet.member?(state.roles, :miner) do
          txs = BitcoinCore.get_top_unconfirmed_transactions(state.mempool)
          tx_hashes = Enum.reduce(txs, MapSet.new(), fn(x, acc) -> MapSet.put(acc, BitcoinCore.transaction_hash(x)) end)
          coinbase_value = BitcoinCore.calc_cainbase_value(state.blockchain, txs)
          {coinbase_addr, _} = BitcoinCore.get_new_address(state.wallet)
          coinbase_tx = BitcoinCore.create_coinbase_transaction(coinbase_addr.address, coinbase_value)
          txs = [coinbase_tx | txs]
          block = BitcoinCore.get_block_template(BitcoinCore.get_best_block_hash(state.blockchain), txs)
          pid = spawn(BitcoinCore, :mine, [block, coinbase_addr, state.id])
          # Process.flag(pid, :priority, :low)
          {:noreply, %{state | mining_process: pid, mining_txs: tx_hashes}}
        else
          Process.send_after(self(), :initiate_mine, 5000)
          {:noreply, state}
        end
      end

      def handle_info(:clean_message_record, state) do
        new_record = BitcoinCore.clean_message_record(state.message_record)
        Process.send_after(self(), :clean_message_record, Const.decode(:network_message_record_ttl))
        {:noreply, %{state | message_record: new_record}}
      end

      def terminate(reason, _state), do: if reason != :normal, do: Logger.error(reason)

      # Aux

      def random_transaction_value(balance) do
        if balance < 0.1 do
          {0.0, 0.0, balance}
        else
          transaction_value_precision = Const.decode(:transaction_value_precision)
          precision_helper = :math.pow(10, transaction_value_precision)
          transaction_value = (balance * precision_helper |> trunc() |> :rand.uniform()) / precision_helper |> Float.round(transaction_value_precision)
          transaction_fee = transaction_value * Enum.random(0..Const.decode(:transaction_fee_percentage_range)) / 100.0 |> Float.round(transaction_value_precision)
          transaction_fee = if transaction_value + transaction_fee > balance, do: balance - transaction_value, else: transaction_fee
          remain_value = balance - transaction_value - transaction_fee |> Float.round(transaction_value_precision)
          {transaction_value, transaction_fee, remain_value}
        end
      end

      defp init_peer(id) do
        neighbors = BitcoinCore.get_initial_neighbors(id)
        blockchain = BitcoinCore.get_initial_blockchain(neighbors)

        state = %{
          id: id,
          name: "peer_#{id}",
          neighbors: neighbors,
          roles: MapSet.new(),
          wallet: BitcoinCore.get_new_wallet(),
          blockchain: blockchain,
          mempool: BitcoinCore.get_new_mempool(),
          message_record: BitcoinCore.get_new_message_record(),
          mining_process: nil,
          mining_txs: nil
        }

        Process.send_after(self(), :exchange_neighbors, 3000)
        Process.send_after(self(), :clean_message_record, Const.decode(:network_message_record_ttl))
        Process.send_after(self(), :initiate_trade, Const.decode(:peer_initiate_auto_trading_after))
        Process.send_after(self(), :initiate_mine, Const.decode(:peer_initiate_mining_after))

        GenServer.cast(TradeCenter, {:peer_join, id})

        state
      end

      defp handle_transaction_received(transaction, sender, state) do
        tx_hash = BitcoinCore.transaction_hash(transaction)
        unless BitcoinCore.message_seen?(state.message_record, :transaction, tx_hash) do
          new_state = %{state | message_record: BitcoinCore.saw_message(state.message_record, :transaction, tx_hash)}
          verification_result = BitcoinCore.verify_transaction(new_state.blockchain, transaction)
          if verification_result == :ok do
            filtered_neightbors = MapSet.delete(new_state.neighbors, sender)
            BitcoinCore.broadcast_message(:transaction, transaction, filtered_neightbors, new_state.id)
            %{new_state | mempool: BitcoinCore.add_unconfirmed_tx(new_state.mempool, transaction, tx_hash)}
          else
            Logger.info("Transaction rejected [reason: #{verification_result}]")
            new_state
          end
        else
          state
        end
      end

      defp handle_transaction_assembled(transaction, new_wallet, state) do
        verification_result = BitcoinCore.verify_transaction(state.blockchain, transaction)
        if verification_result == :ok do
          tx_hash = BitcoinCore.transaction_hash(transaction)
          BitcoinCore.broadcast_message(:transaction, transaction, state.neighbors, state.id)

          %{state |
            message_record: BitcoinCore.saw_message(state.message_record, :transaction, tx_hash),
            mempool: BitcoinCore.add_unconfirmed_tx(state.mempool, transaction, tx_hash),
            wallet: new_wallet
          }
        else
          Logger.info("Transaction rejected [reason: #{verification_result}]")
          state
        end
      end

      defp handle_block_received(block, sender, state) do
        block_hash = BitcoinCore.block_header_hash(block.header)
        unless BitcoinCore.message_seen?(state.message_record, :block, block_hash) do
          new_state = %{state | message_record: BitcoinCore.saw_message(state.message_record, :block, block_hash)}
          verification_result = BitcoinCore.verify_block(new_state.blockchain, block)
          if verification_result == :ok do
            filtered_neightbors = MapSet.delete(new_state.neighbors, sender)
            BitcoinCore.broadcast_message(:block, block, filtered_neightbors, new_state.id)

            {new_blockchain, new_wallet, new_mempool} =
              BitcoinCore.add_block(block, new_state.blockchain, new_state.wallet, new_state.mempool, new_state.mining_process, new_state.mining_txs)

            %{new_state |
              blockchain: new_blockchain,
              wallet: new_wallet,
              mempool: new_mempool
            }
          else
            Logger.info("Block rejected [reason: #{verification_result}]")
            new_state
          end
        else
          state
        end
      end

      defp handle_block_mined(block, coinbase_addr, state) do
        verification_result = BitcoinCore.verify_block(state.blockchain, block)
        new_state =
          if verification_result == :ok do
            Logger.info("Block mined [transaction count: #{length(block.transactions)}]")
            block_hash = BitcoinCore.block_header_hash(block.header)
            BitcoinCore.broadcast_message(:block, block, state.neighbors, state.id)

            new_wallet = BitcoinCore.import_address(state.wallet, coinbase_addr)

            {new_blockchain, new_wallet, new_mempool} = BitcoinCore.add_block(block, state.blockchain, new_wallet, state.mempool)

            %{state |
              message_record: BitcoinCore.saw_message(state.message_record, :block, block_hash),
              blockchain: new_blockchain,
              wallet: new_wallet,
              mempool: new_mempool
            }
          else
            Logger.info("Block rejected [reason: #{verification_result}]")
            state
          end

        Process.send_after(self(), :initiate_mine, 2000)
        new_state
      end

      defp assemble_transaction(id, wallet) do
        partners = get_random_trade_partners(id)
        {transaction_value, transaction_fee, _remain_value} = random_transaction_value(wallet.unspent_balance)

        unless partners == :not_enough_partner or transaction_value == 0.0 do
          out_addresses = Enum.reduce(MapSet.to_list(partners), [], fn(x, acc) ->
            [GenServer.call({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, :new_address) | acc]
          end)

          {in_addresses, sum} = BitcoinCore.combine_unspent_addresses(wallet, transaction_value + transaction_fee)
          {change, change_value} =
            if sum == transaction_value + transaction_fee do
              {false, 0.0}
            else
              {true, sum - transaction_value - transaction_fee}
            end

          out_values = split_out_value(transaction_value, length(out_addresses), [], 0.0)

          {change_address, new_wallet} =
            if change do
              {address, new_wallet} = BitcoinCore.get_new_address(wallet)
              {address.address, new_wallet}
            else
              {nil, wallet}
            end

          new_wallet = Enum.reduce(in_addresses, new_wallet, fn(x, acc) -> BitcoinCore.spend_address(acc, x.address) end)

          transaction = BitcoinCore.create_raw_transaction(in_addresses, out_addresses, out_values, change_address, change_value)

          GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{id}"}}, {:transaction_assembled, transaction, new_wallet})
        end
      end

      defp get_random_trade_partners(id) do
        trading_partner_count = Enum.random(1..Const.decode(:txout_count_range))
        GenServer.call(TradeCenter, {:get_random_trade_partner, id, trading_partner_count})
      end

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
    end
  end
end
