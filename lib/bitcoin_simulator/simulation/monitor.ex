defmodule BitcoinSimulator.Simulation.Monitor do
  use BitcoinSimulator.Simulation.Peer
  use Timex

  alias BitcoinSimulator.BitcoinCore
  alias BitcoinSimulator.Simulation.Node
  alias BitcoinSimulator.Const

  # Server (callbacks)

  def init(arg) do
    Process.register(self(), BitcoinSimulator.Simulation.Monitor)

    state = init_peer(arg)
    state = Map.merge(state, %{
      peer_count: 0,
      trader_count: 0,
      miner_count: 0,
      peers: MapSet.new(),
      traders: MapSet.new(),
      miners: MapSet.new(),
      simulation_started?: false,
      new_blocks: [],
      new_txs: []
    })

    {:ok, state}
  end

  def handle_call(:network_info, _from, state), do: {:reply, state, state}

  def handle_call(:simulation_started?, _from, state), do: {:reply, state.simulation_started?, state}

  def handle_call({:stat, type}, _from, state) do
    {result, new_state} =
      case type do
        :peer_count ->
          {state.peer_count, state}
        :trader_count ->
          {state.trader_count, state}
        :miner_count ->
          {state.miner_count, state}
        :trading_interval ->
          min = GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_min})
          max = GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_max})
          {"#{min} ~ #{max}", state}
        :blockchain_height ->
          {state.blockchain.block_count, state}
        :difficulty ->
          {GenServer.call(Param, {:get_param, :target_difficulty_bits}), state}
        :net_worth ->
          {state.blockchain.block_count * Const.decode(:block_reward) |> Float.round(Const.decode(:transaction_value_precision)), state}
        :tx_frequency ->
          {length(state.new_txs), %{state | new_txs: []}}
        :mine_frequency ->
          {length(state.new_blocks), %{state | new_blocks: []}}
      end
    {:reply, result, new_state}
  end

  def handle_cast(:start_simulation, state) do
    {peers, traders, miners} = spawn_peers(state.peers, state.traders, state.miners, GenServer.call(Param, {:get_param, :peer_count}))

    new_state = %{state |
      peer_count: MapSet.size(peers),
      trader_count: MapSet.size(traders),
      miner_count: MapSet.size(miners),
      peers: peers,
      traders: traders,
      miners: miners,
      simulation_started?: true,
      new_blocks: [],
      new_txs: []
    }

    {:noreply, new_state}
  end

  def handle_cast(:terminate_simulation, state) do
    Enum.each(state.peers, fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, :terminate) end)

    new_state = %{state |
      peer_count: 0,
      trader_count: 0,
      miner_count: 0,
      peers: MapSet.new(),
      traders: MapSet.new(),
      miners: MapSet.new(),
      simulation_started?: false,
      new_blocks: [],
      new_txs: []
    }

    {:noreply, new_state}
  end

  def handle_cast({:transaction, transaction, sender}, state) do
    new_state = handle_transaction_received(transaction, sender, state)
    new_state = %{new_state | new_txs: [transaction | new_state.new_txs]}
    {:noreply, new_state}
  end

  def handle_cast({:transaction_assembled, transaction, new_wallet}, state) do
    new_state = handle_transaction_assembled(transaction, new_wallet, state)
    new_state = %{new_state | new_txs: [transaction | new_state.new_txs]}
    {:noreply, new_state}
  end

  def handle_cast({:block, block, sender}, state) do
    new_state = handle_block_received(block, sender, state)
    new_state = %{new_state | new_blocks: [block | new_state.new_blocks]}
    {:noreply, new_state}
  end

  def handle_cast({:block_mined, block, coinbase_addr}, state) do
    new_state = handle_block_mined(block, coinbase_addr, state)
    new_state = %{new_state | new_blocks: [block | new_state.new_blocks]}
    {:noreply, new_state}
  end

  def handle_cast({:peer_count_change, value}, state) do
    if state.simulation_started? do
      diff = value - state.peer_count
      cond do
        diff > 0 ->
          {new_peers, new_traders, new_miners} = spawn_peers(state.peers, state.traders, state.miners, diff)
          new_state = %{state |
            peer_count: MapSet.size(new_peers),
            trader_count: MapSet.size(new_traders),
            miner_count: MapSet.size(new_miners),
            peers: new_peers,
            traders: new_traders,
            miners: new_miners
          }
          {:noreply, new_state}
        diff < 0 ->
          {new_peers, new_traders, new_miners} = terminate_peers(state.peers, state.traders, state.miners, abs(diff))
          new_state = %{state |
            peer_count: MapSet.size(new_peers),
            trader_count: MapSet.size(new_traders),
            miner_count: MapSet.size(new_miners),
            peers: new_peers,
            traders: new_traders,
            miners: new_miners
          }
          {:noreply, new_state}
        true ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  def handle_cast({:trader_percentage_change, value}, state) do
    if state.simulation_started? do
      peer_count = state.peer_count
      trader_count = peer_count * value / 100 |> trunc()
      current_trader_count = state.trader_count

      diff = trader_count - current_trader_count
      cond do
        diff > 0 ->
          {new_traders, added_traders} = add_trader(state.peers, state.traders, diff)
          notify_role(MapSet.to_list(added_traders), :trader, :add)
          {:noreply, %{state | trader_count: MapSet.size(new_traders), traders: new_traders}}
        diff < 0 ->
          {new_traders, removed_traders} = remove_trader(state.traders, abs(diff))
          notify_role(MapSet.to_list(removed_traders), :trader, :remove)
          {:noreply, %{state | trader_count: MapSet.size(new_traders), traders: new_traders}}
        true ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  def handle_cast({:miner_percentage_change, value}, state) do
    if state.simulation_started? do
      peer_count = state.peer_count
      miner_count = peer_count * value / 100 |> trunc()
      current_miner_count = state.miner_count

      diff = miner_count - current_miner_count
      cond do
        diff > 0 ->
          {new_miners, added_miners} = add_miner(state.peers, state.miners, diff)
          notify_role(MapSet.to_list(added_miners), :miner, :add)
          {:noreply, %{state | miner_count: MapSet.size(new_miners), miners: new_miners}}
        diff < 0 ->
          {new_miners, removed_miners} = remove_miner(state.miners, abs(diff))
          notify_role(MapSet.to_list(removed_miners), :miner, :remove)
          {:noreply, %{state | miner_count: MapSet.size(new_miners), miners: new_miners}}
        true ->
          {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end

  # Aux

  defp get_random_ids(ids, target, result) do
    if MapSet.size(result) == target do
      result
    else
      result = MapSet.put(result, Enum.random(ids))
      get_random_ids(ids, target, result)
    end
  end

  defp spawn_peers(peers, traders, miners, count) do
    notify_role(MapSet.to_list(traders), :trader, :remove)
    notify_role(MapSet.to_list(miners), :miner, :remove)

    target_peer_count = MapSet.size(peers) + count
    trader_count = target_peer_count * GenServer.call(Param, {:get_param, :trader_percentage}) / 100 |> trunc()
    miner_count = target_peer_count * GenServer.call(Param, {:get_param, :miner_percentage}) / 100 |> trunc()

    ids = Enum.reduce(1..count, [], fn(_x, acc) -> [GenServer.call(Tracker, :random_id) | acc] end)
    new_peers = MapSet.new(ids) |> MapSet.union(peers)
    {new_traders, added_traders} = add_trader(new_peers, MapSet.new(), trader_count)
    {new_miners, added_miners} = add_miner(new_peers, MapSet.new(), miner_count)

    spawn_link(fn ->
      Enum.each(ids, fn(id) ->
        {:ok, _} = DynamicSupervisor.start_child(BitcoinSimulator.DynamicSupervisor, Supervisor.child_spec({Node, id}, id: {Node, id}, restart: :temporary))
      end)

      notify_role(MapSet.to_list(added_traders), :trader, :add)
      notify_role(MapSet.to_list(added_miners), :miner, :add)
    end)

    {new_peers, new_traders, new_miners}
  end

  defp terminate_peers(peers, traders, miners, count) do
    notify_role(MapSet.to_list(traders), :trader, :remove)
    notify_role(MapSet.to_list(miners), :miner, :remove)

    target_peer_count = MapSet.size(peers) - count
    trader_count = target_peer_count * GenServer.call(Param, {:get_param, :trader_percentage}) / 100 |> trunc()
    miner_count = target_peer_count * GenServer.call(Param, {:get_param, :miner_percentage}) / 100 |> trunc()

    removed_peers = get_random_ids(MapSet.to_list(peers), count, MapSet.new())
    Enum.each(removed_peers |> MapSet.to_list(), fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, :terminate) end)

    new_peers = MapSet.difference(peers, MapSet.new(removed_peers))
    {new_traders, added_traders} = add_trader(new_peers, MapSet.new(), trader_count)
    notify_role(MapSet.to_list(added_traders), :trader, :add)
    {new_miners, added_miners} = add_miner(new_peers, MapSet.new(), miner_count)
    notify_role(MapSet.to_list(added_miners), :miner, :add)

    {new_peers, new_traders, new_miners}
  end

  defp add_trader(peers, traders, count) do
    non_trader = MapSet.difference(peers, traders)
    added_traders = get_random_ids(non_trader |> MapSet.to_list(), count, MapSet.new())
    {MapSet.union(traders, added_traders), added_traders}
  end

  defp remove_trader(traders, count) do
    removed_traders = get_random_ids(traders |> MapSet.to_list(), count, MapSet.new())
    {MapSet.difference(traders, removed_traders), removed_traders}
  end

  defp add_miner(peers, miners, count) do
    non_miner = MapSet.difference(peers, miners)
    added_miners = get_random_ids(non_miner |> MapSet.to_list(), count, MapSet.new())
    {MapSet.union(miners, added_miners), added_miners}
  end

  defp remove_miner(miners, count) do
    removed_miners = get_random_ids(miners |> MapSet.to_list(), count, MapSet.new())
    {MapSet.difference(miners, removed_miners), removed_miners}
  end

  defp notify_role(ids, role, action) do
    case role do
      :trader ->
        case action do
          :add ->
            Enum.each(ids, fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :trader, :add}) end)
          :remove ->
            Enum.each(ids, fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :trader, :remove}) end)
        end
      :miner ->
        case action do
          :add ->
            Enum.each(ids, fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :miner, :add}) end)
          :remove ->
            Enum.each(ids, fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :miner, :remove}) end)
        end
    end
  end

end
