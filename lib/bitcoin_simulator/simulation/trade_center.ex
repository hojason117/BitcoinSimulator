defmodule BitcoinSimulator.Simulation.TradeCenter do
  use GenServer
  require Logger

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Simulation.TradeCenter)
  end

  # Server (callbacks)

  def init(_) do
    state = %{
      total_peers: 0,
      peer_ids: MapSet.new()
    }

    {:ok, state}
  end

  def handle_call({:get_random_trade_partner, self_id, count}, _from, state) do
    peer_ids_exclude_requester = MapSet.delete(state.peer_ids, self_id)
    if MapSet.size(peer_ids_exclude_requester) < count do
      {:reply, :not_enough_partner, state}
    else
      {:reply, Enum.take_random(peer_ids_exclude_requester, count) |> MapSet.new(), state}
    end
  end

  def handle_call({:join_trading, id}, _from, state) do
    new_state = %{state | total_peers: state.total_peers + 1, peer_ids: MapSet.put(state.peer_ids, id)}
    {:reply, :ok, new_state}
  end

  def handle_call({:leave_trading, id}, _from, state) do
    new_state = %{state | total_peers: state.total_peers - 1, peer_ids: MapSet.delete(state.peer_ids, id)}
    {:reply, :ok, new_state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: Logger.error(reason)

  # Aux

end
