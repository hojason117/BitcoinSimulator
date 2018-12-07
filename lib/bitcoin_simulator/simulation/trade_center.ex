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
      {:reply, get_partner(peer_ids_exclude_requester, MapSet.new(), count), state}
    end
  end

  def handle_cast({:peer_join, id}, state) do
    new_state = %{state | total_peers: state.total_peers + 1, peer_ids: MapSet.put(state.peer_ids, id)}
    {:noreply, new_state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: Logger.error(reason)

  # Aux

  defp get_partner(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: get_partner(set, result, target_count), else: result
  end

end
