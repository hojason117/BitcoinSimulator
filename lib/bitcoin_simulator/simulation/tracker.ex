defmodule BitcoinSimulator.Simulation.Tracker do
  use GenServer

  alias BitcoinSimulator.Const

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Simulation.Tracker)
  end

  # Server (callbacks)

  def init(_) do
    state = %{
      total_peers: 0,
      peer_ids: MapSet.new()
    }

    {:ok, state}
  end

  def handle_call(:random_id, _from, state), do: {:reply, get_random_id(state.peer_ids), state}

  def handle_call({:peer_join, id}, _from, state) do
    neighbors = if state.total_peers == 0, do: MapSet.new(), else: get_random_peers(state.peer_ids, state.total_peers)
    new_state = Map.merge(state, %{total_peers: state.total_peers + 1, peer_ids: MapSet.put(state.peer_ids, id)})
    {:reply, neighbors, new_state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

  defp get_random_id(set) do
    id = :rand.uniform(Const.decode(:max_total_peer))
    if MapSet.member?(set, id), do: get_random_id(set), else: id
  end

  defp get_random_peers(set, total) do
    neighbor_count = Const.decode(:neighbor_count)
    if total < neighbor_count do
      set
    else
      random_peer(set, MapSet.new(), neighbor_count)
    end
  end

  defp random_peer(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: random_peer(set, result, target_count), else: result
  end

end
