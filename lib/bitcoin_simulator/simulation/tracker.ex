defmodule BitcoinSimulator.Simulation.Tracker do
  use GenServer
  require Logger

  alias BitcoinSimulator.Const

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Simulation.Tracker)
  end

  # Server (callbacks)

  def init(_) do
    state = %{
      total_peers: 0,
      peer_ids: MapSet.new(),
      distributed_ids: MapSet.new()
    }

    {:ok, state}
  end

  def handle_call(:random_id, _from, state) do
    id = get_random_id(state.distributed_ids)
    {:reply, id, %{state | distributed_ids: MapSet.put(state.distributed_ids, id)}}
  end

  def handle_call({:peer_join, id}, _from, state) do
    neighbors = Enum.take_random(MapSet.to_list(state.peer_ids), Const.decode(:neighbor_count)) |> MapSet.new()
    new_state = %{state | total_peers: state.total_peers + 1, peer_ids: MapSet.put(state.peer_ids, id)}
    Logger.info("Peer joined [id: #{id}]")
    {:reply, neighbors, new_state}
  end

  def handle_call({:peer_leave, id}, _from, state) do
    new_state = %{
      total_peers: state.total_peers - 1,
      peer_ids: MapSet.delete(state.peer_ids, id),
      distributed_ids: MapSet.delete(state.distributed_ids, id)
    }
    Logger.info("Peer left [id: #{id}]")
    {:reply, :ok, new_state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: Logger.error(reason)

  # Aux

  defp get_random_id(set) do
    id = :rand.uniform(Const.decode(:peer_id_range))
    if MapSet.member?(set, id), do: get_random_id(set), else: id
  end

end
