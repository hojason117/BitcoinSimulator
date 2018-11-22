defmodule BitcoinSimulator.Peer do
  use GenServer

  alias BitcoinSimulator.BitcoinCore.Tracker
  alias BitcoinSimulator.Const

  # Client

  def start_link(arg) do
    name = {:via, Registry, {BitcoinSimulator.Registry, "peer_#{arg}"}}
    GenServer.start_link(__MODULE__, arg, name: name)
  end

  # Server (callbacks)

  def init(arg) do
    neighbors = GenServer.call(Tracker, {:peer_join, arg})

    state = %{
      id: arg,
      name: "peer_#{arg}",
      neighbors: neighbors,
      roles: []
    }

    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))

    {:ok, state}
  end

  def handle_cast({:exchange_neighbors, peer_neighbors}, state) do
    {:noreply, Map.put(state, :neighbors, mix_neighbors(MapSet.union(state.neighbors, peer_neighbors) |> MapSet.delete(state.id)))}
  end

  def handle_info(:exchange_neighbors, state) do
    spawn_link(fn -> exchange_neighbors(state.neighbors) end)
    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))
    {:noreply, state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

  defp mix_neighbors(set) do
    neighbor_count = Const.decode(:neighbor_count)
    if MapSet.size(set) < neighbor_count do
      set
    else
      random_peer(set, MapSet.new(), neighbor_count)
    end
  end

  defp random_peer(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: random_peer(set, result, target_count), else: result
  end

  defp exchange_neighbors(neighbors) do
    Enum.each(MapSet.to_list(neighbors), fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:exchange_neighbors, neighbors}) end)
  end

end
