defmodule BitcoinSimulator.Peer do
  use GenServer

  alias BitcoinSimulator.BitcoinCore.Tracker
  alias BitcoinSimulator.BitcoinCore.Network
  alias BitcoinSimulator.BitcoinCore.Wallet
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
      roles: [],
      wallet: %Wallet.Wallet{}
    }

    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))

    {:ok, state}
  end

  def handle_cast({:exchange_neighbors, peer_neighbors}, state) do
    {:noreply, Map.put(state, :neighbors, Network.mix_neighbors(MapSet.union(state.neighbors, peer_neighbors) |> MapSet.delete(state.id)))}
  end

  def handle_info(:exchange_neighbors, state) do
    spawn_link(Network, :exchange_neighbors, [state.neighbors])
    Process.send_after(self(), :exchange_neighbors, Const.decode(:exchange_neighbors_interval))
    {:noreply, state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

end
