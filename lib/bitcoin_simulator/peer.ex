defmodule BitcoinSimulator.Peer do
  use GenServer

  alias BitcoinSimulator.BitcoinCore.Tracker

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

    {:ok, state}
  end

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

end
