defmodule BitcoinSimulator.Monitor do
  use GenServer

  alias BitcoinSimulator.Peer
  alias BitcoinSimulator.Const
  alias BitcoinSimulator.BitcoinCore.Tracker

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Monitor)
  end

  # Server (callbacks)

  def init(_) do
    peer_count = Const.decode(:initial_peer_count)

    Enum.each(1..peer_count, fn(_x) ->
      id = GenServer.call(Tracker, :random_id)
      {:ok, _} = DynamicSupervisor.start_child(BitcoinSimulator.DynamicSupervisor,
        Supervisor.child_spec({Peer, id}, id: {Peer, id}, restart: :temporary))
    end)

    state = %{
      peer_count: peer_count
    }

    {:ok, state}
  end

  def handle_call(:network_info, _from, state), do: {:reply, state, state}

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

end
