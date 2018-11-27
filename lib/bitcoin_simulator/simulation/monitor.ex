defmodule BitcoinSimulator.Simulation.Monitor do
  use GenServer

  alias BitcoinSimulator.Const
  alias BitcoinSimulator.Simulation.Peer
  alias BitcoinSimulator.Simulation.Tracker

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Monitor)
  end

  # Server (callbacks)

  def init(_) do
    peer_count = Const.decode(:default_peer_count)
    trader_count = peer_count * Const.decode(:default_trader_percentage) / 100 |> trunc()
    miner_count = peer_count * Const.decode(:default_miner_percentage) / 100 |> trunc()

    ids = Enum.reduce(1..peer_count, [], fn(_x, acc) -> [GenServer.call(Tracker, :random_id) | acc] end)
    Enum.each(ids, fn(id) ->
      {:ok, _} = DynamicSupervisor.start_child(BitcoinSimulator.DynamicSupervisor, Supervisor.child_spec({Peer, id}, id: {Peer, id}, restart: :temporary))
    end)

    traders = get_random_roles(ids, trader_count, MapSet.new())
    miner = get_random_roles(ids, miner_count, MapSet.new())

    Enum.each(MapSet.to_list(traders), fn(x) ->
      GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :trader, :add})
    end)

    Enum.each(MapSet.to_list(miner), fn(x) ->
      GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:modify_role, :miner, :add})
    end)

    state = %{
      peer_count: peer_count,
      trader_count: trader_count,
      miner_count: miner_count,
      traders: traders,
      miner: miner
    }

    {:ok, state}
  end

  def handle_call(:network_info, _from, state), do: {:reply, state, state}

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

  defp get_random_roles(ids, target, result) do
    result = MapSet.put(result, Enum.random(ids))
    if MapSet.size(result) == target, do: result, else: get_random_roles(ids, target, result)
  end

end

# net worth, transaction frequency, peer count,role count
