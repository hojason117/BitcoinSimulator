defmodule BitcoinSimulator.Peer do

  defmodule Peer do
    defstruct name: nil, hash: nil
  end

  @hash_func :sha

  use GenServer

  # Client

  def start_link(arg) do
    name = {:via, Registry, {BitcoinSimulator.Registry, "node_#{Enum.at(arg, 1)}"}}
    GenServer.start_link(__MODULE__, arg |> Enum.at(0) |> Map.put(:id, Enum.at(arg, 1)), name: name)
  end

  # Server (callbacks)

  def init(arg) do
    {:ok, arg}
  end

  def handle_cast(:terminate, state) do
    new_state = Map.put(state, :alive, false)
    {:stop, :normal, new_state}
  end

  def terminate(reason, _state) do
    if reason != :normal, do: IO.inspect(reason)
    :timer.sleep(1000)
  end

  # Aux

end
