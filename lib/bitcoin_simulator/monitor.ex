defmodule BitcoinSimulator.Monitor do
  use GenServer

  # Client

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: BitcoinSimulator.Monitor)
  end

  # Server (callbacks)

  def init(arg) do
    {:ok, arg}
  end

  # Aux

end
