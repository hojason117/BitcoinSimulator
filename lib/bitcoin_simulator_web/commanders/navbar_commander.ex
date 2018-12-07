defmodule BitcoinSimulatorWeb.NavbarCommander do
  use Drab.Commander

  alias BitcoinSimulator.Simulation.Monitor

  defhandler toggle_simulation(socket, _sender) do
    if GenServer.call(Monitor, :simulation_started?) do
      GenServer.cast(Monitor, :terminate_simulation)
      Drab.Live.poke socket, BitcoinSimulatorWeb.LayoutView, "navbar.html", simulation_started?: false
    else
      GenServer.cast(Monitor, :start_simulation)
      Drab.Live.poke socket, BitcoinSimulatorWeb.LayoutView, "navbar.html", simulation_started?: true
    end
  end

end
