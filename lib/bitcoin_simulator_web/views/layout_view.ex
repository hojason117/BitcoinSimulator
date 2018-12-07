defmodule BitcoinSimulatorWeb.LayoutView do
  use BitcoinSimulatorWeb, :view

  alias BitcoinSimulator.Simulation.Monitor

  def simulation_started?(), do: GenServer.call(Monitor, :simulation_started?)

  def navbar_title(path) do
    case path do
      "/" ->
        "Dashboard"
      "/params" ->
        "Simulator Parameters"
      "/interactive_peer" ->
        "Interactive Peer"
      "/blocks" ->
        "Blocks"
      "/transactions" ->
        "Transactions"
    end
  end

end
