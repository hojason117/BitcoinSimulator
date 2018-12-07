defmodule BitcoinSimulatorWeb.DashboardController do
  use BitcoinSimulatorWeb, :controller
  use Drab.Controller, commanders: [BitcoinSimulatorWeb.NavbarCommander]

  alias BitcoinSimulator.Simulation.Monitor

  def index(conn, _params) do
    conn
    |> assign(:peer_count, GenServer.call(Monitor, {:stat, :peer_count}))
    |> assign(:trader_count, GenServer.call(Monitor, {:stat, :trader_count}))
    |> assign(:miner_count, GenServer.call(Monitor, {:stat, :miner_count}))
    |> assign(:trading_interval, GenServer.call(Monitor, {:stat, :trading_interval}))
    |> assign(:blockchain_height, GenServer.call(Monitor, {:stat, :blockchain_height}))
    |> assign(:difficulty, GenServer.call(Monitor, {:stat, :difficulty}))
    |> assign(:net_worth, GenServer.call(Monitor, {:stat, :net_worth}))
    |> render("index.html")
  end
end
