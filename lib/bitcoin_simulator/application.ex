defmodule BitcoinSimulator.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias BitcoinSimulator.Simulation.Monitor

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Start the Ecto repository
      # BitcoinSimulator.Repo,
      # Start the endpoint when the application starts
      BitcoinSimulatorWeb.Endpoint,
      # Starts a worker by calling: BitcoinSimulator.Worker.start_link(arg)
      # {BitcoinSimulator.Worker, arg},
      {Registry, keys: :unique, name: BitcoinSimulator.Registry, partitions: System.schedulers_online()},
      Supervisor.child_spec({BitcoinSimulator.Simulation.Param, []}, restart: :transient),
      Supervisor.child_spec({BitcoinSimulator.Simulation.Tracker, []}, restart: :transient),
      {DynamicSupervisor, strategy: :one_for_one, name: BitcoinSimulator.DynamicSupervisor},
      Supervisor.child_spec({BitcoinSimulator.Simulation.TradeCenter, []}, restart: :transient)
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BitcoinSimulator.Supervisor]
    {:ok, pid} = Supervisor.start_link(children, opts)

    {:ok, _} = DynamicSupervisor.start_child(BitcoinSimulator.DynamicSupervisor, Supervisor.child_spec({Monitor, 0}, id: {Monitor, 0}, restart: :transient))

    {:ok, pid}
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BitcoinSimulatorWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
