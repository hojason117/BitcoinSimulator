defmodule BitcoinSimulator.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @total_peers 100

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Start the Ecto repository
      BitcoinSimulator.Repo,
      # Start the endpoint when the application starts
      BitcoinSimulatorWeb.Endpoint,
      # Starts a worker by calling: BitcoinSimulator.Worker.start_link(arg)
      # {BitcoinSimulator.Worker, arg},
      {Registry, keys: :unique, name: BitcoinSimulator.Registry, partitions: System.schedulers_online()},
      Supervisor.child_spec({BitcoinSimulator.Monitor, %{total_peers: @total_peers}}, restart: :transient)
    ]

    peers = Enum.reduce(@total_peers..1, [],
      fn(x, acc) -> [Supervisor.child_spec({BitcoinSimulator.Peer, [%{}, x]}, id: {BitcoinSimulator.Peer, x}, restart: :temporary) | acc] end)

    children = children ++ peers

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BitcoinSimulator.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BitcoinSimulatorWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
