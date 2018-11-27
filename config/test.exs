use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :bitcoin_simulator, BitcoinSimulatorWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
# config :bitcoin_simulator, BitcoinSimulator.Repo,
#   username: "postgres",
#   password: "postgres",
#   database: "bitcoin_simulator_test",
#   hostname: "localhost",
#   pool: Ecto.Adapters.SQL.Sandbox
