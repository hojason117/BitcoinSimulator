defmodule BitcoinSimulatorWeb.DashboardCommander do
  use Drab.Commander

  alias BitcoinSimulator.Simulation.Monitor

  onload :start_live_update

  def start_live_update(socket) do
    spawn_link(fn -> stat_update(socket) end)
  end

  defp stat_update(socket) do
    tx_frequency = GenServer.call(Monitor, {:stat, :tx_frequency})
    mine_frequency = GenServer.call(Monitor, {:stat, :mine_frequency})
    peer_count = GenServer.call(Monitor, {:stat, :peer_count})
    trader_count = GenServer.call(Monitor, {:stat, :trader_count})
    miner_count = GenServer.call(Monitor, {:stat, :miner_count})
    trading_interval = GenServer.call(Monitor, {:stat, :trading_interval})
    blockchain_height = GenServer.call(Monitor, {:stat, :blockchain_height})
    difficulty = GenServer.call(Monitor, {:stat, :difficulty})
    net_worth = GenServer.call(Monitor, {:stat, :net_worth})

    js_script = "
      tx_chart.data.datasets.forEach((dataset) => {
        dataset.data.shift();
        dataset.data.push(#{tx_frequency * 40});
      });
      tx_chart.update();

      mine_chart.data.datasets.forEach((dataset) => {
        dataset.data.shift();
        dataset.data.push(#{mine_frequency * 40});
      });
      mine_chart.update();
    "

    Drab.Core.exec_js(socket, js_script)

    Drab.Live.poke(socket, BitcoinSimulatorWeb.DashboardView, "index.html", peer_count: peer_count, trader_count: trader_count, miner_count: miner_count,
      trading_interval: trading_interval, blockchain_height: blockchain_height, difficulty: difficulty, net_worth: net_worth)

    :timer.sleep(1500)
    stat_update(socket)
  end

end
