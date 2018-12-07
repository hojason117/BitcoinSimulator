defmodule BitcoinSimulator.Simulation.Param do
  use GenServer
  require Logger

  alias BitcoinSimulator.Simulation.Monitor
  alias BitcoinSimulator.Const

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.Simulation.Param)
  end

  # Server (callbacks)

  def init(_) do
    state = %{
      peer_count: Const.decode(:default_peer_count),
      trader_percentage: Const.decode(:default_trader_percentage),
      miner_percentage: Const.decode(:default_miner_percentage),
      peer_auto_trading_interval_range_min: Const.decode(:peer_auto_trading_interval_range_min),
      peer_auto_trading_interval_range_max: Const.decode(:peer_auto_trading_interval_range_max),
      target_difficulty_bits: Const.decode(:target_difficulty_bits)
    }

    {:ok, state}
  end

  def handle_call({:get_param, param_type}, _from, state) do
    case param_type do
      :peer_count ->
        {:reply, state.peer_count, state}
      :trader_percentage ->
        {:reply, state.trader_percentage, state}
      :miner_percentage ->
        {:reply, state.miner_percentage, state}
      :peer_auto_trading_interval_range_min ->
        {:reply, state.peer_auto_trading_interval_range_min, state}
      :peer_auto_trading_interval_range_max ->
        {:reply, state.peer_auto_trading_interval_range_max, state}
      :target_difficulty_bits ->
        {:reply, state.target_difficulty_bits, state}
    end
  end

  def handle_cast({:set_param, param_type, value}, state) do
    case param_type do
      :peer_count ->
        GenServer.cast(Monitor, {:peer_count_change, value})
        {:noreply, %{state | peer_count: value}}
      :trader_percentage ->
        GenServer.cast(Monitor, {:trader_percentage_change, value})
        {:noreply, %{state | trader_percentage: value}}
      :miner_percentage ->
        GenServer.cast(Monitor, {:miner_percentage_change, value})
        {:noreply, %{state | miner_percentage: value}}
      :peer_auto_trading_interval_range_min ->
        {:noreply, %{state | peer_auto_trading_interval_range_min: value}}
      :peer_auto_trading_interval_range_max ->
        {:noreply, %{state | peer_auto_trading_interval_range_max: value}}
      :target_difficulty_bits ->
        {:noreply, %{state | target_difficulty_bits: value}}
    end
  end

  def terminate(reason, _state), do: if reason != :normal, do: Logger.error(reason)

  # Aux

end
