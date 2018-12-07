defmodule BitcoinSimulatorWeb.ParamController do
  use BitcoinSimulatorWeb, :controller
  use Drab.Controller, commanders: [BitcoinSimulatorWeb.NavbarCommander]

  alias BitcoinSimulator.Simulation.Param
  alias BitcoinSimulatorWeb.Router.Helpers

  def index(conn, _params) do
    conn
    |> assign(:peer_count, GenServer.call(Param, {:get_param, :peer_count}))
    |> assign(:trader_percentage, GenServer.call(Param, {:get_param, :trader_percentage}))
    |> assign(:miner_percentage, GenServer.call(Param, {:get_param, :miner_percentage}))
    |> assign(:peer_auto_trading_interval_range_min, GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_min}))
    |> assign(:peer_auto_trading_interval_range_max, GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_max}))
    |> assign(:target_difficulty_bits, GenServer.call(Param, {:get_param, :target_difficulty_bits}))
    |> render("index.html")
  end

  def update(conn, params) do
    conn =
      case params["id"] do
        "peer_count" ->
          value = params["peerCount"] |> String.to_integer()
          if value >= 0 and value <= 1000 do
            GenServer.cast(Param, {:set_param, :peer_count, value})
            conn
          else
            conn |> put_flash(:error, "Peer count should be between 0 ~ 1000.")
          end
        "trader_percentage" ->
          value = params["traderPercentage"] |> String.to_integer()
          if value >= 0 and value <= 100 do
            GenServer.cast(Param, {:set_param, :trader_percentage, value})
            conn
          else
            conn |> put_flash(:error, "Percentage should be between 0 ~ 100.")
          end
        "miner_percentage" ->
          value = params["minerPercentage"] |> String.to_integer()
          if value >= 0 and value <= 100 do
            GenServer.cast(Param, {:set_param, :miner_percentage, value})
            conn
          else
            conn |> put_flash(:error, "Percentage should be between 0 ~ 100.")
          end
        "peer_auto_trading_interval_range_min" ->
          value = params["tradeIntervalMin"] |> String.to_integer()
          cond do
            value < 0 ->
              conn |> put_flash(:error, "Must be greater than 0.")
            value > GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_max}) ->
              conn |> put_flash(:error, "Must be less than peer_auto_trading_interval_range_max.")
            true ->
              GenServer.cast(Param, {:set_param, :peer_auto_trading_interval_range_min, value})
              conn
          end
        "peer_auto_trading_interval_range_max" ->
          value = params["tradeIntervalMax"] |> String.to_integer()
          cond do
            value < 0 ->
              conn |> put_flash(:error, "Must be greater than 0.")
            value < GenServer.call(Param, {:get_param, :peer_auto_trading_interval_range_min}) ->
              conn |> put_flash(:error, "Must be greater than peer_auto_trading_interval_range_min.")
            true ->
              GenServer.cast(Param, {:set_param, :peer_auto_trading_interval_range_max, value})
              conn
          end
        "target_difficulty_bits" ->
          value = params["difficultyBits"] |> String.to_integer()
          if value >= 0 and value <= 256 do
            GenServer.cast(Param, {:set_param, :target_difficulty_bits, value})
            conn
          else
            conn |> put_flash(:error, "Number of leading zeros should be between 0 ~ 256.")
          end
      end

    conn |> redirect(to: Helpers.param_path(conn, :index))
  end

end
