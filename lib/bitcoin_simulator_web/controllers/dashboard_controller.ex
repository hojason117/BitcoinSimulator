defmodule BitcoinSimulatorWeb.DashboardController do
  use BitcoinSimulatorWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
