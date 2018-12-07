defmodule BitcoinSimulatorWeb.InteractivePeerController do
  use BitcoinSimulatorWeb, :controller
  use Drab.Controller, commanders: [BitcoinSimulatorWeb.NavbarCommander]

  def index(conn, _params) do
    conn
    |> render("index.html")
  end
end
