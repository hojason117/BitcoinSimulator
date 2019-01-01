defmodule BitcoinSimulatorWeb.InteractivePeerController do
  use BitcoinSimulatorWeb, :controller
  use Drab.Controller, commanders: [BitcoinSimulatorWeb.NavbarCommander]

  def index(conn, _params) do
    conn
    |> render("index.html")
  end

  def show(conn, _params) do
    conn
    |> render("show.html")
  end
end
