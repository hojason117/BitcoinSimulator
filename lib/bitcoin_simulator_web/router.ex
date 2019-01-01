defmodule BitcoinSimulatorWeb.Router do
  use BitcoinSimulatorWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", BitcoinSimulatorWeb do
    pipe_through :browser

    get "/", DashboardController, :index
    resources "/params", ParamController, only: [:index, :update]
    # resources "/interactive_peer", InteractivePeerController, only: [:index, :show]
    resources "/blockchain_viewer", BlockchainViewerController, only: [:index, :show]
  end

  # Other scopes may use custom stacks.
  # scope "/api", BitcoinSimulatorWeb do
  #   pipe_through :api
  # end
end
