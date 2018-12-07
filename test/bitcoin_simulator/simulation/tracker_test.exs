defmodule BitcoinSimulator.Simulation.TrackerTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.Simulation.Tracker

  test "get random id" do
    id_1 = GenServer.call(Tracker, :random_id)
    id_2 = GenServer.call(Tracker, :random_id)
    refute id_1 == id_2
  end

  test "peer join" do
    id = GenServer.call(Tracker, :random_id)
    neighbors = GenServer.call(Tracker, {:peer_join, id})
    assert MapSet.size(neighbors) == 1
  end

end
