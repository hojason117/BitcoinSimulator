defmodule BitcoinSimulator.Simulation.MonitorTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.Const

  test "did start peers" do
    assert Registry.count(BitcoinSimulator.Registry) == Const.decode(:default_peer_count)
  end

end
