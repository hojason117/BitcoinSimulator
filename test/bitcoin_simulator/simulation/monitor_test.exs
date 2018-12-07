defmodule BitcoinSimulator.Simulation.MonitorTest do
  use ExUnit.Case, async: true

  test "did start peers" do
    assert Registry.count(BitcoinSimulator.Registry) == 1
  end

end
