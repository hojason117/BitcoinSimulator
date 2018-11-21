defmodule BitcoinSimulator.MonitorTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.Const

  test "did start child" do
    assert Registry.count(BitcoinSimulator.Registry) == Const.decode(:initial_peer_count)
  end

end
