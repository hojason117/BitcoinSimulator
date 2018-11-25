defmodule BitcoinSimulator.Simulation.TradeCenterTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.Simulation.TradeCenter
  alias BitcoinSimulator.Const

  test "get random trade partner" do
    self_id = 1
    peer_ids = MapSet.new([self_id, 5, 87, 96, 4, 1687, 69, 888, 773, 274])
    state = %{ total_peers: 10, peer_ids: peer_ids }
    {:reply, partners, _state} = TradeCenter.handle_call({:get_random_trade_partner, self_id, Const.decode(:txout_count_range)}, self(), state)
    assert MapSet.size(partners) == Const.decode(:txout_count_range)
    refute MapSet.member?(partners, self_id)
  end

  test "get random trade partner - not enough partner" do
    self_id = 1
    peer_ids = MapSet.new([self_id, 2, 3])
    state = %{ total_peers: 3, peer_ids: peer_ids }
    {:reply, partners, _state} = TradeCenter.handle_call({:get_random_trade_partner, self_id, 3}, self(), state)
    assert partners == :not_enough_partner
  end

end
