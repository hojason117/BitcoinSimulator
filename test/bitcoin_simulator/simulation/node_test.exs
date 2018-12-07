defmodule BitcoinSimulator.Simulation.NodeTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.Simulation.Node
  alias BitcoinSimulator.Const

  test "exchange neighbors" do
    state = %{ id: 1, neighbors: MapSet.new([2, 3, 4, 5, 6, 7, 8, 9, 10, 11]) }
    peer_neighbors = MapSet.new([1, 12, 13, 14, 15, 16, 17, 18, 19, 20])
    {:noreply, new_state} = Node.handle_cast({:exchange_neighbors, peer_neighbors}, state)
    assert MapSet.size(new_state.neighbors) == Const.decode(:neighbor_count)
    refute MapSet.equal?(state.neighbors, new_state.neighbors)
  end

  test "exchange neighbors result doesn't contain self id" do
    state = %{ id: 1, neighbors: MapSet.new() }
    peer_neighbors = MapSet.new([1, 2, 3, 4, 5])
    {:noreply, new_state} = Node.handle_cast({:exchange_neighbors, peer_neighbors}, state)
    refute MapSet.member?(new_state.neighbors, 1)
  end

  test "random transaction value" do
    balance = 21.96
    {transaction_value, transaction_fee, remain_value} = Node.random_transaction_value(balance)
    assert Float.round(transaction_value + transaction_fee + remain_value, Const.decode(:transaction_value_precision)) == balance
  end

end
