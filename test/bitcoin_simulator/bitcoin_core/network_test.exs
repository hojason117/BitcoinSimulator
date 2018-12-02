defmodule BitcoinSimulator.BitcoinCore.NetworkTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.BitcoinCore.Network

  test "message seen?" do
    record =
      %Network.MessageRecord{
        transactions: Map.new([{:crypto.hash(:sha256, "exist"), Timex.now()}]),
        blocks: Map.new([{:crypto.hash(:sha256, "exist"), Timex.now()}])
      }

    assert Network.message_seen?(record, :transaction, :crypto.hash(:sha256, "exist")) == true
    assert Network.message_seen?(record, :block, :crypto.hash(:sha256, "not exist")) == false
  end

  test "saw message" do
    record = %Network.MessageRecord{}
    assert Network.message_seen?(record, :transaction, :crypto.hash(:sha256, "exist")) == false
    assert Network.message_seen?(record, :block, :crypto.hash(:sha256, "exist")) == false
    record = Network.saw_message(record, :transaction, :crypto.hash(:sha256, "exist"))
    record = Network.saw_message(record, :block, :crypto.hash(:sha256, "exist"))
    assert Network.message_seen?(record, :transaction, :crypto.hash(:sha256, "exist")) == true
    assert Network.message_seen?(record, :block, :crypto.hash(:sha256, "exist")) == true
  end

end
