defmodule BitcoinSimulator.Simulation.Node do
  use BitcoinSimulator.Simulation.Peer

  # Server (callbacks)

  def init(arg), do: {:ok, init_peer(arg)}

  def handle_cast({:transaction, transaction, sender}, state) do
    new_state = handle_transaction_received(transaction, sender, state)
    {:noreply, new_state}
  end

  def handle_cast({:transaction_assembled, transaction, new_wallet}, state) do
    new_state = handle_transaction_assembled(transaction, new_wallet, state)
    {:noreply, new_state}
  end

  def handle_cast({:block, block, sender}, state) do
    new_state = handle_block_received(block, sender, state)
    {:noreply, new_state}
  end

  def handle_cast({:block_mined, block, coinbase_addr}, state) do
    new_state = handle_block_mined(block, coinbase_addr, state)
    {:noreply, new_state}
  end

end
