defmodule BitcoinSimulator.BitcoinCore.Network do
  use Timex

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Simulation.Tracker
  alias BitcoinSimulator.Const

  defmodule MessageRecord do
    defstruct [
      transactions: Map.new(),
      blocks: Map.new()
    ]
  end

  # APIs

  def get_new_message_record, do: %MessageRecord{}

  def get_initial_neighbors(id), do: GenServer.call(Tracker, {:peer_join, id})

  def get_initial_blockchain(neighbors) do
    if MapSet.size(neighbors) != 0 do
      random_peer = neighbors |> MapSet.to_list() |> Enum.random()
      GenServer.call({:via, Registry, {BitcoinSimulator.Registry, "peer_#{random_peer}"}}, :request_blockchain)
    else
      Blockchain.get_new_blockchain()
    end
  end

  def exchange_neighbors(neighbors) do
    Enum.each(MapSet.to_list(neighbors), fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:exchange_neighbors, neighbors}) end)
  end

  def mix_neighbors(neighbors, self_id) do
    neighbors = MapSet.delete(neighbors, self_id)
    neighbor_count = Const.decode(:neighbor_count)
    if MapSet.size(neighbors) < neighbor_count do
      neighbors
    else
      random_peer(neighbors, MapSet.new(), neighbor_count)
    end
  end

  def message_seen?(record, type, hash) do
    case type do
      :transaction ->
        if Map.has_key?(record.transactions, hash), do: true, else: false
      :block ->
        if Map.has_key?(record.blocks, hash), do: true, else: false
    end
  end

  def saw_message(record, type, hash) do
    case type do
      :transaction ->
        %{record | transactions: Map.put(record.transactions, hash, Timex.now())}
      :block ->
        %{record | blocks: Map.put(record.blocks, hash, Timex.now())}
    end
  end

  def clean_message_record(record) do
    tx_list = Map.to_list(record.transactions)
    block_list = Map.to_list(record.blocks)
    current_time = Timex.now()
    ttl = Const.decode(:network_message_record_ttl)
    drop_tx = Enum.reduce(tx_list, [], fn(x, acc) ->
      if Timex.diff(current_time, elem(x, 1), :milliseconds) > ttl, do: [elem(x, 0) | acc], else: acc
    end)
    drop_block = Enum.reduce(block_list, [], fn(x, acc) ->
      if Timex.diff(current_time, elem(x, 1), :milliseconds) > ttl, do: [elem(x, 0) | acc], else: acc
    end)

    %{record |
      transactions: record.transactions |> Map.drop(drop_tx),
      blocks: record.blocks |> Map.drop(drop_block)
    }
  end

  def broadcast_message(type, message, neighbors, sender) do
    case type do
      :transaction ->
        Enum.each(MapSet.to_list(neighbors), fn(x) ->
          GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:transaction, message, sender})
        end)
      :block ->
        Enum.each(MapSet.to_list(neighbors), fn(x) ->
          GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:block, message, sender})
        end)
    end
  end

  # Aux

  defp random_peer(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: random_peer(set, result, target_count), else: result
  end

end
