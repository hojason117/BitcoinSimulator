defmodule BitcoinSimulator.BitcoinCore.Network do
  use Timex

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

  def messageSeen?(record, type, hash) do
    case type do
      :transaction ->
        if Map.has_key?(record.transactions, hash), do: true, else: false
      :block ->
        if Map.has_key?(record.blocks, hash), do: true, else: false
    end
  end

  def sawMessage(record, type, hash) do
    case type do
      :transaction ->
        %{record | transactions: Map.put(record.transactions, hash, Timex.now())}
      :block ->
        %{record | blocks: Map.put(record.blocks, hash, Timex.now())}
    end
  end

  def cleanMessageRecord(_record) do
    # TODO
  end

  def broadcast_message(type, message, neighbors) do
    case type do
      :transaction ->
        Enum.each(MapSet.to_list(neighbors), fn(x) ->
          GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:transaction, message})
        end)
      :block ->
        Enum.each(MapSet.to_list(neighbors), fn(x) ->
          GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:block, message})
        end)
    end
  end

  # Aux

  defp random_peer(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: random_peer(set, result, target_count), else: result
  end

end
