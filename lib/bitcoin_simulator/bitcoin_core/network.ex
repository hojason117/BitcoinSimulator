defmodule BitcoinSimulator.BitcoinCore.Network do
  use Timex

  alias BitcoinSimulator.Const

  defmodule MessageRecord do
    defstruct [
      transactions: Map.new(),
      blocks: Map.new()
    ]
  end

  # APIs

  def getNetworkInfo, do: GenServer.call(BitcoinSimulator.Monitor, :network_info)

  # Aux

  def mix_neighbors(set) do
    neighbor_count = Const.decode(:neighbor_count)
    if MapSet.size(set) < neighbor_count do
      set
    else
      random_peer(set, MapSet.new(), neighbor_count)
    end
  end

  def exchange_neighbors(neighbors) do
    Enum.each(MapSet.to_list(neighbors), fn(x) -> GenServer.cast({:via, Registry, {BitcoinSimulator.Registry, "peer_#{x}"}}, {:exchange_neighbors, neighbors}) end)
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

  def cleanMessageRecord() do
    # TODO
  end

  defp random_peer(set, result, target_count) do
    result = MapSet.put(result, set |> MapSet.to_list() |> Enum.random())
    if MapSet.size(result) < target_count, do: random_peer(set, result, target_count), else: result
  end

end
