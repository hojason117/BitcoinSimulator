defmodule BitcoinSimulator.BitcoinCore.BlockchainServer do
  use GenServer

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.Const

  defmodule Block do
    defstruct [
      header: %{
        previous_block_hash: nil,
        merkle_root_hash: nil,
        time: nil,
        n_bits: nil,
        nonce: nil
      },
      transactions: []
    ]
  end

  defmodule Transaction do
    defstruct [
      in_count: 0,
      tx_in: [],
      out_count: 0,
      tx_out: [],
      time: nil,
      signatures: [],
      public_keys: []
    ]
  end

  defmodule Txin do
    defstruct [
      previous_output: %{
        hash: nil,
        index: 0
      }
    ]
  end

  defmodule Txout do
    defstruct [
      value: 0,
      address: nil
    ]
  end

  # Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: BitcoinSimulator.BitcoinCore.BlockchainServer)
  end

  # Server (callbacks)

  def init(_) do
    hash_digest = Const.decode(:hash_digest)

    genesis_block = %Block{
      header: %{
        previous_block_hash: <<0::size(hash_digest)>>,
        merkle_root_hash: Blockchain.merkle_root([]),
        time: Time.utc_now(),
        n_bits: 0,
        nonce: 0
      },
      transactions: []
    }

    state = %{
      blocks: [genesis_block],
      hashmap: Map.put(%{}, Blockchain.block_header_hash(genesis_block.header), 0),
      block_count: 1,
      genesis_block: genesis_block,
      tip: genesis_block
    }

    {:ok, state}
  end

  def handle_call(:best_block_header, _from, state), do: {:reply, state.tip.header, state}

  def handle_call({:get_block, hash}, _from, state), do: {:reply, Enum.at(state.blocks, state.hashmap[hash]), state}

  def handle_call(:blockchain_info, _from, state) do
    info = %{
      block_count: state.block_count,
      difficulty: state.tip.n_bits,
      best_block_hash: Blockchain.block_header_hash(state.tip.header)
    }

    {:reply, info, state}
  end

  def handle_call(:block_count, _from, state), do: {:reply, state.block_count, state}

  def handle_call(:difficulty, _from, state), do: {:reply, state.tip.header.n_bits, state}

  def handle_call({:block_hash, height}, _from, state), do: {:reply, Blockchain.block_header_hash(Enum.at(state.blocks, height).header), state}

  def handle_call({:block_header, hash}, _from, state), do: {:reply, Enum.at(state.blocks, state.hashmap[hash]), state}

  def terminate(reason, _state), do: if reason != :normal, do: IO.inspect(reason)

  # Aux

end
