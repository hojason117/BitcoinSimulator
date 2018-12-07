defmodule BitcoinSimulator.BitcoinCore.Wallet do

  defmodule Wallet do
    defstruct [
      spent_addresses: [],
      unspent_addresses: Map.new(),
      unspent_balance: 0.0
    ]
  end

  defmodule Address do
    defstruct [
      public_key: nil,
      private_Key: nil,
      address: nil,
      value: 0.0,
      outpoint: %{
        hash: nil,
        index: 0
      }
    ]
  end

  # APIs

  def get_new_wallet, do: %Wallet{}

  def get_new_address(wallet) do
    {public_key, private_key} = :crypto.generate_key(:ecdh, :secp256k1)
    addr = :crypto.hash(:ripemd160, :crypto.hash(:sha256, public_key))

    address =
      %Address{
        public_key: public_key,
        private_Key: private_key,
        address: addr
      }

    new_wallet = %{wallet | unspent_addresses: Map.put(wallet.unspent_addresses, address.address, address)}

    {address, new_wallet}
  end

  def combine_unspent_addresses(wallet, target_value) do
    sorted_addresses = wallet.unspent_addresses |> Map.values() |> sort_addresses_by_value()
    combine_address_helper(sorted_addresses, target_value, [], 0.0, 0)
  end

  def spend_address(wallet, address) do
    spent_addr = wallet.unspent_addresses[address]
    %{wallet |
      spent_addresses: wallet.spent_addresses ++ [spent_addr],
      unspent_addresses: Map.delete(wallet.unspent_addresses, address),
      unspent_balance: wallet.unspent_balance - spent_addr.value
    }
  end

  def import_address(wallet, address) do
    %{wallet |
      unspent_addresses: Map.put(wallet.unspent_addresses, address.address, address),
      unspent_balance: wallet.unspent_balance + address.value
    }
  end

  # Aux

  def update_address_detail(details, wallet) do
    Enum.reduce(details |> Map.to_list(), wallet, fn(x, acc) ->
      addr = x |> elem(0)
      addr_value = x |> elem(1) |> elem(0)

      updated = %{acc.unspent_addresses[addr] |
        value: addr_value,
        outpoint: x |> elem(1) |> elem(1)
      }

      %{acc |
        unspent_addresses: Map.put(acc.unspent_addresses, addr, updated),
        unspent_balance: acc.unspent_balance + addr_value
      }
    end)
  end

  defp sort_addresses_by_value(addresses), do: Enum.sort(addresses, fn(a, b) -> a.value < b.value end)

  defp combine_address_helper(addresses, target_value, result, result_sum, current_index) do
    if result_sum >= target_value do
      {result, result_sum}
    else
      current = Enum.at(addresses, current_index)
      unless current.value == 0.0 do
        combine_address_helper(addresses, target_value, [current | result], result_sum + current.value, current_index + 1)
      else
        combine_address_helper(addresses, target_value, result, result_sum, current_index + 1)
      end
    end
  end

end
