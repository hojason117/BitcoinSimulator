defmodule BitcoinSimulator.BitcoinCore.WalletTest do
  use ExUnit.Case, async: true

  alias BitcoinSimulator.BitcoinCore.Wallet

  test "message seen?" do
    wallet = %Wallet.Wallet{}
    assert wallet.unspent_balance == 0.0

    {address, wallet} = Wallet.get_new_address(wallet)
    assert wallet.unspent_balance == 0.0

    details = %{ address.address => {12.101, %{ hash: <<100::256>>, index: 1 }} }
    wallet = Wallet.update_address_detail(details, wallet)
    assert wallet.unspent_balance == 12.101
    assert wallet.unspent_addresses[address.address].outpoint == %{ hash: <<100::256>>, index: 1 }
  end

end
