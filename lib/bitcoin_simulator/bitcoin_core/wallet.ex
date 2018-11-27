defmodule BitcoinSimulator.BitcoinCore.Wallet do

  defmodule Wallet do
    defstruct [
      spent_addresses: [],
      unspent_addresses: %{},
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

  def getBalance do

  end

  def getNewAddress do
    {public_key, private_key} = :crypto.generate_key(:ecdh, :secp256k1)
    address = :crypto.hash(:ripemd160, :crypto.hash(:sha256, public_key))

    %Address{
      public_key: public_key,
      private_Key: private_key,
      address: address
    }
  end

  def getReceivedByAddress do

  end

  def getTransaction do

  end

  def getWalletInfo do

  end

  def importPrivKey do

  end

  def listReceivedByAddress do

  end

  def listSinceBlock do

  end

  def listTransactions do

  end

  def listUnspent do

  end

  def sendToAddress do

  end

  def signMessage do

  end

end
