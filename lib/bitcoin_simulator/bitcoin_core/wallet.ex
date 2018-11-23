defmodule BitcoinSimulator.BitcoinCore.Wallet do

  defmodule Wallet do
    defstruct [
      addresses: []
    ]
  end

  defmodule Address do
    defstruct [
      public_key: nil,
      private_Key: nil,
      address: nil
    ]
  end

  # APIs

  def getBalance do

  end

  def getNewAddress do
    private_key = :crypto.strong_rand_bytes(32)
    {public_key, ^private_key} = :crypto.generate_key(:ecdh, :secp256k1, private_key)
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
