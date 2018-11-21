defmodule BitcoinSimulator.BitcoinCore do

  alias BitcoinSimulator.BitcoinCore.Blockchain
  alias BitcoinSimulator.BitcoinCore.Mining
  alias BitcoinSimulator.BitcoinCore.Network
  alias BitcoinSimulator.BitcoinCore.Wallet

  # Block Chain

  def getBestBlockHash, do: Blockchain.getBestBlockHash()

  def getBlock(hash), do: Blockchain.getBlock(hash)

  def getBlockChainInfo, do: Blockchain.getBlockChainInfo()

  def getBlockCount, do: Blockchain.getBlockCount()

  def getBlockHash(height), do: Blockchain.getBlockHash(height)

  def getBlockHeader(hash), do: Blockchain.getBlockHeader(hash)

  def getDifficulty, do: Blockchain.getDifficulty()

  # Mining

  def getBlockTemplate, do: Mining.getBlockTemplate()

  def getMiningInfo, do: Mining.getMiningInfo()

  def submitBlock, do: Mining.submitBlock()

  # Network

  def addNode, do: Network.addNode()

  def disconnectNode, do: Network.disconnectNode()

  def getAddedNodeInfo, do: Network.getAddedNodeInfo()

  def getConnectionCount, do: Network.getConnectionCount()

  def getNetworkInfo, do: Network.getNetworkInfo()

  def getPeerInfo, do: Network.getPeerInfo()

  # Wallet

  def getAccount, do: Wallet.getAccount()

  def getBalance, do: Wallet.getBalance()

  def getNewAddress, do: Wallet.getNewAddress()

  def getReceivedByAddress, do: Wallet.getReceivedByAddress()

  def getTransaction, do: Wallet.getTransaction()

  def getWalletInfo, do: Wallet.getWalletInfo()

  def importPrivKey, do: Wallet.importPrivKey()

  def listReceivedByAddress, do: Wallet.listReceivedByAddress()

  def listSinceBlock, do: Wallet.listSinceBlock()

  def listTransactions, do: Wallet.listTransactions()

  def listUnspent, do: Wallet.listUnspent()

  def sendToAddress, do: Wallet.sendToAddress()

  def signMessage, do: Wallet.signMessage()

end
