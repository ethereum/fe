## Deploy your contract.

Since we have written our first contract now, how about we bring it to live and use it on an actual chain?

Deploying such a demo contract to the Ethereum mainnet would be a waste of money but fortunately we have a few other options to choose from. For instance, we can use our very own local blockchain instance which is great for local development. Alternatively we can use a test network that provides developers shared infrastructure to deploy code without spending actual money on it.

In this guide we will learn how to deploy and interact with our guest book on the popular [Görli testnet](https://goerli.net/).

### Setting the stage with dapp.tools

The Ethereum ecosystem provides a rich set of tools to assist smart contract developers in various ways when it comes to developing, testing, deploying and upgrading smart contracts. Fe is still a very young language and there are no tools yet that are tailored for the language. Having said that, most tooling should be flexible enough to work with Fe in some way that might feel slightly less integrated. For this guide we choose to use DappTools which is a very lightweight set of command line tools for managing smart contract development.

To follow this guide, you will first need to head over to [dapp.tools](https://dapp.tools) and follow the installation steps.

> Note: If you are a seasoned smart contract developer who uses different tools, feel free to follow the tutorial using your own toolchain.


### Setting up a Görli user account

To deploy our contract to the Görli testnet we will need to have an Ethereum account that has some GöETH. GöETH has no real value but it is still a resource that is needed as a basic line of defense against spamming the testnet. If you don't have any GöETH yet, you can request some from this [faucet](https://goerli-faucet.slock.it/)

The next thing we need is to create a keystore file for our account so that dapp tools can sign messages via [`ethsign`](https://github.com/dapphub/dapptools/tree/master/src/ethsign).

> **IMPORTANT**: It is good practice to **never** use an Ethereum account for a testnet that is also used for the actual Ethereum mainnet.

To create the keystore file for your testnet account, you can use `ethsign` to import your private key. Run the following command and follow the instructions.

```
ethsign import --keystore ~/.ethereum/keystore/
```

### Making the deployment transaction

Let's recall that we finished our guest book in the previous chapter with the following code.

```python
contract GuestBook:
  messages: Map<address, String<100>>

  pub fn sign(self, book_msg: String<100>):
      self.messages[msg.sender] = book_msg

  pub fn get_msg(self, addr: address) -> String<100>:
      return self.messages[addr].to_mem()
```

If you haven't already, run `./fe guest_book.fe --overwrite` to obtain the bytecode that we want to deploy.

To make the deployment, we will need to send a transaction to a node that participates in the Görli network. We can run our own node, sign up at Infura to use one of their nodes or find an open public node such as `https://goerli-light.eth.linkpool.io` which we will use to keep this tutorial as accessible as possible.

Use the following command to deploy the contract. Please note that `<rpc-url>` needs to be replaced with the URL of the node that we connect to and `<our-eth-address>` needs to be replaced with the Ethereum address that we imported in the previous step.

```
$ ETH_RPC_URL=<rpc-url> ETH_FROM=<our-eth-address> seth send --create output/GuestBook/GuestBook.bin
```

What follows is the actual command and the response that was used when writing the tutorial.

```
$ ETH_RPC_URL=https://goerli-light.eth.linkpool.io ETH_FROM=0x4E14AaF86CF0759d6Ec8C7433acd66F07D093293 seth send --create output/GuestBook/GuestBook.bin
seth-send: warning: `ETH_GAS' not set; using default gas amount
Ethereum account passphrase (not echoed): seth-send: Published transaction with 681 bytes of calldata.
seth-send: 0x241ac045170d0612b67b2319fa08ed8be8b79568e00090c4f84146897b83760b
seth-send: Waiting for transaction receipt...............................
seth-send: Transaction included in block 4858224.
0xcecd2be6d4d01ed7906f502be6321c3721f38bc6
```

As we can see in the output, our transaction [`0x241ac045170d0612b67b2319fa08ed8be8b79568e00090c4f84146897b83760b`](https://goerli.etherscan.io/tx/0x241ac045170d0612b67b2319fa08ed8be8b79568e00090c4f84146897b83760b) to deploy the contract is now included in the Görli blockchain. At the very end of the response we find [`0xcecd2be6d4d01ed7906f502be6321c3721f38bc6`](https://goerli.etherscan.io/address/0xcecd2be6d4d01ed7906f502be6321c3721f38bc6) which is the address where our contract is now deployed.

### Signing the guest book

Now that the guest book is live on the Görli network, everyone can send a transaction to sign it. We will sign it from the same address that was used to deploy the contract but there is nothing preventing anyone to sign it from any other address.

The following command will send a transaction to call `sign(string)` with the message *"We <3 Fe"*.

```
ETH_RPC_URL=<rpc-url> ETH_FROM=<our-eth-address> seth send <contract-address> "sign(string)" '"We <3 Fe"'
```

What follows is again the actual command and the response that was used when writing the tutorial.

```
$ ETH_RPC_URL=https://goerli-light.eth.linkpool.io ETH_FROM=0x4E14AaF86CF0759d6Ec8C7433acd66F07D093293 seth send 0xcecd2be6d4d01ed7906f502be6321c3721f38bc6 "sign(string)" '"We <3 Fe"'
seth-send: warning: `ETH_GAS' not set; using default gas amount
Ethereum account passphrase (not echoed): seth-send: Published transaction with 100 bytes of calldata.
seth-send: 0xf61c042064a501939769b802d1455124b0f8665eb1b070c75c2815ca52bd8706
seth-send: Waiting for transaction receipt.............
seth-send: Transaction included in block 4858368.
```

Just as before, the response tells us the transaction hash [`0xf61c042064a501939769b802d1455124b0f8665eb1b070c75c2815ca52bd8706`](https://goerli.etherscan.io/tx/0xf61c042064a501939769b802d1455124b0f8665eb1b070c75c2815ca52bd8706) which we can inspect on Etherscan.

### Reading the signatures

The `get_msg(address)` API let's us read any signature for any address but it will give us an response of 100 zero bytes for any address that simply hasn't signed the guestbook.

Since reading the messages doesn't change any state within the blochchain, we don't have to send and actual transaction. Instead we just perform a *call* against the local state of the node that we are querying.

To do that run:

```
$ ETH_RPC_URL=<rpc-url> seth call <contract-address> "get_msg(address)" <address-to-check> | seth --to-ascii
```

Notice that the command doesn't need to provide `ETH_FROM` simply because we are not sending an actual transaction.

```
$ ETH_RPC_URL=https://goerli-light.eth.linkpool.io seth call 0xcecd2be6d4d01ed7906f502be6321c3721f38bc6 "get_msg(address)" 0x4E14AaF86CF0759d6Ec8C7433acd66F07D093293 | seth --to-ascii
We <3 Fe
```

As we can see in the last line of the output the signature for address `0x4E14AaF86CF0759d6Ec8C7433acd66F07D093293` is in fact *We <3 Fe*.

Congratulations! You've deployed real Fe code to a live network 🤖


