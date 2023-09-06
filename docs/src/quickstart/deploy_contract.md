## Deploy your contract.

## Prerequisites 

You should have compiled the `GuestBook` contract and have both `Guestbook_abi.json` and `GuestBook.bin` available in your `outputs` folder.
If you don't have any of these components, please revisit [Write your first contract](./first_contract.md).

## Introduction

When you develop smart contracts it is common to test them on local blockchains first because they are quick and easy to create and it doesn't matter if you make mistakes - there is nothing of real value secured by the blockchain as it only exists on your computer. Later, you can deploy your contract on a public testnet to see how it behaves in a more realistic environment where other developers are also testing their code. Finally, when you are very confident that your contract is ready, you can deploy to Ethereum Mainnet (or one of its live Layer-2 networks). Once the contract is deployed on a "live" network, you are handling assets with real world value!

In this guide you will deploy your contract to a **local blockchain**. This will be an "ephemeral" blockchain, meaning it is completely destroyed every time you shut it down and recreated from scratch every time you start it up - it won't save its state when you shut it down. The benefit of this is quick and easy development. Later in the tutorial you will learn how to deploy to a live public testnet too.

## Your developer environment

Everything in this tutorial can be done by sending JSON data directly to an Ethereum node. However, this is often awkward and error-prone, so a rich ecosystem of tooling has been developed to allow developers to interact with Ethereum in familiar languages or using abstractions that simplify the process.  

In this guide you will use [Foundry](https://book.getfoundry.sh/) which is a very lightweight set of command line tools for managing smart contract development. If you already have Foundry installed, head straight to the next section. If you need to install Foundry, head to [getfoundry.sh](https://getfoundry.sh) and follow the installation steps.

> Note: If you are a seasoned smart contract developer, feel free to follow the tutorial using your own toolchain.

## Starting a local network

Foundry has its own local network called [Anvil](https://book.getfoundry.sh/reference/anvil/). You can use it to create a local blockchain on your computer. Open a terminal and run the following very simple command:

```sh
anvil 
```

You will see some ASCII art and configuration details in the terminal. Anvil creates a set of accounts that you can use on this network. The account addresses and private keys are displayed in the console (**never** use these accounts to interact with any live network). You will also see a line reading `listening on 127.0.0.1:8545`. This indicates that your local node is listening for http traffic on your local network on port 8545 - this is important because this is how you will send the necessary information to your node so that it can be added to the blockchain, and how you will interact with the contract after it is deployed.


### Making the deployment transaction

In the previous guide you wrote the folloiwng contract, and compiled it using `./fe build guest_book.fe --overwrite` to obtain the contract bytecode. This compilation stage converts the human-readable Fe code into a format that can be efficiently executed by Ethereum's embedded computer, known as the Ethereum Virtual Machine (EVM). The bytecode is stored at an address on the blockchain. The contract functions are invoked by sending instructions in a transaction to that address.

```fe
contract GuestBook {
  messages: Map<address, String<100>>

  pub fn sign(mut self, ctx: Context, book_msg: String<100>) {
    self.messages[ctx.msg_sender()] = book_msg
  }

  pub fn get_msg(self, addr: address) -> String<100> {
    return self.messages[addr].to_mem()
  }
}
```

If you haven't already, run  to obtain the bytecode that we want to deploy.

To make the deployment, we will need to send a transaction to a node that participates in the Sepolia network. We can run our own node, sign up at [Infura](https://infura.io/) or [Alchemy](https://www.alchemy.com/) to use one of their nodes or use an open public node such as `https://rpc.sepolia.org` which we will use to keep this tutorial as accessible as possible.


> **IMPORTANT**: foundry provides various options to sign transactions including accessing hardware wallets. Run `cast send --help` and check out the different *wallet options* and choose what fits best.

The following command deploys the Guestbook contract to the Sepolia network.

```
cast send --rpc-url https://rpc.sepolia.org --private-key <your-private-key> --create $(cat output/GuestBook/GuestBook.bin)
```

Here's what the response was at the time of writing this tutorial.

```
blockHash               0x80e7a41dcf846519d18613fb225f85107b6832adeb08bc50e880bd6364e2e4bc
blockNumber             3033409
contractAddress         0x810cbd4365396165874C054d01B1Ede4cc249265
cumulativeGasUsed       1485326
effectiveGasPrice       3000000007
gasUsed                 237781
logs                    []
logsBloom
0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
root
status                  1
transactionHash         0x31b41a4177d7eb66f5ea814959b2c147366b6323f17b6f7060ecff424b58df76
transactionIndex        3
type                    2
```

> Note: Don't assume responses to be identical when following the tutorial. Due to the nature of the blockchain environment the content of the responses will always differ slightly.


As we can see in the output, our transaction [`0x31b41a4177d7eb66f5ea814959b2c147366b6323f17b6f7060ecff424b58df76`](https://sepolia.etherscan.io/tx/0x31b41a4177d7eb66f5ea814959b2c147366b6323f17b6f7060ecff424b58df76) to deploy the contract is now included in the Sepolia blockchain. Under `contractAddress` we find [`0x810cbd4365396165874C054d01B1Ede4cc249265`](https://sepolia.etherscan.io/address/0x810cbd4365396165874c054d01b1ede4cc249265) which is the address where our contract is now deployed.

### Signing the guest book

Now that the guest book is live on the Sepolia network, everyone can send a transaction to sign it. We will sign it from the same address that was used to deploy the contract but there is nothing preventing anyone to sign it from any other address.

The following command will send a transaction to call `sign(string)` on our freshly deployed Guestbook contract sitting at address `0x810cbd4365396165874c054d01b1ede4cc249265` with the message *"We <3 Fe"*.

```
cast send --rpc-url https://rpc.sepolia.org --private-key <your-private-key> 0x810cbd4365396165874C054d01B1Ede4cc249265 "sign(string)" '"We <3 Fe"'
```

Here's what the response looked like:

```
blockHash               0x1d2be32e353aafc74aee417a472d3045087fea15ff6c547947e097d3a8806f3b
blockNumber             3033436
contractAddress         
cumulativeGasUsed       197854
effectiveGasPrice       3000000008
gasUsed                 76485
logs                    [{"address":"0x810cbd4365396165874c054d01b1ede4cc249265","topics":["0xcd5d879305a2503cad08d3e2d007778eec8dc5def7bc74dd20875842b2ff7765"],"data":"0x0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a225765203c332046652200000000000000000000000000000000000000000000","blockHash":"0x1d2be32e353aafc74aee417a472d3045087fea15ff6c547947e097d3a8806f3b","blockNumber":"0x2e495c","transactionHash":"0x85f1fa63ddb551627353b7542541e279dd6be64e183c444ab6a42b9d5e481b59","transactionIndex":"0x1","logIndex":"0x3","removed":false}]
logsBloom               0x00000000000000000000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000004000000000000000000000000020000000000000000400000000000000000000000
root                    
status                  1
transactionHash         0x85f1fa63ddb551627353b7542541e279dd6be64e183c444ab6a42b9d5e481b59
transactionIndex        1
type                    2
```

Just as before, the response tells us the transaction hash [`0x85f1fa63ddb551627353b7542541e279dd6be64e183c444ab6a42b9d5e481b59`](https://sepolia.etherscan.io/tx/0x85f1fa63ddb551627353b7542541e279dd6be64e183c444ab6a42b9d5e481b59) which we can inspect on Etherscan.

### Reading the signatures

The `get_msg(address)` API lets us read any signature for any address but it will give us an response of 100 zero bytes for any address that simply hasn't signed the guestbook.

Since reading the messages doesn't change any state within the blockchain, we don't have to send an actual transaction. Instead we just perform a *call* against the local state of the node that we are querying.

To do that run:

```
$ cast call --rpc-url https://rpc.sepolia.org 0x810cbd4365396165874C054d01B1Ede4cc249265 "get_msg(address)" <your-account-address-that-signed-the-guestbook> | cast --to-ascii
```

Notice that the command doesn't need to provide a private key simply because we are not sending an actual transaction.


And the guestbook entry for address `0x4E14AaF86CF0759d6Ec8C7433acd66F07D093293` is in fact:


```
"We <3 Fe"
```

Congratulations! You've deployed real Fe code to a live network 🤖






### Setting up a Sepolia user account

To deploy our contract to the Sepolia testnet we will need to have an Ethereum account that has some SepoliaETH. SepoliaETH has no real value but it is still a resource that is needed as a basic line of defense against spamming the testnet. If you don't have any SepoliaETH yet, you can [request some SepoliaETH](https://ethereum.org/en/developers/docs/networks/#sepolia) from one of the faucets that are listed on the ethereum.org website.


> **IMPORTANT**: It is good practice to **never** use an Ethereum account for a testnet that is also used for the actual Ethereum mainnet.
