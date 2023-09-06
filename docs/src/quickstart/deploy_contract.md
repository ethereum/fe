## Deploy your contract.

## Prerequisites 

You should have compiled the `GuestBook` contract and have both `Guestbook_abi.json` and `GuestBook.bin` available in your `outputs` folder.
If you don't have any of these components, please revisit [Write your first contract](./first_contract.md).

## Introduction

When you develop smart contracts it is common to test them on local blockchains first because they are quick and easy to create and it doesn't matter if you make mistakes - there is nothing of real value secured by the blockchain as it only exists on your computer. Later, you can deploy your contract on a public test network to see how it behaves in a more realistic environment where other developers are also testing their code. Finally, when you are very confident that your contract is ready, you can deploy to Ethereum Mainnet (or one of its live Layer-2 networks). Once the contract is deployed on a "live" network, you are handling assets with real-world value!

In this guide, you will deploy your contract to a **local blockchain**. This will be an "ephemeral" blockchain, meaning it is completely destroyed every time you shut it down and recreated from scratch every time you start it up - it won't save its state when you shut it down. The benefit of this is quick and easy development, and you don't need to find test ETH to pay gas fees. Later in the guide, you will learn how to deploy to a live public test network too.

## Your developer environment

Everything in this tutorial can be done by sending JSON data directly to an Ethereum node. However, this is often awkward and error-prone, so a rich ecosystem of tooling has been developed to allow developers to interact with Ethereum in familiar languages or using abstractions that simplify the process.  

In this guide, you will use [Foundry](https://book.getfoundry.sh/) which is a very lightweight set of command-line tools for managing smart contract development. If you already have Foundry installed, head straight to the next section. If you need to install Foundry, head to [getfoundry.sh](https://getfoundry.sh) and follow the installation steps.

> Note: If you are a seasoned smart contract developer, feel free to follow the tutorial using your own toolchain.

## Deploying to a local network

Foundry has its own local network called [Anvil](https://book.getfoundry.sh/reference/anvil/). You can use it to create a local blockchain on your computer. Open a terminal and run the following very simple command:

```sh
anvil 
```

You will see some ASCII art and configuration details in the terminal. Anvil creates a set of accounts that you can use on this network. The account addresses and private keys are displayed in the console (**never** use these accounts to interact with any live network). You will also see a line reading `listening on 127.0.0.1:8545`. This indicates that your local node is listening for HTTP traffic on your local network on port 8545 - this is important because this is how you will send the necessary information to your node so that it can be added to the blockchain, and how you will interact with the contract after it is deployed.

> Note: Anvil needs to keep running throughout this tutorial - if you close the terminal your blockchain will cease to exist. Once Anvil has started, open a **new terminal** tab/window to run the rest of the commands in this guide.

### Making the deployment transaction

In the previous guide you wrote the following contract, and compiled it using `./fe build guest_book.fe --overwrite` to obtain the contract bytecode. This compilation stage converts the human-readable Fe code into a format that can be efficiently executed by Ethereum's embedded computer, known as the Ethereum Virtual Machine (EVM). The bytecode is stored at an address on the blockchain. The contract functions are invoked by sending instructions in a transaction to that address.

```rust
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

To make the deployment, we will need to send a transaction to your node via its exposed HTTP port (`8545`). 

The following command deploys the Guestbook contract to your local network. Grab the private key of one of your accounts from the information displayed in the terminal running Anvil.

```sh
cast send --rpc-url localhost:8545 --private-key <your-private-key> --create $(cat output/GuestBook/GuestBook.bin)
```

Here's what the response was at the time of writing this tutorial.

```sh
blockHash               0xcee9ff7c0b57822c5f6dd4fbd3a7e9eadb594b84d770f56f393f137785a52702
blockNumber             1
contractAddress         0x5FbDB2315678afecb367f032d93F642f64180aa3
cumulativeGasUsed       196992
effectiveGasPrice       4000000000
gasUsed                 196992
logs                    []
logsBloom               0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
root                    
status                  1
transactionHash         0x3fbde2a994bf2dec8c11fb0390e9d7fbc0fa1150f5eab8f33c130b4561052622
transactionIndex        0
type                    2
```

This response tells you that your contract has been deployed to the blockchain. The transaction was included in block number 1, and the address it was deployed to is provided in the `contractAddress` field - you need this address to interact with the contract.

> Note: Don't assume responses to be identical when following the tutorial. Due to the nature of the blockchain environment the content of the responses will always differ slightly.


### Signing the guest book

Now that the contract is deployed to the blockchain, you can send a transaction to sign it with a custom message. You will sign it from the same address that was used to deploy the contract, but there is nothing preventing you from using any account for which you have the private key (you could experiment by signing from all ten accounts created by Anvil, for example).

The following command will send a transaction to call `sign(string)` on our freshly deployed Guestbook contract sitting at address `0x810cbd4365396165874c054d01b1ede4cc249265` with the message *"We <3 Fe"*.

```sh
cast send --rpc-url http://localhost:8545 --private-key <your-private-key> <contract-address> "sign(string)" '"We <3 Fe"'
```

The response will look approximately as follows:

```sh
blockHash               0xb286898484ae737d22838e27b29899b327804ec45309e47a75b18cfd7d595cc7
blockNumber             2
contractAddress         
cumulativeGasUsed       36278
effectiveGasPrice       3767596722
gasUsed                 36278
logs                    []
logsBloom               0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
root                    
status                  1
transactionHash         0x309bcea0a77801c15bb7534beab9e33dcb613c93cbea1f12d7f92e4be5ecab8c
transactionIndex        0
type                    2
```

### Reading the signatures

The `get_msg(address)` API allows you to read the messages added to the guestbook for a given address. It will give us a response of 100 zero bytes for any address that hasn't yet signed the guestbook.

Since reading the messages doesn't change any state within the blockchain, you don't have to send an actual transaction. Instead, you can perform a *call* against the local state of the node that you are querying.

To do that run:

```sh
$ cast call --rpc-url https://rpc.sepolia.org <contract-address> "get_msg(address)" <your-account-address-that-signed-the-guestbook>
```

Notice that the command doesn't need to provide a private key simply because we are not sending an actual transaction.

The response arrives in the form of hex-encoded bytes padded with zeroes: 

```sh
0x000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000087765203c33204665000000000000000000000000000000000000000000000000
```

Foundry provides a built-in method to convert this hex string into human-readable ASCII. You can do this as follows:

```sh
cast to_ascii "0x000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000087765203c33204665000000000000000000000000000000000000000000000000"
```

or simply pipe the output of the `cast call` to `to_ascii` to do the query and conversion in a single command:

```sh
cast call --rpc-url https://rpc.sepolia.org <contract-address> "get_msg(address)" <your-account-address-that-signed-the-guestbook> | cast --to-ascii
```

Either way, the response will be the message you passed to the `sign(string)` function.

```sh
"We <3 Fe"
```

Congratulations! You've deployed real Fe code to a local blockchain! 🤖


## Deploying to a public test network

Now you have learned how to deploy your contract to a local blockchain, you can consider deploying it to a public test network too. For more complex projects this can be very beneficial because it allows many users to interact with your contract, simulates real network conditions and allows you to interact with other existing contracts on the network. However, to use a public testnet you need to obtain some of that testnet's gas token. 

In this guide you will use the Sepolia test network, meaning you will need some SepoliaETH. SepoliaETH has no real-world value - it is only required to pay gas fees on the network. If you don't have any SepoliaETH yet, you can [request some SepoliaETH](https://ethereum.org/en/developers/docs/networks/#sepolia) from one of the faucets that are listed on the ethereum.org website.

> **IMPORTANT**: It is good practice to **never** use an Ethereum account for a testnet that is also used for the actual Ethereum mainnet.

Assuming you have some SepoliaETH, you can repeat the steps from the local blockchain example, however, instead of pointing Foundry to the RPC endpoint for your Anvil node, you need to point it to a node connected to the Sepolia network. There are several options for this:

- If you run your own node, connect it to the Sepolia network and let it sync. make sure you expose an http port or enable IPC transport.
- You can use an RPC provider such as [Alchemy](https://www.alchemy.com/) or [Infura](https://infura.io/) 
- You can use an open public node such as `https://rpc.sepolia.org`.

Whichever method you choose, you will have an RPC endpoint for a node connected to Sepolia. You can replace the `http://localhost:8545` in the commands with your new endpoint. For example, to deploy the contract using the open public endpoint:

```sh
cast send --rpc-url https://rpc.sepolia.org --private-key <your-private-key> --create $(cat output/GuestBook/GuestBook.bin)
```

Now you have deployed the contract to a public network and anyone can interact with it. 

To demonstrate, you can check out previous versions of this contract deployed on Sepolia in the past:

|                  | address                                                            | link                                                                                                            |
| ---------------- | ------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| deploy tx hash   | 0x31b41a4177d7eb66f5ea814959b2c147366b6323f17b6f7060ecff424b58df76 | [etherscan](https://sepolia.etherscan.io/tx/0x31b41a4177d7eb66f5ea814959b2c147366b6323f17b6f7060ecff424b58df76) |
| contract address | 0x810cbd4365396165874C054d01B1Ede4cc249265                         | [etherscan](https://sepolia.etherscan.io/address/0x810cbd4365396165874c054d01b1ede4cc249265)                    |


Note that calling the `sign(string)` function will cost you some SepoliaETH because the function changes the state of the blockchain (it adds a message to the contract storage). However, `get_msg(address)` does not cost any gas because it is a simple lookup in the node's local database.

Congratulations! You've deployed real Fe code to a live network 🤖

## Summary

Well done!

You have now written and compiled a Fe contract and deployed it to both a local blockchain and a live public test network! You also learned how to interact with the deployed contract using transactions and calls.

Here's some ideas for what you could do next:

- Experiment with different developer tooling
- Get more comfortable with Foundry by exploring the [documentation](https://book.getfoundry.sh/)
- Repeat the steps in this guide but for a more complex contract - be creative!
- Continue to the [projects](./projects.md) page to learn about how to set up more complicated Fe projects