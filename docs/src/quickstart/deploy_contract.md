## Deploy your contract.

Since we have written our first contract now, how about we bring it alive and use it on an actual chain?

Deploying such a demo contract to the Ethereum mainnet would be a waste of money but fortunately we have a few other options to choose from. For instance, we can use our very own local blockchain instance which is great for local development. Alternatively we can use a test network that provides developers shared infrastructure to deploy code without spending actual money on it.

In this guide we will learn how to deploy and interact with our guest book on the [Sepolia testnet](https://sepolia.dev/). Sepolia is the recommended testnet for application development as the previously popular [Görli network is deprecated](https://ethereum.org/en/developers/docs/networks/#goerli) and planned to shut down soon.

### Setting the stage with foundry

The Ethereum ecosystem provides a rich set of tools to assist smart contract developers in various ways when it comes to developing, testing, deploying and upgrading smart contracts. Fe is still a very young language and there are no tools yet that are tailored to the Fe language. Having said that, most tooling should be flexible enough to work with Fe in some way but it might feel slightly less integrated. For this guide we choose to use foundry which is a very lightweight set of command line tools for managing smart contract development.

To follow this guide, you will first need to head over to [getfoundry.sh](https://getfoundry.sh) and follow the installation steps.

> Note: If you are a seasoned smart contract developer who uses different tools, feel free to follow the tutorial using your own toolchain.


### Setting up a Sepolia user account

To deploy our contract to the Sepolia testnet we will need to have an Ethereum account that has some SepoliaETH. SepoliaETH has no real value but it is still a resource that is needed as a basic line of defense against spamming the testnet. If you don't have any SepoliaETH yet, you can [request some SepoliaETH](https://ethereum.org/en/developers/docs/networks/#sepolia) from one of the faucets that are listed on the ethereum.org website.


> **IMPORTANT**: It is good practice to **never** use an Ethereum account for a testnet that is also used for the actual Ethereum mainnet.


### Making the deployment transaction

Let's recall that we finished our guest book in the previous chapter with the following code.

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

If you haven't already, run `./fe build guest_book.fe --overwrite` to obtain the bytecode that we want to deploy.

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
