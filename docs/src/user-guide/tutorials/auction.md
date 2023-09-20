# Auction contract

This tutorial aims to implement a simple auction contract in Fe. Along the way you will learn some foundational Fe concepts.

An open auction is one where prices are determined in real-time by live bidding. The winner is the participant who has made the highest bid at the time the auction ends.

## The auction rules

To run an open auction, you need an item for sale, a seller, a pool of buyers and a deadline after which no more bids will be recognized. In this tutorial we will not have an item per se, the buyers are simply bidding to win! The highest bidder is provably crowned the winner, and the value of their bid is passed to the beneficiary. Bidders can also withdraw their bids at any time.


## Get Started

To follow this guide you should have Fe installed on your computer. If you haven't installed Fe yet, follow the instructions on the [Installation](../installation.md) page.

With Fe installed, you can create a project folder, `auction` that will act as your project root. In that folder, create an empty file called `auction.fe`.

Now you are ready to start coding in Fe!

You will also need [Foundry](https://getfoundry.sh) installed to follow the deployment instructions in this guide - you can use your alternative tooling for this if you prefer.

## Writing the Contract

You can see the entire contract [here](../example_contracts/auction_contract.md). You can refer back to this at any time to check implementation details.

### Defining the `Contract` and initializing variables 

A contract is Fe is defined using the `contract` keyword. A contract requires a constructor function to initialize any state variables used by the contract. If no constructor is defined, Fe will add a default with no state variables. The skeleton of the contract can look as follows:

```rust
contract Auction {
    pub fn __init__() {}
}
```

To run the auction you will need several state variables, some of which can be initialized at the time the contract is instantiated.
You will need to track the address of the beneficiary so you know who to pay out to. You will also need to keep track of the highest bidder, and the amount they have bid. You will also need to keep track of how much each specific address has sent into the contract, so you can refund them the right amount if they decide to withdraw. You will also need a flag that tracks whether or not the auction has ended. The following list of variables will suffice:

```sh
auction_end_time: u256
beneficiary: address
highest_bidder: address
highest_bid: u256
pending_returns: Map<address, u256>
ended: bool
```

Notice that variables are named using camel case (lower case, underscore separated, `like_this`). 
[Addresses](../../spec/type_system/types/address.md) have their own type in Fe - it represents 20 hex-encoded bytes as per the Ethereum specification.

The variables that expect numbers are given the `u256` type. This is an unsigned integer of length 256 bits. There are other [choices for integers](../../spec/type_system/types/numeric.md) too, with both signed and unsigned integers between 8 and 256 bits in length.

The `ended` variable will be used to check whether the auction is live or not. If it has finished `ended` will be set to `true`. There are only two possible states for this, so it makes sense to declare it as a `bool` - i.e. true/false.

The `pending returns` variable is a mapping between N keys and N values, with user addresses as the keys and their bids as values. For this, a `Map` type is used. In Fe, you define the types for the key and value in the Map definition - in this case, it is `Map<address, u256>`. Keys can be any `numeric` type, `address`, `boolean` or `unit`.

Now you should decide which of these variables will have values that are known at the time the contract is instantiated. It makes sense to set the `beneficiary` right away, so you can add that to the constructor arguments.

The other thing to consider here is *how* the contract will keep track of time. On its own, the contract has no concept of time. However, the contract does have access to the current block timestamp which is measured in seconds since the Unix epoch (Jan 1st 1970). This can be used to measure the time elapsed in a smart contract. In this contract, you can use this concept to set a deadline on the auction. By passing a length of time in seconds to the constructor, you can then add that value to the current block timestamp and create a deadline for bidding to end. Therefore, you should add a `bidding_time` argument to the constructor. Its type can be `u256`.

When you have implemented all this, your contract should look like this:

```rust
contract Auction {
    // states
    auction_end_time: u256
    beneficiary: address
    highest_bidder: address
    highest_bid: u256
    pending_returns: Map<address, u256>
    ended: bool

    // constructor
    pub fn __init__(mut self, ctx: Context, bidding_time: u256, beneficiary_addr: address) {
        self.beneficiary = beneficiary_addr
        self.auction_end_time = ctx.block_timestamp() + bidding_time
    }
}
```

Notice that the constructor receives values for `bidding_time` and `beneficiary_addr` and uses them to initialize the contract's `auction_end_time` and `beneficiary` variables.

The other thing to notice about the constructor is that there are two additional arguments passed to the constructor: `must self` and `ctx: Context`.

#### self

`self` is used to represent the specific instance of a Contract. It is used to access variables that are owned by that specific instance. This works the same way for Fe contracts as for, e.g. 'self' in the context of classes in Python, or `this` in Javascript.

Here, you are not only using `self` but you are prepending it with `mut`. `mut` is a keyword inherited from Rust that indicates that the value can be overwritten - i.e. it is "mutable". Variables are not mutable by default - this is a safety feature that helps protect developers from unintended changes during runtime. If you do not make `self` mutable, then you will not be able to update the values it contains.


#### Context

Context is used to track deadlines for requests. It gives the contract the ability to cancel functions that have taken too long to run, and provides a structure for storing values that are scoped to a particular request so that they can be accessed anywhere in the call chain. This will be familiar to Go and Rust developers, as Context is often used in the same way in those languages and is frequently employed when working with external API calls. It is conventional to name the context object `ctx`. 

Read more on [Context in Go](https://www.makeuseof.com/go-contexts/)
Read more on [Context in Rust](https://docs.rs/ctx/latest/ctx/)

In Fe contracts `ctx` will also be used to track transaction and blockchain data including `msg.sender`, `msg.value`, `block.timestamp` etc.


### Bidding

Now that you have your contract constructor and state variables, you can implement some logic for receiving bids. To do this, you will create a method called `bid`. To handle a bid, you will first need to determine whether the auction is still open. If it has closed then the bid should revert. If the auction is open you need to record the address of the bidder and the amount and determine whether their bid was the highest. If their bid is highest, then their address should be assigned to the `highest_bidder` variable and the amount they sent recorded in the `highest_bid` variable. 

This logic can be implemented as follows:

```rust
pub fn bid(mut self, mut ctx: Context) {
    if ctx.block_timestamp() > self.auction_end_time {
        revert AuctionAlreadyEnded()
    }
    if ctx.msg_value() <= self.highest_bid {
        revert BidNotHighEnough(highest_bid: self.highest_bid)
    }
    if self.highest_bid != 0 {
        self.pending_returns[self.highest_bidder] += self.highest_bid
    }
    self.highest_bidder = ctx.msg_sender()
    self.highest_bid = ctx.msg_value()

    ctx.emit(HighestBidIncreased(bidder: ctx.msg_sender(), amount: ctx.msg_value()))
}
```

The method first checks that the current block timestamp is not later than the contract's `aution_end_time` variable. If it *is* later, then the contract reverts. This is triggered using the []`revert`](../spec/statements/revert.md) keyword. The `revert` can accept a struct that becomes encoded as [revert data](https://github.com/ethereum/EIPs/issues/838). Here you can just revert without any arguments. Add the following definition somewhere in `Auction.fe` outside the main contract definition:

```rust
struct AuctionAlreadyEnded {
}
```

The next check is whether the incoming bid exceeds the current highest bid. If not, the bid has failed and it may as well revert. We can repeat the same logic as for `AuctionAlreadyEnded`. We can also report the current highest bid in the revert message to help the user reprice if they want to. Add the following to `auction.fe`:

```rust
struct BidNotHighEnough {
    pub highest_bid: u256
}
```

> Notice that the value being checked is `msg.value` which is included in the `ctx` object. `ctx` is where you can access incoming transaction data.

Next, if the incoming transaction *is* the highest bid, you need to track how much the sender should receive as a payout if their bid ends up being exceeded by another user (i.e. if they get outbid, they get their ETH back). To do this, you add a key-value pair to the `pending_returns` mapping, with the user address as the key and the transaction amount as the value. Both of these come from `ctx` in the form of `msg.sender` and `msg.value`.

Finally, if the incoming bid *is* the highest, you can emit an event. Events are useful because they provide a cheap way to return data from a  contract as they use logs instead of contract storage. Unlike other smart contract languages, there is no `emit` keyword or `Event` type. Instead, you trigger an event by calling the `emit` method on the `ctx` object. You can pass this method a struct that defines the emitted message. You can add the following struct for this event:

```rust
struct HighestBidIncreased {
    #indexed
    pub bidder: address
    pub amount: u256
}
```

You have now implemented all the logic to handle a bid!

### Withdrawing

A previous high-bidder will want to retrieve their ETH from the contract so they can either walk away or bid again. You therefore need to create a `withdraw` method that the user can call. The function will lookup the user address in `pending_returns`. If there is a non-zero value associated with the user's address, the contract should send that amount back to the sender's address. It is important to first update the value in `pending_returns` and *then* send the ETH to the user, otherwise you are exposing a [re-entrancy](https://www.certik.com/resources/blog/3K7ZUAKpOr1GW75J2i0VHh-what-is-a-reentracy-attack) vulnerability (where a user can repeatedly call the contract and receive the ETH multiple times).

Add the following to the contract to implement the `withdraw` method:

```rust
pub fn withdraw(mut self, mut ctx: Context) -> bool {
    let amount: u256 = self.pending_returns[ctx.msg_sender()]

    if amount > 0 {
        self.pending_returns[ctx.msg_sender()] = 0
        ctx.send_value(to: ctx.msg_sender(), wei: amount)
    }
    return true
}
```

### End the auction

Finally, you need to add a way to end the auction. This will check whether the bidding period is over, and if it is, automatically trigger the payment to the beneficiary and emit the address of the winner in an event. 

First, check the auction is not still live - if the auction is live you cannot end it early. If an attempt to end the auction early is made, it should revert using a `AuctionNotYetEnded` struct, which can look as follows:

```rust
struct AuctionNotYetEnded {
}
```

You should also check whether the auction was *already* ended by a previous valid call to this method. In this case, revert with a `AuctionEndAlreadyCalled` struct:

```rust
struct AuctionEndAlreadyCalled {}
```

If the auction is still live, you can end it. First set `self.ended` to `true` to update the contract state. Then emit the event using `ctx.emit()`. Then, send the ETH to the beneficiary. Again, the order is important - you should always send value last to protect against re-entrancy.
Your method can look as follows:

```rust
pub fn action_end(mut self, mut ctx: Context) {
    if ctx.block_timestamp() <= self.auction_end_time {
        revert AuctionNotYetEnded()
    }
    if self.ended {
        revert AuctionEndAlreadyCalled()
    }
    self.ended = true
    ctx.emit(AuctionEnded(winner: self.highest_bidder, amount: self.highest_bid))

    ctx.send_value(to: self.beneficiary, wei: self.highest_bid)
}
```

Congratulations! You just wrote an open auction contract in Fe!


## View functions

To help test the contract without having to decode transaction logs, you can add some simple functions to the contract that simply report the current values for some key state variables (specifically, `highest_bidder`, `highest_bid` and `ended`). This will allow a user to use `eth_call` to query these values in the contract. `eth_call` is used for functions that do not update the state of the blockchain and costs no gas because the queries can be performed on local data.

You can add the following functions to the contract:

```rust
pub fn check_highest_bidder(mut self, ctx: Context) -> address {
    return self.highest_bidder;
}

pub fn check_highest_bid(mut self, ctx: Context) -> u256 {
    return self.highest_bid;
}

pub fn check_ended(mut self, ctx: Context) -> bool {
    return self.ended;
}
```

## Build and deploy the contract

Your contract is now ready to use! Compile it using

```sh
fe build auction.fe
```

You will find the contract ABI and bytecode in the newly created `outputs` directory.

Start a local blockchain to deploy your contract to:

```sh
anvil
```

There are constructor arguments (`bidding_time: u256`, `beneficiary_addr: address`) that have to be added to the contract bytecode so that the contract is instantiated with your desired values. To add constructor arguments you can encode them into bytecode and append them to the contract bytecode.

First, hex encode the value you want to pass to `bidding_time`. In this case, we will use a value of 10:

```sh
cast --to_hex(10)

>> 0xa // this is 10 in hex
```

Ethereum addresses are already hex, so there is no further encoding required. The following command will take the constructor function and the hex-encoded arguments and concatenate them into a contiguous hex string, and save it to `constructor_args.txt`. 

```sh
cast abi-encode "__init__(uint256,address)" "0xa" "0xa0Ee7A142d267C1f36714E4a8F75612F20a79720" > constructor_args.txt
```

Now append these constructor argument bytes to the bytecode generated by `fe build` (not that the `cut -c3` command removes the leading `0x` from the bytes being appended)

```sh
cat constructor_args.txt | cut -c3- >> output/Auction/Auction.bin
```

Now deploy the contract to Anvil

```sh
cast send --rpc-url localhost:8545 --private-key <your-private-key> --create $(cat output/SimpleOpenAuction/SimpleOpenAuction.bin)
```

You will see the contract address reported in your terminal.

Now you can interact with your contract. Start by sending an initial bid, let's say 100 ETH. For contract address `0x700b6A60ce7EaaEA56F065753d8dcB9653dbAD35`:

```sh
cast send 0x700b6A60ce7EaaEA56F065753d8dcB9653dbAD35 "bid()" --value "100ether" --private-key <your-private-key> --from 0xa0Ee7A142d267C1f36714E4a8F75612F20a79720
```
 
You can check whether this was successful by calling the `check_highest_bidder()` function:

```sh
cast call 0x700b6A60ce7EaaEA56F065753d8dcB9653dbAD35 "check_highest_bidder()"
```

You will see a response looking similar to:

```sh
0x000000000000000000000000a0Ee7A142d267C1f36714E4a8F75612F20a79720
```

The characters after the leading zeros are the address for the highest bidder (notice they match the characters after the 0x in the bidding address).

You can do the same to check the highest bid:

```sh
cast call 0x700b6A60ce7EaaEA56F065753d8dcB9653dbAD35 "check_highest_bid()"
```

This returns:

```sh
0x0000000000000000000000000000000000000000000000056bc75e2d63100000
```

Converting the non-zero characters to binary gives the decimal value of your bid (in wei - divide by 1e18 to get the value in ETH):

```sh
cast --to-dec 56bc75e2d63100000

>> 100000000000000000000 // 100 ETH in wei
```

Now you can repeat this process, outbidding the initial bid from another address and check the `higyhest_bidder()` and `highest_bid()` to confirm. Do this a few times, then call `end_auction()` to see the value of the highest bid get transferred to the `beneficiary_addr`. You can always check the balance of each address using:

```sh
cast balance <address>
```

And check whether the auction open time has expired using

```sh
cast <contract-address> "check_ended()"
```


## Summary

Congratulations! You wrote an open auction contract in Fe and deployed it to a local blockchain!

If you are using a local Anvil blockchain, you can use the ten ephemeral addresses created when the network started to simulate a bidding war!

By following this tutorial, you learned:

- basic Fe types, such as `bool`, `address`, `map` and `u256`
- basic Fe styles, such as snake case for variable names
- how to create a `contract` with a constructor
- how to `revert`
- how to handle state variables
- how to avoid reentrancy
- how to use `ctx` to handle transaction data
- how to emit events using `ctx.emit`
- how to deploy a contract with constructor arguments using Foundry
- how to interact with your contract