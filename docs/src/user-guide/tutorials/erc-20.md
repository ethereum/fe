# ERC-20 token contract

This tutorial aims to implement the well-known ERC-20 token standard in Fe. Along the way you will learn some foundational Fe concepts, gain exposure to one of the most significant standards on Ethereum.

## Prerequisites

To get the most out of this guide, you should be familiar with the concept of tokens and have some basic knowledge of the Ethereum blockchain. You should have [Fe](../installation.md) and [Foundry](https://getfoundry.sh) installed on your machine. In the `Build, deploy and test` section, it is assumed you saved your contract code in a file named `erc20.fe`.

## What is ERC-20?

ERC-20 describes the most widely-used standard for fungible token contracts. It includes a common set of rules that a contract must adhere to in order to be fully featured and secure.

> Note that ERC-20 is not the only standard for fungible tokens on Ethereum; however, it is by far the most popular.

A token contract contains a mapping between addresses and the number of the contract's tokens they own (i.e. their balance). When a token is transferred, some token balance is decremented from one address in the mapping and added to another address in the mapping. Tokens can also be created (known as "minting") but usually only by the contract owner. Tokens can also be destroyed ('burned'). Minting and burning tokens change the total supply, whereas transferring does not.

The basic ERC-20 contract standard provides the necessary functions that enable these operations to take place safely. The functions that are absolutely necessary for a contract to meet the ERC-20 standard are defined in the contract interface, as expressed in Solidity below:

```solidity
pragma solidity ^0.6.0;

interface IERC20 {

    function totalSupply() external view returns (uint256);
    function balanceOf(address account) external view returns (uint256);
    function allowance(address owner, address spender) external view returns (uint256);

    function transfer(address recipient, uint256 amount) external returns (bool);
    function approve(address spender, uint256 amount) external returns (bool);
    function transferFrom(address sender, address recipient, uint256 amount) external returns (bool);

    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed owner, address indexed spender, uint256 value);
}
```

The next sections will demonstrate how these functions can be implemnted in Fe.

## The ERC-20 Contract in Fe

### Contract definition and constructor

Contracts are defined using the `contract` keyword, with the contract code following inside curly braces `{...}`. The contract's state varables are defined first, then the constructor function. A constructor function sets some of the contract's state variables when the contract is deployed. The state variables required for an ERC-20 contract are:

- balances: the number of tokens owned by an address, with type `Map<address, u256>`
- allowances: The number of tokens the address owner has approved for spending, with type `Map<address, u256>`
- total supply: The total number of tokens in existence, with type `u256`

There are some ERC-20 metadata variables that are not strictly required but are extremely useful and therefore quite ubiquitous in ERC-20 implementations. These include:

- name: the token name, with type `String<100>`
- symbol: the three-character ticker symbol for the token, with type `String<100>`
- decimals: The number of decimals the token should be denominated in (e.g. ETH is expressed to 18 decimal places - the default if no value is defined in the contract)

> Note that in Fe, Strings are defined with a maximum number of characters, `N: String<N>`. N is used to allocate the maximum number of bytes that can be used to store the String's characters. A `String<N>` can be instantiated using < N characters, but not > N characters. For example `Fe is great` is a sequence of 11 characters that could be stored in a `String<11>`, `String<50>` or a `String<100>` but not `String<1>` or `String<10>`.

In the Fe contract, the contract definition and variable declarations look as follows:

```fe
contract ERC20 {
    _balances: Map<address, u256>
    _allowances: Map<address, u256>
    _total_supply: u256
    _name: String<100>
    _symbol: String<100>
    _decimals: u8
```

The constructor function is called when the contract is deployed. It is typical to set the token name, symbol and number of decimals in the constructor and also to mint some number of tokens. The variables and functions are all associated with the contract instance. In Fe, you can access elements owned by a contract instance using `self`. Blockchain interactions are gated behind a `Context` object that needs to be explicitly passed to the constructor function.

```fe
pub fn __init__(mut self, mut ctx: Context, name: String<100>, symbol: String<100>) {
    self._name = name
    self._symbol = symbol
    self._decimals = u8(18)
    self._mint(ctx, account: ctx.msg_sender(), value: 1000_000_000_000_000_000_000_000)
}
```

This constructor instantiates the `name` and `symbol` variables with constructor arguments passed along with the contract code when the contract is deployed. The value of `decimals` is instantiated using a value hard coded into the contract (`self._decimals = u8(18)`). Finally, there is a call to the `mint()` function. You can learn how the `mint()` function works in the next section.

## Contract functions

### `mint()`

The contract's `mint()` function was called in the constructor, causing some amount of tokens to be created and allocated to an account address during the contract deployment. The ERC-20 `mint()` function is not part of the IERC-20 interface so that an ERC-20 ontrat can be agnostic to the token supply mechanism. You can read more about custom supply mechanisms on the [Open Zeppelin forum](https://forum.openzeppelin.com/t/how-to-implement-erc20-supply-mechanisms/226). In this example, you will add a `mint()` function to control your token supply.

The `mint()` function needs to do the following:

- transfer an amount of tokens from the zero address to some other address
- update the total supply
- update the balances of the token receiver's address
- emit an event

This function will update data in the contract storage, so it requires `mut ctx: Context` to be passed as an argument. It also updates values owned by this contract instance, so it requires `self`. `self` is expected to appear first in the parameter list, followed by `Context`. The account to credit with the new tokens (`account: address`) and the number of tokens to mint (`value: u256`) are also required as function arguments.

The function signature can therefore look as follows:

```fe
fn _mint(mut self, mut ctx: Context, account: address, value: u256) {}
```

Inside the function body, you can first check the tokens are not being transferred to the zero address. Then, you can increase the `total_supply` value by `value`, and increment the balance of `account` by `value`. Finally, emit an event. Events are gated behind `ctx` as they add data to the blockchain (specifically, the receipts trie). The full contract looks as follows:

```fe
fn _mint(mut self, mut ctx: Context, account: address, value: u256) {
    assert account != address(0)
    self._total_supply = self._total_supply + value
    self._balances[account] = self._balances[account] + value
    ctx.emit(Transfer(from: address(0), to: account, value))
}
```


### `totalSupply()`

The `totalSupply()` function is a `view` function that simply returns the value of the `_total_supply` variable. Since this function accesses one of the contract's state variables, it takes `self` as an argument. It does not *change* the value, it just reports it, so `self` does not need to be mutable. There are no blockchain interactions, so `Context` is not needed. This function will be called externally, so you can prepend the function declaration with the access modifier `pub`.

```fe
pub fn totalSupply(self) -> u256 {
    return self._total_supply
}
```

### `balanceOf()`

The `balanceOf()` function returns the curent token value associated with a specific address. That is, it returns the `u256` value associated witha given address in the `_balances` variable which has type `Map<address, u256>`. The function interacts with a contract state variable, but does not change it, so `self` should be its first argument. The function also requires an account to look up, with type `address`. This function will be called externally, so you can prepend the function declaration with the access modifier `pub`.

```fe
pub fn balanceOf(self, _ account: address) -> u256 {
    return self._balances[account]
}
```

### `allowance()`

`allowance()` returns the number of tokens that `spender` is allowed to spend on behalf of owner. The idea is that a token holder approves a certain number of tokens to be spent by a `spender` address. each time `spender` transfers some of those tokens, the remaining allowance is decremented by the transfered amount.

The function interacts with a contract state variable, but does not change it, so `self` should be its first argument. Addresses for the `owner` and `spender` are also required. The function then looks up the remaining allowance in `_allowances` using both `spender` and `owner` as keys. This function will be called externally, so you can prepend the function declaration with the access modifier `pub`.

```fe
pub fn allowance(self, owner: address, spender: address) -> u256 {
    return self._allowances[owner][spender]
}
```

### `transfer()`

`transfer()` is the basic mechanism for moving tokens between accounts. It is implemented here as a private function with a public wrapper.
The public wrapper, `transfer()` can be called externally, and its purpose is to retrieve values from an external caller and propagate them to the private `_transfer()` function. The wrapper takes `mut self` and `mut Context` as the first two arguments because the function updates both the contract storage and the blockchain. The `recipient` address and the value of the tokens to transfer are also required arguments. There is no `sender` argument because the only valid sender is the address calling the function, available as `ctx.msg_sender()`.
 
```fe
pub fn transfer(mut self, mut ctx: Context, recipient: address, value: u256) -> bool {
    self._transfer(ctx, sender: ctx.msg_sender(), recipient, value)
    return true
}
```

The private `_transfer()` function is where the token balances are actually updated. First, the function uses `assert()` to ensure the `sender` and `recipient` addresses are valid. Then, the balance of the `sender` and `recipient` are updated individually. Finally, an event is emitted. Emitting events is gated behind `ctx` because it adds data to the blockchain. The function looks as follows:

```fe
fn _transfer(mut self, mut ctx: Context, sender: address, recipient: address, value: u256) {
    assert sender != 0
    assert recipient != 0
    self._balances[sender] = self._balances[sender] - value
    self._balances[recipient] = self._balances[recipient] + value
    ctx.emit(Transfer(from: sender, to: recipient, value))
}
```

You may have noticed that `ctx.emit` emitted a custom `Event` called `Transfer` that you have not yet defined. An `Event` is defined as a `Struct`. You can define `struct Transfer` as follows:

```fe
struct Transfer {
    #indexed
    pub from: address
    #indexed
    pub to: address
    pub value: u256
}
```


### `transferFrom()`

This function moves some amount of tokens from one account to another after checking an `allowance`. The function returns a `bool` indicating success, and emits an event. This function not only queries some of the contract's state variables and the blockchain data, but it updates them too. This means the contract takes `mut self` and `mut Context` as its first two arguments. Then, addresses for the `sender` and `recipient` addresses and the number of tokens to transfer are also required.

In order for a `transfer` to happen, the sender must have approved the address calling the function to move greater than or equal to the value of tokens being transferred. This is a hard requirement that must never be violated! Therefore, you can use `assert()`. To ensure the allowance exceeds the requested amount, you can use:

```
assert self._allowances[sender][ctx.msg_sender()] >= value
```

If the `assert()` evaluates to `false` then the function reverts with code `0x01`and no transfer takes place. You can also provide a custom error message (see [`assert`](../../spec/statements/assert.md)).

Assuming the `assert` evaluates to `true`, you can make a transfer by calling the contract's `transfer()` function. Then, it is critical to update the `allowances` so that the same sender cannot repeatedly spend the `owner`'s tokens and exceed the original allowance. The new allowance should be the allowance prior to the transfer minus the value of the transfer, with a minimum of zero tokens. This is done by calling the contract's `approve()` function.

```fe
pub fn transferFrom(mut self, mut ctx: Context, sender: address, recipient: address, value: u256) -> bool {
    assert self._allowances[sender][ctx.msg_sender()] >= value
    self._transfer(ctx, sender, recipient, value)
    self._approve(ctx, owner: sender, spender: ctx.msg_sender(), value: self._allowances[sender][ctx.msg_sender()] - value)
    return true
}
```

### `approve()`

`approve()` defines the amount of tokens that a token `owner` has allowed a specific `spender` to move on their behalf using `transferFrom()`.
This means `approve()` updates the value of `allowances` for a specific `address`. There is a public wrapper that can be called externally, and a private function that does that actual approval. The wrapper returns a `bool` indicating a successful approval.

```fe
pub fn approve(mut self, mut ctx: Context, spender: address, value: u256) -> bool {
    self._approve(ctx, owner: ctx.msg_sender(), spender, value)
    return true
}
```

The private function again uses `assert()` to check that the `owner` and `spender` addresses are valid, and then updates the value of `allowance` mapping to `spender` and `owner`. Finally, the function emits an `Event`.

```fe
fn _approve(mut self, mut ctx: Context, owner: address, spender: address, value: u256) {
    assert owner != address(0)
    assert spender != address(0)
    self._allowances[owner][spender] = value
    ctx.emit(Approval(owner, spender, value))
}
```

The `Event` emitted by `_approve()` is a custom `struct` called `Approval()`. You can define it as follows:

```fe
struct Approval {
    #indexed
    pub owner: address
    #indexed
    pub spender: address
    pub value: u256
}
```

> Note that at this point, all the functions required by the ERC-20 interface are defined (along with a couple of extra ones). You could deploy this contract now and it would work with the minimum viable functionality. The remaining functions optionally expand the feature set and improve the token UX.

### name

The token name is set by the constructor. It can be queried by calling a public function returning the value of the contract's `_name` variable. Notice that the value is returned after calling the built-in [`to_mem()`](../../spec/data_layout/storage/to_mem_function.md) function. This `to_mem()` function copies a value from contract storage into memory. This is necessary for all reference types (`tuples`, `arrays`, `strings`, `structs`, `enums`, `Map`), but not for primitive types (`bool`, `address`, `numeric`), in Fe.

```fe
pub fn name(self) -> String<100> {
    return self._name.to_mem()
}
```

### symbol

The token symbol is also set by the constructor. It can be queried by calling a public function returning the value of the contract's `_symbol` variable. Again, because the type of `_symbol` is `String<100>`, which is a reference type, it has to be explicitly copied from storasge to memory using `to_mem()`.

```fe
pub fn symbol(self) -> String<100> {
    return self._symbol.to_mem()
}
```

### decimals

Finally, the number of decimals the token is denominated to can be queried by calling a public function returning the value of the contract's `_decimals` variable. The return type is a primitive `u8` so no explicit copying to memory is required.

pub fn decimals(self) -> u8 {
    return self._decimals
}

Congratulations, you have now written all the logic required to implement the ERC-20 token standard. The next section will guide you through deploying and interacting with your contract!


## Build, deploy and test the contract

Compile the contract using:

```sh
fe build erc20.fe
```

You will find the contract ABI and bytecode in the newly created `outputs` directory.

Start a local blockchain to deploy your contract to:

```sh
anvil
```

There are constructor arguments (`name: String<100>`, `symbol: String<100>`) that have to be added to the contract bytecode so that the contract is instantiated with your desired values. To add constructor arguments you can encode them into bytecode and append them to the contract bytecode.

First, hex encode the values you want to pass as constructor arguments. You can name the token `Fetoken` and assign the symbol `FET`:

```sh
cast --from-ascii("fetoken")
>> 0x6665746f6b656e
cast --from-ascii("fet")
>> 0x666574
```

Ethereum addresses are already hex, so there is no further encoding required. The following command will take the constructor function and the hex-encoded arguments and concatenate them into a contiguous hex string and then deploy the contract with the constructor arguments. 

```sh
cast send --from <your-address> --private-key <your-private-key> --create $(cat output/ERC20/ERC20.bin) $(cast abi-encode "__init__(string,string)" 0x6665746f6b656e 0x666574)
```

You will see the contract address reported in your terminal. The adress you provided the private key for in your deployment command was interpreted by the contract as `msg_sender`, meaning it now owns 1000000000000000000000000 Fe tokens, as defined in the constructor function. This should also be the total supply.

You can check this using:

```sh
cast call <contract-address> "total_supply" | cast --to-dec
>> 1000000000000000000000000

cast call <contract-address> "balanceOf(address)" <your-address> | cast --to-dec
>> 1000000000000000000000000
```

Now, make a transfer from the funded account to another account:

```sh
cast send <contract-address> "transfer(address,uint256)" <recipient-address> <amount> --from <your-address> --private-key <your-priv-key> 
```

You will see the transaction details reported in the terminal, including logs. It is out of scope for this page but if you decoded these logs you would see the Transfer struct you defined in the contract populated with the relevant values.

You can also try uisng `TransferFrom`. In this case, you first need to approve some amount of tokens that can be spent by a specific `spender` account. Then, call `TransferFrom()` with a number of tokens smaller than the apprived amount. You can also experiment with sending amoutns greater than the allowance to check that the transaction reverts as expected.

```sh
cast send <contract-address> "approve(address,uint256)" <spender-address> <amount> --from <your-address> --private-key <your private-key>
cast send <contract-address> "transferFrom(address,address,uint256)" <spender-address> <recipient-address> <amount> --from <spender-address> --private-key <spender-priv-key>
```
You can check the bal;ances have been updated as expected by calling `balanceOf` for each address and checking that each bal;ance has been incremented or decremented by `amount` number of tokens.

> Note that the functions that take `self` and/or `Context` as opposed to `mut self` and/or `mut Context` are accessed using `cast call` rather than `cast send`. This is because they do not modify any contract or blockchain data. This means they do not need to sign and send a transaction or pay any gas. 

You can experiment with the other ERC-20 functions following the same patterns.

## Summary

Congratulations! You have written an ERC-20 token contract and deployed it to a local blockchain. In doing so, you learned:

- the features and structure of the ERC-20 token standard
- how to explicitly move reference values into memory using `to_mem()`
- how to use `self` and `Context` in Fe functions
- how to emit custom events
- how to build a contract using `fe build`
- how to deploy an interact with a contract using Foundry