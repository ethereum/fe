
<br /><br />
<center><img alt="Fe logo" src="images/fe.png" height="300" ></center>

## What is Fe?

Fe is the *next generation smart contract language for Ethereum*.

Fe is a [smart contract](https://ethereum.org/en/smart-contracts/#introduction-to-smart-contracts) language that strives to make developing Ethereum smart contract development *safer, simpler and more fun*.

Smart contracts are programs executed by a computer embedded into Ethereum clients known as the [Ethereum Virtual Machine (EVM)](https://ethereum.org/en/developers/docs/evm/). The EVM executes bytecode instructions that are not human readable. Therefore, developers use higher-level languages that compiles to EVM bytecode. 

Fe is one of these languages.

## Why Fe?

Fe aims to make writing secure smart contract code a great experience. With Fe, writing safe code feels natural and fun.

Fe shares similar syntax with the popular languages [Rust](https://doc.rust-lang.org/book/) and [Python](https://www.python.org/), easing the learning curve for new users. It also implements the best features from Rust to limit dynamic behaviour while also maximizing expressiveness, meaning you can write clean, readable code without sacrificing compile time guarantees.

Fe is:
- statically typed
- expressive
- compiled using Yul
- built to a detailed language specification
- able to limit dynamic behaviour
- rapidly evolving!


## Who is Fe for?

Fe is for *anyone that develops using the EVM*!

Fe compiles to EVM bytecode that can be deployed directly onto Ethereum and EVM-equivalent blockchains.

Fe's syntax will feel familiar to Rust and Python developers.

Here's what a minimal contract looks like in Fe:

```fe
contract GuestBook {
  messages: Map<address, String<100>>

  pub fn sign(mut self, ctx: Context, book_msg: String<100>) {
      self.messages[ctx.msg_sender()] = book_msg
  }
}
```


## What problems does Fe solve?

One of the pain points with smart contract languages is that there can be ambiguities in how the compiler translates the human readable code into EVM bytecode. This can lead to security flaws and unexpected behaviours. 

The details of the EVM can also cause the higher level languages to be less intuitive and harder to master than some other languages. These are some of the pain points Fe aims to solve. By striving to *maximize both human readability and bytecode predictability*, Fe will provide an enhanced developer experience for everyone working with the EVM.


## Get Started

You can read much more information about Fe in these docs. If you want to get building, you can begin with our [Quickstart guide](quickstart/index.md).

You can also get involved in the Fe community by contributing code or documentation to the project Github or joining the conversation on [Discord](https://discord.gg/ywpkAXFjZH). Learn more on our [Contributing](contributing.md) page.
