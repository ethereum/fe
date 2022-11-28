## Write your first Fe contract

Now that we have the compiler installed let's write our first contract. A contract contains the code that will be deployed to the Ethereum blockchain and resides at a specific address.

The code of the contract dictates how:
  - it manipulates its own state
  - interacts with other contracts
  - exposes external APIs to be called from other contracts or users

To keep things simple we will just write a basic *guestbook* where people can leave a message associated with their Ethereum address.

>Note: Real code would not instrument the Ethereum blockchain in such a way as it is a waste of precious resources. This code is for demo purposes only.

### Create a `guest_book.fe` file

Fe code is written in files ending on the `.fe` file extension. Let's create a file `guest_book.fe` and put in the following content.

```fe
contract GuestBook {
  messages: Map<address, String<100>>
}
```

Here we're using a [`map`](../spec/type_system/types/map.md) to associate messages with Ethereum addresses.
The messages will simply be a [`string`](../spec/type_system/types/string.md) of a maximum length of `100` written as `String<100>`.
The addresses are represented by the builtin [`address`](../spec/type_system/types/address.md) type.

Execute `./fe build guest_book.fe` to compile the file. The compiler tells us that it compiled our contract and that it has put the artifacts into a subdirectory called `output`.

```
Compiled guest_book.fe. Outputs in `output`
```

If we examine the `output` directory we'll find a subdirectory `GuestBook` with a `GuestBook_abi.json` and a `GuestBook.bin` file.

```
├── fe
├── guest_book.fe
└── output
    └── GuestBook
        ├── GuestBook_abi.json
        └── GuestBook.bin
```

The `GuestBook_abi.json` is a JSON representation that describes the binary interface of our contract but since our contract doesn't yet expose anything useful its content for now resembles an empty array.

The `GuestBook.bin` is slightly more interesting containing what looks like a gibberish of characters which in fact is the compiled binary contract code written in [hexadecimal](https://en.wikipedia.org/wiki/Hexadecimal) characters.

We don't need to do anything further yet with these files that the compiler produces but they will become important when we get to the point where we want to deploy our code to the Ethereum blockchain.

### Add a method to sign the guest book

Let's focus on the functionality of our world changing application and add a method to sign the guestbook.

```fe
contract GuestBook {
  messages: Map<address, String<100>>

  pub fn sign(mut self, ctx: Context, book_msg: String<100>) {
      self.messages[ctx.msg_sender()] = book_msg
  }
}
```

In Fe, every method that is defined without the [`pub`](../spec/items/visibility_and_privacy.md) keyword becomes private. Since we want people to interact with our contract and call the `sign` method we have to prefix it with `pub`.

Let's recompile the contract again and see what happens.

```
Failed to write output to directory: `output`. Error: Directory 'output' is not empty. Use --overwrite to overwrite.
```

Oops, the compiler is telling us that the `output` directory is a non-empty directory and plays it safe by asking us if we are sure that we want to overwrite it. We have to use the `--overwrite` flag to allow the compiler to overwrite what is stored in the `output` directory.

Let's try it again with `./fe build guest_book.fe --overwrite`.

This time it worked and we can also see that the `GuestBook_abi.json` has become slightly more interesting.

```json
[
  {
    "name": "sign",
    "type": "function",
    "inputs": [
      {
        "name": "book_msg",
        "type": "bytes100"
      }
    ],
    "outputs": []
  }
]
```

Since our contract now has a public `sign` method the corresponding ABI has changed accordingly.

### Add a method to read a message

To make the guest book more useful we will also add a method `get_msg` to read entries from a given address.

```fe,ignore
contract GuestBook {
  messages: Map<address, String<100>>

  pub fn sign(mut self, ctx: Context, book_msg: String<100>) {
      self.messages[ctx.msg_sender()] = book_msg
  }

  pub fn get_msg(self, addr: address) -> String<100> {
      return self.messages[addr]
  }
}
```

However, we will hit another error as we try to recompile the current code.

```
Unable to compile guest_book.fe.
error: value must be copied to memory
  ┌─ guest_book.fe:10:14
  │
8 │       return self.messages[addr]
  │              ^^^^^^^^^^^^^^^^^^^ this value is in storage
  │
  = Hint: values located in storage can be copied to memory using the `to_mem` function.
  = Example: `self.my_array.to_mem()`
```

When we try to return a reference type such as an array from the storage of the contract we have to explicitly copy it to memory using the [`to_mem()`](../spec/data_layout/storage/to_mem_function.md) function.

> Note: In the future Fe will likely introduce immutable storage pointers which might affect these semantics.

The code should compile fine when we change it accordingly.

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

Congratulations! You finished your first little Fe project. 👏
In the next chapter we will learn how to deploy our code and tweak it a bit further.
