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

```python
contract GuestBook:
```

Now, execute `./fe guest_book.fe` to compile the file.

Oops, the compiler is telling us that it didn't expect our code to end here.

```
Unable to compile guest_book.fe.
error: unexpected end of file
  ┌─ guest_book.fe:1:20
  │
1 │ contract GuestBook:
  │                    ^

```

Fe follows Pythonic block indentation rules and the compiler expects us to provide a block of indented code after `GuestBook:`.

Let's expand the code by providing a [`map`](/docs/spec/index.html#51111-hashmap-types) where we can associate messages with Ethereum addresses. The messages will simply be a [`string`](/docs/spec/index.html#51113-string-types) of a maximum length of `100` written as `string100`.
The addresses are represented by the builtin [`address`](/docs/spec/index.html#51110-address-type) type.

```
contract GuestBook:
  messages: Map<address, String<100>>
```

Execute `./fe guest_book.fe` again to recompile the file.

This time, the compiler tells us that it compiled our contract and that it has put the artifacts into a subdirectory called `output`.

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

```python
contract GuestBook:
  messages: Map<address, String<100>>

  pub fn sign(self, book_msg: String<100>):
      self.messages[msg.sender] = book_msg
```

The code should look familiar to those of us that have written Python before except that in Fe every method that is defined without the [`pub`](/docs/spec/index.html#311-visibility-and-privacy) keyword becomes private. Since we want people to interact with our contract and call the `sign` method we have to prefix it with `pub`.

Let's recompile the contract again and see what happens.

```
Failed to write output to directory: `output`. Error: Directory 'output' is not empty. Use --overwrite to overwrite.
```

Oops, the compiler is telling us that the `output` directory is a non-empty directory and plays it safe by asking us if we are sure that we want to overwrite it. We have to use the `--overwrite` flag to allow the compiler to overwrite it is that is stored in the `output` directory.

Let's try it again with `./fe guest_book.fe --overwrite`.

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

```python
contract GuestBook:
  messages: Map<address, String<100>>

  pub fn sign(self, book_msg: String<100>):
      self.messages[msg.sender] = book_msg

  pub fn get_msg(self, addr: address) -> String<100>:
      return self.messages[addr]
```

However, we will hit another error as we try to recompile the current code.

```
Unable to compile guest_book.fe.
error: value must be copied to memory
  ┌─ guest_book.fe:8:14
  │
8 │       return self.messages[addr]
  │              ^^^^^^^^^^^^^^^^^^^ this value is in storage
  │
  = Hint: values located in storage can be copied to memory using the `to_mem` function.
  = Example: `self.my_array.to_mem()`
```

When we try to return a reference type such as an array from the storage of the contract we have to explicitly copy it to memory using the [`to_mem()`](/docs/spec/index.html#623-the-to_mem-function) function.

> Note: In the future Fe will likely introduce immutable storage pointers which might affect these semantics.

The code should compile fine when we change it accordingly.

```python
contract GuestBook:
  messages: Map<address, String<100>>

  pub fn sign(self, book_msg: String<100>):
      self.messages[msg.sender] = book_msg

  pub fn get_msg(self, addr: address) -> String<100>:
      return self.messages[addr].to_mem()
```

Congratulations! You finished your first little Fe project. 👏
In the next chapter we will learn how to deploy our code and tweak it a bit further.
