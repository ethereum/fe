type BookMsg = bytes[100]

contract GuestBook:
    pub guest_book: map<address, BookMsg>

    pub def sign(book_msg: BookMsg):
        self.guest_book[msg.sender] = book_msg

    pub def get_msg(addr: address) -> BookMsg:
        return self.guest_book[addr]
