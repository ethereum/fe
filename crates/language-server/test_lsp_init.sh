#!/bin/bash
(
# Initialize
printf 'Content-Length: 185\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":"file:///tmp","capabilities":{"textDocument":{"definition":{"linkSupport":true}}},"trace":"off"}}'
sleep 0.5

# Initialized notification
printf 'Content-Length: 44\r\n\r\n{"jsonrpc":"2.0","method":"initialized"}'
sleep 0.5

# Open a file
printf 'Content-Length: 164\r\n\r\n{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/test.fe","languageId":"fe","version":1,"text":"struct Foo {}\\n\\nfn test() {\\n    let x: Foo\\n}"}}}'
sleep 0.5

# Goto definition on Foo (line 3, char 11)
printf 'Content-Length: 149\r\n\r\n{"jsonrpc":"2.0","id":2,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/test.fe"},"position":{"line":3,"character":11}}}'
sleep 1
) | cargo run -p fe-language-server 2>&1 | tee lsp_output.log