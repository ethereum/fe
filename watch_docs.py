#!/usr/bin/env python3

import sys

try:
    from livereload import Server, shell
except ImportError:
    print('Must install "livereload" package to use live doc server:', file=sys.stderr)
    print('pip3 install livereload', file=sys.stderr)
    sys.exit(1)

try:
    i = sys.argv.index('--')
except ValueError:
    shell_command = 'make build-docs'
else:
    shell_command = ' '.join(sys.argv[i + 1:])

server = Server()
server.watch('**/*.rs', shell(shell_command))
server.watch('**/*.toml', shell(shell_command))
server.serve(root='target/doc')
