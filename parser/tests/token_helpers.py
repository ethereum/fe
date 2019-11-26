from collections import (
    OrderedDict,
)
from io import (
    BytesIO,
)
import json
import sys
from tokenize import (
    tokenize,
    TokenInfo,
)
from token import (
    tok_name,
    ENCODING,
    ERRORTOKEN,
    NEWLINE,
    NL,
)
from typing import (
    Iterator,
)


def tokens(source_code: bytes) -> Iterator[TokenInfo]:
    """
    Return an iterator over the tokens in a python source string.
    """
    return tokenize(BytesIO(source_code).readline)


def get_token_dict(tok: TokenInfo) -> OrderedDict:
    """
    Return a JSON-serializable representation of a token.
    """
    # The Python tokenizer has a weird quirk.  If a token's string ends with a
    # newline character, it considers the exclusive ending position of the
    # token to be on the same line as the newline character and one column to
    # the right.  It seems like most people would expect *any* text position
    # that immediately follows a newline character to be on the next line at
    # column zero.  Therefore, the Vyper tokenizer follows this convention and
    # we do a bit of value massaging here to make things line up.
    if tok.type in (ERRORTOKEN, NEWLINE, NL) and tok.string[-1:] == '\n':
        end = (tok.end[0] + 1, 0)
    else:
        end = tok.end

    return OrderedDict((
        ('typ', tok_name[tok.type]),
        ('string', tok.string),
        ('start', tok.start),
        ('end', end),
        ('line', tok.line),
    ))


def get_token_json(source_code: bytes) -> str:
    """
    Return a JSON representation of the tokens in a python source string.
    """
    toks = tokens(source_code)

    # Discard the initial ENCODING token
    tok = next(toks)
    assert tok.type == ENCODING

    return json.dumps(
        [get_token_dict(tok) for tok in toks],
        indent=2,
    )


if __name__ == '__main__':
    sys.stdout.write(get_token_json(sys.stdin.read().encode('utf-8')))
