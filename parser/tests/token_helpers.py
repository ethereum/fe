from collections import OrderedDict
from io import BytesIO
import json
from tokenize import tokenize
from token import tok_name, ENCODING


def tokens(source_code: bytes):
    """
    Return an iterator over the tokens in a python source string.
    """
    return tokenize(BytesIO(source_code).readline)


def get_token_dict(tok):
    """
    Return a JSON-serializable representation of a token.
    """
    return OrderedDict((
        ('typ', tok_name[tok.type]),
        ('string', tok.string),
        ('start', tok.start),
        ('end', tok.end),
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
