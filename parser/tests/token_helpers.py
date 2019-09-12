from collections import OrderedDict
import json
from tokenize import tokenize
from token import tok_name, ENCODING


def file_tokens(path):
    """
    Return an iterator over the tokens in a python source file.
    """
    with open(path, 'rb') as f:
        return tokenize(f.readline)


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


def get_token_json(path):
    """
    Return a JSON representation of the tokens in the python source file at
    ``path``.
    """
    tokens = file_tokens(path)

    # Discard the initial ENCODING token
    tok = next(tokens)
    assert tok.type == ENCODING

    return json.dumps(
        map(get_token_dict, tokens),
        indent=2,
    )
