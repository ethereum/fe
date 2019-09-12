from collections import OrderedDict
import json
from tokenize import tokenize
from token import tok_name, ENCODING


def get_file_tokens(path):
    with open(path, 'rb') as f:
        return list(tokenize(f.readline))


def get_token_dict(tok):
    return OrderedDict((
        ('typ', tok_name[tok.type]),
        ('string', tok.string),
        ('start', tok.start),
        ('end', tok.end),
        ('line', tok.line),
    ))


def get_file_token_json(path):
    tokens = get_file_tokens(path)
    if tokens[0].type == ENCODING:  # Encoding token
        tokens = tokens[1:]

    return json.dumps(
        [get_token_dict(tok) for tok in tokens],
        indent=2,
    )
