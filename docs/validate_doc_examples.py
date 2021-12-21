#!/usr/bin/env python3

import pathlib
import hashlib
from typing import Iterable, NamedTuple
import subprocess

THIS_DIR = pathlib.Path(__file__).parent
BOOK_SRC_DIR = THIS_DIR / 'src'
TMP_SNIPPET_DIR = THIS_DIR / 'tmp_snippets'

# This will need to change in the future when code snippets are annotated with ```fe
CODE_SNIPPET_START_MARKER = '```python'
CODE_SNIPPET_END_MARKER = '```'

# List of known snippets that should be skipped. This is usually the case
# when a snippets don't form a fully working contract. In the future if
# mdBook adds a way to hide code lines for non rust languages, these cases
# could be further minimized: https://github.com/rust-lang/mdBook/issues/1475
# TRY TO KEEP THIS LIST SHORT
SKIP_LIST = [
    'release_notes.md_0.fe',
    'sequence_types_in_memory.md_0.fe',
    'to_mem_function.md_0.fe',
    'visibility_and_privacy.md_0.fe',
    'clone_function.md_0.fe',
    'tokens.md_0.fe',
    'constant_size_values_in_storage.md_0.fe',
    'stack.md_0.fe',
    'functions.md_0.fe',
    'first_contract.md_0.fe',
    'first_contract.md_2.fe'
]

class CodeSnippet(NamedTuple):
    path: pathlib.Path
    index: int
    content: str

    def append_content(self, line: str) -> 'CodeSnippet':
        return CodeSnippet(path=self.path, index=self.index, content=self.content + line)

    def get_snippet_hash(self) -> str:
        return hashlib.sha1(self.content.encode('utf-8')).hexdigest()

    def unique_name(self) -> str:
        return f"{self.path.name}_{self.index}.fe"

def get_all_doc_files() -> Iterable[pathlib.Path]:
    for path in BOOK_SRC_DIR.rglob('*.md'):
        yield path

def get_all_snippets_in_file(path: pathlib.Path) -> Iterable[CodeSnippet]:
    with open(path) as f:
        lines = f.readlines()
        snippet = None
        snippet_index = 0
        for line in lines:
            if line.strip().startswith(CODE_SNIPPET_START_MARKER):
                snippet = CodeSnippet(path=path, index=snippet_index, content='')
                snippet_index += 1
            elif line.strip().startswith(CODE_SNIPPET_END_MARKER):
                if snippet != None:
                    yield snippet
                snippet = None
            elif snippet != None:
                snippet = snippet.append_content(line)

def create_fe_file(snippet: CodeSnippet) -> pathlib.Path:
    fe_snippet_path = TMP_SNIPPET_DIR / snippet.unique_name()
    with open(fe_snippet_path, "w") as snippet_file:
        snippet_file.write(snippet.content)
    return fe_snippet_path

def run_snippet(path: pathlib.Path) -> 'subprocess.CompletedProcess[bytes]':
    return subprocess.run(["cargo", "run", str(path), '--overwrite'])

def validate_code_examples() -> None:
    TMP_SNIPPET_DIR.mkdir(exist_ok=True)
    for path in get_all_doc_files():
        for snippet in get_all_snippets_in_file(path):
            if not snippet.unique_name() in SKIP_LIST:
                fe_snippet = create_fe_file(snippet)
                compile_result = run_snippet(fe_snippet)
                if compile_result.returncode == 1:
                    exit(1)

validate_code_examples()
