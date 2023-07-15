#!/usr/bin/env python3

import pathlib
import hashlib
from typing import Iterable, NamedTuple
import subprocess

THIS_DIR = pathlib.Path(__file__).parent
ROOT_DIR = THIS_DIR.parent
BOOK_SRC_DIR = THIS_DIR / 'src'
TMP_SNIPPET_DIR = THIS_DIR / 'tmp_snippets'
NEWS_DIR = THIS_DIR.parent / 'newsfragments'

# use ```fe,ignore on snippets that shouldn't be tested
CODE_SNIPPET_START_MARKER = '```fe'
CODE_SNIPPET_END_MARKER = '```'

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
    for path in NEWS_DIR.rglob('*.md'):
        yield path
    yield ROOT_DIR / 'README.md'

def get_all_snippets_in_file(path: pathlib.Path) -> Iterable[CodeSnippet]:
    with open(path) as f:
        lines = f.readlines()
        snippet = None
        snippet_index = 0
        for line in lines:
            if 'ignore' not in line and line.strip().startswith(CODE_SNIPPET_START_MARKER):
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
    return subprocess.run(["./target/debug/fe", "check", str(path)])

def validate_code_examples() -> None:
    TMP_SNIPPET_DIR.mkdir(exist_ok=True)
    build = subprocess.run(["cargo", "build"])
    if build.returncode == 1:
        exit(1)

    for path in get_all_doc_files():
        for snippet in get_all_snippets_in_file(path):
            fe_snippet = create_fe_file(snippet)
            compile_result = run_snippet(fe_snippet)
            if compile_result.returncode == 1:
                exit(1)

validate_code_examples()
