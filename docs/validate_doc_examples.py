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
    'release_notes.md_f004dd3a3facab60a470d9437ad682c7b48476d4.fe',
    'sequence_types_in_memory.md_d0c350a8f03869f88b1bae6308f168b2a53ba091.fe',
    'to_mem_function.md_947ff72560701a3e190ba73683c3ae685f065188.fe',
    'visibility_and_privacy.md_6482a7013a176c42a2cad1e126074a010edb4d74.fe',
    'clone_function.md_641905887e4885c4fe0e64666dc50089182e6c5b.fe',
    'tokens.md_e3f3d78fba190e6ed6072f18136e063d3ee3fb94.fe',
    # This one should be removed when the ICE is fixed
    'statement_let.md_c444f724992ccfee7bb2acf630cf642b832f62fe.fe',
    'constant_size_values_in_storage.md_c48b7ec7779fb87a61d140585729ef1e4c40a2cd.fe',
    'stack.md_e985510948163c9aa6f7bbd245a3ce1a6bc9b064.fe',
    'functions.md_5a6a3b458a39ef0ec9696e59e36e747a8f7779c9.fe',
    'first_contract.md_1fe46f24899f78d00da0444bf5fa1dfe40eb0a2a.fe',
    'first_contract.md_2992d9a656129dc8e4192e598f90b394bc151750.fe'
]

class CodeSnippet(NamedTuple):
    path: pathlib.Path
    content: str

    def append_content(self, line: str) -> 'CodeSnippet':
        return CodeSnippet(path=self.path, content=self.content + line)

    def get_snippet_hash(self) -> str:
        return hashlib.sha1(self.content.encode('utf-8')).hexdigest()

    def unique_name(self) -> str:
        return f"{self.path.name}_{self.get_snippet_hash()}.fe"

def get_all_doc_files() -> Iterable[pathlib.Path]:
    for path in BOOK_SRC_DIR.rglob('*.md'):
        yield path

def get_all_snippets_in_file(path: pathlib.Path) -> Iterable[CodeSnippet]:
    with open(path) as f:
        lines = f.readlines()
        snippet = None
        for line in lines:
            if line.strip().startswith(CODE_SNIPPET_START_MARKER):
                snippet = CodeSnippet(path=path, content='')
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
    return subprocess.run(["cargo", "run", "--features", "solc-backend", str(path), '--overwrite'])

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
