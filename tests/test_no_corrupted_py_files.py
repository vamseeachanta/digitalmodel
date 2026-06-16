"""Regression guard for #788: every src/**/*.py must be valid Python.

12 files once carried a `.py` extension but were not valid Python (command text,
BOM, mangled imports, an `auth=***` redaction, FastAPI signature corruption) —
they broke imports, packaging, and mypy (mypy aborted exit 2). This test fails
loudly if any non-parseable `.py` file reappears under src/.
"""
import ast
from pathlib import Path

SRC = Path(__file__).resolve().parents[1] / "src"


def test_all_src_py_files_parse() -> None:
    bad: list[str] = []
    for f in SRC.rglob("*.py"):
        try:
            ast.parse(f.read_text(encoding="utf-8"))
        except (SyntaxError, UnicodeDecodeError) as exc:
            bad.append(f"{f.relative_to(SRC.parent)}: {exc}")
    assert not bad, (
        "Non-parseable .py file(s) under src/ (regression of #788 — fix or rename "
        "out of *.py):\n  " + "\n  ".join(bad)
    )
