"""Inventory Python assertions without importing scanned modules.

Each row requires contextual review: a test oracle, an internal invariant and
an evidence trust gate have different purposes. Presence alone establishes no
vulnerability. Python Language Reference, section 7.3, specifies that assert
statements are omitted under optimization; AST parsing retains their source.
"""

from __future__ import annotations

import argparse
import ast
import json
from pathlib import Path
import tokenize


class AssertionVisitor(ast.NodeVisitor):
    """Record enclosing definitions and source locations for manual triage."""

    def __init__(self, path):
        self.path = path
        self.scope = []
        self.rows = []

    def visit_definition(self, node):
        self.scope.append(node.name)
        self.generic_visit(node)
        self.scope.pop()

    visit_ClassDef = visit_definition
    visit_FunctionDef = visit_definition
    visit_AsyncFunctionDef = visit_definition

    def visit_Assert(self, node):
        self.rows.append({
            "path": self.path,
            "line": node.lineno,
            "scope": ".".join(self.scope) or "<module>",
            "test": ast.unparse(node.test),
            "message": ast.unparse(node.msg) if node.msg is not None else None,
        })


def inventory(root, paths):
    """Return a deterministic inventory; missing or unparseable input refuses it."""
    root = Path(root).resolve(strict=True)
    files = set()
    for name in paths:
        candidate = (root / name).resolve()
        if not candidate.exists():
            raise FileNotFoundError("inventory input missing")
        if not candidate.is_relative_to(root):
            raise ValueError("source outside inventory root")
        files.update(candidate.rglob("*.py") if candidate.is_dir() else [candidate])
    rows, scanned = [], []
    for path in sorted(files):
        if not path.resolve().is_relative_to(root):
            raise ValueError("source outside inventory root")
        relative = path.relative_to(root).as_posix()
        with tokenize.open(path) as stream:
            tree = ast.parse(stream.read(), filename=relative)
        visitor = AssertionVisitor(relative)
        visitor.visit(tree)
        rows.extend(visitor.rows)
        scanned.append(relative)
    return {"files_scanned": scanned, "assertions": rows}


def main():
    """Write JSON to stdout; an incomplete inventory exits with an error."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=["src/digitalmodel", "scripts"])
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    args = parser.parse_args()
    print(json.dumps(inventory(args.root, args.paths), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
