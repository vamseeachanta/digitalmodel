#!/usr/bin/env python3
"""Deterministic structural snapshot of the public Python surface (#1961, D5).

Emits, per module under the scanned subtree: the declared exports, every public
symbol with its signature rendered including default *literals*, whether the
module carries an ``if __name__ == "__main__"`` dispatch, and a digest per
generated artifact named in a census file.

It reads **Git blobs, not the working tree**. A snapshot built from the working
tree on both sides of a comparison is the same bytes through the same reader,
which is a symmetric comparison that proves nothing.

This is not a pytest assertion. The surface it describes -- exported API,
emitted defaults, generated artifacts -- is by construction the part internal
tests do not exercise: anything internal depending on it would have surfaced the
problem long ago. A module split once carried a ``__main__`` dispatch away and
turned a documented module-execution path into a silent no-op; 723 passing tests
did not notice, and one namespace comparison did.

Exit codes: 0 emitted, 2 usage, 3 a required module is absent from the census.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import subprocess
import sys
import warnings
from pathlib import Path

EXIT_OK, EXIT_USAGE, EXIT_CENSUS = 0, 2, 3
DEFAULT_SUBTREE = "src/digitalmodel"


def _git(root: Path, *args: str) -> bytes:
    proc = subprocess.run(["git", *args], cwd=str(root), capture_output=True)
    if proc.returncode != 0:
        raise SystemExit(
            f"git {' '.join(args)} failed: {proc.stderr.decode('utf-8', 'replace').strip()}"
        )
    return proc.stdout


def _list_blobs(root: Path, ref: str, subtree: str) -> list[str]:
    out = _git(root, "ls-tree", "-r", "-z", "--name-only", ref, "--", subtree)
    return sorted(p for p in out.decode("utf-8", "surrogateescape").split("\0") if p)


class _BatchReader:
    """One ``git cat-file --batch`` process for the whole run."""

    def __init__(self, root: Path) -> None:
        self._proc = subprocess.Popen(
            ["git", "cat-file", "--batch"],
            cwd=str(root),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )

    def read(self, spec: str) -> bytes:
        assert self._proc.stdin and self._proc.stdout
        self._proc.stdin.write(spec.encode("utf-8", "surrogateescape") + b"\n")
        self._proc.stdin.flush()
        header = self._proc.stdout.readline().split()
        if len(header) < 3:
            raise SystemExit(f"cannot read blob: {spec}")
        size = int(header[2])
        data = self._proc.stdout.read(size)
        self._proc.stdout.read(1)
        return data

    def close(self) -> None:
        assert self._proc.stdin
        self._proc.stdin.close()
        self._proc.wait()


def _exports(tree: ast.Module) -> list[str]:
    for node in tree.body:
        targets = node.targets if isinstance(node, ast.Assign) else []
        if isinstance(node, ast.AnnAssign):
            targets = [node.target]
        for target in targets:
            if isinstance(target, ast.Name) and target.id == "__all__":
                if isinstance(node.value, (ast.List, ast.Tuple)):
                    return sorted(
                        element.value
                        for element in node.value.elts
                        if isinstance(element, ast.Constant) and isinstance(element.value, str)
                    )
    return []


def _signature(node: ast.FunctionDef | ast.AsyncFunctionDef) -> str:
    return "(" + ast.unparse(node.args) + ")"


def _bases(node: ast.ClassDef) -> str:
    return "class(" + ", ".join(ast.unparse(base) for base in node.bases) + ")"


def _symbols(tree: ast.Module) -> dict[str, str]:
    found: dict[str, str] = {}
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            if not node.name.startswith("_"):
                found[node.name] = _signature(node)
        elif isinstance(node, ast.ClassDef) and not node.name.startswith("_"):
            found[node.name] = _bases(node)
            for member in node.body:
                if isinstance(member, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    if not member.name.startswith("_"):
                        found[f"{node.name}.{member.name}"] = _signature(member)
    return found


def _has_main_dispatch(tree: ast.Module) -> bool:
    for node in tree.body:
        if not isinstance(node, ast.If):
            continue
        test = node.test
        if not isinstance(test, ast.Compare) or len(test.comparators) != 1:
            continue
        left, right = test.left, test.comparators[0]
        if isinstance(left, ast.Name) and left.id == "__name__":
            if isinstance(right, ast.Constant) and right.value == "__main__":
                return True
    return False


def _module_entry(data: bytes) -> dict | None:
    try:
        with warnings.catch_warnings():
            # Legacy modules carry invalid escape sequences. Their warnings are
            # not this tool's business and would drown its output.
            warnings.simplefilter("ignore", SyntaxWarning)
            tree = ast.parse(data.decode("utf-8", "replace"))
    except SyntaxError:
        # An unparseable module is recorded, never dropped: dropping it from both
        # sides of a comparison is exactly the symmetric exclusion this guards.
        return {"all": [], "symbols": {}, "main_dispatch": False, "unparseable": True}
    return {
        "all": _exports(tree),
        "symbols": _symbols(tree),
        "main_dispatch": _has_main_dispatch(tree),
    }


def _artifact_digests(reader: _BatchReader, ref: str, census: list[str]) -> dict[str, str]:
    return {
        path: hashlib.sha256(reader.read(f"{ref}:{path}")).hexdigest() for path in sorted(census)
    }


def _census_from(path: Path | None) -> list[str]:
    if path is None:
        return []
    manifest = json.loads(path.read_text(encoding="utf-8"))
    return list(manifest.get("generated_census", []))


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--ref", required=True)
    parser.add_argument("--subtree", default=DEFAULT_SUBTREE)
    parser.add_argument("--manifest", type=Path, default=None)
    parser.add_argument("--require-module", action="append", default=[])
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(sys.argv[1:] if argv is None else argv)
    root = args.root.resolve()
    reader = _BatchReader(root)
    try:
        modules = {}
        for path in _list_blobs(root, args.ref, args.subtree):
            if not path.endswith(".py"):
                continue
            entry = _module_entry(reader.read(f"{args.ref}:{path}"))
            if entry is not None:
                modules[path] = entry
        artifacts = _artifact_digests(reader, args.ref, _census_from(args.manifest))
    finally:
        reader.close()

    snapshot = {
        "schema_version": 1,
        "subtree": args.subtree,
        "modules": modules,
        "artifacts": artifacts,
    }
    print(json.dumps(snapshot, indent=2, sort_keys=True))

    missing = [name for name in args.require_module if name not in modules]
    if missing:
        print(f"census is missing required modules: {missing}", file=sys.stderr)
        return EXIT_CENSUS
    return EXIT_OK


if __name__ == "__main__":
    raise SystemExit(main())
