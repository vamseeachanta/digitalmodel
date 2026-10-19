# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory resource declarations without importing or querying a solver.

Run ``python -m digitalmodel.solvers.orcaflex.resource_audit SOURCE_DIRECTORY``.
All Model attribute calls are candidates, including unresolved API aliases.
Operation evidence is lexical; neither bindings nor call graphs are resolved.
The inventory never establishes a runtime CPU budget.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import io
import json
import os
from pathlib import Path
import tokenize

# Orcina, OrcFxAPI Python reference, Model: Construction and threadCount,
# online edition retrieved 2026-10-19. No version number is stated on the page.
MODEL_REFERENCE = (
    "https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference%2CModel.htm"
)
# Python 3.11 documentation, concurrent.futures, Executor Objects,
# ThreadPoolExecutor and ProcessPoolExecutor; online edition retrieved 2026.
EXECUTOR_REFERENCE = "https://docs.python.org/3.11/library/concurrent.futures.html"
_OPERATIONS = {
    "CalculateStatics": "simulation",
    "RunSimulation": "simulation",
    "TimeHistory": "extraction",
    "RangeGraph": "extraction",
    "StaticResult": "extraction",
    "GetMultipleTimeHistories": "extraction",
    "SaveData": "conversion",
    "SaveSimulation": "conversion",
}
_SCOPES = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)
_POOLS = {"ProcessPoolExecutor", "ThreadPoolExecutor", "Pool"}


def _local_calls(scope: ast.AST):
    """Yield calls in one lexical scope, excluding nested definitions."""
    for child in ast.iter_child_nodes(scope):
        if isinstance(child, _SCOPES):
            continue
        if isinstance(child, ast.Call):
            yield child
        yield from _local_calls(child)


def _argument(call: ast.Call, name: str, position: int) -> ast.AST | None:
    """Read an argument expression without evaluating it."""
    for keyword in call.keywords:
        if keyword.arg == name:
            return keyword.value
    return call.args[position] if len(call.args) > position else None


def _thread_declaration(call: ast.Call, argument: ast.AST | None) -> str:
    """Describe syntax only; a literal does not verify native readback."""
    if any(k.arg is None for k in call.keywords) or any(
        isinstance(arg, ast.Starred) for arg in call.args
    ):
        return "NOT_EVALUATED"
    # Model Construction, MODEL_REFERENCE: handle reuses an existing model;
    # its resource allocation cannot be inferred from this constructor.
    handle = _argument(call, "handle", 2)
    if handle is not None and not (
        isinstance(handle, ast.Constant) and handle.value is None
    ):
        return "NOT_EVALUATED"
    # Orcina Model Construction (MODEL_REFERENCE, online edition 2026):
    # threadCount omitted/None defaults to computer processing cores.
    if (
        argument is None
        or isinstance(argument, ast.Constant)
        and argument.value is None
    ):
        return "DEFAULT_CORES"
    if isinstance(argument, ast.Constant) and type(argument.value) is int:
        if argument.value > 0:
            return "EXPLICIT_LITERAL"
    return "NOT_EVALUATED"


class _Inventory(ast.NodeVisitor):
    """Collect candidates while preserving their lexical operation evidence."""

    def __init__(self, tree: ast.Module, path: str):
        self.path = path
        self.rows: list[dict] = []
        self.scopes: list[ast.AST] = [tree]
        self.operations: dict[ast.AST, list[str]] = {}
        self.model_aliases = {
            alias.asname or alias.name
            for node in ast.walk(tree)
            if isinstance(node, ast.ImportFrom) and node.module == "OrcFxAPI"
            for alias in node.names
            if alias.name == "Model"
        }
        self.pool_aliases = _POOLS | {
            alias.asname or alias.name
            for node in ast.walk(tree)
            if isinstance(node, ast.ImportFrom)
            and node.module
            in {"concurrent.futures", "multiprocessing", "multiprocessing.pool"}
            for alias in node.names
            if alias.name in _POOLS
        }

    def visit_FunctionDef(self, node):
        """Enter a definition without attributing its calls to the parent."""
        self.scopes.append(node)
        self.generic_visit(node)
        self.scopes.pop()

    visit_AsyncFunctionDef = visit_FunctionDef
    visit_ClassDef = visit_FunctionDef
    visit_Lambda = visit_FunctionDef

    def visit_Call(self, node):
        """Record Model candidates and worker-pool declarations independently."""
        name = getattr(node.func, "attr", getattr(node.func, "id", ""))
        model = (
            isinstance(node.func, ast.Attribute)
            and name == "Model"
            or isinstance(node.func, ast.Name)
            and name in self.model_aliases
        )
        pool = name in self.pool_aliases or any(
            k.arg == "max_workers" for k in node.keywords
        )
        if model or pool:
            self._record(node, model)
        self.generic_visit(node)

    def _record(self, node: ast.Call, model: bool):
        scope = self.scopes[-1]
        if scope not in self.operations:
            self.operations[scope] = sorted(
                {
                    _OPERATIONS[call.func.attr]
                    for call in _local_calls(scope)
                    if isinstance(call.func, ast.Attribute)
                    and call.func.attr in _OPERATIONS
                }
            )
        row = {
            "path": self.path,
            "line": node.lineno,
            "scope": ".".join(getattr(s, "name", "<lambda>") for s in self.scopes[1:])
            or "<module>",
            "kind": "model_candidate" if model else "pool_candidate",
            "call": ast.unparse(node.func),
            "scope_operations": self.operations[scope],
            "budget_status": "NOT_EVALUATED",
        }
        if model:
            # Model(filename=None, threadCount=None, handle=None), Orcina
            # Model Construction, MODEL_REFERENCE, online edition 2026.
            argument = _argument(node, "threadCount", 1)
            row["thread_argument"] = ast.unparse(argument) if argument else None
            row["thread_declaration"] = _thread_declaration(node, argument)
        else:
            # Python Executor Objects (EXECUTOR_REFERENCE, Python 3.11):
            # max_workers is the first executor argument. Pool candidates may
            # use processes instead; expanded arguments remain source text.
            argument = _argument(node, "max_workers", 0)
            if argument is None:
                argument = _argument(node, "processes", 0)
            row["worker_argument"] = ast.unparse(argument) if argument else None
        self.rows.append(row)


def audit_source(source: str, path: str) -> list[dict]:
    """Return source observations; never import or execute the supplied text."""
    tree = ast.parse(source)
    inventory = _Inventory(tree, path)
    inventory.visit(tree)
    return sorted(inventory.rows, key=lambda row: row["line"])


def _audit_file(path: Path, root: Path, report: dict):
    """Read and hash one source file, preserving failures in the inventory."""
    relative = path.relative_to(root).as_posix()
    try:
        data = path.read_bytes()
        encoding, _ = tokenize.detect_encoding(io.BytesIO(data).readline)
        rows = audit_source(data.decode(encoding), relative)
    except (SyntaxError, UnicodeError, LookupError, OSError) as error:
        report["errors"].append({"path": relative, "type": type(error).__name__})
        return
    report["files_scanned"].append(relative)
    report["source_sha256"][relative] = hashlib.sha256(data).hexdigest()
    report["findings"].extend(rows)


def audit_tree(root: Path) -> dict:
    """Audit regular Python files with coverage, digests and explicit errors.

    Paths are relative to the selected directory. Symbolic links are excluded.
    Parse/read errors remain errors, rather than implying complete coverage.
    """
    root = Path(root)
    if root.is_symlink() or not root.is_dir():
        raise ValueError("source root must be a regular directory")
    report = {
        "model_reference": MODEL_REFERENCE,
        "executor_reference": EXECUTOR_REFERENCE,
        "budget_status": "NOT_EVALUATED",
        "files_scanned": [],
        "source_sha256": {},
        "skipped_symlinks": [],
        "errors": [],
        "findings": [],
    }
    for directory, dirs, files in os.walk(
        root,
        followlinks=False,
        onerror=lambda e: report["errors"].append(
            {
                "path": Path(e.filename).relative_to(root).as_posix(),
                "type": type(e).__name__,
            }
        ),
    ):
        for name in sorted(dirs + files):
            path = Path(directory) / name
            if path.is_symlink():
                report["skipped_symlinks"].append(path.relative_to(root).as_posix())
        dirs[:] = sorted(d for d in dirs if not (Path(directory) / d).is_symlink())
        for name in sorted(files):
            path = Path(directory) / name
            if path.suffix != ".py" or path.is_symlink():
                continue
            _audit_file(path, root, report)
    report["files_scanned"].sort()
    report["skipped_symlinks"].sort()
    report["findings"].sort(key=lambda row: (row["path"], row["line"]))
    return report


def evaluate_budget(
    workers: int | None, threads_per_model: int | None, cpu_budget: int | None
) -> dict:
    """Compare explicit homogeneous demand with an explicitly allocated budget.

    Accounting identity: demand = sum(threads_per_model for each worker)
    = workers * threads_per_model. This counts configured native threads;
    it is not a measured CPU load or process-tree cap. No reserve fraction,
    host-core default or engineering acceptance coefficient is assumed.
    """
    for value in (workers, threads_per_model, cpu_budget):
        if value is not None and (type(value) is not int or value <= 0):
            raise ValueError("resource counts must be a positive integer or None")
    if any(value is None for value in (workers, threads_per_model, cpu_budget)):
        return {"status": "NOT_EVALUATED", "requested_threads": None}
    demand = workers * threads_per_model
    return {
        "status": "WITHIN_BUDGET" if demand <= cpu_budget else "EXCEEDS_BUDGET",
        "requested_threads": demand,
    }


def main(argv: list[str] | None = None) -> int:
    """Print a JSON source inventory; return nonzero for incomplete coverage."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source_directory", type=Path)
    args = parser.parse_args(argv)
    try:
        report = audit_tree(args.source_directory)
    except ValueError as error:
        parser.error(str(error))
    print(json.dumps(report, indent=2, sort_keys=True))
    return int(bool(report["errors"]))


if __name__ == "__main__":
    raise SystemExit(main())
