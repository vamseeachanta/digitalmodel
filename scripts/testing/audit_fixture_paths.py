# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspect test fixture locations without importing tests or solver interfaces."""

from __future__ import annotations

import ast
import re
import subprocess
from pathlib import Path
from typing import Any

# Path syntax, rather than an engineering coefficient or acceptance tolerance.
ABSOLUTE_PATH = re.compile(r"^(?:[A-Za-z]:[\\/]|/(?:home|mnt|Users)/)")

# Configuration round-trip input only: no file is read or solver executed.
ALLOWLIST = {
    "tests/hydrodynamics/diffraction/test_orcawave_runner.py": {
        "C:" + "/Orcina/OrcaWave/orcawave.exe",
    },
    "tests/hydrodynamics/diffraction/test_aqwa_runner.py": {
        "C:" + r"\Program Files\ANSYS Inc\v241\aqwa\bin\winx64\aqwa.exe",
    },
}


def absolute_literals(source: str) -> list[tuple[int, str]]:
    """Return source lines and values of location-dependent string literals."""
    return sorted(
        (node.lineno, node.value)
        for node in ast.walk(ast.parse(source))
        if isinstance(node, ast.Constant)
        and isinstance(node.value, str)
        and ABSOLUTE_PATH.match(node.value)
    )


def violations(source: str, relative_path: str) -> list[int]:
    """Return offending line numbers, allowing only documented exact values."""
    allowed = ALLOWLIST.get(relative_path, set())
    return [line for line, value in absolute_literals(source) if value not in allowed]


def tracked_files(root: Path, revision: str | None = None) -> list[str]:
    """Read the index without changing metadata or importing test modules."""
    if revision:
        command = ["git", "ls-tree", "-r", "--name-only", revision]
        return subprocess.check_output(command, cwd=root, text=True).splitlines()
    indexed = subprocess.check_output(["git", "ls-files"], cwd=root, text=True).splitlines()
    added = subprocess.check_output(
        ["git", "ls-files", "--others", "--exclude-standard"], cwd=root, text=True,
    ).splitlines()
    return sorted(set(indexed + added))


def suffix_matches(value: str, tracked: list[str]) -> list[str]:
    """Match checkout-independent full paths or at least three final components.

    Three components are a conservative matching heuristic, not proof that
    files share content. Ambiguous matches require manual inspection.
    """
    normalized = value.replace("\\", "/")
    return [
        path for path in tracked
        if normalized.endswith("/" + path)
        or (len(Path(path).parts) >= 3
            and normalized.endswith("/" + "/".join(Path(path).parts[-3:])))
    ]


def evaluate(node: ast.AST, names: dict) -> Any:
    """Resolve simple pathlib expressions statically; never execute test code."""
    if isinstance(node, ast.Constant):
        return node.value
    if isinstance(node, ast.Name):
        return names.get(node.id)
    if isinstance(node, ast.Call):
        if isinstance(node.func, ast.Name) and node.func.id == "Path" and node.args:
            value = evaluate(node.args[0], names)
            return Path(value) if isinstance(value, (str, Path)) else None
        if isinstance(node.func, ast.Attribute) and node.func.attr == "resolve":
            value = evaluate(node.func.value, names)
            return value.resolve() if isinstance(value, Path) else None
    if isinstance(node, ast.Attribute):
        value = evaluate(node.value, names)
        if isinstance(value, Path) and node.attr in {"parent", "parents"}:
            return getattr(value, node.attr)
    if isinstance(node, ast.Subscript):
        value, index = evaluate(node.value, names), evaluate(node.slice, names)
        if isinstance(value, type(Path().parents)) and isinstance(index, int):
            return value[index]
    if isinstance(node, ast.BinOp):
        left, right = evaluate(node.left, names), evaluate(node.right, names)
        if isinstance(node.op, ast.Div) and isinstance(left, Path) and isinstance(right, str):
            return left / right
        if isinstance(node.op, ast.Add) and isinstance(left, str) and isinstance(right, str):
            return left + right
    return None


def skip_guards(
    source: str, file: Path, root: Path, include_reasons: bool = False,
) -> list[dict]:
    """Inventory existence checks associated with skip calls, without imports."""
    tree = ast.parse(source)
    names = {"__file__": str(file)}
    for node in tree.body:
        if isinstance(node, ast.Assign):
            value = evaluate(node.value, names)
            for target in node.targets:
                if isinstance(target, ast.Name):
                    names[target.id] = value
    conditions = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
            if node.func.attr == "skipif" and node.args:
                reason = next((evaluate(keyword.value, names) for keyword in node.keywords
                               if keyword.arg == "reason"), None)
                conditions.append((node.args[0], reason))
        if isinstance(node, ast.If) and any(
            isinstance(child, ast.Call) and isinstance(child.func, ast.Attribute)
            and child.func.attr == "skip"
            for statement in node.body for child in ast.walk(statement)
        ):
            calls = [child for statement in node.body for child in ast.walk(statement)
                     if isinstance(child, ast.Call) and isinstance(child.func, ast.Attribute)
                     and child.func.attr == "skip"]
            reason = evaluate(calls[0].args[0], names) if calls[0].args else None
            conditions.append((node.test, reason))
    rows = []
    for condition, reason in conditions:
        for node in ast.walk(condition):
            if not (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                    and node.func.attr in {"exists", "is_file", "is_dir"}):
                continue
            value = evaluate(node.func.value, names)
            if not isinstance(value, Path):
                continue
            candidate = value if value.is_absolute() else root / value
            try:
                relative = candidate.relative_to(root).as_posix()
            except ValueError:
                relative = "[path removed]"
            rows.append({"file": file.relative_to(root).as_posix(),
                         "line": node.lineno, "path": relative})
            if include_reasons:
                rows[-1]["reason"] = reason if isinstance(reason, str) else "Dynamic reason; see source"
    return rows


def inventory(root: Path, revision: str | None = None) -> dict:
    """Audit all tracked Python tests; report no private absolute locations."""
    tracked = tracked_files(root, revision)
    changed = set(subprocess.check_output(
        ["git", "diff", "--name-only", revision], cwd=root, text=True,
    ).splitlines()) if revision else set()
    rows, guards, syntax_errors = [], [], []
    for relative in tracked:
        if not relative.startswith("tests/") or not relative.endswith(".py"):
            continue
        path = root / relative
        source = (
            subprocess.check_output(["git", "show", f"{revision}:{relative}"],
                                    cwd=root).decode("utf-8-sig")
            if relative in changed else path.read_text(encoding="utf-8-sig")
        )
        try:
            literals = absolute_literals(source)
            guards.extend(skip_guards(source, path, root, include_reasons=True))
        except SyntaxError:
            syntax_errors.append(relative)
            continue
        for line, value in literals:
            matches = suffix_matches(value, tracked)
            rows.append({"file": relative, "line": line,
                         "path": matches[0] if len(matches) == 1 else "[path removed]",
                         "tracked": "yes" if matches else "no",
                         "matches": len(matches),
                         "kind": "drive" if value[1:2] == ":" else "posix"})
    for row in guards:
        row["tracked"] = "yes" if any(
            path == row["path"] or path.startswith(row["path"] + "/")
            for path in tracked
        ) else "no"
    return {"absolute_literals": rows, "skip_guards": guards,
            "syntax_errors": syntax_errors}
