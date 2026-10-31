"""Slice 1: the claim register must cover every public calculation entry point."""

import ast
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[3]
PACKAGE = ROOT / "src/digitalmodel/subsea/on_bottom_stability"
REGISTER = PACKAGE / "docs/validation-claims.md"


def public_functions():
    """Include locally defined public methods and property accessors, too."""
    names = set()
    for path in PACKAGE.glob("*.py"):
        tree = ast.parse(path.read_text())
        for node in tree.body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                names.add(f"{path.stem}.{node.name}")
            elif isinstance(node, ast.ClassDef) and not node.name.startswith("_"):
                for method in node.body:
                    if isinstance(
                        method, ast.FunctionDef
                    ) and not method.name.startswith("_"):
                        names.add(f"{path.stem}.{node.name}.{method.name}")
    return names


def read_rows(text):
    rows = {}
    for line in text.splitlines():
        if line.startswith("| `"):
            cells = [cell.strip() for cell in line.strip("|").split("|")]
            name, equation, reference, clause, status = cells
            assert name.strip("`") not in rows, f"duplicate claim: {name}"
            assert all((equation, reference, clause, status)), name
            assert status in {"ARITHMETIC_TESTED", "CONTRACT_TESTED", "NOT_EVALUATED"}
            rows[name.strip("`")] = cells
    return rows


def test_every_public_function_has_equation_edition_clause_and_status():
    rows = read_rows(REGISTER.read_text())
    assert set(rows) == public_functions()


def test_missing_clause_is_a_failing_negative_control():
    with pytest.raises(AssertionError):
        read_rows(
            "| `example.function` | x=y | F109 2021 (unverified) | | NOT_EVALUATED |"
        )


def test_duplicate_claim_is_rejected():
    row = "| `example.function` | x=y | Repository 2026 | Arithmetic definition | ARITHMETIC_TESTED |"
    with pytest.raises(AssertionError, match="duplicate"):
        read_rows(row + "\n" + row)
