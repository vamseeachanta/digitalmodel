"""A plain ``pytest`` never selects a ``solver`` test; ``-m solver`` still does.

The subprocess checks only collect (``--collect-only``); no test body runs.
"""

from __future__ import annotations

import importlib.util
import os
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SAMPLE = "tests/hydrodynamics/diffraction/test_solver_fixtures.py"
MARKED = "test_solver_marker_deselection"


def _selects_solver():
    """``selects_solver`` from tests/conftest.py, loaded under a private name."""
    spec = importlib.util.spec_from_file_location("_tests_conftest_probe", ROOT / "tests" / "conftest.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.selects_solver


@pytest.mark.parametrize(
    "markexpr, expected",
    [
        ("", False),
        (None, False),
        ("slow", False),
        ("not slow and unit", False),
        ("solvers", False),
        ("solver_x", False),
        ("solver", True),
        ("not solver", True),
        ("solver and not slow", True),
        ("(slow or solver)", True),
        ("not(solver)", True),
        ("solver(reason='x')", True),
        ("slow (n=1) or solver", True),
        # Only a complete marker identifier names solver (pytest's -m lexer
        # reads these as single identifiers).
        ("slow-solver", False),
        ("not slow-solver", False),
        ("solver-slow", False),
        ("solver.extra", False),
        ("not solver.extra", False),
        ("x/solver", False),
        ("x:solver", False),
        ("solver[1]", False),
        # Keyword arguments and strings are not marker names.
        ("slow(solver=1)", False),
        ("slow(name='solver')", False),
        ('slow(name="solver")', False),
        # An expression that does not lex leaves the guard on.
        ("'solver", False),
    ],
)
def test_only_an_expression_naming_solver_opts_in(markexpr, expected):
    assert _selects_solver()(markexpr) is expected


def _collect(*args: str) -> list[str]:
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    out = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-qq", "-p", "no:cacheprovider", SAMPLE, *args],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        timeout=300,
    )
    assert out.returncode in (0, 5), out.stdout + out.stderr
    return [line for line in out.stdout.splitlines() if "::" in line]


def test_plain_run_deselects_solver_tests():
    ids = _collect()
    assert ids, "the sample module should still collect its licence-free tests"
    assert not [i for i in ids if i.endswith(MARKED)]


@pytest.mark.parametrize("markexpr", ["not slow", "not slow-solver", "not solver.extra"])
def test_other_mark_expressions_still_deselect_solver_tests(markexpr):
    assert not [i for i in _collect("-m", markexpr) if i.endswith(MARKED)]


def test_dash_m_solver_selects_only_solver_tests():
    ids = _collect("-m", "solver")
    assert [i.rsplit("::", 1)[-1] for i in ids] == [MARKED]
