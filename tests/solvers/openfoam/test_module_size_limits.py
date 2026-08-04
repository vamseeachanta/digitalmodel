#!/usr/bin/env python3
"""
ABOUTME: Size limits for the modules this issue touches (dm#1574): 400 lines per
module and 50 lines per function. The limits are the repository's universal
rule; this test pins them for the sloshing/OpenFOAM surface that was split as
part of the source-neutrality work so the split cannot quietly regress.
"""

import ast
from pathlib import Path

import pytest

_REPO = Path(__file__).resolve().parents[3]

MAX_MODULE_LINES = 400
MAX_FUNCTION_LINES = 50

# Modules split or rewritten by this issue. Listed explicitly rather than
# globbed so that adding a module to the package is a deliberate decision to
# bring it under this limit.
GOVERNED_MODULES = (
    "src/digitalmodel/solvers/openfoam/pressure_taps.py",
    "src/digitalmodel/solvers/openfoam/pressure_tap_models.py",
    "src/digitalmodel/solvers/openfoam/pressure_tap_analysis.py",
    "src/digitalmodel/solvers/openfoam/sloshing_coupling.py",
    "src/digitalmodel/solvers/openfoam/sloshing_coupling_models.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_case.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep_cli.py",
)

# Named exemption, declared here rather than left implicit. The explorer
# generator's bulk is one embedded HTML template string, not logic; splitting it
# is carried by dm#1903 together with the rest of the capability-generator
# structure. This issue changed only prose inside that template.
EXEMPT_FROM_MODULE_LIMIT = (
    "scripts/capabilities/build_sloshing_explorer.py",
)


def _function_lengths(path: Path):
    tree = ast.parse(path.read_text(encoding="utf-8"))
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            yield node.name, node.end_lineno - node.lineno + 1


@pytest.mark.parametrize("relative", GOVERNED_MODULES)
def test_module_is_within_the_line_limit(relative):
    path = _REPO / relative
    assert path.is_file(), f"{relative} is missing"
    lines = len(path.read_text(encoding="utf-8").splitlines())
    assert lines <= MAX_MODULE_LINES, f"{relative} is {lines} lines"


@pytest.mark.parametrize("relative", GOVERNED_MODULES)
def test_functions_are_within_the_line_limit(relative):
    path = _REPO / relative
    oversized = [
        f"{name} ({length} lines)"
        for name, length in _function_lengths(path)
        if length > MAX_FUNCTION_LINES
    ]
    assert oversized == []


@pytest.mark.parametrize("relative", EXEMPT_FROM_MODULE_LIMIT)
def test_exempt_module_still_exists(relative):
    """The exemption is only meaningful while the file it names exists."""
    assert (_REPO / relative).is_file()
