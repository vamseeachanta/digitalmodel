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

# Measurement basis, stated rather than left implicit.
#
# The module limit counts raw lines: a 400-line file is hard to navigate however
# it is filled.
#
# The function limit counts *logic* lines - the span of the body with a leading
# docstring excluded. The rule exists to bound branching and state a reader must
# hold at once, and counting the docstring measures documentation instead, which
# would penalise the well-documented numerical routines in this package and push
# toward either thinner docs or artificial helper functions. Every function in
# the governed set is comfortably inside the limit on this basis; none was split
# to satisfy it.

# Modules split or rewritten by this issue. Listed explicitly rather than
# globbed so that adding a module to the package is a deliberate decision to
# bring it under this limit.
GOVERNED_MODULES = (
    "src/digitalmodel/solvers/openfoam/artifact_index.py",
    # Arbitrary-hull case construction (#2023). Brought under the limit
    # deliberately: this surface is new, so there is no legacy to grandfather.
    "src/digitalmodel/solvers/openfoam/hull_manifest.py",
    "src/digitalmodel/solvers/openfoam/hull_domain.py",
    "src/digitalmodel/solvers/openfoam/hull_free_surface.py",
    "src/digitalmodel/solvers/openfoam/hull_turbulence.py",
    "src/digitalmodel/solvers/openfoam/hull_case_physics.py",
    "src/digitalmodel/solvers/openfoam/hull_case_dicts.py",
    # N-surface support: hull plus interpenetrating appendages (#2023). New
    # surface, so nothing to grandfather.
    "src/digitalmodel/solvers/openfoam/hull_case_regions.py",
    # Deriving each region's refinement level from its own bounding box. Kept
    # out of hull_case_regions.py, which is already near the limit, and it is
    # a separate concern anyway: that module says which surfaces the mesher
    # meets, this one says how finely it must meet them.
    "src/digitalmodel/solvers/openfoam/region_refinement.py",
    "src/digitalmodel/solvers/openfoam/hull_field_patches.py",
    "src/digitalmodel/solvers/openfoam/hull_case.py",
    # The double-body (no free surface) variant of the same lane (#2023).
    # Same reasoning: new surface, nothing to grandfather.
    "src/digitalmodel/solvers/openfoam/hull_double_body_domain.py",
    "src/digitalmodel/solvers/openfoam/hull_double_body_dicts.py",
    "src/digitalmodel/solvers/openfoam/hull_double_body.py",
    "src/digitalmodel/solvers/openfoam/pressure_taps.py",
    "src/digitalmodel/solvers/openfoam/pressure_tap_models.py",
    "src/digitalmodel/solvers/openfoam/pressure_tap_analysis.py",
    "src/digitalmodel/solvers/openfoam/sloshing_coupling.py",
    "src/digitalmodel/solvers/openfoam/sloshing_coupling_models.py",
    "src/digitalmodel/solvers/openfoam/sloshing_coupling_analysis.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_dicts.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_case.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep_reduction.py",
    "src/digitalmodel/solvers/openfoam/validation/sloshing_sweep_cli.py",
)

# Named exemption, declared here rather than left implicit. The explorer
# generator's bulk is one embedded HTML template string, not logic; splitting it
# is carried by dm#1903 together with the rest of the capability-generator
# structure. This issue changed only prose inside that template.
EXEMPT_FROM_MODULE_LIMIT = (
    "scripts/capabilities/build_sloshing_explorer.py",
)

# NOT GOVERNED, stated so the omission is a decision and not an oversight.
#
# validation/double_body_form_factor.py (#2023) is a scoring manifest in the
# shape of validation/referent_free_resistance.py, which it imports its band
# and its vocabulary from and which is itself ungoverned at ~1000 lines, as is
# validation/ship_resistance.py. A manifest of that kind is one dictionary a
# reader has to see whole; splitting it to reach 400 would put half the
# verdict in another file and leave the two free to disagree about what a
# verdict means. If the resistance-validation surface is brought under the
# limit, it should be brought under together rather than one new module at a
# time.
UNGOVERNED_BY_DECLARATION = (
    "src/digitalmodel/solvers/openfoam/validation/double_body_form_factor.py",
    "src/digitalmodel/solvers/openfoam/validation/referent_free_resistance.py",
    "src/digitalmodel/solvers/openfoam/validation/ship_resistance.py",
)


def _logic_lines(node) -> int:
    """Span of a function body, excluding a leading docstring."""
    body = list(node.body)
    if (
        body
        and isinstance(body[0], ast.Expr)
        and isinstance(body[0].value, ast.Constant)
        and isinstance(body[0].value.value, str)
    ):
        body = body[1:]
    if not body:
        return 0
    return body[-1].end_lineno - body[0].lineno + 1


def _function_lengths(path: Path):
    tree = ast.parse(path.read_text(encoding="utf-8"))
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            yield node.name, _logic_lines(node)


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


@pytest.mark.parametrize("relative", UNGOVERNED_BY_DECLARATION)
def test_ungoverned_module_still_exists_and_is_not_also_governed(relative):
    """A declaration that names a file which has moved says nothing, and a
    file in both lists would make the declaration a silent contradiction."""
    assert (_REPO / relative).is_file(), relative
    assert relative not in GOVERNED_MODULES, relative
