"""
ABOUTME: Per-solver dictionary requirements for the OpenFOAM case builder --
what fvSolution and fvSchemes must contain for a case to start under the
application named in its own controlDict.

Issue #1959. Before this module the builder emitted one solver-agnostic
fvSolution and one solver-agnostic fvSchemes for every application, so an
interFoam case was emitted with a bare p solver, no MULES controls, and
single-phase divergence schemes under `divSchemes { default none; }`. It died
at start-up with "Entry 'cAlpha' not found".

Provenance of the two-phase values (design decision D4): every literal below is
taken from the OpenFOAM v2312 tutorial

    $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak/damBreak

a named, versioned, externally-owned reference that can be cited and
re-derived. Values are deliberately NOT taken from any hand-authored case
directory on the CFD node: a sweep there found 139 fvSolution files in 18
distinct variants, so adopting one would be adopting a fitted constant of
unknown provenance.

This table is a static declaration. The emitter renders from it, which means a
test asserting "the emitted dict contains the keys this table lists" is
circular and cannot fail. The non-circular oracle is a real solver start --
see tests/solvers/openfoam/test_case_runnable.py.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Tuple

from .templates import (
    FV_SOLUTION_SOLVERS,
    FV_SOLUTION_SOLVERS_VOF,
    PIMPLE_BLOCK,
    SIMPLE_BLOCK,
)


#: The tutorial the two-phase literals were derived from, and its version.
VOF_REFERENCE = (
    "OpenFOAM v2312 $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak/damBreak"
)

#: Turbulence transport divergence schemes, shared by every solver. Retained
#: unchanged from the pre-#1959 builder.
_TURBULENCE_DIVS: Tuple[Tuple[str, str], ...] = (
    ("div(phi,k)", "Gauss upwind"),
    ("div(phi,omega)", "Gauss upwind"),
    ("div(phi,epsilon)", "Gauss upwind"),
)

#: Single-phase momentum and viscous stress terms. Retained unchanged.
_SINGLE_PHASE_DIVS: Tuple[Tuple[str, str], ...] = (
    ("div(phi,U)", "Gauss linearUpwind grad(U)"),
    ("div((nuEff*dev(T(grad(U)))))", "Gauss linear"),
)

#: Two-phase terms. interFoam's momentum equation convects with the mass flux
#: rhoPhi, and its viscous stress term carries rho*nuEff with dev2. The alpha
#: equation looks up the schemes under the generic names div(phi,alpha) and
#: div(phirb,alpha) -- NOT under the field-suffixed div(phi,alpha.water), which
#: is never consulted and which the builder previously emitted.
_VOF_DIVS: Tuple[Tuple[str, str], ...] = (
    ("div(rhoPhi,U)", "Gauss linearUpwind grad(U)"),
    ("div(phi,alpha)", "Gauss vanLeer"),
    ("div(phirb,alpha)", "Gauss linear"),
    ("div(((rho*nuEff)*dev2(T(grad(U)))))", "Gauss linear"),
)


@dataclass(frozen=True)
class SolverDictContract:
    """What one OpenFOAM application requires of its case dictionaries.

    Attributes:
        algorithm: Name of the pressure-velocity coupling block.
        algorithm_block: Rendered text of that block.
        solvers_block: Rendered text of the fvSolution ``solvers`` dictionary.
        fv_solution_keys: The solver keys the block declares, for coverage.
        div_schemes: Ordered ``divSchemes`` entries, emitted under
            ``default none`` so that any key the solver looks up and this
            table omits is a loud start-up failure rather than a silent
            fallback.
        needs_alpha_courant: Whether controlDict must bound the interface
            Courant number -- true exactly for VOF solvers.
        reference: Provenance of the numeric literals.
    """

    algorithm: str
    algorithm_block: str
    solvers_block: str
    fv_solution_keys: Tuple[str, ...]
    div_schemes: Tuple[Tuple[str, str], ...]
    needs_alpha_courant: bool
    reference: str


SOLVER_DICTS: Dict[str, SolverDictContract] = {
    "interFoam": SolverDictContract(
        algorithm="PIMPLE",
        algorithm_block=PIMPLE_BLOCK,
        solvers_block=FV_SOLUTION_SOLVERS_VOF,
        fv_solution_keys=(
            '"alpha.water.*"',
            '"pcorr.*"',
            "p_rgh",
            "p_rghFinal",
            "U",
            '"(k|omega|epsilon)"',
            '"(k|omega|epsilon)Final"',
        ),
        div_schemes=_VOF_DIVS + _TURBULENCE_DIVS,
        needs_alpha_courant=True,
        reference=VOF_REFERENCE,
    ),
    "pimpleFoam": SolverDictContract(
        algorithm="PIMPLE",
        algorithm_block=PIMPLE_BLOCK,
        solvers_block=FV_SOLUTION_SOLVERS,
        fv_solution_keys=(
            "p",
            "pFinal",
            "U",
            "UFinal",
            '"(k|omega|epsilon)"',
            '"(k|omega|epsilon)Final"',
        ),
        div_schemes=_SINGLE_PHASE_DIVS + _TURBULENCE_DIVS,
        needs_alpha_courant=False,
        reference="unchanged from the pre-#1959 single-phase builder",
    ),
    "simpleFoam": SolverDictContract(
        algorithm="SIMPLE",
        algorithm_block=SIMPLE_BLOCK,
        solvers_block=FV_SOLUTION_SOLVERS,
        fv_solution_keys=(
            "p",
            "pFinal",
            "U",
            "UFinal",
            '"(k|omega|epsilon)"',
            '"(k|omega|epsilon)Final"',
        ),
        div_schemes=_SINGLE_PHASE_DIVS + _TURBULENCE_DIVS,
        needs_alpha_courant=False,
        reference="unchanged from the pre-#1959 single-phase builder",
    ),
}


def contract_for(solver_name: str) -> SolverDictContract:
    """Return the dictionary contract for ``solver_name``.

    Fails closed: an application with no declared contract raises rather than
    silently receiving a default dictionary that its solver cannot read.
    """
    try:
        return SOLVER_DICTS[solver_name]
    except KeyError:
        raise KeyError(
            f"no dictionary contract declared for solver {solver_name!r}; "
            f"declared solvers are {sorted(SOLVER_DICTS)}"
        ) from None


def render_div_schemes(contract: SolverDictContract) -> str:
    """Render the divSchemes block for ``contract``."""
    width = max(len(key) for key, _ in contract.div_schemes) + 4
    lines = ["divSchemes", "{", f"    {'default':<{width}}none;"]
    for key, scheme in contract.div_schemes:
        lines.append(f"    {key:<{width}}{scheme};")
    lines.append("}")
    return "\n".join(lines)
