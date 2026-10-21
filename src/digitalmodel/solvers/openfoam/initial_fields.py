#!/usr/bin/env python3
"""
ABOUTME: Generators for OpenFOAM initial field files (0/ directory) including
velocity, pressure, turbulence, and alpha.water VOF phase fraction fields.
"""

from __future__ import annotations

from pathlib import Path

from .block_mesh import boundary_patch_types
from .models import TurbulenceModel, TurbulenceType


# ---------------------------------------------------------------------------
# FoamFile header helper (re-declared here to keep module independent)
# ---------------------------------------------------------------------------

_FOAM_FILE_HEADER = """\
/*--------------------------------*- C++ -*----------------------------------*\\
  =========                 |
  \\\\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\\\    /   O peration     | Version: v2306
    \\\\  /    A nd           | Website: www.openfoam.com
     \\/     M anipulation   |
\\*---------------------------------------------------------------------------*/
FoamFile
{{
    version     2.0;
    format      ascii;
    class       {foam_class};
    object      {foam_object};
}}
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //
"""

_FOOTER = ("// ****"
           "******* * * * * * * * * * * * * * * * * * * * * //\n")


def _foam_header(foam_class: str, foam_object: str) -> str:
    return _FOAM_FILE_HEADER.format(
        foam_class=foam_class, foam_object=foam_object
    )


# ---------------------------------------------------------------------------
# boundaryField rendering (default patches, overridden by attached BCs)
# ---------------------------------------------------------------------------


def _render_patch_block(patch: str, entries) -> str:
    lines = [f"    {patch}", "    {"]
    for key, val in entries.items():
        lines.append(f"        {key}    {val};")
    lines.append("    }")
    return "\n".join(lines)


def _boundary_field(field: str, defaults, boundary_conditions=None) -> str:
    """Render a ``boundaryField`` block for ``field``.

    ``defaults`` maps patch name -> ordered dict of foam entries. Any attached
    :class:`BoundaryCondition` whose ``field`` matches overrides (or adds) that
    patch's block, so the case builder honours per-case boundary conditions
    (e.g. a conduit ``flowRateInletVelocity`` inlet) instead of the hardcoded
    defaults. When no BC targets ``field`` the defaults are emitted verbatim.
    """
    overrides = {}
    for bc in (boundary_conditions or []):
        if bc.field == field:
            block = bc.to_foam_dict()[bc.patch_name]
            overrides[bc.patch_name] = {k: str(v) for k, v in block.items()}
    blocks = [
        _render_patch_block(patch, overrides.get(patch, entries))
        for patch, entries in defaults.items()
    ]
    for patch, entries in overrides.items():
        if patch not in defaults:
            blocks.append(_render_patch_block(patch, entries))
    return "boundaryField\n{\n" + "\n".join(blocks) + "\n}\n"


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


_U_DEFAULTS = {
    "inlet": {"type": "fixedValue", "value": "uniform (1 0 0)"},
    "outlet": {"type": "zeroGradient"},
    "bottom": {"type": "noSlip"},
    "top": {"type": "slip"},
    "sides": {"type": "symmetry"},
}


def write_velocity_field(zero_dir: Path, boundary_conditions=None) -> None:
    """Write 0/U velocity initial and boundary conditions.

    Attached ``boundary_conditions`` for field ``U`` override the defaults per
    patch (e.g. a conduit ``flowRateInletVelocity`` inlet).
    """
    content = _foam_header("volVectorField", "U")
    content += "\ndimensions  [0 1 -1 0 0 0 0];\n\ninternalField   uniform (0 0 0);\n\n"
    content += _boundary_field("U", _U_DEFAULTS, boundary_conditions)
    content += "\n" + _FOOTER
    (zero_dir / "U").write_text(content)


def write_pressure_field(
    zero_dir: Path, is_multiphase: bool, boundary_conditions=None
) -> None:
    """Write 0/p or 0/p_rgh pressure initial and boundary conditions.

    Incompressible single-phase simpleFoam and pimpleFoam cases use kinematic
    pressure p (pressure divided by density), with units of m^2/s^2. Multiphase
    interFoam VOF cases use p_rgh = p - rho*g*h, with pascal dimensions.

    Dimensions are confirmed from installed OpenFOAM v2312 tutorial files
    (Development/openfoam repository, release tag OpenFOAM-v2312):
    - tutorials/incompressible/simpleFoam/pitzDaily/0/p
    - tutorials/multiphase/interFoam/laminar/damBreak/damBreak/0.orig/p_rgh

    Attached ``boundary_conditions`` for the pressure field override the
    defaults per patch.
    """
    if is_multiphase:
        field_name, dims = "p_rgh", "[1 -1 -2 0 0 0 0]"
    else:
        field_name, dims = "p", "[0 2 -2 0 0 0 0]"
    defaults = {
        "inlet": {"type": "zeroGradient"},
        "outlet": {"type": "fixedValue", "value": "uniform 0"},
        "bottom": {"type": "zeroGradient"},
        "top": {"type": "totalPressure", "p0": "uniform 0"},
        "sides": {"type": "symmetry"},
    }
    for patch, kind in boundary_patch_types(boundary_conditions).items():
        # OpenFOAM v2312 (2023), fixedFluxPressure, Description: match the
        # prescribed velocity flux and compensate body forces (gravity).
        # https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/inlet/fixedFluxPressure/
        if kind == "wall":
            defaults[patch] = {
                "type": "fixedFluxPressure" if is_multiphase else "zeroGradient"
            }
    content = _foam_header("volScalarField", field_name)
    content += f"\ndimensions  {dims};\n\ninternalField   uniform 0;\n\n"
    content += _boundary_field(field_name, defaults, boundary_conditions)
    content += "\n" + _FOOTER
    # Write to the correct filename for the solver
    (zero_dir / field_name).write_text(content)


def write_turbulence_fields(
    zero_dir: Path, turbulence_model: TurbulenceModel, boundary_conditions=None
) -> None:
    """Write 0/k, 0/omega (or epsilon), and 0/nut turbulence fields.

    All RANS models require the turbulent viscosity field nut in addition
    to the transported turbulence quantities. Mesh wall patches, including
    explicit no-slip closures, receive the same wall defaults as bottom.
    Explicit field boundary conditions override those defaults.
    """
    tm = turbulence_model
    if tm.turbulence_type == TurbulenceType.LAMINAR:
        return

    _write_k_field(zero_dir, boundary_conditions)
    _write_nut_field(zero_dir, boundary_conditions)

    if tm.turbulence_type == TurbulenceType.K_OMEGA_SST:
        _write_omega_field(zero_dir, boundary_conditions)
    elif tm.turbulence_type == TurbulenceType.K_EPSILON:
        _write_epsilon_field(zero_dir, boundary_conditions)


_ALPHA_DEFAULTS = {
    "inlet": {"type": "fixedValue", "value": "uniform 0"},
    "outlet": {"type": "zeroGradient"},
    "bottom": {"type": "zeroGradient"},
    "top": {"type": "inletOutlet", "inletValue": "uniform 0", "value": "uniform 0"},
    "sides": {"type": "symmetry"},
}


def write_alpha_water(zero_dir: Path, boundary_conditions=None) -> None:
    """Write 0/alpha.water VOF phase fraction field.

    Attached ``boundary_conditions`` for ``alpha.water`` override the defaults
    per patch (e.g. an ``inletOutlet`` inlet that admits water on inflow).
    """
    content = _foam_header("volScalarField", "alpha.water")
    content += "\ndimensions  [0 0 0 0 0 0 0];\n\ninternalField   uniform 0;\n\n"
    defaults = {patch: dict(entries) for patch, entries in _ALPHA_DEFAULTS.items()}
    for patch, kind in boundary_patch_types(boundary_conditions).items():
        if kind == "wall":
            defaults[patch] = {"type": "zeroGradient"}
    content += _boundary_field("alpha.water", defaults, boundary_conditions)
    content += "\n" + _FOOTER
    (zero_dir / "alpha.water").write_text(content)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _write_turbulence_field(
    zero_dir: Path, field: str, dimensions: str, initial: str,
    wall_function: str, boundary_conditions=None,
) -> None:
    """Render wall defaults and explicit overrides using the mesh topology.

    Wall types: OpenFOAM v2312 (2023), Usage sections for kqRWallFunction,
    nutkWallFunction, epsilonWallFunction and omegaWallFunction API reference.
    Sources: docs/domains/openfoam/closed_wall_boundaries.md.
    No wall-law coefficients or acceptance limits are introduced here.
    """
    if field == "nut":
        defaults = {
            patch: {"type": "calculated", "value": initial}
            for patch in ("inlet", "outlet", "bottom", "top")
        }
    else:
        defaults = {
            "inlet": {"type": "fixedValue", "value": initial},
            "outlet": {"type": "zeroGradient"},
            "bottom": {"type": "zeroGradient"},
            "top": {"type": "zeroGradient"},
        }
    defaults["sides"] = {"type": "symmetry"}
    for patch, kind in boundary_patch_types(boundary_conditions).items():
        if kind == "wall":
            defaults[patch] = {"type": wall_function, "value": initial}
    content = _foam_header("volScalarField", field)
    content += f"\ndimensions  {dimensions};\n\ninternalField   {initial};\n\n"
    content += _boundary_field(field, defaults, boundary_conditions)
    content += "\n" + _FOOTER
    (zero_dir / field).write_text(content)


# These are the pre-existing field dimensions and solver initialization guesses,
# retained unchanged from initial_fields.py. The guesses are assumed, based on
# previous experience; they are not sourced inflow data or wall-law constants.
# Units follow the OpenFOAM User Guide, online edition, section 2.2.6,
# Dimensional units:
# https://www.openfoam.com/documentation/user-guide/2-openfoam-cases/2-2-basic-inputoutput-file-format
# Case-specific physical boundary values may be supplied as explicit overrides.
def _write_nut_field(zero_dir: Path, boundary_conditions=None) -> None:
    _write_turbulence_field(
        zero_dir, "nut", "[0 2 -1 0 0 0 0]", "uniform 0",
        "nutkWallFunction", boundary_conditions,
    )


def _write_k_field(zero_dir: Path, boundary_conditions=None) -> None:
    _write_turbulence_field(
        zero_dir, "k", "[0 2 -2 0 0 0 0]", "uniform 0.001",
        "kqRWallFunction", boundary_conditions,
    )


def _write_omega_field(zero_dir: Path, boundary_conditions=None) -> None:
    _write_turbulence_field(
        zero_dir, "omega", "[0 0 -1 0 0 0 0]", "uniform 1",
        "omegaWallFunction", boundary_conditions,
    )


def _write_epsilon_field(zero_dir: Path, boundary_conditions=None) -> None:
    _write_turbulence_field(
        zero_dir, "epsilon", "[0 2 -3 0 0 0 0]", "uniform 0.001",
        "epsilonWallFunction", boundary_conditions,
    )
