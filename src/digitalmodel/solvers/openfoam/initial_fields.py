#!/usr/bin/env python3
"""
ABOUTME: Generators for OpenFOAM initial field files (0/ directory) including
velocity, pressure, turbulence, and alpha.water VOF phase fraction fields.
"""

from __future__ import annotations

from pathlib import Path

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
# Public API
# ---------------------------------------------------------------------------


def write_velocity_field(zero_dir: Path) -> None:
    """Write 0/U velocity initial and boundary conditions."""
    content = _foam_header("volVectorField", "U")
    content += """
dimensions  [0 1 -1 0 0 0 0];

internalField   uniform (0 0 0);

boundaryField
{
    inlet
    {
        type    fixedValue;
        value   uniform (1 0 0);
    }
    outlet
    {
        type    zeroGradient;
    }
    bottom
    {
        type    noSlip;
    }
    top
    {
        type    slip;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    content += _FOOTER
    (zero_dir / "U").write_text(content)


def write_pressure_field(zero_dir: Path, is_multiphase: bool) -> None:
    """Write 0/p or 0/p_rgh pressure initial and boundary conditions.

    For VOF (multiphase) simulations, OpenFOAM uses a modified pressure
    p_rgh = p - rho*g*h. The file is named accordingly so the solver
    can find it.
    """
    field_name = "p_rgh" if is_multiphase else "p"
    dims = "[1 -1 -2 0 0 0 0]"

    content = _foam_header("volScalarField", field_name)
    content += f"""
dimensions  {dims};

internalField   uniform 0;

boundaryField
{{
    inlet
    {{
        type    zeroGradient;
    }}
    outlet
    {{
        type    fixedValue;
        value   uniform 0;
    }}
    bottom
    {{
        type    zeroGradient;
    }}
    top
    {{
        type    totalPressure;
        p0      uniform 0;
    }}
    sides
    {{
        type    symmetry;
    }}
}}

"""
    content += _FOOTER
    # Write to the correct filename for the solver
    (zero_dir / field_name).write_text(content)


def write_turbulence_fields(
    zero_dir: Path, turbulence_model: TurbulenceModel
) -> None:
    """Write 0/k, 0/omega (or epsilon), and 0/nut turbulence fields.

    All RANS models require the turbulent viscosity field nut in addition
    to the transported turbulence quantities.
    """
    tm = turbulence_model
    if tm.turbulence_type == TurbulenceType.LAMINAR:
        return

    _write_k_field(zero_dir)
    _write_nut_field(zero_dir)

    if tm.turbulence_type == TurbulenceType.K_OMEGA_SST:
        _write_omega_field(zero_dir)
    elif tm.turbulence_type == TurbulenceType.K_EPSILON:
        _write_epsilon_field(zero_dir)


def write_alpha_water(zero_dir: Path) -> None:
    """Write 0/alpha.water VOF phase fraction field."""
    content = _foam_header("volScalarField", "alpha.water")
    content += """
dimensions  [0 0 0 0 0 0 0];

internalField   uniform 0;

boundaryField
{
    inlet
    {
        type    fixedValue;
        value   uniform 0;
    }
    outlet
    {
        type    zeroGradient;
    }
    bottom
    {
        type    zeroGradient;
    }
    top
    {
        type    inletOutlet;
        inletValue  uniform 0;
        value       uniform 0;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    content += _FOOTER
    (zero_dir / "alpha.water").write_text(content)


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _write_nut_field(zero_dir: Path) -> None:
    """Write 0/nut turbulent viscosity field required by all RANS models."""
    nut_content = _foam_header("volScalarField", "nut")
    nut_content += """
dimensions  [0 2 -1 0 0 0 0];

internalField   uniform 0;

boundaryField
{
    inlet
    {
        type    calculated;
        value   uniform 0;
    }
    outlet
    {
        type    calculated;
        value   uniform 0;
    }
    bottom
    {
        type    nutkWallFunction;
        value   uniform 0;
    }
    top
    {
        type    calculated;
        value   uniform 0;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    nut_content += _FOOTER
    (zero_dir / "nut").write_text(nut_content)


def _write_k_field(zero_dir: Path) -> None:
    k_content = _foam_header("volScalarField", "k")
    k_content += """
dimensions  [0 2 -2 0 0 0 0];

internalField   uniform 0.001;

boundaryField
{
    inlet
    {
        type    fixedValue;
        value   uniform 0.001;
    }
    outlet
    {
        type    zeroGradient;
    }
    bottom
    {
        type    kqRWallFunction;
        value   uniform 0.001;
    }
    top
    {
        type    zeroGradient;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    k_content += _FOOTER
    (zero_dir / "k").write_text(k_content)


def _write_omega_field(zero_dir: Path) -> None:
    omega_content = _foam_header("volScalarField", "omega")
    omega_content += """
dimensions  [0 0 -1 0 0 0 0];

internalField   uniform 1;

boundaryField
{
    inlet
    {
        type    fixedValue;
        value   uniform 1;
    }
    outlet
    {
        type    zeroGradient;
    }
    bottom
    {
        type    omegaWallFunction;
        value   uniform 1;
    }
    top
    {
        type    zeroGradient;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    omega_content += _FOOTER
    (zero_dir / "omega").write_text(omega_content)


def _write_epsilon_field(zero_dir: Path) -> None:
    eps_content = _foam_header("volScalarField", "epsilon")
    eps_content += """
dimensions  [0 2 -3 0 0 0 0];

internalField   uniform 0.001;

boundaryField
{
    inlet
    {
        type    fixedValue;
        value   uniform 0.001;
    }
    outlet
    {
        type    zeroGradient;
    }
    bottom
    {
        type    epsilonWallFunction;
        value   uniform 0.001;
    }
    top
    {
        type    zeroGradient;
    }
    sides
    {
        type    symmetry;
    }
}

"""
    eps_content += _FOOTER
    (zero_dir / "epsilon").write_text(eps_content)
