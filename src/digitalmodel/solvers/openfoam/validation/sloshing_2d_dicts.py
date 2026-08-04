#!/usr/bin/env python3
"""
ABOUTME: OpenFOAM case-dictionary templates for the 2D sloshing validation cases
(#639). Token-substituted rather than str.format-ed because OpenFOAM dicts carry
literal braces. Derived from the interFoam damBreak tutorial.
"""

from __future__ import annotations

import math
from typing import Tuple

from ..motion import (
    render_dynamic_mesh_dict_body,
    render_multi_motion_body,
)

from .sloshing_2d_config import (
    _CASE_DEPTH,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
)

# ---------------------------------------------------------------------------
# Case dictionary templates (token-substituted; OpenFOAM dicts have literal
# braces so we avoid str.format). Derived from the interFoam damBreak tutorial.
# ---------------------------------------------------------------------------

_HEADER = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"


def _hdr(cls: str, obj: str) -> str:
    return _HEADER.format(cls=cls, obj=obj)


_BLOCKMESHDICT = """
// 2D rectangular sloshing tank: breadth L (x) x height H (y), thin z slab.
scale 1;
vertices
(
    (0     0     0)
    (@LX@  0     0)
    (@LX@  @LY@  0)
    (0     @LY@  0)
    (0     0     @DEPTH@)
    (@LX@  0     @DEPTH@)
    (@LX@  @LY@  @DEPTH@)
    (0     @LY@  @DEPTH@)
);
blocks
(
    hex (0 1 2 3 4 5 6 7) (@NX@ @NY@ 1) simpleGrading (1 1 1)
);
edges ();
boundary
(
    leftWall   { type wall;  faces ( (0 4 7 3) ); }
    rightWall  { type wall;  faces ( (1 2 6 5) ); }
    lowerWall  { type wall;  faces ( (0 1 5 4) ); }
    atmosphere { type patch; faces ( (3 7 6 2) ); }
);
defaultPatch { name defaultFaces; type empty; }
"""

_CONTROLDICT = """
application     interFoam;
startFrom       startTime;
startTime       0;
stopAt          endTime;
endTime         @ENDTIME@;
deltaT          @DELTAT@;
writeControl    @WRITECONTROL@;
writeInterval   @WRITEINTERVAL@;
purgeWrite      0;
writeFormat     ascii;
writePrecision  8;
writeCompression off;
timeFormat      general;
timePrecision   8;
runTimeModifiable yes;
adjustTimeStep  @ADJUST@;
maxCo           @MAXCO@;
maxAlphaCo      @MAXCO@;
maxDeltaT       @DELTAT@;
functions
{
    // Free-surface elevation above the wall probe each sampled step.
    interfaceHeight1
    {
        type            interfaceHeight;
        libs            (fieldFunctionObjects);
        alpha           alpha.water;
        locations       ( (@PROBEX@ 0 @PROBEZ@) );
        writeControl    timeStep;
        writeInterval   @SAMPLEEVERY@;
    }
@EXTRAFUNCTIONS@
}
"""

_FVSCHEMES = """
ddtSchemes      { default Euler; }
gradSchemes     { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U)   Gauss linearUpwind grad(U);
    div(phi,alpha)  Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes   { default corrected; }
"""

_FVSOLUTION = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr      2;
        nAlphaSubCycles 1;
        cAlpha          1;
        MULESCorr       yes;
        nLimiterIter    5;
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-8;
        relTol          0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-5; relTol 0; }
    p_rgh     { solver PCG; preconditioner DIC; tolerance 1e-07; relTol 0.05; }
    p_rghFinal { $p_rgh; relTol 0; }
    U { solver smoothSolver; smoother symGaussSeidel; tolerance 1e-06; relTol 0; }
    cellDisplacement { solver PCG; preconditioner DIC; tolerance 1e-06; relTol 0; }
}
PIMPLE
{
    momentumPredictor   no;
    nOuterCorrectors    2;
    nCorrectors         3;
    nNonOrthogonalCorrectors 0;
}
relaxationFactors { equations { ".*" 1; } }
"""

_TRANSPORT = """
phases          (water air);
water
{
    transportModel  Newtonian;
    nu              1e-06;
    rho             1000;
}
air
{
    transportModel  Newtonian;
    nu              1.48e-05;
    rho             1;
}
sigma            0.07;
"""

_GRAVITY_DICT = """
dimensions      [0 1 -2 0 0 0 0];
value           (0 -9.81 0);
"""

_TURBULENCE = """
simulationType  laminar;
"""

_FIELD_ALPHA = """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    leftWall   { type zeroGradient; }
    rightWall  { type zeroGradient; }
    lowerWall  { type zeroGradient; }
    atmosphere { type inletOutlet; inletValue uniform 0; value uniform 0; }
    defaultFaces { type empty; }
}
"""

_FIELD_P_RGH = """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    leftWall   { type fixedFluxPressure; value uniform 0; }
    rightWall  { type fixedFluxPressure; value uniform 0; }
    lowerWall  { type fixedFluxPressure; value uniform 0; }
    atmosphere { type totalPressure; p0 uniform 0; }
    defaultFaces { type empty; }
}
"""

# Static-tank velocity field: slip walls to minimise numerical damping of the
# free-decay mode (the frequency is what matters, not the decay rate).
_FIELD_U_SLIP = """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    leftWall   { type slip; }
    rightWall  { type slip; }
    lowerWall  { type slip; }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
    defaultFaces { type empty; }
}
"""

# Moving-mesh velocity field: walls move with the mesh (forced roll), so the
# fluid must see the wall velocity via movingWallVelocity.
_FIELD_U_MOVING = """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    leftWall   { type movingWallVelocity; value uniform (0 0 0); }
    rightWall  { type movingWallVelocity; value uniform (0 0 0); }
    lowerWall  { type movingWallVelocity; value uniform (0 0 0); }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
    defaultFaces { type empty; }
}
"""


def _dynamic_mesh_dict_text(config: SloshingForcedRollConfig) -> str:
    """A complete constant/dynamicMeshDict for the forced-roll case (reuses #658).

    Single-DOF roll unless an EGA sway is configured, in which case the roll and
    lateral SURGE are superposed via multiMotion."""
    if config.is_combined_motion:
        body = render_multi_motion_body(config.motions())
    else:
        body = render_dynamic_mesh_dict_body(config.motion())
    return _hdr("dictionary", "dynamicMeshDict") + "\n" + body + "\n"


# Default name of the tank roll-reaction moment forces functionObject (#641).
ROLL_MOMENT_FO_NAME = "tankRollMoment"

# Tank wall patches the roll moment integrates over (the atmosphere top patch is
# excluded — it is the open free-surface lid, not a tank wall).
ROLL_MOMENT_PATCHES = ("leftWall", "rightWall", "lowerWall")


def roll_moment_function_object(
    origin: Tuple[float, float, float],
    *,
    name: str = ROLL_MOMENT_FO_NAME,
    patches: Tuple[str, ...] = ROLL_MOMENT_PATCHES,
    write_interval: int = 1,
) -> str:
    """Render an OpenFOAM ``forces`` functionObject for the tank roll moment (#641).

    Emits pressure + viscous force/moment on the tank wall patches about the roll
    centre ``origin``. The 2D sloshing plane is x-y and the physical roll is
    rotation about z (engine YAW), so the roll-reaction moment is the **z
    component** of the moment vector the FO writes to
    ``postProcessing/<name>/<t0>/moment.dat``. ``CofR`` (centre of rotation) is
    the roll axis origin = the tank roll centre; the axis itself is implicitly z
    (we take ``M_z``). For interFoam ``rho`` is the real density field, so we set
    ``rho rho`` (the FO then treats ``p`` as dynamic pressure in Pa).

    Args:
        origin: Roll-axis origin ``(x y z)`` (m) — the tank roll centre; equal to
            the ``PrescribedMotion.origin`` driving the forced roll.
        name: FunctionObject name (its ``postProcessing`` subdirectory).
        patches: Tank wall patches to integrate the moment over.
        write_interval: ``timeStep`` write stride (1 = every step, densest fit).

    Returns:
        The functionObject dict body (to embed in ``controlDict`` ``functions``).
    """
    fmt = "{:.8g}".format
    patch_list = " ".join(patches)
    ox, oy, oz = origin
    return (
        f"    {name}\n"
        "    {\n"
        "        type            forces;\n"
        "        libs            (forces);\n"
        f"        patches         ( {patch_list} );\n"
        "        // interFoam has a real rho field -> p is dynamic pressure (Pa).\n"
        "        rho             rho;\n"
        "        // Roll axis origin (tank roll centre); roll = rotation about z,\n"
        "        // so the roll-reaction moment is moment.dat's z component.\n"
        f"        CofR            ({fmt(ox)} {fmt(oy)} {fmt(oz)});\n"
        "        writeControl    timeStep;\n"
        f"        writeInterval   {int(write_interval)};\n"
        "        log             no;\n"
        "    }"
    )


# ---------------------------------------------------------------------------
# setFields: first-mode cosine perturbation (free-decay)
# ---------------------------------------------------------------------------


def cosine_mode_setfields_body(config: SloshingFreeDecayConfig) -> str:
    """setFieldsDict body: a first-mode cosine perturbation of the free surface.

    The still-water level ``h`` is perturbed as ``eta(x) = h + A*cos(pi*x/L)``
    (high at the left wall, low at the right — the antisymmetric first mode,
    volume-neutral since the cosine integrates to zero over the breadth). The
    surface is discretised as one liquid ``boxToCell`` per mesh column.
    """
    L = config.breadth
    h = config.fill_depth
    A = config.perturbation_amplitude
    n = config.nx
    dx = config.cell_size
    depth = _CASE_DEPTH
    lines = [
        "defaultFieldValues ( volScalarFieldValue alpha.water 0 );",
        "regions",
        "(",
    ]
    for i in range(n):
        x0 = i * dx
        x1 = (i + 1) * dx
        xc = 0.5 * (x0 + x1)
        eta = h + A * math.cos(math.pi * xc / L)
        lines.append("    boxToCell")
        lines.append("    {")
        lines.append(
            f"        box ({x0:.6g} {-depth:.6g} {-depth:.6g}) "
            f"({x1:.6g} {eta:.6g} {2 * depth:.6g});"
        )
        lines.append(
            "        fieldValues ( volScalarFieldValue alpha.water 1 );"
        )
        lines.append("    }")
    lines.append(");")
    return "\n".join(lines) + "\n"
