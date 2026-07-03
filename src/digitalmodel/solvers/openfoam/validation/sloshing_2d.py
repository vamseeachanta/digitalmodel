#!/usr/bin/env python3
"""
ABOUTME: First validated 2D rectangular-tank sloshing case (#639) for the ACMA
B1546 ballast-tank tuned-liquid-damper study. Assembles a 2D interFoam VOF tank
from the existing case stack (blockMesh + setFields partial fill (#659) +
optional prescribed forced roll from the motion engine (#658)) and validates the
measured first-mode sloshing frequency against the analytical prismatic-tank
tanh dispersion relation (spectral_analysis.prismatic_tank_natural_frequency).

Two configurations
------------------
1. Free-decay (PRIMARY / hard gate). A static tank with a small first-mode
   cosine perturbation of the free surface is released and left to oscillate.
   The free-surface elevation at a wall probe is FFT'd and the fundamental
   sloshing frequency is compared to the analytical value within
   ``SLOSHING_FREQ_TOLERANCE`` (~5%). No mesh motion, slip walls (minimise
   numerical damping of the mode).

2. Forced-roll (CORROBORATION). The SPHERIC Test 10 rectangular-tank forced-roll
   benchmark (Delorme et al. 2009): breadth 0.9 m, height 0.508 m, 18% fill
   (h = 0.093 m), forced roll ~4 deg near the first sloshing mode. The tank is
   driven with the prescribed-motion engine (in-plane rotation about the
   out-of-plane z axis = the 2D section's physical roll) and the resonant
   free-surface run-up on the walls is observed. Not gated on reproducing the
   experimental pressure trace (needs the raw dataset).

2D convention
-------------
Sloshing plane is x (breadth L) - y (vertical), z is the thin out-of-plane slab
with ``empty`` front/back patches; gravity is ``(0 -9.81 0)``. The physical roll
of this section is therefore rotation about the z axis, which the motion engine
calls ``MotionType.YAW`` (Euler angle on z). Gravity stays in the global frame,
so rotating the tank reproduces the oscillating body force of the benchmark.

Source / citation
-----------------
- Analytical: linear-potential prismatic-tank dispersion
  omega_n^2 = (n*pi*g/L) * tanh(n*pi*h/L) (implemented in spectral_analysis).
- SPHERIC Test 10 / Delorme, L., Colagrossi, A., Souto-Iglesias, A.,
  Zamora-Rodriguez, R. & Botia-Vera, E. (2009). "A set of canonical problems in
  sloshing. Part I: Pressure field in forced roll - comparison between
  experimental results and SPH." Ocean Engineering 36(2), 168-178.
  Tank 0.9 m x 0.508 m; 18% fill h=0.093 m; first-mode period T1=1.9191 s
  (the analytical tanh relation reproduces T1 to 4 significant figures);
  rotation axis at the centre of the tank floor.
- Dictionaries derive from $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak
  (ESI v2312), consistent with the verified dam-break/Kleefsman cases.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Tuple

from ..motion import MotionType, PrescribedMotion, render_dynamic_mesh_dict_body
from ..partial_fill import (
    partial_fill_box,
    render_set_fields_dict_body,
    snap_fill_to_cell_face,
)
from ..spectral_analysis import (
    GRAVITY,
    compute_fft_spectrum,
    prismatic_tank_natural_frequency,
)

# Primary gate (#639): measured first-mode sloshing frequency tracks the
# analytical tanh dispersion relation within 5%.
SLOSHING_FREQ_TOLERANCE = 0.05

# Out-of-plane slab thickness for the 2D case (m).
_CASE_DEPTH = 0.01


# ---------------------------------------------------------------------------
# Free-decay natural-frequency configuration (PRIMARY gate)
# ---------------------------------------------------------------------------


@dataclass
class SloshingFreeDecayConfig:
    """Config for the free-decay first-mode sloshing-frequency validation.

    A rectangular tank of breadth ``L`` and height ``tank_height`` filled to
    ``fill_level`` is given a small first-mode cosine perturbation of the free
    surface and released. The clean dimensions (L=1.0 m, half full) put the
    analytical first mode at ~0.76 Hz.

    Attributes:
        breadth: Tank breadth ``L`` in the sloshing (x) direction (m).
        tank_height: Internal tank height (y) (m).
        fill_level: Still-water fill fraction of ``tank_height`` (0-1). Snapped
            onto a cell face by the partial-fill helper (#659).
        cells_per_breadth: Uniform cells across the breadth; the cell size
            ``L/cells_per_breadth`` is used in both x and y.
        perturbation_amplitude: First-mode cosine perturbation amplitude (m);
            small vs the fill depth to stay in the linear regime.
        delta_t: Fixed time step (s) — fixed so the probe sampling is uniform
            for the FFT.
        end_time: Physical end time (s); ~12 first-mode periods.
        sample_every: Probe (interfaceHeight) sampling stride in time steps.
        field_write_interval: Full-field snapshot interval (s) for qualitative
            inspection.
        name: Case directory name.
    """

    breadth: float = 1.0
    tank_height: float = 0.6
    fill_level: float = 0.5
    cells_per_breadth: int = 100
    perturbation_amplitude: float = 0.02
    delta_t: float = 0.002
    end_time: float = 16.0
    sample_every: int = 10
    field_write_interval: float = 0.5
    name: str = "validation_sloshing_free_decay"

    @property
    def cell_size(self) -> float:
        """Uniform cell size ``L / cells_per_breadth`` (m)."""
        return self.breadth / self.cells_per_breadth

    @property
    def nx(self) -> int:
        return self.cells_per_breadth

    @property
    def ny(self) -> int:
        """Vertical cell count (tank_height / cell_size, rounded)."""
        return max(1, round(self.tank_height / self.cell_size))

    @property
    def fill_snap(self):
        """Fill snapped onto a vertical cell face (see partial_fill)."""
        return snap_fill_to_cell_face(self.tank_height, self.ny, self.fill_level)

    @property
    def fill_depth(self) -> float:
        """Snapped still-water fill depth ``h`` (m)."""
        return self.fill_snap.fill_height

    def analytical_frequency(self, mode: int = 1) -> float:
        """Analytical first-mode sloshing frequency (Hz) for the snapped fill."""
        return prismatic_tank_natural_frequency(
            self.breadth, self.fill_depth, mode=mode
        )

    @property
    def probe_x(self) -> float:
        """Wall-probe x location (half a cell off the left wall)."""
        return 0.5 * self.cell_size


# ---------------------------------------------------------------------------
# Forced-roll SPHERIC Test 10 configuration (CORROBORATION)
# ---------------------------------------------------------------------------


@dataclass
class SloshingForcedRollConfig:
    """Config for the SPHERIC Test 10 forced-roll rectangular-tank benchmark.

    Attributes:
        breadth: Tank breadth ``L`` (m) — SPHERIC Test 10 = 0.9 m.
        tank_height: Tank height (m) — 0.508 m.
        fill_depth: Still-water fill depth ``h`` (m) — 0.093 m (18% fill).
        roll_amplitude_deg: Forced-roll amplitude (deg) — ~4 deg.
        roll_period: Forced-roll period (s). Defaults to the benchmark's
            lateral-impact drive 0.85*T1 (T1 = first-mode period).
        cells_per_breadth: Uniform cells across the breadth.
        delta_t: Base time step (s); the solver adapts on Courant number.
        n_cycles: Number of forcing periods to run.
        field_write_interval: Full-field snapshot interval (s).
        name: Case directory name.
    """

    breadth: float = 0.9
    tank_height: float = 0.508
    fill_depth: float = 0.093
    roll_amplitude_deg: float = 4.0
    roll_period: float | None = None
    cells_per_breadth: int = 90
    delta_t: float = 0.001
    n_cycles: float = 6.0
    field_write_interval: float = 0.05
    name: str = "validation_sloshing_spheric_test10"

    @property
    def cell_size(self) -> float:
        return self.breadth / self.cells_per_breadth

    @property
    def nx(self) -> int:
        return self.cells_per_breadth

    @property
    def ny(self) -> int:
        return max(1, round(self.tank_height / self.cell_size))

    @property
    def fill_level(self) -> float:
        return self.fill_depth / self.tank_height

    @property
    def fill_snap(self):
        return snap_fill_to_cell_face(self.tank_height, self.ny, self.fill_level)

    def analytical_frequency(self, mode: int = 1) -> float:
        return prismatic_tank_natural_frequency(
            self.breadth, self.fill_depth, mode=mode
        )

    @property
    def first_mode_period(self) -> float:
        """Analytical first-mode period T1 (s)."""
        return 1.0 / self.analytical_frequency()

    @property
    def drive_period(self) -> float:
        """Forced-roll drive period (s) — default 0.85*T1 (lateral-impact case)."""
        if self.roll_period is not None:
            return self.roll_period
        return 0.85 * self.first_mode_period

    @property
    def end_time(self) -> float:
        return self.n_cycles * self.drive_period

    @property
    def roll_origin(self) -> Tuple[float, float, float]:
        """Rotation axis at the centre of the tank floor (m)."""
        return (0.5 * self.breadth, 0.0, 0.0)

    def motion(self) -> PrescribedMotion:
        """Prescribed forced roll = in-plane rotation about z (engine YAW)."""
        return PrescribedMotion(
            MotionType.YAW,
            amplitude=self.roll_amplitude_deg,
            period=self.drive_period,
            origin=self.roll_origin,
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


def _dynamic_mesh_dict_text(motion: PrescribedMotion) -> str:
    """A complete constant/dynamicMeshDict for the forced-roll case (reuses #658)."""
    return _hdr("dictionary", "dynamicMeshDict") + "\n" + render_dynamic_mesh_dict_body(
        motion
    ) + "\n"


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


# ---------------------------------------------------------------------------
# Case builders
# ---------------------------------------------------------------------------


def _write_common(
    case_dir: Path,
    *,
    blockmesh: str,
    control: str,
    setfields: str,
    u_field: str,
    dynamic_mesh: str | None,
    provenance: Dict[str, Any],
) -> Path:
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)
    sysd, constd, zerod = (
        case_dir / "system",
        case_dir / "constant",
        case_dir / "0",
    )
    (sysd / "blockMeshDict").write_text(_hdr("dictionary", "blockMeshDict") + blockmesh)
    (sysd / "controlDict").write_text(_hdr("dictionary", "controlDict") + control)
    (sysd / "fvSchemes").write_text(_hdr("dictionary", "fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_hdr("dictionary", "fvSolution") + _FVSOLUTION)
    (sysd / "setFieldsDict").write_text(
        _hdr("dictionary", "setFieldsDict") + setfields
    )
    (constd / "transportProperties").write_text(
        _hdr("dictionary", "transportProperties") + _TRANSPORT
    )
    (constd / "g").write_text(_hdr("uniformDimensionedVectorField", "g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(
        _hdr("dictionary", "turbulenceProperties") + _TURBULENCE
    )
    if dynamic_mesh is not None:
        (constd / "dynamicMeshDict").write_text(dynamic_mesh)
    (zerod / "alpha.water").write_text(_hdr("volScalarField", "alpha.water") + _FIELD_ALPHA)
    (zerod / "p_rgh").write_text(_hdr("volScalarField", "p_rgh") + _FIELD_P_RGH)
    (zerod / "U").write_text(_hdr("volVectorField", "U") + u_field)
    (case_dir / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    return case_dir


def _blockmesh(config, cfg_ny: int) -> str:
    fmt = "{:.6g}".format
    return (
        _BLOCKMESHDICT
        .replace("@LX@", fmt(config.breadth))
        .replace("@LY@", fmt(config.tank_height))
        .replace("@DEPTH@", fmt(_CASE_DEPTH))
        .replace("@NX@", str(config.nx))
        .replace("@NY@", str(cfg_ny))
    )


def build_free_decay_case(
    config: SloshingFreeDecayConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the free-decay first-mode sloshing-frequency validation case.

    Static tank (no mesh motion), slip walls, a first-mode cosine free-surface
    perturbation initialised by ``setFields``. Run ``blockMesh`` -> ``setFields``
    -> ``interFoam``; the ``interfaceHeight`` functionObject records the wall
    free-surface elevation for the FFT.
    """
    config = config or SloshingFreeDecayConfig()
    case_dir = Path(parent_dir) / config.name
    fmt = "{:.6g}".format

    blockmesh = _blockmesh(config, config.ny)
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@DELTAT@", fmt(config.delta_t))
        .replace("@WRITECONTROL@", "adjustableRunTime")
        .replace("@WRITEINTERVAL@", fmt(config.field_write_interval))
        .replace("@ADJUST@", "no")
        .replace("@MAXCO@", "1")
        .replace("@PROBEX@", fmt(config.probe_x))
        .replace("@PROBEZ@", fmt(0.5 * _CASE_DEPTH))
        .replace("@SAMPLEEVERY@", str(config.sample_every))
        .replace("@EXTRAFUNCTIONS@", "")
    )
    setfields = cosine_mode_setfields_body(config)
    return _write_common(
        case_dir,
        blockmesh=blockmesh,
        control=control,
        setfields=setfields,
        u_field=_FIELD_U_SLIP,
        dynamic_mesh=None,
        provenance=_free_decay_provenance(config),
    )


def build_forced_roll_case(
    config: SloshingForcedRollConfig | None = None,
    parent_dir: Path | str = ".",
    *,
    with_moment: bool = False,
    moment_write_interval: int = 1,
) -> Path:
    """Generate the SPHERIC Test 10 forced-roll validation case.

    Moving-mesh tank driven by the prescribed-motion engine (in-plane roll about
    z), flat partial fill at 18%, ``movingWallVelocity`` walls. Run
    ``blockMesh`` -> ``setFields`` -> ``interFoam`` (dynamic mesh handled by the
    ``dynamicMeshDict``).

    Args:
        config: Forced-roll configuration (defaults to SPHERIC Test 10).
        parent_dir: Parent directory for the generated case directory.
        with_moment: If True, also emit the tank roll-reaction moment ``forces``
            functionObject (#641) about the roll axis (origin = the motion's roll
            centre, axis = z). Its ``moment.dat`` z-component is the roll moment
            consumed by the fill/frequency sweep reduction.
        moment_write_interval: ``timeStep`` stride for the moment FO (1 = every
            step; a dense series improves the first-harmonic fit).
    """
    config = config or SloshingForcedRollConfig()
    case_dir = Path(parent_dir) / config.name
    fmt = "{:.6g}".format
    ny = config.ny

    extra_functions = ""
    if with_moment:
        extra_functions = roll_moment_function_object(
            config.roll_origin, write_interval=moment_write_interval
        )

    blockmesh = _blockmesh(config, ny)
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@DELTAT@", fmt(config.delta_t))
        .replace("@WRITECONTROL@", "adjustableRunTime")
        .replace("@WRITEINTERVAL@", fmt(config.field_write_interval))
        .replace("@ADJUST@", "yes")
        .replace("@MAXCO@", "0.5")
        .replace("@PROBEX@", fmt(0.5 * config.breadth))
        .replace("@PROBEZ@", fmt(0.5 * _CASE_DEPTH))
        .replace("@SAMPLEEVERY@", "5")
        .replace("@EXTRAFUNCTIONS@", extra_functions)
    )

    # Flat partial fill snapped onto a cell face (#659).
    snap = config.fill_snap
    box_min, box_max = partial_fill_box(
        [0.0, 0.0, 0.0],
        [config.breadth, config.tank_height, _CASE_DEPTH],
        snap.fill_height,
        vertical_axis=1,
    )
    setfields = render_set_fields_dict_body(box_min, box_max)

    return _write_common(
        case_dir,
        blockmesh=blockmesh,
        control=control,
        setfields=setfields,
        u_field=_FIELD_U_MOVING,
        dynamic_mesh=_dynamic_mesh_dict_text(config.motion()),
        provenance=_forced_roll_provenance(config),
    )


# ---------------------------------------------------------------------------
# Post-processing: parse interfaceHeight, FFT, measure natural frequency
# ---------------------------------------------------------------------------


def parse_interface_height(
    case_dir: Path | str,
    fo_name: str = "interfaceHeight1",
    *,
    expected_height: float | None = None,
) -> Tuple[List[float], List[float]]:
    """Parse the ``interfaceHeight`` functionObject output into (times, elevation).

    The FO writes ``postProcessing/<fo>/<t0>/height.dat`` with a time column and
    two columns per probe (interface height above the location and distance to
    the interface). We pick the data column whose time-mean is closest to
    ``expected_height`` (the still-water level) as the elevation signal.
    """
    case_dir = Path(case_dir)
    base = case_dir / "postProcessing" / fo_name
    dats = sorted(base.glob("*/height.dat"))
    if not dats:
        raise FileNotFoundError(f"no height.dat under {base}")
    times: List[float] = []
    cols: List[List[float]] = []
    for dat in dats:
        for line in dat.read_text().splitlines():
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            parts = s.split()
            try:
                vals = [float(p) for p in parts]
            except ValueError:
                continue
            times.append(vals[0])
            data = vals[1:]
            if not cols:
                cols = [[] for _ in data]
            for j, v in enumerate(data):
                if j < len(cols):
                    cols[j].append(v)
    if not cols:
        raise RuntimeError(f"no numeric rows parsed from {base}")

    # Choose the column most consistent with a free-surface elevation signal.
    def _score(col: List[float]) -> float:
        m = sum(col) / len(col)
        if expected_height is not None:
            return abs(m - expected_height)
        return -(_variance(col))  # else the most oscillatory column

    best = min(range(len(cols)), key=lambda j: _score(cols[j]))
    return times, cols[best]


def _variance(xs: List[float]) -> float:
    m = sum(xs) / len(xs)
    return sum((x - m) ** 2 for x in xs) / len(xs)


def parse_roll_moment(
    case_dir: Path | str,
    fo_name: str = ROLL_MOMENT_FO_NAME,
) -> Tuple[List[float], List[float]]:
    """Parse the ``forces`` moment time history into ``(times, moment_z)`` (#641).

    Reads ``postProcessing/<fo>/<t0>/moment.dat`` written by the roll-moment
    functionObject and returns the **total** (pressure + viscous) moment z
    component — the roll-reaction moment about the z axis for the 2D x-y sloshing
    plane. Robust to the two ESI column layouts:

    - modern (v2012+): ``time (total)(pressure)(viscous)[(porous)]`` — the first
      vector is the total, so ``M_z = total_z``;
    - legacy: ``time (pressure)(viscous)`` — ``M_z = pressure_z + viscous_z``.

    Parentheses around the vectors are stripped and the columns split on the
    3-vector count, mirroring :meth:`OpenFOAMPostProcessor.parse_force_file`.
    """
    case_dir = Path(case_dir)
    base = case_dir / "postProcessing" / fo_name
    dats = sorted(base.glob("*/moment.dat"))
    if not dats:
        raise FileNotFoundError(f"no moment.dat under {base}")

    times: List[float] = []
    moment_z: List[float] = []
    for dat in dats:
        for line in dat.read_text().splitlines():
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            cleaned = s.replace("(", " ").replace(")", " ")
            parts = cleaned.split()
            try:
                vals = [float(p) for p in parts]
            except ValueError:
                continue
            if len(vals) < 4:
                continue
            t = vals[0]
            rest = vals[1:]
            n_vec = len(rest) // 3
            if n_vec < 2:
                continue
            if n_vec == 2:
                # legacy: (pressure, viscous) -> total_z = p_z + v_z
                mz = rest[2] + rest[5]
            else:
                # modern: first vector is the total
                mz = rest[2]
            times.append(t)
            moment_z.append(mz)

    if not times:
        raise RuntimeError(f"no numeric moment rows parsed from {base}")
    return times, moment_z


def _refine_peak_parabolic(freqs, amp, idx: int) -> float:
    """Sub-bin peak frequency via 3-point parabolic interpolation."""
    if idx <= 0 or idx >= len(amp) - 1:
        return float(freqs[idx])
    a0, a1, a2 = amp[idx - 1], amp[idx], amp[idx + 1]
    denom = a0 - 2.0 * a1 + a2
    if denom == 0.0:
        return float(freqs[idx])
    delta = 0.5 * (a0 - a2) / denom
    df = float(freqs[1] - freqs[0])
    return float(freqs[idx]) + delta * df


def measure_natural_frequency(
    times: List[float],
    elevation: List[float],
    *,
    min_frequency: float = 0.05,
) -> Dict[str, float]:
    """Measure the fundamental sloshing frequency from a wall-elevation series.

    Returns the raw FFT dominant-bin frequency and a parabolically-refined
    estimate. The series is assumed uniformly sampled (fixed solver time step).
    """
    import numpy as np

    t = np.asarray(times, dtype=float)
    y = np.asarray(elevation, dtype=float)
    dt = float(np.mean(np.diff(t)))
    sample_rate = 1.0 / dt
    freqs, amp = compute_fft_spectrum(y, sample_rate, detrend="constant", window=True)

    band = freqs >= min_frequency
    idx_band = np.flatnonzero(band)
    sub = amp[band]
    kk = int(idx_band[int(np.argmax(sub))])
    raw = float(freqs[kk])
    refined = _refine_peak_parabolic(freqs, amp, kk)
    return {
        "raw_frequency": raw,
        "refined_frequency": refined,
        "sample_rate": sample_rate,
        "n_samples": float(len(y)),
        "freq_resolution": float(freqs[1] - freqs[0]),
    }


def analyze_free_decay(
    case_dir: Path | str,
    config: SloshingFreeDecayConfig | None = None,
) -> Dict[str, float]:
    """Full free-decay analysis: measured vs analytical first-mode frequency."""
    config = config or SloshingFreeDecayConfig()
    times, elevation = parse_interface_height(
        case_dir, expected_height=config.fill_depth
    )
    meas = measure_natural_frequency(times, elevation)
    analytical = config.analytical_frequency()
    measured = meas["refined_frequency"]
    rel_err = abs(measured - analytical) / analytical
    return {
        **meas,
        "analytical_frequency": analytical,
        "measured_frequency": measured,
        "relative_error": rel_err,
        "within_tolerance": float(rel_err <= SLOSHING_FREQ_TOLERANCE),
        "fill_depth": config.fill_depth,
        "breadth": config.breadth,
    }


# ---------------------------------------------------------------------------
# Provenance
# ---------------------------------------------------------------------------


def _free_decay_provenance(config: SloshingFreeDecayConfig) -> Dict[str, Any]:
    return {
        "validation_case": "sloshing_2d_free_decay",
        "issue": "#639",
        "gate": "measured first-mode sloshing frequency vs analytical tanh",
        "analytical_frequency_hz": config.analytical_frequency(),
        "analytical_relation": "omega_n^2 = (n*pi*g/L)*tanh(n*pi*h/L)",
        "breadth_m": config.breadth,
        "tank_height_m": config.tank_height,
        "fill_level_requested": config.fill_level,
        "fill_level_snapped": config.fill_snap.fill_level,
        "fill_depth_m": config.fill_depth,
        "perturbation_amplitude_m": config.perturbation_amplitude,
        "mesh_cells": [config.nx, config.ny, 1],
        "delta_t_s": config.delta_t,
        "end_time_s": config.end_time,
        "gravity": GRAVITY,
        "tolerance": SLOSHING_FREQ_TOLERANCE,
    }


def _forced_roll_provenance(config: SloshingForcedRollConfig) -> Dict[str, Any]:
    return {
        "validation_case": "sloshing_2d_spheric_test10",
        "issue": "#639",
        "reference": (
            "Delorme et al. (2009) Ocean Engineering 36(2) 168-178; "
            "SPHERIC Test 10 forced-roll rectangular tank"
        ),
        "breadth_m": config.breadth,
        "tank_height_m": config.tank_height,
        "fill_depth_m": config.fill_depth,
        "fill_level": config.fill_level,
        "roll_amplitude_deg": config.roll_amplitude_deg,
        "drive_period_s": config.drive_period,
        "first_mode_period_s": config.first_mode_period,
        "analytical_frequency_hz": config.analytical_frequency(),
        "roll_origin_m": list(config.roll_origin),
        "mesh_cells": [config.nx, config.ny, 1],
        "n_cycles": config.n_cycles,
        "end_time_s": config.end_time,
    }
