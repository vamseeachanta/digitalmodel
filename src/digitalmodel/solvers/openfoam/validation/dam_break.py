#!/usr/bin/env python3
"""
ABOUTME: Foundational CFD validation case #1165 — 2D dam break (collapse of a
water column), validated against the Martin & Moyce (1952) experiments. This is
the keystone free-surface (interFoam VOF) case: the gate for every marine
free-surface workflow (wave loading, green water, sloshing). Pure reference
data and helpers live here with no OpenFOAM dependency; the case builder writes
a verified interFoam case derived from the canonical damBreak tutorial.

Source / citation
-----------------
- Martin, J.C. & Moyce, W.J. (1952). "Part IV. An experimental study of the
  collapse of liquid columns on a rigid horizontal plane." Phil. Trans. R.
  Soc. Lond. A, 244(882), 312-324. doi:10.1098/rsta.1952.0006. The n^2 = 2
  configuration (column height = twice its base width a).
- Digitized surge-front data: the Lethe project's dam-break example
  (chaos-polymtl/lethe, examples/multiphysics/dam-break/dam-break-2d.py),
  which also applies the standard +0.175 dimensionless gate-release shift.
- OpenFOAM tutorial the dictionaries derive from:
  $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak/damBreak (ESI v2312);
  the tutorial's downstream obstacle is removed to match the plain
  Martin & Moyce tank.

Validation gate (#1165): dimensionless surge front Z(T) tracks Martin & Moyce
within 10% (with the standard gate-release time shift), and the total water
volume is conserved to < 1%.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Sequence, Tuple

# Issue #1165 headline gate: surge front tracks Martin & Moyce within 10%.
DAM_BREAK_FRONT_TOLERANCE = 0.10

# Regression gate for the *solved* case with the gate-release shift applied.
# The verified runs (docs/api/cfd/cases/dam_break/, 288x144 reference mesh and
# the 144x72 builder mesh) both give a mean front deviation of ~3.0%; 6% is
# ~2x headroom against solver-version and meshing variation while still
# failing on any gross departure. See the report
# docs/api/cfd/dam-break-verification.html.
DAM_BREAK_SOLVE_TOLERANCE = 0.06

# Sanity bound on the *uncorrected* comparison. Without the gate-release shift
# the early-time deviation is dominated by the experiment's finite gate-removal
# time (the CFD column is released instantaneously), giving ~10% mean on the
# verified runs; 15% still catches a wrong-physics solve.
DAM_BREAK_RAW_FRONT_TOLERANCE = 0.15

# Mass-conservation gate: total alpha.water volume drift (< 1%).
DAM_BREAK_MASS_TOLERANCE = 0.01

# Standard dimensionless time shift accounting for the experiment's finite
# gate-release (the simulation column is released instantaneously). The Lethe
# dam-break benchmark applies exactly this value to simulation time.
GATE_RELEASE_TIME_SHIFT = 0.175

GRAVITY = 9.81

# Martin & Moyce (1952), n^2 = 2: dimensionless surge-front position
# Z = x/a vs T = t*sqrt(2*g/a), a = column base width. Digitized values from
# the Lethe project (see module docstring); the trivial (T=0, Z=1) point is
# omitted from the deviation metric.
MARTIN_MOYCE_N2_TIME: Tuple[float, ...] = (
    0.41, 0.84, 1.19, 1.43, 1.63, 1.82, 1.97, 2.2, 2.32, 2.5, 2.64, 2.82, 2.96,
)
MARTIN_MOYCE_N2_FRONT: Tuple[float, ...] = (
    1.11, 1.23, 1.44, 1.67, 1.89, 2.11, 2.33, 2.56, 2.78, 3.0, 3.22, 3.44, 3.67,
)


# ---------------------------------------------------------------------------
# Pure reference helpers (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def dimensionless_time(t: float, column_width: float) -> float:
    """Martin & Moyce dimensionless time ``T = t * sqrt(2*g/a)``.

    Args:
        t: Physical time (s, >= 0).
        column_width: Column base width ``a`` (m, > 0).

    Returns:
        Dimensionless time ``T``.

    Raises:
        ValueError: If ``t`` is negative or ``column_width`` not positive.
    """
    if t < 0.0:
        raise ValueError(f"t must be non-negative, got {t}")
    if column_width <= 0.0:
        raise ValueError(f"column_width must be positive, got {column_width}")
    return t * math.sqrt(2.0 * GRAVITY / column_width)


def dimensionless_front(x: float, column_width: float) -> float:
    """Martin & Moyce dimensionless surge-front position ``Z = x/a``."""
    if column_width <= 0.0:
        raise ValueError(f"column_width must be positive, got {column_width}")
    return x / column_width


def martin_moyce_front(t_star: float) -> float:
    """Experimental surge-front position ``Z`` at dimensionless time ``T``.

    Linear interpolation of the digitized Martin & Moyce (1952) n^2=2 data.

    Args:
        t_star: Dimensionless time within the digitized range.

    Returns:
        Dimensionless front position ``Z = x/a``.

    Raises:
        ValueError: If ``t_star`` is outside the digitized data range.
    """
    ts, zs = MARTIN_MOYCE_N2_TIME, MARTIN_MOYCE_N2_FRONT
    if not ts[0] <= t_star <= ts[-1]:
        raise ValueError(
            f"T={t_star} outside the digitized range [{ts[0]}, {ts[-1]}]"
        )
    for i in range(len(ts) - 1):
        if ts[i] <= t_star <= ts[i + 1]:
            frac = (t_star - ts[i]) / (ts[i + 1] - ts[i])
            return zs[i] + frac * (zs[i + 1] - zs[i])
    return zs[-1]  # pragma: no cover — bounds guarded above


def front_deviation(
    t_cfd: Sequence[float],
    z_cfd: Sequence[float],
    time_shift: float = GATE_RELEASE_TIME_SHIFT,
) -> Tuple[float, float]:
    """Mean and max relative deviation of a CFD front history vs Martin & Moyce.

    The CFD curve is interpolated (after adding ``time_shift`` to its times) at
    each digitized experimental time and compared point-wise.

    Args:
        t_cfd: CFD dimensionless times (ascending).
        z_cfd: CFD dimensionless front positions.
        time_shift: Dimensionless shift added to CFD time; the default is the
            standard gate-release correction, pass ``0.0`` for the raw metric.

    Returns:
        ``(mean_abs_rel_deviation, max_abs_rel_deviation)``.

    Raises:
        ValueError: If the (shifted) CFD history does not cover the digitized
            experimental time range.
    """
    if len(t_cfd) != len(z_cfd) or len(t_cfd) < 2:
        raise ValueError("t_cfd/z_cfd must be equal-length with >= 2 samples")
    ts = [t + time_shift for t in t_cfd]
    if ts[0] > MARTIN_MOYCE_N2_TIME[0] or ts[-1] < MARTIN_MOYCE_N2_TIME[-1]:
        raise ValueError(
            f"CFD history [{ts[0]:.2f}, {ts[-1]:.2f}] does not cover the "
            f"experimental range [{MARTIN_MOYCE_N2_TIME[0]}, "
            f"{MARTIN_MOYCE_N2_TIME[-1]}]"
        )
    devs = []
    for t_exp, z_exp in zip(MARTIN_MOYCE_N2_TIME, MARTIN_MOYCE_N2_FRONT):
        # linear interpolation of the CFD curve at t_exp
        for i in range(len(ts) - 1):
            if ts[i] <= t_exp <= ts[i + 1]:
                frac = (t_exp - ts[i]) / (ts[i + 1] - ts[i])
                z_i = z_cfd[i] + frac * (z_cfd[i + 1] - z_cfd[i])
                devs.append(abs(z_i - z_exp) / z_exp)
                break
    return sum(devs) / len(devs), max(devs)


# ---------------------------------------------------------------------------
# Case configuration + builder
# ---------------------------------------------------------------------------


@dataclass
class DamBreakConfig:
    """Config for the Martin & Moyce n^2=2 dam-break validation case.

    Geometry follows the classic damBreak tutorial scale: column base width
    ``a`` and height ``2a`` (n^2=2), in a plain tank of length ``8a`` and
    height ``4a`` (no obstacle). The mesh is uniform with ``cells_per_width``
    cells across the column base so the column edges fall exactly on cell
    faces and ``setFields`` initialises the exact Martin & Moyce column.

    Attributes:
        column_width: Column base width ``a`` (m).
        cells_per_width: Uniform cells across ``a``. The default (18) is the
            fast regression mesh (144x72, interFoam ~10 s); the archived
            reference case uses 36 (288x144) — both verified to the same
            deviation (~3.0% mean with the gate-release shift).
        end_time: Physical end time (s); the default covers the digitized
            experimental range (T = 2.96 -> t = 0.256 s at a = 0.1461 m).
        write_interval: Output write interval (s).
        name: Case directory name.
    """

    column_width: float = 0.1461
    cells_per_width: int = 18
    end_time: float = 0.3
    write_interval: float = 0.01
    name: str = "validation_dam_break"

    @property
    def column_height(self) -> float:
        """Column height ``2a`` (m) — the n^2=2 configuration."""
        return 2.0 * self.column_width

    @property
    def tank_length(self) -> float:
        """Tank length ``8a`` (m)."""
        return 8.0 * self.column_width

    @property
    def tank_height(self) -> float:
        """Tank height ``4a`` (m)."""
        return 4.0 * self.column_width

    @property
    def nx(self) -> int:
        """Streamwise cell count (tank length / (a / cells_per_width))."""
        return 8 * self.cells_per_width

    @property
    def ny(self) -> int:
        """Vertical cell count."""
        return 4 * self.cells_per_width

    @property
    def cell_size(self) -> float:
        """Uniform cell size ``a / cells_per_width`` (m)."""
        return self.column_width / self.cells_per_width

    def time_at(self, t_star: float) -> float:
        """Physical time (s) at dimensionless time ``T``."""
        return t_star / math.sqrt(2.0 * GRAVITY / self.column_width)


# ---------------------------------------------------------------------------
# Verified case templates (docs/api/cfd/cases/dam_break/). Tokens are
# substituted with str.replace (OpenFOAM dictionaries are full of literal
# braces). Dictionaries derive from the interFoam damBreak tutorial (v2312)
# with the obstacle removed and a mass-conservation functionObject added.
# ---------------------------------------------------------------------------

_HEADER = """FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}
"""


def _hdr(cls: str, obj: str) -> str:
    return _HEADER.format(cls=cls, obj=obj)


_BLOCKMESHDICT = """
// Martin & Moyce (1952) dam break, n^2 = 2: column a x 2a in a plain
// 8a x 4a tank (no obstacle). Uniform mesh, dx = dy = a/@CPW@, so the
// column edges (x = a, y = 2a) fall exactly on cell faces.
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
deltaT          0.001;
writeControl    adjustable;
writeInterval   @WRITEINTERVAL@;
purgeWrite      0;
writeFormat     ascii;
writePrecision  6;
writeCompression off;
timeFormat      general;
timePrecision   6;
runTimeModifiable yes;
adjustTimeStep  yes;
maxCo           1;
maxAlphaCo      1;
maxDeltaT       1;
functions
{
    // Total water volume each time step: mass-conservation gate (< 1% drift).
    waterVolume
    {
        type            volFieldValue;
        libs            (fieldFunctionObjects);
        fields          (alpha.water);
        operation       volIntegrate;
        regionType      all;
        writeControl    timeStep;
        writeInterval   1;
        writeFields     false;
        log             false;
    }
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
}
PIMPLE
{
    momentumPredictor   no;
    nOuterCorrectors    1;
    nCorrectors         3;
    nNonOrthogonalCorrectors 0;
}
relaxationFactors { equations { ".*" 1; } }
"""

_SETFIELDSDICT = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {
        box (0 0 -1) (@A@ @H@ 1);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
);
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

_FIELD_U = """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    leftWall   { type noSlip; }
    rightWall  { type noSlip; }
    lowerWall  { type noSlip; }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
    defaultFaces { type empty; }
}
"""

# 2D case depth (m); matches the damBreak tutorial slab thickness.
_CASE_DEPTH = 0.0146


def build_dam_break_case(
    config: DamBreakConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the verified Martin & Moyce dam-break case directory.

    Writes the validated interFoam VOF case (docs/api/cfd/cases/dam_break/):
    a water column ``a x 2a`` (n^2=2) released in a plain ``8a x 4a`` tank,
    uniform mesh with column edges on cell faces, no-slip walls, entraining
    atmosphere, 2D ``empty`` front/back, laminar. A ``waterVolume``
    functionObject records the total water volume every time step for the
    mass-conservation gate. Run with ``setFields`` between meshing and the
    solve (``OpenFOAMRunConfig(run_set_fields=True)``).

    Args:
        config: Dam-break configuration; defaults to ``DamBreakConfig()``.
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory (``system/``, ``constant/``, ``0/``).
    """
    config = config or DamBreakConfig()
    case_dir = Path(parent_dir) / config.name
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    blockmesh = (
        _BLOCKMESHDICT
        .replace("@CPW@", str(config.cells_per_width))
        .replace("@LX@", fmt(config.tank_length))
        .replace("@LY@", fmt(config.tank_height))
        .replace("@DEPTH@", fmt(_CASE_DEPTH))
        .replace("@NX@", str(config.nx))
        .replace("@NY@", str(config.ny))
    )
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
    )
    setfields = (
        _SETFIELDSDICT
        .replace("@A@", fmt(config.column_width))
        .replace("@H@", fmt(config.column_height))
    )

    sysd, constd, zerod = case_dir / "system", case_dir / "constant", case_dir / "0"
    (sysd / "blockMeshDict").write_text(_hdr("dictionary", "blockMeshDict") + blockmesh)
    (sysd / "controlDict").write_text(_hdr("dictionary", "controlDict") + control)
    (sysd / "fvSchemes").write_text(_hdr("dictionary", "fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_hdr("dictionary", "fvSolution") + _FVSOLUTION)
    (sysd / "setFieldsDict").write_text(_hdr("dictionary", "setFieldsDict") + setfields)
    (constd / "transportProperties").write_text(
        _hdr("dictionary", "transportProperties") + _TRANSPORT
    )
    (constd / "g").write_text(
        _hdr("uniformDimensionedVectorField", "g") + _GRAVITY_DICT
    )
    (constd / "turbulenceProperties").write_text(
        _hdr("dictionary", "turbulenceProperties") + _TURBULENCE
    )
    (zerod / "alpha.water").write_text(_hdr("volScalarField", "alpha.water") + _FIELD_ALPHA)
    (zerod / "p_rgh").write_text(_hdr("volScalarField", "p_rgh") + _FIELD_P_RGH)
    (zerod / "U").write_text(_hdr("volVectorField", "U") + _FIELD_U)

    (case_dir / "provenance.json").write_text(
        json.dumps(_provenance(config), indent=2) + "\n"
    )
    return case_dir


# ---------------------------------------------------------------------------
# Solved-case extraction (pyvista, lazily imported)
# ---------------------------------------------------------------------------


def extract_front_history(
    case_dir: Path | str, config: DamBreakConfig | None = None
) -> Tuple[List[float], List[float]]:
    """Dimensionless surge-front history ``(T, Z)`` from a solved case.

    For each written time the front is the downstream face of the furthest
    wet cell (``alpha.water >= 0.5``) in the bottom cell row.

    Requires ``pyvista`` (imported lazily) and a solved case on a
    solver-capable host.
    """
    import numpy as np
    import pyvista as pv

    config = config or DamBreakConfig()
    case_dir = Path(case_dir)
    a, dx = config.column_width, config.cell_size
    (case_dir / "db.foam").write_text("")
    reader = pv.OpenFOAMReader(str(case_dir / "db.foam"))
    t_star, z_front = [], []
    for t in reader.time_values:
        reader.set_active_time_value(t)
        mesh = reader.read()["internalMesh"]
        centers = mesh.cell_centers()
        alpha = np.asarray(centers.cell_data["alpha.water"])
        pts = centers.points
        wet_bottom = (alpha >= 0.5) & (pts[:, 1] < dx)
        x_front = float(pts[wet_bottom, 0].max()) + dx / 2 if wet_bottom.any() else 0.0
        t_star.append(dimensionless_time(float(t), a))
        z_front.append(dimensionless_front(x_front, a))
    return t_star, z_front


def extract_mass_drift(case_dir: Path | str) -> float:
    """Max relative drift of the total water volume over the run.

    Reads the ``waterVolume`` functionObject output
    (``postProcessing/waterVolume/0/volFieldValue.dat``).
    """
    case_dir = Path(case_dir)
    dat = case_dir / "postProcessing" / "waterVolume" / "0" / "volFieldValue.dat"
    volumes = []
    for line in dat.read_text().splitlines():
        if line.startswith("#") or not line.strip():
            continue
        volumes.append(float(line.split()[1]))
    if len(volumes) < 2:
        raise RuntimeError(f"waterVolume functionObject produced no history: {dat}")
    v0 = volumes[0]
    return max(abs(v - v0) for v in volumes) / v0


def _provenance(config: DamBreakConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "dam_break_martin_moyce",
        "issue": "#1165",
        "reference": "Martin & Moyce (1952), n^2=2 column collapse",
        "citations": [
            "Martin, J.C. & Moyce, W.J. (1952), Phil. Trans. R. Soc. Lond. A "
            "244(882) 312-324, doi:10.1098/rsta.1952.0006",
            "Digitized data + gate-release shift: chaos-polymtl/lethe "
            "examples/multiphysics/dam-break/dam-break-2d.py",
            "Dictionaries derived from $FOAM_TUTORIALS/multiphase/interFoam/"
            "laminar/damBreak/damBreak (ESI v2312), obstacle removed",
        ],
        "column_width_m": config.column_width,
        "column_height_m": config.column_height,
        "tank_length_m": config.tank_length,
        "tank_height_m": config.tank_height,
        "mesh_cells": [config.nx, config.ny, 1],
        "end_time_s": config.end_time,
        "gate_release_time_shift": GATE_RELEASE_TIME_SHIFT,
        "tolerances": {
            "front_deviation_shifted": DAM_BREAK_SOLVE_TOLERANCE,
            "front_deviation_raw": DAM_BREAK_RAW_FRONT_TOLERANCE,
            "mass_drift": DAM_BREAK_MASS_TOLERANCE,
        },
        "known_answers": {
            "martin_moyce_T": list(MARTIN_MOYCE_N2_TIME),
            "martin_moyce_Z": list(MARTIN_MOYCE_N2_FRONT),
        },
    }
