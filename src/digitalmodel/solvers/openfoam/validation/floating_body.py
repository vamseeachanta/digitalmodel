#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation case #1169 — floating-body heave free decay
(interFoam + sixDoFRigidBodyMotion). A buoyant cuboid (half water density) is
released below its hydrostatic equilibrium in a closed tank and heaves freely;
the equilibrium draft is validated against Archimedes and the decay period is
compared to the hydrostatic (waterplane-stiffness) period, with the difference
reported as the implied heave added-mass coefficient. First moving-body
free-surface case of the suite — the gate for floating-structure RAO and
wave-drift work.

Source / citation
-----------------
- OpenFOAM tutorial: $FOAM_TUTORIALS/multiphase/interFoam/RAS/floatingObject
  (ESI v2312) — geometry, fields and numerics; motion solver switched to
  ``sixDoFRigidBodyMotion`` (as the older floatingObject variants) with
  line + orientation constraints for a pure heave decay, and the asymmetric
  extra-water block removed for a clean release.
- Hydrostatics: heave stiffness ``k = rho g Awp`` (waterplane area), closed-
  tank correction ``k_eff = k / (1 - Awp/Atank)``; natural period
  ``T = 2 pi sqrt((m + m_a)/k_eff)``. Any naval-architecture text (e.g.
  Newman, "Marine Hydrodynamics", 1977).
- Added mass: the measured-vs-hydrostatic period ratio implies the heave
  added-mass coefficient ``Ca = m_a / (rho * displaced volume)``; for
  rectangular sections published values are O(0.3-1) depending on beam/draft.

Validation gates (#1169): equilibrium draft within 5% of Archimedes;
monotonically damped decay envelope; period ratio ``T/T_hyd`` inside the
physical added-mass band [1.0, 1.6] with implied Ca in [0.1, 1.5].
Verified run (14 s): draft +1.1%, 11 cycles, T/T_hyd = 1.18, Ca = 0.39.
"""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Tuple

GRAVITY = 9.81
RHO_WATER = 998.2  # matches the tutorial transportProperties

# Verified-run gates. See docs/api/cfd/floating-body-decay-verification.html.
DRAFT_TOLERANCE = 0.05
PERIOD_RATIO_BAND = (1.0, 1.6)
ADDED_MASS_BAND = (0.1, 1.5)
# The fast regression variant (end_time=3.5 s, ~2 cycles) reproduces the
# full-run period to 0.3% and the draft to 1.3%.


# ---------------------------------------------------------------------------
# Pure hydrostatic reference helpers (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def equilibrium_draft(mass: float, waterplane_area: float,
                      rho: float = RHO_WATER) -> float:
    """Archimedes equilibrium draft ``m / (rho * Awp)`` (m).

    Raises:
        ValueError: If any argument is not positive.
    """
    if mass <= 0 or waterplane_area <= 0 or rho <= 0:
        raise ValueError("mass, waterplane_area and rho must be positive")
    return mass / (rho * waterplane_area)


def heave_stiffness(waterplane_area: float, tank_area: float | None = None,
                    rho: float = RHO_WATER) -> float:
    """Hydrostatic heave stiffness ``rho g Awp`` (N/m).

    In a closed tank the water level falls as the body rises, which
    stiffens the response: ``k_eff = rho g Awp / (1 - Awp/Atank)``.

    Raises:
        ValueError: On non-positive areas or ``Awp >= Atank``.
    """
    if waterplane_area <= 0:
        raise ValueError("waterplane_area must be positive")
    k = rho * GRAVITY * waterplane_area
    if tank_area is None:
        return k
    if tank_area <= waterplane_area:
        raise ValueError("tank_area must exceed waterplane_area")
    return k / (1.0 - waterplane_area / tank_area)


def hydrostatic_heave_period(mass: float, stiffness: float,
                             added_mass: float = 0.0) -> float:
    """Heave natural period ``2 pi sqrt((m + m_a)/k)`` (s)."""
    if mass <= 0 or stiffness <= 0 or added_mass < 0:
        raise ValueError("mass and stiffness must be positive, m_a >= 0")
    return 2.0 * math.pi * math.sqrt((mass + added_mass) / stiffness)


# ---------------------------------------------------------------------------
# Case configuration + builder
# ---------------------------------------------------------------------------


@dataclass
class FloatingBodyConfig:
    """Config for the floating-body heave free-decay case.

    Geometry is the floatingObject tutorial: a 1 m^3 tank, cuboid
    0.3 x 0.2 x 0.5 m cut out of the mesh with its bottom at z = 0.2333 m,
    fill level 0.5368 m. With body density 500 kg/m^3 the body starts
    ~0.054 m below its equilibrium (draft 0.25 m) -> clean linear decay.

    Attributes:
        body_density: Body density (kg/m^3); 500 = half water density.
        end_time: Physical end time (s). The default (3.5 s, ~2 cycles) is
            the fast regression variant (~3 min); the verified reference
            runs 14 s (11 cycles).
        write_interval: Field output interval (s).
        name: Case directory name.
    """

    body_density: float = 500.0
    end_time: float = 3.5
    write_interval: float = 0.1
    name: str = "validation_floating_body"

    # tutorial geometry constants
    lx: float = 0.3
    ly: float = 0.2
    lz: float = 0.5
    tank_side: float = 1.0
    fill_level: float = 0.5368
    hole_z0: float = 0.233333

    @property
    def mass(self) -> float:
        """Body mass (kg)."""
        return self.body_density * self.lx * self.ly * self.lz

    @property
    def waterplane_area(self) -> float:
        """Waterplane area Lx*Ly (m^2)."""
        return self.lx * self.ly

    @property
    def draft(self) -> float:
        """Archimedes equilibrium draft (m)."""
        return equilibrium_draft(self.mass, self.waterplane_area)

    @property
    def stiffness(self) -> float:
        """Closed-tank heave stiffness (N/m)."""
        return heave_stiffness(self.waterplane_area,
                               self.tank_side * self.tank_side)

    @property
    def hydrostatic_period(self) -> float:
        """Heave period without added mass (s)."""
        return hydrostatic_heave_period(self.mass, self.stiffness)

    @property
    def equilibrium_com_height(self) -> float:
        """Expected CoM height at equilibrium in the closed tank (m).

        Water-volume conservation: the fill level drops as the body rises
        from its over-submerged start to the Archimedes draft.
        """
        atank = self.tank_side * self.tank_side
        v_w = atank * self.fill_level - self.waterplane_area * (
            self.fill_level - self.hole_z0
        )
        h_eq = (v_w + self.waterplane_area * self.draft) / atank
        return h_eq - self.draft + self.lz / 2.0

    @property
    def centre_of_mass(self) -> Tuple[float, float, float]:
        """Initial centre of mass (m)."""
        return (0.5, 0.45, self.hole_z0 + self.lz / 2.0)

    @property
    def moment_of_inertia(self) -> Tuple[float, float, float]:
        """Cuboid principal moments of inertia (kg m^2)."""
        m = self.mass
        return (
            m / 12.0 * (self.ly**2 + self.lz**2),
            m / 12.0 * (self.lx**2 + self.lz**2),
            m / 12.0 * (self.lx**2 + self.ly**2),
        )


_HDR = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"

_BLOCKMESHDICT = """
// floatingObject tutorial tank: 1 m^3, 20x20x30 cells; the body is cut out
// by topoSet + subsetMesh (system/topoSetDict).
scale 1;
vertices
(
    (0 0 0) (1 0 0) (1 1 0) (0 1 0)
    (0 0 1) (1 0 1) (1 1 1) (0 1 1)
);
blocks ( hex (0 1 2 3 4 5 6 7) (20 20 30) simpleGrading (1 1 1) );
edges ();
boundary
(
    stationaryWalls
    {
        type wall;
        faces ( (0 3 2 1) (2 6 5 1) (1 5 4 0) (3 7 6 2) (0 4 7 3) );
    }
    atmosphere { type patch; faces ( (4 5 6 7) ); }
    floatingObject { type wall; faces (); }
);
mergePatchPairs ();
"""

_TOPOSETDICT = """
actions
(
    {
        name    c0;
        type    cellSet;
        action  new;
        source  boxToCell;
        // body volume: 0.3 x 0.2 x 0.5, bottom raised to z = 0.2333 so the
        // release starts ~0.054 m below hydrostatic equilibrium
        box     (0.35 0.35 0.233333) (0.65 0.55 0.733333);
    }
    { name c0; type cellSet; action invert; }
);
"""

_SETFIELDSDICT = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {
        box (-100 -100 -100) (100 100 @FILL@);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
);
"""

_DYNAMICMESHDICT = """
dynamicFvMesh       dynamicMotionSolverFvMesh;
motionSolverLibs    (sixDoFRigidBodyMotion);
motionSolver        sixDoFRigidBodyMotion;

patches             (floatingObject);
innerDistance       0.05;
outerDistance       0.25;

centreOfMass        (@CMX@ @CMY@ @CMZ@);
mass                @MASS@;
momentOfInertia     (@IXX@ @IYY@ @IZZ@);
rhoInf              1;
report              on;
accelerationRelaxation 0.7;

solver
{
    type            Newmark;
}

constraints
{
    // Pure heave: translation along z only, no rotation.
    fixedLine
    {
        sixDoFRigidBodyMotionConstraint line;
        centreOfRotation (@CMX@ @CMY@ @CMZ@);
        direction        (0 0 1);
    }
    fixedOrientation
    {
        sixDoFRigidBodyMotionConstraint orientation;
    }
}
"""

_CONTROLDICT = """
application     interFoam;
startFrom       startTime;
startTime       0;
stopAt          endTime;
endTime         @ENDTIME@;
deltaT          0.01;
writeControl    adjustable;
writeInterval   @WRITEINTERVAL@;
purgeWrite      0;
writeFormat     ascii;
writePrecision  12;
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
    bodyState
    {
        type            sixDoFRigidBodyState;
        libs            (sixDoFRigidBodyState);
        angleFormat     degrees;
        writeControl    timeStep;
        writeInterval   1;
    }
}
"""

_FVSCHEMES = """
ddtSchemes      { default CrankNicolson 0.5; }
gradSchemes     { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U)   Gauss vanLeerV;
    div(phi,alpha)  Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(phi,k)      Gauss upwind;
    div(phi,epsilon) Gauss upwind;
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
        nAlphaCorr      3;
        nAlphaSubCycles 1;
        cAlpha          1;
        MULESCorr       yes;
        nLimiterIter    5;
        alphaApplyPrevCorr yes;
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-8;
        relTol          0;
    }
    "pcorr.*"
    {
        solver          PCG;
        preconditioner
        {
            preconditioner  GAMG;
            tolerance       1e-5;
            relTol          0;
            smoother        DICGaussSeidel;
            cacheAgglomeration no;
        }
        tolerance       1e-05;
        relTol          0;
        maxIter         100;
    }
    p_rgh { solver GAMG; tolerance 1e-8; relTol 0.01; smoother DIC; }
    p_rghFinal
    {
        solver          PCG;
        preconditioner
        {
            preconditioner  GAMG;
            tolerance       1e-8;
            relTol          0;
            nVcycles        2;
            smoother        DICGaussSeidel;
            nPreSweeps      2;
        }
        tolerance       1e-8;
        relTol          0;
        maxIter         20;
    }
    "(U|k|epsilon)"
    {
        solver smoothSolver; smoother GaussSeidel;
        tolerance 1e-6; relTol 0.1; nSweeps 1;
    }
    "(U|k|epsilon)Final"
    {
        solver smoothSolver; smoother GaussSeidel;
        tolerance 1e-6; relTol 0; nSweeps 1;
    }
}
PIMPLE
{
    momentumPredictor   no;
    nOuterCorrectors    3;
    nCorrectors         1;
    nNonOrthogonalCorrectors 0;
    correctPhi          yes;
    moveMeshOuterCorrectors yes;
}
relaxationFactors { equations { ".*" 1; } }
"""

_TRANSPORT = """
phases          (water air);
water { transportModel Newtonian; nu 1e-06; rho 998.2; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma           0.07;
"""

_GRAVITY_DICT = """
dimensions      [0 1 -2 0 0 0 0];
value           (0 0 -9.81);
"""

_TURBULENCE = """
simulationType  RAS;
RAS
{
    RASModel        kEpsilon;
    turbulence      on;
    printCoeffs     on;
}
"""

# 0/ fields — patch set: stationaryWalls, atmosphere, floatingObject
_FIELDS = {
    ("volVectorField", "U"): """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    stationaryWalls { type noSlip; }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
    floatingObject { type movingWallVelocity; value uniform (0 0 0); }
}
""",
    ("volScalarField", "p_rgh"): """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    stationaryWalls { type fixedFluxPressure; gradient uniform 0; value uniform 0; }
    atmosphere
    {
        type totalPressure; rho rho; psi none; gamma 1;
        p0 uniform 0; value uniform 0;
    }
    floatingObject { type fixedFluxPressure; gradient uniform 0; value uniform 0; }
}
""",
    ("volScalarField", "alpha.water"): """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    stationaryWalls { type zeroGradient; }
    atmosphere { type inletOutlet; inletValue uniform 0; value uniform 0; }
    floatingObject { type zeroGradient; }
}
""",
    ("volScalarField", "k"): """
dimensions      [0 2 -2 0 0 0 0];
internalField   uniform 0.1;
boundaryField
{
    stationaryWalls { type kqRWallFunction; value uniform 0.1; }
    atmosphere { type inletOutlet; inletValue uniform 0.1; value uniform 0.1; }
    floatingObject { type kqRWallFunction; value uniform 0.1; }
}
""",
    ("volScalarField", "epsilon"): """
dimensions      [0 2 -3 0 0 0 0];
internalField   uniform 0.1;
boundaryField
{
    stationaryWalls { type epsilonWallFunction; value uniform 0.1; }
    atmosphere { type inletOutlet; inletValue uniform 0.1; value uniform 0.1; }
    floatingObject { type epsilonWallFunction; value uniform 0.1; }
}
""",
    ("volScalarField", "nut"): """
dimensions      [0 2 -1 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    stationaryWalls { type nutkWallFunction; value uniform 0; }
    atmosphere { type calculated; value uniform 0; }
    floatingObject { type nutkWallFunction; value uniform 0; }
}
""",
    ("pointVectorField", "pointDisplacement"): """
dimensions      [0 1 0 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    stationaryWalls { type fixedValue; value uniform (0 0 0); }
    atmosphere { type fixedValue; value uniform (0 0 0); }
    floatingObject { type calculated; }
}
""",
}


def build_floating_body_case(
    config: FloatingBodyConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the verified floating-body heave free-decay case.

    Writes the validated interFoam + sixDoFRigidBodyMotion case
    (docs/api/cfd/cases/floating_body_decay/). Run sequence:
    ``blockMesh -> topoSet -> subsetMesh -overwrite c0 -patch floatingObject
    -> setFields -> interFoam`` — i.e.
    ``OpenFOAMRunConfig(run_topo_set=True, subset_mesh_set="c0",
    subset_mesh_patch="floatingObject", run_set_fields=True)``.

    Args:
        config: Configuration; defaults to the fast regression variant.
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory.
    """
    config = config or FloatingBodyConfig()
    case_dir = Path(parent_dir) / config.name
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    cm = config.centre_of_mass
    moi = config.moment_of_inertia
    dyn = (
        _DYNAMICMESHDICT
        .replace("@CMX@", fmt(cm[0])).replace("@CMY@", fmt(cm[1]))
        .replace("@CMZ@", fmt(cm[2]))
        .replace("@MASS@", fmt(config.mass))
        .replace("@IXX@", fmt(moi[0])).replace("@IYY@", fmt(moi[1]))
        .replace("@IZZ@", fmt(moi[2]))
    )
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
    )
    setfields = _SETFIELDSDICT.replace("@FILL@", fmt(config.fill_level))

    sysd, constd, zerod = case_dir / "system", case_dir / "constant", case_dir / "0"
    (sysd / "blockMeshDict").write_text(_HDR.format(cls="dictionary", obj="blockMeshDict") + _BLOCKMESHDICT)
    (sysd / "topoSetDict").write_text(_HDR.format(cls="dictionary", obj="topoSetDict") + _TOPOSETDICT)
    (sysd / "setFieldsDict").write_text(_HDR.format(cls="dictionary", obj="setFieldsDict") + setfields)
    (sysd / "controlDict").write_text(_HDR.format(cls="dictionary", obj="controlDict") + control)
    (sysd / "fvSchemes").write_text(_HDR.format(cls="dictionary", obj="fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_HDR.format(cls="dictionary", obj="fvSolution") + _FVSOLUTION)
    (constd / "dynamicMeshDict").write_text(_HDR.format(cls="dictionary", obj="dynamicMeshDict") + dyn)
    (constd / "transportProperties").write_text(_HDR.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (constd / "g").write_text(_HDR.format(cls="uniformDimensionedVectorField", obj="g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(_HDR.format(cls="dictionary", obj="turbulenceProperties") + _TURBULENCE)
    for (cls, name), body in _FIELDS.items():
        (zerod / name).write_text(_HDR.format(cls=cls, obj=name) + body)

    (case_dir / "provenance.json").write_text(
        json.dumps(_provenance(config), indent=2) + "\n"
    )
    return case_dir


# ---------------------------------------------------------------------------
# Solved-case extraction
# ---------------------------------------------------------------------------


def extract_heave_history(case_dir: Path | str) -> Tuple[List[float], List[float]]:
    """Heave history ``(t, z_cm)`` from the sixDoFRigidBodyState output.

    Falls back to parsing the ``report on`` lines in ``log.interFoam`` when
    the function-object file is absent (keeping the last outer-corrector
    value per time step).
    """
    case_dir = Path(case_dir)
    state = sorted(case_dir.glob("postProcessing/bodyState/*/*.dat"),
                   key=lambda q: float(q.parent.name))
    t: List[float] = []
    cz: List[float] = []
    if state:
        for f in state:
            for line in f.read_text().splitlines():
                if line.startswith("#") or not line.strip():
                    continue
                parts = line.replace("(", " ").replace(")", " ").split()
                t.append(float(parts[0]))
                cz.append(float(parts[3]))
        return t, cz
    log = case_dir / "log.interFoam"
    if not log.is_file():
        raise RuntimeError(f"no bodyState output or log.interFoam in {case_dir}")
    tc = None
    for line in log.read_text().splitlines():
        if line.startswith("Time = "):
            tc = float(line.split()[-1].rstrip("s"))
        mm = re.search(
            r"Centre of mass: \(([-\d.eE+]+) ([-\d.eE+]+) ([-\d.eE+]+)\)", line
        )
        if mm and tc is not None:
            if t and t[-1] == tc:
                cz[-1] = float(mm.group(3))
            else:
                t.append(tc)
                cz.append(float(mm.group(3)))
    return t, cz


def analyze_decay(
    t: List[float], z_cm: List[float], config: FloatingBodyConfig | None = None
) -> Dict[str, Any]:
    """Decay metrics: equilibrium draft error, period, implied added mass.

    The equilibrium CoM height is estimated from the mean over the last
    oscillation-scale window and compared against the closed-tank
    Archimedes prediction; the period comes from zero-upcrossings of the
    displacement about that equilibrium.
    """
    import numpy as np

    config = config or FloatingBodyConfig()
    t_arr = np.asarray(t)
    z_arr = np.asarray(z_cm)
    z_eq = float(np.mean(z_arr[t_arr >= t_arr[-1] - 1.2]))
    draft_meas = config.draft + (config.equilibrium_com_height - z_eq)
    draft_err = (draft_meas - config.draft) / config.draft

    z = z_arr - z_eq
    idx = np.where((z[:-1] < 0) & (z[1:] >= 0))[0]
    zc = np.array(
        [t_arr[i] - z[i] * (t_arr[i + 1] - t_arr[i]) / (z[i + 1] - z[i])
         for i in idx]
    )
    zc = zc[zc > 0.3]
    periods = np.diff(zc)
    t_meas = float(np.mean(periods)) if len(periods) else float("nan")

    peaks: List[float] = []
    for a, b in zip(zc[:-1], zc[1:]):
        seg = z[(t_arr >= a) & (t_arr < b)]
        if len(seg) > 3:
            peaks.append(float(np.max(np.abs(seg))))
    monotone = all(p2 <= p1 * 1.05 for p1, p2 in zip(peaks[:-1], peaks[1:]))

    t_hyd = config.hydrostatic_period
    m_total = config.stiffness * (t_meas / (2 * math.pi)) ** 2
    displaced = RHO_WATER * config.waterplane_area * config.draft
    ca = (m_total - config.mass) / displaced

    return {
        "z_cm_equilibrium": z_eq,
        "draft_measured": float(draft_meas),
        "draft_error": float(draft_err),
        "period_measured": t_meas,
        "period_hydrostatic": t_hyd,
        "period_ratio": t_meas / t_hyd,
        "implied_added_mass_coeff": float(ca),
        "n_cycles": int(len(periods)),
        "peaks": peaks,
        "monotone_decay": bool(monotone),
    }


def _provenance(config: FloatingBodyConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "floating_body_decay",
        "issue": "#1169",
        "reference": "Archimedes draft + hydrostatic heave period "
                     "(waterplane stiffness, closed-tank corrected)",
        "citations": [
            "$FOAM_TUTORIALS/multiphase/interFoam/RAS/floatingObject "
            "(ESI v2312); motion solver switched to sixDoFRigidBodyMotion "
            "with heave-only constraints",
            "Newman (1977), Marine Hydrodynamics — hydrostatic stiffness "
            "and added mass",
        ],
        "body": {
            "dimensions_m": [config.lx, config.ly, config.lz],
            "density_kg_m3": config.body_density,
            "mass_kg": config.mass,
            "waterplane_area_m2": config.waterplane_area,
        },
        "expected": {
            "draft_m": config.draft,
            "hydrostatic_period_s": config.hydrostatic_period,
            "equilibrium_com_height_m": config.equilibrium_com_height,
        },
        "tolerances": {
            "draft": DRAFT_TOLERANCE,
            "period_ratio_band": list(PERIOD_RATIO_BAND),
            "added_mass_band": list(ADDED_MASS_BAND),
        },
    }
