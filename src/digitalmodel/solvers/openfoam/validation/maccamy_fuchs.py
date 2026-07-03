#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation #1171 — inline wave force on a surface-piercing
vertical circular cylinder, validated against the MacCamy-Fuchs (1954) linear
diffraction closed form (and its small-ka Morison inertia limit).

The first true wave-*loading* validation of the suite (vs the propagation-only
#1170 numerical wave tank): a rigid, bottom-mounted, surface-piercing cylinder
in a regular wave, with the inline horizontal force measured by an OpenFOAM
``forces`` function object and compared to the exact diffraction force.

Reference (frozen, pure-Python)
-------------------------------
MacCamy, R.C. & Fuchs, R.A. (1954), "Wave Forces on Piles: A Diffraction
Theory", U.S. Army Corps of Engineers, Beach Erosion Board, TM-69. The total
inline force amplitude on a cylinder of radius ``a`` in a linear wave of height
``H``, wavenumber ``k`` (from ``omega^2 = g k tanh(kh)``), depth ``h`` is

    F0 = (2 rho g H / k^2) tanh(k h) A(ka),   A(ka) = 1 / |H1'(ka)|
       = 1 / sqrt( J1'(ka)^2 + Y1'(ka)^2 )

with the force leading the incident wave crest / free-surface velocity by

    gamma = 90 deg - delta,   delta = atan2( J1'(ka), Y1'(ka) ).

``delta`` is the diffraction *departure* from the ideal inertial 90 deg lead
(delta -> 0 as ka -> 0); the physically gated quantity is ``gamma`` (the force
lead over the velocity), which -> 90 deg in the long-wave inertia limit. In that
limit the force collapses onto the Morison inertia term with Cm = 2 (added-mass
coefficient Ca = 1 for a circle); the effective inertia coefficient is
``Cm_eff = 4 A(ka) / (pi ka^2)`` -> 2 as ka -> 0.

This module ships the frozen analytical reference (``scipy.special`` derivative
Bessel functions) plus the case config; the OpenFOAM case builder, force
extraction and known-answer gate live alongside (see ``build_maccamy_fuchs_case``
and ``analyze_cylinder_loading``).
"""
from __future__ import annotations

import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Tuple

from .wave_tank import GRAVITY, dispersion_wavenumber

RHO_WATER = 1000.0

# Known-answer gate tolerances (issue #1171).
MACCAMY_FUCHS_FORCE_TOLERANCE = 0.15   # |F_cfd - F_mf| / F_mf, peak inline force
PHASE_TOLERANCE_DEG = 15.0             # force lead over velocity vs gamma
LOADING_PERIOD_TOLERANCE = 0.05        # force period vs wave period
FORCE_DRIFT_TOLERANCE = 0.10           # mean/amplitude drift over the window
MORISON_CM = 2.0                       # inertia coefficient, circle (Ca = 1)

# Diffraction-regime threshold: below this ka the Morison inertia limit is the
# right reference; above it, diffraction (MacCamy-Fuchs) matters. D/L > ~0.2.
DIFFRACTION_KA = 0.5


def _bessel_derivs(ka: float) -> Tuple[float, float]:
    """(J1'(ka), Y1'(ka)) — first-order derivative Bessel functions."""
    from scipy.special import jvp, yvp
    return float(jvp(1, ka)), float(yvp(1, ka))


def maccamy_fuchs_A(ka: float) -> float:
    """Diffraction amplitude factor A(ka) = 1 / sqrt(J1'(ka)^2 + Y1'(ka)^2)."""
    j1p, y1p = _bessel_derivs(ka)
    return 1.0 / math.hypot(j1p, y1p)


def maccamy_fuchs_force(wave_height: float, wave_period: float,
                        depth: float, diameter: float,
                        rho: float = RHO_WATER,
                        g: float = GRAVITY) -> Dict[str, float]:
    """Total inline force amplitude + phase on a surface-piercing cylinder.

    Args:
        wave_height: Regular wave height H (m).
        wave_period: Wave period T (s).
        depth: Still-water depth h (m).
        diameter: Cylinder diameter D (m).

    Returns:
        Dict with ``F_amplitude`` (N, peak inline force), ``phase_delta_deg``
        (diffraction departure from 90 deg), ``phase_lead_velocity_deg``
        (``gamma`` — the gated force lead over velocity), ``Cm_eff``, and the
        wave parameters ``k``, ``ka``, ``wavelength``, ``kd``, ``tanh_kd``.
    """
    k = dispersion_wavenumber(wave_period, depth)
    a = diameter / 2.0
    ka = k * a
    j1p, y1p = _bessel_derivs(ka)
    A = 1.0 / math.hypot(j1p, y1p)
    F0 = (2.0 * rho * g * wave_height / k ** 2) * math.tanh(k * depth) * A
    delta = math.degrees(math.atan2(j1p, y1p))
    return {
        "F_amplitude": F0,
        "phase_delta_deg": delta,
        "phase_lead_velocity_deg": 90.0 - delta,
        "Cm_eff": 4.0 * A / (math.pi * ka ** 2),
        "k": k,
        "ka": ka,
        "wavelength": 2.0 * math.pi / k,
        "kd": k * depth,
        "tanh_kd": math.tanh(k * depth),
    }


def morison_inertia_force(wave_height: float, wave_period: float,
                          depth: float, diameter: float,
                          Cm: float = MORISON_CM,
                          rho: float = RHO_WATER,
                          g: float = GRAVITY) -> float:
    """Small-ka Morison inertia force amplitude (the ka -> 0 anchor).

    F = Cm rho (pi a^2) g (H/2) tanh(k h) — the depth-integrated inertia force
    on a surface-piercing cylinder in a linear wave (the MacCamy-Fuchs force
    converges onto this as ka -> 0).
    """
    k = dispersion_wavenumber(wave_period, depth)
    a = diameter / 2.0
    return Cm * rho * math.pi * a ** 2 * g * (wave_height / 2.0) * math.tanh(k * depth)


# ---------------------------------------------------------------------------
# Case configuration
# ---------------------------------------------------------------------------


@dataclass
class CylinderWaveLoadingConfig:
    """Config for the vertical-cylinder wave-loading case (#1171).

    A surface-piercing rigid cylinder of diameter ``diameter`` at ``cylinder_x``
    in the verified #1170 StokesII wave, on a **3D half-domain** (a symmetry
    plane at y = 0 through the cylinder axis; the far y-wall a slip wall well
    clear of the cylinder). The waterline ``depth`` must land on a background
    cell face (VOF lesson from #1165/#1302), so ``nz`` divides ``depth`` into
    ``tank_height``.

    Headline diffraction point (defaults): D = 0.30 m, T = 0.80 s, H = 0.03 m,
    depth 0.40 m -> ka = 0.955, D/L = 0.30, MacCamy-Fuchs F0 = 14.74 N, which is
    0.72x the Morison inertia force (a 28% diffraction reduction — so a Morison
    prediction genuinely fails the 15% gate and the case discriminates
    diffraction physics).
    """

    wave_height: float = 0.03
    wave_period: float = 0.80
    depth: float = 0.40
    diameter: float = 0.30
    tank_length: float = 10.0
    tank_height: float = 0.60
    y_half_width: float = 1.6           # far slip wall > 5 D from the axis
    cylinder_x: float = 6.0             # established region, clear of inlet ramp
    nx: int = 400
    ny: int = 64
    nz: int = 60                        # depth 0.40 on a face: 0.40 / (0.60/60) = 40
    ramp_time: float = 2.0
    gauges_x: Tuple[float, ...] = (2.0, 3.6, 4.0, 4.4, 4.8, 5.2, 8.0)
    end_time: float = 12.0
    write_interval: float = 0.5
    steady_window: Tuple[float, float] = (8.0, 12.0)
    name: str = "validation_maccamy_fuchs"

    @property
    def wavenumber(self) -> float:
        return dispersion_wavenumber(self.wave_period, self.depth)

    @property
    def wavelength(self) -> float:
        return 2.0 * math.pi / self.wavenumber

    @property
    def radius(self) -> float:
        return self.diameter / 2.0

    @property
    def ka(self) -> float:
        return self.wavenumber * self.radius

    @property
    def diameter_over_wavelength(self) -> float:
        return self.diameter / self.wavelength

    @property
    def reference(self) -> Dict[str, float]:
        """Frozen MacCamy-Fuchs + Morison targets for this config."""
        mf = maccamy_fuchs_force(self.wave_height, self.wave_period,
                                 self.depth, self.diameter)
        mf["morison_force"] = morison_inertia_force(
            self.wave_height, self.wave_period, self.depth, self.diameter)
        mf["mf_over_morison"] = mf["F_amplitude"] / mf["morison_force"]
        return mf


def provenance() -> Dict[str, Any]:
    """Citation + frozen headline targets for the case (report/registry)."""
    cfg = CylinderWaveLoadingConfig()
    ref = cfg.reference
    return {
        "issue": "#1171",
        "reference": "MacCamy, R.C. & Fuchs, R.A. (1954), 'Wave Forces on Piles: "
                     "A Diffraction Theory', U.S. Army Corps of Engineers, Beach "
                     "Erosion Board, Technical Memorandum No. 69",
        "supporting": ["Sarpkaya & Isaacson (1981); Chakrabarti (1987); "
                       "Morison et al. (1950) inertia+drag; Faltinsen (1990)"],
        "wave": {"H": cfg.wave_height, "T": cfg.wave_period, "depth": cfg.depth},
        "cylinder": {"D": cfg.diameter, "x": cfg.cylinder_x},
        "regime": {"ka": ref["ka"], "D_over_L": cfg.diameter_over_wavelength,
                   "diffraction": ref["ka"] > DIFFRACTION_KA},
        "targets": {"F_maccamy_fuchs_N": ref["F_amplitude"],
                    "F_morison_N": ref["morison_force"],
                    "mf_over_morison": ref["mf_over_morison"],
                    "force_lead_velocity_deg": ref["phase_lead_velocity_deg"]},
    }


# ---------------------------------------------------------------------------
# OpenFOAM case builder — 3D half-domain interFoam, StokesII wave (#1170),
# rigid surface-piercing cylinder cut by snappyHexMesh (inline searchable-
# Cylinder, NO STL), inline force measured by a ``forces`` function object.
# ---------------------------------------------------------------------------

# Upstream gauge cluster used by the incident/reflected split at the cylinder.
# The 5 gauges at x = 3.6..5.2 (config.gauges_x) span ~1.6 wavelengths ahead of
# the cylinder (x = 6.0), so the Goda-Suzuki least-squares split cleanly
# separates the incident wave from the cylinder-scattered (reflected) wave.
MF_INCIDENT_SPLIT_X: Tuple[float, float] = (3.5, 5.3)

# Cylinder-axis z-extent margin: the searchableCylinder pokes just past the bed
# and the tank top so snappyHexMesh cuts a clean through-hole (a pile blocking
# flow over the full height), avoiding coincident surfaces on the ground/top
# patches. Deliberate robustness improvement over point1=(x 0 0)/point2=(x 0 H).
_CYL_Z_MARGIN = 0.1

_BLOCKMESHDICT_3D = """
// 3D HALF-domain wave tank (y in [0, @YW@]) for the #1171 cylinder-loading
// case: @LX@ m long x @LZ@ m tall, still water depth @DEPTH@ m. The plane
// y = 0 (through the cylinder axis) is an exact symmetryPlane, so only half
// the cylinder is meshed; the far y-wall is a slip wall > 5 D from the axis.
scale 1;
vertices
(
    (0    0    0)
    (@LX@ 0    0)
    (@LX@ @YW@ 0)
    (0    @YW@ 0)
    (0    0    @LZ@)
    (@LX@ 0    @LZ@)
    (@LX@ @YW@ @LZ@)
    (0    @YW@ @LZ@)
);
blocks ( hex (0 1 2 3 4 5 6 7) (@NX@ @NY@ @NZ@) simpleGrading (1 1 1) );
edges ();
boundary
(
    inlet    { type patch;         faces ( (0 4 7 3) ); }
    outlet   { type patch;         faces ( (1 2 6 5) ); }
    ground   { type wall;          faces ( (0 3 2 1) ); }
    top      { type patch;         faces ( (4 5 6 7) ); }
    symmetry { type symmetryPlane; faces ( (0 1 5 4) ); }
    farside  { type wall;          faces ( (3 7 6 2) ); }
);
mergePatchPairs ();
"""

# snappyHexMesh: cut the (half) cylinder out of the background hex mesh with an
# INLINE searchableCylinder (no STL). castellate + snap ON, addLayers OFF (the
# cylinder is a slip wall; the load is inertia/diffraction dominated). Refine
# the cylinder surface + a shell around it + a free-surface band z in
# [depth-H, depth+H]. locationInMesh is a fluid point well clear of the cylinder.
_SNAPPYHEXMESHDICT = """
castellatedMesh true;
snap            true;
addLayers       false;

geometry
{
    cylinder
    {
        type    searchableCylinder;
        point1  (@CX@ 0 @CZ0@);
        point2  (@CX@ 0 @CZ1@);
        radius  @R@;
    }
    freeSurfaceBand
    {
        type    searchableBox;
        min     (-1 -1 @FSZ0@);
        max     (@LXP@ @YP@ @FSZ1@);
    }
}

castellatedMeshControls
{
    maxLocalCells       2000000;
    maxGlobalCells      8000000;
    minRefinementCells  10;
    nCellsBetweenLevels 3;
    features            ();
    refinementSurfaces
    {
        cylinder
        {
            level     (1 2);
            patchInfo { type wall; }
        }
    }
    resolveFeatureAngle 30;
    refinementRegions
    {
        cylinder
        {
            mode    distance;
            levels  ((@REFDIST@ 1));
        }
        freeSurfaceBand
        {
            mode    inside;
            levels  ((1E15 1));
        }
    }
    locationInMesh          (@LOCX@ @LOCY@ @LOCZ@);
    allowFreeStandingZoneFaces true;
}

snapControls
{
    nSmoothPatch    3;
    tolerance       2.0;
    nSolveIter      30;
    nRelaxIter      5;
    nFeatureSnapIter 10;
    implicitFeatureSnap     false;
    explicitFeatureSnap     true;
    multiRegionFeatureSnap  false;
}

addLayersControls
{
    relativeSizes       true;
    layers              {}
    expansionRatio      1.0;
    finalLayerThickness 0.3;
    minThickness        0.1;
    nGrow               0;
    featureAngle        60;
    nRelaxIter          3;
    nSmoothSurfaceNormals 1;
    nSmoothNormals      3;
    nSmoothThickness    10;
    maxFaceThicknessRatio 0.5;
    maxThicknessToMedialRatio 0.3;
    minMedialAxisAngle  90;
    nBufferCellsNoExtrude 0;
    nLayerIter          50;
}

meshQualityControls
{
    maxNonOrtho         65;
    maxBoundarySkewness 20;
    maxInternalSkewness 4;
    maxConcave          80;
    minVol              1e-13;
    minTetQuality       1e-15;
    minArea             -1;
    minTwist            0.02;
    minDeterminant      0.001;
    minFaceWeight       0.05;
    minVolRatio         0.01;
    minTriangleTwist    -1;
    nSmoothScale        4;
    errorReduction      0.75;
}

mergeTolerance 1e-6;
"""

_SETFIELDSDICT_3D = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {
        box (-1 -1 -1) (@LXP@ @YP@ @DEPTH@);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
);
"""

# Forces function object appended to controlDict functions{} (alongside the
# waveGauges). ``rho rho`` uses the interFoam density FIELD (variable-density
# VOF), NOT a constant rhoInf. Half-domain => the measured Fx is doubled in
# extract_inline_force. Patch ``cylinder`` is created by snappyHexMesh.
_FORCES_FO = """    forces
    {
        type            forces;
        libs            (forces);
        patches         (cylinder);
        rho             rho;
        CofR            (@CX@ 0 0);
        writeControl    timeStep;
        writeInterval   1;
        log             false;
    }
"""

_FIELD_U_3D = """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    inlet    { type waveVelocity; value uniform (0 0 0); }
    outlet   { type waveVelocity; value uniform (0 0 0); }
    ground   { type fixedValue; value uniform (0 0 0); }
    top      { type pressureInletOutletVelocity; value uniform (0 0 0); }
    symmetry { type symmetryPlane; }
    farside  { type slip; }
    cylinder { type slip; }
}
"""

_FIELD_ALPHA_3D = """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    inlet    { type waveAlpha; value uniform 0; }
    outlet   { type zeroGradient; }
    ground   { type zeroGradient; }
    top      { type inletOutlet; inletValue uniform 0; value uniform 0; }
    symmetry { type symmetryPlane; }
    farside  { type zeroGradient; }
    cylinder { type zeroGradient; }
}
"""

_FIELD_P_RGH_3D = """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    inlet    { type fixedFluxPressure; value uniform 0; }
    outlet   { type fixedFluxPressure; value uniform 0; }
    ground   { type fixedFluxPressure; value uniform 0; }
    top      { type totalPressure; p0 uniform 0; }
    symmetry { type symmetryPlane; }
    farside  { type fixedFluxPressure; value uniform 0; }
    cylinder { type fixedFluxPressure; value uniform 0; }
}
"""


def build_maccamy_fuchs_case(
    config: CylinderWaveLoadingConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the surface-piercing-cylinder wave-loading case (#1171).

    Writes a complete, runnable interFoam case: a **3D half-domain** background
    ``blockMesh`` (real ``inlet``/``outlet``/``ground``/``top`` patches, an exact
    ``symmetryPlane`` at y = 0 through the cylinder axis, and a far slip
    ``farside`` wall), the verified #1170 StokesII wave generation (active
    absorption inlet + shallow-water absorption outlet), and a
    ``snappyHexMeshDict`` that carves the rigid cylinder out of the background
    mesh with an **inline searchableCylinder** (no STL). ``controlDict`` carries
    BOTH the ``interfaceHeight`` wave gauges and a ``forces`` function object on
    the cylinder patch.

    Reuses the #1170 numerics verbatim (``fvSchemes``/``fvSolution``/
    ``transportProperties``/``g``/``turbulenceProperties`` and the StokesII
    ``waveProperties`` template) so the incident wave matches the validated NWT.

    Run sequence (rigid + fixed cylinder — NO overset/topoSet/dynamicMesh)::

        blockMesh -> snappyHexMesh -overwrite -> setFields -> interFoam

    i.e. ``OpenFOAMRunConfig(solver="interFoam", run_snappy=True,
    run_set_fields=True, run_solver=True)``.

    Args:
        config: Case configuration; defaults to ``CylinderWaveLoadingConfig()``.
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory.
    """
    from .wave_tank import (
        _HDR, _CONTROLDICT, _FVSCHEMES, _FVSOLUTION, _WAVEPROPERTIES,
        _TRANSPORT, _GRAVITY_DICT, _TURBULENCE,
    )

    config = config or CylinderWaveLoadingConfig()
    case_dir = Path(parent_dir) / config.name
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    lxp = config.tank_length + 1.0
    yp = config.y_half_width + 1.0
    gauge_y = 0.4 * config.y_half_width          # mid-domain, clear of the wake
    gauges = " ".join(f"({fmt(x)} {fmt(gauge_y)} 0.0)" for x in config.gauges_x)

    # -- background mesh ---------------------------------------------------
    blockmesh = (
        _BLOCKMESHDICT_3D
        .replace("@LX@", fmt(config.tank_length))
        .replace("@LZ@", fmt(config.tank_height))
        .replace("@YW@", fmt(config.y_half_width))
        .replace("@DEPTH@", fmt(config.depth))
        .replace("@NX@", str(config.nx))
        .replace("@NY@", str(config.ny))
        .replace("@NZ@", str(config.nz))
    )

    # -- snappyHexMesh (inline searchableCylinder) -------------------------
    snappy = (
        _SNAPPYHEXMESHDICT
        .replace("@CX@", fmt(config.cylinder_x))
        .replace("@CZ0@", fmt(-_CYL_Z_MARGIN * config.tank_height))
        .replace("@CZ1@", fmt((1.0 + _CYL_Z_MARGIN) * config.tank_height))
        .replace("@R@", fmt(config.radius))
        .replace("@FSZ0@", fmt(config.depth - config.wave_height))
        .replace("@FSZ1@", fmt(config.depth + config.wave_height))
        .replace("@REFDIST@", fmt(1.0 * config.diameter))
        .replace("@LXP@", fmt(lxp))
        .replace("@YP@", fmt(yp))
        # a fluid point near the inlet, below the waterline, clear of the
        # cylinder and deliberately off cell faces
        .replace("@LOCX@", fmt(0.3137 * config.tank_length))
        .replace("@LOCY@", fmt(0.4123 * config.y_half_width))
        .replace("@LOCZ@", fmt(0.4271 * config.depth))
    )

    # -- controlDict: reuse #1170, inject the forces FO into functions{} ---
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
        .replace("@GAUGES@", gauges)
    ).rstrip()
    if not control.endswith("}"):
        raise RuntimeError("unexpected controlDict template layout (#1170)")
    forces_fo = _FORCES_FO.replace("@CX@", fmt(config.cylinder_x))
    control = control[:-1] + forces_fo + "}\n"

    # -- StokesII wave (reuse #1170, honour config.ramp_time) --------------
    waveprops = (
        _WAVEPROPERTIES
        .replace("@H@", fmt(config.wave_height))
        .replace("@T@", fmt(config.wave_period))
        .replace("rampTime        3.0;", f"rampTime        {fmt(config.ramp_time)};")
    )
    setfields = (
        _SETFIELDSDICT_3D
        .replace("@LXP@", fmt(lxp))
        .replace("@YP@", fmt(yp))
        .replace("@DEPTH@", fmt(config.depth))
    )

    sysd, constd, zerod = case_dir / "system", case_dir / "constant", case_dir / "0"
    (sysd / "blockMeshDict").write_text(_HDR.format(cls="dictionary", obj="blockMeshDict") + blockmesh)
    (sysd / "snappyHexMeshDict").write_text(_HDR.format(cls="dictionary", obj="snappyHexMeshDict") + snappy)
    (sysd / "controlDict").write_text(_HDR.format(cls="dictionary", obj="controlDict") + control)
    (sysd / "fvSchemes").write_text(_HDR.format(cls="dictionary", obj="fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_HDR.format(cls="dictionary", obj="fvSolution") + _FVSOLUTION)
    (sysd / "setFieldsDict").write_text(_HDR.format(cls="dictionary", obj="setFieldsDict") + setfields)
    (constd / "waveProperties").write_text(_HDR.format(cls="dictionary", obj="waveProperties") + waveprops)
    (constd / "transportProperties").write_text(_HDR.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (constd / "g").write_text(_HDR.format(cls="uniformDimensionedVectorField", obj="g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(_HDR.format(cls="dictionary", obj="turbulenceProperties") + _TURBULENCE)
    (zerod / "U").write_text(_HDR.format(cls="volVectorField", obj="U") + _FIELD_U_3D)
    (zerod / "alpha.water").write_text(_HDR.format(cls="volScalarField", obj="alpha.water") + _FIELD_ALPHA_3D)
    (zerod / "p_rgh").write_text(_HDR.format(cls="volScalarField", obj="p_rgh") + _FIELD_P_RGH_3D)

    (case_dir / "provenance.json").write_text(json.dumps(provenance(), indent=2) + "\n")
    return case_dir


# ---------------------------------------------------------------------------
# Solved-case extraction + known-answer gate
# ---------------------------------------------------------------------------


def extract_inline_force(
    case_dir: Path | str,
    config: CylinderWaveLoadingConfig | None = None,
) -> Tuple[List[float], List[float]]:
    """Inline horizontal force history ``(t, Fx_full)`` from the forces FO.

    Parses ``postProcessing/forces/*/force.dat`` (modern ESI, v2306+: columns
    are ``total | pressure | viscous`` and the FIRST triple is the TOTAL force)
    or the legacy ``forces.dat`` (foundation: parenthesised
    ``(pressure)(viscous)(porous)`` — no total column, so total = pressure +
    viscous). The parenthesis-presence test picks the layout, so neither is
    double-counted. The returned force is **doubled** to recover the full
    cylinder from the y = 0 symmetry half-domain.

    Returns:
        ``(t, Fx_full)`` sorted by time with duplicate time steps de-duped
        (last outer-corrector value kept).

    Raises:
        RuntimeError: If no forces output exists under the case.
    """
    import numpy as np

    case_dir = Path(case_dir)
    files = sorted(
        case_dir.glob("postProcessing/forces/*/force.dat"),
        key=lambda q: float(q.parent.name),
    )
    if not files:
        files = sorted(
            case_dir.glob("postProcessing/forces/*/forces.dat"),
            key=lambda q: float(q.parent.name),
        )
    if not files:
        raise RuntimeError(
            f"no forces output under {case_dir}/postProcessing/forces"
        )

    times: List[float] = []
    fxs: List[float] = []
    for f in files:
        for raw in f.read_text(errors="replace").splitlines():
            line = raw.strip()
            if not line or line.startswith("#"):
                continue
            legacy = "(" in line
            vals = [float(v) for v in line.replace("(", " ").replace(")", " ").split()]
            if not vals:
                continue
            if legacy:
                # (pressure_xyz)(viscous_xyz)(porous_xyz): x = vals[1] + vals[4]
                if len(vals) < 5:
                    continue
                fx = vals[1] + vals[4]
            else:
                # total_xyz | pressure_xyz | viscous_xyz: total x = vals[1]
                if len(vals) < 4:
                    continue
                fx = vals[1]
            times.append(vals[0])
            fxs.append(fx)

    t = np.asarray(times, dtype=float)
    fx = np.asarray(fxs, dtype=float) * 2.0        # half-domain -> full cylinder
    order = np.argsort(t, kind="stable")
    t, fx = t[order], fx[order]
    keep = np.ones(t.size, dtype=bool)
    if t.size > 1:
        keep[:-1] = t[1:] != t[:-1]                # keep the last of each time
    t, fx = t[keep], fx[keep]
    return t.tolist(), fx.tolist()


def analyze_cylinder_loading(
    case_dir: Path | str,
    config: CylinderWaveLoadingConfig | None = None,
) -> Dict[str, Any]:
    """Known-answer gate: measured inline force vs MacCamy-Fuchs (#1171).

    Normalises by the **measured** incident wave at the cylinder (the #1324
    lesson — robust to fetch dissipation): the incident amplitude ``a_i`` and
    the incident-wave phase at the cylinder are recovered from the upstream
    gauge cluster with the broken-gauge-safe :func:`incident_wave_split`, and
    the peak force is compared to :func:`maccamy_fuchs_force` evaluated at the
    measured height ``H = 2 a_i`` (not the nominal input height). The force
    amplitude + phase come from :func:`harmonic_amplitude`; the force lead over
    velocity is the force phase minus the incident-wave phase at the cylinder.

    Gate: ``|Fx - F_mf(H_measured)| / F_mf <= 0.15``, ``|phase_lead - gamma|
    <= 15 deg`` and the force period within 5% of the wave period.

    Writes ``results.json`` (the report reads it) and returns the same dict.
    """
    import numpy as np

    from .wave_excited_body import (
        harmonic_amplitude, incident_wave_split, response_period,
    )

    config = config or CylinderWaveLoadingConfig()
    case_dir = Path(case_dir)
    k = config.wavenumber

    # -- 1. measured incident wave at the cylinder -------------------------
    inc = incident_wave_split(case_dir, config, split_x=MF_INCIDENT_SPLIT_X)
    a_i = float(inc["incident_amplitude"])
    measured_H = 2.0 * a_i

    # incident-wave phase at the cylinder: rebuild the complex incident
    # coefficient from the clean gauges (Goda-Suzuki), then propagate to x_cyl.
    clean = [g for g in inc["gauges"]
             if g["healthy"]
             and MF_INCIDENT_SPLIT_X[0] <= g["x"] <= MF_INCIDENT_SPLIT_X[1]]
    xs = np.array([g["x"] for g in clean], dtype=float)
    amps = np.array([(g["H"] / 2.0) * np.exp(1j * g["phase"]) for g in clean],
                    dtype=complex)
    M = np.column_stack([np.exp(-1j * k * xs), np.exp(1j * k * xs)])
    coef, *_ = np.linalg.lstsq(M, amps, rcond=None)
    # elevation ~ cos(w t + psi); velocity is in phase with elevation
    incident_phase = float(np.angle(coef[0] * np.exp(-1j * k * config.cylinder_x)))

    # -- 2. reference force at the MEASURED height -------------------------
    ref = maccamy_fuchs_force(measured_H, config.wave_period,
                              config.depth, config.diameter)
    f_morison = morison_inertia_force(measured_H, config.wave_period,
                                      config.depth, config.diameter)
    gamma = ref["phase_lead_velocity_deg"]

    # -- 3. measured inline force (full cylinder) --------------------------
    t_list, fx_list = extract_inline_force(case_dir, config)
    t = np.asarray(t_list, dtype=float)
    fx = np.asarray(fx_list, dtype=float)
    w0, w1 = config.steady_window
    m = (t >= w0) & (t <= w1)
    tw, fxw = t[m], fx[m]
    mean_fx, amp_fx, phase_fx = harmonic_amplitude(tw, fxw, config.wave_period)
    period_fx = response_period(tw, fxw)

    # -- 4. gate -----------------------------------------------------------
    lead = math.degrees(phase_fx - incident_phase)
    lead = (lead + 180.0) % 360.0 - 180.0                 # wrap to (-180, 180]
    phase_lead_error = (lead - gamma + 180.0) % 360.0 - 180.0
    force_rel_error = abs(amp_fx - ref["F_amplitude"]) / ref["F_amplitude"]
    within_force = force_rel_error <= MACCAMY_FUCHS_FORCE_TOLERANCE
    within_phase = abs(phase_lead_error) <= PHASE_TOLERANCE_DEG
    if math.isfinite(period_fx):
        period_error = abs(period_fx - config.wave_period) / config.wave_period
    else:
        period_error = float("nan")
    within_period = math.isfinite(period_error) and period_error <= LOADING_PERIOD_TOLERANCE

    result: Dict[str, Any] = {
        "issue": "#1171",
        "config": {
            "wave": {"H": config.wave_height, "T": config.wave_period,
                     "depth": config.depth},
            "cylinder": {"D": config.diameter, "x": config.cylinder_x},
            "regime": {"ka": config.ka,
                       "D_over_L": config.diameter_over_wavelength},
        },
        "incident": {
            "measured_H": measured_H,
            "a_i": a_i,
            "reflection_kr": float(inc["reflection_kr"]),
        },
        "force": {
            "amplitude_N": float(amp_fx),
            "phase_lead_velocity_deg": float(lead),
            "period_s": float(period_fx),
            "mean_N": float(mean_fx),
        },
        "reference": {
            "F_maccamy_fuchs_at_measured_H_N": ref["F_amplitude"],
            "F_morison_N": f_morison,
            "force_lead_velocity_deg": gamma,
        },
        "gate": {
            "force_rel_error": float(force_rel_error),
            "within_force_gate": bool(within_force),
            "phase_lead_error_deg": float(phase_lead_error),
            "within_phase_gate": bool(within_phase),
            "period_error": float(period_error),
            "within_period_gate": bool(within_period),
        },
        "force_history": {"t": list(t_list), "fx": list(fx_list)},
        # flat convenience aliases (see tests/.../test_maccamy_fuchs.py)
        "within_force_gate": bool(within_force),
        "force_amplitude": float(amp_fx),
        "force_reference": ref["F_amplitude"],
        "phase_lead_error_deg": float(phase_lead_error),
    }

    (case_dir / "results.json").write_text(json.dumps(result, indent=2) + "\n")
    return result
