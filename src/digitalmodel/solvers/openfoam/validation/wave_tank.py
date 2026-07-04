#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation case #1170 — 2D regular-wave numerical wave
tank (NWT): StokesII generation at the inlet with active absorption at both
ends, validated against linear/Stokes wave theory (dispersion), the input
wave height, and a multi-gauge reflection estimate. This is the prerequisite
for every incident-wave loading workflow. Pure wave-theory helpers live here
with no OpenFOAM dependency; the case builder derives from the shipped
stokesII tutorial.

Source / citation
-----------------
- OpenFOAM tutorial: $FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII
  (ESI v2312) — waveVelocity/waveAlpha StokesII inlet, shallowWaterAbsorption
  outlet, activeAbsorption at the inlet.
- Wave theory: linear dispersion ``omega^2 = g k tanh(k d)`` (any standard
  text, e.g. Dean & Dalrymple 1991); OpenFOAM regular-wave model docs
  https://www.openfoam.com/documentation/guides/latest/doc/guide-wave-regular-stokesi.html
- Wave-quality practice: Larsen, Fuhrman & Roenby (2019), "Performance of
  interFoam on the simulation of progressive waves" (arXiv:1804.01158) —
  amplitude decay and Courant-number guidance; near-field (< ~1 wavelength
  from the wavemaker) excluded from quality metrics.
- Reflection estimate: least-squares incident/reflected split over a gauge
  array spanning one wavelength (Goda & Suzuki 1976 style, generalised to
  N gauges).

Validation gates (#1170): established-region wave height within 5% of the
input; height decay < 5% over ~1.7 wavelengths; wavenumber within 5% of the
dispersion relation; reflection coefficient < 10%.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Sequence, Tuple

GRAVITY = 9.81

# Verified-run gates (reference 500x55 mesh, 30 s: H max err 4.7%, decay 4.6%,
# k +0.2%, Kr 0.010). See docs/api/cfd/wave-tank-verification.html.
WAVE_HEIGHT_TOLERANCE = 0.05
WAVE_DECAY_TOLERANCE = 0.05
DISPERSION_TOLERANCE = 0.05
REFLECTION_TOLERANCE = 0.10

# Regression (fast) variant gates — 250x28 mesh, 16 s, gauges x in [8, 14]
# (verified: H max err 2.8%, k +0.7%, Kr 0.004). Headroom ~2-3x.
FAST_WAVE_HEIGHT_TOLERANCE = 0.08
FAST_DISPERSION_TOLERANCE = 0.05
FAST_REFLECTION_TOLERANCE = 0.10


# ---------------------------------------------------------------------------
# Pure wave-theory helpers (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def dispersion_wavenumber(period: float, depth: float) -> float:
    """Wavenumber k from the linear dispersion relation.

    Solves ``omega^2 = g k tanh(k d)`` by fixed-point iteration.

    Args:
        period: Wave period T (s, > 0).
        depth: Water depth d (m, > 0).

    Returns:
        Wavenumber k (rad/m).

    Raises:
        ValueError: If period or depth is not positive.
    """
    if period <= 0.0:
        raise ValueError(f"period must be positive, got {period}")
    if depth <= 0.0:
        raise ValueError(f"depth must be positive, got {depth}")
    om = 2.0 * math.pi / period
    # Newton on f(k) = g k tanh(kd) - omega^2 (converges from shallow to
    # deep water; the naive fixed-point iteration diverges for kd << 1).
    x = om * om * depth / GRAVITY
    k = (om / math.sqrt(GRAVITY * depth) if x < 1.0
         else om * om / GRAVITY)
    for _ in range(100):
        th = math.tanh(k * depth)
        f = GRAVITY * k * th - om * om
        fp = GRAVITY * th + GRAVITY * k * depth * (1.0 - th * th)
        step = f / fp
        k -= step
        if abs(step) < 1e-15 * max(k, 1.0):
            break
    return k


def wavelength(period: float, depth: float) -> float:
    """Wavelength L = 2*pi/k from the dispersion relation (m)."""
    return 2.0 * math.pi / dispersion_wavenumber(period, depth)


def celerity(period: float, depth: float) -> float:
    """Phase speed c = omega/k from the dispersion relation (m/s)."""
    om = 2.0 * math.pi / period
    return om / dispersion_wavenumber(period, depth)


def reflection_coefficient(
    gauge_x: Sequence[float],
    complex_amplitudes: Sequence[complex],
    k: float,
) -> Tuple[float, float, float]:
    """Least-squares incident/reflected split over a gauge array.

    Models the complex amplitude at gauge j as
    ``A_j = a_i exp(-i k x_j) + a_r exp(+i k x_j)`` and solves for
    ``a_i``, ``a_r`` in the least-squares sense (Goda-Suzuki style,
    generalised to N gauges).

    Returns:
        ``(Kr, |a_i|, |a_r|)`` with ``Kr = |a_r|/|a_i|``.

    Raises:
        ValueError: On fewer than 3 gauges (under-determined / degenerate).
    """
    import numpy as np

    if len(gauge_x) < 3 or len(gauge_x) != len(complex_amplitudes):
        raise ValueError("need >= 3 gauges with matching amplitudes")
    x = np.asarray(gauge_x, dtype=float)
    A = np.asarray(complex_amplitudes, dtype=complex)
    M = np.column_stack([np.exp(-1j * k * x), np.exp(1j * k * x)])
    coef, *_ = np.linalg.lstsq(M, A, rcond=None)
    a_i, a_r = abs(coef[0]), abs(coef[1])
    return float(a_r / a_i), float(a_i), float(a_r)


# ---------------------------------------------------------------------------
# Case configuration + builder
# ---------------------------------------------------------------------------


@dataclass
class WaveTankConfig:
    """Config for the regular-wave numerical wave tank.

    Geometry follows the stokesII tutorial: a 2D tank in the x-z plane
    (one cell thick in y), StokesII inlet with active absorption, shallow-
    water absorption outlet. The default mesh/duration is the *fast*
    regression variant (~2.5 min solve); the verified reference report uses
    ``nx=500, nz=55, end_time=30``.

    Attributes:
        wave_height: Input regular wave height H (m).
        wave_period: Input wave period T (s).
        depth: Still-water depth d (m).
        tank_length: Tank length (m) — default 20 m (~3.5 wavelengths).
        tank_height: Tank height (m).
        nx, nz: Mesh cells along / vertical.
        end_time: Physical end time (s).
        gauges_x: Wave-gauge x positions (m); the 7 gauges spanning
            x = 8..12.8 cover ~0.8 wavelengths for the reflection split.
        name: Case directory name.
    """

    wave_height: float = 0.05
    wave_period: float = 3.0
    depth: float = 0.4
    tank_length: float = 20.0
    tank_height: float = 0.55
    nx: int = 250
    nz: int = 28
    end_time: float = 16.0
    write_interval: float = 0.25
    gauges_x: Tuple[float, ...] = (
        2.0, 8.0, 8.8, 9.6, 10.4, 11.2, 12.0, 12.8, 14.0, 18.0,
    )
    name: str = "validation_wave_tank"

    @property
    def wavenumber(self) -> float:
        """Dispersion wavenumber for the configured wave (rad/m)."""
        return dispersion_wavenumber(self.wave_period, self.depth)

    @property
    def wavelength(self) -> float:
        """Wavelength (m)."""
        return 2.0 * math.pi / self.wavenumber

    @property
    def celerity(self) -> float:
        """Phase speed (m/s)."""
        return celerity(self.wave_period, self.depth)


_HDR = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"

_BLOCKMESHDICT = """
// 2D numerical wave tank in the x-z plane (y = one 'empty' cell), from the
// stokesII tutorial: @LX@ m long x @LZ@ m tall, still water depth @DEPTH@ m.
scale 1;
vertices
(
    (0     0     0)
    (@LX@  0     0)
    (@LX@  0.04  0)
    (0     0.04  0)
    (0     0     @LZ@)
    (@LX@  0     @LZ@)
    (@LX@  0.04  @LZ@)
    (0     0.04  @LZ@)
);
blocks ( hex (0 1 2 3 4 5 6 7) (@NX@ 1 @NZ@) simpleGrading (1 1 1) );
edges ();
boundary
(
    inlet  { type patch; faces ( (0 4 7 3) ); }
    outlet { type patch; faces ( (1 5 6 2) ); }
    ground { type wall;  faces ( (0 1 2 3) ); }
    top    { type patch; faces ( (4 5 6 7) ); }
    sides  { type empty; faces ( (0 1 5 4) (3 2 6 7) ); }
);
mergePatchPairs ();
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
writePrecision  6;
writeCompression off;
timeFormat      general;
timePrecision   6;
runTimeModifiable yes;
adjustTimeStep  yes;
maxCo           0.65;
maxAlphaCo      0.65;
maxDeltaT       0.05;
functions
{
    // Wave gauges: interface height above the bed. The 7 gauges around
    // mid-tank (x = 8..12.8) feed the reflection split.
    waveGauges
    {
        type            interfaceHeight;
        libs            (fieldFunctionObjects);
        alpha           alpha.water;
        writeControl    timeStep;
        writeInterval   1;
        locations       ( @GAUGES@ );
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
laplacianSchemes { default Gauss linear orthogonal; }
interpolationSchemes { default linear; }
snGradSchemes   { default orthogonal; }
"""

_FVSOLUTION = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr      1;
        nAlphaSubCycles 3;
        cAlpha          1;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-6; relTol 0; }
    p_rgh     { solver PCG; preconditioner DIC; tolerance 1e-6; relTol 0.1; }
    p_rghFinal { solver GAMG; smoother DIC; tolerance 1e-7; relTol 0; }
    U      { solver PBiCG; preconditioner DILU; tolerance 1e-6; relTol 0.1; }
    UFinal { solver PBiCG; preconditioner DILU; tolerance 1e-6; relTol 0; }
}
PIMPLE
{
    momentumPredictor no;
    nCorrectors     2;
    nNonOrthogonalCorrectors 0;
}
"""

_WAVEPROPERTIES = """
inlet
{
    alpha           alpha.water;
    waveModel       StokesII;
    nPaddle         1;
    waveHeight      @H@;
    waveAngle       0.0;
    rampTime        3.0;
    activeAbsorption yes;
    wavePeriod      @T@;
}
outlet
{
    alpha           alpha.water;
    waveModel       shallowWaterAbsorption;
    nPaddle         1;
}
"""

_SETFIELDSDICT = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {
        box (-1 -1 -1) (@LXP@ 1 @DEPTH@);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
);
"""

_TRANSPORT = """
phases          (water air);
water { transportModel Newtonian; nu 1e-06; rho 1000; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma           0.07;
"""

_GRAVITY_DICT = """
dimensions      [0 1 -2 0 0 0 0];
value           (0 0 -9.81);
"""

_TURBULENCE = """
simulationType  laminar;
"""

_FIELD_U = """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    inlet  { type waveVelocity; value uniform (0 0 0); }
    outlet { type waveVelocity; value uniform (0 0 0); }
    ground { type fixedValue; value uniform (0 0 0); }
    top    { type pressureInletOutletVelocity; value uniform (0 0 0); }
    sides  { type empty; }
}
"""

_FIELD_ALPHA = """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    inlet  { type waveAlpha; value uniform 0; }
    outlet { type zeroGradient; }
    ground { type zeroGradient; }
    top    { type inletOutlet; inletValue uniform 0; value uniform 0; }
    sides  { type empty; }
}
"""

_FIELD_P_RGH = """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    inlet  { type fixedFluxPressure; value uniform 0; }
    outlet { type fixedFluxPressure; value uniform 0; }
    ground { type fixedFluxPressure; value uniform 0; }
    top    { type totalPressure; p0 uniform 0; }
    sides  { type empty; }
}
"""


def build_wave_tank_case(
    config: WaveTankConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the verified regular-wave NWT case directory.

    Writes the validated interFoam wave case (docs/api/cfd/cases/wave_tank/):
    StokesII generation with active absorption at the inlet, shallow-water
    absorption at the outlet, an ``interfaceHeight`` wave-gauge array, 2D
    ``empty`` sides, laminar. Run with ``setFields`` between meshing and the
    solve (``OpenFOAMRunConfig(run_set_fields=True)``).

    Args:
        config: Wave-tank configuration; defaults to the fast regression
            variant (``WaveTankConfig()``).
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory.
    """
    config = config or WaveTankConfig()
    case_dir = Path(parent_dir) / config.name
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    gauges = " ".join(f"({fmt(x)} 0.02 0.0)" for x in config.gauges_x)
    blockmesh = (
        _BLOCKMESHDICT
        .replace("@LX@", fmt(config.tank_length))
        .replace("@LZ@", fmt(config.tank_height))
        .replace("@DEPTH@", fmt(config.depth))
        .replace("@NX@", str(config.nx))
        .replace("@NZ@", str(config.nz))
    )
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
        .replace("@GAUGES@", gauges)
    )
    waveprops = (
        _WAVEPROPERTIES
        .replace("@H@", fmt(config.wave_height))
        .replace("@T@", fmt(config.wave_period))
    )
    setfields = (
        _SETFIELDSDICT
        .replace("@LXP@", fmt(config.tank_length + 1))
        .replace("@DEPTH@", fmt(config.depth))
    )

    sysd, constd, zerod = case_dir / "system", case_dir / "constant", case_dir / "0"
    (sysd / "blockMeshDict").write_text(_HDR.format(cls="dictionary", obj="blockMeshDict") + blockmesh)
    (sysd / "controlDict").write_text(_HDR.format(cls="dictionary", obj="controlDict") + control)
    (sysd / "fvSchemes").write_text(_HDR.format(cls="dictionary", obj="fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_HDR.format(cls="dictionary", obj="fvSolution") + _FVSOLUTION)
    (sysd / "setFieldsDict").write_text(_HDR.format(cls="dictionary", obj="setFieldsDict") + setfields)
    (constd / "waveProperties").write_text(_HDR.format(cls="dictionary", obj="waveProperties") + waveprops)
    (constd / "transportProperties").write_text(_HDR.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (constd / "g").write_text(_HDR.format(cls="uniformDimensionedVectorField", obj="g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(_HDR.format(cls="dictionary", obj="turbulenceProperties") + _TURBULENCE)
    (zerod / "U").write_text(_HDR.format(cls="volVectorField", obj="U") + _FIELD_U)
    (zerod / "alpha.water").write_text(_HDR.format(cls="volScalarField", obj="alpha.water") + _FIELD_ALPHA)
    (zerod / "p_rgh").write_text(_HDR.format(cls="volScalarField", obj="p_rgh") + _FIELD_P_RGH)

    (case_dir / "provenance.json").write_text(
        json.dumps(_provenance(config), indent=2) + "\n"
    )
    return case_dir


# ---------------------------------------------------------------------------
# Solved-case extraction
# ---------------------------------------------------------------------------


def extract_wave_quality(
    case_dir: Path | str,
    config: WaveTankConfig | None = None,
    window: Tuple[float, float] = (10.0, 16.0),
    established_x: Tuple[float, float] = (8.0, 14.0),
) -> Dict[str, Any]:
    """Wave-quality metrics from the solved gauges.

    Reads the ``waveGauges`` interfaceHeight output (all time directories,
    concatenated), extracts per-gauge height/complex amplitude over
    ``window``, and returns height errors (established region), the
    phase-fit wavenumber vs dispersion, and the multi-gauge reflection
    coefficient.
    """
    import numpy as np

    config = config or WaveTankConfig()
    case_dir = Path(case_dir)
    om = 2.0 * math.pi / config.wave_period
    k_th = config.wavenumber

    files = sorted(
        case_dir.glob("postProcessing/waveGauges/*/height.dat"),
        key=lambda q: float(q.parent.name),
    )
    if not files:
        raise RuntimeError(f"no waveGauges output under {case_dir}")
    raw = np.vstack([np.loadtxt(f, comments="#") for f in files])
    t = raw[:, 0]
    ncol = raw.shape[1] - 1
    ng = len(config.gauges_x)
    if ncol == ng:
        eta = raw[:, 1:] - config.depth
    elif ncol == 2 * ng:
        eta = raw[:, 1::2] - config.depth
    else:
        raise RuntimeError(f"unexpected gauge column count {ncol} for {ng} gauges")

    m = (t >= window[0]) & (t <= window[1])
    t, eta = t[m], eta[m]
    eta = eta - eta.mean(axis=0)

    gauges: List[Dict[str, Any]] = []
    for j, xg in enumerate(config.gauges_x):
        e = eta[:, j]
        idx = np.where((e[:-1] < 0) & (e[1:] >= 0))[0]
        zc = np.array(
            [t[i] - e[i] * (t[i + 1] - t[i]) / (e[i + 1] - e[i]) for i in idx]
        )
        heights = []
        for a, b in zip(zc[:-1], zc[1:]):
            seg = e[(t >= a) & (t < b)]
            if len(seg) > 4:
                heights.append(float(seg.max() - seg.min()))
        amp = np.trapz(e * np.exp(-1j * om * t), t) * 2 / (t[-1] - t[0])
        gauges.append({
            "x": xg,
            "H": float(np.mean(heights)) if heights else float("nan"),
            "amp": complex(amp),
            "phase": float(np.angle(amp)),
        })

    est = [g for g in gauges
           if established_x[0] <= g["x"] <= established_x[1]
           and g["H"] == g["H"]]
    h_err_max = max(abs(g["H"] - config.wave_height) / config.wave_height
                    for g in est)

    # dispersion from the phase slope over the one-wavelength array
    arr = [g for g in gauges if 7.9 <= g["x"] <= 12.9]
    xs = np.array([g["x"] for g in arr])
    phs = np.unwrap([g["phase"] for g in arr])
    k_meas = float(-np.polyfit(xs, phs, 1)[0])

    kr, a_i, a_r = reflection_coefficient(
        xs, [g["amp"] for g in arr], k_th
    )

    return {
        "gauges": [{k: v for k, v in g.items() if k != "amp"} for g in gauges],
        "h_err_established_max": float(h_err_max),
        "k_measured": k_meas,
        "k_error": float((k_meas - k_th) / k_th),
        "reflection_kr": kr,
        "incident_amplitude": a_i,
        "window": list(window),
    }


def _provenance(config: WaveTankConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "wave_tank_stokes2",
        "issue": "#1170",
        "reference": "linear dispersion omega^2 = g k tanh(k d); input StokesII wave",
        "citations": [
            "$FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII (ESI v2312)",
            "Larsen, Fuhrman & Roenby (2019), arXiv:1804.01158 — interFoam "
            "progressive-wave performance",
            "Goda & Suzuki (1976) — incident/reflected wave resolution from "
            "gauge arrays",
        ],
        "wave": {
            "H_m": config.wave_height, "T_s": config.wave_period,
            "depth_m": config.depth,
            "wavelength_m": config.wavelength,
            "celerity_m_s": config.celerity,
            "wavenumber_rad_m": config.wavenumber,
        },
        "tank": {"length_m": config.tank_length, "height_m": config.tank_height,
                 "mesh": [config.nx, 1, config.nz]},
        "gauges_x_m": list(config.gauges_x),
        "tolerances": {
            "wave_height": WAVE_HEIGHT_TOLERANCE,
            "decay": WAVE_DECAY_TOLERANCE,
            "dispersion": DISPERSION_TOLERANCE,
            "reflection": REFLECTION_TOLERANCE,
        },
    }
