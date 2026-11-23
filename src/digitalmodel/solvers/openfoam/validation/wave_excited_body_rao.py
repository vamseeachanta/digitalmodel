#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation #1324 — heave-RAO curve of the wave-excited
floating body across resonance. Sweeps the verified #1302 overset case
(overInterDyMFoam, 2D overset, 0.2 m half-submerged square) over wave
periods spanning the long-wave limit (RAO -> 1) through heave resonance
(RAO peaks ~2.0 near T ~ 0.9 s) into the short-wave roll-off, and spot-checks
each CFD point against a frozen potential-flow diffraction reference — the
same physics the OrcaWave/AQWA pipeline implements, computed in 2D-consistent
form for the exact section (see ``generate_rao_reference.py``).

This turns the single verified long-wave point of #1302 into a frequency
response: the strongest known-answer validation tier of #1161.

Design (reuses the verified #1302 machinery unchanged):
- Same tank (20 m, depth 0.4 m) and body (x = 13 m, nz = 49 so the 0.4 m
  waterline stays on a cell face). Only the wave period, the background
  ``nx`` (kept at >= ~18 cells per wavelength), the wave-gauge array
  (spacing ~0.2 lambda so the Goda-Suzuki split does not alias on short
  waves) and the solve duration / analysis window (sized from the group
  velocity so short-wave cases do not over-run) vary per point.
- Incident amplitude from the verified broken-gauge-safe upstream split
  (:func:`incident_wave_split`); heave from the sixDoF ``report on`` log
  lines (:func:`extract_heave_from_log`); amplitude from the harmonic fit.
- The sweep gate compares each CFD RAO to the *reference RAO at that period*
  (not the fixed long-wave 1.0 of the single case). Near resonance the CFD
  peak is expected to sit BELOW the inviscid reference — potential flow has
  no viscous / vortex-shedding damping — so that gap is reported as a
  physics finding, with a wider band around the peak.

Source / citation
-----------------
- Potential-flow reference: capytaine linear BEM (Froude-Krylov + diffraction),
  frozen in ``rao_reference_capytaine.csv`` with provenance. The long-wave
  value (RAO 1.014 at T = 3 s) independently reproduces the #1302 CFD point.
- Everything else inherited from #1302 / #1170 / #1169 (see
  :mod:`wave_excited_body`).
"""
from __future__ import annotations

import math
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Any, Dict, List, Tuple

from .wave_excited_body import (
    RHO_WATER,
    WaveExcitedBodyConfig,
    analyze_excited_heave,
    extract_heave_from_log,
    incident_wave_split,
)
from .wave_tank import GRAVITY, dispersion_wavenumber

# Sweep gate bands: CFD RAO vs the frozen potential-flow reference RAO at the
# same period. Wider near resonance, where viscous damping (absent from the
# inviscid reference) legitimately caps the CFD peak below the reference and
# a small period shift moves a steep curve a lot.
RAO_SWEEP_BAND = 0.20          # |CFD - ref| / ref off-resonance
RAO_SWEEP_BAND_RESONANCE = 0.35  # within RESONANCE_GUARD of the ref peak period
RESONANCE_GUARD = 0.12         # +/- fraction of the peak period = "near resonance"

# Target cells per wavelength for the background wave field (horizontal).
CELLS_PER_WAVELENGTH = 18
# The CFD periods actually swept (T = 3 s reused from the verified #1302 run).
SWEEP_PERIODS: Tuple[float, ...] = (1.5, 1.2, 1.0, 0.9, 0.8, 0.7)


# ---------------------------------------------------------------------------
# Frozen potential-flow reference curve
# ---------------------------------------------------------------------------


def reference_data_path() -> Path:
    """Path of the frozen capytaine heave-RAO reference CSV (repo checkout)."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = (parent / "docs" / "api" / "cfd" / "cases" /
                "wave_excited_body" / "rao_reference_capytaine.csv")
        if cand.is_file():
            return cand
    raise FileNotFoundError(
        "rao_reference_capytaine.csv not found — requires a repo checkout "
        "with docs/api/cfd/cases/wave_excited_body/ (regenerate via "
        "generate_rao_reference.py)"
    )


def load_reference_curve(path: Path | str | None = None) -> Dict[str, Any]:
    """Load the frozen reference as sorted-by-period arrays.

    Returns:
        Dict with ``period_s``, ``rao_heave`` (ascending in period) plus the
        source columns; used for interpolation and the report overlay.
    """
    import io

    import numpy as np

    p = Path(path) if path is not None else reference_data_path()
    lines = [ln for ln in p.read_text().splitlines()
             if ln.strip() and not ln.lstrip().startswith("#")]
    raw = np.genfromtxt(io.StringIO("\n".join(lines)), delimiter=",", names=True)
    order = np.argsort(raw["period_s"])
    return {name: np.asarray(raw[name])[order] for name in raw.dtype.names}


def reference_rao_at(period: float,
                     curve: Dict[str, Any] | None = None) -> float:
    """Reference heave RAO at ``period`` (linear interp in period)."""
    import numpy as np

    curve = curve or load_reference_curve()
    ps, raos = curve["period_s"], curve["rao_heave"]
    if period < ps[0] or period > ps[-1]:
        raise ValueError(
            f"period {period} s outside reference range "
            f"[{ps[0]:.3g}, {ps[-1]:.3g}] s"
        )
    return float(np.interp(period, ps, raos))


def reference_peak(curve: Dict[str, Any] | None = None) -> Tuple[float, float]:
    """(peak_period_s, peak_rao) of the frozen reference curve."""
    import numpy as np

    curve = curve or load_reference_curve()
    i = int(np.argmax(curve["rao_heave"]))
    return float(curve["period_s"][i]), float(curve["rao_heave"][i])


# ---------------------------------------------------------------------------
# Per-period case configuration
# ---------------------------------------------------------------------------


def group_velocity(period: float, depth: float) -> float:
    """Finite-depth group velocity c_g (m/s) for wave-arrival timing."""
    k = dispersion_wavenumber(period, depth)
    omega = 2.0 * math.pi / period
    c = omega / k
    kh = k * depth
    # deep-water limit n -> 1/2; guard sinh overflow for large kh
    n = 0.5 if kh > 30.0 else 0.5 * (1.0 + 2.0 * kh / math.sinh(2.0 * kh))
    return n * c


def sweep_gauges(period: float, depth: float,
                 body_x: float = 13.0,
                 n_gauges: int = 8) -> Tuple[float, ...]:
    """Wave-gauge x array upstream of the body, spacing ~0.2 lambda.

    Centred at x = 9.5 m (upstream of the body at x = 13, past the wavemaker
    evanescent zone), spacing clamped so the array both resolves the wave
    (spacing < ~0.35 lambda, away from the Goda-Suzuki lambda/2 singularity)
    and fits between x ~ 7.5 and 12.0 m. Endpoints at x = 2 and 18 m are kept
    for wave-quality context (outside the split window).
    """
    k = dispersion_wavenumber(period, depth)
    wavelength = 2.0 * math.pi / k
    spacing = min(max(0.2 * wavelength, 0.15), 0.9)
    half = (n_gauges - 1) / 2.0
    centre = 9.5
    xs = [round(centre + (j - half) * spacing, 4) for j in range(n_gauges)]
    # keep the split array upstream of the body and past the inlet zone
    xs = [x for x in xs if 7.0 <= x <= min(12.0, body_x - 0.8)]
    return (2.0, *xs, 18.0)


def build_sweep_config(period: float,
                       base: WaveExcitedBodyConfig | None = None,
                       cells_per_wavelength: float = CELLS_PER_WAVELENGTH,
                       ) -> WaveExcitedBodyConfig:
    """Config for one RAO sweep point at ``period`` (verified geometry).

    Adapts, relative to the verified #1302 fast case:
    - ``nx`` for >= ``cells_per_wavelength`` cells per wavelength (never
      coarser than the verified fast 250);
    - the wave-gauge array (:func:`sweep_gauges`);
    - ``end_time`` = ramp + arrival (body_x / c_g) + 4 T settle + 8 T analyse,
      and the analysis ``steady_window`` = the last 8 T.
    ``nz`` stays 49 (0.4 m waterline on a cell face); depth/body unchanged.

    ``cells_per_wavelength`` defaults to :data:`CELLS_PER_WAVELENGTH` (18, the
    verified fast tier). Raising it (e.g. 36) refines the background grid to
    combat numerical wave dissipation over the fetch — the short-wave
    resolution study of #1324, where under-resolved incident waves decay
    before reaching the body and cap the measured RAO below the reference.
    Only ``nx`` changes, so the wave-gauge array and analysis window (hence
    the analysis) are identical to the default-resolution point at the same
    period.
    """
    base = base or WaveExcitedBodyConfig(nz=49)
    k = dispersion_wavenumber(period, base.depth)
    wavelength = 2.0 * math.pi / k
    nx = max(base.nx, int(math.ceil(
        cells_per_wavelength * base.tank_length / wavelength)))

    cg = group_velocity(period, base.depth)
    ramp = 3.0
    arrival = ramp + base.body_x / cg
    settle = 4.0 * period
    analyse = 8.0 * period
    end_time = math.ceil(arrival + settle + analyse)
    window = (round(end_time - analyse, 3), float(end_time))

    return replace(
        base,
        wave_period=period,
        nx=nx,
        end_time=float(end_time),
        steady_window=window,
        gauges_x=sweep_gauges(period, base.depth, base.body_x),
        name=f"validation_wave_excited_body_rao_T{period:g}".replace(".", "p"),
    )


# ---------------------------------------------------------------------------
# Per-period analysis (CFD vs reference)
# ---------------------------------------------------------------------------


def _band_for(period: float, curve: Dict[str, Any] | None = None) -> float:
    """RAO band at ``period`` — wider within RESONANCE_GUARD of the peak."""
    peak_T, _ = reference_peak(curve)
    near = abs(period - peak_T) <= RESONANCE_GUARD * peak_T
    return RAO_SWEEP_BAND_RESONANCE if near else RAO_SWEEP_BAND


def analyze_sweep_point(
    case_dir: Path | str,
    config: WaveExcitedBodyConfig,
    curve: Dict[str, Any] | None = None,
) -> Dict[str, Any]:
    """Analyse one solved sweep case: CFD RAO vs the reference RAO.

    Reuses the verified #1302 extraction (incident split + heave harmonic
    fit) but gates the RAO against the frozen potential-flow reference at
    this period rather than the fixed long-wave 1.0.
    """
    curve = curve or load_reference_curve()
    case_dir = Path(case_dir)
    bg = case_dir / "background"

    quality = incident_wave_split(bg, config)
    a_i = quality["incident_amplitude"]
    t, z = extract_heave_from_log(bg)
    resp = analyze_excited_heave(t, z, a_i, config)  # RAO, draft, period

    rao_cfd = resp["rao"]
    rao_ref = reference_rao_at(config.wave_period, curve)
    band = _band_for(config.wave_period, curve)
    rel_err = abs(rao_cfd - rao_ref) / rao_ref

    return {
        "period_s": config.wave_period,
        "wavelength_m": config.wavelength,
        "lambda_over_beam": config.lambda_over_beam,
        "nx": config.nx,
        "cells_per_wavelength": config.wavelength / (config.tank_length / config.nx),
        "incident_amplitude": a_i,
        "reflection_kr": quality["reflection_kr"],
        "rao_cfd": rao_cfd,
        "rao_reference": rao_ref,
        "rao_rel_error": rel_err,
        "band": band,
        "within_band": bool(rel_err <= band),
        "heave_amplitude": resp["heave_amplitude"],
        "response_period_s": resp["period_measured"],
        "draft_error": resp["draft_error"],
        "gauges_excluded_x": quality["excluded_gauges_x"],
        "steady_window": list(config.steady_window),
    }


def summarize_sweep(points: List[Dict[str, Any]],
                    curve: Dict[str, Any] | None = None) -> Dict[str, Any]:
    """Aggregate sweep metrics + the reference peak for the report/gate."""
    curve = curve or load_reference_curve()
    peak_T, peak_rao = reference_peak(curve)
    ordered = sorted(points, key=lambda d: d["period_s"])
    cfd_peak = max(points, key=lambda d: d["rao_cfd"]) if points else None
    return {
        "n_points": len(points),
        "reference_peak": {"period_s": peak_T, "rao": peak_rao},
        "cfd_peak": (
            {"period_s": cfd_peak["period_s"], "rao": cfd_peak["rao_cfd"]}
            if cfd_peak else None
        ),
        "all_within_band": all(p["within_band"] for p in points),
        "points": ordered,
    }
