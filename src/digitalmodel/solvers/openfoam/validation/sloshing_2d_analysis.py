#!/usr/bin/env python3
"""
ABOUTME: Post-processing for the 2D sloshing validation cases (#639): parses the
interfaceHeight and roll-moment function-object output, measures the first-mode
natural frequency by FFT with parabolic peak refinement, and scores the measured
frequency against the analytical tanh dispersion relation.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, List, Tuple

from ..spectral_analysis import (
    compute_fft_spectrum,
)

from .sloshing_2d_config import (
    SLOSHING_FREQ_TOLERANCE,
    SloshingFreeDecayConfig,
)
from .sloshing_2d_dicts import ROLL_MOMENT_FO_NAME

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


