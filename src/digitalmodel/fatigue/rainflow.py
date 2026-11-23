"""
Rainflow Cycle Counting for Time-Series Fatigue Analysis

Provides rainflow counting via pyLife's FourPointDetector, stress range
histogramming, and an end-to-end fatigue life pipeline that connects
time series -> rainflow -> histogram -> Miner's damage -> design life.

References:
    ASTM E1049-85 Standard Practices for Cycle Counting in Fatigue Analysis
    pyLife v2.2 rainflow module documentation
"""

import numpy as np
import pandas as pd
from pylife.stress.rainflow import FourPointDetector
from pylife.stress.rainflow.recorders import LoopValueRecorder

from .damage import miner_damage
from .sn_curves import get_sn_curve


def rainflow_count(
    time_series: np.ndarray | pd.Series,
    time: np.ndarray | pd.Series | None = None,
) -> pd.DataFrame:
    """
    Perform rainflow cycle counting on a stress time series.

    Uses pyLife's FourPointDetector for closed hysteresis loop detection.
    Residual turning points are counted as half-cycles (0.5).

    Parameters
    ----------
    time_series : array-like
        1-D array of stress values (e.g. MPa). Must not contain NaN.
    time : array-like, optional
        Corresponding time array. Stored for reference but not used
        in counting.

    Returns
    -------
    pd.DataFrame
        Columns: 'stress_range', 'mean_stress', 'cycles'.
        Each row is one counted cycle (1.0) or half-cycle (0.5).
    """
    signal = pd.Series(np.asarray(time_series, dtype=float))
    if signal.isna().any():
        raise ValueError("time_series must not contain NaN values")

    recorder = LoopValueRecorder()
    detector = FourPointDetector(recorder)
    detector.process(signal)

    # -- Full cycles from closed hysteresis loops --
    rows = []
    frm = np.asarray(recorder.values_from, dtype=float)
    to = np.asarray(recorder.values_to, dtype=float)
    if len(frm) > 0:
        stress_range = np.abs(to - frm)
        mean_stress = (to + frm) / 2.0
        for sr, ms in zip(stress_range, mean_stress):
            rows.append({"stress_range": sr, "mean_stress": ms, "cycles": 1.0})

    # -- Half-cycles from residual turning points --
    residuals = detector.residuals
    if len(residuals) > 1:
        for i in range(len(residuals) - 1):
            sr = abs(residuals[i + 1] - residuals[i])
            ms = (residuals[i] + residuals[i + 1]) / 2.0
            rows.append({"stress_range": float(sr), "mean_stress": float(ms), "cycles": 0.5})

    if not rows:
        return pd.DataFrame(columns=["stress_range", "mean_stress", "cycles"])

    return pd.DataFrame(rows)


def stress_histogram(
    cycle_counts: pd.DataFrame,
    bin_edges: np.ndarray | None = None,
    n_bins: int = 20,
) -> pd.DataFrame:
    """
    Bin rainflow cycle counts into a stress range histogram.

    Parameters
    ----------
    cycle_counts : pd.DataFrame
        Output from :func:`rainflow_count` with columns
        'stress_range' and 'cycles'.
    bin_edges : array-like, optional
        Explicit bin edges. If *None*, ``n_bins`` equally spaced bins
        are generated spanning the data range.
    n_bins : int
        Number of bins when ``bin_edges`` is not provided (default 20).

    Returns
    -------
    pd.DataFrame
        Columns: 'stress_range' (bin centre) and 'cycles' (weighted sum).
        Compatible with :func:`~digitalmodel.fatigue.damage.miner_damage`.
    """
    if cycle_counts.empty:
        return pd.DataFrame(columns=["stress_range", "cycles"])

    ranges = cycle_counts["stress_range"].values
    weights = cycle_counts["cycles"].values

    if bin_edges is None:
        lo = ranges.min()
        hi = ranges.max()
        # Avoid degenerate single-value range
        if np.isclose(lo, hi):
            hi = lo + 1.0
        bin_edges = np.linspace(lo, hi, n_bins + 1)

    # Weighted histogram: sum cycle counts per bin
    counts, edges = np.histogram(ranges, bins=bin_edges, weights=weights)

    centres = (edges[:-1] + edges[1:]) / 2.0

    # Drop empty bins
    mask = counts > 0
    return pd.DataFrame({
        "stress_range": centres[mask],
        "cycles": counts[mask],
    }).reset_index(drop=True)


def fatigue_life(
    time_series: np.ndarray | pd.Series,
    sn_curve_name: str,
    environment: str = "air",
    n_bins: int = 20,
    target_years: float = 25.0,
) -> dict:
    """
    End-to-end fatigue pipeline: time series to design life assessment.

    Steps:
        1. Rainflow cycle counting
        2. Stress range histogramming
        3. Miner's rule damage accumulation
        4. Design life evaluation

    Parameters
    ----------
    time_series : array-like
        1-D stress time history (MPa).
    sn_curve_name : str
        DNV-RP-C203 curve name, e.g. 'D', 'F', 'B1'.
    environment : str
        One of 'air', 'seawater_cp', 'free_corrosion'.
    n_bins : int
        Number of histogram bins (default 20).
    target_years : float
        Required design life in years (default 25).

    Returns
    -------
    dict
        Keys:
        - damage : float — total Miner's damage for this time series
        - life_factor : float — years_to_failure / target_years
        - years_to_failure : float
        - pass_fail : str — 'PASS' or 'FAIL'
        - histogram : pd.DataFrame — binned stress histogram
        - cycle_count : pd.DataFrame — raw rainflow output
    """
    sn_curve = get_sn_curve(sn_curve_name, environment)
    cycles = rainflow_count(time_series)
    hist = stress_histogram(cycles, n_bins=n_bins)

    if hist.empty:
        return {
            "damage": 0.0,
            "life_factor": float("inf"),
            "years_to_failure": float("inf"),
            "pass_fail": "PASS",
            "histogram": hist,
            "cycle_count": cycles,
        }

    damage_df = miner_damage(hist, sn_curve)
    total_damage = damage_df.attrs["total_damage"]

    if total_damage > 0:
        years_to_failure = 1.0 / total_damage
    else:
        years_to_failure = float("inf")

    life_factor = years_to_failure / target_years

    return {
        "damage": total_damage,
        "life_factor": life_factor,
        "years_to_failure": years_to_failure,
        "pass_fail": "PASS" if life_factor >= 1.0 else "FAIL",
        "histogram": hist,
        "cycle_count": cycles,
    }
