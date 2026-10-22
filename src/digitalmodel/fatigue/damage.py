# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""
Fatigue Damage Accumulation — Miner's Rule

Provides cumulative damage calculation, design-life checking,
and DNV thickness correction for stress ranges.
"""

import numpy as np
import pandas as pd
from pylife.materiallaws.woehlercurve import WoehlerCurve


def miner_damage(
    stress_histogram: pd.DataFrame,
    sn_curve: WoehlerCurve,
) -> pd.DataFrame:
    """
    Calculate Miner's rule cumulative damage from a stress histogram.

    Parameters
    ----------
    stress_histogram : pd.DataFrame
        Must have columns 'stress_range' (MPa) and 'cycles' (applied count).
    sn_curve : WoehlerCurve
        pyLife WoehlerCurve instance.

    Returns
    -------
    pd.DataFrame
        Copy of input with added 'allowable_cycles' and 'damage' columns.
        The returned DataFrame has a `total_damage` attribute.
    """
    result = stress_histogram.copy()
    if result.empty:
        raise ValueError("Miner damage requires a non-empty stress histogram")
    for column in ("stress_range", "cycles"):
        values = result[column].to_numpy(dtype=float)
        if not np.isfinite(values).all() or (values < 0).any():
            raise ValueError(f"Miner damage {column} must be finite and non-negative")
    stress_ranges = result["stress_range"].values.astype(float)
    with np.errstate(over="ignore", divide="ignore", invalid="ignore"):
        allowable = np.asarray(sn_curve.cycles(stress_ranges), dtype=float)
        if np.isnan(allowable).any() or (allowable <= 0).any():
            raise ValueError("Miner damage allowable cycles must be positive and not NaN")
        damage = result["cycles"].to_numpy(dtype=float) / allowable
    if not np.isfinite(damage).all() or (damage < 0).any():
        raise ValueError("Miner damage contributions must be finite and non-negative")
    result["allowable_cycles"] = allowable
    result["damage"] = damage
    with np.errstate(over="ignore", invalid="ignore"):
        total_damage = float(result["damage"].sum(skipna=False))
    if not np.isfinite(total_damage):
        raise ValueError("Miner total damage must be finite")
    result.attrs["total_damage"] = total_damage
    return result


def design_life_check(
    stress_histogram: pd.DataFrame,
    sn_curve: WoehlerCurve,
    target_years: float = 25.0,
    annual_cycles_factor: float = 1.0,
) -> dict:
    """
    Check whether a component meets its fatigue design life target.

    The histogram is assumed to represent one year of loading scaled by
    ``annual_cycles_factor``.  The total Miner damage for one year is
    extrapolated to ``target_years``.

    Parameters
    ----------
    stress_histogram : pd.DataFrame
        Columns 'stress_range' and 'cycles'.
    sn_curve : WoehlerCurve
        pyLife WoehlerCurve instance.
    target_years : float
        Required design life in years (default 25).
    annual_cycles_factor : float
        Multiplier on histogram cycles to get annual loading (default 1.0).

    Returns
    -------
    dict
        Keys: damage, life_factor, years_to_failure, pass_fail.
    """
    scaled = stress_histogram.copy()
    scaled["cycles"] = scaled["cycles"] * annual_cycles_factor
    result = miner_damage(scaled, sn_curve)
    annual_damage = result.attrs["total_damage"]

    if annual_damage > 0:
        years_to_failure = 1.0 / annual_damage
    else:
        years_to_failure = float("inf")

    life_factor = years_to_failure / target_years

    return {
        "damage": annual_damage * target_years,
        "life_factor": life_factor,
        "years_to_failure": years_to_failure,
        "pass_fail": "PASS" if life_factor >= 1.0 else "FAIL",
    }


def thickness_correction(
    stress_range: np.ndarray,
    t_actual: float,
    t_ref: float = 25.0,
    k: float = 0.25,
) -> np.ndarray:
    """
    DNV-RP-C203 thickness correction for welded joints.

    S_corrected = S * (t_actual / t_ref)^k

    For t_actual > t_ref the corrected stress is higher (penalty).
    For t_actual <= t_ref no correction is applied (factor capped at 1.0).

    Parameters
    ----------
    stress_range : array-like
        Nominal stress ranges (MPa).
    t_actual : float
        Actual plate thickness (mm).
    t_ref : float
        Reference thickness (mm), default 25.
    k : float
        Thickness exponent, default 0.25.

    Returns
    -------
    np.ndarray
        Corrected stress ranges.
    """
    stress_range = np.asarray(stress_range, dtype=float)
    if t_actual <= t_ref:
        return stress_range.copy()
    factor = (t_actual / t_ref) ** k
    return stress_range * factor
