"""
Fatigue Damage Accumulation — Miner's Rule

Provides cumulative damage calculation, design-life checking,
and DNV thickness correction for stress ranges.
"""

import math
from numbers import Real

import numpy as np
import pandas as pd
from pylife.materiallaws.woehlercurve import WoehlerCurve

from ._deterministic import deterministic_power


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
    stress_ranges, applied_cycles = validate_histogram(
        result["stress_range"], result["cycles"]
    )
    allowable = _allowable_cycles(sn_curve, stress_ranges)
    result["allowable_cycles"] = allowable
    allowable = np.asarray(allowable, dtype=float)
    endurance = np.zeros_like(stress_ranges, dtype=bool)
    if isinstance(sn_curve, WoehlerCurve):
        # pyLife defines k_2=+inf as perfect endurance below SD; an
        # infinite result at or above SD remains unqualified.
        limit = np.asarray(sn_curve.SD, dtype=float)
        endurance = np.isposinf(sn_curve.k_2) & (stress_ranges < limit)
        for parameter in (limit, sn_curve.ND, sn_curve.k_1):
            value = np.asarray(parameter, dtype=float)
            endurance &= np.isfinite(value) & (value > 0)
    if (np.any(np.isnan(allowable)) or np.any(allowable <= 0)
            or np.any(~np.isfinite(allowable) & ~endurance
                      & (stress_ranges > 0) & (applied_cycles > 0))):
        raise ValueError("allowable cycles must be positive and known")
    result["damage"] = applied_cycles / allowable
    damages = result["damage"].to_numpy()
    if not np.all(np.isfinite(damages)) or np.any(damages < 0):
        raise ValueError("Miner bin damage must be finite and non-negative")
    # math.fsum is exactly rounded, so the total is independent of bin order
    # and of the SIMD width a NumPy reduction would use.
    try:
        total = math.fsum(float(value) for value in damages)
    except OverflowError:
        total = math.inf
    if not np.isfinite(total) or total < 0:
        raise ValueError("Miner total damage must be finite and non-negative")
    result.attrs["total_damage"] = total
    return result


# pyLife steps that WoehlerCurve.cycles runs; an override of any of them means
# the curve is not plain pyLife Basquin and is evaluated through pyLife itself.
_PYLIFE_STEPS = ("cycles", "basquin_cycles", "_make_k", "broadcast",
                 "transform_to_failure_probability")


def _scalar_parameter(value):
    """Return a real-number curve parameter as a float, or None otherwise.

    Only Python and NumPy integers and floats qualify. Booleans, strings,
    arrays and other objects return None, so pyLife evaluates the curve and
    keeps its own behaviour for them, including its ``TypeError``.
    """
    if isinstance(value, (bool, np.bool_)):
        return None
    if not isinstance(value, (int, float, np.integer, np.floating)):
        return None
    try:
        return float(value)
    except (TypeError, ValueError, OverflowError):
        return None


def _plain_basquin_parameters(sn_curve):
    """(SD, ND, k_1, k_2) when pyLife's default ``cycles`` is plain Basquin.

    Requires an exact ``WoehlerCurve`` with no instance override of a pyLife
    step, finite positive SD and ND, a finite k_1, a k_2 that is finite or
    +inf, and a curve on which every supported pyLife version's default
    failure-probability handling is the identity: at the median (0.5) or
    without scatter (TN = TS = 1). Returns None otherwise, and pyLife's own
    ``cycles`` then evaluates the curve, including its error behaviour.
    """
    if type(sn_curve) is not WoehlerCurve:
        return None
    if any(name in vars(sn_curve) for name in _PYLIFE_STEPS):
        return None
    params = [_scalar_parameter(getattr(sn_curve, name)) for name in
              ("SD", "ND", "k_1", "k_2", "TN", "TS", "failure_probability")]
    if None in params:
        return None
    sd, nd, k_1, k_2, tn, ts, probability = params
    if not all(math.isfinite(value) and value > 0 for value in (sd, nd)):
        return None
    if not math.isfinite(k_1) or not (math.isfinite(k_2) or k_2 == math.inf):
        return None
    if probability != 0.5 and not (tn == 1.0 and ts == 1.0):
        return None
    return sd, nd, k_1, k_2


def _allowable_cycles(sn_curve, stress_ranges):
    """Allowable cycles per bin, evaluated CPU-deterministically.

    For a plain pyLife Basquin curve, pyLife's law and operation order,
    ``N = ND * (S / SD) ** -k`` with ``k = k_1`` at or above SD and ``k_2``
    below it (infinite ``k`` gives infinite N), are reproduced one bin at a
    time, with the power from ``deterministic_power``. ``np.power`` and libm
    ``pow`` give CPU-dependent last bits. Any other curve object, and a
    histogram in which some ``S / SD`` overflows, is evaluated through the
    curve's own ``cycles`` method, unchanged.
    """
    parameters = _plain_basquin_parameters(sn_curve)
    if parameters is None:
        return sn_curve.cycles(stress_ranges)
    sd, nd, k_1, k_2 = parameters
    stresses = stress_ranges.tolist()
    ratios = [stress / sd for stress in stresses]
    if not all(math.isfinite(ratio) for ratio in ratios):
        # pyLife defines the power of an infinite ratio (zero allowable
        # cycles); the deterministic power takes finite bases only.
        return sn_curve.cycles(stress_ranges)
    allowable = np.empty(len(stresses), dtype=float)
    for index, (stress, ratio) in enumerate(zip(stresses, ratios)):
        k = k_2 if stress < sd else k_1
        if math.isfinite(k):
            allowable[index] = nd * deterministic_power(ratio, -k)
        else:
            allowable[index] = math.inf
    return allowable


def validate_histogram(stress_ranges, cycles):
    """Require known non-negative bins; unloaded bins have valid zero damage."""
    try:
        for values in (stress_ranges, cycles):
            raw = np.asarray(values, dtype=object)
            if any(isinstance(value, (bool, np.bool_, complex)) for value in raw.flat):
                raise ValueError("histogram entries must be real numbers, not booleans or complex values")
        stress = np.asarray(stress_ranges, dtype=float)
        counts = np.asarray(cycles, dtype=float)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("histogram stress and cycles must be finite numbers") from exc
    if stress.ndim != 1 or counts.ndim != 1 or not stress.size or not counts.size:
        raise ValueError("histogram must contain non-empty one-dimensional bins")
    if stress.shape != counts.shape:
        raise ValueError("stress_ranges_mpa and cycles must have the same length")
    if not np.all(np.isfinite(stress)) or not np.all(np.isfinite(counts)):
        raise ValueError("histogram stress and cycles must be finite")
    if np.any(stress < 0) or np.any(counts < 0):
        raise ValueError("histogram stress and cycles must be non-negative")
    return stress, counts


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
    for name, value in (("t_actual", t_actual), ("t_ref", t_ref), ("k", k)):
        if (isinstance(value, bool) or not isinstance(value, Real)
                or not np.isfinite(value) or value < 0 or (name != "k" and value == 0)):
            raise ValueError(f"{name} must be finite and {'non-negative' if name == 'k' else 'positive'}")
    stress_range = np.asarray(stress_range, dtype=float)
    if not np.all(np.isfinite(stress_range)) or np.any(stress_range < 0):
        raise ValueError("stress_range must be finite and non-negative")
    if t_actual <= t_ref:
        return stress_range.copy()
    factor = deterministic_power(t_actual / t_ref, k)
    corrected = stress_range * factor
    if not np.all(np.isfinite(corrected)):
        raise ValueError("thickness-corrected stress must be finite")
    return corrected
