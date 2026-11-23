"""
Woehler (S-N) curve fitting from fatigue test data.

Wraps pyLife's Elementary and MaxLikeFull analyzers with a user-friendly
interface that handles data format conversion, and provides a log-log
regression fallback when pyLife's fitting requirements are not met.

Reference: pyLife v2.2.1 — pylife.materialdata.woehler
"""

import warnings

import numpy as np
import pandas as pd
import scipy.stats as stats


def generate_test_data(
    k_1: float,
    log_a: float,
    n_samples: int = 20,
    scatter_log: float = 0.15,
    endurance_stress: float | None = None,
    seed: int = 42,
) -> pd.DataFrame:
    """Generate synthetic fatigue test data for testing the fitting.

    Creates realistic S-N data by sampling from a known Basquin relation
    with log-normal scatter, including runouts at low stress levels.

    Parameters
    ----------
    k_1 : float
        Negative inverse slope of the S-N curve (e.g. 3.0).
    log_a : float
        Log10 intercept of the S-N curve (e.g. 11.855 for DNV Cat F).
    n_samples : int
        Total number of test specimens.
    scatter_log : float
        Standard deviation in log10(N) space.
    endurance_stress : float or None
        If given, specimens at or below this stress are marked as runouts.
        If None, the lowest ~20% of stress levels become runouts.
    seed : int
        Random seed for reproducibility.

    Returns
    -------
    pd.DataFrame
        Columns: 'stress_range', 'cycles', 'runout'.
    """
    rng = np.random.default_rng(seed)

    # Generate stress levels spanning a realistic range
    # Use discrete stress levels (typical of test machines)
    stress_levels = np.array([200, 180, 160, 140, 120, 100, 80, 60, 50, 40])
    n_levels = min(len(stress_levels), max(4, n_samples // 2))
    chosen_levels = stress_levels[:n_levels]

    # Distribute samples across levels
    samples_per_level = np.full(n_levels, n_samples // n_levels, dtype=int)
    remainder = n_samples - samples_per_level.sum()
    for i in range(remainder):
        samples_per_level[i % n_levels] += 1

    stress_all = np.repeat(chosen_levels, samples_per_level)
    log_n_true = log_a - k_1 * np.log10(stress_all)
    scatter = rng.normal(0, scatter_log, len(stress_all))
    log_n_observed = log_n_true + scatter
    cycles = 10**log_n_observed

    # Determine runouts
    if endurance_stress is not None:
        runout = stress_all <= endurance_stress
    else:
        # Mark lowest ~20% of stress levels as runouts
        cutoff_idx = max(1, int(n_levels * 0.8))
        runout_threshold = chosen_levels[cutoff_idx - 1]
        runout = stress_all < runout_threshold

    # For runouts, cap cycles at a plausible run-out limit
    runout_limit = 10 ** (log_a - k_1 * np.log10(stress_all[runout].mean() if runout.any() else 50) + 0.5)
    cycles[runout] = np.maximum(cycles[runout], runout_limit)

    return pd.DataFrame({
        "stress_range": stress_all,
        "cycles": cycles,
        "runout": runout,
    })


def _validate_input(test_data: pd.DataFrame) -> None:
    """Validate input DataFrame has required columns and data."""
    if test_data is None or test_data.empty:
        raise ValueError("test_data must be a non-empty DataFrame.")

    # Accept either 'stress_range' or 'load' for the stress column
    has_stress = "stress_range" in test_data.columns or "load" in test_data.columns
    if not has_stress:
        raise ValueError(
            "test_data must contain a 'stress_range' or 'load' column."
        )
    if "cycles" not in test_data.columns:
        raise ValueError("test_data must contain a 'cycles' column.")


def _to_pylife_format(test_data: pd.DataFrame) -> pd.DataFrame:
    """Convert user-friendly DataFrame to pyLife's expected format.

    pyLife fatigue_data accessor expects columns:
        - 'load': float (stress amplitude or range)
        - 'cycles': float
        - 'fracture': bool (True = fractured, False = runout)
    """
    df = pd.DataFrame()

    # Map stress column
    if "stress_range" in test_data.columns:
        df["load"] = test_data["stress_range"].astype(float)
    else:
        df["load"] = test_data["load"].astype(float)

    df["cycles"] = test_data["cycles"].astype(float)

    # Map fracture/runout column
    if "runout" in test_data.columns:
        df["fracture"] = ~test_data["runout"].astype(bool)
    elif "fracture" in test_data.columns:
        df["fracture"] = test_data["fracture"].astype(bool)
    else:
        # No fracture info: assume all are fractures
        df["fracture"] = True

    return df


def _fallback_log_regression(test_data: pd.DataFrame) -> pd.Series:
    """Simple log-log linear regression fallback.

    Used when pyLife's fitting requirements are not met (too few load
    levels, insufficient mixed zones, etc.).
    """
    df = _to_pylife_format(test_data)
    fractures = df[df["fracture"]]

    if len(fractures) < 2:
        raise ValueError("Need at least 2 fracture data points for fitting.")

    log_s = np.log10(fractures["load"].values)
    log_n = np.log10(fractures["cycles"].values)

    slope, intercept, _, _, _ = stats.linregress(log_s, log_n)
    k_1 = -slope
    log_a = intercept

    # Estimate scatter
    log_n_pred = intercept + slope * log_s
    residuals = log_n - log_n_pred
    std_log_n = np.std(residuals, ddof=1) if len(residuals) > 2 else 0.2

    from pylife.utils.functions import std_to_scattering_range
    TN = std_to_scattering_range(std_log_n)
    TS = TN ** (1.0 / k_1) if k_1 > 0 else np.nan

    # Estimate SD from the lowest fractured load level
    runouts = df[~df["fracture"]]
    if len(runouts) > 0:
        SD = (fractures["load"].min() + runouts["load"].max()) / 2.0
    else:
        SD = fractures["load"].min() * 0.8

    ND = 10 ** (log_a - k_1 * np.log10(SD))

    return pd.Series({
        "k_1": k_1,
        "SD": SD,
        "ND": ND,
        "TN": TN,
        "TS": TS,
        "failure_probability": 0.5,
    })


def fit_woehler_curve(
    test_data: pd.DataFrame,
    method: str = "elementary",
    fixed_params: dict | None = None,
) -> pd.Series:
    """Fit a Woehler (S-N) curve to fatigue test data.

    Parameters
    ----------
    test_data : pd.DataFrame
        Must contain columns:
        - 'stress_range' (or 'load'): stress amplitude/range in MPa
        - 'cycles': cycles to failure or runout
        - 'runout' (bool, True = did not fail) OR 'fracture' (bool, True = failed)
    method : str
        'elementary' for fast estimation, 'maxlike' for maximum likelihood.
    fixed_params : dict or None
        Parameters to hold fixed during fitting (e.g. {'k_1': 3.0}).
        Only used with 'maxlike' method.

    Returns
    -------
    pd.Series
        Keys: k_1, SD, ND, TN, TS, failure_probability.
        Compatible with pyLife WoehlerCurve.
    """
    _validate_input(test_data)

    if method not in ("elementary", "maxlike"):
        raise ValueError(f"method must be 'elementary' or 'maxlike', got '{method}'")

    pylife_df = _to_pylife_format(test_data)

    # Verify we have enough data
    n_fractures = pylife_df["fracture"].sum()
    if n_fractures < 2:
        raise ValueError(
            f"Need at least 2 fracture data points, got {n_fractures}."
        )

    try:
        if method == "elementary":
            from pylife.materialdata.woehler import Elementary

            analyzer = Elementary(pylife_df)
            result = analyzer.analyze()
        else:
            from pylife.materialdata.woehler import MaxLikeFull

            analyzer = MaxLikeFull(pylife_df)
            result = analyzer.analyze(
                fixed_parameters=fixed_params if fixed_params else {}
            )

        return result

    except (ValueError, KeyError) as exc:
        warnings.warn(
            f"pyLife {method} fitting failed ({exc}); "
            "falling back to log-log regression.",
            RuntimeWarning,
            stacklevel=2,
        )
        return _fallback_log_regression(test_data)


def design_curve(
    fitted_params: pd.Series,
    failure_probability: float = 0.023,
) -> pd.Series:
    """Shift a mean (50%) S-N curve to a design curve at a lower failure probability.

    The design curve is derived by shifting the endurance limit SD
    downward using the scatter in load direction TS, based on the
    assumption that load endurance follows a log-normal distribution.

    Parameters
    ----------
    fitted_params : pd.Series
        Result from fit_woehler_curve (must contain k_1, SD, ND, TN, TS).
    failure_probability : float
        Target failure probability (default 0.023 ~ 2-sigma, common
        for DNV design curves).

    Returns
    -------
    pd.Series
        Design curve parameters with adjusted SD and ND.
    """
    if failure_probability <= 0 or failure_probability >= 1:
        raise ValueError("failure_probability must be between 0 and 1 (exclusive).")

    result = fitted_params.copy()
    k_1 = result["k_1"]
    SD_50 = result["SD"]
    TN = result.get("TN", np.nan)

    if np.isnan(TN) or TN <= 1.0:
        warnings.warn(
            "TN scatter not available or invalid; returning unshifted curve.",
            RuntimeWarning,
            stacklevel=2,
        )
        result["failure_probability"] = failure_probability
        return result

    # Standard deviation in log10(N) from scattering range TN
    # TN = 10^(2 * t_quantile * std_log_N) for 10%/90% => std = log10(TN) / (2*1.282)
    # More precisely: std_log_N = log10(TN) / (2 * norm.ppf(0.9))
    std_log_n = np.log10(TN) / (2.0 * stats.norm.ppf(0.9))

    # Quantile shift for target failure probability
    z = stats.norm.ppf(failure_probability)  # negative for prob < 0.5

    # Shift in log10(N) space (from 50% median)
    delta_log_n = z * std_log_n  # negative => fewer cycles allowed

    # Convert to load shift: delta_log_S = delta_log_N / (-k_1)
    delta_log_s = delta_log_n / k_1  # negative => lower SD

    SD_design = SD_50 * 10**delta_log_s
    ND_design = result["ND"] * 10**delta_log_n

    result["SD"] = SD_design
    result["ND"] = ND_design
    result["failure_probability"] = failure_probability

    return result
