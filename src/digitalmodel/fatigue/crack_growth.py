# ABOUTME: Paris-law crack growth (da/dN = C DeltaK^m) integrated directly with
# ABOUTME: numpy: SIF with geometry factors, inspection intervals, growth plots.
"""
Crack growth analysis using Paris' law.

Provides functions for:
- Paris' law numerical integration (crack size vs cycles)
- Stress intensity factor (SIF) calculation with geometry corrections
- Inspection interval estimation per fracture mechanics principles
- Crack growth curve plotting

Reference: BS 7910:2019, Section 8 — Assessment of flaws by fracture mechanics.
"""

import math

import numpy as np
import pandas as pd

from .threshold_growth import GrowthLaw, integrate_growth, validate_scalar


def stress_intensity_factor(stress_range, crack_size, geometry="through_thickness", Y=1.12):
    """Compute stress intensity factor range DeltaK.

    DeltaK = Y * Delta_sigma * sqrt(pi * a)

    Parameters
    ----------
    stress_range : float
        Applied stress range Delta_sigma (MPa).
    crack_size : float or array-like
        Crack half-length *a* (m).
    geometry : str
        Geometry type: 'through_thickness', 'surface', or 'embedded'.
        Adjusts the geometry correction factor Y.
    Y : float
        Explicit geometry correction factor. Used when geometry='through_thickness'.
        For 'surface' and 'embedded', Y is overridden by standard values.

    Returns
    -------
    float or ndarray
        Stress intensity factor range DeltaK (MPa*sqrt(m)).
    """
    stress_range = validate_scalar(stress_range, "stress_range")
    Y = validate_scalar(Y, "Y")
    raw_sizes = np.asarray(crack_size, dtype=object)
    crack_size = np.asarray(
        [validate_scalar(value, "crack_size") for value in raw_sizes.flat],
        dtype=float,
    ).reshape(raw_sizes.shape)
    if stress_range < 0 or Y <= 0 or np.any(crack_size <= 0):
        raise ValueError("stress_range must be non-negative; crack_size and Y positive")

    if geometry == "through_thickness":
        y_factor = Y  # user-supplied or default 1.12
    elif geometry == "surface":
        y_factor = 1.12  # semi-elliptical surface crack (simplified)
    elif geometry == "embedded":
        y_factor = 2.0 / np.pi  # ~0.637 — penny-shaped embedded crack
    else:
        raise ValueError(
            f"Unknown geometry '{geometry}'. "
            "Supported: 'through_thickness', 'surface', 'embedded'."
        )

    with np.errstate(over="raise", invalid="raise"):
        try:
            delta_k = y_factor * stress_range * np.sqrt(np.pi * crack_size)
        except FloatingPointError as exc:
            raise ValueError("stress intensity factor exceeds finite range") from exc
    return float(delta_k) if delta_k.ndim == 0 else delta_k


def _paris_inputs(C, m, delta_K, a_initial, a_critical, da_step):
    law = GrowthLaw(C, m, 0, 0, 1, "Caller-supplied legacy Paris parameters")
    initial = validate_scalar(a_initial, "a_initial")
    critical = validate_scalar(a_critical, "a_critical")
    increment = None if da_step is None else validate_scalar(da_step, "da_step")
    if not callable(delta_K):
        delta_K = validate_scalar(delta_K, "delta_K")
        if delta_K < 0:
            raise ValueError("delta_K must be non-negative")
    if initial <= 0:
        raise ValueError("a_initial must be positive.")
    if initial >= critical:
        raise ValueError("a_initial must be less than a_critical.")
    if increment is not None and increment <= 0:
        raise ValueError("da_step must be positive")
    count = 1000.0 if increment is None else (critical - initial) / increment
    if not math.isfinite(count):
        raise ValueError("da_step implies a nonfinite panel count")
    return law, delta_K, initial, critical, max(1, math.ceil(count))


def paris_law_life(C, m, delta_K, a_initial, a_critical, da_step=None):
    """Integrate Paris' law from initial to critical crack size.

    Uses constant-DeltaK integration when *delta_K* is a scalar (useful
    for quick estimates).  For a full stress-dependent analysis, pass
    *delta_K* as a callable ``delta_K(a)`` that returns DeltaK for a
    given crack size.

    Parameters
    ----------
    C : float
        Paris' law coefficient (units consistent with m, MPa, sqrt(m)).
    m : float
        Paris' law exponent.
    delta_K : float or callable
        If float: constant SIF range used throughout (MPa*sqrt(m)).
        If callable: ``delta_K(a)`` returns DeltaK for crack size *a*.
    a_initial : float
        Initial crack size (m).
    a_critical : float
        Critical crack size at failure (m).
    da_step : float, optional
        Crack increment per integration step (m).  Default is
        ``(a_critical - a_initial) / 1000``.

    Returns
    -------
    cycles_to_failure : float
        Total cycles N from a_initial to a_critical.
    history : pd.DataFrame
        Columns: crack_size (m), cycles (cumulative N), da_dN (m/cycle).

    Notes
    -----
    The shared adaptive Simpson kernel refines the initial ``da_step`` mesh
    to its default error tolerances. On sampled zero DeltaK, propagation is
    arrested: scalar life is infinity and history contains only reached
    points, with no fabricated critical-depth endpoint. Negative/nonfinite
    DeltaK is invalid. The callable smoothness limitations of
    :func:`integrate_growth` apply.
    """
    law, dk, initial, critical, steps = _paris_inputs(
        C, m, delta_K, a_initial, a_critical, da_step,
    )
    result = integrate_growth(law, dk, initial_m=initial, limit_m=critical, steps=steps)
    records = [
        {"crack_size": point.depth_m, "cycles": point.cycles,
         "da_dN": law.rate(dk(point.depth_m) if callable(dk) else dk)}
        for point in result.history
    ]
    return (math.inf if result.life_cycles is None else result.life_cycles), pd.DataFrame(records)


def inspection_interval(
    C,
    m,
    stress_range,
    a_initial,
    a_critical,
    a_detectable,
    geometry="through_thickness",
    safety_factor=2.0,
):
    """Compute recommended inspection interval based on fracture mechanics.

    Parameters
    ----------
    C, m : float
        Paris' law material constants.
    stress_range : float
        Applied stress range Delta_sigma (MPa).
    a_initial : float
        Initial crack size (m).
    a_critical : float
        Critical crack size at failure (m).
    a_detectable : float
        Minimum crack size detectable by inspection (m).
    geometry : str
        Crack geometry type (see :func:`stress_intensity_factor`).
    safety_factor : float
        Safety factor applied to detectable life.  Default 2.0.

    Returns
    -------
    dict
        Keys:
        - total_life: cycles from a_initial to a_critical
        - detectable_life: cycles from a_detectable to a_critical
        - inspection_interval: detectable_life / safety_factor
        - total_history: DataFrame of full crack growth history
        - detectable_history: DataFrame from a_detectable onward

    Notes
    -----
    Arrested propagation returns infinite life/interval with only the
    reached history points.
    """
    a_initial = validate_scalar(a_initial, "a_initial")
    a_critical = validate_scalar(a_critical, "a_critical")
    a_detectable = validate_scalar(a_detectable, "a_detectable")
    safety_factor = validate_scalar(safety_factor, "safety_factor")
    if safety_factor <= 0:
        raise ValueError("safety_factor must be positive")
    if a_detectable < a_initial:
        raise ValueError("a_detectable must be >= a_initial.")
    if a_detectable >= a_critical:
        raise ValueError("a_detectable must be < a_critical.")

    def _dk(a):
        return stress_intensity_factor(stress_range, a, geometry=geometry)

    total_life, total_history = paris_law_life(
        C, m, _dk, a_initial, a_critical
    )
    detectable_life, detectable_history = paris_law_life(
        C, m, _dk, a_detectable, a_critical
    )

    return {
        "total_life": total_life,
        "detectable_life": detectable_life,
        "inspection_interval": detectable_life / safety_factor,
        "total_history": total_history,
        "detectable_history": detectable_history,
    }


def crack_growth_curve_plot(history_df, ax=None):
    """Plot crack size vs cycles from a growth history DataFrame.

    Parameters
    ----------
    history_df : pd.DataFrame
        Must contain columns 'crack_size' and 'cycles' (from :func:`paris_law_life`).
    ax : matplotlib.axes.Axes, optional
        Axes to plot on.  If None a new figure is created.

    Returns
    -------
    matplotlib.axes.Axes
    """
    import matplotlib.pyplot as plt

    if ax is None:
        _, ax = plt.subplots(figsize=(8, 5))

    ax.plot(
        history_df["cycles"],
        history_df["crack_size"] * 1000,  # m -> mm for readability
        linewidth=1.5,
    )
    ax.set_xlabel("Cycles N")
    ax.set_ylabel("Crack size (mm)")
    ax.set_title("Crack Growth — Paris' Law")
    ax.grid(True, alpha=0.3)
    return ax
