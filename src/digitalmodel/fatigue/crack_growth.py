# This module uses py-fatigue (GPL-3.0) for crack growth analysis.
# License implications: any code linking this module inherits GPL-3.0.
# For commercial use, consider reimplementing Paris' law directly.
#
# NOTE: py-fatigue's crack_growth module requires numba jitclass and is
# tightly coupled to its CycleCount/ParisCurve/geometry classes. Due to
# NumPy 2.x incompatibility at time of writing, Paris' law is implemented
# directly using numpy. The math is identical:
#   da/dN = C * (DeltaK)^m
#   DeltaK = Y * Delta_sigma * sqrt(pi * a)
"""
Crack growth analysis using Paris' law.

Provides functions for:
- Paris' law numerical integration (crack size vs cycles)
- Stress intensity factor (SIF) calculation with geometry corrections
- Inspection interval estimation per fracture mechanics principles
- Crack growth curve plotting

Reference: BS 7910:2019, Section 8 — Assessment of flaws by fracture mechanics.
"""

import numpy as np
import pandas as pd


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
    crack_size = np.asarray(crack_size, dtype=float)

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

    delta_k = y_factor * stress_range * np.sqrt(np.pi * crack_size)
    return float(delta_k) if delta_k.ndim == 0 else delta_k


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
    """
    if a_initial >= a_critical:
        raise ValueError("a_initial must be less than a_critical.")
    if a_initial <= 0:
        raise ValueError("a_initial must be positive.")

    if da_step is None:
        da_step = (a_critical - a_initial) / 1000.0

    # Make delta_K callable if scalar
    if callable(delta_K):
        _dk_func = delta_K
    else:
        _dk_val = float(delta_K)
        def _dk_func(a):
            return _dk_val

    a = a_initial
    N = 0.0
    records = []

    while a < a_critical:
        dk = _dk_func(a)
        if dk <= 0:
            break
        da_dN = C * dk ** m
        if da_dN <= 0:
            break

        # cycles for this step
        step = min(da_step, a_critical - a)
        dN = step / da_dN
        records.append({"crack_size": a, "cycles": N, "da_dN": da_dN})

        N += dN
        a += step

    # Final point at critical size
    dk_final = _dk_func(a_critical)
    da_dN_final = C * dk_final ** m if dk_final > 0 else np.inf
    records.append({"crack_size": a_critical, "cycles": N, "da_dN": da_dN_final})

    history = pd.DataFrame(records)
    return N, history


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
    """
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
