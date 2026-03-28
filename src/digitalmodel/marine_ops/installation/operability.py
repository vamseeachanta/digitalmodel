"""Installation operability calculator.

Determines Hs/Tp operational limits for a vessel/structure combination
based on allowable crane tip motions, hook loads, and tilt criteria.

Given a wave scatter diagram, computes percentage operability (weather
window availability) for each vessel.

References:
    DNV-ST-N001 (2021) -- Marine Operations and Marine Warranty
    DNV-RP-H103 (2011) §4, §5 -- Splash Zone and Lowering
"""
from __future__ import annotations

import numpy as np
from typing import Callable
from .models import (
    Vessel,
    Structure,
    InstallationCriteria,
    OperabilityResult,
)
from .crane_tip_motion import (
    crane_tip_raos,
    crane_tip_significant_motion,
    crane_tip_significant_velocity,
)
from .splash_zone import splash_zone_assessment, InstallationCase


def hs_limit_for_criterion(
    vessel: Vessel,
    heading_deg: float,
    criterion_fn: Callable[[float], float],
    allowable: float,
    hs_range: np.ndarray | None = None,
    tp_s: float = 8.0,
) -> float:
    """Find the maximum Hs where a criterion stays within allowable.

    Performs a linear search over Hs values and returns the maximum Hs
    where criterion_fn(Hs) <= allowable.

    Parameters
    ----------
    vessel : Vessel
        Installation vessel.
    heading_deg : float
        Wave heading [deg].
    criterion_fn : callable
        Function mapping Hs -> response value.
    allowable : float
        Maximum allowable response.
    hs_range : np.ndarray, optional
        Hs values to search [m]. Default 0.1 to 5.0 in 0.1 steps.
    tp_s : float
        Peak period for assessment [s].

    Returns
    -------
    float
        Maximum allowable Hs [m]. Returns 0.0 if not feasible.
    """
    if hs_range is None:
        hs_range = np.arange(0.1, 5.05, 0.1)

    hs_limit = 0.0
    for hs in hs_range:
        response = criterion_fn(hs)
        if response <= allowable:
            hs_limit = float(hs)
        else:
            break

    return hs_limit


def compute_operability(
    vessel: Vessel,
    structure: Structure,
    criteria: InstallationCriteria,
    tp_range_s: np.ndarray,
    heading_deg: float = 0.0,
) -> OperabilityResult:
    """Compute Hs limits and governing criterion for a vessel/structure pair.

    For each Tp in the range, finds the maximum Hs satisfying all criteria.
    The governing criterion is whichever gives the lowest limit.

    Parameters
    ----------
    vessel : Vessel
        Installation vessel with RAOs and crane configuration.
    structure : Structure
        Subsea structure being installed.
    criteria : InstallationCriteria
        Allowable limits for the operation.
    tp_range_s : np.ndarray
        Peak periods [s] to evaluate.
    heading_deg : float
        Wave heading [deg] relative to vessel.

    Returns
    -------
    OperabilityResult
        Hs limits, governing criterion, and details.
    """
    # Get crane tip RAOs for this vessel
    tip_raos = crane_tip_raos(vessel)

    # Find heading index (nearest neighbour)
    hdg_idx = int(np.argmin(np.abs(vessel.rao_headings - heading_deg)))

    # Crane tip heave RAO for this heading
    heave_rao = tip_raos["heave"]["amplitude"][:, hdg_idx]

    # Evaluate Hs limits for each criterion across Tp range
    hs_limits_heave = np.zeros(len(tp_range_s))
    hs_limits_velocity = np.zeros(len(tp_range_s))

    for i, tp in enumerate(tp_range_s):
        # Criterion 1: Crane tip heave
        def heave_fn(hs: float, _tp=tp) -> float:
            return crane_tip_significant_motion(
                heave_rao, hs, _tp, vessel.rao_frequencies
            )

        hs_limits_heave[i] = hs_limit_for_criterion(
            vessel, heading_deg, heave_fn, criteria.max_crane_tip_heave_m, tp_s=tp
        )

        # Criterion 2: Crane tip velocity
        def velocity_fn(hs: float, _tp=tp) -> float:
            return crane_tip_significant_velocity(
                heave_rao, hs, _tp, vessel.rao_frequencies
            )

        hs_limits_velocity[i] = hs_limit_for_criterion(
            vessel, heading_deg, velocity_fn, criteria.max_crane_tip_velocity_m_s, tp_s=tp
        )

    # Governing = minimum of all criteria
    hs_combined = np.minimum(hs_limits_heave, hs_limits_velocity)

    # Determine governing criterion (which is more restrictive on average)
    mean_heave_limit = np.mean(hs_limits_heave)
    mean_velocity_limit = np.mean(hs_limits_velocity)

    if mean_heave_limit <= mean_velocity_limit:
        governing = "crane_tip_heave"
    else:
        governing = "crane_tip_velocity"

    # Apply operational contingency factor
    hs_combined *= criteria.alpha_operational

    hs_limit = float(np.min(hs_combined))

    return OperabilityResult(
        hs_limit_m=hs_limit,
        tp_limits_s=hs_combined,
        governing_criterion=governing,
        operability_pct=0.0,  # Set by weather_window_operability()
        details={
            "hs_limits_heave": hs_limits_heave,
            "hs_limits_velocity": hs_limits_velocity,
            "hs_combined": hs_combined,
            "heading_deg": heading_deg,
            "tp_range_s": tp_range_s,
        },
    )


def weather_window_operability(
    operability: OperabilityResult,
    scatter_hs: np.ndarray,
    scatter_tp: np.ndarray,
    scatter_counts: np.ndarray,
) -> float:
    """Compute % operability from a wave scatter diagram.

    Sums occurrences where Hs < Hs_limit for each Tp bin.

    Parameters
    ----------
    operability : OperabilityResult
        Result from compute_operability() with Hs limits per Tp.
    scatter_hs : np.ndarray
        Scatter diagram Hs bin centres [m], shape (n_hs,).
    scatter_tp : np.ndarray
        Scatter diagram Tp bin centres [s], shape (n_tp,).
    scatter_counts : np.ndarray
        Occurrence counts, shape (n_hs, n_tp).

    Returns
    -------
    float
        Operability percentage (0–100).
    """
    total = np.sum(scatter_counts)
    if total == 0:
        return 0.0

    operable = 0.0
    tp_limits = operability.details.get("tp_range_s", np.array([]))
    hs_limits = operability.tp_limits_s

    for j, tp_bin in enumerate(scatter_tp):
        # Interpolate Hs limit at this Tp
        if len(tp_limits) > 0:
            hs_lim = float(np.interp(tp_bin, tp_limits, hs_limits))
        else:
            hs_lim = operability.hs_limit_m

        for i, hs_bin in enumerate(scatter_hs):
            if hs_bin <= hs_lim:
                operable += scatter_counts[i, j]

    return 100.0 * operable / total
