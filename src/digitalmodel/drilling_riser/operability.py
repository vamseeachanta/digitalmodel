"""Drilling riser operability calculations — RAO-based limits, scatter analysis.

Implements operability envelope analysis per 2H-TNE-0050-03 §5.2,
RAO-based significant wave height limits, and watch circle calculations.
"""

from __future__ import annotations


def significant_wave_height_limit(
    rao_amplitude: float,
    allowable_motion_m: float,
) -> float:
    """Calculate limiting significant wave height from RAO and motion limit.

    Hs_limit = allowable_motion / RAO_amplitude

    Per 2H-TNE-0050-03 §5.2 operating envelope analysis.

    Parameters
    ----------
    rao_amplitude : float
        Response amplitude operator at critical period [m/m].
    allowable_motion_m : float
        Maximum allowable motion amplitude [m].

    Returns
    -------
    float
        Limiting significant wave height [m].
    """
    return allowable_motion_m / rao_amplitude


def operability_fraction(
    hs_scatter: list[float],
    hs_limit: float,
) -> float:
    """Calculate fraction of sea states within operability limit.

    Counts sea states with Hs <= Hs_limit divided by total.

    Parameters
    ----------
    hs_scatter : list[float]
        Significant wave heights from scatter diagram [m].
    hs_limit : float
        Limiting significant wave height [m].

    Returns
    -------
    float
        Operability fraction [0-1].
    """
    if not hs_scatter:
        return 0.0
    count_operable = sum(1 for hs in hs_scatter if hs <= hs_limit)
    return count_operable / len(hs_scatter)


def watch_circle_radius_m(
    offset_pct: float,
    water_depth_m: float,
) -> float:
    """Calculate allowable watch circle radius.

    radius = (offset_pct / 100) * water_depth

    Parameters
    ----------
    offset_pct : float
        Allowable offset as percentage of water depth [%].
    water_depth_m : float
        Water depth [m].

    Returns
    -------
    float
        Watch circle radius [m].
    """
    return (offset_pct / 100.0) * water_depth_m
