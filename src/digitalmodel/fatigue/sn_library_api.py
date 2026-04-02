"""
S-N Curve Library API — Programmatic Access to 221 Curves
==========================================================

High-level API wrapping :mod:`sn_library` to provide a clean, typed interface
for listing, querying, and computing with the full S-N curve catalogue.

This module exposes:

- :func:`list_curves` — list/filter the 221-curve catalogue
- :func:`get_curve` — look up one curve by ``curve_id``
- :func:`calculate_endurance` — allowable cycles for a stress range
- :func:`compare_curves` — multi-curve comparison data (plot-ready)

All models are Pydantic v2 for type safety and serialisation.

Issue: #1676 (P0 — fatigue expansion)

References
----------
- DNV-RP-C203 (2021), Fatigue Design of Offshore Steel Structures
- BS 7608:2014, Fatigue design and assessment of steel structures
- API RP 2A-WSD (2014), Section 5
"""

from typing import List, Optional, Union

import numpy as np
from pydantic import BaseModel, Field

from .sn_library import (
    SNCurveRecord,
    get_catalog,
    get_library_curve,
    search_curves as _search_curves,
)


# ---------------------------------------------------------------------------
# Pydantic API models
# ---------------------------------------------------------------------------

class CurveInfo(BaseModel):
    """Lightweight summary of an S-N curve for listing/filtering.

    Attributes
    ----------
    curve_id : str
        Unique identifier (e.g. ``"DNV-RP-C203:D:air"``).
    standard : str
        Source standard name.
    weld_class : str
        Detail category or FAT class label.
    environment : str
        ``"air"``, ``"seawater_cp"``, or ``"free_corrosion"``.
    m1 : float
        Primary (high-stress) slope.
    endurance_limit : float | None
        CAFL in MPa, or None if single-slope.
    """

    curve_id: str
    standard: str
    weld_class: str
    environment: str
    m1: float
    endurance_limit: Optional[float] = None


class SNCurve(BaseModel):
    """Full S-N curve parameters for engineering calculations.

    Attributes
    ----------
    curve_id : str
        Unique identifier.
    standard : str
        Source standard name.
    weld_class : str
        Detail category / FAT class.
    environment : str
        Exposure environment.
    m1 : float
        Negative inverse slope, first segment.
    log_a1 : float
        log10(intercept) for first segment.
    m2 : float | None
        Slope of second segment (beyond knee). None for single-slope.
    log_a2 : float | None
        Intercept of second segment.
    knee_point : float
        Cycle count at slope change (N_D).
    endurance_limit : float | None
        CAFL in MPa at the knee.
    thickness_ref : float
        Reference thickness for correction (mm).
    thickness_exponent : float
        Thickness correction exponent k.
    note : str
        Free-text description.
    """

    curve_id: str
    standard: str
    weld_class: str = ""
    environment: str = "air"
    m1: float
    log_a1: float
    m2: Optional[float] = None
    log_a2: Optional[float] = None
    knee_point: float = 1e7
    endurance_limit: Optional[float] = None
    thickness_ref: float = 25.0
    thickness_exponent: float = 0.25
    note: str = ""


class ComparisonData(BaseModel):
    """Multi-curve comparison data, ready for plotting.

    Attributes
    ----------
    curve_names : list[str]
        Curve IDs in order.
    stress_ranges : object
        The stress range array (stored as list for serialisation).
    endurance_cycles : list
        List of endurance-cycle arrays, one per curve.
    """

    curve_names: List[str]
    stress_ranges: List[float] = Field(default_factory=list)
    endurance_cycles: List  # list of np.ndarray — serialised as lists

    class Config:
        arbitrary_types_allowed = True


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _record_to_curve_info(rec: SNCurveRecord) -> CurveInfo:
    """Convert an SNCurveRecord to a CurveInfo summary."""
    return CurveInfo(
        curve_id=rec.curve_id,
        standard=rec.standard,
        weld_class=rec.curve_class,
        environment=rec.environment,
        m1=rec.m1,
        endurance_limit=rec.endurance_limit,
    )


def _record_to_sncurve(rec: SNCurveRecord) -> SNCurve:
    """Convert an SNCurveRecord to a full SNCurve model."""
    return SNCurve(
        curve_id=rec.curve_id,
        standard=rec.standard,
        weld_class=rec.curve_class,
        environment=rec.environment,
        m1=rec.m1,
        log_a1=rec.log_a1,
        m2=rec.m2,
        log_a2=rec.log_a2,
        knee_point=rec.n_transition,
        endurance_limit=rec.endurance_limit,
        thickness_ref=rec.thickness_ref,
        thickness_exponent=rec.thickness_exponent,
        note=rec.note,
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def list_curves(
    standard: Optional[str] = None,
    weld_class: Optional[str] = None,
) -> List[CurveInfo]:
    """List available S-N curves, optionally filtered.

    Parameters
    ----------
    standard : str, optional
        Filter by standard name (e.g. ``"DNV-RP-C203"``).
    weld_class : str, optional
        Filter by weld/detail class (e.g. ``"D"``, ``"F2"``).

    Returns
    -------
    list[CurveInfo]
        Lightweight summaries of matching curves.

    Examples
    --------
    >>> len(list_curves())  # all 221 curves
    221
    >>> len(list_curves(standard="BS 7608"))  # 20 BS 7608 curves
    20
    """
    records = _search_curves(standard=standard, curve_class=weld_class)
    return [_record_to_curve_info(r) for r in records]


def get_curve(curve_id: str) -> SNCurve:
    """Look up a single S-N curve by its unique ID.

    Parameters
    ----------
    curve_id : str
        Unique identifier, e.g. ``"DNV-RP-C203:D:air"``.

    Returns
    -------
    SNCurve
        Full curve parameters.

    Raises
    ------
    KeyError
        If the curve_id is not in the library.

    Examples
    --------
    >>> c = get_curve("DNV-RP-C203:D:air")
    >>> c.m1
    3.0
    """
    rec = get_library_curve(curve_id)
    return _record_to_sncurve(rec)


def calculate_endurance(
    curve: SNCurve,
    stress_range: Union[float, np.ndarray],
) -> Union[float, np.ndarray]:
    """Calculate allowable cycles N for a given stress range.

    Uses bi-linear log-log model::

        if S > S_D:  N = 10^(log_a1) · S^(-m1)
        else:        N = 10^(log_a2) · S^(-m2)   (if m2 defined)

    For single-slope curves, one slope is used throughout.

    Parameters
    ----------
    curve : SNCurve
        S-N curve parameters.
    stress_range : float or np.ndarray
        Applied stress range(s) in MPa.

    Returns
    -------
    float or np.ndarray
        Allowable number of cycles. ``inf`` for zero/negative stress.

    Examples
    --------
    >>> c = get_curve("DNV-RP-C203:D:air")
    >>> calculate_endurance(c, 100.0)  # ≈ 1.46e6
    """
    S = np.asarray(stress_range, dtype=float)
    scalar = S.ndim == 0
    S = np.atleast_1d(S)

    N = np.full_like(S, np.inf, dtype=float)
    mask_pos = S > 0

    if curve.endurance_limit is not None and curve.m2 is not None and curve.log_a2 is not None:
        high = mask_pos & (S >= curve.endurance_limit)
        low = mask_pos & (S < curve.endurance_limit)
        N[high] = 10 ** curve.log_a1 * S[high] ** (-curve.m1)
        N[low] = 10 ** curve.log_a2 * S[low] ** (-curve.m2)
    else:
        N[mask_pos] = 10 ** curve.log_a1 * S[mask_pos] ** (-curve.m1)

    return float(N[0]) if scalar else N


def compare_curves(
    names: List[str],
    stress_ranges: np.ndarray,
) -> ComparisonData:
    """Generate comparison data for multiple S-N curves.

    Parameters
    ----------
    names : list[str]
        Curve IDs to compare.
    stress_ranges : np.ndarray
        Array of stress range values (MPa) for the comparison.

    Returns
    -------
    ComparisonData
        Contains curve names, shared stress ranges, and per-curve
        endurance cycle arrays.

    Raises
    ------
    KeyError
        If any curve name is not found.

    Examples
    --------
    >>> data = compare_curves(
    ...     ["DNV-RP-C203:D:air", "DNV-RP-C203:E:air"],
    ...     np.linspace(10, 300, 50),
    ... )
    >>> len(data.endurance_cycles)  # one array per curve
    2
    """
    stress_ranges = np.asarray(stress_ranges, dtype=float)
    curves = [get_curve(name) for name in names]
    endurance_arrays = [calculate_endurance(c, stress_ranges) for c in curves]

    return ComparisonData(
        curve_names=list(names),
        stress_ranges=stress_ranges.tolist(),
        endurance_cycles=endurance_arrays,
    )
