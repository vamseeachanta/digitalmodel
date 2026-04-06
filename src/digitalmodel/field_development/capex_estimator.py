# ABOUTME: CAPEX estimation module for offshore host facilities using GoM benchmarks.
# ABOUTME: Issue #1843 — Concept Selection Framework for field_development.
"""
digitalmodel.field_development.capex_estimator
==============================================

Estimates capital expenditure (CAPEX) for offshore host facilities based on
GoM benchmark data from 10 reference fields. Returns a low/base/high USD range
scaled by production capacity and water depth.

GoM Benchmark Data (2024 basis):
- TLP:            $2-6B (Mars 896 m, Ursa 1067 m)
- Spar:           $3-7B (Perdido 2438 m, Whale 2100 m)
- Semi:           $4-10B (Appomattox 2250 m, Thunder Horse 1844 m)
- FPSO:           $5-12B (emerging GoM, Brazil pre-salt analogue)
- Tieback <10 km: $200-500M
- Tieback >20 km: $500M-1.2B

Usage
-----
>>> from digitalmodel.field_development.capex_estimator import estimate_capex
>>> from digitalmodel.field_development.concept_selection import HostType
>>> est = estimate_capex(HostType.TLP, production_capacity_bopd=100_000, water_depth=900)
>>> print(f"${est.base_usd_bn:.1f}B")
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from .concept_selection import HostType


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class CAPEXEstimate:
    """CAPEX estimate for an offshore host facility.

    Attributes
    ----------
    host_type : HostType
        The facility type being estimated.
    low_usd_bn : float
        Low-case CAPEX in USD billions.
    base_usd_bn : float
        Base-case (P50) CAPEX in USD billions.
    high_usd_bn : float
        High-case CAPEX in USD billions.
    basis : str
        Description of benchmark basis and assumptions.
    """

    host_type: HostType
    low_usd_bn: float
    base_usd_bn: float
    high_usd_bn: float
    basis: str


# ---------------------------------------------------------------------------
# Internal benchmark data
# ---------------------------------------------------------------------------

# Base CAPEX (low, base, high) in USD billions at reference conditions
# Reference: 100,000 bopd, 1500 m water depth
_BASE_CAPEX: dict[HostType, tuple[float, float, float]] = {
    HostType.TLP:            (2.0,  4.0,  6.0),
    HostType.SPAR:           (3.0,  5.0,  7.0),
    HostType.SEMI:           (4.0,  7.0, 10.0),
    HostType.FPSO:           (5.0,  8.5, 12.0),
    HostType.SUBSEA_TIEBACK: (0.20, 0.45, 1.2),   # overridden by distance logic
}

_REF_CAPACITY_BOPD = 100_000.0
_REF_DEPTH_M = 1500.0

# Subsea tieback benchmarks keyed by distance band (km)
# (low_bn, base_bn, high_bn)
_TIEBACK_NEAR = (0.10, 0.35, 0.50)   # < 10 km
_TIEBACK_MID  = (0.25, 0.65, 1.00)   # 10-20 km
_TIEBACK_FAR  = (0.50, 0.85, 1.20)   # > 20 km


# ---------------------------------------------------------------------------
# Scaling factors
# ---------------------------------------------------------------------------

def _capacity_scale(capacity_bopd: float) -> float:
    """Non-linear capacity scale factor using a 0.6 power law (economies of scale)."""
    return (capacity_bopd / _REF_CAPACITY_BOPD) ** 0.6


def _depth_scale(depth_m: float, host: HostType) -> float:
    """Depth scale factor relative to reference depth.

    Deeper water increases CAPEX due to longer risers, mooring,
    and installation costs. Factor is ~0.02% per metre difference.
    """
    if host is HostType.SUBSEA_TIEBACK:
        # Tieback depth mainly affects flowline length, not host hull
        return 1.0 + max(0.0, depth_m - _REF_DEPTH_M) * 0.00008
    return 1.0 + max(0.0, depth_m - _REF_DEPTH_M) * 0.00015


def _tieback_capex(distance_km: float) -> tuple[float, float, float]:
    """Return (low, base, high) CAPEX in USD bn for a subsea tieback."""
    if distance_km < 10.0:
        lo, ba, hi = _TIEBACK_NEAR
    elif distance_km <= 20.0:
        # Interpolate between NEAR and FAR based on fraction within 10-20 km
        frac = (distance_km - 10.0) / 10.0
        lo = _TIEBACK_NEAR[0] + frac * (_TIEBACK_FAR[0] - _TIEBACK_NEAR[0])
        ba = _TIEBACK_NEAR[1] + frac * (_TIEBACK_FAR[1] - _TIEBACK_NEAR[1])
        hi = _TIEBACK_NEAR[2] + frac * (_TIEBACK_FAR[2] - _TIEBACK_NEAR[2])
    else:
        # Scale beyond 20 km
        lo, ba, hi = _TIEBACK_FAR
        excess = max(0.0, distance_km - 20.0)
        cost_per_km = 0.020  # $20M/km for longer tieback infrastructure
        lo += excess * cost_per_km * 0.6
        ba += excess * cost_per_km
        hi += excess * cost_per_km * 1.4
    return (round(lo, 3), round(ba, 3), round(hi, 3))


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def estimate_capex(
    host_type: HostType,
    production_capacity_bopd: float,
    water_depth: float,
    tieback_distance_km: Optional[float] = None,
) -> CAPEXEstimate:
    """Estimate CAPEX for an offshore host facility based on GoM benchmarks.

    Parameters
    ----------
    host_type : HostType
        The facility type to estimate (TLP, Spar, Semi, FPSO, Subsea_Tieback).
    production_capacity_bopd : float
        Peak production capacity in barrels of oil per day (must be > 0).
    water_depth : float
        Water depth in metres (must be > 0).
    tieback_distance_km : float, optional
        For Subsea_Tieback only — distance to host in km (must be >= 0).
        Required when host_type is HostType.SUBSEA_TIEBACK.

    Returns
    -------
    CAPEXEstimate
        Low / base / high CAPEX estimates in USD billions.

    Raises
    ------
    ValueError
        If production_capacity_bopd <= 0, water_depth <= 0,
        or tieback_distance_km is missing/negative for tieback type.

    Examples
    --------
    >>> est = estimate_capex(HostType.TLP, 100_000, 900)
    >>> est.low_usd_bn < est.base_usd_bn < est.high_usd_bn
    True
    """
    # --- Validation ---
    if production_capacity_bopd <= 0:
        raise ValueError(
            f"production_capacity_bopd must be positive, got {production_capacity_bopd!r}. "
            "Provide design capacity in barrels per day."
        )
    if water_depth <= 0:
        raise ValueError(
            f"water_depth must be positive, got {water_depth!r}. "
            "Provide water depth in metres."
        )
    if host_type is HostType.SUBSEA_TIEBACK:
        if tieback_distance_km is None:
            raise ValueError(
                "tieback_distance_km is required for Subsea_Tieback host type. "
                "Provide the distance from subsea wells to the host facility in km."
            )
        if tieback_distance_km < 0:
            raise ValueError(
                f"tieback_distance_km must be >= 0, got {tieback_distance_km!r}."
            )

    # --- Estimation ---
    if host_type is HostType.SUBSEA_TIEBACK:
        low, base, high = _tieback_capex(tieback_distance_km)  # type: ignore[arg-type]
        # Scale by production rate (smaller effect for tiebacks)
        cap_factor = (production_capacity_bopd / _REF_CAPACITY_BOPD) ** 0.4
        low  = round(low  * cap_factor, 3)
        base = round(base * cap_factor, 3)
        high = round(high * cap_factor, 3)
        basis = (
            f"GoM subsea tieback benchmark; distance={tieback_distance_km:.1f} km, "
            f"capacity={production_capacity_bopd:,.0f} bopd, depth={water_depth:.0f} m."
        )
    else:
        ref_low, ref_base, ref_high = _BASE_CAPEX[host_type]
        cap_factor = _capacity_scale(production_capacity_bopd)
        dep_factor = _depth_scale(water_depth, host_type)
        low  = round(ref_low  * cap_factor * dep_factor, 3)
        base = round(ref_base * cap_factor * dep_factor, 3)
        high = round(ref_high * cap_factor * dep_factor, 3)
        basis = (
            f"GoM {host_type.value} benchmark (2024 basis); "
            f"ref 100k bopd / 1500 m; capacity_factor={cap_factor:.3f}, "
            f"depth_factor={dep_factor:.3f}. "
            f"Analogues: Perdido Spar $3-7B, Thunder Horse Semi $4-10B, "
            f"Mars TLP $2-6B."
        )

    return CAPEXEstimate(
        host_type=host_type,
        low_usd_bn=low,
        base_usd_bn=base,
        high_usd_bn=high,
        basis=basis,
    )
