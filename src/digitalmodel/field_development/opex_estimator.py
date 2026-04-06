# ABOUTME: OPEX estimation module for offshore host facilities using GoM benchmarks.
# ABOUTME: Issue #1843 — Concept Selection Framework for field_development.
"""
digitalmodel.field_development.opex_estimator
=============================================

Estimates annual operating expenditure (OPEX) for offshore host facilities
based on GoM benchmark data and industry heuristics.

GoM OPEX Benchmarks (2024 basis):
- Deepwater GoM OPEX/bbl: $15-50/bbl depending on host type and field age
- TLP:   $80-250M/yr typical for 100k bopd field
- Spar:  $100-300M/yr
- Semi:  $120-400M/yr (larger crew, higher lease cost)
- FPSO:  $150-500M/yr (lease/manning dominates)
- Tieback: $20-80M/yr incremental to host OPEX

Field age effect: maintenance costs rise ~2% per year after year 5.

Usage
-----
>>> from digitalmodel.field_development.opex_estimator import estimate_opex
>>> from digitalmodel.field_development.concept_selection import HostType
>>> est = estimate_opex(HostType.TLP, production_capacity_bopd=100_000, field_age_years=5)
>>> print(f"${est.base_usd_mm_per_yr:.0f}M/yr")
"""

from __future__ import annotations

from dataclasses import dataclass

from .concept_selection import HostType


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class OPEXEstimate:
    """Annual OPEX estimate for an offshore host facility.

    Attributes
    ----------
    host_type : HostType
        The facility type being estimated.
    low_usd_mm_per_yr : float
        Low-case annual OPEX in USD millions.
    base_usd_mm_per_yr : float
        Base-case (P50) annual OPEX in USD millions.
    high_usd_mm_per_yr : float
        High-case annual OPEX in USD millions.
    opex_per_bbl_usd : float
        Base-case OPEX per barrel of oil equivalent ($/bbl).
    basis : str
        Description of benchmark basis and assumptions.
    """

    host_type: HostType
    low_usd_mm_per_yr: float
    base_usd_mm_per_yr: float
    high_usd_mm_per_yr: float
    opex_per_bbl_usd: float
    basis: str


# ---------------------------------------------------------------------------
# Internal benchmark data
# ---------------------------------------------------------------------------

# Base OPEX (low, base, high) in USD millions/year at reference conditions:
# 100,000 bopd, field age = 5 years
_BASE_OPEX_MM_YR: dict[HostType, tuple[float, float, float]] = {
    HostType.TLP:            (80.0,  160.0,  260.0),
    HostType.SPAR:           (90.0,  175.0,  290.0),
    HostType.SEMI:           (110.0, 210.0,  360.0),
    HostType.FPSO:           (140.0, 270.0,  480.0),
    HostType.SUBSEA_TIEBACK: (20.0,  50.0,   90.0),
}

_REF_CAPACITY_BOPD = 100_000.0

# OPEX aging: annual increase factor in maintenance/inspection costs
# Applied as compounding from year 5 onward (new field has reduced complexity)
_AGING_RATE_PER_YEAR = 0.025   # 2.5% per year beyond year 5
_AGING_REFERENCE_AGE = 5       # years


# ---------------------------------------------------------------------------
# Scaling helpers
# ---------------------------------------------------------------------------

def _capacity_opex_scale(capacity_bopd: float) -> float:
    """OPEX scales sub-linearly with capacity (economies of scale): power 0.7."""
    return (capacity_bopd / _REF_CAPACITY_BOPD) ** 0.7


def _age_factor(field_age_years: int) -> float:
    """Compound aging factor for maintenance/integrity management costs.

    New fields (age 0-5): factor near 1.0.
    Beyond year 5: compounds at _AGING_RATE_PER_YEAR.
    """
    if field_age_years <= _AGING_REFERENCE_AGE:
        # Slight reduction for brand-new facilities (commissioning optimism)
        return max(0.85, 1.0 - 0.03 * (5 - field_age_years))
    excess = field_age_years - _AGING_REFERENCE_AGE
    return (1.0 + _AGING_RATE_PER_YEAR) ** excess


def _opex_per_bbl(base_mm_yr: float, capacity_bopd: float) -> float:
    """Calculate $/bbl from annual OPEX (USD MM/yr) and production capacity."""
    annual_bbl = capacity_bopd * 365.0
    return (base_mm_yr * 1_000_000) / annual_bbl


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def estimate_opex(
    host_type: HostType,
    production_capacity_bopd: float,
    field_age_years: int,
) -> OPEXEstimate:
    """Estimate annual OPEX for an offshore host facility based on GoM benchmarks.

    Parameters
    ----------
    host_type : HostType
        The facility type (TLP, Spar, Semi, FPSO, Subsea_Tieback).
    production_capacity_bopd : float
        Production capacity in barrels of oil per day (must be > 0).
    field_age_years : int
        Age of the field/facility in years (must be >= 0).
        Year 0 = greenfield startup.

    Returns
    -------
    OPEXEstimate
        Low / base / high annual OPEX in USD millions, plus $/bbl metric.

    Raises
    ------
    ValueError
        If production_capacity_bopd <= 0 or field_age_years < 0.

    Examples
    --------
    >>> est = estimate_opex(HostType.TLP, 100_000, 5)
    >>> est.base_usd_mm_per_yr > 0
    True
    """
    # --- Validation ---
    if production_capacity_bopd <= 0:
        raise ValueError(
            f"production_capacity_bopd must be positive, got {production_capacity_bopd!r}. "
            "Provide design capacity in barrels per day."
        )
    if field_age_years < 0:
        raise ValueError(
            f"field_age_years must be >= 0, got {field_age_years!r}. "
            "Use 0 for a new greenfield development."
        )

    # --- Estimation ---
    ref_low, ref_base, ref_high = _BASE_OPEX_MM_YR[host_type]
    cap_factor = _capacity_opex_scale(production_capacity_bopd)
    age_fac    = _age_factor(field_age_years)

    low  = round(ref_low  * cap_factor * age_fac, 2)
    base = round(ref_base * cap_factor * age_fac, 2)
    high = round(ref_high * cap_factor * age_fac, 2)

    per_bbl = round(_opex_per_bbl(base, production_capacity_bopd), 2)

    basis = (
        f"GoM {host_type.value} OPEX benchmark (2024 basis); "
        f"ref 100k bopd, age {_AGING_REFERENCE_AGE} yr; "
        f"capacity_factor={cap_factor:.3f}, age_factor={age_fac:.3f}. "
        f"Typical GoM deepwater OPEX: $15-50/bbl. "
        f"Sources: BSEE production data, EIA GoM operator cost surveys."
    )

    return OPEXEstimate(
        host_type=host_type,
        low_usd_mm_per_yr=low,
        base_usd_mm_per_yr=base,
        high_usd_mm_per_yr=high,
        opex_per_bbl_usd=per_bbl,
        basis=basis,
    )
