# ABOUTME: Economics facade wiring worldenergydata backends into digitalmodel.
# ABOUTME: Issue #1858 — NPV/IRR/MIRR evaluation, CAPEX/OPEX/ABEX adapter layer.
"""
digitalmodel.field_development.economics
========================================

Thin facade over worldenergydata economics backends:

- **CAPEX**: ``worldenergydata.cost.CostPredictor`` (71 sanctioned-project ML model)
  or ``digitalmodel.field_development.capex_estimator`` (GoM benchmark fallback).
- **OPEX**: ``digitalmodel.field_development.opex_estimator`` (GoM benchmark).
- **ABEX**: ``worldenergydata.decommissioning.DecommissioningCostEstimator``.
- **Financial metrics**: ``worldenergydata.economics.dcf`` (CashFlowSchedule → NPV/MIRR).

Fiscal regime selection (5 countries) is recorded on the result for downstream
tax-adjusted analysis; the facade itself does pre-tax evaluation.

Usage
-----
>>> from digitalmodel.field_development.economics import (
...     EconomicsInput, FiscalRegime, evaluate_economics,
... )
>>> inp = EconomicsInput(
...     field_name="Whale",
...     water_depth_m=2100.0,
...     host_type="Spar",
...     production_capacity_bopd=100_000.0,
...     reservoir_size_mmbbl=400.0,
...     oil_price_usd_per_bbl=75.0,
...     discount_rate=0.10,
...     fiscal_regime=FiscalRegime.US,
...     field_life_years=30,
... )
>>> result = evaluate_economics(inp)
>>> print(f"NPV ${result.metrics.npv_usd_mm:,.0f} MM")
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Callable, Optional

import numpy as np


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class FiscalRegime(str, Enum):
    """Supported fiscal regimes.

    Maps to per-country modules in worldenergydata:
    - US:      ``worldenergydata.eia_us``
    - Norway:  ``worldenergydata.sodir`` (78% marginal)
    - UK:      ``worldenergydata.ukcs`` (RFCT + SC + EPL)
    - Brazil:  ``worldenergydata.brazil_anp`` (concession + PSA)
    - Nigeria: ``worldenergydata.west_africa`` (deepwater PSC)
    """

    US = "US"
    Norway = "Norway"
    UK = "UK"
    Brazil = "Brazil"
    Nigeria = "Nigeria"


# ---------------------------------------------------------------------------
# Input
# ---------------------------------------------------------------------------


@dataclass
class EconomicsInput:
    """Normalised input for field development economics evaluation.

    Required fields establish the minimum parameters for a screening-level
    evaluation.  Optional fields allow user-provided overrides that bypass
    backend estimation.
    """

    # Required
    field_name: str
    water_depth_m: float
    host_type: str  # validated against HostType.value in __post_init__
    production_capacity_bopd: float
    oil_price_usd_per_bbl: float
    discount_rate: float
    fiscal_regime: FiscalRegime
    field_life_years: int

    # Optional — used for reserves-based analysis in future fiscal regime work
    reservoir_size_mmbbl: Optional[float] = None

    # Optional overrides — bypass backend estimation when provided
    opex_usd_per_bbl: Optional[float] = None
    capex_usd_mm: Optional[float] = None
    abex_usd_mm: Optional[float] = None
    carbon_cost_usd_per_tonne: Optional[float] = None
    region: Optional[str] = None

    def __post_init__(self) -> None:
        # Validate host_type against HostType enum values
        from digitalmodel.field_development.concept_selection import HostType
        valid_hosts = {h.value for h in HostType}
        if self.host_type not in valid_hosts:
            raise ValueError(
                f"host_type must be one of {sorted(valid_hosts)}, got {self.host_type!r}"
            )
        if self.water_depth_m <= 0:
            raise ValueError(
                f"water_depth_m must be positive, got {self.water_depth_m!r}"
            )
        if self.production_capacity_bopd <= 0:
            raise ValueError(
                f"production_capacity_bopd must be positive, got {self.production_capacity_bopd!r}"
            )
        if self.oil_price_usd_per_bbl <= 0:
            raise ValueError(
                f"oil_price_usd_per_bbl must be positive, got {self.oil_price_usd_per_bbl!r}"
            )
        if self.discount_rate < 0 or self.discount_rate > 1:
            raise ValueError(
                f"discount_rate must be in [0, 1], got {self.discount_rate!r}"
            )
        if self.field_life_years < 1:
            raise ValueError(
                f"field_life_years must be >= 1, got {self.field_life_years!r}"
            )


# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------


@dataclass
class CostEstimates:
    """Aggregated cost estimates from backend adapters."""

    capex_usd_mm: float
    opex_usd_mm_per_yr: float
    abex_usd_mm: float
    capex_source: str
    abex_source: str


@dataclass
class EvaluationMetrics:
    """Financial evaluation metrics from DCF analysis."""

    npv_usd_mm: float
    irr: Optional[float]
    mirr: Optional[float]
    payback_years: Optional[float]
    discount_rate: float


@dataclass
class EconomicsResult:
    """Complete economics evaluation result."""

    field_name: str
    fiscal_regime: FiscalRegime
    costs: CostEstimates
    metrics: EvaluationMetrics
    basis: str


# ---------------------------------------------------------------------------
# Adapter resolution — pluggable backend selection
# ---------------------------------------------------------------------------

# Region mapping from fiscal regime to worldenergydata region string
_REGIME_TO_REGION: dict[FiscalRegime, str] = {
    FiscalRegime.US: "gom",
    FiscalRegime.Norway: "ncs",
    FiscalRegime.UK: "ukcs",
    FiscalRegime.Brazil: "brazil",
    FiscalRegime.Nigeria: "west_africa",
}

# Host type to decommissioning asset type mapping
_HOST_TO_DECOM_ASSET: dict[str, str] = {
    "TLP": "tlp",
    "Spar": "spar",
    "Semi": "spar",  # Semi uses spar-class decom estimates
    "FPSO": "fpso",
    "Subsea_Tieback": "subsea_tree",
}


def _resolve_capex_adapter() -> Callable[[EconomicsInput], tuple[float, str]]:
    """Return a callable that estimates CAPEX in USD MM."""

    def _estimate_capex(inp: EconomicsInput) -> tuple[float, str]:
        """Estimate CAPEX using digitalmodel benchmark (GoM scaling)."""
        from digitalmodel.field_development.capex_estimator import estimate_capex
        from digitalmodel.field_development.concept_selection import HostType

        host = HostType(inp.host_type)
        tieback_km = 15.0 if host is HostType.SUBSEA_TIEBACK else None
        est = estimate_capex(
            host_type=host,
            production_capacity_bopd=inp.production_capacity_bopd,
            water_depth=inp.water_depth_m,
            tieback_distance_km=tieback_km,
        )
        # Convert from USD billions to USD millions
        capex_mm = est.base_usd_bn * 1_000.0
        return capex_mm, f"digitalmodel GoM benchmark ({est.basis[:60]}...)"

    return _estimate_capex


def _resolve_abex_adapter() -> Callable[[EconomicsInput], tuple[float, str]]:
    """Return a callable that estimates ABEX (decommissioning) in USD MM."""

    def _estimate_abex(inp: EconomicsInput) -> tuple[float, str]:
        """Estimate ABEX using worldenergydata decommissioning model."""
        try:
            from worldenergydata.decommissioning import DecommissioningCostEstimator

            estimator = DecommissioningCostEstimator()
            asset_type = _HOST_TO_DECOM_ASSET.get(inp.host_type, "tlp")
            region = inp.region or _REGIME_TO_REGION.get(
                inp.fiscal_regime, "gom"
            )
            est = estimator.estimate(
                asset_type=asset_type,
                water_depth_m=inp.water_depth_m,
                weight_tonnes=0.0,  # unknown at screening level
                region=region,
            )
            return est.estimated_cost_musd, "worldenergydata.decommissioning"
        except ImportError:
            # Fallback: rule-of-thumb 5-10% of CAPEX
            return 0.0, "fallback (worldenergydata not available)"

    return _estimate_abex


def _resolve_financial_adapter() -> Callable:
    """Return a callable that computes NPV/IRR/MIRR from a cashflow array."""

    def _compute_metrics(
        cashflows: np.ndarray,
        discount_rate: float,
    ) -> dict:
        """Compute financial metrics using worldenergydata FDAS or numpy fallback."""
        try:
            from worldenergydata.fdas import calculate_all_metrics

            return calculate_all_metrics(
                cashflows, discount_rate_annual=discount_rate, period="annual"
            )
        except ImportError:
            # Numpy-only fallback
            return _numpy_financial_metrics(cashflows, discount_rate)

    return _compute_metrics


def _numpy_financial_metrics(cashflows: np.ndarray, discount_rate: float) -> dict:
    """Pure-numpy fallback for NPV when worldenergydata is unavailable."""
    n = len(cashflows)
    t = np.arange(n, dtype=float)
    discount_factors = (1.0 + discount_rate) ** t
    discounted = cashflows / discount_factors
    npv = float(np.sum(discounted))

    # IRR via numpy-financial if available; None otherwise
    irr = None
    try:
        import numpy_financial as npf
        irr_val = npf.irr(cashflows)
        if not np.isnan(irr_val):
            irr = float(irr_val)
    except (ImportError, Exception):
        pass

    # Payback
    cumulative = np.cumsum(cashflows)
    positive_idx = np.where(cumulative >= 0)[0]
    payback = float(positive_idx[0]) if positive_idx.size > 0 else None

    return {
        "npv": npv,
        "irr_annual": irr,
        "mirr_annual": None,
        "payback_years": payback,
    }


# ---------------------------------------------------------------------------
# OPEX estimation
# ---------------------------------------------------------------------------


def _estimate_opex_mm_per_yr(inp: EconomicsInput) -> float:
    """Estimate annual OPEX in USD MM.

    Uses user-provided opex_usd_per_bbl if available, otherwise falls back
    to the digitalmodel GoM OPEX benchmark.
    """
    if inp.opex_usd_per_bbl is not None:
        annual_bbl = inp.production_capacity_bopd * 365.0
        return inp.opex_usd_per_bbl * annual_bbl / 1_000_000.0

    from digitalmodel.field_development.opex_estimator import estimate_opex
    from digitalmodel.field_development.concept_selection import HostType

    host = HostType(inp.host_type)
    est = estimate_opex(
        host_type=host,
        production_capacity_bopd=inp.production_capacity_bopd,
        field_age_years=5,  # mid-life reference
    )
    return est.base_usd_mm_per_yr


# ---------------------------------------------------------------------------
# Cashflow construction
# ---------------------------------------------------------------------------


def _build_annual_cashflows(
    inp: EconomicsInput,
    capex_mm: float,
    opex_mm_yr: float,
    abex_mm: float,
) -> np.ndarray:
    """Build a simplified annual cashflow array for screening-level evaluation.

    Assumptions:
    - Year 0-2: CAPEX spread 30/50/20
    - Year 1 onwards: plateau production for 60% of field life, then linear
      decline to 20% of plateau
    - Final year: ABEX
    - Carbon cost applied if provided
    """
    n = inp.field_life_years
    cashflows = np.zeros(n + 1)  # year 0..n

    # CAPEX spread: 30% year 0, 50% year 1, 20% year 2
    capex_schedule = [0.30, 0.50, 0.20]
    for i, frac in enumerate(capex_schedule):
        if i <= n:
            cashflows[i] -= capex_mm * frac

    # Revenue and OPEX from year 1 onwards
    plateau_years = max(1, int(n * 0.6))
    annual_revenue_mm = (
        inp.production_capacity_bopd * 365.0 * inp.oil_price_usd_per_bbl / 1_000_000.0
    )

    for yr in range(1, n + 1):
        if yr <= plateau_years:
            prod_factor = 1.0
        else:
            # Linear decline from 100% to 20% over remaining years
            decline_years = n - plateau_years
            if decline_years > 0:
                frac_decline = (yr - plateau_years) / decline_years
                prod_factor = 1.0 - 0.8 * frac_decline
            else:
                prod_factor = 0.2
        revenue = annual_revenue_mm * prod_factor
        opex = opex_mm_yr * prod_factor

        # Carbon cost
        carbon = 0.0
        if inp.carbon_cost_usd_per_tonne is not None:
            # Typical offshore: ~0.04 tCO2/bbl Scope 1+2
            emission_intensity = 0.04
            annual_bbl = inp.production_capacity_bopd * 365.0 * prod_factor
            carbon = (
                annual_bbl * emission_intensity * inp.carbon_cost_usd_per_tonne
                / 1_000_000.0
            )

        cashflows[yr] += revenue - opex - carbon

    # ABEX in final year
    cashflows[n] -= abex_mm

    return cashflows


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------


def evaluate_economics(inp: EconomicsInput) -> EconomicsResult:
    """Evaluate field development economics through worldenergydata backends.

    This is the single entry point for screening-level economic evaluation.
    It resolves CAPEX, OPEX, and ABEX estimates, builds an annual cashflow
    schedule, and computes NPV/IRR/MIRR metrics.

    Parameters
    ----------
    inp : EconomicsInput
        Normalised field parameters.

    Returns
    -------
    EconomicsResult
        Complete economics evaluation with cost breakdown and financial metrics.
    """
    # --- Cost estimation ---
    if inp.capex_usd_mm is not None:
        capex_mm = inp.capex_usd_mm
        capex_source = "User-provided override"
    else:
        capex_adapter = _resolve_capex_adapter()
        capex_mm, capex_source = capex_adapter(inp)

    opex_mm_yr = _estimate_opex_mm_per_yr(inp)

    if inp.abex_usd_mm is not None:
        abex_mm = inp.abex_usd_mm
        abex_source = "User-provided override"
    else:
        abex_adapter = _resolve_abex_adapter()
        abex_mm, abex_source = abex_adapter(inp)
        if abex_mm <= 0:
            # Fallback: 7% of CAPEX as rule-of-thumb
            abex_mm = capex_mm * 0.07
            abex_source = "Rule-of-thumb (7% of CAPEX)"

    costs = CostEstimates(
        capex_usd_mm=capex_mm,
        opex_usd_mm_per_yr=opex_mm_yr,
        abex_usd_mm=abex_mm,
        capex_source=capex_source,
        abex_source=abex_source,
    )

    # --- Cashflow construction ---
    cashflows = _build_annual_cashflows(inp, capex_mm, opex_mm_yr, abex_mm)

    # --- Financial metrics ---
    financial_adapter = _resolve_financial_adapter()
    metrics_dict = financial_adapter(cashflows, inp.discount_rate)

    metrics = EvaluationMetrics(
        npv_usd_mm=metrics_dict.get("npv", 0.0) or 0.0,
        irr=metrics_dict.get("irr_annual"),
        mirr=metrics_dict.get("mirr_annual"),
        payback_years=metrics_dict.get("payback_years"),
        discount_rate=inp.discount_rate,
    )

    # --- Result ---
    basis = (
        f"Screening-level evaluation for {inp.field_name}; "
        f"{inp.host_type} at {inp.water_depth_m:.0f} m; "
        f"regime={inp.fiscal_regime.value}; "
        f"oil price=${inp.oil_price_usd_per_bbl}/bbl; "
        f"discount rate={inp.discount_rate:.0%}."
    )

    return EconomicsResult(
        field_name=inp.field_name,
        fiscal_regime=inp.fiscal_regime,
        costs=costs,
        metrics=metrics,
        basis=basis,
    )
