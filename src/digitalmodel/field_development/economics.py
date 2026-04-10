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


class DeclineType(str, Enum):
    """Production decline curve type after plateau period.

    Controls how production tapers from plateau rate to end-of-life:
    - EXPONENTIAL: q(t) = q_i * exp(-D*t)
    - HYPERBOLIC:  q(t) = q_i / (1 + b*D*t)^(1/b)
    - HARMONIC:    q(t) = q_i / (1 + D*t)       (hyperbolic with b=1)
    - LINEAR:      straight-line decline to 20% of plateau (legacy default)
    """

    EXPONENTIAL = "exponential"
    HYPERBOLIC = "hyperbolic"
    HARMONIC = "harmonic"
    LINEAR = "linear"


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
# EUR-based decline rate derivation
# ---------------------------------------------------------------------------


def _derive_decline_rate_from_eur(
    reservoir_size_mmbbl: float,
    production_capacity_bopd: float,
    field_life_years: int,
    recovery_factor: float = 0.15,
) -> float:
    """Derive exponential decline rate D so cumulative production ≈ EUR.

    EUR = reservoir_size_mmbbl * recovery_factor.  Plateau is 60% of field
    life at full rate; D is chosen so the post-plateau exponential tail
    constrains total cumulative to EUR.  Uses bisection (50 iterations).
    """
    annual_prod = production_capacity_bopd * 365.0 / 1e6  # MMbbl/yr
    eur = reservoir_size_mmbbl * recovery_factor
    plateau_years = max(1, int(field_life_years * 0.6))
    decline_years = field_life_years - plateau_years
    plateau_cum = annual_prod * plateau_years

    if decline_years <= 0 or plateau_cum >= eur:
        return 2.0  # fast decline — EUR already met or exceeded in plateau

    target_decline_cum = eur - plateau_cum
    max_decline_cum = annual_prod * decline_years

    if target_decline_cum >= max_decline_cum:
        return 0.01  # very slow decline — need nearly full production

    # Bisection: find D where annual_prod * Σ exp(-D*t) = target_decline_cum
    target_sum = target_decline_cum / annual_prod
    t = np.arange(1, decline_years + 1, dtype=float)
    d_low, d_high = 0.001, 5.0
    for _ in range(50):
        d_mid = (d_low + d_high) / 2.0
        cum = float(np.sum(np.exp(-d_mid * t)))
        if cum > target_sum:
            d_low = d_mid
        else:
            d_high = d_mid
    return (d_low + d_high) / 2.0


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

    # Decline curve parameters (default preserves legacy linear behaviour)
    # decline_rate=None means "not explicitly provided by caller"; auto-derive
    # fires only in that case, preventing silent mutation of explicit choices.
    decline_type: DeclineType = DeclineType.LINEAR
    decline_rate: Optional[float] = None
    b_factor: float = 0.5

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
        # EUR-based decline rate derivation — only fires when caller did NOT
        # explicitly provide decline_rate (None sentinel).  An explicit
        # decline_rate=0.0 is preserved as-is and will fail later validation
        # for non-LINEAR types, giving the caller a clear error.
        if (
            self.reservoir_size_mmbbl is not None
            and self.decline_rate is None
            and self.decline_type == DeclineType.LINEAR
        ):
            self.decline_type = DeclineType.EXPONENTIAL
            self.decline_rate = _derive_decline_rate_from_eur(
                self.reservoir_size_mmbbl,
                self.production_capacity_bopd,
                self.field_life_years,
            )

        # For LINEAR with no rate provided: default to 0.0 (not used in calc)
        if self.decline_type == DeclineType.LINEAR and self.decline_rate is None:
            self.decline_rate = 0.0

        # Decline curve validation
        if not isinstance(self.decline_type, DeclineType):
            self.decline_type = DeclineType(self.decline_type)
        if self.decline_type != DeclineType.LINEAR and (
            self.decline_rate is None or self.decline_rate <= 0
        ):
            raise ValueError(
                f"decline_rate must be > 0 for {self.decline_type.value} decline, "
                f"got {self.decline_rate!r}"
            )
        if self.decline_type == DeclineType.HYPERBOLIC:
            # b=1 is the harmonic limit and is physically valid (reduces to
            # 1/(1+Dt)), so the upper bound is inclusive.
            if not (0 < self.b_factor <= 1):
                raise ValueError(
                    f"b_factor must be in (0, 1] for hyperbolic decline, "
                    f"got {self.b_factor!r}"
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
    pre_tax_npv_usd_mm: Optional[float] = None
    pre_tax_irr: Optional[float] = None


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
    """Return a callable that estimates CAPEX in USD MM.

    Tries worldenergydata.cost.CostPredictor (71-project ML model) first,
    then falls back to the digitalmodel GoM benchmark.
    """

    def _estimate_capex(inp: EconomicsInput) -> tuple[float, str]:
        """Estimate CAPEX trying CostPredictor, then GoM benchmark fallback."""
        # --- Primary: worldenergydata ML cost predictor ---
        try:
            from worldenergydata.cost import CostPredictor, CostDataPoint, load_public_dataset
            from worldenergydata.cost.data_collection.calibration_schema import (
                WaterDepthBand, RigType, ActivityType, SubseaType,
                CostType, Confidence,
            )

            dataset = load_public_dataset()
            if len(dataset) >= 5:
                predictor = CostPredictor()
                predictor.fit(dataset)
                region = inp.region or _REGIME_TO_REGION.get(inp.fiscal_regime, "gom")

                # Map water depth to band
                wd = inp.water_depth_m
                if wd <= 300:
                    depth_band = WaterDepthBand.SHALLOW
                elif wd <= 1000:
                    depth_band = WaterDepthBand.MID
                elif wd <= 2000:
                    depth_band = WaterDepthBand.DEEP
                else:
                    depth_band = WaterDepthBand.ULTRA_DEEP

                # Map host type to subsea/rig defaults
                host = inp.host_type
                subsea = (
                    SubseaType.SUBSEA if host == "Subsea_Tieback"
                    else SubseaType.DRY_TREE
                )
                rig = (
                    RigType.DRILLSHIP if wd > 1000
                    else RigType.SEMI_SUB if wd > 300
                    else RigType.JACK_UP
                )

                probe = CostDataPoint(
                    project_name=inp.field_name,
                    region=region,
                    water_depth_m=wd,
                    water_depth_band=depth_band,
                    operator="screening",
                    year_sanction=2024,
                    rig_type=rig,
                    activity_type=ActivityType.DRILLING,
                    hpht=wd > 2000,
                    subsea=subsea,
                    cost_usd_mm=1.0,  # placeholder — not used for prediction
                    cost_type=CostType.TOTAL_CAPEX,
                    source="digitalmodel screening",
                    confidence=Confidence.LOW,
                )
                result = predictor.predict(probe)
                return result.cost_usd_mm, (
                    f"worldenergydata.cost.CostPredictor "
                    f"({predictor._n_train} projects)"
                )
        except (ImportError, Exception):
            pass

        # --- Fallback: digitalmodel GoM benchmark ---
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
# Production decline helpers
# ---------------------------------------------------------------------------


def _production_factors(inp: EconomicsInput, n: int) -> list[float]:
    """Return per-year production multipliers for years 0..n.

    Year 0 is always 0.0 (development).  Years 1..plateau are 1.0.
    Post-plateau behaviour depends on ``inp.decline_type``:

    - LINEAR (default, legacy): straight-line from 1.0 to 0.2
    - EXPONENTIAL: exp(-D * t)  where t = years past plateau
    - HYPERBOLIC:  (1 + b*D*t)^(-1/b)
    - HARMONIC:    1 / (1 + D*t)
    """
    plateau_years = max(1, int(n * 0.6))
    decline_years = n - plateau_years
    dt = inp.decline_type
    factors: list[float] = [0.0]  # year 0

    for yr in range(1, n + 1):
        if yr <= plateau_years:
            factors.append(1.0)
        else:
            t = yr - plateau_years

            if dt == DeclineType.LINEAR:
                if decline_years > 0:
                    frac_decline = t / decline_years
                    factors.append(1.0 - 0.8 * frac_decline)
                else:
                    factors.append(0.2)

            elif dt == DeclineType.EXPONENTIAL:
                factors.append(np.exp(-inp.decline_rate * t))

            elif dt == DeclineType.HYPERBOLIC:
                factors.append(
                    (1.0 + inp.b_factor * inp.decline_rate * t)
                    ** (-1.0 / inp.b_factor)
                )

            elif dt == DeclineType.HARMONIC:
                factors.append(1.0 / (1.0 + inp.decline_rate * t))

    return factors


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
    factors = _production_factors(inp, n)
    annual_revenue_mm = (
        inp.production_capacity_bopd * 365.0 * inp.oil_price_usd_per_bbl / 1_000_000.0
    )

    for yr in range(1, n + 1):
        prod_factor = factors[yr]
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
# Fiscal regime tax adjustment (#2050)
# ---------------------------------------------------------------------------


def _resolve_fiscal_calculator(
    inp: EconomicsInput,
) -> Optional[Callable[[float, float, float, float], float]]:
    """Resolve a fiscal calculator for the given regime.

    Returns a callable ``(revenue_mm, opex_mm, capex_mm, prod_rate_bpd) ->
    government_take_mm`` or ``None`` if the regime's worldenergydata module
    is unavailable.
    """
    regime = inp.fiscal_regime

    try:
        if regime == FiscalRegime.US:
            from worldenergydata.eia_us.analysis.us_fiscal import (
                FederalOnshoreFiscal,
            )

            fiscal = FederalOnshoreFiscal()

            def _us(rev: float, opex: float, capex: float, _pr: float) -> float:
                return fiscal.calculate(rev, opex, capex).total_government_take

            return _us

        if regime == FiscalRegime.UK:
            from worldenergydata.ukcs.analysis.field_economics import (
                UKFiscalRegime,
            )

            fiscal = UKFiscalRegime()

            def _uk(rev: float, opex: float, capex: float, _pr: float) -> float:
                return fiscal.calculate(rev, opex, capex).total_government_take

            return _uk

        if regime == FiscalRegime.Brazil:
            from worldenergydata.brazil_anp.analysis.field_economics import (
                ConcessionRegime,
            )

            fiscal = ConcessionRegime()

            def _br(rev: float, opex: float, capex: float, pr: float) -> float:
                return fiscal.calculate(rev, opex, capex, pr).total_government_take

            return _br

        if regime == FiscalRegime.Nigeria:
            from worldenergydata.west_africa.analysis.field_economics import (
                NigeriaDeepwaterPSC,
            )

            fiscal = NigeriaDeepwaterPSC(water_depth_m=int(inp.water_depth_m))

            def _ng(rev: float, opex: float, capex: float, pr: float) -> float:
                return fiscal.calculate(
                    rev, opex, capex, int(pr),
                ).total_government_take

            return _ng

        if regime == FiscalRegime.Norway:
            from worldenergydata.sodir.npv_norway import (
                NorwayNPVCalculator,
                NorwegianFinancialParameters,
            )

            calc_obj = NorwayNPVCalculator(NorwegianFinancialParameters())

            def _no(rev: float, opex: float, capex: float, _pr: float) -> float:
                taxable = rev - opex - capex
                if taxable <= 0:
                    return 0.0
                taxes = calc_obj.calculate_taxes(taxable)
                return sum(taxes.values())

            return _no

    except (ImportError, Exception):
        return None

    return None


def _apply_fiscal_regime(
    cashflows: np.ndarray,
    inp: EconomicsInput,
    capex_mm: float,
    opex_mm_yr: float,
    abex_mm: float,
) -> np.ndarray:
    """Apply per-year fiscal regime tax adjustment to annual cashflows.

    Dispatches to worldenergydata fiscal modules by regime.  Returns
    adjusted (post-tax) cashflows.  If the module is unavailable or
    the regime is unsupported, returns the original cashflows unchanged.
    """
    calc = _resolve_fiscal_calculator(inp)
    if calc is None:
        return cashflows  # passthrough — no tax adjustment

    n = inp.field_life_years
    adjusted = cashflows.copy()
    factors = _production_factors(inp, n)
    annual_revenue_mm = (
        inp.production_capacity_bopd * 365.0 * inp.oil_price_usd_per_bbl / 1e6
    )

    capex_schedule = [0.30, 0.50, 0.20]

    for yr in range(n + 1):
        prod_factor = factors[yr]
        revenue_yr = annual_revenue_mm * prod_factor
        opex_yr = opex_mm_yr * prod_factor

        capex_yr = 0.0
        if yr < len(capex_schedule):
            capex_yr = capex_mm * capex_schedule[yr]
        if yr == n:
            capex_yr += abex_mm

        if revenue_yr <= 0:
            continue  # no revenue, no tax

        prod_rate_yr = inp.production_capacity_bopd * prod_factor

        try:
            gov_take = calc(revenue_yr, opex_yr, capex_yr, prod_rate_yr)
            if gov_take > 0:
                adjusted[yr] -= gov_take
        except Exception:
            continue  # skip year on calculation error

    return adjusted


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

    # --- Fiscal regime tax adjustment (#2050) ---
    post_tax_cashflows = _apply_fiscal_regime(
        cashflows, inp, capex_mm, opex_mm_yr, abex_mm,
    )
    fiscal_applied = post_tax_cashflows is not cashflows

    # --- Financial metrics ---
    financial_adapter = _resolve_financial_adapter()

    # Pre-tax metrics (only computed when fiscal adjustment is applied)
    pre_tax_npv: Optional[float] = None
    pre_tax_irr: Optional[float] = None
    if fiscal_applied:
        pre_tax_dict = financial_adapter(cashflows, inp.discount_rate)
        pre_tax_npv = pre_tax_dict.get("npv", 0.0) or 0.0
        pre_tax_irr = pre_tax_dict.get("irr_annual")

    # Post-tax metrics (or pre-tax if fiscal was not applied)
    metrics_dict = financial_adapter(post_tax_cashflows, inp.discount_rate)

    metrics = EvaluationMetrics(
        npv_usd_mm=metrics_dict.get("npv", 0.0) or 0.0,
        irr=metrics_dict.get("irr_annual"),
        mirr=metrics_dict.get("mirr_annual"),
        payback_years=metrics_dict.get("payback_years"),
        discount_rate=inp.discount_rate,
        pre_tax_npv_usd_mm=pre_tax_npv,
        pre_tax_irr=pre_tax_irr,
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


# ---------------------------------------------------------------------------
# CashFlowSchedule bridge — worldenergydata.economics.dcf integration
# ---------------------------------------------------------------------------


def build_economics_schedule(
    inp: EconomicsInput,
    capex_usd_mm: Optional[float] = None,
    opex_usd_mm_per_yr: Optional[float] = None,
    abex_usd_mm: Optional[float] = None,
) -> "CashFlowSchedule":
    """Build a worldenergydata CashFlowSchedule from an EconomicsInput.

    This bridges the digitalmodel facade to the full DCF and carbon
    sensitivity functions in ``worldenergydata.economics``.

    Parameters
    ----------
    inp : EconomicsInput
        Field parameters.
    capex_usd_mm, opex_usd_mm_per_yr, abex_usd_mm : float, optional
        Pre-computed cost overrides.  When omitted, costs are estimated
        via the same adapters used in :func:`evaluate_economics`.

    Returns
    -------
    worldenergydata.economics.dcf.CashFlowSchedule
        Structured schedule consumable by ``calculate_npv``, ``calculate_mirr``,
        ``carbon_npv_curve``, ``breakeven_carbon_price``, etc.

    Raises
    ------
    ImportError
        If ``worldenergydata.economics.dcf`` is not available.
    """
    from worldenergydata.economics.dcf import CashFlowSchedule

    n = inp.field_life_years

    # --- Resolve costs if not provided ---
    if capex_usd_mm is None:
        if inp.capex_usd_mm is not None:
            capex_usd_mm = inp.capex_usd_mm
        else:
            capex_usd_mm, _ = _resolve_capex_adapter()(inp)

    if opex_usd_mm_per_yr is None:
        opex_usd_mm_per_yr = _estimate_opex_mm_per_yr(inp)

    if abex_usd_mm is None:
        if inp.abex_usd_mm is not None:
            abex_usd_mm = inp.abex_usd_mm
        else:
            abex_usd_mm, _ = _resolve_abex_adapter()(inp)
            if abex_usd_mm <= 0:
                abex_usd_mm = capex_usd_mm * 0.07

    # --- Build per-year arrays (USD MM) ---
    years = list(range(n + 1))
    capex_arr = [0.0] * (n + 1)
    revenue_arr = [0.0] * (n + 1)
    opex_arr = [0.0] * (n + 1)
    carbon_arr = [0.0] * (n + 1)
    emission_arr = [0.0] * (n + 1)

    # CAPEX spread: 30/50/20 in years 0-2
    capex_schedule = [0.30, 0.50, 0.20]
    for i, frac in enumerate(capex_schedule):
        if i <= n:
            capex_arr[i] = capex_usd_mm * frac * 1_000_000.0  # to USD

    # ABEX in final year
    capex_arr[n] += abex_usd_mm * 1_000_000.0

    # Revenue, OPEX, carbon from year 1
    factors = _production_factors(inp, n)
    annual_revenue = (
        inp.production_capacity_bopd * 365.0 * inp.oil_price_usd_per_bbl
    )
    annual_opex = opex_usd_mm_per_yr * 1_000_000.0
    emission_intensity = 0.04  # tCO2/bbl Scope 1+2

    for yr in range(1, n + 1):
        prod_factor = factors[yr]

        revenue_arr[yr] = annual_revenue * prod_factor
        opex_arr[yr] = annual_opex * prod_factor

        annual_bbl = inp.production_capacity_bopd * 365.0 * prod_factor
        emissions = annual_bbl * emission_intensity
        emission_arr[yr] = emissions

        carbon_price = inp.carbon_cost_usd_per_tonne or 0.0
        carbon_arr[yr] = emissions * carbon_price

    return CashFlowSchedule(
        years=years,
        capex=capex_arr,
        revenue=revenue_arr,
        opex=opex_arr,
        carbon_cost=carbon_arr,
        emission_tco2_per_period=emission_arr,
    )


# ---------------------------------------------------------------------------
# Carbon sensitivity pass-through
# ---------------------------------------------------------------------------


def carbon_sensitivity(
    inp: EconomicsInput,
    discount_rate: Optional[float] = None,
    carbon_prices: Optional[list[float]] = None,
) -> "CarbonSensitivityResult":
    """Run an NPV-vs-carbon-price sweep for a field development scenario.

    Delegates to ``worldenergydata.economics.carbon.carbon_npv_curve``.

    Parameters
    ----------
    inp : EconomicsInput
        Field parameters.
    discount_rate : float, optional
        Override discount rate (defaults to ``inp.discount_rate``).
    carbon_prices : list[float], optional
        Carbon prices to sweep (USD/tonne CO2).
        Defaults to ``[0, 25, 50, 75, 100, 150, 200]``.

    Returns
    -------
    worldenergydata.economics.carbon.CarbonSensitivityResult

    Raises
    ------
    ImportError
        If ``worldenergydata.economics.carbon`` is not available.
    """
    from worldenergydata.economics.carbon import carbon_npv_curve

    schedule = build_economics_schedule(inp)
    rate = discount_rate if discount_rate is not None else inp.discount_rate
    prices = carbon_prices or [0, 25, 50, 75, 100, 150, 200]
    return carbon_npv_curve(schedule, rate, prices)
