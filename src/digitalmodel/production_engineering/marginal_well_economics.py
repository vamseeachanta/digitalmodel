# ABOUTME: Well-by-well economics for marginal/stripper wells, including P&A liability
# ABOUTME: Answers keep / workover / shut-in / plug-now for a single wellbore

"""
Marginal well economics
=======================
Per-well cash-flow economics for stripper and marginal wells, with the
plugging-and-abandonment liability carried explicitly.

Why a separate module from ``field_development.economics``
----------------------------------------------------------
That module screens offshore field developments: CAPEX in USD millions, host
types, water depth, fiscal regimes. A 3 BOPD Illinois Basin rod-pumped well
lives at a completely different scale and faces a different decision. Here the
capital is already spent; the only question left is what to do next with a well
that may or may not cover its own monthly costs.

The decision this module answers
--------------------------------
For one wellbore, which of these maximises value?

======================  =====================================================
``KEEP``                Produce as-is to the economic limit, plug then
``WORKOVER``            Spend to restore rate, then produce to the limit
``SHUT_IN``             Stop producing, keep the well (and the liability)
``PLUG_NOW``            Retire the well and settle the liability today
======================  =====================================================

The point most single-well models miss
--------------------------------------
Plugging cost is **not avoided by producing** — it is *deferred*. A marginal
well is therefore not competing against zero; it is competing against paying
the P&A bill today. Deferral has real time value, which is why wells that look
like they lose money often still rationally stay online::

    NPV(keep)     = PV(net operating cash to economic limit) - PV(P&A at limit)
    NPV(plug now) = -P&A today + bond release today

So a well can carry a *negative* operating margin and still beat plugging, as
long as the deferral of the P&A cost is worth more than the losses. This module
makes that trade explicit rather than hiding it.

Everything here is deterministic and unit-explicit. Nothing is inferred from a
well name, a lease name or an equipment designation.

Units
-----
Rates in BOPD/BWPD, money in nominal USD, time in months. Discount rates are
given as an annual effective fraction and converted to monthly internally.

Scope and limits
----------------
- Screening economics, not a reserve report and not tax advice.
- Prices are deterministic. Use :func:`price_breakeven` for the price at which
  the recommendation flips, rather than guessing a price deck.
- Bond release is modelled as a cash inflow when the well is plugged. Whether a
  given bond is actually refundable is a **regulatory question for the operator
  and their counsel** — pass ``bond_release_usd=0.0`` when unsure.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

__all__ = [
    "Decision",
    "WellCosts",
    "MarginalWellInput",
    "WorkoverCase",
    "MonthlyCashflow",
    "WellEconomicsResult",
    "monthly_rate",
    "economic_limit_month",
    "evaluate_well",
    "price_breakeven",
]


class Decision(str, Enum):
    """Recommended way forward for a single wellbore."""

    KEEP = "keep"
    WORKOVER = "workover"
    SHUT_IN = "shut_in"
    PLUG_NOW = "plug_now"


# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class WellCosts:
    """Recurring and one-off costs attached to one wellbore.

    Fixed costs are the ones a marginal well cannot escape by producing less:
    the pumper's route time, the meter, the base electricity charge, surface
    lease upkeep. Variable costs scale with what comes out of the ground.
    """

    fixed_opex_usd_per_month: float
    """Pumper/route labour, base power, surface upkeep — incurred while producing."""

    variable_opex_usd_per_bbl_oil: float = 0.0
    """Chemicals, trucking, treating — per barrel of oil."""

    water_disposal_usd_per_bbl: float = 0.0
    """Per barrel of produced water (SWD fee, trucking, or lifting power)."""

    annual_carry_usd: float = 0.0
    """Costs owed whether or not the well produces: bond premium, insurance,
    ad-valorem tax, regulatory reporting. Charged monthly as 1/12."""

    shut_in_opex_usd_per_month: float = 0.0
    """Cost of holding a shut-in well (inspection, idle-well compliance).
    The annual carry applies on top of this."""

    plugging_cost_usd: float = 0.0
    """All-in P&A: rig, cement, plugs, surface restoration, filings."""

    bond_release_usd: float = 0.0
    """Cash returned when the well is plugged and released. Set to 0.0 unless
    the operator has confirmed the bond is refundable — this is a regulatory
    question, not an engineering one."""

    def __post_init__(self) -> None:
        for name in (
            "fixed_opex_usd_per_month",
            "variable_opex_usd_per_bbl_oil",
            "water_disposal_usd_per_bbl",
            "annual_carry_usd",
            "shut_in_opex_usd_per_month",
            "plugging_cost_usd",
            "bond_release_usd",
        ):
            if getattr(self, name) < 0:
                raise ValueError(f"{name} must be >= 0")


@dataclass(frozen=True)
class WorkoverCase:
    """A candidate intervention: spend now, get rate back.

    ``success_probability`` scales the *uplift*, not the cost — the money is
    spent either way. That is the honest way to represent a workover on a
    marginal well, where a failed job still gets invoiced.
    """

    cost_usd: float
    uplift_bopd: float
    """Incremental oil rate immediately after the job, on top of the
    post-workover base rate."""

    success_probability: float = 1.0
    incremental_decline_rate_per_yr: Optional[float] = None
    """Decline applied to the uplift. Defaults to the well's own decline.
    Workover uplift often declines faster — set it explicitly if known."""

    def __post_init__(self) -> None:
        if self.cost_usd < 0:
            raise ValueError("cost_usd must be >= 0")
        if self.uplift_bopd < 0:
            raise ValueError("uplift_bopd must be >= 0")
        if not 0.0 <= self.success_probability <= 1.0:
            raise ValueError("success_probability must be in [0, 1]")


@dataclass(frozen=True)
class MarginalWellInput:
    """Everything needed to evaluate one wellbore."""

    well_id: str
    oil_rate_bopd: float
    costs: WellCosts

    water_rate_bwpd: float = 0.0

    # --- decline -----------------------------------------------------------
    decline_rate_per_yr: float = 0.0
    """Nominal annual decline as a fraction (0.08 = 8%/yr). Zero = flat."""

    b_factor: float = 0.0
    """Arps b. 0 = exponential; 0 < b <= 1 = hyperbolic. Shallow conventional
    stripper wells are usually near-exponential and often effectively flat."""

    # --- price realisation --------------------------------------------------
    oil_price_usd_per_bbl: float = 0.0
    price_differential_usd_per_bbl: float = 0.0
    """Deduct from the benchmark: gravity, basis, transport. Positive = a
    deduction, so realised price = price - differential."""

    royalty_fraction: float = 0.0
    severance_tax_fraction: float = 0.0
    working_interest_fraction: float = 1.0
    """Operator's share of costs. Net revenue interest is derived as
    ``working_interest * (1 - royalty)``."""

    # --- evaluation ---------------------------------------------------------
    discount_rate_per_yr: float = 0.10
    max_horizon_months: int = 360
    workover: Optional[WorkoverCase] = None

    def __post_init__(self) -> None:
        if self.oil_rate_bopd < 0 or self.water_rate_bwpd < 0:
            raise ValueError("rates must be >= 0")
        if not 0.0 <= self.b_factor <= 1.0:
            raise ValueError("b_factor must be in [0, 1]")
        if self.decline_rate_per_yr < 0:
            raise ValueError("decline_rate_per_yr must be >= 0")
        for name in ("royalty_fraction", "severance_tax_fraction", "working_interest_fraction"):
            v = getattr(self, name)
            if not 0.0 <= v <= 1.0:
                raise ValueError(f"{name} must be in [0, 1]")
        if self.discount_rate_per_yr <= -1.0:
            raise ValueError("discount_rate_per_yr must be > -1")
        if self.max_horizon_months < 1:
            raise ValueError("max_horizon_months must be >= 1")

    @property
    def realised_price_usd_per_bbl(self) -> float:
        """Wellhead price after the differential, before royalty and tax."""
        return self.oil_price_usd_per_bbl - self.price_differential_usd_per_bbl

    @property
    def net_revenue_interest(self) -> float:
        """Operator's share of revenue = WI x (1 - royalty)."""
        return self.working_interest_fraction * (1.0 - self.royalty_fraction)


# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class MonthlyCashflow:
    """One month of the forecast."""

    month: int
    oil_rate_bopd: float
    oil_volume_bbl: float
    revenue_usd: float
    opex_usd: float
    net_cash_usd: float
    discounted_net_cash_usd: float


@dataclass
class WellEconomicsResult:
    """Result of evaluating one wellbore across all four courses of action."""

    well_id: str
    decision: Decision
    rationale: str

    npv_keep_usd: float
    npv_plug_now_usd: float
    npv_shut_in_usd: float
    npv_workover_usd: Optional[float]

    economic_limit_month: Optional[int]
    """First month whose net operating cash is negative, i.e. when producing
    starts destroying value. ``None`` if the well never turns negative inside
    the horizon."""

    remaining_oil_bbl: float
    """Gross oil to the economic limit (before interests)."""

    monthly_net_operating_cash_usd: float
    """Net operating cash in month 1 — the number an operator recognises."""

    pv_plugging_deferred_usd: float
    """Present value of the P&A bill paid at the economic limit rather than
    today. The deferral benefit is ``plugging_cost - this``."""

    cashflows: list[MonthlyCashflow] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    @property
    def deferral_value_usd(self) -> float:
        """What postponing the P&A bill to the economic limit is worth today."""
        return self.npv_keep_usd - self.npv_plug_now_usd


# ---------------------------------------------------------------------------
# Decline
# ---------------------------------------------------------------------------

def monthly_rate(
    initial_rate: float,
    decline_rate_per_yr: float,
    b_factor: float,
    month: int,
) -> float:
    """Arps rate at ``month`` months after the initial rate.

    ``month=1`` returns the rate one month in, which is what the first
    production month should be evaluated at.

    - ``b_factor == 0``  → exponential:  q = qi * exp(-D*t)
    - ``0 < b <= 1``     → hyperbolic:   q = qi / (1 + b*D*t)^(1/b)
    """
    if initial_rate <= 0:
        return 0.0
    if decline_rate_per_yr <= 0:
        return initial_rate

    t_years = month / 12.0
    if b_factor == 0.0:
        return initial_rate * math.exp(-decline_rate_per_yr * t_years)

    denom = 1.0 + b_factor * decline_rate_per_yr * t_years
    return initial_rate / denom ** (1.0 / b_factor)


def _discount_factor(discount_rate_per_yr: float, month: int) -> float:
    """Mid-period-free monthly discount factor at end of ``month``."""
    monthly = (1.0 + discount_rate_per_yr) ** (1.0 / 12.0) - 1.0
    return 1.0 / (1.0 + monthly) ** month


# ---------------------------------------------------------------------------
# Cash flow
# ---------------------------------------------------------------------------

def _month_cashflow(
    inp: MarginalWellInput,
    month: int,
    oil_bopd: float,
    water_bwpd: float,
) -> MonthlyCashflow:
    """Build one month of cash flow. Days-per-month is fixed at 30.4375."""
    days = 365.25 / 12.0
    oil_bbl = oil_bopd * days
    water_bbl = water_bwpd * days

    gross_revenue = oil_bbl * inp.realised_price_usd_per_bbl
    net_revenue = gross_revenue * inp.net_revenue_interest
    net_revenue *= 1.0 - inp.severance_tax_fraction

    c = inp.costs
    opex = (
        c.fixed_opex_usd_per_month
        + c.annual_carry_usd / 12.0
        + oil_bbl * c.variable_opex_usd_per_bbl_oil
        + water_bbl * c.water_disposal_usd_per_bbl
    ) * inp.working_interest_fraction

    net = net_revenue - opex
    return MonthlyCashflow(
        month=month,
        oil_rate_bopd=oil_bopd,
        oil_volume_bbl=oil_bbl,
        revenue_usd=net_revenue,
        opex_usd=opex,
        net_cash_usd=net,
        discounted_net_cash_usd=net * _discount_factor(inp.discount_rate_per_yr, month),
    )


def economic_limit_month(inp: MarginalWellInput) -> Optional[int]:
    """First month in which net operating cash goes negative.

    Returns ``None`` when the well stays cash-positive across the whole
    horizon — in which case the horizon, not economics, ends the forecast.
    """
    for m in range(1, inp.max_horizon_months + 1):
        oil = monthly_rate(inp.oil_rate_bopd, inp.decline_rate_per_yr, inp.b_factor, m)
        water = monthly_rate(inp.water_rate_bwpd, 0.0, 0.0, m)
        if _month_cashflow(inp, m, oil, water).net_cash_usd < 0:
            return m
    return None


def _produce(
    inp: MarginalWellInput,
    *,
    rate_override: Optional[float] = None,
    extra_rate_bopd: float = 0.0,
    extra_decline_per_yr: Optional[float] = None,
) -> tuple[list[MonthlyCashflow], Optional[int], float]:
    """Run the production case; stop at the economic limit.

    Returns ``(cashflows, limit_month, gross_oil_bbl)``. ``limit_month`` is
    ``None`` when the well is still cash-positive at the horizon.
    """
    base = inp.oil_rate_bopd if rate_override is None else rate_override
    extra_D = inp.decline_rate_per_yr if extra_decline_per_yr is None else extra_decline_per_yr

    flows: list[MonthlyCashflow] = []
    gross_oil = 0.0
    for m in range(1, inp.max_horizon_months + 1):
        oil = monthly_rate(base, inp.decline_rate_per_yr, inp.b_factor, m)
        if extra_rate_bopd > 0:
            oil += monthly_rate(extra_rate_bopd, extra_D, inp.b_factor, m)
        water = monthly_rate(inp.water_rate_bwpd, 0.0, 0.0, m)

        cf = _month_cashflow(inp, m, oil, water)
        if cf.net_cash_usd < 0:
            return flows, m, gross_oil
        flows.append(cf)
        gross_oil += cf.oil_volume_bbl

    return flows, None, gross_oil


def _pv_plug(inp: MarginalWellInput, month: int) -> float:
    """PV of the net P&A cash outflow (cost less bond release) at ``month``."""
    net_cost = inp.costs.plugging_cost_usd - inp.costs.bond_release_usd
    return -net_cost * _discount_factor(inp.discount_rate_per_yr, month)


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------

def evaluate_well(inp: MarginalWellInput) -> WellEconomicsResult:
    """Evaluate one wellbore and recommend a course of action.

    All four NPVs are on the same basis — every one of them ends with the well
    plugged — so they are directly comparable. That is the whole trick: the
    P&A liability is unavoidable, so it must appear in every branch or the
    comparison is rigged in favour of producing.
    """
    warnings: list[str] = []
    if inp.oil_price_usd_per_bbl <= 0:
        warnings.append("oil_price_usd_per_bbl is zero or negative — revenue will be non-positive.")
    if inp.costs.plugging_cost_usd == 0:
        warnings.append(
            "plugging_cost_usd is zero. The P&A liability is the point of this "
            "analysis; a zero here makes 'keep' look better than it is."
        )
    if inp.costs.bond_release_usd > 0:
        warnings.append(
            "bond_release_usd > 0 assumes the bond is refundable on release — "
            "confirm with the regulator before relying on it."
        )

    # --- KEEP: produce to the limit, then plug -----------------------------
    flows, limit, gross_oil = _produce(inp)
    plug_month = limit if limit is not None else inp.max_horizon_months
    pv_ops = sum(cf.discounted_net_cash_usd for cf in flows)
    pv_plug_deferred = _pv_plug(inp, plug_month)
    npv_keep = pv_ops + pv_plug_deferred

    # --- PLUG NOW: settle today (month 0, undiscounted) ---------------------
    npv_plug_now = -(inp.costs.plugging_cost_usd - inp.costs.bond_release_usd)

    # --- SHUT IN: carry the well to the horizon, then plug ------------------
    # No revenue, but idle-well cost and the annual carry keep running.
    #
    # Only the horizon endpoint is evaluated, and that is sufficient: with a
    # constant carry and a constant discount rate, NPV as a function of the
    # plug date is monotonic (each extra month costs the carry and saves the
    # discount on the P&A bill, and neither term changes sign). The optimum is
    # therefore always at an endpoint — plug today (PLUG_NOW) or hold to the
    # horizon (SHUT_IN). A time-varying carry would break that and would need
    # a search over plug dates.
    shut_monthly = inp.costs.shut_in_opex_usd_per_month + inp.costs.annual_carry_usd / 12.0
    shut_monthly *= inp.working_interest_fraction
    pv_shut_ops = -sum(
        shut_monthly * _discount_factor(inp.discount_rate_per_yr, m)
        for m in range(1, inp.max_horizon_months + 1)
    )
    npv_shut_in = pv_shut_ops + _pv_plug(inp, inp.max_horizon_months)

    # --- WORKOVER ----------------------------------------------------------
    npv_workover: Optional[float] = None
    if inp.workover is not None:
        wo = inp.workover
        wo_flows, wo_limit, _ = _produce(
            inp,
            extra_rate_bopd=wo.uplift_bopd * wo.success_probability,
            extra_decline_per_yr=wo.incremental_decline_rate_per_yr,
        )
        wo_plug_month = wo_limit if wo_limit is not None else inp.max_horizon_months
        npv_workover = (
            -wo.cost_usd
            + sum(cf.discounted_net_cash_usd for cf in wo_flows)
            + _pv_plug(inp, wo_plug_month)
        )

    # --- pick ---------------------------------------------------------------
    options: dict[Decision, float] = {
        Decision.PLUG_NOW: npv_plug_now,
        Decision.SHUT_IN: npv_shut_in,
    }

    # KEEP is only a real option if the well has at least one cash-positive
    # month. With an economic limit at month 1 the "keep" branch degenerates
    # into "plug one month from now", which scores fractionally better than
    # plugging today purely because of one month of discounting — and would
    # then be reported as "keep producing" on a well that never produces a
    # profitable month. That is an artefact, not a recommendation.
    if flows:
        options[Decision.KEEP] = npv_keep
    else:
        warnings.append(
            "No cash-positive month at the current price and cost deck, so "
            "'keep producing' is not offered as an option — the choice is "
            "between shutting in and plugging."
        )

    if npv_workover is not None:
        options[Decision.WORKOVER] = npv_workover

    decision = max(options, key=lambda d: options[d])
    rationale = _rationale(decision, options, limit, inp, pv_plug_deferred)

    month1 = flows[0].net_cash_usd if flows else _month_cashflow(
        inp, 1,
        monthly_rate(inp.oil_rate_bopd, inp.decline_rate_per_yr, inp.b_factor, 1),
        inp.water_rate_bwpd,
    ).net_cash_usd

    return WellEconomicsResult(
        well_id=inp.well_id,
        decision=decision,
        rationale=rationale,
        npv_keep_usd=npv_keep,
        npv_plug_now_usd=npv_plug_now,
        npv_shut_in_usd=npv_shut_in,
        npv_workover_usd=npv_workover,
        economic_limit_month=limit,
        remaining_oil_bbl=gross_oil,
        monthly_net_operating_cash_usd=month1,
        pv_plugging_deferred_usd=pv_plug_deferred,
        cashflows=flows,
        warnings=warnings,
    )


def _rationale(
    decision: Decision,
    options: dict[Decision, float],
    limit: Optional[int],
    inp: MarginalWellInput,
    pv_plug_deferred: float,
) -> str:
    """One sentence an operator can act on, with the margin over runner-up."""
    ranked = sorted(options.items(), key=lambda kv: kv[1], reverse=True)
    margin = ranked[0][1] - ranked[1][1] if len(ranked) > 1 else 0.0
    runner_up = ranked[1][0].value if len(ranked) > 1 else "n/a"

    life = (
        f"{limit} months of positive cash remain"
        if limit is not None
        else f"still cash-positive at the {inp.max_horizon_months}-month horizon"
    )
    deferral = inp.costs.plugging_cost_usd - inp.costs.bond_release_usd + pv_plug_deferred

    if decision is Decision.KEEP:
        return (
            f"Keep producing: {life}. Deferring the P&A bill is worth "
            f"${deferral:,.0f} today, and this beats '{runner_up}' by ${margin:,.0f}."
        )
    if decision is Decision.WORKOVER:
        return (
            f"Workover: the intervention pays for itself and beats '{runner_up}' "
            f"by ${margin:,.0f}."
        )
    if decision is Decision.SHUT_IN:
        return (
            f"Shut in: production does not cover its own costs, but holding the "
            f"well beats plugging by ${margin:,.0f} because the P&A bill is deferred."
        )
    return (
        f"Plug now: producing destroys value ({life}) and the deferral benefit "
        f"does not cover the losses. Beats '{runner_up}' by ${margin:,.0f}."
    )


def price_breakeven(
    inp: MarginalWellInput,
    *,
    low: float = 0.0,
    high: float = 500.0,
    tol: float = 0.01,
) -> Optional[float]:
    """Benchmark oil price at which month-1 net operating cash is exactly zero.

    This is the number an operator actually watches: below it, the well loses
    money every month it runs. Returns ``None`` if no crossing exists in
    ``[low, high]`` — typically a well with no oil rate at all.

    Note this is the *operating* breakeven, deliberately excluding P&A. The
    plug-versus-keep decision is :func:`evaluate_well`; this is the simpler
    "am I paying to produce today" question.
    """
    from dataclasses import replace

    def net_at(price: float) -> float:
        probe = replace(inp, oil_price_usd_per_bbl=price)
        oil = monthly_rate(probe.oil_rate_bopd, probe.decline_rate_per_yr, probe.b_factor, 1)
        return _month_cashflow(probe, 1, oil, probe.water_rate_bwpd).net_cash_usd

    f_low, f_high = net_at(low), net_at(high)
    if f_low > 0 or f_high < 0:
        return None

    for _ in range(200):
        mid = 0.5 * (low + high)
        if net_at(mid) < 0:
            low = mid
        else:
            high = mid
        if high - low < tol:
            break
    return 0.5 * (low + high)
