# ABOUTME: Tests for marginal/stripper well economics and the plug-vs-keep decision
# ABOUTME: Pins the deferral-of-P&A behaviour that drives the recommendation

import math

import pytest

from digitalmodel.production_engineering.marginal_well_economics import (
    Decision,
    MarginalWellInput,
    WellCosts,
    WorkoverCase,
    economic_limit_month,
    evaluate_well,
    monthly_rate,
    price_breakeven,
)


# ---------------------------------------------------------------------------
# Fixtures — a plausible shallow conventional stripper well
# ---------------------------------------------------------------------------

def healthy_costs(**kw) -> WellCosts:
    base = dict(
        fixed_opex_usd_per_month=900.0,
        variable_opex_usd_per_bbl_oil=4.0,
        water_disposal_usd_per_bbl=0.75,
        annual_carry_usd=1_800.0,
        shut_in_opex_usd_per_month=60.0,
        plugging_cost_usd=45_000.0,
        bond_release_usd=0.0,
    )
    base.update(kw)
    return WellCosts(**base)


def stripper_well(**kw) -> MarginalWellInput:
    base = dict(
        well_id="SO-TEST-001",
        oil_rate_bopd=6.0,
        water_rate_bwpd=40.0,
        costs=healthy_costs(),
        decline_rate_per_yr=0.08,
        b_factor=0.0,
        oil_price_usd_per_bbl=65.0,
        price_differential_usd_per_bbl=5.0,
        royalty_fraction=0.125,
        severance_tax_fraction=0.046,
        working_interest_fraction=1.0,
        discount_rate_per_yr=0.10,
        max_horizon_months=240,
    )
    base.update(kw)
    return MarginalWellInput(**base)


# ---------------------------------------------------------------------------
# Decline
# ---------------------------------------------------------------------------

class TestDecline:
    def test_zero_decline_is_flat(self):
        assert monthly_rate(10.0, 0.0, 0.0, 120) == 10.0

    def test_exponential_matches_closed_form(self):
        q = monthly_rate(10.0, 0.10, 0.0, 12)
        assert q == pytest.approx(10.0 * math.exp(-0.10), rel=1e-12)

    def test_hyperbolic_declines_slower_than_exponential(self):
        exp_q = monthly_rate(10.0, 0.20, 0.0, 60)
        hyp_q = monthly_rate(10.0, 0.20, 1.0, 60)
        assert hyp_q > exp_q

    def test_harmonic_closed_form(self):
        # b = 1 → q = qi / (1 + D*t)
        q = monthly_rate(10.0, 0.20, 1.0, 12)
        assert q == pytest.approx(10.0 / 1.20, rel=1e-12)

    def test_zero_initial_rate_stays_zero(self):
        assert monthly_rate(0.0, 0.10, 0.0, 5) == 0.0


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

class TestValidation:
    def test_negative_cost_rejected(self):
        with pytest.raises(ValueError, match="fixed_opex_usd_per_month"):
            healthy_costs(fixed_opex_usd_per_month=-1.0)

    def test_b_factor_out_of_range_rejected(self):
        with pytest.raises(ValueError, match="b_factor"):
            stripper_well(b_factor=1.5)

    def test_royalty_out_of_range_rejected(self):
        with pytest.raises(ValueError, match="royalty_fraction"):
            stripper_well(royalty_fraction=1.5)

    def test_bad_success_probability_rejected(self):
        with pytest.raises(ValueError, match="success_probability"):
            WorkoverCase(cost_usd=1.0, uplift_bopd=1.0, success_probability=2.0)


# ---------------------------------------------------------------------------
# Interests and price realisation
# ---------------------------------------------------------------------------

class TestRealisation:
    def test_realised_price_deducts_differential(self):
        assert stripper_well().realised_price_usd_per_bbl == pytest.approx(60.0)

    def test_nri_is_wi_times_one_minus_royalty(self):
        w = stripper_well(working_interest_fraction=0.8, royalty_fraction=0.25)
        assert w.net_revenue_interest == pytest.approx(0.6)


# ---------------------------------------------------------------------------
# The core decision
# ---------------------------------------------------------------------------

class TestDecision:
    def test_healthy_stripper_well_is_kept(self):
        r = evaluate_well(stripper_well())
        assert r.decision is Decision.KEEP
        assert r.monthly_net_operating_cash_usd > 0
        assert r.npv_keep_usd > r.npv_plug_now_usd

    def test_every_branch_ends_plugged_so_npvs_are_comparable(self):
        # Plugging cost appears in all branches; none of them is free.
        r = evaluate_well(stripper_well())
        assert r.npv_plug_now_usd == pytest.approx(-45_000.0)
        assert r.pv_plugging_deferred_usd < 0  # still an outflow, just discounted
        assert abs(r.pv_plugging_deferred_usd) < 45_000.0  # ...and worth less

    def test_deferring_pa_is_what_makes_a_losing_well_worth_holding(self):
        """The module's whole thesis: a well can lose money monthly and still
        beat plugging, because plugging is deferred rather than avoided."""
        w = stripper_well(oil_rate_bopd=0.5, water_rate_bwpd=60.0)
        r = evaluate_well(w)
        assert r.monthly_net_operating_cash_usd < 0     # loses money producing
        assert r.decision is not Decision.KEEP           # so don't produce it
        assert r.npv_shut_in_usd > r.npv_plug_now_usd    # but don't plug it either
        assert r.decision is Decision.SHUT_IN

    def test_plug_now_wins_when_holding_costs_exceed_deferral_value(self):
        # Heavy idle-well carry destroys the deferral benefit.
        w = stripper_well(
            oil_rate_bopd=0.2,
            costs=healthy_costs(
                shut_in_opex_usd_per_month=2_000.0,
                annual_carry_usd=12_000.0,
            ),
        )
        r = evaluate_well(w)
        assert r.decision is Decision.PLUG_NOW
        assert "Plug now" in r.rationale

    def test_keep_is_withdrawn_when_no_month_is_cash_positive(self):
        """Regression: a well with its economic limit at month 1 used to be
        reported as KEEP, because deferring the P&A bill by a single month
        scores marginally better than plugging today. That is a discounting
        artefact — 'keep producing' must not be offered when there is nothing
        profitable to produce."""
        w = stripper_well(
            oil_rate_bopd=0.2,
            costs=healthy_costs(
                shut_in_opex_usd_per_month=2_000.0,
                annual_carry_usd=12_000.0,
            ),
        )
        r = evaluate_well(w)
        assert r.cashflows == []
        assert r.economic_limit_month == 1
        assert r.decision is Decision.PLUG_NOW
        assert any("not offered as an option" in msg for msg in r.warnings)

    def test_zero_plugging_cost_emits_a_warning(self):
        r = evaluate_well(stripper_well(costs=healthy_costs(plugging_cost_usd=0.0)))
        assert any("plugging_cost_usd is zero" in w for w in r.warnings)

    def test_bond_release_warns_that_refundability_is_regulatory(self):
        r = evaluate_well(stripper_well(costs=healthy_costs(bond_release_usd=10_000.0)))
        assert any("refundable" in w for w in r.warnings)

    def test_bond_release_reduces_the_net_pa_bill(self):
        no_bond = evaluate_well(stripper_well())
        with_bond = evaluate_well(stripper_well(costs=healthy_costs(bond_release_usd=10_000.0)))
        assert with_bond.npv_plug_now_usd == pytest.approx(no_bond.npv_plug_now_usd + 10_000.0)

    def test_rationale_is_present_and_names_the_runner_up_margin(self):
        r = evaluate_well(stripper_well())
        assert r.rationale
        assert "$" in r.rationale


class TestWorkover:
    def test_good_workover_is_recommended(self):
        w = stripper_well(
            oil_rate_bopd=3.0,
            workover=WorkoverCase(cost_usd=18_000.0, uplift_bopd=5.0),
        )
        r = evaluate_well(w)
        assert r.decision is Decision.WORKOVER
        assert r.npv_workover_usd > r.npv_keep_usd

    def test_overpriced_workover_is_rejected(self):
        w = stripper_well(workover=WorkoverCase(cost_usd=500_000.0, uplift_bopd=1.0))
        r = evaluate_well(w)
        assert r.decision is not Decision.WORKOVER
        assert r.npv_workover_usd < r.npv_keep_usd

    def test_success_probability_scales_uplift_but_not_cost(self):
        certain = evaluate_well(stripper_well(
            oil_rate_bopd=3.0,
            workover=WorkoverCase(cost_usd=18_000.0, uplift_bopd=5.0, success_probability=1.0),
        ))
        risked = evaluate_well(stripper_well(
            oil_rate_bopd=3.0,
            workover=WorkoverCase(cost_usd=18_000.0, uplift_bopd=5.0, success_probability=0.5),
        ))
        assert risked.npv_workover_usd < certain.npv_workover_usd

    def test_no_workover_case_gives_none(self):
        assert evaluate_well(stripper_well()).npv_workover_usd is None


# ---------------------------------------------------------------------------
# Economic limit and breakeven
# ---------------------------------------------------------------------------

class TestLimitAndBreakeven:
    def test_declining_well_reaches_an_economic_limit(self):
        r = evaluate_well(stripper_well(decline_rate_per_yr=0.30))
        assert r.economic_limit_month is not None
        assert 0 < r.economic_limit_month <= 240

    def test_flat_profitable_well_has_no_limit_in_horizon(self):
        assert economic_limit_month(stripper_well(decline_rate_per_yr=0.0)) is None

    def test_breakeven_price_zeroes_month_one_cash(self):
        from dataclasses import replace

        w = stripper_well()
        be = price_breakeven(w)
        assert be is not None
        at_be = evaluate_well(replace(w, oil_price_usd_per_bbl=be))
        assert at_be.monthly_net_operating_cash_usd == pytest.approx(0.0, abs=5.0)

    def test_below_breakeven_the_well_loses_money(self):
        from dataclasses import replace

        w = stripper_well()
        be = price_breakeven(w)
        below = evaluate_well(replace(w, oil_price_usd_per_bbl=be - 10.0))
        assert below.monthly_net_operating_cash_usd < 0

    def test_more_water_raises_the_breakeven_price(self):
        dry = price_breakeven(stripper_well(water_rate_bwpd=5.0))
        wet = price_breakeven(stripper_well(water_rate_bwpd=200.0))
        assert wet > dry

    def test_no_oil_rate_has_no_breakeven(self):
        assert price_breakeven(stripper_well(oil_rate_bopd=0.0)) is None


# ---------------------------------------------------------------------------
# Cash-flow mechanics
# ---------------------------------------------------------------------------

class TestCashflows:
    def test_cashflows_stop_at_the_economic_limit(self):
        r = evaluate_well(stripper_well(decline_rate_per_yr=0.30))
        assert r.economic_limit_month is not None
        assert len(r.cashflows) == r.economic_limit_month - 1
        assert all(cf.net_cash_usd >= 0 for cf in r.cashflows)

    def test_discounting_reduces_later_months(self):
        r = evaluate_well(stripper_well())
        first, last = r.cashflows[0], r.cashflows[-1]
        assert abs(last.discounted_net_cash_usd / last.net_cash_usd) < abs(
            first.discounted_net_cash_usd / first.net_cash_usd
        )

    def test_working_interest_scales_both_revenue_and_cost(self):
        full = evaluate_well(stripper_well(working_interest_fraction=1.0))
        half = evaluate_well(stripper_well(working_interest_fraction=0.5))
        assert half.cashflows[0].revenue_usd == pytest.approx(
            full.cashflows[0].revenue_usd * 0.5
        )
        assert half.cashflows[0].opex_usd == pytest.approx(
            full.cashflows[0].opex_usd * 0.5
        )

    def test_severance_tax_reduces_net_revenue(self):
        taxed = evaluate_well(stripper_well(severance_tax_fraction=0.046))
        free = evaluate_well(stripper_well(severance_tax_fraction=0.0))
        assert taxed.cashflows[0].revenue_usd < free.cashflows[0].revenue_usd
