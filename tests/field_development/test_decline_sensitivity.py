# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Decline sensitivity contract from issue #1777, using synthetic cashflows.

All scenario numbers below are chosen synthetic inputs, not field estimates.
The three-year example produces one million barrels annually at plateau:
capacity = 1,000,000 / 365 bbl/day, price = 10, OPEX = 2 USD/bbl.
CAPEX = 10 USD MM is paid 30/50/20 percent in years 0/1/2; ABEX = 1
USD MM in year 3. The existing schedule's 60 percent plateau lasts one year.
Thus the cashflows (USD MM) are [-3, 3, 8*f(1)-2, 8*f(2)-1].
The expectations explicitly discount these sums without using schedule helpers.
Other rates, costs and model parameters are synthetic boundary/test inputs.
"""

from copy import deepcopy
from math import exp, log
import sys

import pytest

from digitalmodel.field_development import economics as ec


MODELS = [ec.DeclineType.EXPONENTIAL, ec.DeclineType.HYPERBOLIC,
          ec.DeclineType.HARMONIC]
# Default grid specified in issue #1777, Requested Change.
DEFAULT_RATES = [0.05, 0.10, 0.15, 0.20, 0.25, 0.30]


@pytest.fixture
def scenario(monkeypatch):
    """Exercise real evaluator fallback with optional adapters unavailable."""
    for name in ("worldenergydata.fdas",
                 "worldenergydata.eia_us.analysis.us_fiscal"):
        monkeypatch.setitem(sys.modules, name, None)
    return ec.EconomicsInput(
        field_name="Synthetic", water_depth_m=1000.0, host_type="TLP",
        production_capacity_bopd=1_000_000.0 / 365.0,
        oil_price_usd_per_bbl=10.0, discount_rate=0.10,
        fiscal_regime=ec.FiscalRegime.US, field_life_years=3,
        opex_usd_per_bbl=2.0, capex_usd_mm=10.0, abex_usd_mm=1.0,
        b_factor=0.5,
    )


@pytest.mark.parametrize("model", MODELS)
def test_decline_sweep_does_not_mutate_input(scenario, model, monkeypatch):
    before = deepcopy(scenario)
    rates = [0.30, 0.05, 0.15]
    seen = []
    evaluator = ec.evaluate_economics

    def record(inp):
        seen.append(inp)
        result = evaluator(inp)
        # An adapter mutating a copy must not contaminate later cases.
        assert inp.region is None
        inp.region = "synthetic"
        return result

    monkeypatch.setattr(ec, "evaluate_economics", record)
    result = ec.decline_sensitivity(scenario, model, rates, discount_rate=0.20)
    assert scenario == before
    assert rates == [0.30, 0.05, 0.15]
    assert all(inp is not scenario for inp in seen)
    assert len({id(inp) for inp in seen}) == len(rates) + 1
    # Original LINEAR schedule has f(1)=0.6 and f(2)=0.2, at original 10%.
    assert result.base_npv == pytest.approx(
        -3 + 3 / 1.1 + (8 * 0.6 - 2) / 1.1**2 + (8 * 0.2 - 1) / 1.1**3
    )


@pytest.mark.parametrize("model", MODELS)
def test_decline_models_return_ordered_rates(scenario, model):
    result = ec.decline_sensitivity(scenario, model, [0.30, 0.05, 0.15, 0.15])
    assert isinstance(result, ec.DeclineSensitivityResult)
    assert result.decline_rates == [0.05, 0.15, 0.15, 0.30]
    assert len(result.npv_values) == len(result.decline_rates)
    assert result.npv_values[1] == result.npv_values[2]


@pytest.mark.parametrize("model", MODELS)
def test_decline_npv_decreases_with_rate(scenario, model):
    result = ec.decline_sensitivity(scenario, model)
    assert result.decline_rates == DEFAULT_RATES
    assert all(a > b for a, b in zip(result.npv_values, result.npv_values[1:]))


@pytest.mark.parametrize("model, factors", [
    (ec.DeclineType.EXPONENTIAL, (exp(-0.2), exp(-0.4))),
    (ec.DeclineType.HYPERBOLIC, (1 / 1.1**2, 1 / 1.2**2)),
    (ec.DeclineType.HARMONIC, (1 / 1.2, 1 / 1.4)),
])
def test_decline_npv_matches_hand_discounted_schedule(scenario, model, factors):
    result = ec.decline_sensitivity(scenario, model, [0.2])
    f1, f2 = factors
    expected = -3 + 3 / 1.1 + (8 * f1 - 2) / 1.1**2 + (8 * f2 - 1) / 1.1**3
    assert result.npv_values == pytest.approx([expected])


def test_decline_breakeven_interpolates_sign_change(scenario):
    # At zero discount, exponential D=ln(2) gives f=(1/2,1/4): NPV=3.
    # D=ln(4) gives f=(1/4,1/16): NPV=-0.5. Straight-line root is
    # ln(2) + (3 / 3.5) * (ln(4)-ln(2)). These are hand cashflow sums.
    result = ec.decline_sensitivity(scenario, decline_rates=[log(4), log(2)],
                                    discount_rate=0.0)
    assert result.npv_values == pytest.approx([3.0, -0.5])
    expected = log(2) + (3 / 3.5) * (log(4) - log(2))
    assert result.breakeven_decline_rate == pytest.approx(expected)
    assert log(2) < result.breakeven_decline_rate < log(4)


@pytest.mark.parametrize("capex", [0.0, 100.0])
def test_decline_no_crossing(scenario, capex):
    scenario.capex_usd_mm = capex
    result = ec.decline_sensitivity(scenario)
    assert result.breakeven_decline_rate is None
    assert all(v > 0 for v in result.npv_values) or all(v < 0 for v in result.npv_values)


@pytest.mark.parametrize("model", MODELS)
def test_decline_zero_rate_is_constant_production(scenario, model):
    result = ec.decline_sensitivity(scenario, model, [0.0])
    # f(1)=f(2)=1 in each curve's zero-decline limit.
    expected = -3 + 3 / 1.1 + 6 / 1.1**2 + 7 / 1.1**3
    assert result.npv_values == pytest.approx([expected])


def test_decline_empty_grid(scenario):
    result = ec.decline_sensitivity(scenario, decline_rates=[])
    assert result.decline_rates == []
    assert result.npv_values == []
    assert result.breakeven_decline_rate is None
    assert result.base_npv == pytest.approx(
        -3 + 3 / 1.1 + (8 * 0.6 - 2) / 1.1**2 + (8 * 0.2 - 1) / 1.1**3
    )


@pytest.mark.parametrize("rate", [-0.1, float("nan"), float("inf"), -float("inf")])
def test_decline_invalid_grid(scenario, rate):
    with pytest.raises(ValueError, match="decline_rates"):
        ec.decline_sensitivity(scenario, decline_rates=[0.1, rate])


@pytest.mark.parametrize("rate", [-0.1, 1.1, float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("override", [False, True])
def test_decline_invalid_discount_rate(scenario, rate, override):
    kwargs = {"discount_rate": rate} if override else {}
    if not override:
        scenario.discount_rate = rate
    with pytest.raises(ValueError, match="discount_rate"):
        ec.decline_sensitivity(scenario, **kwargs)


@pytest.mark.parametrize("rate", [0.0, 0.20, 1.0])
def test_decline_discount_override(scenario, rate):
    # D=ln(2) produces f=(1/2,1/4), so cashflows are [-3,3,2,1].
    result = ec.decline_sensitivity(scenario, decline_rates=[log(2)], discount_rate=rate)
    expected = -3 + 3 / (1 + rate) + 2 / (1 + rate)**2 + 1 / (1 + rate)**3
    assert result.npv_values == pytest.approx([expected])
    assert scenario.discount_rate == 0.10


@pytest.mark.parametrize("values, expected", [
    ([4.0, -4.0, 4.0], 0.15),  # two crossings: first sorted bracket
    ([0.0, 4.0, -4.0], 0.10),  # exact zero precedes later crossing
    ([4.0, -4.0, 0.0], 0.15),  # later exact zero must not mask first crossing
    ([4.0, 0.0, -4.0], 0.20),  # exact middle point
    ([4.0, 4.0, 0.0], 0.30),  # exact final point
    ([0.0, 0.0, 0.0], 0.10),  # zero plateau: lowest rate
])
def test_decline_first_crossing_policy(scenario, monkeypatch, values, expected):
    # Synthetic evaluator outputs isolate interpolation from fiscal behaviour.
    # Equal and opposite 4 USD MM points put roots at bracket midpoints.
    evaluator = ec.evaluate_economics
    by_rate = dict(zip([0.1, 0.2, 0.3], values))

    def controlled(inp):
        result = evaluator(inp)
        if inp.decline_type == ec.DeclineType.EXPONENTIAL:
            result.metrics.npv_usd_mm = by_rate[inp.decline_rate]
        return result

    monkeypatch.setattr(ec, "evaluate_economics", controlled)
    result = ec.decline_sensitivity(scenario, decline_rates=[0.3, 0.1, 0.2])
    assert result.breakeven_decline_rate == pytest.approx(expected)


@pytest.mark.parametrize("model", [ec.DeclineType.LINEAR, "unknown"])
def test_decline_rejects_models_without_rate_sweep(scenario, model):
    with pytest.raises(ValueError, match="decline_type"):
        ec.decline_sensitivity(scenario, model)


@pytest.mark.parametrize("b", [0.0, -0.5, 1.1, float("nan"), float("inf")])
def test_decline_validates_hyperbolic_shape(scenario, b):
    scenario.b_factor = b  # LINEAR input does not validate this parameter.
    with pytest.raises(ValueError, match="b_factor"):
        ec.decline_sensitivity(scenario, ec.DeclineType.HYPERBOLIC)


@pytest.mark.parametrize("model, factors", [
    (ec.DeclineType.EXPONENTIAL, (exp(-0.4), exp(-0.8))),
    (ec.DeclineType.HYPERBOLIC, (1 / 1.2**2, 1 / 1.4**2)),
    (ec.DeclineType.HARMONIC, (1 / 1.4, 1 / 1.8)),
])
def test_decline_base_preserves_original_curve(scenario, model, factors):
    scenario.decline_type = model
    scenario.decline_rate = 0.4
    before = deepcopy(scenario)
    result = ec.decline_sensitivity(scenario, decline_rates=[0.1], discount_rate=0.2)
    f1, f2 = factors
    expected = -3 + 3 / 1.1 + (8 * f1 - 2) / 1.1**2 + (8 * f2 - 1) / 1.1**3
    assert result.base_npv == pytest.approx(expected)
    assert scenario == before


def test_decline_override_does_not_hide_invalid_base_discount(scenario):
    scenario.discount_rate = float("nan")
    with pytest.raises(ValueError, match="discount_rate"):
        ec.decline_sensitivity(scenario, discount_rate=0.1)


def test_decline_package_exports():
    from digitalmodel import field_development

    assert field_development.decline_sensitivity is ec.decline_sensitivity
    assert field_development.DeclineSensitivityResult is ec.DeclineSensitivityResult
