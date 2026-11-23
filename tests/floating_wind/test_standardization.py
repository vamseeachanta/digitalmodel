"""Tests for the standardization / learning-curve lever (issue #1256).

Learning rate is a calibration input, not a deck number, so tests assert the
mechanism (identity at reference, Wright's-law doubling behaviour, monotonicity)
and that a plausible deployment lands the fab reduction in the deck's -25/-50%
band — not a single forced value.
"""

import math

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.standardization import (
    LearningCurve,
    StandardizationDiscount,
    apply_standardization,
    default_learning_curve,
    lcoe_with_standardization,
)


@pytest.fixture
def econ():
    return base_case()


def test_identity_at_reference_units(econ):
    """At cumulative_units == reference_units the base case is unchanged."""
    lc = default_learning_curve()
    lcoe = lcoe_with_standardization(
        econ, cumulative_units=lc.reference_units, learning=lc
    ).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(compute_lcoe(econ).lcoe_usd_per_mwh, rel=1e-9)
    assert lcoe == pytest.approx(184.0, rel=0.02)


def test_wrights_law_doubling(econ):
    """Each doubling of cumulative units multiplies cost by the progress ratio."""
    lc = LearningCurve(learning_rate=0.90, reference_units=100.0)
    assert lc.factor(100.0) == pytest.approx(1.0)
    assert lc.factor(200.0) == pytest.approx(0.90)          # one doubling
    assert lc.factor(400.0) == pytest.approx(0.81)          # two doublings
    assert lc.factor(800.0) == pytest.approx(0.729)         # three doublings


def test_more_deployment_lowers_lcoe(econ):
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    lc = default_learning_curve()
    more = lcoe_with_standardization(
        econ, cumulative_units=lc.reference_units * 10, learning=lc
    ).lcoe_usd_per_mwh
    assert more < base


def test_learning_reduces_only_target_components(econ):
    lc = LearningCurve(
        learning_rate=0.90, reference_units=100.0, components=["substructure"]
    )
    scaled = apply_standardization(econ, cumulative_units=1000.0, learning=lc)
    assert scaled.capex.substructure < econ.capex.substructure
    assert scaled.capex.turbine == pytest.approx(econ.capex.turbine)
    assert scaled.capex.installation == pytest.approx(econ.capex.installation)


def test_default_scenario_brackets_deck_fab_reduction(econ):
    """A plausible deployment lands substructure fab reduction in -25%..-50%
    (the deck's slide-15 fab-cost levers, now as deployment milestones)."""
    lc = default_learning_curve()
    for multiple, lo, hi in [(10, 0.25, 0.45), (30, 0.35, 0.55)]:
        f = lc.factor(lc.reference_units * multiple)
        reduction = 1.0 - f
        assert lo <= reduction <= hi, (multiple, reduction)


def test_standardization_discount_composes(econ):
    """Learning + JIP33 discount compose multiplicatively on shared components."""
    lc = LearningCurve(learning_rate=0.90, reference_units=100.0, components=["substructure"])
    disc = StandardizationDiscount(fraction=0.10, components=["substructure"])
    only_learn = apply_standardization(econ, cumulative_units=1000.0, learning=lc)
    both = apply_standardization(
        econ, cumulative_units=1000.0, learning=lc, discount=disc
    )
    assert both.capex.substructure == pytest.approx(
        only_learn.capex.substructure * 0.90
    )


def test_discount_only_component(econ):
    disc = StandardizationDiscount(fraction=0.20, components=["development"])
    lc = LearningCurve(components=["substructure"])
    out = apply_standardization(econ, cumulative_units=100.0, learning=lc, discount=disc)
    assert out.capex.development == pytest.approx(econ.capex.development * 0.80)


def test_zero_cumulative_units_rejected(econ):
    with pytest.raises(ValueError):
        default_learning_curve().factor(0.0)


def test_invalid_learning_rate_rejected():
    with pytest.raises(ValidationError):
        LearningCurve(learning_rate=1.5)
    with pytest.raises(ValidationError):
        LearningCurve(learning_rate=0.0)


def test_unknown_component_rejected():
    with pytest.raises(ValidationError):
        LearningCurve(components=["nonesuch"])
    with pytest.raises(ValidationError):
        StandardizationDiscount(fraction=0.1, components=["nope"])


def test_default_learning_curve_loads():
    lc = default_learning_curve()
    assert lc.learning_rate == pytest.approx(0.90)
    assert lc.reference_units == pytest.approx(100.0)
    assert set(lc.components) == {"substructure", "installation"}
