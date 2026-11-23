"""Tests for LCOE as a trade-space objective (issue #1224).

Builds lightweight screening variants that differ in hull steel mass and checks
that LCOE tracks steel mass, that the Pareto front mixes LCOE with a physical
objective, and that the base engine's LCOE is recovered at the reference mass.
"""

import pytest

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.floaters import FloaterArchetype, FloaterProperties
from digitalmodel.floating_wind.screening import VariantScreening
from digitalmodel.floating_wind.tradespace_economics import (
    LCOE_METRIC_KEY,
    lcoe_records,
    mean_steel_mass_t,
    pareto_front_with_lcoe,
    variant_lcoe,
)


def _props(steel_mass_t: float) -> FloaterProperties:
    """A plausible semi FloaterProperties differing only in steel mass."""
    return FloaterProperties(
        archetype=FloaterArchetype.SEMI,
        draft_m=20.0,
        displacement_t=16000.0,
        steel_mass_t=steel_mass_t,
        ballast_mass_t=2000.0,
        topside_mass_t=2280.0,
        total_mass_t=steel_mass_t + 4280.0,
        waterplane_area_m2=350.0,
        KB_m=10.0,
        BM_m=25.0,
        KG_m=18.0,
        GM_m=17.0,
        heave_natural_period_s=20.0,
        pitch_natural_period_s=28.0,
        pitch_radius_gyration_m=30.0,
        tendon_stabilised=False,
        feasible=True,
    )


def _variant(case_id: str, steel_mass_t: float, *, passed: bool = True) -> VariantScreening:
    return VariantScreening(
        case_id=case_id,
        archetype=FloaterArchetype.SEMI,
        params={"steel_mass_t": steel_mass_t},
        properties=_props(steel_mass_t),
        checks=[],
        feasible=True,
        passed=passed,
        governing_check="stability",
        governing_margin=1.2,
    )


@pytest.fixture
def econ():
    return base_case()


@pytest.fixture
def variants():
    return [
        _variant("light", 3000.0),
        _variant("mid", 4000.0),
        _variant("heavy", 5000.0),
    ]


def test_lcoe_at_reference_mass_equals_base(econ):
    """A variant at exactly the reference steel mass reproduces the base LCOE."""
    v = _variant("ref", 4000.0)
    lcoe = variant_lcoe(v, econ, reference_steel_mass_t=4000.0)
    assert lcoe == pytest.approx(compute_lcoe(econ).lcoe_usd_per_mwh, rel=1e-9)


def test_heavier_hull_costs_more(econ):
    ref = 4000.0
    light = variant_lcoe(_variant("l", 3000.0), econ, reference_steel_mass_t=ref)
    heavy = variant_lcoe(_variant("h", 5000.0), econ, reference_steel_mass_t=ref)
    assert light < heavy


def test_lcoe_records_attach_metric(econ, variants):
    recs = lcoe_records(variants, econ)
    assert all(LCOE_METRIC_KEY in r for r in recs)
    assert len(recs) == 3
    # Ordered light->heavy by construction; LCOE must be increasing.
    lcoes = [r[LCOE_METRIC_KEY] for r in recs]
    assert lcoes == sorted(lcoes)


def test_mean_steel_mass_reference(variants):
    assert mean_steel_mass_t(variants) == pytest.approx(4000.0)


def test_pareto_front_mixes_lcoe_and_physical(econ, variants):
    """With LCOE(min) and steel_mass(min) both driven by steel mass, the
    lightest variant dominates -> a single-point front."""
    front = pareto_front_with_lcoe(
        variants,
        econ,
        [("lcoe_usd_per_mwh", "min"), ("steel_mass_t", "min")],
    )
    assert len(front) == 1
    assert front[0]["case_id"] == "light"


def test_pareto_front_tradeoff_gives_multiple(econ):
    """When objectives conflict, the front keeps the non-dominated set."""
    # Heavier hull (worse LCOE) but better stability margin -> both on the front.
    a = _variant("cheap_wobbly", 3000.0)
    b = _variant("dear_stable", 5000.0)
    b.properties.GM_m = 30.0  # more stable
    front = pareto_front_with_lcoe(
        [a, b],
        econ,
        [("lcoe_usd_per_mwh", "min"), ("GM_m", "max")],
    )
    ids = {r["case_id"] for r in front}
    assert ids == {"cheap_wobbly", "dear_stable"}


def test_passed_only_filter(econ):
    variants = [
        _variant("ok", 3500.0, passed=True),
        _variant("failed", 3000.0, passed=False),
    ]
    front = pareto_front_with_lcoe(
        variants, econ, [("lcoe_usd_per_mwh", "min")], passed_only=True
    )
    assert {r["case_id"] for r in front} == {"ok"}


def test_zero_reference_rejected(econ):
    with pytest.raises(ValueError):
        variant_lcoe(_variant("x", 4000.0), econ, reference_steel_mass_t=0.0)
