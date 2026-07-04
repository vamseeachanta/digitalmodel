"""Tests for turbine-size & farm-size economies of scale (issue #1249).

Anchors: the deck's turbine/farm points (slide 8). The compound 2040 pathway
(slide 6) is asserted for *direction and magnitude* only — its exact $101/$51
depend on the deck's unspecified allocation of the "50% less fab cost" reduction,
which a clean decomposition brackets rather than pins.
"""

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.reliability import (
    ReliabilityScenario,
    lcoe_with_reliability,
)
from digitalmodel.floating_wind.scaling import (
    ComponentScaling,
    EconomiesOfScale,
    default_scaling,
    lcoe_at_scale,
    scale_economics,
)
from digitalmodel.floating_wind.sweep import DriverScenario, LeverChange, run_sweep

_TOL = 0.03  # +/- 3%


@pytest.fixture
def econ():
    return base_case()


def test_identity_at_reference(econ):
    """Scaling to the reference design must reproduce the $184 base exactly."""
    lcoe = lcoe_at_scale(econ, turbine_rating_mw=15.0, turbine_count=33).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(compute_lcoe(econ).lcoe_usd_per_mwh, rel=1e-9)
    assert lcoe == pytest.approx(184.0, rel=_TOL)


def test_deck_turbine_size_point(econ):
    """20 MW x 33 = 660 MW -> ~$161 (deck slide 8)."""
    lcoe = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=33).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(161.0, rel=_TOL)


def test_deck_farm_size_point(econ):
    """20 MW x 25 = 500 MW -> ~$172 (deck slide 8)."""
    lcoe = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=25).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(172.0, rel=_TOL)


def test_bigger_turbine_lowers_lcoe(econ):
    small = lcoe_at_scale(econ, turbine_rating_mw=15.0, turbine_count=33).lcoe_usd_per_mwh
    big = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=33).lcoe_usd_per_mwh
    assert big < small


def test_bigger_farm_lowers_lcoe_at_fixed_rating(econ):
    """At 20 MW, a 660 MW farm beats a 500 MW farm (deck: $161 < $172)."""
    farm_660 = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=33).lcoe_usd_per_mwh
    farm_500 = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=25).lcoe_usd_per_mwh
    assert farm_660 < farm_500


def test_scale_economics_updates_size(econ):
    scaled = scale_economics(econ, turbine_rating_mw=20.0, turbine_count=25)
    assert scaled.turbine_rating_mw == 20.0
    assert scaled.turbine_count == 25
    assert scaled.farm_capacity_mw == pytest.approx(500.0)
    # per-MW components (turbine, mooring, array) are unchanged $/kW
    assert scaled.capex.turbine == pytest.approx(econ.capex.turbine)
    assert scaled.capex.mooring == pytest.approx(econ.capex.mooring)
    # per-unit substructure and per-farm export drop
    assert scaled.capex.substructure < econ.capex.substructure
    assert scaled.capex.export_cable < econ.capex.export_cable


def test_compound_2040_pathway_trends_down(econ):
    """Deck 2040 base (slide 6): 20 MW, 500 MW, fab -50%, install -50%,
    failures -50%. Assert a large reduction toward the deck's ~$101 region
    (magnitude, not exact value — see module docstring)."""
    base_lcoe = compute_lcoe(econ).lcoe_usd_per_mwh  # 184
    scaled = scale_economics(econ, turbine_rating_mw=20.0, turbine_count=25)
    (fab_install,) = run_sweep(
        scaled,
        [DriverScenario(
            name="fab_install",
            changes=[
                LeverChange(path="capex.substructure", scale=0.5),
                LeverChange(path="capex.installation", scale=0.5),
            ],
        )],
    )
    # apply the reduced costs, then the reliability lever
    reduced = scale_economics(econ, turbine_rating_mw=20.0, turbine_count=25)
    reduced = reduced.model_copy(update={
        "capex": reduced.capex.model_copy(update={
            "substructure": reduced.capex.substructure * 0.5,
            "installation": reduced.capex.installation * 0.5,
        })
    })
    lcoe_2040 = lcoe_with_reliability(
        reduced, ReliabilityScenario(failure_rate_reduction=0.5)
    ).lcoe_usd_per_mwh
    # A deep reduction from $184 toward the deck's ~$101 (bracketed, not pinned).
    assert lcoe_2040 < 140.0
    assert lcoe_2040 < base_lcoe * 0.75
    assert 90.0 < lcoe_2040 < 135.0  # documented bracket around deck $101


def test_full_pathway_reaches_below_2040_base(econ):
    """Adding 1 GW farm + 60% capacity factor + 30-yr life pushes further toward
    the deck's $51/MWh floor."""
    scaled = scale_economics(econ, turbine_rating_mw=20.0, turbine_count=50)  # 1 GW
    fin = scaled.financial.model_copy(update={"design_life_years": 30})
    deep = scaled.model_copy(update={"capacity_factor": 0.60, "financial": fin})
    lcoe_deep = compute_lcoe(deep).lcoe_usd_per_mwh
    lcoe_2040ish = lcoe_at_scale(econ, turbine_rating_mw=20.0, turbine_count=25).lcoe_usd_per_mwh
    assert lcoe_deep < lcoe_2040ish


def test_custom_scaling_overrides_defaults(econ):
    """A pure per-MW scaling (no economies) leaves LCOE flat with size."""
    flat = EconomiesOfScale(reference_turbine_mw=15.0, reference_farm_mw=495.0)
    lcoe = lcoe_at_scale(
        econ, turbine_rating_mw=20.0, turbine_count=33, scaling=flat
    ).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(compute_lcoe(econ).lcoe_usd_per_mwh, rel=1e-9)


def test_component_scaling_fraction_guard():
    with pytest.raises(ValidationError):
        ComponentScaling(per_unit=0.7, per_farm=0.5)  # sum > 1


def test_unknown_capex_component_rejected():
    with pytest.raises(ValidationError):
        EconomiesOfScale(
            reference_turbine_mw=15.0,
            reference_farm_mw=495.0,
            capex={"nonesuch": ComponentScaling(per_unit=0.5)},
        )


def test_default_scaling_loads():
    sc = default_scaling()
    assert sc.reference_turbine_mw == 15.0
    assert sc.reference_farm_mw == 495.0
    assert sc.capex["substructure"].per_unit == pytest.approx(0.40)
    assert sc.opex.per_unit == pytest.approx(0.80)
