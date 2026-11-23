"""Tests for the cost-driver sensitivity sweep (issue #1223).

Anchors: the deck's single-driver sensitivities (slides 7, 15, 16) that the
core engine reproduces within +/-2%. Vessel-rate *doubling* is asserted for
sign only (it scales the bundled installation line; see module docstring).
"""

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.sweep import (
    DriverScenario,
    LeverChange,
    apply_change,
    run_sweep,
)

_TOL = 0.02

# Deck single-driver points reproduced within +/-2%.
_DECK_SCENARIOS = [
    (DriverScenario(
        name="design_life_30yr",
        changes=[LeverChange(path="financial.design_life_years", value=30)],
    ), 178.0),
    (DriverScenario(
        name="substructure_-25%",
        changes=[LeverChange(path="capex.substructure", scale=0.75)],
    ), 172.0),
    (DriverScenario(
        name="substructure_-50%",
        changes=[LeverChange(path="capex.substructure", scale=0.50)],
    ), 161.0),
    (DriverScenario(
        name="turbine_-25%",
        changes=[LeverChange(path="capex.turbine", scale=0.75)],
    ), 173.0),
    (DriverScenario(
        name="turbine_-50%",
        changes=[LeverChange(path="capex.turbine", scale=0.50)],
    ), 162.0),
    (DriverScenario(
        name="installation_-50%",
        changes=[LeverChange(path="capex.installation", scale=0.50)],
    ), 177.0),
]


@pytest.fixture
def econ():
    return base_case()


@pytest.mark.parametrize(
    "scenario,expected", _DECK_SCENARIOS, ids=[s.name for s, _ in _DECK_SCENARIOS]
)
def test_reproduces_deck_single_driver(econ, scenario, expected):
    (row,) = run_sweep(econ, [scenario])
    assert row.lcoe_usd_per_mwh == pytest.approx(expected, rel=_TOL)


def test_sweep_row_delta_and_pct(econ):
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    (row,) = run_sweep(
        econ,
        [DriverScenario(name="cf_up", changes=[LeverChange(path="capacity_factor", value=0.60)])],
    )
    assert row.lcoe_usd_per_mwh < base
    assert row.delta_vs_base == pytest.approx(row.lcoe_usd_per_mwh - base)
    assert row.pct_vs_base == pytest.approx(100.0 * row.delta_vs_base / base)


def test_vessel_rate_doubling_sign_only(econ):
    """Doubling the bundled installation line raises LCOE (magnitude approx)."""
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    (row,) = run_sweep(
        econ,
        [DriverScenario(name="install_x2", changes=[LeverChange(path="capex.installation", scale=2.0)])],
    )
    assert row.lcoe_usd_per_mwh > base


def test_combined_levers_apply_together(econ):
    """A scenario with several changes reproduces a compound reduction."""
    scenario = DriverScenario(
        name="fab_and_life",
        changes=[
            LeverChange(path="capex.substructure", scale=0.5),
            LeverChange(path="capex.turbine", scale=0.75),
            LeverChange(path="financial.design_life_years", value=30),
        ],
    )
    (row,) = run_sweep(econ, [scenario])
    # Each lever cuts LCOE; combined must beat any single one.
    singles = run_sweep(econ, [DriverScenario(name=c.path, changes=[c]) for c in scenario.changes])
    assert row.lcoe_usd_per_mwh < min(s.lcoe_usd_per_mwh for s in singles)


def test_apply_change_preserves_int_field(econ):
    changed = apply_change(econ, LeverChange(path="financial.design_life_years", scale=1.2))
    assert isinstance(changed.financial.design_life_years, int)
    assert changed.financial.design_life_years == 30  # round(25*1.2)


def test_apply_change_does_not_mutate_original(econ):
    before = econ.capex.substructure
    apply_change(econ, LeverChange(path="capex.substructure", scale=0.5))
    assert econ.capex.substructure == before


def test_lever_requires_exactly_one_operation():
    with pytest.raises(ValidationError):
        LeverChange(path="capacity_factor")  # neither
    with pytest.raises(ValidationError):
        LeverChange(path="capacity_factor", scale=0.9, value=0.5)  # both


def test_unknown_path_rejected(econ):
    with pytest.raises(ValueError):
        apply_change(econ, LeverChange(path="capex.nonesuch", scale=0.5))
    with pytest.raises(ValueError):
        apply_change(econ, LeverChange(path="a.b.c", scale=0.5))
