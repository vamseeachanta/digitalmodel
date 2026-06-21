"""Tests for the API 17W capping-stack bounded mechanics core (#490)."""

import math

import pytest

from digitalmodel.subsea.capping_stack import (
    GAS_COLUMN_DENSITY,
    check_containment,
    shut_in_wellhead_pressure,
    PressureClass,
    ConnectorClass,
    WellheadSpec,
    CappingStackSpec,
    check_compatibility,
    CappingStack,
    DeploymentScenario,
    feasibility,
)

PSI_TO_PA = 6894.757


# --------------------------------------------------------------------------- #
# Containment pressure adequacy (central check)
# --------------------------------------------------------------------------- #

def test_containment_hand_verified_within_limit():
    """15K stack vs 12,000 psi reservoir / 5000 m gas column.

    Hand calc (see issue-490.md):
      SIWHP = 12000 psi - 200*9.80665*5000 Pa = 72,930,434 Pa (10,577.5 psi)
      RWP(15K) = 103,421,355 Pa ; U = 0.7052 ; contained.
    """
    res = check_containment(
        rated_working_pressure=15000 * PSI_TO_PA,
        reservoir_pressure=12000 * PSI_TO_PA,
        datum_to_mudline_tvd=5000.0,
        column_density=GAS_COLUMN_DENSITY,
    )
    assert res.siwhp == pytest.approx(72_930_434.0, rel=1e-4)
    assert res.utilisation == pytest.approx(0.7052, rel=1e-3)
    assert res.is_contained is True


def test_containment_over_limit_fails():
    """A 10K stack cannot contain the same well (SIWHP > RWP)."""
    res = check_containment(
        rated_working_pressure=10000 * PSI_TO_PA,
        reservoir_pressure=12000 * PSI_TO_PA,
        datum_to_mudline_tvd=5000.0,
    )
    assert res.utilisation > 1.0
    assert res.is_contained is False


def test_containment_boundary_exactly_unity():
    """SIWHP exactly equal to RWP is contained (U == 1.0, inclusive)."""
    rwp = 50_000_000.0
    h = 5000.0
    # choose reservoir so SIWHP == rwp exactly
    p_res = rwp + GAS_COLUMN_DENSITY * 9.80665 * h
    res = check_containment(
        rated_working_pressure=rwp,
        reservoir_pressure=p_res,
        datum_to_mudline_tvd=h,
    )
    assert res.utilisation == pytest.approx(1.0, rel=1e-9)
    assert res.is_contained is True


def test_siwhp_clamped_when_column_overcomes_reservoir():
    """Heavy column heavier than reservoir pressure -> no flow to surface."""
    siwhp = shut_in_wellhead_pressure(
        reservoir_pressure=10_000_000.0,
        datum_to_mudline_tvd=5000.0,
        column_density=1030.0,  # brine, hydrostatic > reservoir
    )
    assert siwhp == 0.0


def test_containment_rejects_nonpositive_rwp():
    with pytest.raises(ValueError):
        check_containment(0.0, 80e6, 5000.0)


# --------------------------------------------------------------------------- #
# Interface compatibility
# --------------------------------------------------------------------------- #

def _wh(pc=PressureClass.PSI_15K):
    return WellheadSpec(
        bore_inch=18.75, pressure_class=pc, connector_class=ConnectorClass.H4_18_75
    )


def test_interface_match_15k():
    stack = CappingStackSpec(
        bore_inch=18.75,
        pressure_class=PressureClass.PSI_15K,
        connector_class=ConnectorClass.H4_18_75,
    )
    res = check_compatibility(stack, _wh())
    assert res.compatible is True
    assert res.reasons == []


def test_interface_pressure_mismatch_10k_on_15k_wellhead():
    """10K stack on a 15K wellhead fails the pressure category."""
    stack = CappingStackSpec(
        bore_inch=18.75,
        pressure_class=PressureClass.PSI_10K,
        connector_class=ConnectorClass.H4_18_75,
    )
    res = check_compatibility(stack, _wh(PressureClass.PSI_15K))
    assert res.compatible is False
    assert any("pressure" in r for r in res.reasons)


def test_interface_higher_rated_stack_passes_pressure():
    """A 20K stack on a 15K wellhead is acceptable on pressure."""
    stack = CappingStackSpec(
        bore_inch=18.75,
        pressure_class=PressureClass.PSI_20K,
        connector_class=ConnectorClass.H4_18_75,
    )
    res = check_compatibility(stack, _wh(PressureClass.PSI_15K))
    assert res.compatible is True


def test_interface_bore_and_connector_mismatch():
    stack = CappingStackSpec(
        bore_inch=16.75,
        pressure_class=PressureClass.PSI_15K,
        connector_class=ConnectorClass.HUB_16,
    )
    res = check_compatibility(stack, _wh(PressureClass.PSI_15K))
    assert res.compatible is False
    assert any("bore" in r for r in res.reasons)
    assert any("connector" in r for r in res.reasons)


# --------------------------------------------------------------------------- #
# Deployment lift feasibility
# --------------------------------------------------------------------------- #

def test_deployment_hand_verified_feasible():
    """30 te stack, 600 kN crane, benign sea state.

    Hand calc: V_disp = 30000/4000 = 7.5 m^3
      W_sub = (30000 - 1025*7.5)*9.80665 = 218,805.9 N
      F_hook = 2.0 * W_sub = 437,611.8 N ; U = 0.7294 ; feasible.
    """
    stack = CappingStack(mass_air=30_000.0)
    scenario = DeploymentScenario(
        crane_swl=600_000.0, water_depth=2000.0, hs=1.5, hs_limit=2.5
    )
    res = feasibility(stack, scenario)
    assert res.submerged_weight == pytest.approx(218_805.9, rel=1e-4)
    assert res.hook_load == pytest.approx(437_611.8, rel=1e-4)
    assert res.lift_utilisation == pytest.approx(0.7294, rel=1e-3)
    assert res.feasible is True
    assert res.governing_constraint == "none"


def test_deployment_lift_overload_governs():
    """Same stack, undersized crane -> lift governs, infeasible."""
    stack = CappingStack(mass_air=30_000.0)
    scenario = DeploymentScenario(
        crane_swl=300_000.0, water_depth=2000.0, hs=1.0, hs_limit=2.5
    )
    res = feasibility(stack, scenario)
    assert res.feasible is False
    assert res.governing_constraint == "lift"
    assert res.lift_utilisation > 1.0


def test_deployment_sea_state_governs():
    """Lift OK but storm exceeds Hs limit -> sea_state governs."""
    stack = CappingStack(mass_air=30_000.0)
    scenario = DeploymentScenario(
        crane_swl=600_000.0, water_depth=2000.0, hs=3.5, hs_limit=2.5
    )
    res = feasibility(stack, scenario)
    assert res.feasible is False
    assert res.governing_constraint == "sea_state"


def test_deployment_explicit_displaced_volume():
    stack = CappingStack(mass_air=30_000.0, displaced_volume=10.0)
    scenario = DeploymentScenario(
        crane_swl=600_000.0, water_depth=2000.0, hs=1.0, hs_limit=2.5
    )
    res = feasibility(stack, scenario)
    expected_wsub = (30_000.0 - 1025.0 * 10.0) * 9.80665
    assert res.submerged_weight == pytest.approx(expected_wsub, rel=1e-9)
    assert res.notes == []  # no default-volume note


def test_deployment_rejects_nonpositive_swl():
    stack = CappingStack(mass_air=30_000.0)
    scenario = DeploymentScenario(
        crane_swl=0.0, water_depth=2000.0, hs=1.0, hs_limit=2.5
    )
    with pytest.raises(ValueError):
        feasibility(stack, scenario)
