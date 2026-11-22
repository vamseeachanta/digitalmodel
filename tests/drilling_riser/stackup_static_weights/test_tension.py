"""Illustrative static tension cases, independent of private source values."""

import pytest


def test_failed_units_overpull_and_setting_limit():
    from digitalmodel.drilling_riser.stackup_static_weights import (
        TensionInput, minimum_tension,
    )
    inputs = TensionInput(
        flooded_steel_weight_n=400000.0, steel_weight_factor_1=1.04,
        additional_supported_weight_n=20000.0, buoyancy_uplift_n=80000.0,
        buoyancy_factor_1=0.9, internal_fluid_weight_n=300000.0,
        displaced_internal_water_weight_n=200000.0,
        tensioner_count=10, failed_tensioner_count=2,
        lower_package_submerged_weight_n=150000.0,
        minimum_lower_package_overpull_n=30000.0, recoil_overpull_n=10000.0,
        dynamic_tension_limit_n=1000000.0, setting_fraction=0.8,
        tensioner_efficiency=1.0, fleet_angle_factor=1.0,
    )
    result = minimum_tension(inputs)
    slip_n = 400000.0 * 1.04 + 20000.0 - 80000.0 * 0.9 + 300000.0 - 200000.0
    top_n = slip_n * 10.0 / (10.0 - 2.0)
    lower_overpull_n = top_n - slip_n - 150000.0
    extra_n = max(30000.0 - lower_overpull_n, 0.0)
    assert result.factored_weight_n == pytest.approx(400000.0 * 1.04 + 20000.0)
    assert result.factored_buoyancy_n == pytest.approx(80000.0 * 0.9)
    assert result.fluid_correction_n == pytest.approx(300000.0 - 200000.0)
    assert result.slip_ring_tension_n == pytest.approx(slip_n)
    assert result.minimum_top_tension_n == pytest.approx(top_n)
    assert result.package_overpull_n == pytest.approx(lower_overpull_n)
    assert result.additional_overpull_n == pytest.approx(extra_n)
    assert result.overall_top_tension_n == pytest.approx(top_n + extra_n + 10000.0)
    assert result.maximum_setting_n == pytest.approx(1000000.0 * 0.8)
    assert result.slip_ring_tension_n == pytest.approx(464000.0)
    assert result.minimum_top_tension_n == pytest.approx(580000.0)
    assert result.package_overpull_n == pytest.approx(-34000.0)
    assert result.overall_top_tension_n == pytest.approx(654000.0)


def test_efficiency_below_unity_and_fleet_angle_raise_top_tension():
    """Hand check: Tsr = 464,000 N, N = 10, n = 2, Rf = 0.90, ffa = 1.02.

    Tmin = 464,000 * 10 / (0.90 * 8) * 1.02 = 657,333.33 N, against
    580,000 N with both factors at unity (+13.3 %).
    """
    from dataclasses import replace
    from digitalmodel.drilling_riser.stackup_static_weights import (
        TensionInput, minimum_tension,
    )
    base = TensionInput(
        flooded_steel_weight_n=400000.0, steel_weight_factor_1=1.04,
        additional_supported_weight_n=20000.0, buoyancy_uplift_n=80000.0,
        buoyancy_factor_1=0.9, internal_fluid_weight_n=300000.0,
        displaced_internal_water_weight_n=200000.0,
        tensioner_count=10, failed_tensioner_count=2,
        lower_package_submerged_weight_n=150000.0,
        minimum_lower_package_overpull_n=30000.0, recoil_overpull_n=10000.0,
        dynamic_tension_limit_n=1000000.0, setting_fraction=0.8,
        tensioner_efficiency=0.90, fleet_angle_factor=1.0,
    )
    rf_only = minimum_tension(base)
    assert 464000.0 * 10.0 / (0.90 * 8.0) == pytest.approx(644444.44, abs=0.01)
    assert rf_only.slip_ring_tension_n == pytest.approx(464000.0)
    assert rf_only.minimum_top_tension_n == pytest.approx(464000.0 * 10.0 / (0.90 * 8.0))
    both = minimum_tension(replace(base, fleet_angle_factor=1.02))
    assert both.minimum_top_tension_n == pytest.approx(464000.0 * 10.0 / (0.90 * 8.0) * 1.02)
    # Delivered at the ring with all 10 units: Tmin * 0.90 / 1.02 = 464,000 * 10 / 8 = 580,000 N,
    # so the riser overpull is 116,000 N; the 0.90 and 1.02 losses are not overpull.
    assert both.riser_overpull_n == pytest.approx(464000.0 * 10.0 / 8.0 - 464000.0)
    assert both.package_overpull_n == pytest.approx(116000.0 - 150000.0)


def test_tensioner_losses_are_not_credited_as_overpull():
    """Codex case: 90 kN demand, Rf 0.90, ffa 1, no failures, 10 kN required overpull.

    The 100 kN setting delivers 90 kN at the ring, so the riser overpull is zero and the
    full 10 kN required overpull is added.
    """
    from digitalmodel.drilling_riser.stackup_static_weights import (
        TensionInput, minimum_tension,
    )
    result = minimum_tension(TensionInput(
        flooded_steel_weight_n=90000.0, steel_weight_factor_1=1.0,
        additional_supported_weight_n=0.0, buoyancy_uplift_n=0.0,
        buoyancy_factor_1=1.0, internal_fluid_weight_n=0.0,
        displaced_internal_water_weight_n=0.0,
        tensioner_count=4, failed_tensioner_count=0,
        lower_package_submerged_weight_n=0.0,
        minimum_lower_package_overpull_n=10000.0, recoil_overpull_n=0.0,
        dynamic_tension_limit_n=1000000.0, setting_fraction=1.0,
        tensioner_efficiency=0.90, fleet_angle_factor=1.0,
    ))
    assert result.minimum_top_tension_n == pytest.approx(100000.0)
    assert result.riser_overpull_n == pytest.approx(0.0, abs=1e-6)
    assert result.additional_overpull_n == pytest.approx(10000.0)
    # The setting must deliver the extra 10 kN at the ring: 10,000 / 0.90 more.
    assert result.overall_top_tension_n == pytest.approx(100000.0 + 10000.0 / 0.90)
    assert result.overall_top_tension_n * 0.90 - 90000.0 == pytest.approx(10000.0)


def test_governing_applied_tension_selects_larger_requirement():
    from digitalmodel.drilling_riser.stackup_static_weights import governing_tension_n
    assert governing_tension_n(500000.0, 600000.0) == 600000.0
    assert governing_tension_n(700000.0, 600000.0) == 700000.0
    assert governing_tension_n(0.0, 0.0) == 0.0
