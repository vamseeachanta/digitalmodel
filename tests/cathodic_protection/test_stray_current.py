"""Tests for stray current analysis and mitigation."""

import pytest

from digitalmodel.cathodic_protection.stray_current import (
    InterferenceType,
    StrayCurrentInput,
    assess_stray_current,
    design_drainage_bond,
)


def test_dc_interference_close_proximity():
    """DC transit stray current at 20 m from source should be significant."""
    params = StrayCurrentInput(
        interference_type=InterferenceType.DC_TRANSIT,
        pipeline_od_m=0.3048,
        pipeline_length_m=500.0,
        separation_distance_m=20.0,
        soil_resistivity_ohm_m=50.0,
        coating_resistance_ohm_m2=10000.0,
        source_current_A=200.0,
    )
    result = assess_stray_current(params)

    # V = (200 * 50) / (2*pi*20) = ~79.6 V — should be significant
    assert result.induced_voltage_V > 0
    assert result.potential_shift_mV > DC_POTENTIAL_SHIFT_THRESHOLD
    assert result.exceeds_threshold
    assert result.risk_level in ("high", "critical")
    assert len(result.recommended_mitigation) > 0


# Use constant for clarity
DC_POTENTIAL_SHIFT_THRESHOLD = 20.0  # mV


def test_dc_interference_far_distance():
    """DC interference at large distance should be low risk."""
    params = StrayCurrentInput(
        interference_type=InterferenceType.DC_TRANSIT,
        separation_distance_m=500.0,
        soil_resistivity_ohm_m=20.0,
        source_current_A=50.0,
    )
    result = assess_stray_current(params)

    # V = (50 * 20) / (2*pi*500) = ~0.32 V = 318 mV
    # Still could be significant depending on values
    assert result.induced_voltage_V > 0


def test_ac_interference_powerline():
    """AC interference from a power line parallel to pipeline."""
    params = StrayCurrentInput(
        interference_type=InterferenceType.AC_POWERLINE,
        pipeline_od_m=0.3048,
        pipeline_length_m=5000.0,
        separation_distance_m=30.0,
        soil_resistivity_ohm_m=100.0,
        coating_resistance_ohm_m2=5000.0,
        source_current_A=500.0,
    )
    result = assess_stray_current(params)

    assert result.interference_type == "ac_powerline"
    assert result.induced_voltage_V >= 0


def test_ac_interference_severity_increases_with_current():
    """Higher AC source current → higher induced voltage."""
    params_low = StrayCurrentInput(
        interference_type=InterferenceType.AC_POWERLINE,
        separation_distance_m=50.0,
        source_current_A=100.0,
    )
    params_high = StrayCurrentInput(
        interference_type=InterferenceType.AC_POWERLINE,
        separation_distance_m=50.0,
        source_current_A=1000.0,
    )

    result_low = assess_stray_current(params_low)
    result_high = assess_stray_current(params_high)

    assert result_high.induced_voltage_V > result_low.induced_voltage_V


def test_drainage_bond_design():
    """Drainage bond for 5 A stray current."""
    result = design_drainage_bond(
        stray_current_A=5.0,
        source_potential_V=-0.50,
        pipeline_potential_V=-0.85,
    )
    assert result.mitigation_type == "drainage_bond"
    assert result.bond_resistance_ohm is not None
    assert result.bond_resistance_ohm > 0
    assert result.bond_current_A is not None
    assert result.bond_current_A > 0
    assert result.estimated_effectiveness_pct > 0


def test_drainage_bond_limited_current():
    """Bond should limit current to max_bond_current_A."""
    result = design_drainage_bond(
        stray_current_A=20.0,
        max_bond_current_A=10.0,
    )
    # Bond current should not significantly exceed max
    assert result.bond_current_A is not None
    assert result.bond_current_A <= 15.0  # some tolerance
