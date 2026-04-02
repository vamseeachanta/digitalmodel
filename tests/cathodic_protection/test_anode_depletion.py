"""Tests for anode depletion tracking and remaining life assessment."""

import pytest

from digitalmodel.cathodic_protection.anode_depletion import (
    AnodeStatus,
    calculate_remaining_life,
    generate_depletion_profile,
    recommend_inspection_interval,
)


def test_remaining_life_half_depleted():
    """Anode at 50% depletion with known current draw."""
    status = AnodeStatus(
        anode_id="A-001",
        original_mass_kg=200.0,
        current_mass_kg=100.0,  # 50% consumed
        elapsed_years=10.0,
        mean_current_A=0.5,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
    )
    result = calculate_remaining_life(status)

    assert result.anode_id == "A-001"
    assert result.depletion_percentage == pytest.approx(50.0, abs=5.0)
    assert result.remaining_mass_kg == pytest.approx(100.0, abs=5.0)
    assert result.remaining_life_years > 0
    assert not result.is_depleted


def test_remaining_life_nearly_depleted():
    """Anode at ~95% depletion should flag as depleted."""
    status = AnodeStatus(
        anode_id="A-002",
        original_mass_kg=200.0,
        current_mass_kg=10.0,  # 5% remaining
        elapsed_years=20.0,
        mean_current_A=0.5,
    )
    result = calculate_remaining_life(status)

    assert result.depletion_percentage > 90.0
    assert result.is_depleted


def test_depletion_profile_25yr():
    """Generate depletion profile over 25 years."""
    profile = generate_depletion_profile(
        original_mass_kg=200.0,
        mean_current_A=0.5,
        design_life_years=25.0,
        time_step_years=5.0,
    )

    assert len(profile.years) >= 6  # 0, 5, 10, 15, 20, 25
    assert profile.years[0] == 0.0
    assert profile.remaining_mass_kg[0] == 200.0
    assert profile.depletion_percentage[0] == 0.0
    # Mass should decrease over time
    assert profile.remaining_mass_kg[-1] < profile.remaining_mass_kg[0]
    assert profile.end_of_life_year > 0


def test_depletion_profile_consumption_rate():
    """Verify consumption rate matches expected kg/year."""
    profile = generate_depletion_profile(
        original_mass_kg=1000.0,
        mean_current_A=1.0,
        design_life_years=10.0,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
        time_step_years=1.0,
    )
    # Consumption = 1.0 * 8760 / (2000 * 0.9) = 4.867 kg/yr
    mass_at_1yr = profile.remaining_mass_kg[1]
    consumption = 1000.0 - mass_at_1yr
    assert consumption == pytest.approx(4.867, rel=0.02)


def test_inspection_recommendation_routine():
    """Low depletion (<50%) should give routine recommendation."""
    status = AnodeStatus(
        anode_id="A-003",
        original_mass_kg=200.0,
        current_mass_kg=160.0,
        elapsed_years=5.0,
        mean_current_A=0.3,
    )
    depletion = calculate_remaining_life(status)
    rec = recommend_inspection_interval(
        depletion_result=depletion,
        design_life_years=25.0,
        elapsed_years=5.0,
    )
    assert rec.urgency == "routine"
    assert rec.next_inspection_years > 0


def test_inspection_recommendation_critical():
    """>90% depleted should trigger critical urgency."""
    status = AnodeStatus(
        anode_id="A-004",
        original_mass_kg=200.0,
        current_mass_kg=5.0,
        elapsed_years=22.0,
        mean_current_A=0.5,
    )
    depletion = calculate_remaining_life(status)
    rec = recommend_inspection_interval(
        depletion_result=depletion,
        design_life_years=25.0,
        elapsed_years=22.0,
    )
    assert rec.urgency == "critical"
    assert rec.next_inspection_years == 0.0
