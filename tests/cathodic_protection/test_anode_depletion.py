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
    # Faraday consumption = 1.0 * 8760 / 2000 = 4.38 kg/yr; the utilisation
    # factor bounds the usable mass (900 kg) and does not divide the
    # consumption (#2211; the former 4.867 double-counted u).
    mass_at_1yr = profile.remaining_mass_kg[1]
    consumption = 1000.0 - mass_at_1yr
    assert consumption == pytest.approx(4.38, rel=1e-3)
    assert profile.usable_mass_kg[0] == pytest.approx(900.0)
    assert profile.usable_mass_kg[1] == pytest.approx(900.0 - 4.38, abs=0.01)
    # End of life = 900 / 4.38 = 205.48 yr
    assert profile.end_of_life_year == pytest.approx(205.48, abs=0.01)


def test_end_of_life_matches_profile_usable_mass_zero_crossing():
    """``end_of_life_year`` is where the profile's usable mass reaches zero.

    200 kg, 0.5 A, 2000 Ah/kg, u = 0.90: rate = 0.5 * 8760 / 2000 = 2.19 kg/yr,
    usable 180 kg, EOL = 180 / 2.19 = 82.19 yr. The usable-mass series is
    positive at 82.0 yr and zero at 82.5 yr, so the zero crossing lies in
    [82.0, 82.5] and contains the EOL. The gross remaining mass is still
    200 - 2.19 * 82.19 = 20 kg = (1 - u) M at end of life.
    """
    profile = generate_depletion_profile(
        original_mass_kg=200.0,
        mean_current_A=0.5,
        design_life_years=100.0,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
        time_step_years=0.5,
    )
    assert profile.end_of_life_year == pytest.approx(82.19, abs=0.01)
    last_positive = max(y for y, u in zip(profile.years, profile.usable_mass_kg) if u > 0)
    first_zero = min(y for y, u in zip(profile.years, profile.usable_mass_kg) if u == 0)
    assert last_positive < profile.end_of_life_year <= first_zero
    assert first_zero - last_positive == pytest.approx(0.5)
    idx = profile.years.index(first_zero)
    assert profile.remaining_mass_kg[idx] == pytest.approx(20.0, abs=1.5)

    status = AnodeStatus(
        anode_id="EOL",
        original_mass_kg=200.0,
        current_mass_kg=200.0,  # no inspection data: consumption from current
        elapsed_years=0.0,
        mean_current_A=0.5,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
    )
    result = calculate_remaining_life(status)
    assert result.remaining_life_years == pytest.approx(profile.end_of_life_year, abs=0.01)


def test_is_depleted_when_consumed_reaches_usable_mass():
    """Depleted when consumed >= M u, i.e. at 90 % depletion for u = 0.90."""
    at_limit = AnodeStatus(
        anode_id="L",
        original_mass_kg=100.0,
        current_mass_kg=10.0,
        elapsed_years=1.0,
        mean_current_A=0.01,
        utilization_factor=0.90,
    )
    below = AnodeStatus(
        anode_id="B",
        original_mass_kg=100.0,
        current_mass_kg=11.0,
        elapsed_years=1.0,
        mean_current_A=0.01,
        utilization_factor=0.90,
    )
    assert calculate_remaining_life(at_limit).is_depleted
    assert calculate_remaining_life(at_limit).remaining_life_years == 0.0
    assert not calculate_remaining_life(below).is_depleted


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
