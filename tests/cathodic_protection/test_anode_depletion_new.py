# ABOUTME: Skeleton tests for cathodic_protection anode_depletion module
# ABOUTME: Tests depletion calculations, remaining life, and inspection intervals

import pytest

from digitalmodel.cathodic_protection.anode_depletion import (
    ANODE_CAPACITY_ALZNI,
    ANODE_CAPACITY_ZN,
    UTILIZATION_FACTOR_STANDOFF,
    AnodeStatus,
)


class TestAnodeStatusModel:
    def test_basic_creation(self):
        status = AnodeStatus(
            anode_id="AN-001",
            original_mass_kg=20.0,
            current_mass_kg=15.0,
            elapsed_years=5.0,
            mean_current_A=0.05,
        )
        assert status.anode_id == "AN-001"
        assert status.original_mass_kg == 20.0
        assert status.current_mass_kg == 15.0
        assert status.elapsed_years == 5.0

    def test_default_capacity_alzni(self):
        status = AnodeStatus(
            anode_id="AN-002",
            original_mass_kg=15.0,
            current_mass_kg=10.0,
            elapsed_years=3.0,
            mean_current_A=0.04,
        )
        assert status.anode_capacity_Ah_kg == pytest.approx(ANODE_CAPACITY_ALZNI, rel=1e-6)

    def test_default_utilization_factor(self):
        status = AnodeStatus(
            anode_id="AN-003",
            original_mass_kg=10.0,
            current_mass_kg=8.0,
            elapsed_years=2.0,
            mean_current_A=0.03,
        )
        assert status.utilization_factor == pytest.approx(UTILIZATION_FACTOR_STANDOFF, rel=1e-6)

    def test_zero_current_mass_allowed(self):
        # depleted anode
        status = AnodeStatus(
            anode_id="AN-004",
            original_mass_kg=10.0,
            current_mass_kg=0.0,
            elapsed_years=25.0,
            mean_current_A=0.02,
        )
        assert status.current_mass_kg == 0.0

    def test_negative_original_mass_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError):
            AnodeStatus(
                anode_id="AN-005",
                original_mass_kg=-5.0,
                current_mass_kg=0.0,
                elapsed_years=1.0,
                mean_current_A=0.01,
            )


class TestAnodeConstants:
    def test_alzni_capacity(self):
        assert ANODE_CAPACITY_ALZNI == pytest.approx(2000.0, rel=1e-6)

    def test_zinc_capacity_lower_than_alzni(self):
        assert ANODE_CAPACITY_ZN < ANODE_CAPACITY_ALZNI

    def test_standoff_utilization(self):
        assert 0.8 < UTILIZATION_FACTOR_STANDOFF <= 1.0
