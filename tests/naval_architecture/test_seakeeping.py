# ABOUTME: Tests for seakeeping module — natural periods, RAO, MSI
# ABOUTME: Validated against PNA Vol III and EN400 Chapter 8
"""Tests for seakeeping module — 6-DOF motion analysis."""

import math

import pytest


class TestNaturalPeriods:
    """Natural period calculations for heave, roll, pitch."""

    def test_roll_period_typical_ship(self):
        from digitalmodel.naval_architecture.seakeeping import (
            natural_roll_period,
        )

        # Typical cargo ship: B=30 m, GM=1.0 m
        t_roll = natural_roll_period(beam_m=30.0, gm_m=1.0)
        # k_xx = 0.40*30 = 12 m, T = 2*pi*12/sqrt(9.81*1.0) ≈ 24.1 s
        assert 20.0 < t_roll < 30.0

    def test_roll_period_increases_with_lower_gm(self):
        from digitalmodel.naval_architecture.seakeeping import (
            natural_roll_period,
        )

        t_high_gm = natural_roll_period(beam_m=20.0, gm_m=2.0)
        t_low_gm = natural_roll_period(beam_m=20.0, gm_m=0.5)
        assert t_low_gm > t_high_gm

    def test_roll_period_negative_gm_raises(self):
        from digitalmodel.naval_architecture.seakeeping import (
            natural_roll_period,
        )

        with pytest.raises(ValueError, match="GM"):
            natural_roll_period(beam_m=20.0, gm_m=-0.5)

    def test_heave_period(self):
        from digitalmodel.naval_architecture.seakeeping import (
            natural_heave_period,
        )

        # Displacement=10000 t, Awp=3000 m²
        t_heave = natural_heave_period(10000.0, 3000.0)
        # m=10e6 kg, stiffness=1025*9.81*3000 ≈ 30.2e6, T ≈ 3.62 s
        assert 3.0 < t_heave < 5.0

    def test_pitch_period(self):
        from digitalmodel.naval_architecture.seakeeping import (
            natural_pitch_period,
        )

        # L=150 m, GML=150 m, disp=10000 t
        t_pitch = natural_pitch_period(150.0, 150.0, 10000.0)
        # Pitch period typically shorter than roll
        assert 3.0 < t_pitch < 10.0


class TestEncounterFrequency:
    """Encounter frequency in head/following/beam seas."""

    def test_head_seas_increases_frequency(self):
        from digitalmodel.naval_architecture.seakeeping import (
            encounter_frequency,
        )

        omega = 0.8  # rad/s
        # Head seas (180°)
        omega_e = encounter_frequency(omega, speed_ms=8.0, heading_deg=180.0)
        assert omega_e > omega  # encounter freq higher in head seas

    def test_following_seas_decreases_frequency(self):
        from digitalmodel.naval_architecture.seakeeping import (
            encounter_frequency,
        )

        omega = 0.8
        omega_e = encounter_frequency(omega, speed_ms=8.0, heading_deg=0.0)
        assert omega_e < omega  # encounter freq lower in following seas

    def test_beam_seas_unchanged(self):
        from digitalmodel.naval_architecture.seakeeping import (
            encounter_frequency,
        )

        omega = 0.8
        omega_e = encounter_frequency(omega, speed_ms=8.0, heading_deg=90.0)
        # cos(90°) ≈ 0, so omega_e ≈ omega
        assert abs(omega_e - omega) < 0.01


class TestRAO:
    """Response Amplitude Operator calculations."""

    def test_rao_at_resonance(self):
        from digitalmodel.naval_architecture.seakeeping import (
            simple_heave_rao,
        )

        # At resonance, RAO peaks
        rao = simple_heave_rao(1.0, 1.0, damping_ratio=0.05)
        assert rao > 5.0  # high amplification

    def test_rao_far_from_resonance(self):
        from digitalmodel.naval_architecture.seakeeping import (
            simple_heave_rao,
        )

        # Far from resonance, RAO ≈ 1.0
        rao = simple_heave_rao(0.1, 1.0, damping_ratio=0.05)
        assert abs(rao - 1.0) < 0.1


class TestMSI:
    """Motion Sickness Incidence calculations."""

    def test_low_accel_low_msi(self):
        from digitalmodel.naval_architecture.seakeeping import (
            motion_sickness_incidence,
        )

        msi = motion_sickness_incidence(0.05, exposure_hours=2.0)
        assert msi < 20.0

    def test_high_accel_high_msi(self):
        from digitalmodel.naval_architecture.seakeeping import (
            motion_sickness_incidence,
        )

        msi = motion_sickness_incidence(0.8, exposure_hours=4.0)
        assert msi > 50.0

    def test_msi_capped_at_100(self):
        from digitalmodel.naval_architecture.seakeeping import (
            motion_sickness_incidence,
        )

        msi = motion_sickness_incidence(5.0, exposure_hours=10.0)
        assert msi == 100.0


class TestSpectralAnalysis:
    """Significant motion from spectral analysis."""

    def test_significant_motion(self):
        from digitalmodel.naval_architecture.seakeeping import (
            significant_motion,
        )

        # Simple test: uniform RAO=1.0, uniform spectrum
        rao = [1.0] * 10
        spectrum = [1.0] * 10
        freq_step = 0.1

        sig = significant_motion(rao, spectrum, freq_step)
        # m0 = sum(1.0 * 1.0 * 0.1) = 1.0, sig = 2*sqrt(1.0) = 2.0
        assert abs(sig - 2.0) < 0.01

    def test_mismatched_arrays_raises(self):
        from digitalmodel.naval_architecture.seakeeping import (
            significant_motion,
        )

        with pytest.raises(ValueError, match="same length"):
            significant_motion([1.0, 2.0], [1.0], 0.1)
