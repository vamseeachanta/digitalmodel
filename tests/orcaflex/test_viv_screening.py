"""Tests for digitalmodel.orcaflex.viv_screening module."""

import math

import pytest

from digitalmodel.orcaflex.viv_screening import (
    BeamProperties,
    BoundaryCondition,
    VIVScreeningInput,
    VIVScreeningResult,
    estimate_response_amplitude,
    strouhal_number,
    viv_screening,
)


class TestStrouhalNumber:
    """Tests for Strouhal number function."""

    def test_subcritical_regime(self):
        """Subcritical Re should give St ≈ 0.2."""
        st = strouhal_number(1e5)
        assert st == pytest.approx(0.20, abs=0.02)

    def test_low_re(self):
        """Low Re should give St ≈ 0.21."""
        st = strouhal_number(100)
        assert st == pytest.approx(0.21)

    def test_supercritical(self):
        """Supercritical Re should give St ≈ 0.25."""
        st = strouhal_number(5e6)
        assert st == pytest.approx(0.25)


class TestVIVScreeningInput:
    """Tests for VIV screening input calculations."""

    def test_reynolds_number(self):
        """Reynolds number should be V*D/nu."""
        viv = VIVScreeningInput(
            outer_diameter=0.3,
            current_speed=1.0,
            kinematic_viscosity=1e-6,
        )
        assert viv.reynolds_number == pytest.approx(300000.0)

    def test_vortex_shedding_frequency(self):
        """Shedding frequency should be St*V/D."""
        viv = VIVScreeningInput(outer_diameter=0.5, current_speed=1.0)
        f_vs = viv.vortex_shedding_frequency
        assert f_vs > 0
        assert f_vs == pytest.approx(viv.st * 1.0 / 0.5)

    def test_stability_parameter(self):
        """Ks = 2 * m* * zeta."""
        viv = VIVScreeningInput(mass_ratio=2.0, damping_ratio=0.02)
        assert viv.stability_parameter == pytest.approx(0.08)

    def test_check_reduced_velocity(self):
        """Reduced velocity check should return correct fields."""
        viv = VIVScreeningInput(current_speed=1.0, outer_diameter=0.3)
        result = viv.check_reduced_velocity(natural_freq=0.5)
        assert "reduced_velocity" in result
        assert "cross_flow_lock_in" in result
        assert "in_line_lock_in" in result


class TestBeamProperties:
    """Tests for beam natural frequency calculation."""

    def test_pinned_pinned_first_mode(self):
        """First mode frequency for pinned-pinned beam."""
        beam = BeamProperties(
            length=100.0,
            outer_diameter=0.3,
            inner_diameter=0.2,
            mass_per_length=100.0,
            effective_tension=0.0,
        )
        f1 = beam.natural_frequency(mode=1)
        assert f1 > 0

    def test_higher_modes_higher_frequency(self):
        """Higher modes should have higher frequencies."""
        beam = BeamProperties(length=100.0, mass_per_length=100.0)
        f1 = beam.natural_frequency(1)
        f2 = beam.natural_frequency(2)
        f3 = beam.natural_frequency(3)
        assert f3 > f2 > f1

    def test_tension_increases_frequency(self):
        """Positive tension should increase natural frequency."""
        beam_no_t = BeamProperties(effective_tension=0.0, length=50.0)
        beam_high_t = BeamProperties(effective_tension=1e6, length=50.0)
        f_no_t = beam_no_t.natural_frequency(1)
        f_high_t = beam_high_t.natural_frequency(1)
        assert f_high_t > f_no_t

    def test_multiple_modes(self):
        """natural_frequencies should return correct count."""
        beam = BeamProperties()
        modes = beam.natural_frequencies(n_modes=5)
        assert len(modes) == 5
        for m in modes:
            assert m["frequency_Hz"] > 0


class TestVIVScreening:
    """Tests for full VIV screening assessment."""

    def test_screening_returns_result(self):
        """VIV screening should return VIVScreeningResult."""
        viv_input = VIVScreeningInput(current_speed=0.8, outer_diameter=0.3)
        beam = BeamProperties(length=100.0, mass_per_length=100.0)
        result = viv_screening(viv_input, beam, n_modes=5)
        assert isinstance(result, VIVScreeningResult)
        assert result.modes_checked == 5

    def test_low_current_passes(self):
        """Very low current should pass screening (no lock-in)."""
        viv_input = VIVScreeningInput(current_speed=0.01, outer_diameter=0.3)
        beam = BeamProperties(length=50.0, mass_per_length=100.0)
        result = viv_screening(viv_input, beam, n_modes=3)
        assert result.screening_pass is True

    def test_details_per_mode(self):
        """Details should contain one entry per mode."""
        viv_input = VIVScreeningInput()
        beam = BeamProperties()
        result = viv_screening(viv_input, beam, n_modes=7)
        assert len(result.details) == 7


class TestResponseAmplitude:
    """Tests for VIV response amplitude estimation."""

    def test_outside_lock_in_zero(self):
        """Outside lock-in range, amplitude should be zero."""
        a_d = estimate_response_amplitude(
            reduced_velocity=2.0,  # below cross-flow range
            outer_diameter=0.3,
            stability_parameter=0.1,
        )
        assert a_d == 0.0

    def test_in_lock_in_positive(self):
        """Inside lock-in range, amplitude should be positive."""
        a_d = estimate_response_amplitude(
            reduced_velocity=6.0,  # center of lock-in
            outer_diameter=0.3,
            stability_parameter=0.1,
        )
        assert a_d > 0

    def test_high_damping_suppresses(self):
        """High stability parameter should suppress amplitude."""
        a_d_low = estimate_response_amplitude(6.0, 0.3, stability_parameter=0.1)
        a_d_high = estimate_response_amplitude(6.0, 0.3, stability_parameter=20.0)
        assert a_d_high < a_d_low
