"""Long-wave (low-frequency) closed-form limits of free-floating body RAOs.

The comparators are textbook results; each test states the expected value from
first principles, not from the implementation.
"""
import math

import pytest

from digitalmodel.hydrodynamics.diffraction.long_wave_limits import (
    LongWaveCheck,
    dynamic_amplification,
    expected_rao,
    judge,
    wavenumber,
)

G = 9.80665


class TestWavenumber:
    def test_satisfies_finite_depth_dispersion(self):
        for omega, h in [(0.03, 500.0), (0.05, 500.0), (0.5, 100.0), (1.2, 30.0)]:
            k = wavenumber(omega, h, G)
            assert omega ** 2 == pytest.approx(G * k * math.tanh(k * h), rel=1e-12)

    def test_deep_water_limit(self):
        omega = 1.5
        assert wavenumber(omega, 5000.0, G) == pytest.approx(omega ** 2 / G, rel=1e-12)

    def test_shallow_water_limit(self):
        # kh << 1: omega = k sqrt(g h)
        omega, h = 0.002, 50.0
        assert wavenumber(omega, h, G) == pytest.approx(omega / math.sqrt(G * h), rel=1e-5)

    def test_infinite_depth(self):
        assert wavenumber(0.7, math.inf, G) == pytest.approx(0.49 / G, rel=1e-14)

    @pytest.mark.parametrize("omega,h", [(0.0, 10.0), (-1.0, 10.0), (1.0, 0.0), (math.nan, 10.0)])
    def test_rejects_invalid(self, omega, h):
        with pytest.raises(ValueError):
            wavenumber(omega, h, G)


class TestDynamicAmplification:
    def test_quasi_static(self):
        assert dynamic_amplification(1e-6, 0.2) == pytest.approx(1.0, abs=1e-9)

    def test_value(self):
        # 1 / |1 - (0.1/0.2)^2| = 1 / 0.75
        assert dynamic_amplification(0.1, 0.2) == pytest.approx(4.0 / 3.0)

    def test_above_resonance_is_magnitude(self):
        assert dynamic_amplification(0.4, 0.2) == pytest.approx(1.0 / 3.0)

    def test_rejects_resonance(self):
        with pytest.raises(ValueError):
            dynamic_amplification(0.2, 0.2)


class TestExpectedRao:
    omega, h = 0.05, 500.0

    def k(self):
        return wavenumber(self.omega, self.h, G)

    def test_heave_is_unity_at_every_heading(self):
        for beta in (0.0, 45.0, 90.0, 180.0):
            assert expected_rao("heave", self.omega, beta, self.h, G) == pytest.approx(1.0)

    def test_surge_follows_horizontal_particle_excursion(self):
        # surface horizontal excursion amplitude in finite depth is a * coth(kh)
        kh = self.k() * self.h
        assert expected_rao("surge", self.omega, 0.0, self.h, G) == pytest.approx(1.0 / math.tanh(kh))
        assert expected_rao("surge", self.omega, 135.0, self.h, G) == pytest.approx(
            math.cos(math.radians(45.0)) / math.tanh(kh))

    def test_sway(self):
        kh = self.k() * self.h
        assert expected_rao("sway", self.omega, 90.0, self.h, G) == pytest.approx(1.0 / math.tanh(kh))

    def test_pitch_and_roll_follow_wave_slope_with_amplification(self):
        k = self.k()
        assert expected_rao("pitch", self.omega, 180.0, self.h, G, natural_frequency=0.8) == pytest.approx(
            k / (1.0 - (self.omega / 0.8) ** 2))
        assert expected_rao("roll", self.omega, 90.0, self.h, G, natural_frequency=0.18) == pytest.approx(
            k / (1.0 - (self.omega / 0.18) ** 2))

    def test_rotation_needs_natural_frequency(self):
        with pytest.raises(ValueError):
            expected_rao("roll", self.omega, 90.0, self.h, G)

    def test_yaw_approximation(self):
        k = self.k()
        kh = k * self.h
        assert expected_rao("yaw", self.omega, 45.0, self.h, G) == pytest.approx(0.5 * k / math.tanh(kh))

    def test_unknown_mode(self):
        with pytest.raises(ValueError):
            expected_rao("bogus", self.omega, 0.0, self.h, G)


class TestJudge:
    def test_within_tolerance_is_not_implausible(self):
        c = judge(observed=1.01, expected=1.0, tolerance=0.02)
        assert isinstance(c, LongWaveCheck)
        assert c.verdict == "not_implausible"
        assert c.relative_difference == pytest.approx(0.01)

    def test_outside_tolerance_is_implausible(self):
        assert judge(observed=1.05, expected=1.0, tolerance=0.02).verdict == "implausible"

    def test_boundary_and_sign(self):
        assert judge(observed=0.98, expected=1.0, tolerance=0.02).verdict == "not_implausible"
        assert judge(observed=0.97, expected=1.0, tolerance=0.02).relative_difference == pytest.approx(-0.03)

    def test_expected_zero_rejected(self):
        with pytest.raises(ValueError):
            judge(observed=0.0, expected=0.0, tolerance=0.02)
