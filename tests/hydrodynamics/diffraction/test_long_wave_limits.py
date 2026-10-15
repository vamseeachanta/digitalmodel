# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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

    def test_rotation_without_amplification_is_pure_wave_slope(self):
        assert expected_rao("roll", self.omega, 90.0, self.h, G) == pytest.approx(self.k())

    def test_yaw_approximation(self):
        k = self.k()
        kh = k * self.h
        assert expected_rao("yaw", self.omega, 45.0, self.h, G) == pytest.approx(0.5 * k / math.tanh(kh))

    def test_unknown_mode(self):
        with pytest.raises(ValueError):
            expected_rao("bogus", self.omega, 0.0, self.h, G)


class TestJudge:
    def test_within_tolerance_is_not_implausible(self):
        c = judge(observed=1.01, expected=1.0, tolerance=0.02,
                  body_length=10, omega=0.05, depth=500)
        assert isinstance(c, LongWaveCheck)
        assert c.verdict == "not_implausible"
        assert c.relative_difference == pytest.approx(0.01)

    def test_outside_tolerance_is_implausible(self):
        assert judge(observed=1.05, expected=1.0, tolerance=0.02,
                     body_length=10, omega=0.05, depth=500).verdict == "implausible"

    def test_boundary_and_sign(self):
        assert judge(observed=0.98, expected=1.0, tolerance=0.02,
                     body_length=10, omega=0.05, depth=500).verdict == "not_implausible"
        assert judge(observed=0.97, expected=1.0, tolerance=0.02,
                     body_length=10, omega=0.05, depth=500).relative_difference == pytest.approx(-0.03)

    def test_expected_zero_needs_absolute_tolerance(self):
        with pytest.raises(ValueError):
            judge(observed=0.0, expected=0.0, tolerance=0.02,
                  body_length=10, omega=0.05, depth=500)

    @pytest.mark.parametrize("observed, verdict", [(0, "not_implausible"),
                                                   (0.01, "not_implausible"),
                                                   (0.02, "implausible")])
    def test_zero_comparator_absolute_path(self, observed, verdict):
        check = judge(observed, 0, 0.02, body_length=10, omega=0.05,
                      depth=500, absolute_tolerance=0.01)
        assert check.verdict == verdict
        assert check.relative_difference is None

    @pytest.mark.parametrize("mode, heading", [
        ("surge", 90), ("sway", 180), ("roll", 180),
        ("pitch", 90), ("yaw", 90), ("yaw", 180),
    ])
    @pytest.mark.parametrize("observed, verdict", [
        (0.001, "not_implausible"), (0.02, "implausible"),
    ])
    def test_cardinal_heading_comparators_use_absolute_path(
        self, mode, heading, observed, verdict
    ):
        expected = expected_rao(mode, 0.05, heading, 500)
        assert abs(expected) < 1e-12
        check = judge(observed, expected, 0.02, body_length=10, omega=0.05,
                      depth=500, absolute_tolerance=0.01)
        assert check.verdict == verdict
        assert check.relative_difference is None
        assert check.absolute_tolerance == 0.01

    @pytest.mark.parametrize("expected", [-0.01, 0.01])
    def test_nonzero_comparator_retains_relative_check(self, expected):
        check = judge(0.001, expected, 0.02, body_length=10, omega=0.05,
                      depth=500, absolute_tolerance=0.01)
        assert check.verdict == "implausible"
        assert check.relative_difference == pytest.approx((0.001 - expected) / expected)

    def test_short_wave_is_not_applicable(self):
        check = judge(1, 1, 0.02, body_length=180, omega=1, depth=500)
        assert check.verdict == "not_applicable"
        assert check.relative_difference is None

    @pytest.mark.parametrize("multiple, verdict", [(9, "not_applicable"),
                                                   (11, "not_implausible")])
    def test_wavelength_must_exceed_ten_body_lengths(self, multiple, verdict):
        length = 180
        omega = math.sqrt(G * 2 * math.pi / (multiple * length))
        assert judge(1, 1, 0.02, body_length=length, omega=omega,
                     depth=math.inf).verdict == verdict


@pytest.mark.parametrize("expected, delta, verdict", [
    (0.0, 0.01, "not_implausible"), (0.0, 0.03, "implausible"),
    (0.01, -0.015, "not_implausible"), (0.01, -0.03, "implausible"),
    (10.0, 0.19, "not_implausible"), (10.0, 0.21, "implausible"),
])
def test_fixed_reference_floor(expected, delta, verdict):
    check = judge(expected + delta, expected, 0.02, body_length=10,
                  omega=0.05, depth=500, reference_amplitude=1.0)
    assert check.verdict == verdict
    assert check.reference_amplitude == 1.0


@pytest.mark.parametrize("mode, heading", [
    ("surge", 90), ("sway", 180), ("roll", 180),
    ("pitch", 90), ("yaw", 90),
])
def test_cardinal_zero_requires_explicit_tolerance_basis(mode, heading):
    expected = expected_rao(mode, 0.05, heading, 500)
    assert expected == 0.0
    with pytest.raises(ValueError, match="zero comparator"):
        judge(0.001, expected, 0.02, body_length=10, omega=0.05, depth=500)


@pytest.mark.parametrize("mode, heading", [("roll", 90), ("pitch", 180), ("yaw", 45)])
def test_rotation_reference_is_fixed_from_comparators(mode, heading):
    reference = max(expected_rao(mode, 0.05, beta, 500)
                    for beta in (0, 45, 90, 135, 180))
    expected = expected_rao(mode, 0.05, heading, 500)
    for observed, verdict in [(expected, "not_implausible"),
                              (10 * expected, "implausible")]:
        check = judge(observed, expected, 0.02, body_length=10, omega=0.05,
                      depth=500, reference_amplitude=reference)
        assert check.verdict == verdict
    assert judge(10 * expected, expected, 0.02, body_length=10, omega=0.05,
                 depth=500, absolute_tolerance=0.01).verdict == "implausible"


@pytest.mark.parametrize("reference", [0, -1, math.inf, math.nan])
def test_invalid_reference_is_rejected(reference):
    with pytest.raises(ValueError, match="reference amplitude"):
        judge(1, 1, 0.02, body_length=10, omega=0.05, depth=500,
              reference_amplitude=reference)


def test_reference_and_absolute_tolerance_are_mutually_exclusive():
    with pytest.raises(ValueError, match="either"):
        judge(0, 0, 0.02, body_length=10, omega=0.05, depth=500,
              reference_amplitude=1, absolute_tolerance=0.01)
