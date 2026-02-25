"""Tests for diffraction_units helper module.

TDD-first: these tests define the expected behavior of all named
unit-conversion functions used across the diffraction pipeline.
"""

import math

import numpy as np
import numpy.testing as npt
import pytest

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    complex_phase_degrees,
    degrees_to_radians,
    density_kg_m3_to_t_m3,
    density_t_m3_to_kg_m3,
    hz_to_rad_per_s,
    inertia_kg_m2_to_t_m2,
    inertia_t_m2_to_kg_m2,
    kg_to_tonnes,
    period_s_to_rad_per_s,
    rad_per_s_to_hz,
    rad_per_s_to_period_s,
    radians_to_degrees,
    tonnes_to_kg,
)


class TestMassConversions:
    def test_kg_to_tonnes_scalar(self):
        assert kg_to_tonnes(1000.0) == pytest.approx(1.0)

    def test_kg_to_tonnes_zero(self):
        assert kg_to_tonnes(0.0) == 0.0

    def test_tonnes_to_kg_scalar(self):
        assert tonnes_to_kg(1.0) == pytest.approx(1000.0)

    def test_kg_to_tonnes_array(self):
        arr = np.array([500.0, 1000.0, 2000.0])
        npt.assert_allclose(kg_to_tonnes(arr), [0.5, 1.0, 2.0])

    def test_tonnes_to_kg_array(self):
        arr = np.array([0.5, 1.0, 2.0])
        npt.assert_allclose(tonnes_to_kg(arr), [500.0, 1000.0, 2000.0])

    def test_roundtrip(self):
        original = 12345.6
        npt.assert_allclose(tonnes_to_kg(kg_to_tonnes(original)), original)

    def test_domain_issc_tlp(self):
        """ISSC TLP mass ~ 54,500 tonnes = 54,500,000 kg."""
        mass_kg = 54_500_000.0
        assert kg_to_tonnes(mass_kg) == pytest.approx(54_500.0)


class TestDensityConversions:
    def test_kg_m3_to_t_m3_seawater(self):
        assert density_kg_m3_to_t_m3(1025.0) == pytest.approx(1.025)

    def test_t_m3_to_kg_m3_seawater(self):
        assert density_t_m3_to_kg_m3(1.025) == pytest.approx(1025.0)

    def test_freshwater(self):
        assert density_kg_m3_to_t_m3(1000.0) == pytest.approx(1.0)

    def test_roundtrip(self):
        original = 1025.0
        npt.assert_allclose(
            density_t_m3_to_kg_m3(density_kg_m3_to_t_m3(original)), original
        )

    def test_array(self):
        arr = np.array([1000.0, 1025.0, 1050.0])
        npt.assert_allclose(density_kg_m3_to_t_m3(arr), [1.0, 1.025, 1.05])


class TestInertiaConversions:
    def test_kg_m2_to_t_m2_scalar(self):
        assert inertia_kg_m2_to_t_m2(5000.0) == pytest.approx(5.0)

    def test_t_m2_to_kg_m2_scalar(self):
        assert inertia_t_m2_to_kg_m2(5.0) == pytest.approx(5000.0)

    def test_roundtrip(self):
        original = 1.23e9
        npt.assert_allclose(
            inertia_t_m2_to_kg_m2(inertia_kg_m2_to_t_m2(original)), original
        )

    def test_array(self):
        arr = np.array([1000.0, 2000.0, 3000.0])
        npt.assert_allclose(inertia_kg_m2_to_t_m2(arr), [1.0, 2.0, 3.0])

    def test_domain_wamit_inertia(self):
        """WAMIT uses t.m^2 (RHO=1), spec uses kg.m^2 — x1000 conversion."""
        wamit_ixx_t_m2 = 1.234e6
        spec_ixx_kg_m2 = inertia_t_m2_to_kg_m2(wamit_ixx_t_m2)
        assert spec_ixx_kg_m2 == pytest.approx(1.234e9)


class TestFrequencyConversions:
    def test_hz_to_rad_per_s(self):
        assert hz_to_rad_per_s(1.0) == pytest.approx(2.0 * math.pi)

    def test_rad_per_s_to_hz(self):
        assert rad_per_s_to_hz(2.0 * math.pi) == pytest.approx(1.0)

    def test_hz_rad_roundtrip(self):
        original = 0.15
        npt.assert_allclose(rad_per_s_to_hz(hz_to_rad_per_s(original)), original)

    def test_rad_per_s_to_period_s(self):
        omega = 2.0 * math.pi  # 1 Hz -> T = 1 s
        assert rad_per_s_to_period_s(omega) == pytest.approx(1.0)

    def test_period_s_to_rad_per_s(self):
        assert period_s_to_rad_per_s(1.0) == pytest.approx(2.0 * math.pi)

    def test_period_roundtrip(self):
        original = 0.6283
        npt.assert_allclose(
            period_s_to_rad_per_s(rad_per_s_to_period_s(original)), original
        )

    def test_hz_to_rad_per_s_array(self):
        freqs_hz = np.array([0.05, 0.1, 0.2])
        expected = freqs_hz * 2.0 * np.pi
        npt.assert_allclose(hz_to_rad_per_s(freqs_hz), expected)

    def test_rad_per_s_to_period_array(self):
        omega = np.array([0.5, 1.0, 2.0])
        expected = 2.0 * np.pi / omega
        npt.assert_allclose(rad_per_s_to_period_s(omega), expected)

    def test_domain_typical_wave_period(self):
        """Typical wave period ~10s -> omega ~0.6283 rad/s."""
        T = 10.0
        omega = period_s_to_rad_per_s(T)
        assert omega == pytest.approx(2.0 * math.pi / 10.0)
        npt.assert_allclose(rad_per_s_to_period_s(omega), T)


class TestAngularConversions:
    def test_radians_to_degrees_scalar(self):
        assert radians_to_degrees(math.pi) == pytest.approx(180.0)

    def test_degrees_to_radians_scalar(self):
        assert degrees_to_radians(180.0) == pytest.approx(math.pi)

    def test_angular_roundtrip(self):
        original = 1.234
        npt.assert_allclose(
            degrees_to_radians(radians_to_degrees(original)), original
        )

    def test_radians_to_degrees_array(self):
        arr = np.array([0.0, math.pi / 2, math.pi])
        npt.assert_allclose(radians_to_degrees(arr), [0.0, 90.0, 180.0])

    def test_complex_phase_degrees_real_positive(self):
        assert complex_phase_degrees(1.0 + 0.0j) == pytest.approx(0.0)

    def test_complex_phase_degrees_imaginary(self):
        assert complex_phase_degrees(0.0 + 1.0j) == pytest.approx(90.0)

    def test_complex_phase_degrees_negative_real(self):
        assert complex_phase_degrees(-1.0 + 0.0j) == pytest.approx(180.0)

    def test_complex_phase_degrees_array(self):
        arr = np.array([1.0 + 0.0j, 0.0 + 1.0j, -1.0 + 0.0j])
        npt.assert_allclose(complex_phase_degrees(arr), [0.0, 90.0, 180.0])

    def test_complex_phase_degrees_matches_np_degrees_np_angle(self):
        """Ensure identical output to the pattern it replaces."""
        z = np.array([1 + 1j, -1 + 2j, 3 - 4j])
        expected = np.degrees(np.angle(z))
        npt.assert_allclose(complex_phase_degrees(z), expected)
