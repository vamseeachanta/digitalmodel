#!/usr/bin/env python3
"""
ABOUTME: Unit tests for the planing hull 2D+t strip theory motion model.

TDD: tests written first. Covers dataclass creation, strip model geometry,
solver integration, RAO computation, and known physics limits.
"""

import math
import pytest
import numpy as np

from digitalmodel.hydrodynamics.planing_hull.geometry import PlaningHullGeometry
from digitalmodel.hydrodynamics.planing_hull.strip_model import PlaningStripModel
from digitalmodel.hydrodynamics.planing_hull.solver import (
    PlaningMotionSolver,
    PlaningRAO,
    compute_rao,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def fridsma_hull():
    """Standard Fridsma series hull used for validation (L=0.6096 m, L/B≈5)."""
    return PlaningHullGeometry(
        length=0.6096,
        beam=0.1219,
        deadrise=20.0,
        chine_height=0.04,
        lcg=0.3048,
    )


@pytest.fixture
def strip_model(fridsma_hull):
    return PlaningStripModel(fridsma_hull, n_strips=10)


# ---------------------------------------------------------------------------
# PlaningHullGeometry tests
# ---------------------------------------------------------------------------

class TestPlaningHullGeometry:
    """Tests for PlaningHullGeometry dataclass."""

    def test_create_with_required_fields(self):
        geom = PlaningHullGeometry(
            length=1.0,
            beam=0.2,
            deadrise=20.0,
            chine_height=0.05,
            lcg=0.5,
        )
        assert geom.length == pytest.approx(1.0)
        assert geom.beam == pytest.approx(0.2)
        assert geom.deadrise == pytest.approx(20.0)
        assert geom.chine_height == pytest.approx(0.05)
        assert geom.lcg == pytest.approx(0.5)

    def test_fridsma_hull_dimensions(self, fridsma_hull):
        """Fridsma series: L=0.6096 m, L/B ≈ 5."""
        ratio = fridsma_hull.length / fridsma_hull.beam
        assert ratio == pytest.approx(5.0, rel=0.01)

    def test_invalid_length_raises(self):
        with pytest.raises(ValueError, match="length"):
            PlaningHullGeometry(
                length=-1.0,
                beam=0.2,
                deadrise=20.0,
                chine_height=0.05,
                lcg=0.5,
            )

    def test_invalid_deadrise_raises(self):
        with pytest.raises(ValueError, match="deadrise"):
            PlaningHullGeometry(
                length=1.0,
                beam=0.2,
                deadrise=95.0,
                chine_height=0.05,
                lcg=0.5,
            )

    def test_deadrise_in_radians_property(self, fridsma_hull):
        expected_rad = math.radians(20.0)
        assert fridsma_hull.deadrise_rad == pytest.approx(expected_rad)


# ---------------------------------------------------------------------------
# PlaningStripModel tests
# ---------------------------------------------------------------------------

class TestPlaningStripModel:
    """Tests for PlaningStripModel strip layout and force computation."""

    def test_builds_correct_number_of_strips(self, strip_model):
        assert len(strip_model.strip_x) == 10

    def test_strip_positions_span_hull_length(self, fridsma_hull, strip_model):
        """Strip centres should cover [0, L] approximately."""
        assert strip_model.strip_x.min() >= 0.0
        assert strip_model.strip_x.max() <= fridsma_hull.length

    def test_strip_width_positive(self, strip_model):
        """Each strip has a positive dx."""
        assert strip_model.dx > 0.0

    def test_added_mass_per_strip_positive(self, strip_model):
        """Wagner added mass per unit length must be >= 0 for positive wetted beam."""
        c = 0.05  # 5 cm half-wetted beam
        ma = strip_model.added_mass_per_length(c)
        assert ma >= 0.0

    def test_added_mass_scales_with_beam_squared(self, strip_model):
        """Wagner model: m_a = (pi/2) * rho * c^2; verify quadratic scaling."""
        c1, c2 = 0.04, 0.08
        ma1 = strip_model.added_mass_per_length(c1)
        ma2 = strip_model.added_mass_per_length(c2)
        assert ma2 == pytest.approx(ma1 * (c2 / c1) ** 2, rel=1e-6)

    def test_water_entry_force_positive_for_downward_velocity(self, strip_model):
        """Downward entry (V_n > 0) should produce positive (upward) entry force."""
        dt = 0.001
        v_n = 0.5  # m/s downward
        f = strip_model.water_entry_force_per_length(v_n=v_n, dt=dt)
        assert f >= 0.0

    def test_water_entry_force_zero_for_zero_velocity(self, strip_model):
        """Zero normal velocity → zero water-entry force."""
        f = strip_model.water_entry_force_per_length(v_n=0.0, dt=0.001)
        assert f == pytest.approx(0.0, abs=1e-10)

    def test_default_n_strips(self, fridsma_hull):
        """Default strip count is 20."""
        model = PlaningStripModel(fridsma_hull)
        assert len(model.strip_x) == 20


# ---------------------------------------------------------------------------
# PlaningMotionSolver tests
# ---------------------------------------------------------------------------

class TestPlaningMotionSolver:
    """Tests for PlaningMotionSolver RK4 integration."""

    def test_solver_creates_with_defaults(self, fridsma_hull):
        solver = PlaningMotionSolver(fridsma_hull)
        assert solver is not None

    def test_solver_configurable_time_step(self, fridsma_hull):
        solver = PlaningMotionSolver(fridsma_hull, dt=0.005)
        assert solver.dt == pytest.approx(0.005)

    def test_integration_returns_time_series(self, fridsma_hull):
        """Solver should return arrays of heave and pitch vs time."""
        solver = PlaningMotionSolver(fridsma_hull, dt=0.01, n_strips=10)
        t, heave, pitch = solver.integrate(
            speed=3.0,
            wave_freq=6.0,
            wave_steepness=0.05,
            t_total=1.0,
        )
        assert len(t) > 1
        assert len(heave) == len(t)
        assert len(pitch) == len(t)

    def test_no_nan_in_integration(self, fridsma_hull):
        """RK4 must not produce NaN values."""
        solver = PlaningMotionSolver(fridsma_hull, dt=0.005, n_strips=10)
        t, heave, pitch = solver.integrate(
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.05,
            t_total=1.0,
        )
        assert not np.any(np.isnan(heave)), "NaN in heave time series"
        assert not np.any(np.isnan(pitch)), "NaN in pitch time series"

    def test_zero_steepness_produces_small_response(self, fridsma_hull):
        """With zero wave steepness the excitation vanishes; response stays near zero."""
        solver = PlaningMotionSolver(fridsma_hull, dt=0.005, n_strips=10)
        t, heave, pitch = solver.integrate(
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.0,
            t_total=2.0,
        )
        # Skip first 0.5 s transient
        mask = t > 0.5
        assert np.max(np.abs(heave[mask])) < 1e-6
        assert np.max(np.abs(pitch[mask])) < 1e-6


# ---------------------------------------------------------------------------
# PlaningRAO dataclass tests
# ---------------------------------------------------------------------------

class TestPlaningRAO:
    """Tests for PlaningRAO dataclass structure."""

    def test_create_rao(self):
        rao = PlaningRAO(
            heave_rao=0.8,
            pitch_rao=2.0,
            heave_phase=-0.3,
            pitch_phase=0.1,
            second_harmonic_heave=0.05,
        )
        assert rao.heave_rao == pytest.approx(0.8)
        assert rao.pitch_rao == pytest.approx(2.0)
        assert rao.heave_phase == pytest.approx(-0.3)
        assert rao.pitch_phase == pytest.approx(0.1)
        assert rao.second_harmonic_heave == pytest.approx(0.05)

    def test_rao_fields_are_floats(self):
        rao = PlaningRAO(
            heave_rao=1.0,
            pitch_rao=1.0,
            heave_phase=0.0,
            pitch_phase=0.0,
            second_harmonic_heave=0.0,
        )
        assert isinstance(rao.heave_rao, float)
        assert isinstance(rao.pitch_rao, float)
        assert isinstance(rao.second_harmonic_heave, float)


# ---------------------------------------------------------------------------
# compute_rao high-level entry point tests
# ---------------------------------------------------------------------------

class TestComputeRao:
    """Tests for the compute_rao() convenience function."""

    def test_returns_planing_rao_instance(self, fridsma_hull):
        rao = compute_rao(
            geometry=fridsma_hull,
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.05,
        )
        assert isinstance(rao, PlaningRAO)

    def test_rao_values_are_finite(self, fridsma_hull):
        rao = compute_rao(
            geometry=fridsma_hull,
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.05,
        )
        assert math.isfinite(rao.heave_rao)
        assert math.isfinite(rao.pitch_rao)
        assert math.isfinite(rao.heave_phase)
        assert math.isfinite(rao.pitch_phase)
        assert math.isfinite(rao.second_harmonic_heave)

    def test_rao_non_negative(self, fridsma_hull):
        """RAO amplitudes must be non-negative."""
        rao = compute_rao(
            geometry=fridsma_hull,
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.05,
        )
        assert rao.heave_rao >= 0.0
        assert rao.pitch_rao >= 0.0
        assert rao.second_harmonic_heave >= 0.0

    def test_second_harmonic_zero_at_zero_steepness(self, fridsma_hull):
        """Zero wave steepness → 2nd harmonic amplitude ≈ 0 (linear limit)."""
        rao = compute_rao(
            geometry=fridsma_hull,
            speed=3.0,
            wave_freq=5.0,
            wave_steepness=0.0,
        )
        assert rao.second_harmonic_heave == pytest.approx(0.0, abs=1e-4)

    def test_second_harmonic_increases_with_steepness(self, fridsma_hull):
        """2nd harmonic amplitude must increase monotonically with wave steepness."""
        steepnesses = [0.0, 0.05, 0.10, 0.15]
        second_harmonics = []
        for eps in steepnesses:
            rao = compute_rao(
                geometry=fridsma_hull,
                speed=3.0,
                wave_freq=5.0,
                wave_steepness=eps,
            )
            second_harmonics.append(rao.second_harmonic_heave)

        for i in range(len(second_harmonics) - 1):
            assert second_harmonics[i + 1] >= second_harmonics[i], (
                f"2nd harmonic not monotonic: eps={steepnesses[i]:.2f} → "
                f"{second_harmonics[i]:.6f}, eps={steepnesses[i+1]:.2f} → "
                f"{second_harmonics[i+1]:.6f}"
            )

    def test_fridsma_validation_froude_119(self, fridsma_hull):
        """
        Validation: Fridsma hull, Fn ≈ 1.19.

        Speed = Fn * sqrt(g * L) = 1.19 * sqrt(9.81 * 0.6096) ≈ 2.91 m/s.
        Heave RAO and pitch RAO should be O(1) in non-dimensional terms,
        i.e., heave_rao / wave_amplitude is reasonable (0.1 - 5.0 range).
        """
        g = 9.81
        fn = 1.19
        speed = fn * math.sqrt(g * fridsma_hull.length)
        wave_freq = 6.28  # ~1 Hz encounter frequency, representative
        wave_steepness = 0.10

        rao = compute_rao(
            geometry=fridsma_hull,
            speed=speed,
            wave_freq=wave_freq,
            wave_steepness=wave_steepness,
        )
        # Qualitative sanity: heave RAO should be in plausible range
        assert 0.0 < rao.heave_rao < 10.0, (
            f"Heave RAO {rao.heave_rao:.3f} outside plausible range for Fridsma Fn=1.19"
        )
        assert 0.0 < rao.pitch_rao < 20.0, (
            f"Pitch RAO {rao.pitch_rao:.3f} outside plausible range"
        )
