"""Tests for installation analysis data models."""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.models import (
    CraneCurve,
    CraneTipConfig,
    InstallationCase,
    InstallationCriteria,
    InstallationPhase,
    GoNoGoState,
    OperabilityResult,
    SplashZoneResult,
    Structure,
    Vessel,
)


class TestStructure:
    """Structure data model."""

    def test_weight_air(self, structure):
        expected = 25000.0 * 9.80665
        assert structure.weight_air_N == pytest.approx(expected, rel=1e-6)

    def test_submerged_weight(self, structure):
        """Submerged weight = weight_air - buoyancy."""
        volume = 6.0 * 4.0 * 3.0  # 72 m3
        buoyancy = 1025.0 * 9.80665 * volume
        expected = structure.weight_air_N - buoyancy
        assert structure.submerged_weight_N() == pytest.approx(expected, rel=1e-6)

    def test_submerged_weight_is_negative_for_light_structure(self):
        """A hollow light structure has negative submerged weight (buoyant)."""
        s = Structure(name="Buoyant", length_m=10.0, width_m=10.0, height_m=10.0, mass_air_kg=100.0)
        assert s.submerged_weight_N() < 0

    def test_projected_area_defaults_to_length_times_width(self):
        s = Structure(name="X", length_m=5.0, width_m=3.0, height_m=2.0, mass_air_kg=1000.0)
        assert s.A_projected_m2 == pytest.approx(15.0)

    def test_projected_area_override(self):
        s = Structure(
            name="X", length_m=5.0, width_m=3.0, height_m=2.0,
            mass_air_kg=1000.0, A_projected_m2=10.0,
        )
        assert s.A_projected_m2 == pytest.approx(10.0)


class TestCraneTipConfig:
    """Crane tip position vector."""

    def test_position_vector(self, crane_tip):
        vec = crane_tip.position_vector
        assert vec == pytest.approx([30.0, 15.0, 25.0])

    def test_origin_crane_tip(self):
        tip = CraneTipConfig(x_m=0.0, y_m=0.0, z_m=0.0)
        assert tip.position_vector == pytest.approx([0.0, 0.0, 0.0])


class TestCraneCurve:
    """Crane load-radius curve."""

    def test_capacity_interpolation(self):
        curve = CraneCurve(
            radii_m=np.array([10.0, 20.0, 30.0]),
            capacities_te=np.array([100.0, 60.0, 30.0]),
            max_hook_load_te=100.0,
        )
        # Midpoint interpolation
        assert curve.capacity_at_radius(15.0) == pytest.approx(80.0)
        # At a known point
        assert curve.capacity_at_radius(20.0) == pytest.approx(60.0)


class TestVessel:
    """Vessel model."""

    def test_has_complete_raos(self, vessel):
        assert vessel.has_complete_raos()

    def test_incomplete_raos(self, frequencies, headings, crane_tip):
        partial = Vessel(
            name="Partial",
            rao_frequencies=frequencies,
            rao_headings=headings,
            rao_data={"heave": {"amplitude": np.zeros((20, 5)), "phase": np.zeros((20, 5))}},
            crane_tip=crane_tip,
        )
        assert not partial.has_complete_raos()


class TestInstallationCriteria:
    """Default criteria values."""

    def test_defaults(self):
        c = InstallationCriteria()
        assert c.max_crane_tip_heave_m == 2.0
        assert c.max_crane_tip_velocity_m_s == 0.5
        assert c.max_hook_load_factor == 1.3
        assert c.max_tilt_deg == 3.0
        assert c.alpha_operational == 1.0


class TestEnums:
    """Enum values."""

    def test_installation_phases(self):
        assert InstallationPhase.SPLASH_ZONE.value == "splash_zone"
        assert InstallationPhase.LANDING.value == "landing"

    def test_go_no_go_states(self):
        assert GoNoGoState.GO.value == "go"
        assert GoNoGoState.MARGINAL.value == "marginal"
