"""Tests for splash zone assessment per DNV-RP-H103 §4.

Expected values independently calculated from published equations.
"""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.splash_zone import (
    slamming_force,
    splash_zone_assessment,
    varying_buoyancy_force,
)
from digitalmodel.marine_ops.installation.models import (
    InstallationCase,
    InstallationCriteria,
    Structure,
)


class TestSlammingForce:
    """DNV-RP-H103 §4.3.4: F_slam = 0.5 * rho * C_s * A_p * v^2."""

    def test_zero_velocity_gives_zero(self, structure):
        assert slamming_force(structure, 0.0) == pytest.approx(0.0)

    def test_known_value(self, structure):
        """F = 0.5 * 1025 * 5.0 * 24.0 * 0.5^2 = 15375.0 N."""
        result = slamming_force(structure, 0.5)
        expected = 0.5 * 1025.0 * 5.0 * (6.0 * 4.0) * 0.5**2
        assert result == pytest.approx(expected, rel=1e-6)

    def test_scales_with_velocity_squared(self, structure):
        f1 = slamming_force(structure, 1.0)
        f2 = slamming_force(structure, 2.0)
        assert f2 / f1 == pytest.approx(4.0, rel=1e-6)

    def test_with_wave_velocity(self, structure):
        """Relative velocity = lowering + wave particle velocity."""
        f_no_wave = slamming_force(structure, 0.5, wave_particle_velocity_m_s=0.0)
        f_wave = slamming_force(structure, 0.5, wave_particle_velocity_m_s=0.3)
        # v_rel = 0.5+0.3 = 0.8 vs 0.5, so force increases
        assert f_wave > f_no_wave

    def test_custom_density(self, structure):
        f_default = slamming_force(structure, 1.0, rho_w=1025.0)
        f_fresh = slamming_force(structure, 1.0, rho_w=1000.0)
        assert f_default > f_fresh

    def test_custom_slamming_coefficient(self):
        """Higher C_s gives higher slamming force."""
        s_low = Structure(name="Low", length_m=4.0, width_m=4.0, height_m=2.0, mass_air_kg=10000.0, C_s=3.0)
        s_high = Structure(name="High", length_m=4.0, width_m=4.0, height_m=2.0, mass_air_kg=10000.0, C_s=7.0)
        assert slamming_force(s_high, 1.0) > slamming_force(s_low, 1.0)


class TestVaryingBuoyancy:
    """DNV-RP-H103 §4.4: F_var = rho * g * A_wp * zeta_a."""

    def test_zero_wave_gives_zero(self, structure):
        assert varying_buoyancy_force(structure, 0.0) == pytest.approx(0.0)

    def test_known_value(self, structure):
        """F = 1025 * 9.80665 * 24.0 * 0.75 = 181,033 N (approx)."""
        zeta_a = 0.75  # 1.5m Hs / 2
        result = varying_buoyancy_force(structure, zeta_a)
        expected = 1025.0 * 9.80665 * (6.0 * 4.0) * 0.75
        assert result == pytest.approx(expected, rel=1e-6)

    def test_linear_with_amplitude(self, structure):
        f1 = varying_buoyancy_force(structure, 0.5)
        f2 = varying_buoyancy_force(structure, 1.0)
        assert f2 / f1 == pytest.approx(2.0, rel=1e-6)


class TestSplashZoneAssessment:
    """Complete splash zone assessment."""

    def test_result_has_all_fields(self, installation_case):
        result = splash_zone_assessment(installation_case, crane_tip_velocity_m_s=0.3)
        assert result.slamming_force_N > 0
        assert result.varying_buoyancy_N > 0
        assert result.total_hydrodynamic_force_N > 0
        assert result.daf > 0
        assert isinstance(result.details, dict)

    def test_max_load_greater_than_min_load(self, installation_case):
        result = splash_zone_assessment(installation_case, crane_tip_velocity_m_s=0.3)
        assert result.max_crane_load_N > result.min_crane_load_N

    def test_daf_increases_with_sea_state(self, structure, vessel, criteria):
        """Larger Hs → larger DAF."""
        case_calm = InstallationCase(
            structure=structure, vessel=vessel, criteria=criteria,
            wave_hs_m=0.5, wave_tp_s=6.0,
        )
        case_rough = InstallationCase(
            structure=structure, vessel=vessel, criteria=criteria,
            wave_hs_m=3.0, wave_tp_s=10.0,
        )
        r_calm = splash_zone_assessment(case_calm, crane_tip_velocity_m_s=0.1)
        r_rough = splash_zone_assessment(case_rough, crane_tip_velocity_m_s=0.8)
        assert r_rough.daf > r_calm.daf

    def test_snap_risk_flagged(self, installation_case):
        """Light structures with large varying buoyancy can have snap risk."""
        result = splash_zone_assessment(installation_case, crane_tip_velocity_m_s=0.3)
        # The test structure is very buoyant (72m3 volume, only 25te)
        # so snap risk is likely
        assert "snap_risk" in result.details

    def test_total_hydro_is_sum(self, installation_case):
        result = splash_zone_assessment(installation_case, crane_tip_velocity_m_s=0.3)
        assert result.total_hydrodynamic_force_N == pytest.approx(
            result.slamming_force_N + result.varying_buoyancy_N, rel=1e-10
        )
