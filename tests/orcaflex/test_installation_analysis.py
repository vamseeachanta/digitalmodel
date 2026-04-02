"""Tests for digitalmodel.orcaflex.installation_analysis module."""

import math

import pytest

from digitalmodel.orcaflex.installation_analysis import (
    DAFInput,
    SlingConfig,
    SplashZoneInput,
    VesselRAO,
    WeightItem,
    WeightManagement,
)


class TestVesselRAO:
    """Tests for vessel RAO and crane tip motion."""

    def test_crane_tip_heave_zero_hs(self):
        """Zero Hs should give zero crane tip motion."""
        rao = VesselRAO()
        assert rao.crane_tip_heave(0.0) == pytest.approx(0.0)

    def test_crane_tip_heave_increases_with_hs(self):
        """Crane tip heave should increase with Hs."""
        rao = VesselRAO()
        h1 = rao.crane_tip_heave(1.0)
        h2 = rao.crane_tip_heave(3.0)
        assert h2 > h1

    def test_crane_tip_velocity_positive(self):
        """Crane tip velocity should be positive for non-zero Hs."""
        rao = VesselRAO()
        v = rao.crane_tip_velocity(hs=2.0, tp=10.0)
        assert v > 0

    def test_velocity_increases_with_shorter_period(self):
        """Shorter period should give higher velocity."""
        rao = VesselRAO()
        v_long = rao.crane_tip_velocity(hs=2.0, tp=15.0)
        v_short = rao.crane_tip_velocity(hs=2.0, tp=6.0)
        assert v_short > v_long


class TestDAF:
    """Tests for Dynamic Amplification Factor."""

    def test_daf_at_least_one(self):
        """DAF should be at least 1.0."""
        daf_input = DAFInput(
            static_weight_kN=500.0,
            crane_tip_heave_m=1.0,
            sling_stiffness_kN_per_m=5000.0,
            lifted_mass_kg=50000.0,
        )
        result = daf_input.calculate_daf(wave_period=10.0)
        assert result["daf"] >= 1.0

    def test_daf_increases_with_heave(self):
        """Larger crane tip heave should give higher DAF."""
        base = dict(
            static_weight_kN=500.0,
            sling_stiffness_kN_per_m=5000.0,
            lifted_mass_kg=50000.0,
            crane_tip_velocity_mps=0.5,
        )
        daf1 = DAFInput(crane_tip_heave_m=0.5, **base).calculate_daf()["daf"]
        daf2 = DAFInput(crane_tip_heave_m=2.0, **base).calculate_daf()["daf"]
        assert daf2 > daf1

    def test_natural_period_positive(self):
        """System natural period should be positive."""
        daf_input = DAFInput(
            static_weight_kN=100.0,
            sling_stiffness_kN_per_m=10000.0,
            lifted_mass_kg=10000.0,
        )
        assert daf_input.natural_period > 0


class TestSlingConfig:
    """Tests for sling tension calculation."""

    def test_sling_tension_per_leg(self):
        """Sling tension should account for angle and number of slings."""
        sling = SlingConfig(num_slings=4, sling_angle_deg=60.0, sling_mbl_kN=2000.0)
        result = sling.calculate_sling_tension(hook_load_kN=1000.0)
        assert result["sling_tension_per_leg_kN"] > 0
        # 4 slings at 60 deg: T ≈ 1000 * 1.25 * 1.05 / (4 * sin(60)) ≈ 379 kN
        assert result["sling_tension_per_leg_kN"] < 500

    def test_steeper_angle_lower_tension(self):
        """Steeper sling angle should give lower sling tension."""
        sling_steep = SlingConfig(sling_angle_deg=80.0)
        sling_shallow = SlingConfig(sling_angle_deg=30.0)
        t_steep = sling_steep.calculate_sling_tension(500.0)["sling_tension_per_leg_kN"]
        t_shallow = sling_shallow.calculate_sling_tension(500.0)["sling_tension_per_leg_kN"]
        assert t_steep < t_shallow

    def test_safety_factor_check(self):
        """Should pass with low load, fail with high load."""
        sling = SlingConfig(sling_mbl_kN=2000.0, safety_factor=3.0)
        result_low = sling.calculate_sling_tension(200.0)
        result_high = sling.calculate_sling_tension(5000.0)
        assert result_low["pass"] is True
        assert result_high["pass"] is False


class TestSplashZone:
    """Tests for splash zone analysis."""

    def test_slamming_force_positive(self):
        """Slamming force should be positive."""
        sz = SplashZoneInput(
            dry_weight_kN=500.0,
            submerged_weight_kN=350.0,
            projected_area_m2=25.0,
            crane_tip_heave_m=1.5,
            wave_period_s=8.0,
        )
        result = sz.calculate_splash_zone_loads()
        assert result["slamming_force_kN"] > 0

    def test_max_load_exceeds_dry_weight(self):
        """Maximum dynamic load should exceed static dry weight."""
        sz = SplashZoneInput(crane_tip_heave_m=2.0)
        result = sz.calculate_splash_zone_loads()
        assert result["max_dynamic_hook_load_kN"] > sz.dry_weight_kN

    def test_zero_heave_small_dynamic(self):
        """Near-zero heave should give small dynamic loads."""
        sz = SplashZoneInput(crane_tip_heave_m=0.01)
        result = sz.calculate_splash_zone_loads()
        # Slamming should be very small
        assert result["slamming_force_kN"] < 1.0


class TestWeightManagement:
    """Tests for weight management."""

    def test_total_weight(self):
        """Total weight should sum all items."""
        wm = WeightManagement(items=[
            WeightItem(description="Structure", dry_weight_kN=500.0, submerged_weight_kN=350.0),
            WeightItem(description="Equipment", dry_weight_kN=200.0, submerged_weight_kN=180.0),
        ])
        assert wm.total_dry_weight == pytest.approx(700.0)
        assert wm.total_submerged_weight == pytest.approx(530.0)

    def test_contingency(self):
        """Contingency should add percentage to dry weight."""
        wm = WeightManagement(
            items=[WeightItem(description="Main", dry_weight_kN=1000.0, submerged_weight_kN=700.0)],
            contingency_pct=10.0,
        )
        assert wm.total_with_contingency == pytest.approx(1100.0)

    def test_cog_calculation(self):
        """COG should be weighted average."""
        wm = WeightManagement(items=[
            WeightItem(description="A", dry_weight_kN=100.0, cog_x=1.0),
            WeightItem(description="B", dry_weight_kN=100.0, cog_x=3.0),
        ])
        cog = wm.calculate_cog()
        assert cog["cog_x"] == pytest.approx(2.0)

    def test_summary_table(self):
        """Summary table should include totals."""
        wm = WeightManagement(items=[
            WeightItem(description="Item1", dry_weight_kN=100.0, submerged_weight_kN=80.0),
        ])
        summary = wm.generate_summary()
        assert len(summary) >= 2  # items + totals
        assert summary[-1]["description"].startswith("TOTAL")
