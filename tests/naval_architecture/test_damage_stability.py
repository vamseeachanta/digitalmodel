# ABOUTME: Tests for damage stability and IMO intact stability criteria
# ABOUTME: Validated against IMO IS Code 2008 (MSC.267(85))
"""Tests for damage_stability module — IMO criteria and flooding calculations."""

import math

import pytest
import yaml
from pathlib import Path

FIXTURES = Path(__file__).parent.parent / "fixtures" / "test_vectors" / "naval_architecture"


@pytest.fixture
def ddg51_gz_curve():
    """DDG-51 GZ curve data (EN400 Chapter 4)."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50, 60, 70],
        "gz_m": [0.0, 0.286, 0.594, 0.945, 1.326, 1.402, 1.219, 0.823],
        "gm_m": 1.10,
    }


@pytest.fixture
def marginal_gz_curve():
    """A vessel that barely passes IMO criteria."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50, 60],
        "gz_m": [0.0, 0.08, 0.16, 0.21, 0.18, 0.10, -0.02],
        "gm_m": 0.16,
    }


@pytest.fixture
def failing_gz_curve():
    """A vessel that fails IMO intact stability criteria."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50],
        "gz_m": [0.0, 0.05, 0.10, 0.12, 0.08, -0.05],
        "gm_m": 0.10,
    }


class TestIMOIntactStability:
    """IMO IS Code 2008 intact stability criteria checks."""

    def test_gz_area_to_30_degrees(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            gz_area_under_curve,
        )

        area = gz_area_under_curve(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            limit_deg=30.0,
        )
        # DDG-51 should easily exceed 0.055 m·rad
        assert area > 0.055, f"Area to 30° = {area:.4f}, need ≥0.055"

    def test_gz_area_to_40_degrees(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            gz_area_under_curve,
        )

        area = gz_area_under_curve(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            limit_deg=40.0,
        )
        assert area > 0.09, f"Area to 40° = {area:.4f}, need ≥0.09"

    def test_gz_area_30_to_40_degrees(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            gz_area_under_curve,
        )

        a30 = gz_area_under_curve(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            limit_deg=30.0,
        )
        a40 = gz_area_under_curve(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            limit_deg=40.0,
        )
        assert (a40 - a30) > 0.03, f"Area 30-40° = {a40-a30:.4f}, need ≥0.03"

    def test_gz_at_30_degrees(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            interpolate_gz,
        )

        gz30 = interpolate_gz(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            30.0,
        )
        assert gz30 >= 0.20, f"GZ at 30° = {gz30:.3f}, need ≥0.20"

    def test_angle_of_max_gz(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            angle_of_max_gz,
        )

        angle = angle_of_max_gz(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
        )
        assert angle >= 25.0, f"Max GZ angle = {angle:.1f}°, need ≥25°"

    def test_check_imo_intact_all_pass(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            check_imo_intact_stability,
        )

        result = check_imo_intact_stability(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            ddg51_gz_curve["gm_m"],
        )
        assert result["overall_pass"] is True
        assert all(c["pass"] for c in result["criteria"])

    def test_check_imo_intact_detects_failures(self, failing_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            check_imo_intact_stability,
        )

        result = check_imo_intact_stability(
            failing_gz_curve["angles_deg"],
            failing_gz_curve["gz_m"],
            failing_gz_curve["gm_m"],
        )
        assert result["overall_pass"] is False
        failed = [c["name"] for c in result["criteria"] if not c["pass"]]
        assert len(failed) >= 1


class TestFloodingCalculations:
    """Damage stability — lost buoyancy method."""

    def test_lost_buoyancy_sinkage(self):
        from digitalmodel.naval_architecture.damage_stability import (
            lost_buoyancy_sinkage,
        )

        # Compartment volume = 500 m³, permeability = 0.85
        # Waterplane area = 2000 m²
        sinkage = lost_buoyancy_sinkage(
            compartment_volume_m3=500.0,
            permeability=0.85,
            waterplane_area_m2=2000.0,
        )
        expected = 500.0 * 0.85 / 2000.0  # = 0.2125 m
        assert abs(sinkage - expected) < 0.001

    def test_permeability_must_be_valid(self):
        from digitalmodel.naval_architecture.damage_stability import (
            lost_buoyancy_sinkage,
        )

        with pytest.raises(ValueError, match="permeability"):
            lost_buoyancy_sinkage(500.0, 1.5, 2000.0)

    def test_flooded_gm(self):
        from digitalmodel.naval_architecture.damage_stability import (
            flooded_gm,
        )

        gm = flooded_gm(
            intact_gm_m=1.5,
            lost_waterplane_inertia_m4=5000.0,
            displacement_tonnes=10000.0,
        )
        # GM_damaged = GM - I_lost / displacement
        expected = 1.5 - 5000.0 / 10000.0  # = 1.0 m
        assert abs(gm - expected) < 0.01


class TestInterpolation:
    """GZ curve interpolation between tabulated points."""

    def test_interpolate_at_tabulated_point(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            interpolate_gz,
        )

        gz = interpolate_gz(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            20.0,
        )
        assert abs(gz - 0.594) < 0.001

    def test_interpolate_between_points(self, ddg51_gz_curve):
        from digitalmodel.naval_architecture.damage_stability import (
            interpolate_gz,
        )

        gz = interpolate_gz(
            ddg51_gz_curve["angles_deg"],
            ddg51_gz_curve["gz_m"],
            15.0,
        )
        # Linear interpolation between 10° (0.286) and 20° (0.594)
        expected = 0.286 + (0.594 - 0.286) * 0.5
        assert abs(gz - expected) < 0.001
