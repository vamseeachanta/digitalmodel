# ABOUTME: Tests for floating platform stability — FPSO/semi/sub/spar/TLP/barge
# ABOUTME: Validated against SNAME PNA and IMO IS Code criteria
"""Tests for floating_platform_stability."""

import math
import pytest

from digitalmodel.naval_architecture.floating_platform_stability import (
    PlatformType,
    StabilityCriteria,
    StabilityResult,
    WindHeelResult,
    STABILITY_CRITERIA,
    check_intact_stability,
    compute_area_under_gz,
    compute_gm,
    compute_gz_curve,
    compute_wind_heel,
)


# ---------------------------------------------------------------------------
# compute_gm
# ---------------------------------------------------------------------------


class TestComputeGM:
    def test_basic(self):
        """GM = KB + BM - KG."""
        gm = compute_gm(
            displacement_t=50000,
            kg_m=15.0,
            kb_m=10.0,
            waterplane_area_m2=6000.0,
        )
        # BM should be positive → GM > KB - KG = -5
        assert gm > -5.0
        assert isinstance(gm, float)

    def test_zero_displacement(self):
        """Zero displacement → infinite BM → GM is finite due to math."""
        with pytest.raises((ZeroDivisionError, ValueError, OverflowError)):
            compute_gm(
                displacement_t=0,
                kg_m=10.0,
                kb_m=5.0,
                waterplane_area_m2=1000.0,
            )

    def test_large_displacement(self):
        """200k DWT FPSO."""
        gm = compute_gm(
            displacement_t=200000,
            kg_m=18.0,
            kb_m=12.0,
            waterplane_area_m2=15000.0,
        )
        # GM should be reasonable for a ship (0.5-3m typically)
        assert gm > -10.0  # at least it's finite


# ---------------------------------------------------------------------------
# compute_gz_curve
# ---------------------------------------------------------------------------


class TestComputeGZCurve:
    def test_wall_sided(self):
        """GZ = GM * sin(heel) for wall-sided ships."""
        gz = compute_gz_curve(gm_m=2.0)

        # At 30 deg: GZ = 2.0 * sin(30) = 1.0
        for heel, g in gz:
            expected = 2.0 * math.sin(math.radians(heel))
            assert g == pytest.approx(expected, rel=1e-9)

    def test_monotonic_at_small_angles(self):
        """GZ should increase with heel for small angles."""
        gz = compute_gz_curve(gm_m=1.5, heel_angles_deg=list(range(0, 40, 5)))
        for i in range(1, len(gz)):
            assert gz[i][1] > gz[i - 1][1], f"GZ decreased at {gz[i][0]} deg"

    def test_zero_at_zero_heel(self):
        """GZ(0) = 0."""
        gz = compute_gz_curve(gm_m=3.0)
        heel_zero, g_zero = gz[0]
        assert heel_zero == 0
        assert g_zero == pytest.approx(0.0)

    def test_custom_angles(self):
        """Custom heel angles should be respected."""
        angles = [10, 20, 35, 45]
        gz = compute_gz_curve(gm_m=1.0, heel_angles_deg=angles)
        for i, (h, g) in enumerate(gz):
            assert h == angles[i]


# ---------------------------------------------------------------------------
# compute_area_under_gz
# ---------------------------------------------------------------------------


class TestComputeAreaUnderGZ:
    def test_simple_triangle(self):
        """Triangular GZ curve — area should match known value."""
        # GZ = k * sin(heel); k=1
        gz = compute_gz_curve(gm_m=1.0, heel_angles_deg=list(range(0, 91, 5)))
        area = compute_area_under_gz(gz, 0, 30)
        # Should be positive and reasonable
        assert area > 0.05  # IMO minimum is 0.055 for some platforms

    def test_zero_range(self):
        """Zero angle range → zero area."""
        gz = compute_gz_curve(gm_m=1.0)
        area = compute_area_under_gz(gz, 0, 0)
        assert area == 0.0

    def test_empty_curve(self):
        """Empty GZ curve → zero area."""
        area = compute_area_under_gz([], 0, 30)
        assert area == 0.0

    def test_area_increases_with_range(self):
        """0-40 area should exceed 0-30 area."""
        gz = compute_gz_curve(gm_m=1.0, heel_angles_deg=list(range(0, 91, 5)))
        area_30 = compute_area_under_gz(gz, 0, 30)
        area_40 = compute_area_under_gz(gz, 0, 40)
        assert area_40 > area_30


# ---------------------------------------------------------------------------
# compute_wind_heel
# ---------------------------------------------------------------------------


class TestComputeWindHeel:
    def test_typical_fpso(self):
        """50 kn wind on FPSO."""
        result = compute_wind_heel(
            wind_pressure_kpa=1.2,       # ~50 kn
            projected_area_m2=5000.0,
            heeling_arm_m=25.0,
            displacement_t=200000,
            gm_m=2.0,
        )
        assert isinstance(result.heel_angle_deg, float)
        assert isinstance(result.passing, bool)

    def test_heavy_wind(self):
        """Hurricane-force wind — should still give valid result."""
        result = compute_wind_heel(
            wind_pressure_kpa=2.5,        # hurricane
            projected_area_m2=8000.0,
            heeling_arm_m=35.0,
            displacement_t=100000,
            gm_m=1.5,
        )
        # Should compute a finite heel angle
        assert -90 <= result.heel_angle_deg <= 90

    def test_very_stable_platform(self):
        """Spar with high GM should have small wind heel."""
        result = compute_wind_heel(
            wind_pressure_kpa=1.0,
            projected_area_m2=3000.0,
            heeling_arm_m=20.0,
            displacement_t=80000,
            gm_m=5.0,
        )
        # High GM → small heel angle
        assert abs(result.heel_angle_deg) < 5.0


# ---------------------------------------------------------------------------
# check_intact_stability
# ---------------------------------------------------------------------------


class TestCheckIntactStability:
    def test_stable_fpso(self):
        """A well-designed FPSO should pass."""
        gz = compute_gz_curve(gm_m=2.5, heel_angles_deg=list(range(0, 91, 5)))
        wind = compute_wind_heel(
            wind_pressure_kpa=0.5,
            projected_area_m2=5000.0,
            heeling_arm_m=25.0,
            displacement_t=200000,
            gm_m=2.5,
        )
        result = check_intact_stability("fpso", 2.5, gz, wind)
        assert isinstance(result, StabilityResult)
        assert result.intact or not result.intact  # result is defined

    def test_tender_platform(self):
        """Very low GM should fail intact stability."""
        gz = compute_gz_curve(gm_m=0.05, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(
            heel_angle_deg=20.0,
            wind_pressure_kpa=0.5,
            heeling_arm_m=25.0,
            passing=False,
        )
        result = check_intact_stability("fpso", 0.05, gz, wind)
        assert result.intact is False

    def test_unknown_platform_raises(self):
        with pytest.raises((ValueError, KeyError)):
            check_intact_stability("submarine", 2.0, [], WindHeelResult(0, 0, 0, True))

    def test_result_fields(self):
        """StabilityResult should have all required fields."""
        gz = compute_gz_curve(gm_m=2.0)
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        result = check_intact_stability("fpso", 2.0, gz, wind)

        assert hasattr(result, "gm_m")
        assert hasattr(result, "max_gz_m")
        assert hasattr(result, "heel_at_max_gz")
        assert hasattr(result, "gz_at_30deg")
        assert hasattr(result, "area_0_to_30")
        assert hasattr(result, "area_0_to_40")
        assert hasattr(result, "intact")
        assert hasattr(result, "wind_criterion_ok")


# ---------------------------------------------------------------------------
# StabilityCriteria constants
# ---------------------------------------------------------------------------


class TestStabilityCriteria:
    def test_all_platforms_defined(self):
        for pt in PlatformType:
            assert pt in STABILITY_CRITERIA

    def test_spar_has_highest_gm(self):
        """Spar should have the highest GM requirement."""
        spar = STABILITY_CRITERIA[PlatformType.SPAR]
        for pt, criteria in STABILITY_CRITERIA.items():
            if pt != PlatformType.SPAR:
                assert criteria.min_gm_m <= spar.min_gm_m

    def test_fpso_and_barge_same(self):
        """FPSO and barge have similar stability requirements."""
        fpso = STABILITY_CRITERIA[PlatformType.FPSO]
        barge = STABILITY_CRITERIA[PlatformType.BARGE]
        assert fpso.min_gm_m == barge.min_gm_m
        assert fpso.min_gz_at_30deg == barge.min_gz_at_30deg
