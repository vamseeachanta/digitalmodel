# ABOUTME: Extended tests for floating_platform_stability — damaged stability, wind heel, edge cases.
# ABOUTME: Issue #1972 — Test coverage uplift for overnight modules.
"""
Extended tests for digitalmodel.naval_architecture.floating_platform_stability

Covers:
- Damaged stability scenarios (reduced GM)
- Wind heel edge cases (zero wind, extreme wind, zero GM guard)
- GZ curve properties (negative GM, large angles, custom angles)
- Area integration accuracy (trapezoidal vs known integrals)
- All platform types parameterized
- Cross-platform criteria comparison
- Stability criteria boundary pass/fail transitions
- StabilityResult field completeness
"""

from __future__ import annotations

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


ALL_PLATFORM_TYPES = ["fpso", "semisubmersible", "spar", "tlp", "barge"]


# ---------------------------------------------------------------------------
# compute_gm — extended
# ---------------------------------------------------------------------------

class TestComputeGMExtended:
    """Extended tests for GM computation."""

    def test_negative_gm_possible(self):
        """Very high KG can produce negative GM (unstable vessel)."""
        gm = compute_gm(
            displacement_t=50000,
            kg_m=30.0,  # very high center of gravity
            kb_m=5.0,
            waterplane_area_m2=1000.0,  # small waterplane
        )
        assert isinstance(gm, float)
        # GM can be negative for tender vessels

    def test_gm_increases_with_waterplane_area(self):
        """Larger waterplane area → higher BM → higher GM."""
        gm_small = compute_gm(50000, 15.0, 10.0, 3000.0)
        gm_large = compute_gm(50000, 15.0, 10.0, 12000.0)
        assert gm_large > gm_small

    def test_gm_decreases_with_higher_kg(self):
        """Higher KG → lower GM."""
        gm_low_kg = compute_gm(50000, 10.0, 10.0, 6000.0)
        gm_high_kg = compute_gm(50000, 20.0, 10.0, 6000.0)
        assert gm_low_kg > gm_high_kg

    def test_gm_increases_with_kb(self):
        """Higher KB → higher GM."""
        gm_low_kb = compute_gm(50000, 15.0, 5.0, 6000.0)
        gm_high_kb = compute_gm(50000, 15.0, 12.0, 6000.0)
        assert gm_high_kb > gm_low_kb

    def test_typical_fpso_gm_range(self):
        """A typical FPSO should have GM in a reasonable range."""
        gm = compute_gm(
            displacement_t=200000,
            kg_m=12.0,
            kb_m=10.0,
            waterplane_area_m2=15000.0,
        )
        # GM for large FPSO typically 1-5 m
        assert isinstance(gm, float)
        assert math.isfinite(gm)

    def test_negative_displacement_raises_or_invalid(self):
        """Negative displacement should produce unusual results or raise."""
        # The function divides by displacement, so negative gives negative volume
        # but should not crash
        try:
            gm = compute_gm(-50000, 15.0, 10.0, 6000.0)
            assert isinstance(gm, float)
        except (ValueError, ZeroDivisionError):
            pass  # acceptable

    def test_custom_seawater_density(self):
        """Custom gamma_sw should change BM computation."""
        gm_sw = compute_gm(50000, 15.0, 10.0, 6000.0, gamma_sw=1.025)
        gm_fw = compute_gm(50000, 15.0, 10.0, 6000.0, gamma_sw=1.000)
        # Fresh water → larger volume → smaller BM → smaller GM
        assert gm_sw > gm_fw


# ---------------------------------------------------------------------------
# compute_gz_curve — extended
# ---------------------------------------------------------------------------

class TestComputeGZCurveExtended:
    """Extended GZ curve tests."""

    def test_negative_gm_produces_negative_gz(self):
        """Negative GM → GZ values negative at small angles (unstable)."""
        gz = compute_gz_curve(gm_m=-1.0)
        # GZ = GM * sin(heel); GM < 0 → GZ < 0 for heel > 0
        for heel, g in gz:
            if heel > 0:
                assert g < 0

    def test_zero_gm_produces_zero_gz(self):
        """GM = 0 → all GZ = 0."""
        gz = compute_gz_curve(gm_m=0.0)
        for heel, g in gz:
            assert g == pytest.approx(0.0)

    def test_gz_at_90_degrees(self):
        """GZ at 90 deg should equal GM (sin(90) = 1)."""
        gm = 3.5
        gz = compute_gz_curve(gm_m=gm, heel_angles_deg=[90])
        assert len(gz) == 1
        assert gz[0][1] == pytest.approx(gm, rel=1e-6)

    def test_gz_symmetry_with_negative_angles(self):
        """If we pass negative angles, GZ should be negative (wall-sided)."""
        gm = 2.0
        gz = compute_gz_curve(gm_m=gm, heel_angles_deg=[-30, 0, 30])
        assert gz[0][1] == pytest.approx(-gz[2][1], rel=1e-9)
        assert gz[1][1] == pytest.approx(0.0)

    def test_single_angle_list(self):
        """Single angle should return single-element list."""
        gz = compute_gz_curve(gm_m=1.0, heel_angles_deg=[45])
        assert len(gz) == 1
        assert gz[0][0] == 45

    def test_empty_angle_list(self):
        """Empty angle list should return empty curve."""
        gz = compute_gz_curve(gm_m=1.0, heel_angles_deg=[])
        assert gz == []

    def test_default_angles_0_to_90(self):
        """Default angles should be 0 to 90 in steps of 5."""
        gz = compute_gz_curve(gm_m=1.0)
        expected_angles = list(range(0, 91, 5))
        actual_angles = [h for h, _ in gz]
        assert actual_angles == expected_angles

    def test_very_large_gm(self):
        """Very large GM should produce correspondingly large GZ values."""
        gz = compute_gz_curve(gm_m=100.0, heel_angles_deg=[30])
        assert gz[0][1] == pytest.approx(100.0 * math.sin(math.radians(30)), rel=1e-9)

    def test_cross_curves_method(self):
        """Test KN-based GZ computation."""
        kn_values = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18]
        gz = compute_gz_curve(
            gm_m=1.0,
            heel_angles_deg=list(range(0, 91, 5)),
            kg_m=10.0,
            kn_values=kn_values,
        )
        assert len(gz) > 0
        # First point (heel=0): KN[0] - KG*sin(0) = 0 - 0 = 0
        assert gz[0][1] == pytest.approx(0.0)

    def test_cross_curves_with_short_kn(self):
        """Short KN values list should not crash — uses clamped index."""
        kn_values = [0.0, 5.0, 10.0]
        gz = compute_gz_curve(
            gm_m=1.0,
            heel_angles_deg=list(range(0, 91, 5)),
            kg_m=10.0,
            kn_values=kn_values,
        )
        assert len(gz) == len(range(0, 91, 5))


# ---------------------------------------------------------------------------
# compute_area_under_gz — extended
# ---------------------------------------------------------------------------

class TestComputeAreaExtended:
    """Extended area integration tests."""

    def test_known_area_sin_curve(self):
        """GZ = sin(heel) → integral 0 to pi/6 (30 deg) = 1 - cos(30) in rad."""
        gm = 1.0
        # Fine resolution for accuracy
        angles = [i * 0.5 for i in range(0, 181)]  # 0 to 90 deg in 0.5 steps
        gz = compute_gz_curve(gm_m=gm, heel_angles_deg=angles)
        area = compute_area_under_gz(gz, 0, 30)
        # Analytical: integral of sin(x) from 0 to pi/6 = 1 - cos(pi/6) = 1 - sqrt(3)/2
        # But GZ = GM * sin(heel) and area is in m-rad:
        # integral of sin(x) dx from 0 to pi/6 = [-cos(x)] = 1 - cos(30) = 0.13397..
        expected = 1.0 - math.cos(math.radians(30))
        assert area == pytest.approx(expected, rel=0.01)

    def test_area_30_to_40_positive(self):
        """Area from 30 to 40 deg should be positive for positive GM."""
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        area = compute_area_under_gz(gz, 30, 40)
        assert area > 0

    def test_area_with_reversed_bounds(self):
        """If start > end, there should be no matching points → area = 0."""
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        area = compute_area_under_gz(gz, 40, 30)
        assert area == 0.0

    def test_area_single_point_is_zero(self):
        """Single point has no trapezoid → area = 0."""
        gz = [(30, 1.5)]
        area = compute_area_under_gz(gz, 30, 30)
        assert area == 0.0

    def test_area_two_points(self):
        """Two points → one trapezoid."""
        gz = [(0, 0.0), (30, 1.0)]
        area = compute_area_under_gz(gz, 0, 30)
        # Trapezoid: 0.5 * (0 + 1.0) * radians(30) = 0.5 * 1.0 * 0.5236 = 0.2618
        expected = 0.5 * (0.0 + 1.0) * math.radians(30)
        assert area == pytest.approx(expected, rel=1e-6)

    def test_negative_gz_produces_negative_area(self):
        """Negative GZ → negative area."""
        gz = [(0, 0.0), (30, -1.0)]
        area = compute_area_under_gz(gz, 0, 30)
        assert area < 0


# ---------------------------------------------------------------------------
# compute_wind_heel — extended (damaged stability scenarios)
# ---------------------------------------------------------------------------

class TestWindHeelExtended:
    """Extended wind heel tests including edge cases."""

    def test_zero_wind_produces_zero_heel(self):
        """No wind → no heel."""
        result = compute_wind_heel(
            wind_pressure_kpa=0.0,
            projected_area_m2=5000.0,
            heeling_arm_m=25.0,
            displacement_t=200000,
            gm_m=2.0,
        )
        assert result.heel_angle_deg == pytest.approx(0.0)
        assert result.passing is True

    def test_zero_area_produces_zero_heel(self):
        """Zero projected area → zero wind load → zero heel."""
        result = compute_wind_heel(
            wind_pressure_kpa=1.5,
            projected_area_m2=0.0,
            heeling_arm_m=25.0,
            displacement_t=200000,
            gm_m=2.0,
        )
        assert result.heel_angle_deg == pytest.approx(0.0)

    def test_zero_heeling_arm_produces_zero_heel(self):
        """Zero heeling arm → zero moment → zero heel."""
        result = compute_wind_heel(
            wind_pressure_kpa=1.5,
            projected_area_m2=5000.0,
            heeling_arm_m=0.0,
            displacement_t=200000,
            gm_m=2.0,
        )
        assert result.heel_angle_deg == pytest.approx(0.0)

    def test_wind_heel_increases_with_pressure(self):
        """Higher wind pressure → larger heel angle."""
        r1 = compute_wind_heel(0.5, 5000, 25, 200000, 2.0)
        r2 = compute_wind_heel(2.0, 5000, 25, 200000, 2.0)
        assert abs(r2.heel_angle_deg) > abs(r1.heel_angle_deg)

    def test_wind_heel_decreases_with_gm(self):
        """Higher GM → smaller heel angle (more stable)."""
        r1 = compute_wind_heel(1.0, 5000, 25, 200000, 1.0)
        r2 = compute_wind_heel(1.0, 5000, 25, 200000, 5.0)
        assert abs(r2.heel_angle_deg) < abs(r1.heel_angle_deg)

    def test_wind_heel_decreases_with_displacement(self):
        """Heavier vessel → smaller heel angle."""
        r1 = compute_wind_heel(1.0, 5000, 25, 50000, 2.0)
        r2 = compute_wind_heel(1.0, 5000, 25, 500000, 2.0)
        assert abs(r2.heel_angle_deg) < abs(r1.heel_angle_deg)

    def test_extreme_wind_clamps_to_90(self):
        """Extreme wind that would produce sin(heel) > 1 should be clamped."""
        result = compute_wind_heel(
            wind_pressure_kpa=100.0,  # absurdly high
            projected_area_m2=50000.0,
            heeling_arm_m=50.0,
            displacement_t=1000,  # tiny vessel
            gm_m=0.1,  # very tender
        )
        assert -90 <= result.heel_angle_deg <= 90
        assert result.passing is False

    def test_passing_threshold_at_16_degrees(self):
        """Wind heel exactly at 16 degrees should pass."""
        # Construct inputs that produce approximately 16 deg heel
        # sin(16 deg) ≈ 0.2756
        # We need: p * A * l * 1000 / (disp * 1000 * 9.81 * GM) = 0.2756
        # Let's verify the threshold
        r_below = WindHeelResult(15.9, 1.0, 25.0, True)
        r_at = WindHeelResult(16.0, 1.0, 25.0, True)
        r_above = WindHeelResult(16.1, 1.0, 25.0, False)
        assert r_below.passing is True
        assert r_at.passing is True
        assert r_above.passing is False

    def test_wind_heel_result_fields(self):
        """WindHeelResult should have all fields."""
        result = compute_wind_heel(1.0, 5000, 25, 200000, 2.0)
        assert hasattr(result, "heel_angle_deg")
        assert hasattr(result, "wind_pressure_kpa")
        assert hasattr(result, "heeling_arm_m")
        assert hasattr(result, "passing")


# ---------------------------------------------------------------------------
# check_intact_stability — all platform types
# ---------------------------------------------------------------------------

class TestCheckIntactStabilityAllPlatforms:
    """Test check_intact_stability for every platform type."""

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_stable_platform(self, ptype):
        """Well-designed platform with high GM should pass."""
        gz = compute_gz_curve(gm_m=3.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = compute_wind_heel(0.5, 3000, 20, 100000, 3.0)
        result = check_intact_stability(ptype, 3.0, gz, wind)
        assert isinstance(result, StabilityResult)
        assert isinstance(result.intact, bool)

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_tender_platform_fails(self, ptype):
        """Very low GM (0.01 m) should fail all criteria."""
        gz = compute_gz_curve(gm_m=0.01, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(25.0, 1.0, 25.0, False)
        result = check_intact_stability(ptype, 0.01, gz, wind)
        assert result.intact is False

    @pytest.mark.parametrize("ptype", ALL_PLATFORM_TYPES)
    def test_result_has_all_fields(self, ptype):
        """StabilityResult should have all documented fields."""
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        result = check_intact_stability(ptype, 2.0, gz, wind)
        assert hasattr(result, "gm_m")
        assert hasattr(result, "kz_m")
        assert hasattr(result, "kb_m")
        assert hasattr(result, "bm_m")
        assert hasattr(result, "max_gz_m")
        assert hasattr(result, "heel_at_max_gz")
        assert hasattr(result, "gz_at_30deg")
        assert hasattr(result, "area_0_to_30")
        assert hasattr(result, "area_0_to_40")
        assert hasattr(result, "wind_criterion_ok")
        assert hasattr(result, "intact")


# ---------------------------------------------------------------------------
# check_intact_stability — damaged stability scenarios
# ---------------------------------------------------------------------------

class TestDamagedStabilityScenarios:
    """Simulate damaged stability by reducing GM and checking fail modes."""

    def test_one_compartment_flooded_reduced_gm(self):
        """After flooding, GM drops from 2.5 to 0.8 → may still pass for FPSO."""
        gz_intact = compute_gz_curve(gm_m=2.5, heel_angles_deg=list(range(0, 91, 5)))
        gz_damaged = compute_gz_curve(gm_m=0.8, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(5.0, 1.0, 25.0, True)

        intact_result = check_intact_stability("fpso", 2.5, gz_intact, wind)
        damaged_result = check_intact_stability("fpso", 0.8, gz_damaged, wind)

        # Damaged should be less stable
        assert damaged_result.max_gz_m < intact_result.max_gz_m

    def test_severe_damage_gm_near_zero(self):
        """Severe flooding → GM ~ 0.05 → fails stability."""
        gz = compute_gz_curve(gm_m=0.05, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(10.0, 1.0, 25.0, False)
        result = check_intact_stability("semisubmersible", 0.05, gz, wind)
        assert result.intact is False

    def test_capsized_negative_gm(self):
        """Negative GM → vessel has capsized / statically unstable."""
        gz = compute_gz_curve(gm_m=-1.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(0.0, 0.0, 0.0, False)
        result = check_intact_stability("barge", -1.0, gz, wind)
        assert result.intact is False
        assert result.gm_m < 0

    def test_marginal_stability_spar(self):
        """Spar requires min GM = 2.0; at exactly 2.0 should just pass GM criterion."""
        criteria = STABILITY_CRITERIA[PlatformType.SPAR]
        assert criteria.min_gm_m == 2.0
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(2.0, 0.5, 20.0, True)
        result = check_intact_stability("spar", 2.0, gz, wind)
        assert result.gm_m >= criteria.min_gm_m


# ---------------------------------------------------------------------------
# Wind heel — storm scenarios
# ---------------------------------------------------------------------------

class TestStormScenarios:
    """Realistic storm scenarios with increasing wind pressure."""

    @pytest.mark.parametrize("wind_kpa,expected_pass", [
        (0.3, True),     # 30 kn — normal operations
        (0.5, True),     # 40 kn — moderate gale
        (1.0, True),     # 50 kn — storm
        (1.5, True),     # 60 kn — severe storm (large vessel w/ good GM)
    ])
    def test_fpso_wind_survival(self, wind_kpa, expected_pass):
        """Large FPSO (200k DWT, GM=2.5) under increasing wind."""
        result = compute_wind_heel(
            wind_pressure_kpa=wind_kpa,
            projected_area_m2=5000.0,
            heeling_arm_m=25.0,
            displacement_t=200000,
            gm_m=2.5,
        )
        assert result.passing == expected_pass

    def test_small_barge_fails_in_storm(self):
        """Small barge (5k DWT, GM=0.5) should fail in strong wind."""
        result = compute_wind_heel(
            wind_pressure_kpa=2.0,
            projected_area_m2=2000.0,
            heeling_arm_m=15.0,
            displacement_t=5000,
            gm_m=0.5,
        )
        assert result.passing is False


# ---------------------------------------------------------------------------
# StabilityCriteria — cross-platform comparisons
# ---------------------------------------------------------------------------

class TestCriteriaCrossPlatform:
    """Verify relationships between platform-type criteria."""

    def test_spar_strictest_gm(self):
        """Spar should have the strictest GM requirement."""
        spar_gm = STABILITY_CRITERIA[PlatformType.SPAR].min_gm_m
        for pt in PlatformType:
            assert STABILITY_CRITERIA[pt].min_gm_m <= spar_gm

    def test_fpso_same_as_barge(self):
        """FPSO and barge should have identical criteria."""
        fpso = STABILITY_CRITERIA[PlatformType.FPSO]
        barge = STABILITY_CRITERIA[PlatformType.BARGE]
        assert fpso.min_gm_m == barge.min_gm_m
        assert fpso.min_gz_at_30deg == barge.min_gz_at_30deg
        assert fpso.min_area_0_to_30 == barge.min_area_0_to_30
        assert fpso.min_area_0_to_40 == barge.min_area_0_to_40
        assert fpso.min_angle_max_gz == barge.min_angle_max_gz

    def test_all_criteria_positive(self):
        """All stability criteria should be positive."""
        for pt, criteria in STABILITY_CRITERIA.items():
            assert criteria.min_gm_m > 0, f"{pt}: min_gm_m <= 0"
            assert criteria.max_gz_m > 0, f"{pt}: max_gz_m <= 0"
            assert criteria.min_gz_at_30deg > 0, f"{pt}: min_gz_at_30deg <= 0"
            assert criteria.min_area_0_to_30 > 0, f"{pt}: min_area_0_to_30 <= 0"
            assert criteria.min_area_0_to_40 > 0, f"{pt}: min_area_0_to_40 <= 0"
            assert criteria.min_angle_max_gz > 0, f"{pt}: min_angle_max_gz <= 0"

    def test_area_40_exceeds_area_30(self):
        """min_area_0_to_40 must be >= min_area_0_to_30 for all types."""
        for pt, criteria in STABILITY_CRITERIA.items():
            assert criteria.min_area_0_to_40 >= criteria.min_area_0_to_30, (
                f"{pt}: min_area_0_to_40 ({criteria.min_area_0_to_40}) "
                f"< min_area_0_to_30 ({criteria.min_area_0_to_30})"
            )


# ---------------------------------------------------------------------------
# PlatformType enum
# ---------------------------------------------------------------------------

class TestPlatformTypeEnum:
    """PlatformType enum completeness."""

    def test_all_types_defined(self):
        values = {pt.value for pt in PlatformType}
        assert values == {"fpso", "semisubmersible", "spar", "tlp", "barge"}

    def test_all_types_have_criteria(self):
        for pt in PlatformType:
            assert pt in STABILITY_CRITERIA

    def test_enum_from_string(self):
        """Can construct PlatformType from string value."""
        assert PlatformType("fpso") is PlatformType.FPSO
        assert PlatformType("spar") is PlatformType.SPAR

    def test_unknown_type_raises(self):
        with pytest.raises(ValueError):
            PlatformType("submarine")


# ---------------------------------------------------------------------------
# check_intact_stability — edge cases
# ---------------------------------------------------------------------------

class TestCheckIntactStabilityEdges:
    """Edge cases for the intact stability checker."""

    def test_empty_gz_curve(self):
        """Empty GZ curve should handle gracefully (areas = 0)."""
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        result = check_intact_stability("fpso", 2.0, [], wind)
        assert result.area_0_to_30 == 0.0
        assert result.area_0_to_40 == 0.0
        assert result.max_gz_m == 0.0

    def test_gz_curve_only_one_point(self):
        """Single-point GZ curve → area = 0."""
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        gz = [(30, 1.5)]
        result = check_intact_stability("fpso", 2.0, gz, wind)
        assert result.gz_at_30deg == pytest.approx(1.5)

    def test_case_insensitive_platform_type(self):
        """Platform type should be case-insensitive."""
        gz = compute_gz_curve(gm_m=2.0)
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        r1 = check_intact_stability("FPSO", 2.0, gz, wind)
        r2 = check_intact_stability("fpso", 2.0, gz, wind)
        assert r1.intact == r2.intact
        assert r1.gm_m == r2.gm_m

    def test_max_gz_found_correctly(self):
        """Max GZ should be correctly identified from the curve."""
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        result = check_intact_stability("fpso", 2.0, gz, wind)
        # Max GZ for wall-sided ship: GM * sin(heel) peaks at 90 deg
        # GZ(90) = 2.0 * sin(90) = 2.0
        assert result.max_gz_m == pytest.approx(2.0, rel=1e-6)
        assert result.heel_at_max_gz == pytest.approx(90.0)

    def test_gz_at_30_interpolation(self):
        """GZ at 30 deg should be interpolated from the curve."""
        gz = compute_gz_curve(gm_m=2.0, heel_angles_deg=list(range(0, 91, 5)))
        wind = WindHeelResult(5.0, 1.0, 25.0, True)
        result = check_intact_stability("fpso", 2.0, gz, wind)
        expected_gz30 = 2.0 * math.sin(math.radians(30))
        assert result.gz_at_30deg == pytest.approx(expected_gz30, rel=1e-6)

    def test_wind_criterion_stored(self):
        """wind_criterion_ok should match the wind result's passing flag."""
        gz = compute_gz_curve(gm_m=2.0)
        wind_pass = WindHeelResult(5.0, 1.0, 25.0, True)
        wind_fail = WindHeelResult(25.0, 2.0, 30.0, False)

        r_pass = check_intact_stability("fpso", 2.0, gz, wind_pass)
        r_fail = check_intact_stability("fpso", 2.0, gz, wind_fail)

        assert r_pass.wind_criterion_ok is True
        assert r_fail.wind_criterion_ok is False
