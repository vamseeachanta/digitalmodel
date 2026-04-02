"""
Tests for S-N Curve Library API — programmatic access to 221 curves.

TDD: tests written first, then sn_library_api.py implemented.
Tests against known DNV-RP-C203, BS 7608, API RP 2A values.

Issue: #1676 (P0 — fatigue expansion)
"""

import math
import pytest
import numpy as np

from digitalmodel.fatigue.sn_library_api import (
    CurveInfo,
    SNCurve,
    ComparisonData,
    list_curves,
    get_curve,
    calculate_endurance,
    compare_curves,
)


# ── Test: list_curves returns all 221+ curves ────────────────────────────────

class TestListCurves:
    """Listing and filtering of the 221 S-N curve library."""

    def test_list_all_returns_221_plus_curves(self):
        """The library must expose at least 221 curves."""
        curves = list_curves()
        assert len(curves) >= 221

    def test_list_curves_returns_curve_info_models(self):
        """Each item must be a CurveInfo Pydantic model."""
        curves = list_curves()
        assert all(isinstance(c, CurveInfo) for c in curves)

    def test_filter_by_standard_dnv(self):
        """Filtering by DNV-RP-C203 should return 42 curves (14 classes × 3 envs)."""
        curves = list_curves(standard="DNV-RP-C203")
        assert len(curves) == 42

    def test_filter_by_standard_bs7608(self):
        """BS 7608 should return 20 curves (10 classes × 2 envs)."""
        curves = list_curves(standard="BS 7608")
        assert len(curves) == 20

    def test_filter_by_standard_api(self):
        """API RP 2A should return 6 curves."""
        curves = list_curves(standard="API RP 2A")
        assert len(curves) == 6

    def test_filter_by_weld_class_d(self):
        """Class 'D' appears in multiple standards."""
        curves = list_curves(weld_class="D")
        # DNV, BS 7608, AWS, PD 5500, CSA, HSE, DoE — at least 7 standards
        standards = {c.standard for c in curves}
        assert len(standards) >= 5

    def test_filter_by_weld_class_b(self):
        """Class 'B' curves should exist in several standards."""
        curves = list_curves(weld_class="B")
        assert len(curves) >= 2

    def test_combined_filter_standard_and_class(self):
        """Filter by both standard and weld class."""
        curves = list_curves(standard="DNV-RP-C203", weld_class="D")
        # DNV D exists in air, seawater_cp, free_corrosion
        assert len(curves) == 3
        for c in curves:
            assert c.standard == "DNV-RP-C203"
            assert c.weld_class == "D"

    def test_filter_returns_empty_for_nonexistent(self):
        """Filtering for a nonexistent standard returns empty list."""
        curves = list_curves(standard="NONEXISTENT-STANDARD-999")
        assert len(curves) == 0

    def test_curve_info_has_required_fields(self):
        """CurveInfo must have curve_id, standard, weld_class, environment."""
        curves = list_curves(standard="DNV-RP-C203")
        c = curves[0]
        assert hasattr(c, "curve_id")
        assert hasattr(c, "standard")
        assert hasattr(c, "weld_class")
        assert hasattr(c, "environment")


# ── Test: get_curve returns full SNCurve details ─────────────────────────────

class TestGetCurve:
    """Lookup individual S-N curves by name."""

    def test_get_dnv_d_air(self):
        """DNV-RP-C203 D air: m1=3.0, log_a1=12.164."""
        curve = get_curve("DNV-RP-C203:D:air")
        assert isinstance(curve, SNCurve)
        assert curve.m1 == 3.0
        assert curve.log_a1 == pytest.approx(12.164, abs=0.01)

    def test_get_dnv_d_air_has_second_slope(self):
        """DNV D air is bilinear: m2=5, log_a2=15.606."""
        curve = get_curve("DNV-RP-C203:D:air")
        assert curve.m2 == 5.0
        assert curve.log_a2 == pytest.approx(15.606, abs=0.01)

    def test_get_dnv_d_air_knee_point(self):
        """Knee point at 1e7 cycles."""
        curve = get_curve("DNV-RP-C203:D:air")
        assert curve.knee_point == pytest.approx(1e7, rel=0.01)

    def test_get_bs7608_f2_air(self):
        """BS 7608 F2 air: m1=3.0, log_a1=11.63."""
        curve = get_curve("BS7608:F2:air")
        assert curve.m1 == 3.0
        assert curve.log_a1 == pytest.approx(11.63, abs=0.01)

    def test_get_iiw_fat90(self):
        """IIW FAT90 should exist and have m1=3.0."""
        curve = get_curve("IIW:FAT90:air")
        assert curve.m1 == 3.0

    def test_get_nonexistent_raises(self):
        """Looking up a nonexistent curve raises KeyError."""
        with pytest.raises(KeyError):
            get_curve("FAKE:CURVE:air")

    def test_sncurve_model_has_all_params(self):
        """SNCurve model has m1, m2, log_a1, log_a2, knee_point."""
        curve = get_curve("DNV-RP-C203:D:air")
        assert hasattr(curve, "m1")
        assert hasattr(curve, "m2")
        assert hasattr(curve, "log_a1")
        assert hasattr(curve, "log_a2")
        assert hasattr(curve, "knee_point")


# ── Test: calculate_endurance — fatigue life for given stress ────────────────

class TestCalculateEndurance:
    """Fatigue life calculations from S-N curve parameters."""

    def test_dnv_d_air_100mpa(self):
        """DNV D air at 100 MPa: N = 10^12.164 / 100^3 ≈ 1.46e6."""
        curve = get_curve("DNV-RP-C203:D:air")
        N = calculate_endurance(curve, 100.0)
        expected = 10 ** 12.164 / 100 ** 3
        assert N == pytest.approx(expected, rel=0.05)

    def test_endurance_decreases_with_higher_stress(self):
        """Higher stress → fewer cycles."""
        curve = get_curve("DNV-RP-C203:D:air")
        N_low = calculate_endurance(curve, 50.0)
        N_high = calculate_endurance(curve, 150.0)
        assert N_low > N_high

    def test_endurance_returns_inf_for_zero_stress(self):
        """Zero stress → infinite life."""
        curve = get_curve("DNV-RP-C203:D:air")
        N = calculate_endurance(curve, 0.0)
        assert N == float("inf")

    def test_endurance_positive_for_positive_stress(self):
        """Any positive stress gives a positive finite cycle count."""
        curve = get_curve("DNV-RP-C203:D:air")
        N = calculate_endurance(curve, 50.0)
        assert N > 0
        assert math.isfinite(N)

    def test_endurance_array_input(self):
        """Should handle numpy array of stress ranges."""
        curve = get_curve("DNV-RP-C203:D:air")
        stresses = np.array([50.0, 100.0, 200.0])
        N = calculate_endurance(curve, stresses)
        assert isinstance(N, np.ndarray)
        assert len(N) == 3
        # Monotonically decreasing
        assert N[0] > N[1] > N[2]


# ── Test: compare_curves — multi-curve comparison data ──────────────────────

class TestCompareCurves:
    """Comparison of multiple S-N curves for plotting."""

    def test_compare_two_curves(self):
        """Compare DNV D air vs BS 7608 D air."""
        names = ["DNV-RP-C203:D:air", "BS7608:D:air"]
        stress_ranges = np.linspace(10, 300, 50)
        result = compare_curves(names, stress_ranges)
        assert isinstance(result, ComparisonData)
        assert len(result.curve_names) == 2
        assert result.stress_ranges is not None
        assert len(result.endurance_cycles) == 2

    def test_compare_returns_array_data(self):
        """Endurance cycles should be numpy arrays."""
        names = ["DNV-RP-C203:D:air", "DNV-RP-C203:E:air"]
        stress_ranges = np.array([50, 100, 150])
        result = compare_curves(names, stress_ranges)
        for cycles in result.endurance_cycles:
            assert isinstance(cycles, np.ndarray)
            assert len(cycles) == 3

    def test_compare_with_invalid_curve_raises(self):
        """Invalid curve name in comparison should raise KeyError."""
        with pytest.raises(KeyError):
            compare_curves(
                ["DNV-RP-C203:D:air", "NONEXISTENT:X:air"],
                np.array([100.0]),
            )
