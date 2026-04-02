     1|"""
     2|Tests for S-N Curve Library API — programmatic access to 221 curves.
     3|
     4|TDD: tests written first, then sn_library_api.py implemented.
     5|Tests against known DNV-RP-C203, BS 7608, API RP 2A values.
     6|
     7|Issue: #1676 (P0 — fatigue expansion)
     8|"""
     9|
    10|import math
    11|import pytest
    12|import numpy as np
    13|
    14|from digitalmodel.fatigue.sn_library_api import (
    15|    CurveInfo,
    16|    SNCurve,
    17|    ComparisonData,
    18|    list_curves,
    19|    get_curve,
    20|    calculate_endurance,
    21|    compare_curves,
    22|)
    23|
    24|
    25|# ── Test: list_curves returns all 221+ curves ────────────────────────────────
    26|
    27|class TestListCurves:
    28|    """Listing and filtering of the 221 S-N curve library."""
    29|
    30|    def test_list_all_returns_221_plus_curves(self):
    31|        """The library must expose at least 221 curves."""
    32|        curves = list_curves()
    33|        assert len(curves) >= 221
    34|
    35|    def test_list_curves_returns_curve_info_models(self):
    36|        """Each item must be a CurveInfo Pydantic model."""
    37|        curves = list_curves()
    38|        assert all(isinstance(c, CurveInfo) for c in curves)
    39|
    40|    def test_filter_by_standard_dnv(self):
    41|        """Filtering by DNV-RP-C203 should return 42 curves (14 classes × 3 envs)."""
    42|        curves = list_curves(standard="DNV-RP-C203")
    43|        assert len(curves) == 42
    44|
    45|    def test_filter_by_standard_bs7608(self):
    46|        """BS 7608 should return 20 curves (10 classes × 2 envs)."""
    47|        curves = list_curves(standard="BS 7608")
    48|        assert len(curves) == 20
    49|
    50|    def test_filter_by_standard_api(self):
    51|        """API RP 2A should return 6 curves."""
    52|        curves = list_curves(standard="API RP 2A")
    53|        assert len(curves) == 6
    54|
    55|    def test_filter_by_weld_class_d(self):
    56|        """Class 'D' appears in multiple standards."""
    57|        curves = list_curves(weld_class="D")
    58|        # DNV, BS 7608, AWS, PD 5500, CSA, HSE, DoE — at least 7 standards
    59|        standards = {c.standard for c in curves}
    60|        assert len(standards) >= 5
    61|
    62|    def test_filter_by_weld_class_b(self):
    63|        """Class 'B' curves should exist in several standards."""
    64|        curves = list_curves(weld_class="B")
    65|        assert len(curves) >= 2
    66|
    67|    def test_combined_filter_standard_and_class(self):
    68|        """Filter by both standard and weld class."""
    69|        curves = list_curves(standard="DNV-RP-C203", weld_class="D")
    70|        # DNV D exists in air, seawater_cp, free_corrosion
    71|        assert len(curves) == 3
    72|        for c in curves:
    73|            assert c.standard == "DNV-RP-C203"
    74|            assert c.weld_class == "D"
    75|
    76|    def test_filter_returns_empty_for_nonexistent(self):
    77|        """Filtering for a nonexistent standard returns empty list."""
    78|        curves = list_curves(standard="NONEXISTENT-STANDARD-999")
    79|        assert len(curves) == 0
    80|
    81|    def test_curve_info_has_required_fields(self):
    82|        """CurveInfo must have curve_id, standard, weld_class, environment."""
    83|        curves = list_curves(standard="DNV-RP-C203")
    84|        c = curves[0]
    85|        assert hasattr(c, "curve_id")
    86|        assert hasattr(c, "standard")
    87|        assert hasattr(c, "weld_class")
    88|        assert hasattr(c, "environment")
    89|
    90|
    91|# ── Test: get_curve returns full SNCurve details ─────────────────────────────
    92|
    93|class TestGetCurve:
    94|    """Lookup individual S-N curves by name."""
    95|
    96|    def test_get_dnv_d_air(self):
    97|        """DNV-RP-C203 D air: m1=3.0, log_a1=12.164."""
    98|        curve = get_curve("DNV-RP-C203:D:air")
    99|        assert isinstance(curve, SNCurve)
   100|        assert curve.m1 == 3.0
   101|        assert curve.log_a1 == pytest.approx(12.164, abs=0.01)
   102|
   103|    def test_get_dnv_d_air_has_second_slope(self):
   104|        """DNV D air is bilinear: m2=5, log_a2=15.606."""
   105|        curve = get_curve("DNV-RP-C203:D:air")
   106|        assert curve.m2 == 5.0
   107|        assert curve.log_a2 == pytest.approx(15.606, abs=0.01)
   108|
   109|    def test_get_dnv_d_air_knee_point(self):
   110|        """Knee point at 1e7 cycles."""
   111|        curve = get_curve("DNV-RP-C203:D:air")
   112|        assert curve.knee_point == pytest.approx(1e7, rel=0.01)
   113|
   114|    def test_get_bs7608_f2_air(self):
   115|        """BS 7608 F2 air: m1=3.0, log_a1=11.63."""
   116|        curve = get_curve("BS7608:F2:air")
   117|        assert curve.m1 == 3.0
   118|        assert curve.log_a1 == pytest.approx(11.63, abs=0.01)
   119|
   120|    def test_get_iiw_fat90(self):
   121|        """IIW FAT90 should exist and have m1=3.0."""
   122|        curve = get_curve("IIW:FAT90:air")
   123|        assert curve.m1 == 3.0
   124|
   125|    def test_get_nonexistent_raises(self):
   126|        """Looking up a nonexistent curve raises KeyError."""
   127|        with pytest.raises(KeyError):
   128|            get_curve("FAKE:CURVE:air")
   129|
   130|    def test_sncurve_model_has_all_params(self):
   131|        """SNCurve model has m1, m2, log_a1, log_a2, knee_point."""
   132|        curve = get_curve("DNV-RP-C203:D:air")
   133|        assert hasattr(curve, "m1")
   134|        assert hasattr(curve, "m2")
   135|        assert hasattr(curve, "log_a1")
   136|        assert hasattr(curve, "log_a2")
   137|        assert hasattr(curve, "knee_point")
   138|
   139|
   140|# ── Test: calculate_endurance — fatigue life for given stress ────────────────
   141|
   142|class TestCalculateEndurance:
   143|    """Fatigue life calculations from S-N curve parameters."""
   144|
   145|    def test_dnv_d_air_100mpa(self):
   146|        """DNV D air at 100 MPa: N = 10^12.164 / 100^3 ≈ 1.46e6."""
   147|        curve = get_curve("DNV-RP-C203:D:air")
   148|        N = calculate_endurance(curve, 100.0)
   149|        expected = 10 ** 12.164 / 100 ** 3
   150|        assert N == pytest.approx(expected, rel=0.05)
   151|
   152|    def test_endurance_decreases_with_higher_stress(self):
   153|        """Higher stress → fewer cycles."""
   154|        curve = get_curve("DNV-RP-C203:D:air")
   155|        N_low = calculate_endurance(curve, 50.0)
   156|        N_high = calculate_endurance(curve, 150.0)
   157|        assert N_low > N_high
   158|
   159|    def test_endurance_returns_inf_for_zero_stress(self):
   160|        """Zero stress → infinite life."""
   161|        curve = get_curve("DNV-RP-C203:D:air")
   162|        N = calculate_endurance(curve, 0.0)
   163|        assert N == float("inf")
   164|
   165|    def test_endurance_positive_for_positive_stress(self):
   166|        """Any positive stress gives a positive finite cycle count."""
   167|        curve = get_curve("DNV-RP-C203:D:air")
   168|        N = calculate_endurance(curve, 50.0)
   169|        assert N > 0
   170|        assert math.isfinite(N)
   171|
   172|    def test_endurance_array_input(self):
   173|        """Should handle numpy array of stress ranges."""
   174|        curve = get_curve("DNV-RP-C203:D:air")
   175|        stresses = np.array([50.0, 100.0, 200.0])
   176|        N = calculate_endurance(curve, stresses)
   177|        assert isinstance(N, np.ndarray)
   178|        assert len(N) == 3
   179|        # Monotonically decreasing
   180|        assert N[0] > N[1] > N[2]
   181|
   182|
   183|# ── Test: compare_curves — multi-curve comparison data ──────────────────────
   184|
   185|class TestCompareCurves:
   186|    """Comparison of multiple S-N curves for plotting."""
   187|
   188|    def test_compare_two_curves(self):
   189|        """Compare DNV D air vs BS 7608 D air."""
   190|        names = ["DNV-RP-C203:D:air", "BS7608:D:air"]
   191|        stress_ranges = np.linspace(10, 300, 50)
   192|        result = compare_curves(names, stress_ranges)
   193|        assert isinstance(result, ComparisonData)
   194|        assert len(result.curve_names) == 2
   195|        assert result.stress_ranges is not None
   196|        assert len(result.endurance_cycles) == 2
   197|
   198|    def test_compare_returns_array_data(self):
   199|        """Endurance cycles should be numpy arrays."""
   200|        names = ["DNV-RP-C203:D:air", "DNV-RP-C203:E:air"]
   201|        stress_ranges = np.array([50, 100, 150])
   202|        result = compare_curves(names, stress_ranges)
   203|        for cycles in result.endurance_cycles:
   204|            assert isinstance(cycles, np.ndarray)
   205|            assert len(cycles) == 3
   206|
   207|    def test_compare_with_invalid_curve_raises(self):
   208|        """Invalid curve name in comparison should raise KeyError."""
   209|        with pytest.raises(KeyError):
   210|            compare_curves(
   211|                ["DNV-RP-C203:D:air", "NONEXISTENT:X:air"],
   212|                np.array([100.0]),
   213|            )
   214|