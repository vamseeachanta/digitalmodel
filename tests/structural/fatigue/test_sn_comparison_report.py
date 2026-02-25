# ABOUTME: Tests for S-N curve comparison report generator (WRK-157 Phase 2)
# ABOUTME: Validates HTML generation, curve overlay, parameter tables, DFF bands

"""Tests for S-N curve comparison report — WRK-157."""

import tempfile

import pytest

from digitalmodel.structural.fatigue.sn_comparison_report import (
    generate_sn_comparison_report,
    _curve_points,
)
from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves


# ===================================================================
# Curve point generation
# ===================================================================

class TestCurvePoints:
    def test_curve_points_returns_arrays(self):
        curve = StandardSNCurves.get_curve("DNV", "D")
        N, S = _curve_points(curve)
        assert len(N) == 200
        assert len(S) == 200

    def test_curve_points_stress_decreases_with_cycles(self):
        curve = StandardSNCurves.get_curve("DNV", "D")
        N, S = _curve_points(curve)
        # Overall trend: stress decreases as cycles increase
        assert S[0] > S[-1]

    def test_curve_points_custom_range(self):
        curve = StandardSNCurves.get_curve("DNV", "D")
        N, S = _curve_points(curve, n_min=1e4, n_max=1e8, n_points=50)
        assert len(N) == 50
        assert N[0] >= 1e4
        assert N[-1] <= 1e8


# ===================================================================
# Report generation
# ===================================================================

class TestReportGeneration:
    def test_generates_html_string(self):
        html = generate_sn_comparison_report([("DNV", "D")])
        assert isinstance(html, str)
        assert "<html>" in html

    def test_contains_plotly_cdn(self):
        html = generate_sn_comparison_report([("DNV", "D")])
        assert "plotly" in html.lower()

    def test_contains_curve_name(self):
        html = generate_sn_comparison_report([("DNV", "D"), ("API", "X")])
        assert "DNV" in html
        assert "API" in html

    def test_contains_parameter_table(self):
        html = generate_sn_comparison_report([("DNV", "D")])
        assert "log" in html  # log10(A) column header
        assert "5.73" in html  # Part of DNV D curve A coefficient

    def test_contains_comparison_table(self):
        html = generate_sn_comparison_report([("DNV", "D"), ("BS", "D")])
        assert "Allowable Cycles" in html
        assert "100" in html  # Reference stress 100 MPa

    def test_multi_standard_comparison(self):
        curves = [
            ("DNV", "D"), ("DNV", "F"),
            ("API", "X"), ("BS", "D"),
            ("AWS", "D"),
        ]
        html = generate_sn_comparison_report(curves)
        assert "DNV D" in html
        assert "API X" in html
        assert "BS D" in html
        assert "AWS D" in html
        assert len(html) > 2000

    def test_custom_title(self):
        html = generate_sn_comparison_report(
            [("DNV", "D")],
            title="Pipeline Girth Weld S-N Comparison",
        )
        assert "Pipeline Girth Weld" in html

    def test_no_dff_bands(self):
        html = generate_sn_comparison_report(
            [("DNV", "D")],
            show_dff_bands=False,
        )
        assert "<html>" in html
        # Should NOT have DFF band labels
        assert "DFF = 2" not in html

    def test_with_dff_bands(self):
        html = generate_sn_comparison_report(
            [("DNV", "D")],
            show_dff_bands=True,
        )
        assert "DFF = 2" in html
        assert "DFF = 3" in html


# ===================================================================
# File output
# ===================================================================

class TestFileOutput:
    def test_writes_to_file(self):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name

        generate_sn_comparison_report(
            [("DNV", "D"), ("API", "X")],
            output_path=path,
        )

        with open(path) as f:
            content = f.read()
        assert "<html>" in content
        assert len(content) > 1000

    def test_returns_html_even_when_writing(self):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name

        html = generate_sn_comparison_report(
            [("DNV", "D")],
            output_path=path,
        )
        assert isinstance(html, str)
        assert "<html>" in html


# ===================================================================
# Edge cases
# ===================================================================

class TestEdgeCases:
    def test_invalid_curve_skipped(self):
        html = generate_sn_comparison_report([
            ("DNV", "D"),  # valid
            ("DNV", "NONEXISTENT"),  # invalid — should be skipped
        ])
        assert "DNV D" in html
        assert "NONEXISTENT" not in html

    def test_all_invalid_curves(self):
        html = generate_sn_comparison_report([
            ("FAKE", "Z"),
        ])
        assert "No valid curves" in html

    def test_single_curve(self):
        html = generate_sn_comparison_report([("BS", "D")])
        assert "BS D" in html

    def test_all_dnv_curves(self):
        """Test with all 14 DNV curves."""
        dnv_classes = list(StandardSNCurves.DNV_CURVES.keys())
        curves = [("DNV", c) for c in dnv_classes]
        html = generate_sn_comparison_report(curves, title="All DNV Curves")
        assert "All DNV Curves" in html
        assert len(html) > 5000


# ===================================================================
# Fatigue limit display
# ===================================================================

class TestFatigueLimits:
    def test_fatigue_limit_shown_in_table(self):
        html = generate_sn_comparison_report([("DNV", "D")])
        # DNV D has fatigue limit of 52.63 MPa
        assert "52.6" in html

    def test_zero_fatigue_limit_shown(self):
        html = generate_sn_comparison_report([("API", "S-N1")])
        # API S-N1 has fatigue limit of 0.0
        assert "0.0" in html
