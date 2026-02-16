# ABOUTME: Tests for three-way design code comparison report (WRK-159)
# ABOUTME: Verifies comparison function, tabular output, HTML report for multiple pipe configs

"""Tests for three-way design code comparison — WRK-159.

Verifies that:
- compare_codes() runs same pipe through multiple registered codes
- Results include utilisation, governing check, and code-specific details
- HTML comparison report is generated with side-by-side charts
- Two pipe configurations (shallow pipeline, deepwater riser) both work
"""

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_comparison import (
    CodeComparisonResult,
    compare_codes,
    generate_comparison_report,
)


# ---------------------------------------------------------------------------
# Fixtures — two representative pipe configurations
# ---------------------------------------------------------------------------

def make_shallow_pipeline():
    """10-inch X65 pipeline in 50m water depth."""
    return {
        "label": "10\" Shallow Water Pipeline",
        "geometry": PipeGeometry(outer_diameter=0.2731, wall_thickness=0.0214, corrosion_allowance=0.001),
        "material": PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        "loads": DesignLoads(internal_pressure=20e6, external_pressure=0.5e6),
    }


def make_deepwater_riser():
    """8-inch X65 riser in 1500m water depth."""
    return {
        "label": "8\" Deepwater Riser",
        "geometry": PipeGeometry(outer_diameter=0.2191, wall_thickness=0.0183, corrosion_allowance=0.0005),
        "material": PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        "loads": DesignLoads(internal_pressure=35e6, external_pressure=15e6),
    }


THREE_API_CODES = [DesignCode.API_RP_1111, DesignCode.API_RP_2RD, DesignCode.API_STD_2RD]


# ===================================================================
# compare_codes() function
# ===================================================================

class TestCompareCodes:
    def test_returns_list_of_results(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        assert isinstance(results, list)
        assert len(results) == 3

    def test_each_result_has_code_and_checks(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        for r in results:
            assert isinstance(r, CodeComparisonResult)
            assert r.code in THREE_API_CODES
            assert r.code_label  # non-empty string
            assert len(r.checks) > 0
            assert r.governing_check is not None
            assert 0.0 <= r.max_utilisation

    def test_all_three_codes_present(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        code_set = {r.code for r in results}
        assert code_set == set(THREE_API_CODES)

    def test_different_codes_produce_different_utilisations(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        utils = [r.max_utilisation for r in results]
        # At least two codes should differ (different factor tables)
        assert len(set(round(u, 6) for u in utils)) >= 2

    def test_deepwater_riser_all_codes_produce_results(self):
        pipe = make_deepwater_riser()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        assert len(results) == 3
        for r in results:
            assert r.max_utilisation > 0

    def test_single_code_comparison(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=[DesignCode.API_RP_1111],
        )
        assert len(results) == 1

    def test_includes_check_details(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
        )
        for r in results:
            assert len(r.details) > 0


# ===================================================================
# CodeComparisonResult dataclass
# ===================================================================

class TestCodeComparisonResult:
    def test_is_safe_when_under_unity(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=[DesignCode.API_RP_1111],
        )
        r = results[0]
        if r.max_utilisation <= 1.0:
            assert r.is_safe
        else:
            assert not r.is_safe

    def test_check_names_are_strings(self):
        pipe = make_shallow_pipeline()
        results = compare_codes(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=[DesignCode.API_RP_1111],
        )
        for check_name in results[0].checks:
            assert isinstance(check_name, str)


# ===================================================================
# HTML comparison report
# ===================================================================

class TestComparisonReport:
    def test_generates_html_string(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        assert isinstance(html, str)
        assert "<html" in html.lower()
        assert "</html>" in html.lower()

    def test_report_contains_all_code_names(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        assert "API-RP-1111" in html
        assert "API-RP-2RD" in html
        assert "API-STD-2RD" in html

    def test_report_contains_utilisation_values(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        # Should contain numeric utilisation values
        assert "utilisation" in html.lower() or "utilization" in html.lower()

    def test_report_contains_pipe_data(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        assert "X65" in html
        assert "0.273" in html  # OD

    def test_report_deepwater_riser(self):
        pipe = make_deepwater_riser()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        assert "<html" in html.lower()
        assert "API-RP-1111" in html

    def test_report_with_output_path(self, tmp_path):
        pipe = make_shallow_pipeline()
        out = tmp_path / "comparison.html"
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
            output_path=str(out),
        )
        assert out.exists()
        content = out.read_text()
        assert content == html

    def test_report_contains_governing_check(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        # Should mention "governing" somewhere
        assert "governing" in html.lower()

    def test_report_contains_comparison_table(self):
        pipe = make_shallow_pipeline()
        html = generate_comparison_report(
            pipe["geometry"], pipe["material"], pipe["loads"],
            codes=THREE_API_CODES,
            title=pipe["label"],
        )
        # HTML table with code comparison
        assert "<table" in html.lower()
        assert "<th" in html.lower()
