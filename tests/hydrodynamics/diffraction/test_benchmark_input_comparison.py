"""Tests for benchmark_input_comparison module.

ABOUTME: Tests for build_input_comparison_html and
build_semantic_equivalence_html functions.
"""
from __future__ import annotations

from typing import Any, Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_input_comparison import (
    build_input_comparison_html,
    build_semantic_equivalence_html,
)

from tests.hydrodynamics.diffraction.conftest import _make_solver_results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_solver_metadata(
    solver_names: list[str],
    with_semantic: bool = False,
) -> Dict[str, Dict[str, Any]]:
    """Build solver_metadata dict for testing."""
    meta: Dict[str, Dict[str, Any]] = {}
    for solver in solver_names:
        entry: Dict[str, Any] = {
            "water_depth": 100.0,
            "mass": "5000000.0",
            "mesh_file": "hull.gdf",
            "mesh_format": "GDF",
            "panel_count": "2400",
        }
        if with_semantic and solver == solver_names[0]:
            entry["_semantic_equivalence"] = {
                "match_count": 42,
                "cosmetic_count": 3,
                "convention_count": 2,
                "significant_count": 0,
                "diffs": [],
            }
        meta[solver] = entry
    return meta


def _make_semantic_metadata_with_diffs(
    solver_names: list[str],
) -> Dict[str, Dict[str, Any]]:
    """Metadata with semantic equivalence diffs for detailed testing."""
    meta = _make_solver_metadata(solver_names)
    meta[solver_names[0]]["_semantic_equivalence"] = {
        "match_count": 35,
        "cosmetic_count": 5,
        "convention_count": 3,
        "significant_count": 2,
        "diffs": [
            {
                "key": "WavesReferredToBy",
                "level": "convention",
                "owd": "frequency (rad/s)",
                "spec": "period (s)",
            },
            {
                "key": "PeriodOrFrequency",
                "level": "convention",
                "owd": "rad/s",
                "spec": "period",
            },
            {
                "key": "BodyMeshPen",
                "level": "cosmetic",
                "owd": "#FF0000",
                "spec": "#0000FF",
            },
            {
                "key": "BodyName",
                "level": "cosmetic",
                "owd": "Hull_v1",
                "spec": "Hull_auto",
            },
            {
                "key": "DivideNonPlanarPanels",
                "level": "significant",
                "owd": "Yes",
                "spec": "No",
            },
            {
                "key": "FreeSurfaceInnerRadius",
                "level": "significant",
                "owd": "50.0",
                "spec": "40.0",
            },
            {
                "key": "WaterlinePen",
                "level": "cosmetic",
                "owd": "#aaa",
                "spec": "#bbb",
            },
            {
                "key": "QTFFrequencyTypes",
                "level": "convention",
                "owd": "sum",
                "spec": "difference",
            },
        ],
    }
    return meta


# ---------------------------------------------------------------------------
# Tests: build_input_comparison_html
# ---------------------------------------------------------------------------


class TestBuildInputComparisonHtml:
    """Tests for build_input_comparison_html."""

    def test_returns_html_string(self):
        names = ["SolverA", "SolverB"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert isinstance(html, str)
        assert "Input Comparison" in html

    def test_contains_solver_names_in_header(self):
        names = ["AQWA", "OrcaWave"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "AQWA" in html
        assert "OrcaWave" in html

    def test_contains_table_structure(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "<table" in html
        assert "<thead>" in html
        assert "<tbody>" in html

    def test_contains_environment_section(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "Environment" in html

    def test_water_depth_rendered(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "100.0" in html

    def test_frequency_range_rendered(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        # Should contain frequency range with count
        assert "(10)" in html  # 10 frequencies from conftest

    def test_heading_range_rendered(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "(5)" in html  # 5 headings from conftest

    def test_empty_metadata_still_renders(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta: Dict[str, Dict[str, Any]] = {"SolverA": {}}
        html = build_input_comparison_html(names, results, meta)
        assert "Input Comparison" in html

    def test_three_solver_columns(self):
        names = ["AQWA", "OrcaWave", "BEMRosetta"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        for name in names:
            assert name in html

    def test_mesh_section_with_metadata(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        assert "Mesh" in html
        assert "hull.gdf" in html

    def test_phase_convention_rendered(self):
        names = ["SolverA"]
        results = {n: _make_solver_results(n) for n in names}
        meta = _make_solver_metadata(names)
        html = build_input_comparison_html(names, results, meta)
        # phase_convention = "unknown" by default from DiffractionResults
        assert "unknown" in html or "Phase" in html


# ---------------------------------------------------------------------------
# Tests: build_semantic_equivalence_html
# ---------------------------------------------------------------------------


class TestBuildSemanticEquivalenceHtml:
    """Tests for build_semantic_equivalence_html."""

    def test_no_semantic_data_returns_empty(self):
        names = ["SolverA", "SolverB"]
        meta = _make_solver_metadata(names, with_semantic=False)
        html = build_semantic_equivalence_html(names, meta)
        assert html == ""

    def test_returns_html_when_semantic_present(self):
        names = ["SolverA", "SolverB"]
        meta = _make_solver_metadata(names, with_semantic=True)
        html = build_semantic_equivalence_html(names, meta)
        assert isinstance(html, str)
        assert len(html) > 0
        assert "Semantic Equivalence" in html

    def test_equivalent_badge_for_zero_significant(self):
        names = ["SolverA"]
        meta = _make_solver_metadata(names, with_semantic=True)
        html = build_semantic_equivalence_html(names, meta)
        assert "EQUIVALENT" in html
        assert "#27ae60" in html  # green badge

    def test_significant_badge_for_nonzero(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        html = build_semantic_equivalence_html(names, meta)
        assert "SIGNIFICANT DIFF(S)" in html

    def test_contains_match_count(self):
        names = ["SolverA"]
        meta = _make_solver_metadata(names, with_semantic=True)
        html = build_semantic_equivalence_html(names, meta)
        assert "42" in html  # match_count

    def test_contains_stats_table(self):
        names = ["SolverA"]
        meta = _make_solver_metadata(names, with_semantic=True)
        html = build_semantic_equivalence_html(names, meta)
        assert "Matching keys" in html
        assert "Cosmetic differences" in html
        assert "Convention differences" in html
        assert "Significant differences" in html
        assert "Total keys compared" in html

    def test_diff_tables_rendered(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        html = build_semantic_equivalence_html(names, meta)
        # Significant diffs expanded, others collapsed
        assert "Significant Differences" in html
        assert "Convention Differences" in html
        assert "Cosmetic Differences" in html

    def test_significant_diff_key_comment(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        html = build_semantic_equivalence_html(names, meta)
        # DivideNonPlanarPanels has a known comment
        assert "non-planar" in html.lower() or "DivideNonPlanarPanels" in html

    def test_convention_diff_key_shown(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        html = build_semantic_equivalence_html(names, meta)
        assert "WavesReferredToBy" in html

    def test_cosmetic_diff_collapsed(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        html = build_semantic_equivalence_html(names, meta)
        # Cosmetic section should be in a <details> tag
        assert "<details" in html

    def test_orange_badge_for_few_significant(self):
        names = ["SolverA"]
        meta = _make_semantic_metadata_with_diffs(names)
        # 2 significant diffs => orange badge
        html = build_semantic_equivalence_html(names, meta)
        assert "#f39c12" in html  # orange badge

    def test_red_badge_for_many_significant(self):
        names = ["SolverA"]
        meta = _make_solver_metadata(names)
        meta["SolverA"]["_semantic_equivalence"] = {
            "match_count": 10,
            "cosmetic_count": 0,
            "convention_count": 0,
            "significant_count": 8,
            "diffs": [
                {"key": f"Param{i}", "level": "significant",
                 "owd": "a", "spec": "b"}
                for i in range(8)
            ],
        }
        html = build_semantic_equivalence_html(names, meta)
        assert "#e74c3c" in html  # red badge
