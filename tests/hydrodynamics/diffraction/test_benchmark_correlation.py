"""Tests for benchmark_correlation module.

ABOUTME: Tests for pairwise correlation heatmap, 6x6 matrix rendering,
hydro coefficients HTML, coupling heatmap HTML, and raw RAO data tables.
"""
from __future__ import annotations

from datetime import datetime
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_correlation import (
    build_coupling_heatmap_html,
    build_hydro_coefficients_html,
    build_raw_rao_data_html,
    plot_pairwise_correlation_heatmap,
    render_6x6_matrix,
)
from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    DeviationStatistics,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
    ConsensusMetrics,
    PairwiseRAOComparison,
    PairwiseResult,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

from tests.hydrodynamics.diffraction.conftest import _make_solver_results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_deviation_stats(correlation: float = 0.99) -> DeviationStatistics:
    """Minimal DeviationStatistics for testing."""
    freqs = np.linspace(0.05, 2.0, 10)
    return DeviationStatistics(
        mean_error=0.01,
        max_error=0.05,
        rms_error=0.02,
        mean_abs_error=0.015,
        correlation=correlation,
        frequencies=freqs,
        errors=np.zeros(10),
    )


def _make_pairwise_rao_comparison(
    dof: DOF,
    solver_a: str = "SolverA",
    solver_b: str = "SolverB",
    correlation: float = 0.99,
) -> PairwiseRAOComparison:
    return PairwiseRAOComparison(
        dof=dof,
        solver_a=solver_a,
        solver_b=solver_b,
        magnitude_stats=_make_deviation_stats(correlation),
        phase_stats=_make_deviation_stats(correlation),
        max_magnitude_diff=0.05,
        max_phase_diff=5.0,
        magnitude_at_max_phase_diff=0.1,
        peak_magnitude=1.0,
        max_phase_diff_x=1.0,
        max_phase_diff_heading_idx=0,
    )


def _make_pairwise_result(
    solver_a: str = "SolverA",
    solver_b: str = "SolverB",
    correlation: float = 0.99,
) -> PairwiseResult:
    rao_comps = {}
    for dof in DOF:
        key = dof.name.lower()
        rao_comps[key] = _make_pairwise_rao_comparison(
            dof, solver_a, solver_b, correlation
        )
    # Build 6x6 correlation dicts with tuple keys
    am_corr = {}
    damp_corr = {}
    for i in range(1, 7):
        for j in range(1, 7):
            am_corr[(i, j)] = correlation
            damp_corr[(i, j)] = correlation
    return PairwiseResult(
        solver_a=solver_a,
        solver_b=solver_b,
        rao_comparisons=rao_comps,
        added_mass_correlations=am_corr,
        damping_correlations=damp_corr,
        overall_agreement="EXCELLENT",
    )


def _make_benchmark_report(
    solver_names: list[str] | None = None,
    correlation: float = 0.99,
) -> BenchmarkReport:
    if solver_names is None:
        solver_names = ["SolverA", "SolverB"]
    pairs = {}
    for i in range(len(solver_names)):
        for j in range(i + 1, len(solver_names)):
            key = f"{solver_names[i]} vs {solver_names[j]}"
            pairs[key] = _make_pairwise_result(
                solver_names[i], solver_names[j], correlation
            )
    consensus = {}
    for dof in DOF:
        consensus[dof.name.lower()] = ConsensusMetrics(
            dof=dof,
            solver_names=solver_names,
            agreement_pairs=[(solver_names[0], solver_names[1])],
            consensus_level="FULL",
            mean_pairwise_correlation=correlation,
        )
    return BenchmarkReport(
        vessel_name="TestVessel",
        solver_names=solver_names,
        comparison_date=datetime.now().isoformat(),
        pairwise_results=pairs,
        consensus_by_dof=consensus,
        overall_consensus="FULL",
        notes=["Test report"],
    )


# ---------------------------------------------------------------------------
# Tests: render_6x6_matrix
# ---------------------------------------------------------------------------


class TestRender6x6Matrix:
    """Tests for the render_6x6_matrix function."""

    def test_returns_html_string(self):
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(i, j): 1.0 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        assert isinstance(html, str)
        assert "<table" in html

    def test_contains_all_labels(self):
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(i, j): 0.95 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        for label in labels:
            assert label in html

    def test_green_background_for_perfect_correlation(self):
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(i, j): 1.0 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        assert "#d5f5e3" in html  # green background

    def test_yellow_background_for_high_correlation(self):
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(i, j): 0.995 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        assert "#fef9e7" in html  # yellow background

    def test_red_background_for_low_correlation(self):
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(i, j): 0.5 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        assert "#fadbd8" in html  # red background

    def test_string_key_format(self):
        """render_6x6_matrix supports string keys like '1,2'."""
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {f"{i},{j}": 0.98 for i in range(1, 7) for j in range(1, 7)}
        html = render_6x6_matrix(corr, labels)
        assert "0.980000" in html

    def test_missing_keys_render_dash(self):
        """Missing keys should render as '-'."""
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {}  # empty
        html = render_6x6_matrix(corr, labels)
        assert "<td>-</td>" in html

    def test_mixed_keys(self):
        """Some present, some missing."""
        labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        corr = {(1, 1): 0.999, (2, 2): 1.0}
        html = render_6x6_matrix(corr, labels)
        assert "<td>-</td>" in html
        assert "0.999000" in html


# ---------------------------------------------------------------------------
# Tests: plot_pairwise_correlation_heatmap
# ---------------------------------------------------------------------------


class TestPlotPairwiseCorrelationHeatmap:
    """Tests for plot_pairwise_correlation_heatmap."""

    def test_returns_path(self, tmp_path):
        report = _make_benchmark_report()
        result = plot_pairwise_correlation_heatmap(report, tmp_path)
        assert isinstance(result, Path)

    def test_creates_output_file(self, tmp_path):
        report = _make_benchmark_report()
        result = plot_pairwise_correlation_heatmap(report, tmp_path)
        assert result.exists()

    def test_three_solver_heatmap(self, tmp_path):
        report = _make_benchmark_report(
            solver_names=["AQWA", "OrcaWave", "BEMRosetta"]
        )
        result = plot_pairwise_correlation_heatmap(report, tmp_path)
        assert result.exists()


# ---------------------------------------------------------------------------
# Tests: build_hydro_coefficients_html
# ---------------------------------------------------------------------------


class TestBuildHydroCoefficientsHtml:
    """Tests for build_hydro_coefficients_html."""

    def test_returns_html_string(self):
        report = _make_benchmark_report()
        html = build_hydro_coefficients_html(report)
        assert isinstance(html, str)
        assert "Hydrodynamic Coefficients" in html

    def test_contains_added_mass_and_damping(self):
        report = _make_benchmark_report()
        html = build_hydro_coefficients_html(report)
        assert "Added Mass" in html
        assert "Radiation Damping" in html

    def test_empty_pairwise_returns_empty(self):
        report = _make_benchmark_report()
        report.pairwise_results = {}
        html = build_hydro_coefficients_html(report)
        assert html == ""

    def test_contains_pair_key(self):
        report = _make_benchmark_report()
        html = build_hydro_coefficients_html(report)
        assert "SolverA vs SolverB" in html


# ---------------------------------------------------------------------------
# Tests: build_coupling_heatmap_html
# ---------------------------------------------------------------------------


class TestBuildCouplingHeatmapHtml:
    """Tests for build_coupling_heatmap_html."""

    def test_returns_path(self, tmp_path):
        am = [[1.0] * 6 for _ in range(6)]
        damp = [[0.99] * 6 for _ in range(6)]
        result = build_coupling_heatmap_html(
            tmp_path, am, damp, "BodyA", "BodyB"
        )
        assert isinstance(result, Path)

    def test_creates_html_file(self, tmp_path):
        am = [[1.0] * 6 for _ in range(6)]
        damp = [[0.99] * 6 for _ in range(6)]
        result = build_coupling_heatmap_html(
            tmp_path, am, damp, "BodyA", "BodyB"
        )
        assert result.exists()
        assert result.suffix == ".html"

    def test_file_contains_body_names(self, tmp_path):
        am = [[1.0] * 6 for _ in range(6)]
        damp = [[0.99] * 6 for _ in range(6)]
        result = build_coupling_heatmap_html(
            tmp_path, am, damp, "Hull", "Turret"
        )
        content = result.read_text(encoding="utf-8")
        assert "Hull" in content
        assert "Turret" in content

    def test_filename_sanitized(self, tmp_path):
        am = [[1.0] * 6 for _ in range(6)]
        damp = [[0.99] * 6 for _ in range(6)]
        result = build_coupling_heatmap_html(
            tmp_path, am, damp, "Body A!", "Body B?"
        )
        # Non-alphanum chars (except _-) stripped from filename
        assert "!" not in result.name
        assert "?" not in result.name

    def test_contains_added_mass_section(self, tmp_path):
        am = [[0.998] * 6 for _ in range(6)]
        damp = [[0.996] * 6 for _ in range(6)]
        result = build_coupling_heatmap_html(
            tmp_path, am, damp, "A", "B"
        )
        content = result.read_text(encoding="utf-8")
        assert "Added Mass Coupling" in content
        assert "Radiation Damping Coupling" in content


# ---------------------------------------------------------------------------
# Tests: build_raw_rao_data_html
# ---------------------------------------------------------------------------


class TestBuildRawRaoDataHtml:
    """Tests for build_raw_rao_data_html."""

    def test_returns_html_string(self):
        names = ["SolverA", "SolverB"]
        results = {
            "SolverA": _make_solver_results("SolverA"),
            "SolverB": _make_solver_results("SolverB"),
        }
        html = build_raw_rao_data_html(results, names)
        assert isinstance(html, str)
        assert "Raw RAO Data" in html

    def test_contains_all_dof_sections(self):
        names = ["SolverA"]
        results = {"SolverA": _make_solver_results("SolverA")}
        html = build_raw_rao_data_html(results, names)
        for dof_name in ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]:
            assert dof_name in html

    def test_contains_details_tag(self):
        names = ["SolverA"]
        results = {"SolverA": _make_solver_results("SolverA")}
        html = build_raw_rao_data_html(results, names)
        assert "<details>" in html
        assert "</details>" in html

    def test_contains_table_data(self):
        names = ["SolverA"]
        results = {"SolverA": _make_solver_results("SolverA")}
        html = build_raw_rao_data_html(results, names)
        assert "<table" in html
        assert "T (s)" in html  # period column header

    def test_two_solver_columns(self):
        names = ["SolverA", "SolverB"]
        results = {
            "SolverA": _make_solver_results("SolverA"),
            "SolverB": _make_solver_results("SolverB"),
        }
        html = build_raw_rao_data_html(results, names)
        assert "SolverA" in html
        assert "SolverB" in html

    def test_heading_filter(self):
        """Passing specific headings should limit columns."""
        names = ["SolverA"]
        results = {"SolverA": _make_solver_results("SolverA")}
        html = build_raw_rao_data_html(results, names, headings=[0.0, 90.0])
        # Should have 0deg and 90deg columns
        assert "0&deg;" in html or "0°" in html
        assert "90&deg;" in html or "90°" in html
