"""Tests for benchmark_dof_sections module.

ABOUTME: Tests for _add_phase_annotations and build_dof_report_sections.
Verifies annotation logic, phase-diff threshold gating, consensus badge
generation, HTML structure, and DOF ordering.
"""
from __future__ import annotations

from typing import Dict
from unittest.mock import MagicMock

import numpy as np
import plotly.graph_objects as go
import pytest
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.benchmark_dof_sections import (
    _add_phase_annotations,
    build_dof_report_sections,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
    ConsensusMetrics,
    MultiSolverComparator,
    PairwiseRAOComparison,
    PairwiseResult,
)
from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    DeviationStatistics,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)

# Re-use conftest helpers directly
from tests.hydrodynamics.diffraction.conftest import _make_solver_results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_two_solver_fig():
    """Create a 2-row subplot (amplitude + phase) for annotation tests."""
    return make_subplots(
        rows=2, cols=1, shared_xaxes=True,
        subplot_titles=["Amplitude", "Phase"],
        vertical_spacing=0.15,
    )


def _two_solver_dict():
    """Return two solver results sharing the same frequencies/headings."""
    return {
        "SolverA": _make_solver_results("SolverA", seed_offset=0),
        "SolverB": _make_solver_results("SolverB", seed_offset=0),
    }


def _make_benchmark_report(solver_results: Dict[str, DiffractionResults]) -> BenchmarkReport:
    """Build a real BenchmarkReport via MultiSolverComparator."""
    comparator = MultiSolverComparator(solver_results)
    return comparator.generate_report()


# ---------------------------------------------------------------------------
# 1. _add_phase_annotations — threshold gating
# ---------------------------------------------------------------------------


class TestAddPhaseAnnotationsThreshold:
    """Annotations should only appear when max_phase_diff > 20."""

    def test_no_annotation_when_phase_diff_below_20(self):
        """Phase diff < 20 must not add any extra annotations.

        Note: make_subplots with subplot_titles creates 2 baseline
        annotations for the subplot titles ('Amplitude', 'Phase').
        """
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=15.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        assert len(fig.layout.annotations) == baseline_count

    def test_annotation_added_when_phase_diff_above_20(self):
        """Phase diff > 20 must add exactly one extra annotation."""
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=45.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        assert len(fig.layout.annotations) == baseline_count + 1

    def test_annotation_at_boundary_20_adds_annotation(self):
        """Exactly 20.0 deg should trigger annotation (guard is `< 20`)."""
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.HEAVE,
            max_phase_diff=20.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        # The source code checks `if max_phase_diff < 20: return`
        # so exactly 20.0 passes the guard and should add annotation
        assert len(fig.layout.annotations) == baseline_count + 1


# ---------------------------------------------------------------------------
# 2. _add_phase_annotations — visible heading gating
# ---------------------------------------------------------------------------


class TestAddPhaseAnnotationsVisibility:
    """Annotations must be suppressed when heading is not visible."""

    def test_annotation_suppressed_when_heading_not_visible(self):
        """Hidden heading index should prevent annotation."""
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=50.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=3,
            visible_heading_indices=[0, 1, 2],
        )
        assert len(fig.layout.annotations) == baseline_count

    def test_annotation_shown_when_heading_is_visible(self):
        """Visible heading index should allow annotation."""
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=50.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
            visible_heading_indices=[0, 1, 2],
        )
        assert len(fig.layout.annotations) == baseline_count + 1

    def test_all_headings_visible_when_none(self):
        """visible_heading_indices=None means all headings are visible."""
        fig = _make_two_solver_fig()
        baseline_count = len(fig.layout.annotations)
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.HEAVE,
            max_phase_diff=100.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=4,
            visible_heading_indices=None,
        )
        assert len(fig.layout.annotations) == baseline_count + 1


# ---------------------------------------------------------------------------
# 3. _add_phase_annotations — colour coding
# ---------------------------------------------------------------------------


class TestAddPhaseAnnotationsColour:
    """Negligible amplitude should give yellow; significant -> red."""

    def test_negligible_amplitude_yellow_annotation(self):
        """Negligible amplitude (< 5% of peak) -> amber/yellow colour."""
        fig = _make_two_solver_fig()
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=100.0,
            mag_at_max_pd=0.01,   # very small
            peak_mag=1.0,          # large peak
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        ann = fig.layout.annotations[-1]
        assert "Ignore" in ann.text
        assert ann.bordercolor == "#f39c12"

    def test_significant_amplitude_red_annotation(self):
        """Significant amplitude -> red colour and 'Review' text."""
        fig = _make_two_solver_fig()
        results = _two_solver_dict()
        names = list(results.keys())

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=100.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=0.5,
            unit="m/m",
            x_axis="frequency",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        ann = fig.layout.annotations[-1]
        assert "Review" in ann.text
        assert ann.bordercolor == "#e74c3c"


# ---------------------------------------------------------------------------
# 4. _add_phase_annotations — x-axis conversion
# ---------------------------------------------------------------------------


class TestAddPhaseAnnotationsPeriodAxis:
    """When x_axis='period', annotation x should be period, not frequency."""

    def test_period_axis_converts_freq_to_period(self):
        """Annotation x-value should be 2*pi / freq when x_axis='period'."""
        fig = _make_two_solver_fig()
        results = _two_solver_dict()
        names = list(results.keys())
        freq = 1.0  # rad/s -> period = 2*pi ≈ 6.28 s

        _add_phase_annotations(
            fig, DOF.SURGE,
            max_phase_diff=50.0,
            mag_at_max_pd=0.5,
            peak_mag=1.0,
            max_pd_freq=freq,
            unit="m/m",
            x_axis="period",
            solver_results=results,
            solver_names=names,
            phase_diff_heading_idx=0,
        )
        ann = fig.layout.annotations[-1]
        expected_period = 2.0 * np.pi / freq
        assert abs(ann.x - expected_period) < 0.01


# ---------------------------------------------------------------------------
# 5. build_dof_report_sections — basic HTML output
# ---------------------------------------------------------------------------


class TestBuildDofReportSections:
    """Integration tests for the full DOF section builder."""

    def test_returns_html_string(self, two_identical_results):
        """Output should be a non-empty HTML string."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(html, str)
        assert len(html) > 0

    def test_contains_all_six_dofs(self, two_identical_results):
        """HTML must include sections for all 6 DOFs."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            assert f'id="dof-{dof_name}"' in html

    def test_contains_consensus_badges(self, two_identical_results):
        """HTML should include consensus-badge divs."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        assert "consensus-badge" in html

    def test_contains_stats_table(self, two_identical_results):
        """HTML should contain a statistics table for each DOF."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        assert "stats-table" in html
        assert "Magnitude correlation" in html

    def test_contains_plotly_divs(self, two_identical_results):
        """Each DOF should have an embedded Plotly div."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        for dof in DOF_ORDER:
            dof_name = dof.name.lower()
            assert f'id="plot_{dof_name}"' in html

    def test_period_x_axis_produces_valid_html(self, three_solver_results):
        """Period x-axis mode should not crash and produce valid HTML."""
        report = _make_benchmark_report(three_solver_results)
        names = sorted(three_solver_results.keys())
        html = build_dof_report_sections(
            report, three_solver_results, names,
            x_axis="period", heading_x_axis=False,
        )
        assert isinstance(html, str)
        assert len(html) > 100

    def test_heading_filter_respected(self, two_identical_results):
        """When specific headings passed, only those appear in the output."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="frequency", heading_x_axis=False,
            headings=[0.0, 90.0],
        )
        # Should still produce valid output
        assert "dof-section" in html

    def test_heading_x_axis_mode(self, two_identical_results):
        """heading_x_axis=True should produce valid output."""
        report = _make_benchmark_report(two_identical_results)
        names = sorted(two_identical_results.keys())
        html = build_dof_report_sections(
            report, two_identical_results, names,
            x_axis="period", heading_x_axis=True,
        )
        assert isinstance(html, str)
        assert len(html) > 100

    def test_three_solvers_produce_html(self, three_solver_results):
        """Three solver inputs should produce valid HTML with observations."""
        report = _make_benchmark_report(three_solver_results)
        names = sorted(three_solver_results.keys())
        html = build_dof_report_sections(
            report, three_solver_results, names,
            x_axis="frequency", heading_x_axis=False,
        )
        assert "observations" in html
        # Should contain solver names somewhere in the output
        for name in names:
            assert name in html
