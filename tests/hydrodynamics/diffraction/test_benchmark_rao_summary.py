"""Tests for benchmark_rao_summary — amplitude/phase summary and HTML rendering.

ABOUTME: Tests for compute_amplitude_summary, compute_phase_summary,
build_summary_table, and render_html_with_table.
"""
from __future__ import annotations

from pathlib import Path

import numpy as np
import plotly.graph_objects as go
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_rao_summary import (
    build_summary_table,
    compute_amplitude_summary,
    compute_phase_summary,
    render_html_with_table,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

from tests.hydrodynamics.diffraction.conftest import (
    _make_solver_results,
    HEADINGS,
    N_HEAD,
)


def _build_two_solver_results():
    return {
        "AQWA": _make_solver_results("AQWA", seed_offset=0),
        "OrcaWave": _make_solver_results("OrcaWave", seed_offset=0, magnitude_scale=1.02),
    }


def _build_three_solver_results():
    return {
        "AQWA": _make_solver_results("AQWA", seed_offset=0),
        "OrcaWave": _make_solver_results("OrcaWave", seed_offset=0, magnitude_scale=1.02),
        "BEMRosetta": _make_solver_results(
            "BEMRosetta", seed_offset=0, magnitude_scale=1.01, heave_bias=0.15,
        ),
    }


# ---------------------------------------------------------------------------
# compute_amplitude_summary
# ---------------------------------------------------------------------------


class TestComputeAmplitudeSummary:
    def test_returns_six_sections(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        assert len(sections) == 6

    def test_section_keys(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        for sec in sections:
            assert "dof" in sec
            assert "unit" in sec
            assert "rows" in sec

    def test_dof_names(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        dof_names = [s["dof"] for s in sections]
        assert "Surge" in dof_names
        assert "Heave" in dof_names
        assert "Yaw" in dof_names

    def test_rows_non_empty(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        for sec in sections:
            assert len(sec["rows"]) > 0

    def test_row_contains_diff_pct(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        for sec in sections:
            for row in sec["rows"]:
                assert "diff_pct" in row

    def test_units_are_valid(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        valid_units = {"m/m", "deg/m"}
        for sec in sections:
            assert sec["unit"] in valid_units

    def test_with_specific_headings(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, [0.0, 90.0])
        for sec in sections:
            # 2 headings * 2 solvers = 4 rows
            assert len(sec["rows"]) == 4

    def test_three_solvers(self):
        results = _build_three_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, [0.0])
        for sec in sections:
            # 1 heading * 3 solvers = 3 rows
            assert len(sec["rows"]) == 3


# ---------------------------------------------------------------------------
# compute_phase_summary
# ---------------------------------------------------------------------------


class TestComputePhaseSummary:
    def test_returns_six_sections(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, None)
        assert len(sections) == 6

    def test_section_keys(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, None)
        for sec in sections:
            assert "dof" in sec
            assert "rows" in sec

    def test_rows_have_phase_keys(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, None)
        for sec in sections:
            for row in sec["rows"]:
                assert "phase_at_peak" in row
                assert "long_period_phase" in row
                assert "phase_diff" in row

    def test_with_headings_filter(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, [0.0, 180.0])
        for sec in sections:
            assert len(sec["rows"]) == 4  # 2 headings * 2 solvers

    def test_phase_diff_ref_solver_is_zero(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, [0.0])
        for sec in sections:
            # First solver's diff from itself should be 0.0
            ref_row = sec["rows"][0]
            assert ref_row["solver"] == names[0]
            assert ref_row["phase_diff"] == "0.0"


# ---------------------------------------------------------------------------
# build_summary_table
# ---------------------------------------------------------------------------


class TestBuildSummaryTable:
    def test_amplitude_mode(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, [0.0])
        html = build_summary_table(sections, "Amplitude")
        assert "<table>" in html
        assert "Peak" in html

    def test_phase_mode(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, [0.0])
        html = build_summary_table(sections, "Phase")
        assert "<table>" in html
        assert "Phase@Peak" in html

    def test_contains_dof_headings(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        html = build_summary_table(sections, "Amplitude")
        assert "Surge" in html
        assert "Heave" in html

    def test_contains_unit(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_amplitude_summary(results, names, None)
        html = build_summary_table(sections, "Amplitude")
        assert "m/m" in html

    def test_empty_sections(self):
        html = build_summary_table([], "Amplitude")
        assert isinstance(html, str)

    def test_returns_string(self):
        results = _build_two_solver_results()
        names = list(results.keys())
        sections = compute_phase_summary(results, names, [0.0])
        html = build_summary_table(sections, "Phase")
        assert isinstance(html, str)


# ---------------------------------------------------------------------------
# render_html_with_table
# ---------------------------------------------------------------------------


class TestRenderHtmlWithTable:
    def test_creates_html_file(self, tmp_path):
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=[1, 2], y=[3, 4]))
        fig.update_layout(title_text="Test")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_amplitude_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "test_render", "Amplitude", tmp_path)
        assert path.exists()
        assert path.suffix == ".html"

    def test_returns_path(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="T")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_phase_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "test_path", "Phase", tmp_path)
        assert isinstance(path, Path)

    def test_html_contains_plotly(self, tmp_path):
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=[1], y=[1]))
        fig.update_layout(title_text="Plot")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_amplitude_summary(results, names, None)
        path = render_html_with_table(fig, summary, "check_plotly", "Amplitude", tmp_path)
        content = path.read_text()
        assert "plotly" in content.lower()

    def test_html_contains_table(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="T")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_amplitude_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "check_table", "Amplitude", tmp_path)
        content = path.read_text()
        assert "<table>" in content

    def test_html_contains_title(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="My RAO Title")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_amplitude_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "title_check", "Amplitude", tmp_path)
        content = path.read_text()
        assert "My RAO Title" in content

    def test_filename_matches(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="T")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_phase_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "my_output", "Phase", tmp_path)
        assert path.name == "my_output.html"

    def test_html_has_grid_layout(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="T")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_amplitude_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "grid_check", "Amplitude", tmp_path)
        content = path.read_text()
        assert "grid" in content

    def test_phase_mode_rendering(self, tmp_path):
        fig = go.Figure()
        fig.update_layout(title_text="Phase Test")
        results = _build_two_solver_results()
        names = list(results.keys())
        summary = compute_phase_summary(results, names, [0.0])
        path = render_html_with_table(fig, summary, "phase_render", "Phase", tmp_path)
        content = path.read_text()
        assert "Phase@Peak" in content
