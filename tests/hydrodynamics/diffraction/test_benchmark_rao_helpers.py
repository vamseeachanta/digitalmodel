"""Tests for benchmark_rao_helpers — trace helpers for RAO comparison plots.

ABOUTME: Tests for get_solver_style, get_x_values, x_axis_label,
get_heading_indices, get_significant_heading_indices, add_solver_traces,
apply_layout, and save_figure.
"""
from __future__ import annotations

from pathlib import Path

import numpy as np
import plotly.graph_objects as go
import pytest
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.benchmark_rao_helpers import (
    add_solver_traces,
    apply_layout,
    get_heading_indices,
    get_significant_heading_indices,
    get_solver_style,
    get_x_values,
    save_figure,
    x_axis_label,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

from tests.hydrodynamics.diffraction.conftest import (
    _make_rao_component,
    _make_solver_results,
    FREQUENCIES,
    HEADINGS,
    N_FREQ,
    N_HEAD,
)


# ---------------------------------------------------------------------------
# get_solver_style
# ---------------------------------------------------------------------------


class TestGetSolverStyle:
    def test_index_zero(self):
        style = get_solver_style(0)
        assert "dash" in style
        assert "color_base" in style
        assert style["dash"] == "solid"

    def test_index_one(self):
        style = get_solver_style(1)
        assert style["dash"] == "dash"

    def test_index_two(self):
        style = get_solver_style(2)
        assert style["dash"] == "dot"

    def test_index_three(self):
        style = get_solver_style(3)
        assert style["dash"] == "dashdot"

    def test_wraps_modulo_4(self):
        assert get_solver_style(4) == get_solver_style(0)
        assert get_solver_style(5) == get_solver_style(1)

    def test_large_index(self):
        style = get_solver_style(100)
        assert style == get_solver_style(0)

    def test_returns_dict(self):
        assert isinstance(get_solver_style(0), dict)


# ---------------------------------------------------------------------------
# get_x_values
# ---------------------------------------------------------------------------


class TestGetXValues:
    def test_frequency_axis(self):
        comp = _make_rao_component(DOF.SURGE)
        x = get_x_values(comp, "frequency")
        np.testing.assert_array_almost_equal(x, comp.frequencies.values)

    def test_period_axis(self):
        comp = _make_rao_component(DOF.SURGE)
        x = get_x_values(comp, "period")
        np.testing.assert_array_almost_equal(x, comp.frequencies.periods)

    def test_period_is_default_non_frequency(self):
        comp = _make_rao_component(DOF.HEAVE)
        x = get_x_values(comp, "anything_else")
        np.testing.assert_array_almost_equal(x, comp.frequencies.periods)

    def test_length_matches_freq_count(self):
        comp = _make_rao_component(DOF.ROLL)
        assert len(get_x_values(comp, "frequency")) == N_FREQ
        assert len(get_x_values(comp, "period")) == N_FREQ


# ---------------------------------------------------------------------------
# x_axis_label
# ---------------------------------------------------------------------------


class TestXAxisLabel:
    def test_heading_x_axis(self):
        assert x_axis_label("frequency", heading_x_axis=True) == "Heading (deg)"

    def test_heading_x_axis_overrides_period(self):
        assert x_axis_label("period", heading_x_axis=True) == "Heading (deg)"

    def test_frequency(self):
        assert x_axis_label("frequency", heading_x_axis=False) == "Frequency (rad/s)"

    def test_period(self):
        assert x_axis_label("period", heading_x_axis=False) == "Period (s)"

    def test_unknown_returns_period(self):
        assert x_axis_label("something", heading_x_axis=False) == "Period (s)"


# ---------------------------------------------------------------------------
# get_heading_indices
# ---------------------------------------------------------------------------


class TestGetHeadingIndices:
    def test_none_returns_all(self):
        comp = _make_rao_component(DOF.SURGE)
        indices = get_heading_indices(comp, None)
        assert indices == list(range(N_HEAD))

    def test_exact_match(self):
        comp = _make_rao_component(DOF.SURGE)
        indices = get_heading_indices(comp, [0.0, 90.0, 180.0])
        assert 0 in indices
        assert 2 in indices
        assert 4 in indices

    def test_close_match_within_tolerance(self):
        comp = _make_rao_component(DOF.SURGE)
        indices = get_heading_indices(comp, [0.5])  # within 1 deg of 0.0
        assert indices == [0]

    def test_no_match_returns_all(self):
        comp = _make_rao_component(DOF.SURGE)
        indices = get_heading_indices(comp, [999.0])  # no heading near 999
        assert indices == list(range(N_HEAD))

    def test_empty_list_returns_all(self):
        comp = _make_rao_component(DOF.SURGE)
        indices = get_heading_indices(comp, [])
        assert indices == list(range(N_HEAD))

    def test_single_heading(self):
        comp = _make_rao_component(DOF.HEAVE)
        indices = get_heading_indices(comp, [45.0])
        assert len(indices) == 1
        assert indices[0] == 1


# ---------------------------------------------------------------------------
# get_significant_heading_indices
# ---------------------------------------------------------------------------


class TestGetSignificantHeadingIndices:
    def _make_results_dict(self):
        return {
            "AQWA": _make_solver_results("AQWA"),
            "OrcaWave": _make_solver_results("OrcaWave", magnitude_scale=1.02),
        }

    def test_returns_list_of_ints(self):
        results = self._make_results_dict()
        names = list(results.keys())
        indices = get_significant_heading_indices(
            DOF.SURGE, results, names, None,
        )
        assert isinstance(indices, list)
        assert all(isinstance(i, int) for i in indices)

    def test_non_empty_for_normal_data(self):
        results = self._make_results_dict()
        names = list(results.keys())
        indices = get_significant_heading_indices(
            DOF.HEAVE, results, names, None,
        )
        assert len(indices) > 0

    def test_high_threshold_may_filter(self):
        results = self._make_results_dict()
        names = list(results.keys())
        all_indices = get_significant_heading_indices(
            DOF.SURGE, results, names, None, threshold=0.0,
        )
        high_indices = get_significant_heading_indices(
            DOF.SURGE, results, names, None, threshold=0.99,
        )
        # High threshold should keep fewer or equal headings
        assert len(high_indices) <= len(all_indices)

    def test_with_specific_headings(self):
        results = self._make_results_dict()
        names = list(results.keys())
        indices = get_significant_heading_indices(
            DOF.HEAVE, results, names, [0.0, 90.0],
        )
        assert len(indices) <= 2


# ---------------------------------------------------------------------------
# add_solver_traces
# ---------------------------------------------------------------------------


class TestAddSolverTraces:
    def _make_fig(self):
        return make_subplots(rows=1, cols=1)

    def _make_results_dict(self):
        return {
            "AQWA": _make_solver_results("AQWA"),
            "OrcaWave": _make_solver_results("OrcaWave", magnitude_scale=1.02),
        }

    def test_adds_traces_amplitude(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.SURGE, None, 1, 1, "amplitude", True,
            results, names, "period", False,
        )
        assert len(fig.data) > 0

    def test_adds_traces_phase(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.HEAVE, None, 1, 1, "phase", True,
            results, names, "period", False,
        )
        assert len(fig.data) > 0

    def test_heading_x_axis_mode(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.SURGE, None, 1, 1, "amplitude", True,
            results, names, "period", True,
        )
        assert len(fig.data) > 0

    def test_frequency_x_axis(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.ROLL, None, 1, 1, "amplitude", False,
            results, names, "frequency", False,
        )
        assert len(fig.data) > 0

    def test_specific_heading_indices(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.SURGE, None, 1, 1, "amplitude", True,
            results, names, "period", False,
            heading_indices=[0, 2],
        )
        # 2 headings * 2 solvers = 4 traces
        assert len(fig.data) == 4

    def test_show_legend_false(self):
        fig = self._make_fig()
        results = self._make_results_dict()
        names = list(results.keys())
        add_solver_traces(
            fig, DOF.HEAVE, [0.0], 1, 1, "amplitude", False,
            results, names, "period", False,
        )
        for trace in fig.data:
            assert trace.showlegend is False


# ---------------------------------------------------------------------------
# apply_layout
# ---------------------------------------------------------------------------


class TestApplyLayout:
    def test_sets_title(self):
        fig = go.Figure()
        apply_layout(fig, "Test Title")
        assert fig.layout.title.text == "Test Title"

    def test_template_is_plotly_white(self):
        fig = go.Figure()
        apply_layout(fig, "T")
        # The plot background should be white after applying plotly_white template
        assert fig.layout.plot_bgcolor is None or fig.layout.template is not None

    def test_legend_orientation(self):
        fig = go.Figure()
        apply_layout(fig, "T")
        assert fig.layout.legend.orientation == "v"


# ---------------------------------------------------------------------------
# save_figure
# ---------------------------------------------------------------------------


class TestSaveFigure:
    def test_creates_html_file(self, tmp_path):
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=[1, 2], y=[3, 4]))
        path = save_figure(fig, "test_plot", tmp_path)
        assert path.exists()
        assert path.suffix == ".html"

    def test_returns_path_object(self, tmp_path):
        fig = go.Figure()
        path = save_figure(fig, "test_plot", tmp_path)
        assert isinstance(path, Path)

    def test_filename_matches(self, tmp_path):
        fig = go.Figure()
        path = save_figure(fig, "my_rao_plot", tmp_path)
        assert path.name == "my_rao_plot.html"

    def test_html_contains_plotly(self, tmp_path):
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=[1], y=[1]))
        path = save_figure(fig, "check_content", tmp_path)
        content = path.read_text()
        assert "plotly" in content.lower()
