"""Tests for reporting/sections/rao_plots.py — build_rao_plots()."""
from __future__ import annotations

from unittest.mock import MagicMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import RAOPlotsConfig
from digitalmodel.orcawave.reporting.sections.rao_plots import (
    build_rao_plots,
    _render_tabs,
    _DOF_LABELS,
    _DOF_UNITS,
)


@pytest.fixture
def config():
    return RAOPlotsConfig()


@pytest.fixture
def phase_config():
    return RAOPlotsConfig(include_phase=True)


@pytest.fixture
def subset_config():
    return RAOPlotsConfig(dofs=["heave", "pitch"])


@pytest.fixture
def heading_filter_config():
    return RAOPlotsConfig(headings=[0.0, 180.0])


# ---------------------------------------------------------------------------
# build_rao_plots — plotly unavailable
# ---------------------------------------------------------------------------


class TestRAOPlotsNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, mock_diff_single_body, config):
        result = build_rao_plots(mock_diff_single_body, config)
        assert "Plotly is required" in result
        assert "section-placeholder" in result


# ---------------------------------------------------------------------------
# build_rao_plots — with plotly
# ---------------------------------------------------------------------------


class TestRAOPlotsWithPlotly:

    def test_returns_html_string(self, mock_diff_single_body, config):
        result = build_rao_plots(mock_diff_single_body, config)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_tabs(self, mock_diff_single_body, config):
        result = build_rao_plots(mock_diff_single_body, config)
        assert "nav-tabs" in result
        assert "tab-content" in result

    def test_all_six_dofs_present(self, mock_diff_single_body, config):
        result = build_rao_plots(mock_diff_single_body, config)
        for dof in ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]:
            assert dof in result

    def test_subset_dofs(self, mock_diff_single_body, subset_config):
        result = build_rao_plots(mock_diff_single_body, subset_config)
        assert "Heave" in result
        assert "Pitch" in result
        # Surge should NOT be in the tabs
        assert "rao-tab-surge" not in result

    def test_heading_filter(self, mock_diff_single_body, heading_filter_config):
        """Heading filter selects only specified headings."""
        result = build_rao_plots(mock_diff_single_body, heading_filter_config)
        assert isinstance(result, str)
        # Should contain 0° and 180° traces but not 90°
        assert "0°" in result
        assert "180°" in result

    def test_include_phase_adds_phase_traces(self, mock_diff_single_body, phase_config):
        result = build_rao_plots(mock_diff_single_body, phase_config)
        assert "phase" in result.lower()

    def test_empty_dofs_returns_placeholder(self, mock_diff_single_body):
        cfg = RAOPlotsConfig(dofs=[])
        result = build_rao_plots(mock_diff_single_body, cfg)
        assert "No DOFs enabled" in result

    def test_include_plotlyjs_cdn(self, mock_diff_single_body, config):
        result = build_rao_plots(mock_diff_single_body, config, include_plotlyjs="cdn")
        # The first figure should include plotly JS
        assert isinstance(result, str)

    def test_two_body_model(self, mock_diff_two_body, config):
        """RAO plots should work with multi-body models (uses first body)."""
        result = build_rao_plots(mock_diff_two_body, config)
        assert isinstance(result, str)
        assert "nav-tabs" in result


# ---------------------------------------------------------------------------
# _render_tabs
# ---------------------------------------------------------------------------


class TestRenderTabs:

    def test_empty_tabs_returns_placeholder(self):
        result = _render_tabs([], [], [])
        assert "No DOFs enabled" in result

    def test_single_tab(self):
        result = _render_tabs(["tab-1"], ["Heave"], ["<p>content</p>"])
        assert "Heave" in result
        assert "tab-1" in result
        assert "<p>content</p>" in result
        assert "active" in result  # first tab is active

    def test_multiple_tabs(self):
        result = _render_tabs(
            ["tab-1", "tab-2", "tab-3"],
            ["A", "B", "C"],
            ["<p>1</p>", "<p>2</p>", "<p>3</p>"],
        )
        assert "A" in result
        assert "B" in result
        assert "C" in result
        # First tab active, others not
        assert 'class="nav-link active"' in result

    def test_tabs_have_bootstrap_structure(self):
        result = _render_tabs(["t1"], ["L1"], ["body"])
        assert "nav-tabs" in result
        assert "tab-content" in result
        assert "tab-pane" in result
        assert "data-bs-toggle" in result


# ---------------------------------------------------------------------------
# Module-level constants
# ---------------------------------------------------------------------------


class TestConstants:

    def test_dof_labels_count(self):
        assert len(_DOF_LABELS) == 6

    def test_dof_units_keys(self):
        for label in _DOF_LABELS:
            assert label in _DOF_UNITS

    def test_translational_units(self):
        for dof in ["surge", "sway", "heave"]:
            assert _DOF_UNITS[dof] == "m/m"

    def test_rotational_units(self):
        for dof in ["roll", "pitch", "yaw"]:
            assert _DOF_UNITS[dof] == "deg/m"
