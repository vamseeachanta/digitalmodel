"""Tests for reporting/sections/qtf_heatmap.py — build_qtf_heatmap()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import QTFHeatmapConfig
from digitalmodel.orcawave.reporting.sections.qtf_heatmap import (
    build_qtf_heatmap,
    _qtf_results_available,
)


@pytest.fixture
def config():
    return QTFHeatmapConfig()


@pytest.fixture
def config_max5():
    return QTFHeatmapConfig(max_delta_omegas=5)


# ---------------------------------------------------------------------------
# _qtf_results_available
# ---------------------------------------------------------------------------


class TestQTFAvailability:

    def test_available_when_qtf_not_none(self):
        diff = MagicMock()
        diff.QTFResults = np.zeros((2, 4, 4, 6))
        assert _qtf_results_available(diff) is True

    def test_unavailable_when_qtf_none(self):
        diff = MagicMock()
        diff.QTFResults = None
        assert _qtf_results_available(diff) is False

    def test_unavailable_when_attr_error(self):
        diff = MagicMock(spec=[])
        # spec=[] prevents auto-creation of attributes
        assert _qtf_results_available(diff) is False

    def test_unavailable_when_runtime_error(self):
        diff = MagicMock()
        type(diff).QTFResults = PropertyMock(side_effect=RuntimeError)
        assert _qtf_results_available(diff) is False


# ---------------------------------------------------------------------------
# build_qtf_heatmap — QTF not available
# ---------------------------------------------------------------------------


class TestQTFNotAvailable:

    def test_returns_skip_message(self, config):
        diff = MagicMock()
        diff.QTFResults = None
        result = build_qtf_heatmap(diff, config)
        assert "not available" in result.lower()
        assert "card" in result
        assert "alert-secondary" in result

    def test_returns_skip_when_attr_error(self, config):
        diff = MagicMock(spec=[])
        # spec=[] makes QTFResults access raise AttributeError
        result = build_qtf_heatmap(diff, config)
        assert "not available" in result.lower()


# ---------------------------------------------------------------------------
# build_qtf_heatmap — plotly unavailable
# ---------------------------------------------------------------------------


class TestQTFNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert "Plotly required" in result


# ---------------------------------------------------------------------------
# build_qtf_heatmap — with data and plotly
# ---------------------------------------------------------------------------


class TestQTFWithData:

    def test_returns_html_string(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_card_structure(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert "card" in result
        assert "QTF Heatmap" in result

    def test_mentions_heading(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert "heading" in result.lower()

    def test_mentions_surge(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert "Surge" in result

    def test_mentions_max_delta_omegas(self, mock_diff_with_qtf, config):
        result = build_qtf_heatmap(mock_diff_with_qtf, config)
        assert "max" in result.lower()

    def test_custom_max_delta_omegas(self, mock_diff_with_qtf, config_max5):
        result = build_qtf_heatmap(mock_diff_with_qtf, config_max5)
        assert "5" in result

    def test_3d_qtf_array(self, config):
        """Handle 3D QTF array (no heading dimension)."""
        diff = MagicMock()
        diff.QTFResults = np.random.rand(4, 4, 6)
        diff.QTFAngularFrequencies = np.array([0.5, 1.0, 1.5, 2.0])
        diff.headings = [180.0]
        result = build_qtf_heatmap(diff, config)
        assert "card" in result

    def test_2d_qtf_array(self, config):
        """Handle 2D QTF array (single dof, single heading)."""
        diff = MagicMock()
        diff.QTFResults = np.random.rand(4, 4)
        diff.QTFAngularFrequencies = np.array([0.5, 1.0, 1.5, 2.0])
        diff.headings = [180.0]
        result = build_qtf_heatmap(diff, config)
        assert "card" in result

    def test_build_card_exception_caught(self, config):
        """If _build_card raises, build_qtf_heatmap returns a warning."""
        diff = MagicMock()
        diff.QTFResults = np.random.rand(2, 4, 4, 6)
        # Make QTFAngularFrequencies raise to trigger exception inside _build_card
        type(diff).QTFAngularFrequencies = PropertyMock(side_effect=ValueError("bad"))
        result = build_qtf_heatmap(diff, config)
        assert "alert-warning" in result
        assert "failed" in result.lower()
