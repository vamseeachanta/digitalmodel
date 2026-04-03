"""Tests for reporting/sections/multi_body.py — build_multi_body()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import MultiBodyConfig
from digitalmodel.orcawave.reporting.sections.multi_body import (
    build_multi_body,
    _DOF_SHORT,
)


@pytest.fixture
def config():
    return MultiBodyConfig()


# ---------------------------------------------------------------------------
# build_multi_body — data unavailable
# ---------------------------------------------------------------------------


class TestMultiBodyUnavailable:

    def test_returns_warning_when_added_mass_fails(self, config):
        diff = MagicMock()
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError("no data"))
        result = build_multi_body(diff, config)
        assert "alert-warning" in result
        assert "unavailable" in result.lower()


# ---------------------------------------------------------------------------
# build_multi_body — single body
# ---------------------------------------------------------------------------


class TestMultiBodySingleBody:

    def test_single_body_shows_no_coupling(self, mock_diff_single_body, config):
        result = build_multi_body(mock_diff_single_body, config)
        assert "Single body" in result or "no coupling" in result.lower()
        assert "card" in result

    def test_single_body_muted_text(self, mock_diff_single_body, config):
        result = build_multi_body(mock_diff_single_body, config)
        assert "text-muted" in result


# ---------------------------------------------------------------------------
# build_multi_body — plotly unavailable for multi-body
# ---------------------------------------------------------------------------


class TestMultiBodyNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        assert "Plotly required" in result


# ---------------------------------------------------------------------------
# build_multi_body — two body with plotly
# ---------------------------------------------------------------------------


class TestMultiBodyTwoBody:

    def test_returns_html_string(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_card_structure(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        assert "card" in result
        assert "Multi-Body Coupling" in result

    def test_contains_heatmap(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        # Plotly heatmap generates HTML with the data
        assert "card-body" in result

    def test_contains_coupling_table(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        assert "coupling" in result.lower()
        assert "RMS" in result
        assert "B1" in result

    def test_contains_body_pair_label(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        # Two bodies -> B1–B2 pair
        assert "B1" in result
        assert "B2" in result

    def test_mentions_period(self, mock_diff_two_body, config):
        result = build_multi_body(mock_diff_two_body, config)
        assert "T =" in result

    def test_three_body_model(self, config):
        """Test with 3 bodies — should show 3 body pairs."""
        diff = MagicMock()
        n_dofs = 18  # 3 bodies
        rng = np.random.RandomState(123)
        diff.addedMass = rng.rand(4, n_dofs, n_dofs) * 1e6
        diff.frequencies = np.array([0.4, 0.3, 0.2, 0.1])
        diff.headings = [0.0, 180.0]
        result = build_multi_body(diff, config)
        # Should have body pairs: B1-B2, B1-B3, B2-B3
        assert "B1" in result
        assert "B2" in result
        assert "B3" in result


# ---------------------------------------------------------------------------
# Module constants
# ---------------------------------------------------------------------------


class TestMultiBodyConstants:

    def test_dof_short_labels(self):
        assert len(_DOF_SHORT) == 6
        assert "Su" in _DOF_SHORT
        assert "Ya" in _DOF_SHORT

    def test_dof_short_all_two_chars(self):
        for label in _DOF_SHORT:
            assert len(label) == 2
