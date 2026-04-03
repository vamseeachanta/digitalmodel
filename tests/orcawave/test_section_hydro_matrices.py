"""Tests for reporting/sections/hydro_matrices.py — build_hydro_matrices()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import HydroMatricesConfig
from digitalmodel.orcawave.reporting.sections.hydro_matrices import (
    build_hydro_matrices,
    _DOF_LABELS,
    _MAX_BODIES,
)


@pytest.fixture
def config():
    return HydroMatricesConfig()


# ---------------------------------------------------------------------------
# build_hydro_matrices — plotly unavailable
# ---------------------------------------------------------------------------


class TestHydroMatricesNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "Plotly required" in result


# ---------------------------------------------------------------------------
# build_hydro_matrices — data unavailable
# ---------------------------------------------------------------------------


class TestHydroMatricesDataUnavailable:

    def test_returns_warning_when_added_mass_fails(self, config):
        diff = MagicMock()
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError("no AM"))
        result = build_hydro_matrices(diff, config)
        assert "alert-warning" in result
        assert "unavailable" in result.lower()


# ---------------------------------------------------------------------------
# build_hydro_matrices — with plotly and valid data
# ---------------------------------------------------------------------------


class TestHydroMatricesWithPlotly:

    def test_returns_html_string(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_card_structure(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "card" in result
        assert "card-header" in result
        assert "card-body" in result

    def test_single_body_has_one_card(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        # Single body -> "Body" (not "Body 1")
        assert "Body" in result

    def test_two_body_has_two_cards(self, mock_diff_two_body, config):
        result = build_hydro_matrices(mock_diff_two_body, config)
        assert "Body 1" in result
        assert "Body 2" in result

    def test_card_mentions_added_mass_and_damping(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "Added Mass" in result
        assert "Damping" in result or "damping" in result.lower()

    def test_max_bodies_limit(self, config):
        """Even with 3+ bodies, only first _MAX_BODIES are plotted."""
        diff = MagicMock()
        n_dofs = 18  # 3 bodies
        diff.addedMass = np.zeros((4, n_dofs, n_dofs))
        diff.damping = np.zeros((4, n_dofs, n_dofs))
        diff.frequencies = np.array([0.3, 0.2, 0.1, 0.05])
        result = build_hydro_matrices(diff, config)
        # Should show at most _MAX_BODIES cards
        assert result.count("card-header") <= _MAX_BODIES


# ---------------------------------------------------------------------------
# Module constants
# ---------------------------------------------------------------------------


class TestHydroConstants:

    def test_dof_labels_has_six(self):
        assert len(_DOF_LABELS) == 6
        assert "Surge" in _DOF_LABELS
        assert "Yaw" in _DOF_LABELS

    def test_max_bodies_positive(self):
        assert _MAX_BODIES > 0
