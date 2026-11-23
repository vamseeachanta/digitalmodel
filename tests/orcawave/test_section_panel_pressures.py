"""Tests for reporting/sections/panel_pressures.py — build_panel_pressures()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import PanelPressuresConfig
from digitalmodel.orcawave.reporting.sections.panel_pressures import (
    build_panel_pressures,
    _pressure_note,
)


@pytest.fixture
def config():
    return PanelPressuresConfig()


# ---------------------------------------------------------------------------
# build_panel_pressures — normal case
# ---------------------------------------------------------------------------


class TestPanelPressuresNormal:

    def test_returns_html_string(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert isinstance(result, str)
        assert "<table" in result

    def test_contains_panel_count(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "Total panels" in result
        assert ">4<" in result or "4" in result

    def test_contains_wetted_area(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "Total wetted area" in result
        # 2.5 + 3.1 + 1.8 + 4.0 = 11.40
        assert "11.40" in result

    def test_contains_min_max_mean_area(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "min" in result.lower()
        assert "max" in result.lower()
        assert "mean" in result.lower()

    def test_contains_body_names(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "Hull" in result
        assert "Skirt" in result

    def test_contains_card_structure(self, config, mock_panel_geometry):
        diff = MagicMock()
        diff.panelGeometry = mock_panel_geometry
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "card" in result
        assert "Panel Geometry Summary" in result


# ---------------------------------------------------------------------------
# build_panel_pressures — edge cases
# ---------------------------------------------------------------------------


class TestPanelPressuresEdgeCases:

    def test_empty_panel_geometry(self, config):
        diff = MagicMock()
        diff.panelGeometry = []
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "Total panels" in result
        # Empty list -> 0 panels but areas array defaults to [0.0]
        assert "0" in result

    def test_panel_geometry_unavailable(self, config):
        diff = MagicMock()
        type(diff).panelGeometry = PropertyMock(side_effect=RuntimeError("no panels"))
        result = build_panel_pressures(diff, config)
        assert "alert-warning" in result
        assert "unavailable" in result.lower()

    def test_panel_without_object_name(self, config):
        diff = MagicMock()
        diff.panelGeometry = [
            {"area": 1.0},  # no objectName key
            {"area": 2.0, "objectName": ""},  # empty objectName
        ]
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        # Should show "—" for bodies
        assert "—" in result

    def test_single_panel(self, config):
        diff = MagicMock()
        diff.panelGeometry = [{"area": 5.5, "objectName": "Barge"}]
        diff.OutputPanelPressures = False
        result = build_panel_pressures(diff, config)
        assert "Barge" in result
        assert "5.5" in result


# ---------------------------------------------------------------------------
# _pressure_note
# ---------------------------------------------------------------------------


class TestPressureNote:

    def test_enabled_shows_success_badge(self):
        diff = MagicMock()
        diff.OutputPanelPressures = True
        result = _pressure_note(diff)
        assert "bg-success" in result
        assert "enabled" in result

    def test_disabled_shows_secondary_badge(self):
        diff = MagicMock()
        diff.OutputPanelPressures = False
        result = _pressure_note(diff)
        assert "bg-secondary" in result
        assert "not output" in result

    def test_missing_attribute_shows_not_output(self):
        diff = MagicMock(spec=[])
        # spec=[] ensures OutputPanelPressures raises AttributeError
        result = _pressure_note(diff)
        assert "bg-secondary" in result

    def test_error_shows_not_output(self):
        diff = MagicMock()
        type(diff).OutputPanelPressures = PropertyMock(side_effect=RuntimeError)
        result = _pressure_note(diff)
        assert "bg-secondary" in result
