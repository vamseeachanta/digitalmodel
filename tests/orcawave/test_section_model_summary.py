"""Tests for reporting/sections/model_summary.py — build_model_summary()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock
import math

import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import ModelSummaryConfig
from digitalmodel.orcawave.reporting.sections.model_summary import (
    build_model_summary,
    _infer_body_count,
    _safe_get,
    _render_table,
)


@pytest.fixture
def config():
    return ModelSummaryConfig()


# ---------------------------------------------------------------------------
# build_model_summary — normal cases
# ---------------------------------------------------------------------------


class TestBuildModelSummary:

    def test_returns_html_string(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert isinstance(result, str)
        assert "<table" in result

    def test_contains_body_count(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "Number of bodies" in result

    def test_single_body_count(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        # Single body: addedMass shape (5, 6, 6) -> 6//6 = 1
        assert ">1<" in result

    def test_two_body_count(self, mock_diff_two_body, config):
        result = build_model_summary(mock_diff_two_body, config)
        # Two body: addedMass shape (4, 12, 12) -> 12//6 = 2
        assert ">2<" in result

    def test_contains_frequency_count(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "Number of periods" in result
        assert ">5<" in result  # 5 frequencies

    def test_contains_period_range(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        # freq range 0.1 to 0.5 Hz -> period 2.0 to 10.0 s
        assert "2.00" in result
        assert "10.00" in result

    def test_contains_heading_count(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "Number of headings" in result
        assert ">3<" in result  # 3 headings

    def test_contains_heading_range(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "0.0" in result
        assert "180.0" in result

    def test_contains_water_depth(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "Water depth" in result
        assert "100.0 m" in result

    def test_contains_water_density(self, mock_diff_single_body, config):
        result = build_model_summary(mock_diff_single_body, config)
        assert "Water density" in result
        assert "1025.0" in result

    def test_infinite_water_depth(self, config):
        diff = MagicMock()
        diff.frequencies = [0.1, 0.2]
        diff.headings = [0.0]
        diff.addedMass = np.zeros((2, 6, 6))
        diff.WaterDepth = float("inf")
        diff.WaterDensity = 1025.0
        result = build_model_summary(diff, config)
        assert "Infinite" in result


# ---------------------------------------------------------------------------
# build_model_summary — edge cases
# ---------------------------------------------------------------------------


class TestBuildModelSummaryEdgeCases:

    def test_zero_frequencies(self, config):
        diff = MagicMock()
        diff.frequencies = []
        diff.headings = [0.0]
        diff.addedMass = np.zeros((0, 6, 6))
        diff.WaterDepth = 50.0
        diff.WaterDensity = 1025.0
        result = build_model_summary(diff, config)
        assert "0" in result  # "Number of periods / frequencies: 0"

    def test_zero_headings(self, config):
        diff = MagicMock()
        diff.frequencies = [0.1]
        diff.headings = []
        diff.addedMass = np.zeros((1, 6, 6))
        diff.WaterDepth = 50.0
        diff.WaterDensity = 1025.0
        result = build_model_summary(diff, config)
        assert "Number of headings" in result

    def test_missing_water_depth_attribute(self, config):
        """When WaterDepth attribute is missing, no crash."""
        diff = MagicMock()
        diff.frequencies = [0.1]
        diff.headings = [0.0]
        diff.addedMass = np.zeros((1, 6, 6))
        # Make WaterDepth raise AttributeError
        type(diff).WaterDepth = PropertyMock(side_effect=AttributeError)
        diff.WaterDensity = 1025.0
        result = build_model_summary(diff, config)
        assert "Water depth" not in result  # Should be omitted gracefully

    def test_missing_water_density_attribute(self, config):
        diff = MagicMock()
        diff.frequencies = [0.1]
        diff.headings = [0.0]
        diff.addedMass = np.zeros((1, 6, 6))
        diff.WaterDepth = 50.0
        type(diff).WaterDensity = PropertyMock(side_effect=AttributeError)
        result = build_model_summary(diff, config)
        assert "Water density" not in result


# ---------------------------------------------------------------------------
# _infer_body_count
# ---------------------------------------------------------------------------


class TestInferBodyCount:

    def test_single_body(self):
        diff = MagicMock()
        diff.addedMass = np.zeros((5, 6, 6))
        assert _infer_body_count(diff) == 1

    def test_two_bodies(self):
        diff = MagicMock()
        diff.addedMass = np.zeros((5, 12, 12))
        assert _infer_body_count(diff) == 2

    def test_three_bodies(self):
        diff = MagicMock()
        diff.addedMass = np.zeros((5, 18, 18))
        assert _infer_body_count(diff) == 3

    def test_error_returns_one(self):
        diff = MagicMock()
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError("no data"))
        assert _infer_body_count(diff) == 1


# ---------------------------------------------------------------------------
# _safe_get
# ---------------------------------------------------------------------------


class TestSafeGet:

    def test_existing_attribute(self):
        obj = MagicMock()
        obj.foo = 42
        assert _safe_get(obj, "foo", None) == 42

    def test_missing_attribute_returns_default(self):
        obj = MagicMock(spec=[])
        assert _safe_get(obj, "nonexistent", "default") == "default"

    def test_exception_returns_default(self):
        obj = MagicMock()
        type(obj).bar = PropertyMock(side_effect=RuntimeError)
        assert _safe_get(obj, "bar", -1) == -1


# ---------------------------------------------------------------------------
# _render_table
# ---------------------------------------------------------------------------


class TestRenderTable:

    def test_empty_rows(self):
        html = _render_table([])
        assert "<table" in html
        assert "<tbody>" in html

    def test_single_row(self):
        html = _render_table([("Label", "Value")])
        assert "Label" in html
        assert "Value" in html
        assert "<tr>" in html

    def test_multiple_rows(self):
        rows = [("A", "1"), ("B", "2"), ("C", "3")]
        html = _render_table(rows)
        for label, value in rows:
            assert label in html
            assert value in html

    def test_bootstrap_classes(self):
        html = _render_table([("X", "Y")])
        assert "table-responsive" in html
        assert "table-bordered" in html
        assert "table-light" in html
