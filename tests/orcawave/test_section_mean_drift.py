"""Tests for reporting/sections/mean_drift.py — build_mean_drift()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import MeanDriftConfig
from digitalmodel.orcawave.reporting.sections.mean_drift import (
    build_mean_drift,
    _DRIFT_ATTRS,
)


@pytest.fixture
def config_with_polar():
    return MeanDriftConfig(include_polar=True)


@pytest.fixture
def config_no_polar():
    return MeanDriftConfig(include_polar=False)


def _make_diff_with_drift(attr_name="meanDriftLoadControlSurface"):
    """Create a mock diff object with mean drift data on the specified attribute."""
    diff = MagicMock()
    n_headings = 3
    n_freqs = 5
    n_dofs = 6

    diff.frequencies = np.array([0.5, 0.4, 0.3, 0.2, 0.1])
    diff.headings = [0.0, 90.0, 180.0]

    rng = np.random.RandomState(42)
    drift = rng.rand(n_headings, n_freqs, n_dofs) * 100
    setattr(diff, attr_name, drift)

    # Make other drift attr unavailable
    other_attrs = [a for a in _DRIFT_ATTRS if a != attr_name]
    for a in other_attrs:
        setattr(diff, a, None)

    return diff


# ---------------------------------------------------------------------------
# build_mean_drift — data unavailable
# ---------------------------------------------------------------------------


class TestMeanDriftUnavailable:

    def test_returns_alert_when_no_drift(self, config_with_polar):
        diff = MagicMock()
        for attr in _DRIFT_ATTRS:
            setattr(diff, attr, None)
        result = build_mean_drift(diff, config_with_polar)
        assert "alert-info" in result
        assert "not available" in result.lower()

    def test_returns_alert_when_all_attrs_error(self, config_with_polar):
        diff = MagicMock()
        for attr in _DRIFT_ATTRS:
            type(diff).__dict__  # ensure mock exists
            # Make attribute access raise
        diff.meanDriftLoadControlSurface = PropertyMock(side_effect=RuntimeError)
        diff.meanDriftLoadPressureIntegration = PropertyMock(side_effect=RuntimeError)
        # getattr on MagicMock won't use PropertyMock unless on type
        type(diff).meanDriftLoadControlSurface = PropertyMock(side_effect=RuntimeError)
        type(diff).meanDriftLoadPressureIntegration = PropertyMock(side_effect=RuntimeError)
        result = build_mean_drift(diff, config_with_polar)
        assert "not available" in result.lower()


# ---------------------------------------------------------------------------
# build_mean_drift — plotly unavailable
# ---------------------------------------------------------------------------


class TestMeanDriftNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, config_with_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_with_polar)
        assert "Plotly required" in result


# ---------------------------------------------------------------------------
# build_mean_drift — with data and plotly
# ---------------------------------------------------------------------------


class TestMeanDriftWithData:

    def test_returns_html_string(self, config_with_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_with_polar)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_card_structure(self, config_with_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_with_polar)
        assert "card" in result
        assert "card-header" in result
        assert "card-body" in result

    def test_mentions_source_attribute(self, config_with_polar):
        diff = _make_diff_with_drift("meanDriftLoadControlSurface")
        result = build_mean_drift(diff, config_with_polar)
        assert "meanDriftLoadControlSurface" in result

    def test_pressure_integration_fallback(self, config_with_polar):
        """When control surface is None, falls back to pressure integration."""
        diff = _make_diff_with_drift("meanDriftLoadPressureIntegration")
        result = build_mean_drift(diff, config_with_polar)
        assert "meanDriftLoadPressureIntegration" in result

    def test_contains_peak_table(self, config_with_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_with_polar)
        assert "table" in result
        assert "Peak drift" in result
        for dof in ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]:
            assert dof in result

    def test_polar_plot_included(self, config_with_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_with_polar)
        # With polar enabled, result should be longer / contain plotly content
        assert len(result) > 200

    def test_polar_plot_excluded(self, config_no_polar):
        diff = _make_diff_with_drift()
        result_polar = build_mean_drift(diff, MeanDriftConfig(include_polar=True))
        result_no_polar = build_mean_drift(diff, config_no_polar)
        # Without polar, result should be shorter
        assert len(result_no_polar) < len(result_polar)

    def test_contains_period_value(self, config_no_polar):
        diff = _make_diff_with_drift()
        result = build_mean_drift(diff, config_no_polar)
        # Should mention period (T = ...)
        assert "T =" in result


# ---------------------------------------------------------------------------
# Module constants
# ---------------------------------------------------------------------------


class TestDriftConstants:

    def test_drift_attrs_are_strings(self):
        assert all(isinstance(a, str) for a in _DRIFT_ATTRS)

    def test_drift_attrs_not_empty(self):
        assert len(_DRIFT_ATTRS) > 0
