# ABOUTME: Light test for the FFS field dashboard data builder — verifies every
# ABOUTME: scenario carries a valid verdict + sufficiency_status and the demo coverage.
"""Tests for :mod:`examples.demos.asset_integrity.ffs_field_dashboard`."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

_MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "examples" / "demos" / "asset_integrity" / "ffs_field_dashboard.py"
)

VERDICTS = {"ACCEPT", "MONITOR", "RE_RATE", "REPAIR", "REPLACE"}
SUFFICIENCY = {"SUFFICIENT", "TAKE_MORE", "ESCALATE"}


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "ffs_field_dashboard_demo", _MODULE_PATH
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def dashboard_data():
    return _load_module().build_dashboard_data()


def test_returns_at_least_four_scenarios(dashboard_data):
    scenarios = dashboard_data["scenarios"]
    assert len(scenarios) >= 4


def test_each_scenario_has_valid_verdict_and_sufficiency(dashboard_data):
    for s in dashboard_data["scenarios"]:
        assert s["verdict"] in VERDICTS, s
        assert s["sufficiency_status"] in SUFFICIENCY, s
        # the heatmap inputs must be present and shaped consistently.
        assert len(s["grid"]) == s["n_rows"]
        assert len(s["loss_pct"]) == s["n_rows"]
        assert all(len(row) == s["n_cols"] for row in s["grid"])


def test_at_least_one_take_more(dashboard_data):
    statuses = [s["sufficiency_status"] for s in dashboard_data["scenarios"]]
    assert "TAKE_MORE" in statuses


def test_at_least_one_not_accept(dashboard_data):
    verdicts = [s["verdict"] for s in dashboard_data["scenarios"]]
    assert any(v != "ACCEPT" for v in verdicts)
