"""Tests for reporting/sections/qa_summary.py — build_qa_summary()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import QASummaryConfig
from digitalmodel.orcawave.reporting.sections.qa_summary import (
    build_qa_summary,
    _run_checks,
    _render_table,
)


@pytest.fixture
def config():
    return QASummaryConfig()


# ---------------------------------------------------------------------------
# build_qa_summary — normal case
# ---------------------------------------------------------------------------


class TestQASummaryNormal:

    def test_returns_html_string(self, mock_diff_single_body, config):
        result = build_qa_summary(mock_diff_single_body, config)
        assert isinstance(result, str)
        assert "<table" in result

    def test_contains_card_structure(self, mock_diff_single_body, config):
        result = build_qa_summary(mock_diff_single_body, config)
        assert "card" in result
        assert "QA Summary" in result

    def test_contains_badge(self, mock_diff_single_body, config):
        result = build_qa_summary(mock_diff_single_body, config)
        assert "badge" in result
        assert "passed" in result

    def test_contains_check_names(self, mock_diff_single_body, config):
        result = build_qa_summary(mock_diff_single_body, config)
        assert "RAOs finite" in result
        assert "Added mass finite" in result
        assert "Damping diagonal" in result
        assert "Heave quasi-static" in result

    def test_pass_fail_labels(self, mock_diff_single_body, config):
        result = build_qa_summary(mock_diff_single_body, config)
        assert "PASS" in result or "FAIL" in result


# ---------------------------------------------------------------------------
# build_qa_summary — all checks pass
# ---------------------------------------------------------------------------


class TestQASummaryAllPass:

    def test_all_pass_shows_green_badge(self, config):
        diff = MagicMock()
        n_freqs = 5
        n_dofs = 6

        freqs_hz = np.array([0.5, 0.4, 0.3, 0.2, 0.1])
        diff.frequencies = freqs_hz
        diff.headings = [0.0, 90.0, 180.0]

        # All finite RAOs
        raos = np.ones((3, n_freqs, n_dofs), dtype=complex)
        # Make heave = 1.0 at longest period for head seas (180 deg = index 2)
        diff.displacementRAOs = raos

        # Finite added mass
        diff.addedMass = np.ones((n_freqs, n_dofs, n_dofs)) * 1e5

        # Non-negative diagonal damping
        dm = np.ones((n_freqs, n_dofs, n_dofs)) * 100.0
        diff.damping = dm

        result = build_qa_summary(diff, config)
        assert "bg-success" in result
        assert "4/4 passed" in result


# ---------------------------------------------------------------------------
# build_qa_summary — some checks fail
# ---------------------------------------------------------------------------


class TestQASummaryFailures:

    def test_nan_raos_fails_check(self, config):
        diff = MagicMock()
        freqs = np.array([0.2, 0.1])
        diff.frequencies = freqs
        diff.headings = [180.0]

        raos = np.ones((1, 2, 6), dtype=complex)
        raos[0, 0, 0] = float("nan")
        diff.displacementRAOs = raos
        diff.addedMass = np.ones((2, 6, 6))
        diff.damping = np.ones((2, 6, 6))

        result = build_qa_summary(diff, config)
        assert "FAIL" in result
        assert "bg-danger" in result

    def test_negative_damping_fails(self, config):
        diff = MagicMock()
        freqs = np.array([0.2, 0.1])
        diff.frequencies = freqs
        diff.headings = [180.0]

        raos = np.ones((1, 2, 6), dtype=complex)
        diff.displacementRAOs = raos
        diff.addedMass = np.ones((2, 6, 6))

        dm = np.ones((2, 6, 6))
        dm[0, 0, 0] = -1.0  # negative diagonal
        diff.damping = dm

        result = build_qa_summary(diff, config)
        assert "FAIL" in result

    def test_heave_quasi_static_fails_when_not_near_one(self, config):
        diff = MagicMock()
        freqs = np.array([0.2, 0.1])
        diff.frequencies = freqs
        diff.headings = [180.0]

        raos = np.ones((1, 2, 6), dtype=complex)
        # The check sorts ascending: [0.1, 0.2] -> sort_idx=[1,0]
        # Then takes raos_s[head_idx, last_valid_idx, 2] = raos_s[0, 1, 2]
        # raos_s[0,1,:] = raos[0,0,:] (original index 0, freq=0.2)
        # So we set raos[0, 0, 2] to make heave fail
        raos[0, 0, 2] = 0.5 + 0j  # heave far from 1.0
        diff.displacementRAOs = raos
        diff.addedMass = np.ones((2, 6, 6))
        diff.damping = np.ones((2, 6, 6))

        result = build_qa_summary(diff, config)
        assert "FAIL" in result


# ---------------------------------------------------------------------------
# build_qa_summary — data errors
# ---------------------------------------------------------------------------


class TestQASummaryDataErrors:

    def test_all_data_unavailable(self, config):
        """When all data attributes fail, checks should all report FAIL."""
        diff = MagicMock()
        type(diff).displacementRAOs = PropertyMock(side_effect=RuntimeError)
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError)
        type(diff).damping = PropertyMock(side_effect=RuntimeError)
        type(diff).frequencies = PropertyMock(side_effect=RuntimeError)
        type(diff).headings = PropertyMock(side_effect=RuntimeError)

        result = build_qa_summary(diff, config)
        assert "0/4 passed" in result or "bg-danger" in result

    def test_partial_data_error(self, config):
        """When some data is available and some not, partial checks run."""
        diff = MagicMock()
        freqs = np.array([0.2, 0.1])
        diff.frequencies = freqs
        diff.headings = [180.0]

        raos = np.ones((1, 2, 6), dtype=complex)
        diff.displacementRAOs = raos

        # Added mass unavailable
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError("no AM"))
        type(diff).damping = PropertyMock(side_effect=RuntimeError("no DM"))

        result = build_qa_summary(diff, config)
        # Should still produce valid HTML
        assert "table" in result
        assert "PASS" in result  # RAOs finite should pass
        assert "FAIL" in result  # AM and DM should fail


# ---------------------------------------------------------------------------
# _run_checks
# ---------------------------------------------------------------------------


class TestRunChecks:

    def test_returns_list_of_dicts(self, mock_diff_single_body):
        checks = _run_checks(mock_diff_single_body)
        assert isinstance(checks, list)
        assert len(checks) == 4
        for c in checks:
            assert "name" in c
            assert "pass" in c
            assert "value" in c

    def test_check_names(self, mock_diff_single_body):
        checks = _run_checks(mock_diff_single_body)
        names = [c["name"] for c in checks]
        assert "RAOs finite" in names
        assert "Added mass finite" in names
        assert "Damping diagonal ≥ 0" in names
        assert "Heave quasi-static" in names

    def test_pass_values_are_bools(self, mock_diff_single_body):
        checks = _run_checks(mock_diff_single_body)
        for c in checks:
            assert isinstance(c["pass"], bool)


# ---------------------------------------------------------------------------
# _render_table
# ---------------------------------------------------------------------------


class TestRenderTable:

    def test_empty_checks(self):
        result = _render_table([])
        assert "<table" in result
        assert "<tbody>" in result

    def test_pass_row_has_success_class(self):
        checks = [{"name": "Test", "pass": True, "value": "OK"}]
        result = _render_table(checks)
        assert "table-success" in result
        assert "PASS" in result

    def test_fail_row_has_danger_class(self):
        checks = [{"name": "Test", "pass": False, "value": "Bad"}]
        result = _render_table(checks)
        assert "table-danger" in result
        assert "FAIL" in result

    def test_multiple_checks(self):
        checks = [
            {"name": "A", "pass": True, "value": "1"},
            {"name": "B", "pass": False, "value": "2"},
        ]
        result = _render_table(checks)
        assert "A" in result
        assert "B" in result
        assert "table-success" in result
        assert "table-danger" in result

    def test_contains_bootstrap_structure(self):
        checks = [{"name": "X", "pass": True, "value": "Y"}]
        result = _render_table(checks)
        assert "table-responsive" in result
        assert "table-bordered" in result
        assert "table-light" in result
        assert "Check" in result
        assert "Result" in result
        assert "Value" in result
