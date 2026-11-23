"""Tests for the shared diffraction validation helper (#611 / #625 Phase-1).

Covers the five run-contract verdicts produced by ``run_validation``:
PASS, WARNING, FAIL, ERROR, SKIPPED — plus issue flattening, canonical
verdict-string preservation (no WARN aliasing), and JSON report export.

All tests are CI-safe: no solver, no OrcFxAPI, only synthetic
DiffractionResults fixtures.
"""

from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)
from digitalmodel.hydrodynamics.diffraction.validation_runner import (
    ALL_VERDICTS,
    ValidationOutcome,
    flatten_issues,
    run_validation,
)


# ---------------------------------------------------------------------------
# Verdict tests
# ---------------------------------------------------------------------------


class TestVerdicts:
    """run_validation maps results to the correct run-contract verdict."""

    def test_pass(self, dense_diffraction_results: DiffractionResults, tmp_path: Path):
        outcome = run_validation(dense_diffraction_results, tmp_path, "dense")
        assert outcome.verdict == "PASS"
        assert outcome.issues == []
        assert outcome.report is not None
        assert outcome.report["overall_status"] == "PASS"

    def test_warning(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        # The sparse conftest fixture (10 freq, 5 head, 0..180) yields WARNING.
        outcome = run_validation(mock_diffraction_results, tmp_path, "sparse")
        assert outcome.verdict == "WARNING"
        assert outcome.issues  # at least one coverage warning
        # Canonical string preserved: never normalized to WARN.
        assert outcome.verdict != "WARN"
        assert outcome.report["overall_status"] == "WARNING"

    def test_fail(
        self, fail_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        outcome = run_validation(fail_diffraction_results, tmp_path, "fail")
        assert outcome.verdict == "FAIL"
        assert any("egative" in issue for issue in outcome.issues)

    def test_error(
        self, dense_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        # Force the underlying validator to raise -> ERROR verdict.
        with patch(
            "digitalmodel.hydrodynamics.diffraction.validation_runner.validate_results",
            side_effect=RuntimeError("boom"),
        ):
            outcome = run_validation(dense_diffraction_results, tmp_path, "err")
        assert outcome.verdict == "ERROR"
        assert outcome.reason == "boom"
        assert outcome.report is None

    def test_skipped_none_results(self, tmp_path: Path):
        outcome = run_validation(None, tmp_path, "none")
        assert outcome.verdict == "SKIPPED"
        assert outcome.reason

    def test_skipped_disabled(
        self, dense_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        outcome = run_validation(
            dense_diffraction_results, tmp_path, "off", enabled=False
        )
        assert outcome.verdict == "SKIPPED"
        # Disabled must short-circuit before touching the validator / writing.
        assert outcome.report is None
        assert not list(tmp_path.glob("*_validation.json"))

    def test_skipped_custom_reason(self, tmp_path: Path):
        outcome = run_validation(
            None, tmp_path, "x", skip_reason="dry-run, no results"
        )
        assert outcome.verdict == "SKIPPED"
        assert outcome.reason == "dry-run, no results"

    def test_all_emitted_verdicts_are_canonical(
        self,
        dense_diffraction_results: DiffractionResults,
        mock_diffraction_results: DiffractionResults,
        fail_diffraction_results: DiffractionResults,
        tmp_path: Path,
    ):
        verdicts = {
            run_validation(dense_diffraction_results, tmp_path, "p").verdict,
            run_validation(mock_diffraction_results, tmp_path, "w").verdict,
            run_validation(fail_diffraction_results, tmp_path, "f").verdict,
            run_validation(None, tmp_path, "s").verdict,
        }
        assert verdicts <= ALL_VERDICTS


# ---------------------------------------------------------------------------
# Report export
# ---------------------------------------------------------------------------


class TestReportExport:
    def test_writes_json_report(
        self, dense_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        outcome = run_validation(dense_diffraction_results, tmp_path, "vessel")
        assert outcome.report_path == tmp_path / "vessel_validation.json"
        assert outcome.report_path.exists()
        data = json.loads(outcome.report_path.read_text())
        assert data["overall_status"] == "PASS"

    def test_no_output_dir_skips_file(
        self, dense_diffraction_results: DiffractionResults
    ):
        outcome = run_validation(dense_diffraction_results, None, "vessel")
        assert outcome.report_path is None
        assert outcome.verdict == "PASS"  # in-memory report still computed


# ---------------------------------------------------------------------------
# Issue flattening
# ---------------------------------------------------------------------------


class TestFlattenIssues:
    def test_flattens_nested_dict(self):
        report = {
            "vessel_name": "V",  # scalar -> ignored
            "overall_status": "WARNING",  # scalar -> ignored
            "physical_validity": {
                "added_mass": ["Negative diagonal term at 0.05"],
                "damping": [],
            },
            "frequency_coverage": {"coverage": ["Min freq too high"]},
            "top_level_list": ["loose issue"],
        }
        issues = flatten_issues(report)
        assert "physical_validity.added_mass: Negative diagonal term at 0.05" in issues
        assert "frequency_coverage.coverage: Min freq too high" in issues
        assert "top_level_list: loose issue" in issues
        # Empty sublists contribute nothing.
        assert all("damping" not in i for i in issues)

    def test_empty_report(self):
        assert flatten_issues({"overall_status": "PASS"}) == []


# ---------------------------------------------------------------------------
# Dataclass defaults
# ---------------------------------------------------------------------------


class TestValidationOutcome:
    def test_defaults(self):
        outcome = ValidationOutcome()
        assert outcome.verdict == "SKIPPED"
        assert outcome.report is None
        assert outcome.report_path is None
        assert outcome.issues == []
        assert outcome.reason is None
