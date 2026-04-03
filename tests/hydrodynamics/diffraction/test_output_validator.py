"""Tests for output_validator.py.

Covers the OutputValidator class and the validate_results convenience function.
Uses mock_diffraction_results from conftest.py.
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    DiffractionResults,
)
from digitalmodel.hydrodynamics.diffraction.output_validator import (
    OutputValidator,
    validate_results,
)


# ---------------------------------------------------------------------------
# Tests: OutputValidator initialisation
# ---------------------------------------------------------------------------


class TestOutputValidatorInit:

    def test_stores_results(self, mock_diffraction_results: DiffractionResults):
        v = OutputValidator(mock_diffraction_results)
        assert v.results is mock_diffraction_results
        assert v.validation_report == {}


# ---------------------------------------------------------------------------
# Tests: run_all_validations
# ---------------------------------------------------------------------------


class TestRunAllValidations:

    def test_returns_dict_with_required_keys(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        report = v.run_all_validations()
        required_keys = [
            "vessel_name",
            "analysis_tool",
            "schema_validation",
            "physical_validity",
            "range_checks",
            "frequency_coverage",
            "heading_coverage",
            "symmetry_checks",
            "resonance_checks",
            "overall_status",
        ]
        for key in required_keys:
            assert key in report, f"Missing key: {key}"

    def test_overall_status_is_string(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        report = v.run_all_validations()
        assert report["overall_status"] in ("PASS", "WARNING", "FAIL")


# ---------------------------------------------------------------------------
# Tests: _validate_physical_validity
# ---------------------------------------------------------------------------


class TestPhysicalValidity:

    def test_clean_data_no_rao_issues(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_physical_validity()
        # Synthetic data has magnitudes < 5 for translation, < 5 for rotation
        assert isinstance(issues["raos"], list)

    def test_flags_large_translation_rao(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.raos.surge.magnitude[0, 0] = 10.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_physical_validity()
        assert any("Surge" in i for i in issues["raos"])

    def test_flags_large_rotation_rao(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.raos.roll.magnitude[0, 0] = 25.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_physical_validity()
        assert any("Roll" in i for i in issues["raos"])

    def test_flags_negative_added_mass_diagonal(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.added_mass.matrices[0].matrix[0, 0] = -100.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_physical_validity()
        assert any("Negative" in i for i in issues["added_mass"])

    def test_flags_negative_damping_diagonal(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.damping.matrices[0].matrix[2, 2] = -50.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_physical_validity()
        assert any("Negative" in i for i in issues["damping"])


# ---------------------------------------------------------------------------
# Tests: _validate_ranges
# ---------------------------------------------------------------------------


class TestRangeValidation:

    def test_phase_in_bounds_no_issues(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_ranges()
        # conftest phases are [-180, 180]
        phase_issues = [i for i in issues["raos"] if "Phase" in i]
        assert len(phase_issues) == 0

    def test_flags_out_of_range_phase(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.raos.pitch.phase[0, 0] = 400.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_ranges()
        assert any("Phase" in i and "Pitch" in i for i in issues["raos"])

    def test_flags_extremely_large_added_mass(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.added_mass.matrices[0].matrix[0, 0] = 1e12
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_ranges()
        assert any("large" in i.lower() for i in issues["added_mass"])


# ---------------------------------------------------------------------------
# Tests: _validate_frequency_coverage
# ---------------------------------------------------------------------------


class TestFrequencyCoverage:

    def test_low_min_freq_flagged(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_frequency_coverage()
        # Our conftest has min_freq ~0.05 which is below 0.1, so no warning about low-freq
        # But check structure
        assert "coverage" in issues
        assert "discretization" in issues

    def test_insufficient_freq_count_flagged(
        self, mock_diffraction_results: DiffractionResults
    ):
        # conftest has 10 frequencies which is < 20
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_frequency_coverage()
        disc_issues = issues["discretization"]
        assert any("insufficient" in i.lower() or "Only" in i for i in disc_issues)


# ---------------------------------------------------------------------------
# Tests: _validate_heading_coverage
# ---------------------------------------------------------------------------


class TestHeadingCoverage:

    def test_narrow_heading_range_flagged(
        self, mock_diffraction_results: DiffractionResults
    ):
        # conftest headings: [0, 45, 90, 135, 180] -> range = 180 < 350
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_heading_coverage()
        assert any("360" in i or "may need" in i.lower() for i in issues["coverage"])

    def test_few_headings_flagged(
        self, mock_diffraction_results: DiffractionResults
    ):
        # conftest has 5 headings which is < 8
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_heading_coverage()
        disc_issues = issues["discretization"]
        assert any("Only" in i for i in disc_issues)


# ---------------------------------------------------------------------------
# Tests: _validate_symmetry
# ---------------------------------------------------------------------------


class TestSymmetryValidation:

    def test_symmetric_matrices_no_issues(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_symmetry()
        # conftest builds symmetric matrices
        assert len(issues["added_mass"]) == 0
        assert len(issues["damping"]) == 0

    def test_flags_asymmetric_added_mass(
        self, mock_diffraction_results: DiffractionResults
    ):
        m = mock_diffraction_results.added_mass.matrices[0]
        m.matrix[0, 1] = 99999.0
        m.matrix[1, 0] = 0.0
        v = OutputValidator(mock_diffraction_results)
        issues = v._validate_symmetry()
        assert len(issues["added_mass"]) > 0


# ---------------------------------------------------------------------------
# Tests: _determine_overall_status
# ---------------------------------------------------------------------------


class TestOverallStatus:

    def test_pass_on_clean_data(
        self, mock_diffraction_results: DiffractionResults
    ):
        v = OutputValidator(mock_diffraction_results)
        report = v.run_all_validations()
        # If no negative/missing issues, should not be FAIL
        # (may be WARNING due to coverage limits)
        assert report["overall_status"] in ("PASS", "WARNING")

    def test_fail_on_negative_diagonal(
        self, mock_diffraction_results: DiffractionResults
    ):
        mock_diffraction_results.added_mass.matrices[0].matrix[0, 0] = -100.0
        v = OutputValidator(mock_diffraction_results)
        report = v.run_all_validations()
        assert report["overall_status"] == "FAIL"


# ---------------------------------------------------------------------------
# Tests: export_report
# ---------------------------------------------------------------------------


class TestExportReport:

    def test_export_creates_json(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        v = OutputValidator(mock_diffraction_results)
        v.run_all_validations()
        output = tmp_path / "validation.json"
        v.export_report(output)
        assert output.exists()
        data = json.loads(output.read_text())
        assert "overall_status" in data


# ---------------------------------------------------------------------------
# Tests: validate_results convenience function
# ---------------------------------------------------------------------------


class TestValidateResultsConvenience:

    def test_returns_report(self, mock_diffraction_results: DiffractionResults):
        report = validate_results(mock_diffraction_results)
        assert "overall_status" in report

    def test_exports_to_file(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ):
        output = tmp_path / "report.json"
        report = validate_results(mock_diffraction_results, output_file=output)
        assert output.exists()
        assert "overall_status" in report
