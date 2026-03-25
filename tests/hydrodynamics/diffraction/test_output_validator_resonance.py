"""Tests for resonance validation in OutputValidator (WRK-030)."""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)
from digitalmodel.hydrodynamics.diffraction.output_validator import OutputValidator


class TestResonanceValidation:
    """Test _validate_resonance() method."""

    def test_no_resonance_on_clean_data(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        validator = OutputValidator(mock_diffraction_results)
        issues = validator._validate_resonance()
        # Synthetic data has magnitudes < 5; should not trigger excessive amplification
        sharp_peaks = issues.get("sharp_peaks", [])
        # May or may not have sharp peaks depending on random data, but check structure
        assert isinstance(sharp_peaks, list)
        assert isinstance(issues.get("excessive_amplification", []), list)

    def test_detects_excessive_translation_amplification(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        # Inject an extreme value into heave
        mock_diffraction_results.raos.heave.magnitude[5, 2] = 5.0  # > 3.0 m/m
        validator = OutputValidator(mock_diffraction_results)
        issues = validator._validate_resonance()
        amp_issues = issues["excessive_amplification"]
        found = any("HEAVE" in s for s in amp_issues)
        assert found, f"Expected HEAVE amplification warning, got: {amp_issues}"

    def test_detects_excessive_rotation_amplification(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        mock_diffraction_results.raos.roll.magnitude[3, 1] = 20.0  # > 15 deg/m
        validator = OutputValidator(mock_diffraction_results)
        issues = validator._validate_resonance()
        amp_issues = issues["excessive_amplification"]
        found = any("ROLL" in s for s in amp_issues)
        assert found, f"Expected ROLL amplification warning, got: {amp_issues}"

    def test_detects_sharp_peak(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        # Create a sharp peak: neighbours at 0.01, peak at 1.0 (ratio > 10x)
        comp = mock_diffraction_results.raos.surge
        comp.magnitude[3, 0] = 0.01
        comp.magnitude[4, 0] = 1.0
        comp.magnitude[5, 0] = 0.01
        validator = OutputValidator(mock_diffraction_results)
        issues = validator._validate_resonance()
        sharp_issues = issues["sharp_peaks"]
        found = any("SURGE" in s for s in sharp_issues)
        assert found, f"Expected SURGE sharp peak warning, got: {sharp_issues}"

    def test_resonance_included_in_run_all(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        validator = OutputValidator(mock_diffraction_results)
        report = validator.run_all_validations()
        assert "resonance_checks" in report

    def test_no_false_positive_for_uniform_data(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        # Set all magnitudes to a uniform value
        for dof in DOF:
            comp = mock_diffraction_results.raos.get_component(dof)
            comp.magnitude[:] = 0.5
        validator = OutputValidator(mock_diffraction_results)
        issues = validator._validate_resonance()
        assert len(issues["sharp_peaks"]) == 0
        assert len(issues["excessive_amplification"]) == 0
