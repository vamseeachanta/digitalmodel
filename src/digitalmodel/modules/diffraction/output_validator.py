#!/usr/bin/env python3
"""
Diffraction Output Validation Module

Comprehensive validation scripts for checking completeness and physical validity
of diffraction analysis results.
"""

import numpy as np
from typing import Dict, List, Tuple
from pathlib import Path
import json

from digitalmodel.modules.diffraction.output_schemas import (
    DiffractionResults, RAOSet, AddedMassSet, DampingSet,
    validate_rao_completeness, validate_matrix_set,
    validate_diffraction_results
)


class OutputValidator:
    """Validate diffraction analysis outputs"""

    def __init__(self, results: DiffractionResults):
        """
        Initialize validator

        Args:
            results: Diffraction results to validate
        """
        self.results = results
        self.validation_report = {}

    def run_all_validations(self) -> Dict:
        """
        Run complete validation suite

        Returns:
            Comprehensive validation report
        """
        print("Running diffraction output validation...")
        print("=" * 70)

        # Basic schema validation
        schema_issues = validate_diffraction_results(self.results)

        # Physical validity checks
        physical_issues = self._validate_physical_validity()

        # Range checks
        range_issues = self._validate_ranges()

        # Frequency coverage checks
        freq_issues = self._validate_frequency_coverage()

        # Heading coverage checks
        heading_issues = self._validate_heading_coverage()

        # Symmetry checks
        symmetry_issues = self._validate_symmetry()

        # Compile comprehensive report
        self.validation_report = {
            'vessel_name': self.results.vessel_name,
            'analysis_tool': self.results.analysis_tool,
            'validation_date': self.results.created_date,
            'schema_validation': schema_issues,
            'physical_validity': physical_issues,
            'range_checks': range_issues,
            'frequency_coverage': freq_issues,
            'heading_coverage': heading_issues,
            'symmetry_checks': symmetry_issues,
            'overall_status': self._determine_overall_status(
                schema_issues, physical_issues, range_issues,
                freq_issues, heading_issues, symmetry_issues
            )
        }

        self._print_validation_summary()

        return self.validation_report

    def _validate_physical_validity(self) -> Dict[str, List[str]]:
        """
        Check physical validity of coefficients

        Returns:
            Dictionary of physical validity issues
        """
        issues = {
            'raos': [],
            'added_mass': [],
            'damping': []
        }

        # RAO physical validity
        raos = self.results.raos

        # Check for unreasonably large RAO values
        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            component = getattr(raos, dof)

            max_mag = np.max(component.magnitude)

            # Translation RAOs typically < 2-3 m/m
            if dof in ['surge', 'sway', 'heave']:
                if max_mag > 5.0:
                    issues['raos'].append(
                        f"{dof.capitalize()}: Maximum RAO magnitude {max_mag:.2f} exceeds typical range"
                    )

            # Rotation RAOs typically < 10-15 deg/m
            else:
                if max_mag > 20.0:
                    issues['raos'].append(
                        f"{dof.capitalize()}: Maximum RAO magnitude {max_mag:.2f} deg/m exceeds typical range"
                    )

        # Added mass physical validity
        # Diagonal terms should be positive
        for matrix in self.results.added_mass.matrices:
            for i in range(6):
                if matrix.matrix[i, i] < 0:
                    issues['added_mass'].append(
                        f"Negative diagonal term at frequency {matrix.frequency:.4f} rad/s, DOF {i+1}"
                    )

        # Damping physical validity
        # Diagonal terms should be non-negative
        for matrix in self.results.damping.matrices:
            for i in range(6):
                if matrix.matrix[i, i] < 0:
                    issues['damping'].append(
                        f"Negative diagonal damping at frequency {matrix.frequency:.4f} rad/s, DOF {i+1}"
                    )

        return issues

    def _validate_ranges(self) -> Dict[str, List[str]]:
        """
        Validate coefficient ranges are reasonable

        Returns:
            Dictionary of range validation issues
        """
        issues = {
            'raos': [],
            'added_mass': [],
            'damping': []
        }

        # Check phase ranges (should be -180 to 180 or 0 to 360)
        raos = self.results.raos

        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            component = getattr(raos, dof)

            min_phase = np.min(component.phase)
            max_phase = np.max(component.phase)

            if min_phase < -180 or max_phase > 360:
                issues['raos'].append(
                    f"{dof.capitalize()}: Phase range [{min_phase:.1f}, {max_phase:.1f}] outside expected bounds"
                )

        # Check for extremely large or small coefficient values
        for matrix in self.results.added_mass.matrices:
            max_val = np.max(np.abs(matrix.matrix))
            if max_val > 1e10:
                issues['added_mass'].append(
                    f"Extremely large coefficient ({max_val:.2e}) at frequency {matrix.frequency:.4f}"
                )

        for matrix in self.results.damping.matrices:
            max_val = np.max(np.abs(matrix.matrix))
            if max_val > 1e10:
                issues['damping'].append(
                    f"Extremely large damping ({max_val:.2e}) at frequency {matrix.frequency:.4f}"
                )

        return issues

    def _validate_frequency_coverage(self) -> Dict[str, List[str]]:
        """
        Validate frequency discretization adequacy

        Returns:
            Frequency coverage issues
        """
        issues = {
            'coverage': [],
            'discretization': []
        }

        freqs = self.results.raos.surge.frequencies

        # Check minimum frequency coverage
        # Typically should cover peak wave periods (0.05 - 0.3 rad/s or 20-120s periods)
        if freqs.min_freq > 0.1:
            issues['coverage'].append(
                f"Minimum frequency {freqs.min_freq:.4f} rad/s may miss low-frequency response (< 0.1 rad/s)"
            )

        # Check maximum frequency
        # Should typically go to at least 2-3 rad/s for deepwater vessels
        if freqs.max_freq < 1.5:
            issues['coverage'].append(
                f"Maximum frequency {freqs.max_freq:.4f} rad/s may miss high-frequency response"
            )

        # Check frequency spacing
        freq_diffs = np.diff(freqs.values)
        max_diff = np.max(freq_diffs)
        min_diff = np.min(freq_diffs)

        if max_diff / min_diff > 5.0:
            issues['discretization'].append(
                f"Inconsistent frequency spacing: ratio {max_diff/min_diff:.2f} suggests non-uniform discretization"
            )

        # Check number of frequencies
        if freqs.count < 20:
            issues['discretization'].append(
                f"Only {freqs.count} frequencies - may be insufficient for accurate interpolation"
            )

        return issues

    def _validate_heading_coverage(self) -> Dict[str, List[str]]:
        """
        Validate heading angle coverage

        Returns:
            Heading coverage issues
        """
        issues = {
            'coverage': [],
            'discretization': []
        }

        heads = self.results.raos.surge.headings

        # Check full circle coverage
        angular_range = heads.max_heading - heads.min_heading

        if angular_range < 350:
            issues['coverage'].append(
                f"Heading range {angular_range:.1f} deg - may need full 360 deg coverage for omnidirectional analysis"
            )

        # Check heading spacing
        if heads.count > 1:
            head_diffs = np.diff(heads.values)
            max_diff = np.max(head_diffs)

            if max_diff > 45:
                issues['discretization'].append(
                    f"Large heading gap ({max_diff:.1f} deg) - may affect interpolation accuracy"
                )

        # Check number of headings
        if heads.count < 8:
            issues['discretization'].append(
                f"Only {heads.count} headings - recommend at least 8 for good directional resolution"
            )

        return issues

    def _validate_symmetry(self) -> Dict[str, List[str]]:
        """
        Check for expected symmetries in results

        Returns:
            Symmetry validation issues
        """
        issues = {
            'added_mass': [],
            'damping': []
        }

        # Check matrix symmetry (should be symmetric for physical validity)
        for matrix in self.results.added_mass.matrices:
            # Compute symmetry error
            symmetry_error = np.max(np.abs(matrix.matrix - matrix.matrix.T))

            if symmetry_error > 0.01 * np.max(np.abs(matrix.matrix)):
                issues['added_mass'].append(
                    f"Matrix at frequency {matrix.frequency:.4f} has significant asymmetry (error: {symmetry_error:.2e})"
                )

        for matrix in self.results.damping.matrices:
            symmetry_error = np.max(np.abs(matrix.matrix - matrix.matrix.T))

            if symmetry_error > 0.01 * np.max(np.abs(matrix.matrix)):
                issues['damping'].append(
                    f"Matrix at frequency {matrix.frequency:.4f} has significant asymmetry (error: {symmetry_error:.2e})"
                )

        return issues

    def _determine_overall_status(self, *issue_dicts) -> str:
        """
        Determine overall validation status

        Returns:
            Overall status: PASS, WARNING, or FAIL
        """
        total_errors = 0
        total_warnings = 0

        for issue_dict in issue_dicts:
            for category, issues in issue_dict.items():
                if isinstance(issues, list):
                    # Critical issues (errors)
                    if 'negative' in str(issues).lower() or 'missing' in str(issues).lower():
                        total_errors += len(issues)
                    else:
                        total_warnings += len(issues)
                elif isinstance(issues, dict):
                    for sub_issues in issues.values():
                        total_warnings += len(sub_issues)

        if total_errors > 0:
            return "FAIL"
        elif total_warnings > 0:
            return "WARNING"
        else:
            return "PASS"

    def _print_validation_summary(self):
        """Print validation summary to console"""
        print("\nValidation Summary")
        print("=" * 70)
        print(f"Overall Status: {self.validation_report['overall_status']}")
        print("=" * 70)

        # Count total issues
        total_issues = 0

        for category, issues in self.validation_report.items():
            if isinstance(issues, dict):
                for subcategory, sub_issues in issues.items():
                    if isinstance(sub_issues, list):
                        count = len(sub_issues)
                        if count > 0:
                            total_issues += count
                            print(f"\n{category}.{subcategory}: {count} issues")
                            for issue in sub_issues[:3]:  # Show first 3
                                print(f"  - {issue}")
                            if count > 3:
                                print(f"  ... and {count - 3} more")

        print("\n" + "=" * 70)
        print(f"Total Issues: {total_issues}")
        print("=" * 70)

    def export_report(self, output_file: Path):
        """
        Export validation report to JSON file

        Args:
            output_file: Path to output JSON file
        """
        with open(output_file, 'w') as f:
            json.dump(self.validation_report, f, indent=2)

        print(f"\nValidation report exported: {output_file}")


# Convenience function

def validate_results(results: DiffractionResults, output_file: Optional[Path] = None) -> Dict:
    """
    Run validation and optionally export report

    Args:
        results: Diffraction results to validate
        output_file: Optional path for JSON report export

    Returns:
        Validation report dictionary
    """
    validator = OutputValidator(results)
    report = validator.run_all_validations()

    if output_file:
        validator.export_report(output_file)

    return report
