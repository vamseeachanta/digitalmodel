#!/usr/bin/env python3
"""
Diffraction Results Comparison Framework

ABOUTME: Statistical comparison framework for analyzing differences between AQWA and OrcaWave diffraction results.

Provides tools for:
- Side-by-side comparison of DiffractionResults
- Statistical deviation analysis
- Frequency-by-frequency comparison
- Heading-by-heading comparison
- Matrix element comparison
- HTML report generation with interactive plots

Version: 3.0.0 (Phase 3)
Status: Benchmark comparison tools
"""

import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, field
from datetime import datetime
import json

from digitalmodel.diffraction.output_schemas import (
    DiffractionResults,
    DOF
)


@dataclass
class DeviationStatistics:
    """Statistics for deviation analysis"""
    mean_error: float
    max_error: float
    rms_error: float
    mean_abs_error: float
    correlation: float
    frequencies: np.ndarray
    errors: np.ndarray


@dataclass
class RAOComparison:
    """RAO comparison results for a single DOF"""
    dof: DOF
    statistics: DeviationStatistics
    magnitude_diff: np.ndarray
    phase_diff: np.ndarray
    max_magnitude_diff_location: Tuple[int, int]  # (freq_idx, heading_idx)
    max_phase_diff_location: Tuple[int, int]


@dataclass
class MatrixComparison:
    """Matrix comparison results"""
    matrix_type: str  # 'added_mass' or 'damping'
    frequencies: np.ndarray
    element_statistics: Dict[Tuple[int, int], DeviationStatistics] = field(default_factory=dict)
    max_deviation_frequency: float = 0.0
    max_deviation_element: Tuple[int, int] = (0, 0)
    max_deviation_value: float = 0.0


@dataclass
class ComparisonReport:
    """Complete comparison report"""
    vessel_name: str
    aqwa_source: str
    orcawave_source: str
    comparison_date: str

    rao_comparisons: Dict[str, RAOComparison] = field(default_factory=dict)
    added_mass_comparison: Optional[MatrixComparison] = None
    damping_comparison: Optional[MatrixComparison] = None

    overall_agreement: str = "UNKNOWN"  # EXCELLENT, GOOD, FAIR, POOR
    notes: List[str] = field(default_factory=list)


class DiffractionComparator:
    """Compare diffraction results from AQWA and OrcaWave"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_results: DiffractionResults,
        tolerance: float = 0.05  # 5% tolerance
    ):
        """
        Initialize comparator

        Args:
            aqwa_results: Results from AQWA converter
            orcawave_results: Results from OrcaWave converter
            tolerance: Relative tolerance for agreement assessment (default: 5%)
        """
        self.aqwa = aqwa_results
        self.orcawave = orcawave_results
        self.tolerance = tolerance

        # Verify same vessel
        if self.aqwa.vessel_name != self.orcawave.vessel_name:
            raise ValueError(
                f"Vessel name mismatch: "
                f"AQWA='{self.aqwa.vessel_name}' vs "
                f"OrcaWave='{self.orcawave.vessel_name}'"
            )

    def _calculate_deviation_stats(
        self,
        values1: np.ndarray,
        values2: np.ndarray,
        frequencies: np.ndarray
    ) -> DeviationStatistics:
        """
        Calculate deviation statistics between two arrays

        Args:
            values1: First set of values
            values2: Second set of values
            frequencies: Frequencies corresponding to values

        Returns:
            Deviation statistics
        """
        # Calculate errors
        errors = values2 - values1

        # Statistics
        mean_error = np.mean(errors)
        max_error = np.max(np.abs(errors))
        rms_error = np.sqrt(np.mean(errors**2))
        mean_abs_error = np.mean(np.abs(errors))

        # Correlation
        correlation = np.corrcoef(values1.flatten(), values2.flatten())[0, 1]

        return DeviationStatistics(
            mean_error=mean_error,
            max_error=max_error,
            rms_error=rms_error,
            mean_abs_error=mean_abs_error,
            correlation=correlation,
            frequencies=frequencies,
            errors=errors
        )

    def compare_raos(self) -> Dict[str, RAOComparison]:
        """
        Compare RAOs for all DOFs

        Returns:
            Dictionary of RAOComparison objects by DOF name
        """
        comparisons = {}

        for dof in DOF:
            dof_name = dof.name.lower()

            # Get RAO components
            aqwa_rao = getattr(self.aqwa.raos, dof_name)
            orcawave_rao = getattr(self.orcawave.raos, dof_name)

            if aqwa_rao is None or orcawave_rao is None:
                continue

            # Ensure same dimensions
            if aqwa_rao.magnitude.shape != orcawave_rao.magnitude.shape:
                print(f"Warning: Shape mismatch for {dof.name} RAOs - skipping")
                continue

            # Magnitude comparison
            mag_diff = orcawave_rao.magnitude - aqwa_rao.magnitude
            max_mag_loc = np.unravel_index(
                np.argmax(np.abs(mag_diff)),
                mag_diff.shape
            )

            # Phase comparison (handle wrap-around)
            phase_diff = orcawave_rao.phase - aqwa_rao.phase
            # Wrap to [-180, 180]
            phase_diff = np.mod(phase_diff + 180, 360) - 180
            max_phase_loc = np.unravel_index(
                np.argmax(np.abs(phase_diff)),
                phase_diff.shape
            )

            # Statistics for magnitude
            stats = self._calculate_deviation_stats(
                aqwa_rao.magnitude,
                orcawave_rao.magnitude,
                aqwa_rao.frequencies.values
            )

            comparison = RAOComparison(
                dof=dof,
                statistics=stats,
                magnitude_diff=mag_diff,
                phase_diff=phase_diff,
                max_magnitude_diff_location=max_mag_loc,
                max_phase_diff_location=max_phase_loc
            )

            comparisons[dof_name] = comparison

        return comparisons

    def compare_added_mass(self) -> MatrixComparison:
        """
        Compare added mass matrices

        Returns:
            MatrixComparison object
        """
        comparison = MatrixComparison(
            matrix_type='added_mass',
            frequencies=self.aqwa.added_mass.frequencies.values
        )

        # Ensure same number of frequencies
        if len(self.aqwa.added_mass.matrices) != len(self.orcawave.added_mass.matrices):
            raise ValueError("Frequency count mismatch in added mass matrices")

        max_dev = 0.0
        max_dev_freq = 0.0
        max_dev_elem = (0, 0)

        # Compare each matrix element across frequencies
        for i in range(6):
            for j in range(6):
                aqwa_values = np.array([
                    m.matrix[i, j] for m in self.aqwa.added_mass.matrices
                ])
                orcawave_values = np.array([
                    m.matrix[i, j] for m in self.orcawave.added_mass.matrices
                ])

                stats = self._calculate_deviation_stats(
                    aqwa_values,
                    orcawave_values,
                    comparison.frequencies
                )

                comparison.element_statistics[(i, j)] = stats

                # Track maximum deviation
                if stats.max_error > max_dev:
                    max_dev = stats.max_error
                    max_dev_freq = comparison.frequencies[np.argmax(np.abs(stats.errors))]
                    max_dev_elem = (i, j)

        comparison.max_deviation_value = max_dev
        comparison.max_deviation_frequency = max_dev_freq
        comparison.max_deviation_element = max_dev_elem

        return comparison

    def compare_damping(self) -> MatrixComparison:
        """
        Compare damping matrices

        Returns:
            MatrixComparison object
        """
        comparison = MatrixComparison(
            matrix_type='damping',
            frequencies=self.aqwa.damping.frequencies.values
        )

        # Ensure same number of frequencies
        if len(self.aqwa.damping.matrices) != len(self.orcawave.damping.matrices):
            raise ValueError("Frequency count mismatch in damping matrices")

        max_dev = 0.0
        max_dev_freq = 0.0
        max_dev_elem = (0, 0)

        # Compare each matrix element across frequencies
        for i in range(6):
            for j in range(6):
                aqwa_values = np.array([
                    m.matrix[i, j] for m in self.aqwa.damping.matrices
                ])
                orcawave_values = np.array([
                    m.matrix[i, j] for m in self.orcawave.damping.matrices
                ])

                stats = self._calculate_deviation_stats(
                    aqwa_values,
                    orcawave_values,
                    comparison.frequencies
                )

                comparison.element_statistics[(i, j)] = stats

                # Track maximum deviation
                if stats.max_error > max_dev:
                    max_dev = stats.max_error
                    max_dev_freq = comparison.frequencies[np.argmax(np.abs(stats.errors))]
                    max_dev_elem = (i, j)

        comparison.max_deviation_value = max_dev
        comparison.max_deviation_frequency = max_dev_freq
        comparison.max_deviation_element = max_dev_elem

        return comparison

    def generate_report(self) -> ComparisonReport:
        """
        Generate complete comparison report

        Returns:
            ComparisonReport object
        """
        print("Comparing AQWA vs OrcaWave diffraction results...")
        print("=" * 80)

        # RAO comparisons
        print("Comparing RAOs...")
        rao_comparisons = self.compare_raos()

        # Matrix comparisons
        print("Comparing added mass matrices...")
        am_comparison = self.compare_added_mass()

        print("Comparing damping matrices...")
        damp_comparison = self.compare_damping()

        # Create report
        report = ComparisonReport(
            vessel_name=self.aqwa.vessel_name,
            aqwa_source=self.aqwa.source_files[0] if self.aqwa.source_files else "Unknown",
            orcawave_source=self.orcawave.source_files[0] if self.orcawave.source_files else "Unknown",
            comparison_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            rao_comparisons=rao_comparisons,
            added_mass_comparison=am_comparison,
            damping_comparison=damp_comparison
        )

        # Assess overall agreement
        report.overall_agreement = self._assess_agreement(report)

        # Generate notes
        report.notes = self._generate_notes(report)

        print(f"\n[OK] Comparison complete")
        print(f"Overall Agreement: {report.overall_agreement}")

        return report

    def _assess_agreement(self, report: ComparisonReport) -> str:
        """
        Assess overall agreement between results

        Returns:
            Agreement level: EXCELLENT, GOOD, FAIR, POOR
        """
        # Check RAO correlations
        rao_corrs = [
            comp.statistics.correlation
            for comp in report.rao_comparisons.values()
        ]

        # Check matrix correlations (diagonal terms only for simplicity)
        am_corrs = [
            report.added_mass_comparison.element_statistics[(i, i)].correlation
            for i in range(6)
        ]
        damp_corrs = [
            report.damping_comparison.element_statistics[(i, i)].correlation
            for i in range(6)
        ]

        all_corrs = rao_corrs + am_corrs + damp_corrs
        min_corr = min(all_corrs)
        mean_corr = np.mean(all_corrs)

        # Classify agreement
        if min_corr > 0.99 and mean_corr > 0.995:
            return "EXCELLENT"
        elif min_corr > 0.95 and mean_corr > 0.98:
            return "GOOD"
        elif min_corr > 0.90 and mean_corr > 0.95:
            return "FAIR"
        else:
            return "POOR"

    def _generate_notes(self, report: ComparisonReport) -> List[str]:
        """Generate summary notes about comparison"""
        notes = []

        # RAO notes
        for dof_name, comp in report.rao_comparisons.items():
            if comp.statistics.correlation < 0.95:
                notes.append(
                    f"Low correlation ({comp.statistics.correlation:.3f}) for {dof_name.upper()} RAO"
                )

        # Matrix notes
        if report.added_mass_comparison:
            am = report.added_mass_comparison
            notes.append(
                f"Maximum added mass deviation: {am.max_deviation_value:.2e} "
                f"at frequency {am.max_deviation_frequency:.3f} rad/s "
                f"for element ({am.max_deviation_element[0]},{am.max_deviation_element[1]})"
            )

        if report.damping_comparison:
            damp = report.damping_comparison
            notes.append(
                f"Maximum damping deviation: {damp.max_deviation_value:.2e} "
                f"at frequency {damp.max_deviation_frequency:.3f} rad/s "
                f"for element ({damp.max_deviation_element[0]},{damp.max_deviation_element[1]})"
            )

        return notes

    def export_report(self, output_file: Path):
        """
        Export comparison report to JSON

        Args:
            output_file: Path to output JSON file
        """
        report = self.generate_report()

        # Convert to dictionary
        report_dict = {
            'vessel_name': report.vessel_name,
            'aqwa_source': report.aqwa_source,
            'orcawave_source': report.orcawave_source,
            'comparison_date': report.comparison_date,
            'overall_agreement': report.overall_agreement,
            'notes': report.notes,
            'rao_comparisons': {},
            'added_mass_comparison': {},
            'damping_comparison': {}
        }

        # RAO comparisons
        for dof_name, comp in report.rao_comparisons.items():
            report_dict['rao_comparisons'][dof_name] = {
                'correlation': float(comp.statistics.correlation),
                'mean_error': float(comp.statistics.mean_error),
                'max_error': float(comp.statistics.max_error),
                'rms_error': float(comp.statistics.rms_error),
                'mean_abs_error': float(comp.statistics.mean_abs_error),
                'max_magnitude_diff_location': comp.max_magnitude_diff_location,
                'max_phase_diff_location': comp.max_phase_diff_location
            }

        # Matrix comparisons
        if report.added_mass_comparison:
            am = report.added_mass_comparison
            report_dict['added_mass_comparison'] = {
                'max_deviation': float(am.max_deviation_value),
                'max_deviation_frequency': float(am.max_deviation_frequency),
                'max_deviation_element': am.max_deviation_element,
                'diagonal_correlations': {
                    i: float(am.element_statistics[(i, i)].correlation)
                    for i in range(6)
                }
            }

        if report.damping_comparison:
            damp = report.damping_comparison
            report_dict['damping_comparison'] = {
                'max_deviation': float(damp.max_deviation_value),
                'max_deviation_frequency': float(damp.max_deviation_frequency),
                'max_deviation_element': damp.max_deviation_element,
                'diagonal_correlations': {
                    i: float(damp.element_statistics[(i, i)].correlation)
                    for i in range(6)
                }
            }

        # Export to JSON
        with open(output_file, 'w') as f:
            json.dump(report_dict, f, indent=2)

        print(f"\n[OK] Comparison report exported to: {output_file}")


def compare_diffraction_results(
    aqwa_results: DiffractionResults,
    orcawave_results: DiffractionResults,
    output_dir: Path,
    tolerance: float = 0.05
) -> ComparisonReport:
    """
    Convenience function for comparing diffraction results

    Args:
        aqwa_results: AQWA results
        orcawave_results: OrcaWave results
        output_dir: Output directory for reports
        tolerance: Relative tolerance (default: 5%)

    Returns:
        ComparisonReport object
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    comparator = DiffractionComparator(aqwa_results, orcawave_results, tolerance)

    # Generate report
    report = comparator.generate_report()

    # Export to JSON
    vessel_name = aqwa_results.vessel_name
    comparator.export_report(output_dir / f"{vessel_name}_comparison.json")

    return report
