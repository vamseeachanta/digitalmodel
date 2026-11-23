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
from typing import Dict, List, Literal, Tuple, Optional
from dataclasses import dataclass, field
from datetime import datetime
import json

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
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
    correlation: Optional[float]
    frequencies: np.ndarray
    errors: np.ndarray
    quality: Literal[
        "COMPARED",
        "IDENTICAL",
        "NULL_RESPONSE",
        # Structurally absent coupling — zero on both legs. Distinct from
        # INSUFFICIENT_DATA: it never claims agreement, but it also does not
        # refuse the report, because a symmetric body has genuinely zero
        # off-diagonal couplings (#1633).
        "NOT_APPLICABLE",
        # A diagonal term that is zero on both legs. Geometry/frame and
        # extraction evidence are absent, so its physical interpretation is
        # unestablished. This refuses without claiming extraction failure.
        "ABSENT_DIAGONAL",
        "INSUFFICIENT_DATA",
        "INSUFFICIENT_SAMPLING",
        "UNTRUSTED_SOURCE",
        "INVALID_ABSCISSA",
    ] = "COMPARED"

    @property
    def sample_count(self) -> Optional[int]:
        """Actual aligned rows, including a statistic with zero variance.

        A refused alignment has an empty axis and therefore an unknown count.
        """
        if not self.frequencies.size:
            return None
        return int(self.frequencies.size)


@dataclass
class RAOComparison:
    """RAO comparison results for a single DOF"""
    dof: DOF
    statistics: DeviationStatistics
    magnitude_diff: np.ndarray
    phase_diff: np.ndarray
    max_magnitude_diff_location: Tuple[int, int]  # (freq_idx, heading_idx)
    max_phase_diff_location: Tuple[int, int]
    refusal_reason: Optional[str] = None


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

    overall_agreement: Optional[str] = None  # EXCELLENT, GOOD, FAIR, POOR
    notes: List[str] = field(default_factory=list)
    comparison_status: str = "REFUSED"
    refusal_reasons: List[str] = field(default_factory=list)


class DiffractionComparator:
    """Compare diffraction results from AQWA and OrcaWave"""

    def __init__(
        self,
        aqwa_results: DiffractionResults,
        orcawave_results: DiffractionResults,
        tolerance: Optional[float] = None,
        *,
        policy=None,
    ):
        """
        Initialize comparator

        Args:
            aqwa_results: Results from AQWA converter
            orcawave_results: Results from OrcaWave converter
            tolerance: Retired scalar; explicit values raise ValueError.
            policy: Explicit uncertainty and resolution criteria, or diagnostic only.
        """
        if tolerance is not None:
            raise ValueError("tolerance requires a comparison policy with justification")
        self.policy = policy
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
        from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator
        return MultiSolverComparator._calculate_deviation_stats(values1, values2, frequencies)

    def _canonical(self):
        """Use the maintained comparator for acceptance and coefficient evidence."""
        from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator
        return MultiSolverComparator({"AQWA": self.aqwa, "OrcaWave": self.orcawave}, policy=self.policy)

    def compare_raos(self) -> Dict[str, RAOComparison]:
        """Adapt the canonical RAO evidence without recomputing its quality."""
        pair = next(iter(self._canonical().compare_raos().values()))
        comparisons = {}
        for name, comp in pair.items():
            magnitude_diff = comp.magnitude_stats.errors
            phase_diff = comp.phase_stats.errors
            def location(values):
                if not values.size:
                    return (0, 0)
                return tuple(int(i) for i in np.unravel_index(np.argmax(np.abs(values)), values.shape))
            comparisons[name] = RAOComparison(
                dof=comp.dof, statistics=comp.magnitude_stats,
                magnitude_diff=magnitude_diff, phase_diff=phase_diff,
                max_magnitude_diff_location=location(magnitude_diff),
                max_phase_diff_location=location(phase_diff),
                refusal_reason=comp.refusal_reason,
            )
        return comparisons

    def _compare_matrices(self, matrix_type: str) -> MatrixComparison:
        canonical = self._canonical()
        comparisons = canonical._compare_matrix_set(matrix_type)
        stats = next(iter(comparisons.values()))
        elements = {(i - 1, j - 1): value for (i, j), value in stats.items()}
        # A cell can refuse statistics after the matrix axis has been aligned.
        alignment, _ = canonical._align_matrix_pair(
            getattr(self.aqwa, matrix_type), getattr(self.orcawave, matrix_type),
        )
        frequencies = alignment.frequencies if alignment is not None else np.array([], dtype=float)
        comparison = MatrixComparison(matrix_type=matrix_type, frequencies=frequencies, element_statistics=elements)
        for element, value in elements.items():
            if value.max_error > comparison.max_deviation_value:
                comparison.max_deviation_value = value.max_error
                comparison.max_deviation_frequency = float(value.frequencies[np.argmax(np.abs(value.errors))])
                comparison.max_deviation_element = element
        return comparison

    def compare_added_mass(self) -> MatrixComparison:
        """Compare signed, aligned coefficients through the canonical producer."""
        return self._compare_matrices("added_mass")

    def compare_damping(self) -> MatrixComparison:
        """Compare signed, aligned coefficients through the canonical producer."""
        return self._compare_matrices("damping")

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

    def _assess_agreement(self, report: ComparisonReport) -> Optional[str]:
        """The canonical policy and evidence determine the legacy verdict."""
        canonical = self._canonical().generate_report()
        report.comparison_status = canonical.comparison_status
        report.refusal_reasons = canonical.refusal_reasons
        return next(iter(canonical.pairwise_results.values())).overall_agreement

    def _generate_notes(self, report: ComparisonReport) -> List[str]:
        """Generate summary notes about comparison"""
        notes = []

        notes.extend(report.refusal_reasons)

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
            'comparison_status': report.comparison_status,
            'refusal_reasons': report.refusal_reasons,
            'rao_comparisons': {},
            'added_mass_comparison': {},
            'damping_comparison': {}
        }

        # RAO comparisons
        for dof_name, comp in report.rao_comparisons.items():
            report_dict['rao_comparisons'][dof_name] = {
                'correlation': (
                    float(comp.statistics.correlation)
                    if comp.statistics.correlation is not None
                    else None
                ),
                'quality': comp.statistics.quality,
                'sample_count': comp.statistics.sample_count,
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
                'sample_counts': {f'{i},{j}': value.sample_count for (i, j), value in am.element_statistics.items()},
                'quality': {f'{i},{j}': value.quality for (i, j), value in am.element_statistics.items()},
                'max_deviation': float(am.max_deviation_value),
                'max_deviation_frequency': float(am.max_deviation_frequency),
                'max_deviation_element': am.max_deviation_element,
                'diagonal_correlations': {
                    i: (
                        float(am.element_statistics[(i, i)].correlation)
                        if am.element_statistics[(i, i)].correlation is not None
                        else None
                    )
                    for i in range(6)
                }
            }

        if report.damping_comparison:
            damp = report.damping_comparison
            report_dict['damping_comparison'] = {
                'sample_counts': {f'{i},{j}': value.sample_count for (i, j), value in damp.element_statistics.items()},
                'quality': {f'{i},{j}': value.quality for (i, j), value in damp.element_statistics.items()},
                'max_deviation': float(damp.max_deviation_value),
                'max_deviation_frequency': float(damp.max_deviation_frequency),
                'max_deviation_element': damp.max_deviation_element,
                'diagonal_correlations': {
                    i: (
                        float(damp.element_statistics[(i, i)].correlation)
                        if damp.element_statistics[(i, i)].correlation is not None
                        else None
                    )
                    for i in range(6)
                }
            }

        # Export to JSON
        with open(output_file, 'w') as f:
            json.dump(report_dict, f, indent=2, allow_nan=False,
                      default=self._canonical()._json_default)

        print(f"\n[OK] Comparison report exported to: {output_file}")


def compare_diffraction_results(
    aqwa_results: DiffractionResults,
    orcawave_results: DiffractionResults,
    output_dir: Path,
    tolerance: Optional[float] = None,
    *,
    policy=None,
) -> ComparisonReport:
    """
    Convenience function for comparing diffraction results

    Args:
        aqwa_results: AQWA results
        orcawave_results: OrcaWave results
        output_dir: Output directory for reports
        tolerance: Retired scalar; explicit values raise ValueError.
        policy: Declared comparison criteria; omitted for diagnostics.

    Returns:
        ComparisonReport object
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    comparator = DiffractionComparator(aqwa_results, orcawave_results, tolerance, policy=policy)

    # Generate report
    report = comparator.generate_report()

    # Export to JSON
    vessel_name = aqwa_results.vessel_name
    comparator.export_report(output_dir / f"{vessel_name}_comparison.json")

    return report
