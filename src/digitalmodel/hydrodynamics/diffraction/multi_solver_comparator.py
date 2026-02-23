#!/usr/bin/env python3
"""
N-way Solver Comparison Framework for Diffraction Results

ABOUTME: Compares diffraction results from multiple solvers (AQWA, OrcaWave,
BEMRosetta, etc.) using pairwise statistical analysis and consensus voting.

Provides:
- Pairwise RAO comparison across all solver combinations
- Added mass and damping matrix comparison with 1-based DOF indexing
- Consensus classification (FULL, MAJORITY, SPLIT, NO_CONSENSUS)
- Outlier solver detection
- JSON-exportable benchmark reports

Version: 1.0.0
Status: Multi-solver benchmark comparison
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from datetime import datetime
from itertools import combinations
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np

from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    DeviationStatistics,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
    RAOComponent,
)


# ---------------------------------------------------------------------------
# Dataclasses
# ---------------------------------------------------------------------------


@dataclass
class PairwiseRAOComparison:
    """RAO comparison between two solvers for a single DOF."""

    dof: DOF
    solver_a: str
    solver_b: str
    magnitude_stats: DeviationStatistics
    phase_stats: DeviationStatistics
    max_magnitude_diff: float
    max_phase_diff: float
    # Context for intelligent commentary on phase diffs
    magnitude_at_max_phase_diff: float = 0.0
    peak_magnitude: float = 0.0
    max_phase_diff_x: float = 0.0  # frequency (rad/s) where max phase diff occurs
    max_phase_diff_heading_idx: int = 0  # heading index where max phase diff occurs


@dataclass
class HydrostaticComparison:
    """Comparison of hydrostatic properties between two solvers."""

    solver_a: str
    solver_b: str
    displacement_volume_diff: float
    mass_diff: float
    cog_diff: List[float]             # [dx, dy, dz]
    cob_diff: List[float]             # [dx, dy, dz]
    waterplane_area_diff: float
    stiffness_matrix_diff: np.ndarray  # 6x6 matrix of absolute differences
    stiffness_matrix_correlation: float


@dataclass
class PairwiseResult:
    """Aggregate pairwise comparison between two solvers."""

    solver_a: str
    solver_b: str
    rao_comparisons: Dict[str, PairwiseRAOComparison]
    added_mass_correlations: Dict[Tuple[int, int], float]
    damping_correlations: Dict[Tuple[int, int], float]
    overall_agreement: str  # EXCELLENT, GOOD, FAIR, POOR
    hydrostatic_comparison: Optional[HydrostaticComparison] = None


@dataclass
class ConsensusMetrics:
    """Consensus analysis for a single DOF across all solvers."""

    dof: DOF
    solver_names: List[str]
    agreement_pairs: List[Tuple[str, str]]
    outlier_solver: Optional[str] = None
    consensus_level: str = "UNKNOWN"
    mean_pairwise_correlation: float = 0.0


@dataclass
class BenchmarkReport:
    """Complete multi-solver benchmark report."""

    vessel_name: str
    solver_names: List[str]
    comparison_date: str
    pairwise_results: Dict[str, PairwiseResult]
    consensus_by_dof: Dict[str, ConsensusMetrics]
    overall_consensus: str
    notes: List[str]


# ---------------------------------------------------------------------------
# MultiSolverComparator
# ---------------------------------------------------------------------------


class MultiSolverComparator:
    """Compare diffraction results from N solvers (N >= 2).

    Args:
        solver_results: Mapping of solver name to DiffractionResults.
        tolerance: Relative tolerance for agreement assessment (default 5%).

    Raises:
        ValueError: If fewer than 2 solvers or vessel names do not match.
    """

    def __init__(
        self,
        solver_results: Dict[str, DiffractionResults],
        tolerance: float = 0.05,
    ) -> None:
        self._results = solver_results
        self.tolerance = tolerance
        self._validate_inputs()
        self.solver_names: List[str] = sorted(solver_results.keys())

    # ------------------------------------------------------------------
    # Validation
    # ------------------------------------------------------------------

    def _validate_inputs(self) -> None:
        """Ensure at least 2 solvers and all vessel names match."""
        if len(self._results) < 2:
            raise ValueError(
                f"At least 2 solvers required, got {len(self._results)}"
            )
        vessel_names = {r.vessel_name for r in self._results.values()}
        if len(vessel_names) > 1:
            raise ValueError(
                f"Vessel name mismatch across solvers: {vessel_names}"
            )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _pair_key(name_a: str, name_b: str) -> str:
        """Alphabetically sorted pair key: 'A-vs-B'."""
        first, second = sorted([name_a, name_b])
        return f"{first}-vs-{second}"

    @staticmethod
    def _calculate_deviation_stats(
        values1: np.ndarray,
        values2: np.ndarray,
        frequencies: np.ndarray,
    ) -> DeviationStatistics:
        """Replicate deviation statistics from comparison_framework."""
        errors = values2 - values1
        mean_error = float(np.mean(errors))
        max_error = float(np.max(np.abs(errors)))
        rms_error = float(np.sqrt(np.mean(errors ** 2)))
        mean_abs_error = float(np.mean(np.abs(errors)))

        flat1 = values1.flatten()
        flat2 = values2.flatten()
        if np.allclose(flat1, flat2):
            correlation = 1.0
        else:
            correlation = float(np.corrcoef(flat1, flat2)[0, 1])

        return DeviationStatistics(
            mean_error=mean_error,
            max_error=max_error,
            rms_error=rms_error,
            mean_abs_error=mean_abs_error,
            correlation=correlation,
            frequencies=frequencies,
            errors=errors,
        )

    def _all_pairs(self) -> List[Tuple[str, str]]:
        """Return all unique solver pairs in alphabetical order."""
        return list(combinations(self.solver_names, 2))

    # ------------------------------------------------------------------
    # RAO comparison
    # ------------------------------------------------------------------

    def compare_raos(
        self,
    ) -> Dict[str, Dict[str, PairwiseRAOComparison]]:
        """Compare RAOs for every solver pair and every DOF.

        Returns:
            Outer key: pair key (e.g. 'AQWA-vs-OrcaWave').
            Inner key: DOF name in lowercase (e.g. 'surge').
            Value: PairwiseRAOComparison.
        """
        result: Dict[str, Dict[str, PairwiseRAOComparison]] = {}

        for solver_a, solver_b in self._all_pairs():
            key = self._pair_key(solver_a, solver_b)
            pair_comparisons: Dict[str, PairwiseRAOComparison] = {}

            res_a = self._results[solver_a]
            res_b = self._results[solver_b]

            for dof in DOF:
                dof_name = dof.name.lower()
                rao_a: RAOComponent = getattr(res_a.raos, dof_name)
                rao_b: RAOComponent = getattr(res_b.raos, dof_name)

                mag_stats = self._calculate_deviation_stats(
                    rao_a.magnitude,
                    rao_b.magnitude,
                    rao_a.frequencies.values,
                )

                # Compute peak magnitude first to decide if phase
                # correlation is physically meaningful.
                avg_mag = 0.5 * (rao_a.magnitude + rao_b.magnitude)
                peak_mag = float(np.max(avg_mag))

                if peak_mag < 1e-10:
                    # Near-zero magnitude: phase is undefined
                    # (atan2(0,0) noise).  Override with perfect
                    # agreement instead of computing noise correlation.
                    zeros = np.zeros_like(rao_a.phase)
                    phase_stats = DeviationStatistics(
                        mean_error=0.0,
                        max_error=0.0,
                        rms_error=0.0,
                        mean_abs_error=0.0,
                        correlation=1.0,
                        frequencies=rao_a.frequencies.values,
                        errors=zeros,
                    )
                else:
                    phase_stats = self._calculate_deviation_stats(
                        rao_a.phase,
                        rao_b.phase,
                        rao_a.frequencies.values,
                    )

                mag_diff = np.abs(rao_b.magnitude - rao_a.magnitude)
                phase_diff = np.abs(rao_b.phase - rao_a.phase)

                # Find where max phase diff occurs and what the
                # amplitude is there (average of both solvers).
                max_pd_idx = np.unravel_index(
                    np.argmax(phase_diff), phase_diff.shape,
                )
                mag_at_max_pd = float(avg_mag[max_pd_idx])
                freq_at_max_pd = float(
                    rao_a.frequencies.values[max_pd_idx[0]],
                )

                pair_comparisons[dof_name] = PairwiseRAOComparison(
                    dof=dof,
                    solver_a=solver_a,
                    solver_b=solver_b,
                    magnitude_stats=mag_stats,
                    phase_stats=phase_stats,
                    max_magnitude_diff=float(np.max(mag_diff)),
                    max_phase_diff=float(np.max(phase_diff)),
                    magnitude_at_max_phase_diff=mag_at_max_pd,
                    peak_magnitude=peak_mag,
                    max_phase_diff_x=freq_at_max_pd,
                    max_phase_diff_heading_idx=int(max_pd_idx[1]),
                )

            result[key] = pair_comparisons

        return result

    # ------------------------------------------------------------------
    # Matrix comparison helpers
    # ------------------------------------------------------------------

    def _compare_matrix_set(
        self,
        matrix_attr: str,
    ) -> Dict[str, Dict[Tuple[int, int], DeviationStatistics]]:
        """Compare added_mass or damping matrices across all pairs.

        Keys use 1-based indexing to match DOF enum values.
        """
        result: Dict[
            str, Dict[Tuple[int, int], DeviationStatistics]
        ] = {}

        for solver_a, solver_b in self._all_pairs():
            key = self._pair_key(solver_a, solver_b)
            pair_stats: Dict[Tuple[int, int], DeviationStatistics] = {}

            set_a = getattr(self._results[solver_a], matrix_attr)
            set_b = getattr(self._results[solver_b], matrix_attr)

            freqs = set_a.frequencies.values

            for i in range(6):
                for j in range(6):
                    vals_a = np.array(
                        [m.matrix[i, j] for m in set_a.matrices]
                    )
                    vals_b = np.array(
                        [m.matrix[i, j] for m in set_b.matrices]
                    )
                    stats = self._calculate_deviation_stats(
                        vals_a, vals_b, freqs,
                    )
                    # 1-based indexing
                    pair_stats[(i + 1, j + 1)] = stats

            result[key] = pair_stats

        return result

    def compare_added_mass(
        self,
    ) -> Dict[str, Dict[Tuple[int, int], DeviationStatistics]]:
        """Compare added mass matrices across all solver pairs."""
        return self._compare_matrix_set("added_mass")

    def compare_damping(
        self,
    ) -> Dict[str, Dict[Tuple[int, int], DeviationStatistics]]:
        """Compare damping matrices across all solver pairs."""
        return self._compare_matrix_set("damping")

    def compare_hydrostatics(self) -> Dict[str, HydrostaticComparison]:
        """Compare hydrostatic results across all solver pairs."""
        result: Dict[str, HydrostaticComparison] = {}

        for solver_a, solver_b in self._all_pairs():
            key = self._pair_key(solver_a, solver_b)

            res_a = self._results[solver_a]
            res_b = self._results[solver_b]

            h_a = getattr(res_a, "hydrostatics", None)
            h_b = getattr(res_b, "hydrostatics", None)

            if not h_a or not h_b:
                continue

            vol_diff = h_b.displacement_volume - h_a.displacement_volume
            mass_diff = h_b.mass - h_a.mass
            cog_diff = [b - a for a, b in zip(h_a.centre_of_gravity, h_b.centre_of_gravity)]
            cob_diff = [b - a for a, b in zip(h_a.centre_of_buoyancy, h_b.centre_of_buoyancy)]
            awp_diff = h_b.waterplane_area - h_a.waterplane_area

            stiff_diff = h_b.stiffness_matrix - h_a.stiffness_matrix

            # Correlation for stiffness matrix (flat)
            flat_a = h_a.stiffness_matrix.flatten()
            flat_b = h_b.stiffness_matrix.flatten()
            if np.allclose(flat_a, flat_b):
                corr = 1.0
            else:
                with np.errstate(invalid='ignore'):
                    c = np.corrcoef(flat_a, flat_b)[0, 1]
                    corr = float(c) if not np.isnan(c) else 1.0

            result[key] = HydrostaticComparison(
                solver_a=solver_a,
                solver_b=solver_b,
                displacement_volume_diff=vol_diff,
                mass_diff=mass_diff,
                cog_diff=cog_diff,
                cob_diff=cob_diff,
                waterplane_area_diff=awp_diff,
                stiffness_matrix_diff=stiff_diff,
                stiffness_matrix_correlation=corr,
            )

        return result

    # ------------------------------------------------------------------
    # Consensus
    # ------------------------------------------------------------------

    def compute_consensus(self) -> Dict[str, ConsensusMetrics]:
        """Classify agreement level per DOF across all solvers.

        Uses both correlation and normalized RMS error to detect bias.
        A pair is 'agreeing' when correlation > 0.99 AND rms_error
        is below the tolerance threshold.

        Consensus levels:
            FULL:         all pairs agree (high corr AND low rms)
            MAJORITY:     >= 2 of 3 pairs agree (for 3 solvers)
            SPLIT:        >= 1 pair agrees
            NO_CONSENSUS: no pairs agree
        """
        rao_comparisons = self.compare_raos()
        consensus: Dict[str, ConsensusMetrics] = {}

        for dof in DOF:
            dof_name = dof.name.lower()
            dof_key = dof.name  # uppercase key

            pairs = self._all_pairs()

            # For each pair, check both correlation AND rms_error
            pair_agrees: Dict[str, bool] = {}
            pair_corrs: Dict[str, float] = {}

            for solver_a, solver_b in pairs:
                pk = self._pair_key(solver_a, solver_b)
                comp = rao_comparisons[pk][dof_name]
                corr = comp.magnitude_stats.correlation
                rms = comp.magnitude_stats.rms_error
                pair_corrs[pk] = corr

                # Agreement requires high correlation AND low rms
                # rms threshold is based on tolerance (default 0.05)
                pair_agrees[pk] = (
                    corr > 0.99 and rms < self.tolerance
                )

            high_pairs = [
                pk for pk, agrees in pair_agrees.items() if agrees
            ]
            low_pairs = [
                pk for pk, agrees in pair_agrees.items() if not agrees
            ]

            mean_corr = float(np.mean(list(pair_corrs.values())))

            # Determine consensus level
            if all(pair_agrees.values()):
                level = "FULL"
            elif len(high_pairs) >= 2:
                level = "MAJORITY"
            elif len(high_pairs) >= 1:
                level = "SPLIT"
            else:
                level = "NO_CONSENSUS"

            # Identify agreement pairs
            agreement_tuples = []
            for pk in high_pairs:
                parts = pk.split("-vs-")
                agreement_tuples.append((parts[0], parts[1]))

            # Outlier detection for 3+ solvers
            outlier: Optional[str] = None
            if len(self.solver_names) >= 3 and level == "MAJORITY":
                solver_counts: Dict[str, int] = {
                    s: 0 for s in self.solver_names
                }
                for pk in low_pairs:
                    parts = pk.split("-vs-")
                    for part in parts:
                        solver_counts[part] += 1
                max_count = max(solver_counts.values())
                if max_count > 0:
                    candidates = [
                        s
                        for s, c in solver_counts.items()
                        if c == max_count
                    ]
                    if len(candidates) == 1:
                        outlier = candidates[0]

            consensus[dof_key] = ConsensusMetrics(
                dof=dof,
                solver_names=list(self.solver_names),
                agreement_pairs=agreement_tuples,
                outlier_solver=outlier,
                consensus_level=level,
                mean_pairwise_correlation=mean_corr,
            )

        return consensus

    # ------------------------------------------------------------------
    # Report generation
    # ------------------------------------------------------------------

    def _assess_pair_agreement(
        self,
        rao_comps: Dict[str, PairwiseRAOComparison],
    ) -> str:
        """Classify overall pairwise agreement."""
        corrs = [
            c.magnitude_stats.correlation
            for c in rao_comps.values()
        ]
        min_corr = min(corrs)
        mean_corr = float(np.mean(corrs))

        if min_corr > 0.99 and mean_corr > 0.995:
            return "EXCELLENT"
        if min_corr > 0.95 and mean_corr > 0.98:
            return "GOOD"
        if min_corr > 0.90 and mean_corr > 0.95:
            return "FAIR"
        return "POOR"

    def generate_report(self) -> BenchmarkReport:
        """Build a complete BenchmarkReport."""
        rao_comparisons = self.compare_raos()
        am_comparisons = self.compare_added_mass()
        damp_comparisons = self.compare_damping()
        hydro_comparisons = self.compare_hydrostatics()
        consensus = self.compute_consensus()

        pairwise_results: Dict[str, PairwiseResult] = {}
        for solver_a, solver_b in self._all_pairs():
            pk = self._pair_key(solver_a, solver_b)

            am_corrs = {
                k: v.correlation
                for k, v in am_comparisons[pk].items()
            }
            damp_corrs = {
                k: v.correlation
                for k, v in damp_comparisons[pk].items()
            }

            agreement = self._assess_pair_agreement(rao_comparisons[pk])

            pairwise_results[pk] = PairwiseResult(
                solver_a=solver_a,
                solver_b=solver_b,
                rao_comparisons=rao_comparisons[pk],
                added_mass_correlations=am_corrs,
                damping_correlations=damp_corrs,
                overall_agreement=agreement,
                hydrostatic_comparison=hydro_comparisons.get(pk),
            )

        # Determine overall consensus from per-DOF levels
        levels = [m.consensus_level for m in consensus.values()]
        if all(lv == "FULL" for lv in levels):
            overall = "FULL"
        elif levels.count("FULL") + levels.count("MAJORITY") >= 4:
            overall = "MAJORITY"
        elif any(lv in ("FULL", "MAJORITY") for lv in levels):
            overall = "SPLIT"
        else:
            overall = "NO_CONSENSUS"

        notes = self._generate_notes(consensus, rao_comparisons)
        vessel_name = next(iter(self._results.values())).vessel_name

        return BenchmarkReport(
            vessel_name=vessel_name,
            solver_names=list(self.solver_names),
            comparison_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            pairwise_results=pairwise_results,
            consensus_by_dof=consensus,
            overall_consensus=overall,
            notes=notes,
        )

    @staticmethod
    def _generate_notes(
        consensus: Dict[str, ConsensusMetrics],
        rao_comparisons: Dict[str, Dict[str, PairwiseRAOComparison]],
    ) -> List[str]:
        """Produce human-readable notes about the comparison."""
        notes: List[str] = []
        for dof_key, metrics in consensus.items():
            if metrics.consensus_level != "FULL":
                notes.append(
                    f"{dof_key}: consensus={metrics.consensus_level}"
                )
            if metrics.outlier_solver:
                notes.append(
                    f"{dof_key}: outlier solver is "
                    f"{metrics.outlier_solver}"
                )
        return notes

    # ------------------------------------------------------------------
    # JSON export
    # ------------------------------------------------------------------

    def export_report_json(self, output_file: Path) -> None:
        """Export benchmark report to a JSON file."""
        report = self.generate_report()
        output_file = Path(output_file)

        data = {
            "vessel_name": report.vessel_name,
            "solver_names": report.solver_names,
            "comparison_date": report.comparison_date,
            "overall_consensus": report.overall_consensus,
            "pairwise_results": self._serialize_pairwise(
                report.pairwise_results,
            ),
            "consensus_by_dof": self._serialize_consensus(
                report.consensus_by_dof,
            ),
            "notes": report.notes,
        }

        output_file.parent.mkdir(parents=True, exist_ok=True)
        with open(output_file, "w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2, default=self._json_default)

    @staticmethod
    def _json_default(obj: object) -> object:
        """Fallback serializer for numpy types."""
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        raise TypeError(f"Object of type {type(obj)} is not serializable")

    @staticmethod
    def _serialize_pairwise(
        pairwise: Dict[str, PairwiseResult],
    ) -> Dict[str, dict]:
        """Convert PairwiseResult dict to JSON-safe dict."""
        out: Dict[str, dict] = {}
        for pk, pr in pairwise.items():
            rao_dict = {}
            for dof_name, comp in pr.rao_comparisons.items():
                rao_dict[dof_name] = {
                    "magnitude_correlation": float(
                        comp.magnitude_stats.correlation,
                    ),
                    "magnitude_rms_error": float(
                        comp.magnitude_stats.rms_error,
                    ),
                    "phase_correlation": float(
                        comp.phase_stats.correlation,
                    ),
                    "max_magnitude_diff": float(comp.max_magnitude_diff),
                    "max_phase_diff": float(comp.max_phase_diff),
                }
            am_corrs = {
                f"{k[0]},{k[1]}": float(v)
                for k, v in pr.added_mass_correlations.items()
            }
            damp_corrs = {
                f"{k[0]},{k[1]}": float(v)
                for k, v in pr.damping_correlations.items()
            }
            hc_dict = None
            if pr.hydrostatic_comparison:
                hc = pr.hydrostatic_comparison
                hc_dict = {
                    "displacement_volume_diff": float(hc.displacement_volume_diff),
                    "mass_diff": float(hc.mass_diff),
                    "cog_diff": [float(v) for v in hc.cog_diff],
                    "cob_diff": [float(v) for v in hc.cob_diff],
                    "waterplane_area_diff": float(hc.waterplane_area_diff),
                    "stiffness_matrix_correlation": float(hc.stiffness_matrix_correlation),
                }

            out[pk] = {
                "solver_a": pr.solver_a,
                "solver_b": pr.solver_b,
                "overall_agreement": pr.overall_agreement,
                "rao_comparisons": rao_dict,
                "added_mass_correlations": am_corrs,
                "damping_correlations": damp_corrs,
                "hydrostatic_comparison": hc_dict,
            }
        return out

    @staticmethod
    def _serialize_consensus(
        consensus: Dict[str, ConsensusMetrics],
    ) -> Dict[str, dict]:
        """Convert ConsensusMetrics dict to JSON-safe dict."""
        out: Dict[str, dict] = {}
        for dof_key, cm in consensus.items():
            out[dof_key] = {
                "consensus_level": cm.consensus_level,
                "mean_pairwise_correlation": float(
                    cm.mean_pairwise_correlation,
                ),
                "outlier_solver": cm.outlier_solver,
                "agreement_pairs": [
                    list(pair) for pair in cm.agreement_pairs
                ],
            }
        return out
