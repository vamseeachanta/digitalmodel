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

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaConfig,
    AbscissaGapError,
    AbscissaOrderError,
    AbscissaOverlapError,
    AlignedResponses,
    InsufficientSampling,
    align_responses,
)


REFUSAL_QUALITIES = {
    "INSUFFICIENT_DATA",
    "INSUFFICIENT_SAMPLING",
    "UNTRUSTED_SOURCE",
    "INVALID_ABSCISSA",
}


@dataclass(frozen=True)
class ComparisonPolicy:
    """Verdict thresholds derived from a declared uncertainty budget."""

    solver_relative_uncertainty: float
    response_absolute_resolution: float
    minimum_explained_variance: float
    justification: str
    abscissa_config: AbscissaConfig = field(default_factory=AbscissaConfig)

    def __post_init__(self) -> None:
        if self.solver_relative_uncertainty <= 0.0:
            raise ValueError("solver_relative_uncertainty must be positive")
        if self.response_absolute_resolution <= 0.0:
            raise ValueError("response_absolute_resolution must be positive")
        if not 0.0 < self.minimum_explained_variance < 1.0:
            raise ValueError("minimum_explained_variance must be between zero and one")
        if not self.justification.strip():
            raise ValueError("comparison policy requires a justification")

    @classmethod
    def from_uncertainties(
        cls,
        *,
        solver_relative_uncertainty: float,
        response_absolute_resolution: float,
        minimum_explained_variance: float,
        justification: str,
        abscissa_config: AbscissaConfig | None = None,
    ) -> "ComparisonPolicy":
        """Build policy from named solver uncertainty and resolution inputs."""
        return cls(
            solver_relative_uncertainty=solver_relative_uncertainty,
            response_absolute_resolution=response_absolute_resolution,
            minimum_explained_variance=minimum_explained_variance,
            justification=justification,
            abscissa_config=abscissa_config or AbscissaConfig(),
        )

    @property
    def relative_rms_tolerance(self) -> float:
        """Worst-case pair budget: each of two solvers contributes at most u."""
        return 2.0 * self.solver_relative_uncertainty

    @property
    def absolute_rms_floor(self) -> float:
        """Pairwise absolute floor from two solver resolution contributions."""
        return 2.0 * self.response_absolute_resolution

    @property
    def null_response_magnitude(self) -> float:
        """Magnitude below the pairwise resolution floor has undefined phase."""
        return self.absolute_rms_floor

    @property
    def correlation_minimum(self) -> float:
        """Correlation implied by the minimum explained-variance fraction."""
        return float(np.sqrt(self.minimum_explained_variance))

    def to_dict(self) -> Dict[str, object]:
        return {
            "solver_relative_uncertainty": self.solver_relative_uncertainty,
            "response_absolute_resolution": self.response_absolute_resolution,
            "minimum_explained_variance": self.minimum_explained_variance,
            "relative_rms_tolerance": self.relative_rms_tolerance,
            "absolute_rms_floor": self.absolute_rms_floor,
            "null_response_magnitude": self.null_response_magnitude,
            "correlation_minimum": self.correlation_minimum,
            "phase_verdict_role": "diagnostic_only",
            "justification": self.justification,
            "abscissa": {
                "minimum_samples": self.abscissa_config.min_samples,
                "minimum_coverage": self.abscissa_config.min_coverage,
                "maximum_relative_gap": self.abscissa_config.max_relative_gap,
                "justification": self.abscissa_config.justification,
            },
        }


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
    relative_rms_error: Optional[float] = None
    refusal_reason: Optional[str] = None


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
    stiffness_matrix_correlation: Optional[float]


@dataclass
class PairwiseResult:
    """Aggregate pairwise comparison between two solvers."""

    solver_a: str
    solver_b: str
    rao_comparisons: Dict[str, PairwiseRAOComparison]
    added_mass_correlations: Dict[Tuple[int, int], Optional[float]]
    damping_correlations: Dict[Tuple[int, int], Optional[float]]
    overall_agreement: Optional[str]  # EXCELLENT, GOOD, FAIR, POOR
    hydrostatic_comparison: Optional[HydrostaticComparison] = None
    added_mass_quality: Dict[Tuple[int, int], str] = field(default_factory=dict)
    damping_quality: Dict[Tuple[int, int], str] = field(default_factory=dict)
    comparison_status: str = "DECIDED"
    refusal_reason: Optional[str] = None


@dataclass
class ConsensusMetrics:
    """Consensus analysis for a single DOF across all solvers."""

    dof: DOF
    solver_names: List[str]
    agreement_pairs: List[Tuple[str, str]]
    outlier_solver: Optional[str] = None
    consensus_level: Optional[str] = None
    mean_pairwise_correlation: Optional[float] = None
    comparison_status: str = "DECIDED"
    refusal_reason: Optional[str] = None


@dataclass
class BenchmarkReport:
    """Complete multi-solver benchmark report."""

    vessel_name: str
    solver_names: List[str]
    comparison_date: str
    pairwise_results: Dict[str, PairwiseResult]
    consensus_by_dof: Dict[str, ConsensusMetrics]
    overall_consensus: Optional[str]
    notes: List[str]
    comparison_status: str = "DECIDED"
    refusal_reasons: List[str] = field(default_factory=list)
    comparison_policy: Optional[Dict[str, object]] = None


# ---------------------------------------------------------------------------
# MultiSolverComparator
# ---------------------------------------------------------------------------


class MultiSolverComparator:
    """Compare diffraction results from N solvers (N >= 2).

    Args:
        solver_results: Mapping of solver name to DiffractionResults.
        policy: Thresholds derived from named physical uncertainty inputs.

    Raises:
        ValueError: If fewer than 2 solvers or vessel names do not match.
    """

    def __init__(
        self,
        solver_results: Dict[str, DiffractionResults],
        tolerance: Optional[float] = None,
        *,
        policy: Optional[ComparisonPolicy] = None,
    ) -> None:
        if tolerance is not None:
            raise ValueError(
                "tolerance requires a comparison policy with justification"
            )
        self._results = solver_results
        self.policy = policy
        self.tolerance = (
            policy.relative_rms_tolerance if policy is not None else None
        )
        self.tolerance_semantics = (
            "symmetric_relative_rms_with_absolute_floor"
        )
        self.tolerance_justification = (
            policy.justification if policy is not None else None
        )
        self._validate_inputs()
        self.solver_names: List[str] = sorted(solver_results.keys())

    # ------------------------------------------------------------------
    # Validation
    # ------------------------------------------------------------------

    def _validate_inputs(self) -> None:
        """Ensure at least 2 solvers, matching vessel names, and matching units."""
        if len(self._results) < 2:
            raise ValueError(
                f"At least 2 solvers required, got {len(self._results)}"
            )
        vessel_names = {r.vessel_name for r in self._results.values()}
        if len(vessel_names) > 1:
            raise ValueError(
                f"Vessel name mismatch across solvers: {vessel_names}"
            )
        self._validate_matrix_units()

    def _validate_matrix_units(self) -> None:
        """Reject coefficient sets whose solvers disagree on units.

        `_compare_matrix_set` differences raw `matrix[i, j]` entries across
        solvers. If one solver reports added mass in kg and another in tonnes
        the deviation statistics are wrong by 1000x with no other symptom, so
        the mismatch has to fail here rather than propagate (#1550).
        """
        for matrix_attr in ("added_mass", "damping"):
            units_by_solver: Dict[str, Dict[str, str]] = {}

            for solver_name, results in self._results.items():
                coefficient_set = getattr(results, matrix_attr, None)
                matrices = getattr(coefficient_set, "matrices", None)
                if not matrices:
                    continue
                units_by_solver[solver_name] = dict(matrices[0].units)

            distinct = {
                tuple(sorted(units.items())) for units in units_by_solver.values()
            }
            if len(distinct) > 1:
                detail = ", ".join(
                    f"{solver}={units}"
                    for solver, units in sorted(units_by_solver.items())
                )
                raise ValueError(
                    f"Unit mismatch across solvers for {matrix_attr}: {detail}. "
                    "Normalise to a common unit before comparing - differencing "
                    "them directly yields deviation statistics wrong by the "
                    "ratio between the two units."
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
        if (
            values1.size == 0
            or values2.size == 0
            or not np.all(np.isfinite(values1))
            or not np.all(np.isfinite(values2))
        ):
            return DeviationStatistics(
                mean_error=0.0,
                max_error=0.0,
                rms_error=0.0,
                mean_abs_error=0.0,
                correlation=None,
                frequencies=frequencies,
                errors=np.array([], dtype=float),
                quality="INSUFFICIENT_DATA",
            )

        errors = values2 - values1
        mean_error = float(np.mean(errors))
        max_error = float(np.max(np.abs(errors)))
        rms_error = float(np.sqrt(np.mean(errors ** 2)))
        mean_abs_error = float(np.mean(np.abs(errors)))

        flat1 = values1.flatten()
        flat2 = values2.flatten()
        if np.array_equal(flat1, flat2):
            correlation = 1.0
            quality = "IDENTICAL"
        elif np.ptp(flat1) == 0.0 or np.ptp(flat2) == 0.0:
            correlation = None
            quality = "INSUFFICIENT_DATA"
        else:
            correlation = float(np.corrcoef(flat1, flat2)[0, 1])
            quality = "COMPARED"

        return DeviationStatistics(
            mean_error=mean_error,
            max_error=max_error,
            rms_error=rms_error,
            mean_abs_error=mean_abs_error,
            correlation=correlation,
            frequencies=frequencies,
            errors=errors,
            quality=quality,
        )

    @staticmethod
    def _calculate_phase_deviation_stats(
        values1: np.ndarray,
        values2: np.ndarray,
        frequencies: np.ndarray,
    ) -> DeviationStatistics:
        """Calculate phase differences on the shortest circular arc."""
        if (
            values1.size == 0
            or values2.size == 0
            or not np.all(np.isfinite(values1))
            or not np.all(np.isfinite(values2))
        ):
            return DeviationStatistics(
                mean_error=0.0,
                max_error=0.0,
                rms_error=0.0,
                mean_abs_error=0.0,
                correlation=None,
                frequencies=frequencies,
                errors=np.array([], dtype=float),
                quality="INSUFFICIENT_DATA",
            )

        errors = (values2 - values1 + 180.0) % 360.0 - 180.0
        error_radians = np.deg2rad(errors)
        circular_mean = float(
            np.rad2deg(
                np.arctan2(
                    np.mean(np.sin(error_radians)),
                    np.mean(np.cos(error_radians)),
                )
            )
        )
        identical = bool(np.array_equal(errors, np.zeros_like(errors)))
        return DeviationStatistics(
            mean_error=circular_mean,
            max_error=float(np.max(np.abs(errors))),
            rms_error=float(np.sqrt(np.mean(errors ** 2))),
            mean_abs_error=float(np.mean(np.abs(errors))),
            correlation=(
                1.0
                if identical
                else float(np.mean(np.cos(error_radians)))
            ),
            frequencies=frequencies,
            errors=errors,
            quality="IDENTICAL" if identical else "COMPARED",
        )

    def _all_pairs(self) -> List[Tuple[str, str]]:
        """Return all unique solver pairs in alphabetical order."""
        return list(combinations(self.solver_names, 2))

    @staticmethod
    def _symmetric_relative_rms(
        values1: np.ndarray,
        values2: np.ndarray,
        *,
        absolute_floor: float,
    ) -> float:
        """Normalize RMS error without amplifying sub-resolution responses."""
        rms_error = float(np.sqrt(np.mean((values2 - values1) ** 2)))
        scale = float(
            np.sqrt((np.mean(values1 ** 2) + np.mean(values2 ** 2)) / 2.0)
        )
        denominator = max(scale, absolute_floor)
        if denominator == 0.0:
            return 0.0 if rms_error == 0.0 else float("inf")
        return rms_error / denominator

    @staticmethod
    def _insufficient_sampling_stats(
        shape: Tuple[int, ...],
    ) -> DeviationStatistics:
        """Represent a sampling refusal without fabricating a correlation."""
        errors = np.zeros(shape, dtype=float)
        return DeviationStatistics(
            mean_error=0.0,
            max_error=0.0,
            rms_error=0.0,
            mean_abs_error=0.0,
            correlation=None,
            frequencies=np.array([], dtype=float),
            errors=errors,
            quality="INSUFFICIENT_SAMPLING",
        )

    @staticmethod
    def _insufficient_data_stats(
        shape: Tuple[int, ...],
    ) -> DeviationStatistics:
        """Represent invalid response values without propagating NaN."""
        errors = np.zeros(shape, dtype=float)
        return DeviationStatistics(
            mean_error=0.0,
            max_error=0.0,
            rms_error=0.0,
            mean_abs_error=0.0,
            correlation=None,
            frequencies=np.array([], dtype=float),
            errors=errors,
            quality="INSUFFICIENT_DATA",
        )

    @staticmethod
    def _untrusted_source_stats(shape: Tuple[int, ...]) -> DeviationStatistics:
        """Refuse a derived coefficient whose source is not solver output."""
        return DeviationStatistics(
            mean_error=0.0,
            max_error=0.0,
            rms_error=0.0,
            mean_abs_error=0.0,
            correlation=None,
            frequencies=np.array([], dtype=float),
            errors=np.zeros(shape, dtype=float),
            quality="UNTRUSTED_SOURCE",
        )

    @staticmethod
    def _invalid_abscissa_stats(shape: Tuple[int, ...]) -> DeviationStatistics:
        """Represent an invalid comparison axis without losing the refusal."""
        return DeviationStatistics(
            mean_error=0.0,
            max_error=0.0,
            rms_error=0.0,
            mean_abs_error=0.0,
            correlation=None,
            frequencies=np.array([], dtype=float),
            errors=np.zeros(shape, dtype=float),
            quality="INVALID_ABSCISSA",
        )

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

                try:
                    alignment = align_responses(
                        rao_a.frequencies.values,
                        rao_a.magnitude,
                        rao_a.phase,
                        rao_b.frequencies.values,
                        rao_b.magnitude,
                        rao_b.phase,
                        config=(
                            self.policy.abscissa_config
                            if self.policy is not None
                            else None
                        ),
                    )
                except (
                    AbscissaOrderError,
                    AbscissaOverlapError,
                    AbscissaGapError,
                ) as exc:
                    unavailable = self._invalid_abscissa_stats(
                        rao_a.magnitude.shape,
                    )
                    pair_comparisons[dof_name] = PairwiseRAOComparison(
                        dof=dof,
                        solver_a=solver_a,
                        solver_b=solver_b,
                        magnitude_stats=unavailable,
                        phase_stats=self._invalid_abscissa_stats(
                            rao_a.phase.shape,
                        ),
                        max_magnitude_diff=0.0,
                        max_phase_diff=0.0,
                        relative_rms_error=None,
                        refusal_reason=f"{type(exc).__name__}: {exc}",
                    )
                    continue
                if isinstance(alignment, InsufficientSampling):
                    unavailable = self._insufficient_sampling_stats(
                        rao_a.magnitude.shape,
                    )
                    pair_comparisons[dof_name] = PairwiseRAOComparison(
                        dof=dof,
                        solver_a=solver_a,
                        solver_b=solver_b,
                        magnitude_stats=unavailable,
                        phase_stats=self._insufficient_sampling_stats(
                            rao_a.phase.shape,
                        ),
                        max_magnitude_diff=0.0,
                        max_phase_diff=0.0,
                        relative_rms_error=None,
                    )
                    continue

                if not isinstance(alignment, AlignedResponses):
                    raise TypeError("unexpected abscissa alignment result")
                frequencies = alignment.frequencies
                magnitude_a = alignment.first.magnitude
                magnitude_b = alignment.second.magnitude
                phase_a = alignment.first.phase_degrees
                phase_b = alignment.second.phase_degrees

                if not all(
                    np.all(np.isfinite(values))
                    for values in (magnitude_a, magnitude_b, phase_a, phase_b)
                ):
                    pair_comparisons[dof_name] = PairwiseRAOComparison(
                        dof=dof,
                        solver_a=solver_a,
                        solver_b=solver_b,
                        magnitude_stats=self._insufficient_data_stats(
                            magnitude_a.shape,
                        ),
                        phase_stats=self._insufficient_data_stats(phase_a.shape),
                        max_magnitude_diff=0.0,
                        max_phase_diff=0.0,
                        relative_rms_error=None,
                    )
                    continue

                mag_stats = self._calculate_deviation_stats(
                    magnitude_a,
                    magnitude_b,
                    frequencies,
                )

                # Compute peak magnitude first to decide if phase
                # correlation is physically meaningful.
                avg_mag = 0.5 * (magnitude_a + magnitude_b)
                peak_mag = float(np.max(avg_mag))

                null_limit = (
                    self.policy.null_response_magnitude
                    if self.policy is not None
                    else None
                )
                if peak_mag == 0.0 or (
                    null_limit is not None and peak_mag < null_limit
                ):
                    # Near-zero magnitude: phase is undefined
                    # (atan2(0,0) noise).  Override with perfect
                    # agreement instead of computing noise correlation.
                    zeros = np.zeros_like(phase_a)
                    phase_stats = DeviationStatistics(
                        mean_error=0.0,
                        max_error=0.0,
                        rms_error=0.0,
                        mean_abs_error=0.0,
                        correlation=1.0,
                        frequencies=frequencies,
                        errors=zeros,
                        quality="NULL_RESPONSE",
                    )
                else:
                    phase_stats = self._calculate_phase_deviation_stats(
                        phase_a,
                        phase_b,
                        frequencies,
                    )

                mag_diff = np.abs(magnitude_b - magnitude_a)
                phase_diff = np.abs(phase_stats.errors)

                # Find where max phase diff occurs and what the
                # amplitude is there (average of both solvers).
                max_pd_idx = np.unravel_index(
                    np.argmax(phase_diff), phase_diff.shape,
                )
                mag_at_max_pd = float(avg_mag[max_pd_idx])
                freq_at_max_pd = float(
                    frequencies[max_pd_idx[0]],
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
                    relative_rms_error=self._symmetric_relative_rms(
                        magnitude_a,
                        magnitude_b,
                        absolute_floor=(
                            self.policy.absolute_rms_floor
                            if self.policy is not None
                            else 0.0
                        ),
                    ),
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
            trusted_sources = all(
                matrix.source == "solver"
                for matrix in (*set_a.matrices, *set_b.matrices)
            )

            for i in range(6):
                for j in range(6):
                    vals_a = np.array(
                        [m.matrix[i, j] for m in set_a.matrices]
                    )
                    vals_b = np.array(
                        [m.matrix[i, j] for m in set_b.matrices]
                    )
                    stats = (
                        self._calculate_deviation_stats(vals_a, vals_b, freqs)
                        if trusted_sources
                        else self._untrusted_source_stats(vals_a.shape)
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
            if np.array_equal(flat_a, flat_b):
                corr = 1.0
            elif (
                not np.all(np.isfinite(flat_a))
                or not np.all(np.isfinite(flat_b))
                or np.ptp(flat_a) == 0.0
                or np.ptp(flat_b) == 0.0
            ):
                corr = None
            else:
                with np.errstate(invalid='ignore'):
                    c = np.corrcoef(flat_a, flat_b)[0, 1]
                    corr = float(c) if not np.isnan(c) else None

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
            pair_corrs: Dict[str, Optional[float]] = {}
            refused_pairs: Dict[str, str] = {}

            for solver_a, solver_b in pairs:
                pk = self._pair_key(solver_a, solver_b)
                comp = rao_comparisons[pk][dof_name]
                corr = comp.magnitude_stats.correlation
                relative_rms = comp.relative_rms_error
                pair_corrs[pk] = corr

                refusal_quality = next(
                    (
                        quality
                        for quality in (
                            comp.magnitude_stats.quality,
                            comp.phase_stats.quality,
                        )
                        if quality in REFUSAL_QUALITIES
                    ),
                    None,
                )
                if refusal_quality is not None:
                    refused_pairs[pk] = comp.refusal_reason or refusal_quality
                    pair_agrees[pk] = False
                elif self.policy is None:
                    refused_pairs[pk] = "UNCONFIGURED_POLICY"
                    pair_agrees[pk] = False
                else:
                    pair_agrees[pk] = self._rao_comparison_agrees(comp)

            high_pairs = [
                pk for pk, agrees in pair_agrees.items() if agrees
            ]
            low_pairs = [
                pk for pk, agrees in pair_agrees.items() if not agrees
            ]

            available_corrs = [
                corr for corr in pair_corrs.values() if corr is not None
            ]
            mean_corr = (
                float(np.mean(available_corrs)) if available_corrs else None
            )

            refusal_reason = None
            if refused_pairs:
                level = None
                comparison_status = "REFUSED"
                refusal_reason = sorted(set(refused_pairs.values()))[0]
            else:
                comparison_status = "DECIDED"
                if len(pairs) == 1:
                    level = "FULL" if high_pairs else "NO_CONSENSUS"
                elif all(pair_agrees.values()):
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
            if comparison_status == "DECIDED" and level == "MAJORITY":
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
                comparison_status=comparison_status,
                refusal_reason=refusal_reason,
            )

        return consensus

    # ------------------------------------------------------------------
    # Report generation
    # ------------------------------------------------------------------

    def _assess_pair_agreement(
        self,
        rao_comps: Dict[str, PairwiseRAOComparison],
    ) -> Optional[str]:
        """Classify a pair using only the configured comparison policy."""
        if self.policy is None:
            return None
        agrees = all(
            self._rao_comparison_agrees(comparison)
            for comparison in rao_comps.values()
        )
        return "EXCELLENT" if agrees else "POOR"

    def _rao_comparison_agrees(
        self,
        comparison: PairwiseRAOComparison,
    ) -> bool:
        """Apply the absolute null floor or the active-response criteria."""
        if self.policy is None:
            return False
        if comparison.phase_stats.quality == "NULL_RESPONSE":
            return (
                comparison.magnitude_stats.rms_error
                <= self.policy.absolute_rms_floor
            )
        return (
            comparison.magnitude_stats.correlation is not None
            and comparison.magnitude_stats.correlation
            > self.policy.correlation_minimum
            and comparison.relative_rms_error is not None
            and comparison.relative_rms_error
            < self.policy.relative_rms_tolerance
        )

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
            am_quality = {
                k: v.quality for k, v in am_comparisons[pk].items()
            }
            damp_quality = {
                k: v.quality for k, v in damp_comparisons[pk].items()
            }

            refusal_reasons = {
                quality
                for quality in (*am_quality.values(), *damp_quality.values())
                if quality in REFUSAL_QUALITIES
            }
            refusal_reasons.update(
                comparison.refusal_reason or quality
                for comparison in rao_comparisons[pk].values()
                for quality in (
                    comparison.magnitude_stats.quality,
                    comparison.phase_stats.quality,
                )
                if quality in REFUSAL_QUALITIES
            )
            if self.policy is None:
                refusal_reasons.add("UNCONFIGURED_POLICY")

            agreement = (
                None
                if refusal_reasons
                else self._assess_pair_agreement(rao_comparisons[pk])
            )

            pairwise_results[pk] = PairwiseResult(
                solver_a=solver_a,
                solver_b=solver_b,
                rao_comparisons=rao_comparisons[pk],
                added_mass_correlations=am_corrs,
                damping_correlations=damp_corrs,
                overall_agreement=agreement,
                hydrostatic_comparison=hydro_comparisons.get(pk),
                added_mass_quality=am_quality,
                damping_quality=damp_quality,
                comparison_status=(
                    "REFUSED" if refusal_reasons else "DECIDED"
                ),
                refusal_reason=(
                    sorted(refusal_reasons)[0] if refusal_reasons else None
                ),
            )

        # Determine overall consensus from per-DOF levels
        levels = [m.consensus_level for m in consensus.values()]
        report_refusals = {
            reason
            for metrics in consensus.values()
            if metrics.comparison_status == "REFUSED"
            for reason in [metrics.refusal_reason]
            if reason is not None
        }
        report_refusals.update(
            result.refusal_reason
            for result in pairwise_results.values()
            if result.refusal_reason is not None
        )
        report_refusals.update(
            quality
            for result in pairwise_results.values()
            for quality in (
                *result.added_mass_quality.values(),
                *result.damping_quality.values(),
            )
            if quality in REFUSAL_QUALITIES
        )
        if report_refusals:
            overall = None
            comparison_status = "REFUSED"
        elif all(lv == "FULL" for lv in levels):
            overall = "FULL"
            comparison_status = "DECIDED"
        elif levels.count("FULL") + levels.count("MAJORITY") >= 4:
            overall = "MAJORITY"
            comparison_status = "DECIDED"
        elif any(lv in ("FULL", "MAJORITY") for lv in levels):
            overall = "SPLIT"
            comparison_status = "DECIDED"
        else:
            overall = "NO_CONSENSUS"
            comparison_status = "DECIDED"

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
            comparison_status=comparison_status,
            refusal_reasons=sorted(report_refusals),
            comparison_policy=(
                self.policy.to_dict() if self.policy is not None else None
            ),
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
            "comparison_status": report.comparison_status,
            "refusal_reasons": report.refusal_reasons,
            "comparison_policy": report.comparison_policy,
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
            json.dump(
                data,
                fh,
                indent=2,
                default=self._json_default,
                allow_nan=False,
            )

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
                    "magnitude_correlation": (
                        float(comp.magnitude_stats.correlation)
                        if comp.magnitude_stats.correlation is not None
                        else None
                    ),
                    "magnitude_quality": comp.magnitude_stats.quality,
                    "magnitude_rms_error": float(
                        comp.magnitude_stats.rms_error,
                    ),
                    "phase_correlation": (
                        float(comp.phase_stats.correlation)
                        if comp.phase_stats.correlation is not None
                        else None
                    ),
                    "phase_quality": comp.phase_stats.quality,
                    "max_magnitude_diff": float(comp.max_magnitude_diff),
                    "max_phase_diff": float(comp.max_phase_diff),
                    "refusal_reason": comp.refusal_reason,
                }
            am_corrs = {
                f"{k[0]},{k[1]}": float(v) if v is not None else None
                for k, v in pr.added_mass_correlations.items()
            }
            damp_corrs = {
                f"{k[0]},{k[1]}": float(v) if v is not None else None
                for k, v in pr.damping_correlations.items()
            }
            am_quality = {
                f"{k[0]},{k[1]}": v
                for k, v in pr.added_mass_quality.items()
            }
            damp_quality = {
                f"{k[0]},{k[1]}": v
                for k, v in pr.damping_quality.items()
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
                    "stiffness_matrix_correlation": (
                        float(hc.stiffness_matrix_correlation)
                        if hc.stiffness_matrix_correlation is not None
                        else None
                    ),
                }

            out[pk] = {
                "solver_a": pr.solver_a,
                "solver_b": pr.solver_b,
                "overall_agreement": pr.overall_agreement,
                "comparison_status": pr.comparison_status,
                "refusal_reason": pr.refusal_reason,
                "rao_comparisons": rao_dict,
                "added_mass_correlations": am_corrs,
                "damping_correlations": damp_corrs,
                "added_mass_quality": am_quality,
                "damping_quality": damp_quality,
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
                "comparison_status": cm.comparison_status,
                "refusal_reason": cm.refusal_reason,
                "mean_pairwise_correlation": (
                    float(cm.mean_pairwise_correlation)
                    if cm.mean_pairwise_correlation is not None
                    else None
                ),
                "outlier_solver": cm.outlier_solver,
                "agreement_pairs": [
                    list(pair) for pair in cm.agreement_pairs
                ],
            }
        return out
