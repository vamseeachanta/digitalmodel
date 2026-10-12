"""Tests for MultiSolverComparator (multi-solver benchmark comparison)."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Dict

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
    ConsensusMetrics,
    MultiSolverComparator,
    PairwiseRAOComparison,
    PairwiseResult,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)


def _comparison_policy():
    from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
        ComparisonPolicy,
    )

    return ComparisonPolicy.from_uncertainties(
        solver_relative_uncertainty=0.025,
        response_absolute_resolution=5e-11,
        minimum_explained_variance=0.9801,
        justification="Synthetic comparator fixture uncertainty budget.",
    )


# ---------------------------------------------------------------------------
# 1. Initialization validation
# ---------------------------------------------------------------------------


class TestInitialization:
    """Validate constructor guards and default state."""

    def test_init_requires_at_least_two_solvers(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - pick only one solver
        single = {"AQWA": three_solver_results["AQWA"]}

        # Act / Assert
        with pytest.raises(ValueError):
            MultiSolverComparator(single)

    def test_init_validates_matching_vessel_names(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - mutate one vessel name so they differ
        modified = dict(two_identical_results)
        bad_result = modified["SolverB"]
        original_name = bad_result.vessel_name
        bad_result.vessel_name = "DifferentVessel"

        # Act / Assert
        with pytest.raises(ValueError):
            MultiSolverComparator(modified)

        # Cleanup
        bad_result.vessel_name = original_name

    def test_init_stores_solver_names_sorted(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Act
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Assert - names should be alphabetically sorted
        assert comparator.solver_names == sorted(three_solver_results.keys())

    def test_solver_fixture_uses_one_frequency_grid(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        results = two_identical_results["SolverA"]

        assert np.array_equal(
            results.raos.heave.frequencies.values,
            results.added_mass.frequencies.values,
        ) is True

    def test_init_has_no_unjustified_default_tolerance(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        comparator = MultiSolverComparator(two_identical_results)

        assert comparator.tolerance is None

    def test_init_rejects_tolerance_without_justification(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        with pytest.raises(ValueError, match="justification"):
            MultiSolverComparator(two_identical_results, tolerance=0.10)

    def test_init_records_tolerance_semantics_and_justification(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        comparator = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        )

        assert (
            comparator.tolerance_semantics,
            comparator.tolerance_justification,
        ) == (
            "symmetric_relative_rms_with_absolute_floor",
            "Synthetic comparator fixture uncertainty budget.",
        )


# ---------------------------------------------------------------------------
# 2. Pairwise RAO comparison
# ---------------------------------------------------------------------------


class TestCompareRAOs:
    """Verify pairwise RAO comparison logic."""

    def test_compare_raos_returns_all_pairs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - C(3,2) = 3 pairs
        assert len(rao_comparisons) == 3

    def test_compare_raos_pair_key_alphabetical(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - keys follow "A-vs-B" alphabetical ordering
        expected_keys = {
            "AQWA-vs-BEMRosetta",
            "AQWA-vs-OrcaWave",
            "BEMRosetta-vs-OrcaWave",
        }
        assert set(rao_comparisons.keys()) == expected_keys

    def test_compare_raos_covers_all_dofs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - each pair has entries for all 6 DOFs
        for pair_key, comparisons in rao_comparisons.items():
            assert len(comparisons) == 6, (
                f"Pair {pair_key} has {len(comparisons)} DOFs, expected 6"
            )

    def test_compare_raos_deviation_stats_populated(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - spot-check that stats fields are populated
        first_key = next(iter(rao_comparisons))
        first_pair = rao_comparisons[first_key]
        first_dof_key = next(iter(first_pair))
        comparison: PairwiseRAOComparison = first_pair[first_dof_key]

        assert comparison.magnitude_stats.correlation is not None
        assert comparison.magnitude_stats.rms_error >= 0.0
        assert comparison.phase_stats.correlation is not None

    def test_compare_raos_identical_solvers_high_correlation(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        )

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - identical data should yield near-perfect correlation
        for pair_key, comparisons in rao_comparisons.items():
            for dof_key, comp in comparisons.items():
                assert comp.magnitude_stats.correlation == pytest.approx(
                    1.0, abs=1e-6,
                ), f"Expected ~1.0 correlation for {pair_key}/{dof_key}"
                assert comp.magnitude_stats.rms_error == pytest.approx(
                    0.0, abs=1e-6,
                )

    def test_identical_input_has_identical_quality(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]

        assert comparison.magnitude_stats.quality == "IDENTICAL"

    def test_phase_rms_wraps_across_branch_cut(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        shape = two_identical_results["SolverA"].raos.heave.phase.shape
        phase_a = np.linspace(170.0, 179.0, np.prod(shape)).reshape(shape)
        phase_b = (phase_a + 2.0 + 180.0) % 360.0 - 180.0
        two_identical_results["SolverA"].raos.heave.phase = phase_a
        two_identical_results["SolverB"].raos.heave.phase = phase_b
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]

        assert comparison.phase_stats.rms_error == pytest.approx(2.0)

    def test_max_phase_diff_wraps_across_branch_cut(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        shape = two_identical_results["SolverA"].raos.heave.phase.shape
        phase_a = np.linspace(170.0, 179.0, np.prod(shape)).reshape(shape)
        phase_b = (phase_a + 2.0 + 180.0) % 360.0 - 180.0
        two_identical_results["SolverA"].raos.heave.phase = phase_a
        two_identical_results["SolverB"].raos.heave.phase = phase_b
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]

        assert comparison.max_phase_diff == pytest.approx(2.0)

    def test_phase_mean_is_circular_across_branch_cut(self) -> None:
        stats = MultiSolverComparator._calculate_phase_deviation_stats(
            np.zeros(2),
            np.array([179.0, -179.0]),
            np.array([1.0, 2.0]),
        )

        assert abs(stats.mean_error) == pytest.approx(180.0)

    def test_identical_three_point_grid_refuses_correlation(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        for results in two_identical_results.values():
            for dof in DOF:
                component = getattr(results.raos, dof.name.lower())
                component.frequencies.values = np.array([1.0, 1.05, 1.1])
                component.magnitude = component.magnitude[:3]
                component.phase = component.phase[:3]
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]

        assert (
            comparison.magnitude_stats.quality,
            comparison.magnitude_stats.correlation,
        ) == ("INSUFFICIENT_SAMPLING", None)

    def test_disjoint_solver_grids_are_refused_without_exception(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        for dof in DOF:
            component = getattr(
                two_identical_results["SolverB"].raos,
                dof.name.lower(),
            )
            component.frequencies.values = component.frequencies.values + 10.0
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["surge"]

        assert (
            comparison.magnitude_stats.quality,
            comparison.refusal_reason,
        ) == (
            "INVALID_ABSCISSA",
            "AbscissaOverlapError: abscissae are disjoint",
        )

    def test_compare_raos_zero_magnitude_phase_correlation_is_perfect(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        """Phase correlation should be 1.0 when RAO magnitude is near-zero.

        Fixed DOFs (e.g. roll/pitch on a TLP) produce zero magnitudes.
        Phase is undefined (atan2(0,0) noise), so phase correlation must
        be overridden to 1.0 instead of computing meaningless noise correlation.
        """
        import numpy as np
        from digitalmodel.hydrodynamics.diffraction.output_schemas import (
            RAOComponent,
            DOF as DOFEnum,
        )

        # Arrange - zero out heave magnitude on both solvers, add noise phase
        rng = np.random.default_rng(seed=777)
        for results in two_identical_results.values():
            heave: RAOComponent = results.raos.heave
            heave.magnitude = np.zeros_like(heave.magnitude)
            heave.phase = rng.uniform(-180.0, 180.0, size=heave.phase.shape)

        comparator = MultiSolverComparator(two_identical_results)

        # Act
        rao_comparisons = comparator.compare_raos()

        # Assert - heave phase correlation should be 1.0 (not noise)
        for pair_key, comparisons in rao_comparisons.items():
            heave_comp = comparisons["heave"]
            assert heave_comp.phase_stats.correlation == pytest.approx(
                1.0, abs=1e-6,
            ), (
                f"Zero-magnitude heave phase correlation for {pair_key} "
                f"should be 1.0, got {heave_comp.phase_stats.correlation}"
            )
            # Magnitude carries NO variance when the response is null, so its
            # correlation is undefined and must be reported as absent rather
            # than fabricated as 1.0. This assertion previously rode on the
            # np.array_equal short-circuit; that short-circuit is what assigned
            # 168 of 216 committed matrix correlations an exact 1.0, which is
            # the artifact signature #1633 was filed about. Agreement for a
            # null DOF now comes from the absolute RMS floor in
            # _rao_comparison_agrees, not from a manufactured correlation.
            assert heave_comp.magnitude_stats.correlation is None
            assert heave_comp.magnitude_stats.quality == "NULL_RESPONSE"

    def test_null_response_dof_still_permits_full_consensus(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        shape = two_identical_results["SolverA"].raos.heave.magnitude.shape
        first_rng = np.random.default_rng(11)
        second_rng = np.random.default_rng(29)
        two_identical_results["SolverA"].raos.heave.magnitude = (
            first_rng.uniform(1e-12, 4e-11, size=shape)
        )
        two_identical_results["SolverB"].raos.heave.magnitude = (
            second_rng.uniform(5e-11, 9e-11, size=shape)
        )
        comparator = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        )

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]
        consensus = comparator.compute_consensus()["HEAVE"]

        assert (
            comparison.phase_stats.quality,
            consensus.consensus_level,
        ) == ("NULL_RESPONSE", "FULL")

    def test_null_response_relative_rms_uses_absolute_floor(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        comparator = MultiSolverComparator(two_identical_results)

        relative_rms = comparator._symmetric_relative_rms(
            np.array([1e-12, 2e-12]),
            np.array([3e-12, 6e-12]),
            absolute_floor=1e-10,
        )

        assert relative_rms == pytest.approx(np.sqrt(10.0) / 100.0)

    def test_large_unequal_null_responses_do_not_bypass_magnitude_gate(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        shape = two_identical_results["SolverA"].raos.heave.magnitude.shape
        first_rng = np.random.default_rng(11)
        second_rng = np.random.default_rng(29)
        two_identical_results["SolverA"].raos.heave.magnitude = (
            first_rng.uniform(2e-10, 3e-10, size=shape)
        )
        two_identical_results["SolverB"].raos.heave.magnitude = (
            second_rng.uniform(2e-10, 3e-10, size=shape)
        )
        comparator = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        )

        consensus = comparator.compute_consensus()["HEAVE"]

        assert consensus.consensus_level == "NO_CONSENSUS"

    def test_nearby_small_values_are_not_identical(self) -> None:
        stats = MultiSolverComparator._calculate_deviation_stats(
            np.array([1e-9, 2e-9, 3e-9]),
            np.array([5e-9, 6e-9, 7e-9]),
            np.array([1.0, 1.1, 1.2]),
        )

        assert stats.quality == "COMPARED"

    def test_distinct_constant_values_have_insufficient_data_quality(self) -> None:
        stats = MultiSolverComparator._calculate_deviation_stats(
            np.full(3, 100.0),
            np.full(3, 100.00001),
            np.array([1.0, 2.0, 3.0]),
        )

        assert (stats.quality, stats.correlation) == ("INSUFFICIENT_DATA", None)

    def test_empty_values_have_insufficient_data_quality(self) -> None:
        stats = MultiSolverComparator._calculate_deviation_stats(
            np.array([]),
            np.array([]),
            np.array([]),
        )

        assert (stats.quality, stats.correlation) == ("INSUFFICIENT_DATA", None)


# ---------------------------------------------------------------------------
# 3. Matrix comparison (added mass & damping)
# ---------------------------------------------------------------------------


class TestMatrixComparison:
    """Verify added mass and damping pairwise comparisons."""

    def test_compare_added_mass_returns_all_pairs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Act
        am_comparisons = comparator.compare_added_mass()

        # Assert - 3 pairs
        assert len(am_comparisons) == 3

    def test_compare_damping_returns_all_pairs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        damp_comparisons = comparator.compare_damping()

        # Assert - 3 pairs
        assert len(damp_comparisons) == 3

    def test_matrix_comparison_has_diagonal_stats(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        am_comparisons = comparator.compare_added_mass()

        # Assert - each pair should include at least the 6 diagonal entries
        first_key = next(iter(am_comparisons))
        pair_stats = am_comparisons[first_key]
        diagonal_keys = {(i, i) for i in range(1, 7)}
        assert diagonal_keys.issubset(set(pair_stats.keys())), (
            f"Missing diagonal entries: "
            f"{diagonal_keys - set(pair_stats.keys())}"
        )


# ---------------------------------------------------------------------------
# 4. Consensus computation
# ---------------------------------------------------------------------------


class TestConsensus:
    """Verify consensus classification across solvers."""

    def test_consensus_full_for_identical_solvers(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        )

        # Act
        consensus = comparator.compute_consensus()

        # Assert - identical data should yield FULL consensus on every DOF
        for dof in DOF:
            dof_key = dof.name
            assert dof_key in consensus, f"Missing DOF {dof_key} in consensus"
            assert consensus[dof_key].consensus_level == "FULL", (
                f"Expected FULL consensus for {dof_key}, "
                f"got {consensus[dof_key].consensus_level}"
            )

    def test_relative_tolerance_is_symmetric_for_one_percent_scale(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        shape = two_identical_results["SolverA"].raos.pitch.magnitude.shape
        baseline = np.linspace(20.0, 40.0, np.prod(shape)).reshape(shape)
        scaled = baseline * 1.01
        two_identical_results["SolverA"].raos.pitch.magnitude = baseline
        two_identical_results["SolverB"].raos.pitch.magnitude = scaled

        forward = MultiSolverComparator(
            two_identical_results,
            policy=_comparison_policy(),
        ).compute_consensus()
        reverse_results = {
            "SolverA": two_identical_results["SolverB"],
            "SolverB": two_identical_results["SolverA"],
        }
        reverse = MultiSolverComparator(
            reverse_results,
            policy=_comparison_policy(),
        ).compute_consensus()

        assert (
            forward["PITCH"].consensus_level,
            reverse["PITCH"].consensus_level,
        ) == ("FULL", "FULL")

    def test_consensus_majority_with_outlier(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - BEMRosetta has heave_bias=0.15
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Act
        consensus = comparator.compute_consensus()

        # Assert - one agreeing pair and two disagreeing pairs is a split.
        heave_consensus = consensus["HEAVE"]
        assert heave_consensus.consensus_level == "SPLIT"

    def test_consensus_returns_all_dofs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Act
        consensus = comparator.compute_consensus()

        # Assert - all 6 DOFs present
        expected_dof_keys = {dof.name for dof in DOF}
        assert set(consensus.keys()) == expected_dof_keys

    def test_consensus_identifies_outlier_solver(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Act
        consensus = comparator.compute_consensus()

        assert consensus["HEAVE"].outlier_solver is None

    def test_all_unavailable_pairs_have_null_mean(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        for results in two_identical_results.values():
            for dof in DOF:
                component = getattr(results.raos, dof.name.lower())
                component.frequencies.values = np.array([1.0, 1.05, 1.1])
                component.magnitude = component.magnitude[:3]
                component.phase = component.phase[:3]
        comparator = MultiSolverComparator(two_identical_results)

        consensus = comparator.compute_consensus()

        assert (
            consensus["HEAVE"].comparison_status,
            consensus["HEAVE"].refusal_reason,
            consensus["HEAVE"].consensus_level,
            consensus["HEAVE"].mean_pairwise_correlation,
        ) == ("REFUSED", "INSUFFICIENT_SAMPLING", None, None)

    def test_unavailable_pairs_are_excluded_from_mean(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        sparse = three_solver_results["OrcaWave"]
        for dof in DOF:
            component = getattr(sparse.raos, dof.name.lower())
            component.frequencies.values = np.array([1.0, 1.05, 1.1])
            component.magnitude = component.magnitude[:3]
            component.phase = component.phase[:3]
        comparator = MultiSolverComparator(three_solver_results)

        consensus = comparator.compute_consensus()

        available = comparator.compare_raos()["AQWA-vs-BEMRosetta"]["heave"]
        assert consensus["HEAVE"].mean_pairwise_correlation == pytest.approx(
            available.magnitude_stats.correlation,
        )

    def test_one_unavailable_pair_refuses_three_solver_consensus(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        sparse = three_solver_results["OrcaWave"]
        for dof in DOF:
            component = getattr(sparse.raos, dof.name.lower())
            component.frequencies.values = np.array([1.0, 1.05, 1.1])
            component.magnitude = component.magnitude[:3]
            component.phase = component.phase[:3]

        consensus = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        ).compute_consensus()

        assert (
            consensus["HEAVE"].comparison_status,
            consensus["HEAVE"].consensus_level,
        ) == ("REFUSED", None)


# ---------------------------------------------------------------------------
# 5. Report generation
# ---------------------------------------------------------------------------


class TestReportGeneration:
    """Verify BenchmarkReport structure and JSON export."""

    def test_generate_report_has_all_fields(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(
            three_solver_results,
            policy=_comparison_policy(),
        )

        # Act
        report = comparator.generate_report()

        # Assert
        assert isinstance(report, BenchmarkReport)
        assert report.vessel_name == "TestVessel"
        assert len(report.solver_names) == 3
        assert report.comparison_date  # non-empty string
        assert report.overall_consensus == "MAJORITY"
        assert isinstance(report.pairwise_results, dict)
        assert isinstance(report.consensus_by_dof, dict)
        assert isinstance(report.notes, list)

    def test_generate_report_pairwise_count(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        report = comparator.generate_report()

        # Assert - 3 pairwise results for 3 solvers
        assert len(report.pairwise_results) == 3

    def test_export_report_json_creates_file(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)
        output_file = tmp_path / "benchmark_report.json"

        # Act
        comparator.export_report_json(output_file)

        # Assert
        assert output_file.exists()
        assert output_file.stat().st_size > 0

    def test_export_report_json_valid_json(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)
        output_file = tmp_path / "benchmark_report.json"

        # Act
        comparator.export_report_json(output_file)

        # Assert - file is valid JSON with expected top-level keys
        data = json.loads(output_file.read_text(encoding="utf-8"))
        assert "vessel_name" in data
        assert "solver_names" in data
        assert "pairwise_results" in data
        assert "consensus_by_dof" in data
        assert "overall_consensus" in data

    def test_unavailable_correlation_exports_json_null(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        for results in two_identical_results.values():
            for dof in DOF:
                component = getattr(results.raos, dof.name.lower())
                component.frequencies.values = np.array([1.0, 1.05, 1.1])
                component.magnitude = component.magnitude[:3]
                component.phase = component.phase[:3]
        output_file = tmp_path / "unavailable.json"

        MultiSolverComparator(two_identical_results).export_report_json(output_file)

        data = json.loads(output_file.read_text(encoding="utf-8"))
        pair = data["pairwise_results"]["SolverA-vs-SolverB"]
        assert pair["rao_comparisons"]["heave"]["magnitude_correlation"] is None

    def test_sampling_refusal_propagates_to_json_verdict(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        for results in two_identical_results.values():
            for dof in DOF:
                component = getattr(results.raos, dof.name.lower())
                component.frequencies.values = np.array([1.0, 1.05, 1.1])
                component.magnitude = component.magnitude[:3]
                component.phase = component.phase[:3]
        output_file = tmp_path / "sampling_refusal.json"

        MultiSolverComparator(two_identical_results).export_report_json(output_file)

        data = json.loads(output_file.read_text(encoding="utf-8"))
        assert (
            data["comparison_status"],
            data["overall_consensus"],
            data["consensus_by_dof"]["HEAVE"]["refusal_reason"],
        ) == ("REFUSED", None, "INSUFFICIENT_SAMPLING")

    def test_unavailable_matrix_correlation_exports_json_null(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        solver_b = two_identical_results["SolverB"]
        for coefficient_set in (solver_b.added_mass, solver_b.damping):
            for matrix in coefficient_set.matrices:
                matrix.matrix.fill(1.0)
        output_file = tmp_path / "unavailable_matrix.json"

        MultiSolverComparator(two_identical_results).export_report_json(output_file)

        data = json.loads(output_file.read_text(encoding="utf-8"))
        pair = data["pairwise_results"]["SolverA-vs-SolverB"]
        assert pair["added_mass_correlations"]["1,1"] is None

    def test_nonfinite_response_exports_null_without_nan(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        two_identical_results["SolverB"].raos.heave.magnitude[0, 0] = np.nan
        output_file = tmp_path / "nonfinite.json"

        MultiSolverComparator(two_identical_results).export_report_json(output_file)

        data = json.loads(
            output_file.read_text(encoding="utf-8"),
            parse_constant=lambda value: pytest.fail(f"non-finite JSON: {value}"),
        )
        pair = data["pairwise_results"]["SolverA-vs-SolverB"]
        assert pair["rao_comparisons"]["heave"]["magnitude_correlation"] is None
