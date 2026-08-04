"""Tests for MultiSolverComparator (multi-solver benchmark comparison)."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Dict

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaOverlapError,
)
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


@pytest.fixture(autouse=True)
def adequate_comparator_abscissae(request: pytest.FixtureRequest) -> None:
    """Keep generic RAO shapes while satisfying the comparator contract."""
    for fixture_name in ("two_identical_results", "three_solver_results"):
        if fixture_name not in request.fixturenames:
            continue
        solver_results = request.getfixturevalue(fixture_name)
        for results in solver_results.values():
            for dof in DOF:
                component = getattr(results.raos, dof.name.lower())
                component.frequencies.values = np.geomspace(
                    1.0,
                    2.0,
                    component.magnitude.shape[0],
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
        comparator = MultiSolverComparator(three_solver_results)

        # Assert - names should be alphabetically sorted
        assert comparator.solver_names == sorted(three_solver_results.keys())

    def test_init_default_tolerance(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Act
        comparator = MultiSolverComparator(two_identical_results)

        # Assert
        assert comparator.tolerance == pytest.approx(0.05)

    def test_init_custom_tolerance(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Act
        comparator = MultiSolverComparator(
            two_identical_results, tolerance=0.10,
        )

        # Assert
        assert comparator.tolerance == pytest.approx(0.10)


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
        comparator = MultiSolverComparator(two_identical_results)

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

    def test_disjoint_solver_grids_raise_overlap_error(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        for dof in DOF:
            component = getattr(
                two_identical_results["SolverB"].raos,
                dof.name.lower(),
            )
            component.frequencies.values = component.frequencies.values + 10.0
        comparator = MultiSolverComparator(two_identical_results)

        with pytest.raises(AbscissaOverlapError, match="abscissae are disjoint"):
            comparator.compare_raos()

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
            # Magnitude correlation should also be 1.0 (both zero)
            assert heave_comp.magnitude_stats.correlation == pytest.approx(
                1.0, abs=1e-6,
            )

    def test_null_response_dof_still_permits_full_consensus(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        for results in two_identical_results.values():
            results.raos.heave.magnitude.fill(0.0)
        comparator = MultiSolverComparator(two_identical_results)

        comparison = comparator.compare_raos()["SolverA-vs-SolverB"]["heave"]
        consensus = comparator.compute_consensus()["HEAVE"]

        assert (
            comparison.phase_stats.quality,
            consensus.consensus_level,
        ) == ("NULL_RESPONSE", "FULL")

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
        comparator = MultiSolverComparator(three_solver_results)

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
        comparator = MultiSolverComparator(two_identical_results)

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

        forward = MultiSolverComparator(two_identical_results).compute_consensus()
        reverse_results = {
            "SolverA": two_identical_results["SolverB"],
            "SolverB": two_identical_results["SolverA"],
        }
        reverse = MultiSolverComparator(reverse_results).compute_consensus()

        assert (
            forward["PITCH"].consensus_level,
            reverse["PITCH"].consensus_level,
        ) == ("FULL", "FULL")

    def test_consensus_majority_with_outlier(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - BEMRosetta has heave_bias=0.15
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        consensus = comparator.compute_consensus()

        # Assert - heave should show MAJORITY (or lower) due to bias
        heave_consensus = consensus["HEAVE"]
        assert heave_consensus.consensus_level in (
            "MAJORITY", "SPLIT", "NO_CONSENSUS",
        ), (
            f"Expected non-FULL consensus for HEAVE, "
            f"got {heave_consensus.consensus_level}"
        )

    def test_consensus_returns_all_dofs(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        consensus = comparator.compute_consensus()

        # Assert - all 6 DOFs present
        expected_dof_keys = {dof.name for dof in DOF}
        assert set(consensus.keys()) == expected_dof_keys

    def test_consensus_identifies_outlier_solver(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        consensus = comparator.compute_consensus()

        # Assert - if heave has an outlier, it should be BEMRosetta
        heave_consensus = consensus["HEAVE"]
        if heave_consensus.outlier_solver is not None:
            assert heave_consensus.outlier_solver == "BEMRosetta"

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

        assert consensus["HEAVE"].mean_pairwise_correlation is None

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


# ---------------------------------------------------------------------------
# 5. Report generation
# ---------------------------------------------------------------------------


class TestReportGeneration:
    """Verify BenchmarkReport structure and JSON export."""

    def test_generate_report_has_all_fields(
        self, three_solver_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)

        # Act
        report = comparator.generate_report()

        # Assert
        assert isinstance(report, BenchmarkReport)
        assert report.vessel_name == "TestVessel"
        assert len(report.solver_names) == 3
        assert report.comparison_date  # non-empty string
        assert report.overall_consensus in (
            "FULL", "MAJORITY", "SPLIT", "NO_CONSENSUS",
        )
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
