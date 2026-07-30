"""Unit-consistency guard for cross-solver comparison (#1550 W1).

`MultiSolverComparator` differences raw matrix entries across solvers
(`_compare_matrix_set`). Before this guard nothing checked that the two
solvers expressed those entries in the same units, so an AQWA result in kg
could be differenced against an OrcaWave result in tonnes, silently producing
`mean_error` / `max_error` / `rms_error` wrong by 1000x.

Note this does NOT affect `compute_consensus`, which derives its verdict only
from `compare_raos()` (dimensionless m/m and deg/m) via `np.corrcoef`, itself
invariant under a uniform scale factor. The blast radius is the deviation
statistics and the rms gate that consumes them.
"""
from __future__ import annotations

import copy
from typing import Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    MultiSolverComparator,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


def _relabel(results: DiffractionResults, attr: str, units: Dict[str, str]) -> None:
    """Relabel every matrix in one coefficient set, leaving magnitudes alone."""
    for matrix in getattr(results, attr).matrices:
        matrix.units = dict(units)


class TestUnitConsistencyGuard:
    """A unit mismatch between two solvers must raise, not silently compare."""

    def test_rejects_added_mass_unit_mismatch(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - SolverB declares tonnes where SolverA declares kg
        modified = copy.deepcopy(two_identical_results)
        _relabel(
            modified["SolverB"],
            "added_mass",
            {"linear": "te", "angular": "te.m^2"},
        )

        # Act / Assert
        with pytest.raises(ValueError, match="[Uu]nit"):
            MultiSolverComparator(modified)

    def test_rejects_damping_unit_mismatch(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        modified = copy.deepcopy(two_identical_results)
        _relabel(
            modified["SolverB"],
            "damping",
            {"linear": "te/s", "angular": "te.m^2/s"},
        )

        # Act / Assert
        with pytest.raises(ValueError, match="[Uu]nit"):
            MultiSolverComparator(modified)

    def test_error_names_the_offending_solvers_and_units(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange
        modified = copy.deepcopy(two_identical_results)
        _relabel(
            modified["SolverB"],
            "added_mass",
            {"linear": "te", "angular": "te.m^2"},
        )

        # Act
        with pytest.raises(ValueError) as excinfo:
            MultiSolverComparator(modified)

        # Assert - the message must be actionable, not just "mismatch"
        message = str(excinfo.value)
        assert "added_mass" in message
        assert "te" in message
        assert "kg" in message

    def test_matching_units_still_construct(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Arrange - both sides relabelled consistently
        modified = copy.deepcopy(two_identical_results)
        for name in ("SolverA", "SolverB"):
            _relabel(
                modified[name],
                "added_mass",
                {"linear": "te", "angular": "te.m^2"},
            )

        # Act - must not raise; the guard is about agreement, not a fixed unit
        comparator = MultiSolverComparator(modified)

        # Assert
        assert comparator.solver_names == ["SolverA", "SolverB"]

    def test_unmodified_fixture_still_constructs(
        self, two_identical_results: Dict[str, DiffractionResults],
    ) -> None:
        # Guards against the check being too strict for the existing corpus.
        comparator = MultiSolverComparator(two_identical_results)
        assert comparator.solver_names == ["SolverA", "SolverB"]
