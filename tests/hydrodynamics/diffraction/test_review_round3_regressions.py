"""Regressions from the round-3 adversarial review of #1633.

Round 3 made the Unit Box perturbation multiplicative, which is correct, but
multiplication leaves a structurally-zero matrix entry at exactly 0.0. Both
legs of a comparison then carry identical all-zero vectors, and the
``np.array_equal`` short-circuit -- which is evaluated *before* the
zero-variance check -- assigned them ``correlation = 1.0``.

Measured on the committed evidence: matrix correlations equal to exactly 1.0
went from 0/216 at b2f9dc8d to 168/216 at 135bcf4d. That is the artifact
signature #1633 was filed about ("72 added-mass/damping correlations came out
at exactly 1.0 and read as perfect agreement"), reintroduced by the fix.

Pearson r is undefined for a constant vector whether or not the two vectors
are equal to each other, so the zero-variance test must come first.
"""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    DiffractionComparator,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    MultiSolverComparator,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    HydrostaticResults,
)


ZEROS = np.zeros((4, 3))
FREQS = np.linspace(0.2, 2.0, 4)


class TestConstantVectorsHaveNoCorrelation:
    """A constant vector has undefined correlation, equal or not."""

    def test_multi_solver_zero_vs_zero_is_insufficient_not_identical(self) -> None:
        stats = MultiSolverComparator._calculate_deviation_stats(
            ZEROS, ZEROS.copy(), FREQS,
        )

        assert stats.correlation is None
        assert stats.quality == "INSUFFICIENT_DATA"

    def test_comparison_framework_zero_vs_zero_is_insufficient(self) -> None:
        # The method takes `self` but never uses it, so an unbound call with
        # None is safe and avoids constructing a full comparator.
        stats = DiffractionComparator._calculate_deviation_stats(
            None, ZEROS, ZEROS.copy(), FREQS,
        )

        assert stats.correlation is None
        assert stats.quality == "INSUFFICIENT_DATA"

    def test_identical_nonconstant_vectors_still_report_identical(self) -> None:
        """The IDENTICAL branch must survive for genuinely varying input."""
        varying = np.array([[1.0, 2.0], [3.0, 4.0]])

        stats = MultiSolverComparator._calculate_deviation_stats(
            varying, varying.copy(), np.array([0.5, 1.0]),
        )

        assert stats.correlation == 1.0
        assert stats.quality == "IDENTICAL"

    def test_identical_nonzero_constant_is_insufficient(self) -> None:
        """A constant 5.0 vector is as undefined as a constant 0.0 one."""
        constant = np.full((3, 2), 5.0)

        stats = MultiSolverComparator._calculate_deviation_stats(
            constant, constant.copy(), np.array([0.5, 1.0, 1.5]),
        )

        assert stats.correlation is None
        assert stats.quality == "INSUFFICIENT_DATA"


class TestHydrostaticConstantStiffness:
    """compare_hydrostatics carries the same short-circuit ordering."""

    def test_identical_zero_stiffness_yields_no_correlation(
        self, two_identical_results,
    ) -> None:
        for name, res in two_identical_results.items():
            res.hydrostatics = HydrostaticResults(
                vessel_name=res.vessel_name,
                displacement_volume=1.0,
                mass=1025.0,
                centre_of_gravity=[0.0, 0.0, -0.25],
                centre_of_buoyancy=[0.0, 0.0, -0.5],
                waterplane_area=1.0,
                stiffness_matrix=np.zeros((6, 6)),
            )

        comparator = MultiSolverComparator(two_identical_results)
        comparisons = comparator.compare_hydrostatics()

        assert comparisons, "expected at least one solver pair"
        for comparison in comparisons.values():
            assert comparison.stiffness_matrix_correlation is None


class TestPhaseVariesAcrossSolvers:
    """Round 3 reseeded phase on dof.value alone, dropping the solver seed."""

    def test_unit_box_phase_differs_between_solvers(self) -> None:
        script = pytest.importorskip(
            "tests.hydrodynamics.diffraction.test_unit_box_benchmark",
        )
        a = script._build_unit_box_results("AQWA", seed=0)
        b = script._build_unit_box_results("OrcaWave", seed=1)

        assert not np.array_equal(
            a.raos.heave.phase, b.raos.heave.phase,
        ), "phase is bit-identical across solvers; an inverted phase convention "\
           "would report perfect agreement"
