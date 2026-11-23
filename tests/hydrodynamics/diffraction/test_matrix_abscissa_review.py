"""Synthetic matrix-grid and unexplained-zero regressions for issue 1633."""
import copy

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator


def set_matrix_grid(result, family, frequencies):
    matrix_set = getattr(result, family)
    template = matrix_set.matrices[0]
    matrix_set.frequencies.values = np.asarray(frequencies, dtype=float)
    matrix_set.frequencies.count = len(frequencies)
    matrix_set.matrices = []
    for frequency in frequencies:
        matrix = copy.deepcopy(template)
        matrix.frequency = float(frequency)
        # Signed linear response crossing zero; interpolation must retain sign.
        matrix.matrix = np.full((6, 6), frequency - 1.2)
        matrix_set.matrices.append(matrix)


@pytest.fixture(params=["added_mass", "damping"])
def matrix_case(request, two_identical_results):
    results = two_identical_results
    first, second = results.values()
    return results, first, second, request.param


def compare_cell(case):
    results, _, _, family = case
    comparator = MultiSolverComparator(results)
    return next(iter(comparator._compare_matrix_set(family).values()))[(1, 1)]


@pytest.mark.parametrize("second_grid", [np.linspace(1.1, 1.5, 9), np.linspace(2, 2.4, 9)])
def test_shifted_grid_cannot_compare_by_index(matrix_case, second_grid):
    _, first, second, family = matrix_case
    set_matrix_grid(first, family, np.linspace(1, 1.4, 9))
    set_matrix_grid(second, family, second_grid)
    stats = compare_cell(matrix_case)
    assert stats.quality == "INVALID_ABSCISSA"
    assert stats.correlation is None


@pytest.mark.parametrize("count", [1, 3])
def test_short_matrix_source_refuses_without_broadcast(matrix_case, count):
    _, first, second, family = matrix_case
    set_matrix_grid(first, family, np.linspace(1, 1.4, 9))
    set_matrix_grid(second, family, np.linspace(1, 1.4, count))
    stats = compare_cell(matrix_case)
    assert stats.quality == "INSUFFICIENT_SAMPLING"
    assert stats.correlation is None


def test_same_short_grid_refuses(matrix_case):
    _, first, second, family = matrix_case
    for result in (first, second):
        set_matrix_grid(result, family, np.linspace(1, 1.1, 3))
    assert compare_cell(matrix_case).quality == "INSUFFICIENT_SAMPLING"


def test_valid_different_grids_align_signed_values(matrix_case):
    _, first, second, family = matrix_case
    set_matrix_grid(first, family, np.linspace(1, 1.4, 9))
    set_matrix_grid(second, family, np.linspace(1, 1.4, 13))
    stats = compare_cell(matrix_case)
    assert stats.quality in {"IDENTICAL", "COMPARED"}
    assert stats.rms_error == pytest.approx(0, abs=1e-14)
    np.testing.assert_array_equal(stats.frequencies, np.linspace(1, 1.4, 9))
    assert stats.correlation == pytest.approx(1)


@pytest.mark.parametrize("damage", ["count", "frequency", "axis_count", "descending", "nonfinite"])
def test_matrix_metadata_must_match_consumed_axis(matrix_case, damage):
    _, first, second, family = matrix_case
    for result in (first, second):
        set_matrix_grid(result, family, np.linspace(1, 1.4, 9))
    matrix_set = getattr(second, family)
    if damage == "count":
        matrix_set.matrices.pop()
    elif damage == "frequency":
        matrix_set.matrices[3].frequency += 0.01
    elif damage == "axis_count":
        matrix_set.frequencies.count += 1
    elif damage == "descending":
        matrix_set.frequencies.values = matrix_set.frequencies.values[::-1]
        matrix_set.matrices.reverse()
    else:
        matrix_set.frequencies.values[3] = float("nan")
    stats = compare_cell(matrix_case)
    assert stats.quality == "INVALID_ABSCISSA"
    assert stats.correlation is None


def test_rotational_zero_remains_unexplained_refusal(matrix_case):
    results, first, second, family = matrix_case
    for result in (first, second):
        for matrix in getattr(result, family).matrices:
            matrix.matrix[5, :] = 0
            matrix.matrix[:, 5] = 0
    comparator = MultiSolverComparator(results)
    stats = next(iter(comparator._compare_matrix_set(family).values()))[(6, 6)]
    assert stats.quality == "ABSENT_DIAGONAL"
    assert stats.correlation is None
    assert "ABSENT_DIAGONAL" in comparator.generate_report().refusal_reasons
