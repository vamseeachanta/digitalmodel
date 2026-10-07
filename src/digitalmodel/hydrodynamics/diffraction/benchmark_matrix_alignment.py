"""Align signed radiation coefficients under the benchmark abscissa contract."""
from dataclasses import dataclass

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaConfig,
    AbscissaOrderError,
    InsufficientSampling,
    assess_sampling,
    build_evaluation_grid,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import AddedMassSet, DampingSet


@dataclass(frozen=True)
class AlignedMatrices:
    frequencies: np.ndarray
    first: np.ndarray
    second: np.ndarray


def _matrix_samples(matrix_set: AddedMassSet | DampingSet) -> tuple[np.ndarray, np.ndarray]:
    """Validate collection and per-matrix axes before positional access."""
    frequencies = np.asarray(matrix_set.frequencies.values, dtype=float)
    if (
        frequencies.ndim != 1
        or frequencies.size == 0
        or not np.all(np.isfinite(frequencies))
        or np.any(frequencies <= 0)
        or np.any(np.diff(frequencies) <= 0)
        or matrix_set.frequencies.count != frequencies.size
        or len(matrix_set.matrices) != frequencies.size
    ):
        raise AbscissaOrderError("matrix count and positive increasing axis must agree")
    actual_frequencies = np.asarray([matrix.frequency for matrix in matrix_set.matrices])
    if not np.array_equal(actual_frequencies, frequencies):
        raise AbscissaOrderError("per-matrix frequencies must match the collection axis")
    values = np.asarray([matrix.matrix for matrix in matrix_set.matrices], dtype=float)
    if values.shape != (frequencies.size, 6, 6):
        raise AbscissaOrderError("each matrix sample must have shape (6, 6)")
    return frequencies, values


def _interpolate_matrix(frequencies, values, grid) -> np.ndarray:
    """Interpolate real signed coefficients without magnitude conversion."""
    if np.array_equal(frequencies, grid):
        return values.copy()
    columns = values.reshape(frequencies.size, 36)
    aligned = np.empty((grid.size, 36), dtype=float)
    for index in range(36):
        column = columns[:, index]
        # An invalid sample outside the overlap must not disappear on resampling.
        aligned[:, index] = (
            np.interp(grid, frequencies, column)
            if np.all(np.isfinite(column)) else np.nan
        )
    return aligned.reshape(grid.size, 6, 6)


def align_matrix_sets(
    first: AddedMassSet | DampingSet,
    second: AddedMassSet | DampingSet,
    config: AbscissaConfig | None = None,
) -> AlignedMatrices | InsufficientSampling:
    """Apply the RAO grid/coverage/gap/sampling rules to real matrix values."""
    first_frequencies, first_values = _matrix_samples(first)
    second_frequencies, second_values = _matrix_samples(second)
    sampling = assess_sampling(first_frequencies, second_frequencies, config)
    if sampling is not None:
        return sampling
    grid = build_evaluation_grid(first_frequencies, second_frequencies, config)
    sampling = assess_sampling(grid, grid, config)
    if sampling is not None:
        return sampling
    return AlignedMatrices(
        grid,
        _interpolate_matrix(first_frequencies, first_values, grid),
        _interpolate_matrix(second_frequencies, second_values, grid),
    )
