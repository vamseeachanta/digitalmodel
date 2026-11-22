# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared diagnostic refusals; no engineering acceptance thresholds are defined."""
from types import SimpleNamespace

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaGapError, AbscissaOrderError, AbscissaOverlapError, InsufficientSampling,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_matrix_alignment import align_matrix_sets


REFUSAL_QUALITIES = frozenset({
    "INSUFFICIENT_DATA", "INSUFFICIENT_SAMPLING", "UNTRUSTED_SOURCE",
    "INVALID_ABSCISSA", "ABSENT_DIAGONAL", "INVALID_HYDROSTATICS",
})


def matrix_diagnostic(first, second, first_axis, second_axis, config=None):
    """Align real signed arrays and retain per-cell quality and sampling.

    Pearson r = Cxy/sqrt(Cxx*Cyy), NumPy corrcoef reference (accessed 2026):
    https://numpy.org/doc/stable/reference/generated/numpy.corrcoef.html
    A zero denominator has no correlation. Six-DOF shape follows
    output_schemas.HydrodynamicMatrix; sampling follows benchmark_abscissa.
    RMS = sqrt(sum((b-a)**2)/n), by the definition of a root mean square.
    This diagnostic does not establish source provenance or acceptance.
    """
    from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator

    def collection(axis, values):
        if axis is None:
            raise AbscissaOrderError("explicit frequency axes are required")
        axis = np.asarray(axis, dtype=float)
        values = np.asarray(values, dtype=float)
        if axis.ndim != 1 or values.shape != (axis.size, 6, 6):
            raise AbscissaOrderError("matrix samples must match a six-DOF axis")
        return SimpleNamespace(
            frequencies=SimpleNamespace(values=axis, count=axis.size),
            matrices=[SimpleNamespace(frequency=f, matrix=m) for f, m in zip(axis, values)],
        )

    try:
        aligned = align_matrix_sets(collection(first_axis, first), collection(second_axis, second), config)
        refusal = "INSUFFICIENT_SAMPLING" if isinstance(aligned, InsufficientSampling) else None
    except (AbscissaOrderError, AbscissaOverlapError, AbscissaGapError, TypeError, ValueError):
        aligned, refusal = None, "INVALID_ABSCISSA"
    if refusal:
        return {
            "correlation": [[None] * 6 for _ in range(6)],
            "quality": [[refusal] * 6 for _ in range(6)],
            "rms_error": [[None] * 6 for _ in range(6)],
            "sample_count": None, "comparison_status": "REFUSED",
        }
    stats = [[MultiSolverComparator._calculate_deviation_stats(
        aligned.first[:, i, j], aligned.second[:, i, j], aligned.frequencies,
    ) for j in range(6)] for i in range(6)]
    refused = any(s.quality in REFUSAL_QUALITIES for row in stats for s in row)
    return {
        "correlation": [[s.correlation for s in row] for row in stats],
        "quality": [[s.quality for s in row] for row in stats],
        "rms_error": [[None if s.quality in REFUSAL_QUALITIES else s.rms_error for s in row] for row in stats],
        "sample_count": int(aligned.frequencies.size),
        "comparison_status": "REFUSED" if refused else "DIAGNOSTIC",
    }
