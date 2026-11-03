# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Worked synthetic coupling diagnostic; no solver or physical acceptance.

Inputs: first frequencies [1, 1.08, 1.16, 1.24, 1.32] rad/s and six evenly
spaced second frequencies over the same interval; each signed coefficient is
omega-1.16 in arbitrary consistent units. These are chosen algebraic inputs.
Method: piecewise linear interpolation preserves this affine function exactly.
The aligned vectors are [-0.16, -0.08, 0, 0.08, 0.16]. Their difference is zero,
so RMS=0 and Pearson r=sum(x*x)/sqrt(sum(x*x)*sum(x*x))=1.
Source: NumPy corrcoef and interp reference documentation, accessed 2026.
https://numpy.org/doc/stable/reference/generated/numpy.corrcoef.html
https://numpy.org/doc/stable/reference/generated/numpy.interp.html
"""
import json

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_evidence import matrix_diagnostic


def demonstrate():
    """Return compact inputs and answers for aligned, constant and refused data."""
    first_axis = np.array([1.0, 1.08, 1.16, 1.24, 1.32])
    second_axis = np.linspace(first_axis[0], first_axis[-1], 6)
    # Six rigid-body DOFs follow output_schemas.HydrodynamicMatrix.
    first = np.broadcast_to((first_axis - 1.16)[:, None, None], (5, 6, 6))
    second = np.broadcast_to((second_axis - 1.16)[:, None, None], (6, 6, 6))
    aligned = matrix_diagnostic(first, second, first_axis, second_axis)
    constants = matrix_diagnostic(np.ones_like(first), np.ones_like(second), first_axis, second_axis)
    missing = matrix_diagnostic(first, second, first_axis, None)
    return {
        "basis": "synthetic algebra; no physical acceptance criteria",
        "frequency_unit": "rad/s",
        "first_frequencies": first_axis.tolist(),
        "second_frequencies": second_axis.tolist(),
        "aligned": {
            "correlation": aligned["correlation"][0][0],
            "rms_error": aligned["rms_error"][0][0],
            "sample_count": aligned["sample_count"],
            "comparison_status": aligned["comparison_status"],
        },
        "constant": {"correlation": constants["correlation"][0][0], "quality": constants["quality"][0][0]},
        "missing_axis": {"correlation": missing["correlation"][0][0], "quality": missing["quality"][0][0]},
    }


if __name__ == "__main__":
    print(json.dumps(demonstrate(), indent=2, allow_nan=False))
