# Hydro-coefficient interpolation coverage audit

This executable example audits only
`digitalmodel.orcawave.hydro_coefficients.interpolate_matrix_at_frequency`.
It checks every entry of synthetic symmetric matrices at interior frequencies
and exact knots. Existing tests check the first diagonal entry; this example
adds distinct diagonal and off-diagonal laws and an executable documentation
check. It does not establish physical benchmark validation.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-26 | Added a hand-derived full-matrix example to cover coupling entries and exact knots. |

## Existing interpolation policy

The implementation is in
[hydro_coefficients.py](../../src/digitalmodel/orcawave/hydro_coefficients.py).
The caller supplies matrices sorted by increasing angular frequency in rad/s.
For an interior query, the helper applies linear interpolation independently
to every matrix entry between the enclosing samples. Exact knots reproduce
the sampled matrix. Queries below or above the sampled range return the
nearest endpoint matrix rather than extrapolating. An empty list raises
`ValueError`; a single sample is returned at any query frequency.

The helper does not sort inputs, validate strictly increasing frequencies,
or impose symmetry. Unsorted or duplicate frequencies are outside this audit.
Symmetry is retained here because both input matrices are symmetric and the
same scalar interpolation weight is applied to every entry. No implementation
or interpolation policy is changed.

## Hand calculation

All numerical inputs below are defined by this synthetic construction, not by
a publication, physical dataset, or solver output. The matrix size follows the
`HydroMatrix6x6` interface. Coefficient values have no physical interpretation;
no mass, damping, or mixed translational and rotational units are inferred.

For one-based row and column indices `i, j` from 1 through 6, define
`u = min(i, j)` and `v = max(i, j)`. Specify the intercept `C` and slope `S`
independently of the interpolation routine:

- Diagonal: `C[i,i] = 100*i` and `S[i,i] = 2*i`.
- Off-diagonal: `C[i,j] = 10*u + v` and `S[i,j] = u - v`.
- Each entry follows `M[i,j](omega) = C[i,j] + omega*S[i,j]`.

The scale factors 100, 2, and 10 are chosen synthetic constants that distinguish
diagonal from coupling entries. Negative coupling slopes also exercise a sign
change. Sample knots are 1.0, 3.0, and 5.0 rad/s. Interior queries are 2.0 rad/s
and 4.5 rad/s, exercising the first and second intervals with different weights.
These frequencies are chosen so all arithmetic is exactly representable in
binary; the checks require equality without a tolerance.

For example, `M[1,1] = 100 + 2*omega`, `M[1,6] = 16 - 5*omega`, and
`M[2,3] = 23 - omega`. Direct substitution gives the following hand checks.
The same expression supplies the expected value of every entry in the code.

| Query omega (rad/s) | M[1,1] | M[1,6] = M[6,1] | M[2,3] = M[3,2] |
| --- | --- | --- | --- |
| 1.0 | 102.0 | 11.0 | 22.0 |
| 2.0 | 104.0 | 6.0 | 21.0 |
| 3.0 | 106.0 | 1.0 | 20.0 |
| 4.5 | 109.0 | -6.5 | 18.5 |
| 5.0 | 110.0 | -9.0 | 18.0 |

## Executable example

From the repository root, run the snippet with `PYTHONPATH=src` in an environment
containing NumPy and Pydantic. It needs no source data or licensed solver.
The smoke test below executes this exact code block in a fresh Python process
and rejects any attempt to import the licensed solver API.

```python
import numpy as np

from digitalmodel.orcawave.hydro_coefficients import (
    HydroMatrix6x6,
    interpolate_matrix_at_frequency,
)

# Explicit intercepts and slopes from Hand calculation above.
intercept = np.array([
    [100, 12, 13, 14, 15, 16],
    [12, 200, 23, 24, 25, 26],
    [13, 23, 300, 34, 35, 36],
    [14, 24, 34, 400, 45, 46],
    [15, 25, 35, 45, 500, 56],
    [16, 26, 36, 46, 56, 600],
])
slope = np.array([
    [2, -1, -2, -3, -4, -5],
    [-1, 4, -1, -2, -3, -4],
    [-2, -1, 6, -1, -2, -3],
    [-3, -2, -1, 8, -1, -2],
    [-4, -3, -2, -1, 10, -1],
    [-5, -4, -3, -2, -1, 12],
])
matrices = [
    HydroMatrix6x6(
        frequency_rad_s=omega,
        values=(intercept + omega * slope).tolist(),
    )
    for omega in (1.0, 3.0, 5.0)
]

for omega in (1.0, 2.0, 3.0, 4.5, 5.0):
    # Independent scalar oracle: the hand affine law, not interpolation output.
    expected = np.array([
        [
            100 * i + 2 * i * omega if i == j
            else 10 * min(i, j) + max(i, j) + (min(i, j) - max(i, j)) * omega
            for j in range(1, 7)
        ]
        for i in range(1, 7)
    ])
    actual = interpolate_matrix_at_frequency(matrices, omega)
    np.testing.assert_array_equal(actual, expected)
    np.testing.assert_array_equal(actual, actual.T)

print("Interpolation audit passed.")
```

Run the complete module test file, including the snippet smoke test, with:

```bash
PYTHONPATH=src uv run --no-project --with pytest --with 'numpy>=1.24,<2' --with 'pydantic>=2.7,<3' python -m pytest tests/orcawave/test_hydro_coefficients.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-interpolation"
```

`TMPDIR` must refer to temporary storage outside the checkout. The explicit
runtime dependencies allow the example to run without installing the full
application or its optional integrations.

## Scope and remaining work

This is a bounded slice of issue #1802, which remains open. Existing passing
tests are retained. This example makes no claim about solver accuracy, physical
added-mass or damping validity, excitation interpolation, interchange formats,
hydrostatics, other hydrodynamics modules, or related repositories. The parent
audit still needs a complete coverage inventory and gap assessment across
those modules and repositories; this example supplies no coverage percentage
for them.
