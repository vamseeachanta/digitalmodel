# NumPy integration compatibility

Numerical integration uses a per-file binding that selects the available NumPy
API without changing the trapezoidal rule, integration arguments or tolerances.
This applies to package code, tests, examples (including notebook code cells),
legacy scripts and API scripts.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-20 | Document the compatible binding and an independent hand calculation. |

## Binding

Place this assignment after the imports and use `_trapezoid` for integration:

```python
import numpy as np

_trapezoid = getattr(np, "trapezoid", None) or np.trapz
```

The second operand is evaluated only when the modern API is absent. An eager
fallback such as `getattr(np, "trapezoid", np.trapz)` accesses the removed
attribute even when the modern function exists.

NumPy documents the replacement in its
[2.0 release notes](https://numpy.org/doc/2.0/release/2.0.0-notes.html#deprecations)
and the removal in its
[2.4 release notes](https://numpy.org/doc/2.4/release/2.4.0-notes.html#removal-of-deprecated-functions-and-arguments).
This compatibility change does not raise the package dependency constraint.

## Worked example

The [NumPy reference, Notes and Examples](https://numpy.org/doc/2.4/reference/generated/numpy.trapezoid.html)
defines integration over successive samples without sorting the coordinates.
Applying the single-segment trapezoidal rule in
[NIST DLMF (2010), Eq. 3.5.1](https://dlmf.nist.gov/3.5#E1) to the piecewise
linear interpolant gives the composite rule

\[
I = \sum_i (x_{i+1}-x_i)\frac{y_i+y_{i+1}}{2}.
\]

Use synthetic inputs `x = [0, 1, 3]` and `y = 2*x + 1 = [1, 3, 7]`.
The first segment contributes `(1-0)*(1+3)/2 = 2`; the second contributes
`(3-1)*(3+7)/2 = 10`. The answer is `12`, also obtained from the exact
antiderivative `[x*x + x]` between `0` and `3`. These values are constructed
inputs and hand calculations, rather than engineering coefficients.

```python
x = np.array([0.0, 1.0, 3.0])
y = 2 * x + 1
answer = _trapezoid(y, x)
assert answer == 12
```

Reversing both arrays gives `-12`. Omitting `x` and supplying `dx=2` gives
`2*((1+3)/2 + (3+7)/2) = 14`. Multiplying `y` by `1+1j` gives `12+12j`.
Stacking two copies of `y` and integrating on `axis=1` gives `[12, 12]`.
These variants exercise coordinate order, spacing, complex values and axes.

## Regression coverage and limits

`tests/scripts/test_cfd_scripts_numpy_compat.py` scans Python files and notebook
code cells under the five source trees. It rejects direct NumPy legacy access,
including alias imports, except for one exact module-level fallback per file.
Comments, strings, notebook prose and other libraries are not NumPy calls.
The guard also tests synthetic legacy-only, modern-only and combined APIs.

The independent hand-integral tests preserve existing reference values and add
spacing, axis, reverse-order and complex-valued cases. The conversion preserves
all existing integration arguments, assertions and tolerances. It removes two
obsolete test-side mutations of the NumPy namespace so tests exercise production
compatibility directly.

Full example execution still requires the examples' data and optional tools.
The compatibility guard reads notebook source; it does not execute the complete
notebook. Other NumPy API migrations and physical-model validation are outside
this change.
