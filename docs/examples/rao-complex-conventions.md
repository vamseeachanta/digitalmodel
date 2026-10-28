# Complex RAO conversion conventions

## Summary

The existing `digitalmodel.orcawave.rao_processing` conversion functions map
amplitude and phase in degrees to complex response and back. This executable
note checks the mathematical convention using synthetic responses, with no
solver input, solver import or solver execution. The formulas are unchanged.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-26 | Added a runnable convention example and independent checks to expose phase units and wrapping. |

## Mathematical convention

For nonnegative amplitude `A` and phase `phi_deg`, define
`theta = phi_deg * pi / 180`. The forward mapping is
`z = A * (cos(theta) + j*sin(theta))`, where `j*j = -1`.
Thus `Re(z) = A*cos(theta)` and `Im(z) = A*sin(theta)`.
Positive phase rotates counterclockwise from the positive real axis.
The inverse uses `A = sqrt(Re(z)**2 + Im(z)**2)` and
`phi_deg = atan2(Im(z), Re(z)) * 180 / pi`.
The degree conversion follows from a full turn being `360` degrees or `2*pi`
radians; the imaginary unit and modulus follow from rectangular complex algebra.

The inverse returns a principal phase between `-180` and `180` degrees;
either endpoint can represent the negative real axis depending on signed zero
and floating-point residuals. Phases separated by any integer multiple of
`360` degrees represent the same complex response, by sine/cosine periodicity.
Compare the reconstructed response, rather than requiring an unwrapped input
phase to return unchanged. At zero amplitude the phase is undefined physically;
any returned numerical angle is not evidence of a physical phase.

For example, with a reconstruction convention `Re(z * exp(j*omega*t))`,
the signal is `A*cos(omega*t + theta)`. Changing the sign of the time exponent
changes the interpretation of lead and lag. These conversion functions do not
establish a solver's time exponent, wave reference, coordinate system, origin,
response units or phase sign. Those require separate documented evidence.

## Synthetic calculation basis

All amplitudes below are the freely chosen synthetic value `2`, in consistent
response units. They are not measured or computed hydrodynamic coefficients.
The angle choices and expected components are independent hand calculations:

| Phase (degrees) | Real component | Imaginary component | Calculation |
| --- | --- | --- | --- |
| `0` | `2` | `0` | Positive real axis: cosine is unity, sine is zero. |
| `90` | `0` | `2` | Positive imaginary axis: cosine is zero, sine is unity. |
| `180` | `-2` | `0` | Negative real axis: cosine is negative unity. |
| `270` | `0` | `-2` | Negative imaginary axis: sine is negative unity. |
| `30` | `sqrt(3)` | `1` | Bisect an equilateral triangle: cosine is `sqrt(3)/2`, sine is `1/2`. |
| `120` | `-1` | `sqrt(3)` | Reflect the complementary triangle into the second quadrant. |
| `-135` | `-sqrt(2)` | `-sqrt(2)` | Equal legs in the third quadrant: each unit component is `-1/sqrt(2)`. |
| `390` | `sqrt(3)` | `1` | `30 + 360`: periodicity preserves both components. |
| `-240` | `-1` | `sqrt(3)` | `120 - 360`: periodicity preserves both components. |

Table: independently derived synthetic expectations for the executable example
and the parameterized tests. The test also evaluates scalar trigonometry using
`math.radians`, `math.cos` and `math.sin`, separately from the NumPy conversion.
The inverse check starts from the hand-calculated rectangular components.

The absolute error budget `1e-12` is a numerical test allowance for these
order-unity synthetic values and floating-point trigonometry at wrapped angles;
relative tolerance is zero. This allowance accommodates residuals at exact axes.
It is not an engineering accuracy criterion. No existing test tolerance changes.

## Executable example

From the repository root, use `PYTHONPATH=src` and a Python environment containing
NumPy and Pydantic, then run the following block. The smoke test executes this
exact block; successful execution produces no output.

```python
import math

import numpy as np

from digitalmodel.orcawave.rao_processing import (
    amplitude_phase_to_complex,
    complex_to_amplitude_phase,
)

# Inputs, exact triangle values and error budget: Synthetic calculation basis.
phase_deg = np.array([0.0, 90.0, 180.0, 270.0, 30.0, 120.0, -135.0, 390.0, -240.0])
amplitude = np.full_like(phase_deg, 2.0)
sqrt_two, sqrt_three = math.sqrt(2.0), math.sqrt(3.0)
expected_real = np.array([2.0, 0.0, -2.0, 0.0, sqrt_three, -1.0, -sqrt_two, sqrt_three, -1.0])
expected_imag = np.array([0.0, 2.0, 0.0, -2.0, 1.0, sqrt_three, -sqrt_two, 1.0, sqrt_three])
expected = expected_real + 1j * expected_imag

z = amplitude_phase_to_complex(amplitude, phase_deg)
np.testing.assert_allclose(z.real, expected_real, rtol=0, atol=1e-12)
np.testing.assert_allclose(z.imag, expected_imag, rtol=0, atol=1e-12)

# Inverse conversion starts with independently calculated complex responses.
amplitude_out, principal_phase_deg = complex_to_amplitude_phase(expected)
np.testing.assert_allclose(amplitude_out, amplitude, rtol=0, atol=1e-12)
assert np.all((-180.0 <= principal_phase_deg) & (principal_phase_deg <= 180.0))
reconstructed = amplitude_phase_to_complex(amplitude_out, principal_phase_deg)
np.testing.assert_allclose(reconstructed, expected, rtol=0, atol=1e-12)
roundtrip = amplitude_phase_to_complex(*complex_to_amplitude_phase(z))
np.testing.assert_allclose(roundtrip, expected, rtol=0, atol=1e-12)

# A complete turn in either direction preserves the complex response.
for turn_deg in (-360.0, 360.0):
    equivalent = amplitude_phase_to_complex(amplitude_out, principal_phase_deg + turn_deg)
    np.testing.assert_allclose(equivalent, expected, rtol=0, atol=1e-12)
```

## Theory context and limits

The repository's [hydrodynamics academic reference list](../research/hydrodynamics-academic-references.md#a-core-textbooks-and-monographs)
provides the existing core-theory reading routes. That list carries bibliographic
metadata; it is not evidence that a particular solver implements this sign
convention. The calculations above are derived explicitly here and do not claim
validation against any listed text or solver.

These checks establish conversion algebra for the stated synthetic inputs only.
They establish no radiation/diffraction accuracy, mesh convergence, solver
feature coverage or cross-tool agreement. The broader industry survey in
issue #1800 remains open: conference and journal practice, vendor documentation,
the open-source landscape, and evidence-based comparisons of solver capabilities
and conventions remain to be assessed.
