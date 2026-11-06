# Slowly varying drift analysis

The slow-drift routines now reject unsupported frequency grids and malformed
difference-frequency QTFs before summing beat-pair variance. Valid complete
Hermitian inputs retain the existing fixed-amplitude, random-phase convention.
The constructed example below gives variance **168.0 kN²** and standard
deviation **13.0 kN**, rounded to one decimal place.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-23 | Document input refusals and a hand calculation to prevent silent use outside the discrete integration domain. |

## Input contract

`newman_approximation` and `full_qtf_slowly_varying` require nonempty,
one-dimensional, finite, positive and strictly increasing `omega`, with uniform
spacing. The wave spectrum must have exactly the same shape, finite values and
nonnegative density. A singleton grid gives zero fluctuation because no distinct
frequency pair exists. Empty grids are refused.

Full QTF integration additionally requires:

- A positive, increasing, uniform `frequencies_rad_s` grid.
- Both `real_part` and `imag_part` shaped exactly `(n, n)`, with finite entries.
- Symmetric real entries, antisymmetric imaginary entries and an exactly zero
  imaginary diagonal: the difference-frequency condition is `Qij = conj(Qji)`.
- A spectrum frequency range that covers every QTF frequency. Linear
  interpolation between covered samples is retained; endpoint extrapolation
  is refused.

Both triangles must be supplied explicitly. Triangular and asymmetric data raise
`ValueError` with a message requesting a complete Hermitian QTF. The routines do
not fill, average or mutate entries. A true zero matrix or real diagonal-only
matrix is valid and gives zero fluctuation. A zero used for missing data in
*both* conjugate positions cannot be distinguished from a physical zero; the
caller remains responsible for that provenance.

Newman coefficient frequencies may be nonuniform because they are interpolation
coordinates, not the integration grid. They must be nonempty, finite, positive
and increasing; the selected coefficient array must match their shape and be
finite. Signed real coefficients are supported. The existing endpoint clamping
of diagonal coefficient interpolation is retained. Only `surge`, `sway` and `yaw`
are accepted as `dof`; another value raises `ValueError`.

## Numerical comparison policy

The keyword-only `grid_rtol` and `qtf_rtol` default to `1e-5`, the documented
relative default in the [NumPy 2.4 `isclose` reference][numpy]. This is an
explicit numerical policy, not an engineering accuracy criterion. A different
tolerance can be supplied by the caller; both must be finite and in `[0, 1)`.
For a missing conjugate partner, comparison of zero with a nonzero entry would
pass at a relative tolerance of one, so that upper bound is excluded.

Grid spacing is compared with the first interval. Real symmetry and imaginary
antisymmetry are checked separately, over all matrix positions. Each uses
`abs(a-b) <= rtol*abs(b)` with zero absolute tolerance; checking every transposed
position applies the comparison in both directions. This avoids a large real
part masking a missing imaginary part, or a dimensional absolute floor accepting
a lone triangle with small coefficients. Within-tolerance differences are
accepted as supplied; the upper triangle still supplies the variance sum.

## Method and sources

[Orcina WebHelp, *Vessel theory: Wave drift and sum frequency loads*][theory],
“Second order load theory” (public web edition accessed 2026-10-23), gives the
ordered-pair second-order force convention. “Newman approximation for
off-diagonal QTFs” describes the arithmetic mean in the original 1974 method.
The repository retains that arithmetic mean, rather than the geometric variant
used by OrcaFlex. [*Vessel types: Wave drift and sum frequency QTFs*][qtf],
“Full QTFs”, symmetry equation (same web edition), gives conjugate symmetry and
a real difference-frequency diagonal.

With fixed wave amplitudes `ai = sqrt(2 Si dw)`, each Hermitian pair combines
into a real sinusoid of amplitude `2 ai aj |Qij|`. Its random-phase variance is
amplitude squared divided by two, hence

```text
variance = 8 dw² sum(i<j) |Qij|² Si Sj
Qij_Newman = (Di + Dj) / 2
PSD[k] = 8 dw sum(i) Q(i,i+k)² Si S(i+k), k > 0
PSD[0] = 0; variance_Newman = sum(PSD) dw
```

These equations follow from the cited ordered-pair convention and elementary
phase averaging; diagonal terms are constant mean loads. The uniform-grid
restriction follows because `omega[j] - omega[i] = (j-i) dw` is needed for lag
bins and each component uses the same `dw`. No new hydrodynamic coefficient or
physical acceptance limit is introduced.

## Worked example

All values are constructed inputs. Use `omega = [1, 2, 3] rad/s`, density
`S = [1, 2, 1] m²/(rad/s)` and diagonal coefficients `D = [1, 2, 3] kN/m²`.
Then `dw = 1`, and the arithmetic Newman pairs are `Q12 = 1.5`, `Q23 = 2.5`
and `Q13 = 2`. The independent hand calculation is:

```text
PSD[1] = 8 (1.5² × 1 × 2 + 2.5² × 2 × 1) = 136
PSD[2] = 8 (2² × 1 × 1) = 32
variance = (136 + 32) × 1 = 168 kN²
std = sqrt(168) kN
```

The following code runs with the source package on `PYTHONPATH` and requires no
solver:

```python
import numpy as np

from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    full_qtf_slowly_varying,
    newman_approximation,
)

omega = np.array([1.0, 2.0, 3.0])
spectrum = np.array([1.0, 2.0, 1.0])
diagonal = [1.0, 2.0, 3.0]
coefficients = MeanDriftCoefficients(
    heading_deg=0,
    frame="x_forward_y_port_z_up",
    heading_convention="from_clockwise_bow",
    frequencies_rad_s=omega.tolist(),
    surge_kn_m2=diagonal,
    sway_kn_m2=diagonal,
    yaw_knm_m2=diagonal,
)
qtf = QTFMatrix(
    dof="surge", heading_deg=0, frequencies_rad_s=omega.tolist(),
    real_part=[[1, 1.5, 2], [1.5, 2, 2.5], [2, 2.5, 3]],
    imag_part=np.zeros((3, 3)).tolist(),
)
psd = newman_approximation(coefficients, spectrum, omega)
variance = psd.sum() * (omega[1] - omega[0])
std = full_qtf_slowly_varying(qtf, spectrum, omega)
print(f"Variance: {variance:.1f} kN²; standard deviation: {std:.1f} kN")
```

Output: `Variance: 168.0 kN²; standard deviation: 13.0 kN`.

## Regression coverage and limits

`tests/orcawave/test_drift_force_validation.py` covers refusals, tolerances,
singleton behaviour, interpolation, real and complex hand calculations. The
expected values are derived in the test docstrings independently of production
integration. `tests/orcawave/test_drift_force_variance.py` already contains the
broadband surge-oscillator Monte Carlo regression requested in issue #2624;
that regression is retained and run with the new validation. It compares a
random-phase envelope-squared force, filtered by a linear oscillator FFT, with
the full-QTF prediction; it does not call a licensed solver.

Nonuniform-grid quadrature and triangular storage conversion remain unsupported.
To proceed after a refusal, supply an explicitly completed Hermitian QTF on a
uniform grid and a covering spectrum. Input validation does not establish QTF
convergence, Newman applicability, or nonlinear response accuracy.

[theory]: https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Wavedriftandsumfrequencyloads.htm
[qtf]: https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltypes,WavedriftandsumfrequencyQTFs.htm
[numpy]: https://numpy.org/doc/2.4/reference/generated/numpy.isclose.html
