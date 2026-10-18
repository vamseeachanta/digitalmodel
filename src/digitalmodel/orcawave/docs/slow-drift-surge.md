# Low-frequency surge and experimental Naess extreme

The offline surge spectrum and static offset pass the synthetic checks below.
The Naess asymptote **does not meet the 10% three-hour maximum criterion** in
the broadband reference. It remains experimental; design acceptance is not
established. The strict expected-failure test retains that unresolved criterion.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-18 | Recorded equations, synthetic checks and reproduction limits. |
| 1 | 2026-10-18 | Isolated the experimental extreme and distinguished identity checks from independent evidence. |

## API and conventions

The force spectrum, surge spectrum and kernel are re-exported from
`digitalmodel.orcawave.drift_forces`. The unqualified experimental extreme is
available only from `digitalmodel.orcawave.slow_drift_response`:

```python
import numpy as np
from digitalmodel.orcawave.drift_forces import (
    full_qtf_force_spectrum, low_frequency_surge_response,
    surge_response_kernel,
)
from digitalmodel.orcawave.slow_drift_response import naess_expected_extreme

# qtf is a complete Hermitian surge QTF in kN/m²; S is m² s/rad.
force_psd = full_qtf_force_spectrum(qtf, S, omega)
mu = np.arange(len(omega)) * (omega[1] - omega[0])
system = dict(mass_kg=1e7, stiffness_kn_m=100., damping_ratio=.05)
kernel = surge_response_kernel(qtf, S, omega, **system)
response = low_frequency_surge_response(
    force_psd, mu, **system, mean_force_kn=100.*kernel.mean_offset_m,
)
# An observed crossing rate is required; this value is an illustrative input.
maximum = naess_expected_extreme(
    kernel.eigenvalues_m, .01, 10800., mean_offset_m=kernel.mean_offset_m,
)
```

Frequency grids are nonnegative, uniform and increasing. The new QTF functions
accept `drift_forces.QTFMatrix`, not `bemrosetta.models.qtf_data.QTFData`. They
require the spectrum and matrix to share the same grid; they do not interpolate,
extrapolate or complete a matrix triangle. The response PSD starts at zero
difference frequency and its DC bin must be zero. Variance is `sum(PSD)*dmu`,
with full endpoint bin weights, consistent with `newman_approximation`.
Mean load is supplied separately, with offset `mean_force_kn/stiffness_kn_m`.
These checks do not retrofit validation into the older force functions; the
remaining work is tracked by [the QTF validation issue](https://github.com/vamseeachanta/digitalmodel/issues/2624).

Mass includes added mass and is supplied in kg, converted to kN s²/m internally.
Stiffness is kN/m, damping is kN s/m. Supply exactly one positive damping
coefficient or positive damping ratio. The transfer function is
`H(mu)=1/(K-M*mu²+i*C*mu)` and `Sx=|H|²*SF` [1, Eq. 14]. Constant
coefficients, a scalar surge coordinate and linearized restoring/damping are
assumed. Aerodynamic thrust/damping, coupled motions and nonlinear moorings are
not calculated.

For fixed wave amplitudes `a_i=sqrt(2*S_i*domega)`, the physical pair matrix
is `A_ij=a_i*a_j*Q_ij*H(omega_i-omega_j)`. Independent real and imaginary wave
quadratures have variance 1/2. Therefore the squared-standard-normal
Kac–Siegert kernel is `B=A/2` [2, 3], rather than the deterministic-amplitude
matrix A. This explicitly converts the wave-amplitude convention in [1, Eq. 20].
The diagonal load is constant in the fixed-amplitude ensemble: `trace(A)` is
retained as the mean offset and the diagonal is removed before diagonalization.
For the centered kernel, `X-mean=sum(lambda_j*(U_j²+V_j²))`; each eigenvalue
occurs twice in the equivalent real matrix. Consequently the dynamic mean is
`2*sum(lambda)=0` and variance is `4*sum(lambda²)`. The Gaussian-quadrature
representation matches the fixed-phase ensemble's first two moments; it does
not establish equality of their finite-component tail distributions.

The implemented asymptote is [1, Eq. 21, attributed there to 2]:

`q=2*ln(n_plus*T)`

`E[max(X-mean)] ~ lambda_max*(q+ln(q)+2*EulerGamma*(1-q**(-1/2)))`.

The logarithm acts on the dimensionless expected crossing count `n_plus*T`.
The API requires `q>=1` to avoid evaluating this expansion at a small logarithm;
that minimal guard does not qualify its accuracy. Light damping and long
duration are required. The omitted term is of order `q**(-2/3)` [1]. The
crossing rate is supplied in Hz from the centered response. The Gaussian
spectral estimate `sqrt(m2/m0)/(2*pi)` is not automatically substituted for a
quadratic-response crossing rate. `tail="lower"` applies the formula to `-X`
and returns the signed minimum plus the static offset. The result is an expected
excursion amplitude, not a quantile or a peak-to-peak height.

## Reproducible evidence

Run from the repository root, with NumPy, SciPy, Pydantic and pytest installed:

```bash
OPENBLAS_NUM_THREADS=1 PYTHONPATH=src python -m pytest \
  tests/orcawave/test_slow_drift_response.py \
  tests/orcawave/test_slow_drift_monte_carlo.py -q -rx
OPENBLAS_NUM_THREADS=1 PYTHONPATH=src python tests/orcawave/test_slow_drift_monte_carlo.py
```

The synthetic reference uses JONSWAP Hs=6 m, Tp=10 s, default gamma=3.3,
constant QTF=10 kN/m², mass=10⁷ kg, natural period=100 s, damping ratio=0.05,
2049 wave bins from 0.2 rad/s with spacing `2*pi/10800` rad/s, 256 independent
random-phase records of 10800 s, 16384 samples per record, and seed 206. Force
is calculated independently as `10*abs(wave_envelope)**2`; an FFT solution of
the scalar oscillator supplies the stationary response with DC removed.

| Quantity | Analytical result | Reference value | Criterion / disposition |
| --- | ---: | ---: | --- |
| Force variance (kN²) | 2022.1 | Ensemble: 2019.5 | Within 5% |
| Surge standard deviation (m) | 2.3102 | Ensemble: 2.2924 | 0.8% high; within 5% |
| Kernel variance (m²) | 5.3372 | Spectrum: 5.3372 | Identity check, relative tolerance 1e-10 |
| Mean offset (m) | 1.1399 | Shared-input mean: 1.1399 | Identity check, relative tolerance 1e-10 |
| Expected dynamic maximum (m) | 6.8346 | Ensemble: 8.5016 | 19.6% low; 10% criterion unmet |

Table 1. Synthetic numerical checks; these are not published benchmark data.

The observed upcrossing rate is 0.00978877 Hz. The approximate 95% sampling
halfwidth of the ensemble mean maximum is 0.1834 m. Sampling uncertainty does
not account for the 1.7 m discrepancy. Comparisons use unrounded values. No tuning factor is applied. The
closed-form tests independently check bichromatic resonance, complex QTF phase,
full endpoint weights, signed tails and a high-precision evaluation of Eq. 21.

## Published values and reproduction limits

Table 1 of [1] gives damping/frequency pairs (0.13, 0.0093 Hz),
(0.08, 0.0095 Hz), (0.04, 0.0099 Hz) at wind speeds 8, 11, 20 m/s.
Tests use those pairs as inputs to synthetic oscillators and check the resonant
identity `|H|=1/(2*zeta*K)`. They do not reproduce the published decay tests.

Table 4 of [1] gives signed three-hour surge heights −23, −18, −19, −21 m
at wind speeds 0, 8, 11, 20 m/s, for Hs=11 m, Tp=12 s, gamma=4.9.
The corresponding signed amplitudes are −11.5, −9, −9.5, −10.5 m, because
the paper defines height as twice amplitude. These values are cited context,
**not independently reproduced results**. The full complex QTF and calibrated
hydrodynamic/aerodynamic model inputs for those calculations have not been
verified for reuse. The public design-reference citation and project results
page do not establish availability or reuse rights for that numerical dataset.
No dataset is downloaded or substituted; all computational fixtures are synthetic.

## References

1. D.R. Lande-Sudall and P.K. Stansby (2026), “Towards conditional low-frequency
   response design waves for floating offshore wind turbine applications,”
   *Applied Ocean Research* 176, 105292.
   [doi:10.1016/j.apor.2026.105292](https://doi.org/10.1016/j.apor.2026.105292).
   The inspected article is CC BY 4.0; no article text or figures are reproduced.
2. A. Naess (1985), “Statistical analysis of second-order response of marine
   structures,” *Journal of Ship Research* 29(4), 270–284.
   [doi:10.5957/jsr.1985.29.4.270](https://doi.org/10.5957/jsr.1985.29.4.270).
   The implemented asymptote is inspected through reference 1; direct
   inspection of this original article is not claimed.
3. M. Kac and A.J.F. Siegert (1947), “On the theory of noise in radio receivers
   with square law detectors,” *Journal of Applied Physics* 18, 383–397.
   [doi:10.1063/1.1697662](https://doi.org/10.1063/1.1697662).
   Quadratic-form moment identities above follow directly from independent
   standard-normal moments.
