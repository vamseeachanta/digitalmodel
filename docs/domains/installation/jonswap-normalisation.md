# JONSWAP energy normalisation

The installation and free-span spectra now multiply their energy normalising
factor. A shared integral of the dimensionless spectrum replaces the approximate
logarithmic fit so that the three gamma cases required by issue #2543 preserve
wave variance within 1%. The multiplier is independent of the supplied frequency
grid; omitted spectral energy remains omitted.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Correct the multiplier, qualify the energy integral and provide a reproducible example. |

## Sources and scope

The public [SIMA documentation, JONSWAP Spectra, sections 1 and 2](https://simasite.azurewebsites.net/docs/latest/simo/modelling/environment/waves/Jonswap.html)
(accessed 2026-10-22; the page does not state a publication year) supplies the
shape, the width parameters and the approximate energy coefficient. The public
[spectral-parameter documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment,DataforJONSWAPandISSCspectra.htm)
(accessed 2026-10-22) independently gives the standard width parameters and the
gamma=1 spectrum limit. No licensed standard was used to obtain coefficients.

Issue #2543 specifies
the acceptance relation `m0 = Hs²/16`, the gamma cases 1.0, 3.3 and 7.0, and a 1%
relative energy tolerance. Heights, periods and response gains in the example
and tests are synthetic inputs, not environmental design data.

## Equations and numerical method

The SIMA section 1 shape, expressed using angular frequency, is

```text
wp = 2*pi/Tp
x = omega/wp
sigma = 0.07 for x <= 1; 0.09 for x > 1
a(x) = exp(-(x-1)²/(2*sigma²))
q(x) = 5*x^-5*exp(-5/(4*x^4))
S(omega) = Hs²/(16*wp) * C_gamma * q(x) * gamma^a(x)
```

The factors 5 and 5/4 follow by substituting the Hs constraint into the section 1
shape; integrating `q` gives exactly 1. The issue's wave-variance relation then
requires

```text
integral(q(x), x=0..infinity) = 1
C_gamma = 1 / integral(q(x)*gamma^a(x), x=0..infinity)
        = 1 / (1 + integral(q(x)*(gamma^a(x)-1), x=0..infinity))
```

The analytical unenhanced integral follows by differentiating
`exp(-5/(4*x^4))`. This supplies the entire unenhanced tail exactly. Only the
enhancement excess is evaluated numerically, using the trapezoidal rule on
`0.5 <= x <= 2`, with step `1/2048` and the width transition at a node. These
are numerical mesh choices, checked against independent scalar Simpson
integration with 6000 intervals in the regression tests.

For `1 <= gamma <= 7`, below `x=0.5` the enhancement exponent is at most
`exp(-0.5*(0.5/0.07)²)`. Above `x=2` it is at most
`exp(-0.5*(1/0.09)²)`. Consequently `gamma^a-1` outside the integration interval
is below `2e-11`. Since `q` is nonnegative and integrates to 1, the discarded
excess integral is bounded by that same value. This bound is an independent
calculation from the sourced shape coefficients, rather than an empirical fit.

The SIMA section 2 approximation `1 - 0.287*ln(gamma)` is a multiplier. Dividing
by it increases the spectrum instead of compensating for peak enhancement.
Simply reversing the operation gives a relative variance error of -1.8% at
gamma=7.0, outside the issue's 1% requirement. The shared numerical factor
removes that fitting error as well as the division defect. Gamma=1 gives an
exact multiplier of 1 and retains the existing unenhanced formula.

Normalising the caller's frequency samples to Hs would redistribute missing
energy into the retained band. The correction therefore uses only gamma.
A sparse, narrow or poorly resolved input grid can still underestimate a
response. Both APIs keep their existing units and arguments: the crane helper
takes peak angular frequency, while the free-span function takes peak period.

## Worked example

Inputs are `Hs=4 m`, `Tp=8 s`, unit displacement RAO at all sampled frequencies,
and gamma equal to 1.0, 3.3 or 7.0. The grid uses 20001 logarithmically spaced
points from `0.01*wp` to `100*wp`. The analytical unenhanced variance fraction above this
upper bound is `1-exp(-5/(4*100^4))`; its percentage rounds upward to 0.1%.
The lower tail is exponentially negligible. The grid choice is a numerical
qualification input, not a recommended grid for arbitrary response functions.

The hand calculation, using the issue's energy relation and the existing
single-amplitude response convention, is

```text
m0_wave = 4²/16 = 1 m²
m0_response = integral(unit_RAO²*S d_omega) = 1 m²
significant single-amplitude motion = 2*sqrt(m0_response) = 2 m
```

Run the example from the repository root:

```bash
PYTHONPATH=src python examples/demos/installation/jonswap_normalisation.py
```

| Gamma | Crane spectrum m0 (m²) | Free-span spectrum m0 (m²) | Significant single-amplitude motion (m) |
| --- | --- | --- | --- |
| 1.0 | 1.0 | 1.0 | 2.0 |
| 3.3 | 1.0 | 1.0 | 2.0 |
| 7.0 | 1.0 | 1.0 | 2.0 |

Values are displayed to one decimal place; the regressions compare the
unrounded integrals against the 1% energy tolerance.

## Verification and limits

`tests/test_jonswap_normalisation.py` checks both spectra against the analytical
wave variance for two heights, two periods and all three requested gamma
values. It also checks the analytical unenhanced density, independently integrated peak
densities, retained-band invariance, zero-height limits, automatic gamma outer
branches, constant-RAO motion, inverse-frequency-RAO velocity and free-span
velocity scaling. The expected motion and velocity calculations are written
out in the test docstrings. The worked example is exercised by the same test
module.

The qualified gamma interval is 1 through 7. No additional gamma selection
rule, response convention, dispersion method, motion transfer sign or design
acceptance criterion is introduced. A constant-RAO example checks energy
scaling; it does not qualify a real response dataset or an installation design.
