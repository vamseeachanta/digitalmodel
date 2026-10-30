# Weibull response extreme analysis

The statistics module fits a three-parameter Weibull law to supplied response
peaks and compares its extrapolation with the existing Rayleigh calculation.
Closed-form controls, independent EDF arithmetic and a synthetic example check
the computation. Fitted-law goodness-of-fit decisions remain `NOT_EVALUATED`
until their calibration is established. The way forward is to justify the peak
population and location bounds, assess tail sensitivity and calibrate decisions
before using an extrapolation for design.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Add distribution, estimation, diagnostics and a synthetic example to support response extreme analysis. |

## Inputs and scope

Import `digitalmodel.orcawave.weibull_fit` directly. The module consumes finite,
uncensored, independent peaks from a stationary population. Response, location
and scale use the same units; shape is dimensionless. Signed time-series
ordinates, correlated peaks and mixtures of operating conditions need a separate
statistical basis. No peak extraction or solver is invoked.

Both `location_bounds` and `shape_bounds` are required for estimation. Their
finite endpoints are analysis inputs. The upper location endpoint must be
strictly below the smallest observation, with a gap justified for the data.
Three distinct observations are the algebraic identification floor; this floor
does not establish statistical adequacy.

## Distribution and moments

Source W below gives the general density and the standard distribution's CDF,
inverse and moments. With shape `k > 0`, scale `lambda > 0`, location `gamma`
and `z = (x - gamma)/lambda`:

```text
f(x) = (k/lambda) z^(k-1) exp(-z^k), x >= gamma
F(x) = 1 - exp(-z^k)
Q(p) = gamma + lambda [-ln(1-p)]^(1/k)
Q_N  = gamma + lambda (ln N)^(1/k), N >= 1
```

The density and CDF are zero below the support. At the support endpoint the
density's right limit is infinite for `k < 1`, `1/lambda` for `k = 1` and zero
for `k > 1`. Quantiles include `Q(0) = gamma` and `Q(1) = infinity`.
The implementation evaluates `Q_N` using `ln N` so that large cycle counts
do not make `1 - 1/N` round to one.

Substitution `t = z^k` in the density integral gives
`E[Z^j] = Gamma(1 + j/k)`. A binomial expansion therefore gives

```text
E[X^r] = sum(j=0..r) choose(r,j) gamma^(r-j) lambda^j Gamma(1+j/k)
E[X]   = gamma + lambda Gamma(1+1/k)
Var[X] = lambda^2 [Gamma(1+2/k) - Gamma(1+1/k)^2]
```

Here `Gamma` is the gamma function, distinct from the location parameter.
Raw integer moments, mean and variance are exposed. Very large moments can
exceed floating-point range. `gamma = 0` recovers the two-parameter law;
`k = 1` gives the exponential law; `k = 2` with
`lambda = sqrt(2)*sigma` gives the Rayleigh amplitude law.

## Parameter estimation

`fit_weibull(samples, method=..., location_bounds=..., shape_bounds=...)`
returns `WeibullFit`, containing the distribution, requested and actual method,
objective and qualifications. Search variables are normalized by sample
standard deviation and translated by the minimum to preserve unit invariance.
The scalar profile optimizer checks both endpoints as well as its interior
solution. It finds a numerical local solution; a global optimum is not claimed.

### Maximum likelihood

Taking the logarithm of W's density and summing gives

```text
ell = n ln(k) - n k ln(lambda)
      + (k-1) sum ln(x_i-gamma) - sum ((x_i-gamma)/lambda)^k
lambda^k = mean((x_i-gamma)^k)
0 = 1/k + mean(ln(x_i-gamma))
    - sum((x_i-gamma)^k ln(x_i-gamma)) / sum((x_i-gamma)^k)
```

The code profiles location, solves the scalar shape score within the supplied
shape bounds and obtains scale analytically. Log-sum-exp avoids large powers
in the shape score. The objective is the mean negative log likelihood of
the normalized samples; objectives from different normalizations are not
directly comparable.

For `k < 1`, holding scale and shape fixed while moving location toward the
sample minimum sends `(k-1) ln(x_min-gamma)` to positive infinity. Thus an
unrestricted three-parameter likelihood can seek the boundary. A bounded
solution with `k <= 1` explicitly falls back to probability-plot fitting and
records the change of method. Bound-active fits are flagged. For `1 < k <= 2`,
differentiating W gives a singular location-score term proportional to
`1/(x-gamma)`. Its square times the near-endpoint density behaves as
`z^(k-3)`, with a divergent integral. Regular asymptotic inference is therefore
not assumed for `k <= 2`; source A separately documents fitted-Weibull
difficulties and restrictions on its critical-value procedure.

### Method of moments

Sample moments use divisor `n`. With `G_j = Gamma(1+j/k)`, solve

```text
sample skew = (G_3 - 3 G_1 G_2 + 2 G_1^3)/(G_2-G_1^2)^(3/2)
lambda = sqrt(sample variance/(G_2-G_1^2))
gamma = sample mean - lambda G_1
```

These follow by expanding the central moments of W. An unattainable skewness,
or a resulting location outside the supplied bracket, raises `ValueError`.
Clipping the moment estimate would break the matching equations.
`distribution_from_moments` also accepts externally supplied population moments
with a required shape bracket.

### Probability-plot least squares

Source P supplies the plotting positions `p_i = (i - 0.3)/(n + 0.4)` and the
Weibull transform. The code extends that two-parameter plot by profiling the
location within its supplied bounds. For each location it regresses
`y_i = ln[-ln(1-p_i)]` on `ln(x_i-gamma)`; slope is `k` and intercept is
`-k ln(lambda)`. Shape is constrained to its supplied bracket. The minimized
objective is mean squared residual in the transformed probability coordinate.
This specifies the regression direction, which matters for noisy data.

## Goodness of fit and uncertainty

For sorted observations, sources K and D define the unadjusted statistics:

```text
D = max_i [F(x_i)-(i-1)/n, i/n-F(x_i)]
A^2 = -n - sum_i (2i-1)/n [ln F(x_i) + ln S(x_(n+1-i))]
```

Survival logs are evaluated directly. A support violation gives infinite AD;
probabilities are not clipped to hide it. A CDF that rounds to one in the upper
tail does not by itself make AD infinite.

Passing a `WeibullFit` to `goodness_of_fit` marks the parameters as estimated.
Both decisions then remain `NOT_EVALUATED`, including when a known-parameter
critical value was supplied. Source K explicitly excludes using its usual
critical values when parameters were estimated from the same observations.
Source D explains the dependence of AD critical values on the distribution,
estimation and statistic adjustment.

For an independently specified `WeibullDistribution`, a caller may supply
`CriticalValue(value, significance, sample_size, source)`. The source and sample
size are mandatory, and sample size must match. The caller must verify table
applicability and raw-statistic scaling. The KS test includes K's published
worked-example value `Dcrit = 0.04301` for `n = 1000` at significance `0.05`.
K does not provide a full table. No published raw AD table applicable to this
bounded fitted model was obtained. Those fitted decisions are deliberately
unevaluated; no normal-law or two-parameter table is substituted. Source A's
Table 0 reference concerns a different fitting procedure and is not applied.

`bootstrap_return_level` samples peaks with replacement, refits each sample
using the same bounds and computes return levels. The replicate count,
confidence and seed are required inputs. Source B describes the percentile
method; endpoints here use linear interpolation between ordered replicates at
`(1-confidence)/2` and `(1+confidence)/2`. Failed refits are counted. Any failure
leaves the interval `NOT_EVALUATED`; successful return levels remain available
for diagnosis. Fallback counts are exposed.

An `EVALUATED` interval means the calculation completed, not that its nominal
coverage has been validated. Boundary estimates, non-regular fits, tail model
error and dependence can invalidate coverage. A fitted-law non-rejection would
also not qualify a remote tail extrapolation.

## Equal-cycle extreme comparison

`compare_rayleigh_extreme` calls the existing `long_term_extreme` unchanged.
Its time fractions `p_i` and zero-crossing periods `Tz_i` give

```text
N = R_seconds * sum_i(p_i/Tz_i)
R_seconds * sum_i(p_i/Tz_i * exp(-x^2/(2 sigma_i^2))) = 1
```

The second equation is the existing Rayleigh expected-exceedance root (source
R2). The first supplies the cycle count for Weibull `Q_N`. The samples must
represent the same cycle-weighted population. A time-weighted mixed sample
generally has different weights. The returned fields include both thresholds,
their ratio and the percentage difference. No existing module is modified.

`Q_N` is a one-expected-exceedance threshold. The probability that all independent
peaks remain below it is `(1-1/N)^N`, tending to `exp(-1)`, or 36.8%.
It is neither an expected maximum nor a chosen high-confidence maximum quantile.
At fixed shape and scale, `dQ_N/dgamma = 1`; re-estimation changes all three
parameters and can amplify the effect of the location bracket.

The seeded Rayleigh comparison has a sampling allowance derived independently
from binomial counting. With `p = 1-1/N`,
`SE(Fhat) = sqrt(p(1-p)/n)` and
`f(Q_N) = Q_N/(N sigma^2)`. First-order propagation gives

```text
SE(Q_N)/Q_N = sqrt((N-1)/n)/(2 ln N)
```

For 20,000 peaks and 1,000 cycles, the test allows six times this value,
9.8% after rounding upward. Six is an explicitly chosen regression allowance;
the unrounded expression is used by the test. This is a scale for a synthetic
sampling control, not a confidence guarantee for a three-parameter fit. A wrong
shape provides the negative control.

## Worked synthetic example

With NumPy, SciPy and Pydantic available:

```bash
PYTHONPATH=src python examples/orcawave/weibull_response_extremes.py
```

The example uses 5,000 independent synthetic peaks, seed 2026, generating shape
2, scale `sqrt(2)` and location zero, in arbitrary response units. The fitted
method is MLE with location bounds `[-0.5, 0]` and shape bounds `[0.5, 5]`.
Those bounds are constructed inputs; the synthetic generating origin justifies
the upper location bound. The nominal 95.0% percentile interval uses 200
resamples and seed 2027. No empirical coefficient is inferred from these choices.

The reference uses one cell with probability one, unit RAOs and constant
`S = 0.5` on `omega = [1,2,3]` rad/s. Hand trapezoids give `m0 = 1` and
`m2 = 4.5`, hence `sigma = 1` and `Tz = 2*pi/sqrt(4.5)` seconds. A duration of
`1000*Tz` gives exactly 1,000 cycles. The wave parameters are placeholders for
this constructed constant-spectrum callback, not measured metocean data.

| Output | Result |
| --- | --- |
| Fitted shape, scale, location | 2.0, 1.4, 0.0 |
| KS statistic | 1.3e-2; decision `NOT_EVALUATED` |
| Raw AD statistic | 6.5e-1; decision `NOT_EVALUATED` |
| Weibull return level | 3.7 response units |
| Rayleigh reference | 3.7 response units |
| Ratio; percentage difference | 1.0; 0.0% |
| Nominal 95.0% percentile interval | 3.6 to 3.8 response units |
| Bootstrap counts | 200 completed, zero failed, zero fallbacks |
| Qualification | Location upper bound active |

The workflow follows the broad response-samples-to-extreme comparison scope of
*Applied Ocean Research* 176 (2026), article 105283. Its DOI resolves to a public
landing page, while the article page returned HTTP 403 when checked on
2026-10-22. Its methods and numerical results were unavailable for verification.
This example uses the independent sources below and synthetic data; it does
not claim to reproduce that article. Further work requires an accessible source
and an independently established peak and tail-model basis.

## Tests and reproduction limits

| Slice | Independent expectation and negative controls |
| --- | --- |
| Distribution | Exponential `(k,lambda,gamma)=(1,2,3)` gives mean 5, variance 4 and second raw moment 29; Rayleigh reduction; support and invalid inputs. |
| Estimation | Exponential population moments, an exact probability-plot line and seeded recovery; unattainable moments, invalid bounds and non-regular fallback. SciPy fit is a secondary cross-check only. |
| Diagnostics | Five transformed probabilities `(0.1,0.3,0.5,0.7,0.9)` give KS 0.1 and the explicitly expanded AD sum; wrong law, support mismatch and invalid critical-value size. |
| Bootstrap | An independently derived fixed-shape exponential estimator gives resample mean times `ln N`; hand order-statistic endpoints; degenerate resamples keep intervals unevaluated. |
| Extremes | Exact equal-cycle Rayleigh identity; analytic sampling allowance; wrong shape and location sensitivity. |
| Example | Hand-integrated reference spectrum, complete output fields, nominal interval and explicit qualifications; invalid sample count. |

The per-slice tests are `tests/orcawave/test_weibull_*.py`. Existing motion
statistics and Rayleigh tests also exercise the unchanged reference calculation.
Statistical controls establish the written equations and constructed cases;
they do not validate a measured response process or a licensed solver.

## Sources

- W: [NIST/SEMATECH e-Handbook, online edition, 1.3.6.6.8, Weibull Distribution](https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm), accessed 2026-10-22. General density, CDF, inverse and gamma moments. Three-parameter moment and likelihood equations above are derived from those expressions.
- P: [NIST/SEMATECH e-Handbook, online edition, 1.3.3.30, Weibull Plot](https://www.itl.nist.gov/div898/handbook/eda/section3/weibplot.htm), accessed 2026-10-22. Definition, plotting positions and linearization; location profiling is the documented extension here.
- K: [NIST/SEMATECH e-Handbook, online edition, 1.3.5.16, Kolmogorov-Smirnov Goodness-of-Fit Test](https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm), accessed 2026-10-22. Definition, parameter-estimation limitation and the 1,000-observation worked-example critical value.
- D: [NIST/SEMATECH e-Handbook, online edition, 1.3.5.14, Anderson-Darling Test](https://www.itl.nist.gov/div898/handbook/eda/section3/eda35e.htm), accessed 2026-10-22. Raw statistic and critical-value scaling restrictions.
- A: [SciPy 1.14.1 reference (2024), `anderson`, Notes and reference 7, Table 0](https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.anderson.html). Documents fitted-Weibull difficulties and critical-table restrictions; that table is not used for this bounded estimator.
- B: [SciPy 1.14.1 reference (2024), `bootstrap`, percentile method](https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.bootstrap.html). Bootstrap resampling and percentile confidence construction.
- R2: [Existing engineering citations, R2](../../../src/digitalmodel/orcawave/docs/engineering-citations.md). Expected-exceedance count, time fractions, response zero-crossing periods and the existing Julian-year conversion.
- Context: [*Applied Ocean Research* 176 (2026), article 105283, DOI](https://doi.org/10.1016/j.apor.2026.105283). Article text unavailable; no numerical or equation claim relies on it.
