# Linear regression uncertainty

`StatisticalAnalysisService.perform_linear_regression` fits a slope and an
intercept to retained observation pairs. Residual variance and coefficient
inference use the same residual degrees of freedom. `RegressionResult.std_error`
is the residual standard deviation; it is not the slope standard error.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-21 | Document residual uncertainty, input validation and limiting cases so returned intervals can be interpreted. |
| 1 | 2026-10-21 | Define constant-response inference and confidence input conversion; clarify interval scope and predictor conditioning limits. |

## Inputs and method

Inputs are two equally sized, one-dimensional numeric arrays and a finite
confidence probability strictly between zero and one. Confidence inputs are
converted to float, including numeric text; invalid conversions and probabilities
raise `ValueError`. A pair is removed when
either value is NaN. At least three pairs must remain, all retained values must
be finite, and the predictor must contain distinct values. Returned predictions
and residuals follow the retained input order.

The returned intervals describe the slope and intercept coefficients only.
Mean-response confidence intervals and prediction intervals for individual
observations are not returned; coefficient intervals are not prediction bands.

For `n` retained pairs, define `Sxx = sum((x - xbar)**2)` and
`Sxy = sum((x - xbar)*(y - ybar))`. The least-squares coefficients are
`b1 = Sxy/Sxx` and `b0 = ybar - b1*xbar`. With two fitted coefficients:

```text
residual = y - (b0 + b1*x)
SSE = sum(residual**2)
df = n - 2
s = sqrt(SSE/df)
SE(b1) = s/sqrt(Sxx)
SE(b0) = s*sqrt(1/n + xbar**2/Sxx)
```

The residual standard deviation follows the [NIST/SEMATECH e-Handbook of
Statistical Methods (2012), section 4.4.3.1, Process Modeling — Least
Squares](https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm):
`s = sqrt(SSE/(n-p))`, where `p` is the number of fitted coefficients.
Coefficient standard errors and Student intervals follow [SciPy 1.16.2 (2025),
`linregress` documentation and its confidence-interval
example](https://docs.scipy.org/doc/scipy-1.16.2/reference/generated/scipy.stats.linregress.html)
and [implementation](https://github.com/scipy/scipy/blob/v1.16.2/scipy/stats/_stats_py.py).

For confidence probability `c`, each interval is `bj +/- tcrit*SE(bj)`, where
`tcrit = Student_isf((1-c)/2, df)`. The two-sided test of zero slope uses
`t = b1/SE(b1)` and `p_value = 2*Student_sf(abs(t), df)`. The survival function
avoids cancellation from subtracting a nearly unit cumulative probability.
The two-tail probability convention is also described in [NIST (2012), section
1.3.6.7.2, Critical Values of the Student's t
Distribution](https://www.itl.nist.gov/div898/handbook/eda/section3/eda3672.htm).

## Worked example

The dimensionless inputs below are synthetic. All expected values follow this
independent hand calculation, rather than an output of the regression method:

```text
x = (1, 2, 3, 4, 5); y = (2, 4, 5, 4, 5)
n = 5; xbar = 3; ybar = 4; Sxx = 10; Sxy = 6
b1 = 3/5; b0 = 11/5
predictions = (14, 17, 20, 23, 26)/5
residuals = (-4, 3, 5, -3, -1)/5
SSE = (16+9+25+9+1)/25 = 12/5
df = 3; s = sqrt(4/5)
SE(b1) = sqrt(2)/5; SE(b0) = sqrt(22)/5
```

At a supplied confidence probability of 95.0 %, Student's upper-tail quantile
uses probability `1/40` and three degrees of freedom. The calculation can be
reproduced with:

```python
import numpy as np

from digitalmodel.solvers.orcaflex.results_analysis.statistical_analysis import (
    StatisticalAnalysisService,
)

# Synthetic inputs and probability; hand calculation and sources above.
x = np.array([1., 2., 3., 4., 5.])
y = np.array([2., 4., 5., 4., 5.])
result = StatisticalAnalysisService().perform_linear_regression(
    x, y, confidence_level=0.95
)
print(f"slope={result.slope:.1f}, intercept={result.intercept:.1f}")
print(f"residual standard deviation={result.std_error:.1f}")
for name, (lower, upper) in result.confidence_intervals.items():
    print(f"{name} interval=({lower:.1f}, {upper:.1f})")
print(f"two-sided p-value={result.p_value:.1%}")
```

Expected output, rounded to one decimal place:

```text
slope=0.6, intercept=2.2
residual standard deviation=0.9
slope interval=(-0.3, 1.5)
intercept interval=(-0.8, 5.2)
two-sided p-value=12.4%
```

Repeating each pair ten times gives fifty retained observations. The coefficients
are unchanged, while `SSE=24`, `Sxx=100`, `df=48` and `s=sqrt(1/2)`.
The tests cover both sizes with and without NaN padding, and compare additional
coefficient intervals against `scipy.stats.linregress` at supplied confidence
probabilities of 90.0 % and 99.0 %. Replication is an algebraic test fixture;
duplicating measured rows does not create independent observations.

## Zero variance and limits

When every retained response equals the first retained response, the slope
p-value and `r_squared` are explicitly NaN. Zero response variance makes both
inferences undefined, including for decimal constants such as `y=0.1` over
`x=(1,2,3,4,5,6,7)`, where fitting can leave small nonzero residuals.

Exactly zero residual variance gives zero coefficient standard errors and
collapsed intervals. A nonzero slope then has a limiting two-sided p-value of
zero. A zero slope with zero standard error gives the undefined statistic
`0/0`; its p-value is explicitly NaN. No arbitrary residual threshold or
regularising constant is introduced. Floating-point fits can leave small
nonzero residuals even for mathematically exact lines.

The Student inference assumes independent normal errors with constant variance
and an appropriate linear model. This calculation does not establish those
assumptions for time histories or repeated measurements. Collapsed intervals
from synthetic perfect fits do not establish zero measurement uncertainty.
The predictor check rejects exactly constant retained values; it does not apply
a scale-relative `Sxx` tolerance. Nearly constant inputs such as
`x=(1,1,1+1e-15)` can pass this check and produce unreliable slopes and intervals.
Qualification of such inputs remains follow-up work. Extremely scaled inputs
and probabilities below the floating-point range also remain numerical
limitations. No solver execution or
engineering acceptance limit is part of this example.
