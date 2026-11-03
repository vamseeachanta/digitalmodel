"""Independent scatter-interval examples for normally distributed differences.

Source: NIST/SEMATECH e-Handbook, online edition accessed 2026-10-19,
sections 1.3.5.8 (standard-deviation interval) and 1.3.6.6.6 (chi-square CDF):
https://www.itl.nist.gov/div898/handbook/eda/section3/eda358.htm
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3666.htm

Independent hand calculation for the cases below: put z=x/2 and a=df/2.
Integrating the cited density gives P(1,z)=1-exp(-z),
P(1/2,z)=erf(sqrt(z)), and, by integration by parts,
P(a+1,z)=P(a,z)-exp(-z)*z**a/Gamma(a+1). Starting at 1 or 1/2,
apply that recurrence and bisect P(df/2,x/2)=p for p=0.025 and 0.975
(the equal tails of the specified 95% interval). The resulting quantiles
are recorded below; no production function or SciPy generates expectations.
For example, df=2 gives q(p)=-2*log(1-p) directly.

Use synthetic readings (-sqrt(df/2), sqrt(df/2), then df-1 zeros):
n=df+1, mean=0, sum of squares=df, hence sample variance=df/(n-1)=1.
The expected SD endpoints are therefore sqrt(df/q(0.975)) and
sqrt(df/q(0.025)), from the cited interval formula. Quantiles are rounded
to twelve significant figures; the relative comparison tolerance of 1e-9
allows that rounding and floating-point arithmetic, not engineering error.
"""

import math

import pytest

from digitalmodel.asset_integrity.campaign_comparability import (
    gauge_from_short_interval,
)


@pytest.mark.parametrize(
    "df, q025, q975",
    [
        (1, 0.000982069117175, 5.02388618731),
        (2, 0.0506356159686, 7.37775890823),
        (13, 5.00875051181, 24.7356048849),
        (16, 6.90766435350, 28.8453507234),
        (20, 9.59077739226, 34.1696069028),
        (21, 10.2828977825, 35.4788759057),
        (100, 74.2219274749, 129.561197186),
    ],
)
def test_scatter_interval_uses_actual_degrees_of_freedom(df, q025, q975):
    """Both endpoints follow the independently integrated quantiles above."""
    amplitude = math.sqrt(df / 2)
    differences = [-amplitude, amplitude] + [0.0] * (df - 1)

    result = gauge_from_short_interval(differences)

    assert result.difference_sd == pytest.approx(1.0)
    assert result.difference_sd_ci95 == pytest.approx(
        (math.sqrt(df / q975), math.sqrt(df / q025)), rel=1e-9
    )


@pytest.mark.parametrize("count", [14, 22])
def test_constant_differences_have_zero_scatter_interval(count):
    """Synthetic zero differences give s=0 and both cited endpoints zero.

    Counts exercise df=13 and df=21, including the old table boundary.
    """
    result = gauge_from_short_interval([0.0] * count)

    assert result.difference_sd_ci95 == (0.0, 0.0)
