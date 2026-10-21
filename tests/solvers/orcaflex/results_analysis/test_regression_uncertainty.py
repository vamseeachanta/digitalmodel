"""Independent hand-calculated regression uncertainty cases."""

import math

import numpy as np
import pytest
from scipy import stats

from digitalmodel.solvers.orcaflex.results_analysis.statistical_analysis import (
    StatisticalAnalysisService,
)


@pytest.mark.parametrize("repeats", [1, 10])
@pytest.mark.parametrize("with_nan", [False, True])
def test_regression_uncertainty_uses_residual_degrees_of_freedom(repeats, with_nan):
    """Hand calculation, using NIST e-Handbook (2012), section 4.4.3.1.

    https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm
    Synthetic dimensionless inputs: x=(1,2,3,4,5), y=(2,4,5,4,5).
    xbar=3, ybar=4, Sxx=10, Sxy=6, hence slope=3/5, intercept=11/5.
    Predictions=(14,17,20,23,26)/5; residuals=(-4,3,5,-3,-1)/5.
    SSE=(16+9+25+9+1)/25=12/5. Repeating the observations r times
    gives n=5r, SSE=12r/5, Sxx=10r; coefficients do not change.
    NIST's s^2=SSE/(n-p), with p=2 fitted coefficients, therefore gives
    s^2=12r/[5(5r-2)], SE(slope)=s/sqrt(10r), and
    SE(intercept)=s*sqrt(1/(5r)+9/(10r))=s*sqrt(11/(10r)).
    The returned std_error denotes residual s, not SE(slope).
    Two-sided 95% Student intervals use t_(0.975,5r-2); see NIST
    section 1.3.6.7.2 (2012), critical-value probability convention:
    https://www.itl.nist.gov/div898/handbook/eda/section3/eda3672.htm
    Quantiles and tail probabilities come from SciPy's t distribution;
    no expected value is obtained from the regression implementation.
    NaN padding must not increase the number of retained observations.
    """
    # Inputs, replication counts and constants are derived in the docstring.
    x = np.tile(np.array([1., 2., 3., 4., 5.]), repeats)
    y = np.tile(np.array([2., 4., 5., 4., 5.]), repeats)
    if with_nan:
        x = np.append(x, [np.nan, 6.])
        y = np.append(y, [6., np.nan])
    result = StatisticalAnalysisService().perform_linear_regression(x, y)

    dof = 5 * repeats - 2
    residual_se = math.sqrt(12 * repeats / (5 * dof))
    slope_se = residual_se / math.sqrt(10 * repeats)
    intercept_se = residual_se * math.sqrt(11 / (10 * repeats))
    t_critical = stats.t.ppf(0.975, dof)  # NIST section 1.3.6.7.2 above.
    assert result.slope == pytest.approx(3 / 5)
    assert result.intercept == pytest.approx(11 / 5)
    assert result.std_error == pytest.approx(residual_se)
    assert result.confidence_intervals["slope"] == pytest.approx(
        (3 / 5 - t_critical * slope_se, 3 / 5 + t_critical * slope_se)
    )
    assert result.confidence_intervals["intercept"] == pytest.approx(
        (11 / 5 - t_critical * intercept_se, 11 / 5 + t_critical * intercept_se)
    )
    assert result.p_value == pytest.approx(2 * stats.t.sf((3 / 5) / slope_se, dof))
    assert result.residuals == pytest.approx(
        np.tile(np.array([-4., 3., 5., -3., -1.]) / 5, repeats)
    )
