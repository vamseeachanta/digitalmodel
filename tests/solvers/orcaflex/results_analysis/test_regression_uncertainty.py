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


@pytest.mark.parametrize("repeats", [1, 10])
@pytest.mark.parametrize("confidence_level", [0.90, 0.99])
def test_coefficient_intervals_agree_with_scipy(repeats, confidence_level):
    """Check n=5 and n=50 against an independent regression implementation.

    Inputs and replication counts are the hand calculation above. Confidence
    levels are synthetic probability inputs, not acceptance criteria. Expected
    coefficient standard errors come from scipy.stats.linregress, documented
    in SciPy 1.16.2 (2025), including its Student interval example:
    https://docs.scipy.org/doc/scipy-1.16.2/reference/generated/scipy.stats.linregress.html
    """
    x = np.tile(np.array([1., 2., 3., 4., 5.]), repeats)
    y = np.tile(np.array([2., 4., 5., 4., 5.]), repeats)
    reference = stats.linregress(x, y)
    result = StatisticalAnalysisService().perform_linear_regression(
        x, y, confidence_level=confidence_level
    )
    # Two coefficients and two tails: NIST section 4.4.3.1 and SciPy above.
    critical = stats.t.isf((1 - confidence_level) / 2, len(x) - 2)
    assert result.slope == pytest.approx(reference.slope)
    assert result.intercept == pytest.approx(reference.intercept)
    assert result.p_value == pytest.approx(reference.pvalue)
    assert result.confidence_intervals["slope"] == pytest.approx(
        (reference.slope - critical * reference.stderr,
         reference.slope + critical * reference.stderr)
    )
    assert result.confidence_intervals["intercept"] == pytest.approx(
        (reference.intercept - critical * reference.intercept_stderr,
         reference.intercept + critical * reference.intercept_stderr)
    )


@pytest.mark.parametrize("retained", [0, 1, 2])
def test_insufficient_retained_pairs_rejected(retained):
    """NIST 4.4.3.1: n-p must be positive; p=2 requires n>=3.

    Synthetic arrays have three raw rows and zero, one or two retained pairs.
    No residual variance or Student interval can be estimated in these cases.
    """
    x = np.arange(3, dtype=float)
    y = x.copy()
    y[retained:] = np.nan
    with pytest.raises(ValueError, match="at least 3 valid data points"):
        StatisticalAnalysisService().perform_linear_regression(x, y)


def test_constant_predictor_rejected():
    """x=(1,1,1) gives Sxx=0, so neither slope nor its SE is identifiable.

    SciPy 1.16.2 (2025), linregress implementation rejects identical x values:
    https://github.com/scipy/scipy/blob/v1.16.2/scipy/stats/_stats_py.py
    """
    with pytest.raises(ValueError, match="distinct"):
        StatisticalAnalysisService().perform_linear_regression(
            np.ones(3), np.arange(3, dtype=float)
        )


@pytest.mark.parametrize("value", [np.inf, -np.inf])
@pytest.mark.parametrize("axis", ["x", "y"])
def test_infinite_retained_values_rejected(value, axis):
    """Synthetic infinities cannot define finite SSE, Sxx or intervals."""
    data = {"x": np.arange(3, dtype=float), "y": np.arange(3, dtype=float)}
    data[axis][0] = value
    with pytest.raises(ValueError, match="finite"):
        StatisticalAnalysisService().perform_linear_regression(data["x"], data["y"])


@pytest.mark.parametrize("confidence_level", [0., 1., -0.1, 1.1, np.nan, np.inf])
def test_invalid_confidence_level_rejected(confidence_level):
    """A finite two-sided Student quantile requires probability in (0,1).

    Endpoints and out-of-domain synthetic inputs follow the quantile convention
    in NIST e-Handbook (2012), section 1.3.6.7.2, cited above.
    """
    with pytest.raises(ValueError, match="confidence_level"):
        StatisticalAnalysisService().perform_linear_regression(
            np.arange(3, dtype=float), np.arange(3, dtype=float), confidence_level
        )


@pytest.mark.parametrize("slope", [-1., 0., 1.])
def test_zero_residual_variance_has_explicit_inference(slope):
    """Hand calculation: x=(-1,-1,1,1), y=b*x, b in {-1,0,1}.

    xbar=ybar=0, Sxx=4, fitted slope=b, intercept=0 and SSE=0. NIST
    4.4.3.1 gives s=SE(slope)=SE(intercept)=0, so intervals collapse.
    For b!=0 the two-sided Student tail tends to zero as s tends to zero;
    for b=0, t=0/0 is undefined and this API reports NaN explicitly. These
    are limiting/algebraic cases, not evidence of measured zero uncertainty.
    """
    x = np.array([-1., -1., 1., 1.])
    # Turn numerical warnings into failures, independently of global filters.
    with np.errstate(divide="raise", invalid="raise"):
        result = StatisticalAnalysisService().perform_linear_regression(x, slope * x)
    assert result.std_error == 0
    assert result.confidence_intervals["slope"] == (slope, slope)
    assert result.confidence_intervals["intercept"] == (0., 0.)
    if slope:
        assert result.p_value == 0
    else:
        assert math.isnan(result.p_value)


def test_small_nonzero_p_value_preserved():
    """Hand calculation: add 10*x to the five-point case, repeated ten times.

    This gives n=50, slope=53/5, intercept=11/5, SSE=24, Sxx=100,
    s^2=24/48=1/2, SE(slope)=sqrt(1/200), t=(53/5)*sqrt(200).
    Expected p=2*Student_sf(t,48), using NIST 4.4.3.1 and the two-sided
    tail convention in SciPy 1.16.2 (2025), _get_pvalue in _stats_py.py.
    Positive and negative slopes must have the same two-sided probability.
    Compare the probability ratio to unity with pytest's default tolerance;
    this prevents a zero result from passing an absolute-error comparison
    simply because the independent probability is very small.
    """
    x = np.tile(np.array([1., 2., 3., 4., 5.]), 10)
    y = np.tile(np.array([2., 4., 5., 4., 5.]), 10) + 10 * x
    expected = 2 * stats.t.sf((53 / 5) * math.sqrt(200), 48)
    assert expected > 0
    for sign in [-1, 1]:
        result = StatisticalAnalysisService().perform_linear_regression(x, sign * y)
        assert result.p_value / expected == pytest.approx(1)


@pytest.mark.parametrize("x", [np.array(1.), np.ones((3, 1))])
def test_non_vector_input_rejected(x):
    """Scalar and matrix fixtures cannot represent paired observation vectors."""
    with pytest.raises(ValueError, match="one-dimensional"):
        StatisticalAnalysisService().perform_linear_regression(x, np.arange(3.))
