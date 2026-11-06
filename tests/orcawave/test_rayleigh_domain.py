"""Rayleigh survival-domain regressions for audit section 11.

Source: SciPy 1.14.1 reference (2024), scipy.stats.rayleigh, Notes:
https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.rayleigh.html
Integrating f(u)=u*exp(-u**2/2) from x/sigma to infinity gives
S(x)=exp(-(x/sigma)**2/2), x>=0, sigma>0. No solver is required.
"""

import math

import numpy as np
import pytest

from digitalmodel.orcawave.motion_statistics import rayleigh_exceedance


@pytest.mark.parametrize("amplitude", [-1.0, -math.inf, math.inf, math.nan])
def test_exceedance_rejects_invalid_amplitude(amplitude):
    """Reject thresholds outside the declared finite, nonnegative domain."""
    with pytest.raises(ValueError, match="amplitude"):
        rayleigh_exceedance(amplitude, sigma=1.0)


@pytest.mark.parametrize("sigma", [0.0, -1.0, -math.inf, math.inf, math.nan])
def test_exceedance_rejects_invalid_scale(sigma):
    """Zero, negative and nonfinite scales do not define this distribution."""
    with pytest.raises(ValueError, match="sigma"):
        rayleigh_exceedance(1.0, sigma=sigma)


@pytest.mark.parametrize("sigma", [1e-300, 1.0, 1e300])
def test_exceedance_zero_threshold(sigma):
    """Hand calculation: S(0)=exp(0)=1 at every positive scale."""
    assert rayleigh_exceedance(0.0, sigma=sigma) == 1.0


@pytest.mark.parametrize("sigma", [1e-300, 1.0, 1e300])
def test_exceedance_depends_on_ratio_without_squaring_dimensions(sigma):
    """Hand calculation: x=2*sigma gives S=exp(-4/2)=0.1353352832366127.

    The scale extremes are synthetic floating-point stress inputs, not physical
    coefficients. The numerical rounding budget is eight binary64 epsilons for
    division, squaring, multiplication and the exponential evaluation.
    """
    assert rayleigh_exceedance(2.0 * sigma, sigma) == pytest.approx(
        0.1353352832366127, rel=8 * np.finfo(float).eps, abs=0
    )


def test_exceedance_far_tail_underflows_to_zero():
    """x/sigma=1e600: exp(-0.5e1200) rounds to zero in binary64.

    These synthetic finite inputs must not raise a floating-point warning or
    overflow exception. The zero is a valid tail probability, unlike bad input.
    """
    with np.errstate(all="raise"):
        assert rayleigh_exceedance(1e300, sigma=1e-300) == 0.0


def test_exceedance_near_origin_rounds_to_one():
    """x/sigma=1e-600: exp(-0.5e-1200) rounds to one in binary64."""
    with np.errstate(all="raise"):
        assert rayleigh_exceedance(1e-300, sigma=1e300) == 1.0
