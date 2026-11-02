# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Equal-cycle comparison with the existing Rayleigh long-term calculation."""

import math

import numpy as np
import pytest

from digitalmodel.orcawave.motion_statistics import ScatterCell
from digitalmodel.orcawave.weibull_fit import (
    WeibullDistribution,
    compare_rayleigh_extreme,
    fit_weibull,
)


def comparison_inputs(cycles=1000):
    """Hand trapezoids: omega=[1,2,3], S=[.5,.5,.5] gives m0=1,m2=4.5.

    Thus sigma=1, Tz=2pi/sqrt(4.5). Choose years for exactly N cycles,
    using the Julian year conversion of motion_statistics.long_term_extreme.
    """
    tz = 2 * math.pi / math.sqrt(4.5)
    return dict(
        scatter=[ScatterCell(hs=1, tp=1, probability=1)],
        rao_amplitude=np.ones(3),
        omega=np.array([1.0, 2.0, 3.0]),
        spectrum_func=lambda omega, hs, tp: np.full_like(omega, 0.5),
        return_period_years=cycles * tz / (365.25 * 24 * 3600),
    )


def test_rayleigh_equal_cycle_exact():
    """NIST W: k=2, lambda=sqrt(2), gamma=0 implies q=sqrt(2lnN).

    The existing single-cell expected-exceedance root has the same equation.
    """
    result = compare_rayleigh_extreme(
        WeibullDistribution(2, math.sqrt(2)), **comparison_inputs()
    )
    assert result.cycles == pytest.approx(1000)
    assert result.weibull_return_level == pytest.approx(math.sqrt(2 * math.log(1000)))
    assert result.rayleigh_return_level == pytest.approx(math.sqrt(2 * math.log(1000)))
    assert result.ratio == pytest.approx(1)
    assert result.difference_percent == pytest.approx(0, abs=1e-10)


def test_rayleigh_sample_fitted_ratio():
    """Sampling tolerance from binomial counting and a hand delta calculation.

    For N=1000 and n=20000 peaks, p=1-1/N, SE(Fhat)=sqrt(p(1-p)/n).
    Rayleigh f(q)=q/(N sigma^2); so SE(q)/q=sqrt((N-1)/n)/(2lnN).
    Six times that value is a conservative synthetic regression envelope
    (about 9.7%); six is a chosen test allowance, not a published critical
    value. It is not claimed to give formal coverage for a fitted law.
    Source: NIST W CDF and differentiation; Bernoulli variance p(1-p).
    """
    count, cycles = 20000, 1000
    samples = math.sqrt(2) * np.random.default_rng(31).weibull(2, count)
    fit = fit_weibull(
        samples, method="mle", location_bounds=(-0.5, 0), shape_bounds=(0.5, 5)
    )
    result = compare_rayleigh_extreme(fit, **comparison_inputs(cycles))
    tolerance = 6 * math.sqrt((cycles - 1) / count) / (2 * math.log(cycles))
    assert abs(result.ratio - 1) < tolerance
    wrong = compare_rayleigh_extreme(
        WeibullDistribution(1, math.sqrt(2)), **comparison_inputs(cycles)
    )
    assert abs(wrong.ratio - 1) > tolerance


def test_location_sensitivity_exact():
    """At fixed k,lambda, shifting gamma by 1 shifts every Q_N by 1 (NIST W)."""
    a = compare_rayleigh_extreme(
        WeibullDistribution(2, math.sqrt(2), 0), **comparison_inputs()
    )
    b = compare_rayleigh_extreme(
        WeibullDistribution(2, math.sqrt(2), 1), **comparison_inputs()
    )
    assert b.weibull_return_level - a.weibull_return_level == pytest.approx(1)
    assert b.ratio - a.ratio == pytest.approx(1 / a.rayleigh_return_level)


def test_multiple_cells_use_cycle_rates():
    """Identical spectra with time fractions .25,.75 retain N=sum(p/Tz)*R."""
    args = comparison_inputs()
    args["scatter"] = [
        ScatterCell(hs=1, tp=1, probability=0.25),
        ScatterCell(hs=2, tp=1, probability=0.75),
    ]
    result = compare_rayleigh_extreme(WeibullDistribution(2, math.sqrt(2)), **args)
    assert result.cycles == pytest.approx(1000)
    assert result.ratio == pytest.approx(1)


def test_invalid_comparison_model():
    with pytest.raises(ValueError):
        compare_rayleigh_extreme(None, **comparison_inputs())


def test_insufficient_cycle_budget():
    with pytest.raises(ValueError, match="Cycles"):
        compare_rayleigh_extreme(WeibullDistribution(2, 1), **comparison_inputs(0.5))
