# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a scalar normal limit state.

All inputs are manufactured mathematical examples in arbitrary consistent units;
none are design values or standard-derived factors. Numerical constants below
come from symmetry, the normal density, or the stated quadrature construction.
"""

import math

import pytest

from digitalmodel.subsea.pipeline.reliability import normal_limit_state


def test_normal_limit_state_equal_means_has_half_probability():
    """Equal means give a symmetric difference: beta = 0 and P(g < 0) = 1/2."""
    beta, probability = normal_limit_state(10.0, 3.0, 10.0, 4.0)
    assert beta == 0.0
    assert probability == 0.5


def _simpson(function, lower, upper, panels):
    """Integrate using paired quadratic panels with weights 1, 4, 2, ..., 1."""
    step = (upper - lower) / panels
    weighted = math.fsum(
        (4 if index % 2 else 2) * function(lower + index * step)
        for index in range(1, panels)
    )
    return step * (function(lower) + weighted + function(upper)) / 3


def test_normal_limit_state_matches_independent_density_quadrature():
    """Integrate f_R(r) f_L(l) over l > r without a normal CDF or erfc.

    Manufactured inputs: means 2 and 1, deviations 1 and 2; hand beta is
    (2 - 1) / sqrt(1 + 4). Each density is truncated at ten deviations;
    the normal tail bound exp(-10**2 / 2) / (10 * sqrt(2*pi)) is below
    1e-22 per tail. Eight hundred Simpson panels in each integral give
    fourth-order convergence; doubling the panels checks the error against
    an absolute tolerance of 1e-8, well above floating-point roundoff.
    """
    resistance_mean, resistance_std = 2.0, 1.0
    load_mean, load_std = 1.0, 2.0

    def density(value, mean, std):
        standardized = (value - mean) / std
        return math.exp(-standardized**2 / 2) / (std * math.sqrt(2 * math.pi))

    def integrate(panels):
        def integrand(resistance):
            load_tail = _simpson(
                lambda load: density(load, load_mean, load_std),
                resistance,
                load_mean + 10 * load_std,
                panels,
            )
            return density(resistance, resistance_mean, resistance_std) * load_tail

        return _simpson(
            integrand,
            resistance_mean - 10 * resistance_std,
            resistance_mean + 10 * resistance_std,
            panels,
        )

    coarse = integrate(400)
    fine = integrate(800)
    beta, probability = normal_limit_state(
        resistance_mean, resistance_std, load_mean, load_std
    )
    assert beta == pytest.approx(1 / math.sqrt(5))
    assert abs(coarse - fine) < 1e-8
    assert probability == pytest.approx(fine, abs=1e-8)


@pytest.mark.parametrize("scale", [1e-200, 1e-3, 1e3, 1e200])
def test_normal_limit_state_preserves_unit_rescaling(scale):
    """Positive powers of ten cancel from (10 - 5) / sqrt(3**2 + 4**2)."""
    original = normal_limit_state(10.0, 3.0, 5.0, 4.0)
    scaled = normal_limit_state(10.0 * scale, 3.0 * scale, 5.0 * scale, 4.0 * scale)
    assert original[0] == 1.0
    assert scaled == pytest.approx(original)


@pytest.mark.parametrize("stds", [(0.0, 5.0), (5.0, 0.0)])
def test_normal_limit_state_allows_one_deterministic_input(stds):
    """One zero variance leaves sigma_g = 5 and beta = (10 - 5) / 5."""
    beta, probability = normal_limit_state(10.0, stds[0], 5.0, stds[1])
    assert beta == 1.0
    assert 0.0 < probability < 0.5


def test_normal_limit_state_reversing_means_complements_probability():
    """Reflection changes beta's sign and swaps the two sides of g = 0."""
    positive = normal_limit_state(10.0, 3.0, 5.0, 4.0)
    negative = normal_limit_state(5.0, 3.0, 10.0, 4.0)
    assert negative[0] == -positive[0]
    assert negative[1] == pytest.approx(1 - positive[1])


def test_normal_limit_state_retains_small_tail_probability():
    """For beta = 10, integrate the density from 10 to 12 independently.

    The omitted tail is below exp(-12**2 / 2) / (12 * sqrt(2*pi)),
    less than 1e-9 of the retained integral. Two hundred Simpson panels
    and a relative tolerance of 1e-6 resolve cancellation to zero in 1-CDF.
    """
    expected = _simpson(
        lambda value: math.exp(-value**2 / 2) / math.sqrt(2 * math.pi),
        10.0,
        12.0,
        200,
    )
    beta, probability = normal_limit_state(10.0, 1.0, 0.0, 0.0)
    assert beta == 10.0
    assert probability == pytest.approx(expected, rel=1e-6, abs=0.0)


@pytest.mark.parametrize("mean", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("argument", ["resistance_mean", "load_mean"])
def test_normal_limit_state_rejects_nonfinite_means(argument, mean):
    """Finite means are required; NaN and either infinity cannot define them."""
    inputs = dict(resistance_mean=10.0, resistance_std=3.0, load_mean=5.0, load_std=4.0)
    inputs[argument] = mean
    with pytest.raises(ValueError, match=argument + " must be finite"):
        normal_limit_state(**inputs)


@pytest.mark.parametrize("std", [-1.0, math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("argument", ["resistance_std", "load_std"])
def test_normal_limit_state_rejects_invalid_deviations(argument, std):
    """A deviation is finite and nonnegative by its mathematical definition."""
    inputs = dict(resistance_mean=10.0, resistance_std=3.0, load_mean=5.0, load_std=4.0)
    inputs[argument] = std
    with pytest.raises(ValueError, match=argument + " must be finite and nonnegative"):
        normal_limit_state(**inputs)


def test_normal_limit_state_rejects_zero_combined_variance():
    """Two zero deviations make division by sigma_g undefined."""
    with pytest.raises(ValueError, match="combined variance must be positive"):
        normal_limit_state(10.0, 0.0, 5.0, 0.0)


@pytest.mark.parametrize("scale", [1e-300, 1e308, 1.7e308])
def test_normal_limit_state_handles_extreme_finite_scales(scale):
    """For means +/-scale and deviations scale, beta = 2 / sqrt(2).

    The scales exercise tiny deviations, subtraction overflow and combined
    deviation overflow, respectively, while the hand beta stays finite.
    """
    beta, probability = normal_limit_state(scale, scale, -scale, scale)
    assert beta == pytest.approx(math.sqrt(2))
    # Mirror symmetry is independent of the absolute scale.
    mirrored = normal_limit_state(-scale, scale, scale, scale)
    assert probability + mirrored[1] == pytest.approx(1.0)


def test_normal_limit_state_handles_combined_deviation_overflow():
    """Equal deviations near the float limit give beta = 1 / sqrt(2).

    The mean difference is finite, while hypot would exceed the float range.
    The manufactured scale changes neither the exact beta nor its sign.
    """
    scale = 1.7e308
    beta, probability = normal_limit_state(scale, scale, 0.0, scale)
    assert beta == pytest.approx(1 / math.sqrt(2))
    assert 0.0 < probability < 0.5


@pytest.mark.parametrize("sign", [1.0, -1.0])
@pytest.mark.parametrize(
    "mean,std,expected_beta",
    [
        (1e308, 0.8, 1.7677669529663688e308),
        (1e308, 1.0, 1.4142135623730951e308),
        (1e308, 2.0, 7.071067811865476e307),
        (1.7e308, 2.0, 1.2020815280171309e308),
    ],
)
def test_normal_limit_state_preserves_finite_beta_when_margin_overflows(
    sign, mean, std, expected_beta
):
    """Opposite means overflow on subtraction while their beta remains finite."""
    # Hand beta: (mean - (-mean)) / sqrt(2*std**2) = sqrt(2)*(mean/std).
    # For std = 1, sqrt(2)*1e308 = 1.4142135623730951e308, within float range.
    beta, probability = normal_limit_state(sign * mean, std, -sign * mean, std)
    assert math.isfinite(beta)
    assert beta == pytest.approx(sign * expected_beta, rel=2e-15)
    assert probability == (0.0 if sign > 0 else 1.0)


@pytest.mark.parametrize("sign", [1.0, -1.0])
@pytest.mark.parametrize(
    "scale", [math.ulp(0.0), 2 * math.ulp(0.0), 3 * math.ulp(0.0), 1e-310, 1.0]
)
def test_normal_limit_state_preserves_subnormal_deviation_ratios(sign, scale):
    """Unit rescaling must preserve beta and the independently integrated tail."""
    # Hand beta: (scale - 0) / sqrt(2*scale**2) = 1/sqrt(2).
    # The standardized density integral from 1/sqrt(2) to infinity is
    # 0.23975006109347674; truncating at 10 omits less than 1e-22.
    expected_tail = _simpson(
        lambda value: math.exp(-value**2 / 2) / math.sqrt(2 * math.pi),
        1 / math.sqrt(2),
        10.0,
        8000,
    )
    assert expected_tail == pytest.approx(0.23975006109347674, abs=2e-14)
    beta, probability = normal_limit_state(sign * scale, scale, 0.0, scale)
    assert beta == pytest.approx(sign * 0.7071067811865475, rel=2e-15)
    expected_probability = expected_tail if sign > 0 else 1 - expected_tail
    assert probability == pytest.approx(expected_probability, abs=2e-14)


@pytest.mark.parametrize("sign", [1.0, -1.0])
def test_normal_limit_state_preserves_finite_beta_when_scaled_margin_overflows(sign):
    """The normalized margin overflows, but division by the norm rescues beta."""
    # Hand beta: 1.7e308 / sqrt(2*0.8**2) = 1.5026019100214134e308.
    beta, probability = normal_limit_state(sign * 1.7e308, 0.8, 0.0, 0.8)
    assert math.isfinite(beta)
    assert beta == pytest.approx(sign * 1.5026019100214134e308, rel=2e-15)
    assert probability == (0.0 if sign > 0 else 1.0)


@pytest.mark.parametrize("sign", [1.0, -1.0])
def test_normal_limit_state_allows_truly_unrepresentable_beta(sign):
    """Beta beyond the float range retains its sign and limiting probability."""
    # Hand beta: sqrt(2)*(1e308/0.5) exceeds the largest finite float.
    beta, probability = normal_limit_state(sign * 1e308, 0.5, -sign * 1e308, 0.5)
    assert beta == math.copysign(math.inf, sign)
    assert probability == (0.0 if sign > 0 else 1.0)
