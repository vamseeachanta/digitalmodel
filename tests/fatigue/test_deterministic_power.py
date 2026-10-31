"""The fatigue engine's power must be exact or once-rounded, on every CPU."""

import math
import random
from decimal import Context, Decimal
from fractions import Fraction

import pytest

from digitalmodel.fatigue._deterministic import deterministic_log10, deterministic_power


def test_integer_exponents_are_exact_then_rounded_once():
    rng = random.Random(7)
    for _ in range(500):
        base = rng.uniform(1e-3, 1e3)
        k = rng.choice([-5, -4, -3, 3, 4, 5])
        assert deterministic_power(base, k) == float(Fraction(base) ** k)


def test_fractional_exponents_match_a_high_precision_reference():
    rng = random.Random(11)
    reference = Context(prec=80)
    for _ in range(500):
        base = rng.uniform(1e-3, 1e3)
        exponent = rng.uniform(-6.0, 6.0)
        expected = float(reference.power(Decimal(base), Decimal(exponent)))
        assert deterministic_power(base, exponent) == expected


@pytest.mark.parametrize("base, exponent, expected", [
    (0.0, -3.0, math.inf),
    (0.0, -0.25, math.inf),
    (0.0, 3.0, 0.0),
    (0.0, 0.25, 0.0),
    (1e200, 5.0, math.inf),
    (1e200, 2.5, math.inf),
    (1e-200, 5.0, 0.0),
    (1e-200, 2.5, 0.0),
    (7.0, 0.0, 1.0),
])
def test_ieee_like_edges(base, exponent, expected):
    assert deterministic_power(base, exponent) == expected


@pytest.mark.parametrize("base, exponent", [(-1.0, 2.0), (math.nan, 2.0), (2.0, math.inf), (math.inf, 1.0)])
def test_invalid_operands_are_rejected(base, exponent):
    with pytest.raises(ValueError):
        deterministic_power(base, exponent)


def test_log10_of_knee_cycles_is_exact():
    assert deterministic_log10(1e6) == 6.0
    assert deterministic_log10(1e7) == 7.0
    with pytest.raises(ValueError):
        deterministic_log10(0.0)
