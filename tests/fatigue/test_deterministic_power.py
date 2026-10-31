"""The fatigue engine's power must be correctly rounded, on every CPU."""

import math
import random
from decimal import Context, Decimal, FloatOperation, localcontext
from fractions import Fraction

import pytest

from digitalmodel.fatigue import _deterministic
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


def test_caller_float_operation_trap_is_not_honoured():
    # Round 3: Decimal(float) raised under a caller's trapped FloatOperation.
    expected = (deterministic_power(30.0 / 25.0, 0.25), deterministic_power(7.5, -3.5),
                deterministic_log10(3.0))
    with localcontext() as caller:
        caller.clear_flags()
        caller.traps[FloatOperation] = True
        caller.prec = 3
        result = (deterministic_power(30.0 / 25.0, 0.25), deterministic_power(7.5, -3.5),
                  deterministic_log10(3.0))
        assert not any(caller.flags.values())  # the caller's context is untouched
    assert result == expected


# 208065 ** 3 = 9007351116674625 is odd with 54 significant bits: it lies
# exactly halfway between the floats 9007351116674624 and 9007351116674626.
_MIDPOINT = 208065 ** 3


def test_exact_midpoint_rounds_half_to_even():
    assert _MIDPOINT.bit_length() == 54 and _MIDPOINT % 2 == 1
    base = float(208065 ** 2)
    assert base == 208065 ** 2
    result = deterministic_power(base, 1.5)
    assert result == float(Fraction(_MIDPOINT))  # IEEE: ties to even
    assert result == _MIDPOINT - 1


@pytest.mark.parametrize("offset, expected", [("1", _MIDPOINT + 1), ("-1", _MIDPOINT - 1)])
def test_value_near_a_midpoint_is_settled_at_higher_precision(offset, expected):
    # Exact value: the midpoint plus or minus 1e-30. At 40 digits it rounds to
    # the midpoint itself, which would then round half to even (down).
    exact = Context(prec=100).add(Decimal(_MIDPOINT), Decimal(offset + "e-30"))
    assert len(exact.as_tuple().digits) == 46  # held exactly
    assert Context(prec=40).plus(exact) == _MIDPOINT
    assert _deterministic._rounded_once(lambda context: context.plus(exact)) == expected


def test_log10_is_correctly_rounded():
    rng = random.Random(13)
    reference = Context(prec=120)
    for _ in range(500):
        value = 10.0 ** rng.uniform(-30.0, 30.0)
        assert deterministic_log10(value) == float(reference.log10(Decimal(value)))
