# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent SH-wave hand calculations for a synthetic elastic layer.

The synthetic layer is h=25 m, vs=200 m/s, so h/vs=1/8 s. With
phase=2*pi*f*h/vs, f=0 gives cos(0)=1, f=1 gives cos(pi/4)=sqrt(2)/2,
f=2 gives the first quarter-wave pole, and f=4 gives cos(pi)=-1.
Higher poles follow f=(2*n+1)*vs/(4*h). These arithmetic identities,
not a reference implementation, supply all physical expected values.
Input sentinels below exercise type, sign and floating-point range only.
"""

import math
import sys
from decimal import Decimal, localcontext

import pytest

from digitalmodel.seismic.elastic_layer import elastic_layer_amplification


# Relative tolerance allows a few binary rounding steps in the hand identities;
# epsilon is defined at https://docs.python.org/3/library/sys.html#sys.float_info.
HAND_REL_TOL = 8 * sys.float_info.epsilon


def test_elastic_layer_dc_amplification_is_unity():
    """At DC, zero surface traction gives constant displacement throughout."""
    result = elastic_layer_amplification(0.0, 25.0, 200.0)

    assert result.amplification == 1.0
    assert result.status == "finite"


def test_elastic_layer_off_resonance_matches_hand_cosine():
    """2*pi*1*25/200=pi/4; 1/(sqrt(2)/2)=sqrt(2)."""
    result = elastic_layer_amplification(1.0, 25.0, 200.0)

    assert result.amplification == pytest.approx(math.sqrt(2), rel=HAND_REL_TOL)
    assert result.status == "finite"


def test_elastic_layer_quarter_wave_resonance_is_explicit():
    """vs/(4*h)=200/(4*25)=2 Hz; an undamped pole is unbounded."""
    result = elastic_layer_amplification(2.0, 25.0, 200.0)

    assert result.status == "unbounded"
    assert result.amplification == math.inf


@pytest.mark.parametrize("frequency_hz", [6.0, 10.0, 14.0, 202.0])
def test_higher_odd_quarter_wave_resonances(frequency_hz):
    """Odd multiples of the independently calculated 2 Hz fundamental."""
    result = elastic_layer_amplification(frequency_hz, 25.0, 200.0)

    assert result.status == "unbounded"
    assert result.amplification == math.inf


def test_negative_cosine_still_gives_positive_amplitude():
    result = elastic_layer_amplification(4.0, 25.0, 200.0)

    assert result.status == "finite"
    assert result.amplification == pytest.approx(1.0, rel=HAND_REL_TOL)


def test_equivalent_travel_time_preserves_amplitude():
    """50/400=25/200=1/8 s; the hand pi/4 phase is unchanged."""
    result = elastic_layer_amplification(1.0, 50.0, 400.0)

    assert result.amplification == pytest.approx(math.sqrt(2), rel=HAND_REL_TOL)


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
def test_rounding_neighbours_of_analytical_pole_are_unbounded(direction):
    result = elastic_layer_amplification(math.nextafter(2.0, direction), 25.0, 200.0)

    assert result.status == "unbounded"


@pytest.mark.parametrize("sign", [-1, 1])
def test_resolvable_near_resonance_is_finite(sign):
    """Offset sqrt(eps) in frequency is much greater than the O(eps) band."""
    frequency_hz = 2.0 * (1 + sign * math.sqrt(sys.float_info.epsilon))
    result = elastic_layer_amplification(frequency_hz, 25.0, 200.0)

    assert result.status == "finite"
    assert math.isfinite(result.amplification)
    assert result.amplification > 1.0


@pytest.mark.parametrize("index", range(3))
@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf, -1.0])
def test_rejects_nonfinite_or_negative_inputs(index, invalid):
    values = [1.0, 25.0, 200.0]
    values[index] = invalid

    with pytest.raises(ValueError):
        elastic_layer_amplification(*values)


@pytest.mark.parametrize("index", [1, 2])
def test_rejects_zero_thickness_or_speed(index):
    values = [1.0, 25.0, 200.0]
    values[index] = 0.0

    with pytest.raises(ValueError):
        elastic_layer_amplification(*values)


@pytest.mark.parametrize("index", range(3))
@pytest.mark.parametrize("invalid", [True, "1", None, 1j, [1.0]])
def test_rejects_non_real_scalar_inputs(index, invalid):
    values = [1.0, 25.0, 200.0]
    values[index] = invalid

    with pytest.raises(ValueError):
        elastic_layer_amplification(*values)


def test_dc_does_not_overflow_for_extreme_finite_layer_properties():
    result = elastic_layer_amplification(0.0, sys.float_info.max, sys.float_info.min)

    assert result.status == "finite"
    assert result.amplification == 1.0


@pytest.mark.parametrize(
    "values",
    [
        (sys.float_info.max, sys.float_info.max, sys.float_info.min),
        (sys.float_info.min, sys.float_info.min, sys.float_info.max),
        (1 / sys.float_info.epsilon, 25.0, 200.0),
    ],
)
def test_rejects_unresolvable_nonzero_phase(values):
    """Overflow, underflow and excessive phase uncertainty are refused."""
    with pytest.raises(ValueError, match="phase"):
        elastic_layer_amplification(*values)


@pytest.mark.parametrize(
    "values",
    [
        (sys.float_info.max, 1.0, sys.float_info.max),
        (1.0, sys.float_info.max, sys.float_info.max),
        (sys.float_info.min, sys.float_info.min, sys.float_info.min),
    ],
)
def test_resolvable_phase_survives_extreme_intermediate_scales(values):
    """First two cases give f*h/vs=1; last gives min, whose cosine rounds to 1."""
    result = elastic_layer_amplification(*values)

    assert result.status == "finite"
    assert result.amplification == pytest.approx(1.0, rel=HAND_REL_TOL)


@pytest.mark.parametrize("index", range(3))
def test_rejects_integer_outside_float_range(index):
    """A synthetic integer above the largest float must not leak OverflowError."""
    values = [1.0, 25.0, 200.0]
    values[index] = int(sys.float_info.max) ** 2

    with pytest.raises(ValueError, match="finite real scalar"):
        elastic_layer_amplification(*values)


@pytest.mark.parametrize(
    "frequency_hex, status",
    [
        ("0x1.fffffffffffefp+0", "unbounded"),
        ("0x1.fffffffffffeep+0", "finite"),
        ("0x1.0000000000007p+1", "unbounded"),
        ("0x1.0000000000008p+1", "finite"),
    ],
)
def test_adjacent_inputs_straddle_each_edge_of_pole_band(frequency_hex, status):
    """Adjacent binary64 frequencies on each side of the 2 Hz pole."""
    result = elastic_layer_amplification(float.fromhex(frequency_hex), 25.0, 200.0)

    assert result.status == status
    assert math.isfinite(result.amplification) == (status == "finite")


@pytest.mark.parametrize(
    "frequency_hex, rejected",
    [
        ("0x1.fffffffffffffp+22", False),
        ("0x1.0000000000000p+23", False),
        ("0x1.0000000000001p+23", True),
    ],
)
def test_adjacent_inputs_straddle_absolute_cosine_error_ceiling(
    frequency_hex, rejected
):
    """8*2**-52*2**23 = 2**-26; h=1 and vs=tau isolate the cutoff."""
    frequency = float.fromhex(frequency_hex)
    if rejected:
        with pytest.raises(ValueError, match="phase"):
            elastic_layer_amplification(frequency, 1.0, math.tau)
    else:
        result = elastic_layer_amplification(frequency, 1.0, math.tau)
        assert result.status == "finite"
        assert math.isfinite(result.amplification)


def test_phase_rejection_reports_error_budget_and_accuracy_criterion():
    """An unresolved result must expose the measured bound and its limit."""
    with pytest.raises(
        ValueError,
        match=r"phase cosine uncertainty 1.49e-08 exceeds limit 1.49e-08",
    ):
        elastic_layer_amplification(math.nextafter(2**23, math.inf), 1.0, math.tau)


def _decimal_phase_and_cosine(frequency, thickness, speed, *, argument=None):
    """Independent 100-digit reference, without binary pi or libm cosine.

    The arctangent identity is pi=16*atan(1/5)-4*atan(1/239). The alternating
    atan series after 160 terms has error below 16/(321*5**321).
    Cosine uses its Maclaurin series after reduction to [-pi, pi]; its
    remainder is below pi**320/320!. Decimal roundoff and reduction at
    phases below 2**24 leave the combined reference error below 1e-80.
    Series: NIST Handbook of Mathematical Functions (2010), sections
    4.24(i) (Power Series) and 4.19 (Maclaurin Series and Laurent Series),
    equations 4.24.3/4.19.2; https://dlmf.nist.gov/4.24.E3 and /4.19.E2.
    """
    with localcontext() as context:
        context.prec = 100

        def atan_inverse(divisor):
            term = Decimal(1) / divisor
            total = term
            for index in range(1, 160):
                term = -term / (divisor * divisor)
                total += term / (2 * index + 1)
            return total

        pi = 16 * atan_inverse(5) - 4 * atan_inverse(239)
        phase = 2 * pi * Decimal(frequency) * Decimal(thickness) / Decimal(speed)
        reduced = (phase if argument is None else Decimal(argument)) % (2 * pi)
        if reduced > pi:
            reduced -= 2 * pi
        term = total = Decimal(1)
        for index in range(1, 160):
            term *= -reduced * reduced / ((2 * index - 1) * (2 * index))
            total += term
        return phase, total


@pytest.mark.parametrize(
    "frequency, thickness, speed",
    [
        (0.0, 25.0, 200.0),
        (1.0, 25.0, 200.0),
        (2.0, 25.0, 200.0),
        (float.fromhex("0x1.fffffffffffefp+0"), 25.0, 200.0),
        (float.fromhex("0x1.fffffffffffeep+0"), 25.0, 200.0),
        (float.fromhex("0x1.0000000000007p+1"), 25.0, 200.0),
        (float.fromhex("0x1.0000000000008p+1"), 25.0, 200.0),
        (float.fromhex("0x1.fffffffffffffp+22"), 1.0, math.tau),
        (float(2**23), 1.0, math.tau),
    ],
)
def test_cosine_error_budget_against_independent_decimal_series(
    frequency, thickness, speed, monkeypatch
):
    """Catch underestimated phase and cosine uncertainty at both boundaries."""
    evaluated = []
    libm_cosine = math.cos

    def observe_cosine(argument):
        value = libm_cosine(argument)
        evaluated.append((argument, value))
        return value

    # Observe actual production arguments; the native cosine still evaluates them.
    monkeypatch.setattr(math, "cos", observe_cosine)
    phase, reference = _decimal_phase_and_cosine(frequency, thickness, speed)
    result = elastic_layer_amplification(frequency, thickness, speed)
    with localcontext() as context:
        context.prec = 100
        epsilon = Decimal(2) ** -52
        bound = 8 * epsilon * max(Decimal(1), abs(phase))
        reference_error = Decimal("1e-80")
        # Qualify the documented libm assumption at these arguments only.
        if evaluated:
            argument, cosine = evaluated[0]
            assert abs(Decimal(argument) - phase) <= 3 * epsilon * max(1, abs(phase))
            _, rounded_reference = _decimal_phase_and_cosine(
                frequency, thickness, speed, argument=argument
            )
            assert abs(Decimal(cosine) - rounded_reference) <= epsilon
        if result.status == "finite":
            denominator = Decimal(1) / Decimal(result.amplification)
            # One reciprocal rounding adds at most eps in absolute error.
            assert abs(denominator - abs(reference)) <= bound + epsilon + reference_error
        else:
            # A computed denominator <= bound may lie another bound from truth.
            assert abs(reference) <= 2 * bound + reference_error
