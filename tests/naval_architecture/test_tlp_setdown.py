# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent geometry checks; see the derivation in docs/naval_architecture/tlp-setdown.md."""

import math
from decimal import Decimal, localcontext

import pytest

from digitalmodel.naval_architecture.platforms.tlp import tlp_setdown


@pytest.mark.parametrize(
    "length, offset, span, drop",
    [(5.0, 3.0, 4.0, 1.0), (13.0, 5.0, 12.0, 1.0)],
)
def test_right_triangle_setdown(length, offset, span, drop):
    """Hand calculation: 3² + 4² = 5²; 5² + 12² = 13², in metres."""
    result = tlp_setdown(length, offset)
    assert result.vertical_span_m == pytest.approx(span)
    assert result.setdown_m == pytest.approx(drop)


def test_zero_offset():
    """At x = 0 the vertical span equals the synthetic 5 m tendon length."""
    result = tlp_setdown(tendon_length_m=5.0, horizontal_offset_m=0.0)
    assert result.vertical_span_m == 5.0
    assert result.setdown_m == 0.0


def test_sign_symmetry():
    """The squared offset gives identical geometry for ±3 m on a 5 m tendon."""
    assert tlp_setdown(5.0, -3.0) == tlp_setdown(5.0, 3.0)


@pytest.mark.parametrize("offset", [1e-4, 1e-8, 1e-12])
def test_small_offset_stability(offset):
    """Synthetic metre offsets probe cancellation; x²/(2L) is the limit."""
    # Independent subtraction at 80 decimal digits resolves the smallest drop.
    # The 1e-14 relative tolerance allows float rounding; no absolute tolerance
    # may hide a zero result. These are numerical test choices, not design limits.
    length = 5.0  # Synthetic length, as in the hand-calculated triangle above.
    with localcontext() as context:
        context.prec = 80
        tendon = Decimal.from_float(length)
        horizontal = Decimal.from_float(offset)
        expected = float(tendon - (tendon**2 - horizontal**2).sqrt())
    result = tlp_setdown(length, offset)
    assert result.setdown_m > 0.0
    assert result.setdown_m == pytest.approx(expected, rel=1e-14, abs=0.0)
    # Binomial expansion: relative correction ~ (x/L)²/4, below 1e-9 here.
    assert result.setdown_m == pytest.approx(
        offset**2 / (2.0 * length), rel=1e-9, abs=0.0
    )


@pytest.mark.parametrize(
    "length, offset",
    [
        (True, 0.0), (False, 0.0), (5.0, True), (5.0, False),
        (float("nan"), 0.0), (float("inf"), 0.0), (-float("inf"), 0.0),
        (5.0, float("nan")), (5.0, float("inf")), (5.0, -float("inf")),
        (0.0, 0.0), (-5.0, 0.0), (5.0, 5.0), (5.0, -5.0),
        (5.0, 6.0), (5.0, -6.0), ("5", 0.0), (5.0, "3"),
        (5.0 + 0j, 0.0), (5.0, None),
    ],
)
def test_invalid_tendon_domain(length, offset):
    """Perturb the synthetic triangle with nonreal or unsupported geometry."""
    with pytest.raises((TypeError, ValueError)):
        tlp_setdown(length, offset)


@pytest.mark.parametrize("scale", [1e-200, 1.0, 1e200])
def test_length_scale_invariance(scale):
    """Scaling the 3-4-5 triangle tests squared underflow and overflow."""
    result = tlp_setdown(5.0 * scale, 3.0 * scale)
    assert result.vertical_span_m / scale == pytest.approx(4.0)
    assert result.setdown_m / scale == pytest.approx(1.0)


def test_offset_just_below_limit():
    """Adjacent floats test the open geometry boundary using decimal geometry."""
    length = 5.0  # Same synthetic triangle length.
    offset = math.nextafter(length, 0.0)
    with localcontext() as context:
        context.prec = 80  # As in the independent small-offset oracle.
        expected = float(
            (Decimal.from_float(length)**2 - Decimal.from_float(offset)**2).sqrt()
        )
    result = tlp_setdown(length, offset)
    assert result.vertical_span_m == pytest.approx(expected, rel=1e-14, abs=0.0)
    assert result.setdown_m < length
