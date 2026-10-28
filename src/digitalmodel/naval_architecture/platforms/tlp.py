# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Idealized TLP set-down from an initially vertical, inextensible tendon.

Geometric basis: right-triangle identity L² = x² + h², with set-down s = L - h.
Rationalization gives s = x² / (L + h), avoiding subtraction near zero offset.
See docs/naval_architecture/tlp-setdown.md for the self-contained derivation,
units, synthetic example and exclusions. No empirical constants are used.

Tendon load, slack, pretension, restoring force and stability: NOT_EVALUATED.
"""

import math
from dataclasses import dataclass
from numbers import Real


@dataclass(frozen=True)
class TLPSetdown:
    """Geometric vertical span and downward displacement, both in metres."""

    vertical_span_m: float
    setdown_m: float


def _finite_real(value: Real, name: str) -> float:
    """Validate one real scalar representable as a finite float."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise TypeError(f"{name} must be a real number, excluding booleans")
    try:
        result = float(value)
    except OverflowError as exc:
        raise ValueError(f"{name} must be representable as a finite float") from exc
    if not math.isfinite(result):
        raise ValueError(f"{name} must be finite")
    return result


def tlp_setdown(tendon_length_m: Real, horizontal_offset_m: Real) -> TLPSetdown:
    """Return span and set-down for a rigid body with a straight tendon.

    The anchor is fixed and the tendon is initially vertical. The rigid body
    translates horizontally and vertically without rotation; the tendon length
    remains constant. Positive set-down means downward motion. The geometry
    presumes a taut tendon but does not establish tension or physical feasibility.

    Args:
        tendon_length_m: Positive finite tendon length in metres.
        horizontal_offset_m: Signed finite horizontal offset in metres, with
            absolute value strictly smaller than the tendon length.

    Returns:
        Vertical span above the anchor and set-down from the initial elevation,
        in metres. Floating-point rounding applies; subnormal results may
        underflow to zero.

    Raises:
        TypeError: Either input is a boolean or is not a real scalar.
        ValueError: Inputs are nonfinite or outside the supported geometry.
    """
    length = _finite_real(tendon_length_m, "tendon_length_m")
    offset = abs(_finite_real(horizontal_offset_m, "horizontal_offset_m"))
    if length <= 0.0:
        raise ValueError("tendon_length_m must be positive")
    if offset >= length:
        raise ValueError("absolute horizontal_offset_m must be less than length")

    # Right-triangle identity, factored and scaled to avoid squared overflow.
    # Subtract lengths before division to retain precision near the taut limit.
    ratio = offset / length
    span = length * math.sqrt(((length - offset) / length) * (1.0 + ratio))
    # Rationalized set-down: x²/(L+h) = |x| (|x|/L) / (1+h/L).
    setdown = offset * (ratio / (1.0 + span / length))
    return TLPSetdown(vertical_span_m=span, setdown_m=setdown)
