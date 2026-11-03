# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Analytical amplitude for one undamped, homogeneous elastic soil layer.

Derivation (vertically incident harmonic SH displacement): put z=0 at the
traction-free surface and z=h at rock. The wave equation gives
u(z)=A*cos(k*z)+B*sin(k*z), where k=2*pi*f/vs. Zero surface shear traction
requires u'(0)=0, hence B=0. In the infinite rock-impedance limit the
interface displacement equals the rock outcrop displacement u_outcrop
(the incident and reflected rock waves double there). Thus
u(h)=A*cos(k*h)=u_outcrop and H=abs(u(0)/u_outcrop)=1/abs(cos(k*h)).
This reference is outcrop motion, not the incident travelling wave alone.
At DC, constant displacement gives H=1. Poles occur at
k*h=(2*n+1)*pi/2, or f=(2*n+1)*vs/(4*h), for nonnegative integer n.
The factors 2 and 4 follow from angular frequency and the quarter wavelength,
respectively; no empirical material constants are used.

Numerical error budget (binary64, round-to-nearest, inputs taken as exact):
let eps=2**-52 and u=eps/2. The rounded tau constant and three mantissa
operations each contribute a relative factor (1+delta), abs(delta)<=u.
With g=4*u/(1-4*u), the product error is at most g; expressed against the
computed phase p, abs(p-exact_phase)<=g*abs(p)/(1-g). This follows by
expanding the four factors and bounding their tail geometrically; see
Accuracy and Stability of Numerical Algorithms, 2nd ed. (2002), section 3.1
(Inner and Outer Products), https://doi.org/10.1137/1.9780898718027.ch3.
Binary exponent scaling is exact for normal results; a subnormal result
adds at most half the smallest subnormal in absolute phase error.
For binary64 the combined phase error is less than 3*eps*max(1, abs(p)).

The cosine bound additionally REQUIRES absolute libm error <=eps. Since
abs(cos(a)-cos(b))<=abs(a-b), the total denominator error is then less than
4*eps*max(1, abs(p)). The classification band T=8*eps*max(1, abs(p))
reserves a further factor of two. A computed cosine magnitude <=T is
unresolved, not proof that the exact input is a pole. Python's math module
wraps the platform C library and does not promise this libm error bound:
https://docs.python.org/3.11/library/math.html. Independent decimal-series
tests qualify the exercised arguments only, not every input or platform.

Accuracy criterion: the absolute denominator-error budget T shall not
exceed 2**-26, retaining 26 binary fractional bits of absolute cosine
resolution. Since eps=2**-52, this limit equals sqrt(eps); algebra gives
abs(p)<=2**23. Equality is accepted; larger phases are rejected. This is
an absolute denominator criterion, not a relative amplification guarantee:
for a finite computed denominator d>T, the relative reciprocal error is
bounded by T/(d-T), plus reciprocal rounding, and can grow near the band.
Nonzero phases that overflow or underflow are also refused. The test oracle
uses NIST Handbook of Mathematical Functions (2010), sections 4.19
(Maclaurin Series and Laurent Series) and 4.24(i) (Power Series), with
explicit truncation bounds and 100-digit arithmetic, independently of libm.

This is a scalar amplitude only: no phase transfer, finite rock impedance,
damping, iteration, motion-record propagation or multiple layers.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from numbers import Real
from sys import float_info
from typing import Literal


@dataclass(frozen=True)
class ElasticLayerAmplitude:
    """Dimensionless amplitude and explicit finite or unbounded status."""

    amplification: float
    status: Literal["finite", "unbounded"]


def elastic_layer_amplification(
    frequency_hz: float,
    thickness_m: float,
    shear_wave_speed_m_s: float,
) -> ElasticLayerAmplitude:
    """Return the surface/outcrop displacement amplitude in stated SI units.

    Args:
        frequency_hz: Finite nonnegative harmonic frequency, in hertz.
        thickness_m: Finite strictly positive layer thickness, in metres.
        shear_wave_speed_m_s: Finite strictly positive shear-wave speed, in m/s.

    Returns:
        Dimensionless amplification with status ``finite``, or positive
        infinity with status ``unbounded`` at a pole within the documented
        floating-point tolerance. The same harmonic amplitude ratio applies
        to velocity and acceleration for nonzero frequency.

    Raises:
        ValueError: An input is not a real scalar, is a boolean, violates its
            finite/sign constraint, or gives a numerically unresolved phase.
    """
    frequency = _real_scalar(frequency_hz, "frequency_hz")
    thickness = _real_scalar(thickness_m, "thickness_m")
    speed = _real_scalar(shear_wave_speed_m_s, "shear_wave_speed_m_s")
    if frequency < 0:
        raise ValueError("frequency_hz must be nonnegative")
    if thickness <= 0 or speed <= 0:
        raise ValueError("thickness_m and shear_wave_speed_m_s must be positive")
    if frequency == 0:
        return ElasticLayerAmplitude(1.0, "finite")

    # Binary exponent scaling avoids spurious intermediate product overflow
    # or travel-time underflow when the final nonzero phase is representable.
    f_mantissa, f_exponent = math.frexp(frequency)
    h_mantissa, h_exponent = math.frexp(thickness)
    v_mantissa, v_exponent = math.frexp(speed)
    try:
        phase = math.ldexp(
            math.tau * f_mantissa * h_mantissa / v_mantissa,
            f_exponent + h_exponent - v_exponent,
        )
    except OverflowError as exc:
        raise ValueError("phase exceeds floating-point range") from exc
    if phase == 0:
        raise ValueError("nonzero phase underflows floating-point range")

    # Apply the derived error budget and absolute cosine-resolution criterion.
    tolerance = 8 * float_info.epsilon * max(1.0, abs(phase))
    accuracy_limit = math.sqrt(float_info.epsilon)
    if tolerance > accuracy_limit:
        raise ValueError(
            f"phase cosine uncertainty {tolerance:.3g} exceeds limit "
            f"{accuracy_limit:.3g}; resonance classification is unresolved"
        )
    denominator = abs(math.cos(phase))
    if denominator <= tolerance:
        return ElasticLayerAmplitude(math.inf, "unbounded")
    return ElasticLayerAmplitude(1.0 / denominator, "finite")


def _real_scalar(value: float, name: str) -> float:
    """Validate without accepting strings, booleans or complex scalars."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real scalar")
    try:
        result = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{name} must be a finite real scalar") from exc
    if not math.isfinite(result):
        raise ValueError(f"{name} must be a finite real scalar")
    return result
