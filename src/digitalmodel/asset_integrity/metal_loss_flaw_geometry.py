"""Projected rectangular metal-loss extents in the axial/circumferential plane.

Source: a legacy engineering workbook, API 579, ``Eqn 5.1`` and ``Eqn 5.2``.
For length a, width b and angle beta to the longitudinal axis, the extents
are s = a*cos(beta)+b*sin(beta), c = a*sin(beta)+b*cos(beta).
Inputs and outputs are metres; the angle is radians, explicitly in [0, pi/2].
The equations are geometric projections, not an assessment of flaw severity.

Input gaps: governing edition, applicability, measured groove dimensions and
whether the rectangular envelope represents the critical thickness profile.
The source labels its angle in degrees but passes it directly to trigonometric
functions. Callers shall convert a degree input through the existing unit
registry before constructing FlawFootprint. The unit error is not reproduced.
Groove radius, crack classification, profile integration, allowable strength
and service acceptance are outside this geometry function.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class FlawFootprint:
    """Positive rectangular dimensions with orientation in the first quadrant."""

    length_m: float
    width_m: float
    orientation_rad: float

    def __post_init__(self) -> None:
        for name in ('length_m', 'width_m', 'orientation_rad'):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
                raise ValueError(f'{name} must be a finite number')
        if self.length_m <= 0 or self.width_m <= 0:
            raise ValueError('length and width must be positive')
        if not 0 <= self.orientation_rad <= math.pi / 2:
            raise ValueError('orientation_rad must be in [0, pi/2]')


@dataclass(frozen=True)
class FlawExtents:
    """Projected dimensions in metres."""

    longitudinal_m: float
    circumferential_m: float


def project_flaw_extents(inputs: FlawFootprint) -> FlawExtents:
    """Compute geometric projections; the source labels Eqn 5.1 and Eqn 5.2."""
    sine = math.sin(inputs.orientation_rad)
    cosine = math.cos(inputs.orientation_rad)
    return FlawExtents(
        inputs.length_m * cosine + inputs.width_m * sine,
        inputs.length_m * sine + inputs.width_m * cosine,
    )
