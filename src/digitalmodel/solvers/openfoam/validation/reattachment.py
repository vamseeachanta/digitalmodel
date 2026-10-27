# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Extract reattachment from an ordered, signed streamwise wall-shear profile.

The numerical reference is linear interpolation between adjacent samples:
the zero divides the interval in the ratio of the shear magnitudes. No
empirical constants, flow model or solver execution are involved.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Literal, Sequence


@dataclass(frozen=True)
class ReattachmentResult:
    """Describe the first resolved rising crossing strictly after the step.

    Attributes:
        status: ``found`` or ``not_found``; absence never means zero length.
        crossing_coordinate: Absolute profile coordinate, or None if absent.
        x: Distance from the step to the crossing, or None if absent.
        x_over_height: Step-relative distance divided by step height, or None.
    """

    status: Literal["found", "not_found"]
    crossing_coordinate: float | None = None
    x: float | None = None
    x_over_height: float | None = None


def extract_reattachment(
    coordinates: Sequence[float],
    wall_shear: Sequence[float],
    *,
    step_location: float,
    step_height: float,
) -> ReattachmentResult:
    """Find the first downstream negative-to-positive wall-shear crossing.

    Coordinates must increase strictly in the downstream direction. Shear
    must be the signed streamwise component, with positive values indicating
    forward flow. Supply consistent length units for coordinates and geometry.
    The returned ``x`` is measured from the step, not the coordinate origin.

    Only adjacent samples bracketing a sign change are interpolated. A bracket
    may straddle the step if its root is strictly downstream. For sampled
    zeros, require negative shear before and positive shear after the zero
    run; report the first zero in that run. A tangency or terminal zero alone
    does not demonstrate reattachment. No extrapolation, smoothing or sorting
    is performed, and the input sequences are not modified.

    Args:
        coordinates: Absolute wall positions in downstream order.
        wall_shear: Signed streamwise shear at each position.
        step_location: Absolute coordinate of the step.
        step_height: Strictly positive step height.

    Returns:
        Crossing position and step-relative length and ratio, or an explicit
        ``not_found`` result with all numerical fields set to None.

    Raises:
        ValueError: If fewer than two matching samples are provided, data or
            geometry are nonfinite, coordinates do not increase strictly,
            height is nonpositive, or the result exceeds finite float range.
    """
    positions = tuple(float(value) for value in coordinates)
    shear = tuple(float(value) for value in wall_shear)
    step = float(step_location)
    height = float(step_height)

    # Two samples define an interval; zero is the shear sign boundary.
    if len(positions) != len(shear) or len(positions) < 2:
        raise ValueError("at least two matching coordinate and shear samples required")
    if not all(math.isfinite(value) for value in (*positions, *shear, step, height)):
        raise ValueError("all samples and step geometry must be finite")
    if height <= 0.0:
        raise ValueError("step_height must be positive")
    if any(right <= left for left, right in zip(positions, positions[1:])):
        raise ValueError("coordinates must be strictly increasing")

    for left in range(len(positions) - 1):
        if shear[left] >= 0.0:
            continue
        right = left + 1
        while right < len(shear) and shear[right] == 0.0:
            right += 1
        if right == len(shear) or shear[right] < 0.0:
            continue

        if right > left + 1:
            crossing = positions[left + 1]
        else:
            # Scale magnitudes before summing to avoid overflow. The fraction
            # is the negative magnitude divided by the sum of magnitudes.
            scale = max(-shear[left], shear[right])
            negative = -shear[left] / scale
            positive = shear[right] / scale
            fraction = negative / (negative + positive)
            # Convex form avoids overflowing a wide coordinate difference.
            crossing = (1.0 - fraction) * positions[left] + fraction * positions[right]

        if crossing <= step:
            continue
        distance = crossing - step
        ratio = distance / height
        if not all(math.isfinite(value) for value in (crossing, distance, ratio)):
            raise ValueError("reattachment position, distance and ratio must be finite")
        return ReattachmentResult(
            status="found",
            crossing_coordinate=crossing,
            x=distance,
            x_over_height=ratio,
        )

    return ReattachmentResult(status="not_found")
