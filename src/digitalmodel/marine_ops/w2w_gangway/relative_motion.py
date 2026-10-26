# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Complex relative point-response arithmetic, without operability criteria."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, fields
from math import isfinite
from types import MappingProxyType


@dataclass(frozen=True)
class ResponseBasis:
    """Explicit basis for one frequency/heading sample of point responses.

    Coordinate conventions describe common axis directions and handedness.
    Phase conventions describe the harmonic sign; phase_reference identifies
    the SAME incident-wave phase origin and normalization on both sides.
    response_units maps every supplied component to its unit, including the
    wave normalization for transfer responses (for example m/m or rad/m).
    All transformations, spatial phase shifts and point offsets are resolved
    by the caller before constructing response mappings.
    """

    frequency: float
    frequency_unit: str
    heading: float
    heading_unit: str
    heading_convention: str
    coordinate_convention: str
    phase_convention: str
    phase_reference: str
    response_units: Mapping[str, str]

    def __post_init__(self) -> None:
        """Reject incomplete declarations and keep a snapshot of unit labels."""
        if not isfinite(self.frequency) or not isfinite(self.heading):
            raise ValueError("frequency and heading must be finite")
        for name in (
            "frequency_unit", "heading_unit", "heading_convention",
            "coordinate_convention", "phase_convention", "phase_reference",
        ):
            label = getattr(self, name)
            if not isinstance(label, str) or not label.strip():
                raise ValueError(f"{name} must be explicit and nonempty")
        units = dict(self.response_units)
        if not units or any(
            not isinstance(label, str) or not label.strip()
            for pair in units.items() for label in pair
        ):
            raise ValueError("response_units must declare nonempty components and units")
        object.__setattr__(self, "response_units", MappingProxyType(units))


@dataclass(frozen=True)
class RelativeMotionRAO:
    """Receiving-point minus access-point complex response and retained basis."""

    responses: Mapping[str, complex]
    basis: ResponseBasis


def compute_relative_rao(
    access_point_response: Mapping[str, complex],
    receiving_point_response: Mapping[str, complex],
    *,
    access_basis: ResponseBasis,
    receiving_basis: ResponseBasis,
) -> RelativeMotionRAO:
    """Subtract responses already resolved at the two physical points.

    Each mapping contains component phasors for one frequency/heading sample.
    Require exact basis and component matches; never interpolate, convert units,
    rotate axes, infer missing components or resolve lever arms. Both point
    responses must share an incident-wave phase reference and normalization.
    The arithmetic identity is H_relative = H_receiving - H_access. Magnitudes
    and phases can be evaluated by the caller AFTER this complex subtraction.

    Args:
        access_point_response: Access-point component phasors.
        receiving_point_response: Receiving-point component phasors.
        access_basis: Explicit conventions and units for access responses.
        receiving_basis: Explicit conventions and units for receiving responses.

    Returns:
        Complex receiving-minus-access components with the matching basis.

    Raises:
        ValueError: Bases differ, component declarations differ, or responses
            contain nonfinite real or imaginary parts.
    """
    for field in fields(ResponseBasis):
        if getattr(access_basis, field.name) != getattr(receiving_basis, field.name):
            raise ValueError(f"mismatched response basis: {field.name}")
    points = []
    for response in (access_point_response, receiving_point_response):
        if response.keys() != access_basis.response_units.keys():
            raise ValueError("components must match response_units")
        point = {component: complex(value) for component, value in response.items()}
        if any(not isfinite(v.real) or not isfinite(v.imag) for v in point.values()):
            raise ValueError("responses must contain finite complex values")
        points.append(point)
    access, receiving = points
    relative = {name: receiving[name] - access[name] for name in access}
    return RelativeMotionRAO(MappingProxyType(relative), access_basis)
