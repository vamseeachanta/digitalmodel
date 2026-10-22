# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Serializable circular pipe geometry with an explicitly unit-labelled area."""

import json
import math
from dataclasses import asdict, dataclass


@dataclass(frozen=True)
class PipeSection:
    """Concentric annulus in metres; a zero bore represents a solid circle."""

    outer_diameter_m: float
    inner_diameter_m: float

    def __post_init__(self) -> None:
        """Require finite numeric geometry with positive material area."""
        for value in (self.outer_diameter_m, self.inner_diameter_m):
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError("pipe diameters must be finite numbers in metres")
            if not math.isfinite(value):
                raise ValueError("pipe diameters must be finite numbers in metres")
        if not self.outer_diameter_m > self.inner_diameter_m >= 0:
            raise ValueError("pipe diameters must satisfy outer > inner >= 0")

    @property
    def area_m2(self) -> float:
        """Return material area as the difference of concentric circle areas."""
        # OpenStax Precalculus 2e (2021), §5.1, Area of a Sector: A=θ*r²/2.
        # With θ = 2π radians (a full circle), A=pi*r².
        # Diameter is twice radius, hence pi*(Do-Di)*(Do+Di)/4.
        # Factoring the difference of squares avoids thin-wall cancellation.
        return (
            (math.pi / 4)
            * (self.outer_diameter_m - self.inner_diameter_m)
            * (self.outer_diameter_m + self.inner_diameter_m)
        )

    def to_json(self) -> str:
        """Serialize geometry with SI units embedded in the field names."""
        return json.dumps(asdict(self), allow_nan=False, sort_keys=True)

    @classmethod
    def from_json(cls, payload: str) -> "PipeSection":
        """Reconstruct validated geometry; both diameters must be explicit."""
        data = json.loads(payload)
        if not isinstance(data, dict) or set(data) != {
            "outer_diameter_m",
            "inner_diameter_m",
        }:
            raise ValueError("pipe JSON requires exactly both SI diameter fields")
        return cls(**data)
