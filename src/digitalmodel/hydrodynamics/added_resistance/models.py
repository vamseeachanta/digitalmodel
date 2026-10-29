# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""SI input models; headings are signed angles from the bow (head seas = 0)."""

from dataclasses import dataclass
from enum import Enum
from math import isfinite
from typing import Sequence


def finite(value: float, name: str, *, positive: bool = False) -> None:
    """Reject nonfinite, negative, and optionally zero physical magnitudes."""
    if not isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(
            f"{name} must be finite and {'positive' if positive else 'nonnegative'}"
        )


def vector(values: Sequence[float], name: str, *, positive: bool = False) -> tuple:
    result = tuple(values)
    if not result:
        raise ValueError(f"{name} must not be empty")
    for value in result:
        finite(value, name, positive=positive)
    return result


@dataclass(frozen=True)
class Ship:
    """No default density, gravity or geometry; bow length follows ITTC Fig. G-1.

    Source: ITTC 7.5-04-01-01.1 (2024, rev. 08), Appendix G.1:
    bow_length_m is the distance from the bow to 95% of maximum waterline beam.
    """

    length_pp_m: float
    beam_m: float
    bow_length_m: float
    draft_m: float
    density_kg_m3: float
    gravity_m_s2: float

    def __post_init__(self):
        for name, value in vars(self).items():
            finite(value, name, positive=True)
        if self.bow_length_m > self.length_pp_m:
            raise ValueError("bow length cannot exceed length between perpendiculars")


@dataclass(frozen=True)
class OperatingPoint:
    """Forward speed in m/s, incoming heading in degrees from bow, port positive."""

    speed_m_s: float
    heading_deg: float

    def __post_init__(self):
        finite(self.speed_m_s, "speed")
        if not isfinite(self.heading_deg) or not -180 <= self.heading_deg <= 180:
            # +/-180 is the signed angle representation, not a method limit.
            raise ValueError("heading must be finite and in [-180, 180] degrees")


@dataclass(frozen=True)
class RegularWaves:
    """Paired wavelength and single amplitude, both in metres, with no interpolation.

    Positive amplitudes keep R/a**2 and the requested transfer function defined.
    The order is preserved; spectral integration sorts frequency separately.
    """

    wavelength_m: tuple[float, ...]
    amplitude_m: tuple[float, ...]

    def __post_init__(self):
        for name in ("wavelength_m", "amplitude_m"):
            object.__setattr__(
                self, name, vector(getattr(self, name), name, positive=True)
            )
        if len(self.wavelength_m) != len(self.amplitude_m):
            raise ValueError("wavelength and amplitude tables must have equal lengths")


class Status(str, Enum):
    EVALUATED = "EVALUATED"
    NOT_EVALUATED = "NOT_EVALUATED"


@dataclass(frozen=True)
class ResistanceRow:
    """One regular-wave result; unevaluated force and transfer are None, never zero."""

    wavelength_m: float
    amplitude_m: float
    omega_rad_s: float
    encounter_rad_s: float
    status: Status
    resistance_n: float | None
    transfer: float | None
    reason: str
    method: str
