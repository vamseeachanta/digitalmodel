# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Primary cementing volume balances for one uniform hole and casing string.

Diameters are inches, depths and lengths are feet, and volumes are barrels
(42 US gallons of 231 cubic inches each). Depth is measured along the casing
from its entrance at zero. Circular area times length is the only model used.

The caller supplies annular excess as a fractional increase in nominal annular
capacity. That same effective capacity sizes cement and spacer and inverts
slurry volume to top of cement. Excess does not increase casing bore capacity.
Fluids are incompressible; complete displacement and no losses are assumed.
Pressure, mixing, bond quality and barrier acceptance are not calculated.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

__all__ = ["CementingVolumes", "PrimaryCementing"]

# pi * diameter^2 / 4 [in^2] * 12 [in/ft] / (42 * 231) [in^3/bbl].
_CAPACITY_FACTOR = 3.0 * math.pi / (42.0 * 231.0)


def _nonnegative(name: str, value: float) -> None:
    """Reject nonfinite, negative and Boolean quantities."""
    if isinstance(value, bool) or not math.isfinite(value) or value < 0.0:
        raise ValueError(f"{name} must be finite and >= 0")


@dataclass(frozen=True)
class CementingVolumes:
    """Volume budget in barrels; displacement starts after the top plug launch.

    Displacement ends at the float collar and excludes surface lines. The
    shoe-track volume remains below the collar. No bump pressure is inferred.
    """

    nominal_annular_slurry_bbl: float
    annular_excess_bbl: float
    shoe_track_bbl: float
    total_slurry_bbl: float
    spacer_bbl: float
    displacement_to_bump_bbl: float


@dataclass(frozen=True)
class PrimaryCementing:
    """Uniform geometry with caller-selected annular excess and collar depth.

    No standard excess, spacer length, cement length or acceptance threshold
    is assumed. A zero excess fraction uses nominal hole diameter. A collar
    at the shoe gives a zero shoe track. Multi-section annuli, rat holes,
    surface equipment and cement returns require a separate volume model.
    """

    hole_diameter_in: float
    casing_od_in: float
    casing_id_in: float
    shoe_depth_ft: float
    float_collar_depth_ft: float
    annular_excess_fraction: float = 0.0

    def __post_init__(self) -> None:
        """Validate geometry and representable capacities before calculating."""
        for name in (
            "hole_diameter_in", "casing_od_in", "casing_id_in", "shoe_depth_ft",
            "float_collar_depth_ft", "annular_excess_fraction",
        ):
            _nonnegative(name, getattr(self, name))
        for name in (
            "hole_diameter_in", "casing_od_in", "casing_id_in", "shoe_depth_ft",
        ):
            if getattr(self, name) == 0.0:
                raise ValueError(f"{name} must be > 0")
        if self.casing_id_in >= self.casing_od_in:
            raise ValueError("casing_id_in must be < casing_od_in")
        if self.casing_od_in >= self.hole_diameter_in:
            raise ValueError("hole_diameter_in must be > casing_od_in")
        if self.float_collar_depth_ft > self.shoe_depth_ft:
            raise ValueError("float_collar_depth_ft must be <= shoe_depth_ft")
        for capacity in (self._bore_capacity, self._effective_annular_capacity):
            if not math.isfinite(capacity) or capacity <= 0.0:
                raise ValueError("geometry must yield finite, positive capacities")
        maximum_volume = self.shoe_depth_ft * (
            self._bore_capacity + self._effective_annular_capacity
        )
        if not math.isfinite(maximum_volume) or maximum_volume <= 0.0:
            raise ValueError("geometry must yield finite, positive volumes")

    @property
    def _bore_capacity(self) -> float:
        return _CAPACITY_FACTOR * self.casing_id_in * self.casing_id_in

    @property
    def _nominal_annular_capacity(self) -> float:
        return (
            _CAPACITY_FACTOR
            * (self.hole_diameter_in - self.casing_od_in)
            * (self.hole_diameter_in + self.casing_od_in)
        )

    @property
    def _effective_annular_capacity(self) -> float:
        return self._nominal_annular_capacity * (1.0 + self.annular_excess_fraction)

    @property
    def _shoe_track_bbl(self) -> float:
        return self._bore_capacity * (
            self.shoe_depth_ft - self.float_collar_depth_ft
        )

    def plan(
        self, *, target_top_of_cement_ft: float, spacer_length_ft: float,
    ) -> CementingVolumes:
        """Size volumes for a target cement top and an annular spacer length.

        Both lengths lie between zero and shoe depth. Spacer length describes
        the slug in the effective annulus and implies no contact-time or
        cleaning acceptance. Slurry fills the annulus below the target plus
        the casing bore below the float collar. Displacement is the bore
        volume from the casing entrance to that collar.
        """
        for name, value in (
            ("target_top_of_cement_ft", target_top_of_cement_ft),
            ("spacer_length_ft", spacer_length_ft),
        ):
            _nonnegative(name, value)
            if value > self.shoe_depth_ft:
                raise ValueError(f"{name} must be <= shoe_depth_ft")
        nominal = self._nominal_annular_capacity * (
            self.shoe_depth_ft - target_top_of_cement_ft
        )
        excess = nominal * self.annular_excess_fraction
        return CementingVolumes(
            nominal_annular_slurry_bbl=nominal,
            annular_excess_bbl=excess,
            shoe_track_bbl=self._shoe_track_bbl,
            total_slurry_bbl=nominal + excess + self._shoe_track_bbl,
            spacer_bbl=self._effective_annular_capacity * spacer_length_ft,
            displacement_to_bump_bbl=self._bore_capacity * self.float_collar_depth_ft,
        )

    def top_of_cement_ft(self, slurry_volume_bbl: float) -> float:
        """Invert placed slurry volume to cement top using the same excess.

        Slurry must fill the shoe track and fit in the track plus the annulus
        to zero depth. Out-of-range quantities are rejected; returns and
        incomplete track fill are outside this model. Only floating-point
        roundoff at a boundary is clamped (relative volume tolerance 1e-12).
        """
        _nonnegative("slurry_volume_bbl", slurry_volume_bbl)
        minimum = self._shoe_track_bbl
        maximum = minimum + self._effective_annular_capacity * self.shoe_depth_ft
        if slurry_volume_bbl < minimum and not math.isclose(
            slurry_volume_bbl, minimum, rel_tol=1e-12,
        ):
            raise ValueError("slurry_volume_bbl cannot fill the shoe track")
        if slurry_volume_bbl > maximum and not math.isclose(
            slurry_volume_bbl, maximum, rel_tol=1e-12,
        ):
            raise ValueError("slurry_volume_bbl exceeds track plus annular capacity")
        top = self.shoe_depth_ft - (
            slurry_volume_bbl - minimum
        ) / self._effective_annular_capacity
        return max(0.0, min(self.shoe_depth_ft, top))
