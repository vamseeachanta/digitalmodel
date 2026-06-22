"""Reduced-order structural utilization model for DNV 2.7-1 offshore containers.

This module implements a **transparent, analytical screening model** of the
governing *offshore lift* load case for offshore containers (CCUs) per
DNV 2.7-1 / DNV-ST-E271 ("Offshore containers"). It is intended for early
sizing and for generating *utilization curves* across a container's
dimensions and member shapes -- it is **not** a substitute for the certified
finite-element analysis the standard ultimately requires.

What is modelled
----------------
A four-point top lift to a single master link. The standard requires lifting
points (and their supporting primary structure) to be designed for the case
where the load is shared by the **two diagonally opposite slings** (the skew
case), so that is taken as governing here.

Primary structure is idealised as a peripheral frame of square hollow sections
(SHS):

* **Top side rails** carry the inward *horizontal* component of the sling
  forces in axial compression.
* **Bottom side rails / floor beams** carry the payload as a uniformly
  distributed load and span between the bottom corner posts in bending.

Utilization is ``demand stress / allowable stress`` with the allowable derived
from yield via a material factor and usage factor. Combined axial+bending
action uses a simple linear interaction.

All factors (DAF, gamma_f, gamma_M, eta) are exposed as named, documented
constants on :class:`DesignFactors` so the assumptions are explicit and
auditable.

References
----------
DNV-ST-E271 (formerly DNV 2.7-1), "Offshore containers" -- lifting design
load cases, skew/two-sling lifting-point requirement, dynamic amplification.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

G = 9.80665  # m/s^2, standard gravity


@dataclass(frozen=True)
class DesignFactors:
    """Named, documented design factors for the screening check.

    Defaults are representative values for an offshore-lift check of a
    welded steel CCU; tune them to the applicable edition/annex of
    DNV-ST-E271 for a specific project.
    """

    daf: float = 1.3          # dynamic amplification factor (offshore hoisting)
    gamma_f: float = 1.0      # load factor (kept in DAF here; separate if desired)
    gamma_m: float = 1.15     # material factor
    eta: float = 0.85         # usage factor on yield
    tare_fraction: float = 0.25  # tare mass / rating R (payload = (1 - tare)*R)


@dataclass(frozen=True)
class SHS:
    """Square hollow section, dimensions in metres."""

    b: float  # outer width [m]
    t: float  # wall thickness [m]

    @property
    def area(self) -> float:
        bi = self.b - 2.0 * self.t
        return self.b ** 2 - bi ** 2

    @property
    def inertia(self) -> float:
        bi = self.b - 2.0 * self.t
        return (self.b ** 4 - bi ** 4) / 12.0

    @property
    def modulus(self) -> float:
        return self.inertia / (self.b / 2.0)

    @classmethod
    def from_mm(cls, b_mm: float, t_mm: float) -> "SHS":
        return cls(b=b_mm / 1000.0, t=t_mm / 1000.0)

    def __str__(self) -> str:  # pragma: no cover - cosmetic
        return f"SHS{self.b*1000:.0f}x{self.t*1000:.0f}"


def sling_angle(length: float, width: float, lift_height: float) -> float:
    """Sling angle from horizontal [rad] for a 4-point lift.

    Slings of equal length run from the top corners to a master link at
    ``lift_height`` above the top frame, centred over the plan centroid.
    """
    half_diag = 0.5 * math.hypot(length, width)
    return math.atan2(lift_height, half_diag)


@dataclass(frozen=True)
class SlingForces:
    vertical_total: float   # total design vertical force [N] (R * g * DAF)
    axial_per_sling: float  # axial force in each *loaded* sling [N] (skew case)
    horizontal_per_corner: float  # inward horizontal component at a corner [N]


def sling_forces(rating_kg: float, beta: float, factors: DesignFactors,
                 skew: bool = True) -> SlingForces:
    """Design sling forces for the offshore lift.

    Parameters
    ----------
    rating_kg : gross mass R (tare + payload) being lifted [kg].
    beta : sling angle from horizontal [rad].
    skew : if True (default, governing per DNV 2.7-1) the load is carried by
        the two diagonally opposite slings; if False, shared by all four.
    """
    vertical = rating_kg * G * factors.daf * factors.gamma_f
    n_loaded = 2 if skew else 4
    axial = vertical / (n_loaded * math.sin(beta))
    horizontal = vertical / (n_loaded * math.tan(beta))
    return SlingForces(vertical_total=vertical,
                       axial_per_sling=axial,
                       horizontal_per_corner=horizontal)


def allowable_stress(fy: float, factors: DesignFactors) -> float:
    """Allowable stress [Pa] = fy * eta / gamma_m."""
    return fy * factors.eta / factors.gamma_m


@dataclass(frozen=True)
class Utilization:
    top_rail: float        # axial utilization of top side rail
    bottom_rail: float     # bending utilization of bottom floor beam
    governing: float       # max of the member checks
    beta_deg: float        # sling angle used [deg]
    sling: SlingForces


def utilization(length: float, width: float, lift_height: float,
                rating_kg: float, section: SHS, fy: float = 355e6,
                factors: DesignFactors | None = None) -> Utilization:
    """Governing primary-structure utilization for the offshore lift.

    Parameters
    ----------
    length, width : container plan dimensions [m].
    lift_height : master-link height above top frame [m] (sets sling angle).
    rating_kg : gross mass R [kg].
    section : SHS used for the peripheral frame members.
    fy : yield strength [Pa] (default S355).
    """
    factors = factors or DesignFactors()
    beta = sling_angle(length, width, lift_height)
    sf = sling_forces(rating_kg, beta, factors)
    sigma_allow = allowable_stress(fy, factors)

    # Top side rail: inward horizontal sling component carried as axial
    # compression along the top frame.
    n_top = sf.horizontal_per_corner
    util_top = (n_top / section.area) / sigma_allow

    # Bottom side rail: payload as UDL, simply supported span ~ length.
    payload = rating_kg * (1.0 - factors.tare_fraction)
    floor_force = payload * G * factors.daf  # design floor load [N]
    # half goes to each of the two long side rails, spread over the length
    w_line = (floor_force / 2.0) / length          # [N/m]
    moment = w_line * length ** 2 / 8.0            # simply supported [N.m]
    util_bot = (moment / section.modulus) / sigma_allow

    governing = max(util_top, util_bot)
    return Utilization(top_rail=util_top, bottom_rail=util_bot,
                       governing=governing, beta_deg=math.degrees(beta),
                       sling=sf)


# Convenience: a small catalogue of representative SHS frame members.
SECTION_CATALOGUE: dict[str, SHS] = {
    "SHS100x6": SHS.from_mm(100, 6),
    "SHS120x8": SHS.from_mm(120, 8),
    "SHS150x8": SHS.from_mm(150, 8),
    "SHS150x10": SHS.from_mm(150, 10),
    "SHS200x10": SHS.from_mm(200, 10),
}
