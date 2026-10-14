# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Elementary manifold screens against independently supplied capacities.

API 17P (Design and Operation of Subsea Production Systems: Subsea Structures
and Manifolds) is the intended equipment reference. A verified edition and
clause-level load definitions are unavailable; these are NOT API 17P design
checks or certification. All allowable capacities and load cases are required
arguments with no default. No licensed design factor or table is reproduced.

Mudmat: bearing pressure=vertical load/area; settlement=pressure/subgrade
modulus (linear uniform Winkler screen, no consolidation or eccentricity).
Header: tensile hoop membrane stress=max(pi-po,0)*OD/(2*t). This thin-wall
approximation is not a wall-sizing code equation. External differential is
compared with a supplied qualified collapse capacity, not calculated here.
Installation and trawl forces have separate supplied capacities; dropped-object
kinetic energy=m*v^2/2 is compared with a supplied absorbed-energy capacity.
Dynamic amplification, buckling, interaction and foundation/impact response
require independent analysis and remain outside these screens. SI units apply.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


def _nonnegative(*values: float) -> None:
    if any(not math.isfinite(value) or value < 0 for value in values):
        raise ValueError("values must be finite and nonnegative")


def _positive(*values: float) -> None:
    _nonnegative(*values)
    if any(value == 0 for value in values):
        raise ValueError("capacities and dimensions must be positive")


@dataclass(frozen=True)
class MudmatScreen:
    """Bearing utilisation and linear settlement compared with its limit."""

    bearing_utilisation: float
    settlement_m: float
    settlement_limit_m: float

    @property
    def passed(self) -> bool:
        """Return whether both supplied criteria are met."""
        return self.bearing_utilisation <= 1 and self.settlement_m <= self.settlement_limit_m


def mudmat_screen(vertical_load_n: float, area_m2: float,
                  bearing_capacity_pa: float, subgrade_modulus_pa_m: float,
                  settlement_limit_m: float) -> MudmatScreen:
    """Check uniform compressive bearing and linear spring settlement."""
    _nonnegative(vertical_load_n)
    _positive(area_m2, bearing_capacity_pa, subgrade_modulus_pa_m, settlement_limit_m)
    pressure = vertical_load_n / area_m2
    return MudmatScreen(pressure / bearing_capacity_pa,
                        pressure / subgrade_modulus_pa_m, settlement_limit_m)


@dataclass(frozen=True)
class HeaderScreen:
    """Independent tensile-hoop and external-pressure utilisations."""

    hoop_utilisation: float
    external_utilisation: float

    @property
    def passed(self) -> bool:
        """Return whether both supplied capacities contain their demands."""
        return max(self.hoop_utilisation, self.external_utilisation) <= 1


def header_screen(internal_pressure_pa: float, external_pressure_pa: float,
                  outer_diameter_m: float, wall_thickness_m: float,
                  allowable_hoop_pa: float, collapse_capacity_pa: float) -> HeaderScreen:
    """Screen differential pressure; thin-wall applicability is caller-qualified."""
    _nonnegative(internal_pressure_pa, external_pressure_pa)
    _positive(outer_diameter_m, wall_thickness_m, allowable_hoop_pa, collapse_capacity_pa)
    if 2 * wall_thickness_m >= outer_diameter_m:
        raise ValueError("wall thickness leaves no pipe bore")
    differential = internal_pressure_pa - external_pressure_pa
    hoop = max(differential, 0) * outer_diameter_m / (2 * wall_thickness_m)
    return HeaderScreen(hoop / allowable_hoop_pa,
                        max(-differential, 0) / collapse_capacity_pa)


@dataclass(frozen=True)
class LoadCaseScreen:
    """Separate force and energy capacity comparisons, without interaction."""

    installation_utilisation: float
    trawl_utilisation: float
    impact_utilisation: float

    @property
    def passed(self) -> bool:
        """Return whether each independent demand is at or below capacity."""
        return max(self.installation_utilisation, self.trawl_utilisation,
                   self.impact_utilisation) <= 1


def load_case_screen(installation_force_n: float, trawl_force_n: float,
                     dropped_mass_kg: float, impact_velocity_m_s: float,
                     installation_capacity_n: float, trawl_capacity_n: float,
                     absorbed_energy_capacity_j: float) -> LoadCaseScreen:
    """Compare specified installation, trawl and dropped-object cases."""
    _nonnegative(installation_force_n, trawl_force_n, dropped_mass_kg, impact_velocity_m_s)
    _positive(installation_capacity_n, trawl_capacity_n, absorbed_energy_capacity_j)
    energy = dropped_mass_kg * impact_velocity_m_s**2 / 2
    return LoadCaseScreen(installation_force_n / installation_capacity_n,
                          trawl_force_n / trawl_capacity_n, energy / absorbed_energy_capacity_j)
