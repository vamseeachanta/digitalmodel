"""Cathodic protection design for marine and offshore structures.

Covers CP design for offshore platforms, jackets, monopiles, and subsea
structures with zone-based current density allocation (splash, tidal,
submerged, buried zones), anode distribution, and retrofitting assessment.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design"
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently Installed
  Steel Offshore Structures"
- ISO 12473 (2006) "General Principles of Cathodic Protection in Seawater"
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class ExposureZone(str, Enum):
    """Structural exposure zones per DNV-RP-B401 §7."""

    ATMOSPHERIC = "atmospheric"
    SPLASH = "splash"
    TIDAL = "tidal"
    SUBMERGED = "submerged"
    BURIED_MUDLINE = "buried_mudline"


class ClimateRegion(str, Enum):
    """Climate region for current density selection per DNV-RP-B401 Table 10-1."""

    TROPICAL = "tropical"
    SUBTROPICAL = "subtropical"
    TEMPERATE = "temperate"
    ARCTIC = "arctic"


# DNV-RP-B401 Table 10-1: Design current densities [mA/m²]
# Format: (initial, mean, final)
ZONE_CURRENT_DENSITY: dict[tuple[ExposureZone, ClimateRegion], tuple[float, float, float]] = {
    # Submerged zone
    (ExposureZone.SUBMERGED, ClimateRegion.TROPICAL): (150.0, 70.0, 90.0),
    (ExposureZone.SUBMERGED, ClimateRegion.SUBTROPICAL): (170.0, 80.0, 100.0),
    (ExposureZone.SUBMERGED, ClimateRegion.TEMPERATE): (200.0, 100.0, 120.0),
    (ExposureZone.SUBMERGED, ClimateRegion.ARCTIC): (250.0, 120.0, 150.0),
    # Buried/mudline zone
    (ExposureZone.BURIED_MUDLINE, ClimateRegion.TROPICAL): (25.0, 20.0, 20.0),
    (ExposureZone.BURIED_MUDLINE, ClimateRegion.SUBTROPICAL): (25.0, 20.0, 20.0),
    (ExposureZone.BURIED_MUDLINE, ClimateRegion.TEMPERATE): (25.0, 20.0, 20.0),
    (ExposureZone.BURIED_MUDLINE, ClimateRegion.ARCTIC): (25.0, 20.0, 20.0),
    # Splash zone — typically not CP-protected (coatings + allowance)
    (ExposureZone.SPLASH, ClimateRegion.TROPICAL): (0.0, 0.0, 0.0),
    (ExposureZone.SPLASH, ClimateRegion.SUBTROPICAL): (0.0, 0.0, 0.0),
    (ExposureZone.SPLASH, ClimateRegion.TEMPERATE): (0.0, 0.0, 0.0),
    (ExposureZone.SPLASH, ClimateRegion.ARCTIC): (0.0, 0.0, 0.0),
    # Tidal zone
    (ExposureZone.TIDAL, ClimateRegion.TROPICAL): (130.0, 60.0, 80.0),
    (ExposureZone.TIDAL, ClimateRegion.SUBTROPICAL): (150.0, 70.0, 90.0),
    (ExposureZone.TIDAL, ClimateRegion.TEMPERATE): (170.0, 80.0, 100.0),
    (ExposureZone.TIDAL, ClimateRegion.ARCTIC): (200.0, 100.0, 120.0),
}


class StructuralZone(BaseModel):
    """A zone of a marine structure for CP design."""

    zone_name: str = Field(..., description="Zone identifier")
    exposure_zone: ExposureZone = Field(..., description="Exposure zone type")
    surface_area_m2: float = Field(..., gt=0, description="Surface area [m²]")
    coating_breakdown_factor: float = Field(
        default=0.0,
        ge=0.0,
        le=1.0,
        description="Coating breakdown factor (0 = fully coated, 1 = bare)",
    )


class MarineCPResult(BaseModel):
    """Result of marine structure CP design."""

    total_initial_current_A: float = Field(
        ..., description="Total initial current demand [A]"
    )
    total_mean_current_A: float = Field(
        ..., description="Total mean current demand [A]"
    )
    total_final_current_A: float = Field(
        ..., description="Total final current demand [A]"
    )
    total_anode_mass_kg: float = Field(
        ..., description="Total anode mass requirement [kg]"
    )
    number_of_anodes: int = Field(
        ..., description="Total number of anodes required"
    )
    zone_details: list[dict] = Field(
        default_factory=list,
        description="Per-zone breakdown of current demands",
    )


class RetrofitAssessment(BaseModel):
    """Assessment of CP retrofit requirements."""

    remaining_anode_life_years: float = Field(
        ..., description="Estimated remaining anode life [years]"
    )
    additional_anodes_needed: int = Field(
        ..., description="Additional anodes required for remaining life"
    )
    additional_mass_kg: float = Field(
        ..., description="Additional anode mass required [kg]"
    )
    is_retrofit_needed: bool = Field(
        ..., description="Whether retrofit is recommended"
    )
    recommendation: str = Field(
        ..., description="Recommendation text"
    )


def marine_structure_current_demand(
    zones: list[StructuralZone],
    climate_region: ClimateRegion = ClimateRegion.TEMPERATE,
    design_life_years: float = 25.0,
    anode_net_mass_kg: float = 200.0,
    anode_capacity_Ah_kg: float = 2000.0,
    utilization_factor: float = 0.90,
) -> MarineCPResult:
    """Calculate current demand and anode requirements for an offshore structure.

    Sums current demand across all exposure zones using DNV-RP-B401
    Table 10-1 current densities and coating breakdown factors.

    Parameters
    ----------
    zones : list[StructuralZone]
        List of structural zones with areas and coating data.
    climate_region : ClimateRegion
        Climate region for current density selection.
    design_life_years : float
        CP design life [years].
    anode_net_mass_kg : float
        Net mass of a single anode [kg].
    anode_capacity_Ah_kg : float
        Anode electrochemical capacity [A-h/kg].
    utilization_factor : float
        Anode utilization factor.

    Returns
    -------
    MarineCPResult
        Total current demand, anode mass, and number of anodes.
    """
    total_initial = 0.0
    total_mean = 0.0
    total_final = 0.0
    zone_details = []

    for zone in zones:
        key = (zone.exposure_zone, climate_region)
        densities = ZONE_CURRENT_DENSITY.get(key, (0.0, 0.0, 0.0))
        ic_initial, ic_mean, ic_final = densities

        # Apply coating breakdown factor
        fc = zone.coating_breakdown_factor if zone.coating_breakdown_factor > 0 else 1.0

        i_initial = zone.surface_area_m2 * fc * ic_initial / 1000.0
        i_mean = zone.surface_area_m2 * fc * ic_mean / 1000.0
        i_final = zone.surface_area_m2 * fc * ic_final / 1000.0

        total_initial += i_initial
        total_mean += i_mean
        total_final += i_final

        zone_details.append({
            "zone_name": zone.zone_name,
            "exposure_zone": zone.exposure_zone.value,
            "surface_area_m2": zone.surface_area_m2,
            "initial_current_A": round(i_initial, 4),
            "mean_current_A": round(i_mean, 4),
            "final_current_A": round(i_final, 4),
        })

    # Anode mass from mean current demand (DNV-RP-B401 Eq 2)
    total_mass = (total_mean * design_life_years * 8760.0) / (
        anode_capacity_Ah_kg * utilization_factor
    )

    n_anodes = max(1, math.ceil(total_mass / anode_net_mass_kg))

    return MarineCPResult(
        total_initial_current_A=round(total_initial, 4),
        total_mean_current_A=round(total_mean, 4),
        total_final_current_A=round(total_final, 4),
        total_anode_mass_kg=round(total_mass, 2),
        number_of_anodes=n_anodes,
        zone_details=zone_details,
    )


def anode_distribution(
    zones: list[StructuralZone],
    total_anodes: int,
    climate_region: ClimateRegion = ClimateRegion.TEMPERATE,
) -> dict[str, int]:
    """Distribute anodes across structural zones proportional to current demand.

    Allocates anodes based on each zone's fraction of total final current
    demand (conservative approach using final/design values).

    Parameters
    ----------
    zones : list[StructuralZone]
        Structural zones.
    total_anodes : int
        Total number of anodes to distribute.
    climate_region : ClimateRegion
        Climate region.

    Returns
    -------
    dict[str, int]
        Anode count per zone name.
    """
    demands: list[tuple[str, float]] = []
    for zone in zones:
        key = (zone.exposure_zone, climate_region)
        densities = ZONE_CURRENT_DENSITY.get(key, (0.0, 0.0, 0.0))
        _, _, ic_final = densities
        fc = zone.coating_breakdown_factor if zone.coating_breakdown_factor > 0 else 1.0
        demand = zone.surface_area_m2 * fc * ic_final
        demands.append((zone.zone_name, demand))

    total_demand = sum(d for _, d in demands)
    if total_demand <= 0:
        # Equal distribution if no demand
        per_zone = max(1, total_anodes // len(zones)) if zones else 0
        return {name: per_zone for name, _ in demands}

    distribution: dict[str, int] = {}
    allocated = 0
    for i, (name, demand) in enumerate(demands):
        fraction = demand / total_demand
        count = max(0, round(fraction * total_anodes))
        distribution[name] = count
        allocated += count

    # Distribute remainder to highest demand zone
    remainder = total_anodes - allocated
    if remainder != 0 and demands:
        max_zone = max(demands, key=lambda x: x[1])[0]
        distribution[max_zone] = distribution.get(max_zone, 0) + remainder

    return distribution


def retrofit_assessment(
    original_anode_mass_kg: float,
    elapsed_years: float,
    design_life_years: float,
    mean_current_A: float,
    measured_potential_V: float = -0.850,
    anode_capacity_Ah_kg: float = 2000.0,
    utilization_factor: float = 0.90,
    anode_net_mass_kg: float = 200.0,
    protection_threshold_V: float = -0.800,
) -> RetrofitAssessment:
    """Assess whether a marine structure CP system needs retrofitting.

    Compares consumed anode mass vs original mass to estimate remaining life,
    and checks measured potential against protection criteria.

    Parameters
    ----------
    original_anode_mass_kg : float
        Total original installed anode mass [kg].
    elapsed_years : float
        Years since installation.
    design_life_years : float
        Original design life [years].
    mean_current_A : float
        Measured/estimated mean current demand [A].
    measured_potential_V : float
        Most recent measured potential [V vs Ag/AgCl].
    anode_capacity_Ah_kg : float
        Anode electrochemical capacity [A-h/kg].
    utilization_factor : float
        Anode utilization factor.
    anode_net_mass_kg : float
        Mass per retrofit anode [kg].
    protection_threshold_V : float
        Minimum acceptable protection potential [V vs Ag/AgCl].

    Returns
    -------
    RetrofitAssessment
        Assessment of remaining life and retrofit needs.
    """
    # Mass consumed so far
    mass_consumed = (mean_current_A * elapsed_years * 8760.0) / (
        anode_capacity_Ah_kg * utilization_factor
    )
    usable_mass = original_anode_mass_kg * utilization_factor
    remaining_mass = max(0, original_anode_mass_kg - mass_consumed)

    # Remaining life from mass
    if mean_current_A > 0:
        remaining_life = (remaining_mass * anode_capacity_Ah_kg * utilization_factor) / (
            mean_current_A * 8760.0
        )
    else:
        remaining_life = design_life_years - elapsed_years

    remaining_design = design_life_years - elapsed_years
    shortfall_years = max(0, remaining_design - remaining_life)

    # Additional mass needed for remaining design life
    additional_mass = 0.0
    if shortfall_years > 0:
        additional_mass = (mean_current_A * shortfall_years * 8760.0) / (
            anode_capacity_Ah_kg * utilization_factor
        )

    additional_anodes = max(0, math.ceil(additional_mass / anode_net_mass_kg))

    # Protection check
    potential_ok = measured_potential_V <= protection_threshold_V
    needs_retrofit = additional_anodes > 0 or not potential_ok

    if needs_retrofit:
        if not potential_ok:
            recommendation = (
                f"URGENT: Measured potential ({measured_potential_V:.3f} V) exceeds "
                f"protection threshold ({protection_threshold_V:.3f} V). "
                f"Install {additional_anodes} retrofit anodes ({additional_mass:.0f} kg)."
            )
        else:
            recommendation = (
                f"Anode depletion projected in {remaining_life:.1f} years. "
                f"Install {additional_anodes} retrofit anodes ({additional_mass:.0f} kg) "
                f"to cover remaining {remaining_design:.0f} year design life."
            )
    else:
        recommendation = (
            f"CP system adequate. Remaining anode life: {remaining_life:.1f} years. "
            f"Next inspection recommended in {min(5.0, remaining_life / 2):.1f} years."
        )

    return RetrofitAssessment(
        remaining_anode_life_years=round(remaining_life, 2),
        additional_anodes_needed=additional_anodes,
        additional_mass_kg=round(additional_mass, 2),
        is_retrofit_needed=needs_retrofit,
        recommendation=recommendation,
    )
