"""Marine structure CP assessment — multi-zone design.

Provides seawater current density lookup by temperature and depth,
zone-based current demand calculation with calcareous deposit correction,
and end-to-end multi-zone CP design for offshore structures.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design" §7, Table 10-1
- DNV-RP-B401 (2017) "Cathodic Protection Design" §10.4 — calcareous deposits
- ISO 12473 (2006) "General Principles of Cathodic Protection in Seawater"
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently
  Installed Steel Offshore Structures"
"""

from __future__ import annotations

import math
from enum import Enum

from pydantic import BaseModel, Field


# ───────────────────────────────────────────────────────────────────────
# Zone types
# ───────────────────────────────────────────────────────────────────────

class ZoneType(str, Enum):
    """Structural zone types per DNV-RP-B401 §7."""

    ATMOSPHERIC = "atmospheric"
    SPLASH = "splash"
    TIDAL = "tidal"
    SUBMERGED = "submerged"
    MUDLINE = "mudline"


# ───────────────────────────────────────────────────────────────────────
# Current density model
# ───────────────────────────────────────────────────────────────────────

# Base mean current densities [mA/m²] at reference conditions (15°C, 30 m depth)
# Per DNV-RP-B401 Table 10-1 (temperate mean values)
BASE_CURRENT_DENSITY: dict[ZoneType, float] = {
    ZoneType.ATMOSPHERIC: 0.0,   # Not CP-protected
    ZoneType.SPLASH: 0.0,        # Not CP-protected (coatings + corrosion allowance)
    ZoneType.TIDAL: 80.0,        # mA/m²
    ZoneType.SUBMERGED: 100.0,   # mA/m² (temperate mean)
    ZoneType.MUDLINE: 20.0,      # mA/m² (buried/mud zone)
}

# Calcareous deposit reduction factors
# Once polarized, calcareous deposits form and reduce current demand
# Typical reduction: 30-50% of initial after 1-2 years
CALCAREOUS_REDUCTION_FACTOR: float = 0.60  # 40% reduction


class Zone(BaseModel):
    """A zone of a marine structure for CP assessment."""

    name: str = Field(..., description="Zone identifier")
    zone_type: ZoneType = Field(..., description="Zone exposure type")
    surface_area_m2: float = Field(
        ..., gt=0, description="Zone surface area [m²]"
    )
    coating_breakdown_factor: float = Field(
        default=1.0,
        ge=0.0,
        le=1.0,
        description="Coating breakdown factor (0 = fully coated, 1 = bare)",
    )


class MarineCPInput(BaseModel):
    """Input parameters for marine structure CP design."""

    structure_name: str = Field(
        default="Marine Structure",
        description="Structure identifier",
    )
    zones: list[Zone] = Field(
        ..., min_length=1, description="List of structural zones"
    )
    water_temperature_c: float = Field(
        default=15.0, description="Seawater temperature [°C]"
    )
    water_depth_m: float = Field(
        default=30.0, ge=0, description="Water depth [m]"
    )
    design_life_years: float = Field(
        default=25.0, gt=0, description="CP design life [years]"
    )
    anode_net_mass_kg: float = Field(
        default=200.0, gt=0, description="Net mass per anode [kg]"
    )
    anode_capacity_Ah_kg: float = Field(
        default=2000.0, gt=0, description="Anode capacity [A-h/kg]"
    )
    utilization_factor: float = Field(
        default=0.90, gt=0, le=1.0, description="Anode utilization factor"
    )


class MarineCPResult(BaseModel):
    """Result of marine CP multi-zone design."""

    structure_name: str = Field(
        ..., description="Structure identifier"
    )
    total_current_demand_A: float = Field(
        ..., description="Total mean current demand [A]"
    )
    total_anode_mass_kg: float = Field(
        ..., description="Total anode mass requirement [kg]"
    )
    number_of_anodes: int = Field(
        ..., description="Number of anodes required"
    )
    zone_demands: list[dict] = Field(
        default_factory=list,
        description="Per-zone current demand breakdown",
    )
    design_life_years: float = Field(
        ..., description="Design life used [years]"
    )


def get_seawater_current_density(
    temperature_c: float,
    depth_m: float,
    calcareous: bool = False,
) -> float:
    """Get seawater current density for submerged bare steel.

    Uses a temperature-depth model based on DNV-RP-B401 Table 10-1.

    Temperature correction (relative to 15°C reference):
        - Colder water → higher oxygen solubility → higher current density
        - Factor: 1.0 + 0.025 * (15 - T) for T < 15°C
        - Factor: 1.0 - 0.015 * (T - 15) for T > 15°C

    Depth correction (relative to 30 m reference):
        - Deeper water → higher pressure → slightly higher current density
        - Factor: 1.0 + 0.0003 * (depth - 30) for depth > 30 m

    Parameters
    ----------
    temperature_c : float
        Seawater temperature [°C].
    depth_m : float
        Water depth [m].
    calcareous : bool
        Whether calcareous deposits have formed (reduces demand).

    Returns
    -------
    float
        Design mean current density [mA/m²].
    """
    base = BASE_CURRENT_DENSITY[ZoneType.SUBMERGED]  # 100 mA/m²

    # Temperature correction
    if temperature_c < 15.0:
        temp_factor = 1.0 + 0.025 * (15.0 - temperature_c)
    elif temperature_c > 15.0:
        temp_factor = max(0.5, 1.0 - 0.015 * (temperature_c - 15.0))
    else:
        temp_factor = 1.0

    # Depth correction
    if depth_m > 30.0:
        depth_factor = 1.0 + 0.0003 * (depth_m - 30.0)
    else:
        depth_factor = 1.0

    density = base * temp_factor * depth_factor

    # Calcareous deposit reduction
    if calcareous:
        density *= CALCAREOUS_REDUCTION_FACTOR

    return density


def calculate_zone_demand(
    zone: Zone,
    temperature_c: float = 15.0,
    depth_m: float = 30.0,
    calcareous: bool = False,
) -> float:
    """Calculate current demand for a single structural zone.

    I_zone = A * f_c * i_c / 1000

    Parameters
    ----------
    zone : Zone
        Structural zone definition.
    temperature_c : float
        Seawater temperature [°C].
    depth_m : float
        Water depth [m].
    calcareous : bool
        Whether calcareous deposits are present.

    Returns
    -------
    float
        Zone current demand [A].
    """
    # Splash and atmospheric zones are not CP-protected
    if zone.zone_type in (ZoneType.SPLASH, ZoneType.ATMOSPHERIC):
        return 0.0

    # Get current density based on zone type
    if zone.zone_type == ZoneType.SUBMERGED:
        density = get_seawater_current_density(
            temperature_c=temperature_c,
            depth_m=depth_m,
            calcareous=calcareous,
        )
    elif zone.zone_type == ZoneType.TIDAL:
        # Tidal zone uses base tidal density with temperature correction
        base = BASE_CURRENT_DENSITY[ZoneType.TIDAL]
        if temperature_c < 15.0:
            temp_factor = 1.0 + 0.025 * (15.0 - temperature_c)
        elif temperature_c > 15.0:
            temp_factor = max(0.5, 1.0 - 0.015 * (temperature_c - 15.0))
        else:
            temp_factor = 1.0
        density = base * temp_factor
        if calcareous:
            density *= CALCAREOUS_REDUCTION_FACTOR
    elif zone.zone_type == ZoneType.MUDLINE:
        # Mudline uses fixed low current density (soil-dominated)
        density = BASE_CURRENT_DENSITY[ZoneType.MUDLINE]
    else:
        density = 0.0

    return zone.surface_area_m2 * zone.coating_breakdown_factor * density / 1000.0


def design_marine_cp(
    input_params: MarineCPInput,
) -> MarineCPResult:
    """Design multi-zone CP system for a marine structure.

    Calculates per-zone current demands, total anode mass, and
    number of anodes required for the full structure.

    Parameters
    ----------
    input_params : MarineCPInput
        Structure definition with zones and environmental parameters.

    Returns
    -------
    MarineCPResult
        Complete CP design result with per-zone breakdown.
    """
    zone_demands: list[dict] = []
    total_demand = 0.0

    for zone in input_params.zones:
        demand = calculate_zone_demand(
            zone=zone,
            temperature_c=input_params.water_temperature_c,
            depth_m=input_params.water_depth_m,
            calcareous=False,  # Design for initial (conservative)
        )
        total_demand += demand
        zone_demands.append({
            "zone_name": zone.name,
            "zone_type": zone.zone_type.value,
            "surface_area_m2": zone.surface_area_m2,
            "coating_breakdown_factor": zone.coating_breakdown_factor,
            "current_demand_A": round(demand, 4),
        })

    # Total anode mass from mean current demand
    # M = (I_mean * t * 8760) / (capacity * u_f)
    total_mass = (total_demand * input_params.design_life_years * 8760.0) / (
        input_params.anode_capacity_Ah_kg * input_params.utilization_factor
    )

    n_anodes = max(1, math.ceil(total_mass / input_params.anode_net_mass_kg))

    return MarineCPResult(
        structure_name=input_params.structure_name,
        total_current_demand_A=round(total_demand, 4),
        total_anode_mass_kg=round(total_mass, 2),
        number_of_anodes=n_anodes,
        zone_demands=zone_demands,
        design_life_years=input_params.design_life_years,
    )
