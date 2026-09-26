"""Marine structure CP assessment — multi-zone design.

Provides seawater current density lookup by temperature and depth,
zone-based current demand calculation, and end-to-end multi-zone CP
design for offshore structures.

Current densities are the DNV-RP-B401 Table 10-1 (initial, final) and
Table 10-2 (mean) step values, keyed by climatic region
(``b401_tables.climate_from_temperature``) and depth band
(``b401_tables.depth_band``); buried surfaces use Sec. 6.3. The former
continuous temperature/depth model and the uncited calcareous-deposit
reduction factor were removed (issue #2207): the B401 design current
densities already account for calcareous deposit formation.

References
----------
- DNV-RP-B401 "Cathodic Protection Design" §6.3, Tables 10-1 and 10-2
- ISO 12473 (2006) "General Principles of Cathodic Protection in Seawater"
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently
  Installed Steel Offshore Structures"
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Any, Final

from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    DepthBand,
    DesignPhase,
    buried_current_density,
    citation_label,
    climate_from_temperature,
    depth_band,
    design_current_density,
)
from digitalmodel.citations import CitedValue


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


_MA_PER_A: Final = 1000.0
_HOURS_PER_YEAR: Final = 8760.0


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
    edition_used: Edition = Field(
        ..., description="DNV-RP-B401 edition used for the marine CP design"
    )
    standard: str = Field(
        ..., description="Standards reference matching the selected edition"
    )
    citations: list[str] = Field(
        default_factory=list,
        description=(
            "Rendered citations ('code_id revision section') of every table "
            "value used, in first-use order"
        ),
    )

    @property
    def edition(self) -> Edition:
        """Alias of ``edition_used`` for report provenance."""
        return self.edition_used

    @model_validator(mode="before")
    @classmethod
    def _default_legacy_metadata(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data

        values = dict(data)
        edition = values.get("edition_used") or DEFAULT_EDITION
        values["edition_used"] = edition
        if not values.get("standard"):
            try:
                values["standard"] = standard_for_edition(edition)
            except KeyError:
                pass
        return values


def seawater_current_density(
    temperature_c: float,
    depth_m: float,
    phase: DesignPhase = DesignPhase.MEAN,
    edition: Edition | None = None,
) -> CitedValue:
    """Cited design current density [A/m²] for submerged bare steel.

    Parameters
    ----------
    temperature_c : float
        Surface seawater temperature [°C] → Table 10-1 / 10-2 climate column.
    depth_m : float
        Water depth [m] → Table 10-1 / 10-2 depth band row.
    phase : DesignPhase
        Initial or final (Table 10-1) or mean (Table 10-2); default mean.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    CitedValue
        Current density in A/m² with its table citation.
    """
    ed = normalize_edition(edition, stacklevel=3)
    return design_current_density(
        climate_from_temperature(temperature_c), depth_band(depth_m), phase, ed
    )


def get_seawater_current_density(
    temperature_c: float,
    depth_m: float,
    calcareous: bool = False,
    phase: DesignPhase = DesignPhase.MEAN,
    edition: Edition | None = None,
) -> float:
    """Get seawater current density for submerged bare steel [mA/m²].

    Step lookup in DNV-RP-B401 Table 10-1 (initial, final) or Table 10-2
    (mean) by climatic region and depth band.

    Parameters
    ----------
    temperature_c : float
        Seawater temperature [°C].
    depth_m : float
        Water depth [m].
    calcareous : bool
        Accepted for signature compatibility and ignored: the B401 design
        current densities already include the effect of calcareous deposit
        formation, so no separate reduction is applied.
    phase : DesignPhase
        Design phase; default mean (Table 10-2).
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    float
        Design current density [mA/m²].
    """
    del calcareous  # B401 densities already include calcareous deposits
    ed = normalize_edition(edition, stacklevel=3)
    return seawater_current_density(temperature_c, depth_m, phase, ed).value * _MA_PER_A


def zone_current_density(
    zone_type: ZoneType,
    temperature_c: float,
    depth_m: float,
    phase: DesignPhase,
    edition: Edition,
) -> CitedValue | None:
    """Cited design current density [A/m²] for one zone type and phase.

    Returns ``None`` for splash and atmospheric zones: CP is not applied
    above the waterline in the scope of B401 (0.0 A/m², nothing to cite).
    """
    zt = ZoneType(zone_type)
    climate = climate_from_temperature(temperature_c)
    if zt is ZoneType.SUBMERGED:
        return design_current_density(climate, depth_band(depth_m), phase, edition)
    if zt is ZoneType.TIDAL:
        # B401 Tables 10-1 / 10-2 have no tidal row; the tidal zone is treated
        # as seawater-exposed bare metal in the shallowest (0-30 m) band.
        return design_current_density(climate, DepthBand.M0_30, phase, edition)
    if zt is ZoneType.MUDLINE:
        # Sec. 6.3: 0.020 A/m2 for bare metal buried in sediments, all phases.
        return buried_current_density(edition)
    return None


def calculate_zone_demand(
    zone: Zone,
    temperature_c: float = 15.0,
    depth_m: float = 30.0,
    calcareous: bool = False,
    phase: DesignPhase = DesignPhase.MEAN,
    edition: Edition | None = None,
) -> float:
    """Calculate current demand for a single structural zone.

    I_zone = A * f_c * i_c

    Parameters
    ----------
    zone : Zone
        Structural zone definition.
    temperature_c : float
        Seawater temperature [°C].
    depth_m : float
        Water depth [m].
    calcareous : bool
        Accepted for signature compatibility and ignored (see
        ``get_seawater_current_density``).
    phase : DesignPhase
        Design phase; default mean.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    float
        Zone current demand [A].
    """
    del calcareous  # B401 densities already include calcareous deposits
    ed = normalize_edition(edition, stacklevel=3)
    cited = zone_current_density(zone.zone_type, temperature_c, depth_m, phase, ed)
    density = 0.0 if cited is None else cited.value
    return zone.surface_area_m2 * zone.coating_breakdown_factor * density


def design_marine_cp(
    input_params: MarineCPInput,
    edition: Edition | None = None,
) -> MarineCPResult:
    """Design multi-zone CP system for a marine structure.

    Calculates per-zone mean current demands (Table 10-2 / Sec. 6.3),
    total anode mass, and number of anodes required for the full structure.

    Parameters
    ----------
    input_params : MarineCPInput
        Structure definition with zones and environmental parameters.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.

    Returns
    -------
    MarineCPResult
        Complete CP design result with per-zone breakdown and citations.
    """
    ed = normalize_edition(edition, stacklevel=3)

    zone_demands: list[dict] = []
    citations: list[str] = []
    total_demand = 0.0

    for zone in input_params.zones:
        cited = zone_current_density(
            zone.zone_type,
            input_params.water_temperature_c,
            input_params.water_depth_m,
            DesignPhase.MEAN,
            ed,
        )
        density = 0.0 if cited is None else cited.value
        label = None if cited is None else citation_label(cited.citation)
        if label is not None and label not in citations:
            citations.append(label)
        demand = zone.surface_area_m2 * zone.coating_breakdown_factor * density
        total_demand += demand
        zone_demands.append({
            "zone_name": zone.name,
            "zone_type": zone.zone_type.value,
            "surface_area_m2": zone.surface_area_m2,
            "coating_breakdown_factor": zone.coating_breakdown_factor,
            "mean_current_density_A_m2": density,
            "current_demand_A": round(demand, 4),
            "citation": label,
        })

    # Total anode mass from mean current demand (DNV-RP-B401 §7.7.1, Eq 2)
    # M = (I_mean * t * 8760) / (capacity * u_f)
    total_mass = (total_demand * input_params.design_life_years * _HOURS_PER_YEAR) / (
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
        edition_used=ed,
        standard=standard_for_edition(ed),
        citations=citations,
    )
