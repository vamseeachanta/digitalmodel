"""Marine structure CP assessment — deprecated facade (issue #2211).

This module used to carry a second multi-zone design next to
:mod:`digitalmodel.cathodic_protection.marine_structure_cp`. It is now a
thin facade: the ``Zone`` / ``MarineCPInput`` / ``MarineCPResult`` models
and the density helpers are kept for callers, and ``design_marine_cp``
maps its input onto ``marine_structure_current_demand`` (which runs the
full DNV-RP-B401 Sec. 7 loop when anode geometry is given). Importing
``design_marine_cp`` emits a :class:`DeprecationWarning`; new code should
use ``marine_structure_cp`` directly.

Current densities are the DNV-RP-B401 Table 10-1 (initial, final) and
Table 10-2 (mean) step values keyed by climatic region
(``b401_tables.climate_from_temperature``) and depth band
(``b401_tables.depth_band``); buried surfaces use Sec. 6.3.
"""

from __future__ import annotations

import warnings
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
    DesignPhase,
    citation_label,
    climate_from_temperature,
    depth_band,
    design_current_density,
)
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ExposureZone,
    StructuralZone,
    marine_structure_current_demand,
)
from digitalmodel.cathodic_protection.marine_structure_cp import (
    zone_current_density as _structure_zone_current_density,
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


_EXPOSURE_BY_ZONE_TYPE: Final[dict[ZoneType, ExposureZone]] = {
    ZoneType.ATMOSPHERIC: ExposureZone.ATMOSPHERIC,
    ZoneType.SPLASH: ExposureZone.SPLASH,
    ZoneType.TIDAL: ExposureZone.TIDAL,
    ZoneType.SUBMERGED: ExposureZone.SUBMERGED,
    ZoneType.MUDLINE: ExposureZone.BURIED_MUDLINE,
}

_MA_PER_A: Final = 1000.0


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
    anode_length_m: float | None = Field(
        default=None,
        gt=0,
        description=(
            "Stand-off anode length [m]; when given the B401 Sec. 7.8 "
            "initial / final current-output checks run"
        ),
    )
    seawater_resistivity_ohm_m: float = Field(
        default=0.30, gt=0, description="Seawater resistivity [ohm-m]"
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
    governing_case: str = Field(
        default="mass", description="'mass', 'initial' or 'final'"
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

    Delegates to ``marine_structure_cp.zone_current_density``. Returns
    ``None`` for splash and atmospheric zones: CP is not applied above the
    waterline in the scope of B401 (0.0 A/m², nothing to cite).
    """
    return _structure_zone_current_density(
        _EXPOSURE_BY_ZONE_TYPE[ZoneType(zone_type)],
        climate_from_temperature(temperature_c),
        depth_m,
        phase,
        edition,
    )


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


def _to_structural_zone(zone: Zone, depth_m: float) -> StructuralZone:
    return StructuralZone(
        zone_name=zone.name,
        exposure_zone=_EXPOSURE_BY_ZONE_TYPE[ZoneType(zone.zone_type)],
        surface_area_m2=zone.surface_area_m2,
        depth_m=depth_m,
        coating_breakdown_factor=zone.coating_breakdown_factor,
    )


def _design_marine_cp(
    input_params: MarineCPInput,
    edition: Edition | None = None,
) -> MarineCPResult:
    """Multi-zone CP design via ``marine_structure_current_demand``.

    Per-zone mean current demands (Table 10-2 / Sec. 6.3), total anode mass
    from the mean demand and the anode count from the B401 Sec. 7 loop
    (mass only unless ``anode_length_m`` is given).

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
    zones = [_to_structural_zone(z, input_params.water_depth_m) for z in input_params.zones]
    result = marine_structure_current_demand(
        zones=zones,
        design_life_years=input_params.design_life_years,
        anode_net_mass_kg=input_params.anode_net_mass_kg,
        anode_capacity_Ah_kg=input_params.anode_capacity_Ah_kg,
        utilization_factor=input_params.utilization_factor,
        edition=ed,
        surface_temperature_c=input_params.water_temperature_c,
        anode_length_m=input_params.anode_length_m,
        seawater_resistivity_ohm_m=input_params.seawater_resistivity_ohm_m,
    )

    zone_demands: list[dict] = []
    for zone, detail in zip(input_params.zones, result.zone_details, strict=True):
        cited = zone_current_density(
            zone.zone_type,
            input_params.water_temperature_c,
            input_params.water_depth_m,
            DesignPhase.MEAN,
            ed,
        )
        zone_demands.append({
            "zone_name": zone.name,
            "zone_type": zone.zone_type.value,
            "surface_area_m2": zone.surface_area_m2,
            "coating_breakdown_factor": zone.coating_breakdown_factor,
            "mean_current_density_A_m2": detail["mean_current_density_A_m2"],
            "current_demand_A": detail["mean_current_A"],
            "citation": None if cited is None else citation_label(cited.citation),
        })

    return MarineCPResult(
        structure_name=input_params.structure_name,
        total_current_demand_A=result.total_mean_current_A,
        total_anode_mass_kg=result.total_anode_mass_kg,
        number_of_anodes=result.number_of_anodes,
        governing_case=result.governing_case,
        zone_demands=zone_demands,
        design_life_years=input_params.design_life_years,
        edition_used=ed,
        standard=standard_for_edition(ed),
        citations=result.citations,
    )


def __getattr__(name: str) -> Any:
    """Emit a DeprecationWarning when ``design_marine_cp`` is imported."""
    if name == "design_marine_cp":
        warnings.warn(
            "digitalmodel.cathodic_protection.marine_cp.design_marine_cp is "
            "deprecated; use marine_structure_cp.marine_structure_current_demand.",
            DeprecationWarning,
            stacklevel=2,
        )
        return _design_marine_cp
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    "MarineCPInput",
    "MarineCPResult",
    "Zone",
    "ZoneType",
    "calculate_zone_demand",
    "design_marine_cp",
    "get_seawater_current_density",
    "seawater_current_density",
    "zone_current_density",
]
