"""Cathodic protection design for marine and offshore structures.

Covers CP design for offshore platforms, jackets, monopiles, and subsea
structures with zone-based current density allocation (splash, tidal,
submerged, buried zones), anode distribution, and retrofitting assessment.

Design current densities come from the cited DNV-RP-B401 table lookups in
``b401_tables`` (Table 10-1 initial/final, Table 10-2 mean, Sec. 6.3 buried)
keyed by climatic region and depth band; every result carries the rendered
citations it used (issue #2207).

Anode count follows the full B401 Sec. 7 loop (issue #2211):
``N = max(N_mass, N_initial, N_final)`` where ``N_mass`` comes from the
mean demand and Eq. 2, ``N_initial`` from the initial demand and the fresh
stand-off anode output (Table 10-7 resistance, Table 10-6 driving voltage)
and ``N_final`` from the final demand and the anode consumed to its
utilisation limit (``anode_sizing.depleted_equivalent_radius``). The
current-output checks run when the anode length is given; otherwise the
count is mass based and ``current_output_checked`` is False.

References
----------
- DNV-RP-B401 "Cathodic Protection Design", Tables 10-1, 10-2, Sec. 6.3
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently Installed
  Steel Offshore Structures"
- ISO 12473 (2006) "General Principles of Cathodic Protection in Seawater"
"""

from __future__ import annotations

from enum import Enum
from typing import Any, Final, NamedTuple

from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.anode_sizing import (
    GOVERNING_MASS,
    depleted_equivalent_radius,
    governing_case,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeMaterial,
    Climate,
    DepthBand,
    DesignPhase,
    buried_current_density,
    citation_label,
    climate_from_temperature,
    depth_band,
    design_current_density,
    design_driving_voltage,
)
from digitalmodel.citations import CitedValue


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


# ClimateRegion is kept for backward compatibility; the table lookups are
# keyed on ``b401_tables.Climate``.
_CLIMATE_BY_REGION: Final[dict[ClimateRegion, Climate]] = {
    ClimateRegion.TROPICAL: Climate.TROPICAL,
    ClimateRegion.SUBTROPICAL: Climate.SUBTROPICAL,
    ClimateRegion.TEMPERATE: Climate.TEMPERATE,
    ClimateRegion.ARCTIC: Climate.ARCTIC,
}

_PHASES: Final[tuple[DesignPhase, ...]] = (
    DesignPhase.INITIAL,
    DesignPhase.MEAN,
    DesignPhase.FINAL,
)

# Bare steel: an omitted coating breakdown factor means f_c = 1.0.
_BARE_STEEL_BREAKDOWN: Final = 1.0

_HOURS_PER_YEAR: Final = kernel.HOURS_PER_YEAR

# Default seawater resistivity [ohm-m] for the anode resistance.
DEFAULT_SEAWATER_RESISTIVITY_OHM_M: Final = 0.30


class DesignLoopResult(NamedTuple):
    """Outcome of the B401 Sec. 7 anode-count loop for stand-off anodes."""

    number_of_anodes: int
    number_of_anodes_mass: int
    number_of_anodes_initial: int
    number_of_anodes_final: int
    governing_case: str
    anode_resistance_initial_ohm: float
    anode_current_output_initial_A: float
    anode_resistance_final_ohm: float
    anode_current_output_final_A: float
    equivalent_radius_initial_m: float
    equivalent_radius_final_m: float


def standoff_anode_design_loop(
    total_mass_kg: float,
    initial_current_A: float,
    final_current_A: float,
    anode_net_mass_kg: float,
    anode_length_m: float,
    utilization_factor: float,
    seawater_resistivity_ohm_m: float = DEFAULT_SEAWATER_RESISTIVITY_OHM_M,
    anode_density_kg_m3: float = kernel.ANODE_DENSITY_ALZNI,
    driving_voltage_V: float | None = None,
) -> DesignLoopResult:
    """B401 Sec. 7 anode count for slender stand-off anodes.

    ``N_mass = ceil(M / m_a)``; ``N_initial = ceil(I_initial / I_a)`` with
    the fresh anode (equivalent radius from ``m_a``, length ``L``);
    ``N_final = ceil(I_final / I_a,final)`` with the anode consumed to its
    utilisation limit: remaining mass ``(1 - u) m_a`` at the same length
    and the mass-based equivalent radius (assumption documented in
    ``anode_sizing.depleted_equivalent_radius``; B401 Sec. 7.8 asks for the
    final resistance of the depleted anode). The Table 10-7 long or short
    slender form is selected from the length ratio of each geometry.

    Parameters
    ----------
    total_mass_kg : float
        Net anode mass requirement M [kg].
    initial_current_A, final_current_A : float
        Initial and final current demands [A].
    anode_net_mass_kg : float
        Net mass of one anode [kg].
    anode_length_m : float
        Anode length [m].
    utilization_factor : float
        Utilisation factor u.
    seawater_resistivity_ohm_m : float
        Seawater resistivity [ohm-m].
    anode_density_kg_m3 : float
        Alloy density for the equivalent radius [kg/m3].
    driving_voltage_V : float, optional
        Design driving voltage [V]; default the Table 10-6 value for an
        Al-based anode in seawater (0.25 V).
    """
    if driving_voltage_V is None:
        driving_voltage_V = design_driving_voltage(
            AnodeMaterial.ALUMINIUM, DEFAULT_EDITION
        ).value

    n_mass = kernel.anode_count(total_mass_kg, anode_net_mass_kg)

    r_initial = kernel.equivalent_radius_from_mass(
        anode_net_mass_kg, anode_length_m, anode_density_kg_m3
    )
    R_initial = kernel.slender_standoff(seawater_resistivity_ohm_m, anode_length_m, r_initial)
    I_initial = kernel.anode_current_output(driving_voltage_V, R_initial)
    n_initial = kernel.anodes_for_current(initial_current_A, I_initial)

    r_final = depleted_equivalent_radius(
        anode_net_mass_kg, anode_length_m, utilization_factor, anode_density_kg_m3
    )
    R_final = kernel.slender_standoff(seawater_resistivity_ohm_m, anode_length_m, r_final)
    I_final = kernel.anode_current_output(driving_voltage_V, R_final)
    n_final = kernel.anodes_for_current(final_current_A, I_final)

    return DesignLoopResult(
        number_of_anodes=max(1, n_mass, n_initial, n_final),
        number_of_anodes_mass=n_mass,
        number_of_anodes_initial=n_initial,
        number_of_anodes_final=n_final,
        governing_case=governing_case(n_mass, n_initial, n_final),
        anode_resistance_initial_ohm=R_initial,
        anode_current_output_initial_A=I_initial,
        anode_resistance_final_ohm=R_final,
        anode_current_output_final_A=I_final,
        equivalent_radius_initial_m=r_initial,
        equivalent_radius_final_m=r_final,
    )


class StructuralZone(BaseModel):
    """A zone of a marine structure for CP design."""

    zone_name: str = Field(..., description="Zone identifier")
    exposure_zone: ExposureZone = Field(..., description="Exposure zone type")
    surface_area_m2: float = Field(..., gt=0, description="Surface area [m²]")
    depth_m: float = Field(
        default=0.0,
        ge=0.0,
        description=(
            "Representative water depth of the zone [m]; selects the "
            "Table 10-1 / 10-2 depth band for submerged surfaces"
        ),
    )
    coating_breakdown_factor: float | None = Field(
        default=None,
        ge=0.0,
        le=1.0,
        description=(
            "Coating breakdown factor f_c (0 = perfect coating, 1 = bare). "
            "None means bare steel (f_c = 1.0); an explicit 0.0 is honoured."
        ),
    )

    @property
    def effective_breakdown_factor(self) -> float:
        """f_c used in the demand calculation: ``None`` is bare steel."""
        if self.coating_breakdown_factor is None:
            return _BARE_STEEL_BREAKDOWN
        return self.coating_breakdown_factor


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
        ..., description="Total number of anodes required (max of the cases)"
    )
    number_of_anodes_mass: int = Field(
        default=0, description="Anodes required by mass (B401 Eq. 2)"
    )
    number_of_anodes_initial: int | None = Field(
        default=None,
        description="Anodes required by the initial current-output check (None if not run)",
    )
    number_of_anodes_final: int | None = Field(
        default=None,
        description="Anodes required by the final (depleted) current-output check",
    )
    governing_case: str = Field(
        default=GOVERNING_MASS, description="'mass', 'initial' or 'final'"
    )
    current_output_checked: bool = Field(
        default=False,
        description="Whether the Sec. 7.8 current-output checks ran (anode length given)",
    )
    anode_resistance_initial_ohm: float | None = Field(
        default=None, description="Fresh stand-off anode resistance [ohm]"
    )
    anode_current_output_initial_A: float | None = Field(
        default=None, description="Fresh stand-off anode current output [A]"
    )
    anode_resistance_final_ohm: float | None = Field(
        default=None, description="Depleted stand-off anode resistance [ohm]"
    )
    anode_current_output_final_A: float | None = Field(
        default=None, description="Depleted stand-off anode current output [A]"
    )
    zone_details: list[dict] = Field(
        default_factory=list,
        description="Per-zone breakdown of current demands",
    )
    edition_used: Edition = Field(
        ...,
        description="DNV-RP-B401 edition used for the marine-structure CP design",
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


def _resolve_climate(
    climate_region: ClimateRegion,
    surface_temperature_c: float | None,
) -> Climate:
    """Climate from the surface temperature when given, else from the region."""
    if surface_temperature_c is not None:
        return climate_from_temperature(surface_temperature_c)
    return _CLIMATE_BY_REGION[ClimateRegion(climate_region)]


def zone_current_density(
    exposure_zone: ExposureZone,
    climate: Climate,
    depth_m: float,
    phase: DesignPhase,
    edition: Edition,
) -> CitedValue | None:
    """Design current density [A/m²] for one exposure zone and phase.

    Parameters
    ----------
    exposure_zone : ExposureZone
        Exposure zone of the surface.
    climate : Climate
        Climatic region (Table 10-1 / 10-2 column).
    depth_m : float
        Representative depth [m]; mapped to a Table 10-1 / 10-2 band.
    phase : DesignPhase
        Initial, mean or final.
    edition : Edition
        Normalized DNV-RP-B401 edition token.

    Returns
    -------
    CitedValue or None
        Cited density in A/m², or ``None`` for zones above the waterline
        where CP is not applied (density 0.0, nothing to cite).
    """
    zone = ExposureZone(exposure_zone)
    if zone is ExposureZone.SUBMERGED:
        return design_current_density(climate, depth_band(depth_m), phase, edition)
    if zone is ExposureZone.TIDAL:
        # B401 Tables 10-1 / 10-2 have no tidal row; the tidal zone is treated
        # as seawater-exposed bare metal in the shallowest (0-30 m) band.
        return design_current_density(climate, DepthBand.M0_30, phase, edition)
    if zone is ExposureZone.BURIED_MUDLINE:
        # Sec. 6.3: 0.020 A/m2 for all phases. The previous 25/20/20 mA/m2
        # were ABS values, not B401.
        return buried_current_density(edition)
    # SPLASH and ATMOSPHERIC: CP is not applied above the waterline in the
    # scope of B401; these zones rely on coating and corrosion allowance.
    # Design choice of this module (0.0 A/m2), no clause to cite.
    return None


def _zone_densities(
    zone: StructuralZone,
    climate: Climate,
    edition: Edition,
) -> tuple[dict[DesignPhase, float], list[str]]:
    """Densities [A/m²] per phase and the distinct citations used."""
    densities: dict[DesignPhase, float] = {}
    labels: list[str] = []
    for phase in _PHASES:
        cited = zone_current_density(
            zone.exposure_zone, climate, zone.depth_m, phase, edition
        )
        if cited is None:
            densities[phase] = 0.0
            continue
        densities[phase] = cited.value
        label = citation_label(cited.citation)
        if label not in labels:
            labels.append(label)
    return densities, labels


def marine_structure_current_demand(
    zones: list[StructuralZone],
    climate_region: ClimateRegion = ClimateRegion.TEMPERATE,
    design_life_years: float = 25.0,
    anode_net_mass_kg: float = 200.0,
    anode_capacity_Ah_kg: float = 2000.0,
    utilization_factor: float = 0.90,
    edition: Edition | None = None,
    surface_temperature_c: float | None = None,
    anode_length_m: float | None = None,
    seawater_resistivity_ohm_m: float = DEFAULT_SEAWATER_RESISTIVITY_OHM_M,
    anode_density_kg_m3: float = kernel.ANODE_DENSITY_ALZNI,
    driving_voltage_V: float | None = None,
) -> MarineCPResult:
    """Calculate current demand and anode requirements for an offshore structure.

    Sums current demand across all exposure zones using the DNV-RP-B401
    Table 10-1 (initial, final) and Table 10-2 (mean) current densities by
    climate and depth band, and each zone's coating breakdown factor, then
    sizes the anodes with the B401 Sec. 7 loop
    (``standoff_anode_design_loop``) when ``anode_length_m`` is given.

    Parameters
    ----------
    zones : list[StructuralZone]
        List of structural zones with areas, depths and coating data.
    climate_region : ClimateRegion
        Climate region for current density selection; ignored when
        ``surface_temperature_c`` is given.
    design_life_years : float
        CP design life [years].
    anode_net_mass_kg : float
        Net mass of a single anode [kg].
    anode_capacity_Ah_kg : float
        Anode electrochemical capacity [A-h/kg].
    utilization_factor : float
        Anode utilization factor.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.
    surface_temperature_c : float, optional
        Surface water temperature [°C]; when given it selects the climatic
        region via ``b401_tables.climate_from_temperature``.
    anode_length_m : float, optional
        Stand-off anode length [m]; enables the initial / final
        current-output checks (B401 Sec. 7.8). ``None`` gives a mass-based
        count with ``current_output_checked = False``.
    seawater_resistivity_ohm_m : float
        Seawater resistivity at the anodes [ohm-m].
    anode_density_kg_m3 : float
        Alloy density for the mass-based equivalent radius [kg/m3].
    driving_voltage_V : float, optional
        Design driving voltage [V]; default the Table 10-6 value for an
        Al-based anode in seawater (0.25 V), cited in the result.

    Returns
    -------
    MarineCPResult
        Total current demand, anode mass, number of anodes per case,
        governing case and citations.
    """
    ed = normalize_edition(edition, stacklevel=3)
    climate = _resolve_climate(climate_region, surface_temperature_c)

    total_initial = 0.0
    total_mean = 0.0
    total_final = 0.0
    zone_details: list[dict] = []
    citations: list[str] = []

    for zone in zones:
        densities, labels = _zone_densities(zone, climate, ed)
        fc = zone.effective_breakdown_factor

        i_initial = zone.surface_area_m2 * fc * densities[DesignPhase.INITIAL]
        i_mean = zone.surface_area_m2 * fc * densities[DesignPhase.MEAN]
        i_final = zone.surface_area_m2 * fc * densities[DesignPhase.FINAL]

        total_initial += i_initial
        total_mean += i_mean
        total_final += i_final

        for label in labels:
            if label not in citations:
                citations.append(label)

        zone_details.append({
            "zone_name": zone.zone_name,
            "exposure_zone": zone.exposure_zone.value,
            "surface_area_m2": zone.surface_area_m2,
            "depth_m": zone.depth_m,
            "climate": climate.value,
            "coating_breakdown_factor": fc,
            "initial_current_density_A_m2": densities[DesignPhase.INITIAL],
            "mean_current_density_A_m2": densities[DesignPhase.MEAN],
            "final_current_density_A_m2": densities[DesignPhase.FINAL],
            "initial_current_A": round(i_initial, 4),
            "mean_current_A": round(i_mean, 4),
            "final_current_A": round(i_final, 4),
            "citations": labels,
        })

    # Anode mass from mean current demand (DNV-RP-B401 §7.7.1, Eq 2)
    total_mass = kernel.anode_mass(
        total_mean, design_life_years, anode_capacity_Ah_kg, utilization_factor
    )
    n_mass = kernel.anode_count(total_mass, anode_net_mass_kg)

    if anode_length_m is None:
        return MarineCPResult(
            total_initial_current_A=round(total_initial, 4),
            total_mean_current_A=round(total_mean, 4),
            total_final_current_A=round(total_final, 4),
            total_anode_mass_kg=round(total_mass, 2),
            number_of_anodes=max(1, n_mass),
            number_of_anodes_mass=n_mass,
            governing_case=GOVERNING_MASS,
            current_output_checked=False,
            zone_details=zone_details,
            edition_used=ed,
            standard=standard_for_edition(ed),
            citations=citations,
        )

    if driving_voltage_V is None:
        cited_voltage = design_driving_voltage(AnodeMaterial.ALUMINIUM, ed)
        driving_voltage_V = cited_voltage.value
        label = citation_label(cited_voltage.citation)
        if label not in citations:
            citations.append(label)

    loop = standoff_anode_design_loop(
        total_mass_kg=total_mass,
        initial_current_A=total_initial,
        final_current_A=total_final,
        anode_net_mass_kg=anode_net_mass_kg,
        anode_length_m=anode_length_m,
        utilization_factor=utilization_factor,
        seawater_resistivity_ohm_m=seawater_resistivity_ohm_m,
        anode_density_kg_m3=anode_density_kg_m3,
        driving_voltage_V=driving_voltage_V,
    )

    return MarineCPResult(
        total_initial_current_A=round(total_initial, 4),
        total_mean_current_A=round(total_mean, 4),
        total_final_current_A=round(total_final, 4),
        total_anode_mass_kg=round(total_mass, 2),
        number_of_anodes=loop.number_of_anodes,
        number_of_anodes_mass=loop.number_of_anodes_mass,
        number_of_anodes_initial=loop.number_of_anodes_initial,
        number_of_anodes_final=loop.number_of_anodes_final,
        governing_case=loop.governing_case,
        current_output_checked=True,
        anode_resistance_initial_ohm=loop.anode_resistance_initial_ohm,
        anode_current_output_initial_A=loop.anode_current_output_initial_A,
        anode_resistance_final_ohm=loop.anode_resistance_final_ohm,
        anode_current_output_final_A=loop.anode_current_output_final_A,
        zone_details=zone_details,
        edition_used=ed,
        standard=standard_for_edition(ed),
        citations=citations,
    )


def anode_distribution(
    zones: list[StructuralZone],
    total_anodes: int,
    climate_region: ClimateRegion = ClimateRegion.TEMPERATE,
    edition: Edition | None = None,
    surface_temperature_c: float | None = None,
) -> dict[str, int]:
    """Distribute anodes across structural zones proportional to current demand.

    Allocates anodes based on each zone's fraction of total final current
    demand (conservative approach using Table 10-1 final values).

    Parameters
    ----------
    zones : list[StructuralZone]
        Structural zones.
    total_anodes : int
        Total number of anodes to distribute.
    climate_region : ClimateRegion
        Climate region; ignored when ``surface_temperature_c`` is given.
    edition : Edition, optional
        DNV-RP-B401 edition token; ``None`` warns and defaults to 2021.
    surface_temperature_c : float, optional
        Surface water temperature [°C] selecting the climatic region.

    Returns
    -------
    dict[str, int]
        Anode count per zone name.
    """
    ed = normalize_edition(edition, stacklevel=3)
    climate = _resolve_climate(climate_region, surface_temperature_c)

    demands: list[tuple[str, float]] = []
    for zone in zones:
        cited = zone_current_density(
            zone.exposure_zone, climate, zone.depth_m, DesignPhase.FINAL, ed
        )
        ic_final = 0.0 if cited is None else cited.value
        demand = zone.surface_area_m2 * zone.effective_breakdown_factor * ic_final
        demands.append((zone.zone_name, demand))

    total_demand = sum(d for _, d in demands)
    if total_demand <= 0:
        # Equal distribution if no demand
        per_zone = max(1, total_anodes // len(zones)) if zones else 0
        return {name: per_zone for name, _ in demands}

    distribution: dict[str, int] = {}
    allocated = 0
    for name, demand in demands:
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
    and checks measured potential against protection criteria. The metal
    consumed is the Faraday mass ``I * t * 8760 / epsilon``; the usable
    mass is ``M * u``; the additional mass for a shortfall is the Eq. 2
    requirement (issue #2211).

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
    # Metal consumed so far (Faraday) and the usable mass still available
    mass_consumed = kernel.mass_consumed(mean_current_A, elapsed_years, anode_capacity_Ah_kg)
    usable_remaining = max(
        0.0, original_anode_mass_kg * utilization_factor - mass_consumed
    )

    # Remaining life from the usable mass
    if mean_current_A > 0:
        remaining_life = (usable_remaining * anode_capacity_Ah_kg) / (
            mean_current_A * _HOURS_PER_YEAR
        )
    else:
        remaining_life = design_life_years - elapsed_years

    remaining_design = design_life_years - elapsed_years
    shortfall_years = max(0, remaining_design - remaining_life)

    # Additional mass needed for remaining design life (Eq. 2 requirement)
    additional_mass = 0.0
    if shortfall_years > 0:
        additional_mass = kernel.anode_mass(
            mean_current_A, shortfall_years, anode_capacity_Ah_kg, utilization_factor
        )

    additional_anodes = kernel.anode_count(additional_mass, anode_net_mass_kg)

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
