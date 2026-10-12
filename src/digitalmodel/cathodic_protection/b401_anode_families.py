"""Independent B401 anode-family sizing for multi-environment structures."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import Edition
from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    calculate_anode_resistance,
    depleted_equivalent_radius,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AMBIENT_ANODE_TEMPERATURE_C,
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    anode_capacity,
    anode_closed_circuit_potential,
    anode_resistance_citation,
    citation_label,
    design_driving_voltage,
    utilisation_factor,
)

CASE_ORDER = ("mass", "initial", "final")


class AnodeFamily(BaseModel):
    """One electrically independent anode family and its electrolyte."""

    name: str = Field(..., min_length=1)
    material: AnodeMaterial = AnodeMaterial.ALUMINIUM
    environment: AnodeEnvironment = AnodeEnvironment.SEAWATER
    anode_type: AnodeType = AnodeType.STAND_OFF
    individual_mass_kg: float = Field(..., gt=0)
    length_m: float = Field(..., gt=0)
    electrolyte_resistivity_ohm_m: float = Field(..., gt=0)
    utilization_factor: float | None = Field(default=None, gt=0, le=1)
    installed_count: int | None = Field(default=None, ge=1)
    anode_surface_temperature_c: float = AMBIENT_ANODE_TEMPERATURE_C
    density_kg_m3: float = Field(default=kernel.ANODE_DENSITY_ALZNI, gt=0)
    radius_m: float | None = Field(default=None, gt=0)
    width_m: float | None = Field(default=None, gt=0)
    thickness_m: float | None = Field(default=None, gt=0)
    exposed_area_m2: float | None = Field(default=None, gt=0)
    final_length_m: float | None = Field(default=None, gt=0)
    final_width_m: float | None = Field(default=None, gt=0)
    final_thickness_m: float | None = Field(default=None, gt=0)
    final_exposed_area_m2: float | None = Field(default=None, gt=0)

    @model_validator(mode="after")
    def _explicit_final_geometry(self) -> "AnodeFamily":
        if self.anode_type is AnodeType.FLUSH_MOUNT:
            fields = (
                self.width_m,
                self.thickness_m,
                self.final_length_m,
                self.final_width_m,
                self.final_thickness_m,
            )
            if any(value is None for value in fields):
                raise ValueError(
                    "flush_mounted families require fresh and final dimensions"
                )
            assert self.width_m is not None and self.thickness_m is not None
            assert self.final_length_m is not None
            assert self.final_width_m is not None
            assert self.final_thickness_m is not None
            fresh = (self.length_m, self.width_m, self.thickness_m)
            final = (self.final_length_m, self.final_width_m, self.final_thickness_m)
            if any(end > start for start, end in zip(fresh, final, strict=True)):
                raise ValueError(
                    "final flush dimensions cannot exceed fresh dimensions"
                )
        if self.anode_type is AnodeType.BRACELET:
            if self.exposed_area_m2 is None or self.final_exposed_area_m2 is None:
                raise ValueError(
                    "bracelet families require fresh and final exposed area"
                )
            if self.final_exposed_area_m2 > self.exposed_area_m2:
                raise ValueError("final bracelet exposed area cannot exceed fresh area")
        return self


class FamilyDemand(BaseModel):
    """Initial, mean and final current demand assigned to one family."""

    initial: float = Field(..., ge=0)
    mean: float = Field(..., gt=0)
    final: float = Field(..., ge=0)


def _utilization(family: AnodeFamily, edition: Edition) -> tuple[float, str, list[str]]:
    if family.utilization_factor is not None:
        return family.utilization_factor, "input", []
    cited = utilisation_factor(_fresh_shape(family), edition)
    return cited.value, cited.citation.section, [citation_label(cited.citation)]


def _fresh_shape(family: AnodeFamily) -> AnodeShape:
    """Classify Table x-8 utilisation from the fresh physical geometry."""
    if family.anode_type is AnodeType.BRACELET:
        return AnodeShape.SHORT_FLUSH_BRACELET
    if family.anode_type is AnodeType.FLUSH_MOUNT:
        assert family.width_m is not None and family.thickness_m is not None
        if (
            family.length_m >= 4.0 * family.width_m
            and family.length_m >= 4.0 * family.thickness_m
        ):
            return AnodeShape.LONG_FLUSH
        return AnodeShape.SHORT_FLUSH_BRACELET
    radius = family.radius_m or kernel.equivalent_radius_from_mass(
        family.individual_mass_kg, family.length_m, family.density_kg_m3
    )
    if family.length_m >= 4.0 * radius:
        return AnodeShape.LONG_SLENDER_STANDOFF
    return AnodeShape.SHORT_SLENDER_STANDOFF


def _radii(family: AnodeFamily, utilization: float) -> tuple[float, float]:
    initial = family.radius_m or kernel.equivalent_radius_from_mass(
        family.individual_mass_kg, family.length_m, family.density_kg_m3
    )
    if family.radius_m is not None:
        final = family.radius_m * (1.0 - utilization) ** 0.5
    else:
        final = depleted_equivalent_radius(
            family.individual_mass_kg,
            family.length_m,
            utilization,
            family.density_kg_m3,
        )
    return initial, final


def _resistances(
    family: AnodeFamily, initial_radius: float, final_radius: float
) -> tuple[float, float]:
    common = (
        family.anode_type,
        family.length_m,
        initial_radius,
        family.electrolyte_resistivity_ohm_m,
    )
    initial = calculate_anode_resistance(
        *common,
        width_m=family.width_m,
        thickness_m=family.thickness_m,
        exposed_area_m2=family.exposed_area_m2,
    )
    final = calculate_anode_resistance(
        family.anode_type,
        family.final_length_m or family.length_m,
        final_radius,
        family.electrolyte_resistivity_ohm_m,
        width_m=family.final_width_m,
        thickness_m=family.final_thickness_m,
        exposed_area_m2=family.final_exposed_area_m2,
    )
    return initial, final


def _governing(ratios: Mapping[str, float]) -> tuple[str, float]:
    case = max(CASE_ORDER, key=lambda key: (ratios[key], -CASE_ORDER.index(key)))
    return case, ratios[case]


def _count_source(family: AnodeFamily) -> str:
    return "input" if family.installed_count is not None else "recommended"


def _verification(
    family: AnodeFamily,
    demand: FamilyDemand,
    design_life_years: float,
    capacity_ah_kg: float,
    driving_voltage_v: float,
    utilization: float,
    mean_current_years_A_year: float | None = None,
) -> dict[str, Any]:
    current_years = (
        demand.mean * design_life_years
        if mean_current_years_A_year is None
        else mean_current_years_A_year
    )
    required_mass = kernel.anode_mass_from_current_years(
        current_years, capacity_ah_kg, utilization
    )
    radii = _radii(family, utilization)
    resistance_initial, resistance_final = _resistances(family, *radii)
    output_initial = kernel.anode_current_output(driving_voltage_v, resistance_initial)
    output_final = kernel.anode_current_output(driving_voltage_v, resistance_final)
    counts = {
        "mass": kernel.anode_count(required_mass, family.individual_mass_kg),
        "initial": kernel.anodes_for_current(demand.initial, output_initial),
        "final": kernel.anodes_for_current(demand.final, output_final),
    }
    recommended = max(1, *counts.values())
    count = family.installed_count or recommended
    ratios = {
        "mass": required_mass / (count * family.individual_mass_kg),
        "initial": demand.initial / (count * output_initial),
        "final": demand.final / (count * output_final),
    }
    governing_case, governing_ratio = _governing(ratios)
    return {
        "required_mass_kg": required_mass,
        "mean_current_years_A_year": current_years,
        "count_by_mass": counts["mass"],
        "count_by_initial_current": counts["initial"],
        "count_by_final_current": counts["final"],
        "recommended_anode_count": recommended,
        "anode_count": count,
        "count_source": _count_source(family),
        "equivalent_radius_initial_m": radii[0],
        "equivalent_radius_final_m": radii[1],
        "resistance_initial_ohm": resistance_initial,
        "resistance_final_ohm": resistance_final,
        "current_output_initial_A": output_initial,
        "current_output_final_A": output_final,
        "total_output_initial_A": count * output_initial,
        "total_output_final_A": count * output_final,
        "adequacy_ratios": ratios,
        "checks": {key: value <= 1.0 for key, value in ratios.items()},
        "governing_case": governing_case,
        "governing_ratio": governing_ratio,
    }


def design_anode_family(
    family: AnodeFamily,
    demand: FamilyDemand,
    design_life_years: float,
    edition: Edition,
    *,
    mean_current_years_A_year: float | None = None,
) -> dict[str, Any]:
    """Size and verify one family with its own cited electrochemistry."""
    capacity = anode_capacity(
        family.material, family.environment, edition, family.anode_surface_temperature_c
    )
    potential = anode_closed_circuit_potential(
        family.material, family.environment, edition, family.anode_surface_temperature_c
    )
    voltage = design_driving_voltage(
        family.material, edition, family.environment, family.anode_surface_temperature_c
    )
    utilization, utilization_source, citations = _utilization(family, edition)
    citations.extend(
        citation_label(cv.citation) for cv in (capacity, potential, voltage)
    )
    citations.append(citation_label(anode_resistance_citation(edition)))
    result = {
        "name": family.name,
        "material": family.material.value,
        "environment": family.environment.value,
        "anode_type": family.anode_type.value,
        "anode_surface_temperature_C": family.anode_surface_temperature_c,
        "current_demand_A": demand.model_dump(),
        "capacity_Ah_kg": capacity.value,
        "closed_circuit_potential_V": potential.value,
        "driving_voltage_V": voltage.value,
        "utilization_factor": utilization,
        "utilization_factor_source": utilization_source,
        "electrolyte_resistivity_ohm_m": family.electrolyte_resistivity_ohm_m,
        "individual_mass_kg": family.individual_mass_kg,
        "citations": sorted(set(citations)),
    }
    result.update(
        _verification(
            family,
            demand,
            design_life_years,
            capacity.value,
            voltage.value,
            utilization,
            mean_current_years_A_year,
        )
    )
    return result


def overall_family_status(results: Mapping[str, Mapping[str, Any]]) -> dict[str, Any]:
    """Return the dimensionless worst-margin family/case and conjunction status."""
    name = max(sorted(results), key=lambda key: results[key]["governing_ratio"])
    result = results[name]
    passed = all(all(row["checks"].values()) for row in results.values())
    return {
        "result": "PASS" if passed else "FAIL",
        "governing_family": name,
        "governing_case": result["governing_case"],
        "governing_ratio": result["governing_ratio"],
        "checks": {family: dict(row["checks"]) for family, row in results.items()},
    }


__all__ = [
    "AnodeFamily",
    "FamilyDemand",
    "design_anode_family",
    "overall_family_status",
]
