"""Engine-adapter implementation for B401 designs with named anode families."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.anode_sizing import AnodeType
from digitalmodel.cathodic_protection.b401_anode_families import (
    AnodeFamily,
    FamilyDemand,
    design_anode_family,
    overall_family_status,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    citation_label,
    edition_provenance,
)

_MATERIAL = {
    "aluminium": AnodeMaterial.ALUMINIUM,
    "aluminum": AnodeMaterial.ALUMINIUM,
    "al-based": AnodeMaterial.ALUMINIUM,
    "zinc": AnodeMaterial.ZINC,
    "zn-based": AnodeMaterial.ZINC,
}
_ENVIRONMENT = {
    "seawater": AnodeEnvironment.SEAWATER,
    "sediment": AnodeEnvironment.SEDIMENT,
    "sediments": AnodeEnvironment.SEDIMENT,
    "buried": AnodeEnvironment.SEDIMENT,
}
_ANODE_TYPE = {
    "stand_off": AnodeType.STAND_OFF,
    "flush_mounted": AnodeType.FLUSH_MOUNT,
    "flush_mount": AnodeType.FLUSH_MOUNT,
    "bracelet": AnodeType.BRACELET,
}


def _token(table: Mapping[str, Any], value: Any, label: str) -> Any:
    token = str(value).strip().lower()
    if token not in table:
        raise ValueError(f"unknown {label} {value!r}; accepted: {sorted(table)}")
    return table[token]


def _family(raw: Mapping[str, Any]) -> AnodeFamily:
    type_name = str(raw.get("type", "stand_off"))
    anode_type = _token(_ANODE_TYPE, type_name, "anode family type")
    fields = {
        "name": raw.get("name"),
        "material": _token(_MATERIAL, raw.get("material", "aluminium"), "material"),
        "environment": _token(
            _ENVIRONMENT, raw.get("environment", "seawater"), "environment"
        ),
        "anode_type": anode_type,
        "individual_mass_kg": raw.get("individual_anode_mass_kg"),
        "length_m": raw.get("length_m"),
        "electrolyte_resistivity_ohm_m": raw.get("electrolyte_resistivity_ohm_m"),
        "installed_count": raw.get("count"),
        "anode_surface_temperature_c": raw.get("anode_surface_temperature_C", 30.0),
    }
    aliases = {
        "utilization_factor": "utilization_factor",
        "density_kg_m3": "density_kg_m3",
        "radius_m": "radius_m",
        "width_m": "width_m",
        "thickness_m": "thickness_m",
        "exposed_area_m2": "exposed_area_m2",
        "final_length_m": "final_length_m",
        "final_width_m": "final_width_m",
        "final_thickness_m": "final_thickness_m",
        "final_exposed_area_m2": "final_exposed_area_m2",
    }
    fields.update(
        {dest: raw[src] for src, dest in aliases.items() if raw.get(src) is not None}
    )
    return AnodeFamily.model_validate(fields)


def _families(raw: Any) -> dict[str, AnodeFamily]:
    if not isinstance(raw, list) or not raw:
        raise ValueError("inputs.anode_families must be a non-empty list")
    families = [_family(item) for item in raw]
    names = [family.name for family in families]
    if len(names) != len(set(names)):
        raise ValueError("duplicate anode family name")
    return {family.name: family for family in families}


def _zone_rows(
    structure: Mapping[str, Any], design_life: float, temp_c: float, edition: Edition
) -> tuple[
    list[Any],
    dict[str, Any],
    dict[str, Any],
    dict[str, Any],
    dict[str, float],
    list[str],
]:
    from digitalmodel.cathodic_protection.engine_adapter import (
        _b401_breakdown,
        _b401_densities,
        _b401_zones,
        _cite,
    )

    zones = _b401_zones(structure)
    breakdown: dict[str, Any] = {}
    densities: dict[str, Any] = {}
    demand: dict[str, Any] = {}
    areas: dict[str, float] = {}
    citations: list[str] = []
    for zone_id, base_zone, category, zone in zones:
        fc, fc_cited = _b401_breakdown(category, zone, design_life, edition)
        dens, dens_cited = _b401_densities(zone, base_zone, temp_c, edition)
        fc["citations"] = sorted(citation_label(cv.citation) for cv in fc_cited)
        dens["citations"] = sorted({citation_label(cv.citation) for cv in dens_cited})
        _cite(citations, *fc_cited, *dens_cited)
        area = zone.design_area_m2
        values = {
            "initial": kernel.current_demand(area, dens["i_initial_A_m2"], fc["f_ci"]),
            "mean": kernel.current_demand(area, dens["i_mean_A_m2"], fc["f_cm"]),
            "final": kernel.current_demand(area, dens["i_final_A_m2"], fc["f_cf"]),
        }
        demand[zone_id] = {
            "area_m2": area,
            "area_basis": zone.area_basis,
            "anode_family": zone.anode_family,
            **{f"I_{key}_A": value for key, value in values.items()},
            **{f"i_{key}_A_m2": dens[f"i_{key}_A_m2"] for key in values},
            **{f"f_c{key[0]}": fc[f"f_c{key[0]}"] for key in values},
        }
        areas[zone_id] = area
        breakdown[zone_id] = fc
        densities[zone_id] = dens
    return zones, breakdown, densities, demand, areas, citations


def _family_demands(
    zones: list[Any],
    demand: Mapping[str, Mapping[str, Any]],
    families: Mapping[str, Any],
) -> dict[str, FamilyDemand]:
    totals = {
        name: {case: 0.0 for case in ("initial", "mean", "final")} for name in families
    }
    referenced: set[str] = set()
    for _, _, _, zone in zones:
        row = demand[zone.zone_name]
        active = any(
            float(row[f"I_{case}_A"]) > 0.0 for case in totals[next(iter(totals))]
        )
        if not active:
            if zone.anode_family is not None:
                raise ValueError(
                    f"zero-demand zone {zone.zone_name!r} cannot name an anode family"
                )
            continue
        if zone.anode_family not in families:
            raise ValueError(
                f"missing anode family {zone.anode_family!r} for zone {zone.zone_name!r}"
            )
        referenced.add(zone.anode_family)
        for case in totals[zone.anode_family]:
            totals[zone.anode_family][case] += float(row[f"I_{case}_A"])
    unused = set(families) - referenced
    if unused:
        raise ValueError(f"unreferenced anode families: {sorted(unused)}")
    return {
        name: FamilyDemand.model_validate(values) for name, values in totals.items()
    }


def _result_payload(
    edition: Edition,
    design_life: float,
    areas: dict[str, float],
    breakdown: dict[str, Any],
    densities: dict[str, Any],
    demand: dict[str, Any],
    families: dict[str, dict[str, Any]],
    citations: list[str],
    status: dict[str, Any],
) -> dict[str, Any]:
    return {
        "standard": standard_for_edition(edition),
        "edition": edition,
        "provenance": edition_provenance(edition),
        "design_life_years": design_life,
        "surface_areas_m2": areas,
        "coating_breakdown": breakdown,
        "current_densities_A_m2": densities,
        "current_demand_A": demand,
        "anode_families": families,
        "anode_requirements": {
            "total_mass_kg": sum(row["required_mass_kg"] for row in families.values()),
            "recommended_anode_count": sum(
                row["recommended_anode_count"] for row in families.values()
            ),
        },
        "current_output_verification": {"families": families, **status},
        "citations": sorted(set(citations)),
        "status": status,
    }


def _record_status(
    cfg: dict[str, Any], results: dict[str, Any], status: dict[str, Any]
) -> None:
    from digitalmodel.cathodic_protection.engine_adapter import _set_status

    governing = f"{status['governing_family']}:{status['governing_case']}"
    _set_status(
        cfg,
        results,
        status["result"] == "PASS",
        governing,
        status["reason"],
        status["checks"],
    )
    result_status = results["status"]
    assert isinstance(result_status, dict)
    result_status.update(
        governing_family=status["governing_family"],
        governing_case=status["governing_case"],
        governing_ratio=status["governing_ratio"],
    )


def run_b401_families(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the B401 route with independently sized named anode families."""
    inputs = cfg["inputs"]
    design_data = inputs.get("design_data", {})
    environment = inputs.get("environment", {})
    edition = normalize_edition(str(design_data.get("edition", DEFAULT_EDITION)))
    design_life = float(design_data.get("design_life", 25.0))
    temp_c = float(environment.get("seawater_temperature_C", 10.0))
    families = _families(inputs.get("anode_families"))
    rows = _zone_rows(inputs.get("structure", {}), design_life, temp_c, edition)
    zones, breakdown, densities, demand, areas, citations = rows
    assigned = _family_demands(zones, demand, families)
    family_results = {
        name: design_anode_family(family, assigned[name], design_life, edition)
        for name, family in families.items()
    }
    status = overall_family_status(family_results)
    status["use_status"] = "client-use-with-eor-check"
    status["reason"] = (
        f"{status['governing_family']}:{status['governing_case']} governs at "
        f"adequacy ratio {status['governing_ratio']:.3f}"
    )
    for row in family_results.values():
        citations.extend(row["citations"])
    zone_demand_rows = list(demand.values())
    for case in ("initial", "mean", "final"):
        demand[f"total_{case}_A"] = sum(
            float(row[f"I_{case}_A"]) for row in zone_demand_rows
        )
    areas["total_m2"] = sum(areas.values())
    results = _result_payload(
        edition,
        design_life,
        areas,
        breakdown,
        densities,
        demand,
        family_results,
        citations,
        status,
    )
    _record_status(cfg, results, status)
    cfg["results"] = results
    return cfg


__all__ = ["run_b401_families"]
