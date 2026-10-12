"""Component-zone demand and F103 coating handling for B401 riser designs."""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import Edition
from digitalmodel.cathodic_protection.b401_tables import citation_label
from digitalmodel.cathodic_protection.f103_tables import (
    FieldJointCoating,
    LinepipeCoating,
    field_joint_coating_constants,
    field_joint_coating_row,
    linepipe_coating_constants,
    linepipe_coating_row,
    normalize_f103_edition,
    resolve_field_joint_coating_2019,
)

DISPOSITIONS = {"designed", "no_cp_demand", "excluded_self_protected", "not_evaluated"}


def positive(value: Any, name: str) -> float:
    """Return a positive finite input or fail closed."""
    number = float(value)
    if not math.isfinite(number) or number <= 0.0:
        raise ValueError(f"{name} must be a positive finite number")
    return number


def _f103_source(
    coating: Mapping[str, Any],
) -> tuple[Any, Any, Any, dict[str, Any], bool]:
    edition = normalize_f103_edition(str(coating.get("edition")))
    basis = str(coating.get("basis"))
    if basis == "f103_linepipe":
        linepipe_system = LinepipeCoating(str(coating.get("system")))
        concrete_raw = coating.get("concrete_weight_coating", False)
        if not isinstance(concrete_raw, bool):
            raise ValueError("concrete_weight_coating must be Boolean")
        concrete = concrete_raw
        a_cv, b_cv = linepipe_coating_constants(linepipe_system, edition, concrete)
        linepipe_row = linepipe_coating_row(linepipe_system, edition, concrete)
        if linepipe_row.concrete_weight_coating is not concrete:
            raise ValueError(
                "concrete_weight_coating does not match the cited F103 linepipe row"
            )
        return (
            a_cv,
            b_cv,
            linepipe_row,
            {"concrete_weight_coating": linepipe_row.concrete_weight_coating},
            True,
        )
    if basis != "f103_field_joint":
        raise ValueError(f"unknown coating basis {basis!r}")
    system_text = str(coating.get("system"))
    field_system = (
        resolve_field_joint_coating_2019(system_text, coating.get("infill"))
        if edition == "2019"
        else FieldJointCoating(system_text)
    )
    a_cv, b_cv = field_joint_coating_constants(field_system, edition)
    _, field_row = field_joint_coating_row(field_system, edition)
    extra = {
        "infill": field_row.infill,
        "compatible_linepipe": field_row.compatible_linepipe,
    }
    return a_cv, b_cv, field_row, extra, False


def f103_breakdown(
    coating: Mapping[str, Any], life: float
) -> tuple[dict[str, Any], list[str], bool]:
    """Resolve cited F103 breakdown factors and fail closed on applicability."""
    edition = normalize_f103_edition(str(coating.get("edition")))
    operating = float(coating["operating_temperature_C"])
    if not math.isfinite(operating):
        raise ValueError("F103 operating_temperature_C must be finite")
    a_cv, b_cv, row, extra, known = _f103_source(coating)
    maximum = row.max_temperature_c
    if maximum is not None and operating > maximum:
        raise ValueError("F103 coating operating temperature exceeds tabulated maximum")
    a, b = a_cv.value, b_cv.value
    citations = sorted({citation_label(cv.citation) for cv in (a_cv, b_cv)})
    result = {
        "basis": str(coating.get("basis")),
        "edition": edition,
        "system": str(coating.get("system")),
        "operating_temperature_C": operating,
        "max_temperature_C": maximum,
        "applicability": "PASS" if maximum is not None and known else "NOT_EVALUATED",
        "a": a,
        "b_per_yr": b,
        "f_ci": kernel.coating_breakdown_linear(a, b, 0.0),
        "f_cm": kernel.coating_breakdown_mean(a, b, life),
        "f_cf": kernel.coating_breakdown_final(a, b, life),
        "citations": citations,
        **extra,
    }
    return result, citations, maximum is None or not known


def _excluded_zone(
    raw: Mapping[str, Any], component: str, zone: str
) -> tuple[dict[str, Any], bool]:
    disposition = str(raw.get("disposition"))
    if raw.get("anode_family") or not str(raw.get("rationale") or "").strip():
        raise ValueError(f"{disposition} zone requires rationale and no family")
    area = positive(
        raw.get("area_m2", raw.get("reinforcement_area_m2")),
        f"component {component} zone {zone} area",
    )
    return {
        "disposition": disposition,
        "area_m2": area,
        "rationale": raw["rationale"],
    }, disposition == "not_evaluated"


def _zone_basis(
    raw: dict[str, Any], component: Mapping[str, Any], edition: Edition
) -> tuple[Any, dict[str, Any], dict[str, Any], dict[str, float], list[str], bool]:
    from digitalmodel.cathodic_protection.engine_adapter import (
        _b401_breakdown,
        _b401_densities,
        _b401_zones,
    )

    life = float(component["design_life_years"])
    environment = component["environment"]
    _, base_zone, category, zone = _b401_zones(
        {"zones": [{**raw, "zone": raw["name"]}]}
    )[0]
    density, density_cited = _b401_densities(
        zone, base_zone, float(environment["seawater_temperature_C"]), edition
    )
    density["citations"] = sorted({citation_label(cv.citation) for cv in density_cited})
    if isinstance(raw.get("coating"), Mapping):
        breakdown, citations, unknown = f103_breakdown(raw["coating"], life)
    else:
        breakdown, coating_cited = _b401_breakdown(category, zone, life, edition)
        breakdown["citations"] = sorted(
            {citation_label(cv.citation) for cv in coating_cited}
        )
        citations, unknown = breakdown["citations"], False
    values = {
        case: kernel.current_demand(
            zone.design_area_m2,
            density[f"i_{case}_A_m2"],
            breakdown[
                f"f_c{'i' if case == 'initial' else 'm' if case == 'mean' else 'f'}"
            ],
        )
        for case in ("initial", "mean", "final")
    }
    return zone, density, breakdown, values, citations, unknown


def _designed_zone(
    raw: dict[str, Any], component: Mapping[str, Any], edition: Edition
) -> tuple[dict[str, Any], str | None, dict[str, float], list[str], bool]:
    coating = isinstance(raw.get("coating"), Mapping)
    if coating and "coating_category" in raw:
        raise ValueError("coating and coating_category are mutually exclusive")
    if coating and component.get("type") != "riser_pipe_joints":
        raise ValueError("F103 coatings are limited to riser pipe joints")
    if coating and raw.get("base_zone") != "submerged":
        raise ValueError("F103 coatings require a submerged linepipe zone")
    zone, density, breakdown, values, citations, unknown = _zone_basis(
        raw, component, edition
    )
    family = raw.get("anode_family")
    if raw.get("disposition") == "no_cp_demand" and (family or any(values.values())):
        raise ValueError("no_cp_demand zone must have cited zero demand and no family")
    if raw.get("disposition") == "designed" and (not family or values["mean"] <= 0.0):
        raise ValueError("designed zone requires positive demand and anode_family")
    out = {
        "disposition": raw["disposition"],
        "area_m2": zone.design_area_m2,
        "area_basis": zone.area_basis,
        "anode_family": family,
        "coating_breakdown": breakdown,
        "current_densities_A_m2": density,
        **{f"I_{case}_A": value for case, value in values.items()},
    }
    return (
        out,
        str(family) if family else None,
        values,
        [*citations, *density["citations"]],
        unknown,
    )


def _add_load(
    loads: dict[str, dict[str, float]],
    family: str | None,
    values: Mapping[str, float],
    life: float,
) -> None:
    if not family:
        return
    load = loads.setdefault(
        family, {"initial": 0.0, "mean": 0.0, "final": 0.0, "current_years": 0.0}
    )
    for case, value in values.items():
        load[case] += value
    load["current_years"] += values["mean"] * life


def _component_result(
    name: str,
    component: Mapping[str, Any],
    zones: dict[str, Any],
    totals: dict[str, float],
    coverage: bool,
    dispositions: list[str],
) -> dict[str, Any]:
    return {
        "name": name,
        "type": component.get("type", "unspecified"),
        "design_life_years": component["design_life_years"],
        "environment": component["environment"],
        "zones": zones,
        "current_demand_A": totals,
        "allocation_checks": {},
        "hosted_anode_families": [],
        "hosted_anode_count": 0,
        "coverage": "NOT_EVALUATED" if coverage else "PASS",
        "zone_dispositions": sorted(set(dispositions)),
    }


def _component_zones(
    name: str, component: dict[str, Any], edition: Edition
) -> tuple[dict[str, Any], dict[str, dict[str, float]], list[str], bool]:
    raw_zones = component.get("zones")
    if not isinstance(raw_zones, list) or not raw_zones:
        raise ValueError(f"component {name} zones must not be empty")
    zones: dict[str, Any] = {}
    loads: dict[str, dict[str, float]] = {}
    citations: list[str] = []
    totals = {case: 0.0 for case in ("initial", "mean", "final")}
    seen: set[str] = set()
    coverage = False
    dispositions = []
    for item in raw_zones:
        raw = dict(item)
        zone_name = str(raw.get("name") or "")
        disposition = str(raw.get("disposition", "designed"))
        raw["disposition"] = disposition
        if not zone_name or zone_name in seen:
            raise ValueError(f"duplicate or empty zone name in component {name}")
        if disposition not in DISPOSITIONS:
            raise ValueError(f"unknown CP disposition {disposition!r}")
        seen.add(zone_name)
        dispositions.append(disposition)
        if disposition in {"excluded_self_protected", "not_evaluated"}:
            zones[zone_name], failed = _excluded_zone(raw, name, zone_name)
            coverage |= failed
            continue
        zone, family, values, cited, unknown = _designed_zone(raw, component, edition)
        zones[zone_name] = zone
        citations.extend(cited)
        coverage |= unknown
        for case, value in values.items():
            totals[case] += value
        _add_load(loads, family, values, float(component["design_life_years"]))
    result = _component_result(name, component, zones, totals, coverage, dispositions)
    return result, loads, citations, coverage


def zone_calculations(
    components: Mapping[str, dict[str, Any]], edition: Edition
) -> tuple[dict[str, Any], dict[str, dict[str, dict[str, float]]], list[str], bool]:
    """Calculate every component zone and aggregate loads by family and target."""
    results: dict[str, Any] = {}
    loads: dict[str, dict[str, dict[str, float]]] = {}
    citations: list[str] = []
    coverage = False
    for name, component in components.items():
        result, local_loads, cited, failed = _component_zones(name, component, edition)
        results[name] = result
        citations.extend(cited)
        coverage |= failed
        for family, load in local_loads.items():
            loads.setdefault(family, {})[name] = load
    return results, loads, sorted(set(citations)), coverage


__all__ = ["positive", "zone_calculations"]
