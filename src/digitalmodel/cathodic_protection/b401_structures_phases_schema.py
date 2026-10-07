"""Validation and edition citations for B401 phased structure inputs."""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection._edition import Edition

COMPONENT_TYPES = {"riser_base", "foundation", "mudmat", "hatch_cover"}
PHASE_TYPES = {"installation", "temporary", "wet_storage", "operating"}
RULES: dict[Edition, tuple[str, ...]] = {
    "2005": (
        "Sec. 6.2.1 (active pre-operation time)",
        "Sec. 6.3.8 (buried surfaces)",
        "Sec. 6.3.12-6.3.13 / Table 10-3 (concrete reinforcement)",
        "Sec. 6.9.2 (mudmats, skirts and piles)",
        "Sec. 7.7.1 Eq. (2) (anode mass)",
        "Sec. 7.8.1-7.8.5 / Eq. (3), Eqs. (5)-(7) (output adequacy)",
        "Sec. 7.9.2 (fresh/depleted resistance)",
        "Sec. 7.13.3 (retrofit documentation)",
    ),
    "2010": (
        "Sec. 6.2.1 (active pre-operation time)",
        "Sec. 6.3.8 (buried surfaces)",
        "Sec. 6.3.12-6.3.13 / Table 10-3 (concrete reinforcement)",
        "Sec. 6.9.2 (mudmats, skirts and piles)",
        "Sec. 7.7.1 Eq. (2) (anode mass)",
        "Sec. 7.8.1-7.8.5 / Eq. (3), Eqs. (5)-(7) (output adequacy)",
        "Sec. 7.9.2 (fresh/depleted resistance)",
        "Sec. 7.13.3 (retrofit documentation)",
    ),
    "2017": (
        "[6.2.1] (active pre-operation time)",
        "[6.3.8] (buried surfaces)",
        "[6.3.12]-[6.3.13] / Table A-3 (concrete reinforcement)",
        "[6.9.2] (mudmats, skirts and piles)",
        "[7.7.1] Eq. (2) (anode mass)",
        "[7.8.1]-[7.8.5] / Eq. (3), Eqs. (5)-(7) (output adequacy)",
        "[7.9.2] (fresh/depleted resistance)",
        "[7.13.3] (retrofit documentation)",
    ),
    "2021": (
        "[3.2.1] (active pre-operation time)",
        "[3.3.8] (buried surfaces)",
        "[3.3.12]-[3.3.13] / Table 8-3 (concrete current drain)",
        "[3.9.3] (mudmats, skirts, piles and suction anchors)",
        "[4.7.1] Eq. (4.2) (anode mass)",
        "[4.8.1]-[4.8.5] / Eq. (4.3), Eqs. (4.5)-(4.7) (output adequacy)",
        "[4.9.2] (fresh/depleted resistance)",
        "[7.4.1], [7.5.3] (retrofit evidence)",
    ),
}


def mapping(value: Any, name: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ValueError(f"{name} must be a mapping")
    return dict(value)


def nonempty(value: Any, name: str) -> str:
    text = str(value or "").strip()
    if not text:
        raise ValueError(f"{name} must be non-empty")
    return text


def number(value: Any, name: str, *, positive: bool = False) -> float:
    result = float(value)
    if not math.isfinite(result) or result < 0.0 or (positive and result <= 0.0):
        qualifier = "positive" if positive else "non-negative"
        raise ValueError(f"{name} must be a {qualifier} finite number")
    return result


def basis(raw: Any, name: str) -> dict[str, str]:
    value = mapping(raw, name)
    source = nonempty(value.get("source"), f"{name}.source")
    if source not in {"project_practice", "calculated_b401_result"}:
        raise ValueError(f"{name}.source must identify project practice or a result")
    result = {"source": source, "reason": nonempty(value.get("reason"), f"{name}.reason")}
    if source == "calculated_b401_result":
        result["reference"] = nonempty(value.get("reference"), f"{name}.reference")
        result["citation"] = nonempty(value.get("citation"), f"{name}.citation")
    return result


def project_basis(raw: Any, name: str) -> dict[str, str]:
    result = basis(raw, name)
    if result["source"] != "project_practice":
        raise ValueError(f"{name}.source must be project_practice")
    return result


def _flatten_zones(
    item: Mapping[str, Any], name: str, family_owner: dict[str, str]
) -> tuple[list[dict[str, Any]], set[str]]:
    zones = item.get("zones")
    if not isinstance(zones, list) or not zones:
        raise ValueError(f"component {name!r} zones must be non-empty")
    flat: list[dict[str, Any]] = []
    families: set[str] = set()
    local_names: set[str] = set()
    for raw_zone in zones:
        zone = mapping(raw_zone, f"component {name}.zone")
        local = nonempty(zone.get("zone"), f"component {name}.zone")
        if local in local_names:
            raise ValueError(f"duplicate zone {local!r} in component {name!r}")
        local_names.add(local)
        family = nonempty(
            zone.get("anode_family"), f"zone {name}::{local}.anode_family"
        )
        owner = family_owner.setdefault(family, name)
        if owner != name:
            raise ValueError(
                f"anode family {family!r} must protect exactly one component"
            )
        zone["zone"] = f"{name}::{local}"
        families.add(family)
        flat.append(zone)
    return flat, families


def flatten_components(
    assessment: Mapping[str, Any],
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    raw_components = assessment.get("components")
    if not isinstance(raw_components, list) or not raw_components:
        raise ValueError("inputs.riser_base_assessment.components must be non-empty")
    flat: list[dict[str, Any]] = []
    summaries: dict[str, Any] = {}
    family_owner: dict[str, str] = {}
    for raw in raw_components:
        item = mapping(raw, "component")
        name = nonempty(item.get("name"), "component.name")
        kind = nonempty(item.get("type"), f"component {name}.type")
        if name in summaries or kind not in COMPONENT_TYPES:
            raise ValueError(f"invalid or duplicate component {name!r} type {kind!r}")
        if item.get("composition_basis") != "project_practice":
            raise ValueError(
                f"component {name}.composition_basis must be project_practice"
            )
        nonempty(item.get("composition_reason"), f"component {name}.composition_reason")
        zones, families = _flatten_zones(item, name, family_owner)
        flat.extend(zones)
        summaries[name] = {
            "type": kind,
            "anode_families": sorted(families),
            "composition_basis": "project_practice",
            "composition_reason": item["composition_reason"],
        }
    return flat, summaries


def rule_citations(edition: Edition) -> list[str]:
    return [f"dnv-rp-b401 {edition} {rule}" for rule in RULES[edition]]


__all__ = [
    "PHASE_TYPES",
    "basis",
    "flatten_components",
    "mapping",
    "nonempty",
    "number",
    "project_basis",
    "rule_citations",
]
