"""Allocation parsing and checks for multi-component B401 designs."""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.b401_anode_families import AnodeFamily
from digitalmodel.cathodic_protection.b401_family_route import _family

FRACTION_TOLERANCE = 1.0e-9
CASES = ("mass", "initial", "final")


def named_rows(raw: Any, label: str) -> dict[str, dict[str, Any]]:
    """Validate a non-empty list of uniquely named mapping rows."""
    if not isinstance(raw, list) or not raw:
        raise ValueError(f"inputs.{label} must be a non-empty list")
    rows: dict[str, dict[str, Any]] = {}
    for item in raw:
        if not isinstance(item, Mapping):
            raise ValueError(f"inputs.{label} entries must be mappings")
        name = str(item.get("name") or "").strip()
        if not name or name in rows:
            raise ValueError(f"duplicate or empty {label} name {name!r}")
        rows[name] = dict(item)
    return rows


def parse_allocations(
    raw: Any, loads: Mapping[str, Mapping[str, Any]]
) -> dict[str, dict[str, float]]:
    """Require reciprocal target mappings and normalized family fractions."""
    allocations: dict[str, dict[str, float]] = {}
    seen: set[tuple[str, str]] = set()
    if not isinstance(raw, list):
        raise ValueError("inputs.allocations must be a list of mappings")
    for row in raw:
        if not isinstance(row, Mapping):
            raise ValueError("inputs.allocations entries must be mappings")
        family, component = str(row.get("anode_family")), str(row.get("component"))
        key = (family, component)
        if key in seen:
            raise ValueError("duplicate reciprocal allocation")
        seen.add(key)
        fraction = float(row.get("capacity_fraction", 1.0))
        if not math.isfinite(fraction) or fraction <= 0.0:
            raise ValueError("capacity_fraction must be positive and finite")
        allocations.setdefault(family, {})[component] = fraction
    if set(allocations) != set(loads):
        raise ValueError("reciprocal allocation families do not match zone families")
    for family, targets in loads.items():
        if set(allocations[family]) != set(targets):
            raise ValueError(
                f"reciprocal allocation targets do not match family {family!r}"
            )
        if not math.isclose(
            sum(allocations[family].values()),
            1.0,
            abs_tol=FRACTION_TOLERANCE,
            rel_tol=0.0,
        ):
            raise ValueError(f"capacity_fraction values for {family!r} must sum to 1")
    return allocations


def parse_families(
    inputs: Mapping[str, Any],
    components: Mapping[str, Mapping[str, Any]],
    loads: Mapping[str, Any],
) -> tuple[dict[str, AnodeFamily], dict[str, str]]:
    """Build families from host-component electrolyte properties."""
    raw_families = named_rows(inputs.get("anode_families"), "anode_families")
    if set(raw_families) != set(loads):
        raise ValueError("unused or missing anode families")
    parsed: dict[str, AnodeFamily] = {}
    hosts: dict[str, str] = {}
    for name, raw in raw_families.items():
        host = str(raw.get("installed_on_component"))
        if host not in components:
            raise ValueError(f"unknown host component {host!r}")
        forbidden_fields = (
            "environment",
            "electrolyte_resistivity_ohm_m",
            "anode_surface_temperature_C",
        )
        for forbidden in forbidden_fields:
            if forbidden in raw:
                raise ValueError(f"component mode family must not set {forbidden}")
        environment = components[host]["environment"]
        enriched = {
            **raw,
            "environment": environment["anode_environment"],
            "electrolyte_resistivity_ohm_m": environment[
                "electrolyte_resistivity_ohm_m"
            ],
            "anode_surface_temperature_C": environment["anode_surface_temperature_C"],
        }
        parsed[name] = _family(enriched)
        hosts[name] = host
    return parsed, hosts


def allocation_check(
    family: Mapping[str, Any], load: Mapping[str, float], fraction: float
) -> dict[str, Any]:
    """Check one protected component against its stated family capacity share."""
    count = int(family["anode_count"])
    unit_mass = float(family["individual_mass_kg"])
    required_mass = kernel.anode_mass_from_current_years(
        load["current_years"],
        float(family["capacity_Ah_kg"]),
        float(family["utilization_factor"]),
    )
    capacities = {
        "mass": fraction * count * unit_mass,
        "initial": fraction * count * float(family["current_output_initial_A"]),
        "final": fraction * count * float(family["current_output_final_A"]),
    }
    required = {
        "mass": required_mass,
        "initial": load["initial"],
        "final": load["final"],
    }
    ratios = {case: required[case] / capacities[case] for case in CASES}
    governing = max(CASES, key=lambda case: (ratios[case], -CASES.index(case)))
    return {
        "capacity_fraction": fraction,
        "required_mass_kg": required_mass,
        "required_anode_count": max(
            kernel.anode_count(required_mass, unit_mass),
            kernel.anodes_for_current(
                load["initial"], float(family["current_output_initial_A"])
            ),
            kernel.anodes_for_current(
                load["final"], float(family["current_output_final_A"])
            ),
        ),
        "allocated_mass_kg": capacities["mass"],
        "allocated_initial_output_A": capacities["initial"],
        "allocated_final_output_A": capacities["final"],
        "adequacy_ratios": ratios,
        "checks": {case: ratio <= 1.0 for case, ratio in ratios.items()},
        "governing_case": governing,
        "governing_ratio": ratios[governing],
    }


def reconcile_allocations(
    allocations: Mapping[str, Mapping[str, float]],
    components: Mapping[str, Mapping[str, Any]],
    families: Mapping[str, Mapping[str, Any]],
) -> dict[str, Any]:
    """Reconcile each allocated capacity basis to its physical family total."""
    fields = {
        "mass_kg": "allocated_mass_kg",
        "initial_output_A": "allocated_initial_output_A",
        "final_output_A": "allocated_final_output_A",
    }
    result: dict[str, Any] = {
        "allocated_fraction_by_family": {
            name: sum(targets.values()) for name, targets in allocations.items()
        }
    }
    for label, field in fields.items():
        allocated = {
            family: sum(
                float(components[target]["allocation_checks"][family][field])
                for target in targets
            )
            for family, targets in allocations.items()
        }
        if label == "mass_kg":
            physical = {
                name: int(row["anode_count"]) * float(row["individual_mass_kg"])
                for name, row in families.items()
            }
        else:
            output_field = f"current_output_{label.removesuffix('_output_A')}_A"
            physical = {
                name: int(row["anode_count"]) * float(row[output_field])
                for name, row in families.items()
            }
        result[f"allocated_{label}_by_family"] = allocated
        result[f"physical_{label}_by_family"] = physical
        result[f"{label}_checks"] = {
            name: math.isclose(
                allocated[name], physical[name], rel_tol=0.0, abs_tol=1e-9
            )
            for name in families
        }
    result["hosted_anode_count"] = sum(
        int(row["anode_count"]) for row in families.values()
    )
    return result


__all__ = [
    "allocation_check",
    "named_rows",
    "parse_allocations",
    "parse_families",
    "reconcile_allocations",
]
