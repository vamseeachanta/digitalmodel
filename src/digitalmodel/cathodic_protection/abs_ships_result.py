"""Result-schema builder for the ABS ship-hull calculation."""

from __future__ import annotations

from typing import Any, Mapping

from digitalmodel.cathodic_protection.abs_ships_tables import citation_label
from digitalmodel.citations import CitedValue

_LIMITATIONS = [
    "Project bare-steel current densities are not constrained to Table 3 ranges.",
    "Initial/maximum coating percentages and their arithmetic mean are project choices; Table 4 gives no time law.",
    "The depleted core area and long-flush resistance interpretation require engineering review.",
    "Special stern, propeller and opening geometry checks remain not evaluated unless separately supplied.",
    "The ABS-specific generic-wiki citation target is not present.",
]


def _requirements(
    demand: Mapping[str, Any],
    mass: float,
    net_mass: float,
    counts: Mapping[str, int],
    required: int,
    selected: int,
) -> dict[str, Any]:
    return {
        "mean_current_A": demand["totals"]["mean"],
        "total_mass_kg": mass,
        "individual_mass_kg": net_mass,
        "mass_count": counts["mass"],
        "initial_output_count": counts["initial_current_output"],
        "mean_output_count": counts["mean_current_output"],
        "final_output_count": counts["final_current_output"],
        "recommended_anode_count": required,
        "selected_anode_count": selected,
        "anode_count": selected,
    }


def _performance(
    outputs: Mapping[str, float],
    initial_r: float,
    final_r: float,
    depleted: dict[str, float] | None,
    selected: int,
    delta_e: float,
    checks: Mapping[str, bool],
) -> dict[str, Any]:
    return {
        "driving_voltage_V": delta_e,
        "resistance_ohm": {"initial": initial_r, "final": final_r},
        "current_output_A": {
            "initial_per_anode": outputs["initial"],
            "final_per_anode": outputs["final"],
            "initial_total": selected * outputs["initial"],
            "final_total": selected * outputs["final"],
        },
        "depleted_geometry": depleted,
        "checks": dict(checks),
    }


def _status(governing: str, checks: Mapping[str, bool]) -> dict[str, Any]:
    return {
        "result": "PASS" if all(checks.values()) else "FAIL",
        "governing_case": governing,
        "reason": (
            "general Section 3/5.2 distribution and bilge-spacing scope checked; "
            "special stern, propeller and opening geometry remains not evaluated"
        ),
        "checks": dict(checks),
        "use_status": "cited-pending-review",
    }


def build_result(
    demand: Mapping[str, Any],
    coating: Mapping[str, float],
    life: float,
    capacity: CitedValue,
    mass: float,
    net_mass: float,
    outputs: Mapping[str, float],
    initial_r: float,
    final_r: float,
    depleted: dict[str, float] | None,
    layout: Mapping[str, Any],
    counts: Mapping[str, int],
    required: int,
    selected: int,
    governing: str,
    checks: Mapping[str, bool],
    delta_e: float,
    cited: list[CitedValue],
) -> dict[str, Any]:
    """Build the stable adapter/report result contract."""
    return {
        "standard": "ABS GN Ships (2017-12)",
        "edition": "2017-12",
        "provenance": "guide table/equation locators; wiki target pending",
        "design_life": life,
        "anode_current_capacity": capacity.value,
        "coating_breakdown_factors": {
            **coating,
            "interpretation": (
                "explicit project initial/maximum percentages converted to fractions; "
                "project mean is their arithmetic mean"
            ),
        },
        "current_densities_mA_m2": demand["densities_mA_m2"],
        "current_demand_A": demand,
        "anode_requirements": _requirements(
            demand, mass, net_mass, counts, required, selected
        ),
        "anode_performance": _performance(
            outputs, initial_r, final_r, depleted, selected, delta_e, checks
        ),
        "layout": dict(layout),
        "citations": list(dict.fromkeys(citation_label(value) for value in cited)),
        "citation_resolution": "PENDING: ABS generic-wiki target is absent",
        "limitations": _LIMITATIONS,
        "status": _status(governing, checks),
    }


__all__ = ["build_result"]
