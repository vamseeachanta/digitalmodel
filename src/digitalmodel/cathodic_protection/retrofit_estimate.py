# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Published-jacket retrofit estimate using the existing B401 anode checks.

Source: Xu (2023), Study on Implementation and Optimization of Cathodic
Protection for Offshore Oil Platform Jacket, Engineering Advances 3(3),
241–245, DOI 10.26855/ea.2023.06.017. Areas and design densities are from
p. 242, Tables 1–2; subtropical densities from p. 244, Table 3; placement
from p. 244, Section 5. The source does not size the proposed clamped anodes.
No standard document is required or read by this module.
"""

from __future__ import annotations

import copy
import csv
import io
import math
import re
from collections.abc import Mapping
from typing import Any, Literal

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    calculate_anode_resistance,
    governing_case,
)
from digitalmodel.cathodic_protection.engine_adapter import (
    _b401_anode_geometry,
    run_cathodic_protection,
)

SOURCE = (
    "Xu (2023), Study on Implementation and Optimization of Cathodic Protection "
    "for Offshore Oil Platform Jacket, Engineering Advances 3(3), 241–245; "
    "p. 242 Tables 1–2 and Section 2; p. 244 Table 3 and Section 5; "
    "doi:10.26855/ea.2023.06.017"
)
PHASES = ("initial", "mean", "final")
# Xu (2023), p. 244 Table 3: printed initial / maintain / late, mA/m2.
TABLE_3_ROWS = {
    "upper-bare-seawater": (170.0, 80.0, 110.0),
    "lower-bare-seawater": (140.0, 70.0, 90.0),
}
# Source p. 242, Tables 1–2: area (m2), base zone and initial/mean/final (A/m2).
PUBLISHED_ZONE_BASIS = {
    "coated-seawater": (652.9, "submerged", 0.01, 0.02, 0.04),
    "upper-bare-seawater": (3726.8, "submerged", 0.15, 0.07, 0.10),
    "lower-bare-seawater": (15737.0, "submerged", 0.12, 0.06, 0.08),
    "buried-steel": (10814.8, "buried", 0.02, 0.02, 0.02),
}
# Xu (2023), p. 244 Section 5: both proposed levels lie in the lower zone.
DEEP_ZONE = "lower-bare-seawater"
DEEP_LEVELS_M = (70.5, 84.1)
METRIC_PREFIX = 1000.0  # SI conversions: mA/A and mm/m; not an empirical input.
PhaseOrder = Literal["initial_mean_final", "initial_final_mean"]


def _require_fixed_fields(
    values: Any, required: Mapping[str, Any], *,
    optional: Mapping[str, Any] | None = None, metadata: tuple[str, ...] = (),
) -> None:
    """Reject missing, changed or unsupported calculation inputs before coercion."""
    message = "The estimate inputs differ from the fixed published case or declared assumed basis"
    optional = optional or {}
    if not isinstance(values, Mapping):
        raise ValueError(message)
    if required.keys() - values.keys() or values.keys() - (required.keys() | optional.keys() | set(metadata)):
        raise ValueError(message)
    for key, expected in {**required, **optional}.items():
        if key not in values:
            continue  # Only optional fields can be absent after the required-key check.
        actual = values[key]
        if isinstance(expected, Mapping):
            _require_fixed_fields(actual, expected)
        elif isinstance(expected, (int, float)):
            if isinstance(actual, bool) or not isinstance(actual, (int, float)):
                raise ValueError(message)
            if actual != expected or not math.isfinite(actual):
                raise ValueError(message)
        elif actual != expected:
            raise ValueError(message)


def _validate_fixed_case(inputs: Any) -> None:
    """Bind source Tables 1–2 and Section 2 to the declared calculation assumptions."""
    _require_fixed_fields(
        inputs, {"calculation_type": "DNV_RP_B401_offshore"},
        metadata=("design_data", "environment", "anode", "structure"),
    )
    _require_fixed_fields(inputs.get("design_data"), {
        "edition": "2021", "design_life": 25.0, "structure_type": "jacket",
    })
    _require_fixed_fields(inputs.get("environment"), {
        "seawater_temperature_C": 10.0, "seawater_resistivity_ohm_m": 0.22,
    })
    _require_fixed_fields(inputs.get("anode"), {
        "material": "aluminium", "type": "stand_off", "length_m": 2.020,
        "individual_anode_mass_kg": 323.4, "count": 569,
    }, optional={"utilization_factor": 0.90, "density_kg_m3": 2750.0}, metadata=("radius_m",))
    structure = inputs.get("structure")
    _require_fixed_fields(structure, {}, metadata=(
        "zones", "connected_current_allowances", "verification_evidence", "report_geometry",
    ))
    _validate_fixed_zones(structure.get("zones"))
    allowances = structure.get("connected_current_allowances")
    if not isinstance(allowances, list) or len(allowances) != 1:
        raise ValueError("The estimate requires the fixed published case connected allowance")
    _require_fixed_fields(
        allowances[0], {"count": 9, "current_A_each": 2.0}, metadata=("source", "phase_basis"),
    )


def _validate_fixed_zones(zones: Any) -> None:
    """Reject changed or duplicate published areas and effective phase densities."""
    if not isinstance(zones, list) or len(zones) != len(PUBLISHED_ZONE_BASIS):
        raise ValueError("The estimate requires exactly the four fixed published case zones")
    seen: set[str] = set()
    for zone in zones:
        name = zone.get("zone") if isinstance(zone, Mapping) else None
        if not isinstance(name, str) or name not in PUBLISHED_ZONE_BASIS or name in seen:
            raise ValueError("The estimate requires unique fixed published case zones")
        seen.add(name)
        area, base, initial, mean, final = PUBLISHED_ZONE_BASIS[name]
        _require_fixed_fields(zone, {
            "zone": name, "base_zone": base, "area_m2": area, "coating_category": "bare",
            "effective_current_density_A_m2": {"initial": initial, "mean": mean, "final": final},
        }, optional={"depth_m": 0.0}, metadata=("area_source", "density_source"))


def _anode_checks(
    current: Mapping[str, float], requirements: Mapping[str, Any],
    design_life: float, outputs: Mapping[str, float],
) -> dict[str, Any]:
    """Size the added-anode estimate without rounding demand or output."""
    mass = kernel.anode_mass(
        current["mean"], design_life,
        requirements["electrochemical_capacity_Ah_kg"],
        requirements["utilization_factor"],
    )
    n_mass = kernel.anode_count(mass, requirements["individual_mass_kg"])
    n_initial = kernel.anodes_for_current(current["initial"], outputs["initial"])
    n_final = kernel.anodes_for_current(current["final"], outputs["final"])
    required = max(n_mass, n_initial, n_final)
    return {
        "net_mass_kg_estimate": mass,
        "count_by_mass_estimate": n_mass,
        "count_by_initial_current_estimate": n_initial,
        "count_by_final_current_estimate": n_final,
        "additional_anodes_estimate": required,
        "governing_check_estimate": (
            governing_case(n_mass, n_initial, n_final) if required else "none"
        ),
    }


def _section_basis(cfg: Mapping[str, Any]) -> dict[str, Any]:
    """Use the published section perimeter, keeping the supplied inputs intact.

    Xu (2023), p. 242 Section 2 gives the section dimensions. Interpreting
    that section as a symmetric trapezoid is an assumption. Radius c/(2*pi)
    uses the existing B401 Table 10-7 note 2 kernel; mass remains independent.
    """
    working = copy.deepcopy(dict(cfg))
    inputs = working["inputs"]
    evidence = inputs["structure"].get("verification_evidence")
    dimensions = evidence.get("published_anode_dimensions") if isinstance(evidence, Mapping) else None
    if not isinstance(dimensions, Mapping):
        raise ValueError("The estimate requires the published anode section dimensions")
    expression = dimensions.get("expression")
    if expression != "2020 mm × (240+270) mm × 270 mm":
        raise ValueError("The estimate requires the fixed published case section dimensions")
    match = re.fullmatch(
        r"(\d+) mm × \((\d+)\+(\d+)\) mm × (\d+) mm", expression,
    )
    if match is None:
        raise ValueError("The estimate requires the published anode section dimensions")
    length, top, bottom, height = (float(v) / METRIC_PREFIX for v in match.groups())
    anode = inputs["anode"]
    if length != anode["length_m"] or anode["type"] != "stand_off":
        raise ValueError("The estimate requires the published stand-off anode type and length")
    perimeter = top + bottom + 2 * math.hypot(height, (bottom - top) / 2)
    radius = kernel.equivalent_radius_from_periphery(perimeter)
    if "radius_m" in anode and not math.isclose(anode["radius_m"], radius):
        raise ValueError("The estimate radius differs from the published section basis")
    anode["radius_m"] = radius
    return working


def estimate_published_jacket_retrofit(
    cfg: Mapping[str, Any], *, phase_order: PhaseOrder,
) -> dict[str, Any]:
    """Return an estimate on stated assumptions, not an acceptance of an installed design.

    Supply the existing published-jacket B401 configuration, including its
    sourced zone areas and design densities. Changed or missing calculation
    inputs and unsupported overrides are rejected before the adapter runs.
    The declared assumed basis is
    B401 edition 2021, life 25 years, capacity 2000 Ah/kg, utilisation 0.90,
    nominal 323.4 kg interpreted as net alloy, density 2750 kg/m3, and a
    depleted section retaining its length and similar shape. These are assumed,
    based on previous experience, not additional claims from Xu (2023).
    Xu p. 242 Section 2 supplies the symmetric-trapezoid dimensions
    2020 mm × (240+270) mm × 270 mm, interpreted as a section-periphery
    radius independently of nominal mass; resistivity 0.22 ohm.m,
    driving voltage 0.25 V and installed count 569. The unchanged B401 route
    determines the original aggregate checks on the declared assumed basis.

    The phase order MUST be selected explicitly. Xu p. 244 Table 3 prints
    initial / maintain / late, so ``initial_mean_final`` follows its headings.
    ``initial_final_mean`` is a separate stated assumption, interchanging
    maintain and late; it must not be presented as the paper's phase mapping.
    Neither choice changes the original design densities or installed count.

    Hand check of the upper-zone estimate with initial/final/mean assumed:
    Xu p. 242 Tables 1–2 gives area 3726.8 m2, initial/mean/final design
    densities 150/70/100 mA/m2. Xu p. 244 Table 3 gives 170/80/110 mA/m2.
    Initial extra estimate = (170-150)*3726.8/1000 = 74.536 A;
    mean extra estimate = (110-70)*3726.8/1000 = 149.072 A;
    final signed extra estimate = (80-100)*3726.8/1000 = -74.536 A.
    The mass-count estimate is ceil(149.072*25*8760/(2000*.90*323.4)) = 57.
    The kernel supplies 8760 h/year and the B401 Sec. 7.7 Eq. 2 mass rule;
    Sec. 7.8 and Table 10-7 identify the existing current-output checks.
    Negative signed changes are retained but add no current-sizing credit.

    Demand contributions are returned per source zone. The pooled shortfall
    estimate is assigned to the lower zone containing the paper's proposed
    deep levels (Xu p. 244 Section 5), without reallocating existing anodes.
    The anode type and size are assumed to match the installed type, since
    no size for the clamped additions is published. Individual contribution
    estimates are rounded up separately; their sum is not the pooled count
    estimate, which is sized once from unrounded current differences.

    Not evaluated: per-level allocation, electrical continuity, measured
    depletion, the date the retrofit is needed by, confirmation after
    installation. The estimate credits no installed-count surplus against
    the added demand and retains the full assumed life for that demand.
    """
    if phase_order not in ("initial_mean_final", "initial_final_mean"):
        raise ValueError("phase_order must explicitly identify the Table 3 mapping")
    _validate_fixed_case(cfg.get("inputs"))
    working = _section_basis(cfg)
    inputs = working["inputs"]
    result = run_cathodic_protection(working)["results"]
    requirements = result["anode_requirements"]
    verification = result["current_output_verification"]
    # Declared assumptions, based on previous experience; Xu does not publish
    # edition, life, capacity or utilisation. Nominal mass and count: p. 242 §2.
    basis = (
        result["edition"], result["design_life_years"],
        requirements["electrochemical_capacity_Ah_kg"],
        requirements["utilization_factor"], requirements["individual_mass_kg"],
        verification["anode_count"], verification["driving_voltage_V"],
    )
    if basis != ("2021", 25.0, 2000.0, 0.90, 323.4, 569, 0.25):
        raise ValueError("The estimate inputs differ from the declared assumed basis")
    demand = result["current_demand_A"]
    zone_ids = tuple(item["zone"] for item in inputs["structure"]["zones"])
    if set(zone_ids) != {
        "coated-seawater", "upper-bare-seawater", DEEP_ZONE, "buried-steel",
    }:
        raise ValueError("The estimate requires the published-jacket zones")

    # Reuse the route's full-precision geometry, not its rounded display output.
    geometry = _b401_anode_geometry(inputs["anode"], requirements["utilization_factor"])
    outputs = {}
    for phase, radius in (
        ("initial", geometry.radius_initial_m), ("final", geometry.radius_final_m),
    ):
        resistance = calculate_anode_resistance(
            AnodeType(requirements["anode_type"]), geometry.length_m, radius,
            inputs["environment"]["seawater_resistivity_ohm_m"],
        )
        outputs[phase] = kernel.anode_current_output(
            verification["driving_voltage_V"], resistance,
        )

    order = phase_order.split("_")
    zones = {}
    for zone_id in zone_ids:
        row = demand[zone_id]
        design_density = {p: row[f"i_{p}_A_m2"] * METRIC_PREFIX for p in PHASES}
        density = dict(zip(order, TABLE_3_ROWS[zone_id])) if zone_id in TABLE_3_ROWS else design_density.copy()
        area = row["area_m2"]
        design_current = {p: area * row[f"i_{p}_A_m2"] for p in PHASES}
        subtropical = {p: area * density[p] / METRIC_PREFIX for p in PHASES}
        extra = {p: area * (density[p] - design_density[p]) / METRIC_PREFIX for p in PHASES}
        sizing = {p: max(extra[p], 0.0) for p in PHASES}
        zones[zone_id] = {
            "label": f"{zone_id} demand contribution estimate",
            "area_m2": area,
            "design_density_mA_m2": design_density,
            "subtropical_density_mA_m2": density,
            "design_current_A": design_current,
            "subtropical_current_A_estimate": subtropical,
            "extra_current_A_estimate": extra,
            "sizing_current_A_estimate": sizing,
            "anodes_estimate": _anode_checks(
                sizing, requirements, result["design_life_years"], outputs,
            ),
            "assigned_anodes_estimate": 0,
        }
    pooled = {p: sum(z["sizing_current_A_estimate"][p] for z in zones.values()) for p in PHASES}
    total_checks = _anode_checks(pooled, requirements, result["design_life_years"], outputs)
    count = total_checks["additional_anodes_estimate"]
    zones[DEEP_ZONE]["assigned_anodes_estimate"] = count
    return {
        "status": "ESTIMATE",
        "basis": "estimate on stated assumptions, not an acceptance of an installed design",
        "source": SOURCE,
        "declared_assumed_basis": (
            "Estimate assumed, based on previous experience: B401 edition 2021; "
            "life 25 years; capacity 2000 Ah/kg; utilisation 0.90; nominal "
            "323.4 kg interpreted as net alloy; symmetric-trapezoid perimeter "
            "radius independent of mass; similar-shape depletion at fixed length."
        ),
        "phase_order": phase_order,
        "phase_order_basis": (
            "Table 3 printed initial / maintain / late; mean follows maintain."
            if phase_order == "initial_mean_final" else
            "Estimate assumed initial / final / mean; Table 3 printed initial / maintain / late."
        ),
        "assumptions": [
            "The estimate uses the declared assumed basis, based on previous experience.",
            "The estimate uses the installed anode type and size for the proposed additions.",
            "The estimate uses a symmetric-trapezoid section perimeter and similar-shape depletion at fixed length.",
            "The estimate assigns both bare-zone shortfalls to the deep zone without reallocating existing anodes.",
            "The estimate credits no installed-count surplus and uses the full assumed design life.",
            "The estimate pools unrounded demand before sizing; individual contribution counts are not additive.",
        ],
        "not_evaluated": [
            "per-level allocation", "electrical continuity", "measured depletion",
            "the date the retrofit is needed by", "confirmation after installation",
        ],
        "design_on_declared_assumed_basis": {
            "count_by_mass": verification["count_by_mass"],
            "count_by_initial_current": verification["count_by_initial_current"],
            "count_by_final_current": verification["count_by_final_current"],
            "required_anodes": verification["recommended_anode_count"],
            "installed_anodes": verification["anode_count"],
            "governing_check": verification["governing_case"],
        },
        "unchanged_connected_allowance_A": result["connected_current_allowance_A"],
        "zones": zones,
        "total_estimate": {
            "label": "Total added demand and anodes estimate",
            "extra_current_A_estimate": {
                p: sum(z["extra_current_A_estimate"][p] for z in zones.values()) for p in PHASES
            },
            "sizing_current_A_estimate": pooled,
            "anodes_estimate": total_checks,
        },
        "placement_estimate": {
            "label": "Deep-zone placement estimate",
            "zone": DEEP_ZONE,
            "depths_m_estimate": list(DEEP_LEVELS_M),
            "assigned_anodes_estimate": count,
            "per_level_allocation_estimate": None,
            "existing_anodes_reallocated": False,
        },
    }


def retrofit_estimate_csv(result: Mapping[str, Any]) -> str:
    """Serialize the chart CSV, retaining estimate labels and phase provenance.

    Demand-anode estimates are independently rounded contributions; assigned
    anode estimates place the pooled count in the deep zone. Only assigned
    counts are additive. Demand fields exclude the unchanged well allowance.
    """
    fields = ["label", "zone", "phase_order", "phase_order_basis", "declared_assumed_basis", "area_m2"]
    for phase in PHASES:
        fields.extend((f"design_{phase}_A", f"subtropical_{phase}_A_estimate", f"extra_{phase}_A_estimate"))
    fields.extend(("demand_anodes_estimate", "assigned_anodes_estimate", "governing_check_estimate", "source"))
    output = io.StringIO()
    writer = csv.DictWriter(output, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    for zone_id, zone in result["zones"].items():
        row = {
            "label": zone["label"], "zone": zone_id,
            "phase_order": result["phase_order"], "phase_order_basis": result["phase_order_basis"],
            "declared_assumed_basis": result["declared_assumed_basis"],
            "area_m2": zone["area_m2"],
            "demand_anodes_estimate": zone["anodes_estimate"]["additional_anodes_estimate"],
            "assigned_anodes_estimate": zone["assigned_anodes_estimate"],
            "governing_check_estimate": zone["anodes_estimate"]["governing_check_estimate"],
            "source": result["source"],
        }
        for phase in PHASES:
            row[f"design_{phase}_A"] = zone["design_current_A"][phase]
            row[f"subtropical_{phase}_A_estimate"] = zone["subtropical_current_A_estimate"][phase]
            row[f"extra_{phase}_A_estimate"] = zone["extra_current_A_estimate"][phase]
        writer.writerow(row)
    return output.getvalue()
