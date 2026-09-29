"""External ship-hull galvanic CP design per ABS GN Ships (December 2017)."""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from typing import Any, Mapping

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.abs_ships_tables import (
    aluminium_properties,
    equation_reference,
    protection_potential,
    zinc_properties,
)
from digitalmodel.cathodic_protection.abs_ships_result import build_result
from digitalmodel.cathodic_protection.abs_ships_layout import evaluate_layout
from digitalmodel.citations import CitedValue

USE_STATUS = "cited-pending-review"


def _mapping(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else {}


def _positive(value: Any, name: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must be a positive finite number") from exc
    if not math.isfinite(number) or number <= 0.0:
        raise ValueError(f"{name} must be a positive finite number")
    return number


def _fraction(value: Any, name: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must be a fraction from 0 to 1") from exc
    if not math.isfinite(number) or not 0.0 <= number <= 1.0:
        raise ValueError(f"{name} must be a fraction from 0 to 1")
    return number


def _percent(value: Any, name: str) -> float:
    number = float(value)
    if not math.isfinite(number) or not 0.0 <= number <= 100.0:
        raise ValueError(f"{name} percentage must be from 0 to 100")
    return number / 100.0


@dataclass(frozen=True)
class DepletedLongFlush:
    net_mass_kg: float
    length_m: float
    cross_section_m2: float
    radius_m: float
    equivalent_width_m: float


def depleted_long_flush(
    net_mass_kg: float,
    utilisation: float,
    length_m: float,
    core_cross_section_m2: float,
    density_kg_m3: float,
) -> DepletedLongFlush:
    """ABS 2/7.5.1-7.5.2(b), using the guide's printed expression."""
    if not 0.70 <= utilisation <= 0.95:
        raise ValueError("anode utilisation must be within ABS range 0.70 to 0.95")
    final_mass = _positive(net_mass_kg, "net_mass_kg") * (1.0 - utilisation)
    final_length = _positive(length_m, "length_m") * (1.0 - 0.1 * utilisation)
    core_area = _positive(core_cross_section_m2, "core_cross_section_m2")
    density = _positive(density_kg_m3, "anode_density")
    final_area = math.pi * final_mass / (final_length * density) + core_area
    final_radius = math.sqrt(2.0 * final_area / math.pi)
    return DepletedLongFlush(
        final_mass, final_length, final_area, final_radius, 2.0 * final_radius
    )


def _alloy(inputs: Mapping[str, Any]) -> tuple[CitedValue, CitedValue]:
    anode = _mapping(inputs.get("anode"))
    design = _mapping(inputs.get("design_data"))
    environment = _mapping(_mapping(inputs.get("environment")).get("seawater"))
    temperature = float(
        design.get("seawater_max_temperature", environment.get("max_temperature", 25.0))
    )
    material = str(anode.get("material", "aluminium")).lower()
    if material in {"aluminium", "aluminum"}:
        potential, capacity = aluminium_properties(str(anode.get("alloy", "A1")))
        if temperature > 25.0:
            raise ValueError(
                "aluminium above 25 C requires verified manufacturer capacity"
            )
        return potential, capacity
    if material == "zinc":
        return zinc_properties(str(anode.get("alloy", "Z1")), temperature_c=temperature)
    raise ValueError("ABS ships supports aluminium/aluminum or zinc anodes")


def _coating_factors(structure: Mapping[str, Any]) -> dict[str, float]:
    initial = _percent(
        structure.get("coating_initial_breakdown_factor"), "initial coating breakdown"
    )
    final = _percent(
        structure.get("coating_breakdown_factor_max"), "maximum coating breakdown"
    )
    if final < initial:
        raise ValueError("maximum coating breakdown must be >= initial")
    return {
        "initial": initial,
        "mean": (initial + final) / 2.0,
        "final": final,
    }


def _demand(inputs: Mapping[str, Any]) -> tuple[dict[str, Any], dict[str, float]]:
    structure = _mapping(inputs.get("structure"))
    current = _mapping(inputs.get("design_current"))
    area = _positive(
        structure.get("steel_total_area", structure.get("steel_coated_area")),
        "structure.steel_total_area",
    )
    coverage = _fraction(
        float(structure.get("area_coverage", 100.0)) / 100.0, "area coverage"
    )
    factors = _coating_factors(structure)
    dynamic = current.get("dynamic_bare_steel_mA_m2")
    if dynamic is None:
        raise ValueError(
            "design_current.dynamic_bare_steel_mA_m2 is required; "
            "the rebuilt route does not infer an ABS value from coated density"
        )
    dynamic = _positive(dynamic, "dynamic bare density")
    static = _positive(current.get("static_bare_steel_mA_m2"), "static bare density")
    time_fraction = _fraction(
        current.get("dynamic_time_fraction"), "dynamic time fraction"
    )
    weighted = time_fraction * dynamic + (1.0 - time_fraction) * static
    densities = {
        "initial": dynamic * factors["initial"],
        "mean": weighted * factors["mean"],
        "final": dynamic * factors["final"],
    }
    coated_area, bare_area = area * coverage, area * (1.0 - coverage)
    coated = {phase: coated_area * value / 1000.0 for phase, value in densities.items()}
    bare = {
        "initial": bare_area * dynamic / 1000.0,
        "mean": bare_area * weighted / 1000.0,
        "final": bare_area * dynamic / 1000.0,
    }
    totals = {phase: coated[phase] + bare[phase] for phase in densities}
    return {
        "coated": coated,
        "uncoated": bare,
        "totals": totals,
        "areas_m2": {"total": area, "coated": coated_area, "uncoated": bare_area},
        "densities_mA_m2": densities,
        "bare_current_basis": {
            "dynamic_mA_m2": dynamic,
            "static_mA_m2": static,
            "dynamic_time_fraction": time_fraction,
            "source": "project inputs; Table 3 is a reference comparator only",
        },
    }, factors


def _driving_voltage(
    inputs: Mapping[str, Any], potential: CitedValue
) -> tuple[float, CitedValue]:
    anode = _mapping(inputs.get("anode"))
    biofouling = _mapping(_mapping(inputs.get("design_data")).get("biofouling"))
    anaerobic = str(biofouling.get("type", "aerobic")).lower() == "anaerobic"
    protection = protection_potential(anaerobic)
    configured_p = float(anode.get("protection_potential", protection.value))
    configured_p = -configured_p if configured_p > 0.0 else configured_p
    configured_a = float(anode.get("closed_circuit_anode_potential", potential.value))
    if not math.isclose(configured_p, protection.value) or not math.isclose(
        configured_a, potential.value
    ):
        raise ValueError("configured potentials must match the cited ABS table values")
    delta_e = protection.value - potential.value
    if delta_e <= 0.0:
        raise ValueError(
            "anode potential must be more negative than protection potential"
        )
    return delta_e, protection


def _resistances(
    inputs: Mapping[str, Any], utilisation: float, net_mass: float
) -> tuple[float, float, dict[str, float] | None]:
    anode = _mapping(inputs.get("anode"))
    physical = _mapping(anode.get("physical_properties"))
    geometry = _mapping(anode.get("geometry"))
    seawater = _mapping(_mapping(inputs.get("environment")).get("seawater"))
    rho = _positive(
        _mapping(seawater.get("resistivity")).get("input"), "seawater resistivity"
    )
    if str(geometry.get("type", "long_flush")) == "short_flush":
        resistance = kernel.short_flush_or_bracelet(
            rho, _positive(geometry.get("exposed_area_m2"), "exposed_area_m2")
        )
        return resistance, resistance, None
    length = _positive(geometry.get("length_m", physical.get("mean_length")), "length")
    width = _positive(geometry.get("width_m", physical.get("width")), "width")
    density = _positive(anode.get("anode_density"), "anode_density")
    height = _positive(
        physical.get("height", net_mass / (density * length * width)), "height"
    )
    initial = kernel.long_flush(rho, length, width, height)
    core_area = _positive(
        physical.get("core_cross_section_m2"), "core_cross_section_m2"
    )
    final = depleted_long_flush(
        net_mass,
        utilisation,
        length,
        core_area,
        density,
    )
    return (
        initial,
        rho / (final.length_m + final.equivalent_width_m),
        asdict(final),
    )


def _requirements(
    demand: Mapping[str, Any],
    mass: float,
    net_mass: float,
    outputs: Mapping[str, float],
    layout: Mapping[str, Any],
) -> tuple[dict[str, int], int, int, str]:
    weakest_output = min(outputs.values())
    counts = {
        "mass": kernel.anode_count(mass, net_mass),
        "initial_current_output": kernel.anodes_for_current(
            demand["totals"]["initial"], outputs["initial"]
        ),
        "mean_current_output": kernel.anodes_for_current(
            demand["totals"]["mean"], weakest_output
        ),
        "final_current_output": kernel.anodes_for_current(
            demand["totals"]["final"], outputs["final"]
        ),
    }
    selected = int(layout.get("selected_anode_count", 0))
    layout_minimum = int(layout.get("minimum_layout_count", 0))
    required = max(max(counts.values()), layout_minimum)
    governing = (
        "layout_distribution"
        if layout_minimum == required
        else max(counts, key=lambda key: counts[key])
    )
    return counts, required, selected, governing


def _citation_values(
    potential: CitedValue, capacity: CitedValue, protection: CitedValue
) -> list[CitedValue]:
    clauses = (
        ("Section 2, Table 3", "reference design current-density ranges"),
        ("Section 2/4.4 and Table 4", "coating breakdown percentage and Jc = Jb fc"),
        ("Section 2/4.5", "initial, mean and maximum current demand"),
        ("Section 2/5", "anode output I = delta E / R"),
        ("Sections 2/6.1-2/6.3", "initial and final resistance"),
        ("Section 2/7.3", "minimum net anode mass"),
        ("Sections 2/7.5.1-2/7.5.2", "depleted anode geometry"),
        ("Section 3/5.2", "hull anode distribution and spacing"),
    )
    return [
        potential,
        capacity,
        protection,
        *(equation_reference(section, note) for section, note in clauses),
    ]


def _checks(
    selected: int,
    counts: Mapping[str, int],
    layout: Mapping[str, Any],
) -> dict[str, bool]:
    return {
        "mass": selected >= counts["mass"],
        "initial_current_output": selected >= counts["initial_current_output"],
        "mean_current_output": selected >= counts["mean_current_output"],
        "final_current_output": selected >= counts["final_current_output"],
        "layout": layout["status"] == "PASS",
    }


def design_abs_ships(inputs: Mapping[str, Any]) -> dict[str, Any]:
    """Return hull demand, mass, output, layout, governing case and checks."""
    demand, coating = _demand(inputs)
    design = _mapping(inputs.get("design_data"))
    anode = _mapping(inputs.get("anode"))
    life = _positive(design.get("design_life"), "design life")
    utilisation = float(anode.get("anode_Utilisation_factor", 0.85))
    potential, capacity = _alloy(inputs)
    delta_e, protection = _driving_voltage(inputs, potential)
    mass = kernel.anode_mass(
        demand["totals"]["mean"], life, capacity.value, utilisation
    )
    physical = _mapping(anode.get("physical_properties"))
    net_mass = _positive(physical.get("net_weight"), "net mass")
    initial_r, final_r, depleted = _resistances(inputs, utilisation, net_mass)
    outputs = {
        "initial": kernel.anode_current_output(delta_e, initial_r),
        "final": kernel.anode_current_output(delta_e, final_r),
    }
    layout = evaluate_layout(_mapping(inputs.get("layout")))
    counts, required, selected, governing = _requirements(
        demand, mass, net_mass, outputs, layout
    )
    checks = _checks(selected, counts, layout)
    cited = _citation_values(potential, capacity, protection)
    return build_result(
        demand,
        coating,
        life,
        capacity,
        mass,
        net_mass,
        outputs,
        initial_r,
        final_r,
        depleted,
        layout,
        counts,
        required,
        selected,
        governing,
        checks,
        delta_e,
        cited,
    )


__all__ = ["USE_STATUS", "depleted_long_flush", "design_abs_ships"]
