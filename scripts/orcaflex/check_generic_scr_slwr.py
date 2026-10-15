# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproduce generic steel-riser static references without a licensed interface.

The OO solutions prescribe low-point geometry. The general solver independently
checks each positive-weight catenary arc with finite EA; upward buoyed arcs are
reflected before evaluation. This is a segment calibration, not a coupled
piecewise boundary-value solve or a native finite-element equilibrium.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
    LazyWaveSolver,
)
from digitalmodel.marine_ops.marine_analysis.catenary.simplified import (
    SimplifiedCatenarySolver,
)
from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
    CatenaryInput,
    CatenarySolver,
)
from digitalmodel.orcaflex.riser_config import RiserPipeProperties


def metrics(tension: float, angle: float, distance: float, length: float) -> dict[str, Any]:
    """Return physical values in explicit engineering units.

    Args:
        tension: Hang-off tension in newtons.
        angle: Departure angle from vertical in degrees.
        distance: Horizontal hang-off to touchdown distance in metres.
        length: Suspended centreline length in metres.

    Returns:
        Unrounded calibration values, rather than report display values.
    """
    return {
        "top_tension_kN": tension / 1000,
        "hangoff_angle_deg": angle,
        "touchdown_horizontal_distance_m": distance,
        "suspended_length_m": length,
    }


def general_arc(length: float, distance: float, height: float, weight: float,
                ea: float) -> Any:
    """Check an arc after reflecting upward buoyed geometry.

    Args:
        length: Unstretched arc length in metres.
        distance: Horizontal endpoint separation in metres.
        height: Absolute endpoint height separation in metres.
        weight: Absolute submerged weight in newtons per metre.
        ea: Pipe axial stiffness in newtons.

    Returns:
        Existing general catenary solver results.
    """
    return CatenarySolver().solve(CatenaryInput(
        length=length, horizontal_span=distance, vertical_span=height,
        weight_per_length=abs(weight), ea_stiffness=ea,
    ))


def scr_reference(p: dict[str, Any], height: float, weight: float,
                  ea: float) -> dict[str, Any]:
    """Compare the SCR geometry against the general elastic arc.

    Args:
        p: Explicit basis parameter values.
        height: Centreline hang-off to touchdown height in metres.
        weight: Submerged weight in newtons per metre.
        ea: Axial stiffness in newtons.

    Returns:
        Both SCR kernel records.
    """
    scr = SimplifiedCatenarySolver().solve_from_angle(p["scr_angle"], height)
    general = general_arc(scr.arc_length, scr.horizontal_distance, height,
                          weight, ea)
    scr_oo = metrics(math.hypot(weight * scr.bend_radius, weight * scr.arc_length),
                     p["scr_angle"], scr.horizontal_distance, scr.arc_length)
    scr_general = metrics(general.total_tension_fairlead, math.degrees(math.atan2(
        general.horizontal_tension, general.vertical_tension_fairlead)),
        scr.horizontal_distance, scr.arc_length)
    scr_general["converged"] = general.converged
    scr_general["elongation_m"] = general.elongation
    return {"oo": scr_oo, "general": scr_general}


def grounded_catenary(height: float, length: float, span: float,
                     weight: float) -> dict[str, Any]:
    """Solve frictionless, inextensible touchdown closure at a fixed span.

    With a = H/w, suspended length S = sqrt(h*(h+2*a)) and horizontal
    distance X = a*acosh(1+h/a), closure is X-S = span-length. Negative
    grounded reserve flags that this assumed touchdown solution is impossible;
    its extrapolated forces are not a physical lifted-end equilibrium.
    """
    if (not all(math.isfinite(v) for v in (height, length, span, weight))
            or height <= 0 or length <= height or span <= 0 or weight <= 0):
        raise ValueError("Finite positive geometry and weight are required")
    target = span - length
    if not -height < target < 0:
        raise ValueError("Span is outside the single-touchdown catenary range")

    def arc(radius: float) -> tuple[float, float]:
        return (math.sqrt(height * (height + 2 * radius)),
                radius * math.acosh(1 + height / radius))

    lower, upper = 0.0, height
    suspended, horizontal = arc(upper)
    while horizontal - suspended < target:
        upper *= 2
        suspended, horizontal = arc(upper)
    for _ in range(100):
        radius = (lower + upper) / 2
        suspended, horizontal = arc(radius)
        if horizontal - suspended < target:
            lower = radius
        else:
            upper = radius
    radius = (lower + upper) / 2
    suspended, horizontal = arc(radius)
    grounded = length - suspended
    return {
        "grounded_length_m": grounded,
        "touchdown_exists": grounded > 0,
        "touchdown_horizontal_distance_m": horizontal,
        "suspended_length_m": suspended,
        "horizontal_tension_kN": weight * radius / 1000,
        "top_tension_kN": weight * (radius + height) / 1000,
        "hangoff_angle_deg": math.degrees(math.atan2(radius, suspended)),
    }


def scr_seabed_design(p: dict[str, Any], height: float, weight: float,
                     nominal: dict[str, Any]) -> dict[str, Any]:
    """Derive SCR seabed reserve from the offset envelope plus explicit margin."""
    offsets = sorted({0.0, -p["intact_offset_pct"], p["intact_offset_pct"],
                      -p["damaged_offset_pct"], p["damaged_offset_pct"]})
    margin = p["scr_seabed_margin"]
    if not math.isfinite(margin) or margin <= 0:
        raise ValueError("SCR seabed margin must be finite and positive")
    suspended = nominal["suspended_length_m"]
    touchdown = nominal["touchdown_horizontal_distance_m"]

    def sweep(bottom: float) -> list[dict[str, Any]]:
        return [{"offset_pct": offset, **grounded_catenary(
            height, suspended + bottom,
            touchdown + bottom - offset * p["water_depth"] / 100, weight,
        )} for offset in offsets]

    original = sweep(p["bottom_length"])
    required = max(0.0, p["bottom_length"] - min(
        state["grounded_length_m"] for state in original
    ))
    selected = math.ceil((required + margin) * 10) / 10
    old_length = suspended + p["bottom_length"]
    liftoff_radius = (old_length ** 2 - height ** 2) / (2 * height)
    liftoff_span = liftoff_radius * math.acosh(1 + height / liftoff_radius)
    liftoff_offset = liftoff_span - touchdown - p["bottom_length"]
    return {
        "method": "Flat seabed; uniform submerged weight; inextensible, frictionless catenary closure",
        "offset_convention": "Positive host offset is toward the anchor; negative is away",
        "limitations": "Negative reserve is a diagnostic extrapolation, not a lifted-end equilibrium. Elasticity, bending, seabed friction and dynamics require native checks.",
        "height_m": height,
        "weight_N_per_m": weight,
        "required_bottom_length_m": required,
        "margin_m": margin,
        "selected_bottom_length_m": selected,
        "anchor_without_margin_m": touchdown + required,
        "selected_anchor_x_m": touchdown + selected,
        "original_liftoff_far_offset_m": liftoff_offset,
        "original_liftoff_far_offset_pct": liftoff_offset / p["water_depth"] * 100,
        "original": original,
        "selected": sweep(selected),
    }


def slwr_reference(p: dict[str, Any], pipe: RiserPipeProperties, height: float,
                   weight: float, ea: float) -> dict[str, Any]:
    """Compare the lazy-wave geometry against reflected elastic arcs.

    Args:
        p: Explicit basis parameter values.
        pipe: Coated nominal pipe cross-section.
        height: Centreline hang-off to touchdown height in metres.
        weight: Submerged bare-section weight in newtons per metre.
        ea: Axial stiffness in newtons.

    Returns:
        Both lazy-wave kernel records and derived buoyancy properties.
    """
    volume = math.pi / 4 * (p["module_diameter"] ** 2 - pipe.total_od ** 2)
    volume *= p["module_length"] / p["module_pitch"]
    buoyed_weight = weight - (p["seawater_density"] - p["module_density"]) * volume * p["gravity"]
    lazy = LazyWaveSolver().solve(LazyWaveConfiguration(
        hangoff_angle=p["slwr_angle"], hangoff_below_msl=p["hangoff_depth"],
        hog_bend_above_seabed=p["water_depth"] - p["hog_depth"] - pipe.total_od / 2,
        sag_bend_elevation=p["sag_elevation"], weight_without_buoyancy=weight,
        weight_with_buoyancy=buoyed_weight, vertical_distance=height,
    ))
    arcs = [general_arc(s.arc_length, s.horizontal_distance, s.vertical_distance,
                        s.weight_per_length, ea) for s in lazy.segments]
    top = arcs[0]
    slwr_general = metrics(top.total_tension_fairlead, math.degrees(math.atan2(
        top.horizontal_tension, top.vertical_tension_fairlead)),
        sum(s.horizontal_distance for s in lazy.segments),
        sum(s.arc_length for s in lazy.segments))
    slwr_general["converged"] = all(a.converged for a in arcs)
    slwr_general["segment_horizontal_tensions_kN"] = [a.horizontal_tension / 1000 for a in arcs]
    slwr_general["segment_elongations_m"] = [a.elongation for a in arcs]
    slwr_oo = metrics(lazy.total_tension, p["slwr_angle"], lazy.total_horizontal_distance,
                      lazy.total_arc_length)
    return {"oo": slwr_oo, "general": slwr_general,
                 "sections_m": [lazy.summary["HangOffToBuoyancy"]["S"],
                                lazy.summary["Buoyancy"]["S"],
                                lazy.summary["BuoyancyToTouchDown"]["S"]],
                 "vertical_closure_error_m": lazy.vertical_closure_error,
                 "buoyancy": {"volume_per_m_m2": volume,
                              "effective_od_m": math.sqrt(pipe.total_od ** 2 + 4 * volume / math.pi),
                              "net_weight_N_per_m": buoyed_weight}}


def calculate(basis: dict[str, Any]) -> dict[str, Any]:
    """Calculate both references from explicit basis records.

    Args:
        basis: Loaded authoritative basis mapping.

    Returns:
        OO and general segment results and operating/flooded submerged weights.
    """
    p = {name: record["value"] for name, record in basis["parameters"].items()}
    pipe = RiserPipeProperties(**{k: p[k] for k in (
        "outer_diameter", "wall_thickness", "coating_thickness",
        "coating_density", "contents_density", "corrosion_allowance",
        "seawater_density",
    )}, grade=basis["pipe_grade"])
    dry_mass = p["steel_density"] * pipe.steel_area + pipe.coating_mass_per_m
    weight = (dry_mass + p["contents_density"] * pipe.internal_area
              - p["seawater_density"] * pipe.displaced_volume_per_m) * p["gravity"]
    ea = p["young_modulus"] * pipe.steel_area
    height = p["water_depth"] - p["hangoff_depth"] - pipe.total_od / 2
    results: dict[str, Any] = {
        "scr": scr_reference(p, height, weight, ea),
        "slwr": slwr_reference(p, pipe, height, weight, ea),
    }
    for result in results.values():
        result["top_tension_difference_pct"] = abs(
            result["general"]["top_tension_kN"] / result["oo"]["top_tension_kN"] - 1
        ) * 100
        result["kernel_comparison"] = (
            "FLAG: top tension differs beyond 2 percent; kernel reconciliation required"
            if result["top_tension_difference_pct"] > 2 else
            "Top tension difference is within 2 percent; native calibration remains required"
        )
    results["operating_weight_N_per_m"] = weight
    results["flooded_weight_N_per_m"] = (
        dry_mass + p["flooded_density"] * pipe.internal_area
        - p["seawater_density"] * pipe.displaced_volume_per_m
    ) * p["gravity"]
    results["scr"]["seabed_offset_sweep"] = scr_seabed_design(
        p, height, weight, results["scr"]["oo"],
    )
    return results


def reference(campaign: Path) -> dict[str, Any]:
    """Bind calculated references to basis, specifications and modular bytes.

    Args:
        campaign: Directory containing the basis and both models.

    Returns:
        A deterministic digest-bound calibration record.
    """
    basis = yaml.safe_load((campaign / "basis.yml").read_text())
    files = [campaign / "basis.yml"]
    for kind in ("scr", "slwr"):
        files.extend(sorted((campaign / kind).rglob("*.yml")))
    return {
        "schema_version": "generic-static-reference-v1",
        "method": "OO low-point geometry; general elastic solver on each reflected arc",
        "limitations": basis["analytical_scope"],
        "geometry_comparison": "General arc endpoints and unstretched lengths are prescribed from OO geometry; their equality is not an independent global geometry check. Elastic arc horizontal tensions need not be continuous.",
        "formulas": ["h = water_depth - hangoff_depth - coated_OD/2",
                     "EA = young_modulus*pi/4*(steel_OD^2-steel_ID^2)",
                     "w_pipe = (steel_mass+coating_mass+contents_mass-rho_water*pi/4*coated_OD^2)*g",
                     "module_volume_per_m = pi/4*(module_OD^2-coated_OD^2)*module_length/module_pitch",
                     "w_buoyed = w_pipe - (rho_water-rho_module)*module_volume_per_m*g",
                     "a = H/w; S = sqrt(h*(h+2*a)); X = a*acosh(1+h/a)",
                     "D = anchor_X - host_offset; solve X-S = D-L; grounded = L-S",
                     "SCR bottom = ceil(10*(maximum suspended-length increase + margin))/10"],
        "sha256": {str(p.relative_to(campaign)): hashlib.sha256(p.read_bytes()).hexdigest() for p in files},
        "analytical_code_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "models": calculate(basis),
    }


def regenerate_scr(campaign: Path) -> None:
    """Update SCR geometry from its basis and use the existing model generator.

    The nominal suspended arc stays unchanged. The lazy-wave specifications
    and model bytes are never written by this preparation step.
    """
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

    basis = yaml.safe_load((campaign / "basis.yml").read_text())
    result = calculate(basis)["scr"]
    spec_path = campaign / "scr/spec.yml"
    spec = yaml.safe_load(spec_path.read_text())
    line = spec["riser"]["lines"][0]
    bottom = result["seabed_offset_sweep"]["selected_bottom_length_m"]
    line["sections"][0]["length"] = result["oo"]["suspended_length_m"]
    line["sections"][-1]["length"] = bottom
    line["end_b"]["position"][0] = result["oo"]["touchdown_horizontal_distance_m"] + bottom
    spec_path.write_text(yaml.safe_dump(spec, sort_keys=False, width=90))
    model = campaign / "scr/modular"
    ModularModelGenerator(spec_path).generate(model)
    # Match the existing end-of-file hook on the directly written master.
    master = model / "master.yml"
    raw = master.read_bytes()
    if not raw.endswith(b"\n"):
        master.write_bytes(raw + b"\n")


def main() -> None:
    """Write the calibration record to an explicitly selected output file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--regenerate-scr", action="store_true")
    args = parser.parse_args()
    if args.regenerate_scr:
        regenerate_scr(args.campaign)
    args.output.write_text(json.dumps(reference(args.campaign), indent=2, allow_nan=False) + "\n")


if __name__ == "__main__":
    main()
