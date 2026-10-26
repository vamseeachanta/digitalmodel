# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Frozen twelve-section tranche for the existing solver-free campaign writer.

Constants below are supplied matrix/criteria values unless identified as unit
conversions or geometry identities. No solver interface or code-check strategy
is imported; the checked wall is a frozen module-screen input, not certification.
"""

import csv
import hashlib
import io
import json
import math
from pathlib import Path
from typing import Any

from digitalmodel.riser_configuration_screen.static import lazy_wave_offsets, scr_offsets
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import EnvironmentBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import GenericModelBuilder
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

CAMPAIGN = Path(__file__).resolve().parents[2] / "examples/riser-campaign"
SCOPE = "matrix-i0339-2.tsv"
CRITERIA = "criteria-i0339-2.json"
SCOPE_SHA256 = "f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1"
VERSION = "I0339-2-v1"
# Matrix OD1: NPS is a nominal designation, not literal inches converted to mm.
NPS_OD_MM = {8: 219.1, 10: 273.1, 12: 323.9}


def source_payloads() -> dict[str, bytes]:
    """Capture the two frozen inputs once for validation and serialization."""
    return {name: (CAMPAIGN / name).read_bytes() for name in (SCOPE, CRITERIA)}


def criteria() -> dict[str, Any]:
    """Read the versioned numerical assumptions and their provenance."""
    return json.loads((CAMPAIGN / CRITERIA).read_text(encoding="utf-8"))


def validate_tranche(t: dict[str, Any], payloads: dict[str, bytes] | None = None) -> None:
    """Reject any departure from frozen bytes, fields, IDs or criteria."""
    payloads = source_payloads() if payloads is None else payloads
    if set(t) != {"criteria_version", "scope_sha256", "criteria_sha256", "rows"}:
        raise ValueError("unexpected frozen tranche fields")
    if t["criteria_version"] != VERSION:
        raise ValueError("frozen criteria version mismatch")
    if t["scope_sha256"] != SCOPE_SHA256 or hashlib.sha256(payloads[SCOPE]).hexdigest() != SCOPE_SHA256:
        raise ValueError("frozen scope digest mismatch")
    if hashlib.sha256(payloads[CRITERIA]).hexdigest() != t["criteria_sha256"]:
        raise ValueError("frozen criteria digest mismatch")
    if json.loads(payloads[CRITERIA])["version"] != VERSION:
        raise ValueError("criteria version mismatch")
    frozen = list(csv.DictReader(io.StringIO(payloads[SCOPE].decode("utf-8")), delimiter="\t"))
    ids = [r["case_id"] for r in t["rows"]]
    expected = {f"R{n:02}-W{w}-{kind}" for n in (8, 10, 12) for w in (1, 2) for kind in ("SCR", "SLWR")}
    if len(ids) != len(set(ids)) or set(ids) != expected:
        raise ValueError("frozen matrix requires exactly twelve unique combinations")
    if t["rows"] != frozen:
        raise ValueError("TSV-to-manifest field parity failed")
    c = json.loads(payloads[CRITERIA])
    for row in t["rows"]:
        section_properties(row, c)


def section_properties(row: dict[str, str], c: dict[str, Any] | None = None) -> dict[str, float]:
    """Use nominal wall for mass/EA/EI and one reduced wall for resistance.

    SI annular areas and second moments are geometric identities. Coating,
    contents, steel, gravity and module values are the supplied property basis.
    """
    basis = criteria() if c is None else c
    c = basis["properties"]
    outer = float(row["outside_diameter_mm"]) / 1000
    if float(row["outside_diameter_mm"]) != NPS_OD_MM[int(row["nps_in"])]:
        raise ValueError("NPS outside diameter mismatch")
    wall = float(row["wall_thickness_mm"]) / 1000
    checked = (1 - c["mill_minus_tolerance_fraction"]) * wall - float(row["corrosion_allowance_mm"]) / 1000
    if not math.isclose(checked, float(row["checked_wall_mm"]) / 1000, abs_tol=1e-12):
        raise ValueError("checked wall deduction mismatch")
    inner = outer - 2 * wall
    area = math.pi * (outer**2 - inner**2) / 4
    inertia = math.pi * (outer**4 - inner**4) / 64
    coated = outer + 2 * c["coating_thickness_m"]
    coating_area = math.pi * (coated**2 - outer**2) / 4
    dry = area * c["steel_density_kg_m3"] + coating_area * c["coating_density_kg_m3"]
    weight = (dry + math.pi * inner**2 / 4 * c["contents_density_kg_m3"]
              - math.pi * coated**2 / 4 * c["seawater_density_kg_m3"]) * c["gravity_m_s2"]
    result = {"outer_diameter_m": outer, "inner_diameter_m": inner, "checked_wall_m": checked,
              "coated_od_m": coated, "dry_mass_kg_m": dry, "EA_N": c["youngs_modulus_Pa"] * area,
              "EI_Nm2": c["youngs_modulus_Pa"] * inertia, "submerged_weight_N_m": weight}
    if row["configuration"] == "SLWR":
        b = basis["buoyancy"]
        volume = b["factor"] * weight / ((c["seawater_density_kg_m3"] - b["density_kg_m3"]) * c["gravity_m_s2"])
        module_od = math.sqrt(coated**2 + 4 * volume / math.pi * b["pitch_m"] / b["length_m"])
        result.update(module_od_m=module_od, module_volume_per_m=volume,
                      buoyed_od_m=math.sqrt(coated**2 + 4 * volume / math.pi),
                      buoyed_dry_mass_kg_m=dry + volume * b["density_kg_m3"],
                      buoyed_weight_N_m=weight * (1 - b["factor"]),
                      buoyed_drag_diameter_m=coated + (module_od - coated) * b["length_m"] / b["pitch_m"])
    return result


def pressures(depth_m: float) -> dict[str, float]:
    """Return gauge kPa at positive depth below MSL; Pa/1000 gives kPa."""
    c = criteria()["properties"]
    return {"internal_kPa_g": c["pressure_at_msl_MPa_g"] * 1000 + c["contents_density_kg_m3"] * c["gravity_m_s2"] * depth_m / 1000,
            "external_kPa_g": c["seawater_density_kg_m3"] * c["gravity_m_s2"] * depth_m / 1000}


def prepare_geometry(scope: dict[str, str], c: dict[str, Any] | None = None) -> dict[str, Any]:
    """Derive reserve over the whole offset envelope without retuning geometry.

    The first solve uses depth as a bracketing tail only. Suspended-length
    differences are independent of the tail; the installed tail is then the
    largest suspended-length increase plus the requested reserve, rounded up
    on the inherited 0.1 m grid. If the friction bound fails, keep a failure.
    """
    c = criteria() if c is None else c
    p = section_properties(scope, c)
    depth = float(scope["water_depth_m"])
    height = depth - c["geometry"]["hangoff_depth_m"]
    offsets = [0.0, *[float(v) for v in scope["static_span_offsets_m"].split(";") if float(v) != 0]]
    config = {"water_depth_m": height, "submerged_weight_n_m": p["submerged_weight_N_m"],
              "grounded_length_m": depth, "hangoff_angle_deg": c["geometry"]["angles_deg"][scope["configuration"]]}
    solver = scr_offsets
    if scope["configuration"] == "SLWR":
        solver = lazy_wave_offsets
        config.update(buoyancy_factor=c["buoyancy"]["factor"], sag_fraction=float(scope["sag_target_m_ASB"])/height,
                      hog_fraction=float(scope["hog_target_m_ASB"])/height)
    trial = solver(config, offsets)
    increase = max(r["suspended_length_m"] - trial[0]["suspended_length_m"] for r in trial)
    grid = c["geometry"]["reserve_grid_m"]
    ground = math.ceil((increase + c["geometry"]["grounded_reserve_m"])/grid) * grid
    config["grounded_length_m"] = ground
    rows = solver(config, offsets)
    total = rows[0]["suspended_length_m"] + ground
    for r in rows:
        r["adverse_friction_min_tension_N"] = r["horizontal_tension_n"] - c["seabed"]["friction"] * p["submerged_weight_N_m"] * r["grounded_length_m"]
        if r["grounded_length_m"] < c["geometry"]["grounded_reserve_m"] - 1e-6:
            raise ValueError("fixed geometry fails grounded reserve")
        if r["adverse_friction_min_tension_N"] < c["geometry"]["minimum_tension_N"]:
            raise ValueError("fixed geometry fails adverse-friction positive-tension bound")
        if max(abs(r["vertical_closure_error_m"]), abs(r["horizontal_closure_error_m"]),
               abs(r["suspended_length_m"] + r["grounded_length_m"] - total)) > 1e-6:
            raise ValueError("fixed geometry closure failed")
        r["shape_global"] = [[s, x-r["offset_m"], z-depth] for s, x, z in r["shape"]]
        if scope["configuration"] == "SLWR" and (
            abs(r["buoyed_start_m"] - rows[0]["buoyed_start_m"]) > 1e-6 or
            abs(r["buoyed_length_m"] - rows[0]["buoyed_length_m"]) > 1e-6
        ):
            raise ValueError("buoyancy material arcs moved")
    baseline = rows[0]
    turns = _turning_points(baseline["shape"])
    if scope["configuration"] == "SLWR" and (
        len(turns) != 2 or abs(turns[0][2] - float(scope["sag_target_m_ASB"])) > 1e-6
        or abs(turns[1][2] - float(scope["hog_target_m_ASB"])) > 1e-6
    ):
        raise ValueError("nominal sag/hog shape misses the frozen ASB targets")
    return {"vertical_separation_m": height, "hangoff_m": [0, 0, -c["geometry"]["hangoff_depth_m"]],
            "contact_centreline_correction_m": 0, "contact_status": "NATIVE_CENTRELINE_READBACK_REQUIRED",
            "anchor_m": [baseline["horizontal_span_m"] + ground, 0, -depth],
            "installed_length_m": total, "nominal_grounded_length_m": ground,
            "nominal_sag_ASB_m": None if not turns else turns[0][2],
            "buoyancy_start_m": baseline["buoyed_start_m"], "buoyancy_length_m": baseline["buoyed_length_m"],
            "buoyancy_end_m": None if baseline["buoyed_start_m"] is None else baseline["buoyed_start_m"] + baseline["buoyed_length_m"],
            "reference": "frictionless analytical geometry; adverse-friction tension bound evaluated separately",
            "offsets": rows}


def _turning_points(shape: list[list[float]]) -> list[list[float]]:
    """Locate sampled catenary segment vertices from a change in slope sign."""
    return [b for a, b, c in zip(shape, shape[1:], shape[2:]) if (b[2]-a[2])*(c[2]-b[2]) < 0]


def _mesh_sections(scope: dict[str, str], g: dict[str, Any], c: dict[str, Any]) -> list[list[Any]]:
    """Place fine windows on fixed material stations over the offset envelope."""
    total = g["installed_length_m"]
    hotspots = [0.0, *[row["suspended_length_m"] for row in g["offsets"]]]
    buoyed_start, buoyed_end = g["buoyancy_start_m"], g["buoyancy_end_m"]
    boundaries = {0.0, total}
    if scope["configuration"] == "SLWR":
        hotspots += [buoyed_start, buoyed_end]
        boundaries.update([buoyed_start, buoyed_end])
        for row in g["offsets"]:
            hotspots += [point[0] for point in _turning_points(row["shape"])]
    half_window = c["numerics"]["fine_window_m"] / 2
    windows = [(max(0.0, s-half_window), min(total, s+half_window)) for s in hotspots]
    for left, right in windows:
        boundaries.update([left, right])
    edges = sorted(boundaries)
    sections = []
    for left, right in zip(edges, edges[1:]):
        middle = (left+right)/2
        kind = "buoyed-pipe" if buoyed_start is not None and buoyed_start < middle < buoyed_end else "pipe"
        fine = any(a <= middle <= b for a, b in windows)
        step = c["numerics"]["fine_segment_m" if fine else "coarse_segment_m"]
        sections.append([kind, right-left, step])
    return sections


def expand_loads(t: dict[str, Any], c: dict[str, Any] | None = None) -> list[dict[str, Any]]:
    """Expand Phase A primary rows; governing QA waits for native responses."""
    c = criteria() if c is None else c
    result = []
    for case in t["rows"]:
        case_id = case["case_id"]
        q0 = case_id + "--Q0"
        result.append({"id": q0, "case_id": case_id, "kind": "statics", "group": "Q0", "span_offset_m": 0,
                       "wave_toward_deg": 0, "current_toward_deg": 0, "current_speed_m_s": 0, "equilibrium_id": None})
        for offset in map(int, case["static_span_offsets_m"].split(";")):
            for wave, current in c["strength"]["heading_pairs_deg"]:
                equilibrium = f"{case_id}--S-{offset:+d}-{current}"
                base = {"case_id": case_id, "span_offset_m": offset, "wave_toward_deg": wave,
                        "current_toward_deg": current, "current_speed_m_s": c["strength"]["current_knots"][0][1]}
                result.append({**base, "id": equilibrium, "kind": "statics", "group": "S", "equilibrium_id": None})
                if offset == 0:
                    result.append({**base, "id": f"{case_id}--D-{wave}", "kind": "dynamics", "group": "D",
                                   "stationary_seconds": c["strength"]["stationary_seconds"], "equilibrium_id": equilibrium})
        for sea, seed in zip(case["fatigue_seas"].split(";"), map(int, case["fatigue_seeds"].split(";"))):
            result.append({"id": f"{case_id}--{sea}", "case_id": case_id, "kind": "fatigue", "group": "F",
                           "sea_state_id": sea, "seed": seed, "span_offset_m": int(case["fatigue_span_offset_m"]),
                           "wave_toward_deg": int(case["fatigue_wave_toward_deg"]), "current_toward_deg": 0,
                           "current_speed_m_s": 0, "stationary_seconds": int(case["fatigue_stationary_seconds"]), "equilibrium_id": q0})
    return result


def validate_loads(t: dict[str, Any], loads: list[dict[str, Any]], c: dict[str, Any] | None = None) -> None:
    """Reject missing, duplicate, altered or additional execution rows."""
    if loads != expand_loads(t, c):
        raise ValueError("load expansion differs from the frozen matrix")


def _line_types(scope: dict[str, str], c: dict[str, Any]) -> list[dict[str, Any]]:
    p = section_properties(scope, c)
    properties = {"StressOD": p["outer_diameter_m"], "StressID": p["outer_diameter_m"] - 2*p["checked_wall_m"],
                  "PoissonRatio": c["properties"]["poisson_ratio"],
                  "Cd": [c["numerics"]["drag_coefficient"], c["numerics"]["axial_drag_coefficient"]],
                  "Ca": [c["numerics"]["added_mass_coefficient"], 0],
                  "NormalDragLiftDiameter": p["coated_od_m"],
                  "SeabedLateralFrictionCoefficient": c["seabed"]["friction"],
                  "SeabedAxialFrictionCoefficient": c["seabed"]["friction"]}
    bare = {"name": "pipe", "category": "General", "outer_diameter": p["coated_od_m"],
            "inner_diameter": p["inner_diameter_m"], "mass_per_length": p["dry_mass_kg_m"]/1000,
            "axial_stiffness": p["EA_N"]/1000, "bending_stiffness": p["EI_Nm2"]/1000, "properties": properties}
    result = [bare]
    if scope["configuration"] == "SLWR":
        result.append({**bare, "name": "buoyed-pipe", "outer_diameter": p["buoyed_od_m"],
                       "mass_per_length": p["buoyed_dry_mass_kg_m"]/1000,
                       "properties": {**properties, "NormalDragLiftDiameter": p["buoyed_drag_diameter_m"]}})
    return result


def native_model(scope: dict[str, str], load: dict[str, Any], g: dict[str, Any],
                 c: dict[str, Any] | None = None) -> dict[str, Any]:
    """Reuse typed native builders; dynamic files are unreleased load templates.

    Floater motions are deliberately absent until exact-source, heading and
    spectral-band clearance. No synthetic or extrapolated motion is substituted.
    The output index, rather than a native solver setting, enforces release gates.
    """
    c = criteria() if c is None else c
    if load["wave_toward_deg"] not in (0, 180):
        raise ValueError("unsupported heading")
    fatigue = load["kind"] == "fatigue"
    dynamic = load["kind"] != "statics"
    duration = [c["numerics"]["build_up_seconds"], load.get("stationary_seconds", c["strength"]["stationary_seconds"])]
    wave = {"type": "dean_stream", "height": c["strength"]["Hs_m"] * c["strength"]["regular_height_factor"] if dynamic else 0,
            "period": c["strength"]["Tp_s"], "direction": load["wave_toward_deg"]}
    if fatigue:
        sea = c["fatigue"]["seas"][load["sea_state_id"]]
        wave.update(type="jonswap", height=sea["Hs_m"], period=sea["Tp_s"], gamma=sea["gamma"])
    sections = _mesh_sections(scope, g, c)
    raw = {}
    if fatigue:
        raw = {"UserSpecifiedRandomWaveSeeds": "Yes", "WaveTrains": [{"WaveTp": sea["Tp_s"],
               "WaveJONSWAPParameters": "Partially specified", "WaveSeed": load["seed"]}]}
    knots = c["strength"]["current_knots"]
    current_speed = load["current_speed_m_s"]
    spec = ProjectInputSpec.model_validate({
        "metadata": {"name": load["id"], "description": "Unreleased screening input; native QA and motion clearance required",
                     "structure": "generic", "operation": "production"},
        "environment": {
            "water": {"depth": float(scope["water_depth_m"]), "density": c["properties"]["seawater_density_kg_m3"]/1000},
            "seabed": {"stiffness": {"normal": c["seabed"]["normal_stiffness_kN_m3"], "shear": c["seabed"]["shear_stiffness_kN_m3"]},
                       "friction_coefficient": c["seabed"]["friction"]},
            "waves": {"trains": [wave]}, "raw_properties": raw,
            "current": {"speed": current_speed, "direction": load["current_toward_deg"],
                        "profile": [[depth, speed/knots[0][1]] for depth, speed in knots]}, "wind": {"speed": 0}},
        "generic": {
            "general_properties": {"UnitsSystem": "SI", "g": c["properties"]["gravity_m_s2"], "StageDuration": duration,
                                   "DynamicsSolutionMethod": "Implicit time domain", "ImplicitUseVariableTimeStep": False,
                                   "ImplicitConstantTimeStep": c["numerics"]["time_step_s"]},
            "line_types": _line_types(scope, c),
            "vessel_types": [{"name": "reference-floater", "length": c["geometry"]["floater_length_m"],
                              "properties": {"Draughts": [{"Name": "reference-draught"}]}}],
            "vessels": [{"name": "floater", "vessel_type": "reference-floater", "connection": "Free",
                         "initial_position": [-load["span_offset_m"], 0, 0], "length": c["geometry"]["floater_length_m"],
                         "properties": {"Draught": "reference-draught", "PrimaryMotion": "None", "SuperimposedMotion": "None",
                                        "IncludedInStatics": "None", "InitialHeading": 0}}],
            "lines": [{"name": "riser", "line_type_refs": [r[0] for r in sections], "properties": {
                "IncludeTorsion": "No", "TopEnd": "End A", "LengthAndEndOrientations": "Explicit", "Representation": "Finite element",
                "EndAConnection": "floater", "EndAX": 0, "EndAY": 0, "EndAZ": g["hangoff_m"][2],
                "EndBConnection": "Fixed", "EndBX": g["anchor_m"][0], "EndBY": 0, "EndBZ": g["anchor_m"][2],
                "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [[0, None], [0, None]],
                "LineType, Length, TargetSegmentLength": sections,
                "ContentsMethod": "Uniform", "ContentsDensity": c["properties"]["contents_density_kg_m3"]/1000,
                "ContentsPressureRefZ": 0, "ContentsPressure": float(scope["design_pressure_MPa_g"])*1000,
                "IncludedInStatics": "Yes", "StaticsStep1": "Catenary", "StaticsStep2": "Full statics"}}]}})
    context = BuilderContext()
    generic = GenericModelBuilder(spec, context).build()
    data = {"General": generic.pop("General"), **EnvironmentBuilder(spec, context).build(), **generic}
    if findings := PostGenerationValidator().validate_data({"model.yml": data}):
        raise ValueError(f"native reference validation failed: {findings}")
    return data
