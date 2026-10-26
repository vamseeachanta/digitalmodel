# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Prepare reproducible SCR and lazy-wave inputs without a licensed interface."""

import argparse
import hashlib
import io
import json
import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.orcaflex.riser_config import calculate_weight_in_water
from digitalmodel.riser_configuration_screen.static import lazy_wave_offsets, scr_offsets
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import EnvironmentBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import GenericModelBuilder
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexDumper

from .validate_manifest import BASIS, CAMPAIGN, load_manifest, unique_object, validate_manifest


RAO_COLUMNS = "RAOPeriodOrFrequency, " + ", ".join(
    f"RAO{dof}{field}" for dof in ("Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw")
    for field in ("Amp", "Phase")
)
HEADER = "# SCREENING INPUT: all engineering parameters and builder defaults are ASSUMED.\n# Native loading, equilibrium and dynamic utilization are not established.\n"


def geometry(case: dict[str, Any], manifest: dict[str, Any]) -> list[dict[str, Any]]:
    """Reuse fixed-length configuration algorithms and enforce campaign criteria.

    Args:
        case: Validated case row; hang-off is at mean sea level.
        manifest: Validated settings and assumed material properties.

    Returns:
        Per-offset geometry in metres and forces in newtons.

    Raises:
        ValueError: Any shape, buoyancy or hang-off criterion is violated.
    """
    pipe, settings = case["pipe"], manifest["settings"]
    weight = calculate_weight_in_water(
        pipe["outer_diameter_m"], pipe["wall_thickness_m"],
        steel_density=pipe["steel_density_kg_m3"],
        seawater_density=settings["seawater_density_kg_m3"],
        contents_density=pipe["contents_density_kg_m3"],
    ) * settings["gravity_m_s2"] / 9.80665
    if weight <= 0:
        raise ValueError("bare riser must have positive submerged weight")
    height = case["water_depth_m"] - pipe["outer_diameter_m"]/2
    configuration = {"water_depth_m": height, "submerged_weight_n_m": weight,
                     "grounded_length_m": case["grounded_length_m"], "hangoff_angle_deg": case["hangoff_angle_deg"]}
    if case["buoyancy"]:
        configuration.update(case["buoyancy"])
    solver = scr_offsets if case["configuration"] == "SCR" else lazy_wave_offsets
    rows: list[dict[str, Any]] = solver(configuration, [o["x_m"] for o in case["offset_cases"]])
    total = rows[0]["suspended_length_m"] + rows[0]["grounded_length_m"]
    tolerance = settings["closure_tolerance_m"]
    for row in rows:
        errors = [row["vertical_closure_error_m"], row["horizontal_closure_error_m"],
                  row["suspended_length_m"] + row["grounded_length_m"] - total]
        if any(not math.isfinite(v) or abs(v) > tolerance for v in errors):
            raise ValueError("geometry closure or installed length criterion failed")
        if row["grounded_length_m"] <= 0 or not 0 < row["top_angle_deg"] < settings["hangoff_angle_limit_deg"]:
            raise ValueError("grounded tail or hang-off criterion failed")
        if case["buoyancy"]:
            start, length = row["buoyed_start_m"], row["buoyed_length_m"]
            fixed = [start - rows[0]["buoyed_start_m"], length - rows[0]["buoyed_length_m"]]
            if any(abs(v) > tolerance for v in fixed) or not 0 < start < start + length < row["suspended_length_m"]:
                raise ValueError("fixed buoyancy material coordinates criterion failed")
            if row["hog_clearance_m"] <= 0:
                raise ValueError("hog clearance criterion failed")
    return rows


def _line_types(case: dict[str, Any], manifest: dict[str, Any]) -> list[dict[str, Any]]:
    pipe, settings = case["pipe"], manifest["settings"]
    outer, wall = pipe["outer_diameter_m"], pipe["wall_thickness_m"]
    inner = outer - 2 * wall
    area = math.pi * (outer**2 - inner**2) / 4
    inertia = math.pi * (outer**4 - inner**4) / 64
    bare = {"name": "pipe", "category": "General", "outer_diameter": outer, "inner_diameter": inner,
            "mass_per_length": area * pipe["steel_density_kg_m3"] / 1000,
            "axial_stiffness": pipe["youngs_modulus_pa"] * area / 1000,
            "bending_stiffness": pipe["youngs_modulus_pa"] * inertia / 1000,
            "properties": {"StressOD": outer, "StressID": inner, "PoissonRatio": pipe["poisson_ratio"],
                           "Cd": [settings["drag_coefficient"], 0],
                           "Ca": [settings["added_mass_coefficient"], 0],
                           "SeabedLateralFrictionCoefficient": 0, "SeabedAxialFrictionCoefficient": 0}}
    result = [bare]
    if case["buoyancy"]:
        seawater, gravity = settings["seawater_density_kg_m3"], settings["gravity_m_s2"]
        mass = area * pipe["steel_density_kg_m3"] + math.pi * inner**2 / 4 * pipe["contents_density_kg_m3"]
        weight = (mass - math.pi * outer**2 / 4 * seawater) * gravity
        buoyancy = case["buoyancy"]
        volume = weight * buoyancy["buoyancy_factor"] / ((seawater - buoyancy["module_density_kg_m3"]) * gravity)
        result.append({**bare, "name": "buoyed-pipe", "outer_diameter": math.sqrt(outer**2 + 4*volume/math.pi),
                       "mass_per_length": bare["mass_per_length"] + volume*buoyancy["module_density_kg_m3"]/1000})
    return result


def _vessel_type(case: dict[str, Any], manifest: dict[str, Any]) -> dict[str, Any]:
    rao = next(r for r in manifest["rao_sets"] if r["id"] == case["rao_set_id"])
    table = [[r["period_s"], *[v for pair in zip(r["amplitudes"], r["phases_deg"]) for v in pair]] for r in rao["rows"]]
    return {"name": "parametric-floater", "length": case["principal_particulars"]["length_m"],
            "properties": {"RAOResponseUnits": "degrees", "RAOWaveUnit": "amplitude",
                           "WavesReferredToBy": "period (s)", "RAOPhaseConvention": "lags",
                           "RAOPhaseUnitsConvention": "degrees", "RAOPhaseRelativeToConvention": "crest",
                           "SurgePositive": "forward", "SwayPositive": "port", "HeavePositive": "up",
                           "RollPositiveStarboard": "down", "PitchPositiveBow": "down", "YawPositiveBow": "port",
                           "Symmetry": "xz plane", "Draughts": [{"Name": "screening-draught",
                               "DisplacementRAOs": {"RAOOrigin": [0, 0, 0], "PhaseOrigin": [0, 0, 0],
                                                    "RAOs": [{"RAODirection": rao["heading_deg"], RAO_COLUMNS: table}]}}]}}


def model_data(case: dict[str, Any], manifest: dict[str, Any], rows: list[dict[str, Any]],
               offset_index: int, sea_index: int) -> dict[str, Any]:
    """Build native YAML data through existing typed generic/environment builders.

    Args:
        case: Validated manifest row.
        manifest: Campaign RAOs and assumed settings.
        rows: Geometry checked by the existing fixed-length algorithms.
        offset_index: Manifest offset index; positive means increased radial span.
        sea_index: Manifest regular-wave state index.

    Returns:
        Reference-checked native model mapping; solver loading remains unverified.
    """
    settings, baseline = manifest["settings"], rows[0]
    total = baseline["suspended_length_m"] + baseline["grounded_length_m"]
    sections = [["pipe", total, settings["segment_length_m"]]]
    if case["buoyancy"]:
        start, length = baseline["buoyed_start_m"], baseline["buoyed_length_m"]
        sections = [["pipe", start, settings["segment_length_m"]],
                    ["buoyed-pipe", length, settings["segment_length_m"]],
                    ["pipe", total - start - length, settings["segment_length_m"]]]
    offset, state = case["offset_cases"][offset_index], case["sea_states"][sea_index]
    current = case["current_profile"]
    spec = ProjectInputSpec.model_validate({
        "metadata": {"name": case["case_id"], "description": "ASSUMED riser campaign input; native loading unverified",
                     "structure": "generic", "operation": "production"},
        "environment": {
            "water": {"depth": case["water_depth_m"], "density": settings["seawater_density_kg_m3"]/1000},
            "seabed": {"stiffness": {"normal": settings["seabed_stiffness_kn_m3"],
                                     "shear": settings["seabed_stiffness_kn_m3"]}, "friction_coefficient": 0},
            "waves": {"trains": [{"type": "airy", "height": state["wave_height_m"],
                                   "period": state["period_s"], "direction": state["direction_deg"]}]},
            "current": {"speed": current["surface_speed_m_s"], "direction": current["direction_deg"],
                        "profile": [[p["depth_fraction"]*case["water_depth_m"], p["factor"]] for p in current["points"]]},
            "wind": {"speed": 0}},
        "generic": {
            "general_properties": {"UnitsSystem": "SI", "g": settings["gravity_m_s2"],
                                   "StageDuration": settings["stage_duration_s"], "DynamicsSolutionMethod": "Implicit time domain",
                                   "ImplicitUseVariableTimeStep": False, "ImplicitConstantTimeStep": settings["time_step_s"]},
            "line_types": _line_types(case, manifest), "vessel_types": [_vessel_type(case, manifest)],
            "vessels": [{"name": "floater", "vessel_type": "parametric-floater", "connection": "Free",
                         "initial_position": [-offset["x_m"], 0, 0], "length": case["principal_particulars"]["length_m"],
                         "properties": {"Draught": "screening-draught", "PrimaryMotion": "None", "SuperimposedMotion": "RAOs",
                                        "IncludedInStatics": "None", "InitialHeading": 0}}],
            "lines": [{"name": "riser", "line_type_refs": [r[0] for r in sections], "properties": {
                "IncludeTorsion": "No", "TopEnd": "End A", "LengthAndEndOrientations": "Explicit",
                "Representation": "Finite element", "EndAConnection": "floater", "EndAX": 0, "EndAY": 0, "EndAZ": 0,
                "EndBConnection": "Fixed", "EndBX": baseline["horizontal_span_m"] + baseline["grounded_length_m"],
                "EndBY": 0, "EndBZ": -case["water_depth_m"] + case["pipe"]["outer_diameter_m"]/2,
                "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [[0, None], [0, None]],
                "LineType, Length, TargetSegmentLength": sections,
                "ContentsMethod": "Uniform", "ContentsDensity": case["pipe"]["contents_density_kg_m3"]/1000,
                "IncludedInStatics": "Yes", "StaticsStep1": "Catenary", "StaticsStep2": "Full statics"}}]},
    })
    context = BuilderContext()
    generic = GenericModelBuilder(spec, context).build()
    data = {"General": generic.pop("General"), **EnvironmentBuilder(spec, context).build(), **generic}
    if findings := PostGenerationValidator().validate_data({"model.yml": data}):
        raise ValueError(f"native reference validation failed: {findings}")
    return data


def _json_bytes(data: dict[str, Any]) -> bytes:
    return (json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + "\n").encode("utf-8")


def _write(root: Path, relative: str, payload: bytes) -> dict[str, Any]:
    path = root / relative
    if any(parent.is_symlink() for parent in [path, *path.parents]):
        raise ValueError("output symlink is not permitted")
    digest = hashlib.sha256(payload).hexdigest()
    if path.exists():
        if hashlib.sha256(path.read_bytes()).hexdigest() != digest:
            raise ValueError(f"existing output digest differs: {relative}")
    else:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("xb") as stream:
            stream.write(payload)
    return {"path": relative, "sha256": digest, "bytes": len(payload)}


def generate(manifest_path: Path | str, output_dir: Path | str) -> dict[str, Any]:
    """Write deterministic inputs and per-file hashes; preserve unexpected data.

    Args:
        manifest_path: Strict campaign JSON path.
        output_dir: Explicit output directory; matching files are reused.

    Returns:
        Digest-bound output index with one row per manifest case.

    Raises:
        ValueError: Invalid inputs, geometry, symlinks or mismatched existing data.
    """
    manifest_payload = Path(manifest_path).read_bytes()
    schema_payload = (CAMPAIGN / "manifest.schema.json").read_bytes()
    basis_payload = BASIS.read_bytes()
    data = json.loads(manifest_payload, object_pairs_hook=unique_object)
    frozen_payloads = None
    if "frozen_tranche" in data:
        from .frozen_scope import source_payloads
        frozen_payloads = source_payloads()
    validate_manifest(data, json.loads(schema_payload, object_pairs_hook=unique_object), basis_payload, frozen_payloads)
    output = Path(output_dir).absolute()
    if any(parent.is_symlink() for parent in [output, *output.parents]):
        raise ValueError("output symlink is not permitted")
    index = {"version": 1, "criteria_version": data["criteria_version"], "basis": "ASSUMED",
             "intended_use": data["intended_use"], "licensed_run_status": "NOT_RUN",
             "manifest_sha256": hashlib.sha256(manifest_payload).hexdigest(),
             "schema_sha256": hashlib.sha256(schema_payload).hexdigest(), "cases": []}
    index["basis_sha256"] = hashlib.sha256(basis_payload).hexdigest()
    # Geometry is checked for the complete campaign before any output is written.
    geometries = [geometry(case, data) for case in data["cases"]]
    frozen_geometries = []
    if "frozen_tranche" in data:
        from .frozen_scope import prepare_geometry
        from .frozen_scope import CRITERIA
        frozen_criteria = json.loads(frozen_payloads[CRITERIA])
        frozen_geometries = [prepare_geometry(row, frozen_criteria) for row in data["frozen_tranche"]["rows"]]
    index["manifest"] = _write(output, "manifest.json", manifest_payload)
    index["schema"] = _write(output, "manifest.schema.json", schema_payload)
    index["basis"] = _write(output, "basis.md", basis_payload)
    for case, rows in zip(data["cases"], geometries):
        record = {"case_id": case["case_id"], "configuration": case["configuration"], "inputs": []}
        record["geometry"] = _write(output, f"{case['case_id']}/geometry.json", _json_bytes({
            "basis": "ASSUMED", "criterion": data["criteria_version"], "units": "length m; force N; angle deg; curvature 1/m",
            "offsets": rows, "status": "ANALYTICAL_GEOMETRY_ONLY"}))
        for offset_index, offset in enumerate(case["offset_cases"]):
            for sea_index, state in enumerate(case["sea_states"]):
                model = model_data(case, data, rows, offset_index, sea_index)
                stream = io.StringIO()
                stream.write(HEADER)
                # Native YAML cannot contain aliases. Copy the value tree before
                # serialization so shared builder lists cannot introduce them.
                tree = json.loads(json.dumps(model, allow_nan=False))
                yaml.dump(tree, stream, Dumper=OrcaFlexDumper, sort_keys=False, width=1000, allow_unicode=True)
                relative = f"{case['case_id']}/{offset['id']}--{state['id']}.yml"
                file = _write(output, relative, stream.getvalue().encode("utf-8"))
                record["inputs"].append({**file, "offset_id": offset["id"], "sea_state_id": state["id"]})
        index["cases"].append(record)
    if frozen_payloads is not None:
        index["frozen_tranche"] = _frozen_inputs(output, data["frozen_tranche"], frozen_payloads, frozen_geometries)
    _write(output, "index.json", _json_bytes(index))
    return index


def _frozen_inputs(output: Path, tranche: dict[str, Any], payloads: dict[str, bytes],
                   geometries: list[dict[str, Any]]) -> dict[str, Any]:
    """Write the frozen tranche with the same serializer and digest guard.

    Governing QA cannot be selected before completed native responses. Those
    calculations are recorded as pending, never counted as written inputs.
    """
    from .frozen_scope import CRITERIA, VERSION, expand_loads, native_model, section_properties, validate_loads
    criteria = json.loads(payloads[CRITERIA])
    loads = expand_loads(tranche, criteria)
    validate_loads(tranche, loads, criteria)
    index = {"criteria_version": VERSION, "scope_sha256": tranche["scope_sha256"],
             "criteria_sha256": tranche["criteria_sha256"], "cases": [],
             "written_counts": {"statics": 0, "dynamics": 0, "fatigue": 0},
             "phase_a_required": criteria["counts"]["phase_a"],
             "pending_governing_qa": criteria["counts"]["pending_governing_qa"],
             "qa_status": criteria["qa"]["status"],
             "snapshots": [_write(output, name, payload) for name, payload in payloads.items()],
             "code_sources": []}
    source_paths = ["scripts/riser_campaign/generate_inputs.py", "scripts/riser_campaign/validate_manifest.py",
                    "scripts/riser_campaign/frozen_scope.py", "src/digitalmodel/riser_configuration_screen/static.py",
                    "src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py",
                    "src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py",
                    "src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py",
                    "src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py",
                    "src/digitalmodel/solvers/orcaflex/modular_generator/schema/__init__.py",
                    "src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py",
                    "src/digitalmodel/solvers/orcaflex/yaml_utils.py",
                    "docs/domains/orcaflex/library/campaigns/generic-scr-slwr/basis.yml",
                    "src/digitalmodel/materials/grades.py"]
    for relative in source_paths:
        entry = _write(output, "source/" + relative, (CAMPAIGN.parents[1] / relative).read_bytes())
        index["code_sources"].append(entry)
    for scope, geometry in zip(tranche["rows"], geometries):
        case_id = scope["case_id"]
        record = {"case_id": case_id, "inputs": []}
        record["geometry"] = _write(output, f"{case_id}/geometry.json", _json_bytes(geometry))
        record["section"] = _write(output, f"{case_id}/section.json", _json_bytes(section_properties(scope, criteria)))
        for load in (r for r in loads if r["case_id"] == case_id):
            model = native_model(scope, load, geometry, criteria)
            stream = io.StringIO()
            stream.write("# UNRELEASED SCREENING INPUT: native QA and readback required.\n")
            if load["kind"] != "statics":
                stream.write("# BLOCKED: qualified heading/band motions and matching equilibrium required; motions absent.\n")
            tree = json.loads(json.dumps(model, allow_nan=False))
            yaml.dump(tree, stream, Dumper=OrcaFlexDumper, sort_keys=False, width=1000, allow_unicode=True)
            file = _write(output, f"{case_id}/{load['id']}.yml", stream.getvalue().encode("utf-8"))
            blockers = ["NATIVE_INPUT_READBACK_REQUIRED", "CONTACT_CENTRELINE_READBACK_REQUIRED"]
            if load["kind"] != "statics":
                blockers += ["RAO_CLEARANCE_REQUIRED", "MATCHING_CONVERGED_EQUILIBRIUM_REQUIRED"]
            record["inputs"].append({**load, **file, "qualification": "NOT_RUN", "release_blockers": blockers})
            index["written_counts"][load["kind"]] += 1
        index["cases"].append(record)
    index["load_ledger"] = _write(output, "frozen-loads.json", _json_bytes({"criteria_version": VERSION, "loads": loads}))
    return index


def main() -> None:
    """Generate inputs and print every case with units and qualification status."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=CAMPAIGN / "manifest.json")
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    data = load_manifest(args.manifest)
    print(f"Start: {len(data['cases'])} manifest cases; 0 licensed runs")
    index = generate(args.manifest, args.output_dir)
    print("case | floater | size | depth (m) | configuration | inputs | basis")
    for case, record in zip(data["cases"], index["cases"]):
        print(f"{case['case_id']} | {case['floater_type']} | {case['size_class']} | {case['water_depth_m']} | "
              f"{case['configuration']} | {len(record['inputs'])} | ASSUMED")
    count = sum(len(r["inputs"]) for r in index["cases"])
    print(f"End: {len(index['cases'])} cases; {count} solver inputs; 0 licensed runs")
    if "frozen_tranche" in index:
        frozen = index["frozen_tranche"]
        print(f"Frozen tranche: {len(frozen['cases'])} cases; written {frozen['written_counts']}; "
              f"Phase A required {frozen['phase_a_required']}; pending governing QA {frozen['pending_governing_qa']}")


if __name__ == "__main__":
    main()
