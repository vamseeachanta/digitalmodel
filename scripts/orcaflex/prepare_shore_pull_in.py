# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Prepare surface pull-in scenarios and structural inputs without a solver."""

from __future__ import annotations

import argparse
import copy
import hashlib
import html
import json
import math
from pathlib import Path
import re
from typing import Any

from digitalmodel.marine_ops.installation.shore_approach import RouteSegment, pull_in
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

import yaml

from digitalmodel.marine_ops.installation.shore_pull_in_provenance import (
    STUDY_PARAMETER_INPUTS, STUDY_PARAMETER_QUALIFICATION, audit_input_sources,
    numeric_paths as numeric_paths, source_locator,
)


DELIVERABLES = ("models.json", "analysis.html", "manifest.template.json", "preparation.json")

class PreparationDumper(yaml.SafeDumper):
    """Preserve the repository's native null spelling."""


PreparationDumper.add_representer(type(None), lambda dumper, value: dumper.represent_scalar("tag:yaml.org,2002:null", "~"))


def digest(path: Path) -> str:
    """Hash the actual committed bytes, including the final newline."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, value: dict) -> None:
    """Write deterministic UTF-8 JSON with one final newline."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")




def weight(basis: dict) -> float:
    """Compute empty coated-pipe weight using traced density and annular areas."""
    pipe = basis["pipeline"]
    diameter = pipe["dimensions"]["outer_diameter"]
    wall = pipe["dimensions"]["wall_thickness"]
    mass = basis["steel_density_te_m3"] * math.pi * wall * (diameter - wall)
    for coat in [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]]:
        outer = diameter + 2 * coat["thickness"]
        mass += coat["density"] * math.pi / 4 * (outer**2 - diameter**2)
        diameter = outer
    return float((mass - basis["water_density_te_m3"] * math.pi / 4 * diameter**2) * basis["gravity_m_s2"])


def analytical_pull(basis: dict, case: dict) -> float:
    """Compute one-segment pull from annular geometry, independently of pull_in.

    This work-energy check integrates friction over horizontal projection and
    net gravitational work over the rise. It does not call the method or weight.
    """
    pipe = basis["pipeline"]
    outer = pipe["dimensions"]["outer_diameter"]
    inner = outer - 2 * pipe["dimensions"]["wall_thickness"]
    steel_area = math.pi * (outer + inner) * (outer - inner) / 4
    mass = steel_area * basis["steel_density_te_m3"]
    for coat in [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]]:
        new_outer = outer + 2 * coat["thickness"]
        mass += math.pi * coat["thickness"] * (outer + coat["thickness"]) * coat["density"]
        outer = new_outer
    displaced_mass = math.pi * outer * outer * basis["water_density_te_m3"] / 4
    effective = (mass - displaced_mass) * basis["gravity_m_s2"] * (1 - case["buoyancy_fraction"])
    length = pipe["segments"][0]["length"]
    horizontal = math.sqrt((length - case["rise_m"]) * (length + case["rise_m"]))
    return float(basis["tail_tension_kn"] + effective * (case["friction"] * horizontal + case["rise_m"]))


def report(table: dict, records: list[dict]) -> str:
    """Build the self-contained analysis skeleton and revision history from data."""
    escape = html.escape
    revisions = "".join(f'<tr><td>{escape(r["revision"])}</td><td>{escape(r["date"])}</td><td>{escape(r["change"])}</td></tr>' for r in table["revisions"])
    cases = {c["id"]: c for c in table["cases"]}
    length = table["basis"]["pipeline"]["segments"][0]["length"]
    rows = "".join(f'<tr><td>{r["id"]}</td><td>{cases[r["id"]]["friction"]:.5f}</td><td>{cases[r["id"]]["rise_m"]}</td><td>{cases[r["id"]]["buoyancy_fraction"]}</td><td>{r["submerged_weight_kn_m"]:.4f}</td><td>{length}</td><td>{r["method"]["pipe_head_tension_kn"]:.1f}</td><td>{r["analytical_pull_kn"]:.1f}</td><td>{r["method"]["winch_demand_kn"]:.1f}</td><td>Not Evaluated</td></tr>' for r in records)
    public_limits = [re.sub(r"https://github.com/[^/]+/[^/]+/issues/([0-9]+)", r"#\1", s)
                     for s in table["known_limits"]]
    limits = "".join(f"<li>{escape(s)}</li>" for s in public_limits)
    sources = "".join(f'<li>{escape(source_locator(s))} — <a href="{escape(s["url"], quote=True)}">Source</a>; retrieved {escape(s["retrieval_date"])}; SHA-256 {s["sha256"]}</li>' for s in table["sources"].values())
    geometry_rows = []
    for reference in table["geometry_basis"].values():
        link = (f' <a href="{escape(reference["url"], quote=True)}">Official reference</a>.'
                if "url" in reference else "")
        if reference["evidence_status"] == "retrieved":
            evidence = (f'Retrieved {escape(reference["retrieval_date"])}; '
                        f'SHA-256 {reference["sha256"]}')
        else:
            evidence = (f'Primary text unavailable; attempted {escape(reference["attempt_date"])}. '
                        f'{escape(reference["reason"])} {escape(reference["basis"])}')
        geometry_rows.append(f'<li>{escape(reference.get("citation", ""))} '
                             f'{escape(reference["locator"])}{link} {evidence} '
                             f'{escape(reference["qualification"])} '
                             f'{escape(reference.get("derivation", ""))}</li>')
    geometry = "".join(geometry_rows)
    assumptions = "".join(f'<tr><td>{escape(r["input"])}</td><td>{r["value"]} {escape(r["unit"])}</td><td>{escape(r["label"])}</td><td>{escape(r["basis"])}</td></tr>' for r in table["assumed_inputs"])
    study_rows = []
    for path in STUDY_PARAMETER_INPUTS:
        entry = table["input_sources"][path]
        if entry.get("qualification") != STUDY_PARAMETER_QUALIFICATION:
            continue
        value = table if path.startswith("cases.") else table["basis"]
        for part in path.split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        study_rows.append(f'<tr><td>{escape(path)}</td><td>{value}</td><td>{escape(entry["qualification"])}</td><td>{escape(entry["basis"])}</td></tr>')
    study_parameters = "".join(study_rows)
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Shore pull-in analysis</title>
<style>body{{font:16px sans-serif;max-width:70rem;margin:2rem auto;padding:1rem}}table{{border-collapse:collapse}}td,th{{border:1px solid #888;padding:.4rem}}code{{overflow-wrap:anywhere}}</style></head><body>
<h1>Shore pull-in analysis</h1><p>Revision {escape(table["revision"])}</p>
<section id="summary"><h2>Summary</h2><p>Three generic surface pull-in scenarios are prepared. Pull forces are analytical, not native solver results. Installation acceptance is Not Evaluated. The next step is source and topology qualification before native execution.</p></section>
<section id="revision-history"><h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>{revisions}</table></section>
<section id="assumed-inputs"><h2>Assumed pipe inputs</h2><p>These five properties are not taken from a public source.</p><table><tr><th>Input</th><th>Value with unit</th><th>Label</th><th>Basis</th></tr>{assumptions}</table></section>
<section id="study-parameters"><h2>Declared study parameters</h2><table><tr><th>Input</th><th>Value</th><th>Qualification</th><th>Reason</th></tr>{study_parameters}</table></section>
<section id="method"><h2>Method and criterion</h2><p>For each straight supported segment: net weight w equals submerged weight minus buoyancy aid force per length; sin(theta) = rise / arc length. Normal load N = w L cos(theta), friction = mu N, gravity = w rise. Head tension is trailing tension plus the ordered force increments. Winch demand is head tension divided by mechanical efficiency. Loss of contact and compression are rejected. Equivalent axial friction follows the cited soil paper; gravity resolution is a straight-segment work balance.</p><p>The independent check uses separately computed annular areas and horizontal projection. Agreement criterion: relative difference no greater than the force-balance tolerance in criteria.json. All prepared cases meet that computational identity; this is not physical validation. Shore winch capacity is unassigned.</p></section>
<h2>Prepared results</h2><table><tr><th>Case</th><th>Friction</th><th>Rise (m)</th><th>Buoyancy fraction</th><th>Submerged weight (kN/m)</th><th>Length (m)</th><th>Method pull (kN)</th><th>Independent pull (kN)</th><th>Winch demand (kN)</th><th>Native result</th></tr>{rows}</table>
<h2>Native analysis skeleton</h2><p>Native model version, measured timing, effective tension, bending moment, curvature, contact reactions, final geometry, convergence and acceptance: Not Evaluated. The first timing case is level-initial. No extrapolated runtime is recorded.</p>
<h2>Limits and way forward</h2><ul>{limits}</ul><p>Assign traced pipe and winch properties; qualify the end connections, buoyancy representation and contacts; approve a manifest digest and native comparison criteria; then measure the first native case before a campaign run.</p>
<h2>Geometry basis</h2><ul>{geometry}</ul><h2>Sources</h2><ul>{sources}</ul></body></html>
'''


def audit_geometry(table: dict) -> None:
    """Admit only the ruled, one-segment, surface-origin S-lay seed."""
    basis = table["basis"]
    pipe = basis["pipeline"]
    if len(pipe["segments"]) != 1:
        raise ValueError("Preparation requires exactly one segment")
    depth = basis["water_depth_m"]
    length = pipe["segments"][0]["length"]
    draft = basis["equipment"]["vessel"]["properties"]["draft"]
    minimum = float(table["public_dataset"]["columns"]["PIPELAY_MIN_WATER_DEPTH_M"])
    radius = pipe["dimensions"]["outer_diameter"] / 2 + sum(
        coat["thickness"] for coat in [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]])
    values = [depth, length, draft, minimum, radius, *(case["rise_m"] for case in table["cases"])]
    if any(isinstance(v, bool) or not math.isfinite(v) for v in values):
        raise ValueError("Geometry inputs must be finite numbers")
    # Depth/draft ratio 1.3 is assumed, based on previous experience.
    # The unavailable PIANC primary text is not evidence for this criterion.
    if depth <= 0 or minimum <= 0 or draft <= 0 or depth < max(minimum, 1.3 * draft):
        raise ValueError("Hull clearance requires depth >= max(minimum lay depth, 1.3 * draft)")
    if length <= depth:
        raise ValueError("Supported length must exceed depth")
    if math.hypot(length - depth, depth - radius) > length:
        raise ValueError("S-lay seed chord exceeds pipe length")
    if any(not 0 <= case["rise_m"] <= depth for case in table["cases"]):
        raise ValueError("Every case rise must lie between zero and depth")


def prepare(campaign: Path, output: Path) -> dict:
    """Evaluate all analytical checks before generating solver-free validator-checked model bundles.

    Model folders are scratch outputs. The reproducible JSON bundle retains
    every YAML component and its byte digest without committing case folders.
    """
    if output.exists() and any(output.iterdir()):
        raise ValueError("An empty output directory is required; existing data are preserved")
    table = json.loads((campaign / "case-table.json").read_text(encoding="utf-8"))
    criteria = json.loads((campaign / "criteria.json").read_text(encoding="utf-8"))
    evidence = table["criteria_evidence"]
    if evidence["sha256"] != digest(campaign / "criteria.json"):
        raise ValueError("Criteria digest mismatch")
    if evidence["recorded_utc"] >= evidence["first_generation_utc"]:
        raise ValueError("Recorded criteria time must precede recorded first generation time")
    if table["revisions"][-1]["revision"] != table["revision"] or criteria["revision"] != table["revision"]:
        raise ValueError("Revision records must match the declared revision")
    audit_geometry(table)
    assumed_inputs = audit_input_sources(table)
    provenance = {"public_dataset": table["public_dataset"], "input_sources": table["input_sources"],
                  "sources": table["sources"], "assumed_inputs": assumed_inputs,
                  "geometry_basis": table["geometry_basis"]}
    identities = [c["id"] for c in table["cases"]]
    if not identities or len(set(identities)) != len(identities) or any(not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", c) for c in identities):
        raise ValueError("Invalid case identity")
    records, specs = evaluate_cases(table, criteria)
    bundle = generate_models(table, output, records, specs, provenance)
    return write_deliverables(campaign, output, table, criteria, provenance, records, bundle)


def evaluate_cases(table: dict, criteria: dict) -> tuple[list, list]:
    """Evaluate head tension and demand before any model generation."""
    basis = table["basis"]
    records = []
    specs = []
    for case in table["cases"]:
        w = weight(basis)
        segment = RouteSegment(basis["pipeline"]["segments"][0]["length"], w, case["friction"], case["rise_m"], w * case["buoyancy_fraction"])
        result = pull_in([segment], tail_tension_kn=basis["tail_tension_kn"], efficiency=basis["winch_efficiency"])
        independent = analytical_pull(basis, case)
        demand = independent / basis["winch_efficiency"]
        error = max(abs(result["pipe_head_tension_kn"] - independent) / max(abs(independent), 1),
                    abs(result["winch_demand_kn"] - demand) / max(abs(demand), 1))
        if error > criteria["force_balance_relative_tolerance"]:
            raise ValueError("Independent analytical force check failed")
        records.append({"id": case["id"], "method": result, "analytical_pull_kn": independent,
                        "analytical_relative_error": error, "submerged_weight_kn_m": w,
                        "native_status": "not_run", "native_wall_seconds": None})
        equipment = copy.deepcopy(basis["equipment"])
        equipment["tensioner"] = {"capacity_kn": max(result["winch_demand_kn"], 1), "tension_value": result["pipe_head_tension_kn"]}
        # This schema capacity is a placeholder equal to calculated demand,
        # never a capacity measurement or an analytical capacity assessment.
        specs.append(ProjectInputSpec.model_validate({
            "metadata": {"name": case["id"], "structure": "pipeline", "operation": "installation/shore-pull-in", "description": "Generic surface pull-in structural preparation"},
            "environment": {"water": {"depth": basis["water_depth_m"], "density": basis["water_density_te_m3"]}, "seabed": {**basis["seabed"], "friction_coefficient": case["friction"]}},
            "pipeline": basis["pipeline"], "equipment": equipment}))
    return records, specs


def generate_models(table: dict, output: Path, records: list, specs: list, provenance: dict) -> dict:
    """Generate models and retain components checked by the solver-free validator."""
    output.mkdir(parents=True, exist_ok=True)
    bundle: dict = {"schema": "shore-pull-in-model-bundle-1", "provenance": provenance, "cases": {}}
    for record, spec, case in zip(records, specs, table["cases"]):
        folder = output / "models" / record["id"]
        ModularModelGenerator.from_spec(spec).generate(folder)
        pin_surface_seed(table, folder)
        # Preparation does not assign standard design factors or strain limits.
        # Remove optional design-check settings emitted by the shared builder;
        # retain structural properties and run the unmodified validator below.
        types_path = folder / "includes/05_line_types.yml"
        types_data = yaml.safe_load(types_path.read_text(encoding="utf-8"))
        pipe_types = {segment.type for segment in spec.pipeline.segments}
        for line_type in types_data["LineTypes"]:
            if line_type["Name"] in pipe_types:
                # The seabed schema coefficient is not emitted by a builder.
                # Set both directions to the case's scalar preparation factor;
                # directional soil behaviour remains unqualified.
                line_type["SeabedAxialFrictionCoefficient"] = case["friction"]
                line_type["SeabedLateralFrictionCoefficient"] = case["friction"]
            for key in list(line_type):
                if key.startswith("DNVSTF101"):
                    del line_type[key]
        types_path.write_text(yaml.dump(types_data, Dumper=PreparationDumper, sort_keys=False), encoding="utf-8")
        canonicalize_surface_components(folder, spec)
        for path in sorted(folder.rglob("*.yml")):
            path.write_text(path.read_text(encoding="utf-8").rstrip() + "\n", encoding="utf-8")
        findings = PostGenerationValidator().validate_directory(folder / "includes")
        if any(f.level == "error" for f in findings):
            raise ValueError("Post-generation validation failed")
        files = {p.relative_to(folder).as_posix(): p.read_text(encoding="utf-8") for p in sorted(folder.rglob("*.yml"))}
        bundle["cases"][record["id"]] = files
        record["files"] = {name: hashlib.sha256(content.encode()).hexdigest() for name, content in files.items()}
        record["validator_errors"] = 0
        record["validator_warnings"] = [f.category for f in findings]
    return bundle


def pin_surface_seed(table: dict, folder: Path) -> None:
    """Set the admitted surface-origin seed explicitly, before validation.

    The preparation owns these endpoint and stiffness settings rather than
    inheriting changing shared-generator defaults. This is an unsolved seed;
    the geometry audit screens its chord before any model is written.
    """
    basis = table["basis"]
    pipe = basis["pipeline"]
    depth = basis["water_depth_m"]
    span = pipe["segments"][0]["length"] - depth
    radius = pipe["dimensions"]["outer_diameter"] / 2 + sum(
        coat["thickness"] for coat in [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]])
    path = folder / "includes/07_lines.yml"
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    for index, line in enumerate(data["Lines"]):
        if line["Name"] != pipe["name"]:
            continue
        line["IncludeTorsion"] = "No"
        connection = next(key for key in line if key.startswith("Connection, ConnectionX"))
        line[connection] = [
            ["Free", 0.0, 0.0, 0, 180, 90, 0, None],
            ["Anchored", -float(span), 0.0, radius, 180, 90 + basis["seabed"]["slope"], 0, None],
        ]
        # Replace the stiffness column set in place to preserve component order.
        data["Lines"][index] = {
            ("ConnectionxBendingStiffness, ConnectionyBendingStiffness"
             if key.startswith("ConnectionxBendingStiffness,") else key):
            ([[], [0, None]] if key.startswith("ConnectionxBendingStiffness,") else value)
            for key, value in line.items()
        }
    path.write_text(yaml.dump(data, Dumper=PreparationDumper, sort_keys=False), encoding="utf-8")


def selector_first(value: Any) -> Any:
    """Normalize nested mappings with mode selectors before dependent fields.

    Each object uses the same fixed ordering regardless of the input schema's
    insertion order. Names identify objects before their properties are set;
    units and mode selectors precede values interpreted under those modes.
    """
    selectors = (
        "Name", "UnitsSystem", "LengthUnits", "MassUnits", "ForceUnits", "g",
        "Category", "Representation", "Geometry", "VesselType", "Connection",
        "PrimaryMotion", "SuperimposedMotion", "IncludedInStatics",
        "DynamicsSolutionMethod", "ImplicitUseVariableTimeStep",
        "VerticalDensityVariation", "SeabedType", "SeabedModel",
        "MultipleCurrentDataCanBeDefined", "CurrentModel",
        "VerticalCurrentVariationMethod", "WindType", "WaveType",
        "WaveCalculationMethod", "WinchType", "WinchControlType",
        "LengthAndEndOrientations", "IncludeTorsion", "PreBendSpecifiedBy",
    )
    if isinstance(value, list):
        return [selector_first(child) for child in value]
    if not isinstance(value, dict):
        return value
    order = [key for key in selectors if key in value]
    order.extend(sorted(key for key in value if key not in selectors))
    return {key: selector_first(value[key]) for key in order}


def canonicalize_surface_components(folder: Path, spec: ProjectInputSpec) -> None:
    """Retain one preparation format across the reviewed generator variants.

    Convert legacy hull and winch columns to the named-draught and staged
    tension forms. Remove undeclared default ramps and their group references.
    These corrections are local to the surface preparation; the shared
    generator and the post-generation validator remain unchanged.
    """
    paths = sorted((folder / "includes").glob("*.yml"))
    components = {path.name: yaml.safe_load(path.read_text(encoding="utf-8")) for path in paths}
    for hull in components["04_vessel_types.yml"]["VesselTypes"]:
        if "Displacement" in hull:
            mass = hull.pop("Displacement")[0]
            inertia = hull.pop("MomentsOfInertia")[0]
            for key in ("CalculationMethod", "WaveReferencePoint", "CentreOfGravityRelativeToCoG"):
                del hull[key]
            hull["Draughts"] = [{
                "Name": "Draught1", "Mass": mass,
                "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ":
                    [[inertia[0], 0, 0], [0, inertia[1], 0], [0, 0, inertia[2]]],
                "CentreOfMass": [0, 0, 0], "DisplacementRAOs": {"RAOOrigin": [0, 0, 0]},
            }]
    for hull in components["06_vessels.yml"]["Vessels"]:
        hull.pop("WaveCalculationMethod", None)
    shapes = components["09_shapes.yml"]["Shapes"]
    removed = {shape["Name"] for shape in shapes if shape["Name"] in {"Ramp inclined", "Ramp-curve"}}
    components["09_shapes.yml"]["Shapes"] = [shape for shape in shapes if shape["Name"] not in removed]
    groups = components["10_groups.yml"]["Groups"]["Structure"]
    for name in removed:
        groups.pop(name, None)
    stage_rows = len(components["01_general.yml"]["General"]["StageDuration"]) + 1
    for winch in components["11_winches.yml"]["Winches"]:
        # This one-segment, surface-origin preparation has no declared stinger.
        position = spec.equipment.tensioner.position_on_vessel or [10, 0, 0]
        for key in ("Connection", "ConnectionPoint", "ConnectionzRelativeTo", "StageMode",
                    "StageTension", "StageValue", "WireType", "StaticsStep1"):
            winch.pop(key, None)
        winch.update({
            "WinchType": "Simple", "WinchControlType": "By stage",
            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo":
                [[spec.equipment.vessel.name, *position], [spec.pipeline.name, 0, 0, 0, "End A"]],
            "StageMode, StageValue": [["Specified tension", spec.equipment.tensioner.tension_value]
                                      for _ in range(stage_rows)],
        })
    for path in paths:
        path.write_text(yaml.dump(selector_first(components[path.name]), Dumper=PreparationDumper, sort_keys=False), encoding="utf-8")
    # Supports precede vessels; groups follow every entity they reference.
    order = ("01_general.yml", "02_var_data.yml", "03_environment.yml", "04_vessel_types.yml",
             "05_line_types.yml", "13_supports.yml", "06_vessels.yml", "14_morison.yml",
             "09_shapes.yml", "07_lines.yml", "11_winches.yml", "10_groups.yml")
    master = folder / "master.yml"
    header, body = master.read_text(encoding="utf-8").split("---\n", 1)
    includes = yaml.safe_load(body)
    rank = {name: index for index, name in enumerate(order)}
    includes.sort(key=lambda item: (rank.get(Path(item["includefile"]).name, len(rank)), item["includefile"]))
    master.write_text(header + "---\n" + yaml.dump(includes, Dumper=PreparationDumper, sort_keys=False), encoding="utf-8")


def write_deliverables(campaign: Path, output: Path, table: dict, criteria: dict,
                       provenance: dict, records: list, bundle: dict) -> dict:
    """Retain model, analysis and launch-disabled manifest outputs."""
    evidence = table["criteria_evidence"]
    identities = [case["id"] for case in table["cases"]]
    write_json(output / "models.json", bundle)
    (output / "analysis.html").write_text(report(table, records), encoding="utf-8")
    native = {"schema": "shore-pull-in-native-manifest-1", "status": "not_run", "launch_enabled": False,
              "provenance": provenance, "case_table_sha256": digest(campaign / "case-table.json"),
              "first_timed_case": identities[0], "expected_cases": identities, "completed_cases": [],
              "qualified_results": 0, "native_version": None, "input_revision": None,
              "input_digest": digest(output / "models.json"), "approved_manifest_digest": None,
              "criteria_revision": criteria["revision"], "criteria_digest": evidence["sha256"],
              "timing": {"case_id": identities[0], "native_wall_seconds": None, "projected_campaign_seconds": None},
              "native_criteria": {"force_balance": None, "geometry": None, "mesh_convergence": None, "stress_limit": None},
              "required_metrics": {"effective_tension": "kN", "bending_moment": "kN.m", "curvature": "1/m", "contact_reactions": "kN", "geometry": "m", "native_wall_seconds": "s"},
              "release_conditions": ["qualified execution adapter", "approved manifest digest", "qualified topology and sources", "assigned native criteria", "timed canary read back", "independent comparison and convergence"]}
    write_json(output / "manifest.template.json", native)
    manifest = {"schema": "shore-pull-in-preparation-1", "case_table_sha256": digest(campaign / "case-table.json"),
                "provenance": provenance,
                "criteria_evidence": evidence, "analytical_before_models": True, "cases": records,
                "native_attempted": 0, "native_completed": 0, "qualified_results": 0,
                "files": {name: digest(output / name) for name in DELIVERABLES if name != "preparation.json"}}
    write_json(output / "preparation.json", manifest)
    return manifest


def main() -> int:
    """Require an explicit preparation mode and refuse native launch."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--dry-run", action="store_true")
    mode.add_argument("--launch", action="store_true")
    args = parser.parse_args()
    if args.launch:
        parser.error("A qualified execution adapter and approved manifest digest are required")
    prepare(args.campaign, args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
