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

from digitalmodel.marine_ops.installation.shore_approach import RouteSegment, pull_in
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

import yaml

DELIVERABLES = ("models.json", "analysis.html", "manifest.template.json", "preparation.json")


def digest(path: Path) -> str:
    """Hash the actual committed bytes, including the final newline."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path: Path, value: dict) -> None:
    """Write deterministic UTF-8 JSON with one final newline."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")


def numeric_paths(value, prefix="") -> set[str]:
    """Identify every numeric input leaf that needs an input source."""
    if isinstance(value, dict):
        return set().union(*(numeric_paths(v, f"{prefix}.{k}" if prefix else k) for k, v in value.items()))
    if isinstance(value, list):
        return set().union(*(numeric_paths(v, f"{prefix}.{i}") for i, v in enumerate(value)))
    return {prefix} if isinstance(value, (int, float)) else set()


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
    return (mass - basis["water_density_te_m3"] * math.pi / 4 * diameter**2) * basis["gravity_m_s2"]


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
    return basis["tail_tension_kn"] + effective * (case["friction"] * horizontal + case["rise_m"])


def report(table: dict, records: list[dict]) -> str:
    """Build the self-contained analysis skeleton and revision history from data."""
    escape = html.escape
    revisions = "".join(f'<tr><td>{escape(r["revision"])}</td><td>{escape(r["date"])}</td><td>{escape(r["change"])}</td></tr>' for r in table["revisions"])
    rows = "".join(f'<tr><td>{r["id"]}</td><td>{r["method"]["pipe_head_tension_kn"]:.1f}</td><td>{r["analytical_pull_kn"]:.1f}</td><td>{r["method"]["winch_demand_kn"]:.1f}</td><td>Not Evaluated</td></tr>' for r in records)
    limits = "".join(f"<li>{escape(s)}</li>" for s in table["known_limits"])
    sources = "".join(f'<li>{escape(s["locator"]) if "locator" in s else "Public dataset, row 68"} — <a href="{escape(s["url"], quote=True)}">Source</a>; retrieved {escape(s["retrieval_date"])}; SHA-256 {s["sha256"]}</li>' for s in table["sources"].values())
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Shore pull-in analysis</title>
<style>body{{font:16px sans-serif;max-width:70rem;margin:2rem auto;padding:1rem}}table{{border-collapse:collapse}}td,th{{border:1px solid #888;padding:.4rem}}code{{overflow-wrap:anywhere}}</style></head><body>
<h1>Shore pull-in analysis</h1><p>Revision {escape(table["revision"])}</p>
<section id="summary"><h2>Summary</h2><p>Three generic surface pull-in scenarios are prepared. Pull forces are analytical, not native solver results. Installation acceptance is Not Evaluated. The next step is source and topology qualification before native execution.</p></section>
<section id="revision-history"><h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>{revisions}</table></section>
<section id="method"><h2>Method and criterion</h2><p>For each straight supported segment: net weight w equals submerged weight minus buoyancy aid force per length; sin(theta) = rise / arc length. Normal load N = w L cos(theta), friction = mu N, gravity = w rise. Head tension is trailing tension plus the ordered force increments. Winch demand is head tension divided by mechanical efficiency. Loss of contact and compression are rejected. Equivalent axial friction follows the cited soil paper; gravity resolution is a straight-segment work balance.</p><p>The independent check uses separately computed annular areas and horizontal projection. Agreement criterion: relative difference no greater than the force-balance tolerance in criteria.json. All prepared cases meet that computational identity; this is not physical validation. Shore winch capacity is unassigned.</p></section>
<h2>Prepared results</h2><table><tr><th>Case</th><th>Method pull (kN)</th><th>Independent pull (kN)</th><th>Winch demand (kN)</th><th>Native result</th></tr>{rows}</table>
<h2>Native analysis skeleton</h2><p>Native model version, measured timing, effective tension, bending moment, curvature, contact reactions, final geometry, convergence and acceptance: Not Evaluated. The first timing case is level-initial. No extrapolated runtime is recorded.</p>
<h2>Limits and way forward</h2><ul>{limits}</ul><p>Assign traced pipe and winch properties; qualify the end connections, buoyancy representation and contacts; approve a manifest digest and native comparison criteria; then measure the first native case before a campaign run.</p>
<h2>Sources</h2><ul>{sources}</ul></body></html>
'''


def prepare(campaign: Path, output: Path) -> dict:
    """Evaluate all analytical checks before generating validated model bundles.

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
        raise ValueError("Criteria must be recorded before first generation")
    if table["revisions"][-1]["revision"] != table["revision"] or criteria["revision"] != table["revision"]:
        raise ValueError("Revision records must match the declared revision")
    required = numeric_paths(table["basis"]) | numeric_paths(table["cases"], "cases")
    if not required <= set(table["input_sources"]):
        raise ValueError("Missing numeric input source")
    for entry in table["input_sources"].values():
        source = table["sources"][entry["source"]]
        if not all(k in source for k in ("url", "retrieval_date", "record_count", "sha256")):
            raise ValueError("Incomplete input provenance")
    identities = [c["id"] for c in table["cases"]]
    if not identities or len(set(identities)) != len(identities) or any(not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", c) for c in identities):
        raise ValueError("Invalid case identity")
    basis = table["basis"]
    records = []
    specs = []
    for case in table["cases"]:
        w = weight(basis)
        segment = RouteSegment(basis["pipeline"]["segments"][0]["length"], w, case["friction"], case["rise_m"], w * case["buoyancy_fraction"])
        result = pull_in([segment], tail_tension_kn=basis["tail_tension_kn"], efficiency=basis["winch_efficiency"])
        independent = analytical_pull(basis, case)
        error = abs(result["pipe_head_tension_kn"] - independent) / max(abs(independent), 1)
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
    output.mkdir(parents=True, exist_ok=True)
    bundle = {"schema": "shore-pull-in-model-bundle-1", "cases": {}}
    for record, spec in zip(records, specs):
        folder = output / "models" / record["id"]
        ModularModelGenerator.from_spec(spec).generate(folder)
        # Preparation does not assign standard design factors or strain limits.
        # Remove optional design-check settings emitted by the shared builder;
        # retain structural properties and run the unmodified validator below.
        types_path = folder / "includes/05_line_types.yml"
        types_data = yaml.safe_load(types_path.read_text(encoding="utf-8"))
        for line_type in types_data["LineTypes"]:
            for key in list(line_type):
                if key.startswith("DNVSTF101"):
                    del line_type[key]
        class PreparationDumper(yaml.SafeDumper):
            """Preserve the repository's native null spelling."""
        PreparationDumper.add_representer(type(None), lambda dumper, value: dumper.represent_scalar("tag:yaml.org,2002:null", "~"))
        types_path.write_text(yaml.dump(types_data, Dumper=PreparationDumper, sort_keys=False), encoding="utf-8")
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
    write_json(output / "models.json", bundle)
    (output / "analysis.html").write_text(report(table, records), encoding="utf-8")
    native = {"schema": "shore-pull-in-native-manifest-1", "status": "not_run", "launch_enabled": False,
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
