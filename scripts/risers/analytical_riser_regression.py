# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Replay pinned assumed risers and independent closed-form comparisons offline.

The standard-library interfaces are loaded from verified source snapshots.
Full readbacks require an output directory inside TMPDIR, outside the checkout.
Metadata contains no source input values or calculated engineering readbacks.
MIT software licensing does not establish third-party payload publication rights.
"""

import argparse
import hashlib
import html
import json
import math
import os
from pathlib import Path
import subprocess
import sys
from types import ModuleType
from typing import Any, NotRequired, TypedDict


class SourcePin(TypedDict):
    """Describe an immutable source identity with an optional byte count."""

    path: str
    sha256: str
    bytes: NotRequired[int]


PINS: dict[str, SourcePin] = {
    "REPORT": {"path": "docs/domains/risers/scr-slwr-coverage-2026-10.md",
               "sha256": "e62d49cbaa8a35ab77a90378548da6ee7291fd1f6a2222e479c77734a5febae9", "bytes": 30230},
    "FIXTURE": {"path": "tests/fixtures/test_vectors/mooring_risers/scr_slwr_generic_cases.json",
                "sha256": "0bec18e98cafd840a898b7e23da15e0eb2bbf96b7a4fd1518e16b79e0a231459", "bytes": 12266},
    "CLOSED": {"path": "docs/benchmarks/catenary_closed_form/benchmark.json",
               "sha256": "33d652a450b54f1a955a2e284dd3258b64b96fc70a908161b7d67bf2762f9765", "bytes": 3970},
    "BASIS": {"path": "docs/benchmarks/catenary_closed_form/README.md",
              "sha256": "c91d4224550021e315f779223b1138008a50548667838212d0f366b999ac476a", "bytes": 12073},
    "MANIFEST": {"path": "docs/benchmarks/catenary_closed_form/inputs/MANIFEST.json",
                 "sha256": "b8fa51937f9eb97ffafa32d236b2d709e82d9afd2dbd0586e3c7ecac935fba61", "bytes": 656},
    "PLAIN": {"path": "docs/benchmarks/catenary_closed_form/inputs/catenary_c1_plain.yml",
              "sha256": "4ce6ad1d6ff54202a002e11cea15fd09125d27fe5bcef655032e2bac77425ffe", "bytes": 2891},
    "ELASTIC": {"path": "docs/benchmarks/catenary_closed_form/inputs/catenary_c2_elastic.yml",
                "sha256": "5588487fb36e425acebb521ac5b925446b3207d87a52d50d6d5eb837e6cfd560", "bytes": 2890},
    "LICENSE": {"path": "LICENSE",
                "sha256": "abe9282066701ba45ec516e77329e14f5e27e5ae462fd1fd23fe9f62e27d4d91", "bytes": 1071},
    "SCR_INTERFACE": {"path": "src/digitalmodel/marine_ops/marine_analysis/catenary/simplified.py",
                      "sha256": "5a0e89d7f4b07f70bb78fd90ed9dc621fae49c7e079c6b43ed81045ab4b5c969"},
    "SLWR_INTERFACE": {"path": "src/digitalmodel/marine_ops/marine_analysis/catenary/lazy_wave.py",
                       "sha256": "a22950e6640006721649df7db82e686bfce4037c7895469d5b23d01c4a37a17b"},
    "INTEGRATION_INTERFACE": {"path": "docs/benchmarks/catenary_closed_form/catenary_hand_values.py",
                              "sha256": "ec0a7c5262873566d28c77fa0271086f3f22d83d0a7f5c4b09e65ecfe14a1197"},
    "WEIGHT_MAPPING": {"path": "src/digitalmodel/orcaflex/riser_config.py",
                       "sha256": "4fa1bf445420b62a1a19d2081bbcc13f7f1fb5c0f60189e4e46bb9a47dfd6acd"},
}
FAMILIES = {"scr-shallow": "SCR", "scr-deep": "SCR",
            "slwr-shallow": "SLWR", "slwr-deep": "SLWR"}
COMMON_UNITS = {
    "water_depth": "m", "hang_off_elevation": "m", "angle": "deg",
    "outer_diameter": "m", "wall_thickness": "m", "coating_thickness": "m",
    "coating_density": "kg/m^3", "contents_density": "kg/m^3",
    "corrosion_allowance": "m", "seawater_density": "kg/m^3",
}
LAZY_UNITS = {"sag_elevation": "m", "hog_elevation": "m",
              "buoyancy_od": "m", "buoyancy_density": "kg/m^3"}
CRITERIA = {
    "depth": {"limit": 1e-6, "units": "m", "form": "absolute", "citation": "REPORT"},
    "angle": {"limit": 1e-9, "units": "deg", "form": "absolute", "citation": "REPORT"},
    "global_force": {"limit": 1e-12, "units": "1", "form": "relative", "citation": "REPORT"},
    "force_consistency": {"limit": 1e-12, "units": "1", "form": "relative", "citation": "REPORT"},
    "closed_tension": {"limit": 5.0, "units": "%", "form": "relative", "citation": "CLOSED/BASIS"},
    "closed_angle": {"limit": 1.0, "units": "deg", "form": "absolute", "citation": "CLOSED/BASIS"},
    "closed_touchdown": {"limit": 5.0, "units": "%", "form": "relative", "citation": "CLOSED/BASIS"},
}
BLOCKERS = [
    {"id": "catenary_manifest", "status": "BLOCKED-IF-MISSING",
     "next_input": "Accepted owner manifest for the first qualified catenary result, with source/run identities, solver version, input/output digests, units, criteria and accepted review."},
    {"id": "lazy_wave_manifest", "status": "BLOCKED-IF-MISSING",
     "next_input": "Accepted owner manifest establishing lazy-wave provenance, governing criteria, accepted review and installation applicability."},
    {"id": "public_reuse", "status": "BLOCKED-IF-MISSING",
     "next_input": "Source-specific public reuse decisions for third-party inputs and result payloads; reusable results remain with the existing private result owner."},
]


class EvidenceError(ValueError):
    """Identify evidence that cannot enter the analytical replay."""


def verify_sources(root: Path) -> dict[str, bytes]:
    """Verify every outer digest before parsing, then check both nested inputs."""
    blobs = {}
    for key, pin in PINS.items():
        path = (Path(root) / pin["path"]).resolve()
        if not path.is_relative_to(Path(root).resolve()):
            raise EvidenceError(f"{key}: source escapes root")
        try:
            blob = path.read_bytes()
        except OSError as error:
            raise EvidenceError(f"{key}: missing source") from error
        if (hashlib.sha256(blob).hexdigest() != pin["sha256"]
                or len(blob) != pin.get("bytes", len(blob))):
            raise EvidenceError(f"{key}: source digest or size mismatch")
        blobs[key] = blob
    entries = json.loads(blobs["MANIFEST"])["files"]
    required = {PINS[key]["path"]: key for key in ("PLAIN", "ELASTIC")}
    if len(entries) != 2 or {row["path"] for row in entries} != set(required):
        raise EvidenceError("nested manifest: input membership mismatch")
    for row in entries:
        key = required[row["path"]]
        if row["sha256"] != hashlib.sha256(blobs[key]).hexdigest() or row["bytes"] != len(blobs[key]):
            raise EvidenceError("nested manifest: input digest or size mismatch")
    return blobs


def finite(value: Any) -> float:
    """Require real finite numbers, including rejection of JSON booleans."""
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
        raise EvidenceError("nonfinite or nonnumeric analytical value")
    return value


def validate_cases(document: dict[str, Any]) -> list[dict[str, Any]]:
    """Admit exactly the four assumed cases with their explicit source units."""
    cases: list[dict[str, Any]] = document.get("cases", [])
    if len(cases) != 4 or {case.get("id") for case in cases} != set(FAMILIES):
        raise EvidenceError("assumed case identifiers are missing or duplicated")
    for case in cases:
        if set(case) != {"id", "type", "values", "provenance"}:
            raise EvidenceError("prepared, native or unrecognised candidate is inadmissible")
        if case["type"] != FAMILIES[case["id"]]:
            raise EvidenceError("assumed family mismatch")
        units = COMMON_UNITS | (LAZY_UNITS if case["type"] == "SLWR" else {})
        values = case["values"]
        if set(values) != set(units) or set(case["provenance"]) != set(units):
            raise EvidenceError("required field or provenance missing")
        for key, unit in units.items():
            finite(values[key])
            provenance = case["provenance"][key]
            if provenance.get("units") != unit or provenance.get("status") != "assumed generic value":
                raise EvidenceError("incompatible units or non-assumed provenance")
        if not 0 < values["angle"] < 90 or values["water_depth"] + values["hang_off_elevation"] <= 0:
            raise EvidenceError("invalid drop or angle")
        if not 0 <= values["corrosion_allowance"] < values["wall_thickness"] < values["outer_diameter"] / 2:
            raise EvidenceError("invalid nominal section")
        if any(values[key] < 0 for key in ("coating_thickness", "coating_density", "contents_density")):
            raise EvidenceError("negative pipe property")
        if values["seawater_density"] <= 0:
            raise EvidenceError("invalid fluid density")
    return cases


def load_interface(key: str, blob: bytes) -> ModuleType:
    """Execute the already verified snapshot without importing package routers."""
    name = "_analytical_riser_" + key.lower()
    module = ModuleType(name)
    sys.modules[name] = module  # Dataclass annotations require a registered module.
    exec(compile(blob, PINS[key]["path"], "exec"), module.__dict__)
    return module


def pipe_weights(values: dict[str, Any]) -> tuple[float, float | None]:
    """Map nominal annuli as in WEIGHT_MAPPING, without a design-model import.

    Steel density and gravity are assumed mapping defaults from the inspected
    source, not measurements or qualified material data. Buoyancy replaces the
    coating in that source's buoyed-section mass balance. Corrosion allowance
    does not alter the nominal mass section. Output is signed downward N/m.
    """
    density, gravity = 7850.0, 9.80665
    od, wall = values["outer_diameter"], values["wall_thickness"]
    inner, outer = od - 2 * wall, od + 2 * values["coating_thickness"]
    area = math.pi / 4
    steel = density * area * (od * od - inner * inner)
    contents = values["contents_density"] * area * inner * inner
    coating = values["coating_density"] * area * (outer * outer - od * od)
    water = values["seawater_density"]
    bare = (steel + contents + coating - water * area * outer * outer) * gravity
    buoyed = None
    if "buoyancy_od" in values:
        buoy_od = values["buoyancy_od"]
        if buoy_od <= od or values["buoyancy_density"] < 0:
            raise EvidenceError("invalid buoyancy section")
        module_mass = values["buoyancy_density"] * area * (buoy_od * buoy_od - od * od)
        buoyed = (steel + contents + module_mass - water * area * buoy_od * buoy_od) * gravity
        if buoyed >= 0:
            raise EvidenceError("buoyed section is not net buoyant")
    if bare <= 0:
        raise EvidenceError("bare section is not net downward")
    return bare, buoyed


def newtons_to_kn(force: float) -> float:
    """Convert N to kN exactly at the comparison boundary."""
    return finite(force) / 1000.0


def angle_from_vertical(horizontal: float, vertical: float) -> float:
    """Recover the hang-off angle in degrees from the force triangle."""
    return math.degrees(math.atan2(finite(horizontal), finite(vertical)))


def metric(actual: float, target: float, limit: float, units: str,
           relative: bool = False, zero_absolute: float | None = None) -> dict[str, Any]:
    """Grade a residual using the target magnitude as relative denominator."""
    finite(actual)
    finite(target)
    difference = abs(actual - target)
    if relative and target == 0:
        if zero_absolute is None:
            return {"status": "BLOCKED-IF-MISSING", "reason": "zero target lacks an absolute criterion"}
        residual, threshold = difference, zero_absolute
    else:
        residual, threshold = (difference / abs(target) if relative else difference), limit
    return {"status": "PASS" if residual <= threshold else "FAIL",
            "residual": residual, "limit": threshold, "units": units}


def aggregate(checks: dict[str, Any]) -> str:
    """Require every enumerated subcriterion; disregard aggregate pass flags."""
    statuses = [value["status"] for value in checks.values()
                if isinstance(value, dict) and "status" in value]
    if "FAIL" in statuses:
        return "FAIL"
    if not statuses or any(status != "PASS" for status in statuses):
        return "BLOCKED-IF-MISSING"
    return "PASS"


def check_geometry(case: dict[str, Any], result: dict[str, Any],
                   weights: list[float]) -> dict[str, Any]:
    """Reconstruct signed depth, angle and global forces without solver flags."""
    segments = result["segments"]
    signs = [1] if case["type"] == "SCR" else [1, -1, -1, 1, 1]
    if len(segments) != len(signs) or len(weights) != len(signs):
        raise EvidenceError("analytical segment membership mismatch")
    drops, loads, weight_errors, horizontal_errors = [], [], [], []
    for segment, weight, sign in zip(segments, weights, signs, strict=True):
        for value in segment.values():
            finite(value)
        radius, span = segment["radius_m"], segment["horizontal_m"]
        if radius <= 0 or span <= 0 or segment["length_m"] <= 0 or weight == 0:
            raise EvidenceError("invalid analytical segment")
        drops.append(sign * radius * (math.cosh(span / radius) - 1))
        loads.append(weight * segment["length_m"])
        weight_errors.append(abs(segment["weight_N_per_m"] / weight - 1))
        horizontal_errors.append(abs(result["horizontal_N"] / (abs(weight) * radius) - 1))
    values = case["values"]
    checks = {
        "depth": metric(math.fsum(drops), values["water_depth"] + values["hang_off_elevation"], 1e-6, "m"),
        "angle": metric(angle_from_vertical(result["horizontal_N"], result["vertical_N"]), values["angle"], 1e-9, "deg"),
        "global_force": metric(result["vertical_N"], math.fsum(loads), 1e-12, "1", relative=True),
        "tension": metric(result["tension_N"], math.hypot(result["horizontal_N"], result["vertical_N"]), 1e-12, "1", relative=True),
        "segment_weights": metric(max(weight_errors), 0, 1e-12, "1"),
        "horizontal_balance": metric(max(horizontal_errors), 0, 1e-12, "1"),
    }
    return checks


def solve_assumed(case: dict[str, Any], modules: dict[str, ModuleType]) -> tuple[dict[str, Any], list[float]]:
    """Adapt the two existing pure kernels to a common SI readback."""
    values = case["values"]
    bare, buoyed = pipe_weights(values)
    drop = values["water_depth"] + values["hang_off_elevation"]
    if case["type"] == "SCR":
        solver = modules["SCR_INTERFACE"].SimplifiedCatenarySolver()
        geometry = solver.solve_from_angle(values["angle"], drop)
        vertical, tension, horizontal = solver.calculate_forces(bare, geometry.arc_length, 90 - values["angle"])
        result = {"horizontal_N": horizontal, "vertical_N": vertical, "tension_N": tension,
                  "segments": [{"length_m": geometry.arc_length, "horizontal_m": geometry.horizontal_distance,
                                "radius_m": geometry.bend_radius, "weight_N_per_m": bare}]}
        return result, [bare]
    module = modules["SLWR_INTERFACE"]
    if buoyed is None:
        raise EvidenceError("lazy-wave buoyancy mapping missing")
    solved = module.LazyWaveSolver().solve(module.LazyWaveConfiguration(
        hangoff_angle=values["angle"], hangoff_below_msl=-values["hang_off_elevation"],
        vertical_distance=drop, sag_bend_elevation=values["sag_elevation"],
        hog_bend_above_seabed=values["hog_elevation"],
        weight_without_buoyancy=bare, weight_with_buoyancy=buoyed,
    ))
    result = {"horizontal_N": solved.horizontal_force, "vertical_N": solved.vertical_force,
              "tension_N": solved.total_tension, "segments": [
                  {"length_m": s.arc_length, "horizontal_m": s.horizontal_distance,
                   "radius_m": s.bend_radius, "weight_N_per_m": s.weight_per_length}
                  for s in solved.segments]}
    return result, [bare, bare, buoyed, buoyed, bare]


def check_closed(actual: dict[str, Any], target: dict[str, Any]) -> dict[str, Any]:
    """Apply CLOSED's individual bands to integration outputs in m, kN, deg."""
    return {
        "top_tension_kN": metric(actual["top_tension_kN"], target["top_tension_kN"], 0.05, "1", relative=True),
        "top_angle_deg_from_vertical": metric(actual["top_angle_deg_from_vertical"], target["top_angle_deg_from_vertical"], 1.0, "deg"),
        "touchdown_horizontal_distance_from_top_m": metric(actual["touchdown_horizontal_distance_from_top_m"], target["touchdown_horizontal_distance_from_top_m"], 0.05, "1", relative=True),
    }


def validate_closed(document: dict[str, Any]) -> list[dict[str, Any]]:
    """Bind comparison identities, units and finite targets to pinned CLOSED."""
    if document["units"] != {"force": "kN", "length": "m", "angle": "degrees"}:
        raise EvidenceError("closed-form units mismatch")
    cases: list[dict[str, Any]] = document["cases"]
    if [(c["case"], c["governing_form"]) for c in cases] != [("C1", "plain"), ("C2", "elastic")]:
        raise EvidenceError("closed-form comparison membership mismatch")
    for case in cases:
        for value in list(case["inputs"].values()) + list(case["expected"].values()):
            finite(value)
    return cases


def source_revision(root: Path) -> str:
    """Read the last analytical-source commit without inherited Git bindings.

    Commits affecting only the adapter, tests or reports do not change this
    identity. Source digests remain authoritative for the replayed bytes.
    """
    environment = {key: value for key, value in os.environ.items()
                   if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    paths = [pin["path"] for pin in PINS.values()]
    result = subprocess.run(["git", "-C", str(root), "log", "-1", "--format=%H", "--", *paths],
                            capture_output=True, text=True, check=True, env=environment)
    return result.stdout.strip()


def run(root: Path) -> dict[str, Any]:
    """Execute only after all source, membership and callable checks succeed."""
    root = Path(root).resolve()
    blobs = verify_sources(root)
    cases = validate_cases(json.loads(blobs["FIXTURE"]))
    closed = validate_closed(json.loads(blobs["CLOSED"]))
    modules = {key: load_interface(key, blobs[key]) for key in (
        "SCR_INTERFACE", "SLWR_INTERFACE", "INTEGRATION_INTERFACE")}
    for key, names in {"SCR_INTERFACE": ["SimplifiedCatenarySolver"],
                       "SLWR_INTERFACE": ["LazyWaveSolver", "LazyWaveConfiguration"],
                       "INTEGRATION_INTERFACE": ["shooting"]}.items():
        if not all(callable(getattr(modules[key], name, None)) for name in names):
            raise EvidenceError("callable pure analytical interface missing")
    assumed, comparisons = [], []
    for case in cases:
        result, weights = solve_assumed(case, modules)
        checks = check_geometry(case, result, weights)
        assumed.append({"id": case["id"], "family": case["type"], "status": aggregate(checks),
                        "checks": checks, "inputs": case, "readback": result})
    for case in closed:
        values = case["inputs"]
        stiffness = math.inf if case["governing_form"] == "plain" else values["EA_N"]
        result = modules["INTEGRATION_INTERFACE"].shooting(
            values["w_N_per_m"], stiffness, values["L_m"], values["h_m"], values["X_m"])
        checks = check_closed(result, case["expected"])
        comparisons.append({"id": case["case"], "form": case["governing_form"],
                            "status": aggregate(checks), "checks": checks,
                            "inputs": case, "readback": result})
    return assemble(root, blobs, assumed, comparisons)


def assemble(root: Path, blobs: dict[str, bytes], assumed: list[dict[str, Any]],
             comparisons: list[dict[str, Any]]) -> dict[str, Any]:
    """Bind completed results and counts without claiming native qualification."""
    return {
        "schema_version": 1, "status": aggregate({row["id"]: row for row in assumed + comparisons}),
        "readiness": "ASSUMED_ANALYTICAL_ONLY", "source_revision": source_revision(root),
        "utility_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "counts": {"assumed_attempted": len(assumed), "assumed_completed": len(assumed),
                   "assumed_passed": sum(row["status"] == "PASS" for row in assumed),
                   "comparisons_attempted": len(comparisons), "comparisons_completed": len(comparisons),
                   "comparisons_passed": sum(row["status"] == "PASS" for row in comparisons),
                   "native_runs": 0, "qualified_results": 0},
        "criteria": CRITERIA, "blockers": BLOCKERS,
        "revision_history": [{"revision": 0, "date": "2026-10-25",
                              "change": "Initial source-bound analytical replay to separate regression evidence from native qualification."}],
        "sources": {key: {"path": pin["path"], "sha256": hashlib.sha256(blobs[key]).hexdigest(),
                          "bytes": len(blobs[key]), "freshness": "Pinned bytes verified at replay; upstream freshness unverified"}
                    for key, pin in PINS.items()},
        "method": "SCR angle geometry and complementary-angle forces; five-segment SLWR; independent RK4 shooting against stored closed forms, C1 inextensible and C2 elastic. N internally, m and degrees; kN comparison boundary. Source revision identifies the last commit affecting pinned analytical sources; digests identify the replayed bytes.",
        "limits": ["Assumed source cases; no measured or qualified installation result.",
                   "Four distinct assumed cases and two separate integration comparisons; no native solver was executed.",
                   "Segment depth and global force are recomputed without solver pass flags or prepared geometry.",
                   "Pipe mass uses the nominal section and the inspected weight mapping; steel density and gravity are assumed mapping defaults.",
                   "Historical aggregate statics flags do not establish governing mesh criteria, accepted review or installation applicability.",
                   "Third-party rights and source-specific payload publication approval remain UNVERIFIED; numerical readbacks stay in scratch.",
                   "No dynamics, bending, fatigue, costing, buoyancy ranking or production optimisation is established."],
        "mapping_defaults": {"steel_density_kg_per_m3": 7850.0, "gravity_m_per_s2": 9.80665,
                             "status": "assumed, based on previous experience", "citation": "WEIGHT_MAPPING"},
        "assumed": assumed, "comparisons": comparisons,
    }


def metadata(report: dict[str, Any]) -> dict[str, Any]:
    """Whitelist public evidence and omit all input/result/residual payloads."""
    keys = ("schema_version", "status", "readiness", "source_revision", "utility_sha256",
            "counts", "criteria", "blockers", "revision_history", "sources", "method", "limits")
    public = {key: report[key] for key in keys}
    for group in ("assumed", "comparisons"):
        public[group] = [{key: row[key] for key in ("id", "status", "family" if group == "assumed" else "form")}
                         | {"criteria_status": {key: value["status"] for key, value in row["checks"].items()}}
                         for row in report[group]]
    return public


def render_html(public: dict[str, Any]) -> str:
    """Render only the public whitelist, with revision history after summary."""
    escaped = lambda value: html.escape(str(value))
    sections = []
    for title, key in (("Revision history", "revision_history"), ("Criteria", "criteria"),
                       ("Analytical outcomes", "assumed"), ("Integration comparisons", "comparisons"),
                       ("Qualification gaps and next inputs", "blockers"), ("Source identities", "sources"),
                       ("Known limits", "limits")):
        sections.append(f"<h2>{title}</h2><pre>{escaped(json.dumps(public[key], indent=2))}</pre>")
    return ("<!doctype html>\n<html lang=\"en\"><meta charset=\"utf-8\">"
            "<title>Analytical riser regression analysis</title>"
            "<style>body{font:1rem system-ui;max-width:75rem;margin:2rem auto;padding:1rem}"
            "pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f4f6f8;padding:1rem}</style>"
            "<h1>Analytical riser regression analysis</h1><h2>Summary</h2>"
            f"<p>Analytical regression status: {escaped(public['status'])}. "
            "Native acceptance and production optimisation remain BLOCKED-IF-MISSING.</p>"
            f"<pre>{escaped(json.dumps(public['counts'], indent=2))}</pre>"
            + "".join(sections) + f"<p>{escaped(public['method'])}</p></html>\n")


def write_reports(report: dict[str, Any], root: Path, out_dir: Path, metadata_only: bool) -> None:
    """Validate destinations before any write; keep full payloads in TMPDIR."""
    root, destination = Path(root).resolve(), Path(out_dir).resolve()
    if not metadata_only:
        scratch = os.environ.get("TMPDIR")
        if (not scratch or not destination.is_relative_to(Path(scratch).resolve())
                or destination.is_relative_to(root)):
            raise EvidenceError("full readbacks require TMPDIR outside the repository")
    names = ["analytical-riser-regression.json", "analytical-riser-regression.html"]
    if not metadata_only:
        names.append("analytical-riser-regression-readback.json")
    if any((destination / name).is_symlink() for name in names):
        raise EvidenceError("output file symlinks are inadmissible")
    public = metadata(report)
    payloads = [json.dumps(public, indent=2, allow_nan=False) + "\n", render_html(public)]
    if not metadata_only:
        payloads.append(json.dumps(report, indent=2, allow_nan=False) + "\n")
    destination.mkdir(parents=True, exist_ok=True)
    for name, payload in zip(names, payloads, strict=True):
        path = destination / name
        path.write_text(payload, encoding="utf-8")
        if path.read_text(encoding="utf-8") != payload:
            raise EvidenceError("saved report readback mismatch")


def main() -> int:
    """Run an offline replay and distinguish failures from qualification gaps."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--metadata-only", action="store_true")
    args = parser.parse_args()
    try:
        report = run(args.root)
        write_reports(report, args.root, args.out_dir, args.metadata_only)
    except (EvidenceError, OSError, KeyError, ArithmeticError) as error:
        print(f"BLOCKED-IF-MISSING: {type(error).__name__}; no report was accepted", file=sys.stderr)
        return 2
    print(json.dumps({"status": report["status"], "counts": report["counts"],
                      "native_acceptance": "BLOCKED-IF-MISSING"}, sort_keys=True))
    return 0 if report["status"] == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main())
