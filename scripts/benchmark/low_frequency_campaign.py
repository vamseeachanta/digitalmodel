# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Prepare matched diffraction specs and screen exports without solver imports.

Schema source: src/digitalmodel/hydrodynamics/diffraction/input_schemas.py,
VesselGeometry, FrequencySpec, WaveHeadingSpec and SolverOptions. This tool
prepares specifications, not native decks or qualified engineering results.
"""

import argparse
import copy
import csv
from decimal import Decimal, InvalidOperation
import hashlib
import importlib.util
import itertools
import json
import math
import os
from pathlib import Path, PurePosixPath

import yaml


# Load only the offline geometry module, avoiding solver package initializers.
_GEOMETRY_SPEC = importlib.util.spec_from_file_location(
    "full_body_mesh", Path(__file__).with_name("full_body_mesh.py")
)
mesh_geometry = importlib.util.module_from_spec(_GEOMETRY_SPEC)
_GEOMETRY_SPEC.loader.exec_module(mesh_geometry)
check_df2_buoyancy = mesh_geometry.check_df2_buoyancy
APPROVALS = (
    "criteria",
    "geometry",
    "heading_convention",
    "weight_report",
    "frequency_list",
)


def require_complete(value):
    """Reject missing and proposed inputs recursively before preparing cases."""
    if isinstance(value, dict):
        for child in value.values():
            require_complete(child)
    elif isinstance(value, list):
        for child in value:
            require_complete(child)
    elif value is None or (
        isinstance(value, str)
        and value.strip().upper().startswith(("MISSING", "PROPOSED", "REQUIRED"))
    ):
        raise ValueError("MISSING or PROPOSED input; campaign cannot issue")


def approved(value):
    """Require a recorded approval reference; authentication remains external."""
    return (
        isinstance(value, str)
        and value.startswith("APPROVED: ")
        and bool(value[10:].strip())
    )


LEVELS = ("coarse", "medium", "fine")
SOLVERS = ("orcawave", "aqwa")
TREATMENTS = ("none", "interior_panels")
DOFS = ("sway", "heave")
COLUMNS = ("run_id", "frequency_rad_s", "heading_deg", "dof", "amplitude", "unit")
# Repository AQG-r0 implementation, qa_gates.py EXTERNAL_CHECKS AQ-10:
# long_period_heave_within_2_percent. This is not a universal validity band.
HEAVE_TARGET = Decimal("1")
HEAVE_BAND = Decimal("0.02")
HEAVE_SOURCE = "src/digitalmodel/hydrodynamics/aqwa/qa_gates.py: EXTERNAL_CHECKS AQ-10"
# Backend mappings inspected in this repository, not guessed native options.
MAPPING_SOURCES = {
    "orcawave": "src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py: _build_body_dict",
    "aqwa": "src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py: ILID AUTO emission",
}


def digest(data):
    """Return the SHA-256 of retained bytes."""
    return hashlib.sha256(data).hexdigest()


def number(value):
    """Reject booleans and non-finite numeric input without rounding samples."""
    try:
        result = Decimal(str(value))
    except (InvalidOperation, ValueError) as exc:
        raise ValueError("finite numeric value required") from exc
    if isinstance(value, bool) or not result.is_finite():
        raise ValueError("finite numeric value required")
    # The JSON report uses floats; reject overflow/underflow rather than emit
    # infinity or silently turn a nonzero sample into zero in that report.
    encoded = float(result)
    if not math.isfinite(encoded) or (encoded == 0 and result != 0):
        raise ValueError("numeric value is outside the report representation")
    return result


def cited(record):
    """Require a positive, explicitly sourced campaign limit."""
    if not isinstance(record, dict) or not isinstance(record.get("source"), str):
        raise ValueError("cited criterion required; NOT_EVALUATED")
    if record.get("status") != "approved" or not approved(record.get("approval")):
        raise ValueError(
            "approved criterion and approval reference required; NOT_EVALUATED"
        )
    value = number(record.get("value"))
    if not record["source"].strip() or value <= 0:
        raise ValueError("positive cited criterion required; NOT_EVALUATED")
    return value


def grid(values, *, headings=False):
    """Use exact decimal identity, canonical headings, and ordered frequencies."""
    if not isinstance(values, list) or not values:
        raise ValueError("nonempty explicit grid required")
    parsed = [number(value) for value in values]
    if len(set(parsed)) != len(parsed) or parsed != sorted(parsed):
        raise ValueError("grid must be unique and increasing")
    # Heading convention: degrees in one revolution, no equivalent 0/360 rows.
    if headings:
        if any(value < 0 or value >= 360 for value in parsed):
            raise ValueError("headings must be canonical degrees")
    elif any(value <= 0 for value in parsed):
        raise ValueError("angular frequencies must be positive")
    return parsed


def local_mesh(root, value):
    """Resolve only existing portable relative mesh paths within the input root."""
    if not isinstance(value, str) or not value or "\\" in value or ":" in value:
        raise ValueError("portable relative mesh path required")
    relative = PurePosixPath(value)
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("mesh path escapes input root")
    path = (root / value).resolve()
    if not path.is_relative_to(root) or not path.is_file():
        raise ValueError("mesh must exist inside input root")
    return path


def mesh_keys(value, trail=()):
    """Find mesh references so unsupported auxiliary assets cannot be relocated."""
    if isinstance(value, dict):
        for key, child in value.items():
            if (
                "mesh" in key.lower()
                and isinstance(child, str)
                and key != "mesh_format"
            ):
                yield trail + (key,)
            yield from mesh_keys(child, trail + (key,))
    elif isinstance(value, list):
        for child in value:
            yield from mesh_keys(child, trail)


def build(base_path, request_path, output):
    """Build a deterministic preparation packet in memory before any writes."""
    base_path, request_path, output = map(Path, (base_path, request_path, output))
    root = base_path.resolve().parent
    base_bytes, request_bytes = base_path.read_bytes(), request_path.read_bytes()
    base = yaml.safe_load(base_bytes)
    request = json.loads(request_bytes)
    if not isinstance(base, dict) or "bodies" in base or "vessel" not in base:
        raise ValueError("single-body unified specification required")
    if list(mesh_keys(base)) != [("vessel", "geometry", "mesh_file")]:
        raise ValueError("auxiliary mesh references are not supported")
    if not isinstance(request.get("source"), str) or not request["source"].strip():
        raise ValueError("campaign source required")
    require_complete(request)
    approvals = request.get("approvals", {})
    if any(not approved(approvals.get(key)) for key in APPROVALS):
        raise ValueError(
            "reviewed approval references required before any case is issued"
        )
    criteria = request["criteria"]
    limits = {
        key: cited(criteria.get(key))
        for key in (
            "mesh_absolute_m_per_m",
            "solver_absolute_m_per_m",
            "asymptotic_max_rad_s",
        )
    }
    if limits["asymptotic_max_rad_s"] > Decimal("0.236"):
        raise ValueError("asymptotic limit must not exceed 0.236 rad/s")
    frequencies = grid(request["frequencies_rad_s"])
    headings = grid(request["headings_deg"], headings=True)
    old_frequency = base["frequencies"]
    if old_frequency.get("input_type") != "frequency" or "range" in old_frequency:
        raise ValueError("base requires explicit angular-frequency values")
    old = grid(old_frequency.get("values"))
    old_headings = grid(base["wave_headings"].get("values"), headings=True)
    if not set(old) < set(frequencies) or not set(old_headings) <= set(headings):
        raise ValueError("retain original samples and add frequency samples")
    # Issue 1548 Recommendation: denser low-frequency grid, not merely more
    # high-frequency samples. The original lowest interval must be refined.
    if len(old) < 2 or not any(old[0] < value < old[1] for value in frequencies):
        raise ValueError("refine the original lowest-frequency interval")
    if not any(value <= limits["asymptotic_max_rad_s"] for value in frequencies):
        raise ValueError("asymptotic selection must not be empty")
    if set(request["meshes"]) != set(LEVELS):
        raise ValueError("ordered coarse, medium and fine meshes required")
    meshes = {level: local_mesh(root, request["meshes"][level]) for level in LEVELS}
    original_mesh = local_mesh(root, base["vessel"]["geometry"]["mesh_file"])
    if any(
        path.suffix.lower() != original_mesh.suffix.lower() for path in meshes.values()
    ):
        raise ValueError("all mesh levels must use the base mesh format")
    if len({digest(path.read_bytes()) for path in meshes.values()}) != len(LEVELS):
        raise ValueError("mesh levels require distinct contents")
    geometry = base["vessel"]["geometry"]
    if geometry.get("waterline_z", 0) != 0:
        raise ValueError("DF2 supports only meshes at waterline z=0")
    if geometry.get("length_units", "m") != "m":
        raise ValueError("DF2 requires declared metre units")
    inertia = base["vessel"]["inertia"]
    if inertia.get("mode", "explicit") != "explicit":
        raise ValueError("DF2 requires explicit mass")
    df2 = {}
    for level, mesh in meshes.items():
        mesh_geometry.check_symmetry(mesh, geometry.get("symmetry", "none"))
        df2[level] = check_df2_buoyancy(
            mesh, inertia["mass"], base["environment"].get("water_density", 1025)
        )
        if df2[level]["decision"] != "PASS":
            raise ValueError(f"DF2 {level}: {df2[level]['decision']}; no case issued")
    paths = {
        "base": base_path,
        "request": request_path,
        "original_mesh": original_mesh,
        **meshes,
    }
    inputs = {
        key: {
            "path": Path(os.path.relpath(path.resolve(), output.resolve())).as_posix(),
            "sha256": digest(path.read_bytes()),
        }
        for key, path in paths.items()
    }
    cases, contents = [], {}
    for solver, level, treatment in itertools.product(SOLVERS, LEVELS, TREATMENTS):
        run_id = f"{solver}_{level}_{treatment}"
        spec = copy.deepcopy(base)
        spec["vessel"]["geometry"]["mesh_file"] = inputs[level]["path"]
        spec["frequencies"] = {
            "input_type": "frequency",
            "values": request["frequencies_rad_s"],
        }
        spec["wave_headings"] = {
            **base["wave_headings"],
            "values": request["headings_deg"],
        }
        spec["wave_headings"].pop("range", None)
        options = spec.setdefault("solver_options", {})
        options.pop("remove_irregular_frequencies", None)
        options["irregular_frequency_method"] = treatment
        name = f"{run_id}.yml"
        contents[name] = yaml.safe_dump(spec, sort_keys=False).encode("utf-8")
        cases.append(
            dict(
                run_id=run_id,
                solver=solver,
                mesh_level=level,
                treatment=treatment,
                mapping_source=MAPPING_SOURCES[solver],
                spec=name,
                sha256=digest(contents[name]),
            )
        )
    manifest = dict(
        inputs=inputs,
        cases=cases,
        criteria=criteria,
        source=request["source"],
        frequencies_rad_s=request["frequencies_rad_s"],
        headings_deg=request["headings_deg"],
        native_execution="NOT_EVALUATED",
        df2=df2,
        approvals=approvals,
    )
    return manifest, contents


def prepare(base_path, request_path, output):
    """Write new specifications exclusively; never overwrite a preparation."""
    manifest, contents = build(base_path, request_path, output)
    output = Path(output)
    output.mkdir(parents=True, exist_ok=False)
    for name, data in contents.items():
        with (output / name).open("xb") as stream:
            stream.write(data)
    write_json(output / "manifest.json", manifest)
    return verify_manifest(output / "manifest.json")


def verify_manifest(path):
    """Rebuild the packet and verify every source and generated specification."""
    path = Path(path)
    manifest = json.loads(path.read_text(encoding="utf-8"))
    for record in manifest["inputs"].values():
        if digest((path.parent / record["path"]).read_bytes()) != record["sha256"]:
            raise ValueError("input digest mismatch")
    base = path.parent / manifest["inputs"]["base"]["path"]
    request = path.parent / manifest["inputs"]["request"]["path"]
    rebuilt, contents = build(base, request, path.parent)
    if rebuilt != manifest:
        raise ValueError("manifest differs from source-bound preparation")
    for name, data in contents.items():
        if (path.parent / name).read_bytes() != data:
            raise ValueError("prepared specification digest mismatch")
    return manifest


def read_rows(path, manifest):
    """Read a complete exact grid of nonnegative translational magnitudes."""
    expected = set(
        itertools.product(
            (case["run_id"] for case in manifest["cases"]),
            map(number, manifest["frequencies_rad_s"]),
            map(number, manifest["headings_deg"]),
            DOFS,
        )
    )
    rows = {}
    with Path(path).open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        if reader.fieldnames != list(COLUMNS):
            raise ValueError("exact CSV column contract required")
        for row in reader:
            if None in row or any(value is None for value in row.values()):
                raise ValueError("malformed CSV row")
            key = (
                row["run_id"],
                number(row["frequency_rad_s"]),
                number(row["heading_deg"]),
                row["dof"],
            )
            amplitude = number(row["amplitude"])
            if (
                key not in expected
                or key in rows
                or row["unit"] != "m/m"
                or amplitude < 0
            ):
                raise ValueError("unknown, duplicate or invalid native-export sample")
            rows[key] = amplitude
    if set(rows) != expected:
        raise ValueError("missing native-export samples")
    return rows


def assess(manifest_path, csv_path):
    """Compare all prescribed pairs; numerical PASS never qualifies native runs."""
    manifest = verify_manifest(manifest_path)
    rows = read_rows(csv_path, manifest)
    criteria = manifest["criteria"]
    limits = {
        key: cited(criteria[key])
        for key in (
            "mesh_absolute_m_per_m",
            "solver_absolute_m_per_m",
            "asymptotic_max_rad_s",
        )
    }
    checks = []
    for key, amplitude in rows.items():
        run_id, frequency, heading, dof = key
        if dof == "heave" and frequency <= limits["asymptotic_max_rad_s"]:
            checks.append(
                dict(
                    kind="heave_asymptote",
                    run_id=run_id,
                    frequency_rad_s=float(frequency),
                    heading_deg=float(heading),
                    amplitude=float(amplitude),
                    source=HEAVE_SOURCE,
                    decision="PASS"
                    if HEAVE_TARGET - HEAVE_BAND
                    <= amplitude
                    <= HEAVE_TARGET + HEAVE_BAND
                    else "FAIL",
                )
            )
    pairs = []
    for solver, treatment in itertools.product(SOLVERS, TREATMENTS):
        for left, right in zip(LEVELS, LEVELS[1:]):
            pairs.append(
                (
                    "mesh_pair",
                    f"{solver}_{left}_{treatment}",
                    f"{solver}_{right}_{treatment}",
                    "mesh_absolute_m_per_m",
                )
            )
    for level, treatment in itertools.product(LEVELS, TREATMENTS):
        pairs.append(
            (
                "solver_pair",
                f"orcawave_{level}_{treatment}",
                f"aqwa_{level}_{treatment}",
                "solver_absolute_m_per_m",
            )
        )
    for solver, level in itertools.product(SOLVERS, LEVELS):
        pairs.append(
            (
                "treatment_delta",
                f"{solver}_{level}_none",
                f"{solver}_{level}_interior_panels",
                None,
            )
        )
    samples = itertools.product(
        map(number, manifest["frequencies_rad_s"]),
        map(number, manifest["headings_deg"]),
        DOFS,
    )
    for frequency, heading, dof in samples:
        for kind, left, right, criterion in pairs:
            difference = abs(
                rows[left, frequency, heading, dof]
                - rows[right, frequency, heading, dof]
            )
            limit = limits[criterion] if criterion else None
            checks.append(
                dict(
                    kind=kind,
                    left=left,
                    right=right,
                    dof=dof,
                    frequency_rad_s=float(frequency),
                    heading_deg=float(heading),
                    difference=float(difference),
                    criterion=criteria[criterion] if criterion else None,
                    decision=("PASS" if difference <= limit else "FAIL")
                    if limit is not None
                    else "NOT_EVALUATED",
                )
            )
    return dict(
        numerical_screen="FAIL"
        if any(c["decision"] == "FAIL" for c in checks)
        else "PASS",
        qualification="NOT_EVALUATED",
        native_execution="NOT_EVALUATED",
        limitation="CSV values do not establish native execution, convergence or physical validity.",
        manifest_sha256=digest(Path(manifest_path).read_bytes()),
        export_sha256=digest(Path(csv_path).read_bytes()),
        criteria=criteria,
        row_count=len(rows),
        checks=checks,
    )


def write_json(path, value):
    """Write an exclusive UTF-8/LF report and verify serialized readback."""
    with Path(path).open("x", encoding="utf-8", newline="\n") as stream:
        stream.write(json.dumps(value, indent=2, allow_nan=False) + "\n")
    if json.loads(Path(path).read_text(encoding="utf-8")) != value:
        raise ValueError("JSON readback mismatch")


def main():
    """Expose preparation and assessment without launching other programs."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    preparation = commands.add_parser("prepare")
    for argument in ("base", "request", "output"):
        preparation.add_argument(argument, type=Path)
    assessment = commands.add_parser("assess")
    for argument in ("manifest", "csv", "report"):
        assessment.add_argument(argument, type=Path)
    args = parser.parse_args()
    if args.command == "prepare":
        prepare(args.base, args.request, args.output)
        return 0
    report = assess(args.manifest, args.csv)
    write_json(args.report, report)
    return 1 if report["numerical_screen"] == "FAIL" else 0


if __name__ == "__main__":
    raise SystemExit(main())
