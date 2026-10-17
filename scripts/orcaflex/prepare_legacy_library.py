# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build preparation records from complete, content-inspected library models."""

from __future__ import annotations

import argparse
import csv
import importlib.util
import json
from hashlib import sha256
from datetime import datetime
from pathlib import Path
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[2]
CHECKER = Path(__file__).with_name("check_legacy_library_sanitisation.py")
_spec = importlib.util.spec_from_file_location("library_inspection", CHECKER)
inspection = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(inspection)


def encode(record: dict) -> bytes:
    """Return canonical JSON with one final newline and finite numbers only."""
    return (json.dumps(record, sort_keys=True, separators=(",", ":"), allow_nan=False) + "\n").encode()


def build_inventory(root: Path, ref: str, retrieved: str, repository_id: int) -> dict:
    """Inventory the publishable subset without retaining excluded locators."""
    record = inspection.publication_records(inspection.audit(root), root)
    record["source_ref"] = ref
    for item in record["inputs"]:
        item["source_url"] = (
            f"https://api.github.com/repositories/{repository_id}/contents/"
            f"{quote(item['path'])}?ref={ref}"
        )
        item["retrieval_date"] = retrieved
        item["record_count"] = 1
    return record


def verify_inputs(inventory: dict, root: Path) -> None:
    """Refuse stale evidence before reading a model or extracting results."""
    for item in inventory["inputs"]:
        path = root / item["path"]
        if item["model_type"] != inspection.model_type(Path(item["path"])):
            raise ValueError("recorded input file role mismatch")
        if Path(item["path"]).is_absolute() or not path.resolve().is_relative_to(root.resolve()) or path.is_symlink():
            raise ValueError("recorded input locator invalid")
        if sha256(path.read_bytes()).hexdigest() != item["sha256"]:
            raise ValueError("source digest mismatch")


def verify_reused_inventory(inventory: dict, root: Path) -> None:
    """Reinspect retained bytes and rebuild dependencies before reusing records."""
    verify_inputs(inventory, root)
    rules = inspection.gate.load_rules()
    provenance = inspection.rule_provenance(rules)
    if any(inventory.get(key) != value for key, value in provenance.items()):
        raise ValueError("recorded rule provenance mismatch")
    records = []
    for item in inventory["inputs"]:
        result = inspection.inspect_model(root / item["path"], item["path"], rules)
        if result["status"] != "pass":
            raise ValueError("recorded input failed content inspection")
        records.append({**item, **result})
    rebuilt = inspection.publication_records({**provenance, "models": records, "summary": {"fail": 0}}, root)
    if rebuilt["models"] != inventory["models"] or len(rebuilt["inputs"]) != len(records):
        raise ValueError("recorded model dependency mismatch")


def inventory_files(record: dict) -> dict[str, bytes]:
    """Split input and model records below the repository's file-size hook limit."""
    models = encode({"models": record["models"]})
    index = {key: value for key, value in record.items() if key != "models"}
    index.update(models_file="models.json", models_sha256=sha256(models).hexdigest())
    return {"inventory.json": encode(index), "models.json": models}


def validation_cache_path(cache_dir: Path, item: dict) -> Path:
    """Bind resumable checks to input bytes and both validator source modules."""
    import yaml

    source = ROOT / "src/digitalmodel/solvers/orcaflex"
    key = {"path": item["path"], "sha256": item["sha256"], "yaml_version": yaml.__version__,
           "validators": {name: sha256((source / name).read_bytes()).hexdigest()
                          for name in ("yaml_validator.py", "yaml_timestep_validation.py")}}
    return cache_dir / (sha256(encode(key)).hexdigest() + ".json")


def cache_validation(cache_dir: Path, item: dict, result: dict) -> None:
    """Keep each completed input check outside the source tree for resumption."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    validation_cache_path(cache_dir, item).write_bytes(encode({
        "result": result, "result_sha256": sha256(encode(result)).hexdigest()}))


def validate_models(inventory: dict, root: Path, cache_dir: Path | None = None) -> dict:
    """Apply the existing YAML validator once per retained input, without a solver."""
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

    verify_inputs(inventory, root)
    validator = OrcaFlexYAMLValidator()
    inputs = {}
    if cache_dir is not None and cache_dir.resolve().is_relative_to(root.resolve()):
        raise ValueError("validation cache must remain outside the repository")
    for item in inventory["inputs"]:
        if cache_dir is not None:
            saved = validation_cache_path(cache_dir, item)
            if saved.exists():
                cached = json.loads(saved.read_bytes())
                if sha256(encode(cached["result"])).hexdigest() != cached["result_sha256"]:
                    raise ValueError("cached validator record digest mismatch")
                inputs[item["path"]] = cached["result"]
                continue
        if item["model_type"] == "specification":
            inputs[item["path"]] = {"status": "not-applicable", "errors": 0,
                                    "warnings": 0, "reason": "parameter-specification-not-native-yaml"}
            if cache_dir is not None:
                cache_validation(cache_dir, item, inputs[item["path"]])
            continue
        try:
            result = validator.validate_file(root / item["path"])
            inputs[item["path"]] = {"status": "pass" if result.valid else "fail",
                                    "errors": len(result.errors), "warnings": len(result.warnings),
                                    "format": result.format_detected}
        except Exception as error:
            # Validator messages may quote source values; retain only the type.
            inputs[item["path"]] = {"status": "fail", "errors": 1, "warnings": 0,
                                    "reason": f"validator-exception-{type(error).__name__}"}
        if cache_dir is not None:
            cache_validation(cache_dir, item, inputs[item["path"]])
    models = []
    for model in inventory["models"]:
        records = [inputs[name] for name in model["inputs"]]
        errors = sum(item["errors"] for item in records) + model["missing_dependency_count"]
        status = "fail" if errors else inputs[model["path"]]["status"]
        models.append({"path": model["path"], "status": status, "errors": errors,
                       "warnings": sum(item["warnings"] for item in records),
                       "inputs": [{"path": name, **inputs[name]} for name in model["inputs"]]})
    return {"validator": "digitalmodel.solvers.orcaflex.yaml_validator.OrcaFlexYAMLValidator",
            "native_solver_used": False, "models": models}


def extract_comparators(inventory: dict, root: Path) -> dict:
    """Extract exact-stem CSV tables; keep absent or ambiguous results explicit.

    A workspace file configures views and channels; it is not a stored numeric
    result. Directory proximity alone does not establish a model association.
    Table tokens and header units are preserved without numeric reinterpretation.
    """
    verify_inputs(inventory, root)
    models = []
    rules = inspection.gate.load_rules()
    for model in inventory["models"]:
        path = root / model["path"]
        candidates = [path.with_suffix(".csv"), path.with_name(path.stem + "_results.csv")]
        tables = []
        reason = "no-unambiguous-associated-stored-numeric-result"
        for candidate in candidates:
            if not candidate.is_file() or candidate.is_symlink():
                continue
            relative = candidate.relative_to(root).as_posix()
            checked = inspection.inspect_model(candidate, relative, rules)
            if checked["status"] != "pass":
                reason = "associated-table-failed-content-inspection"
                continue
            content = candidate.read_bytes()
            rows = list(csv.reader(content.decode("utf-8-sig").splitlines()))
            if len(rows) < 2 or any(len(row) != len(rows[0]) for row in rows[1:]):
                reason = "associated-table-has-no-rectangular-result-data"
                continue
            tables.append({"path": relative, "sha256": sha256(content).hexdigest(),
                           "record_count": len(rows) - 1, "headers": rows[0], "rows": rows[1:],
                           "units": "as-written-in-source-headers", "association": "exact-model-stem"})
        record = {"path": model["path"], "status": "stored" if tables else "unavailable"}
        if tables:
            record["tables"] = tables
        else:
            record["reason"] = reason
        models.append(record)
    return {"native_solver_used": False, "models": models,
            "association_policy": "exact-model-stem; directory proximity is insufficient"}


def manifest_template(files: dict[str, bytes], timed_case: str) -> dict:
    """Bind committed preparation bytes while refusing unqualified execution."""
    manifest = {"schema": "legacy-library-native-template-1", "operations": ["load", "statics", "compare"],
            "files": {name: sha256(content).hexdigest() for name, content in sorted(files.items())},
            "timed_case": timed_case, "native_elapsed_seconds": None,
            "qualified_execution_adapter": None, "approved_manifest_digest": None,
            "launch_status": "refused", "native_solver_used": False,
            "reuse": {"prepared_campaign": "#2469", "digest_bound_report_pattern": "#2486"}}
    if "timed-preparation.json" in files:
        timing = json.loads(files["timed-preparation.json"])
        if timing["path"] != timed_case or timing["native_solver_used"]:
            raise ValueError("timed preparation must refer to this solver-free case")
        manifest["preparation_elapsed_seconds"] = timing["elapsed_seconds"]
    return manifest


def request_launch(manifest: dict) -> None:
    """Refuse launch until a qualified execution adapter and approval exist."""
    raise RuntimeError("launch requires a qualified execution adapter and approved manifest digest")


def verify_criterion_order(criterion: bytes, run: dict) -> None:
    """Verify the saved criterion bytes existed before the first judged run."""
    if sha256(criterion).hexdigest() != run["criterion_sha256"]:
        raise ValueError("criterion digest mismatch")
    recorded = datetime.fromisoformat(json.loads(criterion)["recorded_utc"])
    started = datetime.fromisoformat(run["started_utc"])
    if recorded >= started:
        raise ValueError("criterion must precede the run")


def build_jumper_skill(inventory: dict, root: Path) -> dict[str, bytes]:
    """Describe the four inspected specifications without inferring missing geometry."""
    import yaml

    verify_inputs(inventory, root)
    sources = {item["path"]: item for item in inventory["inputs"]}
    qualified = {item["path"] for item in inventory["models"]}
    groups = []
    for group in ("manifold_to_plet", "plet_to_plem", "sut_mm", "sut_mm/resonance"):
        relative = f"docs/domains/orcaflex/jumper/{group}/spec.yml"
        if relative not in qualified:
            raise ValueError("jumper specification is not in the inspected model set")
        item = sources[relative]
        spec = yaml.load((root / relative).read_bytes(), Loader=getattr(yaml, "CSafeLoader", yaml.SafeLoader))
        env = spec.get("environment", {})
        raw = env.get("raw_properties", {})
        waves = env.get("waves", {})
        generic = spec.get("generic", {})
        lines = generic.get("lines", [])
        line_types = generic.get("line_types", [])
        if lines:
            configuration = (f"Installation rig with {len(lines)} line objects and {len(line_types)} line types; "
                             "the source includes crane wires, slings, connectors and a finite element jumper.")
        elif generic:
            configuration = (f"Vessel and buoy specification with {len(line_types)} line types and no line objects; "
                             "jumper geometry is not established by this specification.")
        else:
            configuration = "Parameter override with no object geometry; a standalone jumper configuration is not established."
        groups.append({"group": group, "source": relative, "source_sha256": item["sha256"],
                       "source_url": item["source_url"], "retrieval_date": item["retrieval_date"],
                       "record_count": 1, "configuration": configuration,
                       "water_depth_m": env.get("water", {}).get("depth"),
                       "wave_height_m": waves.get("height", raw.get("WaveHs")),
                       "wave_height_kind": "Hs" if "WaveHs" in raw and not waves else "height",
                       "wave_period_s": waves.get("period", raw.get("WaveTp")),
                       "wave_direction_deg": waves.get("direction", raw.get("WaveDirection")),
                       "current_speed_m_s": env.get("current", {}).get("speed"),
                       "wind_speed_m_s": env.get("wind", {}).get("speed"),
                       "stages_s": spec.get("simulation", {}).get("stages"),
                       "time_step_s": spec.get("simulation", {}).get("time_step")})
    revision = {"revision": "1", "date": groups[0]["retrieval_date"],
                "change": "Initial source-bound library preparation document and execution limits."}
    record = {"source_ref": inventory["source_ref"], "groups": groups, "revisions": [revision]}
    text = ["---", "name: orcaflex-jumper-analysis",
            "description: Prepare inspected jumper-library inputs with the existing format converter; native execution requires separate qualification.",
            "---", "# Jumper library preparation", "",
            "Prepare solver-free inputs from the recorded specifications and retain validator failures and absent comparators. "
            "The specifications do not establish an M-shape, lazy-S or steep-S classification or a verified operational envelope.",
            "", "## Revision history", "", "| Revision | Date | Change and reason |", "| --- | --- | --- |",
            f"| {revision['revision']} | {revision['date']} | {revision['change']} |", "",
            f"Declared revision: {revision['revision']}.", "", "## Specification groups", "",
            "Values below are declared in each specification, not demonstrated response limits or ranges across every override. "
            "Units follow the repository environment schema. Unrecorded values remain unrecorded.", ""]

    def shown(value):
        if value is None:
            return "not recorded"
        if isinstance(value, list):
            return ", ".join(shown(item) for item in value)
        return str(value)

    for row in groups:
        text.extend([f"### {row['group']}", "", row["configuration"], "",
                     f"Source: `{row['source']}`; digest in `jumper-groups.json`.", "",
                     f"Water depth {shown(row['water_depth_m'])} m; wave {row['wave_height_kind']} "
                     f"{shown(row['wave_height_m'])} m, period {shown(row['wave_period_s'])} s, "
                     f"direction {shown(row['wave_direction_deg'])} degrees. "
                     f"Current {shown(row['current_speed_m_s'])} m/s; wind {shown(row['wind_speed_m_s'])} m/s. "
                     f"Simulation stages {shown(row['stages_s'])} s; time step {shown(row['time_step_s'])} s.", ""])
    text.extend(["## Worked preparation", "",
                 "Set `OUTPUT_DIR` to a writable directory outside the repository. This converts one named model without a solver:",
                 "", "```sh",
                 "PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter single2modular docs/domains/orcaflex/jumper/manifold_to_plet/monolithic/DZ_AHCoff.yml -o \"$OUTPUT_DIR/modular\"",
                 "PYTHONPATH=src uv run --no-sources python -m digitalmodel.solvers.orcaflex.format_converter modular2single \"$OUTPUT_DIR/modular/master.yml\" -o \"$OUTPUT_DIR/roundtrip.yml\"",
                 "```", "",
                 "For parametric preparation, edit a copy of a group's `spec.yml`, retain its source digest, "
                 "validate it against `ProjectInputSpec`, then use the same CLI's `spec2modular` command. "
                 "The two specifications without jumper line geometry require that missing basis before a jumper model can be generated.", "",
                 "Use `validators.json` to identify structural failures and `comparators.json` for stored-result availability. "
                 "A conversion exit code does not establish semantic fidelity: the existing semantic-diff taxonomy and converter "
                 "remain the basis for the outstanding conversion campaign (#2014).", "",
                 "## Resonance and execution limits", "",
                 "The `sut_mm/resonance` group records a separate simulation specification. "
                 "The group name alone does not establish a resonant frequency, dynamic amplification or response criterion. "
                 "Those results require qualified native dynamics and read-back against an explicit comparator (#2014). "
                 "No native result is claimed here. The native manifest template keeps load, statics and comparison refused "
                 "until a qualified execution adapter and an approved manifest digest exist.", "",
                 "The document is retained here as a skill draft; installation at the requested skill location and a complete "
                 "group-wide operating envelope remain open (#2015).", ""])
    return {"jumper-groups.json": encode(record),
            "orcaflex-jumper-analysis/SKILL.md": "\n".join(text).encode()}


def main() -> int:
    """Write deterministic inventory; this entry point never launches a solver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--source-ref", required=True)
    parser.add_argument("--retrieved", required=True)
    parser.add_argument("--repository-id", required=True, type=int)
    parser.add_argument("--inventory", type=Path, help="Reuse a recorded inspected inventory")
    parser.add_argument("--prepare", action="store_true")
    parser.add_argument("--criterion", type=Path)
    parser.add_argument("--validation-cache", type=Path)
    parser.add_argument("--skill", action="store_true")
    parser.add_argument("--timed-preparation", type=Path)
    parser.add_argument("--launch", action="store_true")
    args = parser.parse_args()
    if args.launch:
        request_launch({})
    record = (json.loads(args.inventory.read_text()) if args.inventory else
              build_inventory(ROOT, args.source_ref, args.retrieved, args.repository_id))
    if args.inventory and "models" not in record:
        model_bytes = (args.inventory.parent / record["models_file"]).read_bytes()
        if sha256(model_bytes).hexdigest() != record["models_sha256"]:
            raise ValueError("model record digest mismatch")
        record["models"] = json.loads(model_bytes)["models"]
        record.pop("models_file")
        record.pop("models_sha256")
    verify_inputs(record, ROOT)
    if args.inventory:
        verify_reused_inventory(record, ROOT)
    args.output.mkdir(parents=True, exist_ok=True)
    files = inventory_files(record)
    for name, content in files.items():
        (args.output / name).write_bytes(content)
    if args.prepare:
        if args.criterion is None:
            raise ValueError("preparation requires a recorded criterion")
        if args.validation_cache is None:
            raise ValueError("preparation requires an external validation cache")
        criterion = args.criterion.read_bytes()
        files.update({"validators.json": encode(validate_models(record, ROOT, args.validation_cache)),
                      "comparators.json": encode(extract_comparators(record, ROOT))})
        passed = {item["path"] for item in json.loads(files["validators.json"])["models"]
                  if item["status"] == "pass"}
        timed_case = next((item["path"] for item in record["models"]
                           if item["model_type"] == "yaml_model" and item["path"] in passed), "")
        files["criterion.json"] = criterion
        if args.timed_preparation is not None:
            timing = args.timed_preparation.read_bytes()
            timed_case = json.loads(timing)["path"]
            source = next(item for item in record["inputs"] if item["path"] == timed_case)
            if json.loads(timing)["source_sha256"] != source["sha256"] or timed_case not in passed:
                raise ValueError("timed case must have matching inspected bytes and pass validation")
            files["timed-preparation.json"] = timing
            roundtrip_criterion = args.timed_preparation.with_name("roundtrip-criterion.json").read_bytes()
            verify_criterion_order(roundtrip_criterion, json.loads(timing))
            files["roundtrip-criterion.json"] = roundtrip_criterion
        manifest = manifest_template(files, timed_case)
        manifest["criterion_recorded_utc"] = json.loads(criterion)["recorded_utc"]
        files["native-manifest.template.json"] = encode(manifest)
        for name, content in files.items():
            (args.output / name).write_bytes(content)
    if args.skill:
        for name, content in build_jumper_skill(record, ROOT).items():
            target = args.output / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(content)
    print(json.dumps(record["counts"], sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
