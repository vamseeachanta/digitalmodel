# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build preparation records from complete, content-inspected library models."""

from __future__ import annotations

import argparse
import csv
import importlib.util
import json
from hashlib import sha256
from datetime import datetime, timezone
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
        if not path.is_file():
            raise ValueError("recorded input absent; regenerate legacy-round-trip records")
        if sha256(path.read_bytes()).hexdigest() != item["sha256"]:
            raise ValueError("source digest mismatch; regenerate legacy-round-trip records")


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
    current = inspection.publication_records(inspection.audit(root), root)
    if current["counts"] != inventory.get("counts") or current["models"] != inventory["models"]:
        raise ValueError("library evidence changed; regenerate legacy-round-trip records")


def inventory_files(record: dict) -> dict[str, bytes]:
    """Split input and model records below the repository's file-size hook limit."""
    models = encode({"models": record["models"]})
    index = {key: value for key, value in record.items() if key != "models"}
    index.update(models_file="models.json", models_sha256=sha256(models).hexdigest())
    return {"inventory.json": encode(index), "models.json": models}


def validator_source_digests() -> dict[str, str]:
    """Identify the validator implementation used for records and cache keys."""
    source = ROOT / "src/digitalmodel/solvers/orcaflex"
    return {name: sha256((source / name).read_bytes()).hexdigest()
            for name in ("yaml_validator.py", "yaml_timestep_validation.py",
                         "starting_shape_validation.py")}


def validation_cache_path(cache_dir: Path, item: dict) -> Path:
    """Bind resumable checks to input bytes and all validator source modules."""
    import yaml

    key = {"path": item["path"], "sha256": item["sha256"], "yaml_version": yaml.__version__,
           "validators": validator_source_digests()}
    return cache_dir / (sha256(encode(key)).hexdigest() + ".json")


def cache_validation(cache_dir: Path, item: dict, result: dict) -> None:
    """Cache externally; a self-digest detects corruption, not deliberate editing."""
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
            "validator_sources_sha256": validator_source_digests(),
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
    """Describe inspected specifications without inferring missing geometry."""
    verify_inputs(inventory, root)
    spec = importlib.util.spec_from_file_location("legacy_skill", CHECKER.with_name("legacy_library_skill.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.build_skill(inventory, root, encode)


def parse_args():
    """Read preparation options without importing an execution interface."""
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
    parser.add_argument("--started-utc", help="Recorded preparation start for deterministic replay")
    return parser.parse_args()


def load_inventory(args):
    """Load a complete inventory, rederiving counts when evidence is reused."""
    if args.launch:
        request_launch({})
    record = (json.loads(args.inventory.read_text(encoding="utf-8")) if args.inventory else
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
    return record


def preparation_files(args, record, files):
    """Bind judged records to criteria that precede preparation."""
    if args.criterion is None:
        raise ValueError("preparation requires a recorded criterion")
    if args.validation_cache is None:
        raise ValueError("preparation requires an external validation cache")
    criterion = args.criterion.read_bytes()
    started = args.started_utc or datetime.now(timezone.utc).isoformat()
    if datetime.fromisoformat(started) > datetime.now(timezone.utc):
        raise ValueError("preparation start cannot be in the future")
    binding = {"criterion_sha256": sha256(criterion).hexdigest(), "started_utc": started}
    verify_criterion_order(criterion, binding)
    files.update({"criterion.json": criterion,
                  "validators.json": encode({**validate_models(record, ROOT, args.validation_cache), **binding}),
                  "comparators.json": encode({**extract_comparators(record, ROOT), **binding})})
    passed = {item["path"] for item in json.loads(files["validators.json"])["models"]
              if item["status"] == "pass"}
    timed_case = next((item["path"] for item in record["models"]
                       if item["model_type"] == "yaml_model" and item["path"] in passed), "")
    if args.timed_preparation is not None:
        timing = args.timed_preparation.read_bytes()
        timed_case = json.loads(timing)["path"]
        source = next((item for item in record["inputs"] if item["path"] == timed_case), None)
        if source is None or json.loads(timing)["source_sha256"] != source["sha256"] or timed_case not in passed:
            raise ValueError("timed case must have matching inspected bytes and pass validation")
        files["timed-preparation.json"] = timing
        roundtrip_criterion = args.timed_preparation.with_name("roundtrip-criterion.json").read_bytes()
        verify_criterion_order(roundtrip_criterion, json.loads(timing))
        files["roundtrip-criterion.json"] = roundtrip_criterion
    return timed_case


def main() -> int:
    """Write complete deterministic records; this entry point never launches a solver."""
    args = parse_args()
    record = load_inventory(args)
    files = inventory_files(record)
    timed_case = preparation_files(args, record, files) if args.prepare else ""
    if args.skill:
        files.update(build_jumper_skill(record, ROOT))
    if args.prepare:
        manifest = manifest_template(files, timed_case)
        manifest["criterion_recorded_utc"] = json.loads(files["criterion.json"])["recorded_utc"]
        files["native-manifest.template.json"] = encode(manifest)
    for name, content in files.items():
        target = args.output / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)
    print(json.dumps(record["counts"], sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
