# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Preparation records must be reproducible, complete and solver-free."""

import importlib.util
import json
import sys
from hashlib import sha256
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts/orcaflex/prepare_legacy_library.py"


def implementation():
    spec = importlib.util.spec_from_file_location("library_preparation", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_inventory_build_is_reproducible_and_records_provenance(tmp_path):
    module = implementation()
    library = tmp_path / "source/docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    (library / "model.yml").write_text("General: {}\n")
    root = tmp_path / "source"
    first = module.build_inventory(root, "a" * 40, "2026-10-15", 123)
    assert first == module.build_inventory(root, "a" * 40, "2026-10-15", 123)
    assert first["counts"]["published_models"] == 1
    item = first["inputs"][0]
    assert item["source_url"].startswith("https://api.github.com/repositories/123/")
    assert item["retrieval_date"] == "2026-10-15"
    assert item["record_count"] == 1
    assert item["sha256"] == sha256((library / "model.yml").read_bytes()).hexdigest()
    assert json.loads(module.encode(first)) == first


def test_validator_records_invalid_models_without_emitting_source_content(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    (library / "good.yml").write_text("General: {}\nEnvironment: {}\n")
    (library / "bad.yml").write_text("General: [\n")
    inventory = module.build_inventory(tmp_path, "a" * 40, "2026-10-15", 123)
    results = module.validate_models(inventory, tmp_path)
    assert len(results["models"]) == 2
    by_name = {Path(item["path"]).name: item for item in results["models"]}
    assert by_name["bad.yml"]["status"] == "fail"
    assert by_name["bad.yml"]["errors"] > 0
    assert by_name["good.yml"]["status"] == "pass"
    assert "General: [" not in json.dumps(results)


def test_comparator_preserves_stored_values_and_explains_missing_results(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    for name in ("stored", "missing"):
        (library / f"{name}.yml").write_text("General: {}\n")
    csv = library / "stored.csv"
    csv.write_text("Time (s),Tension (kN)\n0,12.34\n1,15.67\n")
    inventory = module.build_inventory(tmp_path, "a" * 40, "2026-10-15", 123)
    results = module.extract_comparators(inventory, tmp_path)
    by_name = {Path(item["path"]).stem: item for item in results["models"]}
    assert by_name["stored"]["status"] == "stored"
    assert by_name["stored"]["tables"][0]["rows"] == [["0", "12.34"], ["1", "15.67"]]
    assert by_name["stored"]["tables"][0]["sha256"] == sha256(csv.read_bytes()).hexdigest()
    assert by_name["missing"]["status"] == "unavailable"
    assert by_name["missing"]["reason"]
    assert results == module.extract_comparators(inventory, tmp_path)


def test_manifest_binds_each_deliverable_and_refuses_launch(tmp_path):
    module = implementation()
    files = {"inventory.json": b"{}\n", "validators.json": b"{}\n", "comparators.json": b"{}\n"}
    manifest = module.manifest_template(files, "training/model.yml")
    assert manifest["operations"] == ["load", "statics", "compare"]
    assert manifest["native_elapsed_seconds"] is None
    assert manifest["launch_status"] == "refused"
    for name, digest in manifest["files"].items():
        assert digest == sha256(files[name]).hexdigest()
    import pytest
    with pytest.raises(RuntimeError, match="qualified execution adapter"):
        module.request_launch(manifest)


def test_timed_preparation_is_distinct_from_unrun_native_operations():
    module = implementation()
    timing = {"path": "training/model.yml", "elapsed_seconds": 1.2,
              "native_solver_used": False, "operations": ["single2modular", "modular2single"]}
    manifest = module.manifest_template({"timed-preparation.json": module.encode(timing)}, timing["path"])
    assert manifest["preparation_elapsed_seconds"] == 1.2
    assert manifest["native_elapsed_seconds"] is None
    assert manifest["launch_status"] == "refused"


def test_split_inventory_preserves_complete_records_and_binds_models(tmp_path):
    module = implementation()
    record = {"inputs": [], "models": [{"path": "model.yml"}], "counts": {}}
    files = module.inventory_files(record)
    index = json.loads(files["inventory.json"])
    assert "models" not in index
    assert index["models_sha256"] == sha256(files["models.json"]).hexdigest()
    assert json.loads(files["models.json"])["models"] == record["models"]


def test_criterion_digest_and_time_precede_the_run():
    module = implementation()
    criterion = module.encode({"recorded_utc": "2026-10-15T01:00:00+00:00"})
    run = {"criterion_sha256": sha256(criterion).hexdigest(),
           "started_utc": "2026-10-15T01:00:01+00:00"}
    module.verify_criterion_order(criterion, run)
    import pytest
    with pytest.raises(ValueError, match="criterion digest"):
        module.verify_criterion_order(criterion + b" ", run)
    run["started_utc"] = "2026-10-15T00:59:59+00:00"
    with pytest.raises(ValueError, match="precede"):
        module.verify_criterion_order(criterion, run)


def test_resume_cache_is_bound_to_input_bytes(tmp_path):
    module = implementation()
    root = tmp_path / "source"
    library = root / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    path = library / "model.yml"
    path.write_text("General: {}\nEnvironment: {}\n")
    inventory = module.build_inventory(root, "a" * 40, "2026-10-15", 123)
    cache = tmp_path / "cache"
    first = module.validate_models(inventory, root, cache_dir=cache)
    times = {p.name: p.stat().st_mtime_ns for p in cache.iterdir()}
    assert first == module.validate_models(inventory, root, cache_dir=cache)
    assert times == {p.name: p.stat().st_mtime_ns for p in cache.iterdir()}
    path.write_text("General: [\n")
    changed = module.build_inventory(root, "a" * 40, "2026-10-15", 123)
    assert module.validate_models(changed, root, cache_dir=cache)["models"][0]["status"] == "fail"
    assert len(list(cache.iterdir())) == 2


def test_committed_records_cover_models_and_match_manifest_digests():
    module = implementation()
    output = ROOT / "docs/domains/orcaflex/library/legacy-round-trip"
    inventory = json.loads((output / "inventory.json").read_bytes())
    models = json.loads((output / "models.json").read_bytes())["models"]
    validators = json.loads((output / "validators.json").read_bytes())
    comparators = json.loads((output / "comparators.json").read_bytes())
    manifest = json.loads((output / "native-manifest.template.json").read_bytes())
    paths = {item["path"] for item in models}
    assert {item["path"] for item in validators["models"]} == paths
    assert {item["path"] for item in comparators["models"]} == paths
    assert all(item["status"] == "stored" or item.get("reason") for item in comparators["models"])
    inputs = {item["path"] for item in inventory["inputs"]}
    assert all(set(model["inputs"]) <= inputs for model in models)
    assert inventory["counts"]["published_inputs"] + inventory["counts"]["excluded_inputs"] == inventory["counts"]["library_inputs"]
    assert inventory["counts"]["published_models"] == len(models)
    assert sha256((output / "models.json").read_bytes()).hexdigest() == inventory["models_sha256"]
    for name, digest in manifest["files"].items():
        assert sha256((output / name).read_bytes()).hexdigest() == digest
    rules = module.inspection.gate.load_rules()
    for item in inventory["inputs"]:
        assert sha256((ROOT / item["path"]).read_bytes()).hexdigest() == item["sha256"]
        assert module.inspection.inspect_model(ROOT / item["path"], item["path"], rules)["status"] == "pass"


def test_skill_document_is_built_from_all_four_inspected_specifications(tmp_path):
    module = implementation()
    for group in ("manifold_to_plet", "plet_to_plem", "sut_mm", "sut_mm/resonance"):
        path = tmp_path / "docs/domains/orcaflex/jumper" / group / "spec.yml"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("environment:\n  water:\n    depth: 100\ngeneric: {}\n")
    inventory = module.build_inventory(tmp_path, "a" * 40, "2026-10-15", 123)
    files = module.build_jumper_skill(inventory, tmp_path)
    assert files == module.build_jumper_skill(inventory, tmp_path)
    source = json.loads(files["jumper-groups.json"])
    assert len(source["groups"]) == 4
    assert all(row["source_sha256"] == sha256((tmp_path / row["source"]).read_bytes()).hexdigest()
               for row in source["groups"])
    skill = files["orcaflex-jumper-analysis/SKILL.md"].decode()
    assert "name: orcaflex-jumper-analysis" in skill
    latest = source["revisions"][-1]
    assert f"Declared revision: {latest['revision']}." in skill
    assert f"| {latest['revision']} | {latest['date']} | {latest['change']} |" in skill
    for row in source["groups"]:
        assert row["source"] in skill
    path = tmp_path / "finished.md"
    path.write_text(skill)
    assert module.inspection.inspect_model(path, "finished.md", module.inspection.gate.load_rules())["status"] == "pass"


def test_reused_inventory_cannot_relabel_a_failed_inspection(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    (library / "opaque.dat").write_bytes(b"\x00\x01")
    inventory = {"inputs": [{**item, "status": "pass"} for item in module.inspection.inventory(tmp_path)],
                 "models": []}
    import pytest
    with pytest.raises(ValueError, match="content inspection"):
        module.verify_reused_inventory(inventory, tmp_path)


def test_reused_inventory_cannot_omit_a_model_dependency(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    (library / "model.yml").write_text("BaseFile: missing.dat\n")
    inputs = [{**item, "status": "pass"} for item in module.inspection.inventory(tmp_path)]
    inventory = {"inputs": inputs, "models": [{"path": inputs[0]["path"], "inputs": [inputs[0]["path"]],
                 "model_type": "yaml_model", "missing_dependency_count": 0}]}
    import pytest
    with pytest.raises(ValueError, match="model dependency"):
        module.verify_reused_inventory(inventory, tmp_path)


def test_resume_rejects_reclassified_input_even_with_matching_bytes(tmp_path):
    module = implementation()
    root = tmp_path / "source"
    library = root / "docs/domains/orcaflex/training"
    library.mkdir(parents=True)
    (library / "model.yml").write_text("General: [\n")
    inventory = module.build_inventory(root, "a" * 40, "2026-10-15", 123)
    assert module.validate_models(inventory, root, cache_dir=tmp_path / "cache")["models"][0]["status"] == "fail"
    inventory["inputs"][0]["model_type"] = "specification"
    import pytest
    with pytest.raises(ValueError, match="file role"):
        module.validate_models(inventory, root, cache_dir=tmp_path / "cache")
