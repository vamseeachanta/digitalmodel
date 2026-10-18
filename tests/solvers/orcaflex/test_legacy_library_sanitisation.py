# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Per-file sanitisation must distinguish inspected text from opaque inputs."""

from __future__ import annotations

import importlib.util
import json
import sys
from hashlib import sha256
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts/orcaflex/check_legacy_library_sanitisation.py"


def implementation():
    spec = importlib.util.spec_from_file_location("legacy_sanitisation", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_inventory_is_sorted_and_records_source_bytes(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/mooring/buoy"
    library.mkdir(parents=True)
    for name in ("b.yml", "a.dat"):
        (library / name).write_bytes(b"General: {}\n")
    (library / "notes.md").write_text("Not a model.\n")
    records = module.inventory(tmp_path)
    assert [r["path"] for r in records] == [
        "docs/domains/orcaflex/mooring/buoy/a.dat",
        "docs/domains/orcaflex/mooring/buoy/b.yml",
    ]
    assert all(r["sha256"] == sha256(b"General: {}\n").hexdigest() for r in records)
    assert all(r["size_bytes"] == len(b"General: {}\n") for r in records)


@pytest.mark.parametrize(
    ("path", "kind"),
    [
        ("jumper/a.dat", "native_binary"),
        ("jumper/spec.yml", "specification"),
        ("mooring/master.yml", "modular_master"),
        ("mooring/includes/a.yml", "include_fragment"),
        ("mooring/inputs/parameters.yml", "parameters"),
        ("installation/model.yaml", "yaml_model"),
    ],
)
def test_inventory_roles_are_file_roles_not_geometry_guesses(path, kind):
    assert implementation().model_type(Path(path)) == kind


def test_inspected_text_has_a_per_file_pass_record(tmp_path):
    module = implementation()
    path = tmp_path / "model.yml"
    path.write_bytes(b"General:\n  UnitsSystem: SI\n")
    record = module.inspect_model(path, "model.yml", module.gate.load_rules())
    assert record["status"] == "pass"
    assert record["inspected_files"] == 1
    assert record["rules"] == []


def test_binary_is_a_failure_even_if_a_baseline_accepts_its_digest(tmp_path):
    module = implementation()
    path = tmp_path / "model.dat"
    path.write_bytes(b"\x00\x01opaque-model\x00")
    record = module.inspect_model(path, "model.dat", module.gate.load_rules())
    assert record["status"] == "fail"
    assert record["inspected_files"] == 0
    assert record["rules"] == ["content-uninspectable"]
    assert "opaque-model" not in json.dumps(record)


def test_findings_are_reported_as_rules_without_quoting_content(tmp_path):
    module = implementation()
    path = tmp_path / "model.yml"
    synthetic = "# " + "User:" + " sample"
    path.write_text(synthetic + "\nGeneral: {}\n")
    record = module.inspect_model(path, "model.yml", module.gate.load_rules())
    assert record["status"] == "fail"
    assert record["rules"]
    assert synthetic not in json.dumps(record)
    assert "BaseFile" not in json.dumps(record)


def test_report_is_deterministic_and_has_one_record_per_model(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/mooring/buoy"
    library.mkdir(parents=True)
    (library / "text.yml").write_text("General: {}\n")
    (library / "opaque.dat").write_bytes(b"\x00\x01\x02")
    first = module.audit(tmp_path)
    second = module.audit(tmp_path)
    assert first == second
    assert first["summary"] == {"total": 2, "pass": 1, "fail": 1}
    assert len(first["models"]) == len(module.inventory(tmp_path))
    assert first["publication_ready"] is False


def test_output_inside_the_repository_is_refused(tmp_path):
    module = implementation()
    with pytest.raises(ValueError, match="outside the repository"):
        module.write_report(tmp_path / "audit.json", {}, tmp_path)


def test_diagnostics_never_import_the_native_solver():
    module = implementation()
    import ast

    tree = ast.parse(SCRIPT.read_text())
    imports = [
        node.module if isinstance(node, ast.ImportFrom) else alias.name
        for node in ast.walk(tree)
        if isinstance(node, (ast.Import, ast.ImportFrom))
        for alias in node.names
    ]
    assert all("OrcFxAPI" not in (name or "") for name in imports)
    assert module.LIBRARY_GROUPS == ("jumper", "mooring", "installation", "training")


def fixture_publication(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/mooring/buoy"
    library.mkdir(parents=True)
    (library / "hidden.dat").write_bytes(b"\x00private-fixture\x01")
    (library / "dependent.yml").write_text("BaseFile: hidden.dat\n")
    (library / "indirect.yml").write_text("BaseFile: dependent.yml\n")
    (library / "clean.yml").write_text("General: {}\n")
    return module, module.audit(tmp_path)


def test_publication_omits_failed_input_and_all_dependent_models(tmp_path):
    module, report = fixture_publication(tmp_path)
    published = module.publication_records(report, tmp_path)
    encoded = json.dumps(published)
    failed = next(item for item in report["models"] if item["status"] == "fail")
    for withheld in (failed["path"], "hidden.dat", failed["sha256"], "private-fixture"):
        assert withheld not in encoded
    assert [item["path"] for item in published["models"]] == [
        "docs/domains/orcaflex/mooring/buoy/clean.yml"
    ]
    # Passing overrides of an excluded model must not leak dependency names.
    assert "dependent.yml" not in encoded
    assert "indirect.yml" not in encoded


def test_publication_counts_account_for_entire_library(tmp_path):
    module, report = fixture_publication(tmp_path)
    counts = module.publication_records(report, tmp_path)["counts"]
    assert counts == {
        "library_inputs": 4, "published_inputs": 1, "excluded_inputs": 3,
        "failed_inspections": 1, "excluded_models": 3, "published_models": 1,
        "models_with_failed_inputs": 3, "models_with_missing_inputs": 0,
        "models_with_unknown_dependencies": 0,
    }
    assert counts["published_inputs"] + counts["excluded_inputs"] == len(report["models"])


def test_every_recorded_input_passes_a_fresh_inspection(tmp_path):
    module, report = fixture_publication(tmp_path)
    published = module.publication_records(report, tmp_path)
    for item in published["inputs"]:
        assert module.inspect_model(
            tmp_path / item["path"], item["path"], module.gate.load_rules()
        )["status"] == "pass"
    assert published == module.publication_records(report, tmp_path)


def test_publication_refuses_source_digest_mismatch(tmp_path):
    module, report = fixture_publication(tmp_path)
    (tmp_path / "docs/domains/orcaflex/mooring/buoy/clean.yml").write_text("General: null\n")
    with pytest.raises(ValueError, match="digest mismatch"):
        module.publication_records(report, tmp_path)


def test_publication_checks_sibling_inputs_of_modular_model(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/mooring/buoy/case"
    (library / "includes").mkdir(parents=True)
    (library / "master.yml").write_text("- includefile: includes/section.yml\n")
    (library / "includes/section.yml").write_bytes(b"\x00\x01")
    published = module.publication_records(module.audit(tmp_path), tmp_path)
    assert published["models"] == []
    assert published["inputs"] == []
    assert published["counts"]["excluded_models"] == 1


def test_publication_omits_models_with_absent_inputs(tmp_path):
    module = implementation()
    library = tmp_path / "docs/domains/orcaflex/mooring/buoy"
    library.mkdir(parents=True)
    (library / "override.yml").write_text("BaseFile: absent.dat\n")
    published = module.publication_records(module.audit(tmp_path), tmp_path)
    assert published["models"] == []
    assert published["inputs"] == []
    assert published["counts"]["models_with_missing_inputs"] == 1
    assert "absent.dat" not in json.dumps(published)


def test_reference_outside_library_is_not_silently_accepted(tmp_path):
    module = implementation()
    root = tmp_path / "source"
    library = root / "docs/domains/orcaflex/mooring/buoy"
    library.mkdir(parents=True)
    (library / "model.yml").write_text("BaseFile: ../../../../../external.dat\n")
    report = module.audit(root)
    assert report["summary"]["pass"] == 1
    result = module.publication_records(report, root)
    assert result["models"] == []
    assert result["counts"]["models_with_missing_inputs"] == 1
