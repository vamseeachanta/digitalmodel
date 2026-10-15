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
    library = tmp_path / "docs/domains/orcaflex/jumper"
    library.mkdir(parents=True)
    for name in ("b.yml", "a.dat"):
        (library / name).write_bytes(b"General: {}\n")
    (library / "notes.md").write_text("Not a model.\n")
    records = module.inventory(tmp_path)
    assert [r["path"] for r in records] == [
        "docs/domains/orcaflex/jumper/a.dat",
        "docs/domains/orcaflex/jumper/b.yml",
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
    library = tmp_path / "docs/domains/orcaflex/training"
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
