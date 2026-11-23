"""Pinned local qualification must reject drift before importing a solver."""
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.smoke.model_manifest import load_contract, verify_manifest


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def manifest(tmp_path, monkeypatch):
    monkeypatch.setenv("PYTHONHASHSEED", "0")
    contract = load_contract()
    (tmp_path / "source.yml").write_text("metadata: {}\n")
    (tmp_path / "master.yml").write_text("- includefile: general.yml\n")
    (tmp_path / "general.yml").write_text("General:\n  RestartStateRecordingTest: ''\n")
    data = dict(schema_version=1, case_id=contract["case_id"], run_id="test-run",
                source_revision="a" * 40, source={"path": "source.yml", "sha256": sha(tmp_path / "source.yml")},
                master="master.yml", contract=contract, python_hash_seed="0",
                files=[{"path": name, "sha256": sha(tmp_path / name)}
                       for name in ("general.yml", "master.yml", "source.yml")],
                outputs={"simulation": "solve/model.sim", "solve_results": "solve/results.json",
                         "readback_results": "readback/results.json"})
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(data))
    return path, data


def update(path, data):
    path.write_text(json.dumps(data))


def rehash(path, data, filename):
    for record in data["files"]:
        if record["path"] == filename:
            record["sha256"] = sha(path.parent / filename)
    update(path, data)


def test_valid_bundle_returns_resolved_root(manifest):
    path, _ = manifest
    assert verify_manifest(path)["_root"] == path.parent


@pytest.mark.parametrize("field,value", [("schema_version", True), ("extra", 1),
                                        ("source_revision", "bad"), ("python_hash_seed", "1")])
def test_invalid_manifest_fields_rejected(manifest, field, value):
    path, data = manifest
    data[field] = value
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_environment_seed_must_match(manifest, monkeypatch):
    monkeypatch.delenv("PYTHONHASHSEED")
    with pytest.raises(ValueError):
        verify_manifest(manifest[0])


def test_contract_cannot_drop_a_required_result(manifest):
    path, data = manifest
    data["contract"]["results"] = []
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_contract_boolean_cannot_impersonate_thread_count(manifest):
    path, data = manifest
    data["contract"]["resources"]["threads"] = True
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("name", ["../escape.yml", "C:/escape.yml", "sub/../general.yml"])
def test_path_escapes_rejected(manifest, name):
    path, data = manifest
    data["master"] = name
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_changed_file_is_rejected(manifest):
    path, _ = manifest
    (path.parent / "general.yml").write_text("General: {}")
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_unlisted_include_rejected_even_when_hashes_match(manifest):
    path, data = manifest
    (path.parent / "extra.yml").write_text("General: {}")
    (path.parent / "master.yml").write_text("- includefile: extra.yml\n")
    rehash(path, data, "master.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("value", ["~", "'print(1)'", "false"])
def test_nonempty_or_null_script_rejected(manifest, value):
    path, data = manifest
    (path.parent / "general.yml").write_text("General:\n  RestartStateRecordingTest: " + value)
    rehash(path, data, "general.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("text", ["General: &a {Loop: *a}",
                                 "General: {}\nGeneral: {}",
                                 "General: {PythonScript: run_me}",
                                 "Environment: {WaveTimeHistoryFileName: external.txt}",
                                 "- includefile: master.yml"])
def test_ambiguous_hooks_and_include_cycles_rejected(manifest, text):
    path, data = manifest
    (path.parent / "general.yml").write_text(text)
    rehash(path, data, "general.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_duplicate_manifest_keys_rejected(manifest):
    path, _ = manifest
    text = path.read_text()
    path.write_text('{"schema_version": 1,' + text[1:])
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_duplicate_file_identity_rejected(manifest):
    path, data = manifest
    data["files"].append(data["files"][0])
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)
