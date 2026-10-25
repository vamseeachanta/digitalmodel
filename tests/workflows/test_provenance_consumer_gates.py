"""Synthetic provenance contracts from issue 2141; no engineering limits.

Expected identities are independently hashed literal bytes. Archive inventories
are compared with the literal fixture file set, not a production-generated list.
"""

from hashlib import sha256
import json

import pytest

from digitalmodel.workflows import installation_replay as replay
from digitalmodel.workflows import installation_retention as retention
from tests.workflows.test_installation_retention import fixture_run


@pytest.fixture
def replay_manifest(tmp_path):
    """Use the existing version-one schema with a synthetic single-case matrix."""
    entries = {}
    for name in replay.REQUIRED_INPUTS:
        path = tmp_path / f"{name}.json"
        path.write_bytes(b"{}")
        entries[name] = {"path": path.name, "sha256": sha256(b"{}").hexdigest()}
    matrix = {"master_sha256": entries["master"]["sha256"],
              "cases": [{"change_sha256": entries["change"]["sha256"]}]}
    raw = json.dumps(matrix).encode()
    (tmp_path / "matrix.json").write_bytes(raw)
    entries["matrix"]["sha256"] = sha256(raw).hexdigest()
    record = {"version": 1, "case_index": 0, "inputs": entries,
              "retention": {"simulation": False}, "timeout_seconds": 1,
              "comparison": {"absolute_tolerance": 0, "relative_tolerance": 0},
              "pilot_limitations": ["Synthetic provenance test only"],
              "runtime": {"solver_version": "synthetic", "external_files": []}}
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(record))
    return path, record, tmp_path / "output"


@pytest.mark.parametrize("declaration", [None, {}, "", [None], [{}],
                                       [{"path": "", "sha256": ""}]])
def test_external_inventory_requires_explicit_typed_entries(replay_manifest, declaration):
    path, record, output = replay_manifest
    record["runtime"]["external_files"] = declaration
    path.write_text(json.dumps(record))
    with pytest.raises(ValueError, match="external_files"):
        replay.validate_manifest(path, output, check_environment=False)
    assert not output.exists()


def test_omitted_external_inventory_is_not_an_empty_declaration(replay_manifest):
    path, record, output = replay_manifest
    del record["runtime"]["external_files"]
    path.write_text(json.dumps(record))
    with pytest.raises(ValueError, match="external_files"):
        replay.validate_manifest(path, output, check_environment=False)
    assert not output.exists()


def test_explicit_empty_inventory_remains_supported(replay_manifest):
    path, record, output = replay_manifest
    actual, _ = replay.validate_manifest(path, output, check_environment=False)
    assert actual == record
    assert not output.exists()


@pytest.mark.parametrize("runtime", [None, [], "synthetic"])
def test_external_inventory_requires_a_runtime_mapping(replay_manifest, runtime):
    path, record, output = replay_manifest
    record["runtime"] = runtime
    path.write_text(json.dumps(record))
    with pytest.raises(ValueError, match="runtime.*external_files"):
        replay.validate_manifest(path, output, check_environment=False)
    assert not output.exists()


@pytest.mark.parametrize("defect", ["missing", "changed", "duplicate", "extra_key",
                                  "bad_digest", "blank_path"])
def test_external_identity_fails_before_environment_or_outputs(replay_manifest, monkeypatch, defect):
    """Pinned b'original' must reject b'changed' or an absent file exactly."""
    path, record, output = replay_manifest
    external = path.parent / "runtime.bin"
    external.write_bytes(b"original")
    entry = {"path": external.name, "sha256": sha256(b"original").hexdigest()}
    record["runtime"]["external_files"] = [entry]
    if defect == "missing":
        external.unlink()
    elif defect == "changed":
        external.write_bytes(b"changed")
    elif defect == "duplicate":
        record["runtime"]["external_files"].append(dict(entry, path="./runtime.bin"))
    elif defect == "extra_key":
        entry["unrecognized"] = True
    elif defect == "bad_digest":
        entry["sha256"] = "not-a-digest"
    else:
        entry["path"] = "  "
    path.write_text(json.dumps(record))
    monkeypatch.setattr(replay, "_environment", lambda *args: pytest.fail("Environment reached"))
    with pytest.raises(ValueError, match="[Ee]xternal"):
        replay.validate_manifest(path, output)
    assert not output.exists()


def test_valid_external_identity_preserves_source_bytes(replay_manifest, monkeypatch):
    path, record, output = replay_manifest
    external = path.parent / "runtime.bin"
    external.write_bytes(b"original")
    record["runtime"]["external_files"] = [
        {"path": external.name, "sha256": sha256(b"original").hexdigest()}]
    path.write_text(json.dumps(record))
    monkeypatch.setattr(replay, "_environment", lambda *args: None)
    replay.validate_manifest(path, output)
    assert external.read_bytes() == b"original"
    assert not output.exists()


def test_archive_declares_partial_scope_and_excluded_inputs(tmp_path):
    """The literal native input, simulation and log must be excluded and pinned."""
    run = fixture_run(tmp_path)
    omitted = {"source/model.yml": b"synthetic input", "model.sim": b"synthetic result",
               "solver.log": b"synthetic diagnostic"}
    for name, raw in omitted.items():
        path = run / name
        path.parent.mkdir(exist_ok=True)
        path.write_bytes(raw)
    root = tmp_path / "source"
    target = tmp_path / "archive/version"
    result = retention.create_archive(root, [{"path": run.relative_to(root).as_posix()}],
                                      target, target.parent)
    assert result["archive_kind"] == "postprocessing_only"
    assert result["rerun_bundle_complete"] is False
    excluded = result["runs"][0]["coverage"]["excluded_files"]
    assert {row["path"] for row in excluded} == set(omitted)
    for row in excluded:
        assert row["sha256"] == sha256(omitted[row["path"]]).hexdigest()
        assert row["bytes"] == len(omitted[row["path"]])
        assert row["disposition"] == "excluded_from_postprocessing_archive"
        assert not (target / run.relative_to(root) / row["path"]).exists()
        assert (run / row["path"]).read_bytes() == omitted[row["path"]]
    assert json.loads((target / "manifest.json").read_bytes()) == result
    assert not (target / ".incomplete").exists()


def test_failed_archive_copy_retains_incomplete_marker(tmp_path, monkeypatch):
    run = fixture_run(tmp_path)
    root = tmp_path / "source"
    target = tmp_path / "archive/version"

    def fail_copy(*args):
        raise OSError("Synthetic copy failure")

    monkeypatch.setattr(retention, "_copy_run", fail_copy)
    with pytest.raises(OSError, match="Synthetic"):
        retention.create_archive(root, [{"path": run.relative_to(root).as_posix()}],
                                 target, target.parent)
    assert (target / ".incomplete").is_file()
    assert not (target / "manifest.json").exists()


def test_failed_manifest_write_retains_incomplete_marker(tmp_path, monkeypatch):
    run = fixture_run(tmp_path)
    root = tmp_path / "source"
    target = tmp_path / "archive/version"

    def fail_write(*args, **kwargs):
        raise OSError("Synthetic manifest failure")

    monkeypatch.setattr(retention.json, "dump", fail_write)
    with pytest.raises(OSError, match="Synthetic"):
        retention.create_archive(root, [{"path": run.relative_to(root).as_posix()}],
                                 target, target.parent)
    assert (target / ".incomplete").is_file()


@pytest.mark.parametrize("mutation", ["changed", "added", "removed"])
def test_excluded_inventory_is_rechecked_before_manifest(tmp_path, monkeypatch, mutation):
    """An excluded byte change, added file or removed file voids the inventory."""
    run = fixture_run(tmp_path)
    excluded = run / "model.sim"
    excluded.write_bytes(b"original")
    root = tmp_path / "source"
    target = tmp_path / "archive/version"
    copy_run = retention._copy_run

    def tamper(*args):
        files = copy_run(*args)
        if mutation == "changed":
            excluded.write_bytes(b"changed")
        elif mutation == "added":
            (run / "extra.bin").write_bytes(b"new")
        else:
            excluded.unlink()
        return files

    monkeypatch.setattr(retention, "_copy_run", tamper)
    with pytest.raises(ValueError, match="[Ee]xcluded"):
        retention.create_archive(root, [{"path": run.relative_to(root).as_posix()}],
                                 target, target.parent)
    assert (target / ".incomplete").is_file()
    assert not (target / "manifest.json").exists()
