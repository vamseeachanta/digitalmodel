from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
from types import SimpleNamespace

import pytest

from digitalmodel.workflows.openfoam_batch_config import resolve_batch_paths
from digitalmodel.workflows.openfoam_batch_layout import WorkLayout
from digitalmodel.workflows import openfoam_batch_layout as layout_module


def test_hosted_root_is_environment_owned_and_output_remains_local(tmp_path: Path) -> None:
    cfg_dir = tmp_path / "scope" / "case"
    cfg_dir.mkdir(parents=True)
    root = tmp_path / "scratch"
    root.mkdir()
    paths = resolve_batch_paths(
        {"work_root_namespace": "gpu-canary"},
        cfg_dir,
        env={
            "DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
            "DIGITALMODEL_WORK_ROOT": str(root),
        },
    )
    assert paths.operator_root == root.resolve()
    assert paths.output_dir == cfg_dir / "results"
    assert paths.namespace == "gpu-canary"
    with pytest.raises(ValueError, match="operator environment"):
        resolve_batch_paths(
            {"work_root_namespace": "gpu-canary", "work_root": str(root)}, cfg_dir,
            env={"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
                 "DIGITALMODEL_WORK_ROOT": str(root)},
        )


@pytest.mark.parametrize("namespace", ["../escape", ".", "a//b", "a\\b", "x\x00y"])
def test_namespace_rejects_nonportable_components(tmp_path: Path, namespace: str) -> None:
    root = tmp_path / "root"
    root.mkdir()
    with pytest.raises(ValueError, match="namespace"):
        resolve_batch_paths(
            {"work_root_namespace": namespace}, tmp_path,
            env={"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
                 "DIGITALMODEL_WORK_ROOT": str(root)},
        )


def test_hosted_missing_root_rejects_before_side_effect(tmp_path: Path) -> None:
    before = set(tmp_path.iterdir())
    with pytest.raises(ValueError, match="DIGITALMODEL_WORK_ROOT"):
        resolve_batch_paths({}, tmp_path, env={"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand"})
    assert set(tmp_path.iterdir()) == before


def test_trusted_local_requires_precreated_absolute_non_git_root(tmp_path: Path) -> None:
    cfg_dir = tmp_path / "checkout" / "case"
    cfg_dir.mkdir(parents=True)
    (tmp_path / "checkout" / ".git").mkdir()
    external = tmp_path / "scratch"
    external.mkdir()
    paths = resolve_batch_paths(
        {"execution_context": "trusted-local", "work_root": str(external)}, cfg_dir, env={}
    )
    assert paths.operator_root == external.resolve()
    with pytest.raises(ValueError, match="Git checkout"):
        resolve_batch_paths(
            {"execution_context": "trusted-local", "work_root": str(cfg_dir)}, cfg_dir, env={}
        )


def test_owned_layout_rejects_foreign_marker_and_cleans_only_case(tmp_path: Path) -> None:
    layout = WorkLayout.create(tmp_path, "ns", "a" * 64)
    case = layout.case_dir("case-001")
    case.mkdir(parents=True)
    (case / "heavy").write_text("x")
    sibling = layout.run_dir / "keep"
    sibling.write_text("safe")
    layout.clean_case("case-001")
    assert not case.exists() and sibling.read_text() == "safe"

    marker = json.loads(layout.marker_path.read_text())
    marker["identity_sha256"] = "b" * 64
    layout.marker_path.write_text(json.dumps(marker))
    with pytest.raises(ValueError, match="owner marker"):
        WorkLayout.create(tmp_path, "ns", "a" * 64)


def test_external_output_must_remain_beneath_input_scope(tmp_path: Path) -> None:
    cfg_dir, root = tmp_path / "input", tmp_path / "scratch"
    cfg_dir.mkdir()
    root.mkdir()
    env = {"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
           "DIGITALMODEL_WORK_ROOT": str(root)}
    for output in (str(tmp_path / "outside"), "../outside"):
        with pytest.raises(ValueError, match="input-local"):
            resolve_batch_paths({"output_dir": output}, cfg_dir, env=env)


def test_layout_rejects_namespace_and_run_symlinks(tmp_path: Path) -> None:
    outside = tmp_path / "outside"
    outside.mkdir()
    try:
        (tmp_path / "linked").symlink_to(outside, target_is_directory=True)
    except OSError:
        pytest.skip("directory symlinks unavailable")
    with pytest.raises(ValueError, match="symlink"):
        WorkLayout.create(tmp_path, "linked/ns", "a" * 64)
    namespace = tmp_path / "safe"
    namespace.mkdir()
    (namespace / f"openfoam-run-{'b' * 64}").symlink_to(outside, target_is_directory=True)
    with pytest.raises(ValueError, match="symlink"):
        WorkLayout.create(tmp_path, "safe", "b" * 64)


def test_stale_lock_requires_dead_process_proof(tmp_path: Path) -> None:
    layout = WorkLayout.create(tmp_path, "ns", "c" * 64)
    lock = layout.run_dir / ".locks" / "run"
    with layout.lock("run"):
        meta_path = lock / "meta.json"
        meta = json.loads(meta_path.read_text())
        assert meta["process_start"]
        meta["heartbeat_epoch"] = 0
        meta_path.write_text(json.dumps(meta))
        with pytest.raises(RuntimeError, match="held"):
            with layout.lock("run", stale_seconds=1):
                pass
    lock.mkdir()
    (lock / "meta.json").write_text(json.dumps({
        "schema": 1, "pid": 2 ** 30, "boot_id": meta["boot_id"],
        "owner_token": layout.owner_token, "heartbeat_epoch": 0,
    }))
    with layout.lock("run", stale_seconds=1):
        assert lock.is_dir()


def test_root_inode_substitution_blocks_clean(tmp_path: Path) -> None:
    layout = WorkLayout.create(tmp_path, "ns", "d" * 64)
    layout.case_dir("case").mkdir()
    moved = tmp_path.with_name(tmp_path.name + "-moved")
    os.replace(tmp_path, moved)
    tmp_path.mkdir()
    with pytest.raises(ValueError, match="root"):
        layout.clean_case("case")


def test_tombstone_substitution_is_never_path_deleted(tmp_path: Path) -> None:
    tombstone = tmp_path / "tombstone"
    tombstone.mkdir()
    expected = tombstone.stat()
    original = tmp_path / "original"
    os.replace(tombstone, original)
    tombstone.mkdir()
    (tombstone / "replacement.txt").write_text("keep")
    if os.name == "nt":
        layout_module._dispose_tombstone(tmp_path, tombstone, expected)
    else:
        with pytest.raises(ValueError, match="changed"):
            layout_module._dispose_tombstone(tmp_path, tombstone, expected)
    assert (tombstone / "replacement.txt").read_text() == "keep"


def test_lock_heartbeat_refreshes_and_process_start_prevents_pid_reuse(tmp_path: Path) -> None:
    import time

    layout = WorkLayout.create(tmp_path, "ns", "e" * 64)
    lock = layout.run_dir / ".locks" / "run"
    with layout.lock("run", stale_seconds=1):
        first = json.loads((lock / "meta.json").read_text())
        time.sleep(1.2)
        second = json.loads((lock / "meta.json").read_text())
        assert second["heartbeat_epoch"] > first["heartbeat_epoch"]
    lock.mkdir()
    (lock / "meta.json").write_text(json.dumps({
        **second, "heartbeat_epoch": 0, "process_start": "reused-pid-start",
    }))
    with layout.lock("run", stale_seconds=1):
        assert lock.is_dir()


@pytest.mark.parametrize("kind", ["directory", "file"])
def test_nested_entry_substitution_inode_is_rejected(kind: str) -> None:
    expected = SimpleNamespace(st_dev=1, st_ino=2)
    replacement = SimpleNamespace(st_dev=1, st_ino=3)
    assert not layout_module._same_inode(expected, replacement), kind


def test_lock_release_never_uses_path_rmtree(tmp_path: Path, monkeypatch) -> None:
    layout = WorkLayout.create(tmp_path, "ns", "f" * 64)

    def forbidden(*args, **kwargs):
        raise AssertionError("path-based rmtree used")

    monkeypatch.setattr(shutil, "rmtree", forbidden)
    with layout.lock("run"):
        pass
