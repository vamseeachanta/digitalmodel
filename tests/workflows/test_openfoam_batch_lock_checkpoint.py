"""Lock liveness and external checkpoint-v2 contract tests."""

import json
import threading
import time
from pathlib import Path

import pytest

from digitalmodel.workflows import openfoam_batch_layout as layout_module
from digitalmodel.workflows import openfoam_batch_results as result_module
from digitalmodel.workflows.openfoam_batch_config import ExecutionAuthority


IDENTITY = {
    "schema_version": 1,
    "identity_kind": "openfoam-run-v1",
    "identity_sha256": "c" * 64,
}


def _layout(tmp_path: Path):
    root = tmp_path / "root"
    root.mkdir()
    authority = ExecutionAuthority("trusted-local", root, Path("runs"))
    return layout_module.WorkLayout.create(authority, IDENTITY, "cases")


def _record(**changes):
    record = {
        "schema_version": 1,
        "owner_token": "owner",
        "boot_id": "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa",
        "pid": 12,
        "process_start_token": "start-a",
        "heartbeat": 100.0,
    }
    record.update(changes)
    return record


@pytest.mark.parametrize(
    "changes,state,current_boot,reclaimable",
    [
        ({"owner_token": "foreign"}, "dead", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", False),
        ({"heartbeat": 195.0}, "dead", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", False),
        ({}, "unknown", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", False),
        ({}, "alive-match", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", False),
        ({}, "dead", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", True),
        ({}, "alive-mismatch", "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa", True),
        ({}, "unknown", "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb", False),
    ],
)
def test_stale_reclaim_requires_owner_expiry_and_proven_death(
    changes, state, current_boot, reclaimable
):
    assert layout_module.lock_reclaimable(
        _record(**changes),
        owner_token="owner",
        now=200.0,
        current_boot_id=current_boot,
        process_state=state,
        stale_after=10.0,
    ) is reclaimable


def test_identical_concurrent_case_locks_serialize(tmp_path):
    with _layout(tmp_path) as layout:
        entered = []
        first_entered = threading.Event()
        release_first = threading.Event()

        def worker(number):
            with layout.lock("case-a", poll_interval=0.005):
                entered.append(number)
                if number == 1:
                    first_entered.set()
                    release_first.wait(1)

        first = threading.Thread(target=worker, args=(1,))
        second = threading.Thread(target=worker, args=(2,))
        first.start()
        assert first_entered.wait(1)
        second.start()
        time.sleep(0.03)
        assert entered == [1]
        release_first.set()
        first.join(1)
        second.join(1)
        assert entered == [1, 2]


def test_tombstone_collision_never_overwrites_foreign_target(tmp_path, monkeypatch):
    with _layout(tmp_path) as layout:
        lock_path = layout.run_path / ".locks" / "run.lock"
        lock_path.write_text(json.dumps(_record(owner_token=layout.owner_token, heartbeat=0)))
        tombstone = layout.run_path / ".locks" / "run.lock.reclaim-fixed"
        tombstone.write_text("foreign")
        monkeypatch.setattr(layout_module, "_lock_tombstone_name", lambda _name: tombstone.name)
        monkeypatch.setattr(layout_module, "_process_state", lambda _record: "dead")
        with pytest.raises(RuntimeError, match="tombstone"):
            with layout.lock("run", now=lambda: 1000.0, stale_after=1.0):
                pass
        assert tombstone.read_text() == "foreign"
        assert lock_path.exists()


def _checkpoint(layout, case="case-a", **changes):
    row = {"name": case, "status": "completed", "wall_seconds": 1.0}
    payload = {
        "schema_version": 2,
        "identity": IDENTITY,
        "owner_token": layout.owner_token,
        "case": case,
        "status": "completed",
        "result_row": row,
    }
    payload.update(changes)
    case_path = layout.case_path(case)
    case_path.mkdir(parents=True, exist_ok=True)
    (case_path / result_module.EXTERNAL_CHECKPOINT_FILENAME).write_text(json.dumps(payload))


def _load_locked(layout, case="case-a"):
    with layout.lock("run"), layout.lock(case):
        return result_module.load_external_checkpoint(layout, case, IDENTITY)


def test_exact_completed_checkpoint_v2_skips(tmp_path):
    with _layout(tmp_path) as layout:
        _checkpoint(layout)
        assert _load_locked(layout)["status"] == "completed"


@pytest.mark.parametrize(
    "changes",
    [
        {"schema_version": 1},
        {"owner_token": "foreign"},
        {"identity": {**IDENTITY, "identity_sha256": "d" * 64}},
        {"case": "other"},
        {"status": "failed"},
    ],
)
def test_foreign_legacy_or_incomplete_external_checkpoint_reruns(tmp_path, changes):
    with _layout(tmp_path) as layout:
        _checkpoint(layout, **changes)
        assert _load_locked(layout) is None


def test_corrupt_and_oversized_external_checkpoint_rerun(tmp_path):
    with _layout(tmp_path) as layout:
        case = layout.case_path("case-a")
        case.mkdir(parents=True)
        checkpoint = case / result_module.EXTERNAL_CHECKPOINT_FILENAME
        checkpoint.write_text("{")
        assert _load_locked(layout) is None
        _checkpoint(layout, result_row={"status": "completed", "data": "x" * 100})
        with layout.lock("run"), layout.lock("case-a"):
            assert result_module.load_external_checkpoint(
                layout, "case-a", IDENTITY, max_row_bytes=32
            ) is None


def test_checkpoint_requires_both_locks_and_current_owner_marker(tmp_path):
    with _layout(tmp_path) as layout:
        _checkpoint(layout)
        with pytest.raises(RuntimeError, match="locks"):
            result_module.load_external_checkpoint(layout, "case-a", IDENTITY)
        with layout.lock("run"), layout.lock("case-a"):
            marker = layout.run_path / layout_module.OWNER_FILENAME
            marker.write_text("{}")
            with pytest.raises(RuntimeError, match="owned run"):
                result_module.load_external_checkpoint(layout, "case-a", IDENTITY)


def test_legacy_checkpoint_behavior_is_unchanged(tmp_path):
    work = tmp_path / "legacy"
    work.mkdir()
    completed = {"status": "completed", "solver": "legacy"}
    (work / result_module.CHECKPOINT_FILENAME).write_text(json.dumps(completed))
    assert result_module.load_checkpoint(work) == completed
