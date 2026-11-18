"""Expiry, corruption, invalidation and workflow-level entries in WorkflowCache."""

import os
import time
from datetime import datetime

import pytest

from digitalmodel.workflows.workflow_automation.cache import (
    CacheManager,
    WorkflowCache,
)
from digitalmodel.workflows.workflow_automation.models import (
    TaskStatus,
    WorkflowDefinition,
    WorkflowResult,
    WorkflowTask,
)

TASK = WorkflowTask(name="T", task_id="t", module="m", function="f")
INPUTS = {"x": 1}


def age(path, hours):
    old = time.time() - hours * 3600
    os.utime(path, (old, old))


def only_file(directory, pattern):
    (path,) = directory.glob(pattern)
    return path


def workflow(x=1):
    wf = WorkflowDefinition(name="WF", description="d")
    wf.add_task(WorkflowTask(name="A", task_id="a", module="m", function="f",
                             inputs={"x": x}))
    return wf


def result():
    start = datetime(2026, 1, 5, 9, 0, 0)
    return WorkflowResult(
        workflow_id="wf1", workflow_name="WF", status="completed",
        start_time=start, end_time=start,
        task_statuses={"a": TaskStatus.COMPLETED}, outputs={"y": 2.5},
    )


def test_task_entry_survives_restart_until_it_expires(tmp_path):
    WorkflowCache(cache_dir=str(tmp_path), ttl_hours=1).set_task_result(
        TASK, INPUTS, {"v": 3}
    )
    entry = only_file(tmp_path / "tasks", "*.pkl")

    assert WorkflowCache(cache_dir=str(tmp_path), ttl_hours=1).get_task_result(
        TASK, INPUTS
    ) == {"v": 3}

    age(entry, hours=2)
    assert WorkflowCache(cache_dir=str(tmp_path), ttl_hours=1).get_task_result(
        TASK, INPUTS
    ) is None
    assert not entry.exists()


def test_corrupt_task_entry_is_discarded(tmp_path):
    WorkflowCache(cache_dir=str(tmp_path)).set_task_result(TASK, INPUTS, {"v": 3})
    entry = only_file(tmp_path / "tasks", "*.pkl")
    entry.write_bytes(b"not a pickle")

    assert WorkflowCache(cache_dir=str(tmp_path)).get_task_result(TASK, INPUTS) is None
    assert not entry.exists()


def test_unpicklable_result_is_kept_in_memory_only(tmp_path):
    cache = WorkflowCache(cache_dir=str(tmp_path))
    value = {"f": lambda: None}

    cache.set_task_result(TASK, INPUTS, value)

    assert cache.get_task_result(TASK, INPUTS) is value
    # A new instance cannot load the failed disk write and removes it.
    assert WorkflowCache(cache_dir=str(tmp_path)).get_task_result(TASK, INPUTS) is None
    assert list((tmp_path / "tasks").glob("*.pkl")) == []


def test_invalidate_task_removes_memory_and_disk_entries(tmp_path):
    cache = WorkflowCache(cache_dir=str(tmp_path))
    cache.set_task_result(TASK, INPUTS, 7)

    cache.invalidate_task(TASK, INPUTS)
    cache.invalidate_task(TASK, INPUTS)  # absent entry: no error

    assert cache.get_task_result(TASK, INPUTS) is None
    assert list((tmp_path / "tasks").glob("*.pkl")) == []


def test_workflow_summary_round_trip_and_key(tmp_path):
    cache = WorkflowCache(cache_dir=str(tmp_path))

    cache.set_workflow_result(workflow(), result())

    # The stored summary dictionary is returned, not a WorkflowResult.
    assert cache.get_workflow_result(workflow()) == {
        "workflow_id": "wf1",
        "workflow_name": "WF",
        "status": "completed",
        "duration_seconds": 0.0,
        "success_rate": 100.0,
        "task_count": 1,
        "failed_tasks": [],
        "outputs": {"y": 2.5},
    }
    # Changing a task input changes the key.
    assert cache.get_workflow_result(workflow(x=2)) is None


def test_workflow_cache_respects_cache_enabled(tmp_path):
    cache = WorkflowCache(cache_dir=str(tmp_path))
    wf = workflow()
    wf.cache_enabled = False

    cache.set_workflow_result(wf, result())

    assert list((tmp_path / "workflows").iterdir()) == []
    assert cache.get_workflow_result(wf) is None


@pytest.mark.parametrize("damage", ["expire", "corrupt"])
def test_stale_or_corrupt_workflow_entry_is_discarded(tmp_path, damage):
    cache = WorkflowCache(cache_dir=str(tmp_path), ttl_hours=1)
    cache.set_workflow_result(workflow(), result())
    entry = only_file(tmp_path / "workflows", "*.json")
    if damage == "expire":
        age(entry, hours=2)
    else:
        entry.write_text("{not json")

    assert cache.get_workflow_result(workflow()) is None
    assert not entry.exists()


def test_invalidate_workflow_and_clear(tmp_path):
    cache = WorkflowCache(cache_dir=str(tmp_path))
    cache.set_workflow_result(workflow(), result())
    cache.invalidate_workflow(workflow())
    cache.invalidate_workflow(workflow())  # absent entry: no error
    assert cache.get_workflow_result(workflow()) is None

    cache.set_workflow_result(workflow(), result())
    cache.set_task_result(TASK, INPUTS, 1)
    cache.clear()

    assert cache.get_cache_stats()["workflow_cache_entries"] == 0
    assert cache.get_cache_stats()["task_cache_entries"] == 0
    assert cache.get_cache_stats()["memory_cache_entries"] == 0


def test_cache_manager_shares_and_reconfigures_one_cache(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(CacheManager, "_instance", None)

    first = CacheManager.get_cache()
    assert CacheManager.get_cache() is first
    assert first.cache_dir.resolve() == (tmp_path / ".workflow_cache").resolve()

    CacheManager.configure(cache_dir=str(tmp_path / "shared"), ttl_hours=2)
    configured = CacheManager.get_cache()

    assert configured is not first
    assert configured.cache_dir == tmp_path / "shared"
    assert configured.ttl_hours == 2
