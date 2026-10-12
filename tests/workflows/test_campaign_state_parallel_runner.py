"""Solver-free coverage of opt-in campaign stop records."""

import json

from digitalmodel.solvers.orcaflex.parallel_runner import (
    campaign_cases,
    write_stop_record,
)


def test_campaign_cases_maps_native_statuses_without_mutation():
    records = [
        {"case_id": "done", "status": "ok"},
        {"case_id": "bad", "status": "statics_diverged"},
        {"case_id": "next", "status": "not_run"},
    ]
    assert campaign_cases(records) == [
        {"id": "done", "status": "completed"},
        {"id": "bad", "status": "failed", "failure_class": "statics_diverged"},
        {"id": "next", "status": "unrun"},
    ]
    assert records[0]["status"] == "ok"


def test_shared_failure_record_is_json_serialisable(tmp_path):
    path = tmp_path / "stop.json"
    records = [
        {"case_id": "a", "status": "verify_failed"},
        {"case_id": "b", "status": "verify_failed"},
        {"case_id": "c", "status": "not_run"},
    ]
    write_stop_record(path, "more than 5 % of the batch failed", records)
    record = json.loads(path.read_text(encoding="utf-8"))
    assert set(record) == {
        "disposition", "reason", "failed_ids", "untouched_ids",
        "clears_when", "auto_resume_allowed",
    }
    assert record["disposition"] == "SHARED_FAILURE"
    assert record["failed_ids"] == ["a", "b"]
    assert record["untouched_ids"] == ["c"]
    assert "verify_failed" in record["clears_when"]
    assert record["auto_resume_allowed"] is False


def test_finished_batch_record(tmp_path):
    path = tmp_path / "stop.json"
    write_stop_record(path, None, [{"case_id": "a", "status": "ok"}])
    assert json.loads(path.read_text())["disposition"] == "COMPLETE"


def test_policy_record(tmp_path):
    path = tmp_path / "stop.json"
    write_stop_record(path, "solver version drift from the pin", [])
    assert json.loads(path.read_text())["disposition"] == "POLICY_BLOCKED"


def test_none_path_does_not_write_or_consume_records(tmp_path):
    def unread_records():
        raise AssertionError("disabled recording must not consume results")
        yield

    write_stop_record(None, None, unread_records())
    assert list(tmp_path.iterdir()) == []
