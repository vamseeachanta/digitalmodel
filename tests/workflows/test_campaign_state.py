"""Contract tests for solver-neutral campaign state and result evidence."""

from copy import deepcopy

import pytest

from digitalmodel.workflows.campaign_state import (
    CriteriaVersionMismatch,
    LaneState,
    StopDisposition,
    StopRecord,
    check_criteria,
    classify_lane,
    classify_stop,
    preview_criteria_change,
    resume_plan,
    status_line,
    validate_retention,
)


def test_stop_disposition_is_string_enum():
    assert {member.name for member in StopDisposition} == {
        "POLICY_BLOCKED", "NEEDS_DIAGNOSIS", "FAILED_CASES_ISOLATED",
        "SHARED_FAILURE", "COMPLETE",
    }
    assert all(isinstance(member, str) for member in StopDisposition)


def test_finished_campaign_is_complete_even_with_failed_cases():
    record = classify_stop(None, [
        {"id": "done", "status": "completed"},
        {"id": "failed", "status": "failed"},
    ])
    assert isinstance(record, StopRecord)
    assert record.disposition == StopDisposition.COMPLETE
    assert record.failed_ids == ["failed"]
    assert record.untouched_ids == []
    assert record.auto_resume_allowed is False


def test_empty_campaign_is_complete():
    assert classify_stop(None, []).disposition == StopDisposition.COMPLETE


@pytest.mark.parametrize("reason", [
    "Licence checkout failed", "Solver version drift detected",
    "Policy prohibits execution", "Approval required",
])
def test_policy_stops_block_resume(reason):
    record = classify_stop(reason, [{"id": "next", "status": "pending"}])
    assert record.disposition == StopDisposition.POLICY_BLOCKED
    assert record.reason == reason
    assert isinstance(record.clears_when, str) and record.clears_when.strip()
    assert record.auto_resume_allowed is False
    assert resume_plan(record, {"covers_untouched_cases": True}) == []


def test_failure_fraction_with_shared_class_never_resumes():
    record = classify_stop("more than 25 % of the batch failed", [
        {"id": "a", "status": "failed", "failure_class": "mesh"},
        {"id": "b", "status": "failed", "failure_class": "mesh"},
        {"id": "c", "status": "unrun"},
    ])
    assert record.disposition == StopDisposition.SHARED_FAILURE
    assert record.failed_ids == ["a", "b"]
    assert record.untouched_ids == ["c"]
    assert record.clears_when.strip()
    assert record.auto_resume_allowed is False
    assert resume_plan(record, {"covers_untouched_cases": True}) == []


def test_failure_fraction_with_different_classes_needs_diagnosis():
    record = classify_stop("more than 50 % of the batch failed", [
        {"id": "a", "status": "failed", "failure_class": "mesh"},
        {"id": "b", "status": "failed", "failure_class": "numerical"},
        {"id": "next", "status": "pending"},
    ])
    assert record.disposition == StopDisposition.NEEDS_DIAGNOSIS
    assert record.clears_when.strip()
    assert record.auto_resume_allowed is False
    assert resume_plan(record, {"covers_untouched_cases": True}) == []


def test_isolated_failures_record_only_pending_and_unrun_as_untouched():
    record = classify_stop(None, [
        {"id": "failed", "status": "failed"},
        {"id": "pending", "status": "pending"},
        {"id": "running", "status": "running"},
        {"id": "done", "status": "completed"},
        {"id": "unrun", "status": "unrun"},
    ])
    assert record.disposition == StopDisposition.FAILED_CASES_ISOLATED
    assert record.failed_ids == ["failed"]
    assert record.untouched_ids == ["pending", "unrun"]
    assert record.clears_when.strip()
    assert record.auto_resume_allowed is True


@pytest.mark.parametrize("authority", [
    {}, {"covers_untouched_cases": False}, {"covers_untouched_cases": None},
    {"covers_untouched_cases": 1}, {"covers_untouched_cases": "true"},
])
def test_resume_requires_explicit_boolean_authority(authority):
    record = classify_stop(None, [
        {"id": "failed", "status": "failed"},
        {"id": "next", "status": "pending"},
    ])
    assert resume_plan(record, authority) == []


def test_authorized_resume_never_includes_failed_or_running_ids():
    record = classify_stop(None, [
        {"id": "failed", "status": "failed"},
        {"id": "running", "status": "running"},
        {"id": "next", "status": "pending"},
        {"id": "later", "status": "unrun"},
    ])
    assert resume_plan(record, {"covers_untouched_cases": True}) == [
        "next", "later",
    ]


def test_lane_state_members():
    assert {member.name for member in LaneState} == {
        "PREPARED", "SOLVING", "STALLED", "COMPLETE", "FAILED",
    }


def test_lane_complete_requires_all_result_evidence():
    evidence = {"process_alive": False, "exit_code": 0,
                "native_readback_ok": True, "criterion_met": True}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.COMPLETE


@pytest.mark.parametrize("readback,criterion", [
    (None, True), (False, True), (True, None), (True, False), (None, None),
])
def test_exit_zero_without_readback_and_qualification_is_failed(readback, criterion):
    evidence = {"process_alive": False, "exit_code": 0,
                "native_readback_ok": readback, "criterion_met": criterion}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.FAILED


def test_nonzero_exit_is_failed_despite_positive_readback():
    evidence = {"process_alive": False, "exit_code": 2,
                "native_readback_ok": True, "criterion_met": True}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.FAILED


def test_live_lane_with_recent_counter_advance_is_solving():
    evidence = {"process_alive": True, "exit_code": None,
                "progress_counter": 0.0, "progress_counter_at": 95.0}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.SOLVING


def test_live_lane_with_frozen_counter_is_stalled():
    evidence = {"process_alive": True, "exit_code": None,
                "progress_counter": 120.0, "progress_counter_at": 89.0}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.STALLED


@pytest.mark.parametrize("counter,advanced_at", [(None, 99.0), (12.0, None)])
def test_live_lane_without_counter_evidence_is_stalled(counter, advanced_at):
    evidence = {"process_alive": True, "exit_code": None,
                "progress_counter": counter, "progress_counter_at": advanced_at}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.STALLED


def test_lane_without_process_or_exit_is_prepared():
    evidence = {"process_alive": False, "exit_code": None,
                "progress_counter": None, "progress_counter_at": None}
    assert classify_lane(evidence, 100.0, 10.0) == LaneState.PREPARED


def test_matching_criteria_and_empty_rows_are_valid():
    rows = [{"id": "a", "criteria_id": "acceptance", "criteria_version": "v1"}]
    assert check_criteria(rows, "acceptance", "v1") is None
    assert check_criteria([], "acceptance", "v1") is None


def test_criteria_mismatch_names_every_offending_row():
    rows = [
        {"id": "missing-id", "criteria_version": "v1"},
        {"id": "missing-version", "criteria_id": "acceptance"},
        {"id": "wrong-id", "criteria_id": "other", "criteria_version": "v1"},
        {"id": "wrong-version", "criteria_id": "acceptance", "criteria_version": "v2"},
    ]
    with pytest.raises(CriteriaVersionMismatch) as error:
        check_criteria(rows, "acceptance", "v1")
    for row in rows:
        assert row["id"] in str(error.value)


def test_criteria_preview_reports_changes_and_preserves_rows():
    rows = [
        {"id": "withheld", "old": True, "new": False, "details": [1]},
        {"id": "unknown", "old": True, "new": None, "details": [2]},
        {"id": "released", "old": False, "new": True, "details": [3]},
        {"id": "unchanged", "old": True, "new": True, "details": [4]},
    ]
    original = deepcopy(rows)
    assert preview_criteria_change(rows, lambda row: row["old"],
                                   lambda row: row["new"]) == {
        "changed": ["withheld", "unknown", "released"],
        "newly_withheld": ["withheld", "unknown"], "count": 3,
    }
    assert rows == original


def test_empty_criteria_preview():
    assert preview_criteria_change([], lambda row: True, lambda row: False) == {
        "changed": [], "newly_withheld": [], "count": 0,
    }


def test_status_line_exact_results_first_format():
    cases = [
        {"id": "a", "status": "completed", "qualified": True},
        {"id": "b", "status": "completed", "qualified": False},
        {"id": "c", "status": "running"},
        {"id": "d", "status": "failed"},
        {"id": "e", "status": "pending"},
        {"id": "f", "status": "unrun"},
    ]
    assert status_line(cases, {"a", "c", "d", "e"}) == (
        "native 4/6 attempted, 2 completed, 1 qualified; report 1/2 complete"
    )


def test_empty_status_line_exact_format():
    assert status_line([], set()) == (
        "native 0/0 attempted, 0 completed, 0 qualified; report 0/0 complete"
    )


def test_valid_retention_manifest():
    manifest = {"inputs": ["input"], "plotted_arrays": ["array"],
                "metrics": {"peak": 1}, "manifest_digest_algorithm": "sha256",
                "archive_replica": "archive/campaign"}
    assert validate_retention(manifest) == []


@pytest.mark.parametrize("empty", [False, True])
def test_retention_reports_one_problem_per_missing_or_empty_key(empty):
    keys = ["inputs", "plotted_arrays", "metrics",
            "manifest_digest_algorithm", "archive_replica"]
    manifest = dict.fromkeys(keys, "") if empty else {}
    problems = validate_retention(manifest)
    assert len(problems) == len(keys)
    for key in keys:
        assert sum(key in problem for problem in problems) == 1


@pytest.mark.parametrize("algorithm,replica,key", [
    ("md5", "archive", "manifest_digest_algorithm"),
    ("sha256", 123, "archive_replica"),
])
def test_retention_rejects_wrong_digest_or_nonstring_replica(algorithm, replica, key):
    manifest = {"inputs": ["input"], "plotted_arrays": ["array"],
                "metrics": {"peak": 1}, "manifest_digest_algorithm": algorithm,
                "archive_replica": replica}
    problems = validate_retention(manifest)
    assert len(problems) == 1
    assert key in problems[0]
