# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Issue 2298 (2026), contract items 1–5; synthetic software evidence only.

All counters, clock readings and limits are test inputs. Numerical expectations
are independent hand calculations in the individual test docstrings.
"""

import csv
from copy import deepcopy
import io
import json

import pytest

from digitalmodel.workflows.campaign_contract import Campaign
from digitalmodel.workflows.campaign_state import CriteriaVersionMismatch, LaneState


CRITERIA = dict(criteria_id="screen", criteria_version="v1")
RETENTION = dict(inputs=["input.json"], plotted_arrays=["arrays.json"],
                 metrics=["metrics.json"], manifest_digest_algorithm="sha256",
                 archive_replica="archive/campaign")


def launch(ids=("a", "b", "c"), **overrides):
    """The age limit of 10 is an arbitrary required input, not a default."""
    kwargs = dict(case_ids=ids, **CRITERIA, criteria_source="Synthetic basis",
                  retention=deepcopy(RETENTION), stall_after=10,
                  authority={"covers_untouched_cases": True})
    kwargs.update(overrides)
    return Campaign.launch(**kwargs)


def collect(campaign, case_id="a", **overrides):
    kwargs = dict(**CRITERIA, exit_code=0, native_readback_ok=True,
                  criterion_met=True)
    kwargs.update(overrides)
    campaign.collect(case_id, **kwargs)


def test_launch_round_trip_preserves_authority_retention_and_stop(tmp_path):
    """Three pending cases yield zero attempted and three eligible to resume."""
    retention = deepcopy(RETENTION)
    campaign = launch(retention=retention)
    retention["inputs"].append("unrelated")
    campaign.stop()
    path = tmp_path / "campaign.json"
    campaign.save(path)
    restored = Campaign.load(path)
    assert restored.document == campaign.document
    assert restored.document["retention"] == RETENTION
    assert restored.next_cases() == ["a", "b", "c"]
    assert restored.status(**CRITERIA, next_action="Run untouched cases") == (
        "native 0/3 attempted, 0 completed, 0 qualified; report 0/0 complete; "
        "next: Run untouched cases")
    detached = restored.document
    detached["cases"][0]["status"] = "completed"
    assert restored.document != detached


@pytest.mark.parametrize("manifest", [{}, dict(RETENTION, archive_replica="")])
def test_launch_requires_retention_before_any_case(manifest):
    with pytest.raises(ValueError, match="retention"):
        launch(retention=manifest)


@pytest.mark.parametrize("ids", [[], ["a", "a"], [""], [True]])
def test_case_identity_is_unambiguous(ids):
    with pytest.raises(ValueError, match="case"):
        launch(ids)


def test_progress_and_native_results_survive_restart(tmp_path):
    """4→5 at t=20: age at t=30 is 10; t=31 is 11, above input 10."""
    campaign = launch()
    assert campaign.observe("a", counter=4, now=10, process_alive=True) == LaneState.STALLED
    assert campaign.observe("a", counter=5, now=20, process_alive=True) == LaneState.SOLVING
    path = tmp_path / "campaign.json"
    campaign.save(path)
    restored = Campaign.load(path)
    restored.observe("a", counter=5, now=30, process_alive=True)
    assert restored.health(now=30)["a"] == LaneState.SOLVING
    assert restored.health(now=31)["a"] == LaneState.STALLED
    collect(restored)
    assert restored.health(now=31)["a"] == LaneState.COMPLETE


@pytest.mark.parametrize("readback,met,status,qualified", [
    (False, True, "failed", False),
    (True, False, "completed", False),
    (True, None, "completed", None),
    (True, True, "completed", True),
])
def test_exit_zero_is_not_qualification(readback, met, status, qualified):
    """Native completion and qualification are separate per contract items 2,4."""
    campaign = launch()
    collect(campaign, native_readback_ok=readback, criterion_met=met)
    row = campaign.result_rows(**CRITERIA)[0]
    assert row["status"] == status
    assert row["qualified"] is qualified


def test_isolated_failure_resumes_only_untouched_authorized_cases(tmp_path):
    """a failed, b completed, c untouched: only c may resume after restart."""
    campaign = launch()
    collect(campaign, "a", exit_code=1, native_readback_ok=False, criterion_met=None)
    collect(campaign, "b")
    record = campaign.stop()
    assert record.disposition == "FAILED_CASES_ISOLATED"
    assert record.failed_ids == ["a"]
    assert record.clears_when
    path = tmp_path / "campaign.json"
    campaign.save(path)
    assert Campaign.load(path).next_cases() == ["c"]
    denied = launch(authority={"covers_untouched_cases": False})
    denied.stop()
    assert denied.next_cases() == []


@pytest.mark.parametrize("reason,disposition", [
    ("Policy prohibits execution", "POLICY_BLOCKED"),
    ("Output requires diagnosis", "NEEDS_DIAGNOSIS"),
])
def test_recorded_blocker_is_not_cleared_by_resume(reason, disposition):
    campaign = launch()
    assert campaign.stop(reason).disposition == disposition
    assert campaign.next_cases() == []
    assert campaign.document["stop_record"]["reason"] == reason


def test_running_case_prevents_automatic_resume():
    campaign = launch()
    campaign.observe("a", counter=4, now=10, process_alive=True)
    assert campaign.stop().disposition == "NEEDS_DIAGNOSIS"
    assert campaign.next_cases() == []


def test_all_attempted_records_complete_with_failures():
    campaign = launch(["a"])
    collect(campaign, exit_code=1)
    assert campaign.stop().disposition == "COMPLETE"
    assert campaign.next_cases() == []


@pytest.mark.parametrize("consumer", ["result_rows", "status", "workbook_csv", "report"])
def test_every_consumer_rejects_stale_criteria(consumer):
    campaign = launch()
    kwargs = dict(CRITERIA, criteria_version="v0")
    if consumer in ("status", "report"):
        kwargs["next_action"] = "Run pending cases"
    with pytest.raises(CriteriaVersionMismatch):
        getattr(campaign, consumer)(**kwargs)


def test_collector_rejects_stale_result_without_modification():
    campaign = launch()
    before = campaign.document
    with pytest.raises(CriteriaVersionMismatch):
        collect(campaign, criteria_version="v0")
    assert campaign.document == before


def test_criteria_preview_precedes_migration_and_invalidates_report():
    """a,b native complete; old pass set {a,b}, new {b}: one newly withheld.

    Every row (including pending c) changes version. Reporting becomes 0/2
    until regenerated; qualified becomes 1/2, independent of production code.
    """
    campaign = launch()
    collect(campaign, "a")
    collect(campaign, "b")
    campaign.mark_reported(["a", "b"], **CRITERIA)
    before = campaign.document
    preview = campaign.preview_criteria_change(
        criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
        qualifications={"a": False, "b": True})
    assert campaign.document == before
    assert preview["affected_ids"] == ["a", "b", "c"]
    assert preview["qualification_changed_ids"] == ["a"]
    assert preview["newly_withheld"] == ["a"]
    announced = []

    def emit(message):
        assert campaign.document == before
        announced.append(json.loads(message))

    campaign.apply_criteria_change(preview, emit=emit)
    assert announced == [preview]
    governing = dict(criteria_id="screen", criteria_version="v2")
    assert all(row["criteria_version"] == "v2" for row in campaign.result_rows(**governing))
    assert campaign.status(**governing, next_action="Regenerate report") == (
        "native 2/3 attempted, 2 completed, 1 qualified; report 0/2 complete; "
        "next: Regenerate report")
    assert campaign.health(now=30)["a"] == LaneState.FAILED
    with pytest.raises(CriteriaVersionMismatch):
        campaign.report(**CRITERIA, next_action="Review")


def test_migration_requires_current_preview_and_successful_announcement():
    campaign = launch()
    preview = campaign.preview_criteria_change(
        criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
        qualifications={})

    def failed_emit(message):
        raise OSError("announcement unavailable")

    before = campaign.document
    with pytest.raises(OSError, match="announcement"):
        campaign.apply_criteria_change(preview, emit=failed_emit)
    assert campaign.document == before
    collect(campaign)
    with pytest.raises(ValueError, match="stale"):
        campaign.apply_criteria_change(preview, emit=print)


@pytest.mark.parametrize("qualifications", [{}, {"a": "pass"}, {"a": True, "b": False}])
def test_migration_requires_explicit_decisions_for_exactly_completed_rows(qualifications):
    campaign = launch()
    collect(campaign)
    with pytest.raises(ValueError, match="qualification"):
        campaign.preview_criteria_change(
            criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
            qualifications=qualifications)


def test_consumer_views_share_all_rows_and_version_metadata():
    """a done+qualified+reported, b failed, c pending: counts 2,1,1,1."""
    campaign = launch()
    collect(campaign)
    collect(campaign, "b", exit_code=1)
    campaign.mark_reported(["a"], **CRITERIA)
    rows = campaign.result_rows(**CRITERIA)
    workbook = list(csv.DictReader(io.StringIO(campaign.workbook_csv(**CRITERIA))))
    report = campaign.report(**CRITERIA, next_action="Run c")
    assert [row["id"] for row in workbook] == ["a", "b", "c"]
    assert all(row["criteria_version"] == "v1" for row in workbook)
    assert report["rows"] == rows
    assert report["status"] == campaign.status(**CRITERIA, next_action="Run c")
    assert report["retention"] == RETENTION
    assert "native 2/3 attempted, 1 completed, 1 qualified; report 1/1 complete" in report["status"]


def test_load_rejects_mismatched_rows_and_unknown_schema(tmp_path):
    path = tmp_path / "campaign.json"
    campaign = launch()
    document = campaign.document
    document["cases"][0]["criteria_version"] = "v0"
    path.write_text(json.dumps(document))
    with pytest.raises(CriteriaVersionMismatch, match="a"):
        Campaign.load(path)
    document = campaign.document
    document["schema_version"] = "unknown"
    path.write_text(json.dumps(document))
    with pytest.raises(ValueError, match="schema"):
        Campaign.load(path)


@pytest.mark.parametrize("field,bad", [
    ("criteria_source", ""), ("stall_after", -1),
    ("authority", {"covers_untouched_cases": "yes"}),
])
def test_launch_rejects_invalid_contract_inputs(field, bad):
    with pytest.raises(ValueError):
        launch(**{field: bad})


@pytest.mark.parametrize("field,bad", [
    ("exit_code", True), ("native_readback_ok", "yes"), ("criterion_met", "pass"),
])
def test_collection_requires_typed_evidence(field, bad):
    campaign = launch()
    before = campaign.document
    with pytest.raises(ValueError):
        collect(campaign, **{field: bad})
    assert campaign.document == before


def test_saved_resume_record_cannot_launch_while_a_case_is_active():
    """An old pending-case plan is invalid while a previously pending case runs."""
    campaign = launch()
    campaign.stop()
    campaign.observe("a", counter=4, now=10, process_alive=True)
    assert campaign.next_cases() == []


@pytest.mark.parametrize("alter", [
    lambda row: row.update(qualified=True),
    lambda row: row["evidence"].update(exit_code=True),
    lambda row: row["evidence"].update(native_readback_ok=False),
    lambda row: row.update(criteria_source="Different source"),
])
def test_checkpoint_rejects_inconsistent_result_evidence(alter):
    campaign = launch()
    collect(campaign, criterion_met=False)
    document = campaign.document
    alter(document["cases"][0])
    with pytest.raises(ValueError):
        Campaign(document)


def test_unknown_acceptance_is_visible_in_workbook_and_report():
    campaign = launch()
    collect(campaign, criterion_met=None)
    workbook = list(csv.DictReader(io.StringIO(campaign.workbook_csv(**CRITERIA))))
    assert workbook[0]["qualification"] == "NOT_EVALUATED"
    assert campaign.report(**CRITERIA, next_action="Supply acceptance basis")["rows"][0][
        "qualification"] == "NOT_EVALUATED"


def test_modified_preview_is_rejected_before_announcement():
    campaign = launch()
    preview = campaign.preview_criteria_change(
        criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
        qualifications={})
    preview["affected_ids"] = []
    with pytest.raises(ValueError, match="modified"):
        campaign.apply_criteria_change(preview, emit=lambda message: pytest.fail(message))


def test_criteria_changes_retain_migration_history_after_restart(tmp_path):
    campaign = launch()
    preview = campaign.preview_criteria_change(
        criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
        qualifications={})
    campaign.apply_criteria_change(preview, emit=lambda message: None)
    path = tmp_path / "campaign.json"
    campaign.save(path)
    assert Campaign.load(path).document["criteria_changes"] == [preview]


def test_mutating_announcement_cannot_apply_a_stale_preview():
    campaign = launch()
    preview = campaign.preview_criteria_change(
        criteria_id="screen", criteria_version="v2", criteria_source="Revised basis",
        qualifications={})

    def mutate(message):
        collect(campaign)

    with pytest.raises(ValueError, match="stale"):
        campaign.apply_criteria_change(preview, emit=mutate)
    assert campaign.document["criteria"] == dict(CRITERIA, criteria_source="Synthetic basis")


def test_runner_continues_after_isolated_failure_and_never_replays(tmp_path):
    """a fails; b,c succeed. All three run once; repeat runs launch nothing."""
    campaign = launch()
    path = tmp_path / "campaign.json"
    calls = []

    def execute(case_id, criteria):
        calls.append(case_id)
        # The pre-execution checkpoint prevents replay after an abrupt exit.
        assert Campaign.load(path).document["cases"][len(calls) - 1]["status"] == "running"
        return dict(criteria_id=criteria["criteria_id"],
                    criteria_version=criteria["criteria_version"],
                    exit_code=1 if case_id == "a" else 0,
                    native_readback_ok=case_id != "a", criterion_met=case_id != "a")

    assert campaign.run_untouched(execute, checkpoint=path).disposition == "COMPLETE"
    assert calls == ["a", "b", "c"]
    restored = Campaign.load(path)
    restored.run_untouched(execute, checkpoint=path)
    assert calls == ["a", "b", "c"]
    assert restored.status(**CRITERIA, next_action="Review failed a").startswith(
        "native 3/3 attempted, 2 completed, 2 qualified")


@pytest.mark.parametrize("blocked", ["Policy prohibits execution", "Output needs diagnosis"])
def test_runner_preserves_blockers_before_launch(tmp_path, blocked):
    campaign = launch()
    campaign.stop(blocked)
    campaign.run_untouched(lambda *args: pytest.fail("blocked execution"),
                           checkpoint=tmp_path / "campaign.json")
    assert campaign.document["stop_record"]["reason"] == blocked


def test_runner_requires_authority_for_untouched_cases(tmp_path):
    campaign = launch(authority={"covers_untouched_cases": False})
    campaign.run_untouched(lambda *args: pytest.fail("unauthorized execution"),
                           checkpoint=tmp_path / "campaign.json")


def test_runner_records_adapter_failure_and_blocks_replay(tmp_path):
    campaign = launch()
    path = tmp_path / "campaign.json"

    def broken(case_id, criteria):
        raise RuntimeError("adapter unavailable")

    with pytest.raises(RuntimeError, match="adapter unavailable"):
        campaign.run_untouched(broken, checkpoint=path)
    restored = Campaign.load(path)
    assert restored.document["stop_record"]["disposition"] == "NEEDS_DIAGNOSIS"
    assert restored.next_cases() == []


def test_runner_honors_explicit_adapter_stop_after_result(tmp_path):
    campaign = launch()
    calls = []

    def execute(case_id, criteria):
        calls.append(case_id)
        return dict(**CRITERIA, exit_code=1, native_readback_ok=False,
                    criterion_met=None, stop_reason="Licence unavailable")

    record = campaign.run_untouched(execute, checkpoint=tmp_path / "campaign.json")
    assert calls == ["a"]
    assert record.disposition == "POLICY_BLOCKED"
    assert record.untouched_ids == ["b", "c"]


@pytest.mark.parametrize("field", ["criteria_changes", "report_ids", "stop_record"])
def test_incomplete_checkpoint_is_rejected_before_reuse(field):
    document = launch().document
    del document[field]
    with pytest.raises(ValueError, match="checkpoint"):
        Campaign(document)


def test_malformed_migration_history_is_rejected_before_mutation():
    document = launch().document
    document["criteria_changes"] = {}
    with pytest.raises(ValueError, match="criteria_changes"):
        Campaign(document)
