"""Runners record why a batch stopped and the conditions for resumption.
A lane is SOLVING only while recorded output advances.
COMPLETE requires native readback against the governing criterion.
Criteria rows carry an id and version; status lines lead with results.
References: digitalmodel#2298 and workspace-hub#3973.
"""

from dataclasses import dataclass
from enum import Enum
import re


class StopDisposition(str, Enum):
    POLICY_BLOCKED = "POLICY_BLOCKED"
    NEEDS_DIAGNOSIS = "NEEDS_DIAGNOSIS"
    FAILED_CASES_ISOLATED = "FAILED_CASES_ISOLATED"
    SHARED_FAILURE = "SHARED_FAILURE"
    COMPLETE = "COMPLETE"


class LaneState(str, Enum):
    PREPARED = "PREPARED"
    SOLVING = "SOLVING"
    STALLED = "STALLED"
    COMPLETE = "COMPLETE"
    FAILED = "FAILED"


@dataclass(frozen=True)
class StopRecord:
    disposition: StopDisposition
    reason: str | None
    failed_ids: list[str]
    untouched_ids: list[str]
    clears_when: str
    auto_resume_allowed: bool


def classify_stop(reason, cases) -> StopRecord:
    cases = list(cases)
    failed = [case for case in cases if case.get("status") == "failed"]
    untouched = [case["id"] for case in cases
                 if case.get("status") in ("pending", "unrun")]
    text = (reason or "").strip().lower()
    classes = {case.get("failure_class") for case in failed}
    if text and any(word in text for word in
                    ("licence", "license", "version drift", "polic", "approv")):
        disposition = StopDisposition.POLICY_BLOCKED
        clears = "Resolve the licence, version, policy or approval block before execution resumes."
    elif re.search(r"more than .*% of the batch failed", text) and failed and len(classes) == 1 and all(classes):
        disposition = StopDisposition.SHARED_FAILURE
        cls = next(iter(classes))
        clears = f"Diagnose the shared failure class '{cls}' and fix it before any further case is launched."
    elif text:
        disposition = StopDisposition.NEEDS_DIAGNOSIS
        clears = "Diagnose the recorded stop reason and correct its cause before any further case is launched."
    elif untouched:
        disposition = StopDisposition.FAILED_CASES_ISOLATED
        clears = "Isolate failed cases and verify authority covers untouched cases before launching them."
    else:
        disposition = StopDisposition.COMPLETE
        clears = "No cases remain untouched; retain the native results and failure records for qualification."
    return StopRecord(disposition, reason, [case["id"] for case in failed],
                      untouched, clears,
                      disposition == StopDisposition.FAILED_CASES_ISOLATED)


def resume_plan(record: StopRecord, authority) -> list[str]:
    if (record.disposition == StopDisposition.FAILED_CASES_ISOLATED
            and record.auto_resume_allowed
            and authority.get("covers_untouched_cases") is True):
        return list(record.untouched_ids)
    return []


def classify_lane(evidence, now, stall_after) -> LaneState:
    if evidence.get("process_alive"):
        advanced_at = evidence.get("progress_counter_at")
        if (evidence.get("progress_counter") is not None
                and advanced_at is not None
                and 0 <= now - advanced_at <= stall_after):
            return LaneState.SOLVING
        return LaneState.STALLED
    exit_code = evidence.get("exit_code")
    if exit_code is None:
        return LaneState.PREPARED
    if (exit_code == 0 and evidence.get("native_readback_ok") is True
            and evidence.get("criterion_met") is True):
        return LaneState.COMPLETE
    return LaneState.FAILED


class CriteriaVersionMismatch(ValueError):
    """Result rows do not match the governing criteria identity and version."""


def check_criteria(rows, criteria_id, criteria_version) -> None:
    offending = [str(row["id"]) for row in rows
                 if row.get("criteria_id") != criteria_id
                 or row.get("criteria_version") != criteria_version]
    if offending:
        raise CriteriaVersionMismatch("Criteria mismatch for rows: " + ", ".join(offending))


def preview_criteria_change(rows, old_criterion, new_criterion) -> dict:
    changed, withheld = [], []
    for row in rows:
        old, new = old_criterion(row), new_criterion(row)
        if old != new:
            changed.append(row["id"])
            if old is True and new is not True:
                withheld.append(row["id"])
    return {"changed": changed, "newly_withheld": withheld, "count": len(changed)}


def status_line(cases, report_ids) -> str:
    cases = list(cases)
    attempted = sum(case.get("status") not in ("pending", "unrun") for case in cases)
    completed = [case for case in cases if case.get("status") == "completed"]
    qualified = sum(case.get("qualified") is True for case in completed)
    reported = sum(case["id"] in report_ids for case in completed)
    return (f"native {attempted}/{len(cases)} attempted, {len(completed)} completed, "
            f"{qualified} qualified; report {reported}/{len(completed)} complete")


def validate_retention(manifest) -> list[str]:
    problems = []
    for key in ("inputs", "plotted_arrays", "metrics"):
        if not manifest.get(key):
            problems.append(f"{key} must contain retained evidence")
    if manifest.get("manifest_digest_algorithm") != "sha256":
        problems.append("manifest_digest_algorithm must be sha256")
    replica = manifest.get("archive_replica")
    if not isinstance(replica, str) or not replica.strip():
        problems.append("archive_replica must identify an archive location")
    return problems
