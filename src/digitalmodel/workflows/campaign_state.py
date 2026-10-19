"""Runners record why a batch stopped and the conditions for resumption.
A lane is SOLVING only while recorded output advances.
COMPLETE requires native readback against the governing criterion.
Criteria rows carry an id and version; status lines lead with results.
References: digitalmodel#2298 and workspace-hub#3973.
"""

from dataclasses import dataclass
from enum import Enum
import math
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


def _finite_nonnegative(value) -> bool:
    # Zero is the counter/elapsed-time origin, not an acceptance threshold.
    return (type(value) in (int, float) and math.isfinite(value) and value >= 0)


def observe_progress(evidence, *, counter, now) -> dict:
    """Return a new sample without treating polling as output advancement.

    Issue 2298, contract item 2: only a strict counter increase refreshes age.
    The first sample or a counter reset requires another observed increase.
    Counters and timestamps must use consistent units and one clock domain.
    """
    for name, value in (("counter", counter), ("now", now)):
        if not _finite_nonnegative(value):
            raise ValueError(f"{name} must be a finite nonnegative number")
    previous = evidence.get("progress_counter")
    advanced_at = evidence.get("progress_counter_at")
    if _finite_nonnegative(advanced_at) and advanced_at > now:
        raise ValueError("Observation time precedes recorded progress time")
    if not _finite_nonnegative(previous) or counter < previous:
        advanced_at = None
    elif counter > previous:
        advanced_at = now
    elif not _finite_nonnegative(advanced_at):
        advanced_at = None
    return dict(evidence, progress_counter=counter, progress_counter_at=advanced_at)


def classify_lane(evidence, now, stall_after) -> LaneState:
    """Classify evidence using an explicit caller-supplied output-age limit.

    Issue 2298, contract item 2: age = now - last advance; age <= stall_after.
    No engineering acceptance limit is supplied by this module.
    """
    for name, value in (("now", now), ("stall_after", stall_after)):
        if not _finite_nonnegative(value):
            raise ValueError(f"{name} must be a finite nonnegative number")
    if evidence.get("process_alive"):
        advanced_at = evidence.get("progress_counter_at")
        if (_finite_nonnegative(evidence.get("progress_counter"))
                and _finite_nonnegative(advanced_at)
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
    """Reject unknown governing identities before comparing rows (issue 2298 §3)."""
    for name, value in (("criteria_id", criteria_id),
                        ("criteria_version", criteria_version)):
        if not isinstance(value, str) or not value.strip():
            raise ValueError(f"{name} must be a nonempty string")
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


def campaign_status(cases, report_ids, *, criteria_id, criteria_version,
                    next_action) -> str:
    """Format results only after checking every row's criteria (issue 2298 §3–4).

    Collectors, monitors and reports can share this opt-in boundary. Legacy
    status_line remains available for callers without versioned result rows.
    """
    if not isinstance(next_action, str) or not next_action.strip():
        raise ValueError("next_action must be a nonempty string")
    cases = list(cases)
    check_criteria(cases, criteria_id, criteria_version)
    return f"{status_line(cases, report_ids)}; next: {next_action}"


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
