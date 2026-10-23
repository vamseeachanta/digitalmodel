# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Persisted, solver-neutral consumers of the issue 2298 campaign contract.

Source: issue 2298 (2026), proposed contract items 1–5. No physical equation or
engineering threshold is implemented. Callers supply the output-age limit,
native readback evidence, acceptance decisions and their versioned source.
This module never starts a process or reads a solver's native files itself.
"""

from copy import deepcopy
import csv
from dataclasses import asdict
import hashlib
import io
import json
from pathlib import Path
import tempfile

from digitalmodel.workflows.campaign_state import (
    CriteriaVersionMismatch,
    StopDisposition,
    StopRecord,
    campaign_status,
    check_criteria,
    classify_lane,
    classify_stop,
    observe_progress,
    resume_plan,
    validate_retention,
)


def _text(value, name):
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a nonempty string")


def _criteria(criteria_id, criteria_version, criteria_source):
    check_criteria([], criteria_id, criteria_version)
    _text(criteria_source, "criteria_source")
    return dict(criteria_id=criteria_id, criteria_version=criteria_version,
                criteria_source=criteria_source)


def _case_ids(ids):
    ids = list(ids)
    if not ids:
        raise ValueError("case_ids must not be empty")
    for case_id in ids:
        _text(case_id, "case id")
    if len(set(ids)) != len(ids):
        raise ValueError("case ids must be unique")
    return ids


def _digest(document):
    # SHA-256 is the retention identity specified in issue 2298 item 5 and
    # campaign_state.validate_retention; not an engineering coefficient.
    return hashlib.sha256(json.dumps(document, sort_keys=True,
                                     allow_nan=False).encode()).hexdigest()


def _terminal_evidence(exit_code, native_readback_ok, criterion_met):
    if type(exit_code) is not int or type(native_readback_ok) is not bool:
        raise ValueError("exit_code and native_readback_ok require typed evidence")
    if criterion_met is not None and type(criterion_met) is not bool:
        raise ValueError("criterion_met must be boolean or None")
    # Exit success is zero by the process API, not an engineering limit (§2).
    return exit_code == 0 and native_readback_ok


class Campaign:
    """One persisted campaign with shared collector, monitor and export views.

    Use ``launch`` before execution and ``save`` after each observation, result
    or stop. The caller owns serialization of writes; concurrent writers are
    not supported. ``document`` and exported views are detached copies.
    """

    def __init__(self, document):
        self._document = deepcopy(document)
        self._validate()

    @classmethod
    def launch(cls, case_ids, *, criteria_id, criteria_version, criteria_source,
               retention, stall_after, authority):
        """Validate retention and criteria before creating pending cases."""
        criteria = _criteria(criteria_id, criteria_version, criteria_source)
        # "v1" names this module's JSON layout, not an acceptance criterion.
        return cls(dict(schema_version="v1", criteria=criteria,
                        retention=retention, stall_after=stall_after,
                        authority=authority, report_ids=[], stop_record=None,
                        criteria_changes=[],
                        cases=[dict(id=case_id, status="pending", qualified=None,
                                    evidence={}, **criteria)
                               for case_id in _case_ids(case_ids)]))

    @property
    def document(self):
        """Return a detached JSON-compatible checkpoint."""
        return deepcopy(self._document)

    def _validate(self):
        document = self._document
        required = {"schema_version", "criteria", "retention", "stall_after",
                    "authority", "cases", "report_ids", "stop_record", "criteria_changes"}
        if not isinstance(document, dict) or not required <= document.keys():
            raise ValueError("Incomplete campaign checkpoint")
        if document.get("schema_version") != "v1":
            raise ValueError("Unsupported campaign schema_version")
        if not isinstance(document["criteria_changes"], list):
            raise ValueError("criteria_changes must be a list")
        if not isinstance(document["report_ids"], list):
            raise ValueError("report_ids must be a list")
        if document["stop_record"] is not None:
            saved = document["stop_record"]
            StopRecord(**dict(saved, disposition=StopDisposition(saved["disposition"])))
        criteria = document["criteria"]
        _criteria(**criteria)
        problems = validate_retention(document["retention"])
        if problems:
            raise ValueError("Invalid retention: " + "; ".join(problems))
        classify_lane({}, now=0, stall_after=document["stall_after"])
        # Zero here is the clock origin used for input validation (§2).
        if type(document["authority"].get("covers_untouched_cases")) is not bool:
            raise ValueError("authority.covers_untouched_cases must be boolean")
        rows = document["cases"]
        _case_ids(row["id"] for row in rows)
        check_criteria(rows, criteria["criteria_id"], criteria["criteria_version"])
        for row in rows:
            if not isinstance(row["evidence"], dict):
                raise ValueError("evidence must be an object")
            if row["criteria_source"] != criteria["criteria_source"]:
                raise ValueError("Row criteria_source differs from governing source")
            if row["status"] not in {"pending", "running", "completed", "failed"}:
                raise ValueError("Unknown case status")
            if row["qualified"] is not None and type(row["qualified"]) is not bool:
                raise ValueError("qualified must be boolean or None")
            if row["status"] != "completed" and row["qualified"] is True:
                raise ValueError("Only native completed cases may qualify")
            if row["status"] in {"completed", "failed"}:
                evidence = row["evidence"]
                completed = _terminal_evidence(evidence["exit_code"],
                                               evidence["native_readback_ok"],
                                               evidence["criterion_met"])
                expected = evidence["criterion_met"] if completed else False
                if (completed != (row["status"] == "completed")
                        or row["qualified"] is not expected
                        or evidence.get("process_alive") is not False):
                    raise ValueError("Result contradicts native terminal evidence")
        completed = {row["id"] for row in rows if row["status"] == "completed"}
        if not set(document["report_ids"]) <= completed:
            raise ValueError("Reports may reference only native completed cases")
        # Reject non-JSON evidence and nonfinite numbers before saving a checkpoint.
        _digest(document)

    def save(self, path):
        """Atomically replace a checkpoint in an existing destination directory."""
        self._validate()
        path = Path(path)
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8",
                                         dir=path.parent, delete=False) as stream:
            json.dump(self._document, stream, indent=2, allow_nan=False)
            stream.write("\n")
            temporary = Path(stream.name)
        temporary.replace(path)

    @classmethod
    def load(cls, path):
        """Load a checkpoint, rejecting unknown layouts or stale result rows."""
        return cls(json.loads(Path(path).read_text(encoding="utf-8")))

    def _row(self, case_id):
        for row in self._document["cases"]:
            if row["id"] == case_id:
                return row
        raise KeyError(f"Unknown case id: {case_id}")

    def _check_consumer(self, criteria_id, criteria_version):
        criteria = self._document["criteria"]
        if (criteria_id != criteria["criteria_id"]
                or criteria_version != criteria["criteria_version"]):
            raise CriteriaVersionMismatch("Consumer criteria do not match campaign")
        check_criteria(self._document["cases"], criteria_id, criteria_version)

    def observe(self, case_id, *, counter, now, process_alive):
        """Persist a counter sample; repeated polls do not refresh progress age."""
        if type(process_alive) is not bool:
            raise ValueError("process_alive must be boolean")
        row = self._row(case_id)
        if row["status"] in {"completed", "failed"}:
            raise ValueError("Terminal cases require a separate retry campaign")
        evidence = observe_progress(row["evidence"], counter=counter, now=now)
        evidence["process_alive"] = process_alive
        health = classify_lane(evidence, now, self._document["stall_after"])
        row.update(evidence=evidence, status="running")
        return health

    def health(self, *, now):
        """Classify saved output evidence using the launch-time age limit."""
        return {row["id"]: classify_lane(row["evidence"], now,
                                        self._document["stall_after"])
                for row in self._document["cases"]}

    def collect(self, case_id, *, criteria_id, criteria_version, exit_code,
                native_readback_ok, criterion_met, failure_class=None):
        """Collect terminal evidence without treating exit success as acceptance.

        Native completed means successful exit plus successful readback. A
        completed row qualifies only with an explicit True acceptance decision;
        None records NOT_EVALUATED. No acceptance limit is supplied here.
        """
        self._check_consumer(criteria_id, criteria_version)
        completed = _terminal_evidence(exit_code, native_readback_ok, criterion_met)
        if failure_class is not None:
            _text(failure_class, "failure_class")
        row = self._row(case_id)
        if row["status"] in {"completed", "failed"}:
            raise ValueError("Terminal cases require a separate retry campaign")
        row.update(status="completed" if completed else "failed",
                   qualified=criterion_met if completed else False,
                   evidence=dict(row["evidence"], process_alive=False,
                                 exit_code=exit_code,
                                 native_readback_ok=native_readback_ok,
                                 criterion_met=criterion_met),
                   failure_class=failure_class)

    def stop(self, reason=None):
        """Record the blocker and clearance condition for every caller stop."""
        if not reason and any(row["status"] == "running"
                              for row in self._document["cases"]):
            reason = "Active cases require diagnosis before continuation"
        record = classify_stop(reason, self._document["cases"])
        self._document["stop_record"] = asdict(record)
        return record

    def next_cases(self):
        """Return only untouched authorized ids; never erase a recorded blocker."""
        saved = self._document["stop_record"]
        if saved is None or any(row["status"] == "running"
                                for row in self._document["cases"]):
            return []
        record = StopRecord(**dict(saved, disposition=StopDisposition(saved["disposition"])))
        pending = {row["id"] for row in self._document["cases"]
                   if row["status"] == "pending"}
        return [case_id for case_id in resume_plan(record, self._document["authority"])
                if case_id in pending]

    def run_untouched(self, execute, *, checkpoint):
        """Run authorized untouched cases through a caller-supplied adapter.

        ``execute(case_id, criteria)`` returns keyword arguments for ``collect``
        plus an optional ``stop_reason``. A nonzero exit is an isolated failure
        unless the adapter supplies a blocker. An adapter exception or invalid
        result stops for diagnosis. No failed or in-flight case is replayed.
        The callback is responsible for its own solver-specific safety guards.
        """
        if self._document["stop_record"] is None:
            self.stop()
        self.save(checkpoint)
        while candidates := self.next_cases():
            case_id = candidates[0]
            # Record the attempt before control leaves this module. An abrupt
            # termination leaves a running case requiring explicit diagnosis.
            self._row(case_id)["status"] = "running"
            self.save(checkpoint)
            try:
                result = dict(execute(case_id, deepcopy(self._document["criteria"])))
                reason = result.pop("stop_reason", None)
                if reason is not None:
                    _text(reason, "stop_reason")
                self.collect(case_id, **result)
            except Exception:
                # Do not copy adapter exception text into public checkpoints.
                self.stop("Execution adapter raised an exception; diagnosis required")
                self.save(checkpoint)
                raise
            self.stop(reason)
            self.save(checkpoint)
        saved = self._document["stop_record"]
        return StopRecord(**dict(saved, disposition=StopDisposition(saved["disposition"])))

    def result_rows(self, *, criteria_id, criteria_version):
        """Common fail-closed boundary for collector and downstream consumers."""
        self._check_consumer(criteria_id, criteria_version)
        rows = deepcopy(self._document["cases"])
        for row in rows:
            row["qualification"] = (
                "NOT_EVALUATED" if row["qualified"] is None else
                "QUALIFIED" if row["qualified"] else "WITHHELD")
        return rows

    def status(self, *, criteria_id, criteria_version, next_action):
        """Monitor output with native, qualified and current report counts."""
        rows = self.result_rows(criteria_id=criteria_id, criteria_version=criteria_version)
        return campaign_status(rows, self._document["report_ids"],
                               criteria_id=criteria_id, criteria_version=criteria_version,
                               next_action=next_action)

    def mark_reported(self, case_ids, *, criteria_id, criteria_version):
        """Record rows consumed by a report at the explicitly checked version."""
        rows = self.result_rows(criteria_id=criteria_id, criteria_version=criteria_version)
        ids = list(case_ids)
        completed = {row["id"] for row in rows if row["status"] == "completed"}
        if not set(ids) <= completed:
            raise ValueError("Reports may reference only native completed cases")
        self._document["report_ids"] = sorted(set(self._document["report_ids"]) | set(ids))

    def workbook_csv(self, *, criteria_id, criteria_version):
        """Export all rows, with explicit criteria columns, for workbook import."""
        rows = self.result_rows(criteria_id=criteria_id, criteria_version=criteria_version)
        stream = io.StringIO()
        fields = ["id", "status", "qualified", "qualification", "criteria_id", "criteria_version",
                  "criteria_source"]
        writer = csv.DictWriter(stream, fieldnames=fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)
        return stream.getvalue()

    def report(self, *, criteria_id, criteria_version, next_action):
        """Build a report payload from exactly the same checked rows as CSV."""
        return dict(criteria=deepcopy(self._document["criteria"]),
                    rows=self.result_rows(criteria_id=criteria_id,
                                          criteria_version=criteria_version),
                    status=self.status(criteria_id=criteria_id,
                                       criteria_version=criteria_version,
                                       next_action=next_action),
                    retention=deepcopy(self._document["retention"]))

    def preview_criteria_change(self, *, criteria_id, criteria_version,
                                criteria_source, qualifications):
        """Return all affected ids before applying caller-supplied re-evaluations.

        Completed rows require an explicit boolean or None decision. Pending,
        active and failed rows receive the new stamp but cannot qualify.
        A changed acceptance basis requires a changed identity or version (§3).
        """
        new = _criteria(criteria_id, criteria_version, criteria_source)
        old = self._document["criteria"]
        if (new["criteria_id"], new["criteria_version"]) == (
                old["criteria_id"], old["criteria_version"]):
            raise ValueError("A criteria change requires a new identity or version")
        completed = {row["id"]: row for row in self._document["cases"]
                     if row["status"] == "completed"}
        if (set(qualifications) != set(completed)
                or any(value is not None and type(value) is not bool
                       for value in qualifications.values())):
            raise ValueError("qualifications must explicitly cover exactly completed rows")
        changed = [case_id for case_id, row in completed.items()
                   if qualifications[case_id] != row["qualified"]]
        return dict(before_digest=_digest(self._document), old=deepcopy(old), new=new,
                    qualifications=deepcopy(qualifications),
                    affected_ids=[row["id"] for row in self._document["cases"]],
                    qualification_changed_ids=changed,
                    newly_withheld=[case_id for case_id in changed
                                    if completed[case_id]["qualified"] is True
                                    and qualifications[case_id] is not True])

    def apply_criteria_change(self, preview, *, emit):
        """Announce an unchanged preview before changing any row or report stamp."""
        preview = deepcopy(preview)
        if preview["before_digest"] != _digest(self._document):
            raise ValueError("Criteria preview is stale; preview the current rows again")
        expected = self.preview_criteria_change(**preview["new"],
                                                qualifications=preview["qualifications"])
        if preview != expected:
            raise ValueError("Criteria preview was modified")
        emit(json.dumps(preview, sort_keys=True))
        if preview["before_digest"] != _digest(self._document):
            raise ValueError("Criteria preview became stale during announcement")
        for row in self._document["cases"]:
            row.update(preview["new"])
            if row["status"] == "completed":
                row["qualified"] = preview["qualifications"][row["id"]]
                row["evidence"]["criterion_met"] = row["qualified"]
        self._document["criteria"] = deepcopy(preview["new"])
        self._document["report_ids"] = []
        self._document["criteria_changes"].append(preview)
