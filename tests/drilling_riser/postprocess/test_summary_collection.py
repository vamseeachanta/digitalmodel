# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Collect-to-summary regressions using synthetic, unseeded logical cases.

Contract sources: aggregate.py module docstring (final attempts and absent
ledgers) and evaluate.summarise docstring (explicit applicability and coverage).
No engineering limits, solver inputs or external result data are used.
"""

import hashlib
import json

import pytest

from digitalmodel.drilling_riser.postprocess.aggregate import collect, load_verified
from digitalmodel.drilling_riser.postprocess.evaluate import CaseCheck, summarise


def write_run(root, name, cases, outcomes):
    """Write a synthetic manifest, optionally with a ledger and verified results."""
    run = root / name
    run.mkdir()
    (run / "run.json").write_text(
        json.dumps({"cases": [{"case_id": cid} for cid in cases]}),
        encoding="utf-8",
    )
    if outcomes is None:
        return run
    ledger = []
    for cid, outcome in outcomes.items():
        row = {"case_id": cid, "status": outcome, "finished_utc": name}
        if outcome == "ok":
            # This is an already evaluated synthetic observation, not acceptance.
            payload = json.dumps({"case_id": cid, "status": "PASS"}).encode()
            result = run / f"{cid}.json"
            result.write_bytes(payload)
            row.update(
                results_path=result.name,
                results_sha256=hashlib.sha256(payload).hexdigest(),
            )
        ledger.append(row)
    (run / "ledger.jsonl").write_text(
        "\n".join(json.dumps(row) for row in ledger), encoding="utf-8"
    )
    return run


def collected_summary(runs, groups, *, checks=None):
    """Adapt final records, retaining every collector issue as a final outcome."""
    records, issues = collect(runs)
    outcomes = {cid: "ok" for cid in records}
    outcomes.update({issue["case_id"]: issue["status"] for issue in issues})
    if checks is None:
        checks = [
            (cid, CaseCheck("criterion", load_verified(record)["status"], "synthetic"))
            for cid, record in records.items()
        ]
    return summarise(
        checks,
        expected_cases={"criterion": groups},
        ledger_outcomes=outcomes,
    )["criterion"]


@pytest.mark.parametrize("outcomes", [None, {}, {"B": "statics_diverged"}])
def test_missing_sensitivity_result_prevents_campaign_pass(tmp_path, outcomes):
    """Hand count: {A,B} minus {A} = {B}; one gap and no engineering failure.

    Missing ledger, missing ledger row and failed solve have the same coverage
    consequence, while the final outcome retains the distinct cause.
    """
    observed = write_run(tmp_path, "2026-01-01", ["A"], {"A": "ok"})
    sensitivity = write_run(tmp_path, "2026-01-02", ["B"], outcomes)
    result = collected_summary(
        [observed, sensitivity], {"base": ["A"], "sensitivity": ["B"]}
    )
    assert result["status"] == "NOT_EVALUATED"
    assert result["observed_status"] == "PASS"
    assert result["coverage_complete"] is False
    assert result["missing_cases"] == ["B"]
    assert result["missing_count"] == 1
    assert result["counts"]["FAIL"] == 0
    assert result["groups"]["base"]["status"] == "PASS"
    assert result["groups"]["sensitivity"]["status"] == "NOT_EVALUATED"
    expected_outcome = "statics_diverged" if outcomes else "missing"
    assert result["final_outcomes"] == {"A": "ok", "B": expected_outcome}


@pytest.mark.parametrize("reverse_runs", [False, True])
def test_final_failed_attempt_overrides_stale_passing_check(tmp_path, reverse_runs):
    """Hand count: one expected case, latest failed attempt, one coverage gap.

    The final timestamp, rather than iteration order or a stale observation,
    governs coverage under aggregate.py's documented latest-record contract.
    """
    first = write_run(tmp_path, "2026-01-01", ["A"], {"A": "ok"})
    last = write_run(tmp_path, "2026-01-02", ["A"], {"A": "statics_diverged"})
    runs = [last, first] if reverse_runs else [first, last]
    stale = [("A", CaseCheck("criterion", "PASS", "earlier observation"))]
    result = collected_summary(runs, {"base": ["A"]}, checks=stale)
    assert result["observed_status"] == "PASS"
    assert result["status"] == "NOT_EVALUATED"
    assert result["missing_count"] == 1
    assert result["final_outcomes"] == {"A": "statics_diverged"}


def test_successful_retry_completes_missing_sensitivity_coverage(tmp_path):
    """Hand count: two expected minus two final results = zero missing cases."""
    first = write_run(tmp_path, "2026-01-01", ["A", "B"], {"A": "ok"})
    retry = write_run(tmp_path, "2026-01-02", ["B"], {"B": "ok"})
    result = collected_summary([retry, first], {"base": ["A"], "sensitivity": ["B"]})
    assert result["status"] == "PASS"
    assert result["coverage_complete"] is True
    assert result["missing_count"] == 0
    assert result["counts"] == {"PASS": 2, "FAIL": 0, "NOT_EVALUATED": 0}
    assert result["recorded_cases"] == ["A", "B"]
    assert all(group["status"] == "PASS" for group in result["groups"].values())


def test_manifest_without_any_final_results_remains_unevaluated(tmp_path):
    """Hand count: two declared minus zero results = two missing cases."""
    run = write_run(tmp_path, "2026-01-01", ["A", "B"], None)
    result = collected_summary([run], {"base": ["A", "B"]})
    assert result["status"] == "NOT_EVALUATED"
    assert result["missing_count"] == 2
    assert result["missing_cases"] == ["A", "B"]
    assert result["recorded_cases"] == []
    assert result["governing"] is None
