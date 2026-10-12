"""Synthetic final checks and ledger outcomes; no solver or engineering limits."""

import pytest

from digitalmodel.drilling_riser.postprocess.evaluate import (
    CaseCheck,
    evaluate_case,
    summarise,
)


def check(status="PASS", row="CR-02"):
    return CaseCheck(row, status, "synthetic")


def summary(results, cases=("C-1",), outcomes=None):
    if outcomes is None:
        outcomes = {cid: "ok" for cid in cases}
    return summarise(
        results,
        expected_cases={"CR-02": {"G": cases}},
        ledger_outcomes=outcomes,
    )["CR-02"]


def test_observed_pass_and_failed_solve_are_not_engineering_failure():
    s = summary(
        [("C-1", check())],
        ("C-1", "C-2"),
        {"C-1": "ok", "C-2": "statics_diverged"},
    )
    assert s["status"] == "NOT_EVALUATED"
    assert s["observed_status"] == "PASS"
    assert s["coverage_complete"] is False
    assert s["missing_cases"] == ["C-2"]
    assert s["missing_count"] == 1  # two expected minus one result
    assert s["counts"]["FAIL"] == 0
    assert s["final_outcomes"]["C-2"] == "statics_diverged"
    assert s["groups"]["G"]["missing_cases"] == ["C-2"]


def test_manifest_case_without_ledger_stays_visible():
    # Synthetic manifest and final ledger; no absent case is inferred from results.
    manifest = {"cases": [{"case_id": "C-1"}, {"case_id": "C-2"}]}
    ledger = [{"case_id": "C-1", "status": "ok"}]
    s = summary(
        [("C-1", check())],
        tuple(c["case_id"] for c in manifest["cases"]),
        {r["case_id"]: r["status"] for r in ledger},
    )
    assert s["expected_cases"] == ["C-1", "C-2"]
    assert s["recorded_cases"] == ["C-1"]
    assert s["final_outcomes"] == {"C-1": "ok", "C-2": "missing"}
    assert s["missing_count"] == len(manifest["cases"]) - len(ledger)
    assert s["status"] == "NOT_EVALUATED"


def test_failure_only_in_sensitivity_group_stays_visible():
    s = summarise(
        [("C-1", check()), ("C-2", check("FAIL"))],
        expected_cases={"CR-02": {"G": ["C-1"], "I": ["C-2"]}},
        ledger_outcomes={"C-1": "ok", "C-2": "ok"},
    )["CR-02"]
    assert s["status"] == "FAIL"
    assert s["coverage_complete"] is True
    assert s["groups"]["G"]["status"] == "PASS"
    assert s["groups"]["I"]["status"] == "FAIL"
    assert s["groups"]["I"]["counts"]["FAIL"] == 1


def test_empty_selection_is_not_a_pass():
    assert summarise([]) == {}
    s = summary([], ())
    assert s["expected_cases"] == []
    assert s["missing_count"] == 0  # zero expected minus zero recorded
    assert s["status"] == "NOT_EVALUATED"
    assert s["coverage_complete"] is False
    assert s["groups"]["G"]["status"] == "NOT_EVALUATED"


def test_unsupported_criterion_is_unevaluated_not_failed():
    result = evaluate_case({}, {"id": "CR-02", "check_key": None}, {})
    s = summary([("C-1", result)])
    assert s["status"] == "NOT_EVALUATED"
    assert s["coverage_complete"] is False
    assert s["unevaluated_cases"] == ["C-1"]
    assert s["missing_count"] == 0  # ledger result exists; criterion unsupported
    assert s["counts"]["FAIL"] == 0


def test_omitted_expected_cases_cannot_establish_complete_coverage():
    s = summarise([("C-1", check())])["CR-02"]
    assert s["status"] == "NOT_EVALUATED"
    assert s["observed_status"] == "PASS"
    assert s["coverage_complete"] is None
    assert s["expected_cases"] is None
    assert s["missing_count"] is None
    assert s["groups"] == {}


def test_omitted_ledger_evidence_cannot_establish_complete_coverage():
    s = summarise([("C-1", check())], expected_cases={"CR-02": {"G": ["C-1"]}})["CR-02"]
    assert s["status"] == "NOT_EVALUATED"
    assert s["coverage_complete"] is None
    assert s["final_outcomes"] == {"C-1": None}


def test_complete_final_results_pass_and_keep_existing_fields():
    result = check()
    result.u = 1 / 2  # synthetic ordering value, no acceptance calculation
    s = summary([("C-1", result)])
    assert s["status"] == "PASS"
    assert s["coverage_complete"] is True
    assert s["missing_count"] == 0
    assert s["counts"] == {"PASS": 1, "FAIL": 0, "NOT_EVALUATED": 0}
    assert s["governing"]["case_id"] == "C-1"
    assert s["max_u"] == result.u


def test_engineering_failure_and_incomplete_coverage_coexist():
    s = summary([("C-1", check("FAIL"))], ("C-1", "C-2"))
    assert s["status"] == "FAIL"
    assert s["coverage_complete"] is False
    assert s["missing_cases"] == ["C-2"]
    assert s["counts"]["FAIL"] == 1


def test_final_failed_attempt_prevents_stale_check_from_passing():
    s = summary([("C-1", check())], outcomes={"C-1": "statics_diverged"})
    assert s["status"] == "NOT_EVALUATED"
    assert s["missing_cases"] == ["C-1"]


def test_expected_row_without_any_check_is_returned():
    s = summary([], outcomes={})
    assert s["missing_cases"] == ["C-1"]
    assert s["status"] == "NOT_EVALUATED"
    assert s["final_outcomes"] == {"C-1": "missing"}


def test_screening_does_not_govern_or_hide_missing_coverage():
    s = summary([("C-1", check("SCREENING"))])
    assert s["coverage_complete"] is True
    assert s["status"] == "NOT_EVALUATED"
    assert s["governing"] is None
    assert s["counts"]["SCREENING"] == 1


def test_criterion_applicability_and_overlapping_groups_use_unique_cases():
    s = summarise(
        [("C-1", check()), ("C-2", check(row="CR-01"))],
        expected_cases={
            "CR-02": {"G": ["C-1"], "I": ["C-1"]},
            "CR-01": {"I": ["C-2"]},
        },
        ledger_outcomes={"C-1": "ok", "C-2": "ok"},
    )
    assert s["CR-02"]["counts"]["PASS"] == 1  # group overlap is not two cases
    assert s["CR-02"]["expected_cases"] == ["C-1"]
    assert s["CR-01"]["expected_cases"] == ["C-2"]
    assert all(row["status"] == "PASS" for row in s.values())


@pytest.mark.parametrize(
    "results,cases,outcomes",
    [
        ([("C-1", check()), ("C-1", check())], ["C-1"], {"C-1": "ok"}),
        ([("C-2", check())], ["C-1"], {"C-1": "ok"}),
        ([("C-1", check("ok"))], ["C-1"], {"C-1": "ok"}),
        ([], "C-1", {}),
        ([], ["C-1", "C-1"], {}),
        ([], [None], {}),
        ([], ["C-1"], {"C-1": ""}),
    ],
)
def test_ambiguous_inputs_are_rejected(results, cases, outcomes):
    with pytest.raises(ValueError):
        summary(results, cases, outcomes)
