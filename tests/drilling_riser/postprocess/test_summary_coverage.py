# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic final checks and ledger outcomes; no solver or engineering limits."""

from itertools import permutations, product

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



def test_mixed_pass_and_unevaluated_case_prevents_complete_verdict():
    s = summary(
        [("C-1", check()), ("C-2", check("NOT_EVALUATED"))],
        ("C-1", "C-2"),
        {"C-1": "ok", "C-2": "ok"},
    )
    assert s["status"] == "NOT_EVALUATED"
    assert s["observed_status"] == "PASS"
    assert s["coverage_complete"] is False
    assert s["unevaluated_cases"] == ["C-2"]
    assert s["missing_cases"] == []
    assert s["missing_count"] == 0
    assert s["counts"] == {"PASS": 1, "FAIL": 0, "NOT_EVALUATED": 1}


def test_mixed_pass_and_screening_counts_complete_recorded_coverage():
    result = check()
    result.u = 1 / 2  # synthetic ordering value, no acceptance calculation
    s = summary(
        [("C-1", result), ("C-2", check("SCREENING"))],
        ("C-1", "C-2"),
        {"C-1": "ok", "C-2": "ok"},
    )
    assert s["status"] == "PASS"
    assert s["observed_status"] == "PASS"
    assert s["coverage_complete"] is True
    assert s["expected_cases"] == ["C-1", "C-2"]
    assert s["recorded_cases"] == ["C-1", "C-2"]
    assert s["final_outcomes"] == {"C-1": "ok", "C-2": "ok"}
    assert s["missing_cases"] == []
    assert s["missing_count"] == 0
    assert s["unevaluated_cases"] == []
    assert s["counts"] == {"PASS": 1, "FAIL": 0, "NOT_EVALUATED": 0, "SCREENING": 1}
    assert s["governing"]["case_id"] == "C-1"
    assert s["max_u"] == result.u


@pytest.mark.parametrize("expected_cases", [[], "C-1", 1, ("C-1",)])
def test_non_mapping_expected_cases_raise_value_error(expected_cases):
    with pytest.raises(ValueError, match="expected_cases must be a mapping"):
        summarise([], expected_cases=expected_cases)


def test_explicit_none_expected_cases_preserves_unknown_coverage():
    s = summarise(
        [("C-1", check())], expected_cases=None, ledger_outcomes={"C-1": "ok"}
    )["CR-02"]
    assert s["status"] == "NOT_EVALUATED"
    assert s["observed_status"] == "PASS"
    assert s["coverage_complete"] is None
    assert s["expected_cases"] is None
    assert s["missing_cases"] is None
    assert s["missing_count"] is None
    assert s["groups"] == {}


def test_returned_criteria_are_sorted():
    s = summarise(
        [("C-1", check(row="CR-02")), ("C-2", check(row="CR-01"))],
        expected_cases={"CR-02": {"G": ["C-1"]}, "CR-01": {"G": ["C-2"]}},
        ledger_outcomes={"C-1": "ok", "C-2": "ok"},
    )
    assert list(s) == ["CR-01", "CR-02"]


def _permuted_summaries(results, groups, outcomes):
    """Cross every result, within-group case and group insertion order."""
    for result_order in permutations(results):
        for case_orders in product(*(permutations(cases) for cases in groups.values())):
            for group_order in permutations(zip(groups, case_orders)):
                yield summarise(
                    result_order,
                    expected_cases={
                        "CR-02": {group: iter(cases) for group, cases in group_order}
                    },
                    ledger_outcomes=outcomes,
                )["CR-02"]


def _ordered_checks(last_status):
    """Synthetic ranks: C-1=1/4, C-2=2/4, C-3=3/4 (PASS) or 6/4 (FAIL).

    These unique fractions only select the governing observation; statuses are
    supplied explicitly, without evaluating an engineering criterion.
    """
    last_u = 3 / 4 if last_status == "PASS" else 6 / 4
    return (
        ("C-1", CaseCheck("CR-02", "PASS", "synthetic", u=1 / 4)),
        ("C-2", CaseCheck("CR-02", "PASS", "synthetic", u=2 / 4)),
        ("C-3", CaseCheck("CR-02", last_status, "synthetic", u=last_u)),
    )


def _assert_summary_contents(
    actual, *, expected, recorded, missing, counts, outcomes, governing
):
    """Compare semantic sets and keyed outcomes against hand-counted fixtures."""
    assert set(actual["expected_cases"]) == expected
    assert set(actual["recorded_cases"]) == recorded
    assert set(actual["missing_cases"]) == missing
    assert actual["missing_count"] == len(missing)
    assert actual["unevaluated_cases"] == []
    assert actual["counts"] == counts
    assert actual["final_outcomes"] == outcomes
    assert actual["governing"]["case_id"] == governing[0]
    assert actual["governing"]["u"] == governing[1]
    assert actual["max_u"] == governing[1]


@pytest.mark.parametrize("last_status", ["PASS", "FAIL"])
def test_summary_is_invariant_to_case_and_group_order(last_status):
    """Refs #2277: complete coverage is independent of input ordering.

    Hand count: G={C-1,C-2}, I={C-2,C-3}; the union has three cases,
    not four. All three are recorded, so no case is missing. With C-3
    passing, aggregate/G/I PASS counts are 3/2/2. With C-3 failing,
    they are 2/2/1 and FAIL counts are 1/0/1. C-3 governs the union
    and I; C-2 governs G, using the fractions in _ordered_checks.
    Each status exercises 3! * 2! * 2! * 2! = 48 input orders.
    """
    results = _ordered_checks(last_status)
    groups = {"G": ("C-1", "C-2"), "I": ("C-2", "C-3")}
    outcomes = {"C-1": "ok", "C-2": "ok", "C-3": "ok"}
    # Counts are the hand calculation above, independent of summarise.
    aggregate_counts = {"PASS": 3, "FAIL": 0, "NOT_EVALUATED": 0}
    sensitivity_counts = {"PASS": 2, "FAIL": 0, "NOT_EVALUATED": 0}
    last_u = 3 / 4
    if last_status == "FAIL":
        aggregate_counts = {"PASS": 2, "FAIL": 1, "NOT_EVALUATED": 0}
        sensitivity_counts = {"PASS": 1, "FAIL": 1, "NOT_EVALUATED": 0}
        last_u = 6 / 4

    summaries = list(_permuted_summaries(results, groups, outcomes))
    assert len(summaries) == 48
    for actual in summaries:
        assert set(actual["groups"]) == {"G", "I"}
        assert actual["status"] == last_status
        assert actual["observed_status"] == last_status
        assert actual["coverage_complete"] is True
        assert {key: group["status"] for key, group in actual["groups"].items()} == {
            "G": "PASS", "I": last_status
        }
        for group, cases, counts, governing in (
            (actual, {"C-1", "C-2", "C-3"}, aggregate_counts, ("C-3", last_u)),
            (actual["groups"]["G"], {"C-1", "C-2"},
             {"PASS": 2, "FAIL": 0, "NOT_EVALUATED": 0}, ("C-2", 2 / 4)),
            (actual["groups"]["I"], {"C-2", "C-3"}, sensitivity_counts, ("C-3", last_u)),
        ):
            _assert_summary_contents(
                group, expected=cases, recorded=cases, missing=set(), counts=counts,
                outcomes={cid: outcomes[cid] for cid in cases}, governing=governing,
            )
            assert group["coverage_complete"] is True
            assert group["observed_status"] == group["status"]


@pytest.mark.parametrize("last_status", ["PASS", "FAIL"])
@pytest.mark.parametrize("absent_outcome", ["ok", "statics_diverged", "missing"])
def test_incomplete_coverage_survives_group_permutation(last_status, absent_outcome):
    """Refs #2277: every order withholds PASS while retaining observed FAIL.

    Hand count: G={C-1,C-2}, I={C-2,C-3,C-4}, union={C-1,C-2,C-3,C-4}.
    Recorded={C-1,C-2,C-3}, so missing={C-4}: four expected minus three
    recorded is one gap, confined to I. Observed aggregate/G/I PASS counts
    are 3/2/2, or 2/2/1 with C-3 failing; FAIL counts are then 1/0/1.
    C-3 governs the union and I; C-2 governs G. Missing ledger evidence,
    a failed solve and an ok ledger entry without a check all leave the gap.
    Each status/outcome pair exercises 3! * 2! * 3! * 2! = 144 orders.
    """
    results = _ordered_checks(last_status)
    groups = {"G": ("C-1", "C-2"), "I": ("C-2", "C-3", "C-4")}
    outcomes = {"C-1": "ok", "C-2": "ok", "C-3": "ok"}
    if absent_outcome != "missing":
        outcomes["C-4"] = absent_outcome
    expected_outcomes = {**outcomes, "C-4": absent_outcome}
    aggregate_counts = {"PASS": 3, "FAIL": 0, "NOT_EVALUATED": 0}
    sensitivity_counts = {"PASS": 2, "FAIL": 0, "NOT_EVALUATED": 0}
    final_status = "NOT_EVALUATED"
    last_u = 3 / 4
    if last_status == "FAIL":
        aggregate_counts = {"PASS": 2, "FAIL": 1, "NOT_EVALUATED": 0}
        sensitivity_counts = {"PASS": 1, "FAIL": 1, "NOT_EVALUATED": 0}
        final_status = "FAIL"
        last_u = 6 / 4

    summaries = list(_permuted_summaries(results, groups, outcomes))
    assert len(summaries) == 144
    for actual in summaries:
        assert actual["status"] == final_status
        assert actual["observed_status"] == last_status
        assert actual["coverage_complete"] is False
        assert set(actual["groups"]) == {"G", "I"}
        assert {key: group["status"] for key, group in actual["groups"].items()} == {
            "G": "PASS", "I": final_status
        }
        for group, cases, counts in (
            (actual, {"C-1", "C-2", "C-3", "C-4"}, aggregate_counts),
            (actual["groups"]["I"], {"C-2", "C-3", "C-4"}, sensitivity_counts),
        ):
            _assert_summary_contents(
                group, expected=cases, recorded=cases - {"C-4"}, missing={"C-4"},
                counts=counts, outcomes={cid: expected_outcomes[cid] for cid in cases},
                governing=("C-3", last_u),
            )
            assert group["status"] == final_status
            assert group["observed_status"] == last_status
            assert group["coverage_complete"] is False
        base = actual["groups"]["G"]
        _assert_summary_contents(
            base, expected={"C-1", "C-2"}, recorded={"C-1", "C-2"}, missing=set(),
            counts={"PASS": 2, "FAIL": 0, "NOT_EVALUATED": 0},
            outcomes={"C-1": "ok", "C-2": "ok"}, governing=("C-2", 2 / 4),
        )
        assert base["observed_status"] == "PASS"
        assert base["coverage_complete"] is True
