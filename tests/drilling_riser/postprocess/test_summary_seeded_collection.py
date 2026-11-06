# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Seed reconciliation at the collect -> evaluate -> summarise boundary.

Sources: aggregate.collect/load_verified and evaluate.evaluate_case/summarise
docstrings define final attempts, explicit seeds and logical-case coverage.
All data are synthetic. Seed IDs 1 and 2 label two declared repetitions; they
are not a recommended sample size. Setting demand 1 and allowable 2 give
U = 1 / 2 = 0.5 by checks.tension_setting_max; demand 3 gives U = 3 / 2 = 1.5.
These arbitrary inputs test existing verdict propagation, not design limits or
extreme statistics. Dates only order attempts. No solver is imported or run.
"""

import hashlib
import html
import json
import re
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.postprocess.aggregate import collect, load_verified
from digitalmodel.drilling_riser.postprocess.evaluate import evaluate_case, summarise


SEEDS = {"A": {1: "A-seed-1", 2: "A-seed-2"}, "B": {1: "B-seed-1", 2: "B-seed-2"}}
GROUPS = {"base": ["A"], "sensitivity": ["B"]}
ROW = {"id": "criterion", "check_key": "tension_setting_max", "limit": {"value": 2}}


def write_seed_run(root, name, logical_cases, outcomes):
    """Write declared seeds and optional ledger records with synthetic channels."""
    run = root / name
    run.mkdir()
    cases = [
        {"case_id": cid, "matrix_case": {"case_id": logical, "seed": seed}}
        for logical in logical_cases
        for seed, cid in SEEDS[logical].items()
    ]
    (run / "run.json").write_text(json.dumps({"cases": cases}), encoding="utf-8")
    if outcomes is None:
        return run
    ledger = []
    for cid, status in outcomes.items():
        entry = {"case_id": cid, "status": status, "finished_utc": name}
        if status == "ok":
            payload = json.dumps(
                {"channels": {"w5": {"schema": "riser-w5-channels/1"}}}
            ).encode()
            result = run / f"{cid}.json"
            result.write_bytes(payload)
            entry.update(
                results_path=result.name,
                results_sha256=hashlib.sha256(payload).hexdigest(),
            )
        ledger.append(entry)
    (run / "ledger.jsonl").write_text(
        "\n".join(map(json.dumps, ledger)), encoding="utf-8"
    )
    return run


def seeded_summary(runs, *, seeds=SEEDS, groups=GROUPS, row=ROW, demands=None):
    """Explicit caller reconciliation: a logical case needs every declared seed.

    Applicability and seed membership come from declarations, never ok records.
    The first non-ok seed supplies the logical outcome; evaluate_case separately
    exposes missing seeds. This adapter is a test fixture, not an inferred policy.
    """
    records, issues = collect(runs)
    outcomes = {cid: "ok" for cid in records}
    outcomes.update({issue["case_id"]: issue["status"] for issue in issues})
    checks, logical_outcomes = [], {}
    for logical, seed_cases in seeds.items():
        docs = {
            seed: load_verified(records[cid])
            for seed, cid in seed_cases.items()
            if cid in records
        }
        final = [outcomes.get(cid, "missing") for cid in seed_cases.values()]
        logical_outcomes[logical] = next(
            (status for status in final if status != "ok"), "ok"
        )
        ctx = {"top_tension_kips": (demands or {}).get(logical, 1)}
        checks.append((logical, evaluate_case(docs, row, ctx, seed_ids=seed_cases)))
    return summarise(
        checks, expected_cases={row["id"]: groups}, ledger_outcomes=logical_outcomes
    )[row["id"]]


@pytest.mark.parametrize(
    "outcomes",
    [None, {}, {"B-seed-1": "ok"}, {"B-seed-1": "ok", "B-seed-2": "statics_diverged"}],
)
def test_missing_sensitivity_seeds_count_as_one_logical_gap(tmp_path, outcomes):
    """Hand count: expected {A,B}, complete {A}, missing {B}: one logical gap.

    There are zero or one successful B seeds, both less than the two declared.
    A contributes one PASS; B contributes one NOT_EVALUATED, never a solve FAIL.
    """
    base = write_seed_run(
        tmp_path, "2026-01-01", ["A"], dict.fromkeys(SEEDS["A"].values(), "ok")
    )
    sensitivity = write_seed_run(tmp_path, "2026-01-02", ["B"], outcomes)
    result = seeded_summary([base, sensitivity])
    assert result["status"] == "NOT_EVALUATED"
    assert result["observed_status"] == "PASS"
    assert result["coverage_complete"] is False
    assert result["expected_cases"] == result["recorded_cases"] == ["A", "B"]
    assert result["missing_cases"] == result["unevaluated_cases"] == ["B"]
    assert result["missing_count"] == 1
    assert result["counts"] == {"PASS": 1, "FAIL": 0, "NOT_EVALUATED": 1}
    assert result["groups"]["base"]["status"] == "PASS"
    assert result["groups"]["sensitivity"]["status"] == "NOT_EVALUATED"
    assert result["groups"]["sensitivity"]["missing_count"] == 1
    assert result["final_outcomes"]["B"] == (
        "statics_diverged" if outcomes and "B-seed-2" in outcomes else "missing"
    )


@pytest.mark.parametrize("reverse_runs", [False, True])
@pytest.mark.parametrize("recover", [False, True])
def test_final_seed_attempt_controls_logical_coverage(tmp_path, reverse_runs, recover):
    """Hand count: four ok seeds initially cover {A,B}. B's last seed then fails.

    Without recovery: two expected minus one complete = one gap. With a later
    ok retry: two minus two = zero gaps and two PASS checks, in either run order.
    """
    first = write_seed_run(
        tmp_path,
        "2026-01-01",
        ["A", "B"],
        {cid: "ok" for cases in SEEDS.values() for cid in cases.values()},
    )
    failed = write_seed_run(
        tmp_path, "2026-01-02", ["B"], {"B-seed-2": "statics_diverged"}
    )
    runs = [first, failed]
    if recover:
        runs.append(write_seed_run(tmp_path, "2026-01-03", ["B"], {"B-seed-2": "ok"}))
    result = seeded_summary(reversed(runs) if reverse_runs else runs)
    assert result["status"] == ("PASS" if recover else "NOT_EVALUATED")
    assert result["coverage_complete"] is recover
    assert result["missing_cases"] == ([] if recover else ["B"])
    assert result["missing_count"] == (0 if recover else 1)
    assert result["final_outcomes"]["B"] == ("ok" if recover else "statics_diverged")
    assert result["counts"]["PASS"] == (2 if recover else 1)


@pytest.mark.parametrize("missing_base", [False, True])
def test_seeded_engineering_failure_survives_a_separate_coverage_gap(
    tmp_path, missing_base
):
    """Hand calculation: A has U=1/2=0.5; B has U=3/2=1.5 and governs FAIL.

    Dropping both A seeds leaves one logical gap without masking B's engineering
    failure. Complete final solver results and criterion failure can coexist.
    """
    cases = ["B"] if missing_base else ["A", "B"]
    run = write_seed_run(
        tmp_path,
        "2026-01-01",
        ["A", "B"],
        {cid: "ok" for logical in cases for cid in SEEDS[logical].values()},
    )
    result = seeded_summary([run], demands={"B": 3})
    assert result["status"] == result["observed_status"] == "FAIL"
    assert result["coverage_complete"] is (not missing_base)
    assert result["missing_cases"] == (["A"] if missing_base else [])
    assert result["counts"]["FAIL"] == 1
    assert result["groups"]["sensitivity"]["status"] == "FAIL"
    assert result["governing"]["case_id"] == "B"
    assert result["max_u"] == 1.5


def test_seeded_unsupported_criterion_has_results_but_no_acceptance(tmp_path):
    """Hand count: four ok seeds, two logical checks unsupported, zero missing.

    Unsupported checks contribute two NOT_EVALUATED and zero engineering FAIL.
    """
    run = write_seed_run(
        tmp_path,
        "2026-01-01",
        ["A", "B"],
        {cid: "ok" for cases in SEEDS.values() for cid in cases.values()},
    )
    result = seeded_summary([run], row={"id": "unsupported", "check_key": None})
    assert result["status"] == "NOT_EVALUATED"
    assert result["coverage_complete"] is False
    assert result["missing_cases"] == []
    assert result["unevaluated_cases"] == ["A", "B"]
    assert result["counts"] == {"PASS": 0, "FAIL": 0, "NOT_EVALUATED": 2}


def test_empty_seeded_selection_is_not_complete():
    """Hand count: zero declared logical cases and zero checks cannot yield PASS."""
    result = seeded_summary([], seeds={}, groups={"base": []})
    assert result["status"] == "NOT_EVALUATED"
    assert result["coverage_complete"] is False
    assert result["missing_count"] == 0
    assert result["recorded_cases"] == []


def test_documented_worked_example_executes():
    """Hand count: {A,B} minus {A} = {B}; retry gives {A,B} minus {A,B} = {}."""
    path = (
        Path(__file__).resolve().parents[3]
        / "docs"
        / "drilling-riser-summary-coverage.html"
    )
    match = re.search(
        r'<pre id="worked-example"><code>(.*?)</code></pre>',
        path.read_text(encoding="utf-8"),
        re.DOTALL,
    )
    assert match is not None, "coverage contract needs an executable worked example"
    namespace = {}
    exec(compile(html.unescape(match.group(1)), str(path), "exec"), namespace)
    incomplete, complete = namespace["incomplete"], namespace["complete"]
    assert incomplete["status"] == "NOT_EVALUATED"
    assert incomplete["observed_status"] == "PASS"
    assert incomplete["coverage_complete"] is False
    assert incomplete["missing_cases"] == ["B"]
    assert incomplete["missing_count"] == 1
    assert incomplete["final_outcomes"] == {"A": "ok", "B": "missing"}
    assert incomplete["groups"]["base"]["status"] == "PASS"
    assert incomplete["groups"]["sensitivity"]["status"] == "NOT_EVALUATED"
    assert complete["status"] == complete["observed_status"] == "PASS"
    assert complete["coverage_complete"] is True
    assert complete["missing_cases"] == []
    assert complete["missing_count"] == 0
    assert complete["final_outcomes"] == {"A": "ok", "B": "ok"}
    assert all(group["status"] == "PASS" for group in complete["groups"].values())
