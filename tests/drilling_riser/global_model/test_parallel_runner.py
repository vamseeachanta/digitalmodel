"""Native parallel OrcaFlex runner: retry policy, stop rules, ledger, and a solver round trip."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex import orcaflex_api
from digitalmodel.solvers.orcaflex import parallel_runner as pr

# --------------------------------------------------------------------------- fault classes (W306)


@pytest.mark.parametrize(
    ("phase", "licensing", "io_error", "expected"),
    [
        ("load", True, False, ("licence_fault", True)),
        ("statics", True, False, ("licence_fault", True)),
        ("save", False, True, ("infra_fault", True)),
        ("reopen", False, False, ("infra_fault", True)),  # a result that will not reopen: truncated file
        ("statics", False, False, ("statics_diverged", False)),
        ("dynamics", False, False, ("dynamics_failed", False)),
        ("extract", False, False, ("extraction_failed", False)),
        ("build", False, False, ("build_failed", False)),
        ("verify", False, False, ("verify_failed", False)),
    ],
)
def test_fault_class_retries_infrastructure_only(phase, licensing, io_error, expected):
    assert pr.fault_class(phase, licensing=licensing, io_error=io_error) == expected


def test_unstable_simulation_is_not_retried():
    assert pr.fault_class("dynamics", licensing=False, io_error=False)[1] is False


# --------------------------------------------------------------------------- batch stop rules


def test_guard_stops_on_version_drift():
    g = pr.BatchGuard(batch_size=100)
    g.record({"status": "version_drift"}, final=True)
    assert "version" in g.stop_reason


def test_guard_stops_on_two_consecutive_licence_faults():
    g = pr.BatchGuard(batch_size=100)
    g.record({"status": "licence_fault"}, final=False)
    assert g.stop_reason is None
    g.record({"status": "ok"}, final=True)
    g.record({"status": "licence_fault"}, final=False)
    assert g.stop_reason is None  # not consecutive
    g.record({"status": "licence_fault"}, final=False)
    assert "licence" in g.stop_reason


def test_guard_stops_when_more_than_5_percent_of_the_batch_fails():
    g = pr.BatchGuard(batch_size=40)  # 5 % of 40 = 2 cases
    for _ in range(2):
        g.record({"status": "statics_diverged"}, final=True)
    assert g.stop_reason is None
    g.record({"status": "statics_diverged"}, final=True)
    assert "5 %" in g.stop_reason


def test_worker_count_is_capped():
    assert pr.DEFAULT_MAX_WORKERS == 57
    with pytest.raises(ValueError):
        pr.check_workers(0)
    with pytest.raises(ValueError):
        pr.check_workers(10_000)


# --------------------------------------------------------------------------- hashing and ledger


def test_input_digest_covers_model_files_and_case(tmp_path: Path):
    (tmp_path / "includes").mkdir()
    (tmp_path / "master.yml").write_text("a: 1\n", encoding="utf-8")
    (tmp_path / "includes" / "x.yml").write_text("b: 2\n", encoding="utf-8")
    case = {"case_id": "C1", "analysis": "statics", "params": {"h": 1}}
    d1 = pr.input_digest(tmp_path / "master.yml", case)
    d2 = pr.input_digest(tmp_path / "master.yml", {**case, "params": {"h": 2}})
    (tmp_path / "includes" / "x.yml").write_text("b: 3\n", encoding="utf-8")
    d3 = pr.input_digest(tmp_path / "master.yml", case)
    assert len({d1, d2, d3}) == 3


def test_case_ids_must_be_unique_and_safe():
    with pytest.raises(ValueError):
        pr.check_cases([{"case_id": "A", "analysis": "statics"}, {"case_id": "A", "analysis": "statics"}])
    with pytest.raises(ValueError):
        pr.check_cases([{"case_id": "../x", "analysis": "statics"}])
    with pytest.raises(ValueError):
        pr.check_cases([{"case_id": "A", "analysis": "fatigue"}])


# --------------------------------------------------------------------------- solver round trip

solver = pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")


@solver
@pytest.mark.solver
def test_runner_round_trip_two_workers(tmp_path: Path):
    adapter = "tests.drilling_riser.global_model.runner_adapter:ADAPTER"
    cases = [
        {"case_id": "S-1", "analysis": "statics", "params": {}},
        {"case_id": "D-1", "analysis": "dynamics", "params": {"wave_height_m": 3.0}},
    ]
    out = pr.run_cases(cases, adapter=adapter, out_dir=tmp_path, max_workers=2, keep_sim=False,
                       pin="11.6", expected_dll="11.6c")
    assert out["stop_reason"] is None
    by = {r["case_id"]: r for r in out["results"]}
    assert by["S-1"]["status"] == "ok" and by["D-1"]["status"] == "ok"
    bad = pr.run_cases([{"case_id": "BAD-1", "analysis": "statics", "params": {"break_build": True}}],
                       adapter=adapter, out_dir=tmp_path, max_workers=1)
    assert bad["results"][0]["status"] == "build_failed" and bad["results"][0]["attempt"] == 1  # no retry
    ledger = [json.loads(x) for x in (tmp_path / "ledger.jsonl").read_text(encoding="utf-8").splitlines()]
    assert sorted(r["case_id"] for r in ledger) == ["BAD-1", "D-1", "S-1"]
    for r in (by["S-1"], by["D-1"]):
        res = tmp_path / r["results_path"]
        assert pr.sha256_file(res) == r["results_sha256"]
        assert r["sim_sha256"] and r["sim_kept"] is None  # digest recorded, file not kept
        assert r["orcaflex_dll"] == "11.6c" and r["threads"] == 1
        assert r["timings_s"]["total"] > 0
    d = json.loads((tmp_path / by["D-1"]["results_path"]).read_text(encoding="utf-8"))
    assert d["channels"]["te_top_max_n"] >= d["channels"]["te_top_min_n"] > 0
    assert by["D-1"]["timings_s"]["dynamics"] > 0
    assert not list(tmp_path.rglob("*.sim"))
