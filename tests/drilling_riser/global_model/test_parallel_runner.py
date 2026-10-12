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


def test_case_failed_carries_its_status_and_is_never_retried():
    e = pr.CaseFailed("nonphysical_static", "ring yawed 180 deg")
    assert e.status == "nonphysical_static" and "yawed" in str(e)


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
    odd = pr.run_cases([{"case_id": "ODD-1", "analysis": "statics", "params": {"nonphysical": True}}],
                       adapter=adapter, out_dir=tmp_path, max_workers=1)
    assert odd["results"][0]["status"] == "nonphysical_static" and odd["results"][0]["retry"] is False
    ledger = [json.loads(x) for x in (tmp_path / "ledger.jsonl").read_text(encoding="utf-8").splitlines()]
    assert sorted(r["case_id"] for r in ledger) == ["BAD-1", "D-1", "ODD-1", "S-1"]
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


@solver
@pytest.mark.solver
def test_reextract_a_kept_sim_appends_a_ledger_record_and_refuses_a_changed_file(tmp_path: Path):
    from digitalmodel.solvers.orcaflex import sim_retention as sr

    adapter = "tests.drilling_riser.global_model.runner_adapter:ADAPTER"
    case = {"case_id": "S-2", "analysis": "statics", "params": {}}
    out = pr.run_cases([case], adapter=adapter, out_dir=tmp_path, max_workers=1, keep_sim=True)
    rec = out["results"][0]
    assert rec["status"] == "ok" and (tmp_path / rec["sim_kept"]).exists()
    re = pr.reextract(case, adapter=adapter, out_dir=tmp_path, ledger_record=rec)
    assert re["status"] == "ok", re["message"]
    assert re["results_path"] == "results/S-2.reextract.json"
    assert re["input_sha256"] == rec["input_sha256"] and re["sim_sha256"] == rec["sim_sha256"]
    assert sr.latest_ok(tmp_path)["S-2"]["results_path"] == "results/S-2.reextract.json"
    (tmp_path / rec["sim_kept"]).write_bytes(b"changed")
    bad = pr.reextract(case, adapter=adapter, out_dir=tmp_path, ledger_record=rec, tag="again")
    assert bad["status"] == "extraction_failed" and "digest" in bad["message"]


# --------------------------------------------------------------------------- scheduling (no solver)

import threading
import time as _time
from concurrent.futures import ThreadPoolExecutor
from concurrent.futures.process import BrokenProcessPool
from types import SimpleNamespace


class _CountingPool(ThreadPoolExecutor):
    """Thread pool that records how many futures are outstanding (submitted, not yet done)."""

    created: list = []

    def __init__(self, max_workers):
        super().__init__(max_workers=max_workers)
        self.lock = threading.Lock()
        self.outstanding = 0
        self.max_outstanding = 0
        self.submitted: list[tuple[str, int]] = []
        _CountingPool.created.append(self)

    def submit(self, fn, case, **kw):
        with self.lock:
            self.outstanding += 1
            self.max_outstanding = max(self.max_outstanding, self.outstanding)
            self.submitted.append((case["case_id"], kw["attempt"]))
        fut = super().submit(fn, case, **kw)

        def _done(_):
            with self.lock:
                self.outstanding -= 1

        fut.add_done_callback(_done)
        return fut


@pytest.fixture
def counting_pool(monkeypatch):
    _CountingPool.created = []
    monkeypatch.setattr(pr, "_make_pool", _CountingPool)
    return _CountingPool


def _ok(case, *, attempt, **_):
    _time.sleep(0.01)
    return {"case_id": case["case_id"], "attempt": attempt, "status": "ok", "retry": False}


def _cases(n):
    return [{"case_id": f"C{i:02d}", "analysis": "statics"} for i in range(n)]


def test_in_flight_cases_never_exceed_the_worker_slots(monkeypatch, counting_pool, tmp_path):
    monkeypatch.setattr(pr, "run_one", _ok)
    out = pr.run_cases(_cases(12), adapter="x:y", out_dir=tmp_path, max_workers=2)
    assert [r["status"] for r in out["results"]] == ["ok"] * 12
    (pool,) = counting_pool.created
    assert len(pool.submitted) == 12 and pool.max_outstanding <= 2


def test_early_stop_leaves_unsubmitted_cases_not_run(monkeypatch, counting_pool, tmp_path):
    def drift(case, *, attempt, **_):
        return {"case_id": case["case_id"], "attempt": attempt, "status": "version_drift", "retry": False}

    monkeypatch.setattr(pr, "run_one", drift)
    out = pr.run_cases(_cases(6), adapter="x:y", out_dir=tmp_path, max_workers=1)
    assert out["stop_reason"] == "solver version drift from the pin"
    (pool,) = counting_pool.created
    assert pool.submitted == [("C00", 1)]  # nothing submitted after the stop rule tripped
    by = {r["case_id"]: r for r in out["results"]}
    assert by["C00"]["status"] == "version_drift"
    assert all(by[f"C{i:02d}"]["status"] == "not_run" and by[f"C{i:02d}"]["attempt"] == 0 for i in range(1, 6))


def test_broken_pool_is_replaced_and_the_lost_case_retried(monkeypatch, counting_pool, tmp_path):
    def flaky(case, *, attempt, **_):
        if case["case_id"] == "C01" and attempt == 1:
            raise BrokenProcessPool("a worker died")
        return _ok(case, attempt=attempt)

    monkeypatch.setattr(pr, "run_one", flaky)
    out = pr.run_cases(_cases(5), adapter="x:y", out_dir=tmp_path, max_workers=2)
    assert out["stop_reason"] is None
    assert [r["status"] for r in out["results"]] == ["ok"] * 5
    assert {r["case_id"]: r["attempt"] for r in out["results"]}["C01"] == 2
    assert len(counting_pool.created) == 2  # a new pool after the break
    ledger = [json.loads(x) for x in (tmp_path / "ledger.jsonl").read_text(encoding="utf-8").splitlines()]
    assert any(r["case_id"] == "C01" and r["status"] == "infra_fault" for r in ledger)


def test_a_pool_that_keeps_breaking_ends_the_batch_cleanly(monkeypatch, counting_pool, tmp_path):
    def dead(case, *, attempt, **_):
        raise BrokenProcessPool("a worker died")

    monkeypatch.setattr(pr, "run_one", dead)
    monkeypatch.setattr(pr.BatchGuard, "__init__", lambda self, n, fail_fraction=1.0: (
        setattr(self, "limit", n), setattr(self, "failed", 0), setattr(self, "licence_run", 0),
        setattr(self, "stop_reason", None))[0])
    out = pr.run_cases(_cases(8), adapter="x:y", out_dir=tmp_path, max_workers=2, max_attempts=10)
    assert out["stop_reason"] == f"worker pool broken {pr.MAX_POOL_RESTARTS + 1} times"
    assert len(counting_pool.created) == pr.MAX_POOL_RESTARTS + 1
    assert len(out["results"]) == 8 and all(r["status"] in ("infra_fault", "not_run") for r in out["results"])


def test_input_digest_covers_non_yaml_model_files(tmp_path):
    (tmp_path / "master.yml").write_text("a: 1\n", encoding="utf-8")
    (tmp_path / "data").mkdir()
    (tmp_path / "data" / "table.csv").write_text("1,2\n", encoding="utf-8")
    case = {"case_id": "A", "analysis": "statics"}
    d0 = pr.input_digest(tmp_path / "master.yml", case)
    (tmp_path / "data" / "table.csv").write_text("1,3\n", encoding="utf-8")
    assert pr.input_digest(tmp_path / "master.yml", case) != d0


def test_failed_case_leaves_no_orphan_sim_when_not_kept(monkeypatch, tmp_path):
    class Model:
        def __init__(self, path):
            self.state = "static"
            self.threadCount = 1

        def CalculateStatics(self):  # noqa: N802
            pass

        def SaveSimulation(self, path):  # noqa: N802
            Path(path).write_bytes(b"sim")

    ofx = SimpleNamespace(Model=Model, ModelState=SimpleNamespace(InStaticState="static"))

    class Adapter:
        def build(self, case, model_dir):
            model_dir.mkdir(parents=True)
            (model_dir / "master.yml").write_text("a: 1\n", encoding="utf-8")
            return model_dir / "master.yml"

        def extract(self, model, case):
            raise ValueError("extraction broke")

    monkeypatch.setattr(pr, "_api", lambda pin, dll: (ofx, "11.6c"))
    monkeypatch.setattr(pr, "_adapter", lambda path: Adapter())
    rec = pr.run_one({"case_id": "F1", "analysis": "statics"}, adapter="x:y", out_dir=str(tmp_path))
    assert rec["status"] == "extraction_failed" and rec["sim_kept"] is None
    assert not (tmp_path / "work" / "F1" / "F1.sim").exists()
    assert not (tmp_path / "work" / "F1" / "model").exists()
