"""Time-bound .sim retention (owner decision W403): mark a verified, reproducible .sim, delete it after the window."""

from __future__ import annotations

import datetime as dt
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex import sim_retention as sr

T0 = dt.datetime(2026, 9, 27, 12, 0, tzinfo=dt.timezone.utc)


def _sha(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


def _run(tmp_path: Path, cid="C-1", *, channels=None, status="ok", sim=b"SIMDATA", results_sha=None,
         input_sha="a" * 64, dll="11.6c", extra_records=()):
    run = tmp_path / "run"
    (run / "results").mkdir(parents=True)
    (run / "work" / cid).mkdir(parents=True)
    simp = run / "work" / cid / f"{cid}.sim"
    simp.write_bytes(sim)
    res = run / "results" / f"{cid}.json"
    body = json.dumps({"case_id": cid, "channels": channels if channels is not None else {"w5": {"schema": "x"}}})
    res.write_text(body, encoding="utf-8")
    rec = {"case_id": cid, "attempt": 1, "status": status, "input_sha256": input_sha, "orcaflex_dll": dll,
           "sim_sha256": _sha(sim), "sim_bytes": len(sim), "sim_kept": f"work/{cid}/{cid}.sim",
           "results_path": f"results/{cid}.json", "results_sha256": results_sha or _sha(body.encode())}
    with (run / "ledger.jsonl").open("w", encoding="utf-8") as fh:
        for r in (rec, *extra_records):
            fh.write(json.dumps(r) + "\n")
    return run, simp


def _events(run: Path):
    f = run / sr.RETENTION_FILE
    return [json.loads(x) for x in f.read_text(encoding="utf-8").splitlines()] if f.exists() else []


def test_default_window_is_72_hours():
    assert sr.DEFAULT_WINDOW_H == 72.0


def test_verified_sim_is_marked_then_deleted_only_after_the_window(tmp_path):
    run, sim = _run(tmp_path)
    marks = sr.mark(run, now=T0)
    assert [m["case_id"] for m in marks] == ["C-1"]
    ev = _events(run)[0]
    assert ev["event"] == "marked" and ev["input_sha256"] == "a" * 64 and ev["orcaflex_dll"] == "11.6c"
    assert ev["delete_after_utc"] == "2026-09-30T12:00:00Z"
    assert sr.sweep(run, now=T0 + dt.timedelta(hours=71.9)) == [] and sim.exists()
    gone = sr.sweep(run, now=T0 + dt.timedelta(hours=72))
    assert [g["case_id"] for g in gone] == ["C-1"] and not sim.exists()
    assert gone[0]["bytes"] == len(b"SIMDATA")
    assert _events(run)[-1]["event"] == "deleted"
    assert sr.sweep(run, now=T0 + dt.timedelta(hours=100)) == []  # idempotent


def test_marking_is_idempotent(tmp_path):
    run, _ = _run(tmp_path)
    sr.mark(run, now=T0)
    assert sr.mark(run, now=T0 + dt.timedelta(hours=1)) == []
    assert len([e for e in _events(run) if e["event"] == "marked"]) == 1


@pytest.mark.parametrize(("kw", "reason"), [
    ({"results_sha": "0" * 64}, "results digest"),
    ({"channels": {"old": 1}}, "missing channels"),
    ({"input_sha": None}, "input digest"),
    ({"dll": None}, "solver version"),
])
def test_unverified_extraction_is_not_marked(tmp_path, kw, reason):
    run, sim = _run(tmp_path, **kw)
    assert sr.mark(run, now=T0) == []
    skipped = sr.check(run)
    assert skipped and reason in skipped[0]["reason"]
    assert sr.sweep(run, now=T0 + dt.timedelta(days=30)) == [] and sim.exists()


def test_changed_sim_is_not_marked(tmp_path):
    run, sim = _run(tmp_path)
    sim.write_bytes(b"TAMPERED")
    assert sr.mark(run, now=T0) == []
    assert "sim digest" in sr.check(run)[0]["reason"]


def test_failed_case_is_ignored(tmp_path):
    run, sim = _run(tmp_path, status="dynamics_failed")
    assert sr.mark(run, now=T0) == [] and sr.check(run) == []


def test_latest_ok_record_wins_so_a_reextraction_can_verify(tmp_path):
    run, _ = _run(tmp_path, channels={"old": 1})
    body = json.dumps({"case_id": "C-1", "channels": {"w5": {"schema": "x"}}})
    (run / "results" / "C-1.reextract.json").write_text(body, encoding="utf-8")
    first = json.loads((run / "ledger.jsonl").read_text(encoding="utf-8").splitlines()[0])
    re = {**first, "event": "reextract", "results_path": "results/C-1.reextract.json",
          "results_sha256": _sha(body.encode())}
    with (run / "ledger.jsonl").open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(re) + "\n")
    assert [m["case_id"] for m in sr.mark(run, now=T0)] == ["C-1"]


def test_marked_sim_that_changes_before_deletion_is_kept(tmp_path):
    run, sim = _run(tmp_path)
    sr.mark(run, now=T0)
    sim.write_bytes(b"NEWER")
    out = sr.sweep(run, now=T0 + dt.timedelta(hours=80))
    assert out == [] and sim.exists()
    assert _events(run)[-1]["event"] == "kept_changed"


def test_loop_ends_when_no_marked_sim_is_left_even_with_an_unverifiable_one(tmp_path, monkeypatch):
    run, sim = _run(tmp_path)
    other = tmp_path / "other"
    _run(other, channels={"old": 1})  # never markable
    clock = iter([T0, T0 + dt.timedelta(hours=73), T0 + dt.timedelta(hours=74)])
    monkeypatch.setattr(sr, "_now", lambda: next(clock))
    monkeypatch.setattr(sr.time, "sleep", lambda s: None)
    assert sr.main(["loop", "--root", str(tmp_path), "--until-empty", "--interval-min", "0"]) == 0
    assert not sim.exists() and (other / "run" / "work" / "C-1" / "C-1.sim").exists()


def test_cycle_finds_run_dirs_and_reports_totals(tmp_path):
    run, sim = _run(tmp_path)
    s1 = sr.cycle([tmp_path], now=T0)
    assert s1["marked"] == 1 and s1["deleted"] == 0 and s1["pending_bytes"] == len(b"SIMDATA")
    assert s1["marked_pending_bytes"] == len(b"SIMDATA")
    s2 = sr.cycle([tmp_path], now=T0 + dt.timedelta(hours=73))
    assert s2["deleted"] == 1 and s2["freed_bytes"] == len(b"SIMDATA") and s2["pending_bytes"] == 0
    assert s2["marked_pending_bytes"] == 0
    assert not sim.exists()
