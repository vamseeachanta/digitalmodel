"""Aggregator: results by case, SHA-256 verified against the run ledger, latest record per case, grouping."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.postprocess.aggregate import (
    DigestMismatch,
    case_key,
    collect,
    load_verified,
    seed_groups,
)


def _mc(case_id, **kw):
    base = {"case_id": case_id, "group": case_id.rsplit("-", 1)[0], "configuration": "cfg-a", "metocean": "MET-A",
            "heading_deg": 45, "offset_pct_wd": -6, "mud_weight_ppg": 12.5, "top_tension": "TT-A",
            "tensioner": "intact", "purpose": "screening", "mode": "connected drilling; connected non-drilling"}
    base.update(kw)
    return base


def _run(root: Path, name: str, cases: list[dict], records: list[dict]) -> Path:
    d = root / name
    (d / "results").mkdir(parents=True)
    (d / "run.json").write_text(json.dumps({"cases": cases}), encoding="utf-8")
    lines = []
    for rec in records:
        if rec.get("status") == "ok":
            body = json.dumps({"case_id": rec["case_id"], "channels": {"w5": {"x": rec.get("x", 1)}}}).encode()
            (d / "results" / f"{rec['case_id']}.json").write_bytes(body)
            rec = {"results_path": f"results/{rec['case_id']}.json",
                   "results_sha256": rec.get("sha", hashlib.sha256(body).hexdigest()), **rec}
        lines.append(json.dumps(rec))
    (d / "ledger.jsonl").write_text("\n".join(lines) + "\n", encoding="utf-8")
    return d


def _case(cid, mc=None, **params):
    return {"case_id": cid, "analysis": "statics", "run_type": "static", "matrix_case": mc or _mc(cid),
            "params": {"top_tension_n": 1.0, **params}}


def test_latest_record_per_case_wins_across_runs(tmp_path):
    a = _run(tmp_path, "part1", [_case("G-00001"), _case("G-00002")],
             [{"case_id": "G-00001", "status": "ok", "finished_utc": "2026-01-01T00:00:00Z", "x": 1},
              {"case_id": "G-00002", "status": "statics_diverged", "finished_utc": "2026-01-01T00:00:00Z"}])
    b = _run(tmp_path, "part2", [_case("G-00001"), _case("G-00002")],
             [{"case_id": "G-00001", "status": "ok", "finished_utc": "2026-01-02T00:00:00Z", "x": 2},
              {"case_id": "G-00002", "status": "ok", "finished_utc": "2026-01-02T00:00:00Z", "x": 3}])
    recs, issues = collect([a, b])
    assert sorted(recs) == ["G-00001", "G-00002"]
    assert recs["G-00001"].run == "part2"
    assert load_verified(recs["G-00001"])["channels"]["w5"]["x"] == 2
    assert issues == []


def test_a_case_whose_latest_record_failed_has_no_result_and_is_reported(tmp_path):
    a = _run(tmp_path, "p1", [_case("G-00001")],
             [{"case_id": "G-00001", "status": "ok", "finished_utc": "2026-01-01T00:00:00Z"}])
    b = _run(tmp_path, "p2", [_case("G-00001")],
             [{"case_id": "G-00001", "status": "statics_diverged", "finished_utc": "2026-01-03T00:00:00Z",
               "message": "not converged"}])
    recs, issues = collect([a, b])
    assert recs == {}
    assert issues == [{"case_id": "G-00001", "status": "statics_diverged", "run": "p2", "message": "not converged"}]


def test_digest_mismatch_is_refused(tmp_path):
    a = _run(tmp_path, "p1", [_case("G-00001")],
             [{"case_id": "G-00001", "status": "ok", "finished_utc": "2026-01-01T00:00:00Z", "sha": "0" * 64}])
    recs, _ = collect([a])
    with pytest.raises(DigestMismatch):
        load_verified(recs["G-00001"])


def test_record_carries_matrix_case_params_and_seed(tmp_path):
    cases = [_case(f"I-00001-S{s:02d}", _mc("I-00001", group="I", seed=s), top_tension_n=5.0) for s in (1, 2)]
    a = _run(tmp_path, "p1", cases,
             [{"case_id": c["case_id"], "status": "ok", "finished_utc": "2026-01-01T00:00:00Z"} for c in cases])
    recs, _ = collect([a])
    r = recs["I-00001-S02"]
    assert (r.base_id, r.seed, r.group) == ("I-00001", 2, "I")
    assert r.params["top_tension_n"] == 5.0
    groups = seed_groups(recs.values())
    assert list(groups) == ["I-00001"]
    assert [x.seed for x in groups["I-00001"]] == [1, 2]


def test_case_key_groups_by_mode_configuration_metocean_heading_offset_mud_tension():
    k = case_key(_mc("G-00001"))
    assert k == {"mode": "connected drilling; connected non-drilling", "configuration": "cfg-a", "metocean": "MET-A",
                 "heading_deg": 45, "offset_pct_wd": -6, "mud_weight_ppg": 12.5, "top_tension": "TT-A",
                 "tensioner": "intact"}
