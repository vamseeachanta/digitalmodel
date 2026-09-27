"""W5 channel extraction: statistics, coincident load rows, T-M hull, completeness, and a solver round trip."""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser.global_model import w5_channels as w5
from digitalmodel.solvers.orcaflex import orcaflex_api
from digitalmodel.solvers.orcaflex import parallel_runner as pr

from .conftest import synthetic_spec
from .test_nonlinear_vessel_foundation import _vessel_motion


def test_stats():
    s = w5.stats([1.0, 3.0, 2.0, 2.0])
    assert s == {"min": 1.0, "max": 3.0, "mean": 2.0, "std": pytest.approx(math.sqrt(0.5)), "n": 4}


def test_compact_rounds_to_significant_digits_recursively():
    out = w5.compact({"a": 123456.789, "b": [0.000123456789, -9.87654321e9], "c": "x", "d": 3, "e": None})
    assert out == {"a": 123457.0, "b": [0.000123457, -9.87654e9], "c": "x", "d": 3, "e": None}


def test_extreme_rows_carry_coincident_values():
    t = [0.0, 0.1, 0.2, 0.3]
    series = {"te": [5.0, 7.0, 4.0, 6.0], "m": [1.0, -3.0, 2.0, 0.5], "p": [9.0, 9.1, 9.2, 9.3]}
    rows = w5.extreme_rows(t, series, drivers=("te", "m"))
    by = {(r["driver"], r["kind"]): r for r in rows}
    assert by[("te", "max")]["t"] == 0.1 and by[("te", "max")]["m"] == -3.0 and by[("te", "max")]["p"] == 9.1
    assert by[("te", "min")]["t"] == 0.2
    assert by[("m", "max")]["t"] == 0.2 and by[("m", "min")]["t"] == 0.1  # signed extremes
    assert len(rows) == 4


def test_convex_hull_of_a_square_with_interior_points():
    pts = [(0, 0), (1, 0), (1, 1), (0, 1), (0.5, 0.5), (0.2, 0.7)]
    assert sorted(w5.convex_hull_indices(pts)) == [0, 1, 2, 3]


def test_tm_hull_rows_contain_the_extremes_and_all_vector_components():
    n = 200
    t = [i * 0.1 for i in range(n)]
    te = [1000.0 + 100.0 * math.sin(2 * math.pi * i / 50) for i in range(n)]
    tw = [x + 50.0 for x in te]
    mx = [200.0 * math.cos(2 * math.pi * i / 50) for i in range(n)]
    my = [0.0] * n
    rows = w5.tm_hull_rows(t, {"te": te, "tw": tw, "mx": mx, "my": my, "pi": [1.0] * n, "po": [2.0] * n})
    # the periodic signal repeats its maximum; one of the equal samples is a hull vertex
    assert max(r["te"] for r in rows) == pytest.approx(max(te))
    assert max(r["mx"] for r in rows) == pytest.approx(max(mx))
    assert all(set(r) >= {"t", "te", "tw", "mx", "my", "m", "pi", "po"} for r in rows)
    assert len(rows) < n / 2  # the hull is a small subset of the samples


def test_missing_channels_reports_absent_required_keys():
    doc = {"schema": w5.SCHEMA, "analysis": "statics", "points": {}, "range_graphs": {}}
    miss = w5.missing_channels(doc, foundation=False)
    assert "points.ufj" in miss and "range_graphs.Riser" in miss and "stroke" in miss
    assert not any(m.startswith("range_graphs.Conductor") for m in miss)
    assert "range_graphs.Conductor" in w5.missing_channels(doc, foundation=True)


# --------------------------------------------------------------------------- solver round trip


@pytest.fixture
def base_spec(tmp_path: Path) -> str:
    d = synthetic_spec().model_dump(mode="json")
    d["vessel_motion"] = _vessel_motion().model_dump(mode="json")
    p = tmp_path / "model-spec.yml"
    p.write_text(yaml.safe_dump({"model": d}, sort_keys=False), encoding="utf-8")
    return str(p)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_campaign_extraction_carries_the_w5_channel_set(base_spec, tmp_path):
    dyn = {"time_step_s": 0.1, "build_up_s": 9.0, "duration_s": 18.0}
    cases = [
        {"case_id": "ST", "analysis": "statics", "params": {"base_spec": base_spec, "offset_pct_wd": 2.0,
                                                            "current": {"depth_speed_m_s": [[0, 0.5], [300, 0.1]]}}},
        {"case_id": "RG", "analysis": "dynamics", "params": {
            "base_spec": base_spec, "heading_deg": 180.0, "regular_wave": {"height_m": 3.0, "period_s": 9.0},
            "dynamics": dyn}},
        {"case_id": "TF", "analysis": "statics", "params": {"base_spec": base_spec, "tensioners_failed": 1}},
    ]
    out = pr.run_cases(cases, adapter="digitalmodel.drilling_riser.campaign:ADAPTER", out_dir=tmp_path, max_workers=3)
    by = {r["case_id"]: r for r in out["results"]}
    assert {k: r["status"] for k, r in by.items()} == {k: "ok" for k in by}, [r.get("message") for r in by.values()]
    docs = {k: json.loads((tmp_path / r["results_path"]).read_text(encoding="utf-8"))["channels"]["w5"]
            for k, r in by.items()}
    for k, d in docs.items():
        assert d["schema"] == w5.SCHEMA
        assert w5.missing_channels(d, foundation=False) == [], k
    st, rg = docs["ST"], docs["RG"]
    # statics: static values; the LFJ angle is non-zero at an offset with current
    assert st["points"]["lfj"]["static"]["ez_angle_deg"] > 0.01
    assert st["points"]["riser_top"]["static"]["te"] > st["points"]["lfj"]["static"]["te"]
    rr = st["range_graphs"]["Riser"]
    # statics: static values, no _min/_max; every array has the length of its own arc-length grid
    for k in ("te", "tw", "m", "vm", "z_m"):
        assert len(rr[rr["axis_of"][k]]) == len(rr[k]) > 10
    # dynamics: statistics and coincident rows, T-M hull at the stack connectors
    p = rg["points"]["lfj"]
    assert p["stats"]["te"]["max"] >= p["stats"]["te"]["mean"] >= p["stats"]["te"]["min"]
    assert any(r["driver"] == "m" for r in p["extremes"])
    stack_pts = [k for k in rg["points"] if k.startswith("stack:")]
    assert stack_pts and all(rg["points"][k]["tm_hull"] for k in stack_pts)
    assert rg["stroke"]["stats"]["max"] >= rg["stroke"]["stats"]["min"]
    assert len(rg["tensioners"]) == 6 and len(docs["TF"]["tensioners"]) == 5
    assert rg["range_graphs"]["Riser"]["te_min"][0] <= rg["range_graphs"]["Riser"]["te_max"][0]
