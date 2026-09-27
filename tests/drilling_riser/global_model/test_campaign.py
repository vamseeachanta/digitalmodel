"""Riser campaign adapter: case -> spec, seeded statics, timing proxies, runner round trip."""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser import campaign as cp
from digitalmodel.solvers.orcaflex import orcaflex_api
from digitalmodel.solvers.orcaflex import parallel_runner as pr

from .conftest import synthetic_spec
from .test_nonlinear_vessel_foundation import _vessel_motion


@pytest.fixture
def base_spec(tmp_path: Path) -> str:
    d = synthetic_spec().model_dump(mode="json")
    d["vessel_motion"] = _vessel_motion().model_dump(mode="json")
    p = tmp_path / "model-spec.yml"
    p.write_text(yaml.safe_dump({"model": d}, sort_keys=False), encoding="utf-8")
    return str(p)


def _case(base, analysis="statics", **params):
    return {"case_id": "C-1", "analysis": analysis, "params": {"base_spec": base, "statics": "direct", **params}}


def test_offset_is_along_the_heading(base_spec):
    s = cp.case_spec(_case(base_spec, heading_deg=90.0, offset_pct_wd=2.0))
    wd = s.environment.water_depth_m
    assert s.vessel_offset_m[0] == pytest.approx(0.0, abs=1e-9)
    assert s.vessel_offset_m[1] == pytest.approx(0.02 * wd)


def test_case_sets_environment_tension_and_mud(base_spec):
    c = _case(base_spec, "dynamics", heading_deg=180.0, top_tension_n=3.1e6, mud_density_kg_m3=1500.0,
              current={"depth_speed_m_s": [[0, 0.3], [75, 0.3], [76, 0.0]]},
              irregular_wave={"hs_m": 4.2, "tp_s": 9.0, "gamma": 2.0, "seed": 7},
              dynamics={"time_step_s": 0.1, "build_up_s": 10.0, "duration_s": 20.0})
    s = cp.case_spec(c)
    assert s.tensioners.total_vertical_tension_n == 3.1e6
    assert s.contents.density_kg_m3 == 1500.0
    assert s.current.direction_deg == 180.0 and s.irregular_wave.direction_deg == 180.0
    assert s.irregular_wave.seed == 7 and s.regular_wave is None
    assert s.dynamics.duration_s == 20.0


class _Obj:
    def __init__(self, typ="", **res):
        self.typeName, self._res = typ, res

    def StaticResult(self, name, *args):
        return self._res[name]


class _Model(dict):
    objects: list = []


def _fake(base_spec, *, yaw=0.0, tv_factor=1.0, balance_n=0.0):
    """A solved-model stand-in: ring yaw, tensioner vertical (factor on the target) and the ring balance."""
    from digitalmodel.drilling_riser.global_model.hand_checks import tension_references

    spec = cp.case_spec(_case(base_spec))
    t = spec.tensioners.total_vertical_tension_n * tv_factor
    w = tension_references(spec)["ring_weight_n"]
    m = _Model(TensionRing=_Obj(**{"Rotation 3": yaw, "Wetted volume": 0.0}),
               # balance = tensioner vertical - ring weight + riser end-A GZ force - inner-barrel end-B GZ force
               Riser=_Obj(**{"End GZ force": (w - t + balance_n) / 1000.0}),
               InnerBarrel=_Obj(**{"End GZ force": 0.0}))
    return m, spec, t


@pytest.mark.parametrize(("kw", "trips"), [
    ({}, None),
    ({"yaw": 180.0}, "yaw"),
    ({"yaw": -359.6}, None),
    ({"tv_factor": 5682.0 / 6026.0}, "tensioner vertical"),
    ({"balance_n": 345e3}, "balance"),
])
def test_physical_state_checks_trip_on_the_yawed_branch(base_spec, monkeypatch, kw, trips):
    m, spec, t = _fake(base_spec, **kw)
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: t)
    monkeypatch.setattr(cp, "_end_gz_n", lambda model, line, end: model[line].StaticResult("End GZ force") * 1000.0)
    if trips is None:
        info = cp.physical_state_checks(m, spec)
        assert abs(info["ring_yaw_deg"]) < 1.0
    else:
        with pytest.raises(pr.CaseFailed) as e:
            cp.physical_state_checks(m, spec)
        assert e.value.status == "nonphysical_static" and trips in str(e.value)


def test_statics_case_carries_no_wave_or_dynamics_from_the_case(base_spec):
    s = cp.case_spec(_case(base_spec))
    assert s.regular_wave is None and s.irregular_wave is None and s.current is None


solver = [pytest.mark.solver, pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")]


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_seeded_statics_matches_direct_statics_on_the_fixed_base_model(base_spec, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model

    spec = cp.case_spec(_case(base_spec, heading_deg=0.0, offset_pct_wd=1.0))
    m1 = orun.load_and_solve_statics(write_model(spec, tmp_path / "a") / "master.yml")
    m2 = orun.load_model(write_model(spec, tmp_path / "b") / "master.yml")
    info = cp.seeded_statics(m2, spec)
    assert info["steps"] == 7  # -2 % -> +1 % WD in 0.5 % steps
    # the global-force ring balance closes at an offset (the effective-tension form does not)
    chk = cp.physical_state_checks(m2, spec)
    assert abs(chk["ring_balance_n"]) < 1e-4 * spec.tensioners.total_vertical_tension_n
    assert orun.end_effective_tensions(m2)["riser_top_n"] == pytest.approx(
        orun.end_effective_tensions(m1)["riser_top_n"], rel=1e-5)
    assert m2["Vessel"].InitialX == pytest.approx(0.01 * spec.environment.water_depth_m)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_statics_settings_change_the_path_not_the_static_state(base_spec, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model

    plain = _case(base_spec, heading_deg=0.0, offset_pct_wd=1.0, statics="seeded")
    tuned = _case(base_spec, heading_deg=0.0, offset_pct_wd=1.0, statics="seeded", statics_max_iterations=1000,
                  statics_damping=[5, 50], statics_step_pct=0.25)
    ad = cp.RiserCampaignAdapter()
    te = []
    for i, c in enumerate((plain, tuned)):
        m = orun.load_model(ad.build(c, tmp_path / str(i)))
        info = ad.statics(m, c)
        te.append(orun.end_effective_tensions(m)["riser_bottom_n"])
    assert info["statics_max_iterations"] == 1000 and info["statics_damping"] == [5.0, 50.0]
    # still water: aiding current -2 % -> +1 % WD in 0.25 % steps (13 solves), then the current removed (1 solve)
    assert info["steps"] == 14 and info["strategy"] == "aid_current"
    assert te[1] == pytest.approx(te[0], rel=1e-5)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_runner_with_campaign_adapter_statics_modal_regular_and_proxies(base_spec, tmp_path):
    dyn = {"time_step_s": 0.1, "build_up_s": 9.0, "duration_s": 18.0}
    cases = [
        {"case_id": "ST", "analysis": "statics", "params": {"base_spec": base_spec, "modal_modes": 2}},
        {"case_id": "RG", "analysis": "dynamics", "params": {
            "base_spec": base_spec, "heading_deg": 180.0, "regular_wave": {"height_m": 3.0, "period_s": 9.0},
            "dynamics": dyn}},
        {"case_id": "IR", "analysis": "dynamics", "params": {
            "base_spec": base_spec, "heading_deg": 180.0, "dynamics": dyn,
            "irregular_wave": {"hs_m": 2.0, "tp_s": 9.0, "gamma": 2.0, "seed": 3}}},
        {"case_id": "DO", "analysis": "dynamics", "params": {
            "base_spec": base_spec, "heading_deg": 0.0, "dynamics": dyn,
            "proxy": {"kind": "drift_off", "speed_change_m_s": 0.5}}},
        {"case_id": "RC", "analysis": "dynamics", "params": {
            "base_spec": base_spec, "heading_deg": 0.0, "dynamics": {**dyn, "time_step_s": 0.02},
            "proxy": {"kind": "disconnect"}}},
    ]
    out = pr.run_cases(cases, adapter="digitalmodel.drilling_riser.campaign:ADAPTER", out_dir=tmp_path,
                       max_workers=5)
    by = {r["case_id"]: r for r in out["results"]}
    assert {k: r["status"] for k, r in by.items()} == {k: "ok" for k in by}, [r.get("message") for r in by.values()]
    ch = {k: json.loads((tmp_path / r["results_path"]).read_text(encoding="utf-8"))["channels"] for k, r in by.items()}
    assert len(ch["ST"]["modes"]) == 2
    assert ch["RG"]["te_top_max_n"] > ch["RG"]["te_top_min_n"]
    # drift-off proxy: uniform acceleration to 0.5 m/s over 18 s -> 4.5 m travelled along +x
    assert ch["DO"]["series"]["vessel_x_m"]["max"] == pytest.approx(4.5, abs=0.3)
    # disconnect proxy: the released riser rises
    assert ch["RC"]["series"]["ring_z_m"]["max"] > ch["RC"]["series"]["ring_z_m"]["min"]
    assert all(math.isfinite(r["timings_s"]["total"]) for r in by.values())
