"""Drift-off / drive-off and recoil event models of the drilling riser (digitalmodel-data #13, W414): the vessel on a
prescribed drift trajectory (first-order RAO motion superimposed), the regular wave at a set phase, and the EDS
disconnect at the LMRP connector with the anti-recoil tension schedule. Synthetic riser only."""

from __future__ import annotations

import pytest
import yaml

from digitalmodel.drilling_riser.global_model import events as ev
from digitalmodel.drilling_riser.global_model.build import build_generic_spec
from digitalmodel.drilling_riser.global_model.hand_checks import tension_references
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec

TRAJ = {"t_s": [0.0, 10.0, 20.0], "x_m": [0.0, 1.0, 4.0], "y_m": [0.0, 0.5, 2.0]}


def _case(tmp_path, **params):
    p = tmp_path / "spec.yml"
    p.write_text(yaml.safe_dump({"model": synthetic_spec().model_dump(mode="json")}), encoding="utf-8")
    return {"case_id": "EV-1", "analysis": params.pop("analysis", "dynamics"), "params": {"base_spec": str(p), **params}}


DYN = {"time_step_s": 0.05, "build_up_s": 10.0, "duration_s": 20.0}


def test_vessel_trajectory_is_a_primary_time_history_from_the_offset(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    s = cp.case_spec(_case(tmp_path, offset_pct_wd=1.0, vessel_trajectory=TRAJ, dynamics=DYN))
    v = build_generic_spec(s)["generic"]["vessels"][0]["properties"]
    assert v["PrimaryMotion"] == "Time history" and v["PrimaryTimeHistoryDataSource"] == "Internal"
    rows = v[ev.TIME_HISTORY_KEY]
    ox, oy = s.vessel_offset_m
    assert rows[0][:3] == [-10.0, ox, oy]  # held at the offset through the build-up
    assert rows[-1][:3] == [20.0, ox + 4.0, oy + 2.0]
    assert all(r[3:] == [0, 0, 0, 0] for r in rows)


def test_wave_phase_sets_the_regular_wave_time_origin(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    s = cp.case_spec(_case(tmp_path, regular_wave={"height_m": 4.0, "period_s": 10.0}, wave_phase_deg=72.0,
                           dynamics=DYN))
    assert s.wave_time_origin_s == pytest.approx(2.0)
    env = build_generic_spec(s)["environment"]
    train = env["raw_properties"]["WaveTrains"][0]
    assert train["WaveType"] == "Airy" and train["WaveTimeOrigin"] == pytest.approx(2.0)
    assert train["WaveHeight"] == 4.0 and train["WavePeriod"] == 10.0


def test_recoil_splits_the_lmrp_off_the_bop_and_steps_the_tensioners(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    s = cp.case_spec(_case(tmp_path, recoil={"closure_s": 2.5, "step_s": 0.5, "open_fraction": 0.0},
                           regular_wave={"height_m": 3.0, "period_s": 9.0}, dynamics=DYN))
    t0 = s.tensioners.total_vertical_tension_n
    hold = ev.recoil_hold_tension_n(s)
    assert s.recoil.stages[0]["tension_n"] == pytest.approx(t0 + (hold - t0) * 0.1)
    assert s.recoil.stages[-1]["tension_n"] == pytest.approx(hold)
    g = build_generic_spec(s)
    lines = {x["name"]: x for x in g["generic"]["lines"]}
    assert set(lines) >= {"Riser", ev.LMRP_LINE, "Stack"}
    conn = lines[ev.LMRP_LINE]["properties"]["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                                              "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
                                              "ConnectionzRelativeTo"]
    assert conn[0][0] == "Stack" and conn[0][7] == 1  # the LMRP unlatches at the start of stage 1
    assert lines["Riser"]["properties"]["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                                        "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
                                        "ConnectionzRelativeTo"][1][0] == ev.LMRP_LINE
    assert [x for x in lines["Stack"]["line_type_refs"]] == [x.name for x in reversed(s.stack[1:])]
    stages = g["simulation"]["stages"]
    assert stages[0] == 10.0 and stages[1:6] == [0.5] * 5 and sum(stages[1:]) == pytest.approx(20.0)
    w = g["generic"]["winches"][0]["properties"]["StageMode, StageValue"]
    assert len(w) == len(stages) + 1
    ratio = [row[1] / w[0][1] for row in w]
    assert ratio[:2] == [1.0, 1.0] and ratio[2] == pytest.approx(s.recoil.stages[0]["tension_n"] / t0)
    assert ratio[-1] == pytest.approx(hold / t0)


def test_recoil_hold_tension_is_the_released_weight():
    s = synthetic_spec()
    ref = tension_references(s)
    lmrp = s.stack[0]
    released = ref["riser_top_n"] - ref["riser_bottom_n"] + ref["ring_weight_n"]
    chain = ref["stack_chain"][0]
    assert chain["name"] == lmrp.name
    assert ev.recoil_hold_tension_n(s) == pytest.approx(released + chain["submerged_weight_n"])


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_vessel_follows_the_prescribed_trajectory(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    case = _case(tmp_path, vessel_trajectory=TRAJ, dynamics=DYN)
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "m"))
    cp.ADAPTER.statics(m, case)
    orun.run_dynamics(m)
    ofx = orun._api()
    x = m["Vessel"].TimeHistory("X", ofx.SpecifiedPeriod(19.9, 20.0))
    assert float(x[-1]) == pytest.approx(4.0, abs=0.05)
    out = cp.ADAPTER.extract(m, case)
    assert "event_series" in out["w5"]


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_recoil_lifts_the_lmrp_off_the_bop(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    case = _case(tmp_path, recoil={"closure_s": 2.5, "step_s": 0.5, "open_fraction": 1 / 6},
                 dynamics={"time_step_s": 0.02, "build_up_s": 5.0, "duration_s": 15.0})
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "m"))
    cp.ADAPTER.statics(m, case)
    orun.run_dynamics(m)
    out = cp.ADAPTER.extract(m, case)
    es = out["w5"]["event_series"]
    assert es["t"][0] == 0.0 and es["t"][-1] == 15.0  # the whole main stage, over every anti-recoil stage
    assert out["w5"]["time"]["end_s"] == 15.0
    rc = out["w5"]["recoil"]
    assert rc["lmrp_lift_max_m"] > 0.0  # a failed valve leaves net upward force: the LMRP lifts off
    assert "clearance_min_after_release_m" in rc


def test_recoil_hold_factor_scales_the_hold_tension(tmp_path):
    """A hold below the released weight (0.98 x, as the C2 EDP W2B2) lets the string settle; the model has no orifice
    damping, so at exactly the released weight it would coast upward."""
    from digitalmodel.drilling_riser import campaign as cp

    s = cp.case_spec(_case(tmp_path, recoil={"hold_factor": 0.98}, dynamics=DYN))
    assert s.recoil.stages[-1]["tension_n"] == pytest.approx(0.98 * ev.recoil_hold_tension_n(s))


def test_calibrated_line_tension_keeps_the_anti_recoil_schedule(monkeypatch):
    """The tensioner setting of a model variant (calibration) scales every stage; the recoil stages keep their ratio
    to the pre-disconnect tension (event gate G-RC1 found the schedule overwritten: ring rise at twice the closed form)."""
    from digitalmodel.drilling_riser import campaign as cp

    class W:
        def __init__(self, vals):
            self.vals = list(vals)

        def GetDataRowCount(self, name):
            return len(self.vals)

        def GetData(self, name, i):
            return self.vals[i]

        def SetData(self, name, i, v):
            self.vals[i] = v

    ws = [W([100.0, 100.0, 90.0, 80.0]), W([100.0, 100.0, 90.0, 80.0])]
    monkeypatch.setattr(cp, "_ring_tensioners", lambda model: ws)
    cp.set_line_tension(None, 150.0e3)
    for w in ws:
        assert w.vals == pytest.approx([150.0, 150.0, 135.0, 120.0])
