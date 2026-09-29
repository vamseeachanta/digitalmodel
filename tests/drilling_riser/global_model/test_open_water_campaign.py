"""C2 open-water riser on the W4 runner (digitalmodel-data #13, owner decision W415): the W5 channel set of an
open-water case (W403: everything W5 needs is extracted before the .sim is deleted), statics routes and the case
settings the C2 matrix groups use (synthetic riser, no project data)."""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.global_model import w5_channels as w5
from digitalmodel.solvers.orcaflex import orcaflex_api

from .test_open_water import _ow_case, open_water_dict

solver = pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")


def _doc(analysis="statics", **drop):
    pts = {k: {"line": "x", ("static" if analysis == "statics" else "stats"): {"te": 1.0, "ez_angle_deg": 0.1},
               **({"tm_hull": [{"te": 1.0, "m": 2.0}]} if analysis == "dynamics" else {})}
           for k in ("frame", "rotary", "edp", "stack:datum", "stack:LRP|Tree", "stack:top")}
    doc = {"schema": w5.SCHEMA, "riser_kind": "open_water", "analysis": analysis, "points": pts,
           "range_graphs": {k: {} for k in ("Upper", "Riser", "Stack")}, "frame": {}, "top_tension": {}, "stroke": {}}
    for k in drop:
        doc.pop(k, None)
    return doc


def test_open_water_channel_set_is_complete_without_the_drilling_keys():
    assert w5.missing_channels(_doc(), foundation=False) == []
    assert w5.missing_channels(_doc("dynamics"), foundation=False) == []


def test_open_water_channel_set_names_what_is_missing():
    d = _doc()
    del d["points"]["edp"]
    d["range_graphs"].pop("Upper")
    miss = w5.missing_channels(_doc(top_tension=True), foundation=True)
    assert "top_tension" in miss and "range_graphs.Conductor" in miss
    miss = w5.missing_channels(d, foundation=False)
    assert "points.edp" in miss and "range_graphs.Upper" in miss


def test_open_water_points_cover_frame_rotary_riser_boundaries_edp_and_stack():
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    s = OpenWaterRiserSpec.model_validate(open_water_dict())
    pts = w5.open_water_points(s)
    names = [p[0] for p in pts]
    assert names[:2] == ["frame", "rotary"]
    assert "edp" in names and "stack:datum" in names and "stack:top" in names
    assert "riser:Stress joint 4|EDP" in names  # the stress-joint base
    arcs = {p[0]: p[2] for p in pts if p[1] == "Riser" and p[0].startswith("riser:")}
    assert arcs["riser:Stress joint 4|EDP"] == pytest.approx(s.stress_joint_base_arc_m)
    assert len([n for n in names if n.startswith("riser:")]) == len(s.riser) - 1


def test_stack_stiffness_factor_applies_to_open_water_stack_sections(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    base = cp.case_spec(_ow_case(tmp_path))
    s = cp.case_spec(_ow_case(tmp_path, section_stiffness_factors={"LRP": 0.5, "Tree": 0.5}))
    for name in ("LRP", "Tree"):
        a = next(x for x in base.stack if x.name == name)
        b = next(x for x in s.stack if x.name == name)
        assert b.ei_nm2 == pytest.approx(0.5 * a.ei_nm2) and b.ea_n == pytest.approx(0.5 * a.ea_n)
    with pytest.raises(ValueError, match="no section named"):
        cp.case_spec(_ow_case(tmp_path, section_stiffness_factors={"BOP": 2.0}))


def test_open_water_statics_falls_back_to_frame_continuation(monkeypatch, tmp_path):
    """Direct statics first; when it does not converge the model is reloaded and the offset reached by a
    continuation from the straight (zero-offset) start, then with half steps."""
    from digitalmodel.drilling_riser import campaign as cp

    calls = []

    class Fake:
        threadCount = 1

        def __init__(self):
            self.general = type("G", (), {})()
            self.fail = 1

        def LoadData(self, path):
            calls.append(("load", path))

        def CalculateStatics(self):
            calls.append(("statics",))
            if self.fail:
                self.fail -= 1
                raise RuntimeError("Whole system statics: 1000 (latest error = 12.3)")

    monkeypatch.setattr(cp, "physical_state_checks", lambda model, spec: {"physical": True})
    monkeypatch.setattr(cp, "seeded_statics", lambda model, spec, **kw: calls.append(("seeded", kw)) or
                        {"method": "seeded continuation", "steps": 3})
    case = _ow_case(tmp_path, heading_deg=0.0, offset_pct_wd=4.0)
    ad = cp.RiserCampaignAdapter()
    ad.build(case, tmp_path / "m")
    info = ad.statics(Fake(), case)
    assert info["strategy"] == "frame_continuation" and info["physical"]
    assert [c[0] for c in calls] == ["statics", "load", "seeded"]
    assert calls[-1][1]["start"] == (0.0, 0.0)
    assert info["attempts"][0]["strategy"] == "direct"


def test_open_water_seeded_statics_request_uses_the_frame_continuation(monkeypatch, tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    monkeypatch.setattr(cp, "physical_state_checks", lambda model, spec: {"physical": True})
    seen = []
    monkeypatch.setattr(cp, "seeded_statics", lambda model, spec, **kw: seen.append(kw) or {"method": "seeded continuation",
                                                                                          "steps": 11})
    monkeypatch.setattr(cp, "robust_statics", lambda *a, **k: pytest.fail("drilling-riser paths on an open-water case"))
    case = _ow_case(tmp_path, heading_deg=0.0, offset_pct_wd=3.0, statics="seeded")
    ad = cp.RiserCampaignAdapter()
    ad.spec(case)

    class M:
        general = type("G", (), {})()

    info = ad.statics(M(), case)
    assert info["method"] == "seeded continuation" and info["steps"] == 11 and seen and "start" not in seen[0]


@pytest.mark.solver
@solver
@pytest.mark.parametrize("analysis", ["statics", "dynamics"])
def test_open_water_case_extracts_a_complete_w5_channel_set(tmp_path, analysis):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    extra = {"regular_wave": {"height_m": 3.0, "period_s": 8.0},
             "dynamics": {"time_step_s": 0.02, "build_up_s": 8.0, "duration_s": 16.0}} if analysis == "dynamics" else {}
    case = _ow_case(tmp_path, analysis=analysis, heading_deg=0.0, offset_pct_wd=1.0,
                    current={"depth_speed_m_s": [[0.0, 0.5], [280.0, 0.2]]}, **extra)
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "m"))
    cp.ADAPTER.statics(m, case)
    if analysis == "dynamics":
        orun.run_dynamics(m)
    out = cp.ADAPTER.extract(m, case)
    doc = out["w5"]
    assert doc["riser_kind"] == "open_water"
    assert w5.missing_channels(doc, foundation=False) == []
    src = "static" if analysis == "statics" else "stats"
    te_edp = doc["points"]["edp"][src]["te"]
    assert (te_edp if analysis == "statics" else te_edp["mean"]) > 0
    if analysis == "dynamics":
        assert doc["points"]["edp"]["tm_hull"] and doc["stroke"]["stats"]["n"] > 10


def test_open_water_zero_offset_statics_fall_back_to_the_seeded_continuation(monkeypatch, tmp_path):
    """Batch 4: at zero offset the straight start is the target, so the frame continuation from (0, 0) is the direct
    solve again; C2-R1 flowing cases at 0 % WD failed every route. The continuation from the -2 % WD seed converges."""
    from digitalmodel.drilling_riser import campaign as cp

    calls = []

    class Fake:
        threadCount = 1

        def __init__(self):
            self.general = type("G", (), {})()

        def LoadData(self, path):
            calls.append(("load",))

        def CalculateStatics(self):
            calls.append(("statics",))
            raise RuntimeError("Whole system statics: Not converged.")

    def seeded(model, spec, **kw):
        calls.append(("seeded", kw))
        if "start" in kw:  # the continuation from the straight start at (0, 0) is the failing direct solve
            raise RuntimeError("Whole system statics: Not converged.")
        return {"method": "seeded continuation", "steps": 5}

    monkeypatch.setattr(cp, "physical_state_checks", lambda model, spec: {"physical": True})
    monkeypatch.setattr(cp, "seeded_statics", seeded)
    case = _ow_case(tmp_path, heading_deg=0.0, offset_pct_wd=0.0)
    ad = cp.RiserCampaignAdapter()
    ad.build(case, tmp_path / "m")
    info = ad.statics(Fake(), case)
    assert info["strategy"] == "seed_continuation" and info["physical"]
    seeded_calls = [c for c in calls if c[0] == "seeded"]
    assert "start" not in seeded_calls[-1][1]
