"""Hang-off and running topology of the drilling-riser global model (digitalmodel-data #13, owner decision W414
'hangoff_fatigue_then_events'): the riser disconnected at the LMRP connector and hung off - hard (telescopic joint
locked, the string on the vessel at the upper flex joint, no tensioners) or soft (the string on the tensioners, here a
vertical gas-spring about mid-stroke) - with or without the LMRP; the running string hung off at a deployed length
with its payload. Synthetic riser only."""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.global_model import hang_off as ho
from digitalmodel.drilling_riser.global_model.build import build_generic_spec
from digitalmodel.drilling_riser.global_model.hand_checks import effective_tension_chain, ring_weight_n
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec

G = 9.80665
def _seawater(spec):
    d = spec.model_dump()
    d["contents"]["density_kg_m3"] = spec.environment.water_density_kg_m3
    return type(spec).model_validate(d)


# ---------------------------------------------------------------- spec transforms
def test_hard_hang_off_with_lmrp_keeps_the_lmrp_below_the_lfj_and_drops_the_rest():
    s = synthetic_spec()
    h = ho.hang_off_spec(s, "hard", with_lmrp=True)
    assert [x.name for x in h.stack] == ["LMRP"]
    assert h.foundation is None and h.hang_off.mode == "hard" and h.hang_off.with_lmrp
    assert h.wellhead_datum_z_m == pytest.approx(s.lower_flex_joint.pivot_z_m - s.stack[0].length_m)
    assert [x.name for x in h.riser] == [x.name for x in s.riser]


def test_hang_off_without_lmrp_ends_the_string_at_the_riser_adaptor():
    s = synthetic_spec()
    h = ho.hang_off_spec(s, "hard", with_lmrp=False)
    assert "LFJ upper body" not in [x.name for x in h.riser]
    assert h.lower_flex_joint.pivot_z_m == pytest.approx(s.lower_flex_joint.pivot_z_m + s.riser[-1].length_m)
    assert not h.hang_off.with_lmrp


def test_soft_hang_off_spring_carries_the_hung_weight_at_mid_stroke():
    s = _seawater(synthetic_spec())
    h = ho.hang_off_spec(s, "soft", with_lmrp=True, stiffness_fraction=0.10, half_stroke_m=7.12)
    rho_w = s.environment.water_density_kg_m3
    chain = effective_tension_chain([*h.riser, *h.stack], top_z_m=s.tension_ring.z_static_m, top_tension_n=0.0,
                                    rho_water=rho_w, rho_contents=rho_w)
    w = ring_weight_n(s) - chain[-1]["te_bottom_n"]
    assert ho.hung_weight_n(h) == pytest.approx(w)
    assert h.hang_off.spring_tension_n == pytest.approx(w)
    assert h.hang_off.spring_stiffness_n_per_m == pytest.approx(0.10 * w / 7.12)


def test_running_string_trims_the_upper_joints_to_the_deployed_length():
    """Deployed length = the fraction of the riser joints run (the upper joints are run last); the outer barrel and
    the lower flex-joint body stay; the payload hangs below the lower flex joint."""
    s = synthetic_spec()
    mid = sum(x.length_m for x in s.riser[1:-1])
    r = ho.running_spec(s, deployed_pct_wd=50, payload="BOP + LMRP")
    assert [x.name for x in r.stack] == ["LMRP", "BOP"]
    assert r.riser[0].name == s.riser[0].name and r.riser[-1].name == s.riser[-1].name  # outer barrel, LFJ body kept
    assert sum(x.length_m for x in r.riser[1:-1]) == pytest.approx(0.5 * mid)
    assert [x.name for x in r.riser[1:-1]][-1] == s.riser[-2].name  # the lowest joints are deployed first
    assert r.wellhead_datum_z_m == pytest.approx(r.lower_flex_joint.pivot_z_m - s.stack[0].length_m - s.stack[1].length_m)
    assert r.hang_off.mode == "hard" and r.hang_off.with_lmrp
    lm = ho.running_spec(s, deployed_pct_wd=100, payload="LMRP only")
    assert [x.name for x in lm.stack] == ["LMRP"]
    assert lm.lower_flex_joint.pivot_z_m == pytest.approx(s.lower_flex_joint.pivot_z_m)
    with pytest.raises(ValueError, match="deployed"):
        ho.running_spec(s, deployed_pct_wd=150, payload="LMRP only")


# ---------------------------------------------------------------- generic spec
def _generic(h):
    g = build_generic_spec(h)["generic"]
    return g, {x["name"]: x for x in g["lines"]}, {x["name"]: x for x in g["constraints"]}


def test_hard_hang_off_objects():
    g, lines, cons = _generic(ho.hang_off_spec(synthetic_spec(), "hard", with_lmrp=True))
    assert g["winches"] == [] and "links" not in g
    assert "Conductor" not in lines
    slip = cons["SlipJoint"]["properties"]["DOFFree, DOFInitialValue"]
    assert slip[2] == [False]  # telescopic joint locked
    ends = lines["Stack"]["properties"][ho.CONN_KEY]
    assert ends[0][0] == "Free" and ends[1][0] == "Free"  # the LMRP hangs free below the LFJ
    ib = lines["InnerBarrel"]["properties"]
    assert ib["IncludeTorsion"] is True  # the ring yaw is held from the vessel through the locked joint


def test_soft_hang_off_objects():
    h = ho.hang_off_spec(_seawater(synthetic_spec()), "soft", with_lmrp=False)
    g, lines, cons = _generic(h)
    assert g["winches"] == []
    assert "Stack" not in lines
    assert lines["Riser"]["properties"][ho.CONN_KEY][1][0] == "Free"
    assert cons["SlipJoint"]["properties"]["DOFFree, DOFInitialValue"][2][0] is True  # strokes on the spring
    link = next(x for x in g["links"] if x["name"] == ho.SPRING)
    table = link["properties"]["SpringLength, SpringTension"]
    k = h.hang_off.spring_stiffness_n_per_m / 1000.0
    (l0, t0), (l1, t1) = table[0], table[-1]
    assert (t1 - t0) / (l1 - l0) == pytest.approx(k)
    # force at the nominal length (anchor height above the static ring) equals the hung weight
    t_nom = t0 + k * (ho.SPRING_ANCHOR_HEIGHT_M - l0)
    assert t_nom == pytest.approx(h.hang_off.spring_tension_n / 1000.0)


# ---------------------------------------------------------------- campaign cases
def _case(tmp_path, **params):
    import yaml

    p = tmp_path / "spec.yml"
    p.write_text(yaml.safe_dump({"model": synthetic_spec().model_dump(mode="json")}), encoding="utf-8")
    return {"case_id": "HO-1", "analysis": params.pop("analysis", "statics"), "params": {"base_spec": str(p), **params}}


def test_campaign_case_builds_hang_off_and_running_and_damping(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    s = cp.case_spec(_case(tmp_path, hang_off={"mode": "soft", "with_lmrp": True}, mud_density_kg_m3=1025.0))
    assert s.hang_off.mode == "soft" and s.contents.density_kg_m3 == 1025.0
    r = cp.case_spec(_case(tmp_path, running={"deployed_pct_wd": 25, "payload": "LMRP only"}))
    assert r.hang_off.mode == "hard" and r.wellhead_datum_z_m == pytest.approx(r.lower_flex_joint.pivot_z_m - r.stack[0].length_m)
    base = synthetic_spec()
    d = base.model_dump()
    d["structural_damping"] = {"ratio_percent": 0.3, "period_s": 10.0}
    import yaml

    p = tmp_path / "damped.yml"
    p.write_text(yaml.safe_dump({"model": type(base).model_validate(d).model_dump(mode="json")}), encoding="utf-8")
    c = cp.case_spec({"case_id": "D", "analysis": "statics", "params": {"base_spec": str(p), "structural_damping_pct": 0.6}})
    assert c.structural_damping.ratio_percent == 0.6 and c.structural_damping.period_s == 10.0
    with pytest.raises(ValueError, match="structural damping"):
        cp.case_spec(_case(tmp_path, structural_damping_pct=0.6))
    with pytest.raises(ValueError, match="hang-off"):
        cp.case_spec(_case(tmp_path, hang_off={"mode": "hard"}, tensioners_failed=1))


def test_hang_off_w5_channel_set_completeness():
    from digitalmodel.drilling_riser.global_model import w5_channels as w5

    pts = {k: {"static": {"te": 1.0, "ez_angle_deg": 0.1}} for k in ("ufj", "riser_top", "riser_bottom")}
    doc = {"riser_kind": "hang_off", "with_lmrp": False, "analysis": "statics", "points": pts,
           "range_graphs": {"InnerBarrel": {}, "Riser": {}}, "stroke": {}, "ring": {}, "top_load": {}}
    assert w5.missing_channels(doc, foundation=False) == []
    doc["with_lmrp"] = True
    miss = w5.missing_channels(doc, foundation=False)
    assert "points.stack:*" in miss and "range_graphs.Stack" in miss


# ---------------------------------------------------------------- solver
def _solve(h, tmp_path, name):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model

    m = orun.load_model(write_model(h, tmp_path / name) / "master.yml")
    m.CalculateStatics()
    return m


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
@pytest.mark.parametrize("with_lmrp", [True, False])
def test_hard_hang_off_statics_carry_the_string_weight_on_the_vessel(tmp_path, with_lmrp):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    s = _seawater(synthetic_spec())
    h = ho.hang_off_spec(s, "hard", with_lmrp=with_lmrp)
    m = _solve(h, tmp_path, "hard")
    ofx = orun._api()
    top = m["InnerBarrel"].StaticResult("End GZ force", ofx.oeEndA) * 1000.0
    w_ib = sum(ho.section_weight_n(x, top_z_m=0.0, rho_water=0.0, rho_contents=s.contents.density_kg_m3,
                                   in_air=True) for x in h.inner_barrel)
    assert abs(top) == pytest.approx(ho.hung_weight_n(h) + w_ib, rel=5e-3)
    yaw = m["TensionRing"].StaticResult("Rotation 3")
    assert abs((yaw + 180.0) % 360.0 - 180.0) < 1.0


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_soft_hang_off_statics_hang_the_string_on_the_spring_at_mid_stroke(tmp_path):
    s = _seawater(synthetic_spec())
    h = ho.hang_off_spec(s, "soft", with_lmrp=True)
    m = _solve(h, tmp_path, "soft")
    spring = m[ho.SPRING].StaticResult("Tension") * 1000.0
    assert spring == pytest.approx(h.hang_off.spring_tension_n, rel=2e-2)
    ring_z = m["TensionRing"].StaticResult("Z")
    assert abs(ring_z - s.tension_ring.z_static_m) < 0.5  # near mid-stroke


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
@pytest.mark.parametrize("mode,analysis", [("hard", "statics"), ("soft", "dynamics")])
def test_hang_off_case_through_the_adapter_extracts_a_complete_w5_set(tmp_path, mode, analysis):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model import w5_channels as w5

    extra = {"regular_wave": {"height_m": 3.0, "period_s": 9.0},
             "dynamics": {"time_step_s": 0.05, "build_up_s": 9.0, "duration_s": 18.0}} if analysis == "dynamics" else {}
    case = _case(tmp_path, analysis=analysis, hang_off={"mode": mode, "with_lmrp": True}, mud_density_kg_m3=1025.0,
                 heading_deg=45.0, current={"depth_speed_m_s": [[0.0, 0.6], [100.0, 0.3], [300.0, 0.1]]}, **extra)
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "m"))
    info = cp.ADAPTER.statics(m, case)
    assert abs(info["ring_yaw_deg"]) < 1.0
    if analysis == "dynamics":
        orun.run_dynamics(m)
    out = cp.ADAPTER.extract(m, case)
    assert w5.missing_channels(out["w5"], foundation=False) == []
    assert out["w5"]["riser_kind"] == "hang_off"
