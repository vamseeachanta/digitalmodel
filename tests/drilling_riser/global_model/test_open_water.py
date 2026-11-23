"""Open-water completion/workover riser (C2 topology): spec, text model, hand checks and solver round trips.

The synthetic riser is generic (no project data): tension frame on a vertical tensioner force, upper tension
joint, rotary lateral hold, riser joints, a tapered stress joint, EDP | LRP, tree and wellhead to the datum.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.spec import (
    Contents,
    GlobalEnvironment,
    LineSection,
    tube_section,
)
from digitalmodel.solvers.orcaflex import orcaflex_api

E = 207e9
IN = 0.0254
G = 9.80665
RHO_W = 1025.0
RHO_C = 1200.0


def _tube(name, length, od_in, wall_in, *, seg=2.0, extra_mass=0.0, cd=1.0):
    t = tube_section(od_m=od_in * IN, wall_m=wall_in * IN, youngs_modulus_pa=E)
    m = 7850.0 * t["area_m2"] + extra_mass
    return LineSection(name=name, length_m=length, segment_length_m=seg, mass_per_m_kg=m,
                       displaced_volume_per_m_m3=t["area_m2"] + math.pi / 4 * t["id_m"] ** 2, bore_id_m=t["id_m"],
                       ei_nm2=t["ei_nm2"], ea_n=t["ea_n"], drag_diameter_m=od_in * IN, cd_normal=cd, ca_normal=1.0,
                       stress_od_m=od_in * IN, stress_id_m=t["id_m"])


def _body(name, length, *, mass_t, wet_t, seg=0.5):
    return LineSection(name=name, length_m=length, segment_length_m=seg, mass_per_m_kg=mass_t * 1000 / length,
                       displaced_volume_per_m_m3=(mass_t - wet_t) / 1.025 / length, bore_id_m=0.0, ei_nm2=2.0e9,
                       ea_n=3.0e10, drag_diameter_m=2.5, cd_normal=1.0, ca_normal=1.0)


def open_water_dict(**over) -> dict:
    """A 300 m water-depth open-water riser (synthetic)."""
    rotary_z, frame_z = 20.0, 24.0
    upper = [_tube("Upper tension joint", frame_z - rotary_z, 11.5, 2.0, seg=1.0)]
    sj = [_tube(f"Stress joint {i + 1}", 1.5, 11.5 - (i + 0.5) * (11.5 - 8.625) / 4,
                (11.5 - (i + 0.5) * (11.5 - 8.625) / 4 - 7.5) / 2, seg=0.5) for i in range(4)]
    edp = _body("EDP", 3.0, mass_t=30.0, wet_t=22.0)
    riser = [_tube("Tension joint (below rotary)", 6.0, 11.5, 2.0, seg=1.0),
             _tube("Riser joint", 280.0, 8.625, 0.557, seg=4.0, extra_mass=5.0), *sj, edp]
    interface = rotary_z - sum(s.length_m for s in riser)
    stack = [_body("LRP", 3.0, mass_t=30.0, wet_t=22.0), _body("Tree", 4.0, mass_t=50.0, wet_t=43.0)]
    datum = interface - sum(s.length_m for s in stack)
    d = {
        "name": "synthetic-open-water-riser",
        "environment": GlobalEnvironment(water_depth_m=-datum, water_density_kg_m3=RHO_W).model_dump(),
        "contents": Contents(density_kg_m3=RHO_C, pressure_ref_z_m=frame_z).model_dump(),
        "tension_frame": {"mass_kg": 30000.0, "z_m": frame_z},
        "tensioners": {"total_vertical_tension_n": 1.2e6},
        "rotary_z_m": rotary_z,
        "upper": [s.model_dump() for s in upper],
        "riser": [s.model_dump() for s in riser],
        "stack": [s.model_dump() for s in stack],
        "wellhead_datum_z_m": datum,
    }
    d.update(over)
    return d


@pytest.fixture
def ow():
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    return OpenWaterRiserSpec.model_validate(open_water_dict())


# ---------------------------------------------------------------- spec
def test_geometry_closes_and_interface_elevation(ow):
    assert ow.edp_interface_z_m == pytest.approx(ow.rotary_z_m - sum(s.length_m for s in ow.riser))
    assert ow.kind == "open_water"


def test_geometry_that_does_not_close_is_rejected():
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    with pytest.raises(ValueError, match="rotary"):
        OpenWaterRiserSpec.model_validate(open_water_dict(rotary_z_m=19.0))
    with pytest.raises(ValueError, match="datum"):
        OpenWaterRiserSpec.model_validate(open_water_dict(wellhead_datum_z_m=-400.0))


def test_tension_above_the_rating_is_rejected():
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    with pytest.raises(ValueError, match="rated"):
        OpenWaterRiserSpec.model_validate(open_water_dict(tensioners={"total_vertical_tension_n": 1.2e6,
                                                                      "rated_total_n": 1.0e6}))


def test_release_tension_above_the_rating_is_rejected():
    """r1: every commanded tension is checked against the rating, the EDP anti-recoil step included."""
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    rated = {"total_vertical_tension_n": 1.2e6, "rated_total_n": 1.3e6}
    with pytest.raises(ValueError, match="anti-recoil.*rated"):
        OpenWaterRiserSpec.model_validate(open_water_dict(tensioners=rated,
                                                          edp_release={"anti_recoil_tension_n": 2.0e6}))
    ok = OpenWaterRiserSpec.model_validate(open_water_dict(tensioners=rated,
                                                           edp_release={"anti_recoil_tension_n": 1.25e6}))
    assert ok.edp_release.anti_recoil_tension_n == 1.25e6
    # no rating declared: nothing to check against
    OpenWaterRiserSpec.model_validate(open_water_dict(edp_release={"anti_recoil_tension_n": 2.0e6}))


def _hollow_stack_dict(**over) -> dict:
    """The synthetic riser with a 7-1/16 in well bore through the LRP and the tree."""
    d = open_water_dict(**over)
    for s in d["stack"]:
        s["bore_id_m"] = 7.0625 * IN
    return d


# ---------------------------------------------------------------- text model
def test_generic_spec_objects(ow):
    from digitalmodel.drilling_riser.global_model.open_water import build_open_water_generic_spec

    g = build_open_water_generic_spec(ow)["generic"]
    assert [ln["name"] for ln in g["lines"]] == ["Upper", "Riser", "Stack"]
    assert [b["name"] for b in g["buoys_3d"]] == ["TensionFrame"]
    body = g["buoys_6d"][0]
    assert body["name"] == "RotaryBody" and body["connection"] == "Rotary" and body["mass"] == pytest.approx(0.01)
    rot = next(c for c in g["constraints"] if c["name"] == "Rotary")
    assert rot["properties"]["DOFFree, DOFInitialValue"] == [[False], [False], [True, 0.0], [True, 0.0], [True, 0.0], [False]]
    w = next(x for x in g["winches"] if x["name"] == "TopTensioner")
    assert all(v[1] == pytest.approx(1.2e3) for v in w["properties"]["StageMode, StageValue"])
    riser = g["lines"][1]["properties"]
    ends = riser["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, ConnectionDeclination, "
                 "ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"]
    assert ends[0][0] == "Rotary" and ends[1][0] == "Stack" and ends[1][7] is None  # latched: no release stage


def test_edp_release_sets_release_stage_and_anti_recoil_tension(ow):
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec, build_open_water_generic_spec

    d = ow.model_dump()
    d["edp_release"] = {"anti_recoil_tension_n": 0.9e6}
    g = build_open_water_generic_spec(OpenWaterRiserSpec.model_validate(d))["generic"]
    riser = g["lines"][1]["properties"]
    key = next(k for k in riser if k.startswith("Connection, "))
    assert riser[key][1][7] == 1  # End B released at the start of stage 1 (the main stage)
    w = next(x for x in g["winches"] if x["name"] == "TopTensioner")
    vals = [v[1] for v in w["properties"]["StageMode, StageValue"]]
    assert vals[:-1] == [pytest.approx(1.2e3)] * (len(vals) - 1) and vals[-1] == pytest.approx(0.9e3)


def test_stack_line_carries_the_contents_like_the_hand_checks():
    """r1: the Stack line carries the riser contents (density and pressure) through a nonzero bore, as
    tension_references and beam_model count them; a zero-bore stack section adds nothing either way."""
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec, build_open_water_generic_spec

    s = OpenWaterRiserSpec.model_validate(_hollow_stack_dict(
        contents=Contents(density_kg_m3=RHO_C, pressure_ref_z_m=24.0, pressure_pa=5.0e6).model_dump()))
    lines = {ln["name"]: ln["properties"] for ln in build_open_water_generic_spec(s)["generic"]["lines"]}
    for name in ("Upper", "Riser", "Stack"):
        assert lines[name]["ContentsDensity"] == pytest.approx(RHO_C / 1000.0)
        assert lines[name]["ContentsPressure"] == pytest.approx(5.0e3)
        assert lines[name]["ContentsPressureRefZ"] == pytest.approx(24.0)


def test_hollow_stack_contents_enter_the_stack_bottom_reference():
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec, tension_references

    solid = tension_references(OpenWaterRiserSpec.model_validate(open_water_dict()))
    hollow = tension_references(OpenWaterRiserSpec.model_validate(_hollow_stack_dict()))
    contents_w = RHO_C * G * math.pi / 4 * (7.0625 * IN) ** 2 * 7.0  # LRP 3 m + tree 4 m
    assert hollow["edp_bottom_n"] == pytest.approx(solid["edp_bottom_n"])
    assert solid["stack_bottom_n"] - hollow["stack_bottom_n"] == pytest.approx(contents_w, rel=1e-9)


def test_write_model_dispatches_on_the_spec_type(ow, tmp_path):
    from digitalmodel.drilling_riser.global_model.build import write_model

    out = write_model(ow, tmp_path / "ow")
    text = "".join(p.read_text(encoding="utf-8") for p in (out / "includes").glob("*.yml"))
    assert "TensionFrame" in text and "Rotary" in text and "TopTensioner" in text


# ---------------------------------------------------------------- hand checks
def test_tension_references_close_form(ow):
    from digitalmodel.drilling_riser.global_model.open_water import tension_references

    ref = tension_references(ow)
    assert ref["frame_weight_n"] == pytest.approx(30000.0 * G)
    assert ref["upper_top_n"] == pytest.approx(1.2e6 - 30000.0 * G)
    w = 0.0
    for s in (*ow.upper, *ow.riser):
        w += (s.mass_per_m_kg + RHO_C * math.pi / 4 * s.bore_id_m ** 2) * s.length_m * G
    # everything between the frame and the interface is submerged except the part above z = 0
    z, wet_vol = ow.tension_frame.z_m, 0.0
    for s in (*ow.upper, *ow.riser):
        top, bot = z, z - s.length_m
        wet_vol += s.displaced_volume_per_m_m3 * max(0.0, min(0.0, top) - bot)
        z = bot
    w -= RHO_W * wet_vol * G
    w += ow.rotary_mass_kg * G  # the small rotary body rides on the string (free along z)
    assert ref["edp_bottom_n"] == pytest.approx(ref["upper_top_n"] - w, rel=1e-12)
    assert ref["released_weight_n"] == pytest.approx(ref["frame_weight_n"] + w, rel=1e-12)


def test_beam_periods_clamped_pinned_matches_analytic():
    from digitalmodel.drilling_riser.global_model.hand_checks import BeamSegment, tensioned_beam_periods

    L, n, EI, m = 10.0, 40, 1.0e6, 50.0
    segs = [BeamSegment(L / n, EI, 0.0, 0.0, m) for _ in range(n)]
    got = tensioned_beam_periods(segs, 1, clamp_bottom=True)[0]
    exact = 2 * math.pi / (3.9266023 ** 2 * math.sqrt(EI / (m * L ** 4)))
    assert got == pytest.approx(exact, rel=1e-4)
    assert tensioned_beam_periods(segs, 2, pinned_nodes=(0, n)) == pytest.approx(tensioned_beam_periods(segs, 2))


def test_reference_periods_are_positive_and_ascending(ow):
    from digitalmodel.drilling_riser.global_model.open_water import reference_periods

    p = reference_periods(ow, n_modes=5)
    assert len(p) == 5 and all(a > b > 0 for a, b in zip(p, p[1:]))


def test_release_reference_small_waterplane_limit(ow):
    from digitalmodel.drilling_riser.global_model.open_water import release_reference, tension_references

    ref = tension_references(ow)
    d = ow.model_dump()
    d["edp_release"] = {"anti_recoil_tension_n": 1.02 * ref["released_weight_n"]}
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    r = release_reference(OpenWaterRiserSpec.model_validate(d))
    f0 = 0.02 * ref["released_weight_n"]
    assert r["net_force_n"] == pytest.approx(f0)
    t = 0.5  # w t << 1: z ~ F0 t^2 / 2M, v ~ F0 t / M
    assert r["z"](t) == pytest.approx(0.5 * f0 / r["mass_kg"] * t * t, rel=1e-3)
    assert r["v"](t) == pytest.approx(f0 / r["mass_kg"] * t, rel=1e-3)
    # energy identity of the closed form
    assert 0.5 * r["mass_kg"] * r["v"](7.0) ** 2 == pytest.approx(f0 * r["z"](7.0) - 0.5 * r["k_w_n_m"] * r["z"](7.0) ** 2)


# ---------------------------------------------------------------- solver round trips
solver = [pytest.mark.solver, pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")]


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_statics_match_hand_tensions_and_frame_balances(ow, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import tension_references

    m = orun.load_and_solve_statics(write_model(ow, tmp_path / "m") / "master.yml")
    te = orun.open_water_end_tensions(m)
    ref = tension_references(ow)
    assert te["upper_top_n"] == pytest.approx(ref["upper_top_n"], rel=1e-3)
    assert te["edp_bottom_n"] == pytest.approx(ref["edp_bottom_n"], rel=1e-3)
    assert te["upper_top_n"] - te["stack_bottom_n"] == pytest.approx(ref["submerged_weight_n"], rel=5e-3)
    chk = orun.open_water_physical_checks(m, ow)
    assert chk["physical"], chk
    import json

    json.dumps(chk)  # plain Python types (the runner writes the statics info as JSON)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_modes_match_the_beam_reference(ow, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import reference_periods

    m = orun.load_and_solve_statics(write_model(ow, tmp_path / "m") / "master.yml")
    modes = orun.riser_modal_periods(m, n_modes=3)
    ref = reference_periods(ow, n_modes=3)
    assert len(modes) == len(ref) == 3  # r1: an empty or short solver result must not pass the zip
    for got, exp in zip(modes, ref):
        assert got["period_s"] == pytest.approx(exp, rel=0.05)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_hollow_stack_statics_and_modes_match_the_references(tmp_path):
    """r1: a nonzero stack bore - solver and hand checks carry the same contents (stack-bottom tension, periods)."""
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import (
        OpenWaterRiserSpec,
        reference_periods,
        tension_references,
    )

    s = OpenWaterRiserSpec.model_validate(_hollow_stack_dict(
        contents=Contents(density_kg_m3=RHO_C, pressure_ref_z_m=24.0, pressure_pa=5.0e6).model_dump()))
    m = orun.load_and_solve_statics(write_model(s, tmp_path / "hs") / "master.yml")
    te = orun.open_water_end_tensions(m)
    ref = tension_references(s)
    contents_w = RHO_C * G * math.pi / 4 * (7.0625 * IN) ** 2 * 7.0
    assert te["stack_bottom_n"] == pytest.approx(ref["stack_bottom_n"], abs=0.05 * contents_w)
    assert te["upper_top_n"] - te["stack_bottom_n"] == pytest.approx(ref["submerged_weight_n"], rel=1e-3)
    modes = orun.riser_modal_periods(m, n_modes=3)
    exp = reference_periods(s, n_modes=3)
    assert len(modes) == len(exp) == 3
    for got, e in zip(modes, exp):
        assert got["period_s"] == pytest.approx(e, rel=0.05)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_edp_release_momentum_matches_the_closed_form(ow, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import (
        OpenWaterRiserSpec,
        release_reference,
        tension_references,
    )

    d = ow.model_dump()
    d["edp_release"] = {"anti_recoil_tension_n": 1.02 * tension_references(ow)["released_weight_n"]}
    d["dynamics"] = {"time_step_s": 0.01, "build_up_s": 5.0, "duration_s": 12.0}
    spec = OpenWaterRiserSpec.model_validate(d)
    m = orun.load_model(write_model(spec, tmp_path / "rel") / "master.yml")
    m.CalculateStatics()
    m.RunSimulation()
    h = orun.edp_release_history(m)
    r = release_reference(spec)
    v_model = orun.window_mean(h["t_s"], h["edp_vz_m_s"], 10.0, 1.0)
    v_hand = sum(r["v"](10.0 + k / 100.0) for k in range(-100, 101)) / 201
    assert v_model == pytest.approx(v_hand, rel=0.05)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_physical_checks_hold_at_an_offset_in_current(tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    d = open_water_dict(vessel_offset_m=(20.0, 0.0),
                        current={"direction_deg": 0.0, "depth_speed_m_s": [[0.0, 1.0], [280.0, 0.5]]})
    s = OpenWaterRiserSpec.model_validate(d)
    m = orun.load_and_solve_statics(write_model(s, tmp_path / "off") / "master.yml")
    chk = orun.open_water_physical_checks(m, s)
    assert chk["physical"], chk


# ---------------------------------------------------------------- campaign adapter
def _ow_case(tmp_path, **params):
    import yaml

    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    p = tmp_path / "ow-spec.yml"
    p.write_text(yaml.safe_dump({"model": OpenWaterRiserSpec.model_validate(open_water_dict()).model_dump(mode="json")}),
                 encoding="utf-8")
    return {"case_id": "OW-1", "analysis": params.pop("analysis", "statics"), "params": {"base_spec": str(p), **params}}


def test_campaign_case_spec_loads_open_water_and_sets_the_edp_release(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec, tension_references

    case = _ow_case(tmp_path, analysis="dynamics", heading_deg=90.0, offset_pct_wd=2.0, contents_pressure_pa=5.0e6,
                    edp_release={"anti_recoil_factor": 1.05},
                    dynamics={"time_step_s": 0.02, "build_up_s": 10.0, "duration_s": 30.0})
    s = cp.case_spec(case)
    assert isinstance(s, OpenWaterRiserSpec)
    assert s.contents.pressure_pa == 5.0e6
    assert s.vessel_offset_m[1] == pytest.approx(0.02 * s.environment.water_depth_m)
    assert s.edp_release.anti_recoil_tension_n == pytest.approx(1.05 * tension_references(s)["released_weight_n"])


def test_edp_release_default_steps_below_the_released_weight_and_the_gate_factor_is_kept(tmp_path):
    """Owner decision W2B2 (2026-09-28): a C2 EDP disconnect case steps the tension to 0.98 x the released weight (the
    string settles back under control); 1.02 x stays only in the release qualification gate."""
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model.open_water import tension_references

    assert cp.EDP_ANTI_RECOIL_FACTOR == 0.98 and cp.EDP_RELEASE_GATE_FACTOR == 1.02
    dyn = {"time_step_s": 0.02, "build_up_s": 10.0, "duration_s": 30.0}
    s = cp.case_spec(_ow_case(tmp_path, analysis="dynamics", edp_release={}, dynamics=dyn))
    w = tension_references(s)["released_weight_n"]
    assert s.edp_release.anti_recoil_tension_n == pytest.approx(0.98 * w)
    g = cp.case_spec(_ow_case(tmp_path, analysis="dynamics", dynamics=dyn,
                              edp_release={"anti_recoil_factor": cp.EDP_RELEASE_GATE_FACTOR}))
    assert g.edp_release.anti_recoil_tension_n == pytest.approx(1.02 * w)


def test_campaign_rejects_the_drilling_disconnect_proxy_on_open_water(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp

    case = _ow_case(tmp_path, analysis="dynamics", proxy={"kind": "disconnect"},
                    dynamics={"time_step_s": 0.1, "build_up_s": 10.0, "duration_s": 30.0})
    with pytest.raises(ValueError, match="edp_release"):
        cp.ADAPTER.prepare(None, case)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_campaign_adapter_statics_and_extraction_on_open_water(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    case = _ow_case(tmp_path, heading_deg=0.0, offset_pct_wd=3.0, statics="seeded", modal_modes=3,
                    current={"depth_speed_m_s": [[0.0, 0.5], [280.0, 0.2]]})
    master = cp.ADAPTER.build(case, tmp_path / "m")
    m = orun.load_model(master)
    info = cp.ADAPTER.statics(m, case)
    assert info["physical"] and info["steps"] == 11  # -2 % -> +3 % WD in 0.5 % steps
    out = cp.ADAPTER.extract(m, case)
    assert out["edp_bottom_n"] > 0 and out["sj_base_bending_moment_nm"] > 0 and len(out["modes"]) == 3


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_campaign_open_water_statics_default_to_direct(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    case = _ow_case(tmp_path, heading_deg=0.0)
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "d"))
    info = cp.ADAPTER.statics(m, case)
    assert info["method"] == "direct" and info["physical"]


# ---------------------------------------------------------------- stress benchmark (r1)
def _hand_von_mises_max_pa(spec, step_m: float = 0.25) -> float:
    """Independent tube benchmark of the vertical static string: Lame thick-wall hoop and radial stresses from the
    internal (contents) and external (sea) pressures, axial wall stress from the closed-form effective tension
    (Tw = Te + p_i A_i - p_o A_o), no bending; von Mises maximum over the inner and outer fibres and over the
    stressed riser sections."""
    from digitalmodel.drilling_riser.global_model.open_water import tension_references

    ref = tension_references(spec)
    p_ref, ref_z = spec.contents.pressure_pa, spec.contents.pressure_ref_z_m
    worst, z_top = 0.0, spec.rotary_z_m
    for s, link in zip(spec.riser, ref["riser_chain"]):
        if s.stress_od_m is not None:
            ro, ri = s.stress_od_m / 2, s.stress_id_m / 2
            n = max(1, round(s.length_m / step_m))
            for k in range(n + 1):
                f = k / n
                z = z_top - f * s.length_m
                te = link["te_top_n"] + f * (link["te_bottom_n"] - link["te_top_n"])
                p_i = p_ref + RHO_C * G * (ref_z - z)
                p_o = RHO_W * G * max(0.0, -z)
                a_i, a_o = math.pi * ri ** 2, math.pi * ro ** 2
                s_a = (te + p_i * a_i - p_o * a_o) / (a_o - a_i)
                c1 = (p_i * ri ** 2 - p_o * ro ** 2) / (ro ** 2 - ri ** 2)
                c2 = (p_i - p_o) * ri ** 2 * ro ** 2 / (ro ** 2 - ri ** 2)
                for r in (ri, ro):
                    s_h, s_r = c1 + c2 / r ** 2, c1 - c2 / r ** 2
                    worst = max(worst, math.sqrt(0.5 * ((s_a - s_h) ** 2 + (s_h - s_r) ** 2 + (s_r - s_a) ** 2)))
        z_top -= s.length_m
    return worst


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
@pytest.mark.parametrize("pressure_pa", [0.0, 30.0e6])
def test_von_mises_static_and_dynamic_extraction_match_the_tube_benchmark(tmp_path, pressure_pa):
    """r1: the reported von Mises stress (static and dynamic extraction, kPa -> Pa) against an independent tube
    benchmark, with and without a contents pressure large enough to dominate the hoop stress."""
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.open_water import OpenWaterRiserSpec

    d = open_water_dict(contents=Contents(density_kg_m3=RHO_C, pressure_ref_z_m=24.0,
                                          pressure_pa=pressure_pa).model_dump(),
                        dynamics={"time_step_s": 0.02, "build_up_s": 2.0, "duration_s": 4.0})
    s = OpenWaterRiserSpec.model_validate(d)
    hand = _hand_von_mises_max_pa(s)
    m = orun.load_model(write_model(s, tmp_path / f"vm{int(pressure_pa)}") / "master.yml")
    m.CalculateStatics()
    static = orun.open_water_static_responses(m, s)["riser_von_mises_max_pa"]
    assert static == pytest.approx(hand, rel=0.01)
    m.RunSimulation()  # calm water, no vessel motion: the dynamic maximum stays at the static value
    dyn = orun.open_water_governing_responses(m, s)["riser_von_mises_max_pa"]
    assert dyn == pytest.approx(hand, rel=0.01)
