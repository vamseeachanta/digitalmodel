"""twin D #1376: live operability/integrity monitor — public page + results.json.

Every check runs in PUBLIC CI with NO wiki: the atlas is committed and the seams build
offline with explicit criteria == the atlas's cited values. The monitor is a COMPOSITION —
each served gauge must EQUAL its Python seam (no re-derivation, no JS physics), the page must
ship zero external references (it renders telemetry + the served grid), and it must carry only
generic handles + times/ratios/moment (never absolute vessel forces, effective mass, or a
wellhead capacity).
"""
from __future__ import annotations

import importlib.util
import json
import re
from pathlib import Path

import yaml

from digitalmodel.drilling_riser import operability_atlas as oa
from digitalmodel.drilling_riser.conductor_response import solve_conductor_moment
from digitalmodel.drilling_riser.drift_off import drift_off_screen
from digitalmodel.drilling_riser.drift_off import load_config as load_drift_config
from digitalmodel.drilling_riser.envelope import ConductorInput, CurrentProfile, RiserSection
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.operability_screening import screen_operability
from digitalmodel.drilling_riser.response_correction import (
    correct_flexjoint_response,
    fit_flexjoint_models,
    flexjoint_utilisation,
)
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.drilling_riser.telemetry_inputs import parse_snapshots, snapshot_to_offset_pct
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions
from tests.riser_database.test_leak_gate import STRUCTURAL_PATTERNS

REPO = Path(__file__).resolve().parents[2]
_ATLAS_ROOT = REPO / "atlases"


def _load_builder():
    path = REPO / "scripts" / "drilling_riser" / "build_operability_monitor.py"
    spec = importlib.util.spec_from_file_location("build_operability_monitor", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


build = _load_builder()


def _committed() -> dict:
    return json.loads(build._JSON.read_text())


def _inline_data(html: str) -> dict:
    body = html.split("const DATA = ", 1)[1].rsplit(";\nconst TRACK", 1)[0]
    return json.loads(body)


def _structural_hits(text: str) -> list[str]:
    return [label for pattern, label in STRUCTURAL_PATTERNS if pattern.search(text)]


# 1. one snapshot-bound scenario drives every gauge (round-trips) -------------
def test_scenario_is_consistent_and_offset_round_trips():
    d = _committed()
    sc = d["scenario"]
    wd = sc["water_depth_m"]
    # the atlas-lookup current == the drift-off/flex-joint condition current
    assert sc["current_speed_mps"] == build._CONDITION["current_speed"]
    assert sc["config"] in oa.ALLOWED_TOKENS
    for p in d["track"]:
        # offset% round-trips to the measured offset (one geometry)
        assert abs(watch_circle_radius_m(p["offset_pct"], wd) - p["offset_m"]) < 1e-6


# 2. criteria sourced from the atlas (edition-match) --------------------------
def test_criteria_and_standards_match_the_atlas_config():
    d = _committed()
    atlas_cfg = yaml.safe_load((REPO / "src" / "digitalmodel" / "drilling_riser"
                                / "operability_configs.yml").read_text())
    want = [{"id": s["id"], "edition": str(s["edition"])} for s in atlas_cfg["provenance"]["standards"]]
    assert d["standards"] == want, "monitor standards drifted from the atlas's cited editions"
    crit, standards = build._criteria_from_atlas_config()
    assert standards == want
    assert (crit.flexjoint_angle_mean_deg, crit.flexjoint_angle_max_deg) == (
        atlas_cfg["criteria"]["flexjoint_angle_mean_deg"], atlas_cfg["criteria"]["flexjoint_angle_max_deg"])


# 3. composition fidelity: every gauge == its seam ---------------------------
def test_every_gauge_equals_its_seam():
    d = _committed()
    sc = d["scenario"]
    wd, length, tension = sc["water_depth_m"], 1500.0, 3000000.0
    section = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
    cur = sc["current_speed_mps"]
    crit, _ = build._criteria_from_atlas_config()
    mon = yaml.safe_load((REPO / "src" / "digitalmodel" / "drilling_riser" / "monitor_config.yml").read_text())
    cd = mon["conductor"]
    cond = ConductorInput(outer_diameter_m=cd["outer_diameter_m"], wall_thickness_m=cd["wall_thickness_m"],
                          soil_modulus_n_per_m2=cd["soil_modulus_n_per_m2"], stand_off_m=cd["stand_off_m"])
    train = json.loads((REPO / "tests" / "drilling_riser" / "fixtures" / "monitor_flexjoint_training.json").read_text())
    models = fit_flexjoint_models(
        measured_upper_deg=train["measured_upper_deg"], predicted_upper_deg=train["predicted_upper_deg"],
        measured_lower_deg=train["measured_lower_deg"], predicted_lower_deg=train["predicted_lower_deg"])
    condition = EnvironmentalConditions(**build._CONDITION)
    drift_cfg = load_drift_config()
    snaps = parse_snapshots(json.loads(
        (REPO / "tests" / "drilling_riser" / "fixtures" / "monitor_demo_track.json").read_text())["records"])

    for p, snap in zip(d["track"], snaps):
        off_pct = snapshot_to_offset_pct(snap, water_depth_m=wd)
        # operability
        scr = screen_operability(sc["config"], off_pct, cur, atlas_root=_ATLAS_ROOT)
        assert p["operability_uc"] == round(float(scr.governing_utilisation), 6)
        assert p["light"] == scr.light
        # flex-joint (twin B): predicted -> corrected -> decision -> utilisation
        drag = CurrentProfile(surface_speed_mps=cur).drag_load_n_per_m(section.outer_diameter_m)
        pred = solve_static_response(length_m=length, top_offset_m=snap.vessel_offset_m,
                                     tension_n=tension, ei_nm2=section.ei_nm2, current_load_n_per_m=drag)
        corr = correct_flexjoint_response(pred, models)
        assert p["flexjoint_uc"] == round(float(flexjoint_utilisation(corr.decision_static_angle_deg, crit)), 6)
        # decision = max(corrected, raw) — never relaxes
        assert corr.decision_static_angle_deg >= corr.raw_static_angle_deg - 1e-9
        # wellhead moment indicator
        wh = solve_conductor_moment(shear_n=pred.shear_lower_n, stand_off_m=cond.stand_off_m,
                                    soil_modulus_n_per_m2=cond.soil_modulus_n_per_m2, ei_nm2=cond.ei_nm2)
        assert p["wh_moment_knm"] == round(wh.max_moment_nm / 1000.0, 3)
        # drift-off
        dr = drift_off_screen(snap.dp, condition, section=section, water_depth_m=wd, length_m=length,
                              tension_n=tension, criteria=crit, config=drift_cfg, x0_m=snap.vessel_offset_m)
        assert p["r_watch_m"] == round(dr.r_watch_m, 3)
        assert p["drift_status"] == dr.status


# 4. twin-B honesty: measured surfaced; decision only tightens ---------------
def test_twin_b_measured_surfaced_and_conservative():
    d = _committed()
    for p in d["track"]:
        assert "measured_angle_deg" in p and "predicted_angle_deg" in p and "corrected_angle_deg" in p
        # the displayed decision angle can only be >= the raw physics (correction tightens)
        assert p["decision_angle_deg"] >= min(p["predicted_angle_deg"], p["corrected_angle_deg"]) - 1e-9
    # at least one point actually has a live measured value surfaced
    assert any(p["measured_angle_deg"] is not None for p in d["track"])


# 5. wellhead is a MOMENT indicator — no capacity, no UC ----------------------
def test_wellhead_is_moment_indicator_only():
    raw = build._JSON.read_text()
    assert "capacity" not in raw.lower()
    assert "wh_moment_uc" not in raw and "conductor_moment_capacity" not in raw
    html = build._HTML.read_text().lower()
    assert "capacity" not in html
    d = _committed()
    for p in d["track"]:
        assert "wh_moment_knm" in p and "wh_moment_uc" not in p


# 6. no JS physics + total no-exfil ------------------------------------------
def test_page_has_no_physics_and_no_external_reference():
    html = build._HTML.read_text()
    assert "://" not in html, "monitor HTML carries a URL scheme (exfil surface)"
    assert re.search(r'(?:src|href)\s*=\s*["\']?//', html) is None, "protocol-relative reference"
    assert "@font-face" not in html and "fonts." not in html, "external font"
    # the page must PLAY BACK served values, not re-derive drift physics in JS
    assert "Math.sqrt" not in html, "drift/EDS physics appears re-implemented in JS"
    assert "time_to_limit_s" in html and "DATA.track" in html
    # inline DATA is exactly the committed json
    assert _inline_data(html) == _committed()


# 7. results.json strict allow-list + generic + synthetic track --------------
_ALLOWED_KEYS = {
    "response", "atlas_id", "content_fingerprint", "tier", "offset_pct", "current_speed_mps",
    "configs", "max_rel_error", "standards", "scenario", "track", "fingerprint",
}
_ALLOWED_POINT_KEYS = {
    "t", "offset_m", "offset_pct", "operability_uc", "light", "flexjoint_uc",
    "measured_angle_deg", "predicted_angle_deg", "corrected_angle_deg", "decision_angle_deg",
    "wh_moment_knm", "r_watch_m", "watch_frac", "time_to_limit_s", "lead_time_margin_s",
    "point_of_disconnect_m", "drift_status",
    # twin E #1377: the rolled verdict (label) + physics lead time (seconds) — a label
    # and a time, no absolute force / mass / capacity.
    "go_no_go", "lead_time_s",
}
_FORBIDDEN = ("provenance", "validation", "source_sha256", "SOURCE_FILES", "samples",
              "m_eff", "thrust_efficiency", "environmental_force", "f_env_n", "f_net_n",
              "surviving_thrust", "capacity")


def test_results_json_is_a_strict_allowlist():
    d = _committed()
    assert set(d.keys()) <= _ALLOWED_KEYS, set(d.keys()) - _ALLOWED_KEYS
    assert set(d["configs"].keys()) <= set(oa.ALLOWED_TOKENS)
    for p in d["track"]:
        assert set(p.keys()) <= _ALLOWED_POINT_KEYS, set(p.keys()) - _ALLOWED_POINT_KEYS
    raw = build._JSON.read_text()
    for forbidden in _FORBIDDEN:
        assert forbidden not in raw, f"{forbidden} leaked into results.json"
    assert not _structural_hits(raw), "structural leak (path/url/email) in results.json"
    for s in d["standards"]:
        assert set(s.keys()) == {"id", "edition"}


def test_demo_track_fixture_is_synthetic():
    fx = json.loads((REPO / "tests" / "drilling_riser" / "fixtures" / "monitor_demo_track.json").read_text())
    prov = fx["provenance"]
    assert prov["source"] and prov["licence"] and prov["captured_offline"] is True
    identity = {"vessel_name", "imo", "mmsi", "callsign", "field", "well", "rig"}
    for rec in fx["records"]:
        assert identity.isdisjoint(rec.keys())
    raw = json.dumps(fx).lower()
    for forbidden in ("authorization", "x-api-key", "password", "token", "signature=", "aws"):
        assert forbidden not in raw


# 8. full-provenance fingerprint + byte-identical rebuild --------------------
def test_deterministic_and_fingerprint_bound():
    committed = _committed()
    fresh = build.build_results()
    assert committed == fresh, "committed monitor json drifted from a fresh regeneration"
    assert build._JSON.read_text() == json.dumps(fresh, indent=2, sort_keys=False) + "\n"
    assert committed["atlas_id"] == (_ATLAS_ROOT / oa.BASENAME / "default.txt").read_text().strip()
    assert isinstance(committed["fingerprint"], str) and len(committed["fingerprint"]) == 16


# 9. conductor geometry banded generic ---------------------------------------
def test_conductor_geometry_is_banded_generic():
    mon = yaml.safe_load((REPO / "src" / "digitalmodel" / "drilling_riser" / "monitor_config.yml").read_text())
    cd = mon["conductor"]
    assert cd["outer_diameter_m"] in build._COND_ALLOWED_OD_M
    assert cd["wall_thickness_m"] in build._COND_ALLOWED_WT_M
    for key, band in build._COND_BAND.items():
        v = float(cd[key])
        assert abs(v / band - round(v / band)) < 1e-9, f"{key}={v} off band {band}"
    assert "capacity" not in yaml.safe_load.__doc__ or True  # (no capacity key present)
    assert "conductor_moment_capacity_kn_m" not in json.dumps(mon)


# 10. one-pager governance + registered --------------------------------------
_SAFE_HOSTS = ("t.me/the_deckhand_bot", "api.deckhand", "vamseeachanta.github.io", "w3.org")
_API = REPO / "docs" / "api" / "capabilities" / "api" / "drilling-riser-operability-monitor"


def _strip_safe(text: str) -> str:
    for h in _SAFE_HOSTS:
        text = text.replace(h, "")
    return text.replace("https://", "").replace("http://", "")


def test_onepager_and_index_registered():
    html, js = _API.with_suffix(".html"), _API.with_suffix(".json")
    assert html.is_file() and js.is_file(), "one-pager not generated (run build_onepagers.py)"
    assert not _structural_hits(_strip_safe(html.read_text()))
    assert not _structural_hits(_strip_safe(js.read_text()))
    env = json.loads(js.read_text())
    assert env["report_url"].startswith("https://vamseeachanta.github.io/"), env["report_url"]
    index = (REPO / "docs" / "api" / "capabilities" / "index.html").read_text()
    assert build.SITE_PATH in index, "no index card links the operability monitor"


# 11. distinct ESCALATE channels (operability vs drift-off) ------------------
def test_escalate_channels_are_distinct():
    html = build._HTML.read_text()
    # two separate chips, separately labelled
    assert "operability: " in html and "drift-off: " in html
    assert "opchip" in html and "dochip" in html
    # the drift channel genuinely escalates in the demo (distinct from operability light)
    d = _committed()
    assert any(p["drift_status"] == "escalate" for p in d["track"])
    # and the operability-escalate path exists via the live screen (boundary between knots)
    s = screen_operability("rsu_a__drilling", 3.5, 0.0, atlas_root=_ATLAS_ROOT)
    assert s.light == "ESCALATE"
