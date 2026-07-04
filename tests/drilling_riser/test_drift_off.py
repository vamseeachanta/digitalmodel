"""twin C #1375: DP drift-off screen — conservative quasi-static.

Offline (no wiki): criteria use the public standard-factor literals (API RP 16Q
2/4 deg, API STD 2RD 0.67) exactly as twin A/B tests do. Verifies the six
conservatism invariants the T2 review required: SI units (kN->N), single-condition
R_watch/F_env coupling, remaining-travel from the measured offset, the EDS window
with velocity-at-initiation + escalate-if-negative, conservative thrust credit, and
the screening self-mark — plus the standards-traceability structural guard, config
banding, fixture governance, and the deferred solver stub.
"""
from __future__ import annotations

import ast
import json
import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import drift_off as do
from digitalmodel.drilling_riser.drift_off import (
    DRIFT_OFF,
    ESCALATE,
    STATION_HELD,
    drift_off_screen,
    environmental_force_n,
    load_config,
    watch_circle_limit,
)
from digitalmodel.drilling_riser.envelope import (
    EnvelopeCriteria,
    RiserSection,
    compute_operating_envelope,
)
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.telemetry_inputs import DPState
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions

_FIXTURE = Path(__file__).parent / "fixtures" / "drift_off_sample.json"
# public standard-factor criteria (API RP 16Q 2/4 deg; API STD 2RD 0.67) — the
# values the cited getters resolve to; literals for offline CI, matching twin A/B.
_CRIT = EnvelopeCriteria(2.0, 4.0, 0.67)
_SECTION = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
_WD = 1500.0
_LEN = 1500.0
_TEN = 5.0e6


def _cond(current=0.3, hs=3.0, wind=15.0):
    return EnvironmentalConditions(
        wave_hs=hs, wave_tp=10.0, wave_direction=0.0,
        current_speed=current, current_direction=0.0,
        wind_speed=wind, wind_direction=0.0,
    )


def _screen(dp, cond, *, config=None, x0=5.0, v0=0.0):
    return drift_off_screen(
        dp, cond, section=_SECTION, water_depth_m=_WD, length_m=_LEN,
        tension_n=_TEN, criteria=_CRIT, config=config, x0_m=x0, v0_mps=v0,
    )


# -- watch-circle limit == envelope boundary -----------------------------------


def test_watch_circle_limit_is_the_envelope_boundary():
    cond = _cond(current=0.3)
    r_watch, gov = watch_circle_limit(
        cond, section=_SECTION, water_depth_m=_WD, length_m=_LEN,
        tension_n=_TEN, criteria=_CRIT,
    )
    assert 0.0 < r_watch < _WD  # interior boundary (not degenerate)
    off_pct = 100.0 * r_watch / _WD
    # the boundary offset is allowable, the next step out is not
    res = compute_operating_envelope(
        section=_SECTION, water_depth_m=_WD, length_m=_LEN, tension_n=_TEN,
        criteria=_CRIT, offsets_pct=[off_pct, off_pct + 0.25],
        current_speeds_mps=[cond.current_speed],
        seastates=[do.SeaState(hs_m=cond.wave_hs, tp_s=cond.wave_tp)],
    )
    assert bool(res.allowable_mask[0, 0, 0]) and not bool(res.allowable_mask[1, 0, 0])
    assert gov  # a governing limit name is reported


# -- SI units: kN -> N, no false "station held" --------------------------------


def test_environmental_force_is_newtons_not_kilonewtons():
    f_env_n, comps = environmental_force_n(_cond(current=1.0), load_config())
    # a drilling vessel in 1 m/s current + 3 m Hs is MN-scale, not kN-scale
    assert f_env_n > 1.0e6
    # equals the reused kN model * 1000 exactly
    assert f_env_n == pytest.approx(
        (comps["wave_drift_kn"] + comps["current_kn"] + comps["wind_kn"]) * 1000.0)


def test_blackout_case_is_not_reported_station_held():
    # total thruster loss under real metocean must drift off, never "station held"
    r = _screen(DPState(thrusters_online=0, total_available_thrust_n=0.0), _cond())
    assert r.status in (DRIFT_OFF, ESCALATE)
    assert r.f_net_n > 0 and math.isfinite(r.time_to_limit_s)


# -- single-condition coupling (R_watch and F_env share the current) -----------


def test_higher_current_shrinks_watch_circle_and_raises_drift_force():
    dp = DPState(thrusters_online=0, total_available_thrust_n=0.0)
    lo = _screen(dp, _cond(current=0.2))
    hi = _screen(dp, _cond(current=0.5))
    assert hi.r_watch_m < lo.r_watch_m          # angle eats the offset budget
    assert hi.f_net_n > lo.f_net_n              # more drift force
    assert hi.time_to_limit_s < lo.time_to_limit_s  # compounding -> less time


# -- time-to-limit from measured offset (not from rest, not full circle) -------


def test_time_to_limit_uses_remaining_travel_and_low_m_eff():
    dp = DPState(thrusters_online=0, total_available_thrust_n=0.0)
    cfg = load_config()
    r = _screen(dp, _cond(current=0.3), x0=5.0, v0=0.0)
    a = r.f_net_n / cfg["m_eff_low_kg"]         # LOW m_eff end
    expect = math.sqrt(2.0 * (r.r_watch_m - 5.0) / a)  # v0=0 closed form
    assert r.time_to_limit_s == pytest.approx(expect, rel=1e-9)
    # a larger pre-existing offset -> less remaining travel -> shorter time
    assert _screen(dp, _cond(current=0.3), x0=25.0).time_to_limit_s < r.time_to_limit_s


# -- EDS window: velocity at initiation + escalate-if-negative -----------------


def test_point_of_disconnect_accounts_for_velocity_at_initiation():
    dp = DPState(thrusters_online=0, total_available_thrust_n=0.0)
    cfg = load_config()
    r = _screen(dp, _cond(current=0.3), x0=5.0, v0=0.0)
    if r.status != DRIFT_OFF:
        pytest.skip("scenario escalated; covered by the escalate test")
    a = r.f_net_n / cfg["m_eff_low_kg"]
    t_disc = r.time_to_limit_s - cfg["t_eds_s"]
    expect = 5.0 + 0.5 * a * t_disc**2          # x0 + v0*t_disc + 1/2 a t_disc^2
    assert r.point_of_disconnect_m == pytest.approx(expect, rel=1e-9)
    # disconnect is initiated BEFORE the limit (leaves the EDS margin)
    assert r.point_of_disconnect_m < r.r_watch_m


def test_escalate_when_window_is_negative():
    # a huge EDS duration makes t_limit <= t_eds -> cannot disconnect in time
    cfg = load_config()
    cfg = {**cfg, "t_eds_s": 100000.0}
    r = _screen(DPState(total_available_thrust_n=0.0), _cond(), config=cfg)
    assert r.status == ESCALATE
    assert r.lead_time_margin_s < 0


def test_already_over_limit_escalates():
    r = _screen(DPState(total_available_thrust_n=0.0), _cond(current=1.0), x0=100000.0)
    assert r.status == ESCALATE
    assert r.time_to_limit_s == 0.0


# -- conservative thrust credit (DPState is scalar; default eta = 0) -----------


def test_default_thrust_credit_is_zero_and_reported_separately():
    cfg = load_config()
    assert cfg["thrust_efficiency"] == 0.0
    dp = DPState(thrusters_online=6, total_available_thrust_n=9.0e6)
    r = _screen(dp, _cond())
    assert r.surviving_thrust_n == 9.0e6        # reported as a margin
    assert r.f_net_n == pytest.approx(r.f_env_n)  # NOT credited against the drift


def test_station_held_only_with_explicit_efficiency_and_ample_thrust():
    cfg = {**load_config(), "thrust_efficiency": 1.0}
    dp = DPState(thrusters_online=6, total_available_thrust_n=5.0e7)  # >> F_env
    r = _screen(dp, _cond(current=0.3, hs=1.0, wind=8.0), config=cfg)
    assert r.status == STATION_HELD
    assert r.time_to_limit_s == math.inf


# -- screening self-mark -------------------------------------------------------


def test_every_result_self_marks_screening_tier():
    for dp in (DPState(total_available_thrust_n=0.0),
               DPState(total_available_thrust_n=4.0e6)):
        p = _screen(dp, _cond()).provenance
        assert p["tier"] == "quasi_static_screening"
        assert p["dynamic"] is False
        assert p["disclaimer"] and "not a time-domain" in p["disclaimer"].lower()
        assert p["m_eff_band_kg"] and "thrust_efficiency" in p


# -- standards-traceability: the module cites nothing --------------------------


def test_module_imports_no_citations_and_result_holds_no_cited_value():
    src = Path(do.__file__).read_text()
    tree = ast.parse(src)
    for node in ast.walk(tree):
        mod = None
        if isinstance(node, ast.ImportFrom):
            mod = node.module or ""
        elif isinstance(node, ast.Import):
            mod = ",".join(a.name for a in node.names)
        if mod and "citation" in mod.lower():
            raise AssertionError(f"drift_off imports citations: {mod}")
    r = _screen(DPState(total_available_thrust_n=0.0), _cond())
    for val in (r.r_watch_m, r.f_env_n, r.f_net_n, r.time_to_limit_s,
                r.point_of_disconnect_m, r.lead_time_margin_s):
        assert isinstance(val, float)  # plain floats, never a CitedValue


# -- config banding + fixture governance ---------------------------------------


def test_config_is_banded_generic():
    cfg = load_config()
    bands = {"m_eff_low_kg": 5.0e7, "m_eff_high_kg": 5.0e7, "t_eds_s": 10.0}
    for k, band in bands.items():
        assert abs(cfg[k] / band - round(cfg[k] / band)) < 1e-9, f"{k} off-band"
    v, vbands = cfg["vessel"], {
        "length_m": 10.0, "beam_m": 5.0, "draft_m": 2.0,
        "displacement_t": 10000.0, "windage_area_m2": 500.0}
    for k, band in vbands.items():
        assert abs(v[k] / band - round(v[k] / band)) < 1e-9, f"vessel.{k} off-band"
    assert 0.0 <= cfg["thrust_efficiency"] <= 1.0


def test_fixture_provenance_no_credentials_no_identity():
    fx = json.loads(_FIXTURE.read_text())
    prov = fx["provenance"]
    assert prov["source"] and prov["licence"]
    raw = _FIXTURE.read_text().lower()
    for forbidden in ("authorization", "x-api-key", "password", "token", "signature=", "aws"):
        assert forbidden not in raw
    identity_keys = {"vessel_name", "imo", "mmsi", "callsign", "field", "well", "rig"}
    for rec in fx["scenarios"]:
        assert identity_keys.isdisjoint(rec.keys())
    for tok in ("imo", "mmsi", "callsign", "vessel_name"):
        assert f'"{tok}"' not in raw


# -- deferred dynamic tier: solver.licensed=False stub -------------------------


def test_drift_off_stub_library_is_licensed_false_and_registered():
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.refresh import DEFAULT_ATLAS_ROOT, LIBRARY_EXPECTATIONS

    assert "drilling_riser_drift_off" in LIBRARY_EXPECTATIONS
    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, "drilling_riser_drift_off")
    assert atlas.provenance["solver"]["licensed"] is False
    assert atlas.provenance["solver"]["version"] == "STUB"
    # in-coverage predicts; out-of-coverage escalates (never extrapolates)
    ok = atlas.predict({"mode": "drilling", "current_speed_mps": 1.0, "hs_m": 3.0})
    assert ok.in_range and ok.value >= 1.0
    bad = atlas.predict({"mode": "drilling", "current_speed_mps": 9.9, "hs_m": 3.0})
    assert not bad.in_range
