"""twin E #1377 — the integration + demo loop (end-to-end proof of the twin).

Replays the synthetic demo telemetry track against a rolling metocean stream through the
composed seams and rolls ONE conservative GO/CAUTION/NO-GO verdict per step. Offline,
deterministic, no wiki. The heavy end-to-end replay (which recomputes envelope sweeps per
point) runs once; the conservative-roll-up logic is unit-tested directly (fast).
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.decision_spine import DISPLAY_LABEL
from digitalmodel.drilling_riser import twin_loop
from digitalmodel.drilling_riser.twin_loop import _roll_verdict, run_twin_loop
from digitalmodel.marine_ops.installation.go_no_go import DecisionState

_FIX = Path(__file__).parent / "fixtures"
_TRACK = json.loads((_FIX / "monitor_demo_track.json").read_text())["records"]
_MET = json.loads((_FIX / "monitor_metocean_track.json").read_text())["records"]


def _point(**over):
    """A minimal composed-point dict for the roll-up unit tests."""
    p = dict(light="OPERABLE", operability_uc=0.5, flexjoint_uc=0.5,
             drift_status="station_held", lead_time_margin_s=None)
    p.update(over)
    return p


# -- conservative worst-of roll-up (fast, no envelope recompute) ----------------

def test_all_clear_is_go():
    st, _ = _roll_verdict(_point(), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.GO


def test_inoperable_is_no_go():
    st, _ = _roll_verdict(_point(light="INOPERABLE", operability_uc=1.4), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO


def test_none_operability_fails_closed_without_crashing():
    # out-of-range operability -> light ESCALATE, uc None. Must NOT call _classify(None).
    st, _ = _roll_verdict(_point(light="ESCALATE", operability_uc=None), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO


def test_escalate_with_value_is_at_least_caution_not_go():
    st, _ = _roll_verdict(_point(light="ESCALATE", operability_uc=0.98), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.MARGINAL  # never a clean GO


def test_seastate_domain_guard_forces_no_go_above_ceiling():
    # everything else clear, but a rolling Hs beyond the static-screen ceiling is out of domain
    st, _ = _roll_verdict(_point(), wave_hs_m=1.6, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO


def test_nan_seastate_fails_closed():
    # a dropped / NaN metocean sample must NOT bypass the domain guard (NaN > ceiling is
    # False) — it fails closed to NO-GO rather than being silently trusted as in-domain
    st, _ = _roll_verdict(_point(), wave_hs_m=float("nan"), hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO


def test_drift_off_is_magnitude_gated():
    # an OPEN EDS window is CAUTION ...
    st, _ = _roll_verdict(_point(drift_status="drift_off", lead_time_margin_s=120.0), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.MARGINAL
    # ... a non-positive window (cannot disconnect in time) is NO-GO, not CAUTION
    st, _ = _roll_verdict(_point(drift_status="drift_off", lead_time_margin_s=-5.0), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO
    st, _ = _roll_verdict(_point(drift_status="escalate"), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO
    # ... and a drift-off with NO reported EDS margin fails closed (never a trusted CAUTION)
    st, _ = _roll_verdict(_point(drift_status="drift_off", lead_time_margin_s=None), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO


def test_flexjoint_decision_uc_gates_the_verdict():
    st, _ = _roll_verdict(_point(flexjoint_uc=1.2), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.NO_GO
    st, _ = _roll_verdict(_point(flexjoint_uc=0.95), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert st is DecisionState.MARGINAL


def test_lead_time_is_the_drift_off_physics_margin():
    st, lead = _roll_verdict(_point(drift_status="drift_off", lead_time_margin_s=90.0), wave_hs_m=0.8, hs_ceiling_m=1.0)
    assert lead == 90.0  # the physics EDS margin, not a retrospective crossing


# -- end-to-end replay (the proof; slow — recomputes envelope sweeps) -----------

@pytest.fixture(scope="module")
def loop():
    return run_twin_loop(_MET, _TRACK)


def test_loop_covers_the_track(loop):
    assert len(loop) == len(_TRACK)
    for p in loop:
        assert p["go_no_go"] in ("GO", "CAUTION", "NO-GO")
        assert "lead_time_s" in p


def test_rolling_transition_go_to_nogo(loop):
    labels = [p["go_no_go"] for p in loop]
    assert labels[0] == "GO", labels                 # calm, on station
    assert "NO-GO" in labels, labels                 # the drift-off / high-sea excursion
    # the vessel recovers by the end of the drift-then-recover scenario
    assert labels[-1] in ("GO", "CAUTION"), labels
    # the peak-sea step (Hs 2.0 > ceiling, index 5) must be NO-GO (seastate-domain guard)
    assert labels[5] == "NO-GO", labels


def test_loop_is_deterministic(loop):
    again = run_twin_loop(_MET, _TRACK)
    assert [p["go_no_go"] for p in again] == [p["go_no_go"] for p in loop]
    assert again == loop


def test_verdict_is_worst_of_the_served_gauges(loop):
    # composition fidelity: the served verdict equals the roll-up of the served gauges
    for p, met in zip(loop, _MET):
        expect, _ = _roll_verdict(p, wave_hs_m=float(met["wave_hs_m"]),
                                  hs_ceiling_m=twin_loop.STATIC_SCREEN_HS_CEILING_M)
        assert p["go_no_go"] == DISPLAY_LABEL[expect]
