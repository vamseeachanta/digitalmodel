"""Drilling-riser twin loop — the shared per-point composition engine + the rolling
GO/CAUTION/NO-GO integration (twin E #1377, epic #1372; the end-to-end proof).

``evaluate_point`` is the per-telemetry-point composition extracted verbatim from the
twin D monitor build (#1376) so the monitor page and this loop share ONE engine (DRY):
for a single snapshot + environmental condition it composes the five merged seams —
operability (#1283 atlas), flex-joint with the twin-B measured-tracking correction
(decision = max(corrected, raw)), a wellhead bending-MOMENT indicator (kN·m only, no
capacity/UC), and the twin-C drift-off screen — into one allow-listed track dict.

``run_twin_loop`` wires the metocean feed (#1282) into a ROLLING loop and rolls the
composed gauges into ONE conservative, fail-closed verdict per timestep:

  * the verdict is the WORST DecisionState across the gauges (NO_GO > MARGINAL > GO);
  * the operability gauge is consumed via the atlas's own ``light`` (OPERABLE / ESCALATE
    / INOPERABLE) — a missing / out-of-range operability value fails closed to NO_GO
    BEFORE any classify (never a silent GO); ESCALATE is indeterminate, never a clean
    CAUTION;
  * the atlas is FROZEN at its build seastate — a rolling ``wave_hs`` above the build Hs
    puts the operability gauge OUT OF ITS DOMAIN, so the verdict fails closed (the
    seastate-domain guard) rather than trusting a frozen operability call;
  * drift-off is gated on the lead-time MAGNITUDE (a negative EDS window is NO_GO), not
    the categorical status alone;
  * ``lead_time_s`` is the drift-off physics projection only (``lead_time_margin_s``) —
    this is a replay of recorded telemetry, not a forecast, so no retrospective
    first-crossing is claimed.

Limits stay the cited getters (via the atlas config's criteria); the verdict carries the
screening-tier disclaimer. Deterministic + offline (committed atlas + synthetic fixtures).
"""
from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Iterable, Mapping, NamedTuple

import yaml

from digitalmodel.decision_spine import DISPLAY_LABEL, _classify
from digitalmodel.drilling_riser import operability_atlas as oa
from digitalmodel.drilling_riser.conductor_response import solve_conductor_moment
from digitalmodel.drilling_riser.drift_off import drift_off_screen
from digitalmodel.drilling_riser.drift_off import load_config as load_drift_config
from digitalmodel.drilling_riser.envelope import (
    ConductorInput,
    CurrentProfile,
    EnvelopeCriteria,
    RiserSection,
)
from digitalmodel.drilling_riser.operability_screening import screen_operability
from digitalmodel.drilling_riser.response_correction import (
    FlexjointModels,
    correct_flexjoint_response,
    fit_flexjoint_models,
    flexjoint_utilisation,
)
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.drilling_riser.telemetry_inputs import (
    TelemetrySnapshot,
    parse_snapshots,
    snapshot_to_offset_pct,
)
from digitalmodel.marine_ops.installation.go_no_go import DecisionState
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions

#: Severity ordering for the worst-of roll-up.
_SEVERITY = {DecisionState.GO: 0, DecisionState.MARGINAL: 1, DecisionState.NO_GO: 2}


def _num(x):
    """JSON-safe number: inf/nan -> None (station-held time-to-limit is 'no drift')."""
    if x is None or (isinstance(x, float) and (math.isinf(x) or math.isnan(x))):
        return None
    return round(float(x), 3)


def evaluate_point(
    snap: TelemetrySnapshot,
    *,
    token: str,
    section: RiserSection,
    conductor: ConductorInput,
    condition,
    cur: float,
    water_depth_m: float,
    length_m: float,
    tension_n: float,
    criteria: EnvelopeCriteria,
    models: FlexjointModels,
    drift_cfg: dict,
    atlas_root,
    t0,
) -> dict:
    """Compose the five gauges for one telemetry snapshot into the allow-listed track dict.

    This is twin D's per-point loop body verbatim (byte-identical serving); the only
    parameterisation is the environmental ``condition`` / ``cur`` (fixed for the monitor
    demo, rolling for :func:`run_twin_loop`).
    """
    off_pct = snapshot_to_offset_pct(snap, water_depth_m=water_depth_m)
    # -- operability (twin A offset -> #1283 atlas) --
    scr = screen_operability(token, off_pct, cur, atlas_root=atlas_root)
    # -- physics prediction at the live point (for twin B + wellhead) --
    drag = CurrentProfile(surface_speed_mps=cur).drag_load_n_per_m(section.outer_diameter_m)
    pred = solve_static_response(length_m=length_m, top_offset_m=snap.vessel_offset_m,
                                 tension_n=tension_n, ei_nm2=section.ei_nm2, current_load_n_per_m=drag)
    pred_angle = max(abs(pred.angle_upper_deg), abs(pred.angle_lower_deg))
    # -- flex-joint (twin B: correct the prediction; decision = max(corrected, raw)) --
    corr = correct_flexjoint_response(pred, models)
    fj_uc = flexjoint_utilisation(corr.decision_static_angle_deg, criteria)
    meas = snap.measured
    meas_angle = None
    if meas is not None:
        vals = [abs(v) for v in (meas.flexjoint_angle_upper_deg, meas.flexjoint_angle_lower_deg) if v is not None]
        meas_angle = max(vals) if vals else None
    # -- wellhead bending-moment INDICATOR (kN·m only; no capacity, no UC) --
    wh = solve_conductor_moment(shear_n=pred.shear_lower_n, stand_off_m=conductor.stand_off_m,
                                soil_modulus_n_per_m2=conductor.soil_modulus_n_per_m2, ei_nm2=conductor.ei_nm2)
    wh_moment_knm = wh.max_moment_nm / 1000.0
    # -- drift-off (twin C: watch-circle + time-to-limit) --
    dr = drift_off_screen(snap.dp, condition, section=section, water_depth_m=water_depth_m, length_m=length_m,
                          tension_n=tension_n, criteria=criteria, config=drift_cfg, x0_m=snap.vessel_offset_m)
    watch_frac = snap.vessel_offset_m / dr.r_watch_m if dr.r_watch_m > 0 else None
    return {
        "t": round((snap.timestamp - t0).total_seconds(), 1),
        "offset_m": round(snap.vessel_offset_m, 3),
        "offset_pct": round(off_pct, 4),
        "operability_uc": round(float(scr.governing_utilisation), 6) if scr.governing_utilisation is not None else None,
        "light": scr.light,
        "flexjoint_uc": round(float(fj_uc), 6),
        "measured_angle_deg": None if meas_angle is None else round(meas_angle, 3),
        "predicted_angle_deg": round(pred_angle, 3),
        "corrected_angle_deg": round(corr.corrected_static_angle_deg, 3),
        "decision_angle_deg": round(corr.decision_static_angle_deg, 3),
        "wh_moment_knm": round(wh_moment_knm, 3),
        "r_watch_m": round(dr.r_watch_m, 3),
        "watch_frac": None if watch_frac is None else round(watch_frac, 4),
        "time_to_limit_s": _num(dr.time_to_limit_s),
        "lead_time_margin_s": _num(dr.lead_time_margin_s),
        "point_of_disconnect_m": _num(dr.point_of_disconnect_m),
        "drift_status": dr.status,
    }


# --- rolling GO/CAUTION/NO-GO integration -----------------------------------------

#: The #1283 operability atlas is a STATIC screen (built at zero-Hs — waves enter only
#: via the zero RAO term), so it does not see wave-induced flex-joint loading. This is
#: the Hs (m) up to which that static operability screen is treated as valid; a rolling
#: ``wave_hs`` above it puts the operability gauge OUT OF ITS DOMAIN and the verdict fails
#: closed (the seastate-domain guard) rather than trusting a wave-blind operability call.
STATIC_SCREEN_HS_CEILING_M = 1.0


def _roll_verdict(point: Mapping, *, wave_hs_m: float, hs_ceiling_m: float):
    """Roll the composed gauges into ONE conservative DecisionState + physics lead time.

    The verdict is the WORST DecisionState across the gauges (fail-closed):
      * operability — the atlas ``light`` directly (OPERABLE->GO, INOPERABLE->NO_GO,
        ESCALATE->NO_GO when the value is missing/out-of-range else MARGINAL). None is
        normalised to fail-closed BEFORE any classify (never ``_classify(None, ...)``).
      * seastate-domain guard — a rolling Hs above the static-screen ceiling (or a
        missing / NaN Hs) -> NO_GO.
      * flex-joint — the twin-B decision UC via the shared ``_classify`` (caution 0.9).
      * drift-off — magnitude-gated: escalate, a non-positive OR a missing EDS window
        -> NO_GO; an open (finite, positive) window -> MARGINAL; station held -> GO.
    ``lead_time_s`` is the drift-off physics projection only (``lead_time_margin_s``);
    no retrospective first-crossing over the replay is claimed.
    """
    states = []

    light = point["light"]
    uc = point["operability_uc"]
    if light == "OPERABLE":
        states.append(DecisionState.GO)
    elif light == "INOPERABLE":
        states.append(DecisionState.NO_GO)
    else:  # ESCALATE — indeterminate; fail closed when there is no in-range value
        states.append(DecisionState.NO_GO if uc is None else DecisionState.MARGINAL)

    # seastate-domain guard: the wave-blind static operability screen is out of its
    # validated domain above the ceiling -> the verdict cannot be a trusted GO. A
    # missing / NaN rolling Hs (a dropped metocean sample) is treated as out-of-domain
    # too (fail-closed), never silently skipped.
    if not math.isfinite(wave_hs_m) or wave_hs_m > hs_ceiling_m + 1e-9:
        states.append(DecisionState.NO_GO)

    # flex-joint (twin-B decision UC = max(corrected, raw); trips before the raw atlas)
    states.append(_classify(float(point["flexjoint_uc"]), 0.9, 1.0))

    # drift-off, gated on the EDS lead-time MAGNITUDE, not the categorical status alone
    st = point["drift_status"]
    lm = point["lead_time_margin_s"]
    if st == "escalate":
        states.append(DecisionState.NO_GO)
    elif st == "station_held":
        states.append(DecisionState.GO)
    else:  # drift_off — fail-closed: only an OPEN (finite, positive) EDS window is
        # MARGINAL; a missing or non-positive window cannot be a trusted CAUTION.
        states.append(DecisionState.MARGINAL if (lm is not None and lm > 0.0) else DecisionState.NO_GO)

    worst = max(states, key=lambda s: _SEVERITY[s])
    return worst, point["lead_time_margin_s"]


class _Context(NamedTuple):
    token: str
    section: RiserSection
    conductor: ConductorInput
    wd: float
    length: float
    tension: float
    criteria: EnvelopeCriteria
    models: FlexjointModels
    drift_cfg: dict
    root: Path


def _load_context(atlas_root: Path | None = None) -> _Context:
    """Load the fixed composition inputs from the committed config + fixtures (offline).

    Criteria are SOURCED from the atlas's own config (not re-invented), so the drift-off
    watch-circle boundary can never disagree with the operability boundary.
    """
    repo = oa.REPO_ROOT
    src = repo / "src" / "digitalmodel" / "drilling_riser"
    fix = repo / "tests" / "drilling_riser" / "fixtures"
    mon = yaml.safe_load((src / "monitor_config.yml").read_text())
    atlas_cfg = yaml.safe_load((src / "operability_configs.yml").read_text())
    c = atlas_cfg["criteria"]
    criteria = EnvelopeCriteria(
        float(c["flexjoint_angle_mean_deg"]),
        float(c["flexjoint_angle_max_deg"]),
        float(c["von_mises_design_factor"]),
    )
    sc = mon["scenario"]
    cd = mon["conductor"]
    section = RiserSection(outer_diameter_m=float(sc["outer_diameter_m"]),
                           wall_thickness_m=float(sc["wall_thickness_m"]))
    conductor = ConductorInput(
        outer_diameter_m=float(cd["outer_diameter_m"]), wall_thickness_m=float(cd["wall_thickness_m"]),
        soil_modulus_n_per_m2=float(cd["soil_modulus_n_per_m2"]), stand_off_m=float(cd["stand_off_m"]),
    )
    train = json.loads((fix / "monitor_flexjoint_training.json").read_text())
    models = fit_flexjoint_models(
        measured_upper_deg=train["measured_upper_deg"], predicted_upper_deg=train["predicted_upper_deg"],
        measured_lower_deg=train["measured_lower_deg"], predicted_lower_deg=train["predicted_lower_deg"],
    )
    return _Context(
        token=sc["config"], section=section, conductor=conductor,
        wd=float(sc["water_depth_m"]), length=float(sc["length_m"]), tension=float(sc["tension_n"]),
        criteria=criteria, models=models, drift_cfg=load_drift_config(),
        root=(atlas_root or (repo / "atlases")),
    )


def run_twin_loop(
    metocean_records: Iterable[Mapping],
    telemetry_records: Iterable[Mapping],
    *,
    atlas_root: Path | None = None,
    hs_ceiling_m: float = STATIC_SCREEN_HS_CEILING_M,
) -> list[dict]:
    """Replay a telemetry track against a ROLLING metocean stream -> per-step verdicts.

    Deterministic + offline. ``metocean_records`` and ``telemetry_records`` are aligned by
    index (one metocean condition per telemetry snapshot). Each returned dict is the
    allow-listed track point plus ``go_no_go`` (the DISPLAY_LABEL string) + ``lead_time_s``
    (drift-off physics lead time, rounded). No physics is re-derived downstream — a page
    just plays these back.
    """
    ctx = _load_context(atlas_root)
    snaps = parse_snapshots(list(telemetry_records))
    mets = list(metocean_records)
    t0 = snaps[0].timestamp
    out = []
    for snap, met in zip(snaps, mets):
        hs = float(met["wave_hs_m"])
        cur = float(met["current_speed_mps"])
        condition = EnvironmentalConditions(
            wave_hs=hs, wave_tp=float(met["wave_tp_s"]), wave_direction=180.0,
            current_speed=cur, current_direction=180.0,
            wind_speed=float(met.get("wind_speed_mps", 8.0)), wind_direction=180.0,
        )
        point = evaluate_point(
            snap, token=ctx.token, section=ctx.section, conductor=ctx.conductor,
            condition=condition, cur=cur, water_depth_m=ctx.wd, length_m=ctx.length,
            tension_n=ctx.tension, criteria=ctx.criteria, models=ctx.models,
            drift_cfg=ctx.drift_cfg, atlas_root=ctx.root, t0=t0,
        )
        state, lead = _roll_verdict(point, wave_hs_m=hs, hs_ceiling_m=hs_ceiling_m)
        out.append({**point, "go_no_go": DISPLAY_LABEL[state], "lead_time_s": _num(lead)})
    return out
