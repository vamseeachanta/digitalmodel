#!/usr/bin/env python3
"""Build the drilling-riser LIVE operability/integrity MONITOR (twin D #1376, epic #1372).

Re-frames the merged #1284 static explorer into a LIVE monitoring surface: for a synthetic
demo telemetry track it composes, per point, five gauges from the merged twin A/B/C + #1283
+ #1282 seams and serves them as a self-contained public page:

  * operability margin  — ``operability_screening.screen_operability`` (committed atlas lookup)
  * watch-circle + drift-off time-to-limit — ``drift_off.drift_off_screen`` (twin C)
  * flex-joint (twin B) — ``response_correction`` (decision = max(corrected, raw); the LIVE
    measured / predicted / corrected angles are all surfaced)
  * wellhead bending-MOMENT INDICATOR — ``conductor_response.solve_conductor_moment`` (kN·m
    only; NO rated capacity, NO utilisation — a UC vs a fabricated capacity is misleading and a
    real capacity is licensed/client data)

Governance (mirrors #1284): every gauge is PRECOMPUTED in Python and PLAYED BACK by the page
(no drift-off physics or straddle rule re-derived in JS); the results json is a strict
enumerated ALLOW-list serving times / ratios / moment only (never absolute vessel forces,
effective mass, or seam provenance); the page is self-contained (single inline ``const DATA``,
zero ``://``); tokens are the reviewed generic allowlist; the criteria equal the atlas's cited
values (an edition bump fails the build); a full-provenance fingerprint binds every gauge source.

Deterministic: two runs produce byte-identical artifacts.
"""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path

import yaml

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
    correct_flexjoint_response,
    fit_flexjoint_models,
    flexjoint_utilisation,
)
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.drilling_riser.telemetry_inputs import parse_snapshots, snapshot_to_offset_pct
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.subsea.mooring_analysis.models import EnvironmentalConditions

REPO_ROOT = oa.REPO_ROOT
_SRC = REPO_ROOT / "src" / "digitalmodel" / "drilling_riser"
_FIX = REPO_ROOT / "tests" / "drilling_riser" / "fixtures"
_MONITOR_CFG = _SRC / "monitor_config.yml"
_TRACK_FIX = _FIX / "monitor_demo_track.json"
_TRAIN_FIX = _FIX / "monitor_flexjoint_training.json"
_ATLAS_CFG = _SRC / "operability_configs.yml"

_OUT_DIR = REPO_ROOT / "docs" / "api" / "drilling"
_JSON = _OUT_DIR / "operability-monitor.results.json"
_HTML = _OUT_DIR / "operability-monitor.html"
SITE_PATH = "drilling/operability-monitor.html"

#: Fixed demo metocean condition (generic banded) — one condition drives BOTH the atlas
#: current and the drift-off force; the vessel offset varies per snapshot (a fixed sea, a
#: drifting vessel). current_speed is an atlas current knot.
_CONDITION = dict(
    wave_hs=1.0, wave_tp=8.0, wave_direction=180.0,
    current_speed=0.5, current_direction=180.0, wind_speed=8.0, wind_direction=180.0,
)

#: Banded generic conductor geometry (moment indicator only). Enforced by the banding test.
_COND_ALLOWED_OD_M = (0.762,)
_COND_ALLOWED_WT_M = (0.0254,)
_COND_BAND = {"soil_modulus_n_per_m2": 1_000_000.0, "stand_off_m": 5.0}


def _sha(obj) -> str:
    return hashlib.sha256(json.dumps(obj, sort_keys=True).encode()).hexdigest()


def _num(x):
    """JSON-safe number: inf/nan -> None (station-held time-to-limit is 'no drift')."""
    if x is None or (isinstance(x, float) and (math.isinf(x) or math.isnan(x))):
        return None
    return round(float(x), 3)


def _criteria_from_atlas_config() -> tuple[EnvelopeCriteria, list[dict]]:
    """Criteria + standards SOURCED from the atlas's own config (not re-invented), so the
    watch-circle boundary can never disagree with the operability boundary."""
    cfg = yaml.safe_load(_ATLAS_CFG.read_text())
    c = cfg["criteria"]
    crit = EnvelopeCriteria(
        float(c["flexjoint_angle_mean_deg"]),
        float(c["flexjoint_angle_max_deg"]),
        float(c["von_mises_design_factor"]),
    )
    standards = [{"id": s["id"], "edition": str(s["edition"])} for s in cfg["provenance"]["standards"]]
    return crit, standards


def build_results(atlas_root: Path | None = None) -> dict:
    root = atlas_root or (REPO_ROOT / "atlases")
    atlas = Atlas.load(root, oa.BASENAME)
    mon = yaml.safe_load(_MONITOR_CFG.read_text())
    sc = mon["scenario"]
    cd = mon["conductor"]
    criteria, standards = _criteria_from_atlas_config()

    token = sc["config"]
    wd = float(sc["water_depth_m"])
    length = float(sc["length_m"])
    tension = float(sc["tension_n"])
    section = RiserSection(outer_diameter_m=float(sc["outer_diameter_m"]),
                           wall_thickness_m=float(sc["wall_thickness_m"]))
    conductor = ConductorInput(
        outer_diameter_m=float(cd["outer_diameter_m"]), wall_thickness_m=float(cd["wall_thickness_m"]),
        soil_modulus_n_per_m2=float(cd["soil_modulus_n_per_m2"]), stand_off_m=float(cd["stand_off_m"]),
    )
    condition = EnvironmentalConditions(**_CONDITION)
    cur = float(condition.current_speed)
    drift_cfg = load_drift_config()

    train = json.loads(_TRAIN_FIX.read_text())
    models = fit_flexjoint_models(
        measured_upper_deg=train["measured_upper_deg"], predicted_upper_deg=train["predicted_upper_deg"],
        measured_lower_deg=train["measured_lower_deg"], predicted_lower_deg=train["predicted_lower_deg"],
    )

    snaps = parse_snapshots(json.loads(_TRACK_FIX.read_text())["records"])
    t0 = snaps[0].timestamp
    track = []
    for snap in snaps:
        off_pct = snapshot_to_offset_pct(snap, water_depth_m=wd)
        # -- operability (twin A offset -> #1283 atlas) --
        scr = screen_operability(token, off_pct, cur, atlas_root=root)
        # -- physics prediction at the live point (for twin B + wellhead) --
        drag = CurrentProfile(surface_speed_mps=cur).drag_load_n_per_m(section.outer_diameter_m)
        pred = solve_static_response(length_m=length, top_offset_m=snap.vessel_offset_m,
                                     tension_n=tension, ei_nm2=section.ei_nm2, current_load_n_per_m=drag)
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
        dr = drift_off_screen(snap.dp, condition, section=section, water_depth_m=wd, length_m=length,
                              tension_n=tension, criteria=criteria, config=drift_cfg, x0_m=snap.vessel_offset_m)
        watch_frac = snap.vessel_offset_m / dr.r_watch_m if dr.r_watch_m > 0 else None
        track.append({
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
        })

    # scenario config's atlas grid (the heatmap backdrop for the watch-circle-in-envelope view)
    offset_axis = [ax for ax in atlas.axes if ax.name == "offset_pct"][0].grid
    current_axis = [ax for ax in atlas.axes if ax.name == "current_speed_mps"][0].grid
    uc_grid, light_grid = [], []
    for o in offset_axis:
        ucr, ltr = [], []
        for c in current_axis:
            s = screen_operability(token, o, c, atlas_root=root)
            ucr.append(round(float(s.governing_utilisation), 6)); ltr.append(s.light)
        uc_grid.append(ucr); light_grid.append(ltr)

    prov = atlas.provenance
    scenario = {"config": token, "water_depth_m": wd, "current_speed_mps": cur}
    data = {
        "response": atlas.response,
        "atlas_id": atlas.atlas_id,
        "content_fingerprint": prov.get("content_fingerprint"),
        "tier": "quasi_static_screening",
        "offset_pct": [float(v) for v in offset_axis],
        "current_speed_mps": [float(v) for v in current_axis],
        "configs": {token: {"uc": uc_grid, "light": light_grid}},
        "max_rel_error": round(float(atlas.validation.get("max_rel_error", 0.0)), 6),
        "standards": standards,
        "scenario": scenario,
        "track": track,
    }
    data["fingerprint"] = _sha({
        "atlas_fp": prov.get("content_fingerprint"),
        "criteria": [criteria.flexjoint_angle_mean_deg, criteria.flexjoint_angle_max_deg,
                     criteria.von_mises_design_factor],
        "standards": standards,
        "training": _sha(train),
        "conductor": cd,
        "scenario": scenario,
        "track": _sha(track),
    })[:16]
    return data


def render_html(data: dict) -> str:
    return _TEMPLATE.replace("__DATA__", json.dumps(data, indent=2, sort_keys=False))


def main() -> int:
    data = build_results()
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    _JSON.write_text(json.dumps(data, indent=2, sort_keys=False) + "\n")
    html = render_html(data)
    if "://" in html:
        raise SystemExit("monitor HTML contains '://' — external reference not allowed")
    _HTML.write_text(html)
    print(f"wrote {_JSON.relative_to(REPO_ROOT)} + {_HTML.relative_to(REPO_ROOT)} "
          f"({len(data['track'])} track points, atlas {data['atlas_id']}, fp {data['fingerprint']})")
    return 0


_TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Drilling-riser operability &amp; integrity monitor</title>
<style>
  :root{
    --plane:#f9f9f7;--surface:#fcfcfb;--ink:#0b0b0b;--ink2:#52514e;--muted:#898781;
    --grid:#e1e0d9;--ring:rgba(11,11,11,.10);
    --good:#2a8a4a;--warn:#c98a00;--serious:#e34948;--escalate:#7a5cd0;--dot:#0b0b0b;
  }
  @media (prefers-color-scheme:dark){:root{
    --plane:#0d0d0d;--surface:#1a1a19;--ink:#fff;--ink2:#c3c2b7;--muted:#898781;
    --grid:#2c2c2a;--ring:rgba(255,255,255,.10);
    --good:#43b56a;--warn:#e0a52b;--serious:#e66767;--escalate:#a98fe6;--dot:#fff;}}
  :root[data-theme=dark]{--plane:#0d0d0d;--surface:#1a1a19;--ink:#fff;--ink2:#c3c2b7;--muted:#898781;
    --grid:#2c2c2a;--ring:rgba(255,255,255,.10);--good:#43b56a;--warn:#e0a52b;--serious:#e66767;--escalate:#a98fe6;--dot:#fff;}
  :root[data-theme=light]{--plane:#f9f9f7;--surface:#fcfcfb;--ink:#0b0b0b;--ink2:#52514e;--muted:#898781;
    --grid:#e1e0d9;--ring:rgba(11,11,11,.10);--good:#2a8a4a;--warn:#c98a00;--serious:#e34948;--escalate:#7a5cd0;--dot:#0b0b0b;}
  *{box-sizing:border-box}
  body{margin:0;background:var(--plane);color:var(--ink);font:15px/1.5 system-ui,-apple-system,"Segoe UI",sans-serif}
  .wrap{max-width:960px;margin:0 auto;padding:32px 20px 64px}
  header{border-bottom:1px solid var(--grid);padding-bottom:18px;margin-bottom:22px}
  .eyebrow{font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--muted);font-weight:600}
  h1{font-size:25px;margin:6px 0 8px;font-weight:650;letter-spacing:-.01em}
  .sub{color:var(--ink2);max-width:64ch}
  .meta{display:flex;flex-wrap:wrap;gap:8px 16px;margin-top:12px;font-size:12.5px;color:var(--muted)}
  .meta code{font-family:ui-monospace,Menlo,monospace;color:var(--ink2)}
  .panel{background:var(--surface);border:1px solid var(--ring);border-radius:12px;padding:18px;margin-bottom:18px}
  .ctl{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:6px}
  .ctl input[type=range]{flex:1;min-width:220px}
  button.pb{font:inherit;font-size:13px;padding:6px 12px;border:1px solid var(--ring);border-radius:999px;
    background:transparent;color:var(--ink2);cursor:pointer}
  button.pb:focus-visible{outline:2px solid var(--good);outline-offset:2px}
  .tstamp{font-variant-numeric:tabular-nums;color:var(--muted);font-size:12.5px;min-width:64px}
  .gauges{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:14px}
  .g{border:1px solid var(--ring);border-radius:10px;padding:14px}
  .g .lab{font-size:11.5px;letter-spacing:.09em;text-transform:uppercase;color:var(--muted);font-weight:600}
  .g .val{font-size:24px;font-weight:650;margin-top:3px;font-variant-numeric:tabular-nums;letter-spacing:-.01em}
  .g .unit{font-size:13px;color:var(--muted);font-weight:500}
  .g .sub2{font-size:12.5px;color:var(--ink2);margin-top:4px}
  .chip{display:inline-flex;align-items:center;gap:6px;font-size:12px;font-weight:600;padding:2px 9px;border-radius:999px;
    border:1px solid var(--ring)}
  .chip .ic{width:9px;height:9px;border-radius:2px;flex:none}
  .bar{height:8px;border-radius:5px;background:var(--grid);margin-top:9px;overflow:hidden}
  .bar>span{display:block;height:100%;border-radius:5px}
  canvas{display:block;width:100%;height:auto;touch-action:none}
  .row{display:grid;grid-template-columns:1.1fr 1fr;gap:16px;align-items:start}
  @media(max-width:640px){.row{grid-template-columns:1fr}}
  .note{font-size:12.5px;color:var(--muted);margin-top:12px;max-width:66ch}
  table{border-collapse:collapse;width:100%;font-size:12px;margin-top:10px;font-variant-numeric:tabular-nums}
  th,td{border:1px solid var(--grid);padding:3px 6px;text-align:right}
  th{color:var(--muted);font-weight:600}
  [hidden]{display:none!important}
</style>
</head>
<body>
<div class="wrap">
  <header>
    <div class="eyebrow">Drilling riser &middot; digital twin</div>
    <h1>Operability &amp; integrity monitor</h1>
    <p class="sub">Live-style playback of a generic drilling-riser scenario: telemetry offset +
      metocean drive the operability margin, the flex-joint response (physics + measured-tracking
      correction), a wellhead bending-moment indicator, and the drift-off time-to-limit if
      thrusters were lost now. Every value is pre-computed by the analytical seams and played
      back &mdash; the page does no physics. Limits stay cited code values; the correction only
      ever tightens a verdict.</p>
    <div class="meta" id="meta"></div>
  </header>

  <div class="panel">
    <div class="ctl">
      <button class="pb" id="play" aria-label="Play or pause">&#9654; Play</button>
      <input id="scrub" type="range" min="0" value="0" step="1" aria-label="Scrub the telemetry track">
      <span class="tstamp" id="tlab">t = 0 s</span>
    </div>
    <div class="row">
      <div>
        <div class="lab" style="font-size:11.5px;letter-spacing:.09em;text-transform:uppercase;color:var(--muted);font-weight:600;margin-bottom:6px">Watch circle</div>
        <canvas id="wc" width="360" height="300" role="img" aria-label="Watch circle: vessel offset versus allowable radius"></canvas>
      </div>
      <div>
        <div class="lab" style="font-size:11.5px;letter-spacing:.09em;text-transform:uppercase;color:var(--muted);font-weight:600;margin-bottom:6px">Offset track</div>
        <canvas id="tl" width="360" height="150" role="img" aria-label="Vessel offset over time"></canvas>
        <div class="note" id="wcnote" style="margin-top:8px"></div>
      </div>
    </div>
  </div>

  <div class="panel">
    <div class="gauges">
      <div class="g">
        <div class="lab">Operability margin</div>
        <div class="val" id="opv">&mdash;</div>
        <div class="bar"><span id="opbar"></span></div>
        <div class="sub2"><span class="chip" id="opchip"><span class="ic" id="opic"></span><span id="oplt"></span></span></div>
      </div>
      <div class="g">
        <div class="lab">Flex-joint (twin B)</div>
        <div class="val" id="fjv">&mdash; <span class="unit">UC</span></div>
        <div class="bar"><span id="fjbar"></span></div>
        <div class="sub2" id="fjsub"></div>
      </div>
      <div class="g">
        <div class="lab">Wellhead moment indicator</div>
        <div class="val" id="whv">&mdash; <span class="unit">kN&middot;m</span></div>
        <div class="sub2">bending-moment indicator only &mdash; not a utilisation, not a limit</div>
      </div>
      <div class="g">
        <div class="lab">Drift-off (thrusters lost now)</div>
        <div class="val" id="dov">&mdash;</div>
        <div class="sub2"><span class="chip" id="dochip"><span class="ic" id="doic"></span><span id="dolt"></span></span></div>
        <div class="sub2" id="dosub"></div>
      </div>
    </div>
    <p class="note" id="disc"></p>
  </div>
</div>

<script>
const DATA = __DATA__;
const TRACK = DATA.track, OFF = DATA.offset_pct, CUR = DATA.current_speed_mps;
const CFG = DATA.scenario.config, GRID = DATA.configs[CFG];
let idx = 0, playing = false, timer = null;

function css(v){return getComputedStyle(document.documentElement).getPropertyValue(v).trim();}
function statusColor(light){
  if(light==='OPERABLE') return css('--good');
  if(light==='ESCALATE') return css('--escalate');
  return css('--serious'); // INOPERABLE
}
function driftColor(st){
  if(st==='station_held') return css('--good');
  if(st==='escalate') return css('--escalate');
  return css('--warn'); // drift_off
}
function fmtT(s){ if(s===null) return '&infin;'; const m=Math.floor(s/60), r=Math.round(s%60); return m+':'+String(r).padStart(2,'0'); }

// --- watch-circle plan: allowable radius ring + vessel-offset dot (linear scale, no physics) ---
const wc=document.getElementById('wc'), wctx=wc.getContext('2d');
function drawWC(p){
  const dpr=window.devicePixelRatio||1, W=wc.clientWidth||360, H=300;
  wc.width=W*dpr; wc.height=H*dpr; wctx.setTransform(dpr,0,0,dpr,0,0); wctx.clearRect(0,0,W,H);
  const cx=W/2, cy=H/2, R=Math.min(W,H)/2-24;
  const rw=p.r_watch_m||1; const frac=p.watch_frac===null?1:Math.min(1.15,p.watch_frac);
  // allowable ring
  wctx.strokeStyle=css('--grid'); wctx.lineWidth=2; wctx.beginPath(); wctx.arc(cx,cy,R,0,Math.PI*2); wctx.stroke();
  // graded band toward the ring
  wctx.strokeStyle=statusColor(p.light); wctx.lineWidth=2; wctx.beginPath(); wctx.arc(cx,cy,R*Math.min(1,frac),0,Math.PI*2); wctx.stroke();
  // vessel dot (heading up; radius = frac of allowable)
  const dr=R*frac; const dx=cx, dy=cy-dr;
  wctx.fillStyle=css('--dot'); wctx.beginPath(); wctx.arc(dx,dy,5,0,Math.PI*2); wctx.fill();
  wctx.strokeStyle=css('--surface'); wctx.lineWidth=2; wctx.stroke();
  wctx.fillStyle=css('--muted'); wctx.font='11px system-ui'; wctx.textAlign='center';
  wctx.fillText('allowable '+rw.toFixed(0)+' m', cx, cy+R+16);
}
// --- offset timeline ---
const tl=document.getElementById('tl'), tctx=tl.getContext('2d');
function drawTL(){
  const dpr=window.devicePixelRatio||1, W=tl.clientWidth||360, H=150;
  tl.width=W*dpr; tl.height=H*dpr; tctx.setTransform(dpr,0,0,dpr,0,0); tctx.clearRect(0,0,W,H);
  const P=24, maxO=Math.max.apply(null,TRACK.map(p=>p.offset_m))*1.1||1, T=TRACK.length-1||1;
  tctx.strokeStyle=css('--grid'); tctx.lineWidth=1; tctx.beginPath(); tctx.moveTo(P,H-P); tctx.lineTo(W-6,H-P); tctx.stroke();
  tctx.strokeStyle=css('--ink2'); tctx.lineWidth=2; tctx.beginPath();
  TRACK.forEach((p,i)=>{const x=P+(W-P-6)*i/T, y=(H-P)-(H-P-8)*p.offset_m/maxO; i?tctx.lineTo(x,y):tctx.moveTo(x,y);});
  tctx.stroke();
  const cx=P+(W-P-6)*idx/T, cp=TRACK[idx], cy=(H-P)-(H-P-8)*cp.offset_m/maxO;
  tctx.fillStyle=statusColor(cp.light); tctx.beginPath(); tctx.arc(cx,cy,5,0,Math.PI*2); tctx.fill();
  tctx.fillStyle=css('--muted'); tctx.font='11px system-ui'; tctx.textAlign='left'; tctx.fillText('offset (m)',P,14);
}

function chip(chipEl,icEl,ltEl,label,color){ icEl.style.background=color; ltEl.textContent=label; chipEl.style.color=css('--ink2'); }
function setBar(el,frac,color){ el.style.width=Math.max(0,Math.min(1,frac))*100+'%'; el.style.background=color; }

function render(){
  const p=TRACK[idx];
  document.getElementById('scrub').value=idx;
  document.getElementById('tlab').innerHTML='t = '+p.t+' s';
  drawWC(p); drawTL();
  document.getElementById('wcnote').innerHTML='Vessel offset <b>'+p.offset_m.toFixed(0)+' m</b> ('+
    p.offset_pct.toFixed(1)+'% WD) of allowable <b>'+p.r_watch_m.toFixed(0)+' m</b>'+
    (p.watch_frac!==null?' &middot; '+(p.watch_frac*100).toFixed(0)+'% of watch circle':'');
  // operability
  const margin=p.operability_uc===null?null:(1-p.operability_uc);
  document.getElementById('opv').textContent=margin===null?'—':(margin>=0?'+':'')+margin.toFixed(2);
  setBar(document.getElementById('opbar'), p.operability_uc===null?1:p.operability_uc, statusColor(p.light));
  chip(document.getElementById('opchip'),document.getElementById('opic'),document.getElementById('oplt'),'operability: '+p.light,statusColor(p.light));
  // flex-joint
  document.getElementById('fjv').innerHTML=p.flexjoint_uc.toFixed(3)+' <span class="unit">UC</span>';
  setBar(document.getElementById('fjbar'), p.flexjoint_uc, p.flexjoint_uc<=1?css('--good'):css('--serious'));
  document.getElementById('fjsub').innerHTML='measured '+(p.measured_angle_deg===null?'—':p.measured_angle_deg.toFixed(2)+'&deg;')+
    ' &middot; predicted '+p.predicted_angle_deg.toFixed(2)+'&deg; &middot; corrected '+p.corrected_angle_deg.toFixed(2)+'&deg; (decision = max)';
  // wellhead moment indicator
  document.getElementById('whv').innerHTML=p.wh_moment_knm.toFixed(0)+' <span class="unit">kN&middot;m</span>';
  // drift-off
  document.getElementById('dov').innerHTML=p.time_to_limit_s===null?'station held':fmtT(p.time_to_limit_s)+' <span class="unit">to limit</span>';
  chip(document.getElementById('dochip'),document.getElementById('doic'),document.getElementById('dolt'),'drift-off: '+p.drift_status,driftColor(p.drift_status));
  document.getElementById('dosub').innerHTML=p.time_to_limit_s===null?'net drift force does not exceed station-keeping':
    ('disconnect by '+(p.point_of_disconnect_m===null?'—':p.point_of_disconnect_m.toFixed(0)+' m')+
     ' &middot; EDS lead '+(p.lead_time_margin_s===null?'—':p.lead_time_margin_s.toFixed(0)+' s'));
}
function step(d){ idx=(idx+d+TRACK.length)%TRACK.length; render(); }
document.getElementById('scrub').max=TRACK.length-1;
document.getElementById('scrub').oninput=e=>{idx=parseInt(e.target.value,10);render();};
document.getElementById('play').onclick=function(){
  playing=!playing; this.innerHTML=playing?'&#10073;&#10073; Pause':'&#9654; Play';
  if(playing){ timer=setInterval(()=>{idx=(idx+1)%TRACK.length; if(idx===0&&!playing){} render();},900); }
  else { clearInterval(timer); }
};
(function init(){
  const std=DATA.standards.map(s=>s.id+' ('+s.edition+')').join(' &middot; ');
  document.getElementById('meta').innerHTML='Scenario <code>'+CFG+'</code> &middot; standards '+std+
    ' &middot; atlas <code>'+DATA.atlas_id+'</code> &middot; fingerprint <code>'+DATA.fingerprint+'</code>';
  document.getElementById('disc').innerHTML='Screening-tier digital twin (static analytical operability + '+
    'quasi-static drift-off) &mdash; not a certified deliverable. Operability escalate (atlas boundary) and '+
    'drift-off escalate (no time to disconnect) are shown as separate states. The wellhead value is a bending-'+
    'moment indicator only, from generic conductor geometry &mdash; not a utilisation against a rated limit. Flex-joint limits are '+
    'the cited code values; the twin-B correction only tightens the verdict (decision = max(corrected, raw)).';
  render();
})();
window.addEventListener('resize',render);
</script>
</body>
</html>
"""


if __name__ == "__main__":
    raise SystemExit(main())
