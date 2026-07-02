# ABOUTME: Generate the riser-joint flaw-acceptance explorer (docs/api/ffs/) —
# ABOUTME: envelope/placement tables precomputed in Python, embedded as JSON.
"""Build the riser-joint acceptance explorer (#1292).

All numbers are computed here by :mod:`digitalmodel.asset_integrity.riser_joint_ffs`
and embedded as JSON — the page's JS only selects and draws, so the chart is the
Python result by construction (no physics duplicated in JS).

Run:  python3 -m scripts... or  uv run python scripts/ffs/build_riser_joint_explorer.py
"""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

from digitalmodel.asset_integrity.riser_joint_ffs import (
    acceptable_water_depth_ft,
    fleet_rollup,
    level1_flaw_envelope,
    place_joint,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "ffs" / "riser-joint-acceptance-explorer.html"
_FIX = _REPO / "tests" / "asset_integrity" / "test_data" / "real_inspection"

MM = 25.4
OD, WT, GRADE = 21.25, 0.875, "X80"  # the real fleet's main tube

PRESSURES = [500.0, 1500.0, 3000.0]
RATES_MM = [0.08, 0.12, 0.25]        # mm/yr — span of the real register
DURATIONS = [0.0, 1.0, 3.0, 5.0]

# the three fixture joints: measured min wall (mm) from the committed grids
JOINTS = {
    "RJ-101 (severe)": {"wt_min_mm": 15.02, "scan": "Box End"},
    "RJ-102 (moderate)": {"wt_min_mm": 15.11, "scan": "Box End"},
    "RJ-103 (mild)": {"wt_min_mm": 15.42, "scan": "Pin End"},
}


def build_tables() -> dict:
    envelopes = {}
    for p in PRESSURES:
        for cr in RATES_MM:
            for T in DURATIONS:
                for region in ("base", "weld"):
                    env = level1_flaw_envelope(
                        OD, WT, GRADE, p, region=region,
                        corrosion_rate_in_per_yr=cr / MM, campaign_years=T)
                    key = f"{int(p)}|{cr:g}|{T:g}|{region}"
                    envelopes[key] = {
                        "depth_mm": [round(d * MM, 2) for d in env["depth_in"]],
                        "length_mm": [round(v * MM, 1)
                                      for v in env["max_acceptable_length_in"]],
                    }

    # collapse-limited depth vs measured min wall (evacuated basis)
    wt_mm = [14.0 + 0.25 * i for i in range(38)]  # 14.0 .. 23.25 mm
    depth_curve = {
        "wt_min_mm": [round(w, 2) for w in wt_mm],
        "depth_ft": [round(acceptable_water_depth_ft(OD, w / MM, GRADE), 0)
                     for w in wt_mm],
    }

    register = pd.read_csv(_FIX / "gml_results_register.csv")
    main_life = (register[register["component"] == "Main"]
                 .groupby("joint_id")["min_life_years"].min())

    placements = {}
    for name, j in JOINTS.items():
        jid = name.split()[0]
        for depth in (2500.0, 5000.0):
            for T in (1.0, 3.0, 5.0):
                pl = place_joint(
                    jid, float(main_life[jid]), j["wt_min_mm"] / MM,
                    od_in=OD, grade=GRADE,
                    campaign_water_depth_ft=depth, campaign_years=T)
                placements[f"{jid}|{int(depth)}|{T:g}"] = {
                    "joint": name, "min_life_years": round(pl.min_life_years, 2),
                    "acceptable_depth_ft": pl.acceptable_depth_ft,
                    "zones": pl.eligible_zones, "verdict": pl.verdict,
                    "reason": pl.reason,
                }

    flaws = pd.read_csv(_FIX / "flaw_register.csv")
    flaw_pts = [
        {"joint": r.joint_id, "weld": r.weld, "length_mm": float(r.flaw_length_mm),
         "depth_mm": float(r.flaw_depth_mm), "type": r.flaw_type}
        for r in flaws.itertuples()
    ]

    roll = fleet_rollup(register, component="Main", campaign_years=3.0,
                        n_campaigns=4)
    return {
        "geometry": {"od_in": OD, "wt_in": WT, "wt_mm": round(WT * MM, 3),
                     "grade": GRADE},
        "pressures": [int(p) for p in PRESSURES],
        "rates_mm": RATES_MM, "durations": DURATIONS,
        "envelopes": envelopes, "depth_curve": depth_curve,
        "joints": {k.split()[0]: dict(v, label=k) for k, v in JOINTS.items()},
        "placements": placements, "flaws": flaw_pts, "rollup": roll,
    }


_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Riser-Joint Flaw-Acceptance Explorer — drilling riser FFS</title>
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
<style>
  :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
        --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc}
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
       color:var(--ink);line-height:1.5;padding:26px 30px 60px;max-width:1180px;margin:0 auto}
  h1{font-size:24px;color:var(--navy);letter-spacing:-.3px}
  .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:900px}
  .controls{display:flex;flex-wrap:wrap;gap:14px;background:var(--panel);
            border:1px solid var(--line);border-radius:12px;padding:12px 16px;margin-bottom:16px}
  .ctl label{display:block;font-size:11px;font-weight:700;color:var(--muted);
             text-transform:uppercase;letter-spacing:.4px;margin-bottom:3px}
  select{font-size:13.5px;padding:5px 8px;border:1px solid var(--line);border-radius:8px;
         background:var(--soft);color:var(--ink)}
  .row{display:grid;grid-template-columns:3fr 2fr;gap:16px}
  .panel{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:10px}
  .panel h2{font-size:13px;color:var(--navy);padding:4px 8px 8px}
  #verdict{font-size:13.5px;padding:10px 14px}
  #verdict .v{font-size:16px;font-weight:800}
  #verdict .ACCEPT{color:#1f9d57}.RESTRICTED{color:#b7791f}.REPAIR{color:#c0392b}
  #verdict table{width:100%;border-collapse:collapse;font-size:12.5px;margin-top:8px}
  #verdict td{padding:3px 6px;border-bottom:1px solid var(--line)}
  .note{color:var(--muted);font-size:12px;margin-top:14px;max-width:960px}
  .note b{color:var(--navy)}
  a{color:var(--teal)}
  @media(max-width:900px){.row{grid-template-columns:1fr}}
</style>
</head>
<body>
<h1>Riser-Joint Flaw-Acceptance Explorer</h1>
<p class="sub">Level-1 acceptance chart for a marine drilling-riser main tube
(__GEOM__) — maximum acceptable surface-flaw <b>length vs depth</b> from the
validated Modified B31G remaining-strength engine, with base-metal / weld depth
caps and a campaign-end growth allowance. Points are the real (anonymized)
weld-flaw register; the side panel places the three real inspected joints in the
string. Every number is precomputed in Python
(<code>digitalmodel.asset_integrity.riser_joint_ffs</code>) — the page only draws.</p>

<div class="controls">
  <div class="ctl"><label>Design pressure (psi)</label><select id="pres"></select></div>
  <div class="ctl"><label>Corrosion rate (mm/yr)</label><select id="rate"></select></div>
  <div class="ctl"><label>Campaign duration (yr)</label><select id="dur"></select></div>
  <div class="ctl"><label>Joint (real inspection)</label><select id="joint"></select></div>
  <div class="ctl"><label>Campaign water depth (ft)</label><select id="wdep">
    <option>2500</option><option selected>5000</option></select></div>
</div>

<div class="row">
  <div class="panel"><h2>Acceptable surface-flaw dimensions — campaign start vs end</h2>
    <div id="chart" style="height:430px"></div></div>
  <div class="panel"><h2>String placement & collapse-limited depth</h2>
    <div id="verdict"></div>
    <div id="depthchart" style="height:250px"></div></div>
</div>

<p class="note"><b>Basis.</b> Envelope: Modified B31G (0.85 dL) safe pressure ≥ design
pressure, inverted by bisection; depth caps 0.85·WT (base metal) / 0.60·WT (weld) per
the source integrity program; campaign-end curve assesses each depth at
d + rate × duration. Collapse depth: API RP 1111 elastic/yield interaction at the
measured minimum wall, design factor 0.70, fully-evacuated riser (most conservative).
Zone margins (top/bottom 3×, mid 1×) are practice parameters pending the
crack-growth clock (digitalmodel#1270). Weld-region strength currently reuses the
metal-loss engine inside the tighter cap — fracture-mechanics weld envelopes arrive
with Part 9 (#1270).</p>

<script>
const DATA = __DATA__;
const $ = id => document.getElementById(id);
function fill(sel, vals){ $(sel).innerHTML = vals.map(v=>`<option>${v}</option>`).join(""); }
fill("pres", DATA.pressures); fill("rate", DATA.rates_mm); fill("dur", DATA.durations);
fill("joint", Object.keys(DATA.joints));
$("pres").value = "3000"; $("rate").value = "0.25"; $("dur").value = "3";

const C = {base:"#0B3D91", weld:"#0f8a7e", baseEnd:"#7d97c9", weldEnd:"#7fc4bd"};

function envTrace(key, name, color, dashed){
  const e = DATA.envelopes[key];
  const pts = e.length_mm.map((L,i)=>[L, e.depth_mm[i]]).filter(p=>p[0]>0);
  return {x: pts.map(p=>p[0]), y: pts.map(p=>p[1]), name, mode:"lines",
          line:{color, width:2.5, dash: dashed?"dash":"solid"}};
}

function draw(){
  const p=$("pres").value, r=$("rate").value, T=$("dur").value;
  const traces = [
    envTrace(`${p}|${r}|0|base`, "base metal — campaign start", C.base, false),
    envTrace(`${p}|${r}|${T}|base`, "base metal — campaign end", C.baseEnd, true),
    envTrace(`${p}|${r}|0|weld`, "weld — campaign start", C.weld, false),
    envTrace(`${p}|${r}|${T}|weld`, "weld — campaign end", C.weldEnd, true),
  ];
  const endKey = `${p}|${r}|${T}|weld`, env = DATA.envelopes[endKey];
  const accept = f => {
    // acceptable if under the weld campaign-end curve (register flaws are weld flaws)
    let ok=false;
    for(let i=0;i<env.depth_mm.length;i++)
      if(f.depth_mm<=env.depth_mm[i] && f.length_mm<=env.length_mm[i]) ok=true;
    return ok;
  };
  traces.push({x:DATA.flaws.map(f=>f.length_mm), y:DATA.flaws.map(f=>f.depth_mm),
    mode:"markers+text", name:"flaw register (real)", text:DATA.flaws.map(f=>f.joint),
    textposition:"top center", textfont:{size:10},
    marker:{size:11, symbol:"diamond",
            color:DATA.flaws.map(f=>accept(f)?"#1f9d57":"#c0392b"),
            line:{color:"#13233f",width:1}}});
  Plotly.newPlot("chart", traces, {
    margin:{l:60,r:10,t:10,b:45}, paper_bgcolor:"#fff", plot_bgcolor:"#fff",
    xaxis:{title:"flaw length (mm)", gridcolor:"#e5ebf5", rangemode:"tozero"},
    yaxis:{title:"flaw depth (mm)", gridcolor:"#e5ebf5",
           range:[0, DATA.geometry.wt_mm], autorange:false},
    legend:{orientation:"h", y:-0.18}, font:{size:12}
  }, {displayModeBar:false, responsive:true});
  verdict();
}

function verdict(){
  const jid=$("joint").value, d=$("wdep").value, T=$("dur").value;
  const key=`${jid}|${d}|${T==="0" ? "1" : T}`;
  const pl = DATA.placements[key] || DATA.placements[`${jid}|${d}|3`];
  const j = DATA.joints[jid];
  $("verdict").innerHTML = `
    <div><span class="v ${pl.verdict}">${pl.verdict}</span>
      &nbsp;<span style="color:#5b6b86">${pl.reason || "eligible zones: " +
        (pl.zones.join(", ") || "—")}</span></div>
    <table>
      <tr><td>joint</td><td><b>${pl.joint}</b> (${j.scan}, measured min wall
        ${j.wt_min_mm} mm)</td></tr>
      <tr><td>governing remaining life</td><td>${pl.min_life_years} yr</td></tr>
      <tr><td>collapse-qualified depth</td><td>${pl.acceptable_depth_ft.toLocaleString()} ft
        (evacuated basis)</td></tr>
      <tr><td>eligible string zones</td><td>${pl.zones.join(", ") || "none"}</td></tr>
    </table>`;
  const dc = DATA.depth_curve;
  Plotly.newPlot("depthchart", [
    {x:dc.wt_min_mm, y:dc.depth_ft, mode:"lines", name:"qualified depth",
     line:{color:"#0B3D91",width:2.5}},
    {x:[j.wt_min_mm], y:[pl.acceptable_depth_ft], mode:"markers", name:jid,
     marker:{size:11,color:"#c0392b",symbol:"diamond"}},
  ], {
    margin:{l:60,r:10,t:8,b:42}, paper_bgcolor:"#fff", plot_bgcolor:"#fff",
    xaxis:{title:"measured min wall (mm)", gridcolor:"#e5ebf5"},
    yaxis:{title:"qualified water depth (ft)", gridcolor:"#e5ebf5"},
    showlegend:false, font:{size:11.5}
  }, {displayModeBar:false, responsive:true});
}

["pres","rate","dur"].forEach(id=>$(id).addEventListener("change", draw));
["joint","wdep"].forEach(id=>$(id).addEventListener("change", verdict));
draw();
</script>
</body>
</html>
"""


def main() -> None:
    tables = build_tables()
    geom = (f"{tables['geometry']['od_in']}\" OD × {tables['geometry']['wt_in']}\" WT "
            f"{tables['geometry']['grade']}, the real inspected fleet")
    html = _TEMPLATE.replace("__DATA__", json.dumps(tables, separators=(",", ":")))
    html = html.replace("__GEOM__", geom)
    _OUT.write_text(html, encoding="utf-8")
    print(f"wrote {_OUT.relative_to(_REPO)} "
          f"({len(tables['envelopes'])} envelopes, {len(tables['placements'])} placements)")


if __name__ == "__main__":
    main()
