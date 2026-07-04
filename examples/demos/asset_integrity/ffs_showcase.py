# ABOUTME: Builds a self-contained interactive HTML showcase of the digitalmodel
# ABOUTME: Fitness-For-Service (FFS) decision layer (corroded pipe + plate + sufficiency).
"""Fitness-For-Service (FFS) decision-layer showcase.

A single self-contained HTML dashboard (Plotly inlined, works offline) that
demonstrates the validated FFS modules in :mod:`digitalmodel`:

* a **corroded-pipe acceptability explorer** -- pick a pipe preset and a
  remaining-strength method (ASME B31G, Modified B31G, RSTRENG effective-area,
  or DNV-RP-F101) and see the green/red acceptable design window over defect
  length and depth, with the safe = MAOP boundary picked out in bold and the
  B31G allowable-length curve overlaid;
* a **field measurement-sufficiency** card driven by the real Phase-1 engine on
  a representative localized thin-spot grid;
* a **plate metal-loss FFS** chart (buckling utilisation vs metal loss for an
  AH36 ship plate), and a **Level-3 escalation** callout.

Every number comes from the validated modules; nothing is re-implemented here.
This is a *field-screening* aid, not a replacement for detailed analysis.

Run:
    .venv/bin/python examples/demos/asset_integrity/ffs_showcase.py
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import pandas as pd
from plotly.offline import get_plotlyjs

from digitalmodel.asset_integrity.corroded_pipe import (
    SMYS_PSI,
    b31g_original,
    b31g_original_allowable_length,
    modified_b31g,
    rstreng_effective_area,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import (
    SMTS_PSI,
    dnv_f101_single_defect,
)
from digitalmodel.asset_integrity.assessment import (
    FFSRouter,
    GridParser,
    Level1Screener,
    Level2Engine,
    MeasurementSufficiency,
)
from digitalmodel.structural.structural_analysis.models import STEEL_AH36, PlateGeometry
from digitalmodel.structural.structural_analysis.plate_metal_loss_ffs import (
    assess_plate_uniform_loss,
    max_acceptable_loss,
)

OUT_DIR = Path(__file__).resolve().parent / "output"
HTML_PATH = OUT_DIR / "ffs_showcase.html"

COLORS = {
    "navy": "#1a365d", "orange": "#ed8936", "green": "#38a169",
    "red": "#e53e3e", "text": "#2d3748", "muted": "#718096",
}

# Pipe presets: (label, OD in, wt in, API 5L grade). MAOP for the acceptance
# window is the B31.8/Barlow design pressure 0.72 * (2 * SMYS * t / D).
PRESETS = [
    ("20in x 0.500in X52", 20.0, 0.500, "X52"),
    ("30in x 0.375in X52", 30.0, 0.375, "X52"),
    ("24in x 0.500in X65", 24.0, 0.500, "X65"),
]
METHODS = ["B31G", "Modified B31G", "RSTRENG", "DNV-RP-F101"]

DESIGN_FACTOR = 0.72  # B31.8 class-1 / DNV usage factor for the MAOP datum
SAFETY_FACTOR = 1.39  # B31G safe-pressure safety factor


def _maop_psi(D: float, t: float, smys: float) -> float:
    """Barlow design pressure datum: 0.72 * 2*SMYS*t/D (psi)."""
    return DESIGN_FACTOR * 2.0 * smys * t / D


def _safe_pressure(method: str, D: float, t: float, d: float, L: float,
                   smys: float, smts: float) -> float:
    """Safe (allowable) pressure for one defect under the chosen method (psi)."""
    if method == "B31G":
        return b31g_original(D, t, d, L, smys).safe_pressure_psi
    if method == "Modified B31G":
        return modified_b31g(D, t, d, L, smys).safe_pressure_psi
    if method == "RSTRENG":
        # Uniform-depth (rectangular) river-bottom profile for the (d, L) cell.
        Lc = max(L, 1.0e-3)
        return rstreng_effective_area(
            D, t, [0.0, Lc], [d, d], smys).safe_pressure_psi
    if method == "DNV-RP-F101":
        return dnv_f101_single_defect(
            D, t, d, L, smts, usage_factor=DESIGN_FACTOR).allowable_pressure_psi
    raise ValueError(f"unknown method {method!r}")


def _build_corroded_grids() -> dict:
    """Precompute one safe-pressure / acceptance grid per (preset, method)."""
    lengths = [round(0.5 * i, 3) for i in range(0, 41)]        # 0..20 in
    depths_pct = [round(2.5 * j, 3) for j in range(0, 33)]     # 0..80 % of wall

    preset_meta: dict = {}
    grids: dict = {}

    for label, D, t, grade in PRESETS:
        smys = SMYS_PSI[grade]
        smts = SMTS_PSI[grade]
        maop = _maop_psi(D, t, smys)
        preset_meta[label] = {
            "D": D, "t": t, "grade": grade, "smys_psi": smys,
            "smts_psi": smts, "maop_psi": round(maop, 1),
            "intact_psi": round(2.0 * smys * t / D, 1),
        }
        # B31G geometric allowable-length curve (same line for every method).
        allow_x, allow_y = [], []
        for pct in depths_pct:
            d = pct / 100.0 * t
            la = b31g_original_allowable_length(D, t, d)
            if la > 0.0:
                allow_x.append(round(la, 3))
                allow_y.append(pct)

        for method in METHODS:
            safe = []
            ratio = []
            for pct in depths_pct:
                d = pct / 100.0 * t
                safe_row, ratio_row = [], []
                for L in lengths:
                    sp = _safe_pressure(method, D, t, d, L, smys, smts)
                    sp = float(sp) if math.isfinite(sp) else 0.0
                    safe_row.append(round(sp, 1))
                    ratio_row.append(round(sp / maop, 4) if maop > 0 else 0.0)
                safe.append(safe_row)
                ratio.append(ratio_row)
            grids[f"{label}|{method}"] = {
                "safe": safe,
                "ratio": ratio,
                "maop": round(maop, 1),
                "allow_curve": {"x": allow_x, "y": allow_y},
            }

    return {
        "presets": [p[0] for p in PRESETS],
        "methods": METHODS,
        "preset_meta": preset_meta,
        "lengths": lengths,
        "depths_pct": depths_pct,
        "grids": grids,
    }


def _build_sufficiency() -> dict:
    """Run the real Phase-1 engine on a localized 2-cell thin spot (6x6, 0.5in)."""
    nominal_wt = 0.500
    od, smys, design_p = 16.0, SMYS_PSI["X52"], 1000.0

    # 6x6 grid of mildly-corroded 0.470in wall with a 2-cell deep spot at 0.300in.
    grid = pd.DataFrame([[0.470] * 6 for _ in range(6)])
    grid.iat[2, 2] = 0.300
    grid.iat[2, 3] = 0.300

    parsed = GridParser.from_dataframe(grid)
    routing = FFSRouter.classify(parsed, nominal_od_in=od, nominal_wt_in=nominal_wt)

    screener = Level1Screener(
        design_code="B31.8", nominal_od_in=od,
        design_pressure_psi=design_p, smys_psi=smys)
    t_min = screener.t_min()
    t_mm = GridParser.min_thickness(parsed)
    l1 = screener.evaluate(tmm_in=t_mm)

    engine = Level2Engine(
        assessment_type=routing["assessment_type"], nominal_od_in=od,
        nominal_wt_in=nominal_wt, t_min_in=t_min, rsf_a=0.9)
    l2 = engine.evaluate(parsed)

    suf = MeasurementSufficiency().evaluate(
        parsed, nominal_wt_in=nominal_wt,
        assessment_type=routing["assessment_type"],
        level1_result=l1, level2_result=l2)

    return {
        "status": suf.status,
        "confidence": suf.confidence,
        "reasons": list(suf.reasons),
        "actions": [
            {"action": a.action, "location": a.location,
             "rationale": a.rationale, "count": a.count}
            for a in suf.actions
        ],
        "context": {
            "grid_rows": int(grid.shape[0]), "grid_cols": int(grid.shape[1]),
            "nominal_wt_in": nominal_wt, "assessment_type": routing["assessment_type"],
            "t_min_in": round(t_min, 4), "t_mm_in": round(t_mm, 4),
            "l1_verdict": l1["verdict"], "rsf": round(float(l2["rsf"]), 3),
            "rsf_a": round(float(l2["rsf_a"]), 3),
            "od_in": od, "design_psi": design_p, "grade": "X52",
        },
    }


def _build_plate() -> dict:
    """Buckling utilisation vs metal loss for an AH36 ship plate at sigma_x=150 MPa."""
    geom = PlateGeometry(2400.0, 800.0, 16.0)
    sigma_x = 150.0
    max_loss = max_acceptable_loss(geom, STEEL_AH36, sigma_x)

    hi = min(15.5, max(max_loss["metal_loss_mm"] * 1.5, max_loss["metal_loss_mm"] + 4.0))
    losses = [round(float(x), 3) for x in np.linspace(0.0, hi, 28)]
    util, retained = [], []
    for loss in losses:
        r = assess_plate_uniform_loss(geom, STEEL_AH36, loss, sigma_x=sigma_x)
        util.append(round(float(r.utilization), 4))
        retained.append(round(float(r.capacity_retained_frac), 4))

    return {
        "loss_mm": losses,
        "utilization": util,
        "capacity_retained": retained,
        "max_loss_mm": round(float(max_loss["metal_loss_mm"]), 3),
        "max_loss_pct": round(float(max_loss["metal_loss_pct"]), 2),
        "limited_by": max_loss["limited_by"],
        "geom": {"length": 2400, "width": 800, "thickness": 16},
        "material": "AH36", "sigma_x": sigma_x,
    }


def build_showcase_data() -> dict:
    """Precompute all showcase data from the validated FFS modules."""
    return {
        "corroded": _build_corroded_grids(),
        "sufficiency": _build_sufficiency(),
        "plate": _build_plate(),
    }


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    data = build_showcase_data()
    html = _render_html(data)
    HTML_PATH.write_text(html, encoding="utf-8")

    c = data["corroded"]
    suf = data["sufficiency"]
    pl = data["plate"]
    print(f"corroded grids : {len(c['grids'])} combos "
          f"({len(c['presets'])} presets x {len(c['methods'])} methods), "
          f"{len(c['lengths'])} lengths x {len(c['depths_pct'])} depths")
    print(f"sufficiency    : {suf['status']} ({suf['confidence']}) — "
          f"{len(suf['actions'])} action(s)")
    print(f"plate AH36     : max acceptable loss {pl['max_loss_mm']} mm "
          f"({pl['max_loss_pct']}%), limited_by={pl['limited_by']}")
    print(f"HTML  -> {HTML_PATH}  "
          f"({HTML_PATH.stat().st_size/1_048_576:.2f} MB, self-contained)")


# ---------------------------------------------------------------------------
# HTML rendering
# ---------------------------------------------------------------------------
def _sufficiency_html(suf: dict, c: dict) -> str:
    badge = {"SUFFICIENT": c["green"], "TAKE_MORE": c["orange"],
             "ESCALATE": c["red"]}.get(suf["status"], c["muted"])
    ctx = suf["context"]
    reasons = "".join(f"<li>{r}</li>" for r in suf["reasons"])
    if suf["actions"]:
        rows = "".join(
            "<tr><td><b>{action}</b></td><td>{count}</td><td>{location}</td>"
            "<td>{rationale}</td></tr>".format(
                action=a["action"], count=("—" if a["count"] is None else a["count"]),
                location=a["location"], rationale=a["rationale"])
            for a in suf["actions"])
        actions = (
            "<table class='tbl'><thead><tr><th>Action</th><th>Count</th>"
            "<th>Where</th><th>Why</th></tr></thead><tbody>"
            f"{rows}</tbody></table>")
    else:
        actions = "<p class='ok'>No further field readings required — demobilise.</p>"
    return f"""
    <div class="panel">
      <h2>Field measurement sufficiency — real Phase-1 engine</h2>
      <p class="meta">Representative case: {ctx['grid_rows']}x{ctx['grid_cols']} UT grid,
        {ctx['nominal_wt_in']:.3f} in nominal wall, a localized 2-cell deep spot
        (0.300 in) on a {ctx['od_in']:.0f} in {ctx['grade']} line at {ctx['design_psi']:.0f} psi.
        Router → <b>{ctx['assessment_type']}</b>; Level 1 = <b>{ctx['l1_verdict']}</b>
        (t<sub>min</sub>={ctx['t_min_in']} in, t<sub>mm</sub>={ctx['t_mm_in']} in);
        Level 2 RSF={ctx['rsf']} (RSF<sub>a</sub>={ctx['rsf_a']}).</p>
      <div class="verdict-row">
        <span class="big-badge" style="background:{badge}">{suf['status']}</span>
        <span class="conf">confidence: <b>{suf['confidence']}</b></span>
      </div>
      <h3>Why</h3>
      <ul class="reasons">{reasons}</ul>
      <h3>Recommended field actions (before demobilising)</h3>
      {actions}
    </div>"""


def _render_html(data: dict) -> str:
    c = COLORS
    plotly_js = get_plotlyjs()
    corroded_json = json.dumps(data["corroded"])
    plate_json = json.dumps(data["plate"])
    suf_section = _sufficiency_html(data["sufficiency"], c)
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>Fitness-For-Service — Decision Layer Showcase</title>
<script>{plotly_js}</script>
<style>
  :root {{
    --navy:{c['navy']}; --orange:{c['orange']}; --green:{c['green']};
    --red:{c['red']}; --text:{c['text']}; --muted:{c['muted']};
  }}
  * {{ box-sizing:border-box; }}
  body {{ margin:0; font-family:Inter,-apple-system,Segoe UI,Roboto,sans-serif;
         color:var(--text); background:#eef2f6; }}
  header {{ background:linear-gradient(135deg,var(--navy),#2c5282); color:#fff;
            padding:28px 40px; }}
  header h1 {{ margin:0 0 6px; font-size:26px; }}
  header .sub {{ opacity:.92; font-size:15px; max-width:920px; }}
  header .badge {{ display:inline-block; margin-top:12px; background:var(--orange);
            color:#fff; padding:5px 14px; border-radius:20px; font-size:13px;
            font-weight:600; }}
  .wrap {{ max-width:1180px; margin:24px auto; padding:0 20px; }}
  .panel {{ background:#fff; border-radius:12px; box-shadow:0 2px 10px rgba(0,0,0,.06);
            padding:22px 26px; margin-bottom:22px; }}
  .controls {{ display:flex; flex-wrap:wrap; gap:18px; align-items:flex-end; }}
  .ctrl {{ display:flex; flex-direction:column; gap:5px; }}
  .ctrl label {{ font-size:12px; font-weight:600; color:var(--muted);
            text-transform:uppercase; letter-spacing:.04em; }}
  select {{ font-size:15px; padding:8px 12px; border:1px solid #cbd5e0;
            border-radius:8px; background:#fff; min-width:190px; cursor:pointer; }}
  select:focus {{ outline:2px solid var(--navy); }}
  #readout {{ margin-top:18px; padding:16px 20px; border-radius:10px;
            background:#f7fafc; border-left:5px solid var(--muted);
            font-size:15px; line-height:1.5; }}
  #readout.pass {{ border-left-color:var(--green); background:#f0fff4; }}
  #readout.fail {{ border-left-color:var(--red); background:#fff5f5; }}
  .verdict-pass {{ color:var(--green); font-weight:700; }}
  .verdict-fail {{ color:var(--red); font-weight:700; }}
  .hint {{ color:var(--muted); font-size:13px; margin-top:8px; }}
  .legend-zone {{ display:inline-block; width:12px; height:12px; border-radius:3px;
            vertical-align:middle; margin:0 4px 0 12px; }}
  h2 {{ font-size:19px; margin:0 0 14px; color:var(--navy); }}
  h3 {{ font-size:15px; margin:18px 0 8px; color:var(--navy); }}
  .meta {{ color:var(--muted); font-size:13.5px; line-height:1.55; }}
  .verdict-row {{ display:flex; align-items:center; gap:16px; margin:14px 0 4px; }}
  .big-badge {{ color:#fff; font-weight:700; font-size:17px; padding:8px 18px;
            border-radius:8px; letter-spacing:.03em; }}
  .conf {{ color:var(--muted); font-size:14px; }}
  ul.reasons {{ margin:6px 0; padding-left:20px; line-height:1.55; font-size:14px; }}
  .tbl {{ width:100%; border-collapse:collapse; font-size:13.5px; }}
  .tbl th, .tbl td {{ text-align:left; padding:8px 10px; border-bottom:1px solid #edf2f7;
            vertical-align:top; }}
  .tbl th {{ color:var(--muted); text-transform:uppercase; font-size:11px;
            letter-spacing:.04em; }}
  .ok {{ color:var(--green); font-weight:600; }}
  .callout {{ border-left:5px solid var(--orange); background:#fffaf0; }}
  .callout ul {{ line-height:1.6; }}
  footer {{ max-width:1180px; margin:10px auto 40px; padding:0 20px;
            color:var(--muted); font-size:13px; }}
  code {{ background:#edf2f7; padding:1px 5px; border-radius:4px; font-size:12.5px; }}
</style>
</head>
<body>
<header>
  <h1>Fitness-For-Service — Decision Layer Showcase</h1>
  <div class="sub">Remaining-strength acceptability for corroded pipe (ASME B31G,
    Modified B31G, RSTRENG effective-area, DNV-RP-F101), field measurement
    sufficiency, and plate metal-loss buckling FFS (DNV-RP-C201). A field-screening
    aid — <b>not a replacement for detailed analysis</b>.</div>
  <div class="badge">Interactive · validated digitalmodel modules · click the chart to probe a defect</div>
</header>

<div class="wrap">

  <div class="panel">
    <h2>Corroded-pipe acceptability explorer</h2>
    <div class="controls">
      <div class="ctrl"><label>Pipe preset</label><select id="preset"></select></div>
      <div class="ctrl"><label>Remaining-strength method</label><select id="method"></select></div>
      <div class="ctrl" id="presetinfo" style="font-size:13px;color:var(--muted);max-width:330px;line-height:1.5"></div>
    </div>
    <div id="readout">Pick a preset and method, then <b>click anywhere on the chart</b>
      to test a defect (length, depth) against the safe-pressure / MAOP criterion.</div>
    <div class="hint">
      <span class="legend-zone" style="background:var(--green)"></span>Acceptable (safe pressure &ge; MAOP)
      <span class="legend-zone" style="background:var(--red)"></span>Unacceptable (safe pressure &lt; MAOP)
      &nbsp;·&nbsp; the bold black line is the <b>safe = MAOP</b> boundary; the dashed
      orange line is the <b>B31G allowable defect length</b>.
    </div>
  </div>

  <div class="panel">
    <div id="chart" style="width:100%;height:560px;"></div>
  </div>

  {suf_section}

  <div class="panel">
    <h2>Plate metal-loss FFS — AH36 ship plate (buckling)</h2>
    <p class="meta">2400 × 800 × 16 mm AH36 plate field under longitudinal
      &sigma;<sub>x</sub> = 150 MPa. Buckling utilisation re-run on the validated
      DNV-RP-C201 solver as wall is thinned by metal loss.</p>
    <div id="plate" style="width:100%;height:420px;"></div>
  </div>

  <div class="panel callout">
    <h2>When to escalate to Level 3 (detailed analysis)</h2>
    <p class="meta">Level 1 / Level 2 screening is a fast field/office filter. Hand the
      case to a <b>detailed (Level 3) assessment</b> when any of the following apply:</p>
    <ul>
      <li><b>RSF below the floor</b> — remaining strength factor under the safe
        re-rating floor (≈0.50): repair/replace territory, more readings won't help.</li>
      <li><b>Crack-like flaw</b> — sharp/planar indications need fracture-mechanics
        (API 579 Part 9 / BS 7910), not a blunt metal-loss method.</li>
      <li><b>Interacting defects</b> — closely-spaced colonies whose stress fields
        overlap (DNV-RP-F101 interaction spacing).</li>
      <li><b>Complex geometry / loading</b> — bends, tees, nozzles, branch
        connections, combined axial/bending, or thermal/dynamic loads.</li>
      <li><b>Level 2 fail or marginal pass</b> — RSF below RSF<sub>a</sub>, or a pass
        within measurement tolerance.</li>
    </ul>
    <p class="meta">Level 1/Level 2 screening <b>does not replace</b> a detailed
      (Level 3) FFS or a full numerical / FEA assessment.</p>
  </div>

</div>

<footer>
  Generated by <b>digitalmodel</b> · validated FFS modules
  (API 579-1/ASME B31G/DNV-RP-F101/DNV-RP-C201) ·
  field-screening aid, not a substitute for detailed analysis.
</footer>

<script>
const COR = {corroded_json};
const PLATE = {plate_json};
const C = {{navy:'{c['navy']}', orange:'{c['orange']}', green:'{c['green']}', red:'{c['red']}'}};
const $ = id => document.getElementById(id);
let probe = null;

function fill(id, items){{
  const s = $(id);
  items.forEach(v => {{ const o = document.createElement('option');
    o.value = v; o.textContent = v; s.appendChild(o); }});
}}
fill('preset', COR.presets);
fill('method', COR.methods);
$('method').selectedIndex = 1;  // default Modified B31G

function key(){{ return $('preset').value + '|' + $('method').value; }}
function nearestIndex(arr, v){{
  let best=0, bd=Infinity;
  for(let i=0;i<arr.length;i++){{ const d=Math.abs(arr[i]-v); if(d<bd){{bd=d;best=i;}} }}
  return best;
}}

// Red (unacceptable) -> bold boundary at ratio 1 -> green (acceptable). zmax=2.
const RATIO_SCALE = [[0,'#b2182b'],[0.35,'#ef8a62'],[0.49,'#fddbc7'],
                     [0.5,'#e6f5d0'],[0.7,'#a6d96a'],[1,'#1a9850']];

function presetInfo(){{
  const m = COR.preset_meta[$('preset').value];
  $('presetinfo').innerHTML =
    'OD ' + m.D + ' in · wall ' + m.t + ' in · ' + m.grade + '<br>' +
    'MAOP datum <b>' + Math.round(m.maop_psi) + ' psi</b> (0.72·Barlow) · ' +
    'intact ' + Math.round(m.intact_psi) + ' psi';
}}

function render(){{
  const g = COR.grids[key()];
  const traces = [];
  // Green / red acceptance window (ratio = safe / MAOP).
  traces.push({{
    type:'contour', x:COR.lengths, y:COR.depths_pct, z:g.ratio,
    colorscale:RATIO_SCALE, zmin:0, zmax:2,
    contours:{{start:0, end:2, size:0.1, showlines:false}},
    colorbar:{{title:'safe / MAOP', titleside:'right'}},
    hovertemplate:'length %{{x}} in<br>depth %{{y}} %WT<br>safe/MAOP %{{z:.2f}}<extra></extra>'
  }});
  // Bold safe = MAOP boundary (ratio = 1).
  traces.push({{
    type:'contour', x:COR.lengths, y:COR.depths_pct, z:g.ratio,
    showscale:false, contours:{{start:1, end:1, size:1, coloring:'lines'}},
    line:{{color:'#1a202c', width:3}}, hoverinfo:'skip', name:'safe = MAOP'
  }});
  // B31G allowable defect-length curve.
  traces.push({{
    type:'scatter', x:g.allow_curve.x, y:g.allow_curve.y, mode:'lines',
    line:{{color:C.orange, width:2.5, dash:'dash'}}, name:'B31G allowable length',
    hovertemplate:'B31G allowable length %{{x:.1f}} in @ %{{y}} %WT<extra></extra>'
  }});
  if(probe){{
    traces.push({{
      type:'scatter', x:[probe.L], y:[probe.pct], mode:'markers',
      marker:{{size:15, color:'#fff', line:{{color:'#1a202c', width:2.5}}}},
      name:'Defect', hoverinfo:'skip'
    }});
  }}
  const layout = {{
    template:'plotly_white',
    font:{{family:'Inter, sans-serif', size:13, color:C.navy}},
    paper_bgcolor:'#fff', plot_bgcolor:'#fff',
    margin:{{l:64, r:30, t:54, b:56}},
    title:{{text:'Design window — '+$('preset').value+' · '+$('method').value, x:0.02, font:{{size:14}}}},
    xaxis:{{title:'Defect axial length (in)', range:[0, COR.lengths[COR.lengths.length-1]]}},
    yaxis:{{title:'Defect depth (% of wall)', range:[0, COR.depths_pct[COR.depths_pct.length-1]]}},
    legend:{{orientation:'h', y:-0.18}},
    annotations:[
      {{x:COR.lengths[Math.floor(COR.lengths.length*0.16)], y:COR.depths_pct[Math.floor(COR.depths_pct.length*0.18)], text:'ACCEPTABLE', showarrow:false, font:{{color:'#1a5e2a', size:15}}, bgcolor:'rgba(255,255,255,.6)'}},
      {{x:COR.lengths[Math.floor(COR.lengths.length*0.7)], y:COR.depths_pct[Math.floor(COR.depths_pct.length*0.85)], text:'UNACCEPTABLE', showarrow:false, font:{{color:'#9b1c1c', size:15}}, bgcolor:'rgba(255,255,255,.6)'}}
    ]
  }};
  Plotly.react('chart', traces, layout,
    {{displaylogo:false, responsive:true, modeBarButtonsToRemove:['lasso2d','select2d']}});
}}

let clickBound = false;
function bindClickOnce(){{
  if(clickBound) return;
  $('chart').on('plotly_click', ev => {{ const p = ev.points[0]; probeAt(p.x, p.y); }});
  clickBound = true;
}}

function probeAt(L, pct){{
  const g = COR.grids[key()];
  const i = nearestIndex(COR.lengths, L);
  const j = nearestIndex(COR.depths_pct, pct);
  const LL = COR.lengths[i], pp = COR.depths_pct[j];
  const safe = g.safe[j][i];
  const ratio = g.ratio[j][i];
  probe = {{L:LL, pct:pp}};
  const ok = ratio >= 1.0;
  const ro = $('readout');
  ro.className = ok ? 'pass' : 'fail';
  ro.innerHTML =
    '<b>'+$('preset').value+' · '+$('method').value+'</b> &nbsp;|&nbsp; '
    + 'defect '+LL.toFixed(1)+' in long, '+pp.toFixed(0)+' % of wall deep<br>'
    + 'Safe pressure <b>'+Math.round(safe)+' psi</b> vs MAOP '+Math.round(g.maop)+' psi '
    + '(ratio '+ratio.toFixed(2)+') &rarr; '
    + (ok ? '<span class="verdict-pass">ACCEPTABLE</span>'
          : '<span class="verdict-fail">UNACCEPTABLE</span>')
    + (ok ? '' : ' — reduce operating pressure, or repair/replace the defect.');
  render();
}}

['preset','method'].forEach(id => $(id).addEventListener('change', () => {{
  probe=null; resetReadout(); presetInfo(); render(); }}));
function resetReadout(){{ const ro=$('readout'); ro.className='';
  ro.innerHTML='Pick a preset and method, then <b>click anywhere on the chart</b> '
    + 'to test a defect (length, depth) against the safe-pressure / MAOP criterion.'; }}

function renderPlate(){{
  const traces = [
    {{type:'scatter', x:PLATE.loss_mm, y:PLATE.utilization, mode:'lines+markers',
      line:{{color:C.navy, width:3}}, marker:{{size:5}}, name:'Buckling utilisation'}},
    {{type:'scatter', x:[PLATE.loss_mm[0], PLATE.loss_mm[PLATE.loss_mm.length-1]],
      y:[1,1], mode:'lines', line:{{color:C.red, width:2, dash:'dash'}},
      name:'Unity (utilisation = 1.0)'}},
    {{type:'scatter', x:[PLATE.max_loss_mm], y:[1.0], mode:'markers',
      marker:{{size:13, color:C.orange, line:{{color:'#1a202c', width:2}}}},
      name:'Max acceptable loss'}}
  ];
  const layout = {{
    template:'plotly_white',
    font:{{family:'Inter, sans-serif', size:13, color:C.navy}},
    paper_bgcolor:'#fff', plot_bgcolor:'#fff',
    margin:{{l:60, r:24, t:20, b:50}},
    xaxis:{{title:'Metal loss (mm)'}},
    yaxis:{{title:'Buckling utilisation', rangemode:'tozero'}},
    legend:{{orientation:'h', y:-0.22}},
    annotations:[{{x:PLATE.max_loss_mm, y:1.0, ax:0, ay:-46, showarrow:true,
      arrowhead:2, text:'max acceptable loss '+PLATE.max_loss_mm+' mm ('+PLATE.max_loss_pct+'%)',
      font:{{color:'#9b5a00', size:13}}, bgcolor:'rgba(255,255,255,.7)'}}]
  }};
  Plotly.newPlot('plate', traces, layout, {{displaylogo:false, responsive:true}});
}}

presetInfo();
render();
bindClickOnce();
renderPlate();
</script>
</body>
</html>"""


if __name__ == "__main__":
    main()
