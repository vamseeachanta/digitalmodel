# ABOUTME: Builds a self-contained interactive HTML "FFS Field Dashboard" where an
# ABOUTME: inspector picks a scenario and sees verdict + what-to-measure-next + metal-loss heatmap.
"""FFS Field Dashboard — inspector decision aid.

A single self-contained HTML page (Plotly inlined, works offline) that lets a
field inspector pick a thickness-inspection **scenario** from a dropdown and
immediately see three things, all precomputed from the validated FFS
coordinator (no physics re-implemented here):

* the **FFS verdict** (ACCEPT / MONITOR / RE_RATE / REPAIR / REPLACE) with RSF
  vs RSF_a, remaining life and the assessment level reached;
* the **measurement-sufficiency** guidance (SUFFICIENT / TAKE_MORE / ESCALATE)
  with the reasons and a concrete "what to measure next" action table; and
* a **metal-loss heatmap** of the thickness grid (% wall loss per cell), with
  the minimum-thickness cell annotated.

Every number comes from
:func:`digitalmodel.asset_integrity.assessment.assess_component`. This is a
*field-screening* aid, not a replacement for a detailed (Level 3) analysis.

Run:
    .venv/bin/python examples/demos/asset_integrity/ffs_field_dashboard.py
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
from plotly.offline import get_plotlyjs

from digitalmodel.asset_integrity.assessment import (
    FFSComponent,
    assess_component,
)

OUT_DIR = Path(__file__).resolve().parent / "output"
HTML_PATH = OUT_DIR / "ffs_field_dashboard.html"

COLORS = {
    "navy": "#1a365d", "orange": "#ed8936", "green": "#38a169",
    "red": "#e53e3e", "amber": "#dd6b20", "text": "#2d3748", "muted": "#718096",
}

VERDICTS = ("ACCEPT", "MONITOR", "RE_RATE", "REPAIR", "REPLACE")
SUFFICIENCY = ("SUFFICIENT", "TAKE_MORE", "ESCALATE")


def _component(cid: str, od: float, wt: float, psi: float,
               cr: float = 0.003) -> FFSComponent:
    return FFSComponent(
        component_id=cid, design_code="B31.8", nominal_od_in=od,
        nominal_wt_in=wt, design_pressure_psi=psi, smys_psi=52000.0,
        corrosion_rate_in_per_yr=cr, rsf_a=0.90,
    )


def _scenario_grids() -> list[dict]:
    """Define 5 realistic inspection scenarios as (component, thickness grid).

    Grids are built with a fixed RNG seed so the output is reproducible.
    """
    rng = np.random.default_rng(11)

    # 1. Healthy line — 6x8 grid near a 0.500 in nominal wall.
    healthy = np.round(0.492 + rng.uniform(-0.004, 0.004, (6, 8)), 4)

    # 2. General metal loss — fairly uniform ~0.365 in with mild scatter.
    gml = np.round(0.365 + rng.uniform(-0.012, 0.012, (6, 8)), 4)

    # 3. Local pit (under-sampled) — mostly healthy with one deep cell.
    pit = np.round(0.468 + rng.uniform(-0.004, 0.004, (6, 8)), 4)
    pit[2, 3] = 0.225

    # 4. Severe local loss — a deep cluster eating most of the wall.
    severe = np.round(0.455 + rng.uniform(-0.005, 0.005, (6, 8)), 4)
    for i, j in [(2, 2), (2, 3), (2, 4), (2, 5), (3, 2), (3, 3), (3, 4), (3, 5)]:
        severe[i, j] = 0.045

    # 5. Sparse readings — a small 3x3 grid (too few readings).
    sparse = np.round(0.420 + rng.uniform(-0.008, 0.008, (3, 3)), 4)

    return [
        {"name": "Healthy line",
         "blurb": "20 in x 0.500 in X52 line, dense 6x8 UT grid near nominal wall.",
         "component": _component("HEALTHY-20x0.500", 20.0, 0.500, 1000.0),
         "grid": healthy, "force_type": "GML"},
        {"name": "General metal loss",
         "blurb": "16 in x 0.500 in X52 line, uniform area thinning to ~0.37 in.",
         "component": _component("GML-16x0.500", 16.0, 0.500, 1900.0),
         "grid": gml, "force_type": None},
        {"name": "Local pit (under-sampled)",
         "blurb": "16 in x 0.500 in line, healthy wall with one deep pit cell.",
         "component": _component("PIT-16x0.500", 16.0, 0.500, 1000.0),
         "grid": pit, "force_type": None},
        {"name": "Severe local loss",
         "blurb": "16 in x 0.500 in line, deep metal-loss cluster (~0.05 in floor).",
         "component": _component("SEVERE-16x0.500", 16.0, 0.500, 1000.0),
         "grid": severe, "force_type": None},
        {"name": "Sparse readings",
         "blurb": "16 in x 0.500 in line, only a 3x3 spot grid captured.",
         "component": _component("SPARSE-16x0.500", 16.0, 0.500, 1000.0),
         "grid": sparse, "force_type": None},
    ]


def build_dashboard_data() -> dict:
    """Run the validated FFS coordinator on each scenario; return embeddable data.

    Returns a dict with a ``"scenarios"`` list; each entry carries the thickness
    grid (list-of-lists), the per-cell % wall loss, and the unified
    :class:`FFSAssessmentResult` fields used by the dashboard cards + heatmap.
    """
    scenarios = []
    for spec in _scenario_grids():
        comp: FFSComponent = spec["component"]
        grid = spec["grid"]
        result = assess_component(comp, grid, force_type=spec["force_type"])

        nominal = float(comp.nominal_wt_in)
        loss_pct = np.round(100.0 * (nominal - grid) / nominal, 2)
        min_idx = np.unravel_index(int(np.argmin(grid)), grid.shape)
        min_row, min_col = int(min_idx[0]), int(min_idx[1])

        suf = result.sufficiency
        scenarios.append({
            "name": spec["name"],
            "blurb": spec["blurb"],
            "component_id": comp.component_id,
            "design_code": comp.design_code,
            "od_in": float(comp.nominal_od_in),
            "design_psi": float(comp.design_pressure_psi),
            "smys_psi": float(comp.smys_psi),
            "nominal_wt_in": round(nominal, 4),
            "t_min_in": round(float(result.t_min_in), 4),
            "t_measured_min_in": round(float(result.t_measured_min_in), 4),
            "t_measured_avg_in": round(float(result.t_measured_avg_in), 4),
            "n_rows": int(grid.shape[0]),
            "n_cols": int(grid.shape[1]),
            "n_readings": int(grid.size),
            "grid": grid.tolist(),
            "loss_pct": loss_pct.tolist(),
            "min_cell": {"row": min_row, "col": min_col,
                         "t_in": round(float(grid[min_row, min_col]), 4),
                         "loss_pct": round(float(loss_pct[min_row, min_col]), 2)},
            # FFSAssessmentResult fields
            "verdict": result.verdict,
            "assessment_type": result.assessment_type,
            "level_reached": int(result.level_reached),
            "rsf": round(float(result.rsf), 4),
            "rsf_a": round(float(result.rsf_a), 4),
            "folias_factor": round(float(result.folias_factor), 4),
            "remaining_life_yr": round(float(result.remaining_life_yr), 2),
            "rerated_pressure_psi": round(float(result.rerated_pressure_psi), 1),
            "sufficiency_status": result.sufficiency_status,
            "sufficiency_confidence": str(suf.confidence),
            "sufficiency_reasons": [str(r) for r in suf.reasons],
            "sufficiency_actions": [
                {"action": a.action, "location": a.location,
                 "rationale": a.rationale,
                 "count": (None if a.count is None else int(a.count))}
                for a in suf.actions
            ],
        })

    return {"scenarios": scenarios}


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    data = build_dashboard_data()
    html = _render_html(data)
    HTML_PATH.write_text(html, encoding="utf-8")

    print(f"scenarios: {len(data['scenarios'])}")
    print(f"{'scenario':<28} {'verdict':<9} {'sufficiency':<11} {'rsf':>6}")
    print("-" * 60)
    for s in data["scenarios"]:
        print(f"{s['name']:<28} {s['verdict']:<9} "
              f"{s['sufficiency_status']:<11} {s['rsf']:>6.3f}")
    print(f"\nHTML -> {HTML_PATH}  "
          f"({HTML_PATH.stat().st_size / 1_048_576:.2f} MB, self-contained)")


# ---------------------------------------------------------------------------
# HTML rendering
# ---------------------------------------------------------------------------
def _render_html(data: dict) -> str:
    c = COLORS
    data_json = json.dumps(data)
    plotly_js = get_plotlyjs()
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>FFS Field Dashboard — Inspector Decision Aid</title>
<script>{plotly_js}</script>
<style>
  :root {{
    --navy:{c['navy']}; --orange:{c['orange']}; --green:{c['green']};
    --red:{c['red']}; --amber:{c['amber']}; --text:{c['text']}; --muted:{c['muted']};
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
            border-radius:8px; background:#fff; min-width:260px; cursor:pointer; }}
  select:focus {{ outline:2px solid var(--navy); }}
  #blurb {{ color:var(--muted); font-size:13.5px; max-width:520px; line-height:1.5; }}
  .grid2 {{ display:grid; grid-template-columns:1fr 1fr; gap:22px; }}
  @media (max-width:880px) {{ .grid2 {{ grid-template-columns:1fr; }} }}
  h2 {{ font-size:18px; margin:0 0 12px; color:var(--navy); }}
  h3 {{ font-size:14px; margin:16px 0 6px; color:var(--navy); }}
  .meta {{ color:var(--muted); font-size:13px; line-height:1.55; }}
  .big-verdict {{ color:#fff; font-weight:800; font-size:30px; padding:14px 22px;
            border-radius:10px; letter-spacing:.04em; display:inline-block; }}
  .kv {{ display:grid; grid-template-columns:auto 1fr; gap:6px 16px; margin-top:14px;
            font-size:14px; }}
  .kv .k {{ color:var(--muted); }}
  .kv .v {{ font-weight:600; }}
  .big-badge {{ color:#fff; font-weight:700; font-size:17px; padding:8px 18px;
            border-radius:8px; letter-spacing:.03em; display:inline-block; }}
  .conf {{ color:var(--muted); font-size:14px; margin-left:10px; }}
  ul.reasons {{ margin:6px 0; padding-left:20px; line-height:1.55; font-size:13.5px; }}
  .tbl {{ width:100%; border-collapse:collapse; font-size:13px; }}
  .tbl th, .tbl td {{ text-align:left; padding:7px 9px; border-bottom:1px solid #edf2f7;
            vertical-align:top; }}
  .tbl th {{ color:var(--muted); text-transform:uppercase; font-size:10.5px;
            letter-spacing:.04em; }}
  .ok {{ color:var(--green); font-weight:600; font-size:13.5px; }}
  footer {{ max-width:1180px; margin:10px auto 40px; padding:0 20px;
            color:var(--muted); font-size:13px; }}
  code {{ background:#edf2f7; padding:1px 5px; border-radius:4px; font-size:12.5px; }}
</style>
</head>
<body>
<header>
  <h1>FFS Field Dashboard — Inspector Decision Aid</h1>
  <div class="sub">Enter readings &rarr; verdict + what to measure next; field
    screening, not a replacement for detailed analysis.</div>
  <div class="badge">Interactive · validated digitalmodel FFS coordinator · API 579-1 / ASME B31.8</div>
</header>

<div class="wrap">

  <div class="panel">
    <h2>Pick an inspection scenario</h2>
    <div class="controls">
      <div class="ctrl"><label>Scenario</label><select id="scenario"></select></div>
      <div class="ctrl" id="blurb"></div>
    </div>
  </div>

  <div class="grid2">
    <div class="panel">
      <h2>FFS verdict</h2>
      <div id="verdict"></div>
    </div>
    <div class="panel">
      <h2>Measurement sufficiency — what to measure next</h2>
      <div id="sufficiency"></div>
    </div>
  </div>

  <div class="panel">
    <h2>Metal-loss heatmap — % wall loss per cell</h2>
    <p class="meta">Each cell is a UT thickness reading; colour is
      <b>% wall loss</b> = 100·(nominal &minus; t) / nominal. The minimum-thickness
      cell is marked. Hover any cell for thickness and loss.</p>
    <div id="heatmap" style="width:100%;height:460px;"></div>
  </div>

</div>

<footer>
  Generated by <b>digitalmodel</b> · every number from the validated FFS
  coordinator <code>assess_component</code> (API 579-1 / ASME FFS-1 Phase-1
  chain: GridParser &rarr; FFSRouter &rarr; Level 1 &rarr; Level 2 &rarr;
  Decision &rarr; MeasurementSufficiency) · field-screening aid, not a substitute
  for detailed (Level 3) analysis.
</footer>

<script>
const DATA = {data_json};
const C = {{navy:'{c['navy']}', orange:'{c['orange']}', green:'{c['green']}',
            red:'{c['red']}', amber:'{c['amber']}', muted:'{c['muted']}'}};
const $ = id => document.getElementById(id);

const VERDICT_COLOR = {{
  ACCEPT:C.green, MONITOR:C.amber, RE_RATE:C.amber, REPAIR:C.red, REPLACE:C.red
}};
const SUF_COLOR = {{
  SUFFICIENT:C.green, TAKE_MORE:C.amber, ESCALATE:C.red
}};

function fill(id, items){{
  const s = $(id);
  items.forEach((v,i) => {{ const o=document.createElement('option');
    o.value=i; o.textContent=v; s.appendChild(o); }});
}}
fill('scenario', DATA.scenarios.map(s => s.name));

function fmtLife(yr){{
  if(yr===null || isNaN(yr)) return '—';
  if(yr <= 0) return '0 yr (at/over limit)';
  return yr.toFixed(1) + ' yr';
}}

function renderVerdict(s){{
  const col = VERDICT_COLOR[s.verdict] || C.muted;
  $('verdict').innerHTML =
    '<span class="big-verdict" style="background:'+col+'">'+s.verdict+'</span>'
    + '<div class="kv">'
    + '<span class="k">Assessment</span><span class="v">'+s.assessment_type
        +' (Level '+s.level_reached+' reached)</span>'
    + '<span class="k">RSF vs RSF<sub>a</sub></span><span class="v">'
        +s.rsf.toFixed(3)+' vs '+s.rsf_a.toFixed(2)+'</span>'
    + '<span class="k">Remaining life</span><span class="v">'+fmtLife(s.remaining_life_yr)+'</span>'
    + '<span class="k">Min / nominal wall</span><span class="v">'
        +s.t_measured_min_in.toFixed(3)+' / '+s.nominal_wt_in.toFixed(3)+' in'
        +' (t<sub>min</sub> '+s.t_min_in.toFixed(3)+' in)</span>'
    + '<span class="k">Re-rated pressure</span><span class="v">'
        +Math.round(s.rerated_pressure_psi)+' psi (design '+Math.round(s.design_psi)+')</span>'
    + '</div>';
}}

function renderSufficiency(s){{
  const col = SUF_COLOR[s.sufficiency_status] || C.muted;
  const reasons = s.sufficiency_reasons.map(r => '<li>'+r+'</li>').join('');
  let actions;
  if(s.sufficiency_actions.length){{
    const rows = s.sufficiency_actions.map(a =>
      '<tr><td><b>'+a.action+'</b></td><td>'+a.location+'</td><td>'
      +(a.count===null?'—':a.count)+'</td></tr>').join('');
    actions = '<table class="tbl"><thead><tr><th>Action</th><th>Where</th>'
      +'<th>Count</th></tr></thead><tbody>'+rows+'</tbody></table>';
  }} else {{
    actions = '<p class="ok">No further field readings required — demobilise.</p>';
  }}
  $('sufficiency').innerHTML =
    '<span class="big-badge" style="background:'+col+'">'+s.sufficiency_status+'</span>'
    + '<span class="conf">confidence: <b>'+s.sufficiency_confidence+'</b> · '
        +s.n_readings+' readings ('+s.n_rows+'x'+s.n_cols+')</span>'
    + '<h3>Why</h3><ul class="reasons">'+reasons+'</ul>'
    + '<h3>Recommended field actions</h3>'+actions;
}}

// Red (high loss) -> green (low loss).
const LOSS_SCALE = [[0,'#1a9850'],[0.2,'#a6d96a'],[0.4,'#fee08b'],
                    [0.6,'#fc8d59'],[0.8,'#d73027'],[1,'#7f0000']];

function renderHeatmap(s){{
  const xs = Array.from({{length:s.n_cols}}, (_,i)=>i+1);
  const ys = Array.from({{length:s.n_rows}}, (_,i)=>i+1);
  // custom hover carries thickness alongside the %loss z value.
  const customdata = s.grid.map(row => row.map(t => t));
  const trace = {{
    type:'heatmap', x:xs, y:ys, z:s.loss_pct, customdata:customdata,
    colorscale:LOSS_SCALE, zmin:0, zmax:100,
    colorbar:{{title:'% wall loss', titleside:'right'}},
    xgap:2, ygap:2,
    hovertemplate:'col %{{x}}, row %{{y}}<br>thickness %{{customdata:.3f}} in'
      +'<br>wall loss %{{z:.1f}}%<extra></extra>'
  }};
  const mc = s.min_cell;
  const layout = {{
    font:{{family:'Inter, sans-serif', size:13, color:C.navy}},
    paper_bgcolor:'#fff', plot_bgcolor:'#fff',
    margin:{{l:54, r:30, t:44, b:50}},
    title:{{text:'Metal-loss map — '+s.name+' ('+s.assessment_type+')', x:0.02, font:{{size:14}}}},
    xaxis:{{title:'Grid column', dtick:1, constrain:'domain'}},
    yaxis:{{title:'Grid row', dtick:1, autorange:'reversed', scaleanchor:'x', constrain:'domain'}},
    annotations:[{{
      x:mc.col+1, y:mc.row+1, text:'min '+mc.t_in.toFixed(3)+' in<br>('+mc.loss_pct.toFixed(0)+'% loss)',
      showarrow:true, arrowhead:2, arrowcolor:C.navy, ax:0, ay:-42,
      font:{{color:'#fff', size:11}}, bgcolor:'rgba(26,54,93,.85)', borderpad:4
    }}]
  }};
  Plotly.react('heatmap', [trace], layout,
    {{displaylogo:false, responsive:true, modeBarButtonsToRemove:['lasso2d','select2d']}});
}}

function update(){{
  const s = DATA.scenarios[+$('scenario').value];
  $('blurb').innerHTML = s.blurb;
  renderVerdict(s);
  renderSufficiency(s);
  renderHeatmap(s);
}}

$('scenario').addEventListener('change', update);
update();
</script>
</body>
</html>"""


if __name__ == "__main__":
    main()
