# ABOUTME: Generates the self-contained interactive stiffened-panel design-window
# ABOUTME: explorer (dropdowns + acceptable/unacceptable load map) as one HTML file.
"""Build the interactive ship-panel buckling **design-window explorer**.

A single self-contained HTML dashboard (Plotly inlined, works offline) where a
manager selects material grade, stiffener profile and spacing from dropdowns and
immediately sees the *acceptable* (green) versus *unacceptable* (red) load region
as a function of plate thickness, with the bold line marking the maximum
allowable load (utilisation = 1.0). A "colour by" toggle switches the background
to the governing failure mode, and clicking the chart probes any
(thickness, load) operating point for a PASS/FAIL readout.

All numbers come from the validated StiffenedPanelBucklingAnalyzer (DNV-RP-C201).

Run:
    .venv/bin/python examples/demos/structural/ship_panel_design_explorer.py
"""

from __future__ import annotations

import json
from pathlib import Path

from plotly.offline import get_plotlyjs

from digitalmodel.structural.panel_design_explorer import (
    ExplorerConfig,
    build_design_grids,
)

OUT_DIR = Path(__file__).resolve().parent / "output"
HTML_PATH = OUT_DIR / "ship_panel_design_explorer.html"
JSON_PATH = OUT_DIR / "panel_design_grids.json"

COLORS = {
    "navy": "#1a365d", "orange": "#ed8936", "green": "#38a169",
    "red": "#e53e3e", "text": "#2d3748", "muted": "#718096",
    "chart_bg": "#fbfcfd",
}


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    data = build_design_grids(ExplorerConfig())
    JSON_PATH.write_text(json.dumps(data), encoding="utf-8")

    html = _render_html(data)
    HTML_PATH.write_text(html, encoding="utf-8")

    n = len(data["grids"])
    print(f"design grids: {n} combos "
          f"({len(data['grades'])} grades x {len(data['profiles'])} profiles "
          f"x {len(data['spacings'])} spacings x {len(data['spans'])} spans "
          f"x {len(data['taus'])} shear levels)")
    print(f"thickness {data['thickness'][0]}-{data['thickness'][-1]} mm, "
          f"axial {data['load'][0]}-{data['load'][-1]} MPa, spans "
          f"{', '.join(data['spans'])} mm, shear {', '.join(data['taus'])} MPa, "
          f"gamma_m {data['meta']['gamma_m']}")
    print(f"JSON  -> {JSON_PATH}  ({JSON_PATH.stat().st_size/1024:.0f} KB)")
    print(f"HTML  -> {HTML_PATH}  ({HTML_PATH.stat().st_size/1_048_576:.2f} MB, self-contained)")


def _render_html(data: dict) -> str:
    data_json = json.dumps(data)
    plotly_js = get_plotlyjs()
    c = COLORS
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>Ship Panel Buckling — Design-Window Explorer</title>
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
  header .sub {{ opacity:.9; font-size:15px; }}
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
            border-radius:8px; background:#fff; min-width:150px; cursor:pointer; }}
  select:focus {{ outline:2px solid var(--navy); }}
  #readout {{ margin-top:18px; padding:16px 20px; border-radius:10px;
            background:#f7fafc; border-left:5px solid var(--muted);
            font-size:15px; line-height:1.5; }}
  #readout.pass {{ border-left-color:var(--green); background:#f0fff4; }}
  #readout.fail {{ border-left-color:var(--red); background:#fff5f5; }}
  #readout b {{ font-size:17px; }}
  .verdict-pass {{ color:var(--green); font-weight:700; }}
  .verdict-fail {{ color:var(--red); font-weight:700; }}
  .hint {{ color:var(--muted); font-size:13px; margin-top:8px; }}
  .legend-zone {{ display:inline-block; width:12px; height:12px; border-radius:3px;
            vertical-align:middle; margin:0 4px 0 12px; }}
  footer {{ max-width:1180px; margin:10px auto 40px; padding:0 20px;
            color:var(--muted); font-size:13px; }}
  h2 {{ font-size:18px; margin:0 0 14px; color:var(--navy); }}
</style>
</head>
<body>
<header>
  <h1>Ship Hull Stiffened-Panel Buckling — Design-Window Explorer</h1>
  <div class="sub">Acceptable vs unacceptable in-plane loads by panel size, material grade and stiffener profile · DNV-RP-C201</div>
  <div class="badge">Interactive · validated solver · click the chart to probe an operating point</div>
</header>

<div class="wrap">
  <div class="panel">
    <h2>Select panel configuration</h2>
    <div class="controls">
      <div class="ctrl"><label>Material grade</label><select id="grade"></select></div>
      <div class="ctrl"><label>Stiffener profile</label><select id="profile"></select></div>
      <div class="ctrl"><label>Stiffener spacing</label><select id="spacing"></select></div>
      <div class="ctrl"><label>Frame span</label><select id="span"></select></div>
      <div class="ctrl"><label>Colour by</label>
        <select id="colorby">
          <option value="util">Utilisation (acceptable / unacceptable)</option>
          <option value="mode">Governing failure mode</option>
        </select>
      </div>
      <div class="ctrl" style="min-width:230px">
        <label>Shear load &tau; = <b id="tauval">0</b> MPa</label>
        <input type="range" id="tau" min="0" max="0" step="1" value="0" style="width:210px; cursor:pointer;"/>
      </div>
    </div>
    <div id="readout">Pick a configuration, then <b>click anywhere on the chart</b> to test a (thickness, load) operating point.</div>
    <div class="hint">
      <span class="legend-zone" style="background:var(--green)"></span>Acceptable loads (utilisation &le; 1.0)
      <span class="legend-zone" style="background:var(--red)"></span>Unacceptable — buckling failure (utilisation &gt; 1.0)
      &nbsp;·&nbsp; the bold black line is the <b>maximum allowable load</b>.
    </div>
  </div>

  <div class="panel">
    <div id="chart" style="width:100%;height:560px;"></div>
  </div>
</div>

<footer>
  Generated by <b>digitalmodel.structural.panel_design_explorer</b> ·
  every cell from the validated <b>StiffenedPanelBucklingAnalyzer</b>
  (see docs/domains/plate-buckling/panel-buckling-validation-2026-06-26.md) ·
  &gamma;<sub>M</sub> = {data['meta']['gamma_m']} · frame span selectable.
  Modes screened: plate-field, overall column, stiffener tripping. References: DNV-RP-C201 (2010).
</footer>

<script>
const DATA = {data_json};
const C = {{navy:'{c['navy']}', orange:'{c['orange']}', green:'{c['green']}', red:'{c['red']}'}};
const $ = id => document.getElementById(id);
let probe = null;

function fill(id, items, labels){{
  const s = $(id);
  items.forEach((v,i) => {{
    const o = document.createElement('option');
    o.value = v; o.textContent = labels ? labels[i] : v; s.appendChild(o);
  }});
}}
fill('grade', DATA.grades);
fill('profile', DATA.profiles);
fill('spacing', DATA.spacings, DATA.spacings.map(s => s + ' mm'));
fill('span', DATA.spans, DATA.spans.map(s => s + ' mm'));
$('profile').selectedIndex = 1;   // default to the mid T400x120 profile
$('spacing').selectedIndex = 1;   // default 700 mm
$('span').selectedIndex = Math.min(1, DATA.spans.length - 1);   // default 2400 mm
$('tau').max = DATA.taus.length - 1;   // shear slider snaps to precomputed levels

function tauVal(){{ return DATA.taus[+$('tau').value]; }}
function key(){{ return $('grade').value + '|' + $('profile').value + '|' + $('spacing').value + '|' + $('span').value + '|' + tauVal(); }}

function nearestIndex(arr, v){{
  let best=0, bd=Infinity;
  for(let i=0;i<arr.length;i++){{ const d=Math.abs(arr[i]-v); if(d<bd){{bd=d;best=i;}} }}
  return best;
}}

const UTIL_SCALE = [[0,'#1a9850'],[0.35,'#66bd63'],[0.5,'#d9ef8b'],
                    [0.7,'#fee08b'],[0.85,'#fc8d59'],[1,'#d73027']];
const MODE_SCALE = [[0,'#3b82c4'],[0.49,'#3b82c4'],[0.5,'#9aa5b1'],
                    [0.66,'#9aa5b1'],[0.67,'#e53e3e'],[1,'#e53e3e']];

function render(){{
  const g = DATA.grids[key()];
  const colorBy = $('colorby').value;
  const traces = [];

  if(colorBy === 'util'){{
    traces.push({{
      type:'contour', x:DATA.thickness, y:DATA.load, z:g.util,
      colorscale:UTIL_SCALE, zmin:0, zmax:2,
      contours:{{start:0, end:2, size:0.1, showlines:false}},
      colorbar:{{title:'Utilisation', titleside:'right'}},
      hovertemplate:'thickness %{{x}} mm<br>load %{{y}} MPa<br>utilisation %{{z}}<extra></extra>'
    }});
  }} else {{
    traces.push({{
      type:'heatmap', x:DATA.thickness, y:DATA.load, z:g.mode,
      colorscale:MODE_SCALE, zmin:0, zmax:2,
      colorbar:{{title:'Governing<br>mode', tickvals:[0.33,1,1.67], ticktext:['Plate-field','Column','Tripping']}},
      hovertemplate:'thickness %{{x}} mm<br>load %{{y}} MPa<extra></extra>'
    }});
  }}

  // Maximum allowable load (utilisation = 1.0 boundary)
  traces.push({{
    type:'scatter', x:DATA.thickness, y:g.allow, mode:'lines',
    line:{{color:'#1a202c', width:3}}, name:'Max allowable load',
    hovertemplate:'allowable load %{{y}} MPa @ %{{x}} mm<extra></extra>'
  }});

  if(probe){{
    traces.push({{
      type:'scatter', x:[probe.t], y:[probe.load], mode:'markers',
      marker:{{size:15, color:'#fff', line:{{color:'#1a202c', width:2.5}}, symbol:'circle'}},
      name:'Operating point', hoverinfo:'skip'
    }});
  }}

  const layout = {{
    template:'plotly_white',
    font:{{family:'Inter, sans-serif', size:13, color:C.navy}},
    paper_bgcolor:'#fff', plot_bgcolor:'#fff',
    margin:{{l:64, r:30, t:54, b:56}},
    title:{{text:'Design window — '+$('grade').value+' · '+$('profile').value+' · '+$('spacing').value+' mm sp · '+$('span').value+' mm span · τ='+tauVal()+' MPa', x:0.02, font:{{size:14}}}},
    xaxis:{{title:'Plate thickness (mm)'}},
    yaxis:{{title:'Applied longitudinal load σ<sub>x</sub> (MPa)', rangemode:'tozero'}},
    legend:{{orientation:'h', y:-0.18}},
    annotations: colorBy==='util' ? [
      {{x:DATA.thickness[Math.floor(DATA.thickness.length*0.78)], y:DATA.load[Math.floor(DATA.load.length*0.18)], text:'ACCEPTABLE', showarrow:false, font:{{color:'#1a5e2a', size:15, family:'Inter'}}, bgcolor:'rgba(255,255,255,.55)'}},
      {{x:DATA.thickness[Math.floor(DATA.thickness.length*0.22)], y:DATA.load[Math.floor(DATA.load.length*0.82)], text:'UNACCEPTABLE — BUCKLING', showarrow:false, font:{{color:'#9b1c1c', size:15, family:'Inter'}}, bgcolor:'rgba(255,255,255,.55)'}}
    ] : []
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

function probeAt(t, load){{
  const g = DATA.grids[key()];
  const ti = nearestIndex(DATA.thickness, t);
  const lj = nearestIndex(DATA.load, load);
  const tt = DATA.thickness[ti], ll = DATA.load[lj];
  const util = g.util[lj][ti];
  const mode = DATA.meta.mode_labels[String(g.mode[lj][ti])];
  const allow = g.allow[ti];
  probe = {{t:tt, load:ll}};
  const ok = util <= 1.0;
  const ro = $('readout');
  ro.className = ok ? 'pass' : 'fail';
  ro.innerHTML =
    '<b>'+$('grade').value+' · '+$('profile').value+' · '+$('spacing').value+' mm sp · '+$('span').value+' mm span · &tau;='+tauVal()+' MPa</b> &nbsp;|&nbsp; '
    + 'plate '+tt.toFixed(1)+' mm, axial &sigma;<sub>x</sub> = '+ll.toFixed(0)+' MPa<br>'
    + 'Utilisation <b>'+util.toFixed(2)+'</b> &rarr; '
    + (ok ? '<span class="verdict-pass">ACCEPTABLE</span>' : '<span class="verdict-fail">UNACCEPTABLE (buckling)</span>')
    + ' &nbsp;·&nbsp; governing mode: <b>'+mode+'</b><br>'
    + 'Maximum allowable load at '+tt.toFixed(1)+' mm: <b>'+(allow==null?'—':allow.toFixed(0)+' MPa')+'</b>'
    + (ok ? '' : '. To make it acceptable: increase thickness, reduce load, '
        + (mode==='Tripping' ? 'or add/enlarge the stiffener flange.' : 'or reduce stiffener spacing.'));
  render();
}}

['grade','profile','spacing','span','colorby'].forEach(id => $(id).addEventListener('change', () => {{ probe=null; resetReadout(); render(); }}));
$('tau').addEventListener('input', () => {{ $('tauval').textContent = tauVal(); probe=null; resetReadout(); render(); }});
function resetReadout(){{ const ro=$('readout'); ro.className=''; ro.innerHTML='Pick a configuration (incl. shear &tau;), then <b>click anywhere on the chart</b> to test a (thickness, axial load) operating point.'; }}
$('tauval').textContent = tauVal();
render();
bindClickOnce();
</script>
</body>
</html>"""


if __name__ == "__main__":
    main()
