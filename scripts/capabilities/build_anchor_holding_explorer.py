#!/usr/bin/env python
"""Build the drag-anchor holding-capacity explorer.

Runs the geotechnical drag-anchor engine over anchor types and seabed soils
across an anchor-weight sweep, then writes a self-contained interactive HTML
dashboard (data embedded as JSON) plus the raw study JSON.

Outputs:
    docs/api/structural/anchor-holding-explorer.html
    docs/api/structural/anchor-holding-explorer.json

Run:
    python scripts/capabilities/build_anchor_holding_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.geotechnical.anchors import drag_anchor_capacity

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

_SOILS = ["sand", "soft_clay", "stiff_clay"]
_ANCHORS = ["stevpris", "bruce"]
_WEIGHT_KN = [round(50.0 + 25.0 * i, 0) for i in range(0, 19)]   # 50 .. 500 kN


def build_study() -> dict:
    # discover valid (soil, anchor) combos + per-combo efficiency
    soils, anchors, eff = [], [], {}
    series = {}
    for s in _SOILS:
        for a in _ANCHORS:
            try:
                r0 = drag_anchor_capacity(anchor_weight_kn=100.0, soil_type=s, anchor_type=a)
            except Exception:
                continue
            if s not in soils:
                soils.append(s)
            if a not in anchors:
                anchors.append(a)
            eff[f"{s}|{a}"] = round(float(r0.efficiency), 1)
            pts = []
            for w in _WEIGHT_KN:
                r = drag_anchor_capacity(anchor_weight_kn=w, soil_type=s, anchor_type=a)
                pts.append({"w": w, "h": round(float(r.holding_capacity_kn), 0)})
            series[f"{s}|{a}"] = pts
    return {
        "meta": {
            "soils": soils, "anchors": anchors, "efficiency": eff,
            "weight_kn": _WEIGHT_KN, "standard": "DNV-RP-E302 (drag-embedment anchors)",
            "labels": {"soft_clay": "soft clay", "stiff_clay": "stiff clay", "sand": "sand",
                       "stevpris": "Stevpris", "bruce": "Bruce"},
        },
        "series": series,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Drag-anchor holding-capacity explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:23px;color:var(--navy);letter-spacing:-.3px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:780px}
 .spec{display:flex;flex-wrap:wrap;gap:8px;margin-bottom:20px}
 .spec span{font-size:12px;font-weight:600;color:var(--muted);background:#fff;
            border:1px solid var(--line);border-radius:20px;padding:4px 11px}
 .spec b{color:var(--navy)}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .ctrl{display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin-bottom:12px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13.5px}
 select{font:inherit;font-size:13px;padding:5px 9px;border:1px solid var(--line);
        border-radius:8px;background:#fff;color:var(--ink)}
 input[type=range]{flex:1;min-width:220px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:80px}
 .row{display:grid;grid-template-columns:130px 1fr 130px;align-items:center;gap:10px;margin:9px 0}
 .row .name{font-size:12.5px;font-weight:700;color:var(--ink);text-align:right}
 .track{position:relative;background:var(--soft);border:1px solid var(--line);border-radius:7px;height:26px;overflow:hidden}
 .fill{position:absolute;top:0;left:0;height:100%;border-radius:6px 0 0 6px;
       background:linear-gradient(90deg,var(--teal),var(--navy));transition:width .12s}
 .glab{position:absolute;left:8px;top:50%;transform:translateY(-50%);font-size:11px;font-weight:600;
       color:#fff;white-space:nowrap;text-shadow:0 1px 2px rgba(0,0,0,.25)}
 .uval{font-family:ui-monospace,Menlo,monospace;font-size:13px;font-weight:700;text-align:right;color:var(--navy)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:8px}.foot a{color:var(--teal)}
 .cap{font-size:12px;color:var(--muted);margin:2px 0 8px}
</style></head><body>
<h1>Drag-anchor holding capacity &mdash; explorer</h1>
<p class="sub">Ultimate holding capacity of drag-embedment anchors on the DNV-RP-E302 efficiency
method &mdash; the seabed side of a mooring problem, sizing the anchors the mooring-resilience
screen assumes. Pick a seabed soil and drag the anchor (dry) weight to compare anchor types.
Every value is a live <code>drag_anchor_capacity</code> evaluation.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Seabed soil</label><select id="soil"></select>
    <label for="w">Anchor weight</label>
    <input type="range" id="w" min="0" max="0" step="1">
    <span class="wval" id="wval"></span>
  </div>
  <div class="cap">Ultimate holding capacity by anchor type (bar = kN); efficiency = holding / weight.</div>
  <div id="bars"></div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_anchor_holding_explorer.py</code> from
<code>src/digitalmodel/geotechnical/anchors.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/geotechnical/anchors.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, W = M.weight_kn, LAB = M.labels;
const lab = k => LAB[k] || k;
const spec = document.getElementById('spec');
spec.innerHTML = [['Standard', M.standard], ['Anchor types', M.anchors.length],
  ['Soils', M.soils.length], ['Weight range', W[0]+'–'+W[W.length-1]+' kN']]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const selS = document.getElementById('soil');
M.soils.forEach(s=>selS.add(new Option(lab(s), s)));
const slider = document.getElementById('w');
slider.max = W.length-1;

function maxH(){ let mx=0; for(const k in DATA.series) for(const p of DATA.series[k]) if(p.h>mx) mx=p.h; return mx; }
const MX = maxH();

function render(){
  const s = selS.value, idx = +slider.value, w = W[idx];
  document.getElementById('wval').textContent = w.toLocaleString()+' kN';
  document.getElementById('bars').innerHTML = M.anchors.map(a=>{
    const key = s+'|'+a, ser = DATA.series[key];
    if(!ser) return '';
    const p = ser[idx], e = M.efficiency[key];
    const pct = (p.h/MX*100).toFixed(1);
    return `<div class="row"><div class="name">${lab(a)}</div>
      <div class="track"><div class="fill" style="width:${pct}%"></div>
      <div class="glab">${p.h.toLocaleString()} kN &middot; eff ${e}</div></div>
      <div class="uval">${p.h.toLocaleString()} kN</div></div>`;
  }).join('');
}
selS.value = M.soils.includes('soft_clay') ? 'soft_clay' : M.soils[0];
let start = W.indexOf(200.0); if(start<0) start = Math.floor(W.length/2);
slider.value = start; render();
[selS, slider].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "anchor-holding-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "anchor-holding-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v) for v in study["series"].values())
    print(f"wrote {(_OUT_DIR / 'anchor-holding-explorer.html').relative_to(_REPO)} "
          f"({len(study['series'])} soil/anchor combos, {n} points)")


if __name__ == "__main__":
    main()
