#!/usr/bin/env python
"""Build the well inflow-performance (IPR) explorer.

Runs the production-engineering Vogel IPR model over reservoir pressures and
productivity indices, sweeping bottomhole flowing pressure to trace the inflow
curve, then writes a self-contained interactive HTML dashboard + raw JSON.

Outputs:
    docs/api/structural/ipr-explorer.html
    docs/api/structural/ipr-explorer.json

Run:
    python scripts/capabilities/build_ipr_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.production_engineering.ipr_models import ReservoirConditions, VogelIpr

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

_PR = [2000.0, 3000.0, 4000.0]                       # reservoir pressure, psi
_PI = [round(0.5 + 0.5 * i, 1) for i in range(0, 10)]  # 0.5 .. 5.0 bopd/psi
_NPWF = 11                                             # Pwf steps (Pr down to 0)


def build_study() -> dict:
    series = {}
    for pr in _PR:
        series[str(int(pr))] = {}
        for pi in _PI:
            res = ReservoirConditions(reservoir_pressure_psi=pr,
                                      bubble_point_psi=pr,
                                      productivity_index_bopd_psi=pi)
            ipr = VogelIpr.from_productivity_index(reservoir=res, pi_bopd_psi=pi)
            curve = []
            for i in range(_NPWF):
                pwf = pr * (1.0 - i / (_NPWF - 1))
                curve.append({"pwf": round(pwf, 0),
                              "q": round(float(ipr.flow_rate(pwf_psi=pwf)), 1)})
            series[str(int(pr))][str(pi)] = {
                "qmax": round(float(ipr.qmax_bopd), 1), "curve": curve}
    return {
        "meta": {
            "pr": [int(p) for p in _PR], "pi": _PI, "npwf": _NPWF,
            "standard": "Vogel (1968) two-phase IPR",
        },
        "series": series,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Well inflow-performance (IPR) explorer — digitalmodel</title>
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
 .ctrl{display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin-bottom:10px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13.5px}
 select{font:inherit;font-size:13px;padding:5px 9px;border:1px solid var(--line);
        border-radius:8px;background:#fff;color:var(--ink)}
 input[type=range]{flex:1;min-width:220px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:110px}
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:4px 0 2px}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;color:var(--navy);font-weight:700}
 .cap{font-size:12px;color:var(--muted);margin:8px 0}
 .chart{margin-top:6px;display:flex;align-items:flex-end;gap:4px;height:210px;
        border-bottom:2px solid var(--line);border-left:2px solid var(--line);padding-left:4px}
 .bar{flex:1;background:linear-gradient(180deg,var(--teal),var(--navy));border-radius:3px 3px 0 0;
      position:relative;min-height:1px;transition:height .12s}
 .bar span{position:absolute;top:-15px;left:0;right:0;text-align:center;font-size:8.5px;color:var(--muted)}
 .xax{display:flex;gap:4px;padding-left:6px;margin-top:4px}
 .xax div{flex:1;text-align:center;font-size:9px;color:var(--muted)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
</style></head><body>
<h1>Well inflow performance (IPR) &mdash; Vogel explorer</h1>
<p class="sub">The reservoir's deliverability: oil rate versus bottomhole flowing pressure on the
Vogel two-phase inflow model. Pick a reservoir pressure and drag the productivity index to reshape
the inflow curve &mdash; the absolute open-flow potential (rate at zero BHFP) rises with it. Every
point is a live <code>VogelIpr.flow_rate</code> evaluation.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Reservoir pressure</label><select id="pr"></select>
    <label for="pi">Productivity index</label>
    <input type="range" id="pi" min="0" max="0" step="1">
    <span class="wval" id="pival"></span>
  </div>
  <div class="reads">
    <div class="read">Absolute open flow (AOF)<b id="rQ">&mdash;</b></div>
    <div class="read">Rate at 50% drawdown<b id="rH">&mdash;</b></div>
  </div>
  <div class="cap">Oil rate q (bar height, bopd) at each bottomhole flowing pressure P<sub>wf</sub>, from reservoir pressure (left) down to zero (right).</div>
  <div class="chart" id="chart"></div>
  <div class="xax" id="xax"></div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_ipr_explorer.py</code> from
<code>src/digitalmodel/production_engineering/ipr_models.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/production_engineering/ipr_models.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta;
const spec = document.getElementById('spec');
spec.innerHTML = [['Model', M.standard], ['Reservoir pressures', M.pr.length],
  ['PI range', M.pi[0]+'–'+M.pi[M.pi.length-1]+' bopd/psi']]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const selPr = document.getElementById('pr');
M.pr.forEach(p=>selPr.add(new Option(p+' psi', p)));
selPr.value = 3000;
const slider = document.getElementById('pi');
slider.max = M.pi.length-1;

function maxQ(){ let mx=0; for(const pr in DATA.series) for(const pi in DATA.series[pr])
  if(DATA.series[pr][pi].qmax>mx) mx=DATA.series[pr][pi].qmax; return mx; }
const MX = maxQ();

function render(){
  const pr = selPr.value, pi = M.pi[+slider.value], d = DATA.series[pr][pi.toFixed(1)];
  document.getElementById('pival').textContent = pi.toFixed(1)+' bopd/psi';
  document.getElementById('rQ').textContent = d.qmax.toLocaleString()+' bopd';
  const half = d.curve[Math.floor((d.curve.length-1)/2)];
  document.getElementById('rH').textContent = half.q.toLocaleString()+' bopd';
  document.getElementById('chart').innerHTML = d.curve.map(c=>
    `<div class="bar" style="height:${(c.q/MX*100).toFixed(1)}%" title="Pwf ${c.pwf} psi: ${c.q} bopd"></div>`).join('');
  document.getElementById('xax').innerHTML = d.curve.map((c,i)=>
    `<div>${(i%2===0)?c.pwf:''}</div>`).join('');
}
let start = M.pi.indexOf(2.0); if(start<0) start = Math.floor(M.pi.length/2);
slider.value = start; render();
[selPr, slider].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "ipr-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "ipr-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v2["curve"]) for pr in study["series"].values() for v2 in pr.values())
    print(f"wrote {(_OUT_DIR / 'ipr-explorer.html').relative_to(_REPO)} ({n} points)")


if __name__ == "__main__":
    main()
