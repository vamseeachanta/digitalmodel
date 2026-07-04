#!/usr/bin/env python
"""Build the ship resistance & powering explorer.

Runs the Holtrop-Mennen resistance method over a few representative hull forms
across a speed sweep, then writes a self-contained interactive HTML dashboard
(data embedded as JSON, no external assets) plus the raw study JSON.

Outputs:
    docs/api/structural/ship-resistance-explorer.html
    docs/api/structural/ship-resistance-explorer.json

Run:
    python scripts/capabilities/build_ship_resistance_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.naval_architecture import holtrop_mennen as hm

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"
_KN = 1852.0 / 3600.0   # knots -> m/s

# representative screening hull forms (typical block/section/waterplane coeffs)
_HULLS = {
    "Handysize tanker": dict(lwl=175.0, beam=32.0, draft=11.0, cb=0.82, cm=0.99,
                             cwp=0.88, cp=0.828, lcb_pct=-1.0, cstern=0,
                             abt=0.0, tf=11.0, hb=0.0, at=0.0),
    "Supramax bulker": dict(lwl=190.0, beam=32.2, draft=12.5, cb=0.84, cm=0.99,
                            cwp=0.89, cp=0.848, lcb_pct=-1.5, cstern=0,
                            abt=0.0, tf=12.5, hb=0.0, at=0.0),
    "Feeder container": dict(lwl=145.0, beam=23.0, draft=8.5, cb=0.62, cm=0.97,
                             cwp=0.75, cp=0.639, lcb_pct=-0.5, cstern=0,
                             abt=0.0, tf=8.5, hb=0.0, at=0.0),
}
_SPEED_KN = [round(6.0 + 0.5 * i, 1) for i in range(0, 25)]   # 6 .. 18 kn


def build_study() -> dict:
    series = {}
    for name, hull in _HULLS.items():
        pts = []
        for vk in _SPEED_KN:
            v = vk * _KN
            r = float(hm.total_resistance(speed_ms=v, **hull))
            p = float(hm.effective_power(r, v))
            fr = v / (9.80665 * hull["lwl"]) ** 0.5
            pts.append({"v": vk, "r": round(r / 1000.0, 1), "p": round(p / 1000.0, 1),
                        "fn": round(fr, 3)})
        series[name] = pts
    return {
        "meta": {
            "hulls": {n: {"lwl": h["lwl"], "beam": h["beam"], "draft": h["draft"], "cb": h["cb"]}
                      for n, h in _HULLS.items()},
            "speed_kn": _SPEED_KN, "standard": "Holtrop-Mennen (1982/1984)",
        },
        "series": series,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Ship resistance &amp; powering explorer — digitalmodel</title>
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
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:70px}
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:4px 0 2px}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;color:var(--navy);font-weight:700}
 .chart{margin-top:14px;display:flex;align-items:flex-end;gap:3px;height:200px;
        border-bottom:2px solid var(--line);border-left:2px solid var(--line);padding-left:4px}
 .bar{flex:1;background:linear-gradient(180deg,var(--teal),var(--navy));border-radius:3px 3px 0 0;
      position:relative;min-height:1px;opacity:.5;transition:opacity .1s}
 .bar.on{opacity:1;outline:2px solid var(--navy);outline-offset:1px}
 .xax{display:flex;gap:3px;padding-left:6px;margin-top:4px}
 .xax div{flex:1;text-align:center;font-size:9px;color:var(--muted)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:8px}.foot a{color:var(--teal)}
 .cap{font-size:12px;color:var(--muted);margin-top:8px}
</style></head><body>
<h1>Ship resistance &amp; powering &mdash; Holtrop-Mennen explorer</h1>
<p class="sub">Calm-water resistance and effective power for representative merchant hull forms,
by the Holtrop-Mennen statistical method. Pick a hull and drag the speed to trace the
resistance-power curve &mdash; every point is a live <code>total_resistance</code> /
<code>effective_power</code> evaluation, not mocked.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Hull form</label><select id="hull"></select>
    <label for="v">Ship speed</label>
    <input type="range" id="v" min="0" max="0" step="1">
    <span class="wval" id="vval"></span>
  </div>
  <div class="reads">
    <div class="read">Total resistance<b id="rR">&mdash;</b></div>
    <div class="read">Effective power<b id="rP">&mdash;</b></div>
    <div class="read">Froude number<b id="rF">&mdash;</b></div>
  </div>
  <div class="cap">Effective power P<sub>E</sub> = R<sub>T</sub>&middot;V (bar height); the selected speed is outlined.</div>
  <div class="chart" id="chart"></div>
  <div class="xax" id="xax"></div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_ship_resistance_explorer.py</code> from
<code>src/digitalmodel/naval_architecture/holtrop_mennen.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/holtrop_mennen.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, V = M.speed_kn, HULLS = Object.keys(DATA.series);
const spec = document.getElementById('spec');
spec.innerHTML = [['Method', M.standard], ['Hull forms', HULLS.length],
  ['Speed range', V[0]+'–'+V[V.length-1]+' kn']]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const selH = document.getElementById('hull');
HULLS.forEach(h=>selH.add(new Option(h,h)));
const slider = document.getElementById('v');
slider.max = V.length-1;

function maxP(){ let mx=0; for(const h of HULLS) for(const p of DATA.series[h]) if(p.p>mx) mx=p.p; return mx; }
const MXP = maxP();

function render(){
  const h = selH.value, idx = +slider.value, pts = DATA.series[h], p = pts[idx];
  document.getElementById('vval').textContent = V[idx].toFixed(1)+' kn';
  const g = M.hulls[h];
  document.getElementById('rR').textContent = p.r.toLocaleString()+' kN';
  document.getElementById('rP').textContent = p.p.toLocaleString()+' kW';
  document.getElementById('rF').textContent = p.fn.toFixed(3);
  document.getElementById('chart').innerHTML = pts.map((q,i)=>
    `<div class="bar ${i===idx?'on':''}" style="height:${(q.p/MXP*100).toFixed(1)}%" title="${V[i]} kn: ${q.p} kW"></div>`).join('');
  document.getElementById('xax').innerHTML = pts.map((q,i)=>
    `<div>${(i%4===0)?V[i]:''}</div>`).join('');
}
let start = V.indexOf(14.0); if(start<0) start = Math.floor(V.length/2);
slider.value = start; render();
[selH, slider].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "ship-resistance-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "ship-resistance-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v) for v in study["series"].values())
    print(f"wrote {(_OUT_DIR / 'ship-resistance-explorer.html').relative_to(_REPO)} ({n} points)")


if __name__ == "__main__":
    main()
