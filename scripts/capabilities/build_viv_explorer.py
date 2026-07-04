#!/usr/bin/env python
"""Build the VIV reduced-velocity lock-in screening explorer.

Runs the subsea VIV screening engine over representative tubular members across
a current-velocity sweep, then writes a self-contained interactive HTML
dashboard + raw JSON.

Outputs:
    docs/api/structural/viv-explorer.html
    docs/api/structural/viv-explorer.json

Run:
    python scripts/capabilities/build_viv_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.subsea.viv_analysis.screening import VIVScreening

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

_VR_MIN, _VR_MAX = 4.0, 8.0     # cross-flow lock-in band (DNV-RP-C205)

# representative members: (label, natural frequency Hz, diameter m)
_MEMBERS = {
    "Drilling riser joint": (0.30, 0.53),
    "Free-spanning pipeline": (0.80, 0.32),
    "Well conductor": (1.10, 0.76),
}
_V = [round(0.1 + 0.1 * i, 1) for i in range(0, 25)]   # 0.1 .. 2.5 m/s


def build_study() -> dict:
    scr = VIVScreening()
    series = {}
    for name, (fn, d) in _MEMBERS.items():
        pts = []
        for v in _V:
            vr = float(scr.reduced_velocity(current_velocity=v, natural_frequency=fn, diameter=d))
            is_li, margin, status = scr.check_lock_in(vr, vr_min=_VR_MIN, vr_max=_VR_MAX)
            sf = float(scr.calculate_safety_factor(vr, vr_min=_VR_MIN, vr_max=_VR_MAX))
            pts.append({"v": v, "vr": round(vr, 3), "status": status,
                        "sf": round(sf, 3)})
        series[name] = pts
    return {
        "meta": {
            "members": {n: {"fn": fn, "d": d} for n, (fn, d) in _MEMBERS.items()},
            "v": _V, "vr_min": _VR_MIN, "vr_max": _VR_MAX,
            "standard": "DNV-RP-C205 reduced-velocity lock-in",
        },
        "series": series,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>VIV lock-in screening explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--warn:#b7791f;--bad:#c0392b}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:23px;color:var(--navy);letter-spacing:-.3px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:800px}
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
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:6px 0}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;font-weight:700}
 .pill{display:inline-block;padding:3px 12px;border-radius:20px;font-weight:700;font-size:15px}
 .safe{background:#e6f5ec;color:var(--ok)} .marginal{background:#fbf0dd;color:var(--warn)}
 .lockin{background:#fbe6e3;color:var(--bad)}
 .scale{position:relative;height:46px;margin:16px 0 6px;border-radius:9px;overflow:hidden;
        background:linear-gradient(90deg,#e6f5ec 0%,#e6f5ec 33%,#fbf0dd 33%,#fbf0dd 40%,#fbe6e3 40%,#fbe6e3 80%,#fbf0dd 80%,#fbf0dd 100%);border:1px solid var(--line)}
 .band{position:absolute;top:0;bottom:0;border-left:2px dashed var(--bad);border-right:2px dashed var(--bad)}
 .marker{position:absolute;top:-4px;bottom:-4px;width:3px;background:var(--navy);transition:left .1s}
 .marker::after{content:attr(data-v);position:absolute;top:-16px;left:50%;transform:translateX(-50%);
   font-size:10px;font-weight:700;color:var(--navy);white-space:nowrap}
 .scalelab{display:flex;justify-content:space-between;font-size:10px;color:var(--muted);padding:0 2px}
 .cap{font-size:12px;color:var(--muted);margin-top:8px}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
</style></head><body>
<h1>Vortex-induced vibration &mdash; lock-in screening explorer</h1>
<p class="sub">First-pass VIV screening on the reduced-velocity method: a member locks in to
cross-flow vortex shedding when V<sub>r</sub> = V/(f<sub>n</sub>&middot;D) enters the 4&ndash;8 band
(DNV-RP-C205). Pick a member and drag the current to see it cross into lock-in. Every point is a
live <code>VIVScreening</code> evaluation.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Member</label><select id="mem"></select>
    <label for="v">Current velocity</label>
    <input type="range" id="v" min="0" max="0" step="1">
    <span class="wval" id="vval"></span>
  </div>
  <div class="reads">
    <div class="read">Reduced velocity V<sub>r</sub><b id="rVr" style="color:var(--navy)">&mdash;</b></div>
    <div class="read">Status<b><span class="pill" id="rSt">&mdash;</span></b></div>
    <div class="read">Safety factor<b id="rSf" style="color:var(--navy)">&mdash;</b></div>
  </div>
  <div class="scale"><div class="marker" id="mark" data-v=""></div></div>
  <div class="scalelab"><span>V_r 0</span><span>4 (lock-in)</span><span>8</span><span>10</span></div>
  <div class="cap">Green = below onset, amber = marginal, red = in the 4&ndash;8 cross-flow lock-in band. Marker = current V<sub>r</sub>.</div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_viv_explorer.py</code> from
<code>src/digitalmodel/subsea/viv_analysis/screening.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/subsea/viv_analysis/screening.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, V = M.v, MEM = M.members, SCALEMAX = 10.0;
const spec = document.getElementById('spec');
spec.innerHTML = [['Standard', M.standard], ['Lock-in band', M.vr_min+'–'+M.vr_max+' V_r'],
  ['Members', Object.keys(MEM).length]]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const selM = document.getElementById('mem');
Object.keys(MEM).forEach(m=>selM.add(new Option(m+' (f_n '+MEM[m].fn+' Hz, D '+MEM[m].d+' m)', m)));
const slider = document.getElementById('v');
slider.max = V.length-1;
const CLS = {safe:'safe', marginal:'marginal', 'lock-in':'lockin'};

function render(){
  const m = selM.value, idx = +slider.value, p = DATA.series[m][idx];
  document.getElementById('vval').textContent = V[idx].toFixed(1)+' m/s';
  document.getElementById('rVr').textContent = p.vr.toFixed(2);
  const st = document.getElementById('rSt');
  st.textContent = p.status; st.className = 'pill '+(CLS[p.status]||'safe');
  document.getElementById('rSf').textContent = p.sf.toFixed(2);
  const mk = document.getElementById('mark');
  mk.style.left = Math.min(p.vr/SCALEMAX*100, 100).toFixed(1)+'%';
  mk.setAttribute('data-v', p.vr.toFixed(1));
}
selM.value = Object.keys(MEM)[0];
let start = V.indexOf(1.0); if(start<0) start = Math.floor(V.length/2);
slider.value = start; render();
[selM, slider].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "viv-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "viv-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v) for v in study["series"].values())
    print(f"wrote {(_OUT_DIR / 'viv-explorer.html').relative_to(_REPO)} ({n} points)")


if __name__ == "__main__":
    main()
