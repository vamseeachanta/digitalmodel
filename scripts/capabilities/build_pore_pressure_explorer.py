#!/usr/bin/env python
"""Build the drilling pore-pressure / mud-weight explorer.

Runs the Eaton and Bowers pore-pressure predictors over a representative
overpressured well (synthetic normal-compaction sonic log with an overpressure
ramp), then writes a self-contained interactive HTML dashboard + raw JSON.

Outputs:
    docs/api/structural/pore-pressure-explorer.html
    docs/api/structural/pore-pressure-explorer.json

Run:
    python scripts/capabilities/build_pore_pressure_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.well.drilling.pore_pressure import (
    bowers_pore_pressure,
    eaton_pore_pressure,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

_PN = 8.6                         # normal pore-pressure gradient, ppg
_ONSET_FT = 8000.0                # overpressure onset depth
_DEPTH = [float(5000 + 500 * i) for i in range(0, 19)]   # 5000 .. 14000 ft
_KICK_MARGIN = 0.3                # ppg trip/kick margin on min mud weight


def _overburden(z: float) -> float:
    # linear 16.5 ppg at 5000 ft -> 19.0 ppg at 14000 ft
    return 16.5 + (19.0 - 16.5) * (z - 5000.0) / 9000.0


def _dt_normal(z: float) -> float:
    return 170.0 - 0.004 * z       # normal-compaction sonic trend, us/ft


def _dt_observed(z: float) -> float:
    dtn = _dt_normal(z)
    if z < _ONSET_FT:
        return dtn
    # undercompaction ramp: observed transit time up to 1.35x normal by 14000 ft
    r = 1.0 + 0.35 * (z - _ONSET_FT) / (14000.0 - _ONSET_FT)
    return dtn * r


def build_study() -> dict:
    pts = []
    for z in _DEPTH:
        sv = _overburden(z)
        dtn, dto = _dt_normal(z), _dt_observed(z)
        eaton = float(eaton_pore_pressure(overburden_emw_ppg=sv, normal_pressure_emw_ppg=_PN,
                                          observed=dto, normal=dtn, log_type="sonic"))
        bowers = float(bowers_pore_pressure(overburden_emw_ppg=sv, tvd_ft=z, sonic_dt_us_ft=dto))
        pp = max(eaton, bowers)          # governing pore prediction
        pts.append({"z": int(z), "sv": round(sv, 2), "eaton": round(eaton, 2),
                    "bowers": round(bowers, 2), "pp": round(pp, 2),
                    "mw": round(pp + _KICK_MARGIN, 2)})
    return {
        "meta": {
            "pn": _PN, "onset_ft": int(_ONSET_FT), "kick_margin": _KICK_MARGIN,
            "sv_max": 19.0, "standard": "Eaton (1975) + Bowers (1995) pore pressure",
        },
        "series": pts,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Pore-pressure / mud-weight explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--mw:#7c3aed}
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
 input[type=range]{flex:1;min-width:220px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:90px}
 .reads{display:flex;gap:22px;flex-wrap:wrap;margin:6px 0}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:20px;font-weight:700;color:var(--navy)}
 .scale{position:relative;height:40px;margin:14px 0 4px;border-radius:9px;overflow:hidden;
        background:linear-gradient(90deg,#e6f5ec,#e6f5ec);border:1px solid var(--line)}
 .win{position:absolute;top:0;bottom:0;background:rgba(31,157,87,.16);border-left:2px solid var(--ok);border-right:2px solid var(--bad)}
 .tick{position:absolute;top:-3px;bottom:-3px;width:2px}
 .tick.pp{background:var(--bad)} .tick.mw{background:var(--mw)} .tick.sv{background:var(--navy)}
 .tick span{position:absolute;top:-15px;left:50%;transform:translateX(-50%);font-size:9px;font-weight:700;white-space:nowrap}
 .scalelab{display:flex;justify-content:space-between;font-size:10px;color:var(--muted);padding:0 2px}
 .cap{font-size:12px;color:var(--muted);margin-top:10px}
 .cap i{font-style:normal;font-weight:700}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
</style></head><body>
<h1>Pore pressure &amp; mud weight &mdash; drilling explorer</h1>
<p class="sub">Multi-method pore-pressure prediction down a representative overpressured well: the Eaton
and Bowers predictors on a synthetic normal-compaction sonic log with an overpressure ramp below
8,000&nbsp;ft. Drag the bit depth to read the governing pore gradient, the minimum safe mud weight,
and the overburden ceiling. Every point is a live <code>eaton_pore_pressure</code> /
<code>bowers_pore_pressure</code> evaluation.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label for="z">Bit depth (TVD)</label>
    <input type="range" id="z" min="0" max="0" step="1">
    <span class="wval" id="zval"></span>
  </div>
  <div class="reads">
    <div class="read">Eaton pore<b id="rE">&mdash;</b></div>
    <div class="read">Bowers pore<b id="rB">&mdash;</b></div>
    <div class="read">Min. mud weight<b id="rM" style="color:var(--mw)">&mdash;</b></div>
    <div class="read">Overburden<b id="rS">&mdash;</b></div>
  </div>
  <div class="scale"><div class="win" id="win"></div>
    <div class="tick pp" id="tpp"><span style="color:var(--bad)">pore</span></div>
    <div class="tick mw" id="tmw"><span style="color:var(--mw)">mud</span></div>
    <div class="tick sv" id="tsv"><span style="color:var(--navy)">S_v</span></div></div>
  <div class="scalelab"><span>8 ppg</span><span>12</span><span>16</span><span>20 ppg</span></div>
  <div class="cap">Mud-weight window at this depth: keep mud above the <i style="color:var(--bad)">pore</i> gradient (kick) and below the <i style="color:var(--navy)">overburden</i> ceiling. Shaded = the safe band above min mud weight.</div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_pore_pressure_explorer.py</code> from
<code>src/digitalmodel/well/drilling/pore_pressure.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/well/drilling/pore_pressure.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, S = DATA.series, LO = 8.0, HI = 20.0;
const pos = v => Math.max(0, Math.min((v-LO)/(HI-LO)*100, 100));
const spec = document.getElementById('spec');
spec.innerHTML = [['Methods', M.standard], ['Normal gradient', M.pn+' ppg'],
  ['Overpressure onset', M.onset_ft.toLocaleString()+' ft']]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const slider = document.getElementById('z');
slider.max = S.length-1;

function render(){
  const p = S[+slider.value];
  document.getElementById('zval').textContent = p.z.toLocaleString()+' ft';
  document.getElementById('rE').textContent = p.eaton.toFixed(2)+' ppg';
  document.getElementById('rB').textContent = p.bowers.toFixed(2)+' ppg';
  document.getElementById('rM').textContent = p.mw.toFixed(2)+' ppg';
  document.getElementById('rS').textContent = p.sv.toFixed(2)+' ppg';
  const win = document.getElementById('win');
  win.style.left = pos(p.mw)+'%'; win.style.width = Math.max(0, pos(p.sv)-pos(p.mw))+'%';
  document.getElementById('tpp').style.left = pos(p.pp)+'%';
  document.getElementById('tmw').style.left = pos(p.mw)+'%';
  document.getElementById('tsv').style.left = pos(p.sv)+'%';
}
let start = S.findIndex(p=>p.z===11000); if(start<0) start = Math.floor(S.length/2);
slider.value = start; render();
slider.addEventListener('input', render);
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "pore-pressure-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "pore-pressure-explorer.html").write_text(html, encoding="utf-8")
    print(f"wrote {(_OUT_DIR / 'pore-pressure-explorer.html').relative_to(_REPO)} "
          f"({len(study['series'])} depths)")


if __name__ == "__main__":
    main()
