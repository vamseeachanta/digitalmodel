#!/usr/bin/env python
"""Build the tank-sloshing resonance explorer.

Sweeps fill level for a representative LNG-scale cargo/storage tank and, at each
fill, evaluates the analytical sloshing engine — rectangular & cylindrical
fundamental natural periods (linear potential theory) and the API 650 Annex E
convective period — then flags resonance against a representative vessel roll
period.  The physical point the explorer makes: as fill drops, the sloshing
period *lengthens* and sweeps into the vessel-motion band — the coupling that
external diffraction (AQWA/OrcaWave) does not capture.

Outputs:
    docs/api/structural/sloshing-explorer.html
    docs/api/structural/sloshing-explorer.json

Run:
    python scripts/capabilities/build_sloshing_explorer.py

The engine (src/digitalmodel/hydrodynamics/sloshing.py) is pure-stdlib and is
direct-file-loaded here to avoid the slow top-level ``import digitalmodel``
(OrcaFlex-license retry) that would otherwise stall the build.
"""
from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"
_ENGINE = _REPO / "src" / "digitalmodel" / "hydrodynamics" / "sloshing.py"

# Direct-file-load the pure-stdlib engine (register in sys.modules so the
# module's dataclass annotations resolve).
_spec = importlib.util.spec_from_file_location("dm_sloshing_engine", _ENGINE)
slosh = importlib.util.module_from_spec(_spec)
sys.modules["dm_sloshing_engine"] = slosh
_spec.loader.exec_module(slosh)

# Representative LNG-scale tank + vessel excitation.
_RECT_L = 40.0        # prismatic tank length, m
_CYL_D = 40.0         # cylindrical tank diameter, m
_TANK_H = 26.0        # tank height, m
_ROLL_T = 12.0        # representative vessel roll period, s
_TOL = 0.15           # resonance tolerance (fractional)
_FILLS = [round(2.0 + 0.5 * i, 1) for i in range(0, 49)]   # 2.0 .. 26.0 m


def build_study() -> dict:
    pts = []
    for h in _FILLS:
        t_rect = slosh.rectangular_tank_periods(_RECT_L, h, n_modes=1)[0]
        t_cyl = slosh.cylindrical_tank_periods(_CYL_D, h, n_modes=1)[0]
        tc = slosh.api650_convective_period(_CYL_D, h)
        res = slosh.resonance_check([t_rect, t_cyl], [_ROLL_T], _TOL)
        pts.append({
            "h": h,
            "fillpct": round(100.0 * h / _TANK_H),
            "rect": round(t_rect, 2),
            "cyl": round(t_cyl, 2),
            "tc": round(tc, 2),
            "resonant": bool(res),
        })
    return {
        "meta": {
            "rect_l": _RECT_L, "cyl_d": _CYL_D, "tank_h": _TANK_H,
            "roll_t": _ROLL_T, "tol": _TOL,
            "band_lo": round(_ROLL_T * (1 - _TOL), 2),
            "band_hi": round(_ROLL_T * (1 + _TOL), 2),
            "standard": "Faltinsen linear potential theory + API 650 Annex E",
        },
        "series": pts,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Tank sloshing resonance explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--roll:#7c3aed}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:23px;color:var(--navy);letter-spacing:-.3px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:820px}
 .spec{display:flex;flex-wrap:wrap;gap:8px;margin-bottom:20px}
 .spec span{font-size:12px;font-weight:600;color:var(--muted);background:#fff;
            border:1px solid var(--line);border-radius:20px;padding:4px 11px}
 .spec b{color:var(--navy)}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .ctrl{display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin-bottom:12px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13.5px}
 input[type=range]{flex:1;min-width:220px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:120px}
 .reads{display:flex;gap:22px;flex-wrap:wrap;margin:6px 0}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:20px;font-weight:700;color:var(--navy)}
 .scale{position:relative;height:44px;margin:22px 0 4px;border-radius:9px;overflow:hidden;
        background:#f4f8fc;border:1px solid var(--line)}
 .band{position:absolute;top:0;bottom:0;background:rgba(124,58,237,.15);border-left:2px solid var(--roll);border-right:2px solid var(--roll)}
 .tick{position:absolute;top:-4px;bottom:-4px;width:2px}
 .tick.rect{background:var(--navy)} .tick.cyl{background:var(--teal)} .tick.tc{background:#b8860b}
 .tick span{position:absolute;top:-16px;left:50%;transform:translateX(-50%);font-size:9px;font-weight:700;white-space:nowrap}
 .scalelab{display:flex;justify-content:space-between;font-size:10px;color:var(--muted);padding:0 2px}
 .flag{display:inline-block;font-size:12px;font-weight:700;border-radius:20px;padding:4px 12px;margin-top:12px}
 .flag.ok{background:#e6f5ec;color:var(--ok)} .flag.bad{background:#fdecea;color:var(--bad)}
 .cap{font-size:12px;color:var(--muted);margin-top:12px}
 .cap i{font-style:normal;font-weight:700}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
</style></head><body>
<h1>Tank sloshing &mdash; resonance explorer</h1>
<p class="sub">Analytical sloshing screen for an LNG-scale cargo/storage tank. Drag the fill level to read
the fundamental sloshing period (prismatic and cylindrical, linear potential theory) and the API&nbsp;650
Annex&nbsp;E convective period, and watch them move against the vessel roll-period band. As fill drops the
sloshing period lengthens and sweeps <i>into</i> the vessel-motion band &mdash; the coupling external
diffraction (AQWA/OrcaWave) does not capture. Every point is a live engine evaluation.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label for="h">Fill level</label>
    <input type="range" id="h" min="0" max="0" step="1">
    <span class="wval" id="hval"></span>
  </div>
  <div class="reads">
    <div class="read">Prismatic T&#8321<b id="rR">&mdash;</b></div>
    <div class="read">Cylindrical T&#8321<b id="rC">&mdash;</b></div>
    <div class="read">API 650 Tc<b id="rT" style="color:#b8860b">&mdash;</b></div>
    <div class="read">Vessel roll<b id="rV" style="color:var(--roll)">&mdash;</b></div>
  </div>
  <div class="scale"><div class="band" id="band"></div>
    <div class="tick rect" id="trect"><span style="color:var(--navy)">prism</span></div>
    <div class="tick cyl" id="tcyl"><span style="color:var(--teal)">cyl</span></div>
    <div class="tick tc" id="ttc"><span style="color:#b8860b">Tc</span></div></div>
  <div class="scalelab"><span>4 s</span><span>8</span><span>12</span><span>16</span><span>20 s</span></div>
  <div id="flag"></div>
  <div class="cap">Shaded = vessel roll resonance band (&plusmn;<i id="tolpct"></i> of the roll period). When a
  tank period enters the band, partial-fill sloshing can couple with vessel motion &mdash; the case that warrants
  a VOF free-surface CFD check.</div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_sloshing_explorer.py</code> from
<code>src/digitalmodel/hydrodynamics/sloshing.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/hydrodynamics/sloshing.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, S = DATA.series, LO = 4.0, HI = 20.0;
const pos = v => Math.max(0, Math.min((v-LO)/(HI-LO)*100, 100));
const spec = document.getElementById('spec');
spec.innerHTML = [['Method', M.standard], ['Prismatic L', M.rect_l+' m'],
  ['Cylindrical D', M.cyl_d+' m'], ['Tank height', M.tank_h+' m'],
  ['Vessel roll', M.roll_t+' s']]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
document.getElementById('tolpct').textContent = Math.round(M.tol*100)+'%';
document.getElementById('rV').textContent = M.roll_t.toFixed(1)+' s';
const band = document.getElementById('band');
band.style.left = pos(M.band_lo)+'%';
band.style.width = (pos(M.band_hi)-pos(M.band_lo))+'%';
const slider = document.getElementById('h');
slider.max = S.length-1;

function render(){
  const p = S[+slider.value];
  document.getElementById('hval').textContent = p.h.toFixed(1)+' m ('+p.fillpct+'%)';
  document.getElementById('rR').textContent = p.rect.toFixed(2)+' s';
  document.getElementById('rC').textContent = p.cyl.toFixed(2)+' s';
  document.getElementById('rT').textContent = p.tc.toFixed(2)+' s';
  document.getElementById('trect').style.left = pos(p.rect)+'%';
  document.getElementById('tcyl').style.left = pos(p.cyl)+'%';
  document.getElementById('ttc').style.left = pos(p.tc)+'%';
  const f = document.getElementById('flag');
  if(p.resonant){ f.className='flag bad'; f.textContent='⚠ Resonance risk — a tank period is within the roll band → CFD check warranted'; }
  else { f.className='flag ok'; f.textContent='✓ No resonance — tank periods clear of the roll band'; }
}
let start = S.findIndex(p=>p.fillpct>=95); if(start<0) start = S.length-1;
slider.value = start; render();
slider.addEventListener('input', render);
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "sloshing-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "sloshing-explorer.html").write_text(html, encoding="utf-8")
    n_res = sum(1 for p in study["series"] if p["resonant"])
    print(f"wrote {(_OUT_DIR / 'sloshing-explorer.html').relative_to(_REPO)} "
          f"({len(study['series'])} fills, {n_res} resonant)")


if __name__ == "__main__":
    main()
