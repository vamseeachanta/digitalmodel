#!/usr/bin/env python
"""Build the cathodic-protection (sacrificial-anode) explorer.

Runs the DNV-RP-B401 ``marine_structure_current_demand`` engine over a jacket
and a monopile across coating profiles, climate regions and a design-life
sweep, then writes a self-contained interactive HTML dashboard (data embedded
as JSON, no external assets) plus the raw study JSON.

Outputs:
    docs/api/structural/cathodic-protection-explorer.html
    docs/api/structural/cathodic-protection-explorer.json

Run:
    python scripts/capabilities/build_cathodic_protection_explorer.py
"""
from __future__ import annotations

import json
import warnings
from pathlib import Path

warnings.filterwarnings("ignore")  # DNV edition-deprecation warnings fire per call

from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    marine_structure_current_demand,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

# --- DNV-RP-B401 anode constants (Al-Zn-In stand-off) -----------------------
_ANODE_NET_MASS_KG = 200.0
_ANODE_CAPACITY = 2000.0        # A-h/kg
_UTIL = 0.90

# --- reference structures (exposure-zone areas, m^2) ------------------------
_STRUCTURES = {
    "4-leg jacket": [
        ("splash", ExposureZone.SPLASH, 220.0),
        ("tidal", ExposureZone.TIDAL, 180.0),
        ("submerged", ExposureZone.SUBMERGED, 2400.0),
        ("buried_mudline", ExposureZone.BURIED_MUDLINE, 320.0),
    ],
    "XL monopile": [
        ("splash", ExposureZone.SPLASH, 120.0),
        ("tidal", ExposureZone.TIDAL, 95.0),
        ("submerged", ExposureZone.SUBMERGED, 760.0),
        ("buried_mudline", ExposureZone.BURIED_MUDLINE, 540.0),
    ],
}

# --- coating profiles (per-zone coating-breakdown factor) -------------------
_COATINGS = {
    "bare": {ExposureZone.SPLASH: 1.0, ExposureZone.TIDAL: 1.0,
             ExposureZone.SUBMERGED: 1.0, ExposureZone.BURIED_MUDLINE: 1.0},
    "aged-coating": {ExposureZone.SPLASH: 1.0, ExposureZone.TIDAL: 0.70,
                     ExposureZone.SUBMERGED: 0.95, ExposureZone.BURIED_MUDLINE: 1.0},
    "good-coating": {ExposureZone.SPLASH: 1.0, ExposureZone.TIDAL: 0.40,
                     ExposureZone.SUBMERGED: 0.90, ExposureZone.BURIED_MUDLINE: 1.0},
    "premium-coating": {ExposureZone.SPLASH: 1.0, ExposureZone.TIDAL: 0.15,
                        ExposureZone.SUBMERGED: 0.60, ExposureZone.BURIED_MUDLINE: 1.0},
}

_CLIMATES = [c.value for c in (ClimateRegion.TEMPERATE, ClimateRegion.TROPICAL)]
_LIVES = [round(10.0 + 5.0 * i, 1) for i in range(0, 7)]   # 10 .. 40 yr, 5-yr steps


def _zones(structure: str, coating: str):
    prof = _COATINGS[coating]
    return [StructuralZone(zone_name=n, exposure_zone=z, surface_area_m2=a,
                           coating_breakdown_factor=prof[z])
            for (n, z, a) in _STRUCTURES[structure]]


def build_study() -> dict:
    climate_by_value = {c.value: c for c in ClimateRegion}
    series = {}
    for s in _STRUCTURES:
        series[s] = {}
        for cl in _CLIMATES:
            series[s][cl] = {}
            for coat in _COATINGS:
                pts = []
                for life in _LIVES:
                    r = marine_structure_current_demand(
                        zones=_zones(s, coat), climate_region=climate_by_value[cl],
                        design_life_years=life, anode_net_mass_kg=_ANODE_NET_MASS_KG,
                        anode_capacity_Ah_kg=_ANODE_CAPACITY, utilization_factor=_UTIL,
                    )
                    pts.append({
                        "l": life,
                        "mass": round(float(r.total_anode_mass_kg), 1),
                        "n": int(r.number_of_anodes),
                        "im": round(float(r.total_mean_current_A), 2),
                        "if": round(float(r.total_final_current_A), 2),
                    })
                series[s][cl][coat] = pts
    return {
        "meta": {
            "structures": list(_STRUCTURES.keys()),
            "areas": {s: {n: a for (n, _z, a) in z} for s, z in _STRUCTURES.items()},
            "climates": _CLIMATES, "coatings": list(_COATINGS.keys()),
            "lives": _LIVES, "anode_kg": _ANODE_NET_MASS_KG,
            "capacity": _ANODE_CAPACITY, "util": _UTIL,
            "standard": "DNV-RP-B401",
        },
        "series": series,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cathodic-protection anode explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--teal2:#12A6B0}
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
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);
       font-size:16px;min-width:70px}
 .row{display:grid;grid-template-columns:150px 1fr 120px;align-items:center;gap:10px;margin:8px 0}
 .row .name{font-size:12.5px;font-weight:700;color:var(--ink);text-align:right}
 .track{position:relative;background:var(--soft);border:1px solid var(--line);
        border-radius:7px;height:26px;overflow:hidden}
 .fill{position:absolute;top:0;left:0;height:100%;border-radius:6px 0 0 6px;
       background:linear-gradient(90deg,var(--teal),var(--navy));transition:width .12s}
 .glab{position:absolute;left:8px;top:50%;transform:translateY(-50%);font-size:11px;
       font-weight:600;color:#fff;white-space:nowrap;text-shadow:0 1px 2px rgba(0,0,0,.25)}
 .uval{font-family:ui-monospace,Menlo,monospace;font-size:13px;font-weight:700;text-align:right;color:var(--navy)}
 table{width:100%;border-collapse:collapse;font-size:13px;margin-top:4px}
 th,td{padding:7px 10px;text-align:left;border-bottom:1px solid var(--line)}
 th{background:var(--soft);color:var(--muted);font-size:11px;text-transform:uppercase;letter-spacing:.4px}
 td.n{font-family:ui-monospace,Menlo,monospace;text-align:right}
 .foot{color:var(--muted);font-size:12.5px;margin-top:8px}
 .foot a{color:var(--teal)}
 .hint{font-size:12px;color:var(--muted);margin:2px 0 8px}
</style></head><body>
<h1>Cathodic protection &mdash; sacrificial-anode explorer</h1>
<p class="sub">DNV-RP-B401 sacrificial-anode design for a fixed offshore structure. Pick a structure and
seawater climate, then drag the design life to watch the total anode mass grow &mdash; and see how much
a better external coating cuts it. Every value is computed live by
<code>marine_structure_current_demand</code> (current demand &rarr; anode mass &rarr; anode count),
not mocked.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Structure</label><select id="struct"></select>
    <label>Seawater climate</label><select id="climate"></select>
  </div>
  <div class="ctrl">
    <label for="life">Design life</label>
    <input type="range" id="life" min="0" max="0" step="1">
    <span class="wval" id="lval"></span>
  </div>
  <div class="hint">Total sacrificial-anode mass by external-coating quality (bar = kg of Al-Zn-In anode).</div>
  <div id="bars"></div>
</div>

<div class="panel">
  <h3 style="font-size:15px;color:var(--navy);margin-bottom:6px">Design breakdown at selected life</h3>
  <table><thead><tr><th>Coating</th><th>Mean current (A)</th><th>Final current (A)</th>
    <th>Anode mass (kg)</th><th>Anodes (200 kg)</th></tr></thead><tbody id="tbody"></tbody></table>
  <p class="foot">Anode: 200 kg net Al-Zn-In, 2000 A&middot;h/kg, utilization 0.90. Standard: DNV-RP-B401.</p>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_cathodic_protection_explorer.py</code> from
<code>src/digitalmodel/cathodic_protection/</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/cathodic_protection">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, LIVES = M.lives, COATS = M.coatings;
const spec = document.getElementById('spec');
spec.innerHTML = [
  ['Standard', M.standard], ['Anode', M.anode_kg+' kg Al-Zn-In'],
  ['Capacity', M.capacity+' A·h/kg'], ['Utilization', M.util],
  ['Coatings', COATS.length], ['Climates', M.climates.length],
].map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');

const selS = document.getElementById('struct'), selC = document.getElementById('climate');
M.structures.forEach(s=>selS.add(new Option(s,s)));
M.climates.forEach(c=>selC.add(new Option(c,c)));
selC.value = 'temperate';
const life = document.getElementById('life');
life.max = LIVES.length-1;

function maxMass(){
  let mx = 0;
  for(const s of M.structures) for(const c of M.climates) for(const k of COATS)
    for(const p of DATA.series[s][c][k]) if(p.mass>mx) mx=p.mass;
  return mx;
}
const MX = maxMass();

function render(){
  const s = selS.value, c = selC.value, idx = +life.value;
  document.getElementById('lval').textContent = LIVES[idx].toFixed(1)+' yr';
  document.getElementById('bars').innerHTML = COATS.map(k=>{
    const p = DATA.series[s][c][k][idx];
    const pct = (p.mass/MX*100).toFixed(1);
    return `<div class="row"><div class="name">${k}</div>
      <div class="track"><div class="fill" style="width:${pct}%"></div>
      <div class="glab">${p.mass.toLocaleString()} kg &middot; ${p.n} anodes</div></div>
      <div class="uval">${p.mass.toLocaleString()} kg</div></div>`;
  }).join('');
  document.getElementById('tbody').innerHTML = COATS.map(k=>{
    const p = DATA.series[s][c][k][idx];
    return `<tr><td>${k}</td><td class="n">${p.im.toFixed(2)}</td>
      <td class="n">${p.if.toFixed(2)}</td>
      <td class="n">${p.mass.toLocaleString()}</td><td class="n">${p.n}</td></tr>`;
  }).join('');
}
let start = LIVES.indexOf(25.0); if(start<0) start = Math.floor(LIVES.length/2);
life.value = start; render();
[selS, selC, life].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "cathodic-protection-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "cathodic-protection-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v2) for s in study["series"].values()
            for c in s.values() for v2 in c.values())
    print(f"wrote {(_OUT_DIR / 'cathodic-protection-explorer.html').relative_to(_REPO)} "
          f"({n} points)")


if __name__ == "__main__":
    main()
