#!/usr/bin/env python
"""Build the tank-sloshing natural-period & resonance explorer.

Client-facing capability page. Three tiers, all engine-driven:

1. NORMALIZED MASTER CURVE (top) — the dimensionless fundamental sloshing
   frequency parameter Omega1 = omega_1*sqrt(Lc/g) vs the fill/slenderness ratio
   h/Lc, one curve per tank shape (rectangular, upright cylinder, horizontal
   cylinder = road tanker, elliptical/obround). Any physical size collapses onto
   these curves; read Omega1 off the curve and de-normalise to a real period.
2. SHAPE LOOKUP (dropdown) — pick a shape and a real size + fill, get the actual
   natural period(s) and a small lookup table.
3. RESONANCE SCREEN — the marine case: drag fill level for an LNG-scale tank and
   watch the sloshing period sweep the vessel roll-period band (the coupling
   external diffraction misses — the ACMA B1546 ballast-tank-as-tuned-tank study).

Plus a worldwide-relationship reference basis and the CFD tie-in.

Outputs:
    docs/api/structural/sloshing-explorer.html
    docs/api/structural/sloshing-explorer.json

The engine (src/digitalmodel/hydrodynamics/sloshing.py) is pure-stdlib and is
direct-file-loaded to avoid the slow top-level ``import digitalmodel``.
"""
from __future__ import annotations

import importlib.util
import json
import math
import sys
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"
_ENGINE = _REPO / "src" / "digitalmodel" / "hydrodynamics" / "sloshing.py"

_spec = importlib.util.spec_from_file_location("dm_sloshing_engine", _ENGINE)
slosh = importlib.util.module_from_spec(_spec)
sys.modules["dm_sloshing_engine"] = slosh
_spec.loader.exec_module(slosh)
G = slosh.G_STD


def _omega(T):
    return 2.0 * math.pi / T


def _norm_curve(kind):
    """(x = h/Lc, Om = omega_1*sqrt(Lc/g)) points for the master chart."""
    pts = []
    if kind == "rectangular":
        # Lc = L; x = h/L; Om = sqrt(pi*tanh(pi*x)) exactly.
        x = 0.05
        while x <= 1.5001:
            pts.append((round(x, 3), round(math.sqrt(math.pi * math.tanh(math.pi * x)), 4)))
            x += 0.05
    elif kind == "upright-cylinder":
        # Lc = D = 2R; x = h/D; sweep with D = 1.
        D = 1.0
        x = 0.05
        while x <= 1.5001:
            T = slosh.cylindrical_tank_periods(D, x * D, n_modes=1)[0]
            pts.append((round(x, 3), round(_omega(T) * math.sqrt(D / G), 4)))
            x += 0.05
    elif kind == "horizontal-cylinder":
        # Lc = D; x = h/D = fill fraction F; D = 1. Equivalent-rectangle is
        # trustworthy over the mid-fill range — stop before the near-full blow-up.
        D = 1.0
        for i in range(1, 17):
            F = i * 0.05  # 0.05 .. 0.80
            T = slosh.horizontal_cylinder_periods(D, F, n_modes=1)[0]
            om = _omega(T) * math.sqrt(D / G)
            if om > 1.95:
                break
            pts.append((round(F, 3), round(om, 4)))
    elif kind == "elliptical":
        # Road-tanker oval AR = width/height = 1.5; Lc = width W; x = h/W = F/AR.
        W, AR = 1.0, 1.5
        H = W / AR
        for i in range(1, 17):
            F = i * 0.05  # 0.05 .. 0.80
            T = slosh.oval_tank_periods(W, H, F, n_modes=1)[0]
            om = _omega(T) * math.sqrt(W / G)
            if om > 1.95:
                break
            pts.append((round(F / AR, 4), round(om, 4)))
    return pts


SHAPES = [
    {
        "key": "rectangular", "label": "Rectangular / prismatic",
        "lc": "L (length in the excitation direction)",
        "formula": "omega_1^2 = (pi g / L) tanh(pi h / L)",
        "ratio": "h / L (depth / length)", "deep": 1.7725,
        "note": "LNG membrane, seismic storage, ship cargo tanks. Exact linear potential theory.",
        "default": {"span": 40.0, "fill_ratio": 0.5, "span_label": "Length L (m)"},
    },
    {
        "key": "upright-cylinder", "label": "Upright cylinder (vertical axis)",
        "lc": "D (diameter)",
        "formula": "omega_1^2 = (1.841 g / R) tanh(1.841 h / R)",
        "ratio": "h / D (depth / diameter)", "deep": 1.9187,
        "note": "Storage tanks, spar/CALM, spherical-limit. First J1' root 1.841. API 650 Annex E convective.",
        "default": {"span": 40.0, "fill_ratio": 0.5, "span_label": "Diameter D (m)"},
    },
    {
        "key": "horizontal-cylinder", "label": "Horizontal cylinder (road tanker)",
        "lc": "D (diameter)",
        "formula": "equivalent rectangle: omega_1^2 = g (pi / b_s) tanh(pi h_eq / b_s)",
        "ratio": "fill fraction F = h / D", "deep": None,
        "note": "Road tankers, rail tank cars, IMO Type-C. Equivalent-rectangle; best at mid fill.",
        "default": {"span": 2.4, "fill_ratio": 0.5, "span_label": "Diameter D (m)"},
    },
    {
        "key": "elliptical", "label": "Elliptical / obround (fuel tanker)",
        "lc": "W (cross-section width)",
        "formula": "equivalent rectangle on the oval cross-section (a, b_s, h_eq)",
        "ratio": "fill fraction F (at width/height = 1.5)", "deep": None,
        "note": "Highway cargo tanks & automotive fuel tanks (wider than tall). Equivalent-rectangle.",
        "default": {"span": 2.0, "fill_ratio": 0.5, "span_label": "Width W (m), height = W/1.5"},
    },
]


def _lookup_table(kind, span, fill_ratio):
    """Real fundamental period across fills for the selected shape + size."""
    rows = []
    for F in (0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8):
        if kind == "rectangular":
            T = slosh.rectangular_tank_periods(span, F * span, n_modes=1)[0]
        elif kind == "upright-cylinder":
            T = slosh.cylindrical_tank_periods(span, F * span, n_modes=1)[0]
        elif kind == "horizontal-cylinder":
            T = slosh.horizontal_cylinder_periods(span, F, n_modes=1)[0]
        else:
            T = slosh.oval_tank_periods(span, span / 1.5, F, n_modes=1)[0]
        rows.append({"f": F, "T": round(T, 2)})
    return rows


# ---- resonance screen (marine ballast-tank case; unchanged physics) ----
_ROLL_T = 12.0
_TOL = 0.15
_RCYL_D, _TANK_H = 40.0, 26.0


def _resonance_series():
    pts = []
    for i in range(0, 49):
        h = round(2.0 + 0.5 * i, 1)
        t_cyl = slosh.cylindrical_tank_periods(_RCYL_D, h, n_modes=1)[0]
        res = slosh.resonance_check([t_cyl], [_ROLL_T], _TOL)
        pts.append({"h": h, "fillpct": round(100.0 * h / _TANK_H),
                    "cyl": round(t_cyl, 2), "resonant": bool(res)})
    return pts


REFERENCES = [
    "Abramson, NASA SP-106 (1966) — The Dynamic Behavior of Liquids in Moving Containers",
    "Ibrahim, Liquid Sloshing Dynamics (Cambridge, 2005)",
    "Faltinsen & Timokha, Sloshing (Cambridge, 2009)",
    "Dodge, New Dynamic Behavior of Liquids in Moving Containers (SwRI, 2000)",
    "McIver (1989), J. Fluid Mech. 201 — cylindrical & spherical containers, arbitrary depth",
    "Budiansky (1960) — sloshing in circular canals and spherical tanks",
    "Evans & McIver (1987), J. Fluid Mech. — resonant frequencies with a vertical baffle",
    "Housner (1963); API 650 Annex E; Malhotra-Wenk-Wieland (2000); Eurocode 8 Part 4 — seismic tanks",
    "Bosch & Vugts (1966) — roll damping by free-surface anti-roll tanks",
    "Ervin, Barnes & Wolfe, UMTRI-85-35 / FHWA (1985) — cargo-tank-truck stability & rollover",
    "Rajagounder et al. (2016), Engineering Journal 20(1) — automotive fuel-tank sloshing (VOF + baffles)",
    "Micheli et al. (2022), J. Applied Fluid Mechanics 15(2) — spray-tanker sloshing under braking",
]


def build_study():
    shapes = []
    for sh in SHAPES:
        shapes.append({**sh, "curve": _norm_curve(sh["key"]),
                       "lookup": _lookup_table(sh["key"], sh["default"]["span"], 0.5)})
    return {
        "meta": {"g": G, "roll_t": _ROLL_T, "tol": _TOL,
                 "band_lo": round(_ROLL_T * (1 - _TOL), 2),
                 "band_hi": round(_ROLL_T * (1 + _TOL), 2),
                 "cyl_d": _RCYL_D, "tank_h": _TANK_H},
        "shapes": shapes,
        "resonance": _resonance_series(),
        "references": REFERENCES,
    }


_HTML = r"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Tank sloshing — natural-period & resonance explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--roll:#7c3aed;
       --c0:#0B3D91;--c1:#0f8a7e;--c2:#b8860b;--c3:#c0392b}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:24px;color:var(--navy);letter-spacing:-.3px}
 h2{font-size:15px;color:var(--navy);margin:2px 0 10px;letter-spacing:-.2px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:840px}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .tag{display:inline-block;font-size:11px;font-weight:700;letter-spacing:.4px;text-transform:uppercase;
      color:var(--teal);margin-bottom:6px}
 svg{width:100%;height:auto;display:block}
 .legend{display:flex;flex-wrap:wrap;gap:14px;margin-top:10px;font-size:12.5px}
 .legend span{display:flex;align-items:center;gap:6px;color:var(--muted);cursor:pointer;user-select:none}
 .legend i{width:22px;height:3px;border-radius:2px;display:inline-block}
 .legend .off{opacity:.32}
 .ctrl{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:6px 0 12px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13px}
 select,input[type=number]{font:inherit;font-size:14px;padding:6px 9px;border:1px solid var(--line);
      border-radius:8px;background:#fff;color:var(--ink)}
 input[type=range]{flex:1;min-width:180px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:15px;min-width:96px}
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:8px 0}
 .read{font-size:12.5px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;font-weight:700;color:var(--navy)}
 table{border-collapse:collapse;width:100%;font-size:13px;margin-top:6px}
 th,td{text-align:right;padding:5px 9px;border-bottom:1px solid var(--line)}
 th:first-child,td:first-child{text-align:left}
 th{color:var(--muted);font-weight:600;font-size:11.5px;text-transform:uppercase;letter-spacing:.4px}
 td.mono{font-family:ui-monospace,Menlo,monospace;color:var(--navy);font-weight:700}
 .fnote{font-size:12px;color:var(--muted);margin-top:8px}
 .formula{font-family:ui-monospace,Menlo,monospace;font-size:12.5px;background:var(--soft);
      border:1px solid var(--line);border-radius:8px;padding:8px 11px;color:var(--navy);margin:8px 0}
 .scale{position:relative;height:44px;margin:20px 0 4px;border-radius:9px;overflow:hidden;
        background:var(--soft);border:1px solid var(--line)}
 .band{position:absolute;top:0;bottom:0;background:rgba(124,58,237,.15);border-left:2px solid var(--roll);border-right:2px solid var(--roll)}
 .tick{position:absolute;top:-4px;bottom:-4px;width:2px;background:var(--teal)}
 .tick span{position:absolute;top:-16px;left:50%;transform:translateX(-50%);font-size:9px;font-weight:700;color:var(--teal);white-space:nowrap}
 .scalelab{display:flex;justify-content:space-between;font-size:10px;color:var(--muted);padding:0 2px}
 .flag{display:inline-block;font-size:12px;font-weight:700;border-radius:20px;padding:4px 12px;margin-top:12px}
 .flag.ok{background:#e6f5ec;color:var(--ok)} .flag.bad{background:#fdecea;color:var(--bad)}
 ul.refs{list-style:none;font-size:12.5px;color:var(--muted);columns:2;column-gap:26px}
 ul.refs li{padding:3px 0;break-inside:avoid}
 .cfd{background:linear-gradient(135deg,#f4f8fc,#eaf3ff);border:1px solid var(--line);border-radius:12px;padding:14px 16px}
 .cfd b{color:var(--navy)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
 @media(max-width:640px){ul.refs{columns:1}}
</style></head><body>
<h1>Tank sloshing &mdash; natural-period &amp; resonance explorer</h1>
<p class="sub">The fundamental sloshing natural period of a partially filled tank, from validated analytical
theory across tank shapes. The <b>normalized master curve</b> collapses every size onto one dimensionless
family; pick a shape to read real periods; and see how partial fill sweeps a tank into resonance with vessel
roll &mdash; the coupling external diffraction (AQWA/OrcaWave) does not capture. Every value is a live
<code>hydrodynamics.sloshing</code> evaluation.</p>

<div class="panel">
  <span class="tag">Normalized master curve</span>
  <h2>Dimensionless sloshing frequency &Omega;&#8321; = &omega;&#8321;&radic;(L<sub>c</sub>/g) vs fill / slenderness ratio h/L<sub>c</sub></h2>
  <p class="sub" style="margin:0 0 8px">Any tank of a given shape &mdash; any size &mdash; lies on its curve. Read &Omega;&#8321; off the
  curve at your fill ratio, then the real period is <b>T = 2&pi;&radic;(L<sub>c</sub>/g) / &Omega;&#8321;</b>. Deep-liquid asymptotes:
  rectangular &radic;&pi;&nbsp;=&nbsp;1.77, upright cylinder &radic;1.841&nbsp;=&nbsp;1.36 (&times;&radic;2 on a diameter basis).</p>
  <svg id="chart" viewBox="0 0 720 380" role="img" aria-label="Normalized sloshing frequency curves"></svg>
  <div class="legend" id="legend"></div>
  <p class="fnote">Horizontal-cylinder and elliptical curves use the equivalent-rectangle method (good to ~10&ndash;15% at mid
  fill; near-full and violent cases are where a VOF free-surface CFD run takes over).</p>
</div>

<div class="panel">
  <span class="tag">Shape lookup</span>
  <h2>Real natural period for a specific tank</h2>
  <div class="ctrl">
    <label for="shape">Tank shape</label>
    <select id="shape"></select>
    <label for="span" id="spanlab">Size (m)</label>
    <input type="number" id="span" step="0.1" min="0.1" style="width:92px">
  </div>
  <div class="formula" id="formula"></div>
  <div class="reads">
    <div class="read">Fundamental period (½ full)<b id="rT">&mdash;</b></div>
    <div class="read">Characteristic length L<sub>c</sub><b id="rLc" style="font-size:14px">&mdash;</b></div>
    <div class="read">Aspect / fill ratio<b id="rRatio" style="font-size:14px">&mdash;</b></div>
  </div>
  <table><thead><tr><th>Fill fraction</th><th>Fundamental period T&#8321; (s)</th></tr></thead>
    <tbody id="lookrows"></tbody></table>
  <p class="fnote" id="shapenote"></p>
</div>

<div class="panel">
  <span class="tag">Resonance screen &mdash; marine coupling</span>
  <h2>Partial-fill sloshing vs vessel roll (the ballast-tank / anti-roll case)</h2>
  <div class="ctrl">
    <label for="h">Fill level</label>
    <input type="range" id="h" min="0" max="0" step="1">
    <span class="wval" id="hval"></span>
  </div>
  <div class="reads">
    <div class="read">Cylindrical T&#8321;<b id="cT">&mdash;</b></div>
    <div class="read">Vessel roll<b id="cV" style="color:var(--roll)">&mdash;</b></div>
  </div>
  <div class="scale"><div class="band" id="band"></div><div class="tick" id="ctick"><span>tank</span></div></div>
  <div class="scalelab"><span>4 s</span><span>8</span><span>12</span><span>16</span><span>20 s</span></div>
  <div id="cflag"></div>
  <div class="cfd" style="margin-top:14px">
    <b>Where CFD takes over.</b> Analytical periods and the class-simplified screen above close most of a scope.
    Violent / high-fill / impact cases &mdash; and the coupled roll response of partially filled ballast tanks used as
    tuned/anti-roll tanks &mdash; are resolved with VOF free-surface CFD (OpenFOAM), feeding a reduced response back into
    the vessel roll time-domain model. This is the tiering behind the ACMA / Noble ballast-tank sloshing study.
  </div>
</div>

<div class="panel">
  <span class="tag">Worldwide relationship basis</span>
  <h2>References</h2>
  <ul class="refs" id="refs"></ul>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_sloshing_explorer.py</code> from
<code>src/digitalmodel/hydrodynamics/sloshing.py</code>.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/hydrodynamics/sloshing.py">engine source &rarr;</a></p>

<script>
const DATA = __DATA__;
const G = DATA.meta.g, COL = ['var(--c0)','var(--c1)','var(--c2)','var(--c3)'];

/* ---------- master chart ---------- */
const XW=720,YH=380,ML=54,MR=16,MT=14,MB=42, XMAX=1.5, YMAX=2.0;
const px=x=>ML+(x/XMAX)*(XW-ML-MR), py=y=>YH-MB-(y/YMAX)*(YH-MT-MB);
const on = DATA.shapes.map(()=>true);
function drawChart(sel){
  let g='';
  // grid + axes
  for(let gx=0;gx<=1.5;gx+=0.25){g+=`<line x1="${px(gx)}" y1="${py(0)}" x2="${px(gx)}" y2="${py(YMAX)}" stroke="var(--line)"/>`+
    `<text x="${px(gx)}" y="${py(0)+16}" fill="var(--muted)" font-size="10" text-anchor="middle">${gx}</text>`;}
  for(let gy=0;gy<=2;gy+=0.5){g+=`<line x1="${px(0)}" y1="${py(gy)}" x2="${px(XMAX)}" y2="${py(gy)}" stroke="var(--line)"/>`+
    `<text x="${px(0)-8}" y="${py(gy)+3}" fill="var(--muted)" font-size="10" text-anchor="end">${gy.toFixed(1)}</text>`;}
  g+=`<text x="${(ML+XW-MR)/2}" y="${YH-6}" fill="var(--muted)" font-size="11" text-anchor="middle">fill / slenderness ratio  h / Lc</text>`;
  g+=`<text transform="translate(14,${(YH-MB+MT)/2}) rotate(-90)" fill="var(--muted)" font-size="11" text-anchor="middle">Ω₁ = ω₁√(Lc/g)</text>`;
  DATA.shapes.forEach((sh,i)=>{
    if(!on[i]) return;
    const d = sh.curve.map((p,j)=>`${j?'L':'M'}${px(p[0]).toFixed(1)} ${py(p[1]).toFixed(1)}`).join(' ');
    const w = (i===sel)?3.2:1.8, o=(sel<0||i===sel)?1:0.5;
    g+=`<path d="${d}" fill="none" stroke="${COL[i]}" stroke-width="${w}" opacity="${o}"/>`;
  });
  document.getElementById('chart').innerHTML=g;
}
function drawLegend(sel){
  document.getElementById('legend').innerHTML = DATA.shapes.map((sh,i)=>
    `<span data-i="${i}" class="${on[i]?'':'off'}"><i style="background:${COL[i]}"></i>${sh.label}</span>`).join('');
  document.querySelectorAll('#legend span').forEach(el=>el.onclick=()=>{
    const i=+el.dataset.i; on[i]=!on[i]; drawLegend(sel); drawChart(sel);});
}

/* ---------- shape lookup ---------- */
const sel = document.getElementById('shape');
DATA.shapes.forEach((sh,i)=>{const o=document.createElement('option');o.value=i;o.textContent=sh.label;sel.appendChild(o);});
const spanEl=document.getElementById('span');
function renderShape(){
  const i=+sel.value, sh=DATA.shapes[i];
  document.getElementById('spanlab').textContent=sh.default.span_label;
  if(document.activeElement!==spanEl) spanEl.value=sh.default.span;
  const scale=Math.sqrt((+spanEl.value||sh.default.span)/sh.default.span);
  document.getElementById('formula').textContent=sh.formula;
  document.getElementById('rLc').textContent=sh.lc;
  document.getElementById('rRatio').textContent=sh.ratio;
  const half=sh.lookup.find(r=>r.f===0.5);
  document.getElementById('rT').textContent=(half.T*scale).toFixed(2)+' s';
  document.getElementById('lookrows').innerHTML=sh.lookup.map(r=>
    `<tr><td>${Math.round(r.f*100)}%</td><td class="mono">${(r.T*scale).toFixed(2)}</td></tr>`).join('');
  document.getElementById('shapenote').textContent=sh.note;
  drawChart(i); drawLegend(i);
}
sel.onchange=renderShape; spanEl.oninput=renderShape;

/* ---------- resonance screen ---------- */
const M=DATA.meta, S=DATA.resonance, LO=4,HI=20, pos=v=>Math.max(0,Math.min((v-LO)/(HI-LO)*100,100));
const band=document.getElementById('band');
band.style.left=pos(M.band_lo)+'%'; band.style.width=(pos(M.band_hi)-pos(M.band_lo))+'%';
document.getElementById('cV').textContent=M.roll_t.toFixed(1)+' s';
const hs=document.getElementById('h'); hs.max=S.length-1;
function renderRes(){
  const p=S[+hs.value];
  document.getElementById('hval').textContent=p.h.toFixed(1)+' m ('+p.fillpct+'%)';
  document.getElementById('cT').textContent=p.cyl.toFixed(2)+' s';
  document.getElementById('ctick').style.left=pos(p.cyl)+'%';
  const f=document.getElementById('cflag');
  if(p.resonant){f.className='flag bad';f.textContent='⚠ Resonance risk — tank period within the roll band → CFD check warranted';}
  else{f.className='flag ok';f.textContent='✓ No resonance — tank period clear of the roll band';}
}
let st=S.findIndex(p=>p.fillpct>=95); hs.value=st<0?S.length-1:st; renderRes();
hs.addEventListener('input',renderRes);

document.getElementById('refs').innerHTML=DATA.references.map(r=>`<li>${r}</li>`).join('');
renderShape();
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "sloshing-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("__DATA__", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "sloshing-explorer.html").write_text(html, encoding="utf-8")
    print(f"wrote {(_OUT_DIR / 'sloshing-explorer.html').relative_to(_REPO)} "
          f"({len(study['shapes'])} shapes, {len(study['resonance'])} resonance fills)")


if __name__ == "__main__":
    main()
