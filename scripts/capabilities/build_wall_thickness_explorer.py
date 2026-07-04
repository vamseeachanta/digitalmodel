#!/usr/bin/env python
"""Build the wall-thickness multi-code explorer.

Runs the ``WallThicknessAnalyzer`` strategy engine over a reference submarine
pipeline across every registered design code and a wall-thickness sweep, then
writes a self-contained interactive HTML dashboard (data embedded as JSON, no
external assets) plus the raw study JSON.

Outputs:
    docs/api/structural/wall-thickness-explorer.html
    docs/api/structural/wall-thickness-explorer.json

Run:
    python scripts/capabilities/build_wall_thickness_explorer.py
"""
from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

# --- reference case: 12.75" X65 deepwater flowline --------------------------
_OD = 0.3239          # m  (12.75 in)
_GRADE = "X65"
_SMYS = 448.0e6       # Pa
_SMTS = 530.9e6       # Pa
_P_INT = 20.0e6       # Pa  internal (design)
_P_EXT = 10.0e6       # Pa  external (~1000 m water)
_SC = SafetyClass.MEDIUM
_FAB_TOL = 0.125      # fractional (DNV Table 5-5 seamless)

# wall-thickness sweep (mm)
_WALL_MM = [round(8.0 + 0.5 * i, 1) for i in range(0, 41)]   # 8.0 .. 28.0 mm


def _run(wall_m: float, code: DesignCode) -> dict:
    geom = PipeGeometry(
        outer_diameter=_OD, wall_thickness=wall_m,
        corrosion_allowance=0.0, fabrication_tolerance=_FAB_TOL,
    )
    mat = PipeMaterial(
        grade=_GRADE, smys=_SMYS, smts=_SMTS,
        youngs_modulus=207e9, poissons_ratio=0.3,
    )
    loads = DesignLoads(
        internal_pressure=_P_INT, external_pressure=_P_EXT,
        bending_moment=0.0, effective_tension=0.0,
    )
    factors = DesignFactors(safety_class=_SC)
    res = WallThicknessAnalyzer(geom, mat, loads, factors, code=code).perform_analysis()
    checks = {k: round(float(v), 4) for k, v in res.checks.items()}
    return {
        "max_util": round(float(res.max_utilisation), 4),
        "governing": res.governing_check,
        "is_safe": bool(res.is_safe),
        "checks": checks,
        "code_reference": res.code_reference,
    }


def build_study() -> dict:
    codes = list(DesignCode)
    series = {}
    code_ref = {}
    min_wall_pass = {}
    for code in codes:
        pts = []
        passed_at = None
        for wmm in _WALL_MM:
            r = _run(wmm / 1000.0, code)
            code_ref[code.value] = r["code_reference"]
            pts.append({"w": wmm, "u": r["max_util"], "g": r["governing"],
                        "safe": r["is_safe"], "checks": r["checks"]})
            if passed_at is None and r["is_safe"]:
                passed_at = wmm
        series[code.value] = pts
        min_wall_pass[code.value] = passed_at
    return {
        "meta": {
            "od_mm": round(_OD * 1000, 1), "grade": _GRADE,
            "smys_mpa": _SMYS / 1e6, "smts_mpa": _SMTS / 1e6,
            "p_int_mpa": _P_INT / 1e6, "p_ext_mpa": _P_EXT / 1e6,
            "safety_class": _SC.value, "fab_tol": _FAB_TOL,
            "wall_mm": _WALL_MM,
        },
        "code_reference": code_ref,
        "min_wall_pass": min_wall_pass,
        "series": series,
    }


# --- HTML template (self-contained; data embedded) --------------------------
_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Wall-thickness multi-code explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b;--warn:#b7791f}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:23px;color:var(--navy);letter-spacing:-.3px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:760px}
 .spec{display:flex;flex-wrap:wrap;gap:8px;margin-bottom:20px}
 .spec span{font-size:12px;font-weight:600;color:var(--muted);background:#fff;
            border:1px solid var(--line);border-radius:20px;padding:4px 11px}
 .spec b{color:var(--navy)}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .ctrl{display:flex;align-items:center;gap:14px;flex-wrap:wrap}
 .ctrl label{font-weight:700;color:var(--navy);font-size:14px}
 .ctrl input[type=range]{flex:1;min-width:240px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);
       font-size:16px;min-width:78px}
 .bars{margin-top:6px}
 .row{display:grid;grid-template-columns:150px 1fr 78px;align-items:center;gap:10px;margin:7px 0}
 .row .name{font-size:12.5px;font-weight:700;color:var(--ink);text-align:right}
 .track{position:relative;background:var(--soft);border:1px solid var(--line);
        border-radius:7px;height:24px;overflow:hidden}
 .fill{position:absolute;top:0;left:0;height:100%;border-radius:6px 0 0 6px;transition:width .12s}
 .limit{position:absolute;top:-3px;bottom:-3px;width:2px;background:var(--navy);opacity:.55}
 .glab{position:absolute;left:8px;top:50%;transform:translateY(-50%);font-size:11px;
       font-weight:600;color:#1b2a44;white-space:nowrap;text-shadow:0 1px 2px rgba(255,255,255,.7)}
 .uval{font-family:ui-monospace,Menlo,monospace;font-size:13px;font-weight:700;text-align:right}
 .ok{color:var(--ok)} .bad{color:var(--bad)}
 table{width:100%;border-collapse:collapse;font-size:13px;margin-top:4px}
 th,td{padding:7px 10px;text-align:left;border-bottom:1px solid var(--line)}
 th{background:var(--soft);color:var(--muted);font-size:11px;text-transform:uppercase;letter-spacing:.4px}
 td.n{font-family:ui-monospace,Menlo,monospace;text-align:right}
 .foot{color:var(--muted);font-size:12.5px;margin-top:8px}
 .foot a{color:var(--teal)}
 .legend{font-size:12px;color:var(--muted);margin-top:10px}
 .chip{display:inline-block;width:11px;height:11px;border-radius:3px;vertical-align:-1px;margin:0 3px 0 10px}
</style></head><body>
<h1>Wall-thickness &mdash; multi-code utilization explorer</h1>
<p class="sub">One geometry / material / loads spec run through the digitalmodel strategy engine across
every registered design code. Drag the wall thickness to watch each code's governing utilization move;
a bar reaching the navy line (utilization&nbsp;=&nbsp;1.0) is at its code limit. Computed live by
<code>WallThicknessAnalyzer</code> &mdash; values embedded from a real engine run, not mocked.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label for="w">Wall thickness</label>
    <input type="range" id="w" min="0" max="0" step="1">
    <span class="wval" id="wval"></span>
  </div>
  <div class="legend">Governing utilization per code &mdash;
    <span class="chip" style="background:var(--ok)"></span>safe (&lt;1)
    <span class="chip" style="background:var(--bad)"></span>over limit (&ge;1)
    &nbsp;|&nbsp; navy line = utilization 1.0</div>
  <div class="bars" id="bars"></div>
</div>

<div class="panel">
  <h3 style="font-size:15px;color:var(--navy);margin-bottom:6px">Per-mode breakdown &amp; minimum passing wall</h3>
  <table><thead><tr><th>Design code</th><th>Governing mode</th><th>Max util</th>
    <th>Min wall to pass</th><th>Basis</th></tr></thead><tbody id="tbody"></tbody></table>
  <p class="foot">Reference: <span id="ref"></span></p>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_wall_thickness_explorer.py</code> from
<code>src/digitalmodel/structural/analysis/wall_thickness_codes/</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/structural/analysis/wall_thickness_codes">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, W = M.wall_mm, CODES = Object.keys(DATA.series);
const spec = document.getElementById('spec');
spec.innerHTML = [
  ['OD', M.od_mm.toFixed(1)+' mm'], ['Grade', M.grade],
  ['SMYS', M.smys_mpa+' MPa'], ['p<sub>int</sub>', M.p_int_mpa+' MPa'],
  ['p<sub>ext</sub>', M.p_ext_mpa+' MPa'], ['Safety class', M.safety_class],
].map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
document.getElementById('ref').textContent =
  CODES.map(c=>DATA.code_reference[c]).filter((v,i,a)=>a.indexOf(v)===i).join('  ·  ');

const slider = document.getElementById('w');
slider.max = W.length-1;
const clamp = u => Math.min(u, 1.6);
function render(idx){
  const wmm = W[idx];
  document.getElementById('wval').textContent = wmm.toFixed(1)+' mm';
  document.getElementById('bars').innerHTML = CODES.map(c=>{
    const p = DATA.series[c][idx];
    const pct = (clamp(p.u)/1.6*100).toFixed(1);
    const col = p.safe ? 'var(--ok)' : 'var(--bad)';
    const limitPct = (1.0/1.6*100).toFixed(1);
    return `<div class="row"><div class="name">${c}</div>
      <div class="track"><div class="fill" style="width:${pct}%;background:${col}"></div>
      <div class="limit" style="left:${limitPct}%"></div>
      <div class="glab">${p.g}</div></div>
      <div class="uval ${p.safe?'ok':'bad'}">${p.u.toFixed(3)}</div></div>`;
  }).join('');
  document.getElementById('tbody').innerHTML = CODES.map(c=>{
    const p = DATA.series[c][idx];
    const mw = DATA.min_wall_pass[c];
    return `<tr><td>${c}</td><td>${p.g}</td>
      <td class="n ${p.safe?'ok':'bad'}">${p.u.toFixed(3)}</td>
      <td class="n">${mw==null?'&mdash;':mw.toFixed(1)+' mm'}</td>
      <td>${DATA.code_reference[c]}</td></tr>`;
  }).join('');
}
// start near where propagation buckling governs (~19 mm) if present
let start = W.indexOf(19.0); if(start<0) start = Math.floor(W.length/2);
slider.value = start; render(start);
slider.addEventListener('input', e=>render(+e.target.value));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "wall-thickness-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "wall-thickness-explorer.html").write_text(html, encoding="utf-8")
    n = sum(1 for c in study["series"] for _ in study["series"][c])
    print(f"wrote {(_OUT_DIR / 'wall-thickness-explorer.html').relative_to(_REPO)} "
          f"({len(study['series'])} codes, {n} points)")


if __name__ == "__main__":
    main()
