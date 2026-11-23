#!/usr/bin/env python3
"""Upgrade the 4 static CFD verification reports to interactive capability
pages (#1276): replace static chart PNGs with Plotly, add the VIV vortex-street
animation, insert a realistic use-case panel per report. Narrative text,
tables, and verified numbers are preserved from the existing reports.

Usage: upgrade_reports.py <scratchpad> <worktree_root>
Expects solved runs at <scratchpad>/{fp,tfp,naca,cyl} with analyzers run.
"""
import json
import math
import re
import sys
from pathlib import Path

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, str(Path(__file__).parent))
from house import PLOTLY_CDN, _STYLE_EXTRA, plot_div, flipbook, fig_to_jpeg_b64

SP = Path(sys.argv[1])
WT = Path(sys.argv[2])
CFD = WT / "docs/api/cfd"
BLUE, RED, GREEN, GOLD, INK = "#1f6feb", "#d1242f", "#1a7f37", "#9a6700", "#1a2332"
r4 = lambda xs: [round(float(v), 5) for v in xs]


def upgrade(name: str, img_repl: dict, usecase: str, extra_after: dict | None = None,
            drop_texts: list | None = None):
    """Rewrite report `name`: img index -> replacement html; use-case panel
    goes before the 2nd <h2>; plotly+styles into <head>."""
    p = CFD / f"{name}-verification.html"
    s = p.read_text()
    if "plot.ly" in s:
        raise RuntimeError(f"{p.name} already upgraded — reset from origin/main first")
    for frag in (drop_texts or []):
        s = re.sub(r"<p class=\"sub\"[^>]*>[^<]*" + re.escape(frag) + r"[^<]*</p>", "", s)
    s = s.replace("</head>", _STYLE_EXTRA + "\n" + PLOTLY_CDN + "\n</head>", 1)
    # mark interactive in the kicker
    s = re.sub(r'(<div class="kicker">[^<]*)</div>', r"\1 &middot; interactive</div>", s, count=1)
    # replace figures by position
    parts = re.split(r"(<img [^>]+>)", s)
    idx = 0
    for i, part in enumerate(parts):
        if part.startswith("<img"):
            if idx in img_repl:
                parts[i] = img_repl[idx]
            idx += 1
    s = "".join(parts)
    # insert the use-case panel before the second <h2>
    h2s = [m.start() for m in re.finditer(r"<h2[ >]", s)]
    s = s[: h2s[1]] + usecase + "\n" + s[h2s[1]:]
    if extra_after:  # {anchor_text: html_to_insert_after}
        for anchor, html in extra_after.items():
            s = s.replace(anchor, anchor + "\n" + html, 1)
    p.write_text(s)
    print(f"{p.name}: {p.stat().st_size / 1024:.0f} KB")


# ============================================================ flat plate ====
fp = json.loads((SP / "fp/validation_results/results.json").read_text())
rows = [r for r in fp["cf"] if r["re_x"] >= 300]
cf_chart = plot_div("fp-cf", [
    {"x": r4([r["re_x"] for r in rows]), "y": r4([r["cf_blasius"] for r in rows]),
     "mode": "lines", "name": "Blasius 0.664/√Re_x", "line": {"color": INK, "width": 2},
     "hovertemplate": "Re_x=%{x:.0f}<br>Cf=%{y:.4f}<extra>Blasius</extra>"},
    {"x": r4([r["re_x"] for r in rows]), "y": r4([r["cf_cfd"] for r in rows]),
     "mode": "markers", "name": "OpenFOAM simpleFoam (laminar)",
     "marker": {"color": RED, "size": 4},
     "customdata": r4([r["err_pct"] for r in rows]),
     "hovertemplate": "Re_x=%{x:.0f}<br>Cf=%{y:.4f}<br>err=%{customdata:.1f}%<extra>CFD</extra>"},
], {
    "title": {"text": "Local skin friction vs the Blasius solution"},
    "xaxis": {"title": {"text": "Re_x"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "C_f"}, "range": [0, 0.05], "gridcolor": "#eef1f5"},
    "legend": {"x": 0.55, "y": 0.95},
})
# Blasius f'(eta) from the densest profile comparison: rebuild via shooting
from scipy.integrate import solve_ivp
sol = solve_ivp(lambda e, y: [y[1], y[2], -0.5 * y[0] * y[2]], [0, 10],
                [0, 0, 0.332057], max_step=0.01, dense_output=True)
eta_b = np.linspace(0, 6, 200)
fp_b = np.clip(sol.sol(eta_b)[1], 0, 1)
traces = [{"x": r4(eta_b), "y": r4(fp_b), "mode": "lines",
           "name": "Blasius f'(η)", "line": {"color": INK, "width": 2.5},
           "hovertemplate": "η=%{x:.2f}<br>u/U=%{y:.3f}<extra>Blasius</extra>"}]
colors = [BLUE, GREEN, GOLD, RED]
for c, prof in zip(colors, fp["profiles"]):
    eta = np.array(prof["eta"]); uU = np.array(prof["u_over_U"])
    m = (eta <= 6) & (eta > 0)
    traces.append({"x": r4(eta[m][::4]), "y": r4(uU[m][::4]), "mode": "markers",
                   "name": f"x = {prof['x']*1000:.0f} mm (Re_x = {prof['re_x']:.0f})",
                   "marker": {"color": c, "size": 5, "symbol": "circle-open",
                              "line": {"width": 1.5, "color": c}},
                   "hovertemplate": "η=%{x:.2f}<br>u/U=%{y:.3f}<extra>x=" + f"{prof['x']*1000:.0f}mm" + "</extra>"})
prof_chart = plot_div("fp-prof", traces, {
    "title": {"text": "Velocity profiles collapse onto the Blasius similarity solution"},
    "xaxis": {"title": {"text": "η = y·√(U/νx)"}, "range": [0, 6], "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "u / U∞"}, "range": [0, 1.1], "gridcolor": "#eef1f5"},
    "legend": {"x": 0.62, "y": 0.35},
})
fp_usecase = """
<div class="usecase"><h3>What this gates in real work</h3>
<p>The laminar flat plate is the <b>skin-friction foundation</b>: every hull, pontoon and appendage drag
estimate decomposes into friction + pressure components, and the friction part is a boundary-layer
computation exactly like this one. Verifying Cf(x) to ~5% against an exact solution qualifies:</p>
<ul>
<li><b>Model-scale friction lines</b> — towing-tank extrapolation (ITTC-1957) assumes the CFD can
reproduce a flat-plate boundary layer before any hull-form work is trusted.</li>
<li><b>Laminar-regime components</b> — small appendages, model-scale foils and instrument struts at
low Re where the laminar solution applies directly.</li>
<li><b>Mesh/BC discipline</b> — the case demonstrated that leading-edge treatment (not mesh count)
controls the error, a transferable lesson for any wall-bounded mesh in the suite.</li>
</ul></div>"""

upgrade("flat-plate-blasius", {0: cf_chart, 1: prof_chart}, fp_usecase)

# ============================================================ turbulent =====
tf = json.loads((SP / "tfp/tflat_results/results.json").read_text())
yp = np.array(tf["profile_yplus"]); up = np.array(tf["profile_uplus"])
m = yp > 0.05
sub = np.geomspace(0.05, 12, 60)
logx = np.geomspace(8, 300, 60)
law_chart = plot_div("tf-law", [
    {"x": r4(sub), "y": r4(sub), "mode": "lines", "name": "sublayer u⁺ = y⁺",
     "line": {"color": GREEN, "width": 2, "dash": "dot"},
     "hovertemplate": "y⁺=%{x:.2f}<br>u⁺=%{y:.2f}<extra>sublayer</extra>"},
    {"x": r4(logx), "y": r4((1 / 0.41) * np.log(logx) + 5.0), "mode": "lines",
     "name": "log law (κ=0.41, B=5.0)", "line": {"color": INK, "width": 2, "dash": "dash"},
     "hovertemplate": "y⁺=%{x:.1f}<br>u⁺=%{y:.2f}<extra>log law</extra>"},
    {"x": r4(yp[m]), "y": r4(up[m]), "mode": "markers",
     "name": "OpenFOAM k-ω SST (u_τ from CFD wall shear)",
     "marker": {"color": RED, "size": 5, "symbol": "circle-open", "line": {"width": 1.5, "color": RED}},
     "hovertemplate": "y⁺=%{x:.2f}<br>u⁺=%{y:.2f}<extra>CFD</extra>"},
], {
    "title": {"text": f"Law of the wall at Re_x = {tf['profile_re_x']:.2g}"},
    "xaxis": {"title": {"text": "y⁺"}, "type": "log", "range": [-1.05, 2.6], "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "u⁺"}, "range": [0, 25], "gridcolor": "#eef1f5"},
    "legend": {"x": 0.02, "y": 0.98},
})
rows = [r for r in tf["cf"] if r["re_x"] > 2e4]
tf_cf_chart = plot_div("tf-cf", [
    {"x": r4([r["re_x"] for r in rows]), "y": r4([r["cf_schlichting"] for r in rows]),
     "mode": "lines", "name": "Schlichting (turbulent)", "line": {"color": INK, "width": 2},
     "hovertemplate": "Re_x=%{x:.2e}<br>Cf=%{y:.4f}<extra>Schlichting</extra>"},
    {"x": r4([r["re_x"] for r in rows]), "y": r4([r["cf_prandtl"] for r in rows]),
     "mode": "lines", "name": "Prandtl 0.0592·Re_x^(-1/5)",
     "line": {"color": BLUE, "width": 1.6, "dash": "dash"},
     "hovertemplate": "Re_x=%{x:.2e}<br>Cf=%{y:.4f}<extra>Prandtl</extra>"},
    {"x": r4([r["re_x"] for r in rows]), "y": r4([r["cf_cfd"] for r in rows]),
     "mode": "markers", "name": "OpenFOAM k-ω SST",
     "marker": {"color": RED, "size": 3.5},
     "customdata": r4([r["err_schlichting_pct"] for r in rows]),
     "hovertemplate": "Re_x=%{x:.2e}<br>Cf=%{y:.4f}<br>vs Schlichting: %{customdata:.1f}%<extra>CFD</extra>"},
], {
    "title": {"text": "Turbulent skin friction vs empirical correlations"},
    "xaxis": {"title": {"text": "Re_x"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "C_f"}, "range": [0, 0.008], "gridcolor": "#eef1f5"},
    "legend": {"x": 0.55, "y": 0.95},
    "shapes": [{"type": "rect", "x0": 5e5, "x1": 1e6, "y0": 0, "y1": 0.008,
                "fillcolor": "rgba(26,122,63,0.07)", "line": {"width": 0}}],
})
tf_usecase = """
<div class="usecase"><h3>What this gates in real work</h3>
<p>Real marine flows are turbulent — this case is the <b>full-scale friction gate</b>. A wall-resolved
k-&omega; SST boundary layer that lands on the universal law of the wall qualifies:</p>
<ul>
<li><b>Ship &amp; FPSO hull friction</b> — full-scale Cf prediction and the ITTC/Schoenherr line
comparisons behind every powering estimate.</li>
<li><b>Appendage and rudder RANS</b> — the same turbulence model, wall treatment (y⁺ &asymp; 0.1–0.3,
no wall functions) and near-wall meshing used for lifting surfaces (see the NACA0012 case).</li>
<li><b>Current loads on hulls and jackets</b> — drag coefficients used in mooring and DP capability
studies inherit their credibility from exactly this near-wall physics.</li>
</ul></div>"""

upgrade("turbulent-flat-plate", {0: law_chart, 1: tf_cf_chart}, tf_usecase)

# ============================================================ NACA ==========
na = json.loads((SP / "naca/naca_results/results.json").read_text())
pol = np.loadtxt(SP / "naca/polar.csv", delimiter=",", skiprows=1)
al, cl, cd = pol[:, 0], pol[:, 1], pol[:, 2]
slope = na["slope_per_deg"]; a0 = na["built_in_incidence_deg"]
afit = np.array([-2.0, 8.5])
lift_chart = plot_div("na-lift", [
    {"x": r4(afit), "y": r4(slope * (afit - a0)), "mode": "lines",
     "name": f"fit: {slope:.4f}/deg (α₀ = {a0:.2f}°)",
     "line": {"color": BLUE, "width": 2, "dash": "dash"},
     "hovertemplate": "α=%{x:.1f}°<br>Cl=%{y:.3f}<extra>fit</extra>"},
    {"x": r4(afit), "y": r4(0.106 * (afit - a0)), "mode": "lines",
     "name": "experiment slope 0.106/deg", "line": {"color": GREEN, "width": 1.6, "dash": "dot"},
     "hovertemplate": "α=%{x:.1f}°<br>Cl=%{y:.3f}<extra>experiment</extra>"},
    {"x": r4(afit), "y": r4((2 * math.pi * math.pi / 180) * (afit - a0)), "mode": "lines",
     "name": "thin-airfoil 2π/rad (0.1097/deg)", "line": {"color": GOLD, "width": 1.6, "dash": "dot"},
     "hovertemplate": "α=%{x:.1f}°<br>Cl=%{y:.3f}<extra>2π</extra>"},
    {"x": r4(al), "y": r4(cl), "mode": "markers", "name": "OpenFOAM (SA RANS)",
     "marker": {"color": RED, "size": 8, "symbol": "circle-open", "line": {"width": 2, "color": RED}},
     "hovertemplate": "α=%{x:.0f}°<br>Cl=%{y:.3f}<extra>CFD</extra>"},
], {
    "title": {"text": "NACA0012 lift curve — slope vs experiment and thin-airfoil theory"},
    "xaxis": {"title": {"text": "freestream angle of attack α (deg)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "C_l (wind axes)"}, "gridcolor": "#eef1f5"},
    "legend": {"x": 0.02, "y": 0.98},
})
polar_chart = plot_div("na-polar", [
    {"x": r4(cd), "y": r4(cl), "mode": "lines+markers", "name": "OpenFOAM polar",
     "marker": {"color": RED, "size": 7}, "line": {"color": RED, "width": 1.5},
     "text": [f"α={a:.0f}°" for a in al],
     "hovertemplate": "%{text}<br>Cd=%{x:.4f}<br>Cl=%{y:.3f}<extra></extra>"},
], {
    "title": {"text": "Drag polar (steady RANS, linear range)"},
    "xaxis": {"title": {"text": "C_d"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "C_l"}, "gridcolor": "#eef1f5"},
    "showlegend": False,
}, height=380)
na_usecase = """
<div class="usecase"><h3>What this gates in real work</h3>
<p>Lift generation is the physics of <b>every control and lifting surface offshore</b>. A verified
lift-curve slope qualifies:</p>
<ul>
<li><b>Rudders and stabilizer fins</b> — the rudder-manoeuvring capability's lift coefficients
(Clarke/OCIMF screens) assume section data of exactly this kind; RANS closes the loop for
non-catalogue sections (see the rudder explorer under /hydro/).</li>
<li><b>Hydrofoils, thruster nozzles and propeller blade sections</b> — section polars feed BEM and
lifting-line design tools.</li>
<li><b>Wind turbine blades</b> — the floating-wind sizing workflows consume airfoil polars; the same
solver + wall treatment produces them for arbitrary sections.</li>
</ul></div>"""

upgrade("naca0012-airfoil", {1: lift_chart, 2: polar_chart}, na_usecase)

# ============================================================ cylinder ======
# forceCoeffs history (full run)
cfp = sorted((SP / "cyl").glob("postProcessing/forceCoeffs/*/coefficient*.dat"))
rows = np.loadtxt(cfp[0], comments="#")
header = [l for l in cfp[0].read_text().splitlines() if l.startswith("#")][-1].lstrip("#").split()
ci = {n.lower(): i for i, n in enumerate(header)}
t, cdh, clh = rows[:, 0], rows[:, ci["cd"]], rows[:, ci["cl"]]
dec = max(1, len(t) // 2400)
cyl = json.loads((SP / "cyl/cyl_results/results.json").read_text())
force_chart = plot_div("cy-force", [
    {"x": r4(t[::dec]), "y": r4(cdh[::dec]), "mode": "lines", "name": "Cd (drag)",
     "line": {"color": BLUE, "width": 1.5},
     "hovertemplate": "t=%{x:.1f}s<br>Cd=%{y:.3f}<extra></extra>"},
    {"x": r4(t[::dec]), "y": r4(clh[::dec]), "mode": "lines", "name": "Cl (lift)",
     "line": {"color": RED, "width": 1.2},
     "hovertemplate": "t=%{x:.1f}s<br>Cl=%{y:.3f}<extra></extra>"},
], {
    "title": {"text": f"Force-coefficient history — onset, growth and saturation of shedding "
                      f"(mean Cd = {cyl['mean_cd']:.3f}, St = {cyl['strouhal']:.4f})"},
    "xaxis": {"title": {"text": "t (s)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "coefficient"}, "gridcolor": "#eef1f5"},
    "legend": {"x": 0.02, "y": 0.98},
})

# vorticity animation from the dense stage-2 writes
import pyvista as pv
(SP / "cyl/case.foam").write_text("")
reader = pv.OpenFOAMReader(str(SP / "cyl/case.foam"))
times = [tv for tv in reader.time_values if tv >= 150.0]
frames, labels = [], []
for tv in times:
    reader.set_active_time_value(tv)
    mesh = reader.read()["internalMesh"]
    mesh = mesh.compute_derivative(scalars="U", vorticity=True)
    warr = np.asarray(mesh["vorticity"])[:, 2]
    if len(warr) == mesh.n_points:
        pts, w = mesh.points, warr
    else:
        pts, w = mesh.cell_centers().points, warr
    mm = (pts[:, 0] > -3) & (pts[:, 0] < 15) & (np.abs(pts[:, 1]) < 4.5)
    fig, ax = plt.subplots(figsize=(8.6, 4.2))
    ax.tricontourf(pts[mm, 0], pts[mm, 1], np.clip(w[mm], -2, 2), levels=40, cmap="RdBu_r")
    circ = plt.Circle((0, 0), 0.5, color="#222", zorder=5)
    ax.add_patch(circ)
    ax.set_xlim(-3, 15); ax.set_ylim(-4.5, 4.5); ax.set_aspect("equal")
    ax.set_xticks([]); ax.set_yticks([])
    ax.set_title(f"vorticity ω_z   t = {tv:.2f} s", fontsize=11)
    fig.tight_layout(pad=0.4)
    frames.append(fig_to_jpeg_b64(fig)); plt.close(fig)
    labels.append(f"t = {tv:.2f} s")
anim = flipbook("viv-anim", frames, labels, fps=8,
                caption="Kármán vortex street — vorticity ω_z over two shedding periods "
                        "(saturated limit cycle, t = 150–162 s, frames every 0.25 s). "
                        "This alternating shedding is the forcing mechanism of VIV.")

cy_usecase = """
<div class="usecase"><h3>What this gates in real work — VIV screening</h3>
<p>Alternating vortex shedding is the forcing behind <b>vortex-induced vibration</b> of risers,
jacket members, mooring lines and pipeline free spans. This case verifies the two numbers VIV
screening depends on: the shedding frequency (Strouhal, +0.9%) and the force amplitude
(Cd &minus;0.5%, Ĉl in the published band). Try it on a real member:</p>
<div class="calc">
  <div><label for="viv-d">Member diameter D (m)</label><input id="viv-d" type="number" value="0.50" step="0.05" min="0.01"></div>
  <div><label for="viv-u">Current speed U (m/s)</label><input id="viv-u" type="number" value="1.0" step="0.1" min="0.01"></div>
  <div><label for="viv-fn">Structure natural freq f&#8345; (Hz)</label><input id="viv-fn" type="number" value="0.35" step="0.01" min="0.001"></div>
  <div class="out"><b>Shedding freq f&#8347; = St·U/D</b><span id="viv-fs">—</span></div>
  <div class="out"><b>Reduced velocity U&#8345; = U/(f&#8345;D)</b><span id="viv-ur">—</span></div>
  <div class="out"><b>Lock-in screen (4 &le; U&#8345; &le; 8)</b><span id="viv-lock">—</span></div>
</div>
<p class="sub">Uses the sub-critical Strouhal St &asymp; 0.2 (10³ &lt; Re &lt; 2&times;10⁵), the flat part of the
St–Re curve; this verification case sits at Re = 100 where St = 0.164–0.166, the laminar benchmark point.
Lock-in band per DNV-RP-C205 / DNV-RP-F105 screening practice; a hit means fatigue assessment
(e.g. the free-span workflow) is required, not that failure occurs.</p>
<script>
(function(){
  const d=document.getElementById("viv-d"),u=document.getElementById("viv-u"),
        fn=document.getElementById("viv-fn"),fs=document.getElementById("viv-fs"),
        ur=document.getElementById("viv-ur"),lk=document.getElementById("viv-lock");
  function upd(){
    const D=+d.value,U=+u.value,FN=+fn.value;
    if(!(D>0&&U>0&&FN>0)){fs.textContent=ur.textContent=lk.textContent="—";return;}
    const f=0.2*U/D, uR=U/(FN*D);
    fs.textContent=f.toFixed(3)+" Hz";
    ur.textContent=uR.toFixed(2);
    const hit=uR>=4&&uR<=8;
    lk.textContent=hit?"IN lock-in band — assess VIV":"outside band";
    lk.style.color=hit?"#b02a2a":"#1a7a3f";
  }
  [d,u,fn].forEach(el=>el.addEventListener("input",upd)); upd();
})();
</script>
</div>"""

upgrade("cylinder-re100", {0: anim, 1: force_chart}, cy_usecase,
        drop_texts=["Instantaneous spanwise vorticity at t=150"])
print("all reports upgraded")
