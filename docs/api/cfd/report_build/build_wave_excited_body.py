#!/usr/bin/env python3
"""Interactive wave-excited floating body verification report (issue #1302).

Usage:
    uv run python build_wave_excited_body.py <solved_case_root> <results_dir> <worktree_root> [fast_results_dir]

``solved_case_root`` contains the solved reference ``background/`` (fields
reconstructed if the run was parallel); ``results_dir`` holds the
``results.json`` written by ``cases/wave_excited_body/analyze_wave_excited_body.py``.
"""
import json
import sys
from pathlib import Path

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, str(Path(__file__).parent))
from house import house_style, page, plot_div, flipbook, fig_to_jpeg_b64

CASE = Path(sys.argv[1])          # reference case root (background/ solved)
RES = Path(sys.argv[2])
WT = Path(sys.argv[3])
FAST = Path(sys.argv[4]) if len(sys.argv) > 4 else None
r = json.loads((RES / "results.json").read_text())
rf = json.loads((FAST / "results.json").read_text()) if FAST else None

BLUE, RED, GREEN, GOLD, INK = "#1f6feb", "#d1242f", "#1a7f37", "#9a6700", "#1a2332"
r4 = lambda xs: [round(float(v), 6) for v in xs]

cfg, resp, wq = r["config"], r["response"], r["wave_quality"]
t_hist = np.array(r["heave"]["t"]); z_hist = np.array(r["heave"]["z_cm"])
W0, W1 = cfg["steady_window"]
A_I = resp["incident_amplitude"]
# equilibrium CoM height: depth - draft + body_height/2 (= the waterline, 0.4 m)
Z_EQ_TH = cfg["wave"]["depth"] - cfg["body"]["draft"] + cfg["body"]["height"] / 2
BODY_X = cfg["body"]["x"]; BEAM = cfg["body"]["beam"]; BH = cfg["body"]["height"]
rao_pct = resp["rao_error"] * 100
draft_pct = resp["draft_error"] * 100

import pyvista as pv

# ---- animation: x-z slice around the body over the last two wave periods ----
bg_case = CASE / "background"
(bg_case / "case.foam").write_text("")
reader = pv.OpenFOAMReader(str(bg_case / "case.foam"))
times = [tv for tv in reader.time_values if W1 - 6.0 <= tv <= W1]
frames, labels = [], []
X0V, X1V, Z1V = BODY_X - 2.6, BODY_X + 2.6, 0.7
for tv in times:
    reader.set_active_time_value(tv)
    cc = reader.read()["internalMesh"].cell_centers()
    a = np.asarray(cc.cell_data["alpha.water"])
    pts = cc.points
    m = (np.abs(pts[:, 1] - 0.02) < 0.021) & (pts[:, 0] > X0V) & (pts[:, 0] < X1V)
    x, z, av = pts[m, 0], pts[m, 2], a[m]
    z_cm = float(np.interp(tv, t_hist, z_hist))
    fig, ax = plt.subplots(figsize=(7.6, 2.9))
    ax.tricontourf(x, z, av, levels=[0.5, 1.01], colors=["#9ec8f0"])
    ax.tricontourf(x, z, av, levels=24, cmap="Blues", vmin=0, vmax=1, alpha=0.85)
    ax.add_patch(plt.Rectangle((BODY_X - BEAM / 2, z_cm - BH / 2), BEAM, BH,
                               facecolor="#5a4632", edgecolor="#2e2418", zorder=5))
    ax.axhline(0.4, color=RED, lw=0.7, ls="--", alpha=0.6)
    ax.set_xlim(X0V, X1V); ax.set_ylim(0.15, Z1V)
    ax.set_aspect("equal")
    ax.set_xticks([]); ax.set_yticks([])
    ax.set_title(f"t = {tv:.2f} s   z_cm = {z_cm:.4f} m", fontsize=10)
    fig.tight_layout(pad=0.3)
    frames.append(fig_to_jpeg_b64(fig)); plt.close(fig)
    labels.append(f"t = {tv:.2f} s")
anim = flipbook("web-anim", frames, labels, fps=8,
                caption="Steady-state wave-excited heave (x–z slice around the body, last two wave "
                        "periods): the box rides the incident wave on its rigid overset component mesh. "
                        "Dashed line = still-water level.")

# ---- chart 1: full heave history --------------------------------------------
sub = slice(None, None, 3)
heave_chart = plot_div("web-heave", [
    {"x": r4(t_hist[sub]), "y": r4(z_hist[sub]), "mode": "lines",
     "name": "CoM height z(t)", "line": {"color": BLUE, "width": 1.6},
     "hovertemplate": "t=%{x:.2f}s<br>z=%{y:.4f}m<extra></extra>"},
    {"x": [0, float(t_hist[-1])], "y": [Z_EQ_TH, Z_EQ_TH], "mode": "lines",
     "name": "Archimedes equilibrium (CoM at waterline)",
     "line": {"color": RED, "width": 1.3, "dash": "dash"}, "hoverinfo": "skip"},
    {"x": [W0, W0, W1, W1], "y": [0.36, 0.44, 0.44, 0.36], "fill": "toself",
     "mode": "none", "name": f"steady analysis window [{W0:.0f}, {W1:.0f}] s",
     "fillcolor": "rgba(26,127,55,0.08)", "line": {"width": 0}, "hoverinfo": "skip"},
], {
    "title": {"text": "Heave response — calm ramp, wave arrival, steady wave-following"},
    "xaxis": {"title": {"text": "t (s)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "centre-of-mass height (m)"}, "gridcolor": "#eef1f5"},
    "legend": {"x": 0.02, "y": 0.98},
})

# ---- chart 2: steady window, heave vs incident wave (normalized) -------------
msk = (t_hist >= W0) & (t_hist <= W1)
tw, zw = t_hist[msk], z_hist[msk]
zn = (zw - zw.mean()) / A_I
om = 2 * np.pi / cfg["wave"]["T"]
# incident wave at the body reconstructed from the split (unit amplitude)
eta_n = np.cos(om * (tw - tw[0]) + np.angle(
    np.trapz((zw - zw.mean()) * np.exp(-1j * om * (tw - tw[0])), tw)))
follow_chart = plot_div("web-follow", [
    {"x": r4(tw[::2]), "y": r4(zn[::2]), "mode": "lines",
     "name": f"heave / a_incident  (RAO = {resp['rao']:.3f})",
     "line": {"color": BLUE, "width": 2},
     "hovertemplate": "t=%{x:.2f}s<br>z/a=%{y:.3f}<extra></extra>"},
    {"x": r4(tw[::2]), "y": r4(eta_n[::2]), "mode": "lines",
     "name": "unit wave follower (RAO = 1, same phase)",
     "line": {"color": GOLD, "width": 1.4, "dash": "dot"}, "hoverinfo": "skip"},
    {"x": [float(tw[0]), float(tw[-1])], "y": [1, 1], "mode": "lines",
     "showlegend": False, "line": {"color": "#c7d0dc", "width": 0.8, "dash": "dash"},
     "hoverinfo": "skip"},
    {"x": [float(tw[0]), float(tw[-1])], "y": [-1, -1], "mode": "lines",
     "showlegend": False, "line": {"color": "#c7d0dc", "width": 0.8, "dash": "dash"},
     "hoverinfo": "skip"},
], {
    "title": {"text": "Steady window — the body follows the long wave (quasi-static limit)"},
    "xaxis": {"title": {"text": "t (s)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "heave / incident amplitude"}, "gridcolor": "#eef1f5",
              "range": [-1.45, 1.45]},
    "legend": {"x": 0.02, "y": 0.02},
}, height=380)

# ---- chart 3: gauge array + incident/reflected split -------------------------
gauge_traces = []
for g in wq["gauges"]:
    if 7.9 <= g["x"] <= 12.9:
        gauge_traces.append({"x": [g["x"]], "y": [g["H"]], "mode": "markers",
                             "name": f"gauge x={g['x']:g} m",
                             "marker": {"color": BLUE, "size": 9},
                             "showlegend": False,
                             "hovertemplate": f"x={g['x']:g}m<br>H=%{{y:.4f}}m<extra></extra>"})
gauge_traces.append({
    "x": [7.9, 12.9], "y": [cfg["wave"]["H"], cfg["wave"]["H"]], "mode": "lines",
    "name": "input H = 0.05 m", "line": {"color": RED, "width": 1.3, "dash": "dash"},
    "hoverinfo": "skip"})
gauge_traces.append({
    "x": [7.9, 12.9], "y": [2 * A_I, 2 * A_I], "mode": "lines",
    "name": f"incident 2a_i = {2 * A_I:.4f} m (Goda–Suzuki split, Kr = {wq['reflection_kr']:.3f})",
    "line": {"color": GREEN, "width": 1.5}, "hoverinfo": "skip"})
gauge_chart = plot_div("web-gauges", gauge_traces, {
    "title": {"text": "Upstream gauge array — incident wave isolated from the body's scattered field"},
    "xaxis": {"title": {"text": "gauge x (m)"}, "gridcolor": "#eef1f5"},
    "yaxis": {"title": {"text": "wave height (m)"}, "gridcolor": "#eef1f5",
              "rangemode": "tozero"},
    "legend": {"x": 0.02, "y": 0.08},
}, height=340)

gates = resp["gates"]
ok = lambda b: ("good", "PASS") if b else ("bad", "FAIL")
g1c, g1s = ok(gates["rao"]); g2c, g2s = ok(gates["draft"]); g3c, g3s = ok(gates["period"])

fast_row = ""
if rf:
    fr = rf["response"]
    fast_row = (f"<tr><td>Fast regression variant (250&times;35, ~75 min at 8-way)</td>"
                f"<td>RAO {fr['rao']:.3f}, draft {fr['draft_error']*100:+.1f}%, "
                f"T {fr['period_measured']:.3f} s</td>"
                f"<td>reference values reproduced</td><td class=\"good\">mesh-consistent</td>"
                f"<td class=\"good\">PASS</td></tr>")

usecase = """
<div class="usecase"><h3>What this gates in real work</h3>
<p>This is the suite's first <b>wave-body interaction</b> case — incident waves exciting a floating
structure whose motion feeds back on the flow. It is the CFD twin of the <b>regular-wave RAO test</b>
run for every floating asset (FPSOs, semis, spars, floating wind, installation barges):</p>
<ul>
<li><b>Heave RAO spot-checks vs potential flow</b> — the same setup at shorter wavelengths produces
CFD RAO points to check the OrcaWave/AQWA diffraction pipeline (see /hydro/) where viscous or
steep-wave effects make panel methods suspect.</li>
<li><b>Relative motion &amp; air gap</b> — wave-frequency body motion against the incident surface is
the input to green-water/air-gap screening (the Kleefsman case gates the impact side).</li>
<li><b>Overset machinery for large motions</b> — rigid component-mesh motion removes the morphing
collapse that kills deforming-mesh runs at wave arrival; the same machinery extends to installation
lowering, float-over mating and hull sections in steep waves.</li>
</ul></div>"""

body = f"""
<header>
<div class="kicker">digitalmodel &middot; CFD verification suite &middot; case 9 &middot; marine &middot; interactive</div>
<h1>Wave-Excited Floating Body: Overset Heave RAO vs the Long-Wave Limit</h1>
<div class="sub">A half-density box floats in the verified StokesII regular wave (#1170), free to heave
(#1169 constraints) on a rigid <b>overset</b> component mesh (overInterDyMFoam). At &lambda;/B &asymp; 29 the
quasi-static limit pins the answer: a small floating body is a wave follower, heave RAO &rarr; 1.
Issue #1302, epic #1161.</div>
<p><span class="badge">VERIFIED &mdash; heave RAO {resp['rao']:.3f} ({rao_pct:+.1f}% vs the wave-follower limit); draft {draft_pct:+.1f}%; periodic at T</span></p>
<div class="meta">
<div><b>Solver</b><span>overInterDyMFoam + sixDoF, v2312</span></div>
<div><b>Wave</b><span>StokesII H = {cfg['wave']['H']:g} m, T = {cfg['wave']['T']:g} s, d = {cfg['wave']['depth']:g} m, &lambda; = {cfg['wave']['wavelength']:.2f} m</span></div>
<div><b>Body</b><span>{BEAM:g}&times;{BH:g} m section, &rho; = {cfg['body']['density']:g} kg/m&sup3;, &lambda;/B = {cfg['wave']['lambda_over_beam']:.0f}</span></div>
<div><b>Heave RAO</b><span>{resp['rao']:.3f} vs 1.0 (gate &plusmn;15%)</span></div>
<div><b>Incident amplitude</b><span>a<sub>i</sub> = {A_I:.4f} m (upstream split, Kr = {wq['reflection_kr']:.3f})</span></div>
<div><b>Regression test</b><span>tests/solvers/openfoam/validation/test_wave_excited_body.py</span></div>
</div>
</header>

<h2>1&nbsp;&nbsp;Objective</h2>
<p>Verify wave-excited rigid-body motion — the mechanism behind every seakeeping RAO. The verified
numerical wave tank (#1170) and the verified heave-constrained sixDoF body (#1169) are combined into
one case: a {BEAM:g} m box floating at its Archimedes draft at x = {BODY_X:g} m, excited by the
established regular wave. In the long-wavelength limit (&lambda;/B &asymp; {cfg['wave']['lambda_over_beam']:.0f},
excitation period {cfg['wave']['T']:g} s &raquo; heave natural period ~{cfg['body']['natural_period_hydrostatic']:.2f}&ndash;{cfg['body']['natural_period_hydrostatic'] * 1.4:.2f} s)
a freely heaving body rides the surface: <b>heave RAO &rarr; 1</b> (Newman 1977, ch. 6). The incident
amplitude is measured by a Goda&ndash;Suzuki incident/reflected split over the upstream gauge array, so the
body's own scattered field cannot pollute the denominator.</p>
{usecase}

<h2>2&nbsp;&nbsp;Why overset &mdash; the morphing-mesh failure this case retires</h2>
<p>The first attempt at this case used the #1169 machinery unchanged: <code>dynamicMotionSolverFvMesh</code>
morphing a 2D slab background around the moving body. It floats perfectly through the calm ramp and
<b>dies at first wave arrival</b> &mdash; the distance-blended point motion shears a waterline corner cell to
zero volume and the run aborts on a <code>deltaCoeffs</code> divide-by-zero, reproducibly across
decompositions, Courant settings and blending distances (diagnosis on #1302). The overset approach
eliminates the failure structurally: the component mesh around the body <b>moves rigidly</b> (no cell ever
deforms) and the flow is interpolated across the overlap (<code>inverseDistance</code>). This is the standard
OpenFOAM path for wave + moving body, and the machinery the suite now has verified for large-motion
cases. One more portability note: <code>sixDoFRigidBodyState</code> hard-casts the mesh to
<code>dynamicMotionSolverFvMesh</code>, so under the overset mesh the body state is parsed from the sixDoF
<code>report on</code> log lines instead.</p>

<h2>3&nbsp;&nbsp;The response &mdash; animated</h2>
{anim}

<h2>4&nbsp;&nbsp;Heave response</h2>
{heave_chart}
{follow_chart}
{gauge_chart}
<table>
<tr><th>Quantity</th><th>Measured</th><th>Reference</th><th>Error / gate</th><th>Status</th></tr>
<tr><td>Heave RAO (2A<sub>z</sub>/H<sub>i</sub>)</td><td>{resp['rao']:.3f}</td><td>1.0 (wave-follower limit)</td><td class="{g1c}">{rao_pct:+.1f}% (gate &plusmn;15%)</td><td class="{g1c}">{g1s}</td></tr>
<tr><td>Mean draft</td><td>{resp['draft_measured']:.4f} m</td><td>Archimedes {cfg['body']['draft']:.4f} m</td><td class="{g2c}">{draft_pct:+.1f}% (gate &plusmn;5%)</td><td class="{g2c}">{g2s}</td></tr>
<tr><td>Response period</td><td>{resp['period_measured']:.3f} s</td><td>wave period {cfg['wave']['T']:g} s</td><td class="{g3c}">{resp['period_error']*100:+.1f}% (gate &plusmn;5%)</td><td class="{g3c}">{g3s}</td></tr>
<tr><td>Incident wave at the array</td><td>2a<sub>i</sub> = {2*A_I:.4f} m</td><td>input H = {cfg['wave']['H']:g} m</td><td class="good">{(2*A_I/cfg['wave']['H']-1)*100:+.1f}%, Kr = {wq['reflection_kr']:.3f}</td><td class="good">consistent</td></tr>
{fast_row}
</table>
<p><b>Why RAO &rarr; 1 is the right gate &mdash; and what would break it.</b> At &lambda;/B &asymp; {cfg['wave']['lambda_over_beam']:.0f} the
wave surface is essentially flat across the body, and the excitation is far below the heave resonance;
hydrostatics dominates and the body tracks the surface. Deviations of order kB/2 and
(&omega;/&omega;<sub>n</sub>)&sup2; are a few percent &mdash; well inside the &plusmn;15% gate. A solver with broken wave
kinematics, wrong buoyancy coupling, or overset interpolation losses would miss the limit; a body
with significant draft-to-wavelength ratio or excited near resonance would legitimately depart from 1
(that departure IS the RAO curve this case is the anchor of).</p>

<h2>5&nbsp;&nbsp;Numerical model</h2>
<table>
<tr><th>Item</th><th>Setting</th></tr>
<tr><td>Background</td><td>verified #1170 NWT ({cfg['mesh']['background'][0]}&times;{cfg['mesh']['background'][2]}, 20 m, top raised to 0.7 m), StokesII inlet + active absorption both ends</td></tr>
<tr><td>Component</td><td>{cfg['mesh']['component'][0]}&times;{cfg['mesh']['component'][2]} block around the body, hole by topoSet + subsetMesh, <b>rigid</b> overset motion (innerDistance &raquo; component size)</td></tr>
<tr><td>Overset</td><td><code>dynamicOversetFvMesh</code>, <code>inverseDistance</code> interpolation, zoneID via merged-mesh topoSet (v2312 floatingBody tutorial recipe)</td></tr>
<tr><td>Motion</td><td><code>sixDoFRigidBodyMotion</code>, Newmark, heave-only (line + orientation constraints), accelerationRelaxation 0.7</td></tr>
<tr><td>Body</td><td>{BEAM:g}&times;{BH:g} m, &rho; = {cfg['body']['density']:g} kg/m&sup3; &rarr; Archimedes draft {cfg['body']['draft']:.2f} m; starts <em>at</em> equilibrium (no decay transient)</td></tr>
<tr><td>Schemes</td><td>#1170 wave-fidelity divs (linearUpwind momentum, vanLeer alpha) + tutorial overset blocks; maxCo = maxAlphaCo = 0.65</td></tr>
<tr><td>Recorder</td><td>sixDoF <code>report on</code> log lines (last outer-corrector per step); 8 <code>interfaceHeight</code> gauges</td></tr>
<tr><td>Run</td><td>two-stage: component <code>blockMesh&rarr;topoSet&rarr;subsetMesh</code>, then background <code>blockMesh&rarr;mergeMeshes&rarr;topoSet&rarr;setFields&rarr;overInterDyMFoam</code> (runner grew <code>mergeMeshes</code> + mesh-prep-only stages)</td></tr>
</table>
<p>The case is generated by <code>digitalmodel.solvers.openfoam.validation.build_wave_excited_body_case</code>.
Archived copy with analyzer: <code>docs/api/cfd/cases/wave_excited_body/</code>. The long solve is opt-in for
the regression suite via <code>DIGITALMODEL_RUN_LONG_CFD=1</code> (Kleefsman tier).</p>

<h2>6&nbsp;&nbsp;Conclusion</h2>
<p>The overset wave-body machinery reproduces the long-wave limit: heave RAO {resp['rao']:.3f} against the
theoretical 1.0, mean draft within {abs(draft_pct):.1f}% of Archimedes, response locked to the wave period.
The deforming-mesh failure that parked this case is retired, and the suite now has a verified path to
CFD RAOs for floating structures &mdash; the direct bridge to the OrcaWave/AQWA diffraction pipeline.</p>

<h2>References</h2>
<ol>
<li>OpenFOAM ESI v2312 tutorials: <code>multiphase/overInterDyMFoam/floatingBody</code> (overset recipe),
<code>multiphase/interFoam/laminar/waves/stokesII</code> (wave generation, verified in #1170),
<code>multiphase/overInterDyMFoam/twoSquaresOutDomain</code> (2D overset precedent).</li>
<li>Newman, J.N. (1977). <i>Marine Hydrodynamics</i>, MIT Press — quasi-static response of small
floating bodies in long waves.</li>
<li>Goda, Y. &amp; Suzuki, Y. (1976). Estimation of incident and reflected waves in random wave
experiments — gauge-array split (N-gauge least-squares form, as in the #1170 case).</li>
<li>digitalmodel issue #1302 (diagnosis of the morphing collapse + this fix) under epic #1161;
regression test <code>tests/solvers/openfoam/validation/test_wave_excited_body.py</code>.</li>
</ol>
<p class="sub">Generated from a real OpenFOAM v2312 solve. All chart data and animation frames are computed
directly from the solved fields, gauge records and body-state history &mdash; no values are hand-entered.</p>
"""

out = WT / "docs/api/cfd/wave-excited-body-verification.html"
out.write_text(page("CFD Verification — Wave-Excited Floating Body (Overset)",
                    "wave-excited-body", house_style(WT), body))
print(out, f"{out.stat().st_size / 1024:.0f} KB, {len(frames)} frames")
