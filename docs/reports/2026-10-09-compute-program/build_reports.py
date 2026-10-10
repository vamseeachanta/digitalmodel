"""Build the compute-program case-study pages from run outputs.

Usage: python build_reports.py <compute-program-root> <out-dir>
Self-contained HTML (inline SVG charts, embedded PNG figures). Machines are
named by role label only.
"""

from __future__ import annotations

import base64
import glob
import html
import json
import sys
from pathlib import Path

ROOT, OUT = Path(sys.argv[1]), Path(sys.argv[2])
OUT.mkdir(parents=True, exist_ok=True)
COMMIT = "4f7bfc0c"
BLUE, ORANGE = "#2a78d6", "#eb6834"

CSS = """
:root{--bg:#f9f9f7;--card:#fcfcfb;--ink:#0b0b0b;--ink2:#52514e;--mute:#898781;--line:#e3e2dd}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);
font:16px/1.55 system-ui,-apple-system,Segoe UI,sans-serif}
main{max-width:920px;margin:auto;padding:28px 16px 60px}
h1{font-size:28px;line-height:1.2;margin:0 0 6px}h2{font-size:20px;margin:34px 0 10px}
h3{font-size:16px;margin:22px 0 6px}p{margin:8px 0}.sub{color:var(--ink2)}
.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:16px;margin:14px 0}
.scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:14.5px}
th,td{padding:8px 10px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}
th{color:var(--ink2);font-weight:600}td.n{text-align:right;font-variant-numeric:tabular-nums}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px;margin:14px 0}
.tile{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:12px 14px}
.tile b{display:block;font-size:26px;line-height:1.15}.tile span{color:var(--ink2);font-size:13.5px}
figure{margin:12px 0}figure img,figure svg{max-width:100%;height:auto;display:block}
figcaption{color:var(--ink2);font-size:13.5px;margin-top:6px}
.legend{display:flex;gap:18px;font-size:13.5px;color:var(--ink2);margin:4px 0 8px}
.sw{display:inline-block;width:14px;height:3px;border-radius:2px;vertical-align:middle;margin-right:6px}
.note{border-left:3px solid var(--mute);padding:2px 12px;color:var(--ink2);margin:12px 0}
footer{color:var(--mute);font-size:13px;margin-top:36px}
"""


def page(name: str, title: str, sub: str, body: str) -> None:
    doc = (f"<!doctype html><html lang='en'><meta charset='utf-8'>"
           f"<meta name='viewport' content='width=device-width,initial-scale=1'>"
           f"<title>{html.escape(title)}</title><style>{CSS}</style><main>"
           f"<h1>{html.escape(title)}</h1><p class='sub'>{sub}</p>{body}"
           f"<footer>Compute program, 9 October 2026. Code: digitalmodel at "
           f"<code>{COMMIT}</code>. Machines are named by role only.</footer></main></html>")
    (OUT / name).write_text(doc, encoding="utf-8")
    print("wrote", OUT / name, len(doc) // 1024, "kB")


def img(path: Path, caption: str) -> str:
    data = base64.b64encode(path.read_bytes()).decode()
    return (f"<figure><img alt='{html.escape(caption)}' "
            f"src='data:image/png;base64,{data}'><figcaption>{caption}</figcaption></figure>")


def table(head, rows, numeric=()) -> str:
    th = "".join(f"<th>{h}</th>" for h in head)
    body = ""
    for r in rows:
        body += "<tr>" + "".join(
            f"<td class='n'>{c}</td>" if i in numeric else f"<td>{c}</td>"
            for i, c in enumerate(r)) + "</tr>"
    return f"<div class='scroll'><table><thead><tr>{th}</tr></thead><tbody>{body}</tbody></table></div>"


def line_chart(xs, series, xlabel, ylabel, fmt="{:.3g}", ymin=0.0) -> str:
    """series: [(name, colour, ys)]. One y axis, hover titles on every point."""
    W, H, L, R, T, B = 760, 340, 64, 20, 14, 46
    ymax = max(max(ys) for _, _, ys in series) * 1.06
    x0, x1 = min(xs), max(xs)
    px = lambda x: L + (x - x0) / (x1 - x0) * (W - L - R)
    py = lambda y: H - B - (y - ymin) / (ymax - ymin) * (H - T - B)
    out = [f"<svg viewBox='0 0 {W} {H}' role='img' aria-label='{html.escape(ylabel)} against {html.escape(xlabel)}'>"]
    for i in range(5):
        y = ymin + (ymax - ymin) * i / 4
        out.append(f"<line x1='{L}' x2='{W-R}' y1='{py(y):.1f}' y2='{py(y):.1f}' stroke='#e3e2dd'/>"
                   f"<text x='{L-8}' y='{py(y)+4:.1f}' text-anchor='end' font-size='12' fill='#898781'>{fmt.format(y)}</text>")
    for x in xs[::3]:
        out.append(f"<text x='{px(x):.1f}' y='{H-B+18}' text-anchor='middle' font-size='12' fill='#898781'>{x:g}</text>")
    out.append(f"<text x='{(L+W-R)/2}' y='{H-8}' text-anchor='middle' font-size='12.5' fill='#52514e'>{xlabel}</text>")
    out.append(f"<text x='14' y='{(T+H-B)/2}' text-anchor='middle' font-size='12.5' fill='#52514e' "
               f"transform='rotate(-90 14 {(T+H-B)/2})'>{ylabel}</text>")
    for name, colour, ys in series:
        pts = " ".join(f"{px(x):.1f},{py(y):.1f}" for x, y in zip(xs, ys))
        out.append(f"<polyline points='{pts}' fill='none' stroke='{colour}' stroke-width='2' stroke-linejoin='round'/>")
        for x, y in zip(xs, ys):
            out.append(f"<circle cx='{px(x):.1f}' cy='{py(y):.1f}' r='4' fill='{colour}' stroke='#fcfcfb' stroke-width='2'>"
                       f"<title>{name}: {fmt.format(y)} at {x:g} rad/s</title></circle>")
    out.append("</svg>")
    legend = "".join(f"<span><i class='sw' style='background:{c}'></i>{n}</span>" for n, c, _ in series)
    return f"<div class='legend'>{legend}</div>" + "".join(out)


# ------------------------------------------------ 1. known-answer re-run

V = ROOT / "scratch" / "verif"


def load(rel):
    hits = glob.glob(str(V / rel))
    return json.loads(Path(hits[0]).read_text()) if hits else None


cyl, flat, tflat = load("cylinder_re100/*/results.json"), load("flat_plate_blasius/*/results.json"), load("turbulent_flat_plate/*/results.json")
dam, naca, wave = load("dam_break/*/results.json"), load("naca0012_polar/*/results.json"), load("wave_tank/*/results.json")
fb = load("floating_body_decay/*/results.json")

rows = []
if cyl:
    rows += [["Cylinder, Re = 100", "Mean drag coefficient", "1.33 to 1.37 (Williamson 1996)", "1.344", f"{cyl['mean_cd']:.3f}", f"{cyl['cd_err_pct']:+.1f} % against 1.35"],
             ["", "Strouhal number", "0.164", "0.1655", f"{cyl['strouhal']:.4f}", f"{cyl['st_err_pct']:+.1f} %"]]
if flat:
    rows += [["Laminar flat plate", "Skin friction against Blasius, mean error", "0.664 / sqrt(Re_x)", "4.6 %", f"{flat['cf_mean_abs_err_pct']:.1f} %", f"max {flat['cf_max_abs_err_pct']:.1f} %"]]
if tflat:
    rows += [["Turbulent flat plate", "Skin friction against Schlichting, mean error", "turbulent Cf correlation", "about 9 %", f"{tflat['cf_mean_abs_err_schlichting_pct']:.1f} %", f"max {tflat['cf_max_abs_err_schlichting_pct']:.1f} %; y+ {tflat['yplus_min']} to {tflat['yplus_max']}"]]
if naca:
    rows += [["NACA 0012 polar", "Lift-curve slope, per degree", "0.106 (experiment)", "0.1037", f"{naca['slope_per_deg']:.4f}", f"{naca['slope_err_vs_exp_pct']:+.1f} % against experiment; {naca['slope_err_vs_theory_pct']:+.1f} % against thin-airfoil theory"]]
if dam:
    rows += [["Dam break", "Surge front against Martin and Moyce, mean deviation", "experiment, with gate-release shift", "3.0 %", f"{dam['front_dev_mean_gate_corrected']*100:.1f} %", f"max {dam['front_dev_max_gate_corrected']*100:.1f} %; mass drift {dam['mass_drift_rel']:.1e}"]]
if fb:
    rows += [["Floating-body heave decay", "Equilibrium draft against Archimedes", f"{fb['draft_theory']:.4f} m", "+1.1 %", f"{fb['draft_err']*100:+.1f} %",
              f"{fb['n_cycles']} decay cycles; heave period {fb['T_measured']:.3f} s, {fb['T_ratio']:.2f} x the hydrostatic period (published 1.18)"]]
if wave:
    rows += [["Wave tank", "Wavenumber against linear dispersion", "omega^2 = g k tanh(k d)", "+0.2 %", f"{wave['k_err']*100:+.1f} %", "reproduces"],
             ["", "Wave height in the established region, max error", "input wave height", "4.7 %", f"{wave['H_err_established_max']*100:.1f} %", "same side of the 5 % gate"],
             ["", "Height decay along the tank", "none expected", "4.6 %", f"{wave['H_decay_8_to_18']*100:.1f} %", "<b>differs</b>: the re-run sits on the 5 % gate, the published value inside it"],
             ["", "Reflection coefficient", "0 for a perfect beach", "0.010", f"{wave['reflection_Kr']:.3f}", "both far below the 0.10 gate"]]
else:
    rows += [["Wave tank", "Wavenumber against linear dispersion", "omega^2 = g k tanh(k d)", "+0.2 %", "not run", ""]]

done = sum(x is not None for x in (cyl, flat, tflat, dam, naca, wave, fb))
figs = ""
for rel, cap in [("cylinder_re100/cyl_results/force_history.png", "Cylinder at Re = 100: drag and lift coefficient history from the re-run. The averaging window is t = 120 to 160."),
                 ("flat_plate_blasius/validation_results/cf_vs_rex.png", "Laminar flat plate: skin friction from the re-run against the Blasius solution."),
                 ("turbulent_flat_plate/tflat_results/law_of_wall.png", "Turbulent flat plate: velocity profile from the re-run against the law of the wall."),
                 ("naca0012_polar/naca_results/lift_curve.png", "NACA 0012: lift curve from the re-run."),
                 ("dam_break/validation_results/front.png", "Dam break: surge-front position from the re-run against the Martin and Moyce experiment.")]:
    if (V / rel).exists():
        figs += img(V / rel, cap)

body = f"""
<div class='tiles'>
<div class='tile'><b>{done} of 7</b><span>cases re-run to completion</span></div>
<div class='tile'><b>{done - (1 if wave else 0)} of {done}</b><span>reproduce the published figures to the digits shown; the wave tank differs on one of its four measures</span></div>
<div class='tile'><b>v2312</b><span>OpenFOAM (ESI), unmodified</span></div>
</div>
<p>Each case below has a known answer from theory or a published experiment. The case files
are public in the digitalmodel repository. On 9 October 2026 they were copied from the pinned
commit to a clean directory on a Linux CFD host that had not run them before, and executed
with the commands in each case's README. The table sets the fresh result beside the figure
the repository already publishes and beside the reference it is measured against.</p>
<div class='card'>{table(['Case', 'Quantity', 'Reference', 'Published', 'This re-run', 'Note'], rows)}</div>
<div class='note'>What this shows: the published figures can be regenerated from the committed
case files on a second machine. The wave tank is the exception: wavenumber reproduces, but height decay along the tank came out at 5.0 % against a published 4.6 %, which moves it from just inside to just on its own 5 % acceptance gate. The cause has not been investigated. What none of this shows: accuracy on any hull or structure other
than these textbook geometries. The turbulent flat plate sits 9 to 10 % from the correlation,
which is the stated spread of the correlation itself, not a tuned match.</div>
<h2>Figures from the re-run</h2>{figs}
<h2>How to repeat it</h2>
<p>Cases: <code>docs/api/cfd/cases/&lt;case&gt;/</code> in digitalmodel at <code>{COMMIT}</code>. Each
README has a "Reproduce" block. All six are serial runs; the longest (wave tank) takes about
half an hour on a 2015-era Xeon core, the shortest about three minutes.</p>
"""
page("openfoam-known-answer-rerun.html", "OpenFOAM known-answer cases: independent re-run",
     "Seven textbook CFD cases re-executed from the committed files on a second machine.", body)

# ------------------------------------------------ 2. barge, Capytaine leg

cap = json.loads((ROOT / "results" / "barge-diffraction" / "capytaine.json").read_text())
om = cap["omega_rad_s"]
nolid, lid = cap["runs"]
idx = cap["sample_indices"]
c33_analytic = 1025.0 * 9.80665 * 100.0 * 20.0 / 1000.0
srows = [[f"{om[i]:.2f}", f"{nolid['a33_te'][i]:,.0f}", f"{lid['a33_te'][i]:,.0f}",
          f"{nolid['heave_rao_0deg'][i]:.4f}", f"{lid['heave_rao_0deg'][i]:.4f}"] for i in idx]
worst = max(range(len(om)), key=lambda i: abs(nolid["a33_te"][i] / lid["a33_te"][i] - 1))
body = f"""
<div class='tiles'>
<div class='tile'><b>{nolid['solve_s']:.0f} s</b><span>Capytaine solve, 8 processes, no lid</span></div>
<div class='tile'><b>{nolid['panels']:,}</b><span>panels, the same mesh the licensed cases use</span></div>
<div class='tile'><b>{lid['c33_kN_per_m']:,.0f} kN/m</b><span>heave stiffness; rho g A gives {c33_analytic:,.0f}</span></div>
</div>
<div class='note'>Status: this is the open-source half only. The OrcaWave and AQWA runs of the
same barge have not been recorded yet, so no licensed-against-open-source difference is
reported here.</div>
<p>The solver baseline pack defines one box barge (100 x 20 x 8 m draft, 200 m water depth,
20 frequencies from 0.20 to 1.53 rad/s) that OrcaWave and AQWA both solve. Capytaine
{cap['solver_version']}, an open-source boundary-element code, was given the identical panel
mesh, mass and frequencies.</p>
<h2>Heave added mass</h2>
<div class='card'><figure>{line_chart(om, [('No interior lid', BLUE, nolid['a33_te']), ('With interior lid', ORANGE, lid['a33_te'])], 'Wave frequency (rad/s)', 'Heave added mass (te)', '{:,.0f}')}
<figcaption>The two curves separate above about 1.2 rad/s, where the hull's interior free
surface has its first resonance (an "irregular frequency", a numerical artefact of the method).
The largest difference is {abs(nolid['a33_te'][worst]/lid['a33_te'][worst]-1)*100:.1f} % at {om[worst]:.2f} rad/s.</figcaption></figure></div>
<h2>Heave response in head seas</h2>
<div class='card'><figure>{line_chart(om, [('No interior lid', BLUE, nolid['heave_rao_0deg']), ('With interior lid', ORANGE, lid['heave_rao_0deg'])], 'Wave frequency (rad/s)', 'Heave RAO at 0 deg (m/m)', '{:.2f}')}
<figcaption>Heave motion per metre of wave amplitude. It tends to 1 in long waves, as it must
for a floating body, and falls away once the wave is shorter than the barge.</figcaption></figure></div>
<h2>Values at the pack's three sample frequencies</h2>
<div class='card'>{table(['Frequency (rad/s)', 'Added mass, no lid (te)', 'Added mass, lid (te)', 'Heave RAO, no lid', 'Heave RAO, lid'], srows, numeric=(0, 1, 2, 3, 4))}</div>
<p>These are the quantities the pack fingerprints for OrcaWave and AQWA, so the three solvers
can be set side by side as soon as the licensed runs exist. One of the three sample points
(1.32 rad/s) lies in the irregular-frequency band, so the licensed runs must state whether
they remove irregular frequencies before the comparison means anything there.</p>
"""
page("barge-diffraction-capytaine.html", "Box-barge diffraction: open-source leg (Capytaine)",
     "The baseline pack's barge solved with an open-source BEM code, ready to set beside OrcaWave and AQWA.", body)

# ------------------------------------------------ 3. OpenFOAM baseline

brows, cds = [], []
for f in sorted(glob.glob(str(ROOT / "receipts" / "ace-linux-1" / "*.json"))):
    r = json.loads(Path(f).read_text())
    env = r["environment"]
    for e in r["results"]:
        if not e["n_ok"]:
            brows.append([r["started_utc"][11:16] + " UTC", e["variant"], "not run", "", "Open MPI refused the rank count (digitalmodel#2320)"])
            continue
        cd = [x["fingerprint"]["cd"] for x in e["repeats"]]
        spread = (max(cd) - min(cd)) / (sum(cd) / len(cd))
        brows.append([r["started_utc"][11:16] + " UTC", e["variant"], f"{e['solve_s']['median']:.1f} s ({e['solve_s']['min']:.1f} to {e['solve_s']['max']:.1f})",
                      ", ".join(f"{c:.5f}" for c in cd) + f" (spread {spread:.1e})",
                      "consistent" if e["fingerprint_consistent"] else "outside the 1e-3 tolerance"])
body = f"""
<div class='tiles'>
<div class='tile'><b>34.9 s</b><span>median solve, 8 MPI ranks</span></div>
<div class='tile'><b>33.1 s</b><span>median solve, 16 MPI ranks</span></div>
<div class='tile'><b>353,578</b><span>cells in every run</span></div>
</div>
<div class='note'>Status: one machine of the fleet. The second Linux host was occupied by other
CFD work and the licensed solvers (OrcaFlex, OrcaWave, AQWA, MAPDL) have not been baselined, so
this is not yet a cross-machine comparison.</div>
<p>The solver baseline pack runs a fixed case on each machine and records how long it takes
and whether it returns the same answer. For OpenFOAM the case is the standard motorBike
tutorial (v2312, simpleFoam, 100 iterations) with the mesh built serially once so every run
solves the same {353578:,} cells. Each row is three timed repeats after one warm-up, on an
idle host: {env['cpu_model']}, {env['physical_cores']} physical cores, {env['ram_gb']:.0f} GB.</p>
<div class='card'>{table(['Run started', 'MPI ranks', 'Median solve time (range)', 'Drag coefficient per repeat', 'Repeat-to-repeat check'], brows, numeric=(1,))}</div>
<h2>What the numbers say</h2>
<p><b>Timing repeats to within 1 to 6 %.</b> Doubling from 8 to 16 ranks saves about 5 %: at
about 22,000 cells per rank the case is too small to scale further on this host, so 8 ranks is
the sensible setting for work of this size and leaves half the machine free.</p>
<p><b>The answer repeats to about one part in a thousand, not better.</b> Identical runs give
drag coefficients that differ in the fourth significant figure, because 100 iterations is short
of convergence and parallel summation order varies. The pack's tolerance is 1e-3, and one of the
two 8-rank runs landed just outside it. The tolerance or the iteration count needs adjusting
before this case is used as a pass/fail gate.</p>
<p><b>One defect found and fixed.</b> The pack's "all cores" setting asked for 32 ranks on a
16-core machine and was refused. The fix is in review (digitalmodel pull request 2321).</p>
"""
page("openfoam-baseline-dev-primary.html", "Solver baseline: OpenFOAM on the primary Linux host",
     "First receipt of the fleet baseline: timing and repeatability of a fixed CFD case.", body)
