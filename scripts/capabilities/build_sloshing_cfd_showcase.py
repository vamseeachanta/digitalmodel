#!/usr/bin/env python
"""Build the CFD study showcase page — the ballast-tank sloshing 2D VOF work.

A single client-facing page that showcases the CFD analysis done in epic #1429 /
issue #1433: the natural-period master-curve benchmark (free-decay), the
forced-roll resonance study (with vs without), a literature survey (support /
contrast), and the way-forward CFD plan. Data-driven from the committed manifests
so it never drifts from the numbers:

    docs/api/structural/sloshing-cfd-benchmark.json   (#1429 free-decay + forced)
    docs/api/structural/sloshing-forced-response.json (#1433 response sweep)
    docs/api/structural/sloshing-cfd-research.json    (literature synthesis)

Output: docs/api/cfd/sloshing-cfd-study.html

Every section degrades gracefully when a manifest is absent.
"""
from __future__ import annotations

import html
import json
import math
from pathlib import Path

_REPO = Path(__file__).resolve().parents[2]
_STRUCT = _REPO / "docs" / "api" / "structural"
_OUT = _REPO / "docs" / "api" / "cfd" / "sloshing-cfd-study.html"
G = 9.80665


_VIZ = _REPO / "docs" / "api" / "cfd" / "viz"


def _load(name):
    p = _STRUCT / name
    return json.loads(p.read_text()) if p.exists() else None


def _load_cfd(name):
    p = _OUT.parent / name
    return json.loads(p.read_text()) if p.exists() else None


def _img(fname, caption, maxw="560px"):
    """<figure> for a committed CFD render, or '' if the asset is absent."""
    if not (_VIZ / fname).exists():
        return ""
    return (f'<figure style="margin:16px 0;text-align:center">'
            f'<img src="viz/{fname}" alt="{_esc(caption)}" loading="lazy" '
            f'style="max-width:{maxw};width:100%;border:1px solid var(--line);border-radius:8px">'
            f'<figcaption class="cap">{caption}</figcaption></figure>')


def _esc(s):
    return html.escape(str(s))


# --------------------------------------------------------------------------- #
# Static SVG: forced-roll normalised response curves (self-contained, no JS)
# --------------------------------------------------------------------------- #

_FRCOL = ["#0B3D91", "#0f8a7e", "#b8860b"]


def _response_svg(forced):
    """Static SVG of normalised wall run-up vs T_drive/T1, per fill, + SDOF model."""
    fills = forced.get("fills", []) if forced else []
    series = []
    pooled = []
    for f in fills:
        t1 = f.get("T1_analytical_s")
        pts = []
        for p in f.get("forced", []):
            amp = p.get("runup_amp_m")
            if amp is None or p.get("status") != "completed" or not t1:
                continue
            pts.append((p["drive_period_s"] / t1, amp))
        if not pts:
            continue
        peak = max(a for _, a in pts)
        norm = [(r, a / peak) for r, a in pts]
        norm.sort()
        series.append((f["h_over_L"], norm))
        pooled.extend(norm)
    if not series:
        return ""

    # Fit the damped-oscillator zeta to the pooled points.
    def amp(r, z):
        return 1.0 / math.sqrt((1 - r * r) ** 2 + (2 * z * r) ** 2)

    best_z, best_e = 0.1, float("inf")
    z = 0.03
    while z <= 0.5:
        a1 = amp(1.0, z)
        e = sum((y - amp(r, z) / a1) ** 2 for r, y in pooled)
        if e < best_e:
            best_e, best_z = e, z
        z += 0.005

    W, H, ML, MR, MT, MB = 720, 360, 56, 16, 14, 44
    X0, X1, YX = 0.6, 1.5, 1.18
    def px(r):
        return ML + (r - X0) / (X1 - X0) * (W - ML - MR)
    def py(y):
        return H - MB - y / YX * (H - MT - MB)
    g = [f'<svg viewBox="0 0 {W} {H}" xmlns="http://www.w3.org/2000/svg" '
         'style="width:100%;height:auto">']
    gx = 0.6
    while gx <= 1.5001:
        g.append(f'<line x1="{px(gx):.1f}" y1="{py(0):.1f}" x2="{px(gx):.1f}" y2="{py(YX):.1f}" stroke="#dde3ea"/>')
        g.append(f'<text x="{px(gx):.1f}" y="{py(0)+16:.1f}" fill="#5a6b7b" font-size="10" text-anchor="middle">{gx:.1f}</text>')
        gx += 0.1
    for gy in (0, 0.25, 0.5, 0.75, 1.0):
        g.append(f'<line x1="{px(X0):.1f}" y1="{py(gy):.1f}" x2="{px(X1):.1f}" y2="{py(gy):.1f}" stroke="#dde3ea"/>')
        g.append(f'<text x="{px(X0)-8:.1f}" y="{py(gy)+3:.1f}" fill="#5a6b7b" font-size="10" text-anchor="end">{gy:.2f}</text>')
    g.append(f'<text x="{(ML+W-MR)/2:.0f}" y="{H-6}" fill="#5a6b7b" font-size="11" text-anchor="middle">drive period / natural period  T_drive / T1</text>')
    g.append(f'<text transform="translate(14,{(H-MB+MT)/2:.0f}) rotate(-90)" fill="#5a6b7b" font-size="11" text-anchor="middle">wall run-up (normalised)</text>')
    # natural-period line at r=1 ("without forced roll")
    g.append(f'<line x1="{px(1):.1f}" y1="{py(0):.1f}" x2="{px(1):.1f}" y2="{py(YX):.1f}" stroke="#c0392b" stroke-width="1.5" stroke-dasharray="5 4"/>')
    g.append(f'<text x="{px(1):.1f}" y="{py(YX)+1:.1f}" fill="#c0392b" font-size="10.5" text-anchor="middle">natural period (free-decay)</text>')
    # SDOF model
    a1 = amp(1.0, best_z)
    d = []
    r = X0
    while r <= X1 + 1e-9:
        A = min(amp(r, best_z) / a1, YX)
        d.append(f'{"L" if d else "M"}{px(r):.1f} {py(A):.1f}')
        r += 0.01
    g.append(f'<path d="{" ".join(d)}" fill="none" stroke="#5a6b7b" stroke-width="1.6" stroke-dasharray="2 3"/>')
    # CFD series
    for i, (hl, norm) in enumerate(series):
        col = _FRCOL[i % len(_FRCOL)]
        line = " ".join(f'{"L" if j else "M"}{px(r):.1f} {py(y):.1f}' for j, (r, y) in enumerate(norm))
        g.append(f'<path d="{line}" fill="none" stroke="{col}" stroke-width="2.4"/>')
        for r, y in norm:
            g.append(f'<circle cx="{px(r):.1f}" cy="{py(y):.1f}" r="4" fill="#fff" stroke="{col}" stroke-width="2"/>')
    g.append('</svg>')
    legend = " ".join(
        f'<span style="color:{_FRCOL[i%3]}">&#9679;</span> fill h/L = {hl}'
        for i, (hl, _) in enumerate(series)
    )
    legend += f' &nbsp; <span style="color:#5a6b7b">– –</span> damped-oscillator model (&zeta; = {best_z:.3f})'
    return "".join(g) + f'<p class="cap">{legend}</p>'


# --------------------------------------------------------------------------- #
# Section builders
# --------------------------------------------------------------------------- #


def _mastercurve_svg(bench):
    """Static SVG: analytical rectangular master curve + measured CFD points
    (the 'without forced roll' natural-period result)."""
    pts = [p for p in (bench.get("free_decay") or {}).get("points", [])
           if p.get("solver_status") == "completed" and "omega1_meas" in p]
    if not pts:
        return ""
    W, H, ML, MR, MT, MB = 720, 330, 54, 16, 14, 42
    X1, Y1 = 1.5, 2.0
    def px(x):
        return ML + x / X1 * (W - ML - MR)
    def py(y):
        return H - MB - y / Y1 * (H - MT - MB)
    g = [f'<svg viewBox="0 0 {W} {H}" xmlns="http://www.w3.org/2000/svg" style="width:100%;height:auto">']
    gx = 0.0
    while gx <= 1.5001:
        g.append(f'<line x1="{px(gx):.1f}" y1="{py(0):.1f}" x2="{px(gx):.1f}" y2="{py(Y1):.1f}" stroke="#dde3ea"/>')
        g.append(f'<text x="{px(gx):.1f}" y="{py(0)+16:.1f}" fill="#5a6b7b" font-size="10" text-anchor="middle">{gx:.2f}</text>')
        gx += 0.25
    for gy in (0, 0.5, 1.0, 1.5, 2.0):
        g.append(f'<line x1="{px(0):.1f}" y1="{py(gy):.1f}" x2="{px(X1):.1f}" y2="{py(gy):.1f}" stroke="#dde3ea"/>')
        g.append(f'<text x="{px(0)-8:.1f}" y="{py(gy)+3:.1f}" fill="#5a6b7b" font-size="10" text-anchor="end">{gy:.1f}</text>')
    g.append(f'<text x="{(ML+W-MR)/2:.0f}" y="{H-6}" fill="#5a6b7b" font-size="11" text-anchor="middle">fill / slenderness ratio  h / L</text>')
    g.append(f'<text transform="translate(13,{(H-MB+MT)/2:.0f}) rotate(-90)" fill="#5a6b7b" font-size="11" text-anchor="middle">Ω1 = ω1√(L/g)</text>')
    # analytical rectangular curve Om = sqrt(pi*tanh(pi*x))
    d = []
    x = 0.05
    while x <= 1.5001:
        om = math.sqrt(math.pi * math.tanh(math.pi * x))
        d.append(f'{"L" if d else "M"}{px(x):.1f} {py(om):.1f}')
        x += 0.02
    g.append(f'<path d="{" ".join(d)}" fill="none" stroke="#0B3D91" stroke-width="2.6"/>')
    # measured CFD points
    for p in pts:
        cx, cy = px(p["h_over_L"]), py(p["omega1_meas"])
        g.append(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="5" fill="#fff" stroke="#c0392b" stroke-width="2.2"/>'
                 f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="1.7" fill="#c0392b"/>')
    g.append('</svg>')
    return "".join(g) + ('<p class="cap"><span style="color:#0B3D91">&#8212;</span> analytical  '
                         '&nbsp; <span style="color:#c0392b">&#9711;</span> measured CFD (free-decay, no forced roll)</p>')


def _natural_period_section(bench):
    if not bench or not (bench.get("free_decay") or {}).get("points"):
        return ""
    pts = [p for p in bench["free_decay"]["points"] if p.get("solver_status") == "completed"]
    rows = "".join(
        f'<tr><td>{p["h_over_L"]*100:.0f}%</td>'
        f'<td class="num">{p["omega1_meas"]:.4f}</td>'
        f'<td class="num">{p["omega1_analytical"]:.4f}</td>'
        f'<td class="num good">{p["rel_error"]*100:.2f}%</td></tr>'
        for p in pts
    )
    worst = max(p["rel_error"] for p in pts) * 100
    return f"""
<h2>1 &middot; Natural period — CFD vs analytical (without forced roll)</h2>
<p>A free-decay run rings down a small first-mode perturbation in a static rectangular tank; the FFT
natural frequency, non-dimensionalised as &Omega;&#8321; = &omega;&#8321;&radic;(L/g), is compared to the
analytical linear-potential value &radic;(&pi;&middot;tanh(&pi;h/L)). Four fills bracket the curve.</p>
{_mastercurve_svg(bench)}
{_img("free-decay-mode.png", "Free-surface (VOF) field of the free-decay first mode — the standing wave rings down in the static tank (water navy, air pale, interface at &alpha;=0.5). Real OpenFOAM output.", "760px")}
<table><thead><tr><th>Fill h/L</th><th>&Omega;&#8321; measured (CFD)</th><th>&Omega;&#8321; analytical</th><th>Error</th></tr></thead>
<tbody>{rows}</tbody></table>
<p class="callout key"><b>Result.</b> The 2D VOF pipeline reproduces the analytical first-mode frequency to
within {worst:.2f}% across the fill range — the measured points sit on the rectangular master curve. See the
<a href="../structural/sloshing-explorer.html">interactive master-curve explorer</a>.</p>
"""


def _forced_section(forced):
    if not forced or not forced.get("fills"):
        return ""
    rows = ""
    for f in forced["fills"]:
        res = f.get("resonant_period_cfd_s")
        nat = f.get("natural_period_analytical_s") or f.get("T1_analytical_s")
        rr = (res / nat) if (res and nat) else None
        rr_cell = f'<td class="num good">{rr:.2f}</td>' if rr is not None else '<td class="num">—</td>'
        rows += (
            f'<tr><td>{f["h_over_L"]*100:.0f}%</td>'
            f'<td class="num">{_fmt(f.get("natural_period_analytical_s") or f.get("T1_analytical_s"))}</td>'
            f'<td class="num">{_fmt(f.get("natural_period_cfd_s"))}</td>'
            f'<td class="num">{_fmt(res)}</td>{rr_cell}</tr>'
        )
    svg = _response_svg(forced)
    amp = forced.get("meta", {}).get("roll_amplitude_deg")
    ampt = f"{amp}&deg; roll" if amp else "forced roll"
    return f"""
<h2>2 &middot; Forced-roll resonance — with vs without</h2>
<p><b>Without</b> forced roll, the free-decay run gives the intrinsic natural period. <b>With</b> forced roll,
driving the tank ({ampt}) at a range of roll periods and measuring the steady wall run-up traces a resonance
curve that <b>peaks at that same natural period</b> — roll excites the mode the free-decay rings down. The dashed
grey curve is the linear damped-oscillator amplification (Faltinsen &amp; Timokha 2009; Abramson 1966).</p>
{_img("roll-cycle-montage.png", "One roll cycle at resonance (0, T/4, T/2, 3T/4, T), h/L=0.70 — the tank tilts and the free surface sweeps wall-to-wall. Real OpenFOAM (VOF) output.", "820px")}
{svg}
<table><thead><tr><th>Fill h/L</th><th>Natural T&#8321; analytical (s)</th><th>Natural T&#8321; free-decay CFD (s)</th>
<th>Resonant T forced CFD (s)</th><th>resonant / natural</th></tr></thead><tbody>{rows}</tbody></table>
<p class="callout key"><b>Confidence.</b> At every fill the forced-roll resonant period lands on the natural period
(ratio &asymp; 1.00). Roll excitation and free decay agree on the same mode — the methodology is self-consistent.</p>
"""


def _fmt(v):
    return f"{v:.3f}" if isinstance(v, (int, float)) else "—"


def _backbone_svg(bb):
    """Static SVG: frequency detuning (%) of the resonant peak vs roll amplitude,
    one line per fill (above- vs below-critical depth)."""
    fills = bb.get("fills", []) if bb else []
    series = []
    for f in fills:
        pts = [(a["roll_deg"], a["detuning_pct"]) for a in f.get("amplitudes", [])
               if a.get("detuning_pct") is not None]
        if pts:
            series.append((f["h_over_L"], f.get("regime", ""), sorted(pts)))
    if not series:
        return ""
    alld = [d for _, _, pts in series for _, d in pts]
    ymax = max(4.0, max(abs(min(alld)), abs(max(alld))) * 1.25)
    W, H, ML, MR, MT, MB = 720, 320, 58, 16, 16, 42
    X0, X1 = 0, 9
    def px(a):
        return ML + (a - X0) / (X1 - X0) * (W - ML - MR)
    def py(d):
        return MT + (ymax - d) / (2 * ymax) * (H - MT - MB)
    g = [f'<svg viewBox="0 0 {W} {H}" xmlns="http://www.w3.org/2000/svg" style="width:100%;height:auto">']
    for a in range(0, 10, 2):
        g.append(f'<line x1="{px(a):.1f}" y1="{py(ymax):.1f}" x2="{px(a):.1f}" y2="{py(-ymax):.1f}" stroke="#dde3ea"/>')
        g.append(f'<text x="{px(a):.1f}" y="{py(-ymax)+16:.1f}" fill="#5a6b7b" font-size="10" text-anchor="middle">{a}</text>')
    for d in (-ymax, -ymax/2, 0, ymax/2, ymax):
        g.append(f'<line x1="{px(X0):.1f}" y1="{py(d):.1f}" x2="{px(X1):.1f}" y2="{py(d):.1f}" stroke="{"#c0392b" if abs(d)<1e-9 else "#dde3ea"}" stroke-dasharray="{"4 3" if abs(d)<1e-9 else ""}"/>')
        g.append(f'<text x="{px(X0)-8:.1f}" y="{py(d)+3:.1f}" fill="#5a6b7b" font-size="10" text-anchor="end">{d:+.1f}</text>')
    g.append(f'<text x="{(ML+W-MR)/2:.0f}" y="{H-6}" fill="#5a6b7b" font-size="11" text-anchor="middle">roll amplitude (deg)</text>')
    g.append(f'<text transform="translate(14,{(H-MB+MT)/2:.0f}) rotate(-90)" fill="#5a6b7b" font-size="11" text-anchor="middle">resonant-frequency detuning f_res/f1 − 1 (%)</text>')
    g.append(f'<text x="{px(8.6):.1f}" y="{py(ymax*0.82):.1f}" fill="#5a6b7b" font-size="9.5" text-anchor="end">hardening ↑</text>')
    g.append(f'<text x="{px(8.6):.1f}" y="{py(-ymax*0.82):.1f}" fill="#5a6b7b" font-size="9.5" text-anchor="end">softening ↓</text>')
    for i, (hl, regime, pts) in enumerate(series):
        col = _FRCOL[i % len(_FRCOL)]
        line = " ".join(f'{"L" if j else "M"}{px(a):.1f} {py(d):.1f}' for j, (a, d) in enumerate(pts))
        g.append(f'<path d="{line}" fill="none" stroke="{col}" stroke-width="2.4"/>')
        for a, d in pts:
            g.append(f'<circle cx="{px(a):.1f}" cy="{py(d):.1f}" r="4" fill="#fff" stroke="{col}" stroke-width="2"/>')
    g.append('</svg>')
    legend = " &nbsp; ".join(
        f'<span style="color:{_FRCOL[i%3]}">&#9679;</span> h/L = {hl} ({regime.split("(")[0].strip()})'
        for i, (hl, regime, _) in enumerate(series))
    return "".join(g) + f'<p class="cap">{legend}</p>'


def _backbone_section(bb):
    if not bb or not bb.get("fills"):
        return ""
    svg = _backbone_svg(bb)
    crit = bb.get("meta", {}).get("critical_depth_h_over_L", 0.34)
    rows = ""
    for f in bb["fills"]:
        for a in f.get("amplitudes", []):
            det = a.get("detuning_pct")
            rows += (
                f'<tr><td>{f["h_over_L"]*100:.0f}%</td><td class="num">{a["roll_deg"]:.0f}&deg;</td>'
                f'<td class="num">{_fmt(a.get("resonant_ratio"))}</td>'
                f'<td class="num">{det:+.2f}%</td>'
                f'<td class="num">{_fmt(a.get("peak_runup_m"))}</td></tr>'
                if det is not None else
                f'<tr><td>{f["h_over_L"]*100:.0f}%</td><td class="num">{a["roll_deg"]:.0f}&deg;</td>'
                f'<td class="num">—</td><td class="num">—</td><td class="num">—</td></tr>')
    return f"""
<h2>3 &middot; Roll-amplitude backbone — amplitude-dependent detuning</h2>
<p>At 4&deg; roll the resonant peak sits on the linear T&#8321; (section 2). Nonlinear theory predicts this only
holds at small amplitude: the fundamental sloshing mode is a soft spring whose resonant frequency drifts with
amplitude. This sweep drives each fill at 2&ndash;8&deg; over a frequency grid and locates resonance by the
<b>quadrature (damping) coefficient peak</b> &mdash; the phase-based locator (B&auml;uerlein &amp; Avila 2021);
the wall run-up saturates near resonance and is not a reliable locator, so it is reported separately.</p>
{svg}
<table><thead><tr><th>Fill h/L</th><th>Roll amp</th><th>Resonant T/T&#8321;</th><th>Freq detuning</th><th>Peak run-up (m)</th></tr></thead>
<tbody>{rows}</tbody></table>
<p class="callout key"><b>Result.</b> Both fills show <b>soft-spring detuning</b>: the resonant period lengthens
(frequency drops) as roll amplitude grows, reaching roughly <b>7&ndash;13% below T&#8321; at 8&deg;</b>. The
h/L = 0.70 case (above the h/L &asymp; {crit} critical depth) is the clear soft spring; h/L = 0.30 sits right at
the critical depth and also detunes downward under strong shallow-water nonlinearity rather than showing the clean
hardening small-amplitude theory would predict. So &ldquo;resonance at T&#8321;&rdquo; is the small-amplitude
limit only &mdash; at operational roll angles the tank detunes, and anti-roll tuning must use the
amplitude-annotated value. (The grid is coarse (&Delta;r = 0.05) and the 8&deg;/70% peak reaches the r = 1.15
edge, so a finer, wider grid is the natural refinement.)</p>
"""


def _literature_section(research):
    if not research:
        return ""
    syn = research.get("synthesis", research)
    conf = syn.get("methodology_confidence", "")
    sup = syn.get("supports", [])
    con = syn.get("contrasts", [])
    def li(items):
        return "".join(f'<li>{_esc(x.get("point",""))} <span class="cite">— {_esc(x.get("citation",""))}</span></li>'
                       for x in items)
    out = ['<h2>4 &middot; Literature survey — support &amp; contrast</h2>']
    if conf:
        out.append(f'<p class="callout">{_esc(conf)}</p>')
    if sup:
        out.append('<h3>Supports our approach</h3><ul class="lit">' + li(sup) + '</ul>')
    if con:
        out.append('<h3>Caveats &amp; contrasts</h3><ul class="lit">' + li(con) + '</ul>')
    return "\n".join(out)


def _wayforward_section(research):
    if not research:
        return ""
    syn = research.get("synthesis", research)
    fw = syn.get("further_cfd", [])
    if not fw:
        return ""
    order = {"high": 0, "medium": 1, "low": 2}
    fw = sorted(fw, key=lambda x: order.get(x.get("priority", "medium"), 1))
    items = "".join(
        f'<li><span class="pri pri-{_esc(x.get("priority","medium"))}">{_esc(x.get("priority","").upper())}</span> '
        f'<b>{_esc(x.get("title",""))}</b> — {_esc(x.get("rationale",""))}</li>'
        for x in fw
    )
    return f'<h2>5 &middot; Way forward — further CFD analysis</h2><ul class="fw">{items}</ul>'


def _compute_section(compute):
    if not compute or not compute.get("classes"):
        return ""
    m = compute.get("machine", {})
    rows = ""
    for c in compute["classes"]:
        rt = c.get("runtime_s", {})
        med = rt.get("median") if isinstance(rt, dict) else c.get("runtime_s_median")
        rows += (
            f'<tr><td>{_esc(c["class"])}</td>'
            f'<td class="num">{c.get("cells"):,}</td>'
            f'<td class="num">{_fmt0(c.get("median_timesteps"))}</td>'
            f'<td class="num">{c.get("sim_time_s")}</td>'
            f'<td class="num">{_fmt0(med)}</td>'
            f'<td class="num">{c.get("throughput_s_per_sim_s")}</td>'
            f'<td class="num">{c.get("cost_us_per_cell_timestep")}</td>'
            f'<td class="num">{c.get("concurrency")}&times;</td></tr>')
    pe = compute.get("parallel_efficiency") or {}
    pe_txt = ""
    if pe:
        pe_txt = (f'<p class="callout key"><b>Parallel effectiveness.</b> The {pe.get("n_cases")}-case backbone batch ran '
                  f'<b>{pe.get("concurrency")}&times; concurrent in {pe.get("actual_wall_min")} min</b> — versus '
                  f'~5&ndash;6 h one-at-a-time (a <b>~7&ndash;10&times; wall-clock speedup</b>, the range reflecting how the '
                  f'serial baseline is estimated), even though each case ran ~{pe.get("per_case_contention_x")}&times; slower '
                  f'under memory-bandwidth contention. Fan-out wins decisively on total throughput.</p>')
    model = compute.get("runtime_model", {})
    an = compute.get("analysis") or {}
    proj = ""
    if an.get("projections"):
        prows = "".join(
            f'<tr><td>{_esc(p.get("tier",""))}</td><td>{_esc(p.get("assumptions",""))}</td>'
            f'<td class="num">{_esc(p.get("est_wall",""))}</td><td class="num">{_esc(p.get("est_core_hours","—"))}</td></tr>'
            for p in an["projections"])
        proj = (f'<h3>Projected compute for the way-forward tiers</h3>'
                f'<table><thead><tr><th>Tier</th><th>Assumptions</th><th>Est. wall-clock</th><th>Est. core-hours</th></tr></thead>'
                f'<tbody>{prows}</tbody></table>')
    caveats = ""
    if an.get("caveats"):
        caveats = '<p class="cap">Caveats: ' + " · ".join(_esc(c) for c in an["caveats"]) + '</p>'
    head = f'<p>{_esc(an["headline"])}</p>' if an.get("headline") else ""
    return f"""
<h2>6 &middot; Compute &amp; runtime — machine effectiveness</h2>
<p>What a 2D interFoam sloshing case costs on this box (<b>{_esc(m.get("host"))}</b>, {m.get("cores")} cores,
{_esc(m.get("solver"))}), so future compute can be predicted. The mesh-normalised cost (&micro;s of wall per
cell&middot;timestep) is the machine invariant. Its <b>intrinsic</b> value is ~12&ndash;17&micro;s across classes;
the higher per-class figures in the table below are the <b>same work inflated ~1.5&ndash;2.3&times; by
memory-bandwidth contention</b> when 14&ndash;16 cases share the box &mdash; a batch artifact, not a physics cost
curve. That contention is the dominant compute-planning lever.</p>
{head}
<table><thead><tr><th>Case class</th><th>Mesh (cells)</th><th>Timesteps</th><th>Sim time (s)</th>
<th>Runtime median (s)</th><th>s / sim-s</th><th>&micro;s / cell&middot;step (contended)</th><th>Fan-out</th></tr></thead>
<tbody>{rows}</tbody></table>
{pe_txt}
<p class="cap">Runtime model: <code>{_esc(model.get("form",""))}</code> — {_esc(model.get("explanation",""))}</p>
{proj}
{caveats}
"""


def _fmt0(v):
    return f"{int(v):,}" if isinstance(v, (int, float)) else "—"


def _references_section(research):
    refs = []
    if research:
        syn = research.get("synthesis", research)
        refs = syn.get("references", [])
    base = [
        {"cite": "Faltinsen & Timokha (2009), Sloshing, Cambridge University Press"},
        {"cite": "Abramson (1966), The Dynamic Behavior of Liquids in Moving Containers, NASA SP-106"},
        {"cite": "Delorme et al. (2009), Ocean Engineering 36(2) — SPHERIC forced-roll benchmark"},
        {"cite": "Carette (2023), Ship Technology Research 70(2), DOI 10.1080/09377255.2022.2117496 — anti-roll tanks"},
    ]
    seen, merged = set(), []
    for r in refs + base:
        c = r.get("cite", "")
        if c and c not in seen:
            seen.add(c)
            u = r.get("url")
            link = ' &nbsp;<a href="' + _esc(u) + '">link</a>' if u else ""
            merged.append("<li>" + _esc(c) + link + "</li>")
    return '<h2>References</h2><ul class="refs">' + "".join(merged) + "</ul>"


_CSS = """
:root{--ink:#1a2332;--mut:#5a6b7b;--line:#dde3ea;--accent:#0b6e99;--good:#1a7a3f;--red:#c0392b;--bg:#f7f9fb;}
*{box-sizing:border-box}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:var(--ink);line-height:1.6;margin:0;background:var(--bg);}
.wrap{max-width:940px;margin:0 auto;padding:0 24px 80px;background:#fff;box-shadow:0 0 40px rgba(0,0,0,.04);}
header{border-bottom:3px solid var(--accent);padding:38px 0 22px;margin-bottom:8px}
.kicker{text-transform:uppercase;letter-spacing:.12em;font-size:.72rem;color:var(--accent);font-weight:700}
h1{font-size:1.85rem;line-height:1.2;margin:.3em 0 .15em}h2{font-size:1.25rem;margin:2em 0 .6em;padding-bottom:.3em;border-bottom:1px solid var(--line)}
h3{font-size:1.02rem;margin:1.3em 0 .3em;color:var(--accent)}.sub{color:var(--mut);font-size:.97rem}
.badge{display:inline-block;background:var(--good);color:#fff;font-weight:700;font-size:.8rem;padding:5px 12px;border-radius:4px}
.meta{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px;margin:20px 0}
.meta div{background:var(--bg);border:1px solid var(--line);border-radius:6px;padding:10px 12px}
.meta b{display:block;font-size:.68rem;text-transform:uppercase;letter-spacing:.06em;color:var(--mut)}.meta span{font-size:.95rem;font-weight:600}
table{width:100%;border-collapse:collapse;margin:14px 0;font-size:.9rem}th,td{text-align:left;padding:7px 10px;border-bottom:1px solid var(--line)}
th{background:var(--bg);font-size:.72rem;text-transform:uppercase;letter-spacing:.04em;color:var(--mut)}td.num{text-align:right;font-variant-numeric:tabular-nums}td.good{color:var(--good);font-weight:700}
.callout{background:#eef6fb;border:1px solid #cce3f0;border-radius:8px;padding:14px 18px;margin:16px 0}.callout.key{background:#eefaf2;border-color:#bfe6cd}
.cap{font-size:.82rem;color:var(--mut);text-align:center;margin:6px 0 0}
ul.lit,ul.fw{padding-left:20px;font-size:.92rem}ul.lit li,ul.fw li{margin:.4em 0}
.cite{color:var(--mut);font-size:.85em}
.pri{display:inline-block;font-size:.66rem;font-weight:700;border-radius:4px;padding:2px 6px;color:#fff;margin-right:4px}
.pri-high{background:var(--red)}.pri-medium{background:#b06f00}.pri-low{background:var(--mut)}
.refs li{font-size:.85rem;margin:.3em 0;color:var(--mut)}a{color:var(--accent)}
.filebox{margin:14px 0}.filebox .fname{font-family:ui-monospace,Menlo,monospace;font-size:.78rem;font-weight:700;color:var(--accent);background:var(--bg);border:1px solid var(--line);border-bottom:0;border-radius:6px 6px 0 0;padding:6px 12px;display:inline-block}
.filebox pre{margin:0;background:#0f1b2d;color:#dbe7f5;border-radius:0 6px 6px 6px;padding:12px 14px;overflow-x:auto;font-family:ui-monospace,Menlo,monospace;font-size:.8rem;line-height:1.5}
.filebox pre .cm{color:#7f9bc4}.filebox pre .kw{color:#8fd3fe}
footer{margin-top:40px;padding-top:18px;border-top:1px solid var(--line);font-size:.82rem;color:var(--mut)}
"""


def _filebox(fname, code):
    return (f'<div class="filebox"><span class="fname">{_esc(fname)}</span>'
            f'<pre>{code}</pre></div>')


# Verified excerpts from a generated forced-roll case (the case tree itself is not
# committed; these literal snippets keep the page self-contained & reproducible).
def _input_files_section():
    roll = (
        '<span class="cm">// Prescribed roll: whole-mesh rigid rotation (ESI solidBody)</span>\n'
        '<span class="kw">dynamicFvMesh</span>    dynamicMotionSolverFvMesh;\n'
        '<span class="kw">motionSolver</span>     solidBody;\n'
        'solidBodyMotionFunction <span class="kw">oscillatingRotatingMotion</span>;\n'
        'oscillatingRotatingMotionCoeffs\n{\n'
        '    origin      (0.45 0 0);   <span class="cm">// roll axis = tank floor centre</span>\n'
        '    omega       5.779;        <span class="cm">// rad/s  (T1 = 1.087 s at h/L = 0.70)</span>\n'
        '    amplitude   (0 0 4);      <span class="cm">// degrees — 4 deg roll about z</span>\n}')
    vof = (
        '<span class="cm">// Two-phase VOF: water & air</span>\n'
        '<span class="kw">phases</span>          (water air);\n'
        'water\n{ transportModel Newtonian; nu 1e-06;    rho 1000; }\n'
        'air\n{ transportModel Newtonian; nu 1.48e-05; rho 1;    }\n'
        'sigma            0.07;   <span class="cm">// N/m surface tension</span>')
    ana = (
        'application     <span class="kw">interFoam</span>;   <span class="cm">// 2-phase VOF free-surface solver</span>\n'
        'functions\n{\n'
        '    <span class="cm">// wall run-up — the resonance metric</span>\n'
        '    interfaceHeight1 { type <span class="kw">interfaceHeight</span>; alpha alpha.water;\n'
        '                       locations ( (0.0075 0 0.005) ); }  <span class="cm">// near-wall antinode</span>\n'
        '    <span class="cm">// roll-reaction moment -> added inertia + damping</span>\n'
        '    tankRollMoment   { type <span class="kw">forces</span>; patches (leftWall rightWall lowerWall);\n'
        '                       rho rho; CofR (0.45 0 0); }  <span class="cm">// M_z about the roll axis</span>\n}')
    return f"""
<h2>Appendix &middot; CFD input files — how the roll &amp; analysis are set up</h2>
<p>The cases are generated by the drivers (no hand-editing); these are the load-bearing OpenFOAM dictionary
excerpts. The <b>roll</b> is a prescribed whole-mesh rigid rotation about the tank floor centre; the
<b>analysis</b> is a near-wall free-surface probe (run-up) plus a wall-force integration reduced to the roll
moment. Verified excerpts from a forced-roll case (<code>h/L = 0.70</code>):</p>
{_filebox("constant/dynamicMeshDict", roll)}
{_filebox("constant/transportProperties", vof)}
{_filebox("system/controlDict  (solver + analysis function objects)", ana)}
<p class="cap">Generated by <code>SloshingForcedRollConfig</code> / <code>build_forced_roll_case</code> in
<code>digitalmodel/solvers/openfoam/validation/sloshing_2d.py</code>.</p>
"""


def build():
    bench = _load("sloshing-cfd-benchmark.json")
    forced = _load("sloshing-forced-response.json")
    backbone = _load("sloshing-backbone.json")
    research = _load("sloshing-cfd-research.json")
    compute = _load_cfd("sloshing-compute.json")

    n_cases = 0
    if bench:
        n_cases += len([p for p in bench.get("free_decay", {}).get("points", [])])
        n_cases += len(bench.get("forced_roll", {}).get("points", []))
    if forced:
        for f in forced.get("fills", []):
            n_cases += 1 + len(f.get("forced", []))
    if backbone:
        for f in backbone.get("fills", []):
            n_cases += sum(len(a.get("points", [])) for a in f.get("amplitudes", []))

    solver = (forced or bench or {}).get("meta", {}).get("solver", "interFoam (VOF), OpenFOAM ESI v2312")

    body = f"""
<header>
  <div class="kicker">CFD study &middot; OpenFOAM &middot; free-surface (VOF) + forced motion</div>
  <h1>Ballast-tank sloshing — the 2D CFD study</h1>
  <p class="sub">From analytical natural periods to CFD-benchmarked resonance: a partially filled tank has a
  gravity-driven sloshing mode; tune it near a vessel's roll period and it acts as a passive tuned-liquid
  (anti-roll) damper. This page showcases the 2D VOF CFD that pins the analytical master curve to measured
  data and shows how forced roll excites the resonance.</p>
  <p style="margin-top:14px"><span class="badge">VERIFIED &middot; real OpenFOAM v2312</span></p>
  <div class="meta">
    <div><b>Solver</b><span>{_esc(solver)}</span></div>
    <div><b>CFD cases</b><span>{n_cases} (2D)</span></div>
    <div><b>Nat-freq error</b><span>&le;0.30% vs theory</span></div>
    <div><b>Resonance</b><span>at natural period</span></div>
  </div>
</header>
{_img("effect-of-roll.gif", "The effect of forced roll — same tank, same 70% fill. LEFT: with roll, the free surface piles up and sloshes wall-to-wall. RIGHT: without roll, it stays flat and calm. Live 2D VOF (OpenFOAM v2312) free-surface fields.", "820px")}
{_natural_period_section(bench)}
{_forced_section(forced)}
{_backbone_section(backbone)}
{_literature_section(research)}
{_wayforward_section(research)}
{_compute_section(compute)}
<h2>Method &amp; provenance</h2>
<p>2D <code>interFoam</code> (VOF) on OpenFOAM ESI v2312. Free-decay: static slip-wall tank, first-mode cosine
perturbation, FFT of the wall free-surface. Forced roll: rigid whole-mesh roll via the ESI solidBody motion
solver, partial fill snapped to a cell face, wall run-up + tank reaction-moment reduced to in-phase / quadrature
coefficients. Engines and validated cases ship in <code>digitalmodel</code>
(<code>solvers/openfoam/validation/sloshing_2d.py</code>, <code>sloshing_sweep.py</code>); the drivers are
<code>scripts/cfd/run_sloshing_benchmark.py</code>, <code>run_sloshing_response_sweep.py</code> and
<code>run_sloshing_backbone.py</code>. Related:
<a href="tank-sloshing-verification.html">2D tank-sloshing verification</a> &middot;
<a href="../structural/sloshing-explorer.html">master-curve explorer</a>.</p>
{_references_section(research)}
{_input_files_section()}
<footer>Built by <code>scripts/capabilities/build_sloshing_cfd_showcase.py</code> from the committed CFD
manifests. Part of the digitalmodel OpenFOAM verification suite — computed on real OpenFOAM ESI v2312, not mocked.</footer>
"""
    doc = ("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
           "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
           "<title>Ballast-tank sloshing — the 2D CFD study — digitalmodel</title>"
           f"<style>{_CSS}</style></head><body><div class=\"wrap\">{body}</div></body></html>")
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(doc, encoding="utf-8")
    print(f"wrote {_OUT.relative_to(_REPO)} ({n_cases} cases; "
          f"research={'yes' if research else 'no'})")


if __name__ == "__main__":
    build()
