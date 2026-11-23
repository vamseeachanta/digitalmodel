# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Deterministic HTML analysis with inputs, equations, results and rule gaps."""

from __future__ import annotations

import math
from html import escape

from digitalmodel.lifting_lug.assessment import PadeyeAssessment


def _number(value: float | None) -> str:
    return "not available" if value is None else f"{value:.1f}"


def render_report(result: PadeyeAssessment) -> str:
    """Render a single load case; retain the distinction between screen and rules.

    Values are displayed to one decimal place, but decisions use unrounded
    results. A displayed minimum throat is rounded upward to the next tenth.
    """
    plate, load, weld = result.plate, result.load, result.weld
    inputs = [
        ("Main plate thickness (mm)", plate.thickness_mm),
        ("Pin diameter (mm)", plate.pin_diameter_mm),
        ("Hole diameter (mm)", plate.hole_diameter_mm),
        ("Plate width across hole (mm)", plate.width_mm),
        ("Hole centre to loaded free edge (mm)", plate.centre_to_loaded_edge_mm),
        ("Static sling tension, angle already included (kN)", load.static_load_kn),
        ("Dynamic amplification (dimensionless)", load.dynamic_amplification),
        ("Skew factor (dimensionless)", load.skew_factor),
        ("Design sling load (kN)", result.design_load_kn),
        ("Total effective weld length (mm)", weld.effective_length_mm if weld else None),
        ("Effective weld throat (mm)", weld.effective_throat_mm if weld else None),
    ]
    input_rows = "\n".join(
        f"<tr><td>{escape(label)}</td><td>{_number(value)}</td></tr>"
        for label, value in inputs
    )
    check_rows = "\n".join(
        f"<tr><td>{escape(check.name)}</td>"
        f"<td>{_number(check.demand_mpa)}</td>"
        f"<td>{_number(check.allowable_mpa)}</td>"
        f"<td>{_number(check.utilization)}</td><td>{check.status}</td></tr>"
        for check in result.checks
    )
    evidence = "\n".join(
        f"<li><strong>{escape(check.name)}</strong>: {escape(check.method_source)}. "
        f"Allowable basis: {escape(check.allowable_source) or 'not available'}. "
        f"{escape(check.reason)}</li>" for check in result.checks
    )
    gaps = "\n".join(
        f"<li><strong>{escape(check.name)}: {check.status}</strong>. "
        f"{escape(check.method_source)}. {escape(check.reason)}</li>"
        for check in result.qualification_checks
    )
    throat = result.required_weld_throat_mm
    # Ten subdivisions per mm define reporting precision, not a design factor.
    displayed_throat = math.ceil(throat * 10) / 10 if throat is not None else None
    sizing = (
        "Required effective throat cannot be evaluated without effective weld "
        "length and a sourced allowable."
        if throat is None else
        f"The direct-shear calculation requires an effective throat of at least "
        f"{_number(displayed_throat)} mm using the supplied illustrative allowable "
        "and total effective length. This minimum is rounded upward for display. "
        "Reassess a candidate at or above this value, then establish the applicable "
        "weld limits, load path and rule requirements before selecting a weld."
    )
    return f'''<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Padeye and lift point analysis</title>
<style>
body{{font:17px/1.6 system-ui,sans-serif;color:#173245;background:#edf2f5;margin:0}}
main{{max-width:1040px;margin:auto;background:white;padding:44px}}
h1{{font-size:34px;line-height:1.2}}h2{{margin-top:36px;border-bottom:2px solid #ccdce5}}
a{{color:#075783}}nav{{display:flex;flex-wrap:wrap;gap:18px}}
.status{{padding:18px;border-left:5px solid #bd7800;background:#fff5df}}
table{{border-collapse:collapse;width:100%;font-size:15px}}
th,td{{padding:9px;text-align:left;border-bottom:1px solid #d6e0e7}}
th{{background:#edf3f6}}caption{{caption-side:bottom;text-align:left;padding:8px 0}}
code{{overflow-wrap:anywhere}}small{{color:#526572}}
@media(max-width:650px){{main{{padding:20px}}table{{font-size:12px}}th,td{{padding:5px}}}}
</style>
</head>
<body><main>
<header><small>Technical analysis · Revision 0 · Synthetic single-plate illustration</small>
<h1>Padeye and lift point analysis</h1></header>
<nav aria-label="Report sections"><a href="#summary">Summary</a>
<a href="#inputs">Inputs</a><a href="#method">Method</a>
<a href="#results">Results</a><a href="#limits">Limits and way forward</a></nav>
<section id="summary"><h2>Summary</h2>
<p class="status">Nominal screening: <strong>{result.screening_status}</strong>.
Combined disposition: <strong>{result.status}</strong>.
ABS compliance and lifting qualification: <strong>NOT_EVALUATED</strong>.</p>
<p>This calculation compares nominal single-plate and direct-shear weld demands
with explicit supplied allowables. A passing comparison establishes only that
these assumed demands do not exceed those allowables. {sizing}</p></section>
<section id="revision"><h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-22</td><td>Initial calculation format with explicit
inputs and qualification gaps, to make the screening basis reviewable.</td></tr></tbody></table></section>
<section id="inputs"><h2>Inputs and data basis</h2>
<p>Load and resistance basis: {escape(load.basis)}.
The worked example uses synthetic dimensions and illustrative allowables;
no grade properties or rule coefficients are inferred.</p>
<table><thead><tr><th>Parameter</th><th>Value</th></tr></thead>
<tbody>{input_rows}</tbody>
<caption>Dimensions are in mm; stress units are N/mm², equivalent to MPa.
Weld length is already the sum of effective segments, with no implicit doubling.</caption></table></section>
<section id="method"><h2>Method and assumptions</h2>
<p>Let P = static sling tension × dynamic amplification × skew factor.
These factors are required load-case inputs, without automatic rule minima.
The sling tension already includes its angle and load share.
For plate thickness t, pin diameter d, hole diameter D, width W and
hole-centre distance e to the loaded free edge:</p>
<ul><li>Projected bearing area A_b = d t; nominal bearing stress = P / A_b.</li>
<li>Net section A_n = (W − D) t; nominal tensile stress = P / A_n.</li>
<li>Two clear shear planes A_s = 2 (e − D/2) t; nominal tear-out stress = P / A_s.</li>
<li>Effective weld area A_w = L a; direct shear = P / A_w.</li>
<li>Required effective throat a_required = P / (L τ_allow).</li>
<li>Utilization = demand / supplied allowable; nominal PASS requires utilization ≤ 1.</li></ul>
<p>Normal stress uses MIT, <cite>Stress-Strain Curves</cite> (2001), equation (1).
Direct shear uses MIT, <cite>Shear and Torsion</cite> (2000), page 2.
The stated areas follow the assumed geometry and equilibrium; these sources
provide nominal stress definitions, not ABS lug resistance equations.
A centred, in-plane axial load and a concentric direct-shear weld group are required.</p></section>
<section id="results"><h2>Results</h2>
<table><thead><tr><th>Check</th><th>Demand (MPa)</th><th>Allowable (MPa)</th>
<th>Utilization</th><th>Status</th></tr></thead><tbody>{check_rows}</tbody>
<caption>Displayed values have one decimal place. Status uses unrounded values.
Missing criteria are reported as not available, never as zero resistance or a pass.</caption></table>
<p>{sizing}</p><h3>Equation and allowable traceability</h3><ul>{evidence}</ul></section>
<section id="limits"><h2>Limits and way forward</h2><ul>{gaps}</ul>
<p>Establish the applicable ABS edition, equation clauses, resistance limits and
lifting-standard applicability. Review actual weld orientation and load transfer;
where moments or normal throat stresses occur, use a full weld-group analysis.
Then check pin bending and shear, contact and hoop stresses, interaction,
cheek-plate load sharing, supporting structure, fatigue, fabrication, inspection
and testing as applicable. None is established by these nominal comparisons.</p>
<p>The effective throat requirement is not a fillet-leg size, a fabrication
minimum or a complete weld design. Geometry may later be supplied by a drawing
reader; this calculation does not infer dimensions from drawings or a solver.</p></section>
<section id="references"><h2>Sources and reproducibility</h2>
<ul><li><a href="https://web.mit.edu/course/3/3.11/www/modules/ss.pdf">MIT,
Stress-Strain Curves (2001)</a>, equation (1).</li>
<li><a href="https://web.mit.edu/course/3/3.11/www/modules/torsion.pdf">MIT,
Shear and Torsion (2000)</a>, page 2, direct shear.</li>
<li><a href="https://standards.nasa.gov/standard/NASA/NASA-STD-87199">NASA-STD-8719.9C,
Lifting Standard (2024)</a>, section 13.3.2.4; qualification context only.</li>
<li><a href="https://ww2.eagle.org/en/rules-and-resources/rules-and-guides.html">ABS,
public rules index</a>; equation evidence unavailable. No ABS coefficient is applied.</li></ul>
<p>The executable synthetic example and independent test hand calculations accompany
the lifting-lug package. The example regenerates this page without a licensed solver.</p>
</section></main></body></html>
'''
