# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build an offline installation-module analysis from the public demo catalog.

Usage: PYTHONPATH=src uv run --no-project python scripts/reporting/build_installation_module_report.py
An explicit --output-dir redirects both deliverables. --snap-daf selects a
stricter hydrodynamic reserve through the existing calculation API.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import math
import re
from html import escape
from pathlib import Path

from digitalmodel.installation.installation_pamphlet import calculations as calc

ROOT = Path(__file__).resolve().parents[2]
CATALOG = "examples/demos/gtm/data/mudmat_structures.json"
CALCULATIONS = "src/digitalmodel/installation/installation_pamphlet/calculations.py"
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
OUTPUT = ROOT / "docs/reports/modules/installation"
LABELS = ("Small foundation", "Medium foundation", "Large foundation")
CHANGES = {
    "revision": "1",
    "date": "2026-10-25",
    "changes": [
        "Both report outputs and their build inputs are registered for regeneration checks.",
        "Zero-limit envelopes retain finite chart coordinates without changing calculated limits.",
    ],
}


def house_css() -> str:
    """Inline the existing brand and engineering-page styles without alteration."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    brand = re.sub(r"/\*.*?\*/", "", brand, flags=re.S).strip()
    page = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    core = re.search(r"<style>(.*?)</style>", page, re.S).group(1).strip()
    return '\n'.join(line.rstrip() for line in (brand + "\n" + core).splitlines())


def calculate(snap_daf: float = calc.SPLASH_SNAP_DAF) -> dict:
    """Call the module for the complete public foundation example."""
    if not math.isfinite(snap_daf) or snap_daf < calc.SPLASH_SNAP_DAF:
        raise ValueError("The reserve margin must be finite and at least the module default")
    catalog = calc.load_mudmats(ROOT / CATALOG)
    if len(catalog) != len(LABELS):
        raise ValueError("The catalog coverage changed; review structure labels and scope")
    cases = []
    for label, (key, record) in zip(LABELS, catalog.items()):
        result = calc.compute_splashzone_envelope(
            record, calc.DEFAULT_TP_GRID_S, snap_daf=snap_daf
        )
        cases.append({"label": label, "catalog_id": key, "input": record, "module": result})
    tests = {}
    for filename in ("test_installation_pamphlet.py", "test_pipelay_integrity.py", "test_pipelay_stress.py"):
        tree = ast.parse((ROOT / "tests/installation" / filename).read_text(encoding="utf-8"))
        tests[filename] = sum(
            isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name.startswith("test_")
            for node in ast.walk(tree)
        )
    return {
        "periods_s": list(calc.DEFAULT_TP_GRID_S),
        "snap_daf": snap_daf,
        "density_kg_m3": calc.SEAWATER_DENSITY,
        "cases": cases,
        "test_definitions": tests,
        "source_sha256": {
            path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
            for path in (CATALOG, CALCULATIONS, EXEMPLAR, "docs/api/_assets/brand.css",
                         "scripts/reporting/build_installation_module_report.py")
        },
        "changes": CHANGES,
    }


def result_values(data: dict) -> dict[str, float]:
    """Map every displayed worked-result number to its module return value."""
    values = {}
    for index, case in enumerate(data["cases"]):
        result = case["module"]
        for field in ("submerged_weight_kn", "bottom_area_m2", "daf", "v_lowering_ms"):
            values[f"case-{index}.{field}"] = result[field]
        for period, height in result["hs_by_tp"].items():
            values[f"case-{index}.hs.{period}"] = height
    return values


def number(key: str, value: float) -> str:
    """Emit a human-readable value alongside its exact API binding."""
    return f'<span data-result="{escape(key)}" data-value="{value}">{value:.2f}</span>'


def table(headings: tuple[str, ...], rows: list[list[str]]) -> str:
    """Render a real columnar table with caller-escaped cell content."""
    head = ''.join(f'<th scope="col">{h}</th>' for h in headings)
    body = ''.join('<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows)
    return f'<div class="table-scroll"><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def structure_view(case: dict) -> str:
    """Draw the catalog plate, skirts and open-frame envelope in isometric view.

    Sling attachment locations are unavailable. No invented sling geometry is
    drawn. The rigging dimensions are separately echoed with that restriction.
    """
    record = case["input"]
    geom = record["geometry"]
    length, width, height = (geom[k] for k in ("length_m", "width_m", "height_m"))
    scale = 180 / max(length, width)

    def point(x, y, z):
        return (230 + (x - y) * scale * .8, 220 + (x + y) * scale * .28 - z * scale)

    base = [(-length / 2, -width / 2), (length / 2, -width / 2),
            (length / 2, width / 2), (-length / 2, width / 2)]
    points = lambda z: ' '.join(f'{x:.1f},{y:.1f}' for x, y in (point(a, b, z) for a, b in base))
    uprights = ''.join(
        f'<line x1="{point(x,y,0)[0]:.1f}" y1="{point(x,y,0)[1]:.1f}" '
        f'x2="{point(x,y,height)[0]:.1f}" y2="{point(x,y,height)[1]:.1f}"/>'
        for x, y in base
    )
    return f'''<figure><svg viewBox="0 0 460 340" role="img" aria-label="{case['label']} catalog geometry">
<title>{case['label']}: bottom plate, perimeter skirt and open-frame envelope; attachment locations unavailable</title>
<g stroke="var(--brand)" fill="var(--panel-2)" stroke-width="2">
<polygon points="{points(-geom['skirt_depth_m'])}"/><polygon points="{points(0)}"/>
<polygon points="{points(height)}" fill="none"/>{uprights}</g>
<text x="20" y="24" fill="var(--ink)">{case['label']}</text>
<text x="20" y="48" fill="var(--ink)">Length {length:.1f} m; width {width:.1f} m; height {height:.1f} m</text>
<text x="20" y="295" fill="var(--ink)">Plate {geom['bottom_plate_thickness_mm']:.1f} mm; skirt {geom['skirt_depth_m']:.1f} m</text>
<text x="20" y="320" fill="var(--ink)">Envelope geometry only; structural members unavailable</text>
</svg><figcaption>Catalog geometry, drawn to a common projection. Member sizes and fabrication drawings are unavailable. <a href="#catalog-source">Catalog basis</a>.</figcaption></figure>'''


def render(snap_daf: float = calc.SPLASH_SNAP_DAF) -> str:
    """Write the complete analysis around calculated results and cited records."""
    data = calculate(snap_daf)
    periods = data["periods_s"]
    cases = data["cases"]
    all_heights = [h for case in cases for h in case["module"]["hs_by_tp"].values()]
    minimum, maximum = min(all_heights), max(all_heights)
    bindings = result_values(data)
    result_rows = []
    echo_rows = []
    input_rows = []
    geometry = ''.join(structure_view(case) for case in cases)
    for index, case in enumerate(cases):
        result = case["module"]
        record = case["input"]
        for period in periods:
            key = f"{period:.1f}"
            result_rows.append([
                case["label"], f'{period:.1f}',
                number(f"case-{index}.hs.{key}", result["hs_by_tp"][key]),
                {"slam": "Slamming", "drag": "Drag", "inertia": "Inertia"}[result["gov_by_tp"][key]],
                '<a href="#calculation-source">Calculation basis</a>',
            ])
        for field, title, unit in (
            ("submerged_weight_kn", "Submerged weight", "kN"),
            ("bottom_area_m2", "Bottom area", "m²"),
            ("daf", "Reserve divisor", "dimensionless"),
            ("v_lowering_ms", "Lowering speed", "m/s"),
        ):
            echo_rows.append([case["label"], title, number(f"case-{index}.{field}", result[field]), unit,
                              '<a href="#calculation-source">Calculation basis</a>'])
        hc = record["hydrodynamic_coefficients"]
        mp = record["mass_properties"]
        input_rows.append([
            case["label"], f'{mp["mass_air_te"]:.1f}', f'{mp["displaced_volume_m3"]:.1f}',
            f'{hc["slamming_coefficient_Cs"]:.1f}', f'{hc["drag_coefficient_Cd"]:.1f}',
            f'{hc["added_mass_coefficient_Ca"]:.1f}', '<a href="#catalog-source">Catalog basis; assumed</a>',
        ])
    # Keep the per-period table in the appendix. Body results are API-bound
    # endpoint sentences with the same basis and qualifiers as the summary.
    worked = ''.join(
        f'<p>{case["label"]}: the returned splash-zone limit is '
        + number(f"case-{i}.hs.{periods[0]:.1f}", case["module"]["hs_by_tp"][f"{periods[0]:.1f}"])
        + ' m at the shortest supplied period and '
        + number(f"case-{i}.hs.{periods[-1]:.1f}", case["module"]["hs_by_tp"][f"{periods[-1]:.1f}"])
        + ' m at the longest supplied period. These are foundation-only limits under the inherited catalog assumptions. '
        + '<a href="#calculation-source">Calculation basis</a>.</p>'
        for i, case in enumerate(cases)
    )
    appendix_table = table(("Foundation", "Peak period (s)", "Wave-height limit (m)", "Largest individual force", "Source"), result_rows)
    echo_table = table(("Foundation", "Returned quantity", "Value", "Unit", "Source"), echo_rows)
    input_table = table(("Foundation", "Air mass (t)", "Displacement (m³)", "Slamming coefficient", "Drag coefficient", "Added-mass coefficient", "Source"), input_rows)
    definitions = data["test_definitions"]
    count = len(cases) * len(periods)
    options = ''.join(f'<option value="{i}">{case["label"]}</option>' for i, case in enumerate(cases))
    period_options = ''.join(f'<option value="{p}">{p:.1f} s</option>' for p in periods)
    encoded = json.dumps(data, ensure_ascii=False, separators=(',', ':')).replace('<', '\\u003c')
    assert set(bindings)
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Installation module analysis: foundation splash-zone screening</title>
<style>{house_css()}</style>
<style>
.table-scroll{{overflow-x:auto}} svg{{max-width:100%;height:auto}}
label{{display:block;margin:12px 0}} select,button,textarea,input{{font:inherit;max-width:100%;padding:6px}}
textarea{{display:block;width:100%;min-height:90px}} .chart-pair{{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:16px}}
#comment-list{{white-space:pre-wrap;overflow-wrap:anywhere}} .below{{color:var(--go)}} .exceeds{{color:var(--nogo)}} .unknown{{color:var(--caution)}}
@media print{{.comment-controls{{display:none}} .wrap{{box-shadow:none}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><div class="kicker">Marine installation · module analysis</div>
<h1>Foundation splash-zone screening</h1><p>Draft for maintainer review</p></header>

<section id="executive"><h2>1. Executive Summary</h2>
<p>The installation module is a collection of screening calculations for offshore lifting and pipe laying.
Analysis is performed to determine the foundation splash-zone wave-height limits under the illustrative catalog basis; key results are summarized below.</p>
<ul><li>The foundation screen is available for all catalog sizes.
<ul><li>Calculated wave-height limits span {minimum:.2f} m to {maximum:.2f} m across the supplied period grid, with the catalog inputs retained. <a href="#results">Calculated results</a>.</li>
<li>Slamming is the largest individual hydrodynamic term in this example. The sum of the terms is compared with the reduced submerged weight. <a href="#method">Calculation basis</a>.</li></ul></li>
<li>Installation acceptance is NOT_EVALUATED.
<ul><li>Vessel orientation, crane capacity at radius, sling tensions, station-keeping tolerance and the combined sea-state limit require a qualified vessel basis.</li>
<li>Vessel motions, local forecasts and lowering dynamics remain required before an installation decision is made.</li></ul></li></ul>
<h3>Revision history</h3>
{table(("Revision", "Date", "Change and cause"), [
    ["0", "2026-10-21", "Initial analysis; the public foundation example and missing installation acceptance basis are documented."],
    [CHANGES["revision"], CHANGES["date"], "Regeneration inputs and outputs are registered; zero-limit chart ranges are corrected to retain finite coordinates."],
])}
<figure><div id="overview-chart"></div><figcaption>Foundation-only splash-zone limits against peak period. The plotted limit is the module's rounded result; connecting lines guide the eye between calculated period points. A vessel operating limit is NOT_EVALUATED.</figcaption></figure>
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>The foundation splash-zone calculation is examined as the main workflow in this analysis. The public installation example is reproduced through the existing calculation interface, without a dynamic installation simulation.</p>
<p>The module also contains a rigid-jumper screening workflow and scalar pipe-laying checks. Their functions, input contracts and test evidence are identified below; numerical reproduction is limited to the foundation workflow. <a href="#module-source">Module scope</a>.</p>
</section>

<section id="summary"><h2>3. Summary</h2>
<p>The installation controls are separated into calculated foundation demands and required vessel information. A combined envelope is not assigned while any governing system remains unevaluated.</p>
<p>Vessel motions, crane capacity, rigging capacity, station keeping and deeper lowering are NOT_EVALUATED. The foundation curve is therefore retained as a screening result only; no heading-specific operating envelope is issued.</p>
<p>Live forecast support is offered after the vessel-specific operating threshold is established. The trigger wave height and a two-minute forecast acceptance basis remain NOT_EVALUATED. No forecast data is supplied. <a href="#forecast-source">Forecast scope</a>.</p>
<div class="chart-pair"><figure><div id="combined-chart"></div><figcaption>Combined installation envelope: NOT_EVALUATED. The foundation-only calculated limit is shown for context; every other system requires input.</figcaption></figure>
<figure><div id="component-chart"></div><figcaption>Hydrodynamic components and their total against the available force reserve. Individual components are not independently accepted systems.</figcaption></figure></div>
<p class="below">Green means that the calculated total is below the illustrative force limit.</p>
<p class="exceeds">Red means that the calculated total exceeds the illustrative force limit.</p>
<p class="unknown">Amber means that installation acceptance is NOT_EVALUATED.</p>
<label>Foundation <select id="case-select">{options}</select></label>
<label>Peak period <select id="period-select">{period_options}</select></label>
<p id="selected-case" aria-live="polite"></p>
<p>A stricter lower tension reserve is represented by a larger reserve divisor in the calculation input. The corresponding dimensionless reserve is one minus the inverse divisor. This is a static force reserve, not a prediction of minimum dynamic sling tension.</p>
</section>

<section id="design"><h2>4. Design Data</h2>
<p>The physical basis is taken from the public foundation catalog. Every inherited engineering input is marked ASSUMED - to be confirmed; a sourced input is not treated as a qualified installation value.</p>
<h3>4.1 Vessel, crane and rigging</h3>
<p>Vessel particulars, crane radius and capacity, hoist stiffness, reeving, lift-point locations and allowable sling capacities are required inputs. Vessel orientation and station-keeping tolerance are NOT_EVALUATED.</p>
<p>The catalog describes sling counts, nominal sling lengths, nominal angles and hook elevation. Attachment and spreader coordinates are absent, so a dimensionally consistent rigging arrangement and a checked three-dimensional rigging view are NOT_EVALUATED. No sling-load distribution is calculated. <a href="#catalog-source">Catalog basis</a>.</p>
<h3>4.2 Structures</h3>
<p>The open-frame foundation envelope, bottom plate and skirt are shown for each catalog size. These drawings represent supplied envelope dimensions; member layouts and approved fabrication drawings are unavailable.</p>
{geometry}
<h3>4.3 Environment, stages and criteria</h3>
<p>Peak periods from {min(periods):.1f} s to {max(periods):.1f} s are supplied by the calculation's default grid. Water depth, directional wave scatter, wave steepness checks, currents and vessel response amplitude operators are not supplied; a response amplitude operator is the vessel response per unit incident-wave amplitude. No regional wave climate is inferred. <a href="#calculation-source">Calculation basis</a>.</p>
<p>Lift-off and in-air handling precede splash-zone crossing; submerged lowering and landing follow it. Only the splash-zone force balance is calculated here. Static submerged weight is reduced by the inherited reserve divisor before comparison with the sum of slamming, drag and inertia.</p>
<p>The criterion is an illustrative module force balance. The reserve divisor is a caller input, not a substantiated dynamic amplification or minimum sling-tension criterion. Standard-attributed coefficients are inherited from the public catalog as assumptions; the licensed clauses have not been checked. <a href="#standard-source">Standards basis</a>.</p>
</section>

<section id="method"><h2>5. Methodology</h2>
<p>The calculation is performed for every foundation and every supplied peak period. The surface wave velocity and acceleration are inserted into a quadratic force balance, from which the nonnegative significant wave-height root is returned.</p>
<h3>5.1 Theory and equations</h3>
<p>Hₛ is significant wave height in m, Tₚ is peak period in s, vₗ is lowering speed in m/s, ρ is water density in kg/m³, A is bottom projected area in m², V is displaced volume in m³ and W is submerged weight in N. Cₛ, Cᵈ and Cₐ are dimensionless slamming, drag and added-mass coefficients; D is the dimensionless reserve divisor.</p>
<div class="eq">v = πHₛ/Tₚ + vₗ; a = 2π²Hₛ/Tₚ²<br>
Fₛ = ρCₛAv²/2; Fᵈ = ρCᵈAv²/2; Fᵢ = ρ(1 + Cₐ)Va<br>
Fₛ + Fᵈ + Fᵢ ≤ W/D</div>
<p>Fₛ, Fᵈ and Fᵢ are upward slamming, drag and inertia forces in N. The integer coefficients arise from the linear-wave harmonic relations and the force definitions implemented in the cited calculation. Hₛ is used as twice the representative wave amplitude; this substitution does not describe an extreme irregular-wave crest distribution. <a href="#calculation-source">Implemented equations</a>.</p>
<figure><svg viewBox="0 0 720 340" role="img" aria-label="Foundation splash-zone force model and excluded boundary conditions">
<title>Foundation splash-zone force model: upward hydrodynamic demand, downward submerged weight and prescribed lowering speed</title>
<defs><marker id="force-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 L10 5 L0 10 Z" fill="var(--ink)"/></marker></defs>
<path d="M30 145 Q65 125 100 145 T170 145 T240 145 T310 145 T380 145 T450 145 T520 145 T590 145 T660 145" fill="none" stroke="var(--brand)" stroke-width="2"/>
<path d="M280 125 L440 125 L440 150 L280 150 Z M280 150 V178 M440 150 V178" fill="var(--panel-2)" stroke="var(--ink)" stroke-width="3"/>
<path d="M330 240 V165 M390 180 V260 M245 55 V110" fill="none" stroke="var(--ink)" stroke-width="2" marker-end="url(#force-arrow)"/>
<g fill="var(--ink)"><text x="270" y="110">Foundation bottom plate</text><text x="40" y="185">Representative water surface</text>
<text x="170" y="45">Prescribed speed vₗ</text><text x="145" y="240">Fₛ + Fᵈ + Fᵢ upward</text>
<text x="410" y="265">Submerged weight W</text><text x="40" y="305">Vessel-hook motion and line elasticity are excluded; immersion geometry is schematic.</text></g>
</svg><figcaption>Analytical splash-zone force model. Forces are evaluated from the catalog bottom area and displacement; changing buoyancy during immersion is excluded. <a href="#calculation-source">Calculation basis</a>.</figcaption></figure>
<div class="eq">K = ρ(Cₛ + Cᵈ)A/2; J = ρ(1 + Cₐ)V<br>
α = K(π/Tₚ)²; β = 2K(π/Tₚ)vₗ + 2Jπ²/Tₚ²; γ = Kvₗ² − W/D<br>
Hₛ = max[0, (−β + √(β² − 4αγ))/(2α)]</div>
<p>K and J collect the quadratic-drag and inertia coefficients; α, β and γ are the quadratic coefficients. A zero result also occurs when the implemented quadratic or discriminant is invalid. Zero therefore requires input review before it is interpreted as a calm-water operating limit.</p>
<h3>5.2 Public interfaces and outputs</h3>
<p>The foundation function accepts a structure record, a period list, a lowering speed and a reserve divisor. It returns the limiting wave height and governing individual force term at each period, plus submerged weight, bottom area, speed and divisor. Returned wave heights are rounded to hundredths of a metre.</p>
<p>The set interface selects lift records with hydrodynamic data. When a vessel heavy-lift envelope is supplied, the smaller vessel or foundation limit is returned at each period; missing vessel points remain unavailable. The full configuration interface resolves catalogs and a vessel basis, calculates lifts and envelopes, and writes HTML and structured results. No missing-vessel substitute is introduced here. <a href="#api-source">Interface definitions</a>.</p>
</section>

<section id="results"><h2>6. Detailed Results</h2>
<p>The returned foundation limits are summarized below in catalog order. The corresponding periods, units, full numerical coverage and governing terms are given in the appendix result table.</p>
{worked}
<p>Slamming governs the individual-force ranking for each foundation at each period. Acceptance of the complete installation remains NOT_EVALUATED.</p>
</section>

<section id="validation"><h2>7. Validation</h2>
<p>Existing numerical evidence is distinguished from physical validation. The public example is supported by regression assertions and analytical consistency checks; field correlation is not supplied.</p>
<p>The foundation example contains {len(cases)} structures and {count} foundation-period results. Existing splash-zone tests check a small-foundation numerical reference, monotonic period response, weight-to-area sensitivity, combination with the supplied vessel envelope and omission of records lacking hydrodynamic data. Envelope tests also retain unavailable vessel points. <a href="#test-source">Existing test evidence</a>.</p>
<p>The existing pamphlet test file contains {definitions['test_installation_pamphlet.py']} test definitions. Adjacent pipe-laying integrity and stress files contain {definitions['test_pipelay_integrity.py']} and {definitions['test_pipelay_stress.py']} definitions respectively. These are source-inventory counts, not a count of passed engineering cases. The integrity file includes a strict expected failure for the unresolved weld-repair interpretation. <a href="#test-source">Test inventory</a>.</p>
<p>The accompanying rebuild check compares every displayed result with a fresh module call and brackets the force-balance root within the module's rounding interval. Force contributions in the chart are independently reconstructed from the documented equations; they are not additional return fields of the module.</p>
<p>Validation against field measurements is planned using synchronized incident waves, vessel and hook motions, line tension, lowering speed and structure immersion. Acceptance tolerances and a representative validation case remain required before installation use. No field-calibrated accuracy is claimed.</p>
</section>

<section id="findings"><h2>8. Key Findings, Limitations and Recommendations</h2>
<p>The calculated foundation-only limits are available for comparative screening under the inherited assumptions. Complete installation acceptance remains dependent on vessel motions, rigging behaviour and site conditions.</p>
<h3>8.1 Limits and operating recommendations</h3>
<p>For a red force-balance result, reduce incident wave height or change lowering speed and recalculate before proceeding with the lift assessment. A reduction in lowering speed reduces the quadratic hydrodynamic demand in the implemented equation; the speed change also changes exposure duration, which is not modelled. <a href="#calculation-source">Force-balance basis</a>.</p>
<p>Before an operating limit is issued, replace the illustrative hydrodynamic inputs with a reviewed structure basis, establish crane capacity at the working radius, resolve sling geometry and allowable loads, supply directional vessel responses and assess the complete lowering sequence. A screen limit that exceeds the qualified environmental or vessel-motion coverage is referred for engineering review and is not extrapolated. <a href="#api-source">Envelope boundary</a>.</p>
<p>The scalar force balance excludes vessel-hook motion, irregular-wave extremes, buoyancy variation during immersion, sling elasticity, line resonance, current-induced offset and landing interference. No time integration, mesh or time-step convergence is performed. A larger static reserve does not establish a minimum dynamic line tension.</p>
<h3>8.2 Not covered</h3>
<p>The rigid-jumper workflow screens lift-off, in-air bending, splash crossing, lowering and tie-in, then exports case and summary records. Its configured empirical inputs require separate qualification. The pipe-laying package provides scalar buoyancy, tension, route-radius, coating-strain, weld-repair and signed longitudinal-stress checks; combined pressure effects and full contact dynamics are outside those checks. Neither workflow is numerically assessed in this foundation example. <a href="#module-source">Other module areas</a>.</p>
<p>The wider installation module's generic weather and risk-reference features are not adopted as site forecasts or measured failure probabilities. A sample forecast is not substituted for missing live observations.</p>
</section>

<section id="references"><h2>9. References</h2>
<p>The source basis is identified below. Numerical assumptions are inherited from public records rather than taken from a licensed publication.</p>
<h3>9.1 Source qualifications</h3>
<p id="standard-source">DNV-RP-H103, Modelling and Analysis of Marine Operations, 2011, “Lifting through wave zone” and “snap forces”, and DNV-RP-C205, Environmental Conditions and Environmental Loads, 2010, are cited by the implementation and catalog. The catalog also names DNV-ST-N001, Marine Operations and Marine Warranty, 2021. These are unverified implementation attributions; clause text and coefficient applicability have not been checked. No standards compliance is claimed.</p>
<p>The foundation catalog is a public illustrative example, version 1.0. Its geometry, mass properties and coefficients are ASSUMED - to be confirmed. The regression values establish continuity of arithmetic rather than independent published benchmark or measured-installation agreement.</p>
<h3>9.2 Source locators and reproduction</h3>
<ul class="refs">
<li id="catalog-source">Public example: <code>{CATALOG}</code>, version 1.0, geometry, mass properties, hydrodynamic coefficients, projected areas and rigging for every structure. <code>load_mudmats</code> is used without editing its numerical inputs.</li>
<li id="calculation-source">Equations and numerical conventions: <code>{CALCULATIONS}</code>, <code>compute_splashzone_envelope</code>, <code>DEFAULT_TP_GRID_S</code>, <code>SEAWATER_DENSITY</code>, <code>SPLASH_V_LOWERING_MS</code> and <code>SPLASH_SNAP_DAF</code>. The displayed limits are obtained by calling this function.</li>
<li id="api-source">Composition and configuration: <code>compute_splashzone_set</code>, <code>compute_envelopes</code> and <code>assemble_result</code> in the same calculation file; <code>src/digitalmodel/installation/installation_pamphlet/workflow.py</code>, <code>router</code>; public entry-point declaration in the package initializer.</li>
<li id="test-source">Existing evidence: <code>tests/installation/test_installation_pamphlet.py</code>, splash-zone tests; <code>tests/installation/test_pipelay_integrity.py</code> and <code>tests/installation/test_pipelay_stress.py</code>, elementary hand-calculation tests and the unresolved weld-repair diagnostic.</li>
<li id="module-source">Other interfaces: <code>src/digitalmodel/installation/jumper_installation/__init__.py</code>, <code>calculations.py</code>, <code>workflow.py</code>; <code>src/digitalmodel/installation/pipelay/__init__.py</code>, <code>integrity.py</code>, <code>stress.py</code>. The public routers and scalar exports define their respective boundaries.</li>
<li id="forecast-source">Forecast scope: <code>docs/domains/installation/short-horizon-installation-decision-support.html</code>, short-horizon support and missing-evidence boundary. No forecast payload is consumed by this analysis.</li>
<li id="style-source">House-style exemplar: <code>{EXEMPLAR}</code>; brand stylesheet: <code>docs/api/_assets/brand.css</code>. Their styles are inlined, and the document body uses <code>hs-doc</code>.</li>
<li>Reproduction: <code>PYTHONPATH=src uv run --no-project python scripts/reporting/build_installation_module_report.py</code>. An optional <code>--snap-daf</code> argument selects a reserve at least as strict as the module default; its engineering basis requires review. An optional <code>--output-dir</code> redirects the deliverables. Rebuild test: <code>tests/installation/test_installation_module_report.py</code>.</li>
</ul>
<p>The same public example is calculated directly with this interface:</p>
<pre><code>from pathlib import Path
from digitalmodel.installation.installation_pamphlet import calculations as calc

structures = calc.load_mudmats(Path("{CATALOG}"))
results = [
    calc.compute_splashzone_envelope(structure, calc.DEFAULT_TP_GRID_S)
    for structure in structures.values()
]</code></pre>
</section>

<section id="appendix"><h2>10. Appendices</h2>
<p>The input register, numerical coverage and quality evidence are retained here. Unknown system criteria remain separate from the computed foundation screen.</p>
<h3>10.1 Data register, coverage and per-case results</h3>
<p>Every inherited engineering input below is ASSUMED - to be confirmed. Water density is {calc.SEAWATER_DENSITY:.1f} kg/m³; the lowering speed is {calc.SPLASH_V_LOWERING_MS:.2f} m/s and the selected reserve divisor is {snap_daf:.2f}. The default divisor comes from the calculation source; any explicit change is a supplied scenario input. <a href="#calculation-source">Calculation constants</a>.</p>
{input_table}
<p>Submerged weights, bottom areas, selected divisor, speed and foundation-period results are echoed below. The total coverage is {count} foundation-period cells, with no heading dimension. No acceptance of a vessel or rigging system is implied.</p>
{appendix_table}
{echo_table}
{table(("Required evidence", "State", "Engineering disposition", "Source"), [
    ["Directional vessel responses and working crane curve", "NOT_EVALUATED", "Supply qualified vessel and lifting basis before combining limits", "Vessel input required"],
    ["Rigging attachment geometry, spreader layout and capacities", "NOT_EVALUATED", "Resolve geometry and sling tensions before producing a checked rigging view", "Catalog geometry incomplete"],
    ["Field measurements and acceptance tolerance", "NOT_EVALUATED", "Define a comparison basis before physical validation", "Measurement input required"],
    ["Live forecast and support trigger", "NOT_EVALUATED", "Supply forecast and qualified operating envelope", "Forecast input required"],
    ["Time-step sensitivity and extreme-only dynamic failures", "NOT_EVALUATED", "Assess with the complete dynamic installation model", "Algebraic screen only"],
])}
<p>The catalog rigging lengths, nominal angles and hook elevations do not define attachment coordinates or an approved spreader arrangement. Attachment geometry and fabrication drawings remain required for a checked rigging illustration.</p>
<h3>10.2 Quality evidence, changes and comments</h3>
<p>Calculation reproduction is checked by direct function comparison, an independent quadratic-root bracket and exact HTML regeneration. Every catalog size and supplied period is included. Input and result data are embedded for offline inspection; no licensed analysis application is accessed.</p>
<p>The physical drawings are limited to catalog envelope geometry. Fabrication-drawing comparison, interactive visual inspection and comment-file saving in a browser remain pending. The report is a draft until these checks and the engineering basis review are completed.</p>
<details><summary>Engineering changes since the initial issue</summary><div id="change-panel"></div>
<p>The accompanying changes.json contains the same engineering change descriptions for this revision.</p></details>
<details class="comment-controls"><summary>Section comments</summary>
<p>Comments are held in memory until saved. Choose the folder containing this HTML to save report.comments.json beside it. Where folder access is unavailable, download the comments and place that file beside the HTML. Only comments matching this revision and calculation basis can be loaded. Regeneration leaves earlier comment files on disk; retain them separately for reference.</p>
<label>Section <select id="comment-section"></select></label>
<label>Comment <textarea id="comment-text" maxlength="10000"></textarea></label>
<button type="button" id="comment-add">Add comment</button>
<button type="button" id="comment-folder">Save beside report</button>
<button type="button" id="comment-download">Download comments</button>
<label>Load saved comments <input type="file" id="comment-load" accept=".json"></label>
<p id="comment-status" role="status"></p><pre id="comment-list"></pre>
</details>
</section>
<script id="report-data" type="application/json">{encoded}</script>
<script>{INTERACTIVE}</script>
</main></body></html>
'''


INTERACTIVE = r'''
(() => {
  'use strict';
  const data = JSON.parse(document.getElementById('report-data').textContent);
  const el = id => document.getElementById(id);
  const componentName = key => ({slam:'Slamming',drag:'Drag',inertia:'Inertia'}[key]);
  const ns = 'http:' + '//www.w3.org/2000/svg';
  const create = (tag, attrs = {}, text = '') => {
    const node = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([key, value]) => node.setAttribute(key, value));
    node.textContent = text; return node;
  };
  function frame(id, title, xLabel, yLabel) {
    const svg = create('svg', {viewBox:'0 0 520 300', role:'img', 'aria-label':title});
    svg.append(create('title', {}, title));
    svg.append(create('path', {d:'M60 30 V250 H500', fill:'none', stroke:'var(--ink)'}));
    svg.append(create('text', {x:200, y:292, fill:'var(--ink)'}, xLabel));
    svg.append(create('text', {x:5, y:18, fill:'var(--ink)'}, yLabel));
    el(id).replaceChildren(svg); return svg;
  }
  const forces = (record, tp, hs) => {
    const hc=record.input.hydrodynamic_coefficients, mp=record.input.mass_properties;
    const area=record.module.bottom_area_m2, rho=data.density_kg_m3;
    const v=Math.PI*hs/tp+record.module.v_lowering_ms;
    return [rho*hc.slamming_coefficient_Cs*area*v*v/2/1000,
      rho*hc.drag_coefficient_Cd*area*v*v/2/1000,
      rho*(1+hc.added_mass_coefficient_Ca)*mp.displaced_volume_m3*2*Math.PI**2*hs/tp**2/1000];
  };
  function envelope(id, selectedOnly) {
    const svg=frame(id, 'Foundation-only splash-zone limit; vessel acceptance unevaluated', 'Peak period (s)', 'Screen limit Hs (m)');
    const p=data.periods_s, ymin=0;
    const maximum=Math.max(...data.cases.flatMap(c=>Object.values(c.module.hs_by_tp)));
    const ymax=maximum===0?1:maximum*1.15;
    const x=t=>60+(t-p[0])/(p[p.length-1]-p[0])*430;
    const y=h=>250-(h-ymin)/ymax*210;
    const cases=selectedOnly?[data.cases[+el('case-select').value]]:data.cases;
    const colors=['var(--brand)','var(--go)','var(--caution)'];
    p.forEach(t=>svg.append(create('text',{x:x(t)-10,y:273,fill:'var(--ink)'},t.toFixed(1))));
    [0,ymax/2,ymax].forEach(h=>{
      svg.append(create('text',{x:14,y:y(h)+4,fill:'var(--ink)'},h.toFixed(1)));
      svg.append(create('line',{x1:58,x2:500,y1:y(h),y2:y(h),stroke:'var(--grid)'}));
    });
    cases.forEach((c,i)=>{
      const vals=p.map(t=>c.module.hs_by_tp[t.toFixed(1)]);
      svg.append(create('polyline',{points:p.map((t,j)=>`${x(t)},${y(vals[j])}`).join(' '),fill:'none',stroke:colors[i], 'stroke-width':2}));
      p.forEach((t,j)=>{
        const dot=create('circle',{cx:x(t),cy:y(vals[j]),r:5,fill:colors[i],tabindex:0});
        const f=forces(c,t,vals[j]), demand=f.reduce((a,b)=>a+b,0), limit=c.module.submerged_weight_kn/c.module.daf;
        dot.append(create('title',{},`${c.label}; splash crossing; Tp ${t.toFixed(1)} s; calculated Hs limit ${vals[j].toFixed(2)} m; governing individual component ${componentName(c.module.gov_by_tp[t.toFixed(1)])}; total demand ${demand.toFixed(1)} kN; force limit ${limit.toFixed(1)} kN; vessel acceptance NOT_EVALUATED`));
        svg.append(dot);
      });
      svg.append(create('text',{x:65,y:35+i*19,fill:colors[i]},c.label));
    });
  }
  function breakdown() {
    const c=data.cases[+el('case-select').value], tp=+el('period-select').value;
    const root=c.module.hs_by_tp[tp.toFixed(1)], maxHs=Math.ceil(root/.25+1)*.25;
    const limit=c.module.submerged_weight_kn/c.module.daf;
    const samples=Array.from({length:Math.round(maxHs/.25)+1},(_,i)=>i*.25);
    const maxForce=Math.max(limit,...samples.map(h=>forces(c,tp,h).reduce((a,b)=>a+b,0)))*1.15;
    const svg=frame('component-chart','Hydrodynamic demand against the force reserve','Significant wave height (m)','Upward force (kN)');
    const x=h=>60+h/maxHs*430, y=f=>250-f/maxForce*210;
    [0,limit/2,limit].forEach(f=>svg.append(create('text',{x:7,y:y(f)+4,fill:'var(--ink)'},f.toFixed(1))));
    svg.append(create('line',{x1:60,x2:490,y1:y(limit),y2:y(limit),stroke:'var(--nogo)','stroke-dasharray':'6 4'}));
    svg.append(create('text',{x:65,y:y(limit)-7,fill:'var(--nogo)'},`Force limit ${limit.toFixed(1)} kN`));
    ['Slamming','Drag','Inertia','Total'].forEach((component,j)=>{
      const color=['var(--brand)','var(--faint)','var(--muted)','var(--ink)'][j];
      const points=samples.map(h=>{const f=forces(c,tp,h);return [h,j===3?f.reduce((a,b)=>a+b,0):f[j]];});
      svg.append(create('polyline',{points:points.map(([h,f])=>`${x(h)},${y(f)}`).join(' '),fill:'none',stroke:color,'stroke-width':j===3?3:1.5}));
      points.forEach(([h,f])=>{
        const dot=create('circle',{cx:x(h),cy:y(f),r:3,fill:j===3?(f>limit?'var(--nogo)':'var(--go)'):color,tabindex:0});
        dot.append(create('title',{},`${c.label}; splash crossing; Tp ${tp.toFixed(1)} s; Hs ${h.toFixed(2)} m; ${component}; demand ${f.toFixed(1)} kN; total force limit ${limit.toFixed(1)} kN; largest individual term ${componentName(c.module.gov_by_tp[tp.toFixed(1)])}; other components ${['slam','drag','inertia'].filter(k=>k!==c.module.gov_by_tp[tp.toFixed(1)]).map(componentName).join(' and ')}; installation NOT_EVALUATED`));
        svg.append(dot);
      });
      svg.append(create('text',{x:65+j*105,y:38,fill:color},component));
    });
    [0,maxHs/2,maxHs].forEach(h=>svg.append(create('text',{x:x(h)-10,y:273,fill:'var(--ink)'},h.toFixed(2))));
    el('selected-case').textContent=`${c.label}; peak period ${tp.toFixed(1)} s; returned foundation limit ${root.toFixed(2)} m; installation NOT_EVALUATED. Force samples are spaced at 0.25 m.`;
  }
  function redraw(){envelope('overview-chart',false);envelope('combined-chart',true);breakdown();}
  el('case-select').addEventListener('change',redraw);
  el('period-select').addEventListener('change',redraw);
  redraw();
  data.changes.changes.forEach(change=>{const p=document.createElement('p');p.textContent=change;el('change-panel').append(p);});
  const sections=[...document.querySelectorAll('main > section')];
  sections.forEach(section=>{const option=document.createElement('option');option.value=section.id;option.textContent=section.querySelector('h2').textContent;el('comment-section').append(option);});
  let comments=[];
  const identity=JSON.stringify(data.source_sha256)+JSON.stringify(data.cases.map(c=>c.module));
  const show=()=>{el('comment-list').textContent=comments.map(c=>`${c.section}: ${c.text}`).join('\n\n');};
  const payload=()=>JSON.stringify({report:'installation-module-analysis',revision:data.changes.revision,identity,comments},null,2)+'\n';
  el('comment-add').onclick=()=>{
    const text=el('comment-text').value.trim();
    if(!text)return;
    comments.push({section:el('comment-section').value,text});
    el('comment-text').value='';show();el('comment-status').textContent='Comment added in memory. Save to retain it.';
  };
  el('comment-load').onchange=async()=>{
    try{
      const file=el('comment-load').files[0];if(!file)return;
      if(file.size>5000000)throw new Error('Comment file exceeds the supported size.');
      const incoming=JSON.parse(await file.text());
      if(incoming.report!=='installation-module-analysis'||incoming.revision!==data.changes.revision||incoming.identity!==identity||!Array.isArray(incoming.comments)||incoming.comments.some(c=>typeof c.text!=='string'||!sections.some(s=>s.id===c.section)))throw new Error('Comments do not match this calculation basis and revision.');
      comments.push(...incoming.comments);show();el('comment-status').textContent='Matching comments loaded; existing in-memory comments retained.';
    }catch(error){el('comment-status').textContent=error.message;}
  };
  el('comment-folder').onclick=async()=>{
    try{
      if(!window.showDirectoryPicker)throw new Error('Folder access is unavailable. Use Download comments and place the file beside this HTML.');
      const folder=await window.showDirectoryPicker({mode:'readwrite'});
      // Avoid overwriting prior comments: load and merge them first.
      let exists=false;
      try{await folder.getFileHandle('report.comments.json');exists=true;}catch(error){if(error.name!=='NotFoundError')throw error;}
      if(exists)throw new Error('A comments file already exists. Download a new copy and retain or merge the earlier file.');
      const handle=await folder.getFileHandle('report.comments.json',{create:true});
      const content=payload(), writable=await handle.createWritable();
      await writable.write(content);await writable.close();
      if(await(await handle.getFile()).text()!==content)throw new Error('Saved comments differ on read-back.');
      el('comment-status').textContent='Comments saved beside the report and read back.';
    }catch(error){el('comment-status').textContent=error.message;}
  };
  el('comment-download').onclick=()=>{
    const url=URL.createObjectURL(new Blob([payload()],{type:'application/json'}));
    const link=document.createElement('a');link.href=url;link.download='report.comments.json';link.click();
    setTimeout(()=>URL.revokeObjectURL(url),1000);
    el('comment-status').textContent='Download requested. Place the saved file beside the HTML and reload it to check.';
  };
})();
'''


def build(output_dir: Path = OUTPUT, snap_daf: float = calc.SPLASH_SNAP_DAF) -> None:
    """Write deliverables only; existing comment sidecars are never rewritten."""
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "index.html").write_text(render(snap_daf), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


def main() -> None:
    """Parse explicit output and reserve arguments without accessing a solver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=OUTPUT)
    parser.add_argument("--snap-daf", type=float, default=calc.SPLASH_SNAP_DAF)
    args = parser.parse_args()
    build(args.output_dir, args.snap_daf)


if __name__ == "__main__":
    main()
