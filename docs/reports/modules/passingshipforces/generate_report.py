# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the offline report from the documented public workflow example.

Run with PYTHONPATH=src and dependencies numpy, scipy and PyYAML available.
Pass --work-dir outside the checkout for the workflow's CSV calculations.
"""

from __future__ import annotations

import argparse
import copy
import html
import json
import math
import re
from pathlib import Path

import yaml

from digitalmodel.hydrodynamics.wang_passing_ship import (
    DEFAULT_N_IMAGES,
    DEFAULT_QUAD_NODES,
)
from digitalmodel.passing_ship_forces import router

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = ROOT / "docs/domains/hydrodynamics/wang-passing-ship-forces.md"
STYLE_SOURCE = ROOT / "docs/reports/pipeline-ffs-assessment.html"
CHANGES = {
    "revision": "01",
    "date": "2026-10-22",
    "changes": [
        "Reported both signed extrema and rounded chart data for portable rebuilds.",
        "Added independent numerical readback and quantified legacy image truncation.",
        "Clarified axes, source locators, bibliography and portable text encoding.",
    ],
}


def house_style() -> str:
    """Reuse the engineering report layout without a new visual system."""
    return re.search(r"<style>.*?</style>", STYLE_SOURCE.read_text(encoding="utf-8"), re.S).group(0)


def example_config(work_dir: Path) -> dict:
    """Read the public documented example rather than transcribing its values."""
    block = re.search(r"```yaml\n(.*?)```", EXAMPLE.read_text(encoding="utf-8"), re.S).group(1)
    cfg = yaml.safe_load(block)
    cfg["passing_ship_forces"]["output_dir"] = str(work_dir.resolve())
    return cfg


def calculate(work_dir: Path) -> dict:
    """Call the public router for the documented and deep-water cases."""
    cfg = example_config(work_dir / "finite")
    settings = copy.deepcopy(cfg["passing_ship_forces"])
    finite = router(cfg)["passing_ship_forces"]
    deep_cfg = example_config(work_dir / "deep")
    del deep_cfg["passing_ship_forces"]["water_depth"]
    deep = router(deep_cfg)["passing_ship_forces"]
    extrema = worked_numbers(settings, finite)
    probes = [
        ("Surge maximum", "surge_force", extrema["max_surge_force_stagger"]),
        ("Sway abeam", "sway_force", extrema["max_sway_force_stagger"]),
        ("Yaw minimum", "yaw_moment", extrema["min_yaw_moment_stagger"]),
        ("Surge at first stagger", "surge_force", extrema["first_stagger"]),
    ]
    reference_cfg = example_config(work_dir / "image-reference")
    reference_cfg["passing_ship_forces"].update(
        n_images=400, stagger={"values": [probe[2] for probe in probes]}
    )
    reference = router(reference_cfg)["passing_ship_forces"]
    truncation = []
    for (label, component, stagger), row in zip(probes, reference["forces"]):
        legacy = min(finite["forces"], key=lambda r: abs(r["stagger"] - stagger))[component]
        value = row[component]
        truncation.append({"label": label, "component": component, "stagger": stagger,
                           "legacy": legacy, "reference": value,
                           "difference_percent": 100 * (legacy - value) / value})
    return {"settings": settings, "finite": finite, "deep": deep, "truncation": truncation}


def worked_numbers(settings: dict, result: dict) -> dict:
    """Map every displayed example value to its input or returned result."""
    values = {}
    for hull in ("moored_vessel", "passing_vessel"):
        for field in ("length", "midship_area"):
            values[f"{hull}.{field}"] = settings[hull][field]
    for field in ("water_density", "passing_velocity", "separation_distance", "water_depth"):
        values[field] = settings[field]
    values["n_stagger"] = result["n_stagger"]
    values["first_stagger"] = result["forces"][0]["stagger"]
    values["last_stagger"] = result["forces"][-1]["stagger"]
    values["quadrature_nodes"] = DEFAULT_QUAD_NODES
    values["n_images"] = result["n_images"]
    for component in ("surge_force", "sway_force", "yaw_moment"):
        for bound, choose in (("min", min), ("max", max)):
            extreme = choose(row[component] for row in result["forces"])
            tied = [row for row in result["forces"]
                    if math.isclose(row[component], extreme, rel_tol=1e-12, abs_tol=1e-9)]
            row = min(tied, key=lambda r: r["stagger"])
            key = f"{bound}_{component}"
            values[key] = row[component]
            values[f"{key}_stagger"] = 0.0 if abs(row["stagger"]) < 1e-9 else row["stagger"]
            values[f"summary-{key}"] = row[component]
    return values


def number(value: float, key: str = "") -> str:
    """Use engineering precision; snap stagger residuals at display precision."""
    if "stagger" in key and key != "n_stagger":
        value = round(value, 1)
    if value == 0:
        return "0"
    if key in {"water_density", "passing_velocity", "separation_distance", "water_depth"} or "." in key:
        return f"{value:,.4f}".rstrip("0").rstrip(".").replace("-", "−")
    if key in {"n_stagger", "n_images", "quadrature_nodes"}:
        return str(int(value))
    if abs(value) < 0.1:
        return f"{value:.1e}".replace("-", "−")
    return f"{value:,.1f}".replace("-", "−")


def chart_data(model: dict) -> dict:
    """Keep only engineering data, excluding calculation file locations."""
    return {mode: [{field: round(value, 1) for field, value in row.items()}
                   for row in model[mode]["forces"]] for mode in ("finite", "deep")}


def render(model: dict) -> str:
    """Write the complete engineering narrative with computed substitutions."""
    values = worked_numbers(model["settings"], model["finite"])
    def n(key):
        return f'<span data-number="{key}">{number(values[key], key)}</span>'

    fields = [
        ("Moored hull length", "moored_vessel.length", "ft", "Public example"),
        ("Moored midship area", "moored_vessel.midship_area", "ft²", "Public example"),
        ("Passing hull length", "passing_vessel.length", "ft", "Public example"),
        ("Passing midship area", "passing_vessel.midship_area", "ft²", "Public example"),
        ("Water mass density", "water_density", "slug/ft³", "Public example"),
        ("Passing speed", "passing_velocity", "ft/s", "Public example"),
        ("Centreline separation", "separation_distance", "ft", "Public example"),
        ("Water depth", "water_depth", "ft", "Public example"),
        ("First stagger", "first_stagger", "ft", "Returned history"),
        ("Last stagger", "last_stagger", "ft", "Returned history"),
        ("Stagger samples", "n_stagger", "count", "Returned summary"),
        ("Quadrature nodes", "quadrature_nodes", "count", "Calculator default"),
        ("Images on each side", "n_images", "count", "Returned summary"),
    ]
    for component, label, unit in (("surge_force", "surge", "lbf"),
                                   ("sway_force", "sway", "lbf"),
                                   ("yaw_moment", "yaw moment", "ft·lbf")):
        for bound, prefix in (("max", "Maximum"), ("min", "Minimum")):
            key = f"{bound}_{component}"
            fields.extend([(f"{prefix} {label}", key, unit, "Returned history"),
                           (f"{prefix} {label} stagger", f"{key}_stagger", "ft", "Returned history")])
    locators = {"Public example": "source-example", "Returned history": "source-workflow",
                "Returned summary": "source-workflow", "Calculator default": "source-theory"}
    rows = "".join(f'<tr><td>{label}</td><td>{n(key)}</td><td>{unit}</td>'
                   f'<td><a href="#{locators[source]}">{source}</a></td></tr>'
                   for label, key, unit, source in fields)
    truncation_rows = "".join(
        f'<tr><td>{case["label"]}</td><td>{number(case["stagger"], "stagger")}</td>'
        f'<td>{number(case["legacy"])}</td><td>{number(case["reference"])}</td>'
        f'<td>{"ft·lbf" if case["component"] == "yaw_moment" else "lbf"}</td>'
        f'<td>{case["difference_percent"]:+.1f} %</td></tr>' for case in model["truncation"]
    )
    titles = ["Executive Summary", "Introduction", "Summary", "Design Data and Criteria",
              "Methodology", "Worked Example", "Validation Evidence",
              "Key Findings, Limits and Recommendations", "References", "Appendices"]
    ids = ["summary", "introduction", "controls", "basis", "method", "worked-example",
           "validation", "recommendations", "references", "appendices"]
    nav = "".join(f'<li><a href="#{key}">{title}</a></li>' for key, title in zip(ids, titles))
    body = f"""
<section id="summary"><h2>1. Executive Summary</h2>
<p>The passing-ship forces module is a hydrodynamic load calculator for a stationary moored hull beside a parallel moving hull. Analysis is performed to determine surge force, sway force and yaw moment over a longitudinal passage; key results are summarized below.</p>
<ul><li>Load histories are available for the documented public example.
<ul><li>The finite-depth sampled ranges are surge {n('summary-min_surge_force')} to {n('summary-max_surge_force')} lbf, sway {n('summary-min_sway_force')} to {n('summary-max_sway_force')} lbf and yaw {n('summary-min_yaw_moment')} to {n('summary-max_yaw_moment')} ft·lbf. The <a href="#worked-example">worked example</a> supplies the inputs and the peak locations; these are load demands without an acceptance limit.</li>
<li>Positive sway denotes attraction; attraction and repulsion are both retained in the plotted history.</li>
<li>Finite-depth images and the deep-water calculation are presented separately, using the same hulls and speed.</li></ul></li>
<li>An operating decision remains NOT_EVALUATED.
<ul><li>No mooring capacity, allowable motion or port acceptance criterion is supplied. No load acceptance line is assigned.</li>
<li>Use the histories as loading inputs after hull geometry, separation, depth and numerical convergence are reviewed.</li></ul></li></ul>
</section>
<div id="revision-history"><h3>Revision history</h3>
<table><caption>Document revision history</caption><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>00</td><td>2026-10-22</td><td>First issue: public-example load calculation, evidence boundaries and engineering actions are established.</td></tr>
<tr><td>{CHANGES['revision']}</td><td>{CHANGES['date']}</td><td>Both signed extrema, portable chart data and independent numerical readback replace ambiguous peaks; image truncation, axes and source details are clarified.</td></tr></tbody></table></div>
<section id="introduction"><h2>2. Introduction</h2>
<p>The main workflow is assessed from configuration to the returned load history and comma-separated output files. The intended use is preliminary loading for a separate moored-system response assessment, rather than an installation sea-state envelope.</p>
<p>A documented public repository example is reproduced. It is an anonymised numerical demonstration, not a published field trial. The bibliography names the original formulation, but original-paper equations and applicability limits have not been independently read; the equations below are attributed to the repository implementation record.</p>
</section>
<section id="controls"><h2>3. Summary</h2>
<p>The calculation is controlled by a consistent unit system, centreline geometry and a chosen stagger grid. The detailed actions for engineering use are stated here.</p>
<p>Supply immersed midship areas and hull lengths, mass density, relative passing speed, centreline separation and stagger values. Supply depth for the image calculation, or omit depth for the deep-water calculation. Refine stagger spacing before using sampled extrema as design loads.</p>
<p>The returned configuration contains the input particulars, depth mode, sampled signed peaks and their locations, and a row for every stagger containing surge, sway and yaw. The same history and summary are written to separate comma-separated files. An output folder is created when required; existing files with the same names are overwritten by the workflow.</p>
<p>Resolve forces and moments into the mooring model axes. Supply line properties, fenders, damping, pretension and motion criteria before assessing line loads or operating limits. No heading tolerance, wave-height limit or forecast threshold is inferred from this force-only calculation.</p>
</section>
<section id="basis"><h2>4. Design Data and Criteria</h2>
<p>Parabolic immersed sectional-area curves and parallel hull centrelines are used. The public-example values are listed with the computed results in the worked example; the source register distinguishes supplied values from missing design data.</p>
<h3>4.1 Geometry and boundary conditions</h3>
<figure><svg viewBox="0 0 900 360" role="img" aria-label="Plan view of parallel hulls, centreline separation and longitudinal stagger; hull outlines are illustrative">
<defs><marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor"/></marker></defs>
<path d="M 80 240 Q 90 207 125 207 L 370 207 Q 412 207 450 240 Q 412 273 370 273 L 125 273 Q 90 273 80 240 Z" fill="#d3dfe4" stroke="currentColor"/>
<path d="M 300 110 Q 310 77 345 77 L 590 77 Q 632 77 670 110 Q 632 143 590 143 L 345 143 Q 310 143 300 110 Z" fill="#d3dfe4" stroke="currentColor"/>
<g stroke="currentColor" fill="none"><path d="M 65 240 H 730 M 280 110 H 730" stroke-dasharray="6 4"/>
<path d="M 720 240 V 110 M 265 175 H 485 M 690 70 H 800" marker-start="url(#arrow)" marker-end="url(#arrow)"/>
<path d="M 265 230 V 160 M 485 120 V 185" stroke-dasharray="4 4"/></g>
<g fill="currentColor" font-size="17"><text x="130" y="245">Stationary moored hull</text><text x="360" y="115">Passing hull</text>
<text x="315" y="166">Stagger ξ</text><text x="735" y="180">Separation η</text><text x="700" y="54">Speed U; positive x →</text>
<text x="80" y="310">Yaw N acts about the moored midship; positive sway Y acts toward the passing hull.</text>
<text x="80" y="340">Depth h is uniform; no quay wall, bank or mooring restraint is represented.</text></g></svg>
<figcaption>Geometry and boundary-condition schematic. Hull outlines indicate parallel passage only; beam, draft and detailed hull lines are not supplied and the plan view is not a dimensioned hull drawing.</figcaption></figure>
<h3>4.2 Calculation criteria</h3>
<p>Positive separation, positive hull dimensions and positive mass density are mathematical input conditions in the implementation record. They are not a minimum safe clearance. The default outer quadrature uses {DEFAULT_QUAD_NODES} nodes and the finite-depth calculation uses image indices −{DEFAULT_N_IMAGES} through +{DEFAULT_N_IMAGES}, as defined by the calculator defaults. These are numerical settings, not physical applicability limits.</p>
<p>Numerical comparison criteria are taken from the repository tests: the stored worksheet sway spot comparison uses relative tolerance 1e−8; nonzero sweep comparisons use 1e−6; the rounded workbook comparisons use 1e−2. These tolerances measure arithmetic reproduction and do not bound hydrodynamic prediction error. The evidence records and exact locators are listed in the references.</p>
</section>
<section id="method"><h2>5. Methodology</h2>
<p>The implementation is evaluated using closed-form inner integrals and numerical outer integration. The following expressions are transcribed from the calculator theory record, with all coefficients attributed to that record rather than inferred from an unread publication.</p>
<h3>5.1 Hull representation and deep-water loads</h3>
<p>For hull index i, Sᵢ is immersed sectional area, Aᵢ is midship area, Lᵢ is length and xᵢ is a longitudinal coordinate relative to midship. Index 1 denotes the moored hull and index 2 denotes the passing hull. The prime denotes differentiation with respect to xᵢ.</p>
<p>Sᵢ(xᵢ) = Aᵢ(1 − 4xᵢ²/Lᵢ²); S′ᵢ(xᵢ) = −8Aᵢxᵢ/Lᵢ²; −Lᵢ/2 ≤ xᵢ ≤ Lᵢ/2.</p>
<p>Let R = x₂ − x₁ + ξ, with longitudinal stagger ξ and centreline separation η. The kernels are F(x₁) = ∫ S′₂(x₂)R/(η² + R²)³ᐟ² dx₂ and G(x₁) = ∫ S′₂(x₂)/(η² + R²)³ᐟ² dx₂, each integrated along the passing hull.</p>
<p>X = ρU²/(2π) ∫ S′₁F dx₁; Y = ρU²η/π ∫ S′₁G dx₁; N = ρU²η/π ∫ (S′₁x₁ + S₁)G dx₁. The outer integrals span the moored hull. Here ρ is water mass density, U is passing speed, X is surge, Y is sway, N is yaw moment about midship and π is the circle constant. Positive sway denotes attraction. X &gt; 0 acts toward +x. N is the first moment of the attraction-positive lateral load: N &gt; 0 turns the +x end toward the passing hull. No nautical clockwise convention is assigned.</p>
<h3>5.2 Finite depth and numerical integration</h3>
<p>At uniform depth h, image separation is ηₙ = √(η² + 4n²h²). The finite-depth loads are Xₕ = ΣₙX(ξ,ηₙ), Yₕ = ηΣₙY(ξ,ηₙ)/ηₙ and Nₕ = ηΣₙN(ξ,ηₙ)/ηₙ. The sums span the chosen integer image indices. These equations and their coefficients are given in the calculator theory record.</p>
<p>Inner kernels are integrated analytically. Fixed-order Gauss–Legendre quadrature is used for the outer integral. An adaptive Gauss–Lobatto reference path evaluates the same mathematical kernels; agreement between them tests numerical implementation, not independent physical theory.</p>
<p>Consistent foot, square-foot, slug-per-cubic-foot and foot-per-second inputs yield pound-force and foot-pound-force. Consistent metre, square-metre, kilogram-per-cubic-metre and metre-per-second inputs yield newton and newton-metre. Unit conversion is not performed by the workflow.</p>
</section>
<section id="worked-example"><h2>6. Worked Example</h2>
<p>The public documented configuration is read directly and passed to the module entry point. All values below are rendered from the input configuration or returned history and summary; calculation file locations are excluded from this document.</p>
<table><caption>Documented public-example inputs and returned sampled extrema</caption><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead><tbody>{rows}</tbody></table>
<p>The maximum and minimum of each component are taken from the sampled history, independently of the workflow signed absolute-peak summary. For same-sign ties within the relative and absolute tolerances stated in the numerical evidence, the lowest stagger is reported. Staggers and chart loads are rounded to one decimal place; near-zero stagger residuals become zero. Separate components need not reach their extrema at the same stagger. No continuous peak search is performed.</p>
<p>The deep-water comparison is computed with depth omitted and all other physical inputs retained. Each chart shows one load component against stagger. Limit: NOT_EVALUATED. The dashed line denotes zero load only; it is not an acceptance limit.</p>
<div id="charts"></div>
</section>
<section id="validation"><h2>7. Validation Evidence</h2>
<p>Repository evidence is assessed as numerical reproduction, internal consistency and workflow behaviour. Field validation remains NOT_EVALUATED because no measured passage record is present in the assessed evidence.</p>
<p>The stored worksheet case supplies an abeam sway value, a nondimensional deep-water value and finite-depth histories. The rounded workbook table supplies deep-water and finite-depth histories. The corresponding tests compare these stored values against the calculator. Their underlying original calculation documents are not supplied with this report; physical accuracy is not inferred from their agreement.</p>
<p>The numerical tests also compare vectorised and adaptive integration, check the deep-water image limit, speed-squared scaling, stagger symmetry and attraction versus repulsion. Workflow tests compare the returned history with the library call, inspect sampled peaks, read the written files and exercise invalid configuration cases.</p>
<p>The headline tests pin labels, units and both signed extrema to numerical readback and the stored workbook abeam sway oracle (87,640 lbf, rounded-data tolerance 1 %). A direct two-hull raw-kernel quadrature in the report tests independently checks the image-count comparison without importing the calculator antiderivatives. A separate workflow-to-library comparison is an internal consistency check. Same-sign sampled-extremum ties use relative tolerance 1e−12 or absolute tolerance 1e−9 in load units, choosing the lowest stagger.</p>
<p>The rebuild test compares the narrative exactly and parsed chart data within 0.1 in the stated units, covering the rounding interval between platforms. Chart loads and staggers are serialised to one decimal place; this is display precision, not a hydrodynamic error bound. Exact test execution results belong to the accompanying check record; no field qualification follows from a successful rebuild.</p>
<h3>7.1 Image truncation in the worked case</h3>
<p>N = 10 (21 image terms) reproduces the legacy tool. The table compares that truncation with N = 400 (801 image terms) at the same sampled staggers and quadrature order. Difference is 100 × (legacy − reference) / reference; it measures image truncation relative to this longer finite sum, not field prediction error or an infinite-image bound. The legacy image extent 2Nh = 1,100.0 ft is about 1.2 hull lengths, so image count matters even for headline loads.</p>
<table><caption>Image-count sensitivity for the public example</caption><thead><tr><th>Probe</th><th>Stagger (ft)</th><th>21 image terms</th><th>801 image terms</th><th>Unit</th><th>Difference</th></tr></thead><tbody>{truncation_rows}</tbody></table>
<p>This image-count effect is separate from stagger-grid sampling and quadrature error. Extend the image sum and refine the grid for a design assessment; the longer finite sum is a comparison reference only.</p>
</section>
<section id="recommendations"><h2>8. Key Findings, Limits and Recommendations</h2>
<p>Load generation is available for idealised parallel passage, while an operating envelope is not established. The recommendations are tied to the calculation boundary and the evidence gaps.</p>
<p>Complete hull and bathymetry inputs before a design assessment. The parabolic representation omits actual hull lines, free-surface wave effects, viscous separation, bank and quay interactions and transient moored-body feedback. No quantified speed, depth-to-draft or clearance validity range is established by the assessed records. Seek independent hydrodynamic evidence where these effects govern.</p>
<p>Demonstrate quadrature, image-count and stagger-grid convergence for each design case; the existing numerical tests establish agreement only for their stored cases. Use both attractive and repulsive parts of the history and retain the signed yaw moment when coupling to a mooring-response model.</p>
<p>Supply allowable line tension, fender reaction and motion limits before assigning an operating speed or separation. The returned summary label for maximum repulsion is the minimum signed sway; check its sign before interpreting it as a repulsive event. Positive-only input checks do not reject all nonfinite dimensions or densities, so input quality is assessed separately before use.</p>
<h3>8.1 Not covered</h3>
<p>The separate passing-ship package, its configuration interface and its heuristic depth treatment are not assessed here. Mooring dynamics, line loads, fenders, manoeuvring, multiple simultaneous hulls, wave response, crane operations and installation forecasts are not calculated by this workflow.</p>
</section>
<section id="references"><h2>9. References</h2>
<p>The sources are separated into implementation evidence and bibliographic leads. Numeric example values and algorithm settings originate in the public repository records; no publication coefficient is supplied from memory.</p>
<h3>9.1 Evidence and literature</h3>
<p>The public workflow documentation supplies the worked example. The calculator record supplies the equations, default integration settings and sign conventions. Stored numerical oracles and workflow tests supply the reproduction evidence. These are repository records rather than a standards compliance basis.</p>
<p>Dynamic Effects of Ship Passage on Moored Vessels, Journal of the Waterways, Harbors and Coastal Engineering Division, ASCE, Vol. 101, WW3, p. 247, 1975, is cited by the repository as the original slender-body formulation. Prediction of Load on Mooring Ropes of a Container Ship due to the Forces Induced by a Passing Bulk Carrier, MARSIM'03 (2003), equations (2.4)–(2.9), is cited by the repository for the image formulation. Both are bibliographic leads only: original texts were not inspected. Bibliographic details are transcribed from the calculator module docstring; no original-text inspection is claimed.</p>
<h3>9.2 Source locators</h3>
<ul><li id="source-example">Public example: <a href="../../../domains/hydrodynamics/wang-passing-ship-forces.md">workflow documentation</a>, sections “Method, as the legacy code actually computes it”, “Validation lineage” and “Workflow usage”.</li>
<li id="source-theory">Calculator theory and defaults: <a href="../../../../src/digitalmodel/hydrodynamics/wang_passing_ship.py">numerical implementation</a>, module docstring, DEFAULT_QUAD_NODES, DEFAULT_N_IMAGES and component functions.</li>
<li id="source-workflow">Public entry point and output schema: <a href="../../../../src/digitalmodel/passing_ship_forces/workflow.py">workflow implementation</a>. Import router from digitalmodel.passing_ship_forces and call router(cfg), where cfg contains basename: passing_ship_forces and the passing_ship_forces settings mapping shown in the documented example. The supplied dictionary is modified and returned. Set output_dir in that mapping before the call. The forces rows contain stagger, surge_force, sway_force and yaw_moment; results_csv and summary_csv identify the written files.</li>
<li>Stored evidence: <a href="../../../../tests/hydrodynamics/test_wang_passing_ship.py">numerical tests</a>, stored worksheet and workbook cases, numerical consistency and input checks.</li>
<li>Workflow evidence: <a href="../../../../tests/hydrodynamics/test_passing_ship_forces_workflow.py">workflow tests</a>, library comparison, peak summary, output files and configuration checks.</li>
<li>Document checks: <a href="../../../../tests/docs/test_passing_ship_forces_report.py">rebuild and numerical-readback tests</a>.</li>
<li>House layout exemplar: <a href="../../pipeline-ffs-assessment.html">engineering assessment report</a>; its stylesheet is inlined without a replacement palette.</li></ul>
</section>
<section id="appendices"><h2>10. Appendices</h2>
<p>The data register records missing information and the quality checks associated with this issue. No missing capacity is replaced by a guessed number.</p>
<h3>10.1 Data register</h3>
<table><caption>Input evidence and missing design data</caption><thead><tr><th>Item</th><th>Basis</th><th>Disposition</th></tr></thead><tbody>
<tr><td>Example dimensions, density, speed, depth and separation</td><td>Documented public example</td><td>Demonstration only; no site-specific operation is represented.</td></tr>
<tr><td>Parabolic hull representation</td><td>Calculator theory record</td><td>ASSUMED - to be confirmed against immersed area curves before design use.</td></tr>
<tr><td>Uniform depth and parallel passage</td><td>Calculator boundary conditions</td><td>ASSUMED - to be confirmed against bathymetry and the passage track.</td></tr>
<tr><td>Actual beam, draft, hull lines and clearance</td><td>Required input to applicability review</td><td>NOT_EVALUATED. Obtain geometry before setting a passage limit.</td></tr>
<tr><td>Mooring, fender and motion capacity</td><td>Required design input outside this calculator</td><td>NOT_EVALUATED. Supply capacities and perform response analysis.</td></tr>
<tr><td>Field validation</td><td>No measured passage in the assessed records</td><td>NOT_EVALUATED. Obtain a synchronised speed, track, water-level and load or motion record for comparison.</td></tr>
<tr><td>Original-paper applicability limits</td><td>Original texts not inspected</td><td>NOT_EVALUATED. Read an open original source before claiming a published validity envelope.</td></tr>
</tbody></table>
<h3>10.2 Quality pre-flight evidence</h3>
<p>The displayed example values are generated from the public call. The tests check both depth modes, every plotted value, headline values, labels and units, the inlined stylesheet, section links, heading depth and portable regeneration. Independent numerical readback and internal workflow consistency are identified separately; chart comparisons allow the stated rounding interval. The schematic identifies geometry and omitted boundaries; it does not fabricate missing dimensions.</p>
<p>The charts are interactive without a network library. Hover or focus a chart to select a sample; arrow keys change the sample. The tooltip identifies the depth case, component, stagger, signed demand, units and missing acceptance limit. No limit is drawn because none is supplied. The zero reference is explicitly distinguished from an acceptance criterion.</p>
<h3>10.3 Comments and engineering changes</h3>
<p>Comments are retained in this browser when storage is available. Save beside report writes a comments file into a chosen report folder in browsers that support directory access. Otherwise export the comments file and place it beside this report. Import restores a previously saved comments file. Choose the same report folder when reopening on another device; browser storage is not a shared review record.</p>
<details id="changes-panel"><summary>Engineering changes</summary><ul>{''.join('<li>' + html.escape(item) + '</li>' for item in CHANGES['changes'])}</ul></details>
<label for="comment-section">Section</label><select id="comment-section">{''.join('<option value="' + key + '">' + title + '</option>' for key, title in zip(ids, titles))}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<button id="add-comment" type="button">Add comment</button><button id="save-comments" type="button">Save beside report</button><button id="export-comments" type="button">Export comments</button>
<label for="import-comments">Import comments</label><input id="import-comments" type="file" accept="application/json">
<p id="comment-status" role="status"></p><ol id="comment-list"></ol>
</section>"""
    data = json.dumps(chart_data(model), separators=(",", ":"), allow_nan=False)
    return (f'<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">'
            f'<meta name="viewport" content="width=device-width, initial-scale=1">'
            f'<title>Passing-ship forces module analysis</title>{house_style()}</head>\n'
            f'<body class="hs-doc"><header><h1>Passing-ship forces module analysis</h1>'
            f'<p>Draft for engineering review</p></header><div class="layout">'
            f'<nav aria-label="Contents"><ol>{nav}</ol></nav><main>{body}</main></div>'
            f'<script id="chart-data" type="application/json">{data}</script>\n'
            f'<script>{INTERACTION}</script>\n</body></html>\n')


INTERACTION = r"""
(() => {
  const data = JSON.parse(document.getElementById('chart-data').textContent);
  const ns = 'http://www.w3.org/2000/svg';
  const fmt = v => v === 0 ? '0' : (Math.abs(v) < 0.1 ? v.toExponential(1) : v.toLocaleString('en', {minimumFractionDigits:1, maximumFractionDigits:1})).replaceAll('-', '−');
  const el = (tag, attrs, text) => {
    const node = document.createElementNS(ns, tag);
    Object.entries(attrs).forEach(([k,v]) => node.setAttribute(k,v));
    if (text !== undefined) node.textContent = text;
    return node;
  };
  [['surge_force','Surge','lbf'], ['sway_force','Sway','lbf'], ['yaw_moment','Yaw moment','ft·lbf']].forEach(([field,label,units]) => {
    const figure = document.createElement('figure');
    const svg = el('svg', {viewBox:'0 0 800 340', tabindex:'0', role:'img', 'aria-label': label + ' against stagger; arrow keys select a sample'});
    const all = [...data.finite,...data.deep];
    const xmin = Math.min(...all.map(r=>r.stagger)), xmax = Math.max(...all.map(r=>r.stagger));
    const extent = Math.max(...all.map(r=>Math.abs(r[field]))) || 1;
    const x = v=>90+(v-xmin)/(xmax-xmin)*650, y=v=>145-v/extent*105;
    svg.append(el('path',{d:`M 90 30 V 260 H 750 M 90 ${y(0)} H 750`,fill:'none',stroke:'currentColor','stroke-dasharray':'5 4'}));
    [xmin, (xmin+xmax)/2, xmax].forEach(v=>svg.append(el('text',{x:x(v),y:285,'text-anchor':'middle','font-size':14},fmt(v))));
    [-extent,0,extent].forEach(v=>svg.append(el('text',{x:85,y:y(v)+5,'text-anchor':'end','font-size':13},fmt(v))));
    svg.append(el('text',{x:420,y:312,'text-anchor':'middle','font-size':16},'Stagger (ft)'));
    svg.append(el('text',{x:100,y:20,'font-size':16},label+' ('+units+')'));
    [['finite','Finite depth','#137e8a'],['deep','Deep water','#183449']].forEach(([mode,name,color],j)=>{
      svg.append(el('path',{d:data[mode].map((r,i)=>(i?'L':'M')+x(r.stagger)+' '+y(r[field])).join(' '),fill:'none',stroke:color,'stroke-width':2}));
      svg.append(el('text',{x:400+j*160,y:20,fill:color,'font-size':15},name));
    });
    const cursor = el('path',{stroke:'currentColor','stroke-dasharray':'2 3'});
    svg.append(cursor);
    const info = document.createElement('p'); info.setAttribute('aria-live','polite');
    let selected = Math.floor(data.finite.length/2);
    const show = i=>{
      selected = Math.max(0,Math.min(data.finite.length-1,i));
      const r=data.finite[selected]; cursor.setAttribute('d',`M${x(r.stagger)} 30V260`);
      info.textContent = ['finite','deep'].map(mode=>`${mode==='finite'?'Finite depth':'Deep water'}; component: ${label}; stagger: ${fmt(r.stagger)} ft; demand: ${fmt(data[mode][selected][field])} ${units}; Limit: NOT_EVALUATED.`).join(' ');
    };
    svg.addEventListener('pointermove',e=>{
      const p=svg.createSVGPoint();p.x=e.clientX;p.y=e.clientY;
      const px=p.matrixTransform(svg.getScreenCTM().inverse()).x;
      const xi=xmin+(px-90)/650*(xmax-xmin);
      let nearest=0;data.finite.forEach((r,i)=>{if(Math.abs(r.stagger-xi)<Math.abs(data.finite[nearest].stagger-xi))nearest=i;});show(nearest);
    });
    svg.addEventListener('keydown',e=>{if(['ArrowLeft','ArrowRight'].includes(e.key)){e.preventDefault();show(selected+(e.key==='ArrowRight'?1:-1));}});
    const caption=document.createElement('figcaption');caption.textContent=label+' against stagger. Dashed reference: zero load; no acceptance limit supplied.';
    figure.append(svg,caption,info);document.getElementById('charts').append(figure);show(selected);
  });
  const key='passing-ship-forces-comments', status=document.getElementById('comment-status');
  let comments=[];
  const valid = rows=>Array.isArray(rows)&&rows.every(r=>r&&typeof r.section==='string'&&typeof r.text==='string');
  try {const saved=JSON.parse(localStorage.getItem(key)||'[]');if(valid(saved))comments=saved;} catch {status.textContent='Browser storage unavailable. Export comments to retain them.';}
  const display=()=>{
    const list=document.getElementById('comment-list');list.replaceChildren();
    comments.forEach(r=>{const item=document.createElement('li');item.textContent=r.section+': '+r.text;list.append(item);});
  };
  const persist=()=>{try{localStorage.setItem(key,JSON.stringify(comments));status.textContent='Comments retained in this browser.';}catch{status.textContent='Browser storage unavailable. Export comments to retain them.';}display();};
  const content=()=>JSON.stringify(comments,null,2)+'\n';
  document.getElementById('add-comment').onclick=()=>{const input=document.getElementById('comment-text');if(input.value.trim()){comments.push({section:document.getElementById('comment-section').selectedOptions[0].textContent,text:input.value.trim()});input.value='';persist();}};
  document.getElementById('export-comments').onclick=()=>{const url=URL.createObjectURL(new Blob([content()],{type:'application/json'}));const a=document.createElement('a');a.href=url;a.download='report.comments.json';a.click();setTimeout(()=>URL.revokeObjectURL(url),1000);status.textContent='Place the exported comments file beside this report.';};
  document.getElementById('save-comments').onclick=async()=>{if(!window.showDirectoryPicker){status.textContent='Directory access unavailable. Use Export comments.';return;}try{const dir=await window.showDirectoryPicker();const handle=await dir.getFileHandle('report.comments.json',{create:true});const stream=await handle.createWritable();await stream.write(content());await stream.close();status.textContent='Comments saved in the selected folder.';}catch{status.textContent='Comments were not saved. Use Export comments to retain them.';}};
  document.getElementById('import-comments').onchange=async e=>{try{const rows=JSON.parse(await e.target.files[0].text());if(!valid(rows))throw Error();comments=rows;persist();}catch{status.textContent='The selected comments file is not recognised.';}};
  display();
})();
"""


def build(output_dir: Path, work_dir: Path) -> None:
    """Build the issued files, keeping workflow CSVs outside the report folder."""
    if work_dir.resolve().is_relative_to(ROOT):
        raise ValueError("Calculation work directory must be outside the checkout")
    model = calculate(work_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(model), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.work_dir)
