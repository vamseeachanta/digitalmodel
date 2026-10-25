# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the VIV screening report by calling the documented workflow.

Run with PYTHONPATH=src and the calculation dependencies installed. Supply
--scratch outside the checkout; calculation outputs are written there only.
"""

from __future__ import annotations

import argparse
import ast
import csv
import json
import re
from html import escape
from pathlib import Path

import yaml

from digitalmodel.viv_screening.inputs import parse_inputs
from digitalmodel.viv_screening.workflow import router

ROOT = Path(__file__).resolve().parents[2]
REPORT = Path("docs/reports/modules/vivscreening/report.html")
CHANGES = REPORT.with_name("changes.json")
EXAMPLE = Path("examples/workflows/riser-viv-screening/input.yml")
EXEMPLAR = Path("docs/api/cfd/turbulent-flat-plate-verification.html")


def changes() -> dict:
    """Describe the initial engineering content of this report."""
    return {
        "revision": "0",
        "date": "2026-10-21",
        "changes": [
            "Uniform-span current screening and the synthetic fatigue example are documented.",
            "In-line fatigue and physical qualification of response coefficients are identified as not evaluated.",
            "Calculation anchors, source coverage and the input data register are included.",
        ],
    }


def _bands() -> dict:
    """Read implemented screening bands without recopying their coefficients."""
    source = (ROOT / "src/digitalmodel/viv_screening/workflow.py").read_text(encoding="utf-8")
    tree = ast.parse(source)
    function = next(n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == "_mode_rows")
    bands = {}
    for node in ast.walk(function):
        if isinstance(node, ast.Assign) and isinstance(node.value, ast.Call):
            call = node.value
            if isinstance(call.func, ast.Name) and call.func.id == "_intersection":
                bands[node.targets[0].id] = [ast.literal_eval(arg) for arg in call.args[2:4]]
    if set(bands) != {"cf", "il"}:
        raise ValueError("Screening-band extraction changed; review the report basis")
    return bands


def calculate(scratch: Path) -> dict:
    """Call the public workflow and read its emitted per-mode result table."""
    cfg = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    original, beam, viv = parse_inputs(cfg)
    values = {}
    inputs = []

    def collect(key, value, unit, label):
        values[key] = value if isinstance(value, int) else float(value)
        inputs.append((key, label, unit))

    for key, label, unit in [
        ("length_m", "Span length", "m"),
        ("outer_diameter_m", "Outer and hydrodynamic diameter", "m"),
        ("inner_diameter_m", "Inner diameter", "m"),
        ("youngs_modulus_Pa", "Young’s modulus", "Pa"),
        ("mass_per_length_kg_m", "Mass per length including added mass", "kg/m"),
        ("effective_tension_N", "Uniform effective tension", "N"),
    ]:
        collect(key, original["structure"][key], unit, label)
    for key, label, unit in [
        ("kinematic_viscosity_m2_s", "Kinematic viscosity", "m²/s"),
        ("mass_ratio", "Effective mass divided by displaced-fluid mass", "dimensionless"),
        ("damping_ratio", "Structural damping ratio", "dimensionless"),
        ("exposure_fraction", "Fraction of each year exposed to this current", "dimensionless"),
    ]:
        collect(key, original["environment"][key], unit, label)
    collect("speed_m_s", original["environment"]["current"]["speed_m_s"], "m/s", "Uniform current speed")
    for key, label, unit in [
        ("design_life_years", "Design duration", "years"),
        ("dff", "Design fatigue factor", "dimensionless"),
        ("n_modes", "Modes requested", "count"),
        ("unit_modal_stress_MPa", "Stress amplitude at unit displacement-to-diameter ratio", "MPa"),
    ]:
        collect(key, original[key], unit, label)

    output = scratch.resolve() / "example"
    if output.is_relative_to(ROOT):
        raise ValueError("Calculation scratch must be outside the checkout")
    cfg["viv_screening"]["output_dir"] = str(output)
    result = router(cfg)["viv_screening"]
    with Path(result["results_csv"]).open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    values["inertia"] = beam.moment_of_inertia
    values["reynolds"] = viv.reynolds_number
    for key in (
        "modes_checked", "cf_lock_in_count", "il_lock_in_count", "lock_in_count",
        "screening_damage_rate_per_year", "screening_life_years", "required_life_years",
    ):
        values[key] = result[key]
    # The admissible annual damage follows algebraically from reciprocal life.
    values["annual_damage_limit"] = 1 / result["required_life_years"]
    for row in rows:
        mode = int(row["mode"])
        for field in ("mode", "f_n_Hz", "Vr_lo", "A_over_D", "stress_range_MPa", "cycles_per_year", "damage_per_year"):
            value = float(row[field])
            values[f"mode_{mode}_{field}"] = int(value) if field == "mode" else value
    return {
        "values": values, "inputs": inputs, "rows": rows, "bands": _bands(),
        "status": result["screening_status"],
        "il_unassessed": result["il_lock_in_unassessed"],
        "escalation_required": result["escalation"]["required"],
    }


def _style() -> str:
    """Inline the existing brand stylesheet and the exemplar's prose layout."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    page = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", page, re.S).group(1)
    style = re.sub(r"/\*.*?\*/", "", brand + "\n" + layout, flags=re.S)
    return "\n".join(line.rstrip() for line in style.splitlines())


def _chart(data: dict, field: str, unit: str, limit: float, label: str) -> str:
    """Draw a keyboard-focusable modal chart with demand and limit in hover."""
    values = data["values"]
    count = values["modes_checked"]
    demands = [values[f"mode_{mode}_{field}"] for mode in range(1, count + 1)]
    ymax = max(max(demands), limit) * 1.15
    y = lambda value: 230 - 190 * value / ymax
    parts = [f'<svg viewBox="0 0 760 290" role="img" aria-label="{escape(label)}" style="width:100%">',
             '<path d="M70 25 V230 H735" fill="none" stroke="currentColor"/>',
             f'<line x1="70" x2="735" y1="{y(limit):.2f}" y2="{y(limit):.2f}" stroke="var(--nogo)" stroke-dasharray="6 4"/>',
             f'<text x="72" y="{y(limit)-8:.2f}" font-size="12">Limit: {limit:.4g} {unit}</text>',
             '<text x="350" y="278">Mode number</text>',
             f'<text x="72" y="18" font-size="13">{escape(label)} ({unit})</text>']
    for mode, demand in enumerate(demands, start=1):
        x = 85 + (mode - 1) * 630 / max(1, count - 1)
        tip = f"Case: synthetic uniform current; component: span, mode {mode}; demand: {demand:.4g} {unit}; limit: {limit:.4g} {unit}"
        parts.append(
            f'<circle tabindex="0" cx="{x:.2f}" cy="{y(demand):.2f}" r="6" fill="var(--brand)" data-tip="{escape(tip)}">'
            f'<title>{escape(tip)}</title></circle><text x="{x-4:.2f}" y="250" font-size="12">{mode}</text>'
        )
    parts.extend(['</svg>', '<p class="sub" aria-live="polite" data-chart-readout>Hover over or focus a marker to read its demand and limit.</p>'])
    return "\n".join(parts)


def render(data: dict) -> str:
    """Write the complete report text with values supplied by the calculation."""
    values = data["values"]

    def number(key: str, precision: str = ".4g") -> str:
        return (f'<span data-number="{key}" data-format="{precision}">'
                f'{format(values[key], precision)}</span>')

    def ref(index: int) -> str:
        return f'<a href="#ref{index}">[{index}]</a>'

    input_rows = "\n".join(
        f'<tr><td>{escape(label)}</td><td>{number(key, ".8g")}</td><td>{unit}</td><td>{ref(4)}; assumed</td></tr>'
        for key, label, unit in data["inputs"]
    )
    modal_rows = []
    for row in data["rows"]:
        mode = int(row["mode"])
        cells = [number(f"mode_{mode}_{field}", precision) for field, precision in (
            ("mode", ".0f"), ("f_n_Hz", ".6f"), ("Vr_lo", ".4f"),
            ("A_over_D", ".4f"), ("stress_range_MPa", ".3f"),
            ("cycles_per_year", ".0f"), ("damage_per_year", ".6g"),
        )]
        cells.extend(["Yes" if row["cf_lock_in"] == "True" else "No",
                      "Yes" if row["il_lock_in"] == "True" else "No", ref(4)])
        modal_rows.append("<tr>" + "".join(f"<td>{cell}</td>" for cell in cells) + "</tr>")
    cf_low, cf_high = data["bands"]["cf"]
    il_low, il_high = data["bands"]["il"]
    damage_chart = _chart(data, "damage_per_year", "1/year", values["annual_damage_limit"], "Cross-flow annual damage")
    velocity_chart = _velocity_chart(data)
    change_items = "".join(f"<li>{escape(item)}</li>" for item in changes()["changes"])
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Vortex-induced vibration screening module analysis</title>
<style>{_style()}</style></head>
<body class="hs-doc"><main class="wrap">
<header><p class="kicker">Engineering calculation module analysis</p>
<h1>Vortex-induced vibration screening module analysis</h1>
<p>Draft for maintainer review</p></header>

<h2 id="s1">1. Executive summary</h2>
<p>The vortex-induced vibration screening module is a uniform-span current-screening calculation.
Analysis is performed to determine modal lock-in flags and an indicative cross-flow fatigue life;
the key results are summarized below. The documented public synthetic example is evaluated,
and its results are not a field design basis. {ref(4)}</p>
<ul><li>The synthetic example fails the cross-flow life comparison.
<ul><li>The calculated life is {number("screening_life_years", ".1f")} years against the
{number("required_life_years", ".0f")} year required life. Detailed response and fatigue assessment
is the next engineering action before any operating acceptance. {ref(4)}</li></ul></li>
<li>The scope of the decision is limited to screening.
<ul><li>{number("cf_lock_in_count", ".0f")} cross-flow mode and {number("il_lock_in_count", ".0f")}
in-line mode are flagged. In-line fatigue is not evaluated, and a separate in-line fatigue assessment
is required before acceptance. {ref(4)}</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>0</td><td>21 October 2026</td><td>Initial module analysis; calculation evidence and missing qualification are distinguished.</td></tr></tbody></table>
<figure>{damage_chart}<figcaption>Governing screening comparison. The annual cross-flow damage of each
mode is compared with the reciprocal of the required life. The sum of modal damage is used by the module;
the first mode alone exceeds this limit. An operating envelope is not established. {ref(4)}</figcaption></figure>

<h2 id="s2">2. Introduction</h2>
<p>A straight, uniform tubular span in steady current is considered. Riser vibration screening
is performed with analytical beam modes, a reduced-velocity current envelope and supplied modal stress.
Water depth is not supplied; span length is the structural length and is not interpreted as water depth. {ref(4)}</p>
<p>The documented workflow accepts a configuration mapping, calls the analytical modal and fatigue
components, and writes per-mode and summary comma-separated tables. A configuration echo is available
through the engine entry point. The worked calculation calls the same workflow directly. {ref(4)} {ref(5)}</p>

<h2 id="s3">3. Design data</h2>
<p>The input basis is taken from the public synthetic example. All physical properties, environmental
properties, stress calibration and acceptance inputs are marked ASSUMED - to be confirmed for any
application to a real span. {ref(4)}</p>
<p>The model uses {number("length_m", ".1f")} m span length, {number("outer_diameter_m", ".4f")} m
outer diameter and {number("inner_diameter_m", ".4f")} m inner diameter. Pinned supports and
{number("effective_tension_N", ".0f")} N effective tension are specified. The complete data register
is given in Appendix A. {ref(4)}</p>
<p>A spatial current profile is accepted as an alternative to a scalar current. Only the minimum and
maximum profile speeds are used; depth-dependent modal work and shear are not resolved. The effective
beam mass and the effective mass ratio are separate inputs and are not checked for mutual consistency. {ref(4)}</p>
<h3>Criteria and rationale</h3>
<p>Required screening life is design duration multiplied by the design fatigue factor, giving
{number("design_life_years", ".0f")} × {number("dff", ".0f")} =
{number("required_life_years", ".0f")} years. This is a supplied acceptance basis; it is not presented
as a universal standard requirement. The equivalent annual damage limit is
{number("annual_damage_limit", ".5f")} per year. {ref(4)}</p>
<p>Implemented inclusive reduced-velocity bands are {cf_low:g} to {cf_high:g} for cross-flow and
{il_low:g} to {il_high:g} for in-line motion. These are implementation criteria read from the calculation
source. Their independent physical qualification is NOT_EVALUATED. The associated standard references
do not establish that these particular coefficients are qualified for a proposed installation. {ref(4)} {ref(6)}</p>

<h2 id="s4">4. Analysis methodology</h2>
<p>Uniform beam modes are calculated before current lock-in and cross-flow fatigue are assessed.
The first benchmark is the untensioned pinned-beam closed form. Added mass is included in the supplied
mass per unit length; it is not calculated by this workflow. {ref(1)} {ref(4)}</p>
<figure>{_schematic()}<figcaption>Model and boundary conditions. The horizontal view shows the span
coordinate, pinned ends, uniform current normal to the tube and cross-flow displacement normal to the
drawing. The section shows the bore and wall; drawing proportions are schematic. No mesh, host-motion
model, buoyancy distribution or lateral support between the pins is defined. {ref(4)}</figcaption></figure>
<p>For the untensioned uniform beam, the governing equation is
<span class="eq">m ∂²w/∂t² + EI ∂⁴w/∂x⁴ = 0</span>.
Here m is effective mass per length, w is transverse displacement, t is time, x is distance along the span,
E is Young’s modulus and I is second moment of area. The small-deflection beam equation and dispersion
relation are given in the open beam text. {ref(1)}</p>
<p>With zero deflection and zero bending moment at each pin, the sinusoidal mode has wave number
nπ/L. Substitution into the cited dispersion relation gives
<span class="eq">fₙ = n²π/(2L²) √(EI/m)</span>, where fₙ is modal frequency in hertz,
n is positive mode number and L is span length. For an annulus,
<span class="eq">I = ∫ y² dA = π(Dₒ⁴ − Dᵢ⁴)/64</span>; Dₒ and Dᵢ are outer and inner
diameter. Here y is distance from the section’s neutral axis and dA is an elemental cross-sectional area.
The factor follows by integrating the circular section in polar coordinates. The worked
example is confined to this zero-tension, pinned case. {ref(1)}</p>
<p>Reduced velocity is defined as current speed U divided by fₙDₒ. The dimensionless Reynolds number
is U Dₒ/ν, with ν the kinematic viscosity. Reynolds number is the inertia-to-viscosity measure in the
open fluid-mechanics teaching source. The module's reduced-velocity definition is a screening convention
whose standard association remains NOT_EVALUATED here. {ref(3)} {ref(4)} {ref(6)}</p>
<p>The cross-flow amplitude relation, stability factor, piecewise shedding-frequency coefficients,
alternative support constants and tension corrections are implemented approximations. Independent source
qualification of their numerical coefficients is NOT_EVALUATED. Their outputs are reported as module
screening estimates; no unpublished coefficient or licensed table value is reproduced. {ref(4)} {ref(6)}</p>
<p>Full stress range is twice the supplied stress amplitude at unit displacement ratio multiplied by
the calculated displacement-to-diameter ratio. Annual cycles are frequency multiplied by exposure
fraction and the duration of a year. Annual damage is the sum of cycle count divided by endurance
at each stress range; screening life is its reciprocal. Linear cumulative damage and its limitations
are described in the open fatigue text. The module’s stress-range convention and annualisation are
implementation assumptions, not additional material qualification. {ref(2)} {ref(4)}</p>
<p>The workflow escalates when locked modes lack a stress calibration, the response exceeds its
confidence criterion, excessive simultaneous modes are locked, checked frequencies do not cover shedding,
or Reynolds number is outside its implemented window. Damage and life are then suppressed. The exact
trigger constants remain source-qualification gaps and are not adopted as design limits. {ref(4)} {ref(6)}</p>

<h2 id="s5">5. Detailed results</h2>
<p>The synthetic example is computed by calling the module and reading the emitted modal table.
No licensed solver is used. The calculation is a narrow-band cross-flow screening estimate. {ref(4)}</p>
<p>The annular second moment of area is {number("inertia", ".8g")} m⁴ and the first modal frequency
is {number("mode_1_f_n_Hz", ".6f")} Hz. With {number("speed_m_s", ".2f")} m/s current,
the first-mode reduced velocity is {number("mode_1_Vr_lo", ".4f")} and Reynolds number is
{number("reynolds", ".1f")}. The hand-calculation anchors and displayed-input frequency check
are specified in Appendix C. {ref(4)} {ref(5)}</p>
<figure>{velocity_chart}<figcaption>Modal reduced velocity at the scalar current. The bands show
implemented screening boundaries, with physical qualification NOT_EVALUATED. A boundary intersection
flags possible lock-in rather than demonstrating a physical response. {ref(4)} {ref(6)}</figcaption></figure>
<p>The first-mode displacement-to-diameter ratio is {number("mode_1_A_over_D", ".4f")}; its
stress range is {number("mode_1_stress_range_MPa", ".3f")} MPa. Annual cross-flow damage is
{number("screening_damage_rate_per_year", ".5f")} per year, giving
{number("screening_life_years", ".1f")} years against the {number("required_life_years", ".0f")}
year required life. The result is a failed screening comparison, and detailed response and fatigue
assessment is required before any operating acceptance. {ref(4)}</p>
<p>The workflow checks {number("modes_checked", ".0f")} modes and flags
{number("lock_in_count", ".0f")} distinct locked modes. The in-line flag is unassessed for damage.
Zero cross-flow damage in another case therefore does not demonstrate infinite total fatigue life. {ref(4)} {ref(5)}</p>

<h2 id="s6">6. Key findings</h2>
<p>The supplied life requirement is not met by the public example under the module’s response assumptions.
The result is suitable for identifying the need for further engineering assessment, with physical
accuracy still dependent on input calibration and response-model qualification. {ref(4)}</p>
<p>Existing evidence includes analytical-component tests, input rejection and applicability tests,
current-envelope tests, annualisation and fatigue-factor tests, output-location tests and deterministic
example regression checks. The durable workflow test contains a pinned-beam first-frequency comparison.
These records test implementation behaviour; measured-response benchmarking and corpus qualification
remain absent from the documented validation basis. {ref(4)} {ref(5)}</p>

<h2 id="s7">7. Limitations</h2>
<p>Detailed response prediction and a complete fatigue assessment are excluded from this screening scope.
In-line fatigue, sheared-current modal energy, variable tension and geometry, nonlinear response,
stress recovery, thickness effects and site-specific loading are not resolved by this workflow. {ref(4)}</p>
<p>The fatigue curve identifier F1 denotes the selected weld-detail curve in the implementation;
seawater with cathodic protection is the selected environment. No claim of suitability for a particular
weld or environment is made. The returned standards string omits a fatigue-standard edition,
and exact edition and clause equivalence are NOT_EVALUATED. {ref(4)} {ref(6)}</p>
<p>Higher modes for alternative end supports use simplified scaling, and effective mass and mass ratio
are supplied separately. The first flagged cross-flow mode is selected as critical; this is not a ranking
by the largest damage. A passing cross-flow screening result is not an in-line fatigue acceptance. {ref(4)}</p>
<h3>Not covered</h3>
<p>The separate parametric screening adapter, legacy vibration workflows, hosted interfaces, full
time-domain analysis, host motion, drift-off, recoil and weak-point assessment are not covered.
An installation operating envelope is not evaluated because no installation basis is supplied. {ref(4)}</p>

<h2 id="s8">8. Recommendations</h2>
<p>The failed synthetic comparison is addressed by a detailed response and fatigue assessment before
operating acceptance. The open fatigue source identifies stress range, cycle history and the limitations
of linear damage accumulation as essential to interpreting a fatigue estimate. {ref(2)} {ref(4)}</p>
<p>For application to a real span, geometry, effective mass, damping, stress calibration, exposure,
weld category and acceptance basis are to be established from application data. In-line fatigue is to
be included whenever its lock-in flag is set. A change in current, support condition or geometry is a
trigger for reassessment; this report establishes no numerical operating restriction for an installation. {ref(4)}</p>
<p>The first qualification slice is the pinned, untensioned uniform span with supplied stress calibration.
Measured or openly published response and fatigue benchmarks are needed before extending confidence to
alternative boundaries, tension corrections or the implemented response coefficients. Source edition and
clause mapping is to be completed before any standards-compliance claim. {ref(1)} {ref(4)} {ref(6)}</p>

<h2 id="s9">9. References</h2>
<p>The theory basis is separated from the implementation and its evidence. Open sources support the
beam and cumulative-damage derivations; they do not qualify all empirical VIV coefficients.</p>
<h3>9.1 Engineering sources</h3>
<ol class="refs"><li id="ref1"><a href="https://euphonics.org/3-2-1-bending-beams-and-free-free-modes/">Euphonics,
online edition accessed 21 October 2026, chapter 3.2.1, Bending beams and free-free modes</a>,
equations (7) and (12). The pinned-boundary substitution and circular-section integral are derived here.
The source's free-free eigenvalues are not used.</li>
<li id="ref2"><a href="https://eng.libretexts.org/Bookshelves/Mechanical_Engineering/Mechanics_of_Materials_(Roylance)/06%3A_Yield_and_Fracture/6.05%3A_Fatigue">Mechanics
of Materials, open online edition accessed 21 October 2026, chapter 6.5, Fatigue</a>,
S–N curves and cumulative-damage subsections. No empirical S–N constants are copied.</li>
<li id="ref3"><a href="https://www.grc.nasa.gov/www/k-12/airplane/reynolds.html">Beginner’s Guide
to Aeronautics, open online edition accessed 21 October 2026, Reynolds Number</a>, inertia/viscosity
definition. This source is a teaching reference, not a VIV qualification standard.</li>
<li id="ref4">Public synthetic example and current implementation: exact locators are listed in section 9.2.
All worked-example inputs and outputs are sourced from these records.</li>
<li id="ref5">Repository regression and calculation tests: exact locators and tested scope are listed in section 9.2.</li>
<li id="ref6">Implementation-associated standards: DNV-RP-C205 (2019), Environmental conditions and
environmental loads, section 9; DNV-RP-F105 (2017), Free spanning pipelines, section 4;
DNV-RP-C203, Fatigue design of offshore steel structures, edition unspecified in workflow provenance.
Licensed texts were not accessed. Exact clause titles, coefficients and edition equivalence are
NOT_EVALUATED; these associations are not evidence of standards compliance.</li></ol>
<h3>9.2 Evidence locators and calculation instructions</h3>
<table><thead><tr><th>Record</th><th>Locator</th><th>Evidence and boundary</th></tr></thead><tbody>
<tr><td>Public example</td><td><code>{EXAMPLE.as_posix()}</code><br>
<code>examples/workflows/riser-viv-screening/README.md</code></td><td>Synthetic assumptions, documented invocation and expected result; no measured benchmark.</td></tr>
<tr><td>Public workflow</td><td><code>src/digitalmodel/viv_screening/workflow.py:router</code><br>
<code>src/digitalmodel/viv_screening/inputs.py:parse_inputs</code></td><td>Required inputs, current envelope, lock-in flags, escalation, annual damage and output tables.</td></tr>
<tr><td>Analytical dependency</td><td><code>src/digitalmodel/orcaflex/viv_screening.py:BeamProperties</code><br>
<code>src/digitalmodel/fatigue/sn_curves.py:get_sn_curve</code><br>
<code>src/digitalmodel/fatigue/damage.py:miner_damage</code></td><td>Beam modes, response estimate and fatigue accumulation; no solver execution.</td></tr>
<tr><td>Existing regression evidence</td><td><code>tests/orcaflex/test_viv_screening.py</code><br>
<code>tests/workflows/test_viv_screening_workflow.py</code><br>
<code>tests/workflows/test_durable_workflows.py:test_workflow_registry[riser-viv-screening]</code></td>
<td>Core relations, invalid inputs, escalation, output locations, deterministic example and pinned-mode comparison.
Test outcomes for this analysis are recorded separately from test existence.</td></tr>
<tr><td>Report checks</td><td><code>tests/reporting/test_vivscreening_module_report.py</code></td>
<td>Hand-worked anchors, displayed-input headline, module consistency, rebuilt page and source-link checks.</td></tr>
<tr><td>House-style exemplar</td><td><code>{EXEMPLAR.as_posix()}</code><br><code>docs/api/_assets/brand.css</code></td>
<td>Existing prose layout and brand stylesheet are inlined; the required body class is added.</td></tr></tbody></table>
<p>Rebuild from the repository root with calculation dependencies available:</p>
<pre><code>PYTHONPATH=src python scripts/reporting/build_vivscreening_module_report.py --scratch &lt;temporary-directory&gt;</code></pre>
<p>The temporary directory is outside the checkout. Each rebuild calls the module, reads its modal
table, and writes this report and the engineering change register. Calculation outputs are not report deliverables.</p>

<h2 id="s10">10. Acronyms and symbols</h2>
<p>Symbols are defined at their equations and units are included with the inputs and results.
Abbreviations used in the module’s evidence are expanded below.</p>
<table><thead><tr><th>Term</th><th>Meaning</th></tr></thead><tbody>
<tr><td>VIV</td><td>Vortex-induced vibration.</td></tr>
<tr><td>Cross-flow</td><td>Motion perpendicular to the current and span axes.</td></tr>
<tr><td>In-line</td><td>Motion parallel to the current.</td></tr>
<tr><td>S–N</td><td>Relationship between stress range and endurance cycles.</td></tr>
<tr><td>Design fatigue factor</td><td>Multiplier applied to the design duration in the screening comparison.</td></tr>
<tr><td>NOT_EVALUATED</td><td>Evidence or calculation needed for engineering acceptance is absent from this analysis.</td></tr></tbody></table>

<details open><summary>Appendix A. Data register</summary>
<p>The data below are ASSUMED - to be confirmed. Source [4] supplies every numeric input;
no site measurement, material certificate or stress calibration is supplied. The beam boundary is
pinned at both ends; the fatigue curve identifier is F1 with seawater cathodic protection.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source and status</th></tr></thead>
<tbody>{input_rows}</tbody></table>
<p>Field and water depth, host motions, mode-dependent stresses, detailed weld geometry,
current-direction distribution, surface condition and independent empirical coefficient sources are not held.
Physical qualification is NOT_EVALUATED until these records are supplied; substitute values are not inserted.</p></details>

<details><summary>Appendix B. Per-mode results</summary>
<p>Only cross-flow damage is included. The scalar current gives equal lower and upper reduced velocities.
Zero stress, cycle and damage entries outside cross-flow lock-in are workflow placeholders;
in-line response remains not evaluated. The source column refers to the module output described in reference [4].</p>
<table><thead><tr><th>Mode</th><th>Frequency (Hz)</th><th>Reduced velocity (dimensionless)</th>
<th>Displacement/diameter (dimensionless)</th><th>Stress range (MPa)</th><th>Cycles/year</th>
<th>Damage/year</th><th>Cross-flow flagged</th><th>In-line flagged</th><th>Source</th></tr></thead>
<tbody>{''.join(modal_rows)}</tbody></table></details>

<details><summary>Appendix C. Calculation evidence and pre-flight</summary>
<p>Hand-worked anchors cover Reynolds number, annular second moment, required life and full stress range.
The first frequency is also recalculated from the displayed input precision. The report consistency checks
compare every tagged worked-example value with a fresh module call and compare the rebuilt rounded page
with the committed page. These checks establish arithmetic and rendering consistency, not empirical response accuracy.</p>
<p>The beam benchmark uses the displayed diameters without reducing their precision. Annual damage is
compared with the inverse required life. Full fatigue qualification, independent response measurements,
alternative support benchmarks and installation operating limits are NOT_EVALUATED. References resolve
within this report and the numeric inputs retain their significant digits.</p></details>

<details><summary>Engineering changes</summary><p>Revision {changes()["revision"]}:
initial engineering content; there is no earlier issued revision.</p><ul>{change_items}</ul></details>
<details><summary>Comments</summary>
<p>Comments are stored in a separate file beside the report. Select the report folder to save and
reload comments. When folder access is unavailable, export the comments file and place it beside the report;
use Import to reopen it. Unsaved comments are lost when this page is closed.</p>
<label for="comment-section">Section</label><select id="comment-section">
{''.join(f'<option value="s{i}">Section {i}</option>' for i in range(1,11))}</select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4" style="width:100%"></textarea>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-folder" type="button">Select report folder</button>
<button id="comment-save" type="button">Save beside report</button>
<button id="comment-export" type="button">Export</button>
<label>Import <input id="comment-import" type="file" accept=".json"></label>
<p id="comment-status" role="status">No comments saved.</p><ol id="comments"></ol></details>
<script id="calculation-data" type="application/json">{json.dumps(_rounded(data), ensure_ascii=False, sort_keys=True)}</script>
<script id="engineering-changes" type="application/json">{json.dumps(changes(), ensure_ascii=False)}</script>
<script>{_INTERACTIONS}</script>
<footer>Screening estimates from a public synthetic example. Application-specific acceptance remains not evaluated.</footer>
</main></body></html>
'''


def _velocity_chart(data: dict) -> str:
    values = data["values"]
    high = max(max(data["bands"]["cf"]), values["mode_1_Vr_lo"]) * 1.15
    y = lambda value: 230 - 190 * value / high
    parts = ['<svg viewBox="0 0 760 290" role="img" aria-label="Reduced velocity and implemented lock-in bands" style="width:100%">',
             '<path d="M70 25 V230 H735" fill="none" stroke="currentColor"/>',
             '<text x="72" y="18" font-size="13">Reduced velocity (dimensionless)</text>',
             '<text x="350" y="278">Mode number</text>']
    for direction, label in (("cf", "Cross-flow"), ("il", "In-line")):
        for limit in data["bands"][direction]:
            parts.append(f'<line x1="70" x2="735" y1="{y(limit):.2f}" y2="{y(limit):.2f}" stroke="var(--nogo)" stroke-dasharray="6 4"/>')
            parts.append(f'<text x="480" y="{y(limit)-5:.2f}" font-size="12">{label} boundary: {limit:g}</text>')
    count = values["modes_checked"]
    for mode in range(1, count + 1):
        demand = values[f"mode_{mode}_Vr_lo"]
        x = 85 + (mode - 1) * 630 / max(1, count - 1)
        tip = (f"Case: synthetic uniform current; component: span, mode {mode}; demand: {demand:.4f} dimensionless; "
               f"limits: cross-flow {data['bands']['cf'][0]:g} to {data['bands']['cf'][1]:g}, "
               f"in-line {data['bands']['il'][0]:g} to {data['bands']['il'][1]:g}, dimensionless; physical qualification not evaluated")
        parts.append(f'<circle tabindex="0" cx="{x:.2f}" cy="{y(demand):.2f}" r="6" fill="var(--brand)" data-tip="{escape(tip)}"><title>{escape(tip)}</title></circle>')
        parts.append(f'<text x="{x-4:.2f}" y="250" font-size="12">{mode}</text>')
    parts.extend(['</svg>', '<p class="sub" aria-live="polite" data-chart-readout>Hover over or focus a marker to read its demand and limits.</p>'])
    return "\n".join(parts)


def _schematic() -> str:
    return '''<svg viewBox="0 0 820 310" role="img" aria-label="Pinned uniform tube and annular cross-section" style="width:100%">
<rect x="65" y="115" width="440" height="35" fill="var(--panel-2)" stroke="var(--brand)"/>
<path d="M75 150 l-18 30 h36 Z M495 150 l-18 30 h36 Z" fill="none" stroke="currentColor"/>
<path d="M60 190 H510 M90 180 v15 M480 180 v15" fill="none" stroke="currentColor"/>
<text x="200" y="215">Span length L; coordinate x →</text>
<path d="M160 40 v60 l-5 -10 m5 10 l5 -10 M285 40 v60 l-5 -10 m5 10 l5 -10 M410 40 v60 l-5 -10 m5 10 l5 -10" fill="none" stroke="var(--accent)"/>
<text x="185" y="30">Uniform current U ↓</text>
<text x="75" y="245">Pins: displacement and bending moment are zero.</text>
<text x="75" y="270">Cross-flow motion w is normal to the drawing.</text>
<circle cx="665" cy="130" r="63" fill="var(--panel-2)" stroke="var(--brand)"/>
<circle cx="665" cy="130" r="50" fill="var(--panel)" stroke="var(--brand)"/>
<path d="M602 130 H728 M615 140 H715" stroke="currentColor"/>
<text x="590" y="225">Outer diameter Dₒ</text><text x="590" y="248">Inner diameter Dᵢ</text>
<text x="590" y="275">Bore and uniform tube wall</text></svg>'''


def _rounded(value):
    """Serialize only selected calculation data, with portable rounded floats."""
    if isinstance(value, float):
        return float(f"{value:.10g}")
    if isinstance(value, dict):
        # CSV strings contain extra digits; values holds the rounded evidence.
        return {key: _rounded(item) for key, item in value.items() if key != "rows"}
    if isinstance(value, (list, tuple)):
        return [_rounded(item) for item in value]
    return value


_INTERACTIONS = r'''
"use strict";
document.querySelectorAll("[data-tip]").forEach(marker => {
  const show = () => { marker.closest("figure").querySelector("[data-chart-readout]").textContent = marker.dataset.tip; };
  marker.addEventListener("pointerenter", show);
  marker.addEventListener("focus", show);
  marker.addEventListener("click", show);
});
let comments = [], directory;
const status = document.getElementById("comment-status");
function validate(value) {
  if (!Array.isArray(value) || value.some(c => !c || !/^s(?:[1-9]|10)$/.test(c.section) || typeof c.text !== "string")) {
    throw new Error("The comments file must contain section and text entries.");
  }
  return value;
}
function display() {
  const list = document.getElementById("comments");
  list.replaceChildren();
  comments.forEach(comment => {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = "#" + comment.section;
    link.textContent = document.getElementById(comment.section).textContent;
    item.append(link, document.createTextNode(": " + comment.text));
    list.append(item);
  });
}
document.getElementById("comment-add").onclick = () => {
  const input = document.getElementById("comment-text");
  if (!input.value.trim()) return;
  comments.push({section: document.getElementById("comment-section").value, text: input.value.trim()});
  input.value = ""; display(); status.textContent = "Comment added; save or export to keep it.";
};
document.getElementById("comment-folder").onclick = async () => {
  try {
    if (!window.showDirectoryPicker) throw new Error("Folder access is unavailable. Use Export and Import.");
    const selected = await window.showDirectoryPicker({mode: "readwrite"});
    let handle;
    try {
      handle = await selected.getFileHandle("report.comments.json");
    } catch (error) {
      if (error.name !== "NotFoundError") throw error;
    }
    if (handle) {
      const loaded = validate(JSON.parse(await (await handle.getFile()).text()));
      comments = comments.concat(loaded); display();
      status.textContent = "Saved comments loaded; unsaved comments are kept. Save to keep the combined list.";
    } else {
      status.textContent = "Folder selected. Save writes report.comments.json beside the report.";
    }
    directory = selected;
  } catch (error) { status.textContent = error.message; }
};
document.getElementById("comment-save").onclick = async () => {
  try {
    if (!directory) throw new Error("Select the report folder first, or use Export.");
    const handle = await directory.getFileHandle("report.comments.json", {create: true});
    const writer = await handle.createWritable();
    await writer.write(JSON.stringify(comments, null, 2) + "\n"); await writer.close();
    status.textContent = "Comments saved beside the report.";
  } catch (error) { status.textContent = error.message; }
};
document.getElementById("comment-export").onclick = () => {
  const url = URL.createObjectURL(new Blob([JSON.stringify(comments, null, 2) + "\n"], {type: "application/json"}));
  const link = document.createElement("a"); link.href = url; link.download = "report.comments.json";
  link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000);
  status.textContent = "Comments exported. Place the downloaded file beside the report.";
};
document.getElementById("comment-import").onchange = async event => {
  try {
    const file = event.target.files[0]; if (!file) return;
    comments = comments.concat(validate(JSON.parse(await file.text()))); display();
    status.textContent = "Comments imported; save or export to keep the combined list.";
  } catch (error) { status.textContent = error.message; }
};
'''


def main() -> None:
    """Regenerate the report and the matching change-panel record."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=ROOT / REPORT)
    args = parser.parse_args()
    data = calculate(args.scratch)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(render(data), encoding="utf-8", newline="\n")
    args.output.with_name("changes.json").write_text(
        json.dumps(changes(), indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n"
    )


if __name__ == "__main__":
    main()
