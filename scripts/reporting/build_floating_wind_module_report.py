# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the standalone floating-wind module analysis from public repo inputs.

Use PYTHONPATH=src python scripts/reporting/build_floating_wind_module_report.py
--output-dir <directory>. No solver is imported or called by this script.
"""

from __future__ import annotations

import argparse
import ast
import copy
import html
import json
import math
import re
import tomllib
from pathlib import Path

import yaml

from digitalmodel.floating_wind import floaters
from digitalmodel.floating_wind.screening import LoadCase, ScreeningCriteria, _daf, screen_concept
from digitalmodel.floating_wind.workflow import FloatingWindSizingWorkflow
from digitalmodel.orcaflex.batch_parametric import ParameterSweep

REPORT_DIRECTORY = Path("docs/reports/modules/floatingwind")
EXAMPLE = Path("examples/workflows/floating-wind-sizing/input.yml")
STYLE = Path("src/digitalmodel/reporting/assets/calc_report_format.html")
EXEMPLAR = Path("docs/domains/cathodic_protection/reports/cases/S01/report.html")
DATE = "2026-10-22"
CHANGES = {
    "report": "Floating wind concept screening analysis",
    "revisions": [{
        "revision": "0", "date": DATE,
        "changes": [
            "First issue: document concept screening with the public sizing example.",
            "Separate proxy acceptance from physical validation and installation limits.",
            "Expose assumed inputs, per-component limits and unresolved engineering data.",
        ],
    }],
}
NAMES = {"semi": "Semi-submersible", "spar": "Spar", "barge": "Barge"}
FIELDS = {
    "rna_mass_t": ("Rotor and nacelle mass", "t"),
    "tower_mass_t": ("Tower mass", "t"),
    "hub_height_m": ("Hub height above still water", "m"),
    "tower_base_above_swl_m": ("Tower base above still water", "m"),
    "hs_m": ("Significant wave height", "m"),
    "tp_s": ("Peak wave period", "s"),
    "steady_load_kN": ("Steady horizontal load", "kN"),
    "gm_min_m": ("Minimum metacentric height", "m"),
    "daf_limit": ("Maximum dynamic amplification", "dimensionless"),
    "damping_ratio": ("Damping ratio", "dimensionless"),
    "modal_band_s": ("Excluded natural-period band", "s"),
    "mooring_stiffness_kN_per_m": ("Horizontal mooring stiffness", "kN/m"),
    "max_offset_m": ("Maximum steady offset", "m"),
    "n_columns": ("Column count", "count"),
    "column_diameter": ("Column diameter", "m"),
    "column_radius": ("Column centre radius", "m"),
    "diameter": ("Diameter", "m"),
    "draft": ("Draft", "m"),
    "pontoon_height": ("Pontoon height", "m"),
    "pontoon_volume": ("Submerged pontoon volume", "m³"),
    "steel_mass_t": ("Hull steel mass", "t"),
    "length": ("Length", "m"), "beam": ("Beam", "m"),
    "steel_vcg_frac": ("Steel centroid / draft", "dimensionless"),
    "ca_heave": ("Heave added mass / moving mass", "dimensionless"),
    "ca_pitch": ("Pitch added inertia / inertia", "dimensionless"),
    "block_coeff": ("Displaced volume / bounding box volume", "dimensionless"),
    "waterplane_coeff": ("Waterplane area / bounding rectangle area", "dimensionless"),
}
PROPERTY_FIELDS = [
    ("draft_m", "Draft", "m"),
    ("displacement_t", "Displacement", "t"),
    ("steel_mass_t", "Steel mass", "t"),
    ("ballast_mass_t", "Ballast mass", "t"),
    ("topside_mass_t", "Topside mass", "t"),
    ("total_mass_t", "Total mass", "t"),
    ("waterplane_area_m2", "Waterplane area", "m²"),
    ("KB_m", "Centre of buoyancy above keel", "m"),
    ("BM_m", "Metacentric radius", "m"),
    ("KG_m", "Centre of gravity above keel", "m"),
    ("GM_m", "Metacentric height", "m"),
    ("heave_natural_period_s", "Heave natural period", "s"),
    ("pitch_natural_period_s", "Pitch natural period", "s"),
    ("pitch_radius_gyration_m", "Pitch radius of gyration", "m"),
]
CHECKS = {
    "stability": ("Metacentric height", "m"),
    "motion": ("Dynamic amplification", "dimensionless"),
    "modal": ("Natural-period band", "s"),
    "mooring": ("Steady offset", "m"),
}


def _flatten(value, prefix, output):
    if isinstance(value, bool):
        return
    if isinstance(value, (int, float)):
        output[prefix] = value
    elif isinstance(value, dict):
        for key, child in value.items():
            _flatten(child, f"{prefix}.{key}", output)
    elif isinstance(value, (list, tuple)):
        for index, child in enumerate(value):
            _flatten(child, f"{prefix}.{index}", output)


def collect_example(root):
    """Evaluate the documented example through the public workflow and screen."""
    inputs = yaml.safe_load((root / EXAMPLE).read_text())["floating_wind_sizing"]
    summary = FloatingWindSizingWorkflow().router({
        "floating_wind_sizing": copy.deepcopy(inputs),
    })["floating_wind_sizing"]["result"]
    topside = floaters.TurbineTopside(**inputs["topside"])
    criteria = ScreeningCriteria(**inputs["criteria"])
    loads = [LoadCase(**load) for load in inputs["load_cases"]]
    results = [screen_concept(
        concept["archetype"], concept["base_params"],
        [ParameterSweep(**sweep) for sweep in concept["sweeps"]],
        topside, loads, criteria,
    ) for concept in inputs["concepts"]]
    return inputs, results, summary


def table(headings, rows, caption):
    head = "".join(f"<th scope=\"col\">{html.escape(item)}</th>" for item in headings)
    body = "".join("<tr>" + "".join(f"<td>{item}</td>" for item in row) + "</tr>" for row in rows)
    return (f'<div class="tbl-wrap"><table><caption>{caption}</caption>'
            f"<thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>")


def section(number, title, content):
    return (f'<section id="section-{number}"><h2>{number}. {title}</h2>'
            f"{content}</section>")


def ref(name):
    return f'<a href="#source-{name}">{html.escape(SOURCES[name][0])}</a>'


SOURCES = {
    "example": ("Public sizing example", EXAMPLE,
                "Input mapping: topside, load cases, criteria and concept sweeps."),
    "floaters": ("Floater equations and assumptions", Path("src/digitalmodel/floating_wind/floaters.py"),
                 "TurbineTopside, SemiSubmersible.properties, Spar.properties, Barge.properties, "
                 "_assemble, _heave_period and _pitch_period; physical constants."),
    "screening": ("Screening equations and acceptance logic", Path("src/digitalmodel/floating_wind/screening.py"),
                  "screen_concept, LoadCase, ScreeningCriteria and _daf; stability, modal, motion and mooring checks."),
    "workflow": ("Public sizing workflow", Path("src/digitalmodel/floating_wind/workflow.py"),
                 "FloatingWindSizingWorkflow.router; summary and optional JSON output."),
    "tests": ("Existing module tests", Path("tests/floating_wind"),
              "test_floaters.py, test_screening.py, test_tradespace.py, test_report.py and related test files."),
    "pack": ("Package capability exports", Path("src/digitalmodel/floating_wind/__init__.py"),
             "Public floater, economics, reliability, scaling, learning, collaboration and maturity-scoring interfaces."),
    "solver": ("Solver preparation implementation", Path("src/digitalmodel/floating_wind/solver_tier.py"),
               "build_panel_mesh, prepare_variant, dispatch_case and fold_rao_results; preparation is not a completed solve."),
    "trade": ("Trade-space implementation", Path("src/digitalmodel/floating_wind/tradespace.py"),
              "pareto_front and correlation_map; feasible-only and passed-only filters."),
    "economics": ("Economic calculation", Path("src/digitalmodel/floating_wind/economics.py"),
                  "compute_lcoe and base_case; discounted costs and constant annual energy."),
    "qualification": ("Maturity scoring", Path("src/digitalmodel/floating_wind/qualification.py"),
                      "score_concept and rank_concepts; weighted evidence inputs."),
    "rebuild": ("Report numerical checks", Path("tests/floating_wind/test_module_report.py"),
                "Fresh public-module calls, displayed-number equality, source links, limits and deterministic rebuild."),
    "style": ("House presentation exemplar", EXEMPLAR,
              "Standard report layout, revision table, appendices and review controls; "
              "shared presentation is read from the calculation-report stylesheet."),
}


def render_report(root):
    """Write the report text with every example quantity bound to module output."""
    root = Path(root)
    inputs, results, summary = collect_example(root)
    values = {}
    _flatten(inputs, "input", values)
    _flatten(summary, "summary", values)
    for result in results:
        for variant in result.variants:
            prefix = f"variant.{result.archetype.value}.{variant.case_id}"
            _flatten(variant.properties.model_dump(), f"{prefix}.properties", values)
            _flatten(variant.params, f"{prefix}.params", values)
            for check in variant.checks:
                _flatten(check.model_dump(), f"{prefix}.check.{check.name}", values)
    values["constant.density"] = floaters.RHO_SEAWATER
    values["constant.gravity"] = floaters.G_STANDARD
    for archetype, model in [("semi", floaters.SemiSubmersible),
                             ("spar", floaters.Spar), ("barge", floaters.Barge)]:
        fields = ["steel_vcg_frac", "ca_heave", "ca_pitch"]
        if archetype == "barge":
            fields += ["block_coeff", "waterplane_coeff"]
        for field in fields:
            values[f"default.{archetype}.{field}"] = model.model_fields[field].default
    floater_tree = ast.parse((root / SOURCES["floaters"][1]).read_text())
    assembly = next(node for node in floater_tree.body if isinstance(node, ast.FunctionDef) and node.name == "_assemble")
    ballast = next(node.value for node in assembly.body if isinstance(node, ast.Assign)
                   and any(isinstance(target, ast.Name) and target.id == "ballast_cg_m" for target in node.targets))
    if not (isinstance(ballast, ast.BinOp) and isinstance(ballast.op, ast.Mult)
            and isinstance(ballast.left, ast.Constant) and isinstance(ballast.right, ast.Name)
            and ballast.right.id == "draft_m"):
        raise ValueError("Review the changed ballast-centroid expression before rebuilding")
    values["constant.ballast_fraction"] = ballast.left.value
    numbers = {}

    def number(key, digits=1, tag="span"):
        value = values[key]
        if not math.isfinite(value):
            raise ValueError(f"Non-finite report quantity: {key}")
        if value != 0 and round(value, digits) == 0:
            raise ValueError(f"Quantity rounds to zero: {key}")
        if key in numbers and numbers[key]["digits"] != digits:
            raise ValueError(f"Inconsistent precision: {key}")
        numbers[key] = {"value": value, "digits": digits}
        formatted = f"{value:,.{digits}f}".replace("-", "−")
        return f'<{tag} data-number="{key}">{formatted}</{tag}>'

    def heading(title, body):
        return f"<h3>{title}</h3>{body}"

    def paragraph(text):
        return f"<p>{text}</p>"

    semi = results[0]
    exemplar_variant = next(v for v in semi.variants if all(
        v.params[key] == value for key, value in inputs["concepts"][0]["base_params"].items()
    ))
    example_prefix = f"variant.semi.{exemplar_variant.case_id}"
    s = []
    s.append(section(1, "Executive Summary", f"""
<p>Floating wind concept screening is a preliminary calculation of buoyancy, intact stability,
rigid-body natural periods and steady mooring offset. Analysis is performed to determine which
parametric concepts meet the supplied screening limits; key results are summarized below.</p>
<ul><li>A shortlist is obtained from the public sizing example.
<ul><li>{number('summary.total_passed', 0)} of {number('summary.total_variants', 0)} variants meet
all supplied proxy limits, including the minimum metacentric height and dynamic amplification limit.
This result permits further concept assessment; design acceptance is not established.</li></ul></li>
<li>The principal semi-submersible study is screened against the supplied load cases.
<ul><li>{number('summary.concepts.0.n_passed', 0)} of {number('summary.concepts.0.n_variants', 0)}
semi-submersibles pass. The same scope and acceptance qualifications are retained in the detailed results.</li></ul></li>
<li>An installation decision is not established.
<ul><li>Heading tolerance, significant-wave-height operating limits and forecast triggers are not evaluated.
The motion proxy does not use wave height, and no installation system is represented.</li></ul></li></ul>
<h3>Revision history</h3>
{table(['Revision', 'Date', 'What changed and why'], [('0', DATE,
'First issue: document the public concept screen, its numerical evidence and engineering limits.')], 'Report issue history.')}
<p>Source: {ref('example')}, {ref('workflow')} and {ref('screening')}.</p>
"""))
    s.append(section(2, "Introduction", f"""
<p>The module is used to reduce a set of floating-wind concepts to candidates for further analysis.
The calculation is separated from the evidence needed to accept a particular structure.</p>
<p>Semi-submersible, spar, tension-leg and barge archetypes are represented by parametric models.
Displacement and ballast are determined at a specified draft; small-angle stability and uncoupled
heave and pitch periods are then calculated. Variant grids, optional linear mooring offset,
trade-space ranking and standalone concept data sheets are provided. Source: {ref('floaters')},
{ref('screening')} and {ref('trade')}.</p>
<p>The main workflow in this report is the documented public repository sizing example.
Its semi-submersible sweep is treated in detail; the accompanying spar and barge cases are retained
for coverage. These are generic example geometries and assumed loading, rather than a reconstruction
of a published reference support platform. Source: {ref('example')}.</p>
<h3>Not covered</h3>
<p>Cost sensitivities, reliability and availability adjustments, scaling, learning, collaboration and
maturity scoring are available as separate overlays. They are not applied to the physical shortlist
in this report. Tendon sizing, fatigue, ultimate strength, dynamic cables, aerodynamic control,
installation rigging and coupled wave response are not evaluated here. Source: {ref('pack')},
{ref('economics')} and {ref('qualification')}.</p>
"""))
    s.append(section(3, "Summary", f"""
<p>The public sizing example is evaluated without an output-file request so that the calculation
is kept separate from the original example directory. The returned summary and full variant records
are used for every displayed result. Source: {ref('workflow')} and {ref('rebuild')}.</p>
<p>The minimum metacentric height is {number('input.criteria.gm_min_m')} m; the maximum dynamic
amplification is {number('input.criteria.daf_limit')}. Natural periods inside the inclusive band
from {number('input.criteria.modal_band_s.0')} to {number('input.criteria.modal_band_s.1')} s are rejected.
The maximum steady offset is {number('input.criteria.max_offset_m')} m. These are assumed example
criteria, not cited standard acceptance limits. Source: {ref('example')}.</p>
<p>Each chart shows one returned quantity against one varied geometry dimension. A limit is drawn,
and case and component details are available on hover, keyboard focus or touch. Select a component
below to inspect the same semi-submersible sweep without combining margins of different units.</p>
<div class="card"><label for="chart-select">Screening component </label>
<select id="chart-select"><option value="stability">Metacentric height</option>
<option value="motion">Dynamic amplification</option><option value="modal">Natural periods</option>
<option value="mooring">Steady offset</option></select>
<div id="chart-host"></div><p id="chart-detail" aria-live="polite">Select a point to inspect its demand and limit.</p></div>
<p>The shortlist is controlled by all checks together. No significant-wave-height envelope is inferred
from these points. A forecast service trigger is not evaluated because no operating sea-state limit
is calculated. Source: {ref('screening')}.</p>
"""))
    schematic = _schematic(inputs, lambda key, digits=1: number(key, digits, "tspan"))
    s.append(section(4, "Design Data", f"""
<p>Geometry, topside mass and site cases are read from the public sizing example.
All example design inputs are classified as ASSUMED - to be confirmed; their adoption for a real
structure is not established. The complete data register is provided in the appendix. Source: {ref('example')}.</p>
<h3>Physical model and boundary conditions</h3>
{schematic}
<p>Columns are treated as identical cylinders, evenly spaced on a ring above a submerged pontoon volume.
The keel is the vertical datum and the still-water plane is at the specified draft. Vertical
hydrostatic restoring is calculated from waterplane area. Pitch inertia is assembled from lumped
steel, ballast and topside masses with a horizontal spread term. A linear horizontal mooring spring
is used only for steady offset; it is not coupled into the vertical natural periods. No mesh is
used in the closed-form screen. Source: {ref('floaters')} and {ref('screening')}.</p>
<h3>Inputs and outputs</h3>
<p>A topside mass model, a non-empty load-case list, archetype geometry, sweep values and screening
criteria are supplied. Dimensions and masses are positive; pontoon height is less than draft.
The example uses a full-factorial combination of the listed sweep values. Additional input guards
and assumed defaults are identified in the data register. Source: {ref('floaters')} and {ref('screening')}.</p>
<p>The screen returns properties, component demand and limit records, feasibility and an overall
verdict for every variant. The sizing workflow returns concept and variant counts plus a passing
shortlist, ordered by the returned governing margin; optional JSON output contains that summary.
Rejected variants are available from the full screen result, rather than the shortlist summary.
No physical response history or line-tension time series is returned. Source: {ref('screening')}
and {ref('workflow')}.</p>
<h3>Criteria and rationale</h3>
<p>The metacentric-height floor identifies weak initial restoring; the motion-amplification ceiling
screens proximity to resonance. The excluded period band screens overlap with the assumed energetic
wave band, and the offset ceiling screens a steady watch circle. Their numeric values are example
inputs; standard compliance, damaged stability, heel limits, freeboard and line strength are not
established. Source: {ref('example')} and {ref('screening')}.</p>
<p>Sea-water density is {number('constant.density')} kg/m³ and gravitational acceleration is
{number('constant.gravity', 5)} m/s². These are implementation constants; environmental calibration
is not supplied. Added-mass, centroid and damping coefficients remain assumed model inputs rather
than measured properties. Source: {ref('floaters')}.</p>
"""))
    s.append(section(5, "Methodology", f"""
<p>The main equations are applied to each geometry and then screened across the supplied load cases.
The formulae below describe the implemented model; the source records are linked at each step.</p>
<h3>Hydrostatics and mass balance</h3>
<div class="formula">A = πD²/4; V = nA(T − h) + Vₚ; A<sub>w</sub> = nA<br>
I<sub>w</sub> = nπD⁴/64 + nAR²/2; BM = I<sub>w</sub>/V; GM = KB + BM − KG<br>
KB = [Vₚh/2 + nA(T − h)(T + h)/2]/V<br>
m = ρV; m<sub>b</sub> = m − m<sub>s</sub> − m<sub>t</sub>; KG = Σmᵢzᵢ/Σmᵢ</div>
<p>The column count is n, column diameter is D, column radius is R, draft is T, pontoon height is h,
and submerged pontoon volume is Vₚ. V is displaced volume, A is one column area, A<sub>w</sub> is
waterplane area and I<sub>w</sub> is its second moment about a horizontal centroidal axis.
KB, BM, KG and GM are centre of buoyancy, metacentric radius, centre of gravity and metacentric
height above the keel as applicable. Density is ρ. Displaced mass is m; ballast, steel and topside
masses are m<sub>b</sub>, m<sub>s</sub> and m<sub>t</sub>. A component centroid is zᵢ.
The summation symbol denotes addition over all mass components. Source: {ref('floaters')}.</p>
<p>Negative required ballast is clamped to zero and the concept is marked infeasible. The topside
centroid is a mass-weighted rotor and nacelle at hub height plus a uniform tower at its midpoint.
Ballast is placed at a fixed low fraction of draft, and the steel centroid is a specified fraction
of draft. Free-surface ballast effects are not included. Source: {ref('floaters')}.</p>
<h3>Natural periods and motion proxy</h3>
<div class="formula">T<sub>h</sub> = 2π√[m(1 + c<sub>h</sub>)/(ρgA<sub>w</sub>)]<br>
I<sub>p</sub> = Σmᵢ(zᵢ − KG)² + mR²/2<br>
T<sub>p,n</sub> = 2π√[I<sub>p</sub>(1 + c<sub>p</sub>)/(ρgVGM)]<br>
r = T<sub>n</sub>/T<sub>w</sub>; Q = 1/√[(1 − r²)² + (2ζr)²]</div>
<p>Heave and pitch natural periods are T<sub>h</sub> and T<sub>p,n</sub>. Pitch inertia is I<sub>p</sub>;
c<sub>h</sub> and c<sub>p</sub> are added-mass and added-inertia ratios; g is gravitational acceleration.
The forcing-to-natural frequency ratio is r, natural period is T<sub>n</sub>, peak wave period is
T<sub>w</sub>, damping ratio is ζ and dimensionless dynamic amplification is Q. The motion check
uses the largest heave or pitch amplification across all load cases. Wave height is stored but
is not used in this calculation. Source: {ref('floaters')} and {ref('screening')}.</p>
<p>The inertia expression above applies to the semi-submersible. The spar and barge use their own
horizontal mass-spread terms. A non-positive metacentric height produces an infinite pitch period;
the motion proxy returns a finite placeholder for non-finite periods, so feasibility and stability
are assessed separately. Tendon-stabilised periods require a separate stiffness basis. Source:
{ref('floaters')} and {ref('screening')}.</p>
<h3>Offset and aggregation</h3>
<div class="formula">x = F/k; acceptance = feasible AND all applicable checks pass</div>
<p>Steady offset is x, steady horizontal load is F and linear horizontal stiffness is k.
The mooring check is omitted when either stiffness or allowable offset is absent. The current
example supplies both. Motion and offset retain the worst load-case result; stability and modal
checks are independent of the load-case label. Source: {ref('screening')}.</p>
<p>The returned governing check is selected using the smallest raw signed margin. Metres, seconds
and dimensionless margins are mixed by that implementation; the reported governing label is not
a physically normalized utilization ranking. Each demand and limit is therefore presented separately.
Source: {ref('screening')}.</p>
"""))
    result_rows = []
    for index, result in enumerate(results):
        result_rows.append((NAMES[result.archetype.value],
                            number(f"summary.concepts.{index}.n_variants", 0),
                            number(f"summary.concepts.{index}.n_passed", 0), ref('workflow')))
    s.append(section(6, "Detailed Results", f"""
<p>The configured proxy limits are applied to every example variant. The original example geometry
is retained as the worked semi-submersible case; no input is tuned to obtain a passing result.</p>
<h3>Worked semi-submersible</h3>
<p>At the original example geometry, displacement is
{number(example_prefix + '.properties.displacement_t')} t, required ballast is
{number(example_prefix + '.properties.ballast_mass_t')} t and metacentric height is
{number(example_prefix + '.properties.GM_m')} m. Heave and pitch natural periods are
{number(example_prefix + '.properties.heave_natural_period_s')} s and
{number(example_prefix + '.properties.pitch_natural_period_s')} s. The maximum motion amplification is
{number(example_prefix + '.check.motion.value')} against a limit of
{number(example_prefix + '.check.motion.limit')}; steady offset is
{number(example_prefix + '.check.mooring.value')} m against
{number(example_prefix + '.check.mooring.limit')} m. Source: {ref('example')} and {ref('floaters')}.</p>
<p>The complete semi-submersible sweep returns {number('summary.concepts.0.n_passed', 0)} passes from
{number('summary.concepts.0.n_variants', 0)} variants. The rejected variants fail the motion proxy
at the supplied longer-period sea state. They are excluded from the shortlist pending an
independent response calculation; geometry and mass distribution are re-assessed if the motion
limit remains exceeded. Source: {ref('screening')} and the per-variant appendix.</p>
<h3>Coverage across the accompanying concepts</h3>
{table(['Concept', 'Variants (count)', 'Passes (count)', 'Source'], result_rows, 'Summary returned by the public workflow.')}
<p>Across the public example, {number('summary.total_passed', 0)} of
{number('summary.total_variants', 0)} variants meet all supplied proxy limits, including the minimum
metacentric height and dynamic amplification limit. Design acceptance is not established.
Source: {ref('workflow')} and {ref('screening')}.</p>
<p>The spar variants meet the supplied proxy criteria. The barge variants are rejected because their
natural periods occupy the excluded band and their motion amplification exceeds the supplied ceiling.
The next assessment separates the periods from that band and reduces amplification, or substantiates
a different band and damping basis with site data and hydrodynamic evidence.
A barge installation limit is not inferred. Source: {ref('screening')} and {ref('floaters')}.</p>
<p>Complete returned properties and the component demand, limit, signed margin and disposition
are provided in the appendix. A period on either band boundary is rejected even when its signed
margin is zero. Source: {ref('screening')}.</p>
"""))
    s.append(section(7, "Validation", f"""
<p>Repository evidence is classified by the quantity actually checked. Algebraic identities and
regression tests are distinguished from independent validation of a complete floating structure.</p>
<p>The existing floater tests check mass closure, the metacentric-height identity, the circular-waterplane
metacentric radius, unstable and infeasible cases, geometry rejection and broad period plausibility.
The screening tests check the optional linear offset, modal rejection and verdict aggregation.
The trade-space and reporting tests exercise ranking and rendering interfaces. Source: {ref('tests')}.</p>
<p>For the spar, the independent circular-waterplane reduction BM = D²/(16T) is compared with the
returned metacentric radius in the existing tests. For offset, the independent static spring
relation x = F/k is checked. These comparisons address algebraic quantities; the reference values
are not model-test or field measurements. Source: {ref('tests')}.</p>
<p>The committed public sizing example is a reproducible software example. Its geometry and
coefficients are not established as an exact published support-platform definition. The inspected
floater tests check algebraic relationships and plausible ranges; independent hydrodynamic and
coupled-response comparison is not established by those tests. No new independent benchmark or
measurement is introduced by this report. Source: {ref('example')}, {ref('tests')} and {ref('solver')}.</p>
<p>The report rebuild test independently calls the public screen and sizing workflow, compares every
registered example value, checks all displayed rounding and chart limits, and verifies byte-for-byte
reproduction. A changed module result changes the report. These are reproducibility checks,
not physical validation. Source: {ref('rebuild')}.</p>
"""))
    s.append(section(8, "Key Findings, Limitations and Recommendations", f"""
<p>The shortlist is conditional on the assumed input and criterion set. Further assessment is
required before a physical concept is selected for design or an installation is authorized.</p>
<h3>Key findings</h3>
<p>The workflow closes the mass balance and applies separate stability, period, amplification and
offset checks. The semi-submersible and spar results identify candidates for further work; barge
modal rejection identifies a mismatch with the supplied period band. Source: {ref('workflow')}
and {ref('screening')}.</p>
<h3>Limits of use</h3>
<p>Small-angle initial stability is represented; large heel, damaged stability, reserve buoyancy,
freeboard, steel strength, tendon capacity, ballast tank free surfaces and fatigue are not evaluated.
Added mass, inertia and damping are coefficient approximations. No wave-excitation amplitude,
directional response, aerodynamic damping or controller coupling is calculated. Source: {ref('floaters')}
and {ref('screening')}.</p>
<p>A horizontal stiffness is not checked for positivity by the screening criterion model, and an
empty load-case list is not a supported calculation basis. A positive stiffness, a non-empty case
set and an ordered finite period band are required as supplied engineering inputs before use.
Feasibility also checks ballast mass rather than ballast storage capacity. Source: {ref('screening')}.</p>
<p>Solver preparation supplies primitive meshes for spar and barge concepts, while column-stabilised
concepts require supplied meshes. Dispatch status records intent and does not establish a completed
solve. Native model loading, response-unit reconciliation and independent coupled results remain
not evaluated. Source: {ref('solver')}.</p>
<h3>Recommendations</h3>
<p>Replace assumed masses, centroids, geometry, coefficients and site cases with an agreed design
basis before a shortlist is used for concept selection. Assess ballast capacity and free surfaces,
then compare hydrostatics and natural periods with an independent calculation and measured or
published response evidence. Source: {ref('floaters')} and {ref('tests')}.</p>
<p>Retain the motion-rejected semi-submersibles and band-rejected barges outside the shortlist until
the motion and period-band exceedances are resolved by a change to geometry or by a substantiated
response and site criterion. Review each component separately; avoid interpreting the mixed-unit governing
margin as a utilization. Source: {ref('screening')}.</p>
<p>Supply a complete geometry mesh and a versioned response basis for shortlisted concepts before
coupled analysis. For installation planning, define the installation system, headings, directional
sea states and component limits, then determine the operating envelope and forecast triggers in
a separate analysis. Source: {ref('solver')} and the missing-data register.</p>
"""))
    edition = tomllib.loads((root / "pyproject.toml").read_text())["project"]["version"]
    source_rows = []
    for key, (title, path, description) in SOURCES.items():
        source_rows.append((f'<span id="source-{key}">{title}</span>',
                            f'<a href="../../../../{path.as_posix()}">{path.as_posix()}</a>',
                            html.escape(description)))
    s.append(section(9, "References", f"""
<p>The calculation basis is the repository implementation and its public sizing example.
Repository edition {html.escape(edition)} is used; coefficient provenance beyond the cited code
is not established by this report.</p>
<h3>9.1 Basis of citations</h3>
<p>Numeric inputs are read from the example mapping, constants and defaults from the module,
and results from fresh module calls. Formula coefficients are those of the cited implementation.
No standard-prescribed numerical acceptance value is asserted. A primary external reference
cited by the module was unavailable for retrieval; its contents are not adopted as calculation
or validation evidence. Independent literature comparison remains not evaluated.</p>
<h3>9.2 Source locators</h3>
{table(['Record', 'Repository locator', 'Evidence used'], source_rows, 'Source records and calculation locators.')}
<p>The public entry points for reproduction are
<code>digitalmodel.floating_wind.screening.screen_concept</code> and
<code>digitalmodel.floating_wind.workflow.FloatingWindSizingWorkflow.router</code>.
The standalone rebuild command is
<code>PYTHONPATH=src python scripts/reporting/build_floating_wind_module_report.py --output-dir &lt;directory&gt;</code>.
The default output is the report directory. The repository stylesheet is reused directly and
inlined; the requested document class is included alongside the existing style class.</p>
"""))
    input_rows = _input_rows(inputs, number)
    property_rows, check_rows = [], []
    for result in results:
        for variant in result.variants:
            name = f"{NAMES[result.archetype.value]} {int(variant.case_id.rsplit('_', 1)[1]) + 1}"
            prefix = f"variant.{result.archetype.value}.{variant.case_id}"
            for field, value in variant.params.items():
                if isinstance(value, (int, float)):
                    label, unit = FIELDS[field]
                    property_rows.append((name, label, number(f"{prefix}.params.{field}",
                                          2 if field == "column_radius" else 1), unit, ref('example')))
            for field, label, unit in PROPERTY_FIELDS:
                property_rows.append((name, label, number(f"{prefix}.properties.{field}"), unit, ref('floaters')))
            for check in variant.checks:
                label, unit = CHECKS[check.name]
                limit = (number('input.criteria.modal_band_s.0') + ' to ' +
                         number('input.criteria.modal_band_s.1')) if check.name == 'modal' else number(f"{prefix}.check.{check.name}.limit")
                check_rows.append((name, label, number(f"{prefix}.check.{check.name}.value"),
                                   limit, number(f"{prefix}.check.{check.name}.margin"), unit,
                                   "The check passes." if check.passed else "The check is rejected.", ref('screening')))
    inventory = []
    for path in sorted((root / 'tests/floating_wind').glob('test_*.py')):
        if path.name == 'test_module_report.py':
            continue
        tree = ast.parse(path.read_text())
        count = sum(isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name.startswith('test_') for node in ast.walk(tree))
        inventory.append((path.stem.removeprefix('test_').replace('_', ' ').capitalize(), str(count)))
    s.append(section(10, "Appendices", f"""
<p>The full example basis and per-component records are retained for detailed review.
The tables support numerical reproduction and do not establish engineering acceptance.</p>
<h3>Data register</h3>
{table(['Input group', 'Quantity', 'Value', 'Units', 'Basis'], input_rows, 'All example inputs and exposed default coefficients.')}
<p>The remaining internal ballast centroid and horizontal-inertia idealizations are fixed by the
floater implementation. Ballast storage, free-surface correction and measured damping are not
supplied; their physical qualification remains not evaluated. Source: {ref('floaters')}.</p>
{table(['Missing data', 'Status', 'Required disposition'], [
('Measured geometry, steel and ballast centroids, ballast capacity', 'Not evaluated.', 'Supply an agreed mass and geometry basis before design use.'),
('Site-specific wave band, damping and nonlinear mooring response', 'Not evaluated.', 'Replace assumed screening inputs with qualified site and response data.'),
('Independent platform hydrodynamic benchmark', 'Not evaluated.', 'Supply open reference data and assess each response quantity separately.'),
('Installation headings, equipment, stage loads and component criteria', 'Not evaluated.', 'Complete an installation analysis before operating limits are set.'),
], 'Missing-data register and next assessments.')}
<h3>Returned properties and per-component acceptance</h3>
<p>Variant ordinals identify the order in each concept grid. The modal demand is the period selected
by the module as nearest to, or inside, the excluded band; both periods are retained in the properties.
The full inclusive band is shown as its limit, rather than only the upper bound stored in the check.
Signed modal margin at a boundary does not override rejection. Source: {ref('screening')}.</p>
{table(['Variant', 'Property', 'Value', 'Units', 'Source'], property_rows, 'Fresh module properties for every variant.')}
{table(['Variant', 'Component', 'Demand', 'Limit', 'Signed margin', 'Units', 'Disposition', 'Source'], check_rows,
'All returned component checks. Margins have different units and are not ranked across components.')}
<h3>Quality pre-flight evidence</h3>
<p>Displayed example values and chart coordinates are bound to the returned numerical records.
The rebuild tests check numerical equality, rounding, internal links, house-style inclusion and
self-contained controls. No browser-based visual or file-picker check is claimed. Source: {ref('rebuild')}.</p>
{table(['Existing test area', 'Test functions (count)'], inventory,
'Source inventory of test functions, excluding the report checks; parameter expansion and execution counts are not inferred.')}
<p>The general engine workflow test is outside the solver-free reproduction path because its imports
include a native solver interface. The report generator calls the sizing workflow directly.
Existing test names and detailed checks are located in the source register. Source: {ref('tests')}
and {ref('workflow')}.</p>
<h3>Comments and engineering changes</h3>
<p>Select a section, enter a comment and save the JSON file beside this report. A supporting browser
opens a file picker; other browsers download a file for placement beside the report. Loading an
existing comment file restores its content. Comments are not sent over a network.</p>
<label for="comment-section">Section </label><select id="comment-section"></select>
<label for="comment-text">Comment </label><textarea id="comment-text" rows="4"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="load-comments">Load comment JSON </label><input type="file" id="load-comments" accept="application/json">
<p id="comment-message" role="status"></p><ol id="comments-list"></ol>
<label for="last-revision">Changes since revision </label><select id="last-revision">
<option value="-1">Before first issue</option><option value="0">Current issue</option></select>
<ul id="changes-list"></ul>
"""))
    charts = _charts(semi, inputs, number)
    data = {"numbers": numbers, "charts": charts}
    style = re.search(r"<style>(.*?)</style>", (root / STYLE).read_text(), re.S).group(1)
    extra = """
<style>
.hs-doc .main{padding:0}.hs-doc section{padding:32px 0}.hs-doc h3{margin:24px 0 12px}
.hs-doc td,.hs-doc th{white-space:normal;text-align:left}.hs-doc caption{text-align:left;padding:12px}
.hs-doc textarea{display:block;width:100%;max-width:65ch;margin:12px 0}
.hs-doc button,.hs-doc select,.hs-doc input{font:inherit;margin:8px 6px 8px 0;padding:6px}
.hs-doc .chart-point{cursor:pointer}.hs-doc .chart-point:focus{outline:2px solid var(--cfd)}
.hs-doc .schematic{width:100%;height:auto}.hs-doc .schematic text{font-family:var(--sans);font-size:14px}
</style>
"""
    navigation = ''.join(f'<a href="#section-{index}">{index}</a> ' for index in range(1, 11))
    payload = json.dumps(data, ensure_ascii=False, allow_nan=False).replace('<', '\\u003c')
    changes_payload = json.dumps(CHANGES, ensure_ascii=False).replace('<', '\\u003c')
    return f"""<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="Floating wind concept screening: equations, public example, evidence and engineering limits.">
<title>Floating wind concept screening analysis</title>
<style>{style}</style>{extra}</head><body class="hs-doc doc">
<header class="hero"><div class="wrap"><p class="eyebrow">Module analysis</p>
<h1>Floating wind concept screening analysis</h1><p class="lede">Draft for client review</p>
<p>Concept sizing, calculated example and limits of use.</p><nav aria-label="Report sections">{navigation}</nav></div></header>
<main class="wrap">{''.join(s)}</main>
<script id="report-data" type="application/json">{payload}</script>
<script id="report-changes" type="application/json">{changes_payload}</script>
<script>{INTERACTION}</script>
</body></html>
"""


def _input_rows(inputs, number):
    rows = []
    for key in inputs['topside']:
        label, unit = FIELDS[key]
        rows.append(('Topside', label, number(f'input.topside.{key}'), unit, 'ASSUMED - to be confirmed; ' + ref('example')))
    for index, load in enumerate(inputs['load_cases']):
        for key in ('hs_m', 'tp_s', 'steady_load_kN'):
            label, unit = FIELDS[key]
            rows.append((load['name'].capitalize(), label, number(f'input.load_cases.{index}.{key}'), unit,
                         'ASSUMED - to be confirmed; ' + ref('example')))
    for key in inputs['criteria']:
        label, unit = FIELDS[key]
        display = (number('input.criteria.modal_band_s.0') + ' to ' + number('input.criteria.modal_band_s.1')) if key == 'modal_band_s' else number('input.criteria.' + key, 2 if key == 'damping_ratio' else 1)
        rows.append(('Screening', label, display, unit, 'ASSUMED - to be confirmed; ' + ref('example')))
    for index, concept in enumerate(inputs['concepts']):
        for key in concept['base_params']:
            label, unit = FIELDS[key]
            rows.append((NAMES[concept['archetype']] + ' base', label,
                         number(f'input.concepts.{index}.base_params.{key}', 2 if key == 'column_radius' else 1),
                         unit, 'ASSUMED - to be confirmed; ' + ref('example')))
        for sweep_index, sweep in enumerate(concept['sweeps']):
            label, unit = FIELDS[sweep['name']]
            display = ', '.join(number(f'input.concepts.{index}.sweeps.{sweep_index}.values.{i}',
                                      2 if sweep['name'] == 'column_radius' else 1) for i in range(len(sweep['values'])))
            rows.append((NAMES[concept['archetype']] + ' sweep', label, display, unit,
                         'ASSUMED - to be confirmed; ' + ref('example')))
        fields = ['steel_vcg_frac', 'ca_heave', 'ca_pitch']
        if concept['archetype'] == 'barge':
            fields += ['block_coeff', 'waterplane_coeff']
        for field in fields:
            label, unit = FIELDS[field]
            rows.append((NAMES[concept['archetype']] + ' default', label,
                         number(f"default.{concept['archetype']}.{field}", 2), unit,
                         'ASSUMED - to be confirmed; ' + ref('floaters')))
    rows.append(('Shared default', 'Ballast centroid / draft', number('constant.ballast_fraction', 2),
                 'dimensionless', 'ASSUMED - to be confirmed; ' + ref('floaters')))
    return rows


def _schematic(inputs, number):
    """Draw the three-column idealization, with dimensions taken from inputs."""
    return f"""<figure class="card"><svg class="schematic" viewBox="0 0 900 420" role="img"
aria-label="Three-column semi-submersible plan and elevation with lumped topside and mooring spring">
<g fill="none" stroke="var(--ink-2)" stroke-width="2">
<path d="M180 80 L95 230 L265 230 Z" stroke-width="20" stroke="var(--hairline)"/>
<circle cx="180" cy="80" r="23"/><circle cx="95" cy="230" r="23"/><circle cx="265" cy="230" r="23"/>
<path d="M180 180 L265 230" stroke-dasharray="5 5"/>
<path d="M480 290 V205 H520 V290 M700 290 V205 H740 V290 M470 290 H750 V315 H470 Z"/>
<path d="M500 205 L600 178 L720 205 M600 178 V65 M580 178 L600 65 L620 178"/>
<circle cx="600" cy="65" r="7"/><path d="M600 65 L575 25 M600 65 L645 65 M600 65 L578 100"/>
<path d="M420 238 H820" stroke="var(--ro)"/><path d="M420 315 H820" stroke-dasharray="5 5"/>
<path d="M800 240 V315 M790 240 H810 M790 315 H810"/>
<path d="M470 310 L442 330 L426 322 L412 341 L396 333 L380 351"/>
<path d="M375 339 V369 M365 339 V369"/>
</g>
<g fill="var(--ink-2)"><text x="40" y="35">Plan: evenly spaced columns</text>
<text x="40" y="283">Column diameter: {number('input.concepts.0.base_params.column_diameter')} m</text>
<text x="40" y="308">Column radius: {number('input.concepts.0.base_params.column_radius', 2)} m</text>
<text x="40" y="333">Column count: {number('input.concepts.0.base_params.n_columns')} </text>
<text x="640" y="35">Lumped rotor and nacelle</text><text x="645" y="130">Uniform tower</text>
<text x="650" y="225">Still water</text><text x="820" y="285">Draft</text>
<text x="635" y="345">Keel datum</text><text x="350" y="395">Horizontal spring to a fixed reference</text>
<text x="600" y="375">Draft: {number('input.concepts.0.base_params.draft')} m</text>
</g></svg><figcaption>Model idealization; view dimensions identify the input geometry, not fabrication detail.
The pontoon shape, freeboard, tower taper and rotor geometry are schematic. No dimensional installation
or rigging drawing is supplied. Source: {ref('example')} and {ref('floaters')}.</figcaption></figure>"""


def _charts(result, inputs, number):
    charts = []
    for quantity, (label, unit) in CHECKS.items():
        limits = ({'stability': ['input.criteria.gm_min_m'], 'motion': ['input.criteria.daf_limit'],
                   'modal': ['input.criteria.modal_band_s.0', 'input.criteria.modal_band_s.1'],
                   'mooring': ['input.criteria.max_offset_m']})[quantity]
        points = []
        for variant in result.variants:
            prefix = f'variant.semi.{variant.case_id}'
            check = next(check for check in variant.checks if check.name == quantity)
            fields = [('Heave', f'{prefix}.properties.heave_natural_period_s'),
                      ('Pitch', f'{prefix}.properties.pitch_natural_period_s')] if quantity == 'modal' else [(label, f'{prefix}.check.{quantity}.value')]
            case_name = 'Geometry'
            if quantity == 'motion':
                periods = [('Heave', variant.properties.heave_natural_period_s),
                           ('Pitch', variant.properties.pitch_natural_period_s)]
                candidates = [(load['name'], component, _daf(period, load['tp_s'], inputs['criteria']['damping_ratio']))
                              for load in inputs['load_cases'] for component, period in periods]
                case_name, component, _ = max(candidates, key=lambda candidate: candidate[2])
                fields = [(component + ' amplification', f'{prefix}.check.motion.value')]
            elif quantity == 'mooring':
                case_name = max(inputs['load_cases'], key=lambda load: load['steady_load_kN'])['name']
            for component, demand in fields:
                points.append({'x': f'{prefix}.params.column_diameter',
                               'radius': f'{prefix}.params.column_radius', 'demand': demand,
                               'case': f"Variant {int(variant.case_id.rsplit('_', 1)[1]) + 1}",
                               'component': component, 'units': unit, 'passed': check.passed,
                               'load_case': case_name.capitalize()})
            # Chart coordinates share the same numeric records as the appendix.
            for field in (f'{prefix}.params.column_diameter', f'{prefix}.params.column_radius'):
                number(field, 2 if field.endswith('column_radius') else 1)
        charts.append({'quantity': quantity, 'label': label, 'units': unit,
                       'limits': limits, 'points': points})
    return charts


INTERACTION = r"""
(() => {
  'use strict';
  const byId = id => document.getElementById(id);
  const data = JSON.parse(byId('report-data').textContent);
  const changes = JSON.parse(byId('report-changes').textContent);
  const value = key => data.numbers[key].value;
  const format = key => value(key).toLocaleString('en-US', {
    minimumFractionDigits: data.numbers[key].digits,
    maximumFractionDigits: data.numbers[key].digits
  }).replace('-', '−');
  const element = (tag, text, parent) => {
    const item = document.createElement(tag);
    if (text !== undefined) item.textContent = text;
    if (parent) parent.appendChild(item);
    return item;
  };
  const svgElement = (tag, attributes, text, parent) => {
    const item = document.createElementNS('http://www.w3.org/2000/svg', tag);
    Object.entries(attributes).forEach(([key, v]) => item.setAttribute(key, String(v)));
    if (text !== undefined) item.textContent = text;
    parent.appendChild(item);
    return item;
  };
  function draw() {
    const chart = data.charts.find(chart => chart.quantity === byId('chart-select').value);
    const host = byId('chart-host');
    host.replaceChildren();
    const svg = svgElement('svg', {viewBox: '0 0 850 420', role: 'img',
      'aria-label': chart.label + ' against column diameter'}, undefined, host);
    const xs = chart.points.map(point => value(point.x));
    const ys = chart.points.map(point => value(point.demand)).concat(chart.limits.map(value));
    const xmin = Math.min(...xs), xmax = Math.max(...xs), ymin = Math.min(0, ...ys), ymax = Math.max(...ys) * 1.15;
    const xp = x => 75 + (x - xmin) / (xmax - xmin || 1) * 650;
    const yp = y => 340 - (y - ymin) / (ymax - ymin || 1) * 280;
    svgElement('path', {d: 'M75 50 V340 H760', class: 'axis', fill: 'none'}, undefined, svg);
    const xKeys = [...new Map(chart.points.map(point => [value(point.x), point.x])).values()];
    xKeys.forEach(key => svgElement('text', {x: xp(value(key)), y: 365, 'text-anchor': 'middle'}, format(key), svg));
    svgElement('text', {x: 350, y: 400, class: 'axtitle'}, 'Column diameter (m)', svg);
    svgElement('text', {x: 75, y: 25, class: 'axtitle'}, chart.label + ' (' + chart.units + ')', svg);
    chart.limits.forEach(key => {
      svgElement('path', {d: `M75 ${yp(value(key))} H760`, stroke: 'var(--proj)', 'stroke-width': 2, 'stroke-dasharray': '6 4'}, undefined, svg);
      svgElement('text', {x: 765, y: yp(value(key))}, 'Limit ' + format(key), svg);
    });
    const groups = [...new Set(chart.points.map(point => value(point.radius) + point.component))];
    const palette = ['var(--cfd)', 'var(--ro)', 'var(--proj)', 'var(--ink-2)'];
    groups.forEach((group, index) => {
      const points = chart.points.filter(point => value(point.radius) + point.component === group).sort((a,b) => value(a.x)-value(b.x));
      svgElement('polyline', {points: points.map(point => `${xp(value(point.x))},${yp(value(point.demand))}`).join(' '), fill: 'none', stroke: palette[index % palette.length]}, undefined, svg);
    });
    chart.points.forEach(point => {
      const state = point.passed ? 'The check passes.' : 'The check is rejected.';
      const tip = `${point.case}; ${point.component}; ${point.load_case}; column radius ${format(point.radius)} m; demand ${format(point.demand)} ${point.units}; limit ${chart.limits.map(format).join(' to ')} ${point.units}. ${state}`;
      const color = point.passed ? 'var(--good)' : 'var(--proj)';
      const dot = svgElement('circle', {cx: xp(value(point.x)), cy: yp(value(point.demand)), r: 7,
        fill: color, stroke: 'var(--ink)', tabindex: 0, class: 'chart-point', 'aria-label': tip}, undefined, svg);
      svgElement('title', {}, tip, dot);
      ['pointerenter', 'focus', 'click'].forEach(event => dot.addEventListener(event, () => { byId('chart-detail').textContent = tip; }));
    });
    byId('chart-detail').textContent = 'Green: the check passes. Amber: the check is rejected. Dashed lines: supplied limits. Hover, focus or touch a point for case details.';
  }
  byId('chart-select').addEventListener('change', draw);
  draw();
  const sections = [...document.querySelectorAll('main > section')];
  sections.forEach(section => {
    const option = element('option', section.querySelector('h2').textContent, byId('comment-section'));
    option.value = section.id;
  });
  let comments = {report: changes.report, revision: changes.revisions[0].revision, comments: []};
  function showComments() {
    byId('comments-list').replaceChildren();
    comments.comments.forEach(comment => element('li', comment.section + ': ' + comment.text, byId('comments-list')));
  }
  byId('add-comment').addEventListener('click', () => {
    const text = byId('comment-text').value.trim();
    if (!text) { byId('comment-message').textContent = 'Enter a comment before adding it.'; return; }
    comments.comments.push({section: byId('comment-section').value, text});
    byId('comment-text').value = '';
    showComments();
    byId('comment-message').textContent = 'Comment added. Save the JSON file to retain comments.';
  });
  byId('save-comments').addEventListener('click', async () => {
    const blob = new Blob([JSON.stringify(comments, null, 2) + '\n'], {type: 'application/json'});
    try {
      if ('showSaveFilePicker' in window) {
        const file = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
        const stream = await file.createWritable();
        await stream.write(blob); await stream.close();
        byId('comment-message').textContent = 'Comments saved to the selected file.';
      } else {
        const link = element('a'); const url = URL.createObjectURL(blob);
        link.href = url; link.download = 'report.comments.json'; link.click(); URL.revokeObjectURL(url);
        byId('comment-message').textContent = 'Comment JSON downloaded. Place it beside the report.';
      }
    } catch (error) {
      byId('comment-message').textContent = error.name === 'AbortError' ? 'Save cancelled; comments remain open.' : 'Save failed; comments remain open. Try saving again.';
    }
  });
  byId('load-comments').addEventListener('change', async event => {
    const file = event.target.files[0]; if (!file) return;
    try {
      if (file.size > 5000000) throw new Error('File too large');
      const loaded = JSON.parse(await file.text());
      if (loaded.report !== comments.report || loaded.revision !== comments.revision || !Array.isArray(loaded.comments) ||
          loaded.comments.some(item => !item || typeof item.text !== 'string' || !sections.some(section => section.id === item.section))) {
        throw new Error('Mismatched comment file');
      }
      comments.comments.push(...loaded.comments.filter(item => !comments.comments.some(old => old.section === item.section && old.text === item.text)));
      showComments(); byId('comment-message').textContent = 'Matching comments loaded; existing comments retained.';
    } catch (error) { byId('comment-message').textContent = 'Comment file is invalid or belongs to a different report revision.'; }
  });
  function showChanges() {
    byId('changes-list').replaceChildren();
    const revisions = changes.revisions.filter(row => Number(row.revision) > Number(byId('last-revision').value));
    revisions.forEach(row => row.changes.forEach(change => element('li', change, byId('changes-list'))));
    if (!revisions.length) element('li', 'No engineering changes since the selected revision.', byId('changes-list'));
  }
  byId('last-revision').addEventListener('change', showChanges); showChanges();
})();
"""


def build_report(root, output_directory):
    """Write the HTML and change-panel sidecar to a caller-selected directory."""
    document = render_report(Path(root))
    output_directory = Path(output_directory)
    output_directory.mkdir(parents=True, exist_ok=True)
    output = output_directory / "report.html"
    output.write_text(document, encoding="utf-8")
    (output_directory / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8")
    return output


def main():
    """Build the report in its repository directory or an explicit destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path)
    arguments = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    build_report(root, arguments.output_dir or root / REPORT_DIRECTORY)


if __name__ == "__main__":
    main()
