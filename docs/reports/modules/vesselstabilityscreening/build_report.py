# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Build the self-contained stability report; calculations write only to scratch.

Rebuild: PYTHONPATH=src python docs/reports/modules/vesselstabilityscreening/build_report.py
         --scratch <temporary-directory>
"""

from __future__ import annotations

import argparse
import ast
import copy
import hashlib
import html
import json
import re
from pathlib import Path

from digitalmodel.naval_architecture.vessel_stability_screening import gz_from_kn
from digitalmodel.vessel_stability_screening import router


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
CALCULATIONS = (
    "src/digitalmodel/vessel_stability_screening/workflow.py",
    "src/digitalmodel/naval_architecture/vessel_stability_screening.py",
    "src/digitalmodel/naval_architecture/damage_stability_screening.py",
    "src/digitalmodel/naval_architecture/damage_stability.py",
)
TESTS = (
    "test_vessel_stability_screening.py",
    "test_vessel_stability_screening_workflow.py",
    "test_damage_stability_screening.py",
    "test_vessel_stability_screening_damage_workflow.py",
)
CHANGES = {
    "revision": "A",
    "date": "2026-10-22",
    "changes": [
        "Initial issue: the intact screening method, public cross-curve example and synthetic loading case are documented.",
        "The effects of free surface and the limits of table-based acceptance are distinguished from operational approval.",
    ],
}


def lookup(data: dict, locator: str):
    """Resolve a report numeric binding."""
    value = data
    for part in locator.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def format_value(value: float, digits: int) -> str:
    """Format displayed values without hiding a nonzero sign."""
    text = f"{value:.{digits}f}"
    if value and float(text) == 0:
        bound = f"{10 ** -digits:.{digits}f}"
        return "<" + bound if value > 0 else ">−" + bound
    return text.replace("-", "−")


def house_style() -> str:
    """Inline the existing report palette and layout without a new theme."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    css = re.sub(r"/\*.*?\*/", "", brand + "\n" + layout, flags=re.S)
    css = "\n".join(line.rstrip() for line in css.splitlines())
    return f"<style>\n{css}\n</style>"


def indexed_tests() -> list[dict]:
    """Count test definitions, separately from execution evidence."""
    rows = []
    for name in TESTS:
        path = ROOT / "tests/naval_architecture" / name
        tree = ast.parse(path.read_text(encoding="utf-8"))
        count = sum(isinstance(n, ast.FunctionDef) and n.name.startswith("test_") for n in ast.walk(tree))
        rows.append({"path": str(path.relative_to(ROOT)), "count": count})
    return rows


def calculate(scratch: Path) -> dict:
    """Call the documented API and the routed workflow on explicit inputs."""
    scratch = scratch.resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the repository")
    sources = json.loads((HERE / "sources.json").read_text(encoding="utf-8"))
    supplied = sources["synthetic"]

    def routed(label: str, fsm: float) -> dict:
        settings = {
            "vessel": {"lbp_m": supplied["lbp_m"], "deck_edge_immersion_deg": supplied["deck_edge_immersion_deg"]},
            "hydrostatics": copy.deepcopy(supplied["hydrostatics"]),
            "loading_condition": copy.deepcopy(supplied["loading_condition"]),
            "gz": copy.deepcopy(supplied["gz"]),
            "max_kg": {"enabled": True, "tolerance_m": supplied["max_kg_tolerance_m"]},
            "output_dir": str(scratch / label),
        }
        settings["loading_condition"]["items"][2]["fsm_t_m"] = fsm
        cfg = router({"basename": "vessel_stability_screening", "vessel_stability_screening": settings})
        result = cfg["vessel_stability_screening"]
        # Output locations and the standard workflow prose are not report data.
        return {key: value for key, value in result.items()
                if key != "output_dir" and not key.endswith("_csv") and key != "governance"}

    intact = routed("reference", supplied["loading_condition"]["items"][2]["fsm_t_m"])
    excess = routed("excess-free-surface", supplied["excess_fsm_t_m"])
    pressed = routed("zero-free-surface", 0.0)
    public = copy.deepcopy(sources["public_example"])
    kn_m = [x * public["m_per_ft"] for x in public["kn_ft"]]
    kg_m = public["kg_ft"] * public["m_per_ft"]
    curve = gz_from_kn(public["heel_deg"], kn_m, kg_m)
    public.update(kn_m=kn_m, kg_m=kg_m, gz_m=list(curve.gz_m))
    # Percentage difference uses unrounded calculated and published values.
    public["difference_pct"] = [
        None if ref == 0 else 100 * (calc / (ref * public["m_per_ft"]) - 1)
        for calc, ref in zip(curve.gz_m, public["published_gz_ft"])
    ]
    tests = indexed_tests()
    return {
        "intact": intact, "excess": excess, "public": public,
        "sensitivity": [{"fsc_m": r["fsc_m"], "gm_fluid_m": r["gm_fluid_m"]} for r in (pressed, intact, excess)],
        "geometry": {k: supplied[k] for k in ("lbp_m", "beam_m", "depth_m", "deck_edge_immersion_deg", "water_density_t_m3")},
        "tests": tests, "test_count": sum(row["count"] for row in tests),
        "calculation_sources": {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest() for p in CALCULATIONS},
    }


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render cells supplied as escaped text or bound markup."""
    return "<table><thead><tr>" + "".join(f"<th>{html.escape(x)}</th>" for x in headers) + \
        "</tr></thead><tbody>" + "".join("<tr>" + "".join(f"<td>{x}</td>" for x in row) + "</tr>" for row in rows) + "</tbody></table>"


def rounded(value):
    """Bound embedded arrays to six decimal places across platforms."""
    if isinstance(value, dict):
        return {k: rounded(v) for k, v in value.items()}
    if isinstance(value, list):
        return [rounded(v) for v in value]
    return round(value, 6) if isinstance(value, float) else value


def render(data: dict) -> str:
    """Write the report narrative with all example numbers bound to API data."""
    def n(path: str, digits: int = 4) -> str:
        value = html.escape(format_value(lookup(data, path), digits))
        return f'<span data-value="{path}" data-digits="{digits}">{value}</span>'

    def ref(number: int, label: str = "Source") -> str:
        return f'<a href="#ref-{number}">{label}</a>'

    input_rows = []
    for i, item in enumerate(data["intact"]["loading_condition"]["items"]):
        p = f"intact.loading_condition.items.{i}."
        input_rows.append([html.escape(item["name"]), n(p + "weight_t", 0), n(p + "vcg_m", 1), n(p + "lcg_m", 1),
                           n(p + "fsm_t_m", 0) if "fsm_t_m" in item else "None applied", ref(4)])
    input_table = table(["Item", "Mass (t)", "Height (m)", "Position from aft perpendicular (m)", "Free-surface moment (t m)", "Source"], input_rows)
    hydro_rows = []
    for i, row in enumerate(data["intact"]["hydrostatics"]["table"]):
        p = f"intact.hydrostatics.table.{i}."
        hydro_rows.append([n(p + "draft_m", 1), n(p + "displacement_t", 0), n(p + "km_m"), n(p + "lcb_m", 1), n(p + "lcf_m", 1), n(p + "mct_t_m_per_cm"), ref(4)])
    hydro_table = table(["Draft (m)", "Displacement (t)", "KM (m)", "LCB (m)", "LCF (m)", "MCT (t m/cm)", "Source"], hydro_rows)
    results = []
    for key, label, unit in [
        ("displacement_t", "Displacement", "t"), ("draft_m", "Table draft", "m"),
        ("kg_m", "Solid centre of gravity", "m"), ("fsc_m", "Free-surface rise", "m"),
        ("kg_fluid_m", "Fluid centre of gravity", "m"), ("km_m", "Metacentre above keel", "m"),
        ("gm_fluid_m", "Fluid metacentric height", "m"), ("trim_m", "Trim by stern", "m"),
        ("governing_kg_limit_m", "Limiting fluid centre of gravity", "m"),
    ]:
        results.append([label, n("intact." + key, 0 if unit == "t" else 4), unit, "Routed calculation"])
    result_table = table(["Quantity", "Value", "Unit", "Source"], results)
    criteria = []
    for i, row in enumerate(data["intact"]["criteria_results"]):
        p = f"intact.criteria_results.{i}."
        criteria.append([html.escape(row["description"]).replace("deg", "degrees"), n(p + "value"), n(p + "required"),
                         html.escape(row["unit"]).replace("m*rad", "m rad").replace("deg", "degrees"), "The criterion is met." if row["status"] == "pass" else "The criterion is not met.", ref(1)])
    criterion_table = table(["Criterion", "Calculated", "Minimum", "Unit", "Status", "Source"], criteria)
    public_rows = []
    for i in range(len(data["public"]["heel_deg"])):
        p = f"public."
        public_rows.append([n(p + f"heel_deg.{i}", 0), n(p + f"kn_ft.{i}", 2), n(p + f"kn_m.{i}", 6), n(p + f"gz_m.{i}"),
                            n(p + f"published_gz_ft.{i}", 2), "Not defined at the zero reference" if data["public"]["difference_pct"][i] is None else n(p + f"difference_pct.{i}", 1), ref(2, "Course example")])
    public_table = table(["Heel (degrees)", "Published cross-curve arm (ft)", "Cross-curve arm (m)", "Calculated righting arm (m)", "Published corrected arm (ft)", "Difference from published corrected arm (%)", "Source"], public_rows)
    kn_rows = [[n(f"intact.gz.heel_deg.{i}", 0), n(f"intact.gz.kn_m.{i}", 6), n(f"intact.gz_curve_result.{i}.gz_m"), ref(4)] for i in range(len(data["intact"]["gz"]["heel_deg"]))]
    kn_table = table(["Heel (degrees)", "Supplied KN (m)", "Calculated GZ (m)", "Source"], kn_rows)
    tests_table = table(["Coverage", "Test definitions", "Source"], [[label, n(f"tests.{i}.count", 0), '<a href="#records">Calculation tests</a>'] for i, label in enumerate(["Intact calculation", "Intact workflow", "Damage calculation", "Damage workflow"])])
    kg_rows = [[html.escape(row["description"]), n(f"intact.kg_limits.{i}.kg_limit_m"), "The search is capped at KM." if row["limited_by_km"] else "The criterion limits the search.", ref(1)] for i, row in enumerate(data["intact"]["kg_limits"])]
    kg_table = table(["Criterion", "Fluid KG limit (m)", "Interpretation", "Source"], kg_rows)
    sources = json.loads((HERE / "sources.json").read_text(encoding="utf-8"))
    refs = "".join(f'<li id="ref-{i}">{html.escape(row["title"])}; {html.escape(row["edition"])}; {html.escape("; ".join(row["locators"]))}. <a href="{html.escape(row["url"], quote=True)}">Open source</a>. Accessed {row["retrieved_utc"][:10]}.</li>' for i, row in enumerate(sources["public_sources"], 1))
    refs += '<li id="ref-4">Public synthetic loading and cross-curve fixture; current repository edition. The input data are listed in the appendix; implementation records are listed below.</li>'
    records = "".join(f'<li><code>{html.escape(p)}</code></li>' for p in CALCULATIONS)
    records += "".join(f'<li><code>{html.escape(row["path"])}</code></li>' for row in data["tests"])
    records += '<li><code>docs/domains/ship-design/vessel-stability-screening.md</code>: documented workflow and public synthetic example.</li>'
    records += '<li><code>docs/api/cfd/turbulent-flat-plate-verification.html</code> and <code>docs/api/_assets/brand.css</code>: stylesheet exemplar.</li>'
    records += '<li><code>sources.json</code>: extracted inputs, editions, clauses, retrieval status and source-response digests.</li>'
    changes = "".join(f"<li>{html.escape(text)}</li>" for text in CHANGES["changes"])
    body = f"""
<header><div class="kicker">Marine stability · Module analysis</div>
<h1>Vessel stability screening</h1><p>Draft for maintainer review</p></header>
<nav aria-label="Report sections"><a href="#executive">Executive summary</a> · <a href="#method">Methodology</a> · <a href="#results">Detailed results</a> · <a href="#validation">Validation</a> · <a href="#recommendations">Recommendations</a> · <a href="#appendix">Appendix</a> · <a href="#comments">Comments</a></nav>

<section id="executive"><h2>1. Executive Summary</h2>
<p>Vessel stability screening is a table-based calculation of loading equilibrium and righting-arm criteria. Analysis is performed to determine the effect of loading and free surface on the supplied stability data; key results are summarized below.</p>
<ul><li>The synthetic reference condition meets the evaluated general intact criteria.
<ul><li>The fluid metacentric height is {n('intact.gm_fluid_m')} m against the general minimum of {n('intact.criteria_results.5.required', 2)} m. The result applies to supplied synthetic cross-curves, including their nonphysical extension beyond deck-edge immersion.</li></ul></li>
<li>The public cross-curve example is calculated from its published inputs.
<ul><li>The righting arm at {n('public.heel_deg.5', 0)} degrees is {n('public.gz_m.5')} m. No downflooding angle or operational sea-state limit is established by that example.</li></ul></li>
<li>The excessive-free-surface condition does not meet the initial stability criterion.
<ul><li>The fluid metacentric height is {n('excess.gm_fluid_m')} m. Slack-tank exposure is to be reduced and the loading condition recalculated before acceptance, as required by the free-surface basis in <a href="#ref-1">the Code</a>.</li></ul></li></ul>
<h3>Revision history</h3>
{table(['Revision', 'Date', 'What changed and why'], [['A', '2026-10-22', 'Initial issue: the calculation basis and evidence limits are documented to support engineering use.']])}
</section>

<section id="introduction"><h2>2. Introduction</h2>
<p>Loading conditions are screened using supplied hydrostatic and cross-curve tables. Geometry-based hydrostatics and an approved stability booklet are not produced by this module.</p>
<p>The documented workflow accepts a configuration mapping and returns its results with comma-separated-value tables. The underlying calculation functions also accept typed weight items, hydrostatic rows and righting-arm arrays. The main intact workflow is covered here; the public example exercises the cross-curve correction directly.</p>
</section>

<section id="summary"><h2>3. Summary</h2>
<p>The controls needed for a loading decision are identified from the supplied data and evaluated criteria. An operational envelope is not established by the static screen.</p>
<p>Mass, centres of gravity, tank moments, table displacement range, opening immersion angles and criterion applicability are to be established before interpreting acceptance. Heading, significant wave height, peak period, crane dynamics and forecast support are NOT_EVALUATED; no sea-state or heading limit is inferred.</p>
<figure><svg id="sensitivity-chart" viewBox="0 0 720 300" role="img" aria-label="Fluid metacentric height against free-surface correction"></svg>
<figcaption>Fluid metacentric height against free-surface correction at constant loading mass. The general initial-stability minimum is drawn. Select a point with the control to inspect the demand and limit.</figcaption></figure>
<label for="sensitivity-select">Free-surface case</label><input id="sensitivity-select" type="range" min="0" max="2" value="1" step="1"><p id="sensitivity-readout" aria-live="polite"></p>
<p>The initial metacentric-height criterion alone does not establish acceptance of every righting-arm criterion. The excessive-free-surface condition fails the combined intact screen; reduce free-surface moments, then repeat every applicable check. <a href="#ref-1">Code, Part B, free surfaces</a>.</p>
</section>

<section id="basis"><h2>4. Design Data</h2>
<p>The physical system is represented by a loading inventory and supplied stability tables. The public example and synthetic reference condition are separate data sets.</p>
<h3>4.1 Physical system and assumptions</h3>
<p>The synthetic geometry has length {n('geometry.lbp_m', 0)} m, beam {n('geometry.beam_m', 0)} m and depth {n('geometry.depth_m', 0)} m. Water density is a supplied input of {n('geometry.water_density_t_m3', 3)} t/m³. These data are assumed, based on previous experience; ASSUMED - to be confirmed for physical use. <a href="#ref-4">Synthetic fixture</a>.</p>
<figure><svg viewBox="0 0 720 280" role="img" aria-label="Synthetic wall-sided hull section showing keel, centre of gravity, buoyancy and upright waterline">
<path d="M180 35 L180 220 L540 220 L540 35" fill="none" stroke="var(--brand)" stroke-width="3"/>
<path d="M181 147 H539 V219 H181 Z" fill="var(--panel-2)"/>
<path d="M80 147 H640 M360 20 V250" fill="none" stroke="var(--muted)" stroke-dasharray="6 4"/>
<path d="M360 205 V163 M360 65 V111" stroke="var(--brand)" stroke-width="3"/>
<path d="M353 174 L360 163 L367 174 M353 100 L360 111 L367 100" fill="none" stroke="var(--brand)" stroke-width="2"/>
<circle cx="360" cy="111" r="4" fill="var(--ink)"/><circle cx="360" cy="184" r="4" fill="var(--ink)"/>
<text x="380" y="113">G: centre of gravity; weight downward</text><text x="380" y="188">B: centre of buoyancy; force upward</text>
<text x="85" y="138">Upright waterline</text><text x="315" y="245">K: keel datum</text>
<text x="182" y="270">Transverse section; beam and depth given above</text></svg>
<figcaption>The wall-sided section is schematic. Static equilibrium is imposed with weight and buoyancy aligned; dynamic waves, moorings and rigging are absent. The synthetic fixture is not an as-built drawing.</figcaption></figure>
<h3>4.2 Inputs, outputs and criteria</h3>
<p>Inputs use metres, tonnes and degrees; righting-arm areas use metre-radians. Longitudinal positions are measured forward from the aft perpendicular. Trim is positive by the stern. The keel datum is used here for vertical heights; all supplied data require a common datum.</p>
<p>Hydrostatic rows require draft, displacement and metacentre height. Longitudinal buoyancy and flotation centres and the moment to change trim enable end-draft estimates. Weight items require mass and vertical and longitudinal centres; tank free-surface moments are supplied directly or calculated from rectangular dimensions and density. A supplied righting-arm table is already corrected; cross-curves require correction for the fluid centre of gravity.</p>
<p>Returned outputs include total displacement, solid and fluid gravity centres, mean draft, metacentric height, trim, criterion values and margins, righting-arm samples and limiting fluid gravity-centre values. Conditional outputs cover lifting and damage. The overall status includes only configured checks; absent analyses do not become accepted results.</p>
<p>The general intact criteria are used as a calculation demonstration, with the minima shown in Detailed Results and their basis in the public Code. Applicability to a vessel type is not established. Optional weather pressure, lifting and damage criteria remain required cited inputs when those analyses are requested. <a href="#ref-1">Code, Part A, righting-lever properties</a>.</p>
</section>

<section id="method"><h2>5. Methodology</h2>
<p>Weight moments are summed, hydrostatics are interpolated and free-surface correction is applied before the stability criteria are evaluated. Numerical acceptance is reported for the supplied tables.</p>
<h3>5.1 Loading and equilibrium</h3>
<div class="eq">W = Σwᵢ; KG = Σ(wᵢzᵢ)/W; LCG = Σ(wᵢxᵢ)/W.</div>
<p>W is displacement mass, wᵢ is an item mass, zᵢ is its height above keel and xᵢ is its position from the aft perpendicular. KG and LCG are the resulting vertical and longitudinal gravity centres. These are mass-moment balances. <a href="#ref-2">Course Notes, Summer 2020, section 3.2.2.1</a>.</p>
<div class="eq">FSM = ρₜlb³/12; FSC = ΣFSM/W; KGfluid = KG + FSC; GMfluid = KM − KGfluid.</div>
<p>FSM is free-surface moment; ρₜ is tank-fluid density, l is free-surface length and b is transverse breadth. FSC is the virtual gravity-centre rise. KM is metacentre height above keel; GMfluid is its height above the fluid gravity centre, also called initial fluid metacentric height, GM0. The divisor follows from the second moment of a rectangular free surface. <a href="#ref-2">Course Notes, Summer 2020, sections 4.8.3–4.8.3.1</a>.</p>
<div class="eq">q = q₁ + (W − W₁)(q₂ − q₁)/(W₂ − W₁); trim = W(LCB − LCG)/(100 MCT).</div>
<p>q is any interpolated hydrostatic property between rows indexed by masses W₁ and W₂. LCB and LCF denote the longitudinal buoyancy and flotation centres. MCT is moment to change trim per centimetre; the factor converts centimetres to metres. End drafts distribute trim about LCF over the length between perpendiculars. Interpolation is restricted to the tabulated range. <a href="#ref-2">Course Notes, Summer 2020, sections 1.6 and 3.6.2</a>.</p>
<h3>5.2 Righting arms and limiting gravity centre</h3>
<div class="eq">GZ(φ) = KN(φ) − KGfluid sin φ; A = Σ[(GZᵢ + GZᵢ₊₁)/2](φᵢ₊₁ − φᵢ).</div>
<p>φ is heel angle, GZ is the righting arm, KN is the cross-curve arm relative to the keel reference and A is area under the curve. Angles in the integration are radians. The area expression is derived by integrating each straight-line segment exactly. Piecewise linear interpolation and trapezoidal integration are used. <a href="#ref-2">Course Notes, Summer 2020, sections 1.6, 2.8 and 4.5</a>.</p>
<p>The limiting fluid gravity centre is searched by bisection between the keel and the metacentre for each configured criterion. The lowest finite limit governs; an infeasible criterion remains a failure, and a result capped at the metacentre is identified. The synthetic example uses search tolerance {n('intact.max_kg.tolerance_m')} m as a numerical input, not an engineering allowance. The criteria are sourced to the <a href="#ref-1">Code</a>; bisection is an implementation method.</p>
</section>

<section id="results"><h2>6. Detailed Results</h2>
<p>The main intact workflow is calculated for the public synthetic reference fixture. A separate published cross-curve calculation is then performed through the module function.</p>
<h3>6.1 Routed intact condition</h3>
{result_table}
<p>The supplied KN table is synthetic and extends beyond the geometric deck-edge immersion of {n('geometry.deck_edge_immersion_deg', 2)} degrees. Its extended righting arms are numerical test inputs; acceptance of those inputs is not evidence of stability at large heel. Downflooding is NOT_EVALUATED because an opening inventory is absent. <a href="#ref-4">Synthetic fixture</a>.</p>
{criterion_table}
<p>The limiting value above is the fluid gravity-centre limit for this supplied curve and displacement. A permissible solid gravity centre requires deduction of the condition-specific free-surface rise. The full criterion breakdown is provided in the appendix.</p>
<h3>6.2 Published cross-curve worked example</h3>
<article id="worked-example">
<p>The published tabulated cross-curve arms are passed to the module after unit conversion. The published gravity-centre height is {n('public.kg_ft', 2)} ft, equivalent to {n('public.kg_m', 6)} m. The exact length conversion used is {n('public.m_per_ft')} m/ft. <a href="#ref-2">Course example</a>; <a href="#ref-3">unit conversion source</a>.</p>
<p>At heel {n('public.heel_deg.5', 0)} degrees, the published cross-curve arm is {n('public.kn_ft.5', 2)} ft. Subtracting the sine correction and converting to metres gives {n('public.gz_m.5')} m. This is the headline result calculated from the displayed inputs.</p>
<p>The complete input and comparison table is provided in the appendix. Differences are calculated against the published rounded corrected arms and increase in percentage magnitude near a zero righting arm. The published input gravity centre is used without replacement by rounded intermediate corrections. Negative arms at the largest heel indicate a restoring-moment reversal in the supplied curve; that range is excluded from any positive-stability interpretation. A flooding limit, full loading inventory and opening geometry are not supplied for this calculation, so operational acceptance is NOT_EVALUATED. <a href="#ref-2">Course example and stability interpretation</a>.</p>
</article>
<figure><svg id="public-chart" viewBox="0 0 720 300" role="img" aria-label="Published example righting arm against heel angle"></svg>
<figcaption>Calculated righting arm against heel for the public example. The zero-arm line marks the sign change. It is not an approved operating-heel limit; downflooding remains NOT_EVALUATED.</figcaption></figure>
<label for="public-select">Published example heel sample</label><input id="public-select" type="range" min="0" max="13" value="5" step="1"><p id="public-readout" aria-live="polite"></p>
</section>

<section id="validation"><h2>7. Validation</h2>
<p>Calculation evidence is provided by existing arithmetic and workflow tests and by the report's hand anchors and consistency checks. Field validation and agreement with an approved stability analysis are not established.</p>
<p>The existing suite contains {n('test_count', 0)} test definitions across the calculation and workflow areas listed below. Counts are indexed from test definitions and are not execution receipts. Coverage includes hydrostatic interpolation, loading moments, rectangular free surface, trim, righting-arm integration, limiting gravity centre, missing citations and damage-condition edge cases.</p>
{tests_table}
<p>The report tests separately assert mass conservation, virtual gravity-centre rise, fluid metacentric height and published righting-arm values against hand-worked literals. Consistency checks compare displayed numbers with fresh module calls and compare a rebuilt page with the committed page. These checks do not establish statutory applicability or field performance.</p>
<p>The public corrected-arm table has rounded intermediate sine corrections. Percentage differences are shown against that table rather than described as disagreement with a physical measurement. A future validation set requires approved hydrostatic tables, a loading survey, tank state records and matching opening limits. <a href="#ref-1">Code, stability information and free surfaces</a>.</p>
</section>

<section id="recommendations"><h2>8. Key Findings, Limitations and Recommendations</h2>
<p>The evaluated criteria are met by the synthetic reference inputs. Use is limited to static screening of supplied tables and does not authorize a loading or installation operation.</p>
<h3>8.1 Findings and actions</h3>
<p>The excessive-free-surface condition has fluid metacentric height {n('excess.gm_fluid_m')} m against {n('excess.criteria_results.5.required', 2)} m minimum and fails the initial-stability check. Reduce the sum of free-surface moments by an approved tank-management arrangement, update weights and centres, and repeat every applicable criterion before acceptance. Tank subdivision and accurate tank-state correction are addressed in <a href="#ref-2">the Course Notes, minimizing free-surface effects</a> and <a href="#ref-1">the Code, free surfaces</a>.</p>
<p>The public curve becomes negative at large heel. Keep any operating interpretation within approved opening and stability limits; obtain the downflooding information before establishing that range. The sign change is shown for numerical interpretation only. <a href="#ref-2">Course Notes, range of stability</a>; <a href="#ref-1">Code, righting-lever properties</a>.</p>
<p>Hydrostatic lookup rejects extrapolation, but the shared righting-arm interpolator holds endpoint values outside its supplied angular range. Supply curves spanning every requested evaluation angle and reject truncated curves before screening. The maximum-arm angle is taken from tabulated samples, so angular discretization affects that criterion and the limiting gravity-centre result.</p>
<h3>8.2 Not covered</h3>
<p>Weather, crane lifting, added-weight flooding, direct damaged hydrostatics and damaged righting-arm criteria are available sub-areas but are not calculated in this report. The added-weight route reuses intact hydrostatics with a free-surface approximation; it is not a geometry-based lost-buoyancy solution. Waves, response operators, sea-state envelopes, progressive flooding, rigging, transient sloshing and field comparisons are NOT_EVALUATED.</p>
<p>The method accepts externally supplied numerical data without establishing their physical provenance. Complete finite-value screening, datum reconciliation, criterion applicability, survey uncertainty and angular convergence are to be reviewed before engineering acceptance. The general criterion set alone does not establish full compliance with the Code.</p>
</section>

<section id="references"><h2>9. References and Implementation Records</h2>
<p>The technical basis is drawn from open institutional sources and the public calculation fixtures. Source response records and extracted inputs are supplied with the report.</p>
<h3>9.1 Technical references</h3><ol class="refs">{refs}</ol>
<h3 id="records">9.2 Implementation and evidence locators</h3>
<p>The public application programming interface is <code>digitalmodel.vessel_stability_screening.router</code>; direct functions are provided by <code>digitalmodel.naval_architecture.vessel_stability_screening</code>. The published example calls <code>gz_from_kn</code>. The intact cases call <code>router</code>, including its table exports. No external solver is invoked.</p>
<ul>{records}</ul>
<p>The report generator is <code>build_report.py</code>. Rebuild from the repository root with <code>PYTHONPATH=src python docs/reports/modules/vesselstabilityscreening/build_report.py --scratch &lt;temporary-directory&gt;</code>. Calculation outputs are written only to the supplied temporary directory.</p>
</section>

<section id="appendix"><h2>10. Appendices</h2>
<p>Detailed inputs, criterion breakdowns and evidence controls are provided for calculation review. Missing physical data remain separate from the numerical demonstration.</p>
<h3>10.1 Input and data register</h3>
<p>The published cross-curve example is listed first, followed by the synthetic loading, hydrostatic and cross-curve inputs. Percentage difference is calculated as the calculated arm divided by the published corrected arm, less unity, expressed as a percentage.</p>
{public_table}
{input_table}{hydro_table}{kn_table}
{table(['Data', 'Basis and status', 'Action'], [
['Synthetic loading and hydrostatics', 'ASSUMED - to be confirmed; assumed, based on previous experience. Public synthetic fixture.', 'Replace with approved condition data before operational use.'],
['Synthetic cross-curves', 'Supplied test inputs; physical validity beyond deck edge is not established.', 'Replace with approved cross-curves covering the required angle range.'],
['Public cross-curves and gravity centre', 'Course example; published precision preserved.', 'Use only as a calculation example.'],
['Downflooding and opening geometry', 'NOT_EVALUATED; no value is assumed.', 'Supply opening immersion angles and applicable limits.'],
['Wave, heading and installation limits', 'NOT_EVALUATED; no sea-state assumption is made.', 'Perform a separate operation-specific dynamic analysis.'],
['Criterion applicability and approved booklet', 'NOT_EVALUATED for a physical vessel.', 'Establish the governing basis before acceptance.'],
])}
<h3>10.2 Criterion breakdown and quality checks</h3>
{kg_table}
<p>All displayed calculation values are generated from returned results or echoed inputs. Public arrays are embedded at bounded precision. Hand anchors use stated arithmetic tolerances; rebuild consistency checks give the exact rebuild command on a stale result or calculation-source digest. Text is read and written explicitly as UTF-8, with line-feed writes. Report tests are to be executed with the optional Python UTF-8 environment override unset.</p>
<p>The pre-flight checks cover the reference links, ten-section structure, source access records, inlined house style, numerical bindings, hand arithmetic, input precision, comment export controls and public wording. Numerical checks are separate from approval of the physical inputs. Browser interaction testing is not included.</p>
<details><summary>Engineering changes since initial issue</summary><ul>{changes}</ul></details>
<details id="comments"><summary>Section comments</summary>
<p>Comments remain local. Choose a report section, enter a note and add it to the list. Save beside the report as report.comments.json. Where a file picker is unavailable, move the downloaded file beside the report. Existing comments are loaded with the file control.</p>
<label for="comment-section">Section</label><select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="4"></textarea>
<button id="comment-add" type="button">Add comment</button><button id="comment-save" type="button">Save comments</button>
<label for="comment-import">Load comments</label><input id="comment-import" type="file" accept="application/json,.json">
<p id="comment-status" aria-live="polite"></p><ol id="comment-list"></ol></details>
</section>
"""
    payload = json.dumps(rounded(data), ensure_ascii=False, separators=(",", ":"), allow_nan=False).replace("<", "\\u003c")
    return '<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n<title>Vessel stability screening module analysis</title>\n' + house_style() + '</head><body class="hs-doc"><main class="wrap">' + body + \
        f'<script id="report-data" type="application/json">{payload}</script>\n' + INTERACTION + '</main></body></html>\n'


INTERACTION = """<script>
"use strict";
const record = JSON.parse(document.getElementById("report-data").textContent);
const svgNS = "http://www.w3.org/2000/svg";
function element(parent, tag, attrs, text) {
  const el = document.createElementNS(svgNS, tag);
  for (const [key, value] of Object.entries(attrs)) el.setAttribute(key, value);
  if (text !== undefined) el.textContent = text;
  parent.appendChild(el); return el;
}
function chart(id, xs, ys, limit, xLabel, component, caseLabel, xUnit, yUnit) {
  const svg = document.getElementById(id + "-chart");
  const ymin = Math.min(limit, ...ys), ymax = Math.max(limit, ...ys);
  const range = ymax - ymin || 1;
  const px = x => 70 + (x - Math.min(...xs)) / (Math.max(...xs) - Math.min(...xs) || 1) * 600;
  const py = y => 245 - (y - ymin + range * 0.1) / (range * 1.2) * 210;
  element(svg, "path", {d: "M70 25V245H680", fill: "none", stroke: "var(--ink)"});
  element(svg, "line", {x1: 70, x2: 680, y1: py(limit), y2: py(limit), stroke: "var(--nogo)", "stroke-dasharray": "6 4"});
  element(svg, "text", {x: 74, y: py(limit) - 7, fill: "var(--ink)"}, "Limit " + limit.toFixed(2) + " " + yUnit);
  element(svg, "polyline", {points: xs.map((x,i) => px(x)+","+py(ys[i])).join(" "), fill: "none", stroke: "var(--brand)", "stroke-width": 2});
  element(svg, "text", {x: 300, y: 286, fill: "var(--ink)"}, xLabel + " (" + xUnit + ")");
  element(svg, "text", {x: 75, y: 18, fill: "var(--ink)"}, component + " (" + yUnit + ")");
  [Math.min(...xs), Math.max(...xs)].forEach(x => element(svg, "text", {x:px(x), y:265, fill:"var(--ink)"}, x.toFixed(2)));
  [ymin, ymax].forEach(y => element(svg, "text", {x:4, y:py(y), fill:"var(--ink)"}, y.toFixed(2)));
  const readout = document.getElementById(id + "-readout");
  const control = document.getElementById(id + "-select");
  function describe(i) {
    return "Case: " + caseLabel(i) + "; component: " + component + "; " + xLabel + ": " + xs[i].toFixed(2) + " " + xUnit + "; demand: " + ys[i].toFixed(4) + " " + yUnit + "; limit: " + limit.toFixed(2) + " " + yUnit + ". " + (ys[i] >= limit ? "The plotted minimum is met." : "The plotted minimum is not met.");
  }
  xs.forEach((x,i) => {
    const point = element(svg, "circle", {cx:px(x), cy:py(ys[i]), r:5, fill:ys[i]>=limit?"var(--go)":"var(--nogo)", tabindex:0, "aria-label":describe(i)});
    element(point, "title", {}, describe(i));
    point.addEventListener("pointerover", () => {control.value=i; readout.textContent=describe(i);});
    point.addEventListener("focus", () => {control.value=i; readout.textContent=describe(i);});
  });
  const marker = element(svg, "circle", {r:9, fill:"none", stroke:"var(--ink)", "stroke-width":2, "pointer-events":"none"});
  function select() {const i=Number(control.value); marker.setAttribute("cx",px(xs[i])); marker.setAttribute("cy",py(ys[i])); readout.textContent=describe(i);}
  control.addEventListener("input",select); select();
}
chart("sensitivity", record.sensitivity.map(r=>r.fsc_m), record.sensitivity.map(r=>r.gm_fluid_m), record.intact.criteria_results[5].required, "Free-surface correction", "Fluid metacentric height", i=>["No free surface", "Reference", "Excess free surface"][i], "m", "m");
chart("public", record.public.heel_deg, record.public.gz_m, 0, "Heel", "Righting arm", ()=>"Public example", "degrees", "m");
let comments = [];
const sectionChoice = document.getElementById("comment-section");
const status = document.getElementById("comment-status");
for (const section of document.querySelectorAll("main > section")) {
  const option = document.createElement("option"); option.value=section.id; option.textContent=section.querySelector("h2").textContent; sectionChoice.appendChild(option);
}
function showComments() {
  const list = document.getElementById("comment-list"); list.replaceChildren();
  for (const note of comments) {const li=document.createElement("li"); li.textContent=note.section+": "+note.text; list.appendChild(li);}
}
document.getElementById("comment-add").addEventListener("click",()=>{
  const input=document.getElementById("comment-text"); if (!input.value.trim()) return;
  comments.push({section:sectionChoice.value,text:input.value.trim()}); input.value=""; showComments(); status.textContent="Comment added locally; save to keep it.";
});
document.getElementById("comment-save").addEventListener("click",async()=>{
  const payload = JSON.stringify(comments,null,2)+"\\n";
  try {
    if (window.showSaveFilePicker) {
      const handle=await window.showSaveFilePicker({suggestedName: "report.comments.json",types:[{description:"Comments",accept:{"application/json":[".json"]}}]});
      const stream=await handle.createWritable(); await stream.write(payload); await stream.close();
    } else {
      const url=URL.createObjectURL(new Blob([payload],{type:"application/json"})); const a=document.createElement("a"); a.href=url; a.download="report.comments.json"; a.click(); setTimeout(()=>URL.revokeObjectURL(url),1000);
    }
    status.textContent="Comments exported; keep the file beside the report.";
  } catch (error) {status.textContent="Comments were not saved. "+error.message;}
});
document.getElementById("comment-import").addEventListener("change",async event=>{
  try {
    const file=event.target.files[0]; if (!file) return;
    const notes=JSON.parse(await file.text()); const sections=new Set(Array.from(sectionChoice.options,o=>o.value));
    if (!Array.isArray(notes)||notes.some(n=>!n||!sections.has(n.section)||typeof n.text!=="string")) throw new Error("The comment file is not in the expected format.");
    comments=notes; showComments(); status.textContent="Comments loaded locally.";
  } catch (error) {status.textContent=error.message;}
});
</script>
"""


def build(output: Path, scratch: Path) -> None:
    """Write the report and its engineering change panel."""
    data = calculate(scratch)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(render(data), encoding="utf-8", newline="\n")
    (output.parent / "changes.json").write_text(json.dumps(CHANGES, ensure_ascii=False, indent=2) + "\n", encoding="utf-8", newline="\n")


def main() -> None:
    """Accept an explicit external scratch location."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=HERE / "report.html")
    args = parser.parse_args()
    build(args.output, args.scratch)


if __name__ == "__main__":
    main()
