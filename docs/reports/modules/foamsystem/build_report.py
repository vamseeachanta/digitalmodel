# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the foam system analysis; all calculation output stays in scratch.

From the repository root:
    PYTHONPATH=src uv run --no-project --with pyyaml python \
        docs/reports/modules/foamsystem/build_report.py --scratch <temporary-directory>
"""

from __future__ import annotations

import argparse
import ast
import copy
import html
import json
import re
from pathlib import Path

import yaml

from digitalmodel.foam_system.workflow import router

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = "examples/workflows/foam-system-report-pack/foam_system_sizing.yml"
NODE_LABELS = {"PUMP": "Pump", "J1": "Header junction", "MON_FWD": "Forward monitor", "MON_AFT": "Aft monitor"}
TESTS = (
    "tests/fire_safety/test_foam_system_sizing.py",
    "tests/fire_safety/test_foam_criteria_library.py",
    "tests/report_pack/test_foam_report_pack.py",
)


def metrics(result: dict) -> dict:
    """Select headline values from the public workflow return value."""
    return {
        "design_flow": result["demand"]["design_solution_flow_lpm"],
        "concentrate": result["concentrate_result"]["total_concentrate_l"],
        "injection": result["concentrate_result"]["injection_rate_lpm"],
        "utilization": result["proportioner_result"]["utilization_percent"],
        "head": result["hydraulics_result"]["required_pump_head_m"],
        "head_margin": result["hydraulics_result"]["pump_head_margin_m"],
        "flow_margin": result["hydraulics_result"]["pump_flow_margin_lpm"],
    }


def calculate(scratch: Path) -> dict:
    """Call the module on the published synthetic example and sensitivity cases."""
    scratch = scratch.resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Scratch must be outside the repository")
    cfg = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
    inputs = copy.deepcopy(cfg["foam_system_sizing"])
    del inputs["output_dir"]
    cfg["foam_system_sizing"]["output_dir"] = str(scratch / "example")
    out = router(cfg)["foam_system_sizing"]
    # This is a sensitivity range, not an operating envelope. Every point is
    # obtained by calling the same workflow with scaled terminal flow inputs.
    curve = []
    for percent in range(50, 151, 5):
        scale = percent / 100
        case = yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
        case["foam_system_sizing"]["output_dir"] = str(scratch / f"sensitivity-{percent}")
        for terminal in case["foam_system_sizing"]["hydraulics"]["terminals"]:
            terminal["flow_lpm"] *= scale
        result = router(case)["foam_system_sizing"]["hydraulics_result"]
        curve.append({"scale": scale, "flow": result["total_flow_lpm"],
                      "head": round(result["required_pump_head_m"], 4),
                      "component": NODE_LABELS[result["governing_terminal"]],
                      "limit": inputs["hydraulics"]["pump"]["rated_head_m"]})
    inventory = []
    for path in TESTS:
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        count = sum(isinstance(node, ast.FunctionDef) and node.name.startswith("test_") for node in tree.body)
        inventory.append({"path": path, "functions": count})
    # Exclude filesystem locations from the calculation record embedded in HTML.
    return {"inputs": inputs,
            "result": {key: out[key] for key in ("demand", "concentrate_result", "proportioner_result", "hydraulics_result")},
            "metrics": metrics(out), "curve": curve,
            "inventory": inventory}


def lookup(data: dict, path: str):
    """Resolve a dot-separated report value, including list indices."""
    value = data
    for key in path.split("."):
        value = value[int(key)] if isinstance(value, list) else value[key]
    return value


def display(value) -> str:
    """Keep exact input digits and show computed heads to engineering precision."""
    if isinstance(value, (int, float)):
        return f"{value:.1f}" if value % 1 else f"{value:.0f}"
    return str(value)


def rounded(value):
    """Avoid CPU-dependent last digits in the embedded calculation record."""
    if isinstance(value, float):
        return round(value, 4)
    if isinstance(value, dict):
        return {key: rounded(item) for key, item in value.items()}
    if isinstance(value, list):
        return [rounded(item) for item in value]
    return value


def house_style() -> str:
    """Inline the existing brand stylesheet and prose-report layout unchanged."""
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    return f"<style>\n{brand}\n{layout}\n</style>"


def render(data: dict) -> str:
    """Write the engineering narrative with values bound to calculation fields."""
    def v(path: str) -> str:
        return f'<span data-value="{path}">{html.escape(display(lookup(data, path)))}</span>'

    def ref(number: int) -> str:
        return f'<a href="#ref-{number}">[{number}]</a>'

    def check(path: str) -> str:
        outcome = "passes" if lookup(data, path) else "fails"
        return f'<span data-check="{path}">{outcome}</span>'

    def table(headers, rows, caption, identifier):
        head = "".join(f"<th>{cell}</th>" for cell in headers)
        body = "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
        return f'<table id="{identifier}"><caption>{caption}</caption><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>'

    governing_terminal = html.escape(NODE_LABELS[data["result"]["hydraulics_result"]["governing_terminal"]])
    governing_area_index = next(i for i, area in enumerate(data["result"]["demand"]["per_area"])
                               if area["name"] == data["result"]["demand"]["governing_area"])
    governing_area_path = f"result.demand.per_area.{governing_area_index}"
    area_rows = []
    for i, area in enumerate(data["result"]["demand"]["per_area"]):
        p = f"result.demand.per_area.{i}"
        area_rows.append([html.escape(area["name"]), v(p + ".area_m2"),
                          v(p + ".application_rate_lpm_per_m2"), v(p + ".discharge_time_min"),
                          v(p + ".solution_flow_lpm"), ref(1) + " " + ref(5)])
    areas_table = table(["Basis", "Area (m²)", "Rate (L/min/m²)", "Duration (min)", "Demand (L/min)", "Source"],
                        area_rows, "Table A.1. Public synthetic inputs and returned area demands.", "areas")
    run_rows = []
    for i, run in enumerate(data["inputs"]["hydraulics"]["runs"]):
        p = f"inputs.hydraulics.runs.{i}"
        run_rows.append([html.escape(NODE_LABELS[run["from"]] + " → " + NODE_LABELS[run["to"]]), v(p + ".length_m"), v(p + ".diameter_mm"), ref(5)])
    runs_table = table(["Pipe", "Length (m)", "Internal diameter (mm)", "Source"], run_rows,
                       "Table A.2. Assumed pipe geometry; equivalent fitting lengths default to zero in this example.", "runs")
    terminal_rows = []
    for i, terminal in enumerate(data["inputs"]["hydraulics"]["terminals"]):
        p = f"inputs.hydraulics.terminals.{i}"
        terminal_rows.append([html.escape(NODE_LABELS[terminal["node"]]), v(p + ".flow_lpm"),
                              v(p + ".required_pressure_bar"), v(p + ".elevation_m"), ref(5)])
    terminals_table = table(["Terminal", "Flow (L/min)", "Pressure (bar)", "Elevation (m)", "Source"],
                            terminal_rows, "Table A.3. Assumed terminal boundary conditions.", "terminals")
    inventory_rows = [[label, v(f"inventory.{i}.functions"), ref(6)] for i, label in enumerate(
        ("Sizing and workflow", "Criteria library", "Report-pack example"))]
    inventory_table = table(["Evidence area", "Test functions", "Source"], inventory_rows,
                            "Table A.4. Existing test-function inventory, not a test execution record.", "inventory")
    result_rows = [["Solution supply", v("metrics.design_flow"), "L/min", "Greatest area basis", "Table A.1"],
                   ["Concentrate inventory", v("metrics.concentrate"), "L", "Selected duration and concentration", "Table A.1; Table A.5"],
                   ["Concentrate injection", v("metrics.injection"), "L/min", "Selected concentration", "Table A.5"],
                   ["Proportioner utilization", v("metrics.utilization"), "%", "Assumed maximum solution flow", "Table A.5"],
                   ["Required pump head", v("metrics.head"), "m", governing_terminal + " governs", "Tables A.2–A.3"],
                   ["Pump head margin", v("metrics.head_margin"), "m", "Assumed pump rating", "Table A.5"],
                   ["Pump flow margin", v("metrics.flow_margin"), "L/min", "Assumed pump rating", "Table A.5"]]
    result_table = table(["Quantity", "Value", "Unit", "Basis", "Source"], result_rows,
                         "Table 5.1. Returned screening quantities; equipment ratings are assumed.", "results-table")
    p = "inputs.proportioner"
    h = "inputs.hydraulics"
    equipment = table(["Input", "Value", "Unit", "Source"], [
        ["Concentration", v("inputs.concentrate.concentration_percent"), "%", ref(5)],
        ["Reserve", v("inputs.concentrate.reserve_percent"), "%", ref(5)],
        ["Proportioner minimum solution flow", v(p + ".min_solution_flow_lpm"), "L/min", ref(5)],
        ["Proportioner maximum solution flow", v(p + ".max_solution_flow_lpm"), "L/min", ref(5)],
        ["Proportioner maximum injection", v(p + ".max_concentrate_flow_lpm"), "L/min", ref(5)],
        ["Proportioner rated concentration", v(p + ".rated_concentration_percent"), "%", ref(5)],
        ["Pump rated flow", v(h + ".pump.rated_flow_lpm"), "L/min", ref(5)],
        ["Pump rated head", v(h + ".pump.rated_head_m"), "m", ref(5)],
        ["Pump elevation", v(h + ".pump.elevation_m"), "m", ref(5)],
        ["Water-like solution density", v(h + ".density_kg_m3"), "kg/m³", ref(5)],
        ["Hazen–Williams coefficient C", v(h + ".hazen_williams_c"), "dimensionless", ref(5)],
    ], "Table A.5. Equipment and fluid inputs. All are ASSUMED - to be confirmed; no vendor rating or material record is supplied.", "equipment")
    checks = json.loads((HERE / "checks.json").read_text(encoding="utf-8"))
    check_rows = [[html.escape(row["scope"]), str(row["passed"]), str(row["unavailable"]), html.escape(row["note"])] for row in checks["test_results"]]
    checks_table = table(["Check", "Passed", "Unavailable", "Qualification"], check_rows,
                         "Table A.6. Recorded test results; dependency failures are not engineering results.", "checks-table")
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    payload = json.dumps(rounded(data), ensure_ascii=False, allow_nan=False).replace("</", "<\\/")
    changes_payload = json.dumps(changes, ensure_ascii=False).replace("</", "<\\/")
    body = f"""
<header><p class="kicker">Foam system module analysis</p>
<h1>Foam system sizing analysis</h1><p class="sub">Draft for maintainer review</p></header>
<section id="summary"><h2>1 Executive Summary</h2>
<p>A low-expansion foam system is a water and concentrate supply feeding discharge devices over a protected area.
Analysis is performed to determine solution demand, concentrate inventory, proportioner duty and pipe-network head demand;
key results are summarized below. The public synthetic example is evaluated as a screening calculation {ref(5)}.</p>
<ul><li>The calculation is used to identify the governing supply basis.
<ul><li>The returned governing area is {v('result.demand.governing_area')} at {v('metrics.design_flow')} L/min, with
{v('metrics.concentrate')} L of concentrate at the example duration and concentration {ref(1)} {ref(5)}.</li></ul></li>
<li>The example is used to identify equipment data required for design review.
<ul><li>The returned head margin is {v('metrics.head_margin')} m against the assumed
{v('inputs.hydraulics.pump.rated_head_m')} m pump rating; installed-system acceptance is NOT_EVALUATED.
Pressure losses through the proportioner and discharge-device performance are absent {ref(5)} {ref(6)}.</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-22</td><td>The public synthetic sizing example and evidence limits are documented to support input review.</td></tr>
<tr><td>1</td><td>2026-10-22</td><td>Independent hydraulic anchors, rounded calculation records, returned screening outcomes and both monitor capacity criteria are documented to address review findings.</td></tr>
<tr><td>2</td><td>2026-10-25</td><td>The pressure-unit citation is corrected to Table 9 of NIST SP 811; the conversion value and calculation results are unchanged.</td></tr></tbody></table>
</section>
<nav aria-label="Contents"><p><a href="#introduction">Introduction</a> · <a href="#basis">Design data</a> ·
<a href="#method">Methodology</a> · <a href="#results">Results</a> · <a href="#validation">Validation</a> ·
<a href="#limits">Limits</a> · <a href="#recommendations">Recommendations</a> · <a href="#references">References</a> ·
<a href="#appendix">Appendix and comments</a></p></nav>
<section id="introduction"><h2>2 Introduction</h2>
<p>The documented sizing workflow is assessed from its public example and existing tests. The calculation is used for preliminary
low-expansion foam sizing; a fire-extinguishment prediction is not made {ref(5)} {ref(6)}.</p>
<p>The main workflow comprises protected-area demand, concentrate quantity, rated proportioner checks and a single-scenario tree network.
Criteria are selected by library key or supplied inline with a standard, edition and clause. Missing citation fields and library-key
collisions are rejected. A citation string records the selected basis; its presence does not establish applicability {ref(6)}.</p>
<p>Inputs comprise protected areas and criteria, the single-fire or simultaneous-area policy, optional concurrent hoses,
concentration and reserve, optional proportioner ratings, and optional hydraulic geometry and terminal duties.
The returned configuration contains per-area demand, concentrate quantities, envelope flags and hydraulic margins.
Three comma-separated tables are written for demand with citations, pipe flow and friction loss, and terminal path heads;
the latter two are written only when hydraulics is supplied. The public example also demonstrates transfer of these tables
into a report pack {ref(5)} {ref(6)}.</p>
</section>
<section id="basis"><h2>3 Design Data and Criteria</h2>
<p>The documented public example is used without altering its engineering inputs. Its areas, geometry, concentration and equipment
ratings are synthetic inputs, marked ASSUMED - to be confirmed; the source tables are in the appendix {ref(5)}.</p>
<h3>3.1 Physical arrangement and boundary conditions</h3>
<p>Water is mixed with concentrate upstream of the pipe header. Flow is prescribed at both monitors and terminal pressure is imposed;
no nozzle discharge curve or pump operating-point intersection is solved. The diagram shows connectivity and elevation, not a fabrication drawing.</p>
<figure id="schematic"><svg viewBox="0 0 860 310" role="img" aria-labelledby="system-title" style="width:100%">
<title id="system-title">Water supply, concentrate injection, pump and branched monitor header; synthetic boundary conditions</title>
<defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0L10 5L0 10Z" fill="currentColor"/></marker></defs>
<g fill="none" stroke="currentColor" stroke-width="2">
<path d="M35 193V250H112V193M35 200Q74 184 112 200M112 225H150"/>
<circle cx="174" cy="225" r="24"/><path d="M162 237L190 225L162 213Z"/>
<path d="M198 225H455V115H598M455 160H720V115" marker-end="url(#arrow)"/>
<path d="M242 160V195H284V160ZM263 195V225"/>
<path d="M584 115V103H608L629 89M706 115V103H730L751 89"/>
<path d="M568 100Q620 60 660 98M690 100Q743 60 784 98" stroke-dasharray="4 4"/>
</g><g font-size="13" fill="currentColor">
<text x="22" y="277">Water supply</text><text x="145" y="277">Pump</text>
<text x="212" y="147">Concentrate</text><text x="289" y="192">Proportioner</text>
<text x="315" y="251">Header → junction</text><text x="535" y="44">Forward monitor</text><text x="697" y="44">Aft monitor</text>
<text x="535" y="62">Prescribed flow and pressure</text><text x="508" y="292">Elevated terminals; pump is elevation datum</text>
</g></svg><figcaption>Figure 3.1. Synthetic tree arrangement. Geometry and boundary values are given in Tables A.2–A.3.
The mixing-device loss is omitted. Monitor throw and coverage geometry are NOT_EVALUATED.</figcaption></figure>
<h3>3.2 Acceptance basis</h3>
<p>The greatest of the cargo deck, largest tank section and largest monitor area demands is selected. The opened Fire Safety Systems
Code resolution, adopted in 2000, sets these supply bases and an additional monitor minimum in Chapter 14, paragraph 2.2.1.1;
the inert-gas-fitted duration is addressed in paragraph 2.2.1.2 {ref(1)}. The repository library labels its basis as the 2015 consolidated
edition and uses different clause numbering. That consolidated edition was not opened; equivalence to later amendments is NOT_EVALUATED {ref(5)} {ref(6)}.</p>
<p>The example assumes inert-gas-fitted service. Each monitor must supply at least 50 % of the greater rate required by
Chapter 14, paragraphs 2.2.1.1.1 and 2.2.1.1.2, under paragraph 2.2.2.1: 0.5 × max(1800, 3000) = 1500 L/min here {ref(1)} {ref(5)}.
This governs over the separate 1250 L/min floor in paragraphs 2.2.1.1.3 and 2.2.2.2 {ref(1)}.
Neither monitor check is enforced by the area-rate multiplication. The forward and aft example monitors are assigned
{v('inputs.hydraulics.terminals.0.flow_lpm')} and {v('inputs.hydraulics.terminals.1.flow_lpm')} L/min respectively,
meeting the governing per-monitor rate at equality. Monitor coverage and the required fire-main
simultaneity are outside this calculation. A nonnegative pump margin and a proportioner point inside the assumed envelope are screening
criteria selected from the example, with actual equipment suitability NOT_EVALUATED {ref(5)} {ref(6)}.</p>
</section>
<section id="method"><h2>4 Analysis Methodology</h2>
<p>Area demand and concentrate conservation are calculated before equipment duty and pipe-network head are assessed. The solution
is treated as water for friction screening, using the synthetic density and pipe coefficient in Table A.5 {ref(5)}.</p>
<h3>4.1 Demand and concentrate</h3>
<div class="eq">Qᵢ = Aᵢ rᵢ; Q = max(Qᵢ) + Qₕ; V꜀ = c [max(Qᵢ tᵢ) + Vₕ] (1 + R); Q꜀ = Q c.</div>
<p>Aᵢ is protected area in m²; rᵢ is solution application rate in L/min/m²; Qᵢ and Q are area and design flows in L/min;
Qₕ is concurrent hose flow in L/min; tᵢ is discharge time in minutes; Vₕ is hose solution volume in litres; c is concentrate volume fraction;
R is reserve fraction; V꜀ is total concentrate in litres; Q꜀ is injection in L/min. Rate times area and flow times duration implement
Chapter 14, paragraphs 2.2.1.1–2.2.1.2 of the opened 2000 resolution {ref(1)}; volume fraction and percent conversion follow
NIST Special Publication 811, 2008 edition, Sections 8.6.6 and 7.10.2 {ref(3)}. Reserve and hose allowances are explicit inputs.</p>
<p>The maximum-flow and maximum-volume scenarios are selected separately: different discharge durations can make different areas
govern flow and inventory. For simultaneous protected areas the sum policy replaces each maximum with a sum. Hose streams are
always added. In this example no hoses or reserve are specified {ref(5)} {ref(6)}.</p>
<h3>4.2 Equipment and pipe network</h3>
<p>Solution flow is checked between the proportioner's input minimum and maximum; concentrate injection is checked against its input
maximum; concentration is checked against its input rating. Utilization is design flow divided by maximum rated solution flow,
expressed as a percentage. These are envelope comparisons with assumed inputs, not vendor performance calculations {ref(5)} {ref(6)}.</p>
<div class="eq">hꜰ = 10.67 L q^1.852 / (C^1.852 d^4.87); H = Σhꜰ + (zₜ − zₚ) + pₜ/(ρg).</div>
<p>hꜰ is friction loss in m; L is physical plus equivalent pipe length in m; q is pipe flow in m³/s; C is the dimensionless
Hazen–Williams pipe coefficient; d is internal diameter in m; H is required pump head in m; zₜ and zₚ are terminal and pump elevations
in m; pₜ is required terminal pressure in Pa; ρ is solution density in kg/m³; g is standard gravitational acceleration in m/s².
The required head is the largest terminal-path head. Flow in each pipe is the sum of downstream terminal flows.</p>
<p>The friction equation is the implementation's rounded SI form of Hazen–Williams. The public EPANET 2.2 manual, 2020 edition,
Section 3.1, Table 3.1 gives the customary-unit coefficient 4.727, flow exponent 1.852 and diameter exponent 4.871 {ref(2)}.
SI conversion gives approximately 10.67; the implementation rounds the diameter exponent to 4.87. Exact equality to the EPANET
form is not asserted. Unit conversion is based on NIST Special Publication 811, 2008 edition, Appendix B.8 {ref(3)}.
Hazen–Williams is empirical for water in turbulent flow; its use for foam solution is an assumption requiring fluid data {ref(2)}.</p>
<p>Pressure head and static lift follow the energy balance in University Physics, Volume 1, 2016 edition, Section 14.6 {ref(4)}.
The implementation uses standard gravity 9.80665 m/s² and 100000 Pa per bar; the unit bases are NIST Special Publication 811,
2008 edition, Appendix B.8 and Section 5.2, Table 9 {ref(3)}. Mixing-device loss, velocity-head changes and nozzle curves are omitted.</p>
</section>
<section id="results"><h2>5 Detailed Results</h2>
<p>The public workflow is called on the documented example to obtain the values below. The returned governing area is
{v('result.demand.governing_area')} for solution supply, and {governing_terminal} governs pump head {ref(5)}.</p>
<p>The displayed inputs give {v(governing_area_path + '.area_m2')} m² ×
{v(governing_area_path + '.application_rate_lpm_per_m2')} L/min/m² = {v('metrics.design_flow')} L/min.
At {v(governing_area_path + '.discharge_time_min')} min and
{v('inputs.concentrate.concentration_percent')} % concentration, the returned inventory is {v('metrics.concentrate')} L.
The returned injection is {v('metrics.injection')} L/min. Input precision is preserved for this arithmetic.</p>
{result_table}
<p>The proportioner envelope check {check('result.proportioner_result.ok')} for the assumed ratings.
The pump flow check {check('result.hydraulics_result.pump_flow_ok')} and the pump head check
{check('result.hydraulics_result.pump_head_ok')} against the assumed ratings.
These results do not establish installed performance: no pump curve, mixing-device loss or discharge-device test is supplied.</p>
<figure id="head-chart"><svg id="chart" viewBox="0 0 820 340" role="img" aria-label="Required pump head against prescribed total flow with assumed pump-head limit" style="width:100%"></svg>
<label for="case-slider">Select sensitivity case</label> <input id="case-slider" type="range" min="0" max="20" value="10" aria-label="Sensitivity case">
<p id="chart-tooltip" aria-live="polite"></p>
<figcaption>Figure 5.1. Required head versus prescribed total terminal flow. The dashed line is the assumed pump-head limit.
Hover over a point or select a case for component, demand, limit and units. Terminal pressures remain fixed, and both terminal flows
are scaled together; no curve intersection or operating envelope is inferred.</figcaption></figure>
<p>The sensitivity range is a selected arithmetic range of half to one and a half times the example terminal flows, rather than an
operating allowance. Values beyond the assumed pump flow rating are displayed for sensitivity only. No monitor pressure-flow relation
is supplied, so the sensitivity points are not discharge-device operating points.</p>
</section>
<section id="validation"><h2>6 Validation Evidence</h2>
<p>The repository holds analytical arithmetic tests, hydraulic screening tests and a report-pack example. This evidence exercises
software behavior; physical validation against a fire test or measured hydraulic network is not established {ref(6)}.</p>
<p>The sizing tests cover demand, concentration and reserve arithmetic, proportioner flags, Hazen–Williams arithmetic, laminar
Darcy–Weisbach agreement with Hagen–Poiseuille, a turbulent cross-method comparison, tree validation and routed outputs.
The criteria tests cover citation fields, explicit source-review flags, selection and collisions. The report-pack tests compare
checked-in sizing tables with fresh calculation output and exercise rendering. The inventory in Table A.4 counts test functions,
without implying execution or engineering acceptance.</p>
<p>The accompanying report tests assert hand-worked supply, inventory, injection, hydraulic head, head margin, utilization and flow margin,
exercise both outcomes of each returned screening flag and changes in governing labels, and compare every tagged worked-example
value with the workflow return, recalculate chart points and rebuild the committed page at displayed precision. These latter checks
are consistency checks. The hand arithmetic does not validate the regulatory inputs. Execution results and checks not performed are
listed in the appendix; no field-validation record is held with this report.</p>
</section>
<section id="limits"><h2>7 Limits of Use and Not Covered</h2>
<p>Use is limited to preliminary low-expansion sizing with cited, applicable criteria and a connected tree network. Installed-system
acceptance is NOT_EVALUATED because equipment, coverage and governing-edition evidence are absent.</p>
<p>Area-rate multiplication does not enforce either the supply-based per-monitor requirement or the fixed monitor floor. Optional pump and proportioner checks use input ratings,
and hydraulic terminal flow is not reconciled automatically with the demand calculation. Concentrate hose allowances rely on the caller
passing the same hoses and durations used in demand. Nonfinite input rejection is not established across all input fields {ref(6)}.</p>
<p>Not covered: loop balancing; medium- or high-expansion foam; fuel compatibility and environmental approval; foam quality and
extinguishment; monitor throw and coverage; fire-main simultaneity; suction losses and cavitation; vendor mixing-device losses;
transients; and detailed equipment selection. The alternative Darcy–Weisbach branch, storage-tank criteria and helideck criteria
are described in the module documentation but are not worked examples here {ref(6)}.</p>
<p>The alternative friction branch uses an abrupt laminar-to-turbulent switch, whereas the public EPANET manual describes a transition
interpolation. This report supplies no accuracy claim for that transition region {ref(2)} {ref(6)}.</p>
</section>
<section id="recommendations"><h2>8 Recommendations</h2>
<p>Input qualification and omitted physical losses are resolved before design acceptance. The recommendations below address the
specific evidence gaps exposed by this calculation.</p>
<ul><li>Obtain the applicable governing edition, fuel basis, coverage drawing and inert-gas status; review each supply basis and duration
against the opened resolution and applicable amendments before issuing a design {ref(1)}.</li>
<li>Check each monitor against both Chapter 14, paragraph 2.2.2.1 (50 % of the greater supply basis, 1500 L/min here) and paragraph
2.2.2.2 (1250 L/min floor) before accepting any area-based result. The greater requirement governs; review monitor coverage and fire-main
simultaneity separately {ref(1)} {ref(5)}.</li>
<li>Replace assumed equipment ratings with supplier pressure-flow curves and include mixing-device, suction and fitting losses.
Reassess head margin whenever the selected flow, geometry or fluid properties change; the energy balance and empirical water-friction
limitations provide the trigger {ref(2)} {ref(4)}.</li>
<li>Reconcile terminal supply with the governing area demand in every scenario. If a required minimum or equipment envelope is exceeded,
revise device allocation or equipment rating and repeat the screening before acceptance {ref(1)} {ref(5)}.</li>
<li>Extend analytical input-rejection and transition-region tests before expanding the supported hydraulic use range. Compare a measured
network or qualified public benchmark before making a physical-performance accuracy claim {ref(2)} {ref(6)}.</li></ul>
</section>
<section id="references"><h2>9 References</h2>
<p>The public references provide the physical and regulatory basis. Repository records identify the implementation and synthetic inputs;
a library label alone is not a review of its cited edition.</p>
<h3>9.1 Public engineering sources</h3>
<ol class="refs"><li id="ref-1"><a href="https://wwwcdn.imo.org/localresources/en/KnowledgeCentre/IndexofIMOResolutions/MSCResolutions/MSC.98(73).pdf">International Code for Fire Safety Systems, Resolution MSC.98(73), adopted 2000</a>, Chapter 14,
paragraphs 2.1.3, 2.2.1.1–2.2.1.2, 2.2.2.1–2.2.2.2 and 2.3. Original resolution; no claim of current consolidated applicability.</li>
<li id="ref-2"><a href="https://usepa.github.io/EPANET2.2/3_network_model.html">EPANET 2.2 User Manual, 2020</a>, Section 3.1, Pipes, Table 3.1 and friction-regime discussion. It is a water-network method source, not a foam-system benchmark.</li>
<li id="ref-3"><a href="https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf">NIST Special Publication 811, 2008 edition</a>, Sections 5.1–5.2, 7.10.2 and 8.6.6, and Appendix B.8: units, percent, fractions and standard-gravity basis.</li>
<li id="ref-4"><a href="https://openstax.org/books/university-physics-volume-1/pages/14-6-bernoullis-equation">University Physics, Volume 1, 2016 edition</a>, Section 14.6, Bernoulli’s Equation. It supplies the steady-flow energy-balance basis.</li></ol>
<p>These public sources were opened on 2026-10-22; their access metadata and content digests are supplied in the source record.
Full publications are not distributed with this report.</p>
<h3>9.2 Repository records and rebuild locators</h3>
<p id="ref-5">[5] Documented public synthetic example: <code>{EXAMPLE}</code>;
usage and limitations: <code>docs/domains/fire_safety/foam-system-sizing.md</code>.
The example is an arithmetic fixture, not a measured installation.</p>
<p id="ref-6">[6] Public entry point: <code>digitalmodel.foam_system.router</code>;
pure sizing functions: <code>src/digitalmodel/foam_system/sizing.py</code>;
workflow: <code>src/digitalmodel/foam_system/workflow.py</code>; cited criteria:
<code>src/digitalmodel/foam_system/data/foam_criteria_library.yml</code>.
Test records: <code>{TESTS[0]}</code>, <code>{TESTS[1]}</code> and <code>{TESTS[2]}</code>.
Checked-in example tables: <code>examples/workflows/foam-system-report-pack/data/</code>.</p>
<p>Report source: <code>docs/reports/modules/foamsystem/build_report.py</code>;
consistency and hand-anchor tests: <code>tests/reporting/test_foamsystem_module_report.py</code>;
source access: <code>docs/reports/modules/foamsystem/sources.json</code>;
test execution record: <code>docs/reports/modules/foamsystem/checks.json</code>.
Rebuild from repository root with <code>PYTHONPATH=src uv run --no-project --with pyyaml python docs/reports/modules/foamsystem/build_report.py --scratch &lt;temporary-directory&gt;</code>.
The temporary directory is supplied outside the repository. No licensed solver is required.</p>
</section>
<section id="appendix"><h2>10 Appendix</h2>
<p>The detailed input register and calculation inventory are provided for review. Missing physical evidence remains an input requirement.</p>
<h3>10.1 Data register</h3>
{areas_table}{runs_table}{terminals_table}{equipment}
<p>Areas, concentration, reserve, geometry, fluid coefficient, density, monitor duty and equipment ratings in Tables A.1–A.5 are
ASSUMED - to be confirmed from the public synthetic example. The rates and duration in Table A.1 are library inputs with the opened
original-resolution basis in reference [1]; applicability of the library's consolidated edition remains NOT_EVALUATED.
No geometry scale or monitor coverage boundary is inferred from Figure 3.1.</p>
<p>Required inputs not supplied: governing-edition review, fluid viscosity and foam compatibility, supplier curves, mixing-device loss,
fitting schedule, suction conditions, fire-main demand, coverage drawing and measured acceptance results. Each is NOT_EVALUATED.</p>
<h3>10.2 Symbols and units</h3>
<p>Demand and inventory symbols are defined beside the demand equations. Hydraulic symbols are defined beside the head equations.
L/min denotes litres per minute; m, mm and m² denote metres, millimetres and square metres; bar denotes pressure;
kg/m³ denotes density. SI means the International System of Units. The displayed percentages use the percent definition in reference [3].</p>
<h3>10.3 Quality pre-flight and evidence inventory</h3>
{inventory_table}
<p>The embedded numerical record is generated by calls to the module, with floating-point results rounded for cross-platform comparison.
The automated checks cover tagged values, hand arithmetic, chart consistency, local links, heading structure, explicit text encoding,
line endings and a fresh page rebuild. The inherited stylesheet is inlined and the body carries the house document class.
The page has no external script or stylesheet dependency.</p>
{checks_table}
<p>The report tests are executed with the Python UTF-8 override unset. Chart and comment handlers are exercised with a simulated document,
including save, load, export and rejected permission. Browser rendering and actual file-picker interaction are not exercised.
The application-routing check ends at an unavailable data-library dependency; its test and import are unchanged in the base revision.
The sizing and library checks and the report-pack example complete without this application import.</p>
<h3>10.4 Engineering changes and section comments</h3>
<p>The change panel records engineering changes since the previous revision. Comments are stored as a separate file beside the HTML
when that location is selected in the save dialog. Load a saved file to continue a review. Browser file access requires permission;
where the file dialog is unavailable, the export is downloaded and is placed beside the HTML manually.</p>
<details id="changes-panel"><summary>Engineering changes</summary><ul id="changes-list"></ul></details>
<label for="comment-section">Report section</label><select id="comment-section"></select>
<label for="comment-text">Comment</label><textarea id="comment-text" rows="3" style="width:100%"></textarea>
<button id="add-comment" type="button">Add comment</button>
<button id="save-comments" type="button">Save comments beside report</button>
<label for="load-comments">Load comments</label><input id="load-comments" type="file" accept="application/json">
<p id="comment-status" aria-live="polite"></p><ol id="comment-list"></ol>
</section>
<script id="calculation-data" type="application/json">{payload}</script>
<script id="changes-data" type="application/json">{changes_payload}</script>
"""
    script = r"""
<script>
(() => {
  const data = JSON.parse(document.getElementById('calculation-data').textContent);
  const changes = JSON.parse(document.getElementById('changes-data').textContent);
  for (const change of changes.changes) {
    const item = document.createElement('li'); item.textContent = change;
    document.getElementById('changes-list').append(item);
  }
  const svg = document.getElementById('chart'), ns = 'http://www.w3.org/2000/svg';
  function element(tag, attrs, text) {
    const node = document.createElementNS(ns, tag);
    for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, value);
    if (text !== undefined) node.textContent = text;
    svg.append(node); return node;
  }
  const points = data.curve, maxHead = Math.ceil(Math.max(...points.map(p => p.head), points[0].limit) / 10) * 10;
  const minFlow = points[0].flow, maxFlow = points[points.length-1].flow;
  const x = value => 80 + (value-minFlow)/(maxFlow-minFlow)*680;
  const y = value => 275-value/maxHead*220;
  element('path', {d:'M80 55V275H760', fill:'none', stroke:'currentColor'});
  for (let i=0; i<=4; i++) {
    const head = maxHead*i/4, flow = minFlow+(maxFlow-minFlow)*i/4;
    element('text', {x:70, y:y(head)+4, 'text-anchor':'end'}, head.toFixed(1));
    element('text', {x:x(flow), y:298, 'text-anchor':'middle'}, flow.toFixed(0));
  }
  element('text', {x:80, y:27}, 'Required pump head (m)');
  element('text', {x:390, y:330, 'text-anchor':'middle'}, 'Prescribed total terminal flow (L/min)');
  element('line', {x1:80,x2:760,y1:y(points[0].limit),y2:y(points[0].limit),stroke:'var(--nogo)','stroke-dasharray':'8 4'});
  element('text', {x:755,y:y(points[0].limit)-8,'text-anchor':'end'}, 'Assumed pump-head limit');
  element('polyline', {points:points.map(p=>`${x(p.flow)},${y(p.head)}`).join(' '),fill:'none',stroke:'var(--brand)','stroke-width':2});
  const marker = element('circle', {cx:0,cy:0,r:7,fill:'none',stroke:'currentColor','stroke-width':2});
  function select(i) {
    const p = points[i], rating = data.inputs.hydraulics.pump.rated_flow_lpm;
    marker.setAttribute('cx',x(p.flow)); marker.setAttribute('cy',y(p.head));
    document.getElementById('case-slider').value=i;
    document.getElementById('chart-tooltip').textContent = `Case: terminal flow ${p.flow.toFixed(0)} L/min; component: ${p.component}; head demand: ${p.head.toFixed(1)} m; assumed head limit: ${p.limit.toFixed(1)} m. ` + (p.flow>rating ? `Flow exceeds the assumed ${rating.toFixed(0)} L/min pump rating; this is a sensitivity case.` : 'Flow remains within the assumed pump rating.');
  }
  points.forEach((p,i)=>{
    const point=element('circle',{cx:x(p.flow),cy:y(p.head),r:5,fill:'var(--brand)',tabindex:0,role:'button','aria-label':`Select flow ${p.flow.toFixed(0)} litres per minute`});
    const title=document.createElementNS(ns,'title');
    title.textContent=`Case ${p.flow.toFixed(0)} L/min; ${p.component}; demand ${p.head.toFixed(1)} m; limit ${p.limit.toFixed(1)} m`;point.append(title);
    point.addEventListener('mouseenter',()=>select(i));point.addEventListener('focus',()=>select(i));
    point.addEventListener('click',()=>select(i));
  });
  document.getElementById('case-slider').addEventListener('input',event=>select(Number(event.target.value)));
  select(10);
  const sections = Array.from(document.querySelectorAll('section[id]')).map(s=>({id:s.id,title:s.querySelector('h2').textContent}));
  const sectionSelect=document.getElementById('comment-section'), text=document.getElementById('comment-text');
  sections.forEach(s=>{const option=document.createElement('option');option.value=s.id;option.textContent=s.title;sectionSelect.append(option);});
  let comments=[];
  function showComments() {
    const list=document.getElementById('comment-list');list.replaceChildren();
    comments.forEach(c=>{const item=document.createElement('li');item.textContent=`${c.section}: ${c.text}`;list.append(item);});
  }
  document.getElementById('add-comment').addEventListener('click',()=>{
    if(!text.value.trim())return;
    comments.push({section:sectionSelect.value,text:text.value.trim()});text.value='';showComments();
    document.getElementById('comment-status').textContent='Comment added in memory. Save comments to keep them.';
  });
  document.getElementById('save-comments').addEventListener('click',async()=>{
    const content=JSON.stringify({revision:changes.revision,comments},null,2)+'\n';
    try {
      if(window.showSaveFilePicker) {
        const handle=await window.showSaveFilePicker({suggestedName:'report.comments.json',types:[{description:'Section comments',accept:{'application/json':['.json']}}]});
        const stream=await handle.createWritable();await stream.write(content);await stream.close();
        document.getElementById('comment-status').textContent='Comments saved to the selected file.';
      } else {
        const url=URL.createObjectURL(new Blob([content],{type:'application/json'}));
        const a=document.createElement('a');a.href=url;a.download='report.comments.json';a.click();setTimeout(()=>URL.revokeObjectURL(url),1000);
        document.getElementById('comment-status').textContent='Comments exported. Place the downloaded file beside the report.';
      }
    } catch(error) {document.getElementById('comment-status').textContent='Comments were not saved. Select a writable location and try again.';}
  });
  document.getElementById('load-comments').addEventListener('change',async event=>{
    try {
      const loaded=JSON.parse(await event.target.files[0].text());
      if(loaded.revision!==changes.revision||!Array.isArray(loaded.comments)||!loaded.comments.every(c=>sections.some(s=>s.id===c.section)&&typeof c.text==='string'))throw Error('Invalid comments');
      comments=loaded.comments;showComments();document.getElementById('comment-status').textContent='Saved comments loaded.';
    } catch(error) {document.getElementById('comment-status').textContent='The comment file has an invalid format or a different revision.';}
  });
})();
</script>
"""
    return ('<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width,initial-scale=1">\n'
            '<title>Foam system sizing analysis</title>\n' + house_style() +
            '\n</head><body class="hs-doc"><div class="wrap">\n' + body + script + '\n</div></body></html>\n')


def build(scratch: Path) -> str:
    """Calculate and render without writing any report or temporary file in the repository."""
    return render(calculate(scratch))


def main() -> None:
    """Generate the single-file HTML deliverable."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=HERE / "report.html")
    args = parser.parse_args()
    args.output.write_text(build(args.scratch), encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
