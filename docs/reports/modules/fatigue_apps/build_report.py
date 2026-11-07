# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the bounded fatigue applications report using explicit scratch storage."""
from __future__ import annotations

import argparse
import csv
import hashlib
import html
import json
import re
import sys
from pathlib import Path

from digitalmodel.structural.fatigue_apps.tension_stress_conversion import (
    TensionStressConverter,
)

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
PACKAGE = "src/digitalmodel/structural/fatigue_apps/"
TESTS = "tests/structural/fatigue_apps/"
EXEMPLAR = "docs/reports/pipeline-ffs-assessment.html"
REVISION = "00"
DATE = "2026-10-28"  # Fixed issue date, never the rebuild time.
REBUILD = (
    'PYTHONPATH=src uv run --no-project --python python3 python '
    'docs/reports/modules/fatigue_apps/build_report.py '
    '--output-dir "$TMPDIR/fatigue-apps-report" '
    '--scratch-dir "$TMPDIR/fatigue-apps-calculation"'
)
CHANGES = {
    "report": "fatigue-applications", "revision": REVISION, "date": DATE,
    "changes": [
        "Added the synthetic uniform-bar lookup example to distinguish numerical agreement from structural calibration.",
        "Documented separate converter interfaces, load scaling, counting guards and unresolved damage qualification.",
        "Added parsed output, controlled calculation mutation and isolated rebuild checks to detect stale results.",
    ],
}
# Each numerical input below is derived in the existing hand-calculation test;
# source-prefixes supplies the exact SI definitions, not a calibrated geometry.
LOOKUP_ROWS = ((25.0, 10.0), (100.0, 40.0), (250.0, 100.0))
PROBES = (("node", 25.0), ("node", 100.0), ("node", 250.0),
          ("interpolation", 62.5), ("interpolation", 175.0),
          ("extrapolation", 0.0), ("extrapolation", 500.0))
SOURCE_SPECS = (
    ("hand", TESTS + "test_tension_stress_conversion_hand_calcs.py",
     "Independent uniform-bar derivation", "Module docstring and uniform-bar tests"),
    ("offline", PACKAGE + "tension_stress_conversion.py",
     "Offline converter", "TensionStressConverter: constructor, lookup loading, interpolation and conversion"),
    ("exports", PACKAGE + "__init__.py", "Package public exports", "__all__"),
    ("scaling", PACKAGE + "load_scaling.py", "Load scaling",
     "LoadScalingProcessor: reference selection, scaling limits, superposition"),
    ("processor", PACKAGE + "reference_seastate_processor.py", "Reference-sea-state processing",
     "TensionToStressConverter; process_fatigue_condition; process_configuration"),
    ("counter", PACKAGE + "rainflow_counter.py", "Exported counter",
     "RainflowCounter.count_cycles; _rainflow_algorithm"),
    ("counting", PACKAGE + "rainflow_counting.py", "Alternate counter and damage handoff",
     "RainflowCounter.process_batch: unconditional refusal"),
    ("contract", "src/digitalmodel/fatigue/counting_contract.py", "Counting contract",
     "refuse_damage_calculation; fapps_counting and fapps_counter registry entries"),
    ("damage", PACKAGE + "fatigue_damage_calculator.py", "Exported damage calculation",
     "SNCurveParameters.threshold; cycles_to_failure; calculate_damage; calculate_fatigue_life"),
    ("alternate", PACKAGE + "fatigue_damage.py", "Alternate damage calculation",
     "SNCurve; FatigueDamageCalculator"),
    ("validation", PACKAGE + "fatigue_validation/validator.py", "Validation placeholder",
     "FatigueValidator.validate_all"),
    ("example", PACKAGE + "examples/load_scaling_runner.py", "Existing standalone example",
     "LoadScalingProcessor.process_fatigue_condition; unchanged"),
    ("pack", PACKAGE + "docs/agent-pack.md", "Knowledge pack",
     "Purpose, methods, known failure modes; historical evidence, not a current execution record"),
    ("scaling-test", TESTS + "test_load_scaling.py", "Baseline scaling suite",
     "Import of absent LoadScaler; explicit collection fails"),
    ("report-test", TESTS + "test_module_report.py", "Report acceptance checks",
     "Independent Decimal oracle, mutation, parsed HTML, scratch paths and deterministic rebuild"),
    ("layout", EXEMPLAR, "Report layout exemplar", "Embedded house stylesheet"),
)


def source_record() -> dict:
    """Record exact local evidence bytes without recording execution paths."""
    sources = [{
        "id": "source-" + key, "title": title, "path": path, "locator": locator,
        "sha256": hashlib.sha256((ROOT / path).read_bytes()).hexdigest(),
        "use": "implementation evidence" if key != "hand" else "independent synthetic derivation",
    } for key, path, title, locator in SOURCE_SPECS]
    sources.extend([
        {"id": "source-prefixes", "title": "NIST Metric (SI) Prefixes",
         "url": "https://www.nist.gov/pml/owm/metric-si-prefixes",
         "locator": "Prefixes table: kilo, mega and milli",
         "use": "Exact unit factors; no engineering calibration"},
        {"id": "source-roundoff", "title": "Python system floating-point information",
         "url": "https://docs.python.org/3/library/sys.html#sys.float_info",
         "locator": "sys.float_info.epsilon; gamma bound derived in report acceptance tests",
         "use": "Floating-point roundoff only"},
    ])
    return {"report": CHANGES["report"], "revision": REVISION, "sources": sources}


def calculate(scratch_dir: Path) -> dict:
    """Supply the analytic lookup explicitly; retain unrounded module results."""
    scratch = Path(scratch_dir)
    scratch.mkdir(parents=True, exist_ok=True)
    lookup_path = scratch / "synthetic_uniform_bar_lookup.csv"
    with lookup_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(("tension_kn", "stress_mpa"))
        writer.writerows(LOOKUP_ROWS)
    converter = TensionStressConverter(
        lookup_table_path=str(lookup_path), output_dir=str(scratch / "stress-output"),
    )
    # Synthetic area and SI factors: source-hand and source-prefixes.
    basis = {"area_mm2": 2500.0, "newtons_per_kn": 1000.0,
             "m2_per_mm2": 1e-6, "pascals_per_mpa": 1e6}
    basis["area_m2"] = basis["area_mm2"] * basis["m2_per_mm2"]
    basis["slope_mpa_per_kn"] = basis["newtons_per_kn"] / basis["area_m2"] / basis["pascals_per_mpa"]
    cases = []
    for regime, tension in PROBES:
        expected = tension * basis["newtons_per_kn"] / basis["area_m2"] / basis["pascals_per_mpa"]
        stress = float(converter.convert_tension_to_stress(tension))
        cases.append({"regime": regime, "tension_kn": tension,
                      "stress_mpa": stress, "expected_mpa": expected,
                      "absolute_error_mpa": abs(stress - expected)})
    # source-report-test derives 16 operations, magnification 4 and scale 200 MPa.
    unit_roundoff = sys.float_info.epsilon / 2
    gamma = 16 * unit_roundoff / (1 - 16 * unit_roundoff)
    tolerance = gamma * 4 * max(row["expected_mpa"] for row in cases)
    return {
        "basis": basis, "lookup_rows": [list(row) for row in LOOKUP_ROWS],
        "units": {"area_mm2": "mm^2", "area_m2": "m^2", "newtons_per_kn": "N/kN",
                  "m2_per_mm2": "m^2/mm^2", "pascals_per_mpa": "Pa/MPa",
                  "slope_mpa_per_kn": "MPa/kN", "tension_kn": "kN",
                  "stress_mpa": "MPa", "expected_mpa": "MPa", "absolute_error_mpa": "MPa"},
        "cases": cases, "tolerance": {"absolute_mpa": tolerance, "relative": 0},
        "comparison": {"max_error_mpa": max(row["absolute_error_mpa"] for row in cases)},
    }


def lookup(record: dict, locator: str):
    """Resolve a numeric binding in the calculation record."""
    for key in locator.split("."):
        record = record[int(key)] if isinstance(record, list) else record[key]
    return record


def escaped_json(record: dict) -> str:
    """Keep JSON data inert in an HTML script element."""
    return (json.dumps(record, indent=2, sort_keys=True, ensure_ascii=True, allow_nan=False)
            .replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e"))


def ref(key: str) -> str:
    """Link a method or numerical basis to an in-document evidence record."""
    return f'[<a href="#source-{html.escape(key, quote=True)}">{html.escape(key)}</a>]'


def table(headers, rows, caption) -> str:
    """Render trusted markup cells and escape all plain headers and captions."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + "</tbody><caption>" + html.escape(caption) + "</caption></table></div>")


def method_map() -> str:
    """Distinguish implemented code, checked arithmetic and unevaluated use."""
    methods = [
        ("Load scaling", "LoadScalingProcessor; squared wind-speed ratio and wave-height ratio; clipped factors; summed histories.",
         "IMPLEMENTED", "NOT_EVALUATED here; baseline test cannot collect.", "scaling"),
        ("Reference-sea-state processing", "ReferenceSeaStateProcessor; select histories, scale, combine, count, convert, accumulate damage.",
         "IMPLEMENTED", "NOT_EVALUATED; timestamps and case completeness need qualification.", "processor"),
        ("Lookup nodes", "Offline TensionStressConverter; supplied synthetic force/area table.",
         "IMPLEMENTED", "INDEPENDENTLY_CHECKED against uniform-bar equilibrium only.", "hand"),
        ("Lookup interpolation", "Offline converter; linear segments between supplied nodes.",
         "IMPLEMENTED", "INDEPENDENTLY_CHECKED against uniform-bar equilibrium only.", "offline"),
        ("Lookup extrapolation", "Offline converter; endpoint segment continued outside the supplied table.",
         "IMPLEMENTED", "INDEPENDENTLY_CHECKED on synthetic probes only; no physical extrapolation authority.", "offline"),
        ("Exported converter", "TensionToStressConverter; numpy.interp clamps outside the table; separate CSV schema.",
         "IMPLEMENTED", "NOT_EVALUATED; distinct from the offline example.", "processor"),
        ("Counting", "Exported rainflow_counter delegates to shared reversal counting and applies an optional range gate.",
         "IMPLEMENTED", "NOT_EVALUATED by this report; registry marks fapps_counter conforming.", "counter"),
        ("Alternate counting handoff", "rainflow_counting.process_batch calls refuse_damage_calculation before reading files.",
         "GUARDED", "NOT_EVALUATED; diagnostic counting stays callable; damage handoff raises.", "counting"),
        ("Damage processing", "Exported calculator: stress concentration, curve evaluation, cycle ratios, occurrence weighting and annualisation.",
         "IMPLEMENTED", "NOT_EVALUATED; cycle/stress threshold inconsistency remains a finding.", "damage"),
        ("Alternate damage processing", "Separate curve library and damage calculator; not the package export.",
         "IMPLEMENTED", "NOT_EVALUATED; no alternate curve is used here.", "alternate"),
        ("Validation framework", "FatigueValidator.validate_all returns NOT_IMPLEMENTED.",
         "PLACEHOLDER", "NOT_EVALUATED; no clearance is inferred.", "validation"),
    ]
    return table(["Route", "Source-inspected data flow", "Implementation", "Evidence disposition", "Source"],
                 [[*(html.escape(cell) for cell in row[:-1]), ref(row[-1])] for row in methods],
                 "Method map. Implementation status does not establish numerical or physical qualification.")


def build(record: dict) -> str:
    """Render the ten-section report from the calculated record."""
    def n(key, equation, source="hand", scientific=False):
        unit = record["units"].get(key.split(".")[-1], "MPa")
        fmt = ".1e" if scientific else ".1f"
        return (f'<span data-value="{html.escape(key)}" data-unit="{html.escape(unit)}" '
                f'data-equation="{html.escape(equation, quote=True)}" data-source="#source-{html.escape(source)}" '
                f'data-format="{"scientific" if scientific else "fixed"}">{format(lookup(record, key), fmt)}</span> '
                + html.escape(unit))

    titles = ("Executive Summary", "Introduction", "Data Basis", "Theory and Equations",
              "Worked Example", "Validation Evidence", "Limits of Use", "Recommendations",
              "References", "Appendix")
    nav = "".join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(titles, 1))
    style = re.search(r"<style>(.*?)</style>", (ROOT / EXEMPLAR).read_text(encoding="utf-8"), re.S)[1]
    basis_rows = [
        ["Synthetic uniform-bar area", n("basis.area_mm2", "declared synthetic geometry"), ref("hand")],
        ["Converted cross-sectional area", n("basis.area_m2", "A_m2=A_mm2*m2_per_mm2", scientific=True), ref("hand")],
        ["Kilo force factor", n("basis.newtons_per_kn", "kilo=10^3", "prefixes", True), ref("prefixes")],
        ["Squared milli length factor", n("basis.m2_per_mm2", "milli^2=(10^-3)^2", "prefixes", True), ref("prefixes")],
        ["Mega pressure factor", n("basis.pascals_per_mpa", "mega=10^6", "prefixes", True), ref("prefixes")],
        ["Derived uniform-bar slope", n("basis.slope_mpa_per_kn", "newtons_per_kn/A_m2/pascals_per_mpa"), ref("hand")],
    ]
    worked = []
    for i, case in enumerate(record["cases"]):
        root = f"cases.{i}."
        worked.append([html.escape(case["regime"]),
                       n(root + "tension_kn", "synthetic table or probe"),
                       n(root + "stress_mpa", "linear lookup segment", "offline"),
                       n(root + "expected_mpa", "sigma=F/A"),
                       n(root + "absolute_error_mpa", "abs(module-hand)", "report-test")])
    numeric_table = table(["Regime", "Tension (kN)", "Module stress (MPa)", "Hand stress (MPa)",
                           "Absolute difference (MPa)"], worked,
                          "Synthetic results. Input stresses at nodes are the declared lookup table; other rows are separate interpolation and extrapolation probes.")
    revisions = table(["Revision", "Date", "What changed and why"], [[REVISION, DATE,
        "First issue: separated the synthetic converter check from unresolved counting, damage and physical stress-transfer qualification."]],
        "Revision history. Earlier issue records will be retained on re-issue.")
    limits = table(["Method or intended use", "Status", "Evidence required before use"], [
        ["Real stress-transfer tables", "NOT_EVALUATED", "Independently reviewed geometry, boundary conditions, units and calibrated stress-transfer evidence."],
        ["Stress-concentration factors", "NOT_EVALUATED", "A documented local stress definition, geometric detail and independent calibration."],
        ["Fatigue damage", "NOT_EVALUATED", "Qualified curve provenance, dimensionally consistent branch selection and independently checked accumulation."],
        ["Counting qualification", "NOT_EVALUATED", "Independent reference histories, residual-cycle policy, gate sensitivity and full range/count agreement."],
        ["Service life", "NOT_EVALUATED", "Complete operating cases, occurrence basis, measured sample timing and a qualified damage calculation."],
    ], "Limits of use. Missing engineering evidence is not represented by a zero result.")
    comparison = table(["Quantity", "Value (MPa)", "Equation and basis"], [
        ["Maximum unrounded absolute difference", n("comparison.max_error_mpa", "max(abs(module-hand))", "report-test"), ref("report-test")],
        ["Absolute comparison budget", n("tolerance.absolute_mpa", "gamma_16*4*200", "report-test", True), ref("report-test")],
    ], "Numerical comparison. Displayed differences are rounded; the decision uses unrounded values.")
    passes = record["comparison"]["max_error_mpa"] <= record["tolerance"]["absolute_mpa"]
    disposition = ("The maximum unrounded difference meets the roundoff criterion." if passes else
                   "The maximum unrounded difference exceeds the roundoff criterion; the synthetic check fails.")
    sources = []
    for source in source_record()["sources"]:
        href = "../../../../" + source["path"] if "path" in source else source["url"]
        digest = f' SHA-256: <code>{source["sha256"]}</code>.' if "sha256" in source else ""
        sources.append(f'<li id="{source["id"]}"><a href="{html.escape(href, quote=True)}">'
                       f'{html.escape(source["title"])}</a>. {html.escape(source["locator"])}.'
                       f' {html.escape(source["use"])}.{digest}</li>')
    changes = "".join(f"<li>{html.escape(change)}</li>" for change in CHANGES["changes"])
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fatigue applications module analysis: load scaling and stress conversion</title>
<style>{style}</style>
<style>pre{{white-space:pre-wrap;overflow-wrap:anywhere}}code{{overflow-wrap:anywhere}}
:focus-visible{{outline:3px solid var(--report-accent)}}#engineering-changes{{border:1px solid var(--line);padding:16px}}</style>
</head><body class="hs-doc" id="fatigue-applications" data-revision="{REVISION}">
<header><h1>Fatigue applications module analysis: load scaling and stress conversion</h1>
<p>Draft for maintainer review · Revision {REVISION} · {DATE}</p>
<p>Synthetic example; numerical software check only</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The fatigue applications package implements reference-sea-state load scaling, cycle counting,
stress conversion and fatigue damage processing. This analysis checks only the offline converter
against a declared uniform-bar force/area surrogate. The synthetic lookup nodes, interpolation
and extrapolation are checked separately against the roundoff criterion in Section 6.</p>
<p>{disposition} Real stress-transfer tables, stress-concentration factors, counting qualification,
fatigue damage and service life remain NOT_EVALUATED. The way forward is to qualify the missing
inputs and methods before any engineering life assessment is issued.</p></section>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
<section id="section-2"><h2>2. Introduction</h2>
<p>The public package exports counting, damage, reference-sea-state processing and visualisation
routes {ref("exports")}. The existing standalone scaling example remains a separate input-driven
workflow {ref("example")}. The following method map describes source-inspected routes;
only the offline converter example is executed by this report.</p>
{method_map()}
<p>The exported <code>TensionToStressConverter</code> and offline
<code>TensionStressConverter</code> are different interfaces. The former clamps at the supplied
endpoints; the latter uses linear segment extrapolation {ref("processor")} {ref("offline")}.
Results from this example cannot be transferred between those interfaces without another check.</p></section>
<section id="section-3"><h2>3. Data Basis</h2>
<p>The geometry, table loads and probe loads are synthetic, taken from the independent uniform-bar
test basis {ref("hand")}. No measured, production or finite-element stress-transfer dataset is used.
The input lookup is written explicitly under the supplied scratch directory before construction;
both lookup and converter output paths are supplied. Default lookup creation is excluded.</p>
{table(["Quantity", "Value (unit stated)", "Basis and source"], basis_rows, "Input and derived quantity register. SI factors are definitions; geometry is synthetic.")}
<p>The node rows in Section 5 are the supplied synthetic lookup table in full. The other rows
are numerical probes. No concentration factor is supplied or applied; omission does not qualify
a physical stress-concentration factor. All values retain their stated units in the embedded record.</p></section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<p>For uniform axial stress on a constant area, force equilibrium gives
<code>F = integral(sigma dA) = sigma A</code>. Thus
<code>sigma_MPa = (T_kN * 1000) / (2500 * 10^-6) / 10^6 = 0.4 T_kN</code>.
The area is declared synthetic; kilo, milli and mega supply the exact conversion factors
{ref("hand")} {ref("prefixes")}. This relation does not model bending, local stress or structural flexibility.</p>
<p>The implemented converter evaluates a linear segment,
<code>sigma(T) = sigma_a + (sigma_b - sigma_a)(T - T_a)/(T_b - T_a)</code>.
Interior segments interpolate; the endpoint segments continue beyond the table because the
interpolator sets <code>fill_value="extrapolate"</code> {ref("offline")}.
Independent force/area arithmetic establishes the comparison, rather than the interpolator itself.</p>
<p>For the documented processing flow, wind factors use the squared speed ratio and wave factors
the height ratio; scaled tension histories are added {ref("scaling")}. Counting produces ranges and counts
{ref("counter")}. Damage processing evaluates a curve, sums count-to-failure ratios, applies occurrence
weighting and annualises by duration {ref("damage")}. These equations describe implemented flow;
no counting, curve, damage or annual-life result is calculated here.</p></section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>The supplied table and probes give the following converter results. Lookup nodes, interpolation
and extrapolation are identified separately. Module values come from fresh converter calls;
hand values come from force equilibrium on the synthetic area.</p>
{numeric_table}
<p>The absolute difference uses the unrounded values before display formatting.
This is an analytic surrogate, not FEA-derived evidence.</p></section>
<section id="section-6"><h2>6. Validation Evidence</h2>
<p>{disposition}</p>{comparison}
<p>The comparison allowance is derived for this example: binary64 unit roundoff is
<code>u = epsilon/2</code>; <code>gamma_16 = 16u/(1 - 16u)</code>. The segment expression uses
six rounded operations; force/area with unit conversion uses four; comparison uses one;
the non-dyadic prefix factor needs one representation allowance. Four reserve operations
give the stated budget {ref("report-test")}. A magnification allowance of
<code>4.0</code> exceeds the largest segment coordinate <code>8/3</code> at the upper probe;
the stress scale is the maximum hand value <code>200.0 MPa</code>. The absolute criterion is
<code>gamma_16 * 4 * 200 MPa</code>, with zero relative tolerance {ref("report-test")} {ref("roundoff")}.
The budget applies only to these finite synthetic probes and accounts for numerical roundoff,
not uncertainty in geometry, calibration or operating conditions.</p>
<p>The existing converter suite checks table nodes, interpolation, extrapolation, unsorted rows
and query order {ref("hand")}. The new suite checks the unrounded oracle, every displayed quantity and unit,
a fresh independent module call, controlled result mutation, inert JSON, isolated paths and identical rebuild bytes
{ref("report-test")}. Reproducibility does not establish physical validation.</p>
{table(["Suite", "Collected (tests)", "Passed (tests)", "Failed (tests)", "Skipped (tests)", "Collection errors", "Before / after disposition"], [
    ["Converter hand calculations", "3 / 3", "3 / 3", "0 / 0", "0 / 0", "0 / 0", "Unchanged synthetic arithmetic scope; source-hand."],
    ["Load scaling", "0 / 0", "0 / 0", "0 / 0", "0 / 0", "1 / 1", "Baseline import error: LoadScaler is absent; source-scaling-test."],
], "Existing suite baseline and final counts. An import error is distinct from a failed or skipped test.")}
<p>The load-scaling suite imports a name absent from the implementation; the package defines
<code>LoadScalingProcessor</code> {ref("scaling-test")} {ref("scaling")}. This baseline is recorded;
it is not replaced with a compatibility alias or counted as passing evidence.</p></section>
<section id="section-7"><h2>7. Limits of Use</h2>{limits}
<p>The offline converter can create an assumed default table when input is missing or malformed,
and can use a fallback slope for an underspecified table {ref("offline")}. Explicit synthetic input prevents
those branches in this example; the production behaviours remain unchanged.</p>
<p>The alternate counter batch route calls <code>refuse_damage_calculation</code> and raises before file
processing {ref("counting")} {ref("contract")}. Diagnostic counting remains available. The contract registry
records separate verdicts for the exported and alternate counters; neither is newly qualified by this report.</p>
<p>The exported damage calculator compares stress against a threshold documented in cycles
{ref("damage")}. Reference processing truncates histories, discards their timestamps during combination,
annualises using an assumed sampling interval, and can represent missing damage cases as infinite life
{ref("processor")}. These are correctness concerns requiring separate qualification, not evidence of a service-life result.
The knowledge pack contains historical checks whose code pins and line references are not treated as current clearance
{ref("pack")}. The validation framework remains a placeholder {ref("validation")}.</p></section>
<section id="section-8"><h2>8. Recommendations</h2>
<p>Retain this example as a converter regression and a reproducible report scaffold.
Before physical use, supply a reviewed stress-transfer table with its geometry, stress definition,
units and calibration; establish concentration factors separately. Reject unqualified fallbacks
in the consuming engineering assessment rather than interpreting their presence as evidence.</p>
<p>Resolve the stale load-scaling test interface and test the current processor against independent scaling
and superposition controls. Qualify the selected counting path with complete histories and residual-cycle policy;
retain the existing refusal for the alternate damage handoff. Resolve the stress/cycle threshold inconsistency,
curve provenance, timestamp alignment and missing-case handling before independent damage accumulation and life checks.
The parent module report issue remains open for this broader numerical qualification.</p></section>
<section id="section-9"><h2>9. References</h2>
<p>Implementation records identify inspected code; the independent uniform-bar derivation supplies
the numerical comparison. SI prefixes supply unit definitions only. No licensed standard table or solver output
is used. Source digests are retained in <code>sources.json</code> and below.</p><ol>{"".join(sources)}</ol></section>
<section id="section-10"><h2>10. Appendix</h2>
<p><code>T</code>: tension in kN; <code>sigma</code>: stress in MPa; <code>A</code>: area;
<code>SCF</code>: stress-concentration factor; <code>FEA</code>: finite-element analysis.
NOT_EVALUATED means the stated engineering use lacks the required evidence.</p>
<p>Run from the repository root with the converter dependencies installed and
<code>TMPDIR</code> set to the supplied scratch directory. The explicit output folder receives the page,
source record and change record; all calculation intermediates stay in the explicit scratch folder.</p>
<pre><code>{html.escape(REBUILD)}</code></pre>
<details id="engineering-changes" open><summary>Engineering changes · Revision {REVISION}</summary>
<ul>{changes}</ul></details>
<script type="application/json" id="calculation-record">{escaped_json(record)}</script>
<script type="application/json" id="change-record">{escaped_json(CHANGES)}</script>
</section></main></div></body></html>
'''


def write_report(output_dir: Path, scratch_dir: Path) -> None:
    """Write the complete deterministic deliverable set in the requested folder."""
    record = calculate(scratch_dir)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(build(record), encoding="utf-8", newline="\n")
    for name, data in (("changes.json", CHANGES), ("sources.json", source_record())):
        (output_dir / name).write_text(json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + "\n",
                                      encoding="utf-8", newline="\n")


def main() -> None:
    """Require explicit destinations for the report and every intermediate."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--scratch-dir", type=Path, required=True)
    args = parser.parse_args()
    write_report(args.output_dir, args.scratch_dir)


if __name__ == "__main__":
    main()
