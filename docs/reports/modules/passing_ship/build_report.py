# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the bounded passing-ship module report with offline section helpers."""

from __future__ import annotations

import argparse
from fractions import Fraction
import html
import importlib.util
import json
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parents[4]
REVISION = "00"
DATE = "2026-10-28"  # Fixed issue date, not a build timestamp.
PACKAGE = "src/digitalmodel/hydrodynamics/passing_ship/"
EXEMPLAR = "docs/reports/pipeline-ffs-assessment.html"
REBUILD = ('PYTHONPATH=src uv run --no-project --with pytest python '
           'docs/reports/modules/passing_ship/build_report.py '
           '--output-dir "$TMPDIR/passing-ship-report"')
# All numerical inputs are the synthetic definitions of H1 in README.md.
LENGTH_M = 4.0
STATIONS_M = (-2.0, -1.0, 0.0, 1.0, 2.0)
CHANGES = {
    "report": "passing-ship-section-analysis",
    "revision": REVISION,
    "date": DATE,
    "changes": [
        "Added the module method map and synthetic section analysis to distinguish implementation from independent evidence.",
        "Bound visible results to shipped section helpers and recorded physical-force qualification as NOT_EVALUATED.",
        "Preserved the standalone example and added isolated deterministic rebuild and calculation-mutation checks.",
    ],
}


def source(identifier, title, path, symbols, use):
    """Describe a repository source without claiming external qualification."""
    return {"id": identifier, "title": title, "path": path,
            "symbols": symbols, "use": use}


SOURCES = {"sources": [
    source("source-h1", "H1 synthetic boundary conditions and independent derivation",
           "docs/reports/modules/passing_ship/README.md", [], "Input basis and hand oracle H1"),
    source("source-formulations", "Offline section, kernel and force functions",
           PACKAGE + "formulations.py",
           ["s1_function", "s2_function", "ds1_dx", "ds2_dx", "f_kernel", "g_kernel",
            "calculate_surge_force_infinite", "calculate_sway_force_infinite",
            "calculate_yaw_moment_infinite", "finite_depth_correction", "calculate_forces_with_depth"],
           "Only s1_function and ds1_dx evaluated by this report"),
    source("source-config", "Configuration models and YAML parsing",
           PACKAGE + "configuration.py",
           ["VesselConfig", "EnvironmentalConfig", "CalculationConfig", "PassingShipConfig",
            "YAMLConfigParser", "UnitConverter", "ConfigurationMerger"], "Source inspection only"),
    source("source-calculator", "Calculator, batch scenarios and result cache",
           PACKAGE + "calculator.py",
           ["PassingShipCalculator", "ForceResults", "ResultCache", "from_config_file",
            "from_imperial", "calculate_forces", "calculate_batch", "parametric_study"],
           "Source inspection only"),
    source("source-cli", "Command-line single and batch processing",
           PACKAGE + "cli.py", ["create_parser", "process_single", "process_batch", "main"],
           "Source inspection only; not invoked"),
    source("source-workflow", "Configuration workflow and CSV output",
           PACKAGE + "workflow.py", ["router"], "Source inspection only; not invoked"),
    source("source-benchmark", "Existing benchmark report generator",
           PACKAGE + "benchmark_report.py", ["generate_benchmark_report"],
           "Source inspection only; no benchmark values adopted and no sweep run"),
    source("source-preparation", "Prepared cases and deferred benchmark comparison",
           PACKAGE + "preparation.py", ["execute_cases", "compare_benchmark", "launch_solver"],
           "Source inspection only; no execution or launch"),
    source("source-schema", "Canonical analysis specification and force history",
           PACKAGE + "input_schemas.py", ["PassingShipSpec"], "Source inspection only"),
    source("source-history", "Force time history",
           PACKAGE + "force_time_history.py", ["ForceTimeHistory", "generate_time_history"],
           "Source inspection only"),
    source("source-conventions", "Package sign and unit conventions",
           PACKAGE + "CONVENTIONS.md", [], "Documentation discrepancy finding; not a qualification basis"),
    source("source-methodology", "Existing methodology description",
           "docs/domains/marine-engineering/python-code-passing-ship/methodology.md", [],
           "Documentation discrepancy finding; not a qualification basis"),
    source("source-status", "Recorded benchmark deferral",
           "docs/domains/passing_ship/preparation/benchmark_status.json", [],
           "Numeric reference and peak-window evidence remain incomplete"),
    source("source-test", "Independent report regression",
           "tests/hydrodynamics/passing_ship/test_module_report.py",
           ["test_worked_values_match_independent_calculation", "test_visible_values_follow_computed_results",
            "test_report_rebuild_is_deterministic", "test_scope_limits_and_report_pointers"],
           "Independent oracle, visible readback, mutation and reproduction checks"),
    source("source-style", "Repository report presentation pattern", EXEMPLAR, [], "Inline report style only"),
]}


def load_formulations():
    """Load shipped helpers directly, avoiding plotting and solver imports."""
    spec = importlib.util.spec_from_file_location(
        "report_section_formulations", ROOT / PACKAGE / "formulations.py")
    if spec is None or spec.loader is None:
        raise ImportError("Section formulations could not be loaded")
    module = importlib.util.module_from_spec(spec)
    previous = sys.dont_write_bytecode
    try:
        sys.dont_write_bytecode = True
        spec.loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = previous
    return module


def calculate() -> dict:
    """Compute only the synthetic section; retain unrounded values and units."""
    helpers = load_formulations()
    rows = []
    for x in STATIONS_M:
        shape = helpers.s1_function(x, LENGTH_M)
        slope = helpers.ds1_dx(x, LENGTH_M)
        # H1: independently solve boundary equations, then substitute each x.
        expected_shape = float(1 - Fraction(x) ** 2 / 4)
        expected_slope = float(-Fraction(x) / 2)
        rows.append({
            "x_m": x, "shape": shape, "slope_per_m": slope,
            "expected_shape": expected_shape, "expected_slope_per_m": expected_slope,
            "shape_error": abs(shape - expected_shape),
            "slope_error_per_m": abs(slope - expected_slope),
            "meaning": "one-sided from interior" if abs(x) == LENGTH_M / 2 else "interior derivative",
        })
    return {"length_m": LENGTH_M, "rows": rows, "units": {
        "length_m": "m", "x_m": "m", "shape": "dimensionless", "slope_per_m": "m^-1",
        "expected_shape": "dimensionless", "expected_slope_per_m": "m^-1",
        "shape_error": "dimensionless", "slope_error_per_m": "m^-1",
    }}


def json_text(value) -> str:
    """Serialize deterministically and prevent script-element termination."""
    return (json.dumps(value, indent=2, ensure_ascii=True, allow_nan=False)
            .replace("<", "\\u003c").replace(">", "\\u003e").replace("&", "\\u0026") + "\n")


def table(headers, rows, caption) -> str:
    """Render trusted markup cells with escaped headers and caption."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + f"</tbody><caption>{html.escape(caption)}</caption></table></div>")


def ref(identifier, label) -> str:
    """Link to a source entry within this self-contained page."""
    return f'<a href="#{html.escape(identifier, quote=True)}">{html.escape(label)}</a>'


def render(record: dict) -> str:
    """Render visible quantities from the supplied calculation, never literals."""
    def number(key):
        value = record
        for part in key.split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        unit = record["units"][key.split(".")[-1]]
        shown = f"{value:.1f}".replace("-0.0", "0.0")
        return (f'<span data-value="{html.escape(key)}" data-unit="{html.escape(unit)}" '
                f'data-equation="H1">{html.escape(shown)}</span>')

    titles = ["Executive Summary", "Introduction", "Data Basis", "Theory and Equations",
              "Worked Example", "Validation Evidence", "Limits of Use", "Recommendations",
              "References", "Appendix"]
    nav = "".join(f'<li><a href="#section-{i}">{title}</a></li>'
                  for i, title in enumerate(titles, 1))
    revisions = table(["Revision", "Date", "What changed and why"], [
        [REVISION, DATE, "First module report: bounded synthetic section evidence and explicit deferred qualification."]],
        "Revision history")
    changes = "".join(f"<li>{html.escape(item)}</li>" for item in CHANGES["changes"])
    inputs = table(["Input", "Value", "Unit", "Basis"], [
        ["Section length L", number("length_m"), "m", ref("source-h1", "H1 synthetic definition")],
        ["Stations", "See computed station column below", "m", ref("source-h1", "H1 synthetic definition")],
        ["Boundary values", "Zero endpoints; unit midpoint", "dimensionless", ref("source-h1", "H1 synthetic constraints")],
    ], "Table 1. Synthetic inputs; no measured geometry or force inputs.")
    keys = ("x_m", "shape", "expected_shape", "shape_error", "slope_per_m",
            "expected_slope_per_m", "slope_error_per_m")
    results = table(["x [m]", "S computed [dimensionless]", "S H1 [dimensionless]",
                     "Absolute difference [dimensionless]", "Slope computed [m^-1]",
                     "Slope H1 [m^-1]", "Absolute difference [m^-1]", "Slope meaning"],
                    [[number(f"rows.{i}.{key}") for key in keys] + [html.escape(row["meaning"])]
                     for i, row in enumerate(record["rows"])],
                    "Table 2. Computed values use section helpers; controls and units follow H1. Display rounded to one decimal place.")
    method_rows = [
        [ref("source-formulations", "s1_function; ds1_dx"), "Implemented", "H1 at stated stations", "INDEPENDENTLY_CHECKED: section only"],
        [ref("source-formulations", "s2_function; ds2_dx; f_kernel; g_kernel"), "Implemented", "Not checked by H1", "NOT_EVALUATED"],
        [ref("source-formulations", "Surge, sway, yaw and depth wrappers"), "Implemented", "No physical reference comparison here", "NOT_EVALUATED"],
        [ref("source-config", "Configuration, parser, conversion and merging"), "Implemented", "Source inspection only", "NOT_EVALUATED"],
        [ref("source-calculator", "PassingShipCalculator; calculate_batch; parametric_study"), "Implemented", "Source inspection only", "NOT_EVALUATED"],
        [ref("source-cli", "CLI single and batch entry points"), "Implemented", "Source inspection only", "NOT_EVALUATED"],
        [ref("source-workflow", "router configuration workflow"), "Implemented", "Source inspection only", "NOT_EVALUATED"],
        [ref("source-schema", "PassingShipSpec") + "; " + ref("source-history", "generate_time_history"), "Implemented", "Source inspection only", "NOT_EVALUATED"],
        [ref("source-benchmark", "generate_benchmark_report") + "; " + ref("source-preparation", "compare_benchmark"), "Implemented", "Source inspection only; no benchmark executed", "NOT_EVALUATED"],
    ]
    methods = table(["Method / source", "Implementation", "Independent evidence in this report", "Qualification status"],
                    method_rows, "Table 3. Implementation is established by source inspection, independently checked scope by H1 only.")
    limits = table(["Quantity or qualification", "Status"], [
        [category, "NOT_EVALUATED"] for category in (
            "Physical forces", "Force coefficients", "Separation/depth validity", "Full force-calculator qualification")],
        "Table 4. Deferred engineering evidence.")
    references = "".join(
        f'<li id="{html.escape(s["id"])}"><a href="../../../../{html.escape(s["path"])}">'
        f'{html.escape(s["title"])}</a>: {html.escape(", ".join(s["symbols"]) or s["use"])}. '
        f'{html.escape(s["use"])}</li>' for s in SOURCES["sources"])
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Passing-ship module analysis: synthetic section and qualification limits</title>
<style>{style}
pre{{white-space:pre-wrap;overflow-wrap:anywhere}}code{{overflow-wrap:anywhere}}
:focus-visible{{outline:3px solid var(--report-accent);outline-offset:3px}}
</style></head>
<body class="hs-doc"><header>
<h1>Passing-ship module analysis: synthetic section and qualification limits</h1>
<p>Draft for maintainer review</p><p>Revision {REVISION} · {DATE}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The section-shape helpers are checked against an independent synthetic quadratic.
The report provides a source-linked module map and reproducible section results.
Physical-force qualification is NOT_EVALUATED; the parent issue remains open for broader numerical qualification.</p>
<p>The solution for this bounded example is the normalized parabola H1 and its interior derivative.
The way forward is to reconcile force conventions, obtain independent numeric references and establish an applicability envelope before using forces.</p>
</section>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
<section id="section-2"><h2>2. Introduction</h2>
<p>The package exposes configuration, force calculation, batch processing, histories and plotting interfaces.
This report calls only <code>s1_function</code> and <code>ds1_dx</code>, loaded directly from the shipped source.
NumPy and SciPy are needed by that file; package initializers, benchmark generators and solver adapters are not imported by this builder.</p>
<p>Normalized shape is dimensionless, while a dimensional sectional area requires an additional physical area scale.
The synthetic section illustrates an algebraic property, without an engineering load campaign.</p>
</section>
<section id="section-3"><h2>3. Data Basis</h2>
{inputs}
<p>Every numerical input is synthetic under H1. Stations are measured from the section midpoint.
No area scale, density, speed, separation or depth is supplied. There are no external reference values or adopted standards tables.</p>
</section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<div id="H1"><h3>H1: independent hand derivation</h3>
<p>Let S(x) = a*x^2 + b*x + c. The synthetic conditions S(-2 m) = S(2 m) = 0 and S(0) = 1 give
c = 1, 4 m^2*a - 2 m*b + 1 = 0, and 4 m^2*a + 2 m*b + 1 = 0.
Subtracting gives b = 0 m^-1; substitution gives a = -1/4 m^-2.</p>
<p>Thus S = 1 - x^2/(4 m^2) and dS/dx = -x/(2 m^2).
At x = (-2, -1, 0, 1, 2) m, direct substitution gives S = (0, 3/4, 1, 3/4, 0)
and interior slopes (1/2, 0, -1/2) m^-1. The endpoint polynomial limits are (1, -1) m^-1.</p>
<p>The shipped expressions S = 1 - (2*x/L)^2 and dS/dx = -8*x/L^2 reduce to H1 at the synthetic length.
They return zero outside the section. Endpoint slopes are one-sided limits from the interior;
the zero extension is not differentiable at the join at either endpoint.</p>
<p>Roundoff budget: all selected values and intermediate operations are exactly representable binary integers,
halves or quarters. Multiplication, squaring, division by powers of two and subtraction are exact here.
The absolute comparison tolerance is zero in dimensionless shape and m^-1 slope; relative tolerance is zero.
This budget applies only to these stations and operations, not to quadrature or a physical-force tolerance.</p></div>
</section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>Computed columns call the shipped helpers. H1 control columns evaluate the independent quadratic.
Absolute differences use unrounded values. All table quantities bind to H1 and the calculation record.
One decimal place is used for display; the exact rational controls and unrounded comparisons remain in the regression tests.
For example, the control 3/4 displays as 0.8 dimensionless; display rounding is not a numerical-error allowance.</p>
{results}
</section>
<section id="section-6"><h2>6. Validation Evidence</h2>
{methods}
<p>The independent regression checks every unrounded example quantity and its units. Parsed HTML readback compares
each visible quantity with the hand oracle and a fresh helper call. A controlled helper mutation must change visible results
and fail oracle comparison. Rebuilds in isolated directories compare all delivered bytes, including source and change records.</p>
<p>Reproducibility is not physical validation. Existing section, kernel, force-property and depth tests are regression coverage;
their passing status alone supplies no independent physical-force reference or separation/depth envelope.</p>
<p>Validation commands and measured counts are recorded with the change review; this static report embeds no runtime test count or timestamp.</p>
</section>
<section id="section-7"><h2>7. Limits of Use</h2>
{limits}
<p>No force in N or moment in N m is computed. Force coefficients, calibration, force-sign qualification,
finite-depth suitability, mooring response and solver qualification remain outside this example.</p>
<p>Finding: the existing methodology describes positive lateral direction to port, while package conventions say starboard.
The methodology also describes dimensional sectional areas, whereas these section helpers return normalized shape.
The convention document's force-scaling narrative and the calculator's delegated force path require reconciliation.
These are source-inspection findings, not a determination of correct physical signs or scaling.</p>
<p>The existing benchmark deferral records an unavailable numeric reference. H1 does not resolve that gap,
establish a peak-search window or justify applying another comparison's force tolerance.</p>
</section>
<section id="section-8"><h2>8. Recommendations</h2>
<ol><li>Reconcile documented axes, section normalization, area scaling and the actual calculator path against an independently reviewed method basis.</li>
<li>Obtain a public numeric force/coefficient reference with geometry, units, signs and uncertainty; establish the peak-search window and error budget before comparison.</li>
<li>Check quadrature convergence and depth/separation applicability on that basis, then review histories and downstream response separately.</li>
<li>Retain NOT_EVALUATED for the deferred categories until their evidence is available. Keep the parent issue open.</li></ol>
</section>
<section id="section-9"><h2>9. References</h2>
<p>Sources are repository files inspected for this report; no external publication values or licensed material are reproduced.</p>
<ol>{references}</ol>
</section>
<section id="section-10"><h2>10. Appendix</h2>
<h3>Entry points inspected, not executed</h3>
<p><code>YAMLConfigParser.parse_file/parse_string</code> builds validated configuration models.
<code>VesselConfig</code> requires length, beam and draft in m and a dimensionless block coefficient;
<code>EnvironmentalConfig</code> carries depth in m, density in kg/m^3 and current speed in m/s.
An absent depth selects the infinite-depth path. <code>CalculationConfig</code> carries stagger and lateral separation in m,
passing speed in m/s and numerical integration/cache settings. <code>PassingShipConfig</code> combines both geometries,
environment, calculation settings and output-unit selection. Input-model checks do not establish a physical validity envelope.</p>
<p>
<code>PassingShipCalculator.from_config_file</code> constructs a calculator; <code>from_imperial</code> converts dimensional inputs.
<code>calculate_forces(separation, stagger, velocity)</code> returns surge, sway and yaw, with optional caching.
<code>calculate_batch(scenarios, parallel, n_workers)</code> consumes scenario dictionaries;
<code>parametric_study</code> varies one parameter. These descriptions are source inspection only.</p>
<p>The CLI <code>process_single</code> reads a configuration; <code>process_batch</code> discovers YAML files with a pattern,
supports recursive discovery and a dry run, and optionally uses a thread pool. The workflow <code>router</code> writes scenario forces to CSV.
<code>generate_benchmark_report</code> builds a separate sweep report with runtime metadata;
<code>compare_benchmark</code> requires supplied numeric references and criteria and does not qualify peak sampling.
None of these entry points is run for this report.</p>
<h3>Symbols and rebuild</h3>
<p>L: section length in m; x: midpoint-relative station in m; S: normalized dimensionless shape;
dS/dx: slope in m^-1. Force interfaces use SI force N and moment N m; neither is a result here.</p>
<p>With NumPy and SciPy available in the active environment, run from the repository root with a supplied writable scratch directory:</p>
<pre>{html.escape(REBUILD)}</pre>
<p>The builder writes only report.html, changes.json and sources.json in the explicit output directory.
All content is embedded; file links are repository-relative source locators. There are no output-directory paths,
runtime timestamps, external resources or import-time outputs. The standalone example CLI and output remain separate.</p>
<details id="engineering-changes"><summary>Engineering changes</summary><ul>{changes}</ul></details>
</section>
</main></div>
<script type="application/json" id="calculation-record">{json_text(record)}</script>
</body></html>
'''


def build(output_dir: Path) -> Path:
    """Write exactly the report and its deterministic source/change records."""
    contents = {"report.html": render(calculate()), "changes.json": json_text(CHANGES),
                "sources.json": json_text(SOURCES)}
    output_dir.mkdir(parents=True, exist_ok=True)
    for name in contents:
        if (output_dir / name).is_symlink():
            raise ValueError("Report targets must not be symbolic links")
    for name, content in contents.items():
        (output_dir / name).write_text(content, encoding="utf-8")
    return output_dir / "report.html"


def main() -> None:
    """Require an explicit destination for every requested rebuild."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir)
    print("Wrote report.html, changes.json and sources.json")


if __name__ == "__main__":
    main()
