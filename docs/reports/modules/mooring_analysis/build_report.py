# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the bounded touchdown catenary report into an explicit directory.

Run the scratch-only rebuild command in the adjacent README from the repo root.
Only CatenaryAnalyzer.solve_catenary is calculated; other routes are documented.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
from pathlib import Path

from digitalmodel.subsea.mooring_analysis.catenary import CatenaryAnalyzer
from digitalmodel.subsea.mooring_analysis.models import LineType, MooringLineProperties

ROOT = Path(__file__).resolve().parents[4]
REVISION, DATE = "00", "2026-10-28"  # Fixed issue revision, never runtime metadata.
CASE = "tests/subsea/mooring_analysis/test_catenary_geometry.py"
PACKAGE = "src/digitalmodel/subsea/mooring_analysis/"
TESTS = "tests/subsea/mooring_analysis/"
HERE = "docs/reports/modules/mooring_analysis/"
REBUILD = (
    'PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project '
    '--python "$TMPDIR/venv/bin/python" python '
    'docs/reports/modules/mooring_analysis/build_report.py '
    '--output-dir "$TMPDIR/mooring-report"'
)
CHANGES = {
    "report": "mooring-touchdown-catenary",
    "revision": REVISION,
    "date": DATE,
    "changes": [
        "Added a synthetic touchdown example with independent geometry and force balance, so numerical agreement is distinguished from design acceptance.",
        "Added a source-linked method map, explicit qualification gaps and a reproducible report builder with visible-value checks.",
    ],
}
# These are repository evidence, not a second material dataset or standards copy.
SOURCE_PATHS = {
    "case-a": (CASE, "test_vertical_span_closed_form: Case A; synthetic inputs and 0.1% numerical criterion"),
    "catenary": (PACKAGE + "catenary.py", "CatenaryAnalyzer: current touchdown formulation and other methods"),
    "models": (PACKAGE + "models.py", "MooringLineProperties, CatenaryResult and MooringLine.axial_stiffness"),
    "designer": (PACKAGE + "designer.py", "MooringDesigner: environmental, offset and condition screens"),
    "cli": (PACKAGE + "cli.py", "catenary_analysis, design_verification, generate_orcaflex_model"),
    "entry-point": ("pyproject.toml", "project.scripts: mooring-analysis"),
    "series-tests": (TESTS + "test_series_stiffness.py", "Independent series-elongation regressions; no geometric-stiffness qualification"),
    "pinned-tests": (TESTS + "test_pinned_catenary_example.py", "Separate CatenarySolver pinned-end route; not calculated by this report"),
    "module-pack": (PACKAGE + "docs/agent-pack.md", "Discovery context; historical formulas require reconciliation with current catenary.py"),
    "report-tests": (TESTS + "test_module_report.py", "Independent oracle, visible-result mutations, escaping and rebuild checks"),
    "builder": (HERE + "build_report.py", "Calculation and rendering implementation"),
}
UNITS = {
    "weight_per_length": "kN/m", "horizontal_tension": "kN",
    "vertical_height": "m", "catenary_parameter": "m", "arc_length": "m",
    "horizontal_distance": "m", "fairlead_tension": "kN", "touchdown_tension": "kN",
    "fairlead_angle": "degrees", "grounded_length": "m",
}
EQUATIONS = {
    "weight_per_length": "w=q*g/1000 (N to kN, exact SI prefix)",
    "horizontal_tension": "H=prescribed horizontal component",
    "vertical_height": "z=prescribed vertical span above touchdown",
    "catenary_parameter": "a=H/w",
    "arc_length": "s=sqrt(z*(z+2*a))=100*sqrt(3)",
    "horizontal_distance": "x=a*acosh(1+z/a)=100*ln(2+sqrt(3))",
    "fairlead_tension": "T_top=sqrt(H²+(w*s)²)=H+w*z",
    "touchdown_tension": "T_touchdown=H (vertical component zero)",
    "fairlead_angle": "theta=atan2(w*s,H)=60 degrees",
    "grounded_length": "L_ground=L-s=500-100*sqrt(3) (L exceeds s)",
}


def quantity(value, unit, equation="synthetic prescribed input"):
    """Bind a value to its unit and the carried-forward synthetic example."""
    return {"value": float(value), "unit": unit, "equation": equation, "source": "case-a"}


def calculate() -> dict:
    """Calculate Case A once and retain unrounded values for comparison."""
    # Basis: CASE, test_vertical_span_closed_form, Case A; issue 3319.
    # Unit-valued placeholders are arbitrary positive dataclass inputs, unused
    # by solve_catenary. They are not actual strength, stiffness or diameter data.
    inputs = {key: {**quantity(value, unit), "basis": "synthetic"} for key, value, unit in (
        ("mass_equivalent", 1000, "kg/m"), ("gravity", 9.81, "m/s²"),
        ("total_length", 500, "m"), ("vertical_height", 100, "m"),
        ("horizontal_tension", 981, "kN"), ("diameter", 1, "mm"),
        ("mbl", 1, "kN"), ("ea", 1, "kN"), ("drag_coeff", 1, "dimensionless"),
    )}
    def value(key):
        return inputs[key]["value"]
    line = MooringLineProperties(
        LineType.CHAIN, value("total_length"), value("diameter"), value("mbl"),
        value("mass_equivalent"), value("ea"), drag_coeff=value("drag_coeff"),
    )
    analyzer = CatenaryAnalyzer(water_depth=value("vertical_height"))
    if analyzer.g != value("gravity"):
        raise ValueError("Module gravity differs from the declared illustrative Case A basis")
    result = analyzer.solve_catenary(
        line, value("horizontal_tension"), vertical_height=value("vertical_height"),
    )
    values = {key: getattr(result, key) for key in UNITS if key != "weight_per_length"}
    values["weight_per_length"] = line.weight_water * analyzer.g / 1000  # Exact SI prefix.
    # Independent integrated identities at z/a=1, cosh(x/a)=2 (Case A).
    expected = dict(
        weight_per_length=9.81, horizontal_tension=981, vertical_height=100,
        catenary_parameter=100, arc_length=100 * math.sqrt(3),
        horizontal_distance=100 * math.log(2 + math.sqrt(3)),
        fairlead_tension=1962, touchdown_tension=981, fairlead_angle=60,
        grounded_length=500 - 100 * math.sqrt(3),
    )
    difference = {key: abs(values[key] - expected[key]) / abs(expected[key]) * 100 for key in UNITS}
    # Round the unrounded maximum UP to a tenth of a percent for a true bound.
    bound = math.ceil(max(difference.values()) * 10) / 10
    return {
        "inputs": inputs,
        "computed": {key: quantity(values[key], unit, EQUATIONS[key]) for key, unit in UNITS.items()},
        "independent": {key: quantity(expected[key], unit, EQUATIONS[key]) for key, unit in UNITS.items()},
        "difference": difference, "agreement_bound_percent": bound,
        "criterion_percent": 0.1,  # CASE: issue 2561 numerical agreement only.
    }


def source_record() -> dict:
    """Pin source bytes without embedding checkout locations or access times."""
    return {"sources": [
        {"id": key, "path": path, "locator": locator,
         "sha256": hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}
        for key, (path, locator) in SOURCE_PATHS.items()
    ]}


def json_text(value) -> str:
    """Serialize deterministically and prevent embedded script termination."""
    return (json.dumps(value, indent=2, ensure_ascii=False, allow_nan=False)
            .replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
            .replace("\u2028", "\\u2028").replace("\u2029", "\\u2029"))


def link(source, label=None) -> str:
    """Link a local source with escaped visible text and relative destination."""
    path = SOURCE_PATHS[source][0]
    return f'<a href="../../../../{html.escape(path, quote=True)}">{html.escape(label or path)}</a>'


def table(headers, rows, caption) -> str:
    """Render pre-escaped cells and place the caption below the table."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + f"</tbody><caption>{html.escape(caption)}</caption></table></div>")


def numeric(record, group, key) -> str:
    """Render visible units and source/equation bindings from the same record."""
    if group == "difference":
        item = quantity(record[group][key], "%", "abs(computed-independent)/abs(independent)*100")
    else:
        item = record[group][key]
    digits = 2 if key == "gravity" else 1  # Preserve declared input precision.
    attrs = {"data-value": f"{group}.{key}", "data-unit": item["unit"],
             "data-source": item["source"], "data-equation": item["equation"]}
    attributes = " ".join(f'{k}="{html.escape(v, quote=True)}"' for k, v in attrs.items())
    return f'<span {attributes}>{item["value"]:.{digits}f} {html.escape(item["unit"])}</span>'


def method_map() -> str:
    """Separate implemented interfaces from independently checked evidence."""
    rows = [
        [link("catenary", "solve_catenary"), "Implemented: uniform positive-weight, inextensible touchdown line.", "Independently checked: Case A only in this report."],
        [link("catenary", "solve_for_horizontal_tension"), "Implemented target-tension iteration.", "Existing Case A inverse regression; not calculated by this report. Broader qualification: NOT_EVALUATED."],
        [link("catenary", "calculate_stiffness / check_touchdown"), "Implemented geometric/elastic expressions and touchdown search.", "NOT_EVALUATED; the geometry example does not qualify these methods."],
        [link("models", "MooringLine.axial_stiffness"), "Implemented series axial compliance.", link("series-tests", "Independent elongation regressions") + "; distinct from geometric catenary stiffness. Full stiffness qualification: NOT_EVALUATED."],
        [link("designer", "MooringDesigner"), "Implemented environmental, offset, intact/damaged and condition-dispatch screens.", "Source inspection only; design-code compliance and equilibrium: NOT_EVALUATED."],
        [link("cli", "mooring-analysis CLI"), "Implemented catenary, design, material-list and model-export command routes.", "Source inspection only; no command or exporter executed."],
        [link("pinned-tests", "CatenarySolver pinned-end example"), "Separate general solver and independent pinned-end fixture.", "Distinct endpoint/elasticity contract; not calculated or qualified here."],
        ["DYNMOOR legacy dynamic port", "Excluded from report scope.", "NOT_EVALUATED; no legacy-port or dynamic mooring qualification."],
    ]
    return table(["Method and source", "Implemented", "Independent evidence and disposition"], rows,
                 "Table 1. Method evidence map; implementation is not qualification.")


def build(record: dict) -> str:
    """Render the ten-section report from computed quantities and fixed sources."""
    titles = ["Summary", "Introduction and method map", "Data basis", "Theory and equations",
              "Worked example", "Validation evidence", "Limits of use", "Way forward", "References", "Appendix and rebuild"]
    revisions = table(["Revision", "Date", "What changed and why"], [[REVISION, DATE,
        html.escape("; ".join(CHANGES["changes"]))]], "Revision history. First report issue.")
    inputs = table(["Input", "Value and unit", "Basis and use"], [
        [html.escape(key), numeric(record, "inputs", key),
         html.escape(item["basis"]) + "; " + ("unused positive placeholder" if key in {"diameter", "mbl", "ea", "drag_coeff"} else "Case A arithmetic input")]
        for key, item in record["inputs"].items()
    ], "Table 2. Synthetic inputs; the chain enumeration selects no material-library properties.")
    results = table(["Quantity", "Module result and unit", "Independent result and unit", "Absolute relative difference (%)", "Equation / source"], [
        [html.escape(key), numeric(record, "computed", key), numeric(record, "independent", key),
         numeric(record, "difference", key), html.escape(EQUATIONS[key]) + "; " + link("case-a", "Case A")]
        for key in UNITS
    ], "Table 3. Computed results compared with integrated identities. Comparisons are displayed to one decimal place; tests retain unrounded values.")
    max_difference = max(record["difference"].values())
    verdict = ("Numerical agreement criterion met" if max_difference <= record["criterion_percent"]
               else "Numerical agreement criterion exceeded")
    comparison = (f'{verdict}: differences are within {record["agreement_bound_percent"]:.1f}% '
                  f'against the carried-forward {record["criterion_percent"]:.1f}% numerical criterion. '
                  'The bound is rounded upward from unrounded relative differences; it is not design acceptance.')
    sources = source_record()
    references = table(["Source", "Repository evidence", "Use"], [
        [html.escape(source["id"]), link(source["id"]), html.escape(source["locator"])]
        for source in sources["sources"]
    ], "Table 4. Repository sources are pinned by SHA-256 in the embedded and adjacent source record.")
    sections = [
        f'<p>This mooring analysis documents one synthetic, uniform touchdown catenary. {html.escape(comparison)}</p>'
        '<p>No full-module, standards-compliance or solver-qualification claim is made. The result supplies an arithmetic control and a documented way forward.</p>'
        f'<div id="revision-history"><h3>Revision history</h3>{revisions}</div>',
        '<p>The public interfaces are inventoried below. A horizontal touchdown tangent governs this example; it is not a solution for two general suspended endpoints.</p>' + method_map(),
        '<p>The weight_water API field is a submerged mass-equivalent in kg/m, not a supplied force in kN/m. '
        'The declared illustrative g=9.81 m/s² is used: w=q*g/1000=9.81 kN/m. '
        'The inputs are synthetic arithmetic choices, not recommended line properties. No measured or manufacturer data enter the calculation.</p>'
        + inputs + '<p>Diameter, minimum breaking load, EA and drag coefficient are unused placeholders. '
        'Their positive values do not establish capacity or applicability. Grounded length exceeds zero for this case; no seabed friction is calculated.</p>',
        '<p>Integrating dz/dx=sinh(x/a) and ds/dx=cosh(x/a) from the horizontal tangent gives '
        'z=a*(cosh(x/a)−1), s=a*sinh(x/a), a=H/w. Eliminating x gives s²=z²+2az. '
        'For Case A, a=z=100 m, cosh(x/a)=2, so s=100*sqrt(3) m and '
        'x=100*ln(2+sqrt(3)) m. Force balance gives T_top=H+w*z=1962 kN, '
        'T_touchdown=H=981 kN and tan(theta)=sqrt(3), hence theta=60 degrees. '
        'The remaining grounded length is 500−100*sqrt(3) m. These identities are independently derived, '
        'rather than inferred from module outputs.</p>' + link("case-a", "Input and equation basis: existing synthetic Case A"),
        '<p>CatenaryAnalyzer.solve_catenary is called once with the declared height, length, mass-equivalent and horizontal tension. '
        'The following displayed quantities are bound to the calculation record, including unit and equation metadata.</p>' + results,
        '<p>' + html.escape(comparison) + '</p><p>The existing catenary geometry suite covers both closed-form cases, '
        'an explicit-height override, an inverse-tension check and buoyancy rejection. The series stiffness suite checks '
        'common-force elongation, splitting invariance and invalid segment guards. Those regressions do not establish geometric stiffness accuracy.</p>'
        '<p>The report suite checks all unrounded quantities against hand arithmetic and a fresh module call; parsed HTML checks every visible value and unit. '
        'Controlled mutations of every solver result field must change visible results and the numerical disposition. '
        'Two isolated rebuilds compare every delivered output byte. Reproducibility does not establish physical validation.</p>'
        '<p>The roundoff budget is 256 binary64 spacings at unity: fewer than 32 elementary operations, '
        'a factor-two library-rounding margin and a factor-four conditioning allowance. '
        'The largest subtraction condition is (500+100*sqrt(3))/(500−100*sqrt(3)), below four for this Case A. '
        'It is a test precision allowance, independent of the carried-forward 0.1% numerical criterion.</p>' + link("report-tests", "Executable evidence"),
        '<ul>' + ''.join(f'<li>{limit}: NOT_EVALUATED. {reason}</li>' for limit, reason in (
            ("Design-code compliance", "DNV-OS-E301 is named by the package; edition, governing clauses, load cases and acceptance mapping are not established here."),
            ("Actual line strength", "No qualified material or breaking-load data are supplied."),
            ("Stiffness qualification", "Neither tangent geometric stiffness nor physical elastic response is qualified by this example."),
            ("Environmental loading", "No environmental input, heading combination or system equilibrium is evaluated."),
            ("Dynamic mooring behavior", "No dynamic simulation or licensed solver is invoked; the DYNMOOR port is excluded."),
        )) + '</ul><p>Source-inspection findings: solve_catenary clips negative grounded length and does not reject a suspended length exceeding total length; '
        'finite-input and vertical-span validation remain incomplete. check_touchdown retains height/coordinate expressions inconsistent with the corrected geometry route; '
        'calculate_stiffness uses a separate geometric expression without independent qualification here. '
        'The designer uses the first segment, applies a distance-ratio correction without an equilibrium residual, ignores yaw in that correction and has a broad fallback. '
        'Environmental forces are combined in one heading. These concerns are findings for further work, not fixes or qualification claims.</p>'
        + link("catenary", "Analyzer evidence") + '; ' + link("designer", "Designer evidence"),
        '<ol><li>Retain Case A as an arithmetic regression. Add independent boundary, finite-input, short-line and general-endpoint checks before extending applicability.</li>'
        '<li>Reconcile touchdown-search geometry and derive tangent stiffness for the intended boundary conditions; compare with independent derivatives.</li>'
        '<li>Establish qualified material properties, environmental cases, equilibrium criteria and the governing standard edition before assigning design acceptance.</li>'
        '<li>Qualify the pinned-end route independently and obtain reviewed dynamic evidence separately. Keep the broader module-report issue open for numerical qualification.</li></ol>',
        references + '<p>The module pack is historical discovery evidence: its catenary equation descriptions and issue-pending entries are not current qualification. '
        'Current source and the closed-form regression govern this report. No licensed standard tables or numerical design factors are reproduced.</p>',
        '<p>Source-inspected CLI routes: mooring-analysis catenary accepts mass per length and a horizontal or fairlead tension, '
        'then also calculates stiffness. It is therefore not executed by this report. mooring-analysis design-verification '
        'builds a screening system, calculates environmental loads and analyzes conditions; the shared material object is mutated by its length assignment. '
        'mooring-analysis generate-model is a placeholder; list-materials lists the library. '
        'The Python designer routes through analyze_line_tension and intact/damaged condition checks. No exporter or commercial tool is executed.</p>'
        + link("cli", "CLI source") + '; ' + link("entry-point", "Installed command declaration")
        + '<p>From the repository root, use the prepared scratch interpreter with the module dependencies. '
        'The explicit destination receives only report.html, changes.json and sources.json; no intermediate files are written by the builder.</p>'
        f'<pre><code>{html.escape(REBUILD)}</code></pre>'
        '<p>JSON calculation, source and change records below are embedded for offline inspection. '
        'The page uses no external resources. Moving the page does not change its displayed analysis; repository links require the repository tree.</p>',
    ]
    nav = ''.join(f'<li><a href="#section-{i}">{html.escape(title)}</a></li>' for i, title in enumerate(titles, 1))
    body = ''.join(f'<section id="section-{i}"><h2>{i}. {html.escape(title)}</h2>{content}</section>'
                   for i, (title, content) in enumerate(zip(titles, sections), 1))
    changes = ''.join(f'<li>{html.escape(change)}</li>' for change in CHANGES["changes"])
    embedded = ''.join(f'<script type="application/json" id="{key}">{json_text(value)}</script>'
                       for key, value in (("calculation-record", record), ("change-record", CHANGES), ("source-record", sources)))
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Mooring analysis: synthetic touchdown catenary</title>
<style>
:root{{color-scheme:light;--ink:#182b36;--accent:#086d76}}*{{box-sizing:border-box}}
body{{margin:0;color:var(--ink);background:#f5f4ee;font:16px/1.6 system-ui,sans-serif}}
header,main,nav,aside{{max-width:1200px;margin:auto;padding:1.5rem 2rem}}
header{{border-bottom:4px solid var(--accent)}}h1{{font-size:2.2rem;line-height:1.2}}
h2{{color:var(--accent)}}section{{margin:2rem 0;padding:1rem;background:white;border-top:1px solid #bdcccd}}
a{{color:#065d76;overflow-wrap:anywhere}}nav ol{{columns:2}}td,th{{padding:.65rem;text-align:left;vertical-align:top;border-bottom:1px solid #d4dddd}}
table{{border-collapse:collapse;width:100%}}caption{{caption-side:bottom;text-align:left;font-size:.9rem;padding:.7rem}}
.table-wrap{{overflow:auto}}pre{{white-space:pre-wrap;overflow-wrap:anywhere;background:#edf3f3;padding:1rem}}
aside{{background:#e6efed}}:focus-visible{{outline:3px solid var(--accent)}}
@media(max-width:650px){{header,main,nav,aside{{padding:1rem}}nav ol{{columns:1}}}}
@media print{{body{{background:white}}nav{{display:none}}section{{break-inside:auto}}}}
</style></head><body>
<header><h1>Mooring analysis: synthetic touchdown catenary</h1><p>Revision {REVISION} · {DATE} · Draft for maintainer review</p></header>
<nav aria-label="Contents"><ol>{nav}</ol></nav><main>{body}</main>
<aside id="change-panel"><h2>Change panel</h2><ul>{changes}</ul></aside>
{embedded}
</body></html>
'''


def main():
    """Write the complete deterministic output set to a supplied destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    destination = parser.parse_args().output_dir
    record = calculate()
    outputs = {"report.html": build(record), "changes.json": json_text(CHANGES) + "\n",
               "sources.json": json_text(source_record()) + "\n"}
    destination.mkdir(parents=True, exist_ok=True)
    for name, content in outputs.items():
        (destination / name).write_bytes(content.encode("utf-8"))


if __name__ == "__main__":
    main()
