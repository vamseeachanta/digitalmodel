# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build a bounded pipe capacity module report into an explicit directory."""

import argparse
import ast
import hashlib
import html
import json
import math
from pathlib import Path

from digitalmodel.structural.pipe_capacity.common.PipeSizing import PipeSizing


ROOT = Path(__file__).resolve().parents[4]
PACKAGE = "src/digitalmodel/structural/pipe_capacity/"
REVISION = "0"
# Fixed publication date; no runtime clock or output-directory metadata is used.
DATE = "2026-10-28"
REBUILD = (
    'PYTHONPATH=src uv run --no-project --with pytest python '
    'docs/reports/modules/pipe_capacity/build_report.py '
    '--output-dir "$TMPDIR/pipe-capacity-report"'
)
CHANGES = {
    "report": "pipe-capacity-module-analysis",
    "revisions": [{
        "revision": REVISION,
        "date": DATE,
        "changes": [
            "Added a source-linked method map to distinguish implementation from numerical qualification.",
            "Added synthetic annulus integrals, computed results and parsed-output checks with reproducible records.",
            "Retained explicit capacity exclusions and the existing standalone example without alteration.",
        ],
    }],
}

# Repository-source citations only. Standards names describe code attribution;
# no licensed publication, table or design factor is reproduced or qualified.
# Entries are (stable locator, file within package, exact symbol, evidence).
METHODS = (
    (
        'router',
        'pipe_capacity.py',
        'PipeCapacity.router',
        'Public dispatch mutates and returns cfg through PipeComponents.evaluate_pipe_capacity.',
    ),
    (
        'components',
        'common/pipe_components.py',
        'PipeComponents.evaluate_pipe_capacity',
        'Constructs custom sizing, material and capacity objects; inserts defaults when mappings are missing.',
    ),
    (
        'load-dispatch',
        'custom/PipeCapacity.py',
        'PipeCapacity.evaluate_load_conditions',
        'Iterates design rows and code selectors for internal pressure, external pressure and propagation.',
    ),
    (
        'common-section',
        'common/PipeSizing.py',
        'PipeSizing.pipe_section_properties',
        'Common annulus geometry only: disk contributions, area and second moments independently checked for this probe.',
    ),
    (
        'custom-section',
        'custom/PipeSizing.py',
        'PipeSizing.pipe_section_properties',
        'Router-selected custom annulus implementation; separate from the common method exercised here.',
    ),
    (
        'other-sizing',
        'PipeSizing.py',
        'PipeSizing.pipe_section_properties',
        'Additional top-level sizing implementation; no equivalence claim across copies.',
    ),
    (
        'section-stiffness',
        'common/PipeSizing.py',
        'PipeSizing.get_fea_properties',
        'Adds stiffness and mass using material mappings; no material properties are supplied in this report.',
    ),
    (
        'system-sizing',
        'custom/PipeSizing.py',
        'PipeSizing.system_properties',
        'Equivalent pipe and submerged-weight assembly; outside the single-annulus probe.',
    ),
    (
        'material',
        'custom/MaterialProperties.py',
        'MaterialProperties.evaluate_material_properties',
        'Material-selection and derating route; material strength remains unevaluated.',
    ),
    (
        'internal-selectors',
        'custom/PipeCapacity.py',
        'PipeCapacity.internal_pressure_specification_code_based_evaluation',
        'ASME B31, API STD 2RD, API RP 1111, API RP 16Q, CFR and DNV selector branches; attribution is not compliance evidence.',
    ),
    (
        'external-selectors',
        'custom/PipeCapacity.py',
        'PipeCapacity.external_pressure_specification_code_based_evaluation',
        'External-pressure dispatch includes API STD 2RD, API TR 5C3 and DNV routes.',
    ),
    (
        'propagation-selectors',
        'custom/PipeCapacity.py',
        'PipeCapacity.collapse_propagation_specification_code_based_evaluation',
        'Propagation dispatch includes API RP 1111, API STD 2RD and DNV routes.',
    ),
    (
        'api-burst',
        'custom/PipeCapacity.py',
        'PipeCapacity.evaluate_burst_pressure_API_STD_2RD',
        'API STD 2RD-2013 pressure method; numerical and edition qualification not attempted.',
    ),
    (
        'api-collapse-inverse',
        'custom/PipeCapacity.py',
        'PipeCapacity.evaluate_collapse_minimum_thickness_API_STD_2RD',
        'Finding: marked incomplete and returns a placeholder; must not be used as a qualified thickness inverse.',
    ),
    (
        'api-rp-burst',
        'custom/PipeCapacity.py',
        'PipeCapacity.evaluate_burst_pressure_API_RP_1111',
        'API RP 1111-2009 pressure method and a separate inverse; no pressure formula qualification here.',
    ),
    (
        'asme-burst',
        'custom/PipeCapacity.py',
        'PipeCapacity.evaluate_burst_pressure_modified_burlow_equation',
        'Modified Barlow branch selected by ASME B31; edition and design factors need independent acceptance.',
    ),
    (
        'dnv-wall',
        'custom/PipeCapacity.py',
        'DNVWallThickness',
        'Shared DNV wall-thickness object supplies containment, collapse and propagation; edition-specific factors remain unverified.',
    ),
    (
        'f201-combined',
        'custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py',
        'combined_loading_internal_overpressure',
        'Standalone DNV-OS-F201 internal-overpressure interaction; not the pressure-selector route and not exercised here.',
    ),
    (
        'f201-external',
        'custom/DNV_OS_F201/dnv_os_f201_lrfd_wsd.py',
        'combined_loading_external_overpressure',
        'Standalone DNV-OS-F201 external-overpressure interaction; no combined-load qualification.',
    ),
    (
        'f101-legacy',
        'custom/DNV_OS_F101/RigidPipe.py',
        'DNVOSF101Pipe.pressureContainmentResistance',
        'Finding: legacy containment overwrites its method name with its scalar output; exclude from numerical references.',
    ),
    (
        'f101-collapse',
        'custom/DNV_OS_F101/RigidPipe.py',
        'DNVOSF101Pipe.collapse',
        'Separate legacy F101 collapse route; its source comment names Local Buckling - Collapse Check.',
    ),
    (
        'api-legacy-burst',
        'custom/API_STD_2RD/APISTD2RDMethods.py',
        'APISTD2RDBurst',
        'Separate API burst calculation; not established as equivalent to the routed method.',
    ),
    (
        'api-legacy-collapse',
        'custom/API_STD_2RD/APISTD2RDMethods.py',
        'APISTD2RDCollapse',
        'Separate API collapse calculation; not independently qualified here.',
    ),
    (
        'api-legacy-interaction',
        'custom/API_STD_2RD/APISTD2RDMethods.py',
        'APISTD2RDMethod1Utilization',
        'Separate API interaction calculation; not independently qualified here.',
    ),
    (
        'asme-legacy',
        'custom/ASMEMethods.py',
        'ASMEB318LogitudinalStress',
        'Finding: standalone ASME longitudinal calculation fixes load terms internally; supplied-load qualification is needed.',
    ),
    (
        'common-capacity',
        'common/PipeCapacity.py',
        'PipeCapacity',
        'Parallel common capacity implementation tested by the existing common suite; regression is not independent physical evidence.',
    ),
    (
        'api-tr',
        'custom/PipeCapacity.py',
        'API_TR_5C3',
        'API TR 5C3 external-pressure and interpolation route; bounds, units and overrides need qualification.',
    ),
    (
        'cfr',
        'custom/PipeCapacity.py',
        'CFR_30_Part_250',
        'CFR pressure/thickness calculations; regulatory currency and application have not been evaluated.',
    ),
    (
        'von-mises',
        'custom/PipeCapacity.py',
        'VonMises_Pipe',
        'Finding: stress and interpolation route has mixed force/pressure treatment requiring a separate units review.',
    ),
    (
        'api-16q',
        'custom/PipeCapacity.py',
        'API_RP_16Q',
        'API RP 16Q-2017 pressure interface delegates to VonMises_Pipe; not a qualified combined-loading result.',
    ),
)


def source_record():
    """Locate definitions with AST parsing without importing engineering routes."""
    methods = []
    for identifier, filename, symbol, evidence in METHODS:
        path = PACKAGE + filename
        text = (ROOT / path).read_text(encoding="utf-8")
        node = ast.parse(text)
        for part in symbol.split("."):
            node = next(child for child in node.body if getattr(child, "name", None) == part)
        snippet = "".join(text.splitlines(keepends=True)[node.lineno - 1:node.end_lineno])
        methods.append({
            "id": identifier, "path": path, "symbol": symbol,
            "start_line": node.lineno, "end_line": node.end_lineno,
            "sha256": hashlib.sha256(snippet.encode("utf-8")).hexdigest(),
            "implementation": "IMPLEMENTED",
            "qualification": "INDEPENDENTLY_CHECKED" if identifier == "common-section" else "NOT_EVALUATED",
            "evidence": evidence,
        })
    return {
        "basis": "Repository-source review; no external standard was consulted. Implementation means a definition exists, not that it is complete or correct.",
        "independent_basis": "Synthetic concentric annulus; polar integrals in report section polar-integrals and the independent test oracle.",
        "qualification_scope": "Common annulus geometry at the stated synthetic inputs only.",
        "methods": methods,
    }


def calculate():
    """Return unrounded module results and separately integrated geometric controls."""
    # Synthetic input definition: Do=4 m, Di=2 m, hence Ro=2 m and Ri=1 m.
    # No unit conversion is performed by PipeSizing.
    cfg = {"Outer_Pipe": {"Geometry": {"Nominal_OD": 4.0, "Nominal_ID": 2.0}}}
    PipeSizing(cfg).pipe_section_properties("Outer_Pipe")
    props = cfg["Outer_Pipe"]["section_properties"]["pipe"]
    geometry = cfg["Outer_Pipe"]["Geometry"]
    ro, ri = geometry["Nominal_OD"] / 2, geometry["Nominal_ID"] / 2
    # Independent radial antiderivatives: integral r dr = r^2/2;
    # integral r^3 dr = r^4/4. Angular integrals are 2*pi and pi.
    radial_area = (ro**2 - ri**2) / 2
    radial_moment = (ro**4 - ri**4) / 4
    values = {
        **geometry, "Ro": ro, "Ri": ri,
        **{key: value for key, value in props.items() if key != "geometry"},
        "Ix": props["I"], "Iy": props["I"],
    }
    controls = {
        "Nominal_OD": (4.0, "m", "Do: defined synthetic outside diameter"),
        "Nominal_ID": (2.0, "m", "Di: defined synthetic inside diameter"),
        "Ro": (ro, "m", "Ro = Do/2"), "Ri": (ri, "m", "Ri = Di/2"),
        "Ao": (2 * math.pi * ro**2 / 2, "m^2", "Ao = 2*pi*[r^2/2]_0^Ro"),
        "Ai": (2 * math.pi * ri**2 / 2, "m^2", "Ai = 2*pi*[r^2/2]_0^Ri"),
        "Io": (math.pi * ro**4 / 4, "m^4", "Io = pi*[r^4/4]_0^Ro"),
        "Ii": (math.pi * ri**4 / 4, "m^4", "Ii = pi*[r^4/4]_0^Ri"),
        "Jo": (2 * math.pi * ro**4 / 4, "m^4", "Jo = 2*pi*[r^4/4]_0^Ro"),
        "Ji": (2 * math.pi * ri**4 / 4, "m^4", "Ji = 2*pi*[r^4/4]_0^Ri"),
        "A": (2 * math.pi * radial_area, "m^2", "A = 2*pi*[r^2/2]_Ri^Ro"),
        "I": (math.pi * radial_moment, "m^4", "I = Ix = Iy = pi*[r^4/4]_Ri^Ro"),
        "Ix": (math.pi * radial_moment, "m^4", "Ix = pi*[r^4/4]_Ri^Ro"),
        "Iy": (math.pi * radial_moment, "m^4", "Iy = Ix by circular symmetry"),
        "J": (2 * math.pi * radial_moment, "m^4", "J = 2*pi*[r^4/4]_Ri^Ro = Ix + Iy"),
    }
    return {key: {"value": values[key], "reference": reference, "unit": unit,
                  "equation": equation, "basis": "synthetic" if unit == "m" else "polar-integrals"}
            for key, (reference, unit, equation) in controls.items()}


def json_text(record):
    """Serialize deterministic JSON, rejecting nonfinite engineering values."""
    return json.dumps(record, indent=2, ensure_ascii=True, allow_nan=False) + "\n"


def embedded_json(record):
    """Escape raw-text script delimiters and XML/HTML special characters."""
    return json_text(record).replace("&", "\\u0026").replace("<", "\\u003c").replace(
        ">", "\\u003e"
    )


def display_record(record):
    """Keep embedded numbers at display precision; calculate() retains full precision."""
    return {key: {name: round(value, 1) if isinstance(value, float) else value
                  for name, value in item.items()} for key, item in record.items()}


def source_link(source):
    """Render a relative repository link with exact source-line and symbol evidence."""
    target = f'../../../../{source["path"]}#L{source["start_line"]}-L{source["end_line"]}'
    label = f'{source["path"]}:{source["start_line"]}-{source["end_line"]} ({source["symbol"]})'
    return f'<a href="{html.escape(target, quote=True)}">{html.escape(label)}</a>'


def build(record):
    """Render the ten-section report from computed data with escaped prose."""
    sources = source_record()
    rows = []
    for key, item in record.items():
        difference = abs(item["value"] - item["reference"])
        relative = difference / abs(item["reference"]) * 100
        basis = "input-basis" if item["basis"] == "synthetic" else "polar-integrals"
        rows.append(
            f'<tr data-quantity="{html.escape(key, quote=True)}"><th scope="row">{html.escape(key)}</th>'
            f'<td class="value">{item["value"]:.1f}</td>'
            f'<td class="reference">{item["reference"]:.1f}</td>'
            f'<td class="unit">{html.escape(item["unit"])}</td>'
            f'<td class="difference">{difference:.1f}</td><td class="relative">{relative:.1f}%</td>'
            f'<td class="binding"><a href="#{basis}">{html.escape(item["equation"])}</a></td></tr>'
        )
    method_rows = "\n".join(
        f'<tr id="method-{source["id"]}"><td>{source_link(source)}</td>'
        f'<td>{html.escape(source["evidence"])}</td><td>{source["implementation"]}</td>'
        f'<td>{source["qualification"]}</td></tr>' for source in sources["methods"]
    )
    limits = "\n".join(
        f'<tr><th scope="row">{item}</th><td>NOT_EVALUATED</td></tr>'
        for item in ("pressure containment", "collapse", "combined loading", "material strength", "code compliance")
    )
    changes = "".join(f"<li>{html.escape(item)}</li>" for item in CHANGES["revisions"][0]["changes"])
    titles = ("Summary", "Introduction and Method Map", "Data Basis", "Theory and Equations",
              "Worked Example", "Validation Evidence", "Limits of Use", "Recommendations",
              "References", "Appendix")
    nav = "".join(f'<li><a href="#section-{n}">{html.escape(title)}</a></li>'
                  for n, title in enumerate(titles, 1))
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Pipe capacity module analysis: bounded section geometry</title>
<style>
:root {{ color-scheme: light; font-family: system-ui, sans-serif; color: #172c38; background: #f4f6f7; }}
body {{ max-width: 100rem; margin: auto; padding: 1.5rem; line-height: 1.6; }}
header, main, nav {{ background: white; padding: 1.5rem; border: 1px solid #cbd5dc; }}
.layout {{ display: grid; grid-template-columns: 15rem minmax(0, 1fr); gap: 1rem; margin-top: 1rem; }}
nav {{ align-self: start; }} section {{ margin-bottom: 2rem; }} h1,h2,h3 {{ line-height: 1.3; }}
a {{ color: #005a76; overflow-wrap: anywhere; }} table {{ border-collapse: collapse; width: 100%; }}
th,td {{ border: 1px solid #cbd5dc; padding: .5rem; text-align: left; vertical-align: top; overflow-wrap: anywhere; }}
th {{ background: #edf3f5; }} caption {{ caption-side: bottom; text-align: left; padding: .5rem; }}
.table-wrap {{ overflow-x: auto; }} pre {{ white-space: pre-wrap; overflow-wrap: anywhere; }}
details {{ padding: 1rem; background: #edf3f5; }} svg {{ max-width: 24rem; }}
@media (max-width: 65rem) {{ .layout {{ grid-template-columns: minmax(0, 1fr); }} }}
@media print {{ nav {{ display: none; }} .layout {{ display: block; }} body {{ padding: 0; }} }}
</style></head><body class="hs-doc">
<header><h1>Pipe capacity module analysis: bounded section geometry</h1>
<p>Draft for maintainer review. Revision {REVISION} · {DATE}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>
<section id="section-1"><h2>1. Summary</h2>
<p>The module provides sizing and code-selected capacity routes. This report maps their implementation evidence
and checks only the common annular section geometry against independent polar integrals.
The chosen dimensions are geometric probes, not realistic pipe dimensions or code allowables.</p>
<p>Pressure containment, collapse, combined loading, material strength and code compliance remain
NOT_EVALUATED. The parent issue remains open for broader numerical qualification.</p></section>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>{REVISION}</td><td>{DATE}</td><td>Initial bounded module report to separate source evidence,
independent geometry checks and unevaluated capacity methods.</td></tr></tbody></table></div>
<section id="section-2"><h2>2. Introduction and Method Map</h2>
<p>The public router calls PipeComponents, which selects custom implementations. The report calls only
common.PipeSizing.pipe_section_properties directly. A check of that method does not qualify the custom
copy, the router, other sizing paths or standards-specific calculations.</p>
<p>IMPLEMENTED means a source definition exists, including incomplete methods. INDEPENDENTLY_CHECKED
is limited to the stated synthetic geometry. NOT_EVALUATED means no numerical qualification is supplied
by this report. Code labels for standards identify intended attribution; authoritative editions,
clause text and suitability for a design have not been checked.</p>
<div class="table-wrap"><table><thead><tr><th>Exact source and entry point</th><th>Method and evidence</th>
<th>Implementation</th><th>Qualification in this report</th></tr></thead><tbody>{method_rows}</tbody>
<caption>Table 1. Method evidence, including correctness findings requiring separate assessment.</caption></table></div></section>
<section id="section-3"><h2>3. Data Basis</h2>
<div id="input-basis"><p>All numerical geometry inputs are defined synthetic values: outside diameter Do = 4.0 m,
inside diameter Di = 2.0 m, hence outer radius Ro = 2.0 m and inner radius Ri = 1.0 m.
These are arithmetic probes of a concentric annulus about its centroid. No service input is assumed.</p></div>
<p>The method is unit agnostic: consistent input lengths are interpreted as metres here, with no conversion.
Area units are m^2 and all second-moment units are m^4. No material, pressure, corrosion, manufacturing,
temperature, density, design factor or loading mapping is supplied.</p>
<pre>{{"Outer_Pipe": {{"Geometry": {{"Nominal_OD": 4.0, "Nominal_ID": 2.0}}}}}}</pre>
<p>The dimensional input definition is the source for these constants; the independent integrals supply
all equation coefficients. Dates and revision identifiers are document metadata, not engineering inputs.</p></section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<div id="polar-integrals"><p>Let x = r cos(theta), y = r sin(theta), and dA = r dr dtheta,
with Ri &lt;= r &lt;= Ro and angle from zero to 2*pi. The angular integral of unity is 2*pi;
the angular integral of sin(theta)^2 or cos(theta)^2 is pi.</p>
<pre>A = 2*pi * [r^2/2]_1^2 = 3*pi m^2
Ix = Iy = pi * [r^4/4]_1^2 = 15*pi/4 m^4
J = 2*pi * [r^4/4]_1^2 = 15*pi/2 m^4 = Ix + Iy
I denotes either centroidal diametral moment, Ix or Iy.
Disk controls: Ao,Ai = 2*pi*[r^2/2]_0^R;
Io,Ii = pi*[r^4/4]_0^R; Jo,Ji = 2*pi*[r^4/4]_0^R.
R is Ro for the outer disk and Ri for the inner disk.</pre>
<p>The radial antiderivatives follow integration of r and r^3. Substituting R = D/2 yields the module's
diameter expressions: A = pi*(Do^2 - Di^2)/4, I = pi*(Do^4 - Di^4)/64,
J = pi*(Do^4 - Di^4)/32. The test oracle uses the evaluated polar integrals directly.</p></div>
<figure><svg viewBox="0 0 300 250" role="img" aria-labelledby="annulus-title">
<title id="annulus-title">Concentric annulus and centroidal axes; schematic</title>
<circle cx="130" cy="125" r="90" fill="#d6e6ec" stroke="#172c38" />
<circle cx="130" cy="125" r="45" fill="white" stroke="#172c38" />
<path d="M20 125 H245 M130 20 V230" fill="none" stroke="#172c38" stroke-dasharray="4 4" />
<path d="M130 125 L194 61 M130 125 H175" fill="none" stroke="#005a76" />
<text x="184" y="74">Ro</text><text x="148" y="118">Ri</text>
<text x="248" y="130">x</text><text x="137" y="24">y</text></svg>
<figcaption>Figure 1. Synthetic cross-section with radii and centroidal axes; no physical validation is implied.</figcaption></figure></section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>The builder calls the existing common sizing method. Every value in the calculation column comes from
that call or its geometry inputs; Ix and Iy alias I by symmetry. The independent column uses the polar integrals.
All quantities, including disk contributions, are compared unrounded in the tests.</p>
<div class="table-wrap"><table><thead><tr><th>Quantity</th><th>Calculation</th><th>Independent</th>
<th>Unit</th><th>Absolute difference (same unit)</th><th>Relative difference</th><th>Equation / input source</th></tr></thead>
<tbody id="example-values">{''.join(rows)}</tbody>
<caption>Table 2. Synthetic inputs, derived radii and calculated section properties; one decimal place.</caption></table></div>
<p>Differences are computed before rounding. A displayed zero is a rounding result, not proof of exact equality.
All displayed comparisons use one decimal place or 0.1% increments. The embedded calculation record uses the
same display precision; the builder's calculate function returns unrounded values.</p></section>
<section id="section-6"><h2>6. Validation Evidence</h2>
<p>The new report suite checks independent unrounded geometry and units, J = Ix + Iy, each visible result
against both the oracle and a fresh module call, and a controlled area mutation that must change visible output.
It also checks isolated deterministic rebuilds, source resolution, escaped HTML/JSON, import safety and output confinement.
Reproducibility is not physical validation.</p>
<p>The numerical tolerance is derived specifically for this probe: at most eight rounded steps per expression
path, sixteen across the module and reference paths, including representation of pi. Integer powers and binary
scale factors are exact for these inputs. With binary64 epsilon = 2^-52, use gamma(16) =
16*epsilon/(1 - 16*epsilon); the relative budget is gamma(16)*5/3. The subtraction amplification
for area is (Ro^2 + Ri^2)/(Ro^2 - Ri^2) = 5/3; for fourth powers it is 17/15, which is smaller.
Using epsilon rather than half epsilon overbounds individual rounding steps. No engineering tolerance is inferred.</p>
<p>The existing standalone example suite preserves its separate command and output contract. The common
capacity suite covers regression behaviour of a parallel implementation; passing those assertions does not
provide independent pressure, collapse, material or standards qualification.</p></section>
<section id="section-7"><h2>7. Limits of Use</h2>
<table><thead><tr><th>Assessment</th><th>Status</th></tr></thead><tbody id="scope-limits">{limits}</tbody>
<caption>Table 3. Explicit exclusions from this report.</caption></table>
<p>Geometry alone establishes no allowable pressure, utilisation, code compliance or fitness for service.
No licensed solver is imported or queried. No standards table, factor or production engineering behaviour is changed.</p>
<p>The method map records incomplete inverses, implicit defaults, parallel implementations, fixed load terms
and legacy units concerns as source findings. Their presence is not a numerical evaluation or an accepted design basis.</p></section>
<section id="section-8"><h2>8. Recommendations</h2>
<p>Before service use, select a single dispatch path and intended standard edition; establish geometry units,
material certification, dimensional allowances, load envelopes, temperature and explicit design factors.
Replace or exclude incomplete inverses before using thickness recommendations. Review fixed loads, pressure/force
conversions, interpolation bounds and material defaults against documented input contracts.</p>
<p>Then obtain independent references for pressure, collapse and interaction, derive case-specific tolerances,
test boundaries and failed cases, and review edition-specific clause applicability. Qualify each sizing copy
and the complete router separately. Keep the parent issue open until that broader evidence is reviewed.</p></section>
<section id="section-9"><h2>9. References</h2>
<p><a href="#polar-integrals">Independent polar-coordinate derivation</a> supplies the geometric oracle.
<a href="sources.json">Source record</a> contains relative paths, exact symbols, line ranges and SHA-256 digests
of the reviewed definitions. No external source or licensed standard was consulted for this report.</p>
<ul><li><a href="../../../../{PACKAGE}docs/agent-pack.md">Module knowledge pack</a>: broader inventory and existing qualification gaps.</li>
<li><a href="../../../../examples/workflows/pipe-capacity/input.yml">Existing routed workflow input</a>: separate engineering example, not evaluated here.</li>
<li><a href="../../../../tests/structural/pipe_capacity/test_module_report.py">Report tests and independent oracle</a>.</li>
<li><a href="../../../../tests/structural/pipe_capacity/test_worked_example.py">Standalone example checks</a>.</li>
<li><a href="../../../../tests/structural/pipe_capacity/test_pipe_capacity_common.py">Common capacity regression checks</a>.</li>
<li><a href="build_worked_example.py">Unchanged standalone example builder</a>.</li></ul></section>
<section id="section-10"><h2>10. Appendix</h2>
<h3>Rebuild and output contract</h3><pre>{html.escape(REBUILD)}</pre>
<p>Supply writable scratch storage through TMPDIR. The builder writes only report.html, sources.json and
changes.json in the explicit output directory. All intermediate data stays in memory. The generated page has
no external resources, absolute paths or runtime timestamps. Relative repository links resolve beside this report;
an isolated exported page retains all narrative, equations, results and styling without fetching linked files.</p>
<p>The builder requires the standard library and the existing common sizing class. An already prepared
interpreter can run the command without uv; preparing pytest through uv may require network access.</p>
<details id="engineering-changes" open="open"><summary>Engineering changes</summary>
<ul>{changes}</ul><p><a href="changes.json">Change record</a> · <a href="build_report.py">Report builder</a>
· <a href="README.md">Report index and reproduction instructions</a></p></details>
<h3>Calculation record</h3><p>The embedded JSON records display-precision quantities, units and equation bindings.
Full-precision numerical evidence is checked through calculate(), not reconstructed from rounded text.</p>
<script type="application/json" id="calculation-record">{embedded_json(display_record(record))}</script>
</section></main></div></body></html>
'''


def write_report(output_dir):
    """Write only the three deliverables under the supplied directory."""
    record = calculate()
    page = build(record)
    sources = json_text(source_record())
    changes = json_text(CHANGES)
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, text in (("report.html", page), ("sources.json", sources), ("changes.json", changes)):
        (output_dir / name).write_text(text, encoding="utf-8")


def main():
    """Require an explicit output directory for command-line rebuilds."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    write_report(args.output_dir)


if __name__ == "__main__":
    main()
