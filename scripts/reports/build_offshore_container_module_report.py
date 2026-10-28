# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build a bounded offshore-container analysis using normal public API imports.

Both --scratch and --output-dir are required. Scratch must be outside the
checkout. The output directory receives only the small deterministic delivery.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import re
from dataclasses import asdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
PACKAGE = "src/digitalmodel/structural/offshore_container"
CHANGES = [{
    "revision": "A",
    "date": "2026-10-28",
    "change": "Added a reproducible module analysis with independent geometry and caller-factor arithmetic to expose the evidence and unresolved method limits.",
}]
SOURCES = (
    f"{PACKAGE}/__init__.py",
    f"{PACKAGE}/dnv_2_7_1.py",
    f"{PACKAGE}/docs/worked-shs-sling-geometry.md",
    "src/digitalmodel/citations/registry.py",
    "src/digitalmodel/citations/schema.py",
    "scripts/reports/build_offshore_container_module_report.py",
    "tests/reporting/test_offshore_container_module_report.py",
    "docs/api/_assets/brand.css",
)
REFERENCES = (
    ("D1/D2", f"../../../../{PACKAGE}/docs/worked-shs-sling-geometry.md",
     "Committed hand derivation: concentric square integrals and centred sling triangle; synthetic geometry only."),
    ("API", f"../../../../{PACKAGE}/dnv_2_7_1.py",
     "Reviewed calculation source; equations are described as implemented, without adopting its standard-derived defaults."),
    ("EXPORTS", f"../../../../{PACKAGE}/__init__.py",
     "Public package exports; inventory below includes data classes and the section catalogue."),
    ("CITATIONS", "../../../../src/digitalmodel/citations/registry.py",
     "Registry plumbing reviewed as code, without publishing its numeric standard factors."),
    ("VALIDATOR", "../../../../src/digitalmodel/citations/schema.py",
     "Citation validation checks metadata, not clause content or the selected design basis."),
    ("SI", "https://www.bipm.org/en/measurement-units/si-prefixes",
     "BIPM SI prefixes: milli, centi, kilo. Public page checked on the revision date."),
    ("GRAVITY", "https://www.bipm.org/en/committees/cg/cgpm/3-1901/resolution-2",
     "BIPM standard-gravity definition, third CGPM resolution 2. Public page checked on the revision date."),
    ("TEST", "../../../../tests/reporting/test_offshore_container_module_report.py",
     "Analytical assertions, corrupted-result controls, byte rebuilds, manifest readback and local-link checks."),
    ("BUILDER", "../../../../scripts/reports/build_offshore_container_module_report.py",
     "Explicit public API calls and deterministic presentation generation."),
)


def json_text(value):
    """Serialize evidence without timestamps, paths or nonfinite numbers."""
    return json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n"


def embedded_json(value):
    """Prevent data from closing its application/json script element."""
    return (json_text(value).replace("&", "\\u0026").replace("<", "\\u003c")
            .replace(">", "\\u003e"))


def calculate(scratch):
    """Evaluate the public methods with explicit synthetic assumptions."""
    from digitalmodel.structural.offshore_container import (
        SHS,
        DesignFactors,
        allowable_stress,
        sling_angle_to_vertical,
        utilization,
    )
    from digitalmodel.structural.offshore_container.dnv_2_7_1 import G

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    # A1/A2: synthetic geometry from D1/D2; A3: arbitrary arithmetic inputs.
    # Every factor is explicit. None is obtained from DesignFactors defaults.
    factors = DesignFactors(
        structure_load_factor=1.6,
        padeye_load_factor=1.2,
        single_padeye_load_factor=1.7,
        usage_factor=0.5,
        default_sling_angle_to_vertical_deg=12.0,
        tare_fraction=0.1,
    )
    inputs = {
        "b_m": 0.2, "t_m": 0.05, "b_mm": 200.0, "t_mm": 50.0,
        "length_m": 6.0, "width_m": 8.0, "lift_height_m": 5.0,
        "rating_kg": 1000.0, "fy_Pa": 200e6, "n_padeyes": 4,
        "padeye_area_m2": 0.02, "gravity_m_s2": G,
        "factors": asdict(factors),
    }
    section = SHS(b=inputs["b_m"], t=inputs["t_m"])
    converted = SHS.from_mm(b_mm=inputs["b_mm"], t_mm=inputs["t_mm"])

    def properties(member):
        return {"area_m2": member.area, "inertia_m4": member.inertia,
                "modulus_m3": member.modulus}

    result = {
        "schema_version": 1,
        "reviewed_source_revision": REVIEWED_REVISION,
        "evidence_class": "Synthetic geometry and caller-factor arithmetic; physical suitability NOT_EVALUATED",
        "inputs": inputs,
        "sections": {"metres": properties(section), "millimetres": properties(converted)},
        "angle_rad": sling_angle_to_vertical(
            inputs["length_m"], inputs["width_m"], inputs["lift_height_m"]),
        "allowable_stress_Pa": allowable_stress(inputs["fy_Pa"], factors),
        "utilization": asdict(utilization(
            length=inputs["length_m"], width=inputs["width_m"],
            lift_height=inputs["lift_height_m"], rating_kg=inputs["rating_kg"],
            section=section, fy=inputs["fy_Pa"], factors=factors,
            n_padeyes=inputs["n_padeyes"], padeye_area_m2=inputs["padeye_area_m2"],
        )),
        "open_findings": [
            {"topic": "Strength", "status": "NOT_EVALUATED", "finding":
             "The scalar axial and bending demands omit frame action, projection of horizontal pull onto individual rail axes, member buckling, combined stress, real-section effects, joints, welds, corrosion and other load cases."},
            {"topic": "Padeye", "status": "NOT_EVALUATED", "finding":
             "Net-area axial stress omits pin bearing, tear-out, bending, welds, shackle fit, out-of-plane load and local geometry."},
            {"topic": "Sharing", "status": "NOT_EVALUATED", "finding":
             "Attachment count is silently clamped to the implemented range; equal sharing over n minus one attachments is a model assumption, not an equilibrium solution for actual rigging."},
            {"topic": "Input domain", "status": "NOT_EVALUATED", "finding":
             "Public methods do not reject invalid dimensions, nonpositive areas, material stresses or inappropriate factors. The worked inputs are positive with a nonzero hollow core; wider domain behaviour is not qualified."},
            {"topic": "Unused fields", "status": "NOT_EVALUATED", "finding":
             "single_padeye_load_factor and default_sling_angle_to_vertical_deg do not affect utilization. A single attachment is mapped to the lower supported count; geometry always determines angle."},
            {"topic": "Factor provenance", "status": "NOT_EVALUATED", "finding":
             "utilization does not call factor_citations. The citation registry has its own values, validates metadata rather than clause content and does not bind caller overrides. No licensed standard was accessed."},
            {"topic": "Certification", "status": "NOT_EVALUATED", "finding":
             "No accepted design basis, independent design review, fabrication evidence, prototype test, operational qualification or certification is supplied."},
        ],
    }
    (scratch / "calculation.json").write_text(json_text(result), encoding="utf-8", newline="\n")
    return result


def number_rows(data):
    """Generate the complete input/output table, keeping each unit and basis."""
    rows = []

    def add(label, value, unit, basis):
        # Human display is rounded; results.json preserves floating-point values.
        shown = f"{value:.6g}"
        rows.append("<tr>" + "".join(f"<td>{html.escape(str(cell))}</td>"
                                     for cell in (label, shown, unit, basis)) + "</tr>")

    units = {
        "b_m": "m", "t_m": "m", "b_mm": "mm", "t_mm": "mm",
        "length_m": "m", "width_m": "m", "lift_height_m": "m",
        "rating_kg": "kg", "fy_Pa": "Pa", "n_padeyes": "count",
        "padeye_area_m2": "m²", "gravity_m_s2": "m/s²",
    }
    for name, unit in units.items():
        if name == "gravity_m_s2":
            basis = "GRAVITY + SI: conventional input G, not site gravity"
        elif name in {"b_mm", "t_mm"}:
            basis = "A1 + SI: same section in millimetres"
        elif name in {"b_m", "t_m"}:
            basis = "A1 / D1: synthetic section input"
        elif name in {"length_m", "width_m", "lift_height_m", "n_padeyes"}:
            basis = "A2 / D2: synthetic centred corner attachments"
        else:
            basis = "A3: synthetic arithmetic input; no accepted design basis"
        add(name, data["inputs"][name], unit, basis)
    for name, value in data["inputs"]["factors"].items():
        add(name, value, "deg" if name.endswith("_deg") else "1",
            "A3: explicit synthetic factor; no standard prescription")
    for route, section in data["sections"].items():
        for name, unit in (("area_m2", "m²"), ("inertia_m4", "m⁴"), ("modulus_m3", "m³")):
            add(f"{route}.{name}", section[name], unit, "SHS public property / D1 integral")
    add("angle_rad", data["angle_rad"], "rad", "sling_angle_to_vertical / D2 right triangle")
    add("allowable_stress_Pa", data["allowable_stress_Pa"], "Pa",
        "allowable_stress / A3: usage_factor × fy")
    for name, value in data["utilization"].items():
        unit = "deg" if name.endswith("_deg") else "kN" if name.endswith("_kN") else "1"
        add(f"utilization.{name}", value, unit,
            "utilization / API equations + A3; arithmetic only")
    return rows


def geometry_figure(data):
    """Generate an inline geometric sketch from the supplied section inputs."""
    inputs = data["inputs"]
    # Presentation coordinates are dimensionless and carry no engineering scale.
    ratio = (inputs["b_m"] - 2 * inputs["t_m"]) / inputs["b_m"]
    inner = 160 * ratio
    offset = (160 - inner) / 2
    label = html.escape(
        f'b={inputs["b_m"]:g} m; t={inputs["t_m"]:g} m; '
        f'L={inputs["length_m"]:g} m; B={inputs["width_m"]:g} m; '
        f'h={inputs["lift_height_m"]:g} m')
    return f'''<figure><svg viewBox="0 0 760 255" role="img" aria-labelledby="geometry-title geometry-description">
<title id="geometry-title">Ideal SHS and centred sling triangle</title>
<desc id="geometry-description">{label}. Sketch only; the triangle lies in the corner-to-centre vertical plane.</desc>
<rect x="25" y="30" width="160" height="160" fill="var(--panel-2)" stroke="currentColor"/>
<rect x="{25 + offset:g}" y="{30 + offset:g}" width="{inner:g}" height="{inner:g}" fill="var(--panel)" stroke="currentColor"/>
<text x="25" y="220">Ideal concentric section</text>
<path d="M395 190 L555 190 L555 30 Z" fill="none" stroke="currentColor"/>
<text x="565" y="110">h</text><text x="455" y="213">r</text>
<text x="455" y="88">sling</text><text x="535" y="65">v</text>
<text x="395" y="240">r = half-diagonal; v from vertical</text>
</svg><figcaption>{label} [A1, A2]. Positive h is upward; r is outward from plan centre to corner. Figure generated by the builder.</figcaption></figure>'''


def render(data):
    """Render ten report sections with escaped evidence and inline house style."""
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    history = "".join("<tr>" + "".join(f"<td>{html.escape(row[key])}</td>"
                                      for key in ("revision", "date", "change")) + "</tr>" for row in CHANGES)
    findings = "".join(f'<tr><td>{html.escape(row["topic"])}</td><td>{html.escape(row["status"])}</td>'
                       f'<td>{html.escape(row["finding"])}</td></tr>' for row in data["open_findings"])
    references = "".join(f'<li><a href="{html.escape(link, quote=True)}">{html.escape(key)}</a>: {html.escape(text)}</li>'
                         for key, link, text in REFERENCES)
    numbers = "".join(number_rows(data))
    geom = data["sections"]["metres"]
    values = data["utilization"]
    nav = " · ".join(f'<a href="#s{i}">{title}</a>' for i, title in enumerate(
        ("Summary", "Scope", "Inputs", "Method", "Worked calculation", "Checks", "Results", "Limits", "Way forward", "Evidence"), 1))
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Offshore-container module analysis</title><style>{brand}
.wrap{{max-width:1100px;margin:2rem auto;padding:0 1.25rem}}
header,section,details{{background:var(--panel);border:1px solid var(--edge);border-radius:var(--r);padding:1.25rem;margin:1rem 0}}
h1,h2,h3{{color:var(--brand)}}a{{color:var(--brand)}}nav{{line-height:2}}
table{{border-collapse:collapse;width:100%;font-size:.92rem}}th,td{{text-align:left;vertical-align:top;border:1px solid var(--edge);padding:.6rem;overflow-wrap:anywhere}}
th,.eq{{background:var(--panel-2)}}caption,figcaption{{text-align:left;color:var(--muted);padding:.6rem 0;caption-side:bottom}}
.eq{{padding:1rem;overflow-wrap:anywhere}}svg{{width:100%;height:auto}}.scroll{{overflow-x:auto}}code{{overflow-wrap:anywhere}}
@media print{{header,section,details{{break-inside:avoid}}.wrap{{max-width:none}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><p class="lbl">Structural calculation evidence</p><h1>Offshore-container module analysis</h1>
<p>Ideal section geometry, sling geometry and caller-factor arithmetic. Physical qualification: NOT_EVALUATED.</p><nav aria-label="Report sections">{nav}</nav></header>
<section><h2 id="s1">1. Summary</h2>
<p>The public package calculates ideal SHS properties, a centred sling angle and reduced member demand-to-allowable ratios.
The independently derived section and sling example is reproduced: A = {geom["area_m2"]:.4g} m², I = {geom["inertia_m4"]:.4g} m⁴,
Z = {geom["modulus_m3"]:.4g} m³ and v = {values["sling_angle_to_vertical_deg"]:.1f}° [D1/D2].
An explicitly synthetic usage scalar gives {data["allowable_stress_Pa"]:.4g} Pa [A3]. These are arithmetic checks, not accepted design factors or a resistance assessment.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<details id="changes-panel"><summary>Change evidence</summary><p>The revision row is generated from <a href="changes.json">changes.json</a>.
Machine-readable calculation evidence is in <a href="results.json">results.json</a>; <a href="manifest.json">manifest.json</a> binds source and delivered bytes.
The <a href="README.md">rebuild instructions</a> explain the evidence boundary.</p></details></section>
<section><h2 id="s2">2. Context and scope</h2>
<p>Decision: determine which software outputs have independent arithmetic support and which claims require further engineering evidence.
The exact reviewed source revision is <code>{REVIEWED_REVISION}</code>. No calculation source is changed by this report.
It is a single synthetic example; range coverage, physical validation and acceptance-policy review are NOT_EVALUATED.</p>
<p>The load path assumed by the implementation is mass → factored weight → attachment vertical load → resolved sling tension and inward horizontal load → top-rail axial stress.
Separately, a factored internal weight becomes a uniform load shared between long floor rails, then simple-span bending stress.
A padeye receives only a scalar net-area stress check [API].</p></section>
<section><h2 id="s3">3. Input basis</h2>
<p>A1 is the synthetic sharp-cornered, concentric section in D1: outer side b and uniform wall t. A2 is the centred rectangle and link height in D2.
All four attachments are at the plan corners. The origin is the plan centre at the attachment plane; x follows length, y follows width and z points upward.
The sling angle v is measured from positive vertical toward a corner. Stress and returned ratios are unsigned scalar magnitudes; signed force equilibrium is not solved.</p>
<p>A3 consists of arbitrary mass, yield stress, net area and explicit factor inputs chosen only to exercise arithmetic.
Every DesignFactors field is supplied. Synthetic factors are not prescribed by a standard and are not an accepted design basis.
The unused angle and single-attachment fields are retained to expose their lack of effect.
Inputs and outputs, units and their bases are all in the numbers table in Results.</p>
<p>The SI prefix milli gives mm = m × 10³; centi gives cm = m × 10²; kilo gives kN = N / 10³ [SI].
The public gravity definition is 980.665 cm/s², giving G = 9.80665 m/s² [GRAVITY, SI]. G is conventional gravity, not site gravity.</p>
{geometry_figure(data)}</section>
<section><h2 id="s4">4. Method</h2>
<p>Let R be mass in kg, g acceleration in m/s², kS the structure scalar, kP the attachment scalar, fT the tare fraction and u the usage scalar.
The following equations describe the existing algorithm, with scalar factors provided by the caller [API].</p>
<div class="eq">T = fT R; FL = kS R g [N]; Fi = (kS R − T) g [N]; σallow = u fy [Pa].</div>
<div class="eq">n = clamp(ninput, 2, 4); Q = kP R g / (n − 1) [N]; RSL = Q / cos(v) [N]; H = Q tan(v) [N].</div>
<p>The count bounds are the implementation policy [API], not a standard prescription adopted here. A single requested attachment is mapped to the lower bound; there is no separate single-attachment calculation.</p>
<div class="eq">Utop = (H/A) / σallow; w = Fi/(2L) [N/m]; Mmax = w L²/8 = Fi L/16 [N m];
Ufloor = (Mmax/Z)/σallow; Upad = (RSL/Apad)/σallow; Ugov = max(Utop, Ufloor, Upad).</div>
<p>The rail share divisor represents two long rails. For a simply supported beam, reactions are wL/2 and M(x) = wLx/2 − wx²/2;
setting its derivative to zero gives x = L/2 and the displayed maximum moment. This independently derives the beam coefficient, but does not establish that the actual floor behaves as that beam.
N/m² = Pa; N m/m³ = Pa; stress divided by allowable stress is dimensionless. Loads returned in kN divide the calculated newtons by the SI kilo scale.
Angle in degrees is radians × 180/π by the definitions of a full turn and a degree [D2].</p>
<h3>Public function and data inventory</h3>
<table><thead><tr><th>Public item</th><th>Inputs → outputs and review disposition</th></tr></thead><tbody>
<tr><td>DesignFactors</td><td>Caller scalars, angle in degrees and tare fraction → frozen factor record. Defaults are not reproduced or adopted here.</td></tr>
<tr><td>SHS; area, inertia, modulus; from_mm; __str__</td><td>b,t in m → A in m², I in m⁴, Z in m³; from_mm converts b,t from mm; string formatting gives a nominal mm label. Ideal geometry only; input validation absent.</td></tr>
<tr><td>sling_angle_to_vertical</td><td>L,B,h in m → atan2(half-diagonal,h) in rad; centred geometry only.</td></tr>
<tr><td>allowable_stress</td><td>fy in Pa and factor record → u fy in Pa; multiplication does not establish material suitability or the correct design criterion.</td></tr>
<tr><td>utilization</td><td>L,B,h [m], R [kg], section, fy [Pa], factor record, attachment count, net area [m²] → Utilization. All arguments used by the example are explicit.</td></tr>
<tr><td>Utilization</td><td>top_rail, floor, pad_eye and governing [1]; sling_angle_to_vertical_deg [deg]; rsl_kN and FL_kN [kN]. All returned fields appear in Results.</td></tr>
<tr><td>factor_citations</td><td>Optional repository root → mapping of factor names to CitedValue records. Independent from utilization; no runtime citation resolution is requested for this synthetic report.</td></tr>
<tr><td>SECTION_CATALOGUE</td><td>Predefined named SHS objects. Catalogue selection, real corner radii and material qualification are outside this example.</td></tr>
</tbody></table>
<p>factor_citations imports a separate registry and requests citation-backed values. CitationResolutionError produces an empty mapping and a warning, issued once through module state.
The schema validates code identity, publisher and revision in metadata; it does not validate clause locators or clause text [CITATIONS, VALIDATOR].
The wrapper catches resolution errors including missing or mismatched metadata; the underlying validator's fail-closed behavior therefore does not make utilization fail closed.
The tare fraction is not requested by this citation wrapper. Caller overrides are not tied to resolved citation values. Standard-factor provenance and suitability remain NOT_EVALUATED.</p></section>
<section><h2 id="s5">5. Worked calculation</h2>
<p>D1 subtracts the inner square from the outer: bi = b − 2t = 0.1 m. Integrating y² over each concentric square gives
I = ∫ y² dA = (b⁴ − bi⁴)/12. Thus A = 0.2² − 0.1² = 0.03 m²,
I = (0.2⁴ − 0.1⁴)/12 = 0.000125 m⁴ and Z = I/(b/2) = 0.00125 m³.
The integral of y² over a centred rectangle gives the denominator; it is not a fitted coefficient [D1].</p>
<p>D2 gives r = √((6/2)² + (8/2)²) = √(3² + 4²) = 5 m. Height is also 5 m,
so tan(v) = r/h = 1 and v = π/4 rad. Both SI constructor routes are checked against the same integrals.</p>
<p>A3 sets u = 0.5 and fy = 200 × 10⁶ Pa, hence σallow = 100 × 10⁶ Pa.
With the explicit mass and factors in Results, Q = 400g N, RSL = 400g√2 N, H = 400g N,
Fi = 1500g N, Mmax = 562.5g N m and FL = 1600g N. These coefficients follow solely from A3 substituted into Method.
Independent assertions use these closed expressions and the D1 section integrals. They establish arithmetic, not a strength or acceptance basis.</p></section>
<section><h2 id="s6">6. Checks</h2>
<p>The test file [TEST] checks the normal public API against D1/D2 and the explicit A3 allowable scalar, separately from production replay.
The synthetic load expressions check every returned Utilization field. Section properties use relative tolerance 1e−12 with zero absolute allowance;
angles use absolute tolerance 1e−12 in the relevant angular unit with zero relative allowance. Other synthetic arithmetic uses relative tolerance 1e−12 with zero absolute allowance.
These tolerances permit floating-point rounding only; they are not measurement uncertainties or design margins.</p>
<p>Separate tests generate two clean external deliveries, compare all bytes with the committed delivery, read every manifest digest independently,
and check local links, sections and embedded JSON. Changed values and reversed signs fail the analytical oracle even when builder and report agree.
Altering a delivered result also fails the byte manifest. Rebuilding or replaying the same calculation is a freshness check, not independent numerical evidence.</p>
<p>No claim of physical validation, input-domain coverage or design acceptance follows from these software checks.</p></section>
<section><h2 id="s7">7. Results</h2>
<p>The numbers table is generated from public-method outputs and explicit inputs, rather than typed result values.
Human-readable values are rounded; the exact floating-point serialization is retained in results.json. The governing value is only the largest of the implemented scalar ratios.
There is no pass/fail or compliance decision.</p>
<div class="scroll"><table><caption>Input → method → answer chain. A1/A2: synthetic geometry; A3: caller arithmetic; API: implementation equation; D1/D2: independent derivations.</caption>
<thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source or derivation</th></tr></thead><tbody>{numbers}</tbody></table></div></section>
<section><h2 id="s8">8. Limits and NOT_EVALUATED findings</h2>
<table><thead><tr><th>Topic</th><th>Status</th><th>Open finding</th></tr></thead><tbody>{findings}</tbody></table>
<p>The implementation cites DNVGL-ST-E271 (2.7-1), Offshore containers, August 2017. The licensed text was not accessed;
clause content, factor values and standard conformity are NOT_EVALUATED. No default factor values are published in this report.</p></section>
<section><h2 id="s9">9. Way forward</h2>
<p>Before physical use, establish an accepted load and resistance basis with the applicable standard and material data;
resolve attachment count, rigging geometry and eccentric load sharing; qualify frame, floor and padeye resistance with the relevant failure modes;
and add input-domain validation with reviewed boundary cases. Obtain independent engineering review and required test evidence.
Treat these as open method and acceptance-policy findings; this report does not replace algorithms or select a design basis.</p>
<p>A parameter study and physical-use readiness remain NOT_EVALUATED. This single-point example supplies no lookup envelope or range qualification.</p></section>
<section><h2 id="s10">10. References and evidence</h2><ul>{references}</ul>
<p>The manifest records exact SHA-256 source and artifact bytes. Its own digest is excluded to avoid a circular binding;
the rebuild test compares its complete bytes. Source revision describes the reviewed calculation, while individual digests bind the delivered evidence.</p></section>
<script id="calculation" type="application/json">{embedded_json(data)}</script>
<script id="change-history" type="application/json">{embedded_json(CHANGES)}</script>
</main></body></html>
'''
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


def readme():
    """Give portable rebuild instructions with no environment identity."""
    return """# Offshore-container module analysis

The [report](index.html) connects public section and sling geometry to
independent hand derivations, then demonstrates caller-factor arithmetic.
All factors in the example are synthetic inputs, not a prescribed standard basis.
Strength, padeye adequacy, actual load sharing and certification are NOT_EVALUATED.

## Rebuild

From the repository root, choose external scratch and output directories:

```sh
PYTHONPATH=src python scripts/reports/build_offshore_container_module_report.py \\
  --scratch "$REPORT_SCRATCH" --output-dir "$REPORT_OUTPUT"
```

Both arguments are required. Scratch is rejected if it is inside the checkout.
Only final deliverables belong in this folder; intermediate calculation evidence
is written to scratch. Run the report tests with an external pytest base:

```sh
PYTHONPATH=src python -m pytest tests/reporting/test_offshore_container_module_report.py \\
  -q -p no:cacheprovider --basetemp="$TEST_SCRATCH"
```

The builder generates index.html, results.json, changes.json, this README and
manifest.json. The manifest binds every other delivered file and the focused
source evidence using SHA-256 over exact UTF-8 bytes with LF endings. The manifest
cannot bind itself; byte-rebuild tests compare it in full. No paths, run times or
environment identities are serialized. The HTML contains inline CSS, SVG and
escaped JSON; no network assets or executable scripts are needed to read it.

The tests separate normal API assertions against geometry integrals and explicit
synthetic arithmetic from freshness checks. Negative controls reverse signs and
change magnitudes even when report and builder agree. No physical acceptance,
parameter-range qualification or standards-compliance claim follows from them.
"""


def build(scratch, output_dir):
    """Write only final deliverables to the destination and bind their bytes."""
    data = calculate(scratch)
    output = Path(output_dir).resolve()
    scratch = Path(scratch).resolve()
    if output == scratch or output in scratch.parents or scratch in output.parents:
        raise ValueError("Scratch and output must be separate directories")
    output.mkdir(parents=True, exist_ok=True)
    outputs = {"index.html": render(data), "results.json": json_text(data),
               "changes.json": json_text(CHANGES), "README.md": readme()}
    for name, text in outputs.items():
        (output / name).write_text(text, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1,
        "reviewed_source_revision": REVIEWED_REVISION,
        "digest_convention": "SHA-256 over exact bytes; manifest self-binding excluded; full bytes checked by rebuild test.",
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in SOURCES},
        "artifacts": {name: hashlib.sha256((output / name).read_bytes()).hexdigest() for name in outputs},
    }
    (output / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")


def main():
    """Parse explicit destination paths and build the small report delivery."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
