# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build an offline tubulars report from public stress helpers and source review."""

import argparse
import ast
import hashlib
import html
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = Path("src/digitalmodel/well/tubulars")
DESTINATION = ROOT / "docs/reports/modules/well_tubulars"
EXAMPLE = PACKAGE / "docs/worked-tubular-stress-invariants.md"
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
BUILDER = "scripts/reports/build_well_tubulars_module_report.py"
TEST = "tests/reporting/test_well_tubulars_module_report.py"
BRAND = "docs/api/_assets/brand.css"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Added the public-function inventory, synthetic stress calculation and independent checks to make the module scope and unqualified methods explicit.",
}]
# All engineering inputs are the assumptions in the worked example, Table 1.
GEOMETRY = {"od_in": 10.2, "id_in": 9.8}
LOADS = {
    "internal": (500.0, 100.0, 6000.0, 2000.0),
    "reversed": (100.0, 500.0, 6000.0, 2000.0),
    "pure_shear": (0.0, 0.0, 0.0, 1500.0),
}
# Sanitized metadata inventory: source attributes are identified without copying
# people, organizations, licensed table values or operational acceptance factors.
METADATA = {
    "casing": "CODE_REFERENCE: API 5CT / API 5C3; CasingGrade.standard: API 5CT. Editions and clauses are unspecified.",
    "casing_design": "SOURCE_REFERENCE and DesignFactors.reference: workshop presentation, API 5C3, NACE MR0175. SourServiceAssessment.reference: NACE MR0175 / ISO 15156; source comments name Table A.3. Editions and applicability are unqualified.",
    "tubing_design": "SOURCE_REFERENCE: workshop presentation and API 5C3; module docstring also names API TR 5C3 and tubing-packer force analysis. Editions and clauses are unspecified.",
    "design_envelope": "Module docstring: API TR 5C3 / ISO 10400:2011, drilling textbook chapter and SPE 48330. No helper-level structured citation metadata. Only the local worked-example derivations are used for arithmetic checks.",
    "sucker_rod": "CODE_REFERENCE: API 11B / modified Goodman; module docstring also names API 11BR. Editions, clauses and service-factor basis are unspecified.",
}
# Output units and semantics are reviewed from the source bodies, not inferred
# from a successful test or from catalogue values. No ratings are calculated here.
OUTPUTS = {
    "TubularGeometry": "Geometry holder: wall and mean diameter (in), metal area (in²), yield force (lbf); supplied strength and ratings (psi).",
    "compute_hoop_stress": "Signed mean-diameter hoop membrane stress (psi).",
    "compute_vme_stress": "Nonnegative plane-stress equivalent stress including shear (psi).",
    "IsotropicVmeEnvelope": "Envelope object; compute returns axial-force (lbf), differential-pressure (psi) pairs.",
    "ApiEllipseEnvelope": "Envelope object; compute returns axial-force (lbf), differential-pressure (psi) pairs.",
    "AnisotropicVmeEnvelope": "Envelope object with directional yield ratio; compute returns lbf/psi pairs.",
    "design_envelope_points": "Mapping of named envelope arrays, each containing lbf/psi coordinate pairs.",
    "CasingGrade": "Grade holder: minimum/maximum yield and minimum tensile strengths (psi), standard identifier.",
    "casing_grade": "CasingGrade lookup; no material qualification is inferred.",
    "internal_yield_pressure": "Internal yield pressure (psi); wall-tolerance basis unqualified.",
    "pipe_body_yield_strength": "Pipe-body axial yield force (lbf).",
    "collapse_pressure": "Collapse pressure (psi) and regime label; all regime qualifications remain open.",
    "Casing": "Product object with ID/wall (in), area (in²), diameter/wall ratio, burst/collapse (psi), body yield (lbf), regime/reference and geometry bridge.",
    "list_sizes": "Sorted casing OD (in), nominal weight (lb/ft) pairs.",
    "wall_for_size": "Catalogue wall thickness (in).",
    "casing": "Casing product from grade and either nominal weight or explicit wall, exclusively.",
    "list_tubing_sizes": "Sorted tubing OD (in), nominal weight (lb/ft) pairs.",
    "tubing_wall_for_size": "Tubing catalogue wall thickness (in).",
    "tubing": "Casing product using the tubing catalogue or an explicit wall, exclusively.",
    "api_round_pressure_psi": "Rounded pressure (psi); source rounding convention is not independently qualified.",
    "api_round_force_lbf": "Rounded axial force (lbf); source rounding convention is not independently qualified.",
    "PressureProfile": "Piecewise-linear depth (ft), pressure (psi) holder; at interpolates pressure (psi).",
    "fluid_column_profile": "PressureProfile in ft/psi for a fluid column and surface pressure.",
    "shut_in_tubing_pressure": "Surface shut-in pressure (psi) from reservoir pressure and gas gradient.",
    "tubing_leak_internal_profile": "Internal tubing-leak PressureProfile (ft/psi).",
    "injection_internal_profile": "Internal static injection PressureProfile (ft/psi).",
    "burst_external_profile": "External backup PressureProfile (ft/psi), with top-of-cement and shoe anchors.",
    "full_evacuation_internal_profile": "Evacuated internal PressureProfile (ft/psi).",
    "DesignFactors": "Factor holder (dimensionless), pressure threshold (psi) and reference; burst_for_sicp selects a factor. Defaults are not an adopted design basis.",
    "ModeCheckResult": "Mode, rating/load (psi for pressure or equivalent-stress modes; lbf for tension), dimensionless factors, governing depth (ft), flag and load-case label; as_dict serializes them.",
    "check_burst": "ModeCheckResult for net internal pressure versus burst rating.",
    "reduced_yield_under_tension": "Axial-stress-equivalent yield (psi).",
    "collapse_pressure_under_tension": "Derated collapse pressure (psi) and regime label.",
    "check_collapse": "ModeCheckResult for net external pressure, optionally tension-derated.",
    "buoyancy_factor": "Dimensionless buoyancy factor using the source steel density assumption.",
    "axial_load_profile": "Arrays of depth (ft) and buoyed axial load (lbf).",
    "ballooning_tension_delta_lbf": "Restrained ballooning axial-force increment (lbf).",
    "cooling_tension_delta_lbf": "Restrained cooling axial-force increment (lbf); temperature drop is positive.",
    "check_tension": "ModeCheckResult for buoyed weight and additional axial force versus body yield.",
    "check_triaxial": "ModeCheckResult from thin-wall hoop plus axial plane stress; name does not establish through-wall stress treatment.",
    "max_frac_surface_pressure": "Surface pressure (psi) conditional on supplied burst factor and profiles.",
    "h2s_partial_pressure_psia": "H₂S partial absolute pressure (psia).",
    "SourServiceAssessment": "Screening flag, partial/total pressure (psia), well type, grade names and reference; criteria unqualified.",
    "sour_service_screen": "SourServiceAssessment; no environmental or grade acceptance established.",
    "grade_sour_ok": "Grade/temperature screening flag; no material acceptance established.",
    "ConnectionClass": "Connection metadata with thread count (per in), dimensionless efficiency range and strength basis; joint_strength_lbf returns a screening force (lbf).",
    "connection_class": "ConnectionClass lookup; joint performance unqualified.",
    "ProductionCasingWell": "Well/load holder: depths (ft), fluid densities (lb/gal), pressures (psi), gas gradient (psi/ft).",
    "check_production_casing": "Mapping of burst, collapse, tension and equivalent-stress ModeCheckResult records.",
    "shut_in_tubing_profile": "Internal shut-in PressureProfile (ft/psi).",
    "packer_fluid_annulus_profile": "Annulus fluid PressureProfile (ft/psi).",
    "shut_in_piston_force_lbf": "Signed packer pressure-area axial force (lbf).",
    "TubingWell": "Well/load holder: depth (ft), fluid density (lb/gal), pressures (psi), optional packer bore (in), gas gradient (psi/ft).",
    "check_production_tubing": "Mapping of shut-in burst, evacuated collapse and tension ModeCheckResult records.",
    "RodGrade": "Rod grade and minimum/maximum tensile-strength holder (psi).",
    "rod_grade": "RodGrade lookup; no material qualification is inferred.",
    "rod_area": "Circular rod metal area (in²).",
    "modified_goodman_allowable": "Modified-Goodman peak allowable stress (psi), conditional on service factor.",
    "SuckerRod": "Rod product; area (in²), stress/allowable (psi), cycle loading (dimensionless), flag/reference and to_rod_section bridge.",
    "sucker_rod": "SuckerRod product from grade, diameter (in) and service factor (dimensionless).",
    "RodTaperSection": "Rod product and section length (ft).",
    "rod_string_goodman": "Per-section stress/allowable (psi), loading ratio, flag, diameter (in), length (ft), reference; sections include peak/minimum loads (lbf).",
}


def encode(data):
    """Serialize deterministic JSON without nonfinite values or platform newlines."""
    return json.dumps(data, indent=2, sort_keys=True, ensure_ascii=False,
                      allow_nan=False) + "\n"


def escaped_json(data):
    """Protect a JSON script data block from HTML termination and entity text."""
    return encode(data).replace("&", "\\u0026").replace("<", "\\u003c").replace(
        ">", "\\u003e")


def argument_names(node):
    """Describe required and optional arguments without publishing defaults."""
    args = node.args
    positional = args.posonlyargs + args.args
    required = len(positional) - len(args.defaults)
    names = [arg.arg + (" (optional)" if index >= required else "")
             for index, arg in enumerate(positional) if arg.arg != "self"]
    names += [arg.arg + (" (optional)" if default is not None else " (required)")
              for arg, default in zip(args.kwonlyargs, args.kw_defaults)]
    return ", ".join(names) or "No call arguments; object state where applicable."


def inventory():
    """Inventory every public definition, including non-reexported helpers."""
    rows = []
    for path in sorted((ROOT / PACKAGE).glob("*.py")):
        if path.stem == "__init__":
            continue
        for node in ast.parse(path.read_text(encoding="utf-8")).body:
            if not isinstance(node, (ast.FunctionDef, ast.ClassDef)) or node.name.startswith("_"):
                continue
            methods = []
            if isinstance(node, ast.ClassDef):
                fields = []
                for child in node.body:
                    if isinstance(child, ast.AnnAssign) and isinstance(child.target, ast.Name):
                        value = child.value
                        derived = (isinstance(value, ast.Call) and any(
                            item.arg == "init" and isinstance(item.value, ast.Constant)
                            and item.value.value is False for item in value.keywords))
                        if not derived:
                            fields.append(child.target.id + (" (optional)" if value else " (required)"))
                    if isinstance(child, ast.FunctionDef) and not child.name.startswith("_"):
                        methods.append({"name": child.name, "inputs": argument_names(child)})
                constructor = next((child for child in node.body if isinstance(
                    child, ast.FunctionDef) and child.name == "__init__"), None)
                inputs = argument_names(constructor) if constructor else ", ".join(fields)
            else:
                inputs = argument_names(node)
            rows.append({
                "module": path.stem, "name": node.name, "line": node.lineno,
                "inputs": inputs, "outputs": OUTPUTS[node.name], "methods": methods,
                "citation_metadata": METADATA[path.stem],
                "status": "ARITHMETIC_CHECKED" if node.name in {
                    "compute_hoop_stress", "compute_vme_stress"} else "NOT_EVALUATED",
            })
    return rows


def external_scratch(scratch):
    """Reject intermediate writes inside the checkout."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Scratch must be outside the checkout")
    return scratch


def calculate(scratch):
    """Execute only the documented public stress helpers with unchanged inputs."""
    from digitalmodel.well.tubulars import compute_hoop_stress, compute_vme_stress

    external_scratch(scratch)
    cases = {}
    for name, (internal, external, axial, shear) in LOADS.items():
        differential = internal - external
        hoop = compute_hoop_stress(differential, **GEOMETRY)
        cases[name] = {
            "internal_pressure_psi": internal, "external_pressure_psi": external,
            "differential_pressure_psi": differential, "axial_psi": axial,
            "shear_psi": shear, "hoop_psi": hoop,
            "vme_psi": compute_vme_stress(axial, hoop, shear),
        }
    return {"geometry": GEOMETRY.copy(), "cases": cases, "stress_unit": "psi",
            "basis": "Synthetic assumptions from worked-tubular-stress-invariants.md, Table 1; no material strength or design factor is supplied."}


def source_paths():
    """Bind reviewed code, derivation, builder, tests and shared styling."""
    return sorted([str(path.relative_to(ROOT)) for path in (ROOT / PACKAGE).glob("*.py")]
                  + [EXAMPLE.as_posix(), BUILDER, TEST, BRAND])


def source_link(path, label):
    """Use portable repository-relative links, with escaped labels and targets."""
    return f'<a href="../../../../{html.escape(str(path), quote=True)}">{html.escape(label)}</a>'


def inventory_html():
    """Render input/output contracts without grade values or acceptance defaults."""
    rows = []
    for row in inventory():
        methods = "; ".join(f'{item["name"]}({item["inputs"]})' for item in row["methods"])
        rows.append(
            '<tr><td>' + source_link(PACKAGE / (row["module"] + ".py"), row["module"])
            + f':<code>{html.escape(row["name"])}</code></td><td>{html.escape(row["inputs"])}'
            + (f'<br>Members: {html.escape(methods)}' if methods else '')
            + f'</td><td>{html.escape(row["outputs"])}</td><td>{row["status"]}</td></tr>')
    return "\n".join(rows)


def render(data):
    """Render the ten-section analysis with embedded results and local assets."""
    esc = html.escape
    rows = []
    inputs = []
    for name, case in data["cases"].items():
        cells = "".join(f'<td>{case[field]:.1f}</td>' for field in (
            "differential_pressure_psi", "axial_psi", "shear_psi", "hoop_psi", "vme_psi"))
        rows.append(f'<tr><th scope="row">{esc(name)}</th>{cells}<td>[A, B]</td></tr>')
        values = "".join(f'<td>{case[field]:.1f}</td>' for field in (
            "internal_pressure_psi", "external_pressure_psi", "axial_psi", "shear_psi"))
        inputs.append(f'<tr><th scope="row">{esc(name)}</th>{values}<td>Assumed [A]</td></tr>')
    history = "".join('<tr>' + ''.join(f'<td>{esc(item[key])}</td>' for key in (
        "revision", "date", "change")) + '</tr>' for item in CHANGES)
    metadata = "".join(f'<li><code>{esc(name)}.py</code>: {esc(value)}</li>'
                       for name, value in METADATA.items())
    evidence = "\n".join('<li>' + source_link(path, path) + '</li>' for path in source_paths())
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / BRAND).read_text(encoding="utf-8"), flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Well tubulars module analysis</title>
<style>{brand}
.wrap{{max-width:var(--wrap);margin:auto;padding:2rem}}
header,section,details{{background:var(--panel);border:1px solid var(--edge);border-radius:var(--r);padding:1.4rem;margin:1rem 0}}
h1,h2,h3{{color:var(--brand)}} p,li{{line-height:1.65}}
.table-scroll{{overflow:auto}} table{{border-collapse:collapse;width:100%;margin:1rem 0}}
th,td{{text-align:left;padding:.65rem;border:1px solid var(--edge);vertical-align:top}}
th{{background:var(--panel-2)}}caption,figcaption{{caption-side:bottom;text-align:left;color:var(--muted);padding:.6rem 0}}
code,pre{{overflow-wrap:anywhere}}pre{{white-space:pre-wrap;background:var(--panel-2);padding:1rem}}
svg{{width:100%;height:auto;color:var(--ink)}}.notice{{border-left:4px solid var(--caution);padding-left:1rem}}
@media print{{header,section,details{{break-inside:auto}}.wrap{{padding:0}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><h1>Well tubulars module analysis</h1>
<p>Casing, tubing, stress envelopes and rod strings: source inventory and a synthetic thin-wall calculation.</p>
<p class="notice">Arithmetic evidence only. Design acceptance and physical qualification are NOT_EVALUATED.</p>
<p><a href="results.json">Calculation JSON</a> · <a href="manifest.json">Byte manifest</a> ·
<a href="changes.json">Change history</a> · <a href="README.md">Rebuild instructions</a></p></header>
<section><h2 id="s1">1. Summary</h2>
<p>The documented thin-wall stress helpers are executed for internal pressure, reversed differential pressure and pure shear. Hoop stresses are
{data["cases"]["internal"]["hoop_psi"]:.1f} psi and {data["cases"]["reversed"]["hoop_psi"]:.1f} psi for the pressure cases;
the equivalent stresses are {data["cases"]["internal"]["vme_psi"]:.1f} psi,
{data["cases"]["reversed"]["vme_psi"]:.1f} psi and {data["cases"]["pure_shear"]["vme_psi"]:.1f} psi, respectively [A, B].
Independent equilibrium and principal-stress checks cover these assumed cases only [C].</p>
<h3>Revision history</h3><div class="table-scroll"><table>
<thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody>
<caption>Initial issue and change basis; the same records are delivered in changes.json.</caption></table></div></section>
<section><h2 id="s2">2. Context and scope</h2>
<p>The package exposes product lookups, pressure profiles, axial loads, design-factor comparisons,
stress-envelope coordinates and rod-string cycle checks. Their presence is an inventory fact, not qualification.
Only <code>compute_hoop_stress</code> and <code>compute_vme_stress</code> are executed for engineering results in this report.</p>
<p>Reviewed source revision: <code>{REVIEWED_REVISION}</code>. The manifest records the actual source digests used for this build;
the fixed revision identifies the review basis rather than a claim that later source edits have been reviewed [D].</p>
<details><summary>Public function and class inventory</summary>
<p>Every public top-level definition in the package is listed, including module helpers not re-exported by
<code>__init__.py</code>. Public members are listed under their class. Private helpers are outside this API inventory.
Optional marks describe the implementation, without adopting its defaults.</p>
<div class="table-scroll"><table><thead><tr><th>Entry point</th><th>Required inputs and optional arguments</th><th>Output contract</th><th>Evidence status</th></tr></thead>
<tbody>{inventory_html()}</tbody><caption>Public API at the recorded source revision [D]. Output contracts are descriptive; only the two stress helpers have arithmetic evidence here.</caption></table></div>
<p>Shared unit key: <code>_in</code> = inches; <code>_in2</code> = square inches;
<code>_ft</code> = feet of true vertical depth unless a section length is stated;
<code>_psi</code> = lbf/in²; <code>_psia</code> = absolute psi;
<code>_lbf</code> = force; <code>_ppf</code> = nominal weight in lb/ft;
<code>_ppg</code> = density in lb/gal; <code>_psi_ft</code> = pressure gradient;
<code>_f</code> = degrees Fahrenheit (temperature drop for cooling);
<code>_ppm</code> = parts per million. Design/service factors, yield ratios, efficiencies and loading ratios are dimensionless.
Counts select numerical resolution. Profile objects contain ft/psi pairs; product, geometry, grade, well and factor objects carry the fields listed above.</p>
<p>Exported data containers are also present: <code>API_5CT_GRADES</code>, <code>CASING_SIZES</code>,
<code>TUBING_SIZES</code>, <code>CONNECTION_CLASSES</code>, <code>NACE_MIN_SERVICE_TEMP_F</code>,
<code>API_11B_ROD_GRADES</code> and <code>ROD_SIZES_IN</code>. Values are not reproduced or qualified.</p></details></section>
<section><h2 id="s3">3. Input basis</h2>
<p>{esc(data["basis"])} Outside diameter is {data["geometry"]["od_in"]:.3f} in and inside diameter is
{data["geometry"]["id_in"]:.3f} in [A]. Positive normal stress denotes tension. Differential pressure is internal minus external pressure.
No unit conversion is performed; supplied axial stress is independent, with no implicit closed-end load.</p>
<div class="table-scroll"><table><thead><tr><th>Case</th><th>Internal (psi)</th><th>External (psi)</th><th>Axial (psi)</th><th>Shear (psi)</th><th>Basis</th></tr></thead>
<tbody>{''.join(inputs)}</tbody><caption>Synthetic inputs reproduced unchanged from the worked example [A], not measurements or catalogue data.</caption></table></div>
</section>
<section><h2 id="s4">4. Method</h2>
<p>The hoop helper replaces both pressure surfaces by the mean diameter and balances a longitudinal half-cylinder.
The equivalent-stress helper assumes plane stress with zero radial normal stress and includes in-plane shear [A, B].</p>
<pre>D = (OD + ID)/2; t = (OD − ID)/2
Δp D L = 2 σh t L
T = [[a, s, 0], [s, h, 0], [0, 0, 0]]
m = (a + h)/2; r = sqrt(((a − h)/2)² + s²)
Principal stresses: λ = (m + r, m − r, 0)
q = sqrt(((λ₁ − λ₂)² + (λ₂ − λ₃)² + (λ₃ − λ₁)²)/2)</pre>
<p>The factors of two arise from cut-wall count, mean geometry and invariant normalization.
The independent test uses equilibrium and tensor eigenvalues, rather than the expanded production expression [A, C].</p>
</section>
<section><h2 id="s5">5. Worked calculation</h2>
<p>The hand derivation uses an arbitrary one-inch length, mean diameter 10.0 in and wall thickness 0.200 in.
The positive-pressure resultant is 400.0 × 10.0 × 1.0 = 4000.0 lbf. The two cut strips have combined
area 2 × 0.200 × 1.0 = 0.4 in², giving {data["cases"]["internal"]["hoop_psi"]:.1f} psi [A, B].</p>
<figure><svg viewBox="0 0 760 220" role="img" aria-labelledby="cut-title cut-description">
<title id="cut-title">Longitudinal half-cylinder free body</title>
<desc id="cut-description">A mean-surface pressure resultant is balanced by two tensile cut-wall forces. The diagram is schematic and not to scale.</desc>
<path d="M150 160 Q150 30 380 30 Q610 30 610 160" fill="var(--panel-2)" stroke="currentColor" stroke-width="2"/>
<path d="M150 160 L150 205 M140 190 L150 205 L160 190 M610 160 L610 205 M600 190 L610 205 L620 190 M380 90 L380 5 M370 20 L380 5 L390 20" fill="none" stroke="currentColor" stroke-width="2"/>
<text x="405" y="55">Δp D L</text><text x="175" y="195">σh t L</text><text x="515" y="195">σh t L</text>
<text x="225" y="140">Mean surface, length L</text></svg>
<figcaption>Builder-generated equilibrium schematic [A]. Geometry is conceptual; pressure reversal reverses the force signs.</figcaption></figure>
<p>For the in-plane tensor [[6000.0, 2000.0], [2000.0, 10000.0]] psi, the characteristic equation gives
8000.0 ± sqrt(8000000.0) psi and zero. Principal differences give sqrt(88000000.0) psi.
Reversing pressure alone gives −10000.0 psi hoop stress and principal values −2000.0 ± sqrt(68000000.0) psi and zero;
the equivalent stress becomes sqrt(208000000.0) psi. Pure shear of 1500.0 psi has principal values
(+1500.0, −1500.0, 0.0) psi and gives 1500.0 × sqrt(3) psi [A]. These are hand-derived expectations, not separate production results.</p>
</section>
<section><h2 id="s6">6. Checks</h2>
<p>The independent regression compares signed hoop stress and equivalent stress against the force balance and
principal differences above. The retained relative tolerance is <code>1e-12</code> and absolute tolerance is
<code>1e-8 psi</code> [A, C]. This rounding budget covers decimal geometry subtraction and square-root arithmetic;
it is not model uncertainty or a physical accuracy bound.</p>
<p>Separate checks rebuild into fresh external directories, compare all delivered bytes, replay the public helpers,
read every manifest digest independently, check local links and exercise tampered results. Negative controls change
a value or a hoop sign and regenerate mutually consistent HTML, JSON and hashes; the independent numerical assertion
still rejects them [C]. Replaying the same calculation is a freshness check only.</p>
</section>
<section><h2 id="s7">7. Results</h2>
<div class="table-scroll"><table><thead><tr><th>Case</th><th>Δp (psi)</th><th>Axial (psi)</th><th>Shear (psi)</th><th>Hoop (psi)</th><th>Equivalent (psi)</th><th>Basis</th></tr></thead>
<tbody>{''.join(rows)}</tbody><caption>Public-helper results, displayed to one decimal place [A, B]. Unrounded values are retained in results.json for numerical checks.</caption></table></div>
<p>No capacity, utilisation, allowable stress or design acceptance is calculated. The negative hoop value demonstrates
pressure sign only; it is not collapse resistance. A stress invariant alone does not establish elastic response for a material.</p>
<details><summary>Unrounded calculation evidence</summary><pre>{esc(encode(data))}</pre></details>
</section>
<section><h2 id="s8">8. Limits / NOT_EVALUATED</h2>
<p>The stress example assumes a straight circular uniform thin wall, small deformation, uniform loads and plane stress
away from discontinuities. The mean-surface substitution omits the distinct inner/outer pressure-surface correction;
absolute pressures and radial stress can matter. No universal thin-wall accuracy threshold is selected [A].</p>
<ul><li>Collapse regimes, regime transitions and imperfection sensitivity: NOT_EVALUATED. Independent method and applicability evidence are absent.</li>
<li>Catalogue dimensions/grades, connections, anisotropy and sour-service criteria: NOT_EVALUATED. Qualified source editions, clauses and material/environment evidence are absent.</li>
<li>Design acceptance and default factors: NOT_EVALUATED. No design basis or acceptance policy is adopted.</li>
<li>Rod fatigue/service factors, wear, corrosion, ovality, stability, thermal loads and operating qualification: NOT_EVALUATED. This calculation supplies no evidence for them.</li></ul>
<h3>Open source-review findings</h3>
<ul><li><code>check_production_casing</code> chooses the governing burst result but passes the tubing-leak profile to
<code>check_triaxial</code> even when injection governs burst. Cross-case triaxial adequacy is not established [D].</li>
<li>The isotropic and anisotropic VME implementations switch quadratic roots halfway through a single axial sweep.
Full envelope branch coverage is not established by this report. No envelope is plotted or used as an acceptance boundary [D].</li>
<li>The function named <code>check_triaxial</code> uses a plane-stress membrane invariant; it does not evaluate radial stress through the wall [D].</li></ul>
<p>These findings require separate method review. No algorithm or acceptance policy is replaced here.</p>
</section>
<section><h2 id="s9">9. Way forward</h2>
<p>The maintainers should obtain independent review of the stress derivation and resolve the source-review findings
before selecting design load combinations. Qualification of a design requires a stated standard edition and applicable clauses,
qualified product/material data, load and environment evidence, explicit acceptance factors and independent checks
for every governing failure mode. Unqualified methods remain open; this report does not close their acceptance decisions.</p>
</section>
<section><h2 id="s10">10. References and evidence</h2>
<ul><li id="ref-a">[A] {source_link(EXAMPLE, 'Worked tubular stress invariants')}: Table 1 synthetic assumptions, half-cylinder derivation,
principal-difference derivation and tolerance rationale. Its cited open teaching references are background; they have not been independently retrieved for this report.</li>
<li id="ref-b">[B] {source_link(PACKAGE / 'design_envelope.py', 'Stress helper implementation')}: public methods used for every calculated stress result.</li>
<li id="ref-c">[C] {source_link(TEST, 'Report regression tests')}: independent oracle, mutation controls, byte rebuild, manifest and links.
{source_link(BUILDER, 'Report builder')}: generated figures and rendering.</li>
<li id="ref-d">[D] Package source at the reviewed revision above; citation attributes are inventoried below.
Metadata is a discovery lead, not evidence that the named standard or workshop method has been checked.</li></ul>
<ul>{metadata}</ul>
<p>No licensed tables, catalogue values or default acceptance-factor values are reproduced. Bibliographic names from legacy
metadata are omitted; the linked source at the recorded revision retains the original attributes. No external source was used
to qualify these methods. The independent hand derivation [A] is the numerical basis.</p>
<details><summary>Source files bound by the manifest</summary><ul>{evidence}</ul></details>
<p><a href="../../../../docs/README.md">Documentation index</a> ·
{source_link(PACKAGE / 'README.md', 'Package guide')}</p>
</section>
<details id="changes-panel"><summary>Engineering changes</summary><p>{esc(CHANGES[-1]["change"])}</p>
<p><a href="changes.json" download>Download change history</a></p></details>
<script id="calculation" type="application/json">{escaped_json(data)}</script>
<script id="change-history" type="application/json">{escaped_json(CHANGES)}</script>
</main></body></html>
'''


def readme():
    """Document portable reproduction and the evidence boundary."""
    return '''# Well tubulars module analysis

[Open the report](index.html). The page contains its styling, generated diagram
and calculation data; it needs no remote assets or network connection.

## Rebuild

Use the source package and its normal NumPy and pandas dependencies. From the
repository root, set `PYTHONPATH=src` and run:

```sh
python scripts/reports/build_well_tubulars_module_report.py --scratch "$REPORT_SCRATCH" --output-dir "$REPORT_OUTPUT"
python -m pytest tests/reporting/test_well_tubulars_module_report.py -q -rA -p no:cacheprovider --basetemp="$TEST_SCRATCH"
```

Set the scratch variables to fresh external temporary directories. Output may
be an external directory or this exact delivery directory. Scratch and output
must be disjoint. No intermediate files are written into the source checkout.

## Evidence

- `results.json` records unrounded public-helper results and synthetic inputs.
- `changes.json` matches the revision history directly after the report summary.
- `manifest.json` binds the other delivered files and calculation/review sources
  with SHA-256 digests. It excludes itself to avoid a circular digest.
- The fixed reviewed revision identifies the source review. Current file digests
  identify build inputs; fresh digests do not imply a fresh engineering review.
- Independent force balance and principal-stress differences check the numerical
  results. Rebuilds and production replay alone establish reproducibility only.
- The manifest is an integrity record, not a signature or an acceptance decision.

The example assumptions and rounding budget come from the package's
[worked derivation](../../../../src/digitalmodel/well/tubulars/docs/worked-tubular-stress-invariants.md).
Collapse regimes, catalogue grades, sour-service criteria, rod fatigue and
design acceptance remain `NOT_EVALUATED`. No licensed table values or default
acceptance factors are reproduced. Engineering qualification remains open.
'''


def build(scratch, output_dir):
    """Write deterministic deliverables and keep all intermediates external."""
    scratch = external_scratch(scratch)
    output = Path(output_dir).resolve()
    if (scratch == output or scratch in output.parents or output in scratch.parents):
        raise ValueError("Scratch and output directories must be disjoint")
    if (output == ROOT or ROOT in output.parents) and output != DESTINATION:
        raise ValueError("Output inside the checkout must be the exact report directory")
    data = calculate(scratch)
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "api-results.json").write_text(encode(data), encoding="utf-8", newline="\n")
    output.mkdir(parents=True, exist_ok=True)
    content = {"index.html": render(data), "results.json": encode(data),
               "changes.json": encode(CHANGES), "README.md": readme()}
    for name, value in content.items():
        (output / name).write_text(value, encoding="utf-8", newline="\n")
    manifest = {
        "reviewed_revision": REVIEWED_REVISION,
        "scope": "Thin-wall helper arithmetic only; design acceptance NOT_EVALUATED.",
        "inventory": inventory(),
        "sources": {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
                    for name in source_paths()},
        "outputs": {name: hashlib.sha256((output / name).read_bytes()).hexdigest()
                    for name in content},
        "manifest_note": "Manifest excludes itself; file digests bind bytes, not engineering qualification.",
    }
    (output / "manifest.json").write_text(encode(manifest), encoding="utf-8", newline="\n")


def main():
    """Require explicit locations; provide no implicit checkout output."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
