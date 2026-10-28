# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build an offline subsea module analysis from normal public package imports.

Only the synthetic static case defined below is executed. Geometry follows
direct annulus integration; bending follows linear strain and Hooke's law;
flow uses a stipulated caller coefficient, not an erosional design basis.
All input numbers are illustrative assumptions, never equipment data.
"""

import argparse
import hashlib
import html
import json
import math
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
SCRIPT = "scripts/reports/build_subsea_trees_connectors_module_report.py"
TEST = "tests/reporting/test_subsea_trees_connectors_module_report.py"
DESTINATION = "docs/reports/modules/subsea_trees_connectors"
BRAND = "docs/api/_assets/brand.css"
CHANGES = [{"revision": "A", "date": "2026-10-28", "change":
            "The package interfaces, synthetic static calculation and evidence gaps are presented to bound the available engineering evidence."}]

# Synthetic input basis [H1-H4] in the report: no code-prescribed factors.
INPUTS = {
    "connector": {"outer_diameter_m": 0.2, "wall_thickness_m": 0.05,
                  "youngs_modulus_pa": 200e9, "bend_radius_m": 100.0,
                  "supplied_stress_limit_pa": 400e6},
    "tree": {"diameter_m": 0.2, "density_kg_m3": 1000.0,
             "coefficient_si": 2 * math.sqrt(1000)},
    "handoff": {"production_rate_m3_s": math.pi / 100, "annulus_rate_m3_s": 0.0,
                "upstream_pressure_pa": 5e6, "supplied_drop_pa": 1e6},
}
INPUT_UNITS = {
    "outer_diameter_m": "m", "wall_thickness_m": "m", "youngs_modulus_pa": "Pa",
    "bend_radius_m": "m", "supplied_stress_limit_pa": "Pa", "diameter_m": "m",
    "density_kg_m3": "kg/m³", "coefficient_si": "(m/s)√(kg/m³)",
    "production_rate_m3_s": "m³/s", "annulus_rate_m3_s": "m³/s",
    "upstream_pressure_pa": "Pa", "supplied_drop_pa": "Pa",
}

# Each numerical result is obtained from a public property or return value.
# The expressions are hand-derivation annotations, not a second implementation.
RESULTS = {
    "geometry": {
        "inner_diameter_m": ("Inner diameter", "m", "Dᵢ = D − 2t [H1]"),
        "area_m2": ("Annular area", "m²", "A = ∫ r dr dθ = 3π/400 [H1]"),
        "bore_area_m2": ("Connector bore area", "m²", "Aᵢ = ∫ r dr dθ = π/400 [H1]"),
        "second_moment_m4": ("Second moment", "m⁴", "I = ∫ y² dA = 3π/128000 [H1]"),
        "section_modulus_m3": ("Section modulus", "m³", "Z = I/(D/2) = 3π/12800 [H1]"),
    },
    "bending": {
        "curvature_per_m": ("Curvature", "1/m", "κ = 1/R [H2]"),
        "stress_pa": ("Outer-fibre elastic stress", "Pa", "σ = E(D/2)/R [H2]"),
        "moment_nm": ("Elastic moment", "N·m", "M = ∫ σy dA = EI/R [H2]"),
        "utilization": ("Stress utilization", "1", "u = σ / supplied stress limit [H2]"),
        "within_supplied_limit": ("Supplied stress criterion met", "boolean", "u ≤ unity; conditional arithmetic only [H2]"),
    },
    "tree": {
        "velocity_ceiling_m_s": ("Stipulated velocity ceiling", "m/s", "v = C/√ρ [H3]"),
        "rate_ceiling_m3_s": ("Stipulated rate ceiling", "m³/s", "Q = v∫ dA = π/50 [H3]"),
    },
    "handoff": {
        "production_rate_m3_s": ("Retained production rate", "m³/s", "Qout = Qin = π/100; no clipping [H4]"),
        "annulus_rate_m3_s": ("Retained annulus rate", "m³/s", "Supplied zero; no active annulus [H4]"),
        "downstream_pressure_pa": ("Downstream pressure", "Pa", "pout = pin − supplied Δp [H4]"),
        "within_supplied_ceiling": ("Supplied rate criterion met", "boolean", "Each port rate ≤ its supplied ceiling [H4]"),
    },
}


def inventory():
    """Describe every public callable; inventories are not execution evidence."""
    rows = [
        ("trees/design_checks.py", "select_rated_working_pressure; tree_rating_check; TreeRatingResult",
         "Design pressure and rating (psi); class and well temperatures (°F)",
         "Discrete rating; pressure/temperature flags, margin (psi), utilization and passed",
         "INSPECTED ONLY. Embedded rating tables and qualification claims are not independently established; no tables are reproduced."),
        ("trees/flow_capacity.py", "bore_capacity; BoreCapacity",
         "Bore diameter (m), density (kg/m³), supplied C ((m/s)√(kg/m³))",
         "Velocity ceiling (m/s), rate ceiling (m³/s)",
         "EXECUTED. Conditional ceiling arithmetic; no erosion, multiphase or coefficient qualification."),
        ("trees/flow_capacity.py", "flow_handoff; FlowHandoff",
         "Separate production/annulus rates (m³/s), capacities, upstream pressure and supplied loss (Pa)",
         "Unclipped separate rates (m³/s), downstream pressure (Pa), within_capacity flag",
         "EXECUTED. Annulus capacity is absent only for zero annulus flow. Loss is supplied, not predicted."),
        ("trees/pressure_control.py", "liquid_critical_drop; trim_cv; valve_pressure_drop; series_pressure_drop; liquid_choke_flow; ChokeFlow",
         "Absolute pressure/drop (psi), rate (gpm (US)), effective Cv, specific gravity, travel, trim, rangeability, recovery/fluid factors",
         "Critical drop, valve/series loss (psi), effective Cv, liquid rate (gpm (US)), capped drop (psi) and regime",
         "INSPECTED ONLY. Liquid-only no-attached-fittings screen; factors require qualification. No gas, flashing, viscosity correction or installed-network solution."),
        ("trees/tree_catalog.py", "TreeEntry; TreeCatalog.get; TreeCatalog.select",
         "Caller HTTPS source, key, topology/orientation, bores/envelope (m), rating (Pa), range (°C), masses (kg), valve count",
         "Immutable caller entries; lookup by key; selection by orientation/minimum bore and pressure",
         "INSPECTED ONLY. No seeded equipment records; source URLs are validated syntactically, not independently qualified."),
        ("connectors/section_properties.py", "PipeSection.inner_diameter; PipeSection.area; PipeSection.bore_area; PipeSection.moment_of_inertia; PipeSection.section_modulus",
         "Outer diameter and wall thickness (m)",
         "Inner diameter (m), annular/bore areas (m²), second moment (m⁴), elastic modulus of section (m³)",
         "EXECUTED. Homogeneous circular geometry; not flexible-layer properties."),
        ("connectors/bending_analysis.py", "bending_from_radius; BendingResult",
         "PipeSection, positive radius (m), elastic modulus and explicit stress limit (Pa)",
         "Curvature (1/m), elastic stress (Pa), moment (N·m), utilization and passed",
         "EXECUTED. Linear elastic curvature model; default unbounded limit is not used. No contact, ovalization or flexible-layer response."),
        ("connectors/connector_design.py", "end_cap_force; verify_connector; ConnectorType; ConnectorMaterial; ConnectorCheckResult.utilisation",
         "Section, material strength/modulus (Pa), pressure (Pa), axial/preload/seating forces (N), moment (N·m), seal diameter (m), usage factor and make-up type",
         "Pressure thrust, separating/residual force (N), stress/allowable (Pa), body/seal utilization, governing check, notes and passed",
         "INSPECTED ONLY. Thin-wall hoop approximation, signed bending treatment, default factors and seal policy need engineering review; no acceptance basis is selected here."),
        ("connectors/thermal_expansion.py", "thermal_expansion_load; ThermalResult",
         "Section, heated length (m), temperature change (K or °C difference), expansion coefficient (1/K), modulus (Pa), restraint fraction",
         "Thermal strain, free growth (m), fully restrained stress (Pa), fraction-scaled axial force (N)",
         "INSPECTED ONLY. Restrained-bar model; geometry stiffness and restraint fraction are supplied assumptions, not solved reactions."),
        ("connectors/jumper_catalog.py", "get_jumper_spec; JumperType; JumperSpec; JUMPER_CATALOG",
         "Rigid, flexible or vertical family enum",
         "Family description, governing-check tags and dynamic flag",
         "INSPECTED ONLY. Routing metadata; not a sized or qualified product catalogue."),
        ("connectors/fatigue.py", "assess_jumper_fatigue; JumperFatigueResult",
         "Eligible specification, homogeneous section, signed radius history (m), modulus (Pa), exposure (years), fatigue factor, SCF, curve/environment",
         "Converted stress history (MPa) and native fatigue assessment",
         "NOT_EVALUATED. Normal package import reaches the fatigue dependency stack. No histories, qualified curve, environment or hot-spot basis are supplied."),
        ("connectors/orcaflex_export.py", "build_jumper_line_type",
         "Dynamic specification, section, name, dry mass/length (kg/m), effective axial stiffness (N), bending/torsional stiffness (N·m²)",
         "Offline generic structural fragment (m, tonnes/m, kN, kN·m²)",
         "INSPECTED ONLY. No solver is imported or invoked by this mapping. Full model, connections, hydrodynamics and dynamic response remain absent."),
    ]
    return [dict(source="src/digitalmodel/subsea/" + path, interface=interface,
                 inputs=inputs, outputs=outputs, limits=limits)
            for path, interface, inputs, outputs, limits in rows]


def external_scratch(scratch):
    """Resolve the destination before permitting intermediate-file writes."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    return scratch


def calculate(scratch):
    """Execute only the specified static examples using normal public imports."""
    external_scratch(scratch)
    from digitalmodel.subsea import connectors, trees

    c, t, h = INPUTS["connector"], INPUTS["tree"], INPUTS["handoff"]
    section = connectors.PipeSection(c["outer_diameter_m"], c["wall_thickness_m"])
    bend = connectors.bending_from_radius(section, c["bend_radius_m"],
                                          youngs_modulus=c["youngs_modulus_pa"],
                                          allowable_stress=c["supplied_stress_limit_pa"])
    bore = trees.bore_capacity(t["diameter_m"], t["density_kg_m3"], t["coefficient_si"])
    flow = trees.flow_handoff(h["production_rate_m3_s"], h["annulus_rate_m3_s"],
                              bore, None, h["upstream_pressure_pa"], h["supplied_drop_pa"])
    return {"schema_version": "1", "status": "SYNTHETIC_STATIC_ONLY", "inputs": INPUTS,
            "inventory": inventory(), "outputs": {
                "geometry": {"inner_diameter_m": section.inner_diameter,
                             "area_m2": section.area, "bore_area_m2": section.bore_area,
                             "second_moment_m4": section.moment_of_inertia,
                             "section_modulus_m3": section.section_modulus},
                "bending": {"curvature_per_m": bend.curvature, "stress_pa": bend.bending_stress,
                            "moment_nm": bend.bending_moment, "utilization": bend.utilisation,
                            "within_supplied_limit": bend.passed},
                "tree": {"velocity_ceiling_m_s": bore.velocity_m_s, "rate_ceiling_m3_s": bore.rate_m3_s},
                "handoff": {"production_rate_m3_s": flow.production_rate_m3_s,
                            "annulus_rate_m3_s": flow.annulus_rate_m3_s,
                            "downstream_pressure_pa": flow.downstream_pressure_pa,
                            "within_supplied_ceiling": flow.within_capacity}}}


def json_text(value):
    """Use stable UTF-8/LF text without timestamps or environment state."""
    return json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2, allow_nan=False) + "\n"


def embedded_json(value):
    """Prevent HTML parser termination while retaining exact JSON readback."""
    return json_text(value).replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")


def format_value(value, *, thickness=False):
    """Round visible values only; retain full binary64 values in results.json."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if thickness:
        return f"{value:.3f}"
    return f"{value:.6g}"


def source_evidence():
    """Bind package, builder, test and canonical style bytes separately."""
    packages = ROOT / "src/digitalmodel/subsea"
    paths = sorted(path.relative_to(ROOT).as_posix()
                   for name in ("trees", "connectors") for path in (packages / name).glob("*.py"))
    paths.extend([SCRIPT, TEST, BRAND])
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in paths}


def source_link(path):
    """Link inspected package bytes to the exact reviewed source revision."""
    return f"https://github.com/vamseeachanta/digitalmodel/blob/{REVIEWED_REVISION}/{path}"


def input_rows(data):
    rows = []
    for group, values in data["inputs"].items():
        basis = {"connector": "H1, H2", "tree": "H3", "handoff": "H4"}[group]
        for key, value in values.items():
            cell = format_value(value, thickness=key == "wall_thickness_m")
            rows.append(f'<tr><td>{group}.{key}</td><td data-input="{group}.{key}">{cell}</td>'
                        f'<td>{INPUT_UNITS[key]}</td><td>Synthetic assumption [{basis}]</td></tr>')
    return "".join(rows)


def result_rows(data):
    rows = []
    for group, values in data["outputs"].items():
        for key, value in values.items():
            label, unit, basis = RESULTS[group][key]
            rows.append(f'<tr><td>{html.escape(label)}</td><td data-result="{group}.{key}">'
                        f'{format_value(value)}</td><td>{html.escape(unit)}</td>'
                        f'<td>{html.escape(basis)}</td></tr>')
    return "".join(rows)


def render(data, sources):
    """Compose the ten-section report around API results and hand derivations."""
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / BRAND).read_text(encoding="utf-8"), flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    history = "".join("<tr>" + "".join(f"<td>{html.escape(row[key])}</td>"
                                     for key in ("revision", "date", "change")) + "</tr>"
                      for row in CHANGES)
    interfaces = "".join(
        f'<tr><td><a href="{source_link(row["source"])}">{html.escape(row["source"])}</a>'
        f'<br><code>{html.escape(row["interface"])}</code></td><td>{html.escape(row["inputs"])}</td>'
        f'<td>{html.escape(row["outputs"])}</td><td>{html.escape(row["limits"])}</td></tr>'
        for row in data["inventory"])
    digests = "".join(f'<li><code>{html.escape(path)}</code>: <code>{digest}</code></li>'
                      for path, digest in sources.items())
    b, h, t = data["outputs"]["bending"], data["outputs"]["handoff"], data["outputs"]["tree"]
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Subsea trees and connectors module analysis</title>
<style>{brand}
.wrap{{max-width:1100px;margin:auto;padding:2rem}}header,.panel{{background:var(--panel);padding:1.5rem;border:1px solid var(--edge);border-radius:var(--r)}}
h1,h2,h3{{color:var(--brand)}}h2{{margin-top:2.4rem}}a{{color:var(--brand)}}
table{{width:100%;border-collapse:collapse;background:var(--panel);font-size:.9rem}}th,td{{text-align:left;vertical-align:top;padding:.65rem;border:1px solid var(--edge)}}
th{{background:var(--panel-2)}}.scroll{{overflow-x:auto}}code{{overflow-wrap:anywhere}}.eq{{padding:1rem;background:var(--panel-2);border-left:3px solid var(--accent)}}
svg{{display:block;width:100%;max-width:850px;margin:auto;color:var(--ink)}}svg text{{font:16px sans-serif;fill:currentColor}}figcaption,.caption{{color:var(--muted);margin:.5rem 0 1rem}}
@media print{{.wrap{{padding:0}}h2{{break-after:avoid}}tr,figure{{break-inside:avoid}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><p>Draft for maintainer review</p><h1>Subsea trees and connectors module analysis</h1>
<p>Static arithmetic evidence and a bounded inventory of the public package interfaces.</p>
<nav><a href="#s3">Inputs</a> · <a href="#s5">Worked calculation</a> · <a href="#s8">Evidence gaps</a> · <a href="#s10">Sources</a></nav></header>
<h2 id="s1">1. Summary</h2>
<p>The synthetic connector outer-fibre stress is {format_value(b['stress_pa'])} Pa against a caller-supplied limit of {format_value(data['inputs']['connector']['supplied_stress_limit_pa'])} Pa; utilization is {format_value(b['utilization'])} [H2]. The returned stress criterion flag is {format_value(b['within_supplied_limit'])}. This establishes conditional linear-elastic arithmetic, not equipment suitability.</p>
<p>The stipulated tree rate ceiling is {format_value(t['rate_ceiling_m3_s'])} m³/s [H3]. The retained production rate is {format_value(h['production_rate_m3_s'])} m³/s and downstream pressure is {format_value(h['downstream_pressure_pa'])} Pa for the supplied loss [H4]. These quantities do not establish an operational envelope.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>Change and cause</th></tr></thead><tbody>{history}</tbody></table>
<p class="caption">Revision history. Initial publication of the bounded analysis; the same record is retained in changes.json.</p>
<h2 id="s2">2. Context and scope</h2>
<p>The trees package exposes rating-envelope, catalogue, liquid pressure-control and separate-port flow functions. The connectors package exposes circular section properties, connector load screening, thermal growth, elastic curvature, family metadata, fatigue assessment and offline structural export. The inventory below distinguishes executed helpers from source inspection.</p>
<p>The connection boundary is caller-managed. Tree handoff supplies separate rates and pressure; connector mechanics accepts geometry, loads and material properties. No public function automatically maps those ports into connector loads, temperatures or a solved network. The synthetic tree bore and connector bore are different, independent arithmetic fixtures; no physical adapter sizing or connected installation is implied.</p>
<figure><svg viewBox="0 0 850 230" role="img" aria-label="Separate tree flow and connector mechanics boundaries">
<g fill="none" stroke="currentColor"><rect x="20" y="25" width="340" height="155" rx="10"/><rect x="490" y="25" width="340" height="155" rx="10"/><path d="M360 100H480 M465 90L480 100L465 110" stroke-dasharray="6 4"/></g>
<text x="40" y="55">Tree public API</text><text x="40" y="90">Bore ceiling → separate-port handoff</text><text x="40" y="125">Rates and pressure are retained data</text>
<text x="510" y="55">Connector public API</text><text x="510" y="90">Section → supplied bend radius</text><text x="510" y="125">Elastic stress, moment, utilization</text>
<text x="225" y="215">Dashed boundary: mapping and network coupling are NOT_EVALUATED</text>
</svg><figcaption>Figure A. Builder-generated interface schematic; no physical topology or scale is inferred.</figcaption></figure>
<div class="scroll"><table><thead><tr><th>Source and interface</th><th>Inputs and units</th><th>Outputs and units</th><th>Evidence and limits</th></tr></thead><tbody>{interfaces}</tbody></table></div>
<p class="caption">Table A. Public function, class and property inventory at the reviewed source revision. Constructor validation methods are implementation details.</p>
<h2 id="s3">3. Input basis</h2>
<p>All calculation inputs below are synthetic assumptions selected for exact hand calculation [H1–H4]. They are not measured, catalogue, field or standards-derived data. The stress limit and flow coefficient are caller-supplied assumptions. No independent design basis is supplied for either.</p>
<div class="scroll"><table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead><tbody>{input_rows(data)}</tbody></table></div>
<p class="caption">Table B. Inputs passed to the public APIs. Display values are rounded to six significant figures, except wall thickness shown to three decimals; results.json retains unrounded binary64 values. The coefficient is exactly specified as C = 2√1000 in the stated SI units, and production rate as π/100 m³/s.</p>
<p>Rating functions use psi and °F; pressure-control functions use gallons per minute (US) and psi; the tree registry uses Pa and °C. The executed examples use only SI values. No conversion between those interfaces is assumed.</p>
<h2 id="s4">4. Method</h2>
<p>[H1] With outer radius a = D/2 and inner radius b = a − t, polar area is dA = r dr dθ. Integration over the annulus gives A = π(a² − b²). The bending second moment is ∫(r sin θ)² r dr dθ = π(a⁴ − b⁴)/4, since ∫sin²θ dθ over a circle is π. The elastic section modulus is Z = I/a. All factors follow these integrations; no standard table is used.</p>
<p class="eq">[H2] κ = 1/R; ε(y) = y/R; σ(y) = Eε(y); σouter = Ea/R; M = ∫σy dA = EI/R; u = σouter/σlimit.</p>
<p>The section remains homogeneous and linearly elastic, with an explicitly supplied radius and stress limit. The comparison u ≤ unity concerns only the supplied scalar limit; yield, stability, seal capacity, external pressure and load combinations are not assessed.</p>
<p class="eq">[H3] vceiling = C/√ρ; Abore = ∫ dA = πD²/4; Qceiling = Abore vceiling.</p>
<p>The ceiling is stipulated arithmetic, not erosional qualification. The SI coefficient has dimensions (m/s)√(kg/m³). Coefficient selection, sand, corrosion, multiphase slip and pressure-loss prediction are outside the evidence.</p>
<p class="eq">[H4] Qproduction,out = Qproduction,in; Qannulus,out = Qannulus,in; pout = pin − Δpsupplied.</p>
<p>Each rate is compared with its port ceiling without clipping or mixing. With zero annulus rate no annulus capacity is supplied. The supplied pressure drop is a boundary input, not a network solution.</p>
<h2 id="s5">5. Worked calculation</h2>
<p>[H1] The independent hand calculation uses a = 1/10 m and b = 1/20 m. The annular area is π[(1/10)² − (1/20)²] = 3π/400 m²; the bore area is π/400 m². Integration gives I = π[(1/10)⁴ − (1/20)⁴]/4 = 3π/128000 m⁴ and Z = I/(1/10) = 3π/12800 m³.</p>
<p>[H2] The outer-fibre strain is (1/10)/100 = 1/1000. Multiplication by the supplied E gives σ = 200e6 Pa. The moment is (200e9 Pa)(3π/128000 m⁴)/(100 m), and the ratio to the supplied 400e6 Pa limit is one half. Curvature is 1/(100 m).</p>
<p>[H3] For the independent tree bore, C/√ρ = (2√1000)/√1000 = 2 m/s. Integrating the circular bore gives π/100 m², so the rate ceiling is π/50 m³/s. [H4] The incoming π/100 m³/s is retained, annulus rate is zero, and (5e6 − 1e6) Pa = 4e6 Pa is passed downstream. Unrounded π is used throughout.</p>
<h2 id="s6">6. Checks and evidence criterion</h2>
<p>The dedicated report test compares normal public API outputs and delivered values with the independent integrations and balances above. Positive results use relative tolerance 1e-12 and zero absolute tolerance, allowing binary64 arithmetic roundoff only. Zeros and booleans are checked exactly. This numerical criterion is not a physical modelling tolerance or design acceptance criterion.</p>
<p>The negative controls double an area or rate and reverse a stress or pressure sign while regenerating mutually consistent HTML, JSON and digests. The independent numerical assertion rejects those bundles. A separate result-byte edit is rejected by digest readback. Agreement between builder and report alone is insufficient.</p>
<p>Two fresh external rebuilds are compared byte for byte with each other and the delivered HTML, results, history, manifest and README. These checks establish deterministic delivery and freshness, separately from the numerical oracle. The manifest binds every other delivered file; it cannot contain its own digest. Source digests bind the inspected package bytes, builder, test and house style.</p>
<p>The normal connectors import reaches the fatigue stack, including pylife, even for static helpers. Execution uses an x86 runtime with the actual dependencies; package initialization is not bypassed. No solver is started, imported or queried by the report calculation.</p>
<h2 id="s7">7. Results</h2>
<div class="scroll"><table><thead><tr><th>Public API result</th><th>Value</th><th>Unit</th><th>Independent basis</th></tr></thead><tbody>{result_rows(data)}</tbody></table></div>
<p class="caption">Table C. Returned values for the synthetic static case; each result is taken directly from a public method or property. Boolean results refer to the supplied scalar criteria only.</p>
<h2 id="s8">8. Limits and NOT_EVALUATED</h2>
<table><thead><tr><th>Evidence gap</th><th>Status and missing basis</th><th>Required next evidence</th></tr></thead><tbody>
<tr><td>Fatigue assessment</td><td>NOT_EVALUATED. No load history, qualified curve/environment, exposure or hot-spot basis is supplied.</td><td>Supply those inputs and independently assess stress conversion, cycle counting and damage.</td></tr>
<tr><td>Dynamic response</td><td>NOT_EVALUATED. Offline export is a structural fragment; no complete topology, hydrodynamics, damping or response result is supplied.</td><td>Assemble and review the complete model and an independent response benchmark.</td></tr>
<tr><td>Network coupling</td><td>NOT_EVALUATED. Port handoff retains data but solves no pressure-flow or thermomechanical boundary problem.</td><td>Define interface units, geometry, losses, continuity and pressure/load mapping.</td></tr>
<tr><td>Standards compliance</td><td>NOT_EVALUATED. Applicable editions, clauses, load combinations and acceptance basis are not established.</td><td>Establish the governing design basis and qualify equations, factors and rating data before design use.</td></tr>
</tbody></table><p class="caption">Table D. Separate evidence gaps; none is resolved by the static arithmetic example.</p>
<p>Open source findings: the combined connector check uses a thin-wall hoop approximation and signed bending contribution with caller/default policy factors. The synthetic thick annulus is used only for exact geometry and pure bending, never for that pressure model. Limiting-fibre stress envelopes, nonfinite-input handling and allowable-stress domain validation require separate assessment. Defaults and embedded rating-table claims are not independently qualified. No arithmetic correction or algorithm replacement is made by this report.</p>
<h2 id="s9">9. Way forward</h2>
<p>The synthetic static arithmetic can be reproduced using the command in the accompanying README. Physical use remains conditional on qualified material, geometry, flow-coefficient and load data, plus an explicit acceptance basis. The rating, pressure-control, thermal and combined-stress interfaces should receive independent worked cases before their output is used for design. The separate fatigue, dynamic, network and standards gaps above should remain open.</p>
<h2 id="s10">10. References and evidence</h2>
<p>Reviewed package source revision: <code>{REVIEWED_REVISION}</code>. The inventory links target this exact revision. The package bytes and report-generation evidence are recorded below; the report is not a qualification of later revisions.</p>
<ul><li>[H1] Direct polar integration of circular/annular area and second moment, derived in the Method and Worked calculation sections.</li>
<li>[H2] Linear strain and Hooke's law, integrated over the section as derived above; stress limit is a synthetic caller input.</li>
<li>[H3] Stipulated coefficient relation and direct bore-area integration, derived above; no erosional coefficient or standard limit is adopted.</li>
<li>[H4] Separate-port continuity and subtraction of the supplied pressure loss, derived above.</li>
<li>Pressure-control source inventory: Control Valve Handbook, sixth edition (2023), sections Control Valve Functions and Characteristics Terminology and Sizing Valves for Liquids; Control Valve Sourcebook — Oil &amp; Gas (2013), Determining Allowable Sizing Pressure Drop. These titles identify the source code's citations; their numerical criteria are not used or reproduced in this report.</li>
<li><a href="https://github.com/vamseeachanta/digitalmodel/issues/3342">Module-report scope and outstanding qualification work</a>.</li></ul>
<details><summary>SHA-256 source evidence</summary><ul>{digests}</ul></details>
<p><a href="results.json">Unrounded calculation data</a> · <a href="changes.json">Revision history</a> · <a href="manifest.json">Delivery manifest</a> · <a href="README.md">Rebuild command</a></p>
<script id="calculation" type="application/json">{embedded_json(data)}</script>
<script id="changes" type="application/json">{embedded_json(CHANGES)}</script>
</main></body></html>
'''


def readme():
    """Keep regeneration instructions portable and explicit about the boundary."""
    return '''# Subsea trees and connectors module analysis

Open [index.html](index.html) for the self-contained analysis. The example uses
synthetic inputs and normal public package imports; it establishes static
arithmetic only. Fatigue assessment, dynamic response, network coupling and
standards compliance remain NOT_EVALUATED.

From the repository root, use the existing x86 Python environment with the
package calculation dependencies, including pylife. Set `TMPDIR` to external
scratch and run:

```sh
PYTHONPATH=src uv run python scripts/reports/build_subsea_trees_connectors_module_report.py \\
  --scratch "$TMPDIR/subsea-report-scratch" \\
  --output-dir docs/reports/modules/subsea_trees_connectors
PYTHONPATH=src uv run python -m pytest \\
  tests/reporting/test_subsea_trees_connectors_module_report.py \\
  --basetemp="$TMPDIR/subsea-report-tests" -p no:cacheprovider
```

The command requires explicit scratch and output destinations. Intermediates
are permitted only outside the checkout; only the five final deliverables are
written into the output directory. Text uses UTF-8 and LF. Inputs and unrounded
API outputs are retained in [results.json](results.json), revision history in
[changes.json](changes.json), and source/delivery digests in
[manifest.json](manifest.json). The manifest hashes every other delivered file;
its own bytes are covered by the exact rebuild test.

Two independent rebuild directories are compared byte for byte with each other
and this delivery. Separate hand-integrated annulus, linear-strain and flow
balance oracles check numerical meaning. Negative controls reject consistently
regenerated wrong values and sign reversals. Rebuild equality alone establishes
freshness, not numerical or physical validity.

All methods and assumptions are traced in the report. A changed package source
requires renewed source review and a revised source pin; regeneration alone
does not qualify a changed engineering method.
'''


def build(scratch, output_dir):
    """Stage all files externally, then write only the named deliverables."""
    scratch = external_scratch(scratch)
    output_dir = Path(output_dir).resolve()
    if scratch == output_dir or scratch in output_dir.parents or output_dir in scratch.parents:
        raise ValueError("Scratch and output directories must be separate")
    scratch.mkdir(parents=True, exist_ok=True)
    data = calculate(scratch)
    sources = source_evidence()
    files = {"index.html": render(data, sources), "results.json": json_text(data),
             "changes.json": json_text(CHANGES), "README.md": readme()}
    manifest = {"schema_version": "1", "reviewed_source_revision": REVIEWED_REVISION,
                "case_status": data["status"], "sources": sources,
                "files": {name: hashlib.sha256(text.encode("utf-8")).hexdigest()
                          for name, text in files.items()}}
    files["manifest.json"] = json_text(manifest)
    for name, text in files.items():
        (scratch / name).write_text(text, encoding="utf-8", newline="\n")
    output_dir.mkdir(parents=True, exist_ok=True)
    for name in files:
        (output_dir / name).write_bytes((scratch / name).read_bytes())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
