# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the standalone ANSYS module analysis from public repository data.

From the repository root, with the package on the import path:
    PYTHONPATH=src python docs/reports/modules/ansys/build_report.py --output-dir OUTPUT

No solver, licence utility, network request or external plotting library is used.
"""

import argparse
import hashlib
import json
import re
from decimal import Decimal, localcontext
from html import escape
from pathlib import Path

from digitalmodel.ansys.cylinder_benchmark import build_case, frozen_basis
from digitalmodel.ansys.cylinder_reference import reference_text

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
EXAMPLE = ROOT / "examples/ansys/cylinder-benchmark"
DATE = "2026-10-22"
REVISION_DATE = "2026-10-25"
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
LABELS = {
    "sigma_r": ("Radial stress", "MPa"),
    "sigma_theta": ("Hoop stress", "MPa"),
    "sigma_z": ("Axial stress", "MPa"),
    "tau_rz": ("Radial–axial shear stress", "MPa"),
    "sigma_vm": ("Equivalent stress", "MPa"),
    "u_r": ("Radial displacement", "mm"),
    "u_z": ("Axial displacement", "mm"),
}
CHANGES = {
    "report": "ANSYS module analysis", "revision": "2", "date": REVISION_DATE,
    "changes": [
        {"section": "section-3", "change": "The fixed open-cylinder basis and its use restrictions are stated."},
        {"section": "section-5", "change": "Analytical stresses, displacements and prepared mesh sizes are presented."},
        {"section": "section-8", "change": "Design inputs and native evidence needed for engineering acceptance are identified."},
        {"section": "appendix-b", "change": "Comment imports reject conflicting identities; save and export enforce reload limits."},
    ],
}


def compute():
    """Call public module functions on the retained cylinder example."""
    manifest = json.loads((EXAMPLE / "manifest.json").read_text(encoding="utf-8"))
    retained = json.loads((EXAMPLE / "reference.json").read_text(encoding="utf-8"))
    basis = frozen_basis()
    meshes = []
    for case_id in manifest["case_order"]:
        case = build_case(case_id)
        meshes.append({"case_id": case_id, "pressure_mpa": case["pressure_mpa"],
                       "radial_divisions": case["radial_divisions"],
                       "axial_divisions": case["axial_divisions"],
                       "nodes": len(case["nodes"]), "elements": len(case["elements"])})
    pressure = meshes[-1]["pressure_mpa"]
    stations = []
    for row in retained["rows"]:
        values = reference_text(row["pressure_mpa"], row["r_mm"], row["y_mm"])
        if values != row["values"]:
            raise ValueError("Retained analytical reference differs from current module")
        if row["pressure_mpa"] == pressure:
            stations.append({**row, "values": values})
    middle = sorted({s["y_mm"] for s in stations}, key=Decimal)
    y = middle[len(middle) // 2]
    coarse = build_case(manifest["case_order"][1])
    profile = [{"r_mm": r, "values": reference_text(pressure, r, y)}
               for r in sorted({n["x_mm"] for n in coarse["nodes"]}, key=Decimal)]
    maximum_difference = Decimal(0)
    for row in stations:
        original = next(r for r in retained["rows"] if r["station_id"] == row["station_id"]
                        and r["pressure_mpa"] == pressure)
        for key, value in row["values"].items():
            target = Decimal(original["values"][key])
            if target:
                maximum_difference = max(maximum_difference, abs((Decimal(value) - target) / target) * 100)
    return {"basis": basis, "pressure_mpa": pressure, "y_mm": y,
            "meshes": meshes, "stations": stations, "profile": profile,
            "zero_control": reference_text(meshes[0]["pressure_mpa"], profile[0]["r_mm"], y),
            "max_difference_pct": str(maximum_difference)}


def numeric_values(data):
    """Register all engineering numbers rendered in the worked example."""
    values = {f"basis.{k}": v for k, v in data["basis"].items()
              if k in ("inner_radius_mm", "wall_thickness_mm", "axial_length_mm",
                       "youngs_modulus_mpa", "poisson_ratio", "external_pressure_mpa", "delta_temperature_C")}
    values.update({"pressure": data["pressure_mpa"], "axial_station": data["y_mm"],
                   "difference_pct": data["max_difference_pct"]})
    for i, row in enumerate(data["stations"]):
        values.update({f"station.{i}.{k}": v for k, v in row["values"].items()})
        values.update({f"station.{i}.{k}": row[k] for k in ("r_mm", "y_mm")})
    for i, row in enumerate(data["profile"]):
        values[f"profile.{i}.r_mm"] = row["r_mm"]
        for k in ("sigma_r", "sigma_vm"):
            values[f"profile.{i}.{k}"] = row["values"][k]
    for i, row in enumerate(data["meshes"]):
        for k in ("pressure_mpa", "radial_divisions", "axial_divisions", "nodes", "elements"):
            values[f"mesh.{i}.{k}"] = str(row[k])
    for k, v in data["zero_control"].items():
        values[f"control.{k}"] = v
    return values


def format_value(value, key):
    """Use engineering precision, a true minus, and no rounded nonzero zero."""
    places = 4 if key.endswith(("u_r", "u_z")) else 1
    if key.endswith(("nodes", "elements", "divisions")):
        places = 0
    number = Decimal(value)
    with localcontext() as context:
        context.prec = 60
        text = f"{number:.{places}f}"
    if number and Decimal(text) == 0:
        text = f"{number:.1E}"
    if not number:
        text = f"{abs(number):.{places}f}"
    return text.replace("-", "−")


def style():
    """Inline the existing house report stylesheet and shared brand tokens."""
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    local = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    return (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8") + "\n" + local


def render(data):
    """Write the complete report text with values inserted only from the register."""
    numbers = numeric_values(data)

    def n(key):
        return f'<span data-number="{key}">{format_value(numbers[key], key)}</span>'

    def cite(name):
        return f'<a href="#ref-{name}">[Source {name}]</a>'

    def table(headers, rows, caption):
        return ('<div style="overflow-x:auto"><table><caption>' + caption + '</caption><thead><tr>' +
                ''.join(f'<th scope="col">{x}</th>' for x in headers) +
                '</tr></thead><tbody>' + ''.join('<tr>' + ''.join(f'<td>{x}</td>' for x in row) +
                '</tr>' for row in rows) + '</tbody></table></div>')

    basis_rows = []
    for k, label, unit in (
        ("inner_radius_mm", "Inner radius a", "mm"),
        ("wall_thickness_mm", "Wall thickness t", "mm"),
        ("axial_length_mm", "Axial length L", "mm"),
        ("youngs_modulus_mpa", "Young’s modulus E", "MPa"),
        ("poisson_ratio", "Poisson’s ratio ν", "dimensionless"),
        ("external_pressure_mpa", "External pressure", "MPa"),
        ("delta_temperature_C", "Temperature change", "°C"),
    ):
        basis_rows.append([label, n("basis." + k), unit, cite("B")])
    basis_rows.append(["Internal pressure p", n("pressure"), "MPa", cite("B")])
    input_table = table(["Input", "Value", "Unit", "Source"], basis_rows, "Table A. Example input register.")
    mesh_table = table(["Load", "Radial divisions", "Axial divisions", "Nodes", "Elements", "Source"],
        [[n(f"mesh.{i}.pressure_mpa") + " MPa"] +
         [n(f"mesh.{i}.{k}") for k in ("radial_divisions", "axial_divisions", "nodes", "elements")] +
         [cite("B")] for i, row in enumerate(data["meshes"])], "Table B. Prepared meshes; no solution is inferred.")
    station_table = table(["Radius (mm)", "Axial position (mm)"] +
        [label + " (" + unit + ")" for label, unit in LABELS.values()] + ["Source"],
        [[n(f"station.{i}.r_mm"), n(f"station.{i}.y_mm")] +
         [n(f"station.{i}.{k}") for k in LABELS] + [cite("C")]
         for i, row in enumerate(data["stations"])], "Table C. All retained analytical sample positions.")
    control_table = table(["Response", "Value", "Unit", "Source"],
        [[label, n("control." + k), unit, cite("C")] for k, (label, unit) in LABELS.items()],
        "Table D. Zero-pressure analytical control.")
    primary = next(i for i, row in enumerate(data["stations"])
                   if row["r_mm"] == data["profile"][0]["r_mm"] and row["y_mm"] == data["y_mm"])
    q = lambda key: n(f"station.{primary}.{key}")
    schematic = schematic_svg(data, n)
    charts = chart(data, "sigma_r", n, cite) + chart(data, "sigma_vm", n, cite)
    reference_rows = [
        ["A", "Applied Mechanics of Solids, online text, unnumbered web edition, sections 4.1.7–4.1.9; accessed " + DATE +
         '. <a href="https://solidmechanics.org/Text/Chapter4_1/Chapter4_1.htm">Axisymmetric elasticity</a>. '
         "Radial equilibrium, boundary tractions, Hooke law and the axial force condition are used; the stated idealization is retained."],
        ["B", '<a href="../../../../examples/ansys/cylinder-benchmark/prepared/basis-criteria.json">Public cylinder basis and numerical criteria</a>; '
         '<a href="../../../../examples/ansys/cylinder-benchmark/manifest.json">example manifest</a>; '
         '<a href="../../../../src/digitalmodel/ansys/cylinder_benchmark.py">mesh preparation functions</a>. '
         "The repository edition accompanying this report supplies demonstration inputs, mesh divisions and element definitions; material allowables are absent."],
        ["C", '<a href="../../../../examples/ansys/cylinder-benchmark/reference.json">Retained analytical samples</a>; '
         '<a href="../../../../examples/ansys/cylinder-benchmark/producer_derivation.json">equation derivation</a>; '
         '<a href="../../../../src/digitalmodel/ansys/cylinder_reference.py">reference calculation functions</a>. '
         "The repository edition accompanying this report supplies the open-end elastic solution and equivalent-stress identity. The retained record explicitly excludes native results and engineering qualification."],
        ["D", '<a href="../../../../src/digitalmodel/ansys/docs/agent-pack.md">Module knowledge pack, revision 2</a>; '
         '<a href="../../../../docs/domains/ansys/analysis-workflow.html">analysis workflow, revision 1</a>. '
         "The interface inventory and capability restrictions are used; historical completion claims are not adopted."],
        ["E", '<a href="../../../../tests/ansys/test_cylinder_reference.py">Analytical identity tests</a>; '
         '<a href="../../../../tests/ansys/test_cylinder_benchmark.py">mesh tests</a>; '
         '<a href="../../../../tests/ansys/test_example_goldens.py">retained-result tests</a>; '
         '<a href="../../../../tests/ansys/test_golden_acceptance.py">evidence tamper tests</a>; '
         '<a href="../../../../tests/ansys/test_module_report.py">report rebuild tests</a>. '
         "These are software checks. Live solution and component acceptance are separate evidence requirements."],
    ]
    refs = ''.join(f'<li id="ref-{r[0]}">{r[1]}</li>' for r in reference_rows)
    content = f'''<header><div class="kicker">Finite element analysis</div>
<h1>ANSYS module analysis</h1><p class="sub">Draft for maintainer review</p></header>
<nav aria-label="Report sections">{' · '.join(f'<a href="#section-{index}">{index}</a>' for index in range(1, 11))}</nav>
<h2 id="section-1">1. Executive Summary</h2>
<p>The ANSYS module is a Python interface for preparing structural analysis inputs, reading result records and calculating supporting engineering quantities. Analysis is performed to determine the response of an open-ended elastic cylinder and the extent of the available software evidence; key results are summarized below.</p>
<ul><li>The analytical bore response is available for comparison with a future numerical solution.
<ul><li>The bore equivalent stress is {q('sigma_vm')} MPa and radial displacement is {q('u_r')} mm under {n('pressure')} MPa internal pressure. A design allowable is absent and strength acceptance remains NOT_EVALUATED. {cite('B')} {cite('C')}</li></ul></li>
<li>The prepared meshes are available for a controlled numerical comparison.
<ul><li>Mesh preparation and analytical evaluation are demonstrated. Mesh convergence, native execution and component design acceptance remain NOT_EVALUATED in this analysis. {cite('B')} {cite('E')}</li></ul></li></ul>
<h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead><tbody>
<tr><td>1</td><td>{DATE}</td><td>Initial module analysis; the reproducible analytical example and evidence restrictions are presented.</td></tr>
<tr><td>2</td><td>{REVISION_DATE}</td><td>Conflicting comment imports and oversized save or export payloads are rejected to preserve records and reloadability.</td></tr></tbody></table>
<h2 id="section-2">2. Introduction</h2>
<p>The purpose, scope and status of the analysis are presented in this section. The main workflow is assessed from fixed cylinder inputs through mesh preparation and analytical response calculation.</p>
<p>Text inputs are generated for ANSYS Parametric Design Language. Material and section records, design-point tables, journal metadata and output listings are read by separate interfaces. Thickness, weld-linearization and fatigue calculations are supplied as supporting methods. Input generation is distinguished from solution execution and engineering acceptance. {cite('D')}</p>
<h3>Not covered</h3>
<p>Bolted joints, contact, thermal coupling, fatigue life, weld assessment, lifting plates, foundation plates and hydrodynamic load transfer are not evaluated here. Their interfaces are listed in the module knowledge pack; their physical assumptions and acceptance criteria require separate analyses. {cite('D')}</p>
<h2 id="section-3">3. Design Data and Basis</h2>
<p>The demonstration geometry, loading and material parameters are defined by the public cylinder example. The values in Table A are illustrative inputs and are not material allowables or a pressure rating. {cite('B')}</p>
<p>The inner radius is {n('basis.inner_radius_mm')} mm, wall thickness is {n('basis.wall_thickness_mm')} mm and axial length is {n('basis.axial_length_mm')} mm. The outside is unloaded; the open ends carry no end-cap force. Axial displacement is restrained on the bottom edge and radial motion remains free. Tension is positive. {cite('B')}</p>
{schematic}
<p>Acceptance of the analytical calculation is based on the radial traction identities, zero axial stress, equilibrium and isotropic elastic relations. These checks are selected because the boundary conditions admit the stated Lamé field. The retained sample comparison checks reproducibility of that field. A material strength criterion and a numerical solution are absent; engineering acceptance is NOT_EVALUATED. {cite('A')} {cite('C')} {cite('E')}</p>
<h2 id="section-4">4. Analysis Methodology</h2>
<p>The cylinder mesh and analytical response are computed by the module from the retained public basis. No licensed solver is started or queried.</p>
<p>The preparation function returns command text, nodes, connectivity, loaded faces, restraints and response positions. The reference function accepts decimal pressure, radius and axial position strings and returns radial, hoop, axial and equivalent stress, radial–axial shear stress, and radial and axial displacement. The fixed basis is enforced; this reference is not a general cylinder design interface. Implementation locators and the invocation are given in section 9.2. {cite('B')} {cite('C')}</p>
<h3>Elasticity equations</h3>
<p>For internal pressure p, inner radius a and outer radius b, the integration constants A and B are determined by the radial tractions. Radius r is measured from the axis; axial position y is measured from the restrained bottom edge. E is Young’s modulus and ν is Poisson’s ratio. The following equations are the open-end specialization of the axisymmetric elastic solution. {cite('A')} {cite('C')}</p>
<div class="eq">dσᵣ/dr + (σᵣ − σθ)/r = 0;<br>
A = pa²/(b² − a²); B = Ab²;<br>
σᵣ = A − B/r²; σθ = A + B/r²; σ𝓏 = τᵣ𝓏 = 0;<br>
uᵣ = [(1 − ν)Ar + (1 + ν)B/r]/E; u𝓏 = −2νAy/E;<br>
σᵥ = √(σᵣ² + σθ² − σᵣσθ).</div>
<p>The inner traction is σᵣ(a) = −p and the outer traction is σᵣ(b) = 0. The axial condition represents open ends with a compatible bottom displacement datum. End caps or an axial restraint over the full length change the axial stress and the equivalent stress. Equation coefficients are algebraic identities from the cited derivation; no design-standard coefficient is used. {cite('A')} {cite('C')}</p>
<h2 id="section-5">5. Detailed Results</h2>
<p>The analytical responses are calculated at the retained sample positions and through the wall. The computed values are summarized here and listed in Table C. {cite('C')}</p>
<p>At the bore and axial position {n('axial_station')} mm, radial stress is {q('sigma_r')} MPa, hoop stress is {q('sigma_theta')} MPa and equivalent stress is {q('sigma_vm')} MPa. Radial displacement is {q('u_r')} mm and axial displacement is {q('u_z')} mm. {cite('C')}</p>
<p>The radial traction distribution is shown with its inner and outer boundary targets. The equivalent-stress distribution is shown separately; a strength limit is not drawn because an allowable has not been supplied. Hover, focus a point or move the radius selector to read a result.</p>
{charts}
<p>The largest absolute percentage difference from the retained nonzero analytical samples is {n('difference_pct')} %. Expected-zero components are checked as exact zeros rather than divided by zero. This comparison demonstrates reproduction of the retained analytical calculation; it is not an independent finite-element solution. {cite('C')} {cite('E')}</p>
{mesh_table}
<p>The prepared meshes have increasing radial and axial subdivision. No solved response is inferred from the element count; a convergence trend remains NOT_EVALUATED. The zero-pressure analytical control is listed in Table D. {cite('B')} {cite('C')}</p>
<h3>Available validation evidence</h3>
<p>Existing analytical tests check boundary tractions, equilibrium, elastic constitutive relations, zero pressure and rejection of out-of-domain inputs. Mesh tests check connectivity, midside positions, positive corner orientation, response adjacency, pressure faces and restraint membership. The report rebuild checks displayed values against module calls and the retained samples. {cite('E')}</p>
<p>Separate retained-result tests compare historical pressure-wall results with a closed-form response and inspect reaction balance, result status and evidence binding; foundation-plate result records are also tested. Tamper tests reject altered or inconsistent records. Those records represent different model definitions and are not substituted for the fixed cylinder mesh solutions in this analysis. Their presence does not establish convergence or an accepted component design. {cite('D')} {cite('E')}</p>
<h2 id="section-6">6. Key Findings</h2>
<p>The analytical response and the boundary of demonstrated use are identified from the calculations and retained records. The response is applicable to the stated open-ended elastic idealization. {cite('A')} {cite('C')}</p>
<ul><li>The bore equivalent stress is {q('sigma_vm')} MPa and radial displacement is {q('u_r')} mm under {n('pressure')} MPa internal pressure.
<ul><li>A design allowable is absent and strength acceptance remains NOT_EVALUATED. {cite('B')} {cite('C')}</li></ul></li>
<li>Mesh preparation and analytical evaluation are demonstrated.
<ul><li>Mesh convergence, native execution and component design acceptance remain NOT_EVALUATED in this analysis. {cite('B')} {cite('E')}</li></ul></li></ul>
<h2 id="section-7">7. Limitations</h2>
<p>The limits of use are stated for the physical model and the software evidence. A numerical result outside this basis requires a separate definition and assessment.</p>
<p>Material response is linear, isotropic and isothermal. Plasticity, instability, fatigue, residual stress and temperature-dependent strength are not evaluated. The uncertain quantity for strength use is the applicable allowable stress; it is not inferred from elastic modulus. {cite('A')} {cite('B')}</p>
<p>The idealization has no heads, end-cap force, nozzle, weld, contact or local attachment. Stress concentration and end-restraint effects are not represented. The public elasticity source distinguishes axial boundary conditions and end effects; transfer to a physical cylinder requires an assessment of those conditions. {cite('A')} {cite('D')}</p>
<p>The reference function accepts the fixed example pressures and geometry only. Different pressures, wall dimensions and material constants are not supported by this reference interface. An unloaded analytical control is not evidence of solver completion. {cite('B')} {cite('C')}</p>
<p>General preparation helpers contain incomplete geometry and loading branches. The module knowledge pack identifies head and reinforcement-pad placeholders, uniform temperature loading in the pressure-wall helper, and absent external-pressure buckling assessment. Complete design-code coverage is not established. {cite('D')}</p>
<h2 id="section-8">8. Recommendations</h2>
<p>The next engineering actions are defined by the missing acceptance inputs and evidence. Use is restricted to analytical comparison and input inspection until those gaps are closed. {cite('B')} {cite('D')}</p>
<p>For strength assessment, supply the governing design-standard edition, material product form, temperature, allowable basis, joint quality and corrosion allowance. Recalculate the applicable section checks with that basis before accepting any component. A limit exceedance requires a revised section or an explicitly assessed operating restriction; no pressure rating is assigned here. {cite('D')}</p>
<p>For numerical acceptance, retain solved outputs for the prepared meshes, the unloaded control, boundary tractions and reactions, and compare the defined stresses and displacements with the analytical solution under the example’s numerical criteria. Record convergence and unresolved diagnostics before accepting a response. {cite('B')} {cite('E')}</p>
<p>For extended use, complete the missing geometry and loading branches and establish separate public benchmarks for each claimed physical feature. Prioritize the closed load path and end-restraint definition before adding nozzles, thermal gradients or contact. {cite('A')} {cite('D')}</p>
<h2 id="section-9">9. References</h2>
<p>The public theory and repository evidence used in this analysis are identified below. Design-standard values are not reproduced.</p>
<h3>9.1 Public theory</h3><ul class="refs">{refs[:refs.index('<li id="ref-B"')]}</ul>
<h3>9.2 Repository evidence and reproducibility</h3><ul class="refs">{refs[refs.index('<li id="ref-B"'):]}</ul>
<p>The public entry points used here are <code>cylinder_benchmark.build_case(case_id)</code>, <code>cylinder_benchmark.frozen_basis()</code> and <code>cylinder_reference.reference_text(pressure, r, y)</code>, all within the ANSYS package. The first nonzero case in the public manifest supplies the plotted radial positions. Every retained analytical position is evaluated; the middle axial position is selected for the displayed through-wall response.</p>
<p>Rebuild from the repository root with <code>PYTHONPATH=src python docs/reports/modules/ansys/build_report.py --output-dir OUTPUT</code>. The output directory is supplied explicitly. The check command <code>PYTHONPATH=src python -m pytest tests/ansys/test_module_report.py -p no:cacheprovider</code> compares the rebuilt page and change panel with the delivered files.</p>
<p>The layout and inlined styles are taken from the <a href="../../../../docs/api/cfd/turbulent-flat-plate-verification.html">existing house report exemplar</a> and shared brand stylesheet. The required document body class is retained. The finite element family uses the supplied ten-section structure; hull resistance and mesh-flow criteria do not apply.</p>
<h2 id="section-10">10. Acronyms and Symbols</h2>
<p>The terms and symbols used in the analysis are defined below. Stress and pressure use megapascals; length and displacement use millimetres.</p>
<p>ANSYS is the analysis software product. APDL means ANSYS Parametric Design Language. FEA means finite element analysis. MPa means megapascal; mm means millimetre. a and b are the inner and outer radii; t is wall thickness; L is axial length; p is internal pressure; r is radial position; y is axial position. E is Young’s modulus; ν is Poisson’s ratio; A and B are elastic integration constants. σᵣ, σθ and σ𝓏 are radial, hoop and axial normal stresses; τᵣ𝓏 is radial–axial shear stress; σᵥ is von Mises equivalent stress. uᵣ and u𝓏 are radial and axial displacements.</p>
<p>In the result legend, a result marked NOT_EVALUATED has no completed assessment against the stated engineering criterion. Analytical values are calculated values under the stated idealization, not an acceptance verdict.</p>
<h2 id="appendix-a">Appendix A. Input and Response Register</h2>
<p>The complete example inputs and retained analytical responses are presented for calculation review. No material allowable is assumed.</p>
{input_table}{station_table}{control_table}
<h3>Missing data register</h3>
<table><thead><tr><th>Missing input</th><th>Disposition</th><th>Effect</th><th>Source</th></tr></thead><tbody>
<tr><td>Material allowable and governing standard edition</td><td>Required input; NOT_EVALUATED</td><td>Strength acceptance and pressure rating are unassigned.</td><td>{cite('D')}</td></tr>
<tr><td>Representative component geometry and end restraint</td><td>Required input; NOT_EVALUATED</td><td>Local stress and applicability to service are unassigned.</td><td>{cite('A')} {cite('D')}</td></tr>
<tr><td>Native result set for the prepared meshes</td><td>Required evidence; NOT_EVALUATED</td><td>Numerical accuracy and convergence are unassigned.</td><td>{cite('B')} {cite('E')}</td></tr>
<tr><td>Demonstration material represents a service material</td><td>ASSUMED - to be confirmed</td><td>The elastic inputs remain illustrative; no service-material attribution is made.</td><td>{cite('B')}</td></tr>
</tbody></table>
<h2 id="appendix-b">Appendix B. Calculation Quality Checks</h2>
<p>The delivered calculation is checked by reproducible software assertions. These checks do not replace an engineering review of model applicability. {cite('E')}</p>
<ul><li>The report is rebuilt and compared byte for byte with the delivered HTML and change panel.</li>
<li>Every registered input, response, mesh size and plotted value is derived from module calls or the retained public example.</li>
<li>Radial traction, zero axial stress, isotropic elastic displacement and zero-pressure identities are independently checked.</li>
<li>Internal links, unique section identifiers, standalone assets and the house document class are checked.</li>
<li>Comment saving and loading are checked without starting a browser. Browser file access remains subject to the reader’s explicit permission.</li></ul>
<h3>Engineering changes</h3><ul>{''.join('<li>' + escape(row['change']) + '</li>' for row in CHANGES['changes'])}</ul>
<p>The change panel records the initial analysis and the comment-persistence correction. The adjacent <code>changes.json</code> contains the same engineering changes.</p>
'''
    identity = hashlib.sha256(content.encode()).hexdigest()
    config = {"report": "ANSYS module analysis", "revision": CHANGES["revision"], "content_digest": identity,
              "sections": [f"section-{i}" for i in range(1, 11)] + ["appendix-a", "appendix-b"]}
    comments = '''<aside aria-label="Report comments"><h2 id="comments">Report comments</h2>
<p>Select a section and enter a comment. Choose the folder containing this report to save comments beside it as <code>report.comments.json</code>. Existing files are loaded before saving. Where folder access is unavailable, export a copy and save it beside the report. Unsaved comments are lost when the page closes.</p>
<label>Section <select id="comment-section"></select></label>
<label>Comment <textarea id="comment-text" maxlength="10000"></textarea></label>
<button id="comment-add" type="button">Add comment</button>
<button id="comment-folder" type="button">Choose report folder</button>
<button id="comment-save" type="button">Save beside report</button>
<button id="comment-export" type="button">Export comments</button>
<label>Load exported comments <input id="comment-load" type="file" accept=".json"></label>
<p id="comment-status" role="status">Comments are not saved.</p><ul id="comment-list"></ul></aside>'''
    return ('<!doctype html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">\n'
            '<meta name="viewport" content="width=device-width,initial-scale=1">\n'
            '<title>ANSYS module analysis</title>\n<style>' + style() + '</style>\n'
            '</head><body class="hs-doc"><main class="wrap">\n' + content + comments +
            '<script id="comment-config" type="application/json">' + json.dumps(config) + '</script>\n'
            '<script>\n' + INTERACTION + '\n</script></main></body></html>\n')


def schematic_svg(data, n):
    """Draw the actual generated coarse mesh and its boundary conditions."""
    case = build_case(data["meshes"][1]["case_id"])
    a = float(data["basis"]["inner_radius_mm"])
    t = float(data["basis"]["wall_thickness_mm"])
    length = float(data["basis"]["axial_length_mm"])
    # A uniform scale preserves the physical wall-slice aspect ratio.
    scale = 1.2
    xy = {row["node_id"]: (340 + (float(row["x_mm"]) - a) * scale,
                           335 - float(row["y_mm"]) * scale) for row in case["nodes"]}
    polygons = []
    for element in case["elements"]:
        points = ' '.join(f'{xy[i][0]:.1f},{xy[i][1]:.1f}' for i in element["nodes"][:4])
        polygons.append(f'<polygon points="{points}" fill="none" stroke="#8593ab" stroke-width="0.6"/>')
    arrows = ''.join(f'<path d="M 310 {y} h 26 m -7 -4 l 7 4 l -7 4" stroke="#0b3d91" fill="none"/>'
                     for y in (85, 145, 205, 265, 315))
    return f'''<figure><svg viewBox="0 0 760 400" role="img" aria-label="Axisymmetric cylinder wall, generated mesh and boundary conditions">
<text x="25" y="28">Open-ended cylinder: meridional wall slice</text>
<path d="M 180 335 V 45 M 175 55 l 5 -10 l 5 10 M 180 335 H 435" stroke="#13233f" fill="none"/>
<text x="130" y="48">Axis y</text><text x="425" y="355">r →</text>
{''.join(polygons)}{arrows}
<text x="230" y="180">Internal</text><text x="230" y="200">pressure p</text>
<text x="430" y="100">Outer traction = 0</text><text x="430" y="125">Top is axially free</text>
<path d="M 340 345 H {340 + t * scale:.1f}" stroke="#13233f"/>
<text x="430" y="315">Bottom: axial displacement = 0</text><text x="430" y="338">Radial displacement is free</text>
<text x="230" y="375">Inner radius a; wall thickness t; axial length L</text>
</svg><figcaption>Figure A. Generated coarse mesh at the physical wall-slice aspect ratio. Eight-node quadrilateral connectivity is used; midside nodes are included in the calculation. The axis-to-wall gap is shortened for legibility. Dimensions are listed in Table A.</figcaption></figure>'''


def chart(data, quantity, n, cite):
    """Create an offline SVG with keyboard, range selection and native hover."""
    rows = data["profile"]
    values = [float(row["values"][quantity]) for row in rows]
    lo, hi = min(values), max(values)
    rlo, rhi = float(rows[0]["r_mm"]), float(rows[-1]["r_mm"])
    points, circles, details = [], [], []
    for i, row in enumerate(rows):
        value = values[i]
        x = 90 + (float(row["r_mm"]) - rlo) / (rhi - rlo) * 540
        y = 230 - (value - lo) / (hi - lo) * 170
        text = (f'Pressure cylinder; {LABELS[quantity][0]}; radius '
                f'{format_value(row["r_mm"], "radius")} mm; demand '
                f'{format_value(row["values"][quantity], quantity)} MPa; '
                'strength limit NOT_EVALUATED')
        points.append(f'{x:.1f},{y:.1f}')
        circles.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="5" fill="#0b3d91" tabindex="0" '
                       f'data-chart="{quantity}" data-index="{i}"><title>{escape(text)}</title></circle>')
        details.append(f'<span id="{quantity}-point-{i}"' + ('' if i == 0 else ' hidden') + '>' +
                       'Pressure cylinder; radius ' + n(f"profile.{i}.r_mm") + ' mm; ' +
                       LABELS[quantity][0].lower() + ' ' + n(f"profile.{i}.{quantity}") +
                       ' MPa; strength limit NOT_EVALUATED.</span>')
    targets = ''
    if quantity == 'sigma_r':
        targets = '<text x="90" y="42">Inner target: −p</text><text x="460" y="42">Outer target: zero</text>'
        # Boundary targets are drawn as open squares at the specified tractions.
        targets += '<rect x="83" y="223" width="14" height="14" fill="none" stroke="#d8443f"/>'
        targets += '<rect x="623" y="53" width="14" height="14" fill="none" stroke="#d8443f"/>'
    return f'''<figure aria-label="{LABELS[quantity][0]} through the cylinder wall">
<svg viewBox="0 0 720 290" role="img" aria-label="{LABELS[quantity][0]} versus radius">
<path d="M 90 50 V 240 H 650" fill="none" stroke="#13233f"/>
<text x="90" y="20">{LABELS[quantity][0]} (MPa)</text>{targets}
<polyline points="{' '.join(points)}" fill="none" stroke="#0b3d91" stroke-width="2"/>
{''.join(circles)}<text x="320" y="280">Radius (mm)</text></svg>
<label>Radius selector <input type="range" id="range-{quantity}" min="0" max="{len(rows)-1}" value="0" step="1" data-chart="{quantity}"></label>
<p id="readout-{quantity}" role="status">{''.join(details)}</p>
<figcaption>{'Figure B. Radial stress; squares show the specified boundary tractions.' if quantity == 'sigma_r' else 'Figure C. Equivalent elastic stress; a material strength limit is not supplied.'} {cite('B')} {cite('C')}</figcaption></figure>'''


INTERACTION = r"""(() => {
  'use strict';
  const config = JSON.parse(document.getElementById('comment-config').textContent);
  const el = id => document.getElementById('comment-' + id);
  const sections = new Set(config.sections);
  const maxComments = 10000, maxBytes = 5000000;
  let comments = [], directory = null;
  const status = text => { el('status').textContent = text; };
  function validate(value) {
    if (!value || value.report !== config.report || value.revision !== config.revision ||
        value.content_digest !== config.content_digest || !Array.isArray(value.comments))
      throw new Error('Comment file belongs to a different report or revision.');
    if (value.comments.length > maxComments)
      throw new Error('Comment count exceeds the limit of ' + maxComments + ' records.');
    for (const row of value.comments) {
      if (!row || typeof row.id !== 'string' || !sections.has(row.section) ||
          typeof row.text !== 'string' || !row.text.trim() || row.text.length > 10000)
        throw new Error('Invalid comment record.');
    }
    return value.comments;
  }
  function merge(incoming) {
    const byId = new Map(comments.map(row => [row.id, row]));
    for (const row of incoming) {
      if (byId.has(row.id) && JSON.stringify(byId.get(row.id)) !== JSON.stringify(row))
        throw new Error('Conflicting comment identity; retain both files for review.');
      byId.set(row.id, row);
    }
    comments = [...byId.values()];
  }
  function show() {
    el('list').replaceChildren();
    for (const row of comments) {
      const li = document.createElement('li');
      li.textContent = row.section + ': ' + row.text;
      el('list').appendChild(li);
    }
  }
  function payload() {
    const value = {...config, comments};
    validate(value);
    const text = JSON.stringify(value, null, 2) + '\n';
    const bytes = new TextEncoder().encode(text).byteLength;
    if (bytes > maxBytes)
      throw new Error('Comment payload is ' + bytes + ' UTF-8 bytes; size limit is ' + maxBytes + '.');
    return text;
  }
  for (const id of sections) {
    const option = document.createElement('option'); option.value = id;
    option.textContent = document.getElementById(id).textContent;
    el('section').appendChild(option);
  }
  el('add').onclick = () => {
    const text = el('text').value.trim();
    if (!text) return status('Enter a comment first.');
    comments.push({id: crypto.randomUUID(), section: el('section').value, text});
    el('text').value = ''; show(); status('Comment added; save to retain it.');
  };
  el('folder').onclick = async () => {
    try {
      if (!window.showDirectoryPicker) throw new Error('Folder access is unavailable; export comments instead.');
      const selected = await window.showDirectoryPicker({mode: 'readwrite'});
      const html = await (await selected.getFileHandle('report.html')).getFile();
      const text = await html.text();
      const parsed = new DOMParser().parseFromString(text, 'text/html');
      const supplied = JSON.parse(parsed.getElementById('comment-config').textContent);
      if (supplied.content_digest !== config.content_digest || supplied.report !== config.report ||
          supplied.revision !== config.revision) throw new Error('The selected folder contains a different report.');
      try {
        const file = await (await selected.getFileHandle('report.comments.json')).getFile();
        if (file.size > maxBytes) throw new Error('Comment file exceeds the size limit.');
        merge(validate(JSON.parse(await file.text())));
      } catch (error) { if (error.name !== 'NotFoundError') throw error; }
      directory = selected; show(); status('Report folder selected. Existing comments are retained.');
    } catch (error) { directory = null; status(error.message); }
  };
  el('save').onclick = async () => {
    try {
      if (!directory) throw new Error('Choose the report folder first, or export comments.');
      try {
        const previous = await (await directory.getFileHandle('report.comments.json')).getFile();
        if (previous.size > maxBytes) throw new Error('Comment file exceeds the size limit.');
        merge(validate(JSON.parse(await previous.text())));
      } catch (error) { if (error.name !== 'NotFoundError') throw error; }
      const text = payload();
      const handle = await directory.getFileHandle('report.comments.json', {create: true});
      const stream = await handle.createWritable(); await stream.write(text); await stream.close();
      show(); status('Comments saved beside the report.');
    } catch (error) { status(error.message); }
  };
  el('export').onclick = () => {
    try {
      const url = URL.createObjectURL(new Blob([payload()], {type: 'application/json'}));
      const link = document.createElement('a'); link.href = url; link.download = 'report.comments.json';
      link.click(); URL.revokeObjectURL(url); status('Comments exported; save the file beside the report.');
    } catch (error) { status(error.message); }
  };
  el('load').onchange = async () => {
    try {
      const file = el('load').files[0]; if (!file) return;
      if (file.size > maxBytes) throw new Error('Comment file exceeds the size limit.');
      merge(validate(JSON.parse(await file.text()))); show(); status('Matching comments loaded; save to retain additions.');
    } catch (error) { status(error.message); }
    el('load').value = '';
  };
  for (const quantity of ['sigma_r', 'sigma_vm']) {
    const slider = document.getElementById('range-' + quantity);
    const points = document.getElementById('readout-' + quantity).children;
    const select = index => { for (let i = 0; i < points.length; i++) points[i].hidden = i !== index;
      slider.value = String(index); };
    slider.oninput = () => select(Number(slider.value));
    for (const circle of document.querySelectorAll('circle[data-chart="' + quantity + '"]')) {
      circle.onfocus = circle.onmouseenter = () => select(Number(circle.dataset.index));
    }
  }
})();"""


def build(output_dir):
    """Write the report and its change panel to an explicit destination."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(compute()), encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    build(parser.parse_args().output_dir)
