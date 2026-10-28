# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build a deterministic module methods report from synthetic arithmetic controls."""
import argparse
import ast
from hashlib import sha256
from html import escape
import json
from pathlib import Path
import re

import numpy as np

from digitalmodel.marine_ops.fowt_quasi_static.response import (
    natural_periods,
    response_operators,
)

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "src/digitalmodel/marine_ops/fowt_quasi_static"
DESTINATION = ROOT / "docs/reports/modules/fowt_quasi_static"
DELIVERABLES = ("index.html", "changes.json", "manifest.json", "README.md")
# [S1] Synthetic controls specified here; [H1] independently derives their answers.
# Coordinates are unit-normalized modal coordinates, not a physical hull basis.
INPUTS = {
    "source": "S1: committed synthetic eigenvalue and impedance controls",
    "natural": {"mass_diagonal": [1.] * 6, "added_diagonal": [0.] * 6,
                "stiffness_diagonal": [1., 4., 9., 16., 25., 36.],
                "grids": {"sampled": [0.5, 6.5, 0.5],
                          "between": [0.5, 6.5, 1.0],
                          "endpoints": [1., 6., 0.5]}},
    "response": {"omega": 1., "mass_diagonal": [2.] * 6,
                 "added_diagonal": [1.] * 6, "stiffness_diagonal": [5.] * 6,
                 "radiation_diagonal": [1.] * 6, "extra_damping_fraction": 0.,
                 "excitation_real": [2., 0., 0., 0., 0., 0.]},
}
CHANGES = [{"revision": "A", "date": "2026-10-28", "change":
            "Initial synthetic methods analysis; sampled natural roots are retained and arithmetic evidence is separated from engineering acceptance."},
           {"revision": "B", "date": "2026-10-28", "change":
            "Calculated report values are rounded to 12 decimal places before serialization to remove final-bit numerical differences within the test tolerances."}]
# Contracts are based on the bounded source review, not execution of a campaign.
CONTRACTS = {
    "basis": ("Rigid mass and inertia assembly", "Supply attributed component masses, tensors and reference origins; tower density is piecewise linear."),
    "geometry": ("Submerged union geometry and volume", "Supply consistent circle, pontoon and draft dimensions; intersection topology and panel refinement require separate assessment."),
    "hydrodynamics": ("Radiation, diffraction and hydrostatics", "Supply physical geometry, density, gravity, depth, frequencies and headings; native BEM and mesh convergence remain unevaluated."),
    "mooring": ("Quasi-static catenaries and mean equilibrium", "Supply consistent submerged weight, elasticity and line geometry; equilibrium holds draft and attitude fixed and reports their residual reactions."),
    "response": ("Coupled frequency response and spectral statistics", "Use finite positive ordered frequencies and compatible six-mode matrices; sampled coverage, interpolation and damping must be qualified for physical use."),
    "workflow": ("Input loading, provenance and campaign assembly", "Use attributed inputs and digest-bound retained coefficients; load-case coverage, interpolation and convergence criteria require separate qualification."),
    "report": ("Analysis presentation and case completeness", "Supply complete validated campaign results; rendering cannot qualify an engineering model."),
}
PURPOSES = {
    "parallel_axis": "Shift inertia by m[(r dot r)I-r outer r].",
    "aggregate": "Combine component masses, centres and tensors at still water level.",
    "hull_mesh": "Build an external submerged surface with reflection symmetry.",
    "analytical_volume": "Integrate exact circle/rectangle union sections independently of panels.",
    "low_frequency_solver": "Configure the finite-depth low-frequency Green function.",
    "cylinder_added_mass": "Calculate a separate cylinder heave added-mass control.",
    "solve_hydrodynamics": "Assemble native radiation/diffraction coefficients and hydrostatic restoring.",
    "coefficient_arrays": "Extract frequency, heading and mode tensors in explicit axis order.",
    "grounded_horizontal_tension": "Invert the grounded inextensible catenary relation.",
    "mooring_system": "Construct elastic lines and a horizontally free body.",
    "forces": "Solve lines at prescribed position and return all restoring components.",
    "stiffness": "Use centred position perturbations for a restoring tangent matrix.",
    "equilibrate": "Balance horizontal mean thrust with fixed draft and attitude.",
    "line_results": "Return tensions, seabed contact, anchor angle and factored utilisation.",
    "spectrum": "Normalize the positive-frequency wave spectrum to supplied height.",
    "wave_statistics": "Integrate response moments and narrow-band Rayleigh statistics.",
    "mass_matrix": "Assemble coupled still-water-origin mass with optional symmetry projection.",
    "natural_periods": "Find the lowest positive root per eigenbranch and assign by mass-weighted participation.",
    "response_operators": "Solve the complex coupled impedance for each frequency and heading.",
    "load_basis": "Load attributed basis and explicit analysis overrides within the source root.",
    "interpolate_hydrodynamics": "Interpolate real and imaginary coefficients without extrapolation.",
    "constraint_reaction": "Balance constrained residual, gravitational and buoyancy loads.",
    "frequency_grid": "Return the campaign frequency samples including low-frequency coverage.",
    "source_digest": "Record raw-byte and parsed-syntax digests.",
    "executed_code_digests": "Bind the local panel-calculation source dependencies.",
    "bem_inputs": "Record physical inputs, frequency settings and numerical package versions.",
    "verify_retained_panels": "Reject stale or incomplete input and output provenance.",
    "panel_runs": "Compute and retain the separate panel-refinement campaign.",
    "load_cases": "Assemble heading, sea-state and line-state response cases.",
    "convergence": "Compare common-frequency coefficients at selected fine-grid peaks.",
    "summarise": "Combine statics, response, sensitivity and campaign summaries.",
    "number": "Format report numbers with bounded significant figures.",
    "delta": "Format relative differences with explicit zero-reference handling.",
    "offset": "Round offsets consistently and suppress numerical negative zero.",
    "table": "Render labelled analysis tables.",
    "section": "Render a numbered report section.",
    "validate_cases": "Require unique and complete load-case combinations.",
    "build_report": "Render a separate analysis report from retained campaign results.",
}


def inventory():
    """Inventory public definitions without importing native or campaign modules."""
    rows = []
    for path in sorted(MODULE.glob("*.py")):
        for node in ast.parse(path.read_text(encoding="utf-8")).body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                purpose, contract = CONTRACTS[path.stem]
                rows.append({"module": path.stem, "function": node.name,
                             "arguments": [arg.arg for arg in node.args.args + node.args.kwonlyargs],
                             "purpose": PURPOSES[node.name], "contract": f"{purpose}. {contract}",
                             "status": "ARITHMETIC_QUALIFIED" if node.name in
                             ("natural_periods", "response_operators") else "NOT_EVALUATED"})
    return rows


def calculate(scratch):
    """Execute only the public response functions on committed synthetic inputs."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    natural = INPUTS["natural"]
    periods = {}
    for name, (start, stop, step) in natural["grids"].items():
        omega = np.arange(start, stop + step / 2, step)
        added = np.tile(np.diag(natural["added_diagonal"]), (len(omega), 1, 1))
        periods[name] = natural_periods(
            omega, added, np.diag(natural["mass_diagonal"]),
            np.diag(natural["stiffness_diagonal"]),
        ).tolist()
    control = INPUTS["response"]
    response = response_operators(
        np.array([control["omega"]]), np.diag(control["added_diagonal"])[None],
        np.diag(control["radiation_diagonal"])[None],
        np.array(control["excitation_real"], dtype=complex)[None, None],
        np.diag(control["mass_diagonal"]), np.diag(control["stiffness_diagonal"]),
        control["extra_damping_fraction"],
    )[0, 0]
    data = {"inputs": INPUTS, "periods": periods,
            "response_real": response.real.tolist(), "response_imag": response.imag.tolist(),
            "amplitude": abs(response).tolist(),
            "phase_rad": [float(np.angle(q)) if abs(q) else None for q in response],
            "assessments": {key: "NOT_EVALUATED" for key in
                            ("campaign_convergence", "mooring_design_acceptance", "native_bem")}}
    data = canonical_numbers(data)
    (scratch / "calculation.json").write_text(json_text(data), encoding="utf-8")
    return data


def canonical_numbers(value):
    """Bound synthetic report rounding to 5e-13 before rendering and hashing."""
    if isinstance(value, float):
        rounded = round(value, 12)
        return rounded if rounded else 0.0
    if isinstance(value, list):
        return [canonical_numbers(item) for item in value]
    if isinstance(value, dict):
        return {key: canonical_numbers(item) for key, item in value.items()}
    return value


def json_text(value):
    """Serialize with stable key ordering and a final newline."""
    return json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"


def table(headers, rows):
    """Render a small escaped data table."""
    return '<table><thead><tr>' + ''.join(f'<th>{escape(h)}</th>' for h in headers) + \
        '</tr></thead><tbody>' + ''.join(rows) + '</tbody></table>'


def row(values):
    """Escape table cells."""
    return '<tr>' + ''.join(f'<td>{escape(str(v))}</td>' for v in values) + '</tr>'


def period_row(mode, data):
    """Format public-function periods for each independently controlled mode."""
    return row([mode + 1, *[f'{data["periods"][key][mode]:.4g}'
                           for key in ("sampled", "between", "endpoints")], "H1, S1, C1"])


def response_row(mode, data):
    """Format every component; a zero response has undefined phase."""
    phase = data["phase_rad"][mode]
    return row([mode + 1, f'{data["response_real"][mode]:.4g}',
                f'{data["response_imag"][mode]:.4g}', f'{data["amplitude"][mode]:.4g}',
                "undefined" if phase is None else f'{phase:.4g}', "H2, S1, C1"])


def render(data):
    """Render the established ten-section house layout with bounded claims."""
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(), flags=re.S)
    sections = []

    def section(index, title, body):
        sections.append(f'<section><h2 id="s{index}">{index}. {title}</h2>{body}</section>')

    section(1, "Executive Summary", """<p>The quasi-static floating-wind module links rigid-body properties, submerged geometry, mooring equilibrium, hydrodynamic coefficients and linear response. This analysis qualifies arithmetic for synthetic modal roots and a complex impedance only [S1, H1, H2, C1].</p>
<p>Exact sampled roots and range endpoints are retained. Campaign convergence, mooring design acceptance and native BEM results are NOT_EVALUATED. No physical design input or operating limit is inferred.</p>""")
    sections.append('<section id="revision-history"><h3>Revision history</h3>' + table(
        ("Revision", "Date", "What changed and why"),
        [row([c["revision"], c["date"], c["change"]]) for c in CHANGES]) + '</section>')
    section(2, "Introduction", """<p>The bounded public-function review covers mooring, geometry, hydrostatics, response and workflow methods. Only response arithmetic is executed. The separate analysis-report workflow and its data are not regenerated. Native functions are inventoried by parsing source, without importing or executing a solver [C2].</p>""")
    inputs = [row(["Natural control", "M=I; A=0; K=diag(1,4,9,16,25,36)",
                   "normalized modal mass; stiffness in modal mass/s²", "S1, H1"]),
              row(["Natural grids", "0.5 to 6.5 by 0.5; 0.5 to 6.5 by 1; 1 to 6 by 0.5",
                   "rad/s", "S1"]),
              row(["Response control", "ω=1; M=2I; A=I; K=5I; B=I; extra damping=0",
                   "rad/s; normalized modal mass, mass/s², mass/s; dimensionless fraction", "S1, H2"]),
              row(["Excitation", "F=(2,0,0,0,0,0), real", "normalized generalized force", "S1, H2"])]
    section(3, "Design Data and Basis", """<p>All constants are committed synthetic eigenvalue controls [S1], not design inputs. Unit-normalized modal coordinates replace physical translation/rotation coordinates; response is in normalized displacement units. Identity matrices and zero coefficients are algebraic definitions. No environmental, hull or line properties are assumed.</p>""" + table(
        ("Control", "Inputs", "Units", "Source"), inputs) +
        '<details><summary>Complete committed synthetic inputs [S1]</summary><pre>' +
        escape(json_text(INPUTS)) + '</pre></details>')
    methods = [row([r["module"] + '.' + r["function"], ', '.join(r["arguments"]),
                    r["purpose"], r["contract"], r["status"]]) for r in inventory()]
    section(4, "Analysis Methodology", """<p>Natural roots solve det[K−ω²(M+A(ω))]=0. The implementation linearly interpolates added mass, sorts eigenbranches, retains the lowest positive root on each branch, and assigns modes by dominant mass-weighted participation. Sampled residuals within 32 binary64 epsilons of the eigenvalue/frequency-squared scale are treated as roots; adjacent intervals are excluded to prevent duplicate candidates [C1].</p>
<p>Modal crossings, multiple roots on a frequency-dependent branch and nearly degenerate mode labels remain unresolved. The search does not establish roots beyond the sampled band. Coupled-system coverage is retained in the existing analytical eigenvalue test [C3].</p>
<p>The response convention is Z=K−ω²(M+A)−iω(B+Bextra), Zq=F. Extra damping is applied only to the implementation-selected components. This example sets it to zero and does not qualify that damping model [C1].</p>
<p>The inventory status ARITHMETIC_QUALIFIED applies only to the controls shown here; every other method remains NOT_EVALUATED. Function arguments are listed in source order; defaults and return contracts remain in the source [C2].</p>""" + table(
        ("Public function", "Arguments", "Purpose", "Contract / review limit", "Example status"), methods))
    section(5, "Detailed Results", """<h3>Independent eigenvalue calculation [H1]</h3>
<p>For j=1,…,6, det(K−ω²I)=∏(j²−ω²). The positive roots are ω=j rad/s and T=2π/j s. These follow by factoring the diagonal determinant independently of the numerical search. Both sampled endpoints and strictly bracketed roots are exercised [S1, C3].</p>""" + table(
        ("Mode j", "Sampled T (s)", "Between-grid T (s)", "Endpoint-grid T (s)", "Source"),
        [period_row(i, data) for i in range(6)]) + """<h3>Independent complex calculation [H2]</h3>
<p>At ω=1, Z=5−(2+1)−i=2−i. Multiplying 2/(2−i) by (2+i)/(2+i) gives q=(4+2i)/5. Its amplitude is √20/5 and phase is atan2(2,4) rad. Every unforced component is zero; phase at zero amplitude is undefined. The full complex vector is tested, rather than compared with another solver [C3].</p>""" + table(
        ("Mode", "Real q", "Imaginary q", "Amplitude", "Phase (rad)", "Source"),
        [response_row(i, data) for i in range(6)]))
    section(6, "Key Findings", """<p>Strict sign-change bracketing missed exact sampled roots. Explicit sampled-root candidates correct that defect while retaining the modal assignment step [C1, C3]. The diagonal response control establishes complex sign, amplitude and phase for this impedance convention.</p>
<p>Calculated report values are rounded to 12 decimal places before scratch output, HTML serialization and artifact hashing. Rounding contributes at most 5e-13 in the stated normalized units, below the 1e-12 absolute test tolerance. Undefined phase remains null. This precision applies to these bounded synthetic controls; it does not change solver arithmetic or establish reproducibility for arbitrary inputs [S1, H1, H2, C3].</p>
<p>Period tests use relative tolerance 1e-11 and absolute tolerance 1e-12 s, allowing the bracket solver's default absolute frequency tolerance and binary64 rounding over this bounded range. The diagonal impedance has condition number one; relative and absolute response tolerances of 1e-12 are conservative relative to binary64 rounding for this small solve. These are software-test tolerances, not engineering acceptance criteria [H1, H2, C3].</p>""")
    section(7, "Limitations and Not Covered", '<ul>' + ''.join(
        f'<li>{escape(key)}: {value}</li>' for key, value in data["assessments"].items()) + """</ul>
<p>No native BEM results, hull mesh, hydrostatic stiffness, mooring solution or campaign load cases are generated. The grounded catenary formula is inextensible; elastic seabed behaviour is delegated by the mooring workflow. Horizontal statics constrain draft and attitude. Finite-difference mooring tangents, geometry topology, spectral tails, symmetry projection, retained-coefficient digests and campaign convergence still require independent qualification [C2].</p>
<p>Linear response excludes nonlinear line dynamics, second-order drift and coupled aerodynamic/control behaviour. A synthetic matrix cannot establish physical resonance accuracy, line strength, anchor capacity or stationkeeping acceptance. No conformity with a standard is claimed.</p>""")
    section(8, "Recommendations", """<p>Before physical use, establish attributed geometry, mass origins, line properties, environmental data and acceptance criteria. Independently check geometry volume and hydrostatic restoring, test catenary and elastic line demand, refine native panels and frequency coverage, and compare coupled response against an independent physical benchmark. Qualify constrained reactions and damaged-line cases. Leave the issue open while these methods remain unqualified [C2].</p>""")
    section(9, "References and Appendices", """<ul>
<li>S1: synthetic input definitions in the report builder; all numerical controls are defined for arithmetic qualification.</li>
<li>H1: independent diagonal determinant factorization in Detailed Results and the hand-value tests.</li>
<li>H2: independent scalar complex division in Detailed Results and the hand-value tests.</li>
<li>C1: response.py, natural_periods and response_operators; implementation contract, not an independent oracle.</li>
<li>C2: public-function source inventory in Analysis Methodology; exact source bytes are bound by manifest.json.</li>
<li>C3: test_module_report_hand_values.py, test_response.py and test_fowt_quasi_static_module_report.py; arithmetic, coupling and rebuild evidence.</li>
<li>L1: existing report layout and canonical brand palette, inlined from the sources named in manifest.json.</li></ul>
<p>No external numerical source or licensed publication is required for the algebraic controls. Manifest source digests bind inputs, implementation, tests and presentation assets. Artifact digests bind index.html, changes.json and README.md; manifest.json does not hash itself. Scratch paths, timestamps and runtime package versions are excluded from deterministic deliverables. Direct rebuild tests compare every deliverable byte and all displayed worked rows.</p>""")
    section(10, "Acronyms and Symbols", """<p>BEM: boundary element method. CG: centre of gravity. RAO: response amplitude operator. M: rigid mass. A: added mass. K: restoring stiffness. B: radiation damping. Z: complex impedance. F: generalized excitation. q: complex displacement. ω: angular frequency. T: natural period. I: identity matrix. i: imaginary unit. All example coordinates are normalized modal coordinates [S1].</p>""")
    changes = json.dumps(CHANGES, sort_keys=True).replace('<', '\\u003c')
    payload = json.dumps(data, sort_keys=True, allow_nan=False).replace('<', '\\u003c')
    panel = '''<details id="changes-panel"><summary>Change panel</summary><p>Revision A: initial synthetic analysis and sampled-root correction.</p><p>Revision B: bounded numerical serialization for reproducible report bytes.</p><button id="save-changes" type="button">Save change record</button></details>
<script>document.getElementById('save-changes').addEventListener('click', async () => {
const text = document.getElementById('change-data').textContent;
if ('showSaveFilePicker' in window) {
const handle = await window.showSaveFilePicker({suggestedName:'changes.json'});
const stream = await handle.createWritable(); await stream.write(text); await stream.close();
} else {
const url = URL.createObjectURL(new Blob([text], {type:'application/json'}));
const link = document.createElement('a'); link.href=url; link.download='changes.json'; link.click(); URL.revokeObjectURL(url);
}});</script>'''
    page = ('<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            '<title>Quasi-static floating wind module analysis</title><style>' + brand + '\n' + layout +
            '</style></head><body class="hs-doc"><main class="wrap">'
            '<header><h1>Quasi-static floating wind module analysis</h1><p>Synthetic arithmetic qualification · Revision A</p></header>' +
            ''.join(sections) + panel + f'<script id="calculation" type="application/json">{payload}</script>' +
            f'<script id="change-data" type="application/json">{changes}</script></main></body></html>\n')
    return '\n'.join(line.rstrip() for line in page.splitlines()) + '\n'


def readme():
    """Provide a portable rebuild command and explicit engineering limits."""
    return """# Quasi-static floating wind module analysis

[Read the module report](index.html). The report executes committed synthetic
modal and scalar impedance controls through public response functions.
The other public functions are inventoried with explicit qualification limits.

Rebuild from the repository root with NumPy and SciPy available:

```sh
PYTHONPATH=src python scripts/reports/build_fowt_quasi_static_module_report.py --scratch ../report-scratch --output-dir docs/reports/modules/fowt_quasi_static
```

Scratch must be outside the checkout. Only the four deliverables are written to
the output directory. No native solver, named study data or campaign is used.
The manifest binds source bytes and artifact digests, excluding its own digest.
Two independent rebuilds must reproduce every committed deliverable byte.
Calculated report values are rounded to 12 decimal places before serialization
and hashing. The maximum rounding contribution is 5e-13 in the stated normalized
units, below the 1e-12 absolute test tolerance. Adjacent binary64 rounding in both
directions is exercised by byte comparisons of all four deliverables. This bound
applies to these synthetic controls, without changing the solver arithmetic.

The positive natural roots follow from det(K−ω²I)=product(j²−ω²), giving T=2π/j.
The scalar response follows from 2/(2−i)=(4+2i)/5. These independent calculations
are the expected values in the hand-value and report tests. Numerical tolerances
and normalized units are stated in the report; these controls are not design data.
Campaign convergence, mooring design acceptance and native BEM are NOT_EVALUATED.

Lesson: exact sampled roots require explicit retention as well as interval
bracketing; the endpoint and exact-grid regressions preserve this requirement.
"""


def build(scratch, output_dir):
    """Write deterministic deliverables and a source/artifact digest manifest."""
    data = calculate(scratch)
    output = Path(output_dir)
    output.mkdir(parents=True, exist_ok=True)
    content = {"index.html": render(data), "changes.json": json_text(CHANGES), "README.md": readme()}
    sources = [Path(__file__), *sorted(MODULE.glob('*.py')),
               ROOT / "tests/marine_ops/fowt_quasi_static/test_module_report_hand_values.py",
               ROOT / "tests/marine_ops/fowt_quasi_static/test_response.py",
               ROOT / "tests/reporting/test_fowt_quasi_static_module_report.py",
               ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html",
               ROOT / "docs/api/_assets/brand.css"]
    manifest = {"sources": {p.relative_to(ROOT).as_posix(): sha256(p.read_bytes()).hexdigest()
                            for p in sorted(sources)},
                "artifacts": {name: sha256(value.encode('utf-8')).hexdigest()
                              for name, value in sorted(content.items())},
                "basis": "S1 synthetic controls; H1 diagonal factorization; H2 scalar complex division",
                "scope": "Response arithmetic only; native BEM and engineering acceptance NOT_EVALUATED"}
    content['manifest.json'] = json_text(manifest)
    for name, value in content.items():
        (output / name).write_text(value, encoding='utf-8', newline='\n')
    return manifest


def main():
    """Accept explicit scratch and delivery destinations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--scratch', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == '__main__':
    main()
