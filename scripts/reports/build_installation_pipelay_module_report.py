# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the pipelay arithmetic report using only pure public calculations.

Input basis: locally selected illustrative arithmetic controls.
Independent derivations: tests/installation/test_pipelay_report_hand_values.py.
Maintainer acceptance of this basis is pending. These are not qualified design inputs.
"""

import argparse
import copy
import hashlib
import html
import json
import re
import tempfile
from fractions import Fraction
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/installation_pipelay"
SCRIPT = "scripts/reports/build_installation_pipelay_module_report.py"
SOURCES = [
    SCRIPT,
    "src/digitalmodel/installation/pipelay/stress.py",
    "src/digitalmodel/installation/pipelay/integrity.py",
    "tests/installation/test_pipelay_report_hand_values.py",
    "tests/installation/test_pipelay_stress.py",
    "tests/installation/test_pipelay_stress_finite_inputs.py",
    "tests/installation/test_pipelay_integrity.py",
    "tests/reporting/test_installation_pipelay_module_report.py",
    "docs/api/_assets/brand.css",
]
# Inputs are selected locally to give simple, independently derived hand values;
# no external specification or qualified design basis supplies these controls.
INPUTS = {
    "stress": {"outer_diameter": 200, "bend_radius": 100000,
               "youngs_modulus": 200000, "axial_force": 20000,
               "steel_area": 1000, "smys": 400, "allowable_fraction": 3 / 4},
    "route": {"bottom_tension": 100000, "lateral_friction": 1 / 2,
              "submerged_weight_per_length": 1000},
}
INPUT_UNITS = {
    "stress": {"outer_diameter": "mm", "bend_radius": "mm",
               "youngs_modulus": "N/mm²", "axial_force": "N",
               "steel_area": "mm²", "smys": "N/mm²", "allowable_fraction": "1"},
    "route": {"bottom_tension": "N", "lateral_friction": "1",
              "submerged_weight_per_length": "N/m"},
}
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Initial synthetic stress and route-equilibrium analysis, public-method "
              "inventory and explicit qualification gaps are provided to make the "
              "arithmetic reproducible without implying design compliance.",
}, {
    "revision": "B", "date": "2026-10-28",
    "change": "Input provenance is corrected to locally selected illustrative arithmetic "
              "controls because no external input specification is identified; "
              "maintainer acceptance of this basis remains pending and the "
              "inputs and calculated results are unchanged.",
}]


def method_inventory() -> list[dict[str, str]]:
    """Record dimensional review separately from arithmetic and qualification."""
    rows = []
    groups = [
        ("stress", ["bending_stress_from_radius", "axial_stress", "smys_stress_check"],
         "INDEPENDENTLY_CHECKED", "Consistent N/mm² arithmetic only; tensile outer fibre.",
         "Approved load case, wall-force definition, material data and installation criterion."),
        ("stress", ["allowable_bend_radius"], "NOT_EVALUATED",
         "Stress cancels in E*(OD/2)/(f*SMYS), leaving length; pure bending only.",
         "Independent inverse-radius reference and an approved pure-bending criterion."),
        ("integrity", ["bending_strain_from_radius"], "INDEPENDENTLY_CHECKED",
         "Length divided by length gives strain; small-curvature geometry only.",
         "Measured curvature and valid geometry for the intended installation case."),
        ("integrity", ["minimum_route_radius"], "INDEPENDENTLY_CHECKED",
         "N/(dimensionless*N/m) gives m; static uniform friction and positive weight.",
         "Qualified soil friction, effective bottom tension, weight and route geometry."),
        ("integrity", ["concrete_crushing_strain", "concrete_crushing_check"],
         "NOT_EVALUATED", "Strain scaling and ratios are dimensionless; coating transfer "
         "and strict inequality at the limit are unqualified.",
         "Identified coating-model edition and clause, composite-section response, "
         "interface behavior, measured strain limit and approved safety factor."),
        ("integrity", ["route_radius_factor_of_safety"], "NOT_EVALUATED",
         "Length/length gives a ratio; the documented target is not adopted.",
         "Independent reference and an approved route-radius criterion."),
        ("integrity", ["weld_repair_allowable_length", "is_weld_repair_acceptable"],
         "NOT_EVALUATED", "Both return consistent length comparisons, but a fractional "
         "maximum below the minimum makes the helper contracts disagree.",
         "Governing standard edition, repair clause and approved interpretation when "
         "the fractional cap is below the minimum; retain the expected-failure test."),
        ("integrity", ["displaced_weight_per_length", "submerged_weight_per_length"],
         "NOT_EVALUATED", "kg/m³*m/s²*m² gives N/m; dry weight must already be N/m. "
         "Physical sign, finiteness and geometry validation are incomplete.",
         "Independent buoyancy reference, total coated geometry, fluid density and dry weight."),
        ("integrity", ["horizontal_lay_tension", "top_lay_tension"], "NOT_EVALUATED",
         "N/m*m gives N; the flexible static catenary omits bending stiffness and dynamics.",
         "Independent catenary reference with boundary conditions, submerged weight, "
         "water depth and touchdown radius."),
    ]
    for module, names, qualification, limits, evidence in groups:
        for name in names:
            rows.append({"name": f"{module}.{name}", "review": "DIMENSIONALLY_REVIEWED",
                         "qualification": qualification, "limits": limits,
                         "evidence_needed": evidence})
    return sorted(rows, key=lambda row: row["name"])


def external_scratch(scratch: Path) -> Path:
    """Resolve symlinks and directory junctions before rejecting checkout staging."""
    path = Path(scratch).resolve()
    if path == ROOT or ROOT in path.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    return path


def calculate(scratch: Path) -> dict[str, Any]:
    """Execute the public API on the cited synthetic inputs."""
    from digitalmodel.installation.pipelay import integrity, stress

    external_scratch(scratch)
    inputs = copy.deepcopy(INPUTS)
    geometry = {key: inputs["stress"][key] for key in ("outer_diameter", "bend_radius")}
    bending = stress.bending_stress_from_radius(
        **geometry, youngs_modulus=inputs["stress"]["youngs_modulus"]
    )
    axial = stress.axial_stress(inputs["stress"]["axial_force"], inputs["stress"]["steel_area"])
    check = stress.smys_stress_check(**inputs["stress"])
    values = {
        "bending_strain": (integrity.bending_strain_from_radius(**geometry), "1", "S1; S3"),
        "bending_stress": (bending, "N/mm²", "S1; S2"),
        "axial_stress": (axial, "N/mm²", "S1; S2"),
        "longitudinal_stress": (check.longitudinal_stress, "N/mm²", "S1; S2"),
        "allowable_stress": (check.allowable_stress, "N/mm²", "S1; S2"),
        "utilisation": (check.utilisation, "1", "S1; S2"),
        "percent_smys": (check.percent_smys, "%", "S1; S2"),
        "passes": (check.passes, "boolean", "S1; S2"),
        "route_radius": (integrity.minimum_route_radius(**inputs["route"]), "m", "S1; S3"),
    }
    return {"inputs": inputs, "input_units": copy.deepcopy(INPUT_UNITS),
            "results": {key: {"value": value, "unit": unit, "source": source}
                        for key, (value, unit, source) in values.items()},
            "qualification": "ARITHMETIC_ONLY", "methods": method_inventory()}


def number(value: float | bool, unit: str) -> str:
    """Render dimensionless ratios exactly and other values to one decimal."""
    if isinstance(value, bool):
        return str(value)
    if unit == "1" and value != int(value):
        return str(Fraction(value).limit_denominator())
    return f"{value:.1f}"


def result_row(name: str, row: dict[str, Any]) -> str:
    """Render the returned result with units and its source binding."""
    cells = [name, number(row["value"], row["unit"]), row["unit"], row["source"]]
    return "<tr>" + "".join(f"<td>{html.escape(cell)}</td>" for cell in cells) + "</tr>"


def json_text(value: Any) -> str:
    """Serialize finite structured results with stable ordering and line endings."""
    return json.dumps(value, ensure_ascii=False, allow_nan=False, sort_keys=True, indent=2) + "\n"


def render(scratch: Path) -> str:
    """Render the existing ten-section report format around calculated values."""
    data = calculate(scratch)
    results = "".join(result_row(name, row) for name, row in data["results"].items())
    input_rows = "".join(
        f"<tr><td>{group}.{name}</td><td>{number(value, data['input_units'][group][name])}</td>"
        f"<td>{data['input_units'][group][name]}</td><td>S1; supplied synthetic control</td></tr>"
        for group, quantities in data["inputs"].items() for name, value in quantities.items()
    )
    methods = "".join(
        "<tr>" + "".join(f"<td>{html.escape(row[key])}</td>" for key in
                        ("name", "review", "qualification", "limits", "evidence_needed")) + "</tr>"
        for row in data["methods"]
    )
    revision = "".join(f"<tr><td>{row['revision']}</td><td>{row['date']}</td>"
                       f"<td>{html.escape(row['change'])}</td></tr>" for row in CHANGES)
    sources = "".join(f"<li><code>{path}</code>; SHA-256 "
                      f"<code>{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</code></li>"
                      for path in SOURCES)
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / SOURCES[-1]).read_text(encoding="utf-8"), flags=re.S)
    r = data["results"]
    total = number(r["longitudinal_stress"]["value"], r["longitudinal_stress"]["unit"])
    allowable = number(r["allowable_stress"]["value"], r["allowable_stress"]["unit"])
    disposition = "meets" if r["passes"]["value"] else "exceeds"
    page = f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Pipelay stress and integrity analysis</title><style>{brand}
body{{margin:0;background:var(--ground);color:var(--ink);font:1rem/1.6 system-ui,sans-serif}}
main{{max-width:var(--wrap);margin:auto;padding:2rem}}table{{border-collapse:collapse;width:100%;margin-top:1rem}}
th,td{{padding:.6rem;border:1px solid var(--edge);text-align:left;vertical-align:top;overflow-wrap:anywhere}}
th{{background:var(--panel-2)}}h1,h2,h3{{color:var(--brand)}}code{{overflow-wrap:anywhere}}
.table-wrap{{overflow-x:auto}}details{{padding:1rem;background:var(--panel)}}textarea{{width:100%}}
@media print{{main{{padding:0}}button,textarea{{display:none}}}}
</style></head><body class="hs-doc"><main>
<h1>Pipelay stress and integrity analysis</h1>
<p>Pure calculation routes · Synthetic arithmetic controls · No compliance claim</p>
<h2 id="s1">1. Executive Summary</h2>
<p>The synthetic combined longitudinal stress is {total} N/mm² and {disposition} the supplied
arithmetic threshold of {allowable} N/mm² [S1, S2]. This comparison qualifies the demonstrated
arithmetic only. Physical installation acceptance is NOT_EVALUATED.</p>
<h3>Revision history</h3>
<div class="table-wrap"><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody>{revision}</tbody></table></div><p>Table R. Issue history; earlier rows will be retained on reissue.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{html.escape(CHANGES[-1]['change'])}</p></details>
<h2 id="s2">2. Introduction</h2>
<p>The pipelay public stress and integrity helpers are reviewed for dimensional consistency.
Independent hand calculations check the worked stress and route-equilibrium results [S1, S4].
The examples are separate controls, not an integrated installation design. No licensed solver is used.</p>
<h2 id="s3">3. Design Data and Basis</h2>
<p>Every input below is SYNTHETIC: supplied for arithmetic demonstration [S1]. No material
certificate, surveyed route, coating measurement or standard acceptance factor is supplied.
The geometry is in mm for stress and the route weight is in N/m for the separate equilibrium example.
No implicit conversion is applied. Strength and allowable fraction are supplied controls.</p>
<p>The locally selected illustrative arithmetic controls are pending maintainer acceptance
of that demonstration basis. They are not qualified design inputs [S1].</p>
<div class="table-wrap"><table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead>
<tbody>{input_rows}</tbody></table></div><p>Table D. Complete synthetic design data with units and sources.</p>
<h2 id="s4">4. Analysis Methodology</h2>
<p>Small-curvature outer-fibre strain is OD/(2R), and elastic bending stress is E times that
strain. Axial stress is force divided by steel area; the demonstrated tensile outer-fibre stress
is their sum. The supplied fraction times supplied strength is the arithmetic threshold.
Utilisation is total stress divided by that threshold; percent strength is total divided by
strength times the percent conversion [S1, S2, S4]. These geometric and ratio identities
introduce no standard-derived criteria. The route calculation balances F/R = μw [S1, S3, S4].</p>
<p>Every public function in the two source modules is inventoried below. DIMENSIONALLY_REVIEWED
records a source review; INDEPENDENTLY_CHECKED records only the independent synthetic arithmetic
in this report. Neither establishes physical qualification. The returned immutable records
SmysCheck and CrushingCheck hold stress and coating results respectively [S2, S3].</p>
<div class="table-wrap"><table><thead><tr><th>Public function</th><th>Review</th><th>Arithmetic status</th>
<th>Limits</th><th>Evidence needed for qualification</th></tr></thead><tbody>{methods}</tbody></table></div>
<p>Table M. Complete public-method inventory, dimensional findings and outstanding evidence [S2, S3, S4].</p>
<h2 id="s5">5. Detailed Results</h2>
<p>Every displayed result is calculated by the public functions. Dimensionless ratios are
shown as fractions; other displayed values use one decimal place. Comparisons use unrounded
returned values. The complete precision payload is embedded for reproducible readback.</p>
<div class="table-wrap"><table><thead><tr><th>Result</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead>
<tbody>{results}</tbody></table></div><p>Table V. Complete worked-result payload and source register keys.</p>
<h2 id="s6">6. Key Findings</h2>
<p>The combined-stress result {disposition} its supplied arithmetic criterion ({total} compared
with {allowable} N/mm²) [S1, S2]. The independent force-equilibrium example checks radius in m,
as required by N divided by N/m [S1, S3, S4]. These results do not establish installation suitability.</p>
<p>The stress module's inconsistent force/area unit example is corrected to distinguish N/mm²
from N/m². The executable examples are covered by the independent arithmetic test [S2, S4].</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>Weld-repair acceptance is NOT_EVALUATED: the fractional-cap/minimum-length interpretation
is unresolved, and the existing expected-failure test is retained [S3, S5]. Coating response is
NOT_EVALUATED because strain transfer, material limits and the governing model are unverified.
No default criterion or standard-derived value is reproduced or changed.</p>
<p>The stress combination describes a tensile outer fibre; signed compression, the opposite
fibre, equivalent stress, ovalisation, buckling, plasticity and load interaction require separate
assessment. Physical sign and geometry validation are incomplete. The integrity helpers do not
systematically reject nonfinite values. The demonstrated inputs are finite and positive; this
report does not qualify those input domains [S2, S3]. Dynamic lay behavior, stinger contact,
soil variability and native solver comparisons are not calculated.</p>
<h2 id="s8">8. Recommendations</h2>
<p>Before engineering use, supply the load case, effective versus wall tension definition,
material certificate, qualified geometry and an approved installation criterion. Supply measured
pipe-soil friction and submerged weight for route equilibrium. Resolve the repair-length conflict
against the governing edition and repair clause before adopting either helper's result. Obtain
coating-section evidence and strain limits before using a coating acceptance flag. Independently
check the buoyancy and catenary methods with qualified boundary conditions. These missing inputs
are required evidence, rather than zeroes or inferred criteria [S2–S5].</p>
<h2 id="s9">9. References and Appendices</h2>
<ol><li id="S1">S1 — Locally selected illustrative arithmetic controls in
<code>INPUTS</code> in <code>{SCRIPT}</code>, pinned by the source digest below.
The basis is pending maintainer acceptance. These are not qualified design inputs;
no external input specification is identified.</li>
<li id="S2">S2 — Public stress module: geometric elasticity, axial stress and supplied-strength comparison.</li>
<li id="S3">S3 — Public integrity module: geometric strain, force equilibrium and inventoried helpers.</li>
<li id="S4">S4 — Independent hand-value test and dedicated report tests, listed below.
The test docstrings derive the reference values and justify a binary64 roundoff tolerance.</li>
<li id="S5">S5 — Existing stress, finite-input and integrity tests; the repair interpretation
remains an explicit expected failure. Existing tests alone are not independent physical evidence.</li></ol>
<p>The exact source editions are bound by the following SHA-256 digests. The manifest hashes
all source dependencies and the other delivered artifacts; its own bytes are checked by deterministic
rebuild. The canonical report palette is inlined from the existing brand stylesheet. No licensed
numerical content is copied; no standard-compliance conclusion is made.</p><ol>{sources}</ol>
<p>Rebuild: <code>PYTHONPATH=src uv run --no-project python {SCRIPT} --scratch "$TMPDIR/pipelay-report"</code>.
The scratch location must be supplied outside the checkout. The builder accepts
<code>--output-dir</code>; published files are index.html, changes.json, manifest.json and README.md.
Temporary staging is external and is removed after publication.</p>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p>OD: outer diameter. E: Young's modulus. SMYS: specified minimum yield strength.
N: newton. mm: millimetre. m: metre. NOT_EVALUATED: the required qualification evidence is absent.</p>
<label for="notes">Engineering review notes</label><textarea id="notes" rows="5"></textarea>
<button id="save-notes" type="button">Save comments beside report</button>
<p id="save-status" aria-live="polite">Comments remain unsaved until exported.</p>
</main><script id="calculation" type="application/json">{json_text(data)}</script>
<script id="changes" type="application/json">{json_text(CHANGES)}</script><script>
document.getElementById('save-notes').addEventListener('click', async () => {{
  const text = document.getElementById('notes').value;
  try {{
    if (window.showSaveFilePicker) {{
      const handle = await window.showSaveFilePicker({{suggestedName:'comments.txt'}});
      const stream = await handle.createWritable(); await stream.write(text); await stream.close();
    }} else {{
      const url = URL.createObjectURL(new Blob([text], {{type:'text/plain'}}));
      const link = document.createElement('a'); link.href = url; link.download = 'comments.txt';
      link.click(); URL.revokeObjectURL(url);
    }}
    document.getElementById('save-status').textContent = 'Comments exported; retain beside report.';
  }} catch (error) {{
    document.getElementById('save-status').textContent = 'Export incomplete; comments remain unsaved.';
  }}
}});
</script></body></html>'''
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


README = f'''# Pipelay stress and integrity analysis

[Open the module report](index.html). The report covers every public function in
the pipelay stress and integrity modules, with independent synthetic stress and
route-equilibrium arithmetic. Supplied strength and fraction are arithmetic
controls, not standard criteria. Engineering qualification is NOT_EVALUATED.

The locally selected illustrative arithmetic controls are pending maintainer acceptance
of the demonstration basis. They are not qualified design inputs; no external input
specification is identified.

The committed inputs are in [{SCRIPT}](../../../../{SCRIPT}); the independent
derivations are in the hand-value test. No solver or extra runtime package is required.

Rebuild from the repository root, with TMPDIR set to an external scratch directory:

```sh
PYTHONPATH=src uv run --no-project python {SCRIPT} --scratch "$TMPDIR/pipelay-report"
```

Use `--output-dir` to publish into a separate directory. Temporary staging remains
outside the checkout. The four deliverables are `index.html`, `changes.json`,
`manifest.json` and this README. The manifest hashes every source dependency and
the other three deliverables; deterministic rebuild checks the manifest itself.

The coating-model assumptions and weld-repair interpretation remain
NOT_EVALUATED. Their required evidence and the remaining method qualification
gaps are listed in the report. Revision history and the change panel accompany
the summary; engineering comments can be exported beside the report.
'''


def build(output_dir: Path, scratch: Path) -> dict[str, Any]:
    """Stage external temporary files, then write only the named deliverables."""
    scratch = external_scratch(scratch)
    page = render(scratch)
    artifacts = {"index.html": page, "changes.json": json_text(CHANGES), "README.md": README}
    manifest = {
        "schema": "installation-pipelay-report/1",
        "input_basis": "Locally selected illustrative arithmetic controls; not qualified design inputs",
        "input_basis_review": "PENDING_MAINTAINER_ACCEPTANCE",
        "qualification": "ARITHMETIC_ONLY",
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
                    for path in SOURCES},
        "artifacts": {name: hashlib.sha256(content.encode("utf-8")).hexdigest()
                      for name, content in artifacts.items()},
    }
    artifacts["manifest.json"] = json_text(manifest)
    scratch.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=scratch, prefix="pipelay-") as temporary:
        staging = Path(temporary)
        for name, content in artifacts.items():
            (staging / name).write_bytes(content.encode("utf-8"))
        destination = Path(output_dir).resolve()
        destination.mkdir(parents=True, exist_ok=True)
        for name in artifacts:
            (destination / name).write_bytes((staging / name).read_bytes())
    return manifest


def main() -> None:
    """Parse the output and mandatory external-scratch locations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    parser.add_argument("--scratch", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.scratch)


if __name__ == "__main__":
    main()
