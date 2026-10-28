# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the bounded parachute method inventory and synthetic arithmetic report.

Both --scratch (outside the checkout) and --output-dir must be explicit.
Normal public imports require NumPy and PyYAML. No CAD or transient API is run.
"""
import argparse
import ast
from dataclasses import asdict
import hashlib
import html
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/structural/parachute"
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
WORKED = f"{PACKAGE}/docs/worked-synthetic-canopy-drag.md"
FRAME_TEST = "tests/structural/parachute/test_frame_solver.py"
REVIEWED_MODULES = ["parachute_drag", "frame_model", "frame_geometry_3d", "frame_solver", "member_check"]
STYLE = ["docs/api/_assets/brand.css", "docs/api/cfd/turbulent-flat-plate-verification.html"]
SOURCES = [f"{PACKAGE}/{name}.py" for name in REVIEWED_MODULES] + [
    WORKED, FRAME_TEST, "tests/reporting/test_parachute_module_report.py", *STYLE,
]
CHANGES = [{"revision": "A", "date": "2026-10-28", "change":
            "Initial method inventory and synthetic drag and cantilever calculations establish arithmetic evidence and explicit limits."}]
INPUTS = {
    # Synthetic assumptions: WORKED input table and FRAME_TEST TestCantileverBeam.
    "drag": {"chute_diameter_ft": 5.0, "cd": 1.1, "cx": 1.25, "rho": 0.0023,
             "speeds_mph": [40.0, 60.0]},
    "cantilever": {"length_in": 100.0, "E_psi": 29e6, "I_in4": 10.0,
                   "A_in2": 5.0, "tip_fy_lbf": -1000.0},
}


def json_text(value):
    """Canonical evidence encoding; no paths, clocks or non-finite numbers."""
    return json.dumps(value, sort_keys=True, indent=2, ensure_ascii=False, allow_nan=False) + "\n"


def calculate(scratch):
    """Execute only the normal public drag and linear elastic solver APIs."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    from digitalmodel.structural.parachute.parachute_drag import calculate_drag_force
    from digitalmodel.structural.parachute.frame_solver import solve_frame

    drag_input = {key: value for key, value in INPUTS["drag"].items() if key != "speeds_mph"}
    drag = [asdict(calculate_drag_force(speed_mph=speed, **drag_input))
            for speed in INPUTS["drag"]["speeds_mph"]]
    frame = INPUTS["cantilever"]
    result = solve_frame(
        nodes={0: (0.0, 0.0), 1: (frame["length_in"], 0.0)},
        members=[{"id": 0, "nodes": (0, 1), "section": {"A": frame["A_in2"], "I": frame["I_in4"]}}],
        fixed_nodes=[0], forces={1: (0.0, frame["tip_fy_lbf"], 0.0)}, E=frame["E_psi"],
    )
    data = {"inputs": INPUTS, "drag": drag, "cantilever": {
        "tip_uy_in": float(result.displacements[4]),
        "root_fy_lbf": float(result.reactions[0]["fy"]),
        "root_mz_lbf_in": float(result.reactions[0]["mz"]),
    }}
    # Calculation evidence is retained externally; only deterministic deliverables
    # are written to the selected output directory by build().
    (scratch / "calculation.json").write_text(json_text(data), encoding="utf-8", newline="\n")
    return data


def source_link(path, label=None):
    """Link a source within the repository from the delivered report folder."""
    return f'<a href="../../../../{html.escape(path, quote=True)}">{html.escape(label or path)}</a>'


def inventory():
    """Read callable names and parameters without executing geometry factories."""
    units = {
        "parachute_drag": "mph, ft, slug/ft³ and dimensionless coefficients → DragResult (lbf, N, ft/s, ft²)",
        "frame_model": "tube dimensions in → section A, I, S, r (in², in⁴, in³, in); factory → FrameModel with nodes, members, restraints and material dictionary",
        "frame_geometry_3d": "coordinates and section dimensions in → geometry records; queries → filtered records, node identifiers or chord length in",
        "frame_solver": "nodes in, forces lbf, moments lbf in, E psi, A in², I in⁴ → FrameResult (translations in, rotations rad, reactions and member forces)",
        "member_check": "forces lbf, moments lbf in, section dimensions in, supplied stress limits psi → stress dictionaries psi, dimensionless unity ratios and Boolean outcomes",
    }
    rows = []
    for name in REVIEWED_MODULES:
        path = f"{PACKAGE}/{name}.py"
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        callables = []
        for node in tree.body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                args = ", ".join(arg.arg for arg in node.args.args)
                callables.append(f"{node.name}({args}) [line {node.lineno}]")
            elif isinstance(node, ast.ClassDef):
                callables.append(f"{node.name} [line {node.lineno}]")
                for method in node.body:
                    if isinstance(method, ast.FunctionDef) and not method.name.startswith("_"):
                        args = ", ".join(arg.arg for arg in method.args.args if arg.arg != "self")
                        callables.append(f"{node.name}.{method.name}({args}) [line {method.lineno}]")
        entries = "<br>".join(html.escape(item) for item in callables)
        rows.append(f'<tr><td>{source_link(path, name)}</td><td><code>{entries}</code></td>'
                    f'<td>{html.escape(units[name])}</td></tr>')
    return "".join(rows)


def render(data):
    """Render all result cells from returned values, with local generated figures."""
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / STYLE[0]).read_text(encoding="utf-8"), flags=re.S)
    exemplar = (ROOT / STYLE[1]).read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    # Keep delivered CSS stable under the repository whitespace checks.
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    layout = "\n".join(line.rstrip() for line in layout.splitlines())
    history = "".join('<tr>' + ''.join(f'<td>{html.escape(row[key])}</td>'
                      for key in ("revision", "date", "change")) + '</tr>' for row in CHANGES)
    drag_rows = "".join('<tr>' + ''.join(f'<td>{row[key]:.1f}</td>' for key in
                        ("speed_mph", "speed_fps", "chute_area_ft2", "force_lbs", "force_n"))
                        + '<td>[D, W]</td></tr>' for row in data["drag"])
    frame_rows = "".join(f'<tr><td>{label}</td><td>{data["cantilever"][key]:.1f}</td>'
                         f'<td>{unit}</td><td>[S, H]</td></tr>' for key, label, unit in [
                             ("tip_uy_in", "Tip vertical displacement", "in"),
                             ("root_fy_lbf", "Root vertical reaction", "lbf"),
                             ("root_mz_lbf_in", "Root reaction moment (positive counterclockwise)", "lbf in"),
                         ])
    # SVG coordinates are page-layout units, not additional engineering data.
    bars = "".join(f'<rect x="180" y="{45 + i * 65}" width="{row["force_n"] / max(r["force_n"] for r in data["drag"]) * 400:.1f}" height="30" fill="currentColor"/>'
                   f'<text x="10" y="{65 + i * 65}">{row["speed_mph"]:.1f} mph</text>'
                   f'<text x="590" y="{65 + i * 65}">{row["force_n"]:.1f} N [D, W]</text>'
                   for i, row in enumerate(data["drag"]))
    references = "".join(f'<li>{source_link(path)}; SHA-256: '
                         f'<code>{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</code></li>'
                         for path in SOURCES)
    payload = json_text(data).replace("<", "\\u003c").replace("&", "\\u0026")
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Parachute module analysis</title><style>{brand}\n{layout}</style></head>
<body class="hs-doc"><main class="wrap">
<header><h1>Parachute module analysis</h1><p>Existing methods and synthetic arithmetic evidence</p></header>
<nav aria-label="Evidence"><a href="results.json">Unrounded results</a> · <a href="manifest.json">Byte manifest</a> · <a href="README.md">Rebuild instructions</a> · <a href="#changes-panel">Change history</a></nav>
<h2 id="s1">1. Summary</h2>
<p>The existing drag law and linear frame solver reproduce independent arithmetic for the synthetic inputs below. The computed force and signed displacement/reaction tables come from normal public API calls [D, S]. This is an existing-method inventory, not equipment qualification. Physical response and transient deployment are NOT_EVALUATED.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<h2 id="s2">2. Context and scope</h2>
<p>Reviewed source revision: <code>{REVIEWED_REVISION}</code>. Source digests below and the manifest identify the exact reviewed files and delivery bytes. Public drag, geometry, linear solver and member-check interfaces are inventoried from source. Only the drag and synthetic elastic cantilever cases are executed. Geometry factories, member acceptance policies, adjacent sizing/assessment utilities, CAD and viewers are not exercised; no product selection or deployment recommendation follows.</p>
<table><thead><tr><th>Source</th><th>Public records, functions and methods (parameter names)</th><th>Input → output contract and units</th></tr></thead><tbody>{inventory()}</tbody></table>
<p>Generated dataclass constructors follow the record fields in the cited source. Node identifiers and connectivity are bookkeeping, not dimensional inputs. Geometry records describe coordinates and connectivity; they do not make the planar solver a spatial solver. The public frame solver returns displacements ordered by sorted node identifiers, restrained-node reactions and local member-end forces [S].</p>
<h2 id="s3">3. Input basis</h2>
<p>[W] is the existing synthetic drag document; [T] is the existing cantilever test fixture. These are software examples, not measured or qualified data. All empirical drag inputs are assumed, based on previous experience, as labelled in [W]. The elastic properties below are synthetic assumptions from [T], not a specified material.</p>
<table><thead><tr><th>Input</th><th>Value and units</th><th>Basis</th></tr></thead><tbody>
<tr><td>Projected circular diameter</td><td>5 ft</td><td>Synthetic geometric assumption [W]; not a fabric diameter</td></tr>
<tr><td>Relative flow speeds</td><td>40 and 60 mph</td><td>Synthetic assumptions [W]</td></tr>
<tr><td>Mass density</td><td>0.0023 slug/ft³</td><td>Assumed, based on previous experience [W]</td></tr>
<tr><td>Drag coefficient</td><td>1.1 (dimensionless)</td><td>Assumed, based on previous experience [W]; projected-disk reference</td></tr>
<tr><td>Opening multiplier</td><td>1.25 (dimensionless)</td><td>Assumed, based on previous experience [W]; algebraic, not transient</td></tr>
<tr><td>Cantilever length L</td><td>100 in</td><td>Synthetic assumption [T]</td></tr>
<tr><td>Elastic modulus E</td><td>29e6 psi</td><td>Synthetic assumption [T]</td></tr>
<tr><td>Section inertia I and area A</td><td>10 in⁴ and 5 in²</td><td>Synthetic assumptions [T]; no tube section is inferred</td></tr>
<tr><td>Tip vertical force P</td><td>-1000 lbf</td><td>Synthetic assumption [T]; upward is positive</td></tr>
<tr><td>Restraints and other applied loads</td><td>Zero root translations and rotation; zero other forces and moments</td><td>Ideal fixed root and isolated tip loading [T, H]</td></tr>
</tbody></table>
<details><summary>Machine-readable synthetic input register</summary><pre>{html.escape(json_text(data["inputs"]))}</pre></details>
<h2 id="s4">4. Method</h2>
<p>[D] computes F = ρ V² Cd A Cx / 2 and A = π(d/2)². The projected area follows ∫2πr dr over the disk radius [W]; π is the mathematical circle constant and the integers are algebraic factors. The drag equation source in [W] is the NASA drag equation; Cx is the caller's extra multiplier. With coherent customary units, slug ft/s² is lbf. The N output uses the rounded conversion 4.44822 N/lbf [D].</p>
<p>[S] assembles linear axial and Euler–Bernoulli bending stiffness, transforms local to global coordinates, restrains all degrees of freedom at fixed nodes, solves K u = F, then recovers R = K u − F and local member-end forces. The coordinates, forces and elastic properties must be unit consistent. This report uses in, lbf and psi throughout the frame case. It does not infer geometric nonlinearity, shear deformation or material plasticity.</p>
<figure><svg viewBox="0 0 760 170" role="img" aria-label="Synthetic fixed-root cantilever and downward tip force; schematic only">
<path d="M100 30 V125 M85 35 L100 50 M85 60 L100 75 M85 85 L100 100 M100 75 H620 M620 75 V125 M610 115 L620 125 L630 115" fill="none" stroke="currentColor"/>
<text x="120" y="45">Fixed root: v = slope = zero [H]</text><text x="330" y="100">L = 100 in [T]</text><text x="500" y="150">P = -1000 lbf [T]</text>
</svg><figcaption>Generated schematic of the synthetic boundary conditions; not to scale [T, H].</figcaption></figure>
<h2 id="s5">5. Worked calculation</h2>
<p>The independent SI route in [W] uses exact definitions: ft = 0.3048 m, mile = 1609.344 m, hour = 3600 s and lbf = 4.4482216152605 N. The coherent mass conversion is slug = (4.4482216152605 / 0.3048) kg. Density becomes 0.0023 × (4.4482216152605 / 0.3048) / 0.3048³ kg/m³. Integrating the disk gives A = π(5 × 0.3048 / 2)² m². For each assumed speed, V = speed × 1609.344 / 3600 m/s and F = ρ V² × 1.1 × A × 1.25 / 2 N [W]. Dimensions reduce to kg m/s². No production helper is used for this independent calculation.</p>
<p>[H] independently integrates EI v″ = P(L − x). With v(0) = v′(0) = 0, EI v′ = P(Lx − x²/2) and EI v = P(Lx²/2 − x³/6). Thus v(L) = P L³/(3 E I) = -100/87 in for [T]. Force balance gives root vertical force −P = +1000 lbf; moment balance gives root moment −PL = +100000 lbf in (counterclockwise), of the same magnitude. These factors and zero boundary values are algebraic identities and idealized restraints, not empirical constants.</p>
<h2 id="s6">6. Checks</h2>
<p>The independent report tests [Q] compare unrounded normal API and delivered values with [W, H]. Relative tolerance is 5e-7 for N, covering the rounded production N/lbf factor; 1e-12 for lbf, area, speed and the speed-square force ratio, covering floating-point roundoff; and 1e-10 for the signed cantilever displacement and reactions in this small well-scaled linear system. Absolute tolerance is zero. These are arithmetic consistency tolerances, not physical accuracy estimates.</p>
<p>With all other drag inputs fixed, F(60)/F(40) = (60/40)² = 9/4 [W]. Negative controls alter a result value or sign, regenerate an internally consistent report and manifest, and require the independent numerical assertions to fail [Q]. Separate exact production replay is only a freshness check. Two clean external builds and an independent readback of all manifest digests check reproducibility; changing a delivered result fails the byte binding even before the numerical oracle is applied [Q].</p>
<h2 id="s7">7. Results</h2>
<p>Every cell below is formatted from a returned public API value [D, S]. Results are displayed to one decimal place; unrounded values are retained in <a href="results.json">results.json</a> and embedded below for audit. No expected force is taken from a rounded display.</p>
<table><thead><tr><th>Speed (mph)</th><th>Speed (ft/s)</th><th>Disk area (ft²)</th><th>Force (lbf)</th><th>Force (N)</th><th>Basis</th></tr></thead><tbody>{drag_rows}</tbody></table>
<figure><svg viewBox="0 0 760 180" role="img" aria-label="Computed synthetic drag forces at the assumed speeds">{bars}</svg><figcaption>Generated force bars, with lengths scaled to the largest returned force; no external assets [D, W].</figcaption></figure>
<table><thead><tr><th>Cantilever output</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead><tbody>{frame_rows}</tbody></table>
<details><summary>Unrounded public API calculation data</summary><pre>{html.escape(json_text(data))}</pre></details>
<h2 id="s8">8. Limits / NOT_EVALUATED</h2>
<p>Physical canopy coefficients, inflation history, fluid–structure coupling, transient peak loading, physical frame geometry, material properties, connections, buckling, fatigue and operational qualification are NOT_EVALUATED. The synthetic elastic case does not qualify a physical material or frame. Drag and cantilever loads are independent demonstrations; this report does not transfer the computed drag to an actual structure.</p>
<p>Open method findings from source: the drag API accepts non-positive diameter as zero area while the area helper rejects it, and finite-value/coefficient/density validation is incomplete [D]. The frame solver requires a restrained, nonsingular, unit-consistent model and performs no general conditioning assessment [S]. No broader input-validity claim is made.</p>
<p>Member checks are inventoried only. The source uses a fixed yield-based allowable multiplier, a thin-wall average shear approximation, a supplied common section/material for all members and maxima of absolute end forces; it does not establish a general acceptance policy, stress interaction envelope or connection qualification. Invalid section values can produce zero stress or non-finite unity values [M]. These are open findings requiring a separate method and acceptance-basis review, not algorithm replacements in this report. Existing built-in geometry/material defaults are not endorsed or used.</p>
<h2 id="s9">9. Way forward</h2>
<p>Retain the arithmetic checks when revising the existing methods. Review finite-input contracts, diameter handling, conditioning and member acceptance policy separately before extending claims. Any physical assessment requires independently established geometry, loads, material data, response evidence and acceptance criteria. Qualification, standards basis and unqualified parts of the module-report request remain open; no equipment recommendation is made.</p>
<h2 id="s10">10. References and evidence</h2>
<ul><li>[D] {source_link(f"{PACKAGE}/parachute_drag.py")} — drag public API, returned units and rounded force conversion.</li>
<li>[W] {source_link(WORKED)} — synthetic assumptions, independent SI route and exact NIST definitions (SP 811 Appendix B.8, Chapter 5 Table 6, footnote 23 and Chapter 4 Table 3); NASA drag-equation source is linked there. External documents are cited through this existing source register, not newly qualified physical data.</li>
<li>[S] {source_link(f"{PACKAGE}/frame_solver.py")} — linear element, assembly, restraint and reaction methods.</li>
<li>[T] {source_link(FRAME_TEST)} — TestCantileverBeam synthetic fixture.</li>
<li>[H] Hand derivation in Worked calculation — integrated elastic curve and independent static balance.</li>
<li>[M] {source_link(f"{PACKAGE}/member_check.py")} — existing member and connection formulas, inventory only.</li>
<li>[Q] {source_link("tests/reporting/test_parachute_module_report.py")} — independent oracles, negative controls, local links, manifest and byte rebuild checks.</li></ul>
<p>The exact source register, including the house-style exemplar and canonical palette inlined above, is bound below and in the manifest. Section numbers, revision identifiers and date are document metadata; numerical method inputs and answers use the cited or hand-derived bases above. No external scripts, fonts or images are required.</p><ul>{references}</ul>
<details id="changes-panel"><summary>Change history</summary><pre>{html.escape(json_text(CHANGES))}</pre><a href="changes.json">Machine-readable change history</a></details>
<script id="calculation" type="application/json">{payload}</script>
</main></body></html>
'''


def readme():
    """Return the deterministic delivery guide."""
    return """# Parachute module analysis

[index.html](index.html) inventories existing methods and presents synthetic drag
and elastic cantilever arithmetic. Physical and operational qualification are
`NOT_EVALUATED`. The report does not select equipment or an acceptance basis.

## Rebuild

With NumPy and PyYAML available, run from the repository root with source imports:

```sh
PYTHONPATH=src python scripts/reports/build_parachute_module_report.py --scratch <external-scratch> --output-dir <output-directory>
```

Scratch must be outside the checkout; every intermediate file stays there.
Both arguments are required. Output directories contain only the small report
artifacts. Fixed revision metadata and exact-byte digests exclude execution
timestamps, absolute paths and environment identities.

[results.json](results.json) retains unrounded public API values;
[changes.json](changes.json) matches the report revision history.
[manifest.json](manifest.json) binds the report, results, history and this guide,
the builder and its source evidence using SHA-256. The manifest does not hash
itself. Digests prove byte identity, not physical accuracy or authenticity.

Independent numerical tests use the exact SI definitions in the
[existing worked example](../../../../src/digitalmodel/structural/parachute/docs/worked-synthetic-canopy-drag.md)
and an integrated elastic curve with force/moment balance. Replaying the builder
alone is a freshness check. Test assertions also reject sign/value mutations
even when the regenerated page, result data and digests agree.
"""


def build(scratch, output_dir):
    """Write deterministic deliverables, then bind their exact delivered bytes."""
    data = calculate(scratch)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    outputs = {"index.html": render(data), "results.json": json_text(data),
               "changes.json": json_text(CHANGES), "README.md": readme()}
    for name, content in outputs.items():
        (output_dir / name).write_text(content, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1,
        "digest_convention": "SHA-256 over exact UTF-8 delivered bytes with LF line endings; manifest excludes itself.",
        "reviewed_source_revision": REVIEWED_REVISION,
        "generator": Path(__file__).relative_to(ROOT).as_posix(),
        "generator_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in SOURCES},
        "artifacts": {name: hashlib.sha256((output_dir / name).read_bytes()).hexdigest() for name in outputs},
    }
    (output_dir / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")


def main():
    """Parse explicit calculation and delivery locations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
