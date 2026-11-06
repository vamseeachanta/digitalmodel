# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build a self-contained hull library interface and synthetic panel analysis.

Run from the repository root with its dependencies and PYTHONPATH=src:
    python docs/reports/modules/hull_library/build_report.py --output-dir "$TMPDIR/hull-report"
"""

from __future__ import annotations

import argparse
import ast
import copy
import html
import json
import math
import re
from hashlib import sha256
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_scaler import (
    scale_mesh_parametric,
    scale_mesh_uniform,
)

ROOT = Path(__file__).resolve().parents[4]
PACKAGE = "src/digitalmodel/hydrodynamics/hull_library"
EXAMPLE = f"{PACKAGE}/docs/worked-panel-scaling.md"
EXEMPLAR = "docs/reports/pipeline-ffs-assessment.html"
REVISION = "00"
DATE = "2026-10-28"  # Document issue date; never an execution timestamp.
REBUILD = ('PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 MPLCONFIGDIR="$TMPDIR/matplotlib" '
           'XDG_CACHE_HOME="$TMPDIR/xdg-cache" uv run --no-project --python "$(command -v python3)" python '
           'docs/reports/modules/hull_library/build_report.py --output-dir "$TMPDIR/hull-report"')
CHANGES = {
    "report": "hull-library-panel-scaling", "revision": REVISION, "date": DATE,
    "issue": "3317",
    "changes": [
        "Added an interface evidence map and synthetic panel analysis to separate implementation from numerical qualification.",
        "Bound visible results to fresh scaling calculations, independent geometry and reproducible source records.",
    ],
}
# All geometry and factors are synthetic assumptions from EXAMPLE, Inputs.
# One model length unit is assigned one metre; this is not measured geometry.
VERTICES = [[1, 2, -3], [5, 2, -3], [5, 5, 1], [1, 5, 1]]
REFERENCE = [2, -1, 3]
FACTORS = {"source": [1, 1, 1], "uniform": [2, 2, 2], "parametric": [3, 2, 1 / 2]}
UNITS = {
    "factors": "1", "vertices": "m", "reference_point": "m", "panels": "index",
    "edge_ab": "m", "edge_ad": "m", "edge_lengths": "m", "normal": "1",
    "center": "m", "panel_area": "m^2", "total_area": "m^2",
    "n_vertices": "count", "n_panels": "count",
}
EQUATIONS = {
    "factors": "E1", "vertices": "E1", "reference_point": "E1", "panels": "E5",
    "edge_ab": "E2", "edge_ad": "E2", "edge_lengths": "E2", "normal": "E3",
    "center": "E4", "panel_area": "E2", "total_area": "E2",
    "n_vertices": "E5", "n_panels": "E5",
}
LIMITS = ("physical hull suitability", "hydrodynamic responses", "response-table provenance",
          "mesh convergence", "solver qualification")


def derive(factors: list[float]) -> dict:
    """Derive this rectangle from perpendicular edges, without mesh routines."""
    sx, sy, sz = factors
    # EXAMPLE, Derivation: AB=(4,0,0), AD=(0,3,4), so AB' dot AD'=0.
    length = 4 * sx
    width = math.sqrt((3 * sy) ** 2 + (4 * sz) ** 2)
    return {
        "factors": factors,
        "vertices": [[p[i] * factors[i] for i in range(3)] for p in VERTICES],
        "reference_point": [REFERENCE[i] * factors[i] for i in range(3)],
        "panels": [[0, 1, 2, 3]],
        "edge_ab": [length, 0, 0], "edge_ad": [0, 3 * sy, 4 * sz],
        "edge_lengths": [length, width],
        "normal": [0, -4 * sz / width, 3 * sy / width],
        "center": [3 * sx, 3.5 * sy, -sz],
        "panel_area": length * width, "total_area": length * width,
        "n_vertices": len(VERTICES), "n_panels": 1,
    }


def snapshot(mesh: PanelMesh, factors: list[float]) -> dict:
    """Collect actual mesh quantities before formatting or comparison."""
    ab = mesh.vertices[1] - mesh.vertices[0]
    ad = mesh.vertices[3] - mesh.vertices[0]
    return {
        "factors": factors, "vertices": mesh.vertices.tolist(),
        "reference_point": list(mesh.reference_point), "panels": mesh.panels.tolist(),
        "edge_ab": ab.tolist(), "edge_ad": ad.tolist(),
        "edge_lengths": [float(np.linalg.norm(ab)), float(np.linalg.norm(ad))],
        "normal": mesh.normals[0].tolist(), "center": mesh.panel_centers[0].tolist(),
        "panel_area": float(mesh.panel_areas[0]), "total_area": mesh.total_area,
        "n_vertices": mesh.n_vertices, "n_panels": mesh.n_panels,
    }


def differences(actual, expected):
    """Retain unrounded absolute differences with the same scalar structure."""
    if isinstance(actual, dict):
        return {k: differences(v, expected[k]) for k, v in actual.items()}
    if isinstance(actual, list):
        return [differences(a, e) for a, e in zip(actual, expected, strict=True)]
    return abs(actual - expected)


def calculate() -> dict:
    """Execute only in-memory panel construction and the two scaling calls."""
    source = PanelMesh(vertices=np.array(VERTICES, dtype=np.float64),
                       panels=np.array([[0, 1, 2, 3]], dtype=np.int32),
                       reference_point=REFERENCE.copy(), name="synthetic_rectangle",
                       metadata={"basis": "synthetic assumptions"})
    before = copy.deepcopy(source.__dict__)
    cases = {"source": snapshot(source, FACTORS["source"])}
    cases["uniform"] = snapshot(scale_mesh_uniform(source, FACTORS["uniform"][0]), FACTORS["uniform"])
    cases["parametric"] = snapshot(scale_mesh_parametric(source, *FACTORS["parametric"]), FACTORS["parametric"])
    unchanged = all(np.array_equal(value, source.__dict__[key]) if isinstance(value, np.ndarray)
                    else value == source.__dict__[key] for key, value in before.items())
    reference = {name: derive(factors) for name, factors in FACTORS.items()}
    return {"units": UNITS.copy(), "cases": cases, "reference": reference,
            "difference": differences(cases, reference), "source_unchanged": unchanged}


def method_map() -> list[dict]:
    """Map every package export without executing any geometry or lookup route."""
    tree = ast.parse((ROOT / PACKAGE / "__init__.py").read_text(encoding="utf-8"))
    exports = next(ast.literal_eval(node.value) for node in tree.body
                   if isinstance(node, ast.Assign) and any(isinstance(t, ast.Name) and t.id == "__all__"
                                                          for t in node.targets))
    locations = {alias.asname or alias.name: f"{PACKAGE}/{node.module.replace('.', '/')}.py"
                 for node in tree.body if isinstance(node, ast.ImportFrom) and node.level == 1
                 for alias in node.names}
    return [{"name": name, "path": locations[name], "implemented": True,
             "checked": "INDEPENDENTLY_CHECKED" if name in {
                 "scale_mesh_uniform", "scale_mesh_parametric"} else "NOT_EVALUATED"}
            for name in exports]


def source_record() -> dict:
    """Bind repository source bytes; public or measured response data are absent."""
    paths = {row["path"] for row in method_map()} | {
        f"{PACKAGE}/__init__.py", f"{PACKAGE}/docs/agent-pack.md", EXAMPLE, EXEMPLAR,
        "src/digitalmodel/hydrodynamics/bemrosetta/models/mesh_models.py",
        "tests/hydrodynamics/hull_library/test_documented_panel_scaling.py",
        "tests/hydrodynamics/hull_library/test_mesh_scaler.py",
        "tests/hydrodynamics/hull_library/test_module_report.py",
        "docs/reports/modules/hull_library/build_report.py",
    }
    return {"report": CHANGES["report"], "revision": REVISION,
            "data_basis": "Synthetic assumptions only; no response datasets are consumed.",
            "sources": [{"path": path, "sha256": sha256((ROOT / path).read_bytes()).hexdigest(),
                         "basis": "synthetic input and hand derivation" if path == EXAMPLE
                         else "repository implementation or documentation; not physical qualification",
                         "locator": "Inputs; Derivation; Tolerance basis" if path == EXAMPLE
                         else "public interfaces and report evidence"} for path in sorted(paths)]}


def flatten(value, prefix=""):
    """Enumerate scalar result paths for visible comparison tables."""
    if isinstance(value, (dict, list)):
        for key, child in (value.items() if isinstance(value, dict) else enumerate(value)):
            yield from flatten(child, f"{prefix}.{key}" if prefix else str(key))
    else:
        yield prefix, value


def rounded(value):
    """Embed at displayed precision; the calculation API retains full precision."""
    if isinstance(value, dict):
        return {k: rounded(v) for k, v in value.items()}
    if isinstance(value, list):
        return [rounded(v) for v in value]
    return round(value, 1) if isinstance(value, float) else value


def safe_json(value) -> str:
    """Prevent embedded data from terminating its inert JSON script element."""
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, allow_nan=False)
            .replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
            .replace("\u2028", "\\u2028").replace("\u2029", "\\u2029"))


def link(path: str, label: str | None = None) -> str:
    """Resolve authored sources from the delivered repository report folder."""
    return f'<a href="../../../../{html.escape(path, quote=True)}">{html.escape(label or path)}</a>'


def table(headers, rows, caption) -> str:
    """Render trusted cell markup; arbitrary source text is escaped by callers."""
    return ('<div class="table-wrap"><table><thead><tr>'
            + "".join(f'<th scope="col">{html.escape(h)}</th>' for h in headers)
            + "</tr></thead><tbody>"
            + "".join("<tr>" + "".join(f"<td>{c}</td>" for c in row) + "</tr>" for row in rows)
            + f"</tbody><caption>{html.escape(caption)}</caption></table></div>")


def quantities(record: dict) -> str:
    """Bind every displayed scalar to its computed, reference or difference record."""
    rows = []
    references = dict(flatten(record["reference"]))
    delta = dict(flatten(record["difference"]))
    for key, computed in flatten(record["cases"]):
        metric = key.split(".")[1]
        unit, equation = record["units"][metric], EQUATIONS[metric]
        digits = 0 if unit in {"index", "count"} else 1
        cells = [html.escape(key)]
        for group, value in (("cases", computed), ("reference", references[key]), ("difference", delta[key])):
            cells.append(f'<span data-value="{group}.{key}" '
                         f'data-unit="{html.escape(unit)}" data-equation="{equation}" '
                         f'data-source="synthetic-panel">{value:.{digits}f}</span>')
        rows.append(cells + [html.escape(unit), f'<a href="#{equation}">{equation}</a>; '
                            '<a href="#synthetic-panel">synthetic basis</a>'])
    return table(["Case and quantity (zero-based component)", "Module result", "Hand calculation",
                  "Absolute difference", "Unit", "Equation and source"], rows,
                 "Table 5.1. All synthetic example scalars; comparisons use unrounded values before display rounding.")


def build(record: dict, *, changes: dict | None = None) -> str:
    """Render supplied results, never substitute cached or hand-entered output."""
    changes = CHANGES if changes is None else changes
    titles = ["Executive Summary", "Introduction", "Data Basis", "Theory and Equations",
              "Worked Example", "Validation Evidence", "Limits of Use", "Recommendations", "References", "Appendix"]
    style = re.search(r"<style>(.*?)</style>", (ROOT / EXEMPLAR).read_text(encoding="utf-8"), re.S)[1]
    nav = "".join(f'<li><a href="#section-{i}">{title}</a></li>' for i, title in enumerate(titles, 1))
    revisions = table(["Revision", "Date", "What changed and why"], [[html.escape(changes["revision"]),
                       html.escape(changes["date"]), html.escape(" ".join(changes["changes"]))]], "Revision history")
    methods = table(["Public interface", "Implementation", "Independent evidence in this report"],
                    [[link(row["path"], row["name"]), "IMPLEMENTED", row["checked"]]
                     for row in method_map()], "Table 2.1. Export inventory; implementation alone does not establish qualification.")
    limits = "".join(f"<li>{limit}: NOT_EVALUATED</li>" for limit in LIMITS)
    change_panel = "".join(f"<li>{html.escape(item)}</li>" for item in changes["changes"])
    sources = source_record()
    source_table = table(["Source", "Use and locator", "SHA-256"], [[link(s["path"]),
                         html.escape(s["basis"] + "; " + s["locator"]), f'<code>{s["sha256"]}</code>']
                         for s in sources["sources"]], "Table 9.1. Source-byte bindings; no external response dataset is used.")
    unchanged = "unchanged" if record["source_unchanged"] else "CHANGED: investigate source mutation"
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Hull library: interfaces and synthetic panel scaling analysis</title><style>{style}</style></head>
<body class="hs-doc"><header><h1>Hull library: interfaces and synthetic panel scaling analysis</h1>
<p>Draft for maintainer review</p><p>Revision {html.escape(changes["revision"])} · {html.escape(changes["date"])}</p></header>
<div class="layout"><nav aria-label="Contents"><ol>{nav}</ol></nav><main>
<section id="section-1"><h2>1. Executive Summary</h2>
<p>The synthetic rectangle is evaluated for origin-based uniform and independent-axis panel scaling.
The module quantities are compared with perpendicular-edge hand calculations in the worked example table.
This is a bounded numerical example; the package's wider interfaces remain NOT_EVALUATED here.</p>
<p>Reproducibility does not establish physical validation. The next engineering step is a separately scoped
qualification with a declared physical geometry, response provenance and convergence criterion.</p></section>
<div id="revision-history"><h3>Revision history</h3>{revisions}</div>
<section id="section-2"><h2>2. Introduction</h2>
<p>The source-linked inventory separates implemented interfaces from independently checked methods.
INDEPENDENTLY_CHECKED applies only to the stated rectangle, positive factors and floating-point budget.</p>
<p>HullProfile and HullStation describe offsets and dimensions; HullMeshGenerator.generate accepts a profile
and MeshGeneratorConfig and returns PanelMesh. HullLookup.get_hull_form returns a dimensional match, not a response.
RaoRegistry.get_raos returns references; load_rao_data checks stored integrity. RAODatabase.query filters stored entries.
HullCatalog.compute_motions and compute_accelerations consume response data. These routes are documented by source inspection;
no hull is generated and no response lookup or response calculation is executed for this report.</p>{methods}</section>
<section id="section-3"><h2>3. Data Basis</h2>
<p id="synthetic-panel">All coordinates, connectivity, reference points and factors are synthetic assumptions from
{link(EXAMPLE, 'the preserved worked example, Inputs and Derivation')}. Model coordinates are assigned metres;
areas use square metres. Factors and normals are dimensionless. Connectivity uses ordered, zero-based indices.</p>
<p>The complete input and output values are bound in Table 5.1: source.vertices, source.reference_point,
source.panels and the factors in each case. The source factor is the identity transformation.
Reference points are scaled about the Cartesian origin; they are not scaling pivots.</p>
<p>No measured, historical or response-table data are consumed. No physical constants or standards tables are needed.
The tilted rectangle is planar with perpendicular edges; it does not represent a closed wetted hull.</p></section>
<section id="section-4"><h2>4. Theory and Equations</h2>
<p>The equations follow Cartesian geometry in {link(EXAMPLE, 'the existing Derivation section')}.
The independent calculation uses edge lengths, while PanelMesh computes the sum of triangle areas.</p>
<p id="E1">E1: P′ = diag(sx, sy, sz) P and R′ = diag(sx, sy, sz) R.</p>
<p id="E2">E2: AB′ = B′ − A′, AD′ = D′ − A′; AB′ · AD′ = 0.
Length = sqrt(edge · edge). Area = |AB′| |AD′|. Total area is the sum of panel areas.</p>
<p id="E3">E3: n′ = (AB′ × AD′) / |AB′ × AD′|, using the specified corner winding.</p>
<p id="E4">E4: centre = arithmetic mean of the corner coordinates.</p>
<p id="E5">E5: connectivity is copied; vertex and panel counts are preserved.</p>
<p id="roundoff">The comparison budget is 64ε max(1, |q|), in the unit of each scalar q,
where ε is binary64 machine epsilon. The dyadic coordinate products and edge differences are exact at these magnitudes.
Cross products, squared norms, square roots, the triangle sum and normal division require fewer than 64 scalar operations;
the nonzero components have no subtractive cancellation for this rectangle. The operation allowance supplies a
roundoff budget for this example, not a fabrication tolerance or a hydrodynamic acceptance criterion.
Connectivity and counts are checked exactly. No wider coordinate range is qualified.</p></section>
<section id="section-5"><h2>5. Worked Example</h2>
<p>The module result, independent calculation and absolute difference are shown for every component.
Vertices, reference points, edges, edge lengths, normal, centre, panel area, total area, factors and topology are included.
Scalar components follow Cartesian axis order; vertices follow the specified corner order.
The parametric area follows perpendicular-edge lengths rather than a product of only the horizontal scale factors.</p>
{quantities(record)}<p>Source arrays and attributes after the scaling calls: {unchanged}.</p></section>
<section id="section-6"><h2>6. Validation Evidence</h2>
<p>{link('tests/hydrodynamics/hull_library/test_module_report.py', 'Report tests')} compare unrounded quantities
with an independent Cartesian oracle under the stated roundoff budget and check the displayed units and equations.
Fresh public calls corroborate the computed record. A controlled area mutation checks that visible results and differences change.
Two output directories are rebuilt and compared byte for byte with the delivered HTML, source and change records.</p>
<p>{link('tests/hydrodynamics/hull_library/test_documented_panel_scaling.py', 'Existing documented example tests')}
and {link('tests/hydrodynamics/hull_library/test_mesh_scaler.py', 'existing scaler tests')} remain separate regression coverage.
The test definitions are evidence locators; live execution counts are reported with the change, rather than generated as an unobserved pass claim.</p>
<p>Output comparisons use one decimal place; topology and counts remain integers. Embedded calculation data use the same
display precision. Full-precision assertions use calculate() results before rendering. Rounded equality alone does not satisfy the numerical criterion.</p></section>
<section id="section-7"><h2>7. Limits of Use</h2><ul>{limits}</ul>
<p>Geometric scaling does not scale hydrodynamic responses. Response frequency, heading, amplitude, phase, coordinate-frame
and loading-condition conventions require independent evidence before reuse. No response values are obtained here.</p>
<p>Mesh generation, refinement, topology repair, geometry kernels, external meshing backends and solver execution are outside
this worked computation. Existing tests of those interfaces are not adopted as numerical qualification in this report.</p></section>
<section id="section-8"><h2>8. Recommendations</h2>
<p>A separate qualification should declare geometry provenance, loading condition, units and coordinate conventions,
compare mesh properties against an independent geometry, and prescribe convergence and response acceptance criteria
before any engineering response is adopted. Backend selection and solver qualification require separately routed work.</p>
<p>Source-review findings: validate_scaled_mesh uses centroid-radial normal checks and an origin-relative draft estimate;
these do not establish outward winding of a general hull. Shallow metadata copies do not promise nested independence after later edits.
Dimensional nearest matching and response filtering do not establish physical suitability. These concerns are recorded for broader qualification;
production behavior is unchanged. See {link(f'{PACKAGE}/mesh_scaler.py', 'scaling and validation interfaces')} and
{link(f'{PACKAGE}/docs/agent-pack.md', 'module evidence and known limits')}.</p></section>
<section id="section-9"><h2>9. References</h2><p>Sources are repository-relative and hash-bound in sources.json.
The example is synthetic; implementation references are not independent physical observations.</p>{source_table}</section>
<section id="section-10"><h2>10. Appendix</h2>
<h3>Rebuild</h3><p>From the repository root, with its Python dependencies active and supplied scratch storage:</p>
<pre><code>{html.escape(REBUILD)}</code></pre>
<p>The explicit output directory receives only report.html, changes.json and sources.json. No intermediates are written.
The report is self-contained; source links resolve from its repository report directory. No network resources are loaded.</p>
<h3>Engineering changes</h3><details id="engineering-changes"><summary>Changes in this revision</summary><ul>{change_panel}</ul></details>
<p>Readiness: independent checking is limited to the synthetic panel. The broader module qualification remains open.</p></section>
</main></div><script type="application/json" id="calculation-record">{safe_json({'calculation': rounded(record), 'changes': changes})}</script>
</body></html>
'''


def write_report(output_dir: Path) -> None:
    """Write exactly the deliverables into an explicitly supplied directory."""
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(build(calculate()), encoding="utf-8", newline="\n")
    for name, value in (("changes.json", CHANGES), ("sources.json", source_record())):
        (output_dir / name).write_text(json.dumps(value, ensure_ascii=False, indent=2, allow_nan=False) + "\n",
                                       encoding="utf-8", newline="\n")


def main() -> None:
    """Require an explicit destination for deterministic offline generation."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    write_report(parser.parse_args().output_dir)


if __name__ == "__main__":
    main()
