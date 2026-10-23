# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a deterministic, offline naval architecture evidence supplement.

Reproduce with PYTHONPATH=src python scripts/reporting/build_naval_evidence_report.py.
The oscillator equation is Mechanics Map (2021), section 15.5:
https://eng.libretexts.org/@go/page/55503
Synthetic sample coordinates are demonstration inputs, not acceptance limits.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import re
from html import escape
from pathlib import Path

from digitalmodel.naval_architecture.compliance import run_compliance_checks
from digitalmodel.naval_architecture.seakeeping import simple_heave_rao

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/navalarchitecture/evidence"
PACKAGE = ROOT / "src/digitalmodel/naval_architecture"
SOURCE = "https://eng.libretexts.org/@go/page/55503"
SECTIONS = ("summary", "scope", "inputs", "method", "worked-example", "pitfalls",
            "limits", "recommendations", "evidence", "symbols")
CHANGES = {
    "document": "naval-architecture-evidence-analysis", "revision": 1,
    "history": [{"revision": 1, "date": "2026-10-22",
                 "change": "Record explicit missing evidence, empty selections and uncapped response for issue #2597."}],
}


def _disposition(path: str, function: str, kind: str) -> str:
    """Classify candidates conservatively; no heuristic claims qualification."""
    filename = Path(path).name
    if kind == "broad_handler":
        return "FOLLOW_UP: geometry metadata or partial-reader fallback; qualify omitted geometry and units"
    if kind == "bare_handler":
        return "FOLLOW_UP: exception evidence requires review"
    if kind == "all":
        if function in ("passed", "ok"):
            return "GUARDED: empty result collections cannot pass"
        if filename == "damage_stability.py":
            return "FIXED_SET: six criteria constructed before reduction"
        if filename == "compliance.py":
            return "GUARDED: curve length validated before finite-value reduction"
        return "STRUCTURAL: geometry or input validation; not a compliance verdict"
    if filename in {"maneuvering_envelope_workflow.py", "loading_computer.py",
                    "workflow_calculations.py"}:
        return "FOLLOW_UP: qualify physical assumptions separately from output and sampling defaults"
    if filename in {"rudder_stock_torque.py", "yaw_moment.py"}:
        return "FOLLOW_UP: distinguish declared hull context from optional artifact settings"
    if filename == "region_report.py":
        return "GUARDED: absent cut evidence produces NOT_EVALUATED note"
    return "REVIEWED: lookup, counter, optional metadata or output control; no direct success verdict"


def sweep() -> list[dict]:
    """Enumerate every explicit mapping default, all reduction and broad handler.

    This inventory is a reproducible candidate search, not a proof about all
    possible numerical defaults. Function-argument defaults are a follow-up.
    No source values or identity-bearing data are copied into the inventory.
    """
    rows = []
    for path in sorted(PACKAGE.rglob("*.py")):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        parents = {child: parent for parent in ast.walk(tree)
                   for child in ast.iter_child_nodes(parent)}
        for node in ast.walk(tree):
            kind = None
            if isinstance(node, ast.Call):
                func = node.func
                if isinstance(func, ast.Attribute) and func.attr == "get" and len(node.args) > 1:
                    kind = "mapping_default"
                elif ((isinstance(func, ast.Name) and func.id == "all")
                      or (isinstance(func, ast.Attribute) and func.attr == "all")):
                    kind = "all"
            elif isinstance(node, ast.ExceptHandler):
                if node.type is None:
                    kind = "bare_handler"
                elif isinstance(node.type, ast.Name) and node.type.id in {"Exception", "BaseException"}:
                    kind = "broad_handler"
            if kind is None:
                continue
            parent = parents.get(node)
            while parent is not None and not isinstance(parent, (ast.FunctionDef, ast.AsyncFunctionDef)):
                parent = parents.get(parent)
            function = parent.name if parent is not None else "module"
            relative = path.relative_to(ROOT).as_posix()
            rows.append({"path": relative, "line": node.lineno, "function": function,
                         "kind": kind, "disposition": _disposition(relative, function, kind)})
    return sorted(rows, key=lambda r: (r["path"], r["line"], r["kind"]))


def response(ratio: float, damping: float) -> dict:
    """Retain an explicit singularity instead of serializing infinity as JSON."""
    try:
        value = simple_heave_rao(ratio, 1.0, damping)
    except ValueError:
        return {"r": ratio, "damping": damping, "value": None, "status": "UNBOUNDED"}
    return {"r": ratio, "damping": damping, "value": value, "status": "FINITE"}


def calculate() -> dict:
    """Evaluate every table and figure point through the public functions."""
    # Selected mathematical cases from the cited equation; no hull data assumed.
    examples = [(0.5, 0.0), (0.5, 0.05), (1.0, 0.05), (1.0, 0.0)]
    # Plot interval and sample count are presentation choices, not physics.
    curves = [response(index / 40, damping) for damping in (0.0, 0.05)
              for index in range(81)]
    paths = sorted(PACKAGE.rglob("*.py")) + [Path(__file__), ROOT / "docs/api/_assets/brand.css"]
    return {
        "basis": {"kind": "synthetic arithmetic", "equation_source": SOURCE,
                  "equation_edition": "2021", "equation_section": "15.5"},
        "partial": run_compliance_checks({"draft_m": 7.0, "depth_m": 10.0}),
        "empty_selection": run_compliance_checks({}, check_ids=[]),
        "missing_depth": run_compliance_checks({"draft_m": 7.0}, check_ids=["draft_depth_ratio"]),
        "missing_modulus": run_compliance_checks(
            {"lwl_m": 150.0, "beam_m": 20.0, "draft_m": 7.0, "cb": 0.65},
            check_ids=["section_modulus"]),
        "oscillator": [response(r, z) for r, z in examples], "curves": curves,
        "sweep": sweep(),
        "source_digests": {p.relative_to(ROOT).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                           for p in paths},
    }


def table(headings: list[str], rows: list[list]) -> str:
    """Escape every supplied cell; report data never becomes markup."""
    head = "".join(f"<th scope='col'>{escape(str(value))}</th>" for value in headings)
    body = "".join("<tr>" + "".join(f"<td>{escape(str(value))}</td>" for value in row) + "</tr>"
                   for row in rows)
    return f"<div class='table-wrap'><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>"


def figure(curves: list[dict]) -> str:
    """Draw module-computed curves; a gap marks the unbounded response.

    Display clipping at the damped resonance height is a chart range only.
    Numeric results remain uncapped in results.json and the worked table.
    """
    finite = [r["value"] for r in curves if r["value"] is not None and r["damping"] > 0]
    ceiling = max(finite)
    paths = []
    for damping in sorted({r["damping"] for r in curves}):
        segment = []
        for row in [r for r in curves if r["damping"] == damping]:
            if row["value"] is None:
                if segment:
                    paths.append((damping, segment))
                segment = []
                continue
            # SVG dimensions and transforms are presentation coordinates.
            segment.append(f"{50 + 280 * row['r']:.1f},{280 - 240 * min(row['value'], ceiling) / ceiling:.1f}")
        if segment:
            paths.append((damping, segment))
    lines = "".join(
        f"<polyline class='{'undamped' if damping == 0 else 'damped'}' points='{' '.join(points)}'/>"
        for damping, points in paths)
    return ("<figure><svg viewBox='0 0 660 340' role='img' aria-labelledby='chart-title chart-desc'>"
            "<title id='chart-title'>Computed oscillator magnification</title>"
            f"<desc id='chart-desc'>Frequency ratio from zero to two. Display height {ceiling:.1f}. "
            "Undamped resonance has a gap; no finite value is implied.</desc>"
            "<path class='axes' d='M50 35 V280 H620'/>"
            f"{lines}<text x='290' y='330'>Frequency ratio r</text>"
            "<text x='5' y='22'>H</text><text x='45' y='305'>0.0</text>"
            f"<text x='10' y='42'>{ceiling:.1f}</text>"
            "<text x='320' y='305'>1.0</text><text x='600' y='305'>2.0</text></svg>"
            "<figcaption>Figure 1. Module-computed response: dashed undamped, solid damped. "
            "The vertical display range clips large finite values; the calculation does not. "
            "The undamped gap at r = 1.0 denotes ValueError.</figcaption></figure>")


def render(data: dict) -> str:
    """Render ten sections with revision history immediately after the summary."""
    compliance_rows = [[r["name"], r["status"], ", ".join(r["missing_inputs"]) or "none",
                        r["clause_reference"] or "UNVERIFIED"] for r in data["partial"]["checks"]]
    response_rows = [[f"{r['r']:.1f}", str(r["damping"]),
                      f"{r['value']:.1f}" if r["value"] is not None else "ValueError: unbounded"]
                     for r in data["oscillator"]]
    sweep_rows = [[r["path"] + ":" + str(r["line"]), r["function"], r["kind"], r["disposition"]]
                  for r in data["sweep"]]
    history = table(["Revision", "Date", "Change and reason"],
                    [[r["revision"], r["date"], r["change"]] for r in CHANGES["history"]])
    sections = {
        "summary": "<h2>1 Summary</h2><p>The preliminary compliance calculation remains NOT_EVALUATED "
                   "when required evidence is absent. An explicit empty selection is NOT_EVALUATED. "
                   "Undamped oscillator resonance raises ValueError. Supply missing data and qualify "
                   "the applicable rule basis before using a screen for a design decision.</p>",
        "scope": "<h2>2 Scope</h2><p>Draft supplement to the naval architecture knowledge pack, issue #2597. "
                 "This section file complements the separate stability worked report without replacing it. "
                 "The existing missing-input and resonance fixes are extended with structured evidence, "
                 "explicit selection and empty-collection refusals.</p>",
        "inputs": "<h2>3 Inputs and basis</h2><p>All dimensions and damping ratios below are synthetic mathematical "
                  "inputs. Draft 7.0 m and depth 10.0 m demonstrate completeness; length 150.0 m, beam 20.0 m "
                  "and block coefficient 0.65 demonstrate an absent measured section modulus. These values "
                  "are selected arithmetic examples, not measured geometry or acceptance limits.</p>"
                  "<p>Each compliance row records status, missing_inputs (including invalid data), "
                  "clause_reference and reference_status. A null clause reference is explicitly UNVERIFIED. "
                  "The legacy dictionary fields remain available; pass is derived from the typed status.</p>",
        "method": f"<h2>4 Method and source</h2><p><a href='{SOURCE}'>Mechanics Map, 2021 online edition, "
                  "section 15.5, magnification-factor equation and Figure 2</a>: "
                  "H = 1/sqrt((1-r²)² + (2ζr)²), with r = ω/ωn. "
                  "At r = 1.0 and ζ = 0.0 the denominator vanishes; no finite steady response exists. "
                  "ValueError follows the module's refusal style.</p><p>Completeness is evaluated before "
                  "each compliance screen. Aggregate FAIL takes precedence over NOT_EVALUATED; PASS "
                  "requires a nonempty selection with every selected row passing. Legacy compliance "
                  "expressions and limits are unchanged and are not qualified by this supplement.</p>",
        "worked-example": "<h2>5 Worked examples</h2><p>Independent arithmetic: T/D = 7.0/10.0 = 0.7. "
                          "This row can be calculated, but the other required data are absent. "
                          f"The aggregate is {data['partial']['status']}.</p>"
                          + table(["Check", "Status", "Missing or invalid inputs", "Clause"], compliance_rows)
                          + "<p>At r = 0.5 and ζ = 0.0, H = 4/3. With ζ = 0.05, denominator squared "
                          "is 9/16 + 1/400 = 113/200, hence H = sqrt(200/113). At r = 1.0 and "
                          "ζ = 0.05, H = 1/(2 × 0.05) = 10.0.</p>"
                          + table(["r", "ζ (input)", "Computed H"], response_rows) + figure(data["curves"]),
        "pitfalls": "<h2>6 Known failure modes and pitfalls</h2>"
                    + table(["Condition", "Historical behavior (#2597)", "Current behavior", "Detection"], [
                        ["Missing depth", "PASS", data["missing_depth"]["status"], "test_missing_geometry_has_structured_evidence"],
                        ["Missing section modulus", "PASS using fabricated modulus", data["missing_modulus"]["status"], "test_missing_geometry_has_structured_evidence"],
                        ["Empty selection", "Empty reduction could PASS", data["empty_selection"]["status"], "test_explicit_empty_selection_is_not_evaluated"],
                        ["Undamped resonance", "100.0", "ValueError: unbounded", "test_undamped_resonance_raises"],
                        ["Empty damage, lifting or symmetry result", "True from all([])", "False with missing-evidence note for symmetry", "test_evidence_gaps.py"],
                        ["Empty response spectrum", "Zero motion", "ValueError", "test_empty_spectral_evidence_is_refused"],
                        ["Absent clean-cut flag", "Clean assumed", "NOT_EVALUATED note", "test_missing_cut_evidence_emits_unevaluated_note"],
                    ]),
        "limits": "<h2>7 Limits of use and NOT_EVALUATED list</h2><p>Legacy freeboard, proportion, "
                  "displacement/length, strength, immersion and trim coefficients have no qualified "
                  "clause here. Their clause references are null; a preliminary PASS does not establish "
                  "regulatory acceptance. Section-modulus coefficient and units require qualification. "
                  "IMO Part A 2.3 weather, downflooding applicability, full stability compliance, "
                  "hull-specific damping, radiation and wave excitation remain NOT_EVALUATED. "
                  "The spectrum guard rejects absent arrays; it does not qualify spectral discretization.</p>",
        "recommendations": "<h2>8 Recommendations and follow-up</h2><p>Supply measured geometry and strength; "
                           "select the applicable rule edition and clause; use an approved stability and "
                           "structural assessment for acceptance. For motion, use qualified system damping "
                           "and excitation before making an operating decision.</p><p>The sweep appendix "
                           "lists every explicit mapping default, all reduction and broad exception handler "
                           "in the package. Physical defaults in maneuvering, loading and resistance and "
                           "partial geometry-reader fallbacks need method-specific qualification. They are "
                           "follow-ups, not newly accepted inputs. Function-argument defaults and unrelated "
                           "formula correctness are outside this pattern sweep.</p>",
        "evidence": "<h2>9 Evidence and reproduction</h2><p>Rebuild: "
                    "<code>PYTHONPATH=src python scripts/reporting/build_naval_evidence_report.py</code>. "
                    "Use --check to refuse stale artifacts, or --output with a scratch directory. "
                    "results.json retains full-precision computed values, all curve points, source digests "
                    "and sweep candidates. Visible results use one decimal place; damping inputs retain "
                    "their specified precision. Historical behavior is from issue #2597; no historical "
                    "calculation is rerun. Test docstrings retain independent expected arithmetic.</p>"
                    + table(["Location", "Function", "Candidate kind", "Disposition"], sweep_rows),
        "symbols": "<h2>10 Symbols and units</h2>"
                   + table(["Symbol", "Meaning", "Unit"], [["T, D", "Draft, depth", "m"],
                            ["r", "Wave / natural frequency", "dimensionless"],
                            ["ζ", "Damping ratio", "dimensionless"], ["H", "Oscillator magnification", "dimensionless"]]),
    }
    content = "".join(f'<section id="{key}">{sections[key]}</section>'
                      + (f'<section id="revision-history"><h2>Revision history</h2>{history}</section>'
                         if key == "summary" else "") for key in SECTIONS)
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"),
                   flags=re.DOTALL)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    style = """body{font:16px/1.6 system-ui;background:var(--ground);color:var(--ink);margin:0}
main{max-width:1100px;margin:auto;padding:2rem}section{background:var(--panel);padding:1.5rem;margin:1rem 0}
h1,h2{color:var(--brand)}table{border-collapse:collapse;width:100%}td,th{padding:.5rem;border:1px solid var(--edge);text-align:left}
.table-wrap{overflow:auto}svg{width:100%;max-height:400px}.damped,.undamped{fill:none;stroke:var(--brand);stroke-width:2}
.undamped{stroke-dasharray:6 4;stroke:var(--nogo)}.axes{fill:none;stroke:var(--ink)}text{fill:var(--ink)}a{color:var(--brand)}
"""
    return ("<!doctype html><html lang='en'><head><meta charset='utf-8'>"
            "<meta name='viewport' content='width=device-width, initial-scale=1'>"
            f"<title>Naval architecture evidence analysis</title><style>{brand}\n{style}</style></head>"
            f"<body class='hs-doc'><main><h1>Naval architecture evidence analysis</h1>{content}</main></body></html>\n")


def artifacts() -> dict[str, str]:
    """Return the complete deterministic publication set without writing."""
    data = calculate()
    return {"report.html": render(data),
            "results.json": json.dumps(data, indent=2, allow_nan=False) + "\n",
            "changes.json": json.dumps(CHANGES, indent=2) + "\n"}


def build(output: Path = OUTPUT) -> list[Path]:
    """Write only the three named deliverables to the requested destination."""
    output.mkdir(parents=True, exist_ok=True)
    paths = []
    for name, content in artifacts().items():
        path = output / name
        path.write_text(content, encoding="utf-8")
        paths.append(path)
    return paths


def main() -> None:
    """Rebuild deliverables or check freshness without rewriting files."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    if args.check:
        stale = [name for name, content in artifacts().items()
                 if not (args.output / name).is_file()
                 or (args.output / name).read_bytes() != content.encode("utf-8")]
        if stale:
            parser.exit(1, "Stale evidence artifacts: " + ", ".join(stale) + "\n")
    else:
        build(args.output)


if __name__ == "__main__":
    main()
