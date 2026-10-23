# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the generic benchmark from committed inputs, without a solver.

Run with the repository source on the Python import path. Only final report
artifacts are written to --output. Tests use an external scratch directory.
"""

from __future__ import annotations

import argparse
import html
import json
from pathlib import Path
import subprocess

from bs4 import BeautifulSoup

from digitalmodel.drilling_riser import benchmark, generic_campaign
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.provenance import DataSource, Provenance
from digitalmodel.reporting.spec import (
    DocumentMeta,
    FigureBlock,
    ReportSpec,
    RevisionRow,
    Section,
    TableBlock,
    TextBlock,
)

ROOT = Path(__file__).resolve().parents[2]
DATE = "2026-10-22"
CHANGES = {"revision": "1", "date": DATE, "changes": [
    "Proposed offset and uniform-current inputs retain explicit public-source requirements.",
    "Column slopes, top-force screen and chord extension are recalculated from the generic template.",
    "Stored-summary agreement requires matched identity, quantity definitions and sourced criteria.",
    "Absent native results and assembly acceptance remain NOT_EVALUATED.",
]}
DRAG_SOURCE = ("https://www.orcina.com/webhelp/OrcaFlex/Content/html/"
               "Linetheory,Hydrodynamicandaerodynamicloads.htm")
BEAM_SOURCE = ("https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/"
               "resources/mit2_080jf13_lecture6/")


def text(value):
    return TextBlock(markdown=value)


def number(value):
    return "NOT_EVALUATED" if value is None else f"{value:.1f}"


def figure(rows, metric, title, unit):
    """Draw every point from computed values; SVG coordinates are layout only."""
    xmax = max(row["offset_pct"] for row in rows)
    ymax = max(row["screens"][metric] for row in rows) or 1
    points = []
    curves = {}
    colors = ("#00796b", "#3949ab", "#a94d00", "#704090")
    speeds = sorted({row["current_mps"] for row in rows})
    for row in rows:
        x = 75 + 580 * row["offset_pct"] / xmax
        y = 305 - 250 * row["screens"][metric] / ymax
        speed = row["current_mps"]
        color = colors[speeds.index(speed) % len(colors)]
        curves.setdefault(speed, []).append((x, y))
        # Store one-decimal data labels, matching visible engineering precision.
        value = number(row["screens"][metric])
        points.append(f'<circle cx="{x:.3f}" cy="{y:.3f}" r="3" fill="{color}" '
                      f'data-case="{row["case_id"]}" data-value="{value}">'
                      f'<title>{row["case_id"]}: {value} {unit}</title></circle>')
    paths = []
    for index, (speed, values) in enumerate(curves.items()):
        coordinates = " ".join(f"{x:.3f},{y:.3f}" for x, y in values)
        paths.append(f'<polyline points="{coordinates}" fill="none" stroke="{colors[index]}"/>')
        paths.append(f'<text x="{80 + index * 150}" y="365" fill="{colors[index]}">'
                     f'{speed:.1f} m/s</text>')
    svg = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 390" '
           f'role="img" aria-label="{html.escape(title)}" data-metric="{metric}">'
           f'<title>{html.escape(title)}</title>'
           '<path d="M75 45 V305 H670" fill="none" stroke="currentColor"/>'
           f'<text x="12" y="55">{ymax:.1f}</text><text x="45" y="310">0.0</text>'
           f'<text x="75" y="330">0.0</text><text x="640" y="330">{xmax:.1f}</text>'
           f'<text x="80" y="25">{html.escape(unit)}</text>'
           '<text x="260" y="345">Vessel offset (% water depth)</text>'
           + "".join(paths + points) + '</svg>')
    return FigureBlock(figure_id=metric.replace("_", "-"), title=title, svg=svg,
                       caption="Diagnostic column screen; curves show proposed uniform currents. "
                       "No assembly acceptance or mechanical stroke is established.")


def committed_results():
    """Select only tracked, unchanged JSON summaries in the report input area.

    Future result additions must be committed before the published report uses
    them. Untracked and modified summaries cannot silently change the report.
    """
    folder = ROOT / benchmark.REPORT / "results"
    for path in sorted(folder.glob("*.json")):
        relative = path.relative_to(ROOT).as_posix()
        tracked = subprocess.run(["git", "ls-files", "--error-unmatch", "--", relative],
                                 cwd=ROOT, capture_output=True, check=False)
        unchanged = subprocess.run(["git", "diff", "--quiet", "HEAD", "--", relative],
                                   cwd=ROOT, capture_output=True, check=False)
        if tracked.returncode or unchanged.returncode or path.is_symlink():
            raise ValueError("Result summary must be committed and unchanged")
    return folder if folder.is_dir() else None


def build_spec():
    """Use the shared typed report engine and retain a complete evidence ledger."""
    rows = benchmark.evaluate(ROOT, results=committed_results())
    basis = generic_campaign.column_basis(ROOT)
    original = generic_campaign.case_table(ROOT)
    case_rows = [[row["case_id"], number(row["offset_pct"]), number(row["offset_m"]),
                  number(row["current_mps"]), "Uniform surface-to-seabed; source required"]
                 for row in rows]
    screen_rows = [[row["case_id"], *[number(row["screens"][key]) for key in (
        "lower_angle_deg", "upper_angle_deg", "top_tension_kn", "geometric_extension_m")],
        "NOT_EVALUATED"] for row in rows]
    results = [[row["case_id"], key, number(item["value"]),
                number(item["difference_pct"]), item["status"],
                item["solver_version"] or "NOT_EVALUATED", item["reason"]]
               for row in rows for key, item in row["results"].items()]
    # A live worked example is selected from the committed grid, not hard-coded.
    example = next(row for row in rows if row["offset_pct"] > 0 and row["current_mps"] == 0)
    factor = basis["min_tension_kn"] / basis["weight_kn"]
    reference = generic_campaign.SPEC.as_posix()
    sections = [
        Section(key="summary", title="Summary", blocks=[text(
            f"{len(rows)} proposed connected-mode offset and current cases are retained from "
            "the existing generic preparation campaign. All reported numerical responses are "
            "diagnostic column screens. Every native result and engineering acceptance is "
            "NOT_EVALUATED until committed results, applicable criteria and evidence are supplied. "
            "No operating window is established. This internal program benchmarking analysis "
            "prepares a comparison; it contains no completed program benchmark.")]),
        Section(key="revision-history", title="Revision history", blocks=[TableBlock(
            title="Issue history", columns=["Revision", "Date", "Change and reason"],
            rows=[["1", DATE, "Initial benchmark evidence report to distinguish diagnostic screens "
                   "from unavailable native results and acceptance."]])]),
        Section(key="scope", title="Scope and inputs", blocks=[text(
            f"Only the generic template {reference} is read. Existing generated inputs under "
            f"{generic_campaign.DEST.as_posix()}/models are reused. No vendor example or "
            "project-derived model is opened by this builder. Generation uses "
            "generic_campaign.build_models and its two licence-free YAML validators. "
            "YAML checks establish syntax and structural consistency, not native equilibrium. "
            "The separate stack-up calculation and limitations report is tracked in pull "
            "request #3226; its module documentation is not repeated here."), text(
            f"Water depth {number(generic_campaign.read_spec(ROOT)['environment']['water']['depth'])} m "
            f"comes from {reference}, environment.water.depth. Geometry, stiffness, densities "
            "and drag coefficients retain that template's basis. The proposed grid and "
            "full-depth uniform current remain required inputs: " + benchmark.PENDING + ". "
            "Retaining numerical proposals does not qualify their coverage or authorize a run."),
            TableBlock(title="Proposed case table", columns=["Case", "Offset (%)", "Offset (m)",
                       "Current (m/s)", "Profile and qualification"], rows=case_rows,
                       source="cases.json; each numeric proposal includes its source and required-input flag")]),
        Section(key="method", title="Method and equation sources", blocks=[text(
            "Offset X = p H / 100 (percentage definition), implemented by "
            "operability.watch_circle_radius_m. The factor 100 converts percent to a fraction. "
            "Uniform drag q = rho Cd D U² / 2 uses the normal Morison drag term in "
            f"[OrcaFlex WebHelp, Line theory: Hydrodynamic and aerodynamic loads]({DRAG_SOURCE}), "
            "Standard formulation, accessed 2026-10-22. No wave or vessel motion is applied."), text(
            "For constant tension T and pinned ends, EI y'''' − T y'' = q; "
            "y(0)=0, y(L)=X and y''(0)=y''(L)=0. "
            f"[MIT OpenCourseWare, Structural Mechanics (2013), Lecture 6]({BEAM_SOURCE}), "
            "equations (6.3)–(6.6), supplies equilibrium and moment-curvature relations; "
            "the constant-tension specialization and boundary conditions are derived in "
            "riser_response.py. Reported slopes are absolute small-angle estimates in degrees. "
            "Variable effective tension, actual joint stiffness and assembly geometry are omitted; "
            "these estimates are not qualified assembly joint rotations."), text(
            f"Top force T = W f uses stackup.top_tension_required. W = {number(basis['weight_kn'])} kN "
            "comes from generic_campaign.column_basis (annular steel plus bore contents minus "
            "displaced water, multiplied by the recorded gravity assumption). The existing "
            "campaign factor is retained explicitly; it has no sourced engineering acceptance "
            "status. The column excludes attached members and treats the above-water portion "
            "as submerged. Mechanical stroke has no applicable screen here. "
            "Chord extension sqrt(L²+X²)−L follows Pythagoras and is only geometric extension."), text(
            "Stored-summary difference (%) = 100 (result − screen) / |screen|. "
            "An agreement PASS requires |difference| ≤ supplied criterion and a source. "
            "No agreement criterion is supplied in this report. A zero screen has undefined "
            "percentage difference. Unit, quantity definition, case identity and result origin "
            "must match; absent, malformed or stale data are NOT_EVALUATED. An agreement "
            "PASS would validate only that declared comparison, not assembly acceptance. "
            "The required engineering angle, tension and stroke criteria are " + benchmark.PENDING + ".")]),
        Section(key="worked-example", title="Worked example", blocks=[text(
            f"Case {example['case_id']}: offset {number(example['offset_pct'])}% gives "
            f"X = {number(example['offset_m'])} m using the template depth; U = "
            f"{number(example['current_mps'])} m/s makes q = 0.0 N/m. The pinned column "
            "is a straight tilt y=Xz/L. Therefore both slopes are X/L radians, or "
            f"{number(example['screens']['lower_angle_deg'])} degrees, with zero bending moment. "
            f"The length L = {number(basis['length'])} m is taken from the template main line. "
            f"T = W f = {number(example['screens']['top_tension_kn'])} kN; f is inherited "
            f"from the campaign assumption ({factor:g}; assumed, based on previous experience). "
            f"The chord extension is {number(example['screens']['geometric_extension_m'])} m. "
            "The independent regression example uses simple mathematical inputs; its full "
            "arithmetic is written in test_screen_offset_hand_calculation. Mechanical stroke "
            "and engineering acceptance remain NOT_EVALUATED.")]),
        Section(key="screens", title="Diagnostic screens", blocks=[
            TableBlock(title="Column estimates; no acceptance grading", columns=["Case", "Lower slope (deg)",
                       "Upper slope (deg)", "Top force (kN)", "Chord extension (m)",
                       "Mechanical stroke (m)"], rows=screen_rows),
            *[figure(rows, key, title, unit) for key, title, unit in (
                ("lower_angle_deg", "Lower column slope estimate", "degrees"),
                ("upper_angle_deg", "Upper column slope estimate", "degrees"),
                ("top_tension_kn", "Assumed constant column top force", "kN"),
                ("geometric_extension_m", "Geometric chord extension", "m"))]]),
        Section(key="results", title="Stored results and comparisons", blocks=[text(
            "The following fields are placeholders. Synthetic fixtures exercise the reader "
            "only and are never selected as engineering evidence. Solver version, native "
            "joint rotations, assembly tension, mechanical stroke and their acceptance are "
            "NOT_EVALUATED. Future native channels must retain their own definitions; they "
            "must not be relabelled as constant-tension column outputs."),
            TableBlock(title="Complete result ledger", columns=["Case", "Quantity", "Stored value",
                       "Difference (%)", "Status", "Recorded version", "Reason"], rows=results)]),
        Section(key="limits", title="Limits of use", blocks=[text(original["screen_limitations"]), text(
            "The proposed range has no independent public benchmark justification. No licensed "
            "solver is run or imported. No wave response, heave, drift-off, recoil, fatigue, "
            "VIV, equipment capacity or qualified operating envelope is established. "
            "Native assembly comparisons require variable tension and matched definitions; "
            "the simplified curves cannot support equipment acceptance.")]),
        Section(key="pending", title="NOT_EVALUATED and way forward", blocks=[TableBlock(
            title="Required evidence", columns=["Item", "Status", "Way forward"], rows=[
                ["Sweep ranges and profile", "NOT_EVALUATED", benchmark.PENDING],
                ["Native result version and channels", "NOT_EVALUATED", "Commit sourced summaries and exact input identities"],
                ["Assembly flex-joint angles", "NOT_EVALUATED", "Establish variable-tension comparison and applicable criteria"],
                ["Assembly tension checks", "NOT_EVALUATED", "Use the existing assembly basis and qualify native channels"],
                ["Mechanical stroke and capacity", "NOT_EVALUATED", "Supply actual equipment motion definition and capacity"],
                ["Screen agreement limits", "NOT_EVALUATED", benchmark.PENDING],
            ]), text("All later solver work and acceptance remain outside this build. Select public "
                     "references and matched response definitions before using the stored-summary interface.")]),
        Section(key="sources", title="Sources and reproduction", blocks=[text(
            f"Template: {reference}. Input proposals: cases.json and "
            "generic_campaign.case_table. Mechanics: MIT Structural Mechanics (2013), "
            "Lecture 6, equations (6.3)–(6.6); OrcaFlex WebHelp Standard drag formulation "
            "(accessed 2026-10-22), linked in Method. Numerical implementations: "
            "src/digitalmodel/drilling_riser/riser_response.py, stackup.py, operability.py, "
            "generic_campaign.py. No licensed standard value is used as an acceptance limit. "
            "The separate assembly criterion basis is described in generic_tension.py; "
            "it is not substituted for the diagnostic force screen."), text(
            "Rebuild from the repository root with the documented dependencies installed: "
            "PYTHONPATH=src python scripts/reporting/build_drilling_riser_benchmark.py "
            "--output docs/reports/modules/drilling-riser-benchmark. "
            "The test reruns the build in an external temporary directory and compares both "
            "artifacts byte for byte. No browser, native file or external service is needed.")]),
    ]
    sources = [generic_campaign.SPEC, benchmark.REPORT / "cases.json",
               Path("scripts/reporting/build_drilling_riser_benchmark.py"),
               Path("src/digitalmodel/drilling_riser/benchmark.py")]
    sources += [Path("src/digitalmodel/drilling_riser") / name
                for name in generic_campaign.calculation_fingerprints()]
    return ReportSpec(document=DocumentMeta(number="G0000-RPT-012-00", revision="00",
        title="Drilling riser program benchmarking analysis: preparation",
        project="Generic analytical basis", client="Generic engineering basis", date=DATE,
        revision_history=[RevisionRow(rev="00", date=DATE, description="Initial benchmark preparation")]),
        sections=sections, tool_version="benchmark-report-1",
        provenance=Provenance(sources=[DataSource(kind="committed input or implementation",
            identifier=path.as_posix(), digest=generic_campaign.digest(ROOT / path)) for path in sources]))


def build(output: Path):
    """Write the deterministic HTML and engineering change register."""
    document = BeautifulSoup(render_html(build_spec(), pdf_status="HTML only"), "html.parser")
    document.body["class"] = list(document.body.get("class", [])) + ["hs-doc"]
    references = document.select_one("#references-cited p.prose")
    if references is not None:
        references.string = "Public sources and equation locators are listed in Method and Sources."
    generic_campaign.write_text(output / "index.html", str(document))
    generic_campaign.write_text(output / "changes.json", json.dumps(CHANGES, indent=2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / benchmark.REPORT)
    build(parser.parse_args().output)
