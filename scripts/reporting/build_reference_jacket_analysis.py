# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the reproducible reference-jacket cathodic protection analysis.

The public geometry fixture and existing CP route are the numerical basis.
Unqualified CP inputs retain assumption status; no licensed table is exported.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import re
from dataclasses import asdict
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import (
    _citations_from_labels,
    anode_design_report,
)
from digitalmodel.cathodic_protection.report_reference import (
    reference_arrangements,
    reference_comparison,
    reference_figure,
    reference_member_tables,
    reference_zone_blocks,
)
from digitalmodel.cathodic_protection.report_verification import _model_outputs
from digitalmodel.citations import validate_citation
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.figures import figure_from_columns
from digitalmodel.reporting.spec import FigureBlock, ReportSpec, Section, TableBlock, TextBlock

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml"
DESTINATION = ROOT / "docs/domains/cathodic_protection/reports/reference-jacket"
STANDARD_REVISION = "bc2472877b0af9e399ccb4b4fcb8eb8a8b7b5721"
SECTION_TITLES = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
ASSUMED = "assumed"


def digest(path: Path) -> str:
    """Return the digest of bytes actually read from a file."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def checks(cfg: dict[str, Any], result: dict[str, Any]) -> dict[str, Any]:
    """Compare native mass and output against native demand before rounding."""
    req = result["anode_requirements"]
    demand = result["current_demand_A"]
    count = result["current_output_verification"]["anode_count"]
    initial, final = _model_outputs(cfg["inputs"], result)
    mass = kernel.anode_mass(
        demand["total_mean_A"], result["design_life_years"],
        req["electrochemical_capacity_Ah_kg"], req["utilization_factor"],
    )
    values = (
        ("Mean-current lifetime mass", mass,
         count * cfg["inputs"]["anode"]["individual_anode_mass_kg"], "kg"),
        ("Initial current output", demand["total_initial_A"], count * initial, "A"),
        ("Final current output", demand["total_final_A"], count * final, "A"),
    )
    return {"anode_count": count, "checks": [
        {"quantity": name, "required": required, "provided": provided,
         "unit": unit, "margin_percent": 100 * (provided / required - 1),
         "passes": provided >= required}
        for name, required, provided, unit in values
    ]}


def sensitivity(cfg: dict[str, Any], count: int) -> list[dict[str, Any]]:
    """Compute count requirements over illustrative design-life scenarios."""
    life = float(cfg["inputs"]["design_data"]["design_life"])
    scenarios = []
    for evaluated in (0.8 * life, life, 1.2 * life, 2 * life):
        candidate = copy.deepcopy(cfg)
        candidate["inputs"]["design_data"]["design_life"] = evaluated
        candidate["inputs"]["anode"]["count"] = count
        result = run_cathodic_protection(candidate)["results"]
        verification = result["current_output_verification"]
        scenarios.append({"quantity": "Design life", "base_years": life,
                          "evaluated_years": evaluated,
                          "required_count": verification["recommended_anode_count"],
                          "governing_case": verification["governing_case"],
                          "count_by_mass": verification["count_by_mass"],
                          "count_by_initial_current": verification["count_by_initial_current"],
                          "count_by_final_current": verification["count_by_final_current"],
                          "comparison": checks(candidate, result)})
    return scenarios


def design_data(cfg: dict[str, Any]) -> list[Any]:
    """Present the seven required basis items with source and effect."""
    inputs = cfg["inputs"]
    model, env, anode = cfg["structural_model"], inputs["environment"], inputs["anode"]
    life = inputs["design_data"]["design_life"]
    return [
        TextBlock(markdown=("## 3.1 Site data\n\n"
            f"Water depth is {model['water_depth_m']:.1f} m from the pinned public model [2]. "
            "The site is a reference geometry, with no installation location selected. "
            "Water-level range, splash boundaries and wave conditions are not established; "
            "above-water protection is therefore not evaluated.")),
        TextBlock(markdown=("## 3.2 Structure and equipment data\n\n"
            f"The {cfg['dimensions']['leg_count']['value']}-leg tubular jacket is represented "
            f"by {len(model['members'])} members [2]. Section properties and axis coordinates "
            "are sourced; true lengths, external areas and steel mass are derived. "
            "Pile embedment is an initial estimate in the paper [1], not an installed schedule. "
            f"Design life is {life:.1f} yr; coating category is "
            f"{inputs['structure']['zones'][0]['coating_category']}; both are {ASSUMED} [I1], "
            "illustrative service-life and coating choices without installation evidence. "
            "They control lifetime mass and current demand. "
            "Buried driven-pile surface is bare: no coating credit is applied in any phase. "
            f"The aluminium stand-off cylinder is {anode['length_m']:.1f} m long with "
            f"radius {anode['radius_m']:.3f} m, {ASSUMED} [I1] as an illustrative cylinder. "
            f"Net mass is {anode['individual_anode_mass_kg']:.1f} kg, derived from "
            f"π × radius² × length × a typical Al-Zn-In alloy density of "
            f"{kernel.ANODE_DENSITY_ALZNI:.1f} kg/m³ (assumed); no core is deducted. "
            "The solid-cylinder idealisation is not a qualified product. "
            f"The {anode['count']}-anode case is a stated trial arrangement, "
            "not a published installation count. "
            "Dimensions and alloy performance affect both mass and output. "
            "Attachment, core, product qualification and spatial layout are not established.")),
        TextBlock(markdown=("## 3.3 Environmental data\n\n"
            f"Temperature {env['seawater_temperature_C']:.1f} °C and seawater resistivity "
            f"{env['seawater_resistivity_ohm_m']:.1f} Ω m are {ASSUMED} [I1], "
            "illustrative seawater conditions without site measurements. "
            "The temperate climate selection is assumed for this reference case. "
            "Temperature selects current-demand and alloy-performance lookups; resistivity "
            "controls anode resistance and current output. Soil resistivity, burial condition "
            "and measured potentials are not established; buried and spatial adequacy remain conditional.")),
        TextBlock(markdown=("## 3.4 Vessel data\n\nNot applicable: the assessed "
            "configuration is a fixed jacket; no vessel installation analysis is performed.")),
        TextBlock(markdown=("## 3.5 Criteria and source clauses\n\n"
            "DNV-RP-B401, Cathodic protection design, May 2021 [3]: "
            "Anode mass; Anode resistance; Anode current output. "
            "The aggregate mass criterion is provided net mass ≥ lifetime required mass; "
            "initial and final output criteria are provided current ≥ corresponding current demand. "
            "Selection basis [3]: Design current densities (temperate climate, upper and lower "
            "water-column depth bands and buried steel); Coating breakdown factors "
            "(category III water-column steel, bare buried piles); Anode materials "
            "(aluminium capacity, utilisation and protective potential). "
            "The calculation applies this selection [I2]; no standard table is reproduced. "
            "An aggregate PASS denotes these comparisons only; no local potential criterion is evaluated.")),
        TextBlock(markdown="## 3.6 Method\n\nThe physical method and equations are presented in Section 4."),
        TextBlock(markdown=(f"## 3.7 Load cases\n\nThe stated {anode['count']}-anode trial arrangement "
            f"and the re-sized {cfg['results']['current_output_verification']['recommended_anode_count']}-anode "
            "arrangement are evaluated for initial, mean and final conditions. "
            "Water-column steel is split at the depth-band boundary of the design current-density clause; buried pile "
            "area is evaluated separately as bare steel. Illustrative design-life scenarios "
            "keep the anode geometry and recompute the required count; "
            "the doubled-life case also includes increased water-column coating breakdown.")),
    ]


def comparison_tables(evidence: dict[str, Any]) -> list[TableBlock]:
    """Format comparison numbers to one decimal without rounding the verdict."""
    blocks = []
    for name, arrangement in evidence["arrangements"].items():
        for unit in ("kg", "A"):
            blocks.append(TableBlock(
                title=f"{name.capitalize()} arrangement: {unit} comparison",
                columns=["Check", f"Required ({unit})", f"Provided ({unit})", "Margin (%)", "Result"],
                rows=[[c["quantity"], f"{c['required']:.1f}", f"{c['provided']:.1f}",
                       f"{c['margin_percent']:+.1f}", "PASS" if c["passes"] else "FAIL"]
                      for c in arrangement["checks"] if c["unit"] == unit],
                source="[I2]; criterion: provided ≥ required",
            ))
    return blocks


def interactive_figures(evidence: dict[str, Any]) -> list[FigureBlock]:
    """Build engine-native interactive charts from calculated quantities."""
    zones = evidence["zone_demand_A"]
    demand = figure_from_columns("bar", list(zones), {
        phase.capitalize(): [row[f"I_{phase}_A"] for row in zones.values()]
        for phase in ("initial", "mean", "final")
    }, x_label="Exposure zone", y_label="Current demand (A)")
    scenarios = evidence["sensitivity"]
    life = figure_from_columns("line", [s["evaluated_years"] for s in scenarios], {
        "Required count": [s["required_count"] for s in scenarios],
        "Stated count": [evidence["arrangements"]["stated"]["anode_count"]] * len(scenarios),
    }, x_label="Design life (yr)", y_label="Anode count")
    for figure in (demand, life):
        for trace in figure["data"]:
            trace["hovertemplate"] = "%{x}: %{y:.1f}<extra>%{fullData.name}</extra>"
    return [
        FigureBlock(title="Current demand by zone and phase", figure_id="zone-demand",
                    plotly=demand, caption="Bare driven piles receive no coating credit; "
                    "initial, mean and final loads are computed on the declared basis."),
        FigureBlock(title="Required anode count against design life", figure_id="life-count",
                    plotly=life, caption="Required count is the maximum of mass, initial "
                    "and final current counts; lifetime mass governs the evaluated cases."),
    ]


def sensitivity_table(evidence: dict[str, Any]) -> TableBlock:
    """Present lifetime sizing and its governing criterion without overclaiming."""
    return TableBlock(title="Design-life sizing with bare piles",
        columns=["Life (yr)", "Mass count", "Initial count", "Final count", "Required count", "Governing criterion"],
        rows=[[f"{s['evaluated_years']:.1f}", s["count_by_mass"],
               s["count_by_initial_current"], s["count_by_final_current"],
               s["required_count"], s["governing_case"]] for s in evidence["sensitivity"]],
        source="[3]; fixed anode geometry and environment; water-column coating breakdown varies with life.")


def revision_history() -> Section:
    """Retain the issued history and record the public example data basis."""
    return Section(key="revision-history", title="Revision history", blocks=[
        TableBlock(title="Revision history", columns=["Revision", "Date", "What changed and why"],
            rows=[["1", "2026-10-22", "Published-geometry worked example label added; calculated results retained."],
                  ["0", "2026-10-14", "Issued for information."]]),
    ])


def build_document() -> tuple[ReportSpec, dict[str, Any]]:
    """Build an issued reference analysis on the declared assumed basis."""
    cfg = run_cathodic_protection(yaml.safe_load(FIXTURE.read_text(encoding="utf-8")))
    arrangements = reference_arrangements(cfg)
    evidence: dict[str, Any] = {
        "status": "Issued for information",
        "input_sha256": digest(FIXTURE), "arrangements": {},
    }
    for name, result in arrangements.items():
        if result is not None:
            evidence["arrangements"][name] = checks(cfg, result)
    count = evidence["arrangements"]["stated"]["anode_count"]
    evidence["sensitivity"] = sensitivity(cfg, count)
    evidence["zone_demand_A"] = {
        z["zone"]: {f"I_{phase}_A": cfg["results"]["current_demand_A"][z["zone"]][f"I_{phase}_A"]
                    for phase in ("initial", "mean", "final")}
        for z in cfg["inputs"]["structure"]["zones"]
    }
    spec = anode_design_report(cfg)
    spec.document.project = "Worked example on published geometry"
    spec.worked_example_presentation = True
    spec.document.revision = "01"
    spec.document.number = spec.document.number[:-2] + "01"
    blocks = section_blocks(cfg, evidence, spec)
    spec.sections = [Section(key=f"analysis-{i}", title=title, blocks=content)
                     for i, (title, content) in enumerate(zip(SECTION_TITLES, blocks), 1)]
    spec.appendices = [Section(key="structural-evidence", title="Structural evidence", blocks=[
        *reference_member_tables(cfg["structural_model"]),
        *structural_comparison(cfg["structural_model"]),
    ]), Section(key="reproduction", title="Reproduction", blocks=[
        TextBlock(markdown="```\nPYTHONPATH=src uv run --no-sources python "
                  "scripts/reporting/build_reference_jacket_analysis.py\n```"),
        TextBlock(markdown=f"Input SHA-256: {evidence['input_sha256']}"),
    ])]
    citations = _citations_from_labels(cfg["results"]["citations"], "Existing CP route; no standard tables exported.")
    for citation in citations:
        validate_citation(citation)
    evidence["citations"] = [asdict(c) for c in citations]
    evidence["geometry_sources"] = cfg["structural_model"]["source_files"]
    evidence["reporting_standard_revision"] = STANDARD_REVISION
    return spec, evidence


def structural_comparison(model: dict[str, Any]) -> list[Any]:
    """Compare printed structural mass on the transition-piece-inclusive basis."""
    table = reference_comparison(model)[0]
    combined = model["steel_mass_kg"] + model["excluded_concentrated_mass_kg"]
    for row, comparison in zip(table.rows, model["comparisons"]):
        if comparison["quantity"] == "Tubular jacket steel mass":
            printed = comparison["printed"]
            row[:4] = ["Structural mass, tubular steel plus transition piece",
                       f"{printed:,.1f}", f"{combined:,.1f}",
                       f"{100 * (combined / printed - 1):+.1f} %"]
            row[5] = "[1], pp. 810–811, Section 3.2; transition piece included"
    return [table, TableBlock(title="Tubular steel mass (informative)",
        columns=["Quantity", "Mass (kg)", "Source"],
        rows=[["Tubular jacket steel alone", f"{model['steel_mass_kg']:,.0f}",
               "[2]; derived from tubular sections and lengths"]])]



def summary_text(cfg: dict[str, Any], evidence: dict[str, Any]) -> str:
    """State the mass shortfall and re-sized solution with their criteria."""
    stated = evidence["arrangements"]["stated"]
    count = stated["anode_count"]
    mass = stated["checks"][0]
    resized = evidence["arrangements"].get("rechecked", stated)
    summary = (f"The reference jacket at {cfg['structural_model']['water_depth_m']:.1f} m "
        f"water depth is analysed with {count} anodes. "
        "Aggregate lifetime mass and initial and final current output are compared with "
        "the required quantities below, using DNV-RP-B401, Cathodic protection design, "
        "May 2021 [3]. Issued for information on a declared assumed basis. "
        "This is a worked example on published geometry with synthetic CP inputs. "
        f"With buried piles bare, {mass['provided']:.1f} kg is provided against "
        f"{mass['required']:.1f} kg required: the lifetime mass criterion fails, "
        f"with a {-mass['margin_percent']:.1f} % shortfall relative to required mass. "
        f"The re-sized solution is {resized['anode_count']} anodes and passes the aggregate "
        "mass and initial/final current criteria on this basis. Environmental, water-column "
        "coating, lifetime and anode inputs are assumed. Spatial "
        "protection, splash exposure and installed piles "
        "remain unresolved. The way forward is to use the re-sized count for this reference "
        "basis, qualify product and installation inputs and assess spatial protection.")
    return summary


def method_text() -> str:
    """Describe the existing physical calculation and its boundaries."""
    method = ("The tubular external area is calculated as A = π D L (4.1), with axis "
        "length L split by elevation and depth. Phase current demand is I = Σ A j f (4.2), "
        "where j is the design current density [3] and f is coating breakdown. "
        "Lifetime required mass is M = I_mean × hours/year × life / (capacity × utilisation) "
        "(4.3). Individual output is i = driving potential / resistance (4.4); "
        "aggregate output is N i. Fresh and depleted resistance are evaluated separately. "
        "The required count is the upward-rounded maximum of mass, initial-output and "
        "final-output counts. The calculation is performed with [I2]; "
        "it is not an independent benchmark. "
        "The coordinate model uses metres with z positive upward from still-water level. "
        "Tubular intersections, caps, internal surfaces, shielding and current distribution "
        "are not resolved. Source dimensions and mass are compared separately from CP adequacy.")
    return method


def finding_blocks(evidence: dict[str, Any], count: int) -> list[TextBlock]:
    """Bind each finding to native demand, capacity and a criterion."""
    findings = []
    for index, check in enumerate(evidence["arrangements"]["stated"]["checks"], 1):
        verdict = "passes" if check["passes"] else "fails"
        findings.append(TextBlock(markdown=(f"## 6.{index} {check['quantity']}\n\n"
            f"The stated {count}-anode arrangement {verdict} the aggregate criterion: "
            f"{check['provided']:.1f} {check['unit']} provided against "
            f"{check['required']:.1f} {check['unit']} required; margin "
            f"{check['margin_percent']:+.1f} %. Criterion: provided ≥ required [3]; "
            "the comparison applies only to the declared inputs, not local protection.")))
    cases = "; ".join(f"{s['evaluated_years']:.1f} yr: {s['required_count']} anodes "
                      f"({s['governing_case']} governs)" for s in evidence["sensitivity"])
    findings.append(TextBlock(markdown=("## 6.4 Required count against design life\n\n"
        f"Bare-pile cases: {cases}. Required count is the maximum of lifetime mass, "
        "initial output and final output requirements. Lifetime mass governs each case. "
        "Doubled life increases exposure duration and water-column coating breakdown, "
        "so required count is recomputed rather than simply doubled. "
        "The count curve quantifies the sizing consequence "
        "of the assumed life; it does not qualify that assumption.")))
    findings.append(TextBlock(markdown=("## 6.5 Anode selection\n\n"
        "The anode size is not optimised. A larger stand-off anode would reduce the "
        "count toward a balance between lifetime mass and final current output. "
        "Qualified product geometry, steel-core displacement and output shall be "
        "evaluated together before a product and layout are selected.")))
    return findings


def reference_text(spec: ReportSpec, evidence: dict[str, Any]) -> str:
    """List published and internal evidence separately."""
    references = ("## 9.1 Published references\n\n"
        "[1] Mroczek, Arwade and Lackner (2023), Design optimization of offshore wind "
        "jacket piles by assessing support structure orientation relative to metocean "
        "conditions, Wind Energy Science 8, 807–817, DOI 10.5194/wes-8-807-2023. "
        "CC BY 4.0.\n\n"
        "[2] Mroczek (2023), IEA-15-240-RWT jacket model, public OpenFAST coordinate "
        "and circular tubular schedules, revision da77a393569b6c5ddf7aa424e5b0a92d37eba833. "
        "Apache License 2.0. Values were transcribed; lengths, areas and mass are derived. "
        "\n\n"
        "[3] DNV-RP-B401, Cathodic protection design, May 2021: Design current densities; "
        "Coating breakdown factors; Anode materials; Anode mass; Anode resistance; "
        "Anode current output.\n\n"
        "## 9.2 Internal references\n\n"
        f"[I1] Input data file, SHA-256 {evidence['input_sha256']} (Appendix: Reproduction).\n\n"
        f"[I2] digitalmodel cathodic protection module, version {spec.tool_version}")
    return references


def section_blocks(cfg: dict[str, Any], evidence: dict[str, Any],
                   spec: ReportSpec) -> list[list[Any]]:
    """Assemble the ten report sections from evaluated evidence."""
    count = evidence["arrangements"]["stated"]["anode_count"]
    summary = summary_text(cfg, evidence)
    method = method_text()
    findings = finding_blocks(evidence, count)
    references = reference_text(spec, evidence)
    model = cfg["structural_model"]
    printed_mass = next(row["printed"] for row in model["comparisons"]
                        if row["quantity"] == "Tubular jacket steel mass")
    combined_mass = model["steel_mass_kg"] + model["excluded_concentrated_mass_kg"]
    mass_reading = (f"Printed structural mass is {printed_mass / 1000:,.1f} t; "
        f"tubular steel plus transition-piece masses give {combined_mass / 1000:,.1f} t "
        f"({100 * (combined_mass / printed_mass - 1):+.1f} %). "
        "The printed figure is read as including the transition piece. "
        "Installed pile mass is not established.")
    blocks = [
        [TextBlock(markdown=summary)],
        [TextBlock(markdown="The reference-jacket geometry is used to evaluate aggregate "
            "sacrificial-anode requirements. Public structural data qualify the geometry; "
            "they do not qualify the CP environmental or product basis. The analysis "
            "covers one reference configuration and its diagnostic lifetime sensitivity.")],
        design_data(cfg), [TextBlock(markdown=method)],
        [reference_figure(cfg["structural_model"]), *interactive_figures(evidence),
         *comparison_tables(evidence), sensitivity_table(evidence),
         *reference_zone_blocks(cfg["structural_model"], cfg["initial_pile_estimate"])],
        findings,
        [TextBlock(markdown="\n\n".join(cfg["cp_limitations"]) + "\n\n" +
            mass_reading + "\n\n"
            "Environment, coating, design life, cylinder geometry, no deducted core and "
            f"alloy applicability are {ASSUMED}. Aggregate outputs are approximate "
            "until these inputs are qualified; no spatial protection acceptance is established.")],
        [TextBlock(markdown="Confirm design life, coating condition, seawater and soil "
            "properties and qualified anode product data before selecting the arrangement. "
            "Confirm installed pile geometry before "
            "finalising exposed area. Establish splash boundaries and above-water measures. "
            "Assess spacing, shielding and remote potentials against the applicable "
            "protection criterion before design acceptance. A dated monitoring interval "
            "requires installed condition and survey evidence, which are not available.")],
        [TextBlock(markdown=references)],
        [TextBlock(markdown="CP: cathodic protection; A: ampere; Ah: ampere-hour; "
            "kg: kilogram; m: metre; yr: year; Ω m: resistivity. N: anode count; "
            "D: outside diameter (m); L: axis length (m); A in equation 4.1: area (m²); "
            "I: aggregate current (A); i: individual output (A); j: current density (A/m²); "
            "f: dimensionless coating breakdown; M: net alloy mass (kg). "
            "PASS/FAIL: satisfaction of a named aggregate comparison, not design acceptance.")],
    ]
    return blocks

def build(destination: Path) -> list[Path]:
    """Write deterministic HTML and its numerical/citation evidence sidecar."""
    spec, evidence = build_document()
    spec.sections.insert(1, revision_history())
    revision = spec.sections[1].blocks[0].rows[0][0]
    page = presentation(render_html(spec), revision)
    destination.mkdir(parents=True, exist_ok=True)
    report = destination / "cathodic-protection-analysis.html"
    report.write_text(page, encoding="utf-8", newline="\n")
    evidence["report_sha256"] = digest(report)
    sidecar = destination / "cathodic-protection-analysis.citations.json"
    sidecar.write_text(json.dumps(evidence, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8", newline="\n")
    return [report, sidecar]


def presentation(page: str, revision: str) -> str:
    """Apply section captions and reference links without changing templates."""
    def section(match: re.Match[str]) -> str:
        body = match.group()
        key = match.group(1)
        number = key.removeprefix("analysis-") if key.startswith("analysis-") else "A"
        table_count = 0
        figure_count = 0

        def table(item: re.Match[str]) -> str:
            nonlocal table_count
            table_count += 1
            content = item.group(1) + item.group(3)
            title = item.group(2)
            if key == "revision-history":
                return content
            return content + f'<div class="cap">Table {number}.{table_count} – {title}</div>'

        body = re.sub(
            r'(<div class="tbl"[^>]*>)\s*<div class="cap">Table \d+\. (.*?)</div>'
            r'(.*?</table>\s*</div>)', table, body, flags=re.S,
        )

        def figure(item: re.Match[str]) -> str:
            nonlocal figure_count
            figure_count += 1
            title = f"Figure {number}.{figure_count} – {item.group(2)}"
            content = item.group(3).replace('<figcaption class="fig-cap">',
                f'<figcaption class="fig-cap">{title}. ', 1)
            return item.group(1) + content + "</figure>"

        body = re.sub(r'(<figure[^>]*>)\s*<div class="chart-title">Figure \d+\. (.*?)</div>'
                      r'(.*?)</figure>', figure, body, flags=re.S)
        if key == "revision-history":
            body = body.replace('<span class="secnum"></span>', '', 1)
        elif number != "A":
            body = body.replace('<span class="secnum"></span>',
                                f'<span class="secnum applabel">{number}</span>', 1)
        if key == "analysis-9":
            for marker in ("1", "2", "3", "I1", "I2"):
                body = body.replace(f"[{marker}]", f'<span id="reference-{marker}">[{marker}]</span>', 1)
        return body

    page = re.sub(r'<section id="([^"]+)">.*?</section>', section, page, flags=re.S)
    page = page.replace("Draft for review", f"Revision {revision} — Issued for information")
    page = re.sub(r'(?<!>)(\[(I?\d+)\])',
                  lambda m: f'<a href="#reference-{m.group(2)}">{m.group(1)}</a>', page)
    return page


def main() -> None:
    """Build the reference analysis in the requested output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DESTINATION)
    args = parser.parse_args()
    for path in build(args.output):
        print(f"{path.name}: {digest(path)}")


if __name__ == "__main__":
    main()
