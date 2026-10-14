# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the reproducible draft reference-jacket cathodic protection analysis.

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
from digitalmodel.reporting.spec import ReportSpec, Section, TableBlock, TextBlock

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml"
DESTINATION = ROOT / "docs/domains/cathodic_protection/reports/reference-jacket"
STANDARD_REVISION = "bc2472877b0af9e399ccb4b4fcb8eb8a8b7b5721"
SECTION_TITLES = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
ASSUMED = "assumed, based on previous experience"


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
    """Test a derived doubled-life case with the same physical arrangement."""
    candidate = copy.deepcopy(cfg)
    life = float(candidate["inputs"]["design_data"]["design_life"])
    candidate["inputs"]["design_data"]["design_life"] = 2 * life
    candidate["inputs"]["anode"]["count"] = count
    result = run_cathodic_protection(candidate)["results"]
    comparison = checks(candidate, result)
    return [{"quantity": "Design life", "base_years": life,
             "evaluated_years": 2 * life, "derivation": "twice the declared design life",
             "changes_conclusion": any(not c["passes"] for c in comparison["checks"]),
             "comparison": comparison}]


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
            f"{inputs['structure']['zones'][0]['coating_category']}; both are {ASSUMED} [I1]. "
            "They control lifetime mass and current demand. "
            f"The aluminium stand-off cylinder is {anode['length_m']:.1f} m long with "
            f"radius {anode['radius_m']:.3f} m, {ASSUMED} [I1]. "
            f"Net mass is {anode['individual_anode_mass_kg']:.1f} kg, derived from "
            "π × radius² × length × the repository alloy density [I2]; no core is deducted. "
            "Dimensions and alloy performance affect both mass and output. "
            "Attachment, core, product qualification and spatial layout are not established.")),
        TextBlock(markdown=("## 3.3 Environmental data\n\n"
            f"Temperature {env['seawater_temperature_C']:.1f} °C and seawater resistivity "
            f"{env['seawater_resistivity_ohm_m']:.1f} Ω m are {ASSUMED} [I1]. "
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
            "Current densities, coating breakdown, capacity, utilisation and potentials are "
            "selected by the repository edition route [I2], not independently verified "
            "against the licensed standard in this analysis. Their applicability is assumed "
            "and requires confirmation; no standard table is reproduced. "
            "An aggregate PASS denotes these comparisons only; no local potential criterion is evaluated.")),
        TextBlock(markdown="## 3.6 Method\n\nThe physical method and equations are presented in Section 4."),
        TextBlock(markdown=("## 3.7 Load cases\n\nThe stated arrangement is the "
            "mass-sized quantity evaluated for initial, mean and final coating conditions. "
            "Water-column steel is split at the repository depth-band boundary; buried pile "
            "area is evaluated separately. A doubled-design-life sensitivity retains the "
            "same anode count and all other inputs; it is a diagnostic scenario, not a selected design.")),
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
                source="[I2]; criterion: provided ≥ required; native values retained in the citation sidecar.",
            ))
    return blocks


def build_document() -> tuple[ReportSpec, dict[str, Any]]:
    """Build a draft from the frozen fixture, with qualified comparisons."""
    cfg = run_cathodic_protection(yaml.safe_load(FIXTURE.read_text()))
    arrangements = reference_arrangements(cfg)
    evidence: dict[str, Any] = {
        "status": "DRAFT: input qualification required",
        "input_sha256": digest(FIXTURE), "arrangements": {},
    }
    for name, result in arrangements.items():
        if result is not None:
            evidence["arrangements"][name] = checks(cfg, result)
    count = evidence["arrangements"]["stated"]["anode_count"]
    evidence["sensitivity"] = sensitivity(cfg, count)
    spec = anode_design_report(cfg)
    blocks = section_blocks(cfg, evidence, spec)
    spec.sections = [Section(key=f"analysis-{i}", title=title, blocks=content)
                     for i, (title, content) in enumerate(zip(SECTION_TITLES, blocks), 1)]
    spec.appendices = [Section(key="structural-evidence", title="Structural evidence", blocks=[
        *reference_member_tables(cfg["structural_model"]),
        *reference_comparison(cfg["structural_model"]),
    ])]
    citations = _citations_from_labels(cfg["results"]["citations"], "Existing CP route; no standard tables exported.")
    for citation in citations:
        validate_citation(citation)
    evidence["citations"] = [asdict(c) for c in citations]
    evidence["geometry_sources"] = cfg["structural_model"]["source_files"]
    evidence["reporting_standard_revision"] = STANDARD_REVISION
    return spec, evidence



def summary_text(cfg: dict[str, Any], count: int) -> str:
    """State the conditional solution and its governing evidence gaps."""
    summary = (f"The reference jacket at {cfg['structural_model']['water_depth_m']:.1f} m "
        f"water depth is analysed with {count} anodes. "
        "Aggregate lifetime mass and initial and final current output are compared with "
        "the required quantities below, using DNV-RP-B401, Cathodic protection design, "
        "May 2021 [3]. Draft: input qualification required. The arrangement is a conditional "
        "solution on the declared basis; environmental, coating, lifetime and anode inputs "
        "are assumed. The doubled-life sensitivity changes the conclusion. Spatial "
        "protection, splash exposure, installed piles and the structural mass discrepancy "
        "remain unresolved. The way forward is input confirmation and a spatial protection assessment.")
    return summary


def method_text() -> str:
    """Describe the existing physical calculation and its boundaries."""
    method = ("The tubular external area is calculated as A = π D L (4.1), with axis "
        "length L split by elevation and depth. Phase current demand is I = Σ A j f (4.2), "
        "where j is the edition-selected current density and f is coating breakdown. "
        "Lifetime required mass is M = I_mean × hours/year × life / (capacity × utilisation) "
        "(4.3). Individual output is i = driving potential / resistance (4.4); "
        "aggregate output is N i. Fresh and depleted resistance are evaluated separately. "
        "The required count is the upward-rounded maximum of mass, initial-output and "
        "final-output counts. Native comparisons precede display rounding. The existing "
        "CP route [I2] supplies the calculation; it is not an independent solver benchmark. "
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
    scenario = evidence["sensitivity"][0]
    findings.append(TextBlock(markdown=("## 6.4 Input qualification\n\n"
        f"Increasing the assumed lifetime from {scenario['base_years']:.1f} to "
        f"{scenario['evaluated_years']:.1f} yr while retaining {count} anodes fails "
        "at least one aggregate criterion. Design life has no qualified source and "
        "changes the conclusion; the report remains a draft. Other assumed inputs "
        "also require confirmation; no uncertainty bound is established.")))
    return findings


def reference_text(spec: ReportSpec) -> str:
    """List published and internal evidence separately."""
    references = ("## 9.1 Published references\n\n"
        "[1] Mroczek, Arwade and Lackner (2023), Design optimization of offshore wind "
        "jacket piles by assessing support structure orientation relative to metocean "
        "conditions, Wind Energy Science 8, 807–817, DOI 10.5194/wes-8-807-2023. "
        "CC BY 4.0. The held fixture supplies the paper locators; the paper was not reopened.\n\n"
        "[2] Mroczek (2023), IEA-15-240-RWT jacket model, public OpenFAST coordinate "
        "and circular tubular schedules, revision da77a393569b6c5ddf7aa424e5b0a92d37eba833. "
        "Apache License 2.0. Values were transcribed; lengths, areas and mass are derived. "
        "Source attribution and the licence remain with the input fixture.\n\n"
        "[3] DNV-RP-B401, Cathodic protection design, May 2021: Design current densities; "
        "Coating breakdown factors; Anode materials; Anode mass; Anode resistance; "
        "Anode current output. Clause titles follow the repository calculation basis; "
        "the licensed document was not reopened.\n\n"
        "## 9.2 Internal references\n\n"
        f"[I1] {FIXTURE.relative_to(ROOT).as_posix()}; jacket.yml supplies the assumed CP inputs.\n\n"
        "[I2] digitalmodel CP engine adapter, kernels and edition route; "
        f"software version {spec.tool_version}. Citation schema: digitalmodel.citations.schema.Citation.\n\n"
        "[I3] Proposed engineering reporting conventions, P6, 13 October 2026; "
        f"workspace-hub revision {STANDARD_REVISION}.\n\n"
        "[I4] Reproduction: PYTHONPATH=src uv run --no-sources python "
        "scripts/reporting/build_reference_jacket_analysis.py; "
        "LLM_WIKI_PATH identifies the existing citation wiki. "
        "The report and sidecar are emitted together; no website publication is performed.")
    return references


def section_blocks(cfg: dict[str, Any], evidence: dict[str, Any],
                   spec: ReportSpec) -> list[list[Any]]:
    """Assemble the ten report sections from evaluated evidence."""
    count = evidence["arrangements"]["stated"]["anode_count"]
    summary = summary_text(cfg, count)
    history = TableBlock(title="Revision history", columns=["Revision", "Date", "What changed and why"],
        rows=[["00", "2026-10-14", "Draft reference-jacket analysis; sourced geometry and conditional CP comparisons."]])
    method = method_text()
    findings = finding_blocks(evidence, count)
    references = reference_text(spec)
    blocks = [
        [TextBlock(markdown=summary), history, *comparison_tables(evidence)],
        [TextBlock(markdown="The reference-jacket geometry is used to evaluate aggregate "
            "sacrificial-anode requirements. Public structural data qualify the geometry; "
            "they do not qualify the CP environmental or product basis. The analysis "
            "covers one reference configuration and its diagnostic lifetime sensitivity.")],
        design_data(cfg), [TextBlock(markdown=method)],
        [reference_figure(cfg["structural_model"]), *comparison_tables(evidence),
         *reference_zone_blocks(cfg["structural_model"], cfg["initial_pile_estimate"])],
        findings,
        [TextBlock(markdown="\n\n".join(cfg["cp_limitations"]) + "\n\n" +
            cfg["structural_model"]["mass_limitation"] + "\n\n"
            "Environment, coating, design life, cylinder geometry, no deducted core and "
            f"alloy applicability are {ASSUMED}. Aggregate outputs are approximate "
            "until these inputs are qualified; no spatial protection acceptance is established.")],
        [TextBlock(markdown="Confirm design life, coating condition, seawater and soil "
            "properties and qualified anode product data before selecting the arrangement. "
            "Resolve the structural mass discrepancy and installed pile geometry before "
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
    page = presentation(render_html(spec))
    destination.mkdir(parents=True, exist_ok=True)
    report = destination / "cathodic-protection-analysis.html"
    report.write_text(page, encoding="utf-8", newline="\n")
    evidence["report_sha256"] = digest(report)
    sidecar = destination / "cathodic-protection-analysis.citations.json"
    sidecar.write_text(json.dumps(evidence, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")
    return [report, sidecar]


def presentation(page: str) -> str:
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
        if number != "A":
            body = body.replace('<span class="secnum"></span>',
                                f'<span class="secnum applabel">{number}</span>', 1)
        if key == "analysis-9":
            for marker in ("1", "2", "3", "I1", "I2", "I3", "I4"):
                body = body.replace(f"[{marker}]", f'<span id="reference-{marker}">[{marker}]</span>', 1)
        return body

    page = re.sub(r'<section id="([^"]+)">.*?</section>', section, page, flags=re.S)
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
