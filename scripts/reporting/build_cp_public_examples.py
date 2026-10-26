# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild four public CP examples and their release records without solver edits.

This presentation-only release step compares freshly routed results with the
saved results before writing a candidate. The complete-portfolio source-commit
and import guards remain in the separate review-portfolio builder.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import re
import stat
from pathlib import Path
from typing import Any

import yaml

from build_cp_review_portfolio import (
    canonical_citations, check_existing_pack, digest, load_fixture, verify_pack,
    write_json, write_receipt,
)
from cp_review_document import (
    SECTION_TITLES, _review_controls, comment_seed, validate_comments,
)
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.cathodic_protection.b401_structures_phases_schema import rule_citations
from digitalmodel.citations import Citation, validate_citation
from digitalmodel.reporting.engine import render_html, write_report
from digitalmodel.reporting.spec import RevisionRow, Section, TableBlock, TextBlock

ROOT = Path(__file__).resolve().parents[2]
REPORTS = ROOT / "docs/domains/cathodic_protection/reports"
CASES = {"S02": ("Monopile", "monopile.yml"),
         "S03": ("Subsea manifold", "manifold.yml"),
         "S06": ("Bracelet pipeline", "pipeline.yml")}
B401 = "DNV-RP-B401, Cathodic protection design, May 2021"
PRODUCT = "https://www.dnv.com/energy/standards-guidelines/dnv-rp-b401-cathodic-protection-design/"
PIPELINE_PRODUCT = "https://www.dnv.com/energy/standards-guidelines/dnv-rp-f103-cathodic-protection-of-submarine-pipelines/"
CHANGE = ("Worked-example data bases are stated on the first screen. "
          "Public report wording replaces internal review wording. "
          "Criteria and applicable design recommendations are stated beside the results. "
          "Calculated results are unchanged.")


def read_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def compare_results(before: Any, after: Any, path: str = "results") -> None:
    """Compare the complete result tree, with exact numeric equality, not HTML bytes."""
    if isinstance(before, bool) or isinstance(after, bool):
        equal = type(before) is type(after) and before == after
    elif isinstance(before, (int, float)) and isinstance(after, (int, float)):
        equal = math.isfinite(before) and math.isfinite(after) and before == after
    elif isinstance(before, dict) and isinstance(after, dict):
        equal = before.keys() == after.keys()
        if equal:
            for key in before:
                compare_results(before[key], after[key], f"{path}/{key}")
    elif isinstance(before, list) and isinstance(after, list):
        equal = len(before) == len(after)
        if equal:
            for index, (old, new) in enumerate(zip(before, after)):
                compare_results(old, new, f"{path}/{index}")
    else:
        equal = type(before) is type(after) and before == after
    if not equal:
        raise ValueError(f"results changed at {path}")


def number(value: Any, name: str, *, positive: bool = False) -> float:
    """Reject missing, non-finite, non-applicable and boolean decision inputs."""
    try:
        result = float(value)
    except (TypeError, ValueError, OverflowError) as error:
        raise ValueError(f"NOT_EVALUATED: required input {name}") from error
    if isinstance(value, bool) or not math.isfinite(result) or result < 0 or (positive and result <= 0):
        raise ValueError(f"NOT_EVALUATED: invalid input {name}")
    return result


def count(value: Any, name: str) -> int:
    result = number(value, name, positive=True)
    if not result.is_integer():
        raise ValueError(f"NOT_EVALUATED: non-integer {name}")
    return int(result)


def record(value: Any, name: str) -> dict[str, Any]:
    if not isinstance(value, dict) or not value:
        raise ValueError(f"NOT_EVALUATED: required record {name}")
    return value


def b401_checks(results: dict[str, Any]) -> list[dict[str, Any]]:
    """Use the routed mass and phase-output criteria without changing results."""
    if results.get("edition") != "2021":
        raise ValueError("NOT_EVALUATED: recommendation basis requires the May 2021 edition")
    req = record(results.get("anode_requirements"), "anode requirements")
    ver = record(results.get("current_output_verification"), "current output")
    statuses = record(record(results.get("status"), "status").get("checks"), "checks")
    n = count(ver.get("anode_count"), "anode count")
    values = [("Lifetime mass", n * number(req.get("individual_mass_kg"), "unit mass", positive=True),
               req.get("total_mass_kg"), "kg", "[4.7.1], Eq. (4.2)", "mass")]
    values.extend((f"{phase.capitalize()} current output", ver.get(f"total_anode_current_output_{phase}_A"),
                   ver.get(f"{phase}_current_demand_A"), "A", "[4.8.1]–[4.8.5]", f"{phase}_current_output")
                  for phase in ("initial", "final"))
    checks = []
    for label, supplied, demand, unit, locator, key in values:
        provided = number(supplied, label + " provided", positive=True)
        required = number(demand, label + " required", positive=True)
        passed = provided >= required
        if statuses.get(key) is not passed:
            raise ValueError(f"NOT_EVALUATED: missing or inconsistent status for {label}")
        checks.append(dict(label=label, provided=provided, required=required, unit=unit,
                           status="PASS" if passed else "FAIL", basis=f"{B401}, {locator}",
                           margin=100 * (provided / required - 1), criterion="Provided ≥ required"))
    return checks


def design_count(results: dict[str, Any]) -> int:
    b401_checks(results)
    ver = results["current_output_verification"]
    needed = count(ver.get("recommended_anode_count"), "recommended count")
    by_case = [count(ver.get(f"count_by_{key}"), f"count by {key}")
               for key in ("mass", "initial_current", "final_current")]
    if needed != max(by_case):
        raise ValueError("NOT_EVALUATED: inconsistent recommended count")
    req = results["anode_requirements"]
    calculated = [math.ceil(number(req.get("total_mass_kg"), "required mass", positive=True) /
                            number(req.get("individual_mass_kg"), "unit mass", positive=True))]
    for phase in ("initial", "final"):
        calculated.append(math.ceil(number(ver.get(f"{phase}_current_demand_A"), "phase demand", positive=True) /
                                    number(ver.get(f"anode_current_output_{phase}_A"), "unit output", positive=True)))
    if calculated != by_case:
        raise ValueError("NOT_EVALUATED: saved sizing counts disagree with mass or output quantities")
    return needed


def pipeline_checks(results: dict[str, Any]) -> list[dict[str, Any]]:
    req = record(results.get("anode_requirements"), "anode requirements")
    spacing = record(results.get("anode_spacing_m"), "spacing")
    demand = record(results.get("current_demand_A"), "current demand")
    statuses = record(record(results.get("status"), "status").get("checks"), "checks")
    n = count(req.get("anode_count"), "bracelet count")
    standard = "DNVGL-RP-F103, September 2019, amended May 2021"
    values = [("Lifetime mass", n * number(req.get("individual_anode_mass_kg"), "unit mass", positive=True),
               req.get("total_anode_mass_kg"), "kg", "Anode mass", "mass"),
              ("Final current output", n * number(req.get("anode_current_output_A"), "unit output", positive=True),
               demand.get("final_current_demand_A"), "A", "Table 6-3; [6.7.11]", "final_current_output"),
              ("Spacing", spacing.get("max_spacing_m"), spacing.get("spacing_m"), "m",
               "Anode distribution; attenuation analysis", "spacing")]
    checks = []
    for label, supplied, demand_value, unit, locator, key in values:
        provided = number(supplied, label + " capacity", positive=True)
        required = number(demand_value, label + " demand", positive=True)
        passed = provided >= required
        recorded = statuses.get(key)
        if key != "mass" and recorded is not passed:
            raise ValueError(f"NOT_EVALUATED: inconsistent pipeline {label}")
        checks.append(dict(label=label, provided=provided, required=required, unit=unit,
                           status="PASS" if passed else "FAIL", basis=f"{standard}, {locator}",
                           margin=100 * (provided / required - 1),
                           criterion="Allowed spacing ≥ actual spacing" if key == "spacing" else "Provided ≥ required"))
    return checks


def check_table(checks: list[dict[str, Any]]) -> TableBlock:
    return TableBlock(title="Assessed arrangement against each criterion",
        columns=["Quantity", "Provided or limit", "Required or actual", "Unit", "Margin (%)", "Status", "Criterion and basis"],
        rows=[[c["label"], f"{c['provided']:.1f}", f"{c['required']:.1f}", c["unit"],
               f"{c['margin']:+.1f}", c["status"], c["criterion"] + "; " + c["basis"]] for c in checks],
        source="Saved results; sizing and output criteria are identified in Section 9.2.")


def recommendations(results: dict[str, Any], checks: list[dict[str, Any]]) -> str:
    failed = [c for c in checks if c["status"] == "FAIL"]
    if not failed:
        return ("No assessed aggregate criterion fails. Qualify the input basis and anode product "
                "before applying these quantities to a physical installation. Local protection is NOT_EVALUATED; "
                "Required input: electrical continuity, anode positions and a potential survey.")
    needed = design_count(results)
    ver, req = results["current_output_verification"], results["anode_requirements"]
    extra = max(0, needed - ver["anode_count"])
    mass = number(req.get("individual_mass_kg"), "unit mass", positive=True)
    lines = ["The design action is based on the assessed aggregate shortfall."]
    for check in failed:
        lines.append(f"## 8.{len(lines)} {check['label']}\n\n"
            f"The {check['label'].lower()} criterion fails: {check['provided']:.1f} {check['unit']} "
            f"provided against {check['required']:.1f} {check['unit']} required "
            f"({check['margin']:+.1f} % margin). Criterion: {check['criterion']}. Basis: {check['basis']}. "
            f"At design, increase the arrangement from {ver['anode_count']} to {needed} anodes of the "
            f"stated type and unit mass. This adds {extra} anodes and {extra * mass:.1f} kg net alloy. "
            "The count is the saved maximum of the mass, initial-output and final-output counts. "
            "The increased arrangement is a sizing recommendation; it is not an installed-system acceptance. "
            f"Basis: {B401}, [4.7.1], Eq. (4.2), and [4.8.1]–[4.8.5], Eq. (4.3).")
    lines.append(f"In service, retrofit quantity and intervention date are NOT_EVALUATED. "
        f"Required input: installed count, measured remaining alloy mass, elapsed service, future demand, "
        f"electrical continuity and measured potentials. Basis: {B401}, [7.4.1] and [7.5.3] "
        "(retrofit evidence). Obtain these records before selecting an intervention. "
        "Monitoring interval and numerical action trigger are NOT_EVALUATED; an applicable criterion "
        "and survey basis are required. No operating limit is inferred from an aggregate sizing calculation.")
    return "\n\n".join(lines)


def public_spec(cfg: dict[str, Any], title: str):
    """Compose the public report from typed source blocks, without HTML token swaps."""
    spec = anode_design_report(cfg)
    results = cfg["results"]
    checks = b401_checks(results) if results.get("edition") == "2021" else pipeline_checks(results)
    product = PRODUCT if results.get("edition") == "2021" else PIPELINE_PRODUCT
    publication = B401 if results.get("edition") == "2021" else (
        "DNVGL-RP-F103, Cathodic protection of submarine pipelines, September 2019, amended May 2021")
    overall = "FAIL" if any(c["status"] == "FAIL" for c in checks) else "PASS"
    if results.get("status", {}).get("result") != overall:
        raise ValueError("NOT_EVALUATED: overall result disagrees with criteria")
    findings = "\n\n".join(f"- {c['label']}: {c['status']}. {c['provided']:.1f} {c['unit']} "
        f"provided or allowed against {c['required']:.1f} {c['unit']} required or actual. "
        f"Margin: {c['margin']:+.1f} %. Criterion: {c['criterion']}. Basis: {c['basis']}." for c in checks)
    spec.document.project = "Worked example on synthetic data"
    spec.document.client = "Not applicable"
    spec.verification_presentation = True
    spec.worked_example_presentation = True
    spec.document.title = f"Cathodic protection analysis: {title.lower()} worked example"
    old_history = spec.document.revision_history
    spec.document.revision = "01"
    spec.document.number = spec.document.number[:-2] + "01"
    spec.document.date = "2026-10-22"
    spec.document.revision_history = [RevisionRow(rev="01", date="2026-10-22", description=CHANGE), *old_history]
    life = number(results.get("design_life_years"), "design life", positive=True)
    texts = [
        (f"The {title.lower()} is a worked example on synthetic data. Analysis is performed to determine "
         f"aggregate anode demand and capacity over {life:.1f} years. The key results are summarized below.\n\n"
         + findings + "\n\nUse the Recommendations section to resolve the assessed failures. "
         "Local protection is NOT_EVALUATED; an installation-specific potential assessment is required."),
        "The sacrificial-anode calculation is presented as a worked example. No physical installation is represented. "
        "Synthetic dimensions and conditions are illustrative inputs, assumed, based on previous experience.",
        f"The design life is {life:.1f} years. Complete input data and the zone and anode tables are provided "
        "in the calculation appendices. A realistic structural drawing is NOT_EVALUATED; member dimensions "
        "and an installed anode layout are required inputs.",
        "Aggregate requirements are calculated for the declared areas, coating condition and environment. "
        "Anode count is sized against lifetime mass and current output. The criteria and the edition basis "
        "are stated beside each result. Equations and source locators are listed in Section 9.2.",
        "The assessed arrangement is compared with each required quantity below. The margin is calculated "
        "from unrounded values as 100 × (provided / required − 1). For spacing, the margin compares allowed "
        "spacing with actual spacing.",
        "The current assessed results are stated below.\n\n" + findings,
        "Aggregate adequacy applies only to the stated synthetic input basis. Spatial protection, shielding, "
        "electrical continuity and installation condition are NOT_EVALUATED. Required input: anode layout, "
        "member geometry, field potentials and remaining anode mass. A survey interval and intervention "
        "date are not established by this worked example.",
        recommendations(results, checks),
        "## 9.1 Published references\n\n" + f"{publication}. [Publisher product page]({product}). "
        "Citation metadata identify the standard; no standard text is reproduced.\n\n"
        "## 9.2 Calculation basis and locators\n\n" + "\n\n".join(sorted({c["basis"] for c in checks})),
        "CP means cathodic protection. PASS and FAIL state whether a named aggregate criterion is met. "
        "NOT_EVALUATED means that the stated evidence or applicable criterion is missing. "
        "A is ampere; kg is kilogram; m is metre; m² is square metre; yr is year. "
        "N is anode count; I is current; A in the area equation is surface area; "
        "f is coating breakdown fraction. R is resistance in ohms.",
    ]
    original = spec.sections
    appendices = [s for s in original if s.key not in {"adequacy", "references"}]
    for section in appendices:
        section.subtitle = "Calculation data; edition basis and locators are listed in Section 9.2."
    # Retain the numerical output table, while the public criteria table replaces use-policy blocks.
    details = next(s for s in original if s.key == "adequacy")
    appendices.append(Section(key="output-data", title="Anode output data", blocks=[
        b for b in details.blocks if isinstance(b, TableBlock) and b.title != "Adequacy checks"
    ]))
    spec.appendices = appendices
    spec.sections = [Section(key=f"example-{i}", title=heading, blocks=[TextBlock(markdown=text)])
                     for i, (heading, text) in enumerate(zip(SECTION_TITLES, texts), 1)]
    spec.sections[4].blocks.append(check_table(checks))
    spec.sections.insert(1, Section(key="revision-history", title="Revision history", blocks=[
        TableBlock(title="Revision history", columns=["Revision", "Date", "What changed and why"],
                   rows=[[r.rev, r.date, r.description] for r in spec.document.revision_history])]))
    spec.citations = canonical_citations(spec.citations)
    if results.get("edition") == "2021":
        for locator in rule_citations("2021"):
            if any(subject in locator for subject in ("anode mass", "output adequacy", "retrofit evidence")):
                record = dict(spec.citations[0], section=locator, note="Recommendation basis; repository rule register")
                spec.citations.append(record)
    return spec


def validate_citations(spec) -> None:
    for record in spec.citations:
        validate_citation(Citation(**record), repo_root=ROOT)


def write_public_comments(page: str, spec, folder: Path, case_id: str) -> str:
    """Reuse the comment UI and validator without changing review-only draft controls."""
    if not re.fullmatch(r"[A-Z][A-Z0-9_-]{0,39}", case_id):
        raise ValueError("case_id must be a bounded public identifier")
    config = {"report_id": case_id, "revision": spec.document.revision,
              "content_sha256": hashlib.sha256(page.encode("utf-8")).hexdigest(),
              "sections": [{"id": s.key, "title": s.title}
                           for s in [*spec.sections, *spec.appendices]]}
    encoded = json.dumps(config, ensure_ascii=True).replace("<", "\\u003c")
    script = Path(__file__).with_name("cp_review_ui.js").read_text(encoding="utf-8")
    controls = ('<!--CP_REVIEW_START-->' + _review_controls() +
                '<script id="cp-review-config" type="application/json">' + encoded +
                f'</script><script>{script}</script><!--CP_REVIEW_END-->')
    page = page.replace("</body>", controls + "</body>")
    seed = comment_seed(page, case_id, spec.document.revision)
    seed["visual_review"] = "not evaluated"
    validate_comments(page, seed)
    write_json(folder / "report.comments.json", seed)
    return page


def release_pack(folder: Path, *, report: str, basis: str, result_basis: str,
                 previous_release: dict[str, Any] | None = None) -> None:
    """Regenerate change and release files, then use the existing pack checksum step."""
    write_json(folder / "changes.json", {"revision": "1", "date": "2026-10-22", "changes": [CHANGE]})
    (folder / "CHANGES.html").write_text(
        '<!doctype html><html lang="en"><meta charset="utf-8"><title>Report changes</title>'
        f'<h1>Report changes</h1><p>{CHANGE}</p></html>\n', encoding="utf-8", newline="\n")
    release = copy.deepcopy(previous_release) if previous_release else {"source_class": basis}
    release.update({"data_basis": basis, "report": report,
        "results_comparison": "unchanged", "comparison_basis": result_basis,
        "results_sha256": digest(folder / "results.json")})
    write_json(folder / "release.json", release)
    write_receipt(folder)
    verify_pack(folder)


def build_case(case_id: str, destination: Path) -> Path:
    """Rebuild only one explicitly selected synthetic example to a candidate tree."""
    if case_id not in CASES:
        raise ValueError("case is outside public-example scope")
    title, filename = CASES[case_id]
    source = REPORTS / "cases" / case_id
    check_existing_pack(source)
    cfg = yaml.safe_load((source / "input.yml").read_text(encoding="utf-8"))
    fixture = load_fixture(filename)
    compare_results(fixture["inputs"], cfg["inputs"], "inputs")
    routed = run_cathodic_protection(copy.deepcopy(cfg))
    compare_results(read_json(source / "results.json"), routed["results"])
    spec = public_spec(routed, title)
    validate_citations(spec)
    folder = destination / "cases" / case_id
    if folder.resolve() == source.resolve():
        raise ValueError("build to a separate candidate before installation")
    check_existing_pack(folder)
    write_report(spec, folder, "report", pdf="off")
    page = write_public_comments(render_html(spec), spec, folder, case_id)
    (folder / "report.html").write_text(page, encoding="utf-8", newline="\n")
    write_json(folder / "results.json", routed["results"])
    (folder / "input.yml").write_text((source / "input.yml").read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    write_json(folder / "source-citations.json", read_json(source / "source-citations.json"))
    verification = read_json(source / "verification.json")
    if verification["calculation_result"] != routed["results"]["status"]["result"]:
        raise ValueError("saved verification disagrees with the fresh calculation")
    write_json(folder / "verification.json", verification)
    release_pack(folder, report="report.html", basis="synthetic data", result_basis="saved results.json",
                 previous_release=read_json(source / "release.json"))
    return folder


def reference_results(evidence: dict[str, Any]) -> dict[str, Any]:
    """Select every pre-existing numerical result tree in the jacket sidecar."""
    return {key: evidence[key] for key in ("arrangements", "sensitivity", "zone_demand_A")}


def build_reference(destination: Path) -> Path:
    import build_reference_jacket_analysis as reference

    source = REPORTS / "reference-jacket"
    baseline = read_json(source / "cathodic-protection-analysis.citations.json")
    spec, evidence = reference.build_document()
    compare_results(reference_results(baseline), reference_results(evidence))
    validate_citations(spec)
    spec.sections.insert(1, reference.revision_history())
    folder = destination / "reference-jacket"
    if folder.resolve() == source.resolve():
        raise ValueError("build to a separate candidate before installation")
    check_existing_pack(folder)
    folder.mkdir(parents=True, exist_ok=True)
    page = reference.presentation(render_html(spec), "1")
    page = write_public_comments(page, spec, folder, "REFERENCE-JACKET")
    report = folder / "cathodic-protection-analysis.html"
    report.write_text(page, encoding="utf-8", newline="\n")
    evidence["report_sha256"] = digest(report)
    write_json(folder / "cathodic-protection-analysis.citations.json", evidence)
    write_json(folder / "results.json", reference_results(evidence))
    write_json(folder / "verification.json", {"results_unchanged": True,
        "comparison_basis": "all numerical trees in the saved citation sidecar",
        "engineering_acceptance": "NOT_EVALUATED"})
    write_json(folder / "report_manifest.json", {"artifacts": {
        "html": report.name, "citations": "cathodic-protection-analysis.citations.json",
        "results": "results.json", "pdf": None}, "source_class": "published geometry",
        "input_sha256": evidence["input_sha256"], "pdf": "not generated"})
    release_pack(folder, report=report.name, basis="published geometry with synthetic CP inputs",
                 result_basis="all numerical trees in the saved citation sidecar")
    return folder


def validate_install_path(path: Path, root: Path) -> None:
    """Reject links and Windows reparse points in every installation segment."""
    current = root
    for part in ("", *path.relative_to(root).parts):
        current = current / part
        try:
            details = current.lstat()
        except FileNotFoundError:
            # New pack files are permitted; their existing parents are checked.
            continue
        if stat.S_ISLNK(details.st_mode) or getattr(details, "st_reparse_tag", 0):
            raise ValueError("unsafe installation path")
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("unsafe installation destination")


def install(candidate: Path) -> None:
    """Copy verified selected packs only; retain unrelated release receipt entries."""
    folders = [candidate / "cases" / key for key in CASES] + [candidate / "reference-jacket"]
    receipt_path = REPORTS / "portfolio-receipt.json"
    validate_install_path(receipt_path, REPORTS)
    receipt = read_json(receipt_path)
    for folder in folders:
        validate_install_path(folder, candidate)
        relative = folder.relative_to(candidate)
        target = REPORTS / relative
        validate_install_path(target, REPORTS)
        for path in sorted(folder.iterdir()):
            validate_install_path(path, candidate)
            validate_install_path(target / path.name, REPORTS)
            if not path.is_file():
                raise ValueError("unsafe report file")
        verify_pack(folder)
        check_existing_pack(target)
        if (target / "results.json").is_file():
            compare_results(read_json(target / "results.json"), read_json(folder / "results.json"))
        for path in sorted(folder.iterdir()):
            name = (relative / path.name).as_posix()
            old = target / path.name
            if name in receipt and digest(old) != receipt[name]:
                raise ValueError("existing selected release receipt differs")
    for folder in folders:
        relative = folder.relative_to(candidate)
        target = REPORTS / relative
        for path in sorted(folder.iterdir()):
            (target / path.name).write_text(path.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
            receipt[(relative / path.name).as_posix()] = digest(target / path.name)
        verify_pack(target)
    write_json(receipt_path, receipt)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--install", action="store_true")
    args = parser.parse_args()
    if args.output.resolve().is_relative_to(ROOT.resolve()):
        parser.error("candidate output must be outside the repository")
    for case_id in CASES:
        build_case(case_id, args.output)
    build_reference(args.output)
    if args.install:
        install(args.output)
    print("Four public examples rebuilt; numerical results unchanged; release checksums verified.")


if __name__ == "__main__":
    main()
