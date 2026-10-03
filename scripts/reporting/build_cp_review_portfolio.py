"""Build repository CP review packs without modifying engines or templates.

Run from the repository root with the repository's src on PYTHONPATH and a
current licensed-citation wiki checkout configured through LLM_WIKI_PATH.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import html
import json
from importlib.metadata import version
from pathlib import Path
from typing import Any

import yaml  # type: ignore[import-untyped]

from cp_portfolio_contract import (
    coverage_summary, resolve_pointer, validate_child,
)

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs"
DESTINATION = ROOT / "docs/domains/cathodic_protection/reports"
SOURCE_REVISION = "bf638ed1"
CASES = (
    ("S01", "Jacket", "jacket.yml"),
    ("S02", "Monopile", "monopile.yml"),
    ("S03", "Subsea manifold", "manifold.yml"),
    ("S04", "Ship hull", "ships.yml"),
    ("S05", "Floating offshore structure", "fpso.yml"),
    ("S06", "Bracelet pipeline", "pipeline.yml"),
    ("S07", "Terminal-bank flowline", "anode_bank.yml"),
    ("S08", "Multi-component riser", "multi_component_riser.yml"),
    ("S09", "Phased riser base and retrofit", "riser_base_phased.yml"),
    ("S10", "Temporary service", "temporary_service.yml"),
    ("S11", "Concrete, buried and seawater families", "mixed_family.yml"),
)
BENCHMARKS = (
    ("A", "Hybrid-riser buoyancy", "Historical numerical acceptance deferred"),
    ("B", "Temporary riser and foundation", "Source exception needs qualification"),
    ("C", "Coated floating-storage hull", "Rebuilt ABS route; private data not released"),
    ("D", "Walking-mitigation mattress", "Current private comparison required"),
    ("E", "Bracelet pipeline", "Field-joint infill evidence missing; acceptance unverified"),
)


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False,
                               allow_nan=False) + "\n", encoding="utf-8", newline="\n")


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load_fixture(filename: str) -> dict[str, Any]:
    """Permit only frozen repository fixtures, never an arbitrary client input."""
    release = json.loads(Path(__file__).with_name("cp_portfolio_inputs.json").read_text("utf-8"))
    entry = release.get(filename)
    if not entry or entry.get("source_class") != "synthetic_repository_regression":
        raise ValueError("missing repository fixture release record")
    if digest(FIXTURES / filename) != entry["sha256"]:
        raise ValueError("release fixture checksum changed; review new input before rebuilding")
    return dict(yaml.safe_load((FIXTURES / filename).read_text("utf-8")))


def check_existing_pack(folder: Path) -> None:
    """Do not overwrite reviewer work or a locally modified review package."""
    if (folder / "checksums.json").exists():
        verify_pack(folder)
    sidecar = folder / "report.comments.json"
    if sidecar.exists():
        state = json.loads(sidecar.read_text("utf-8"))
        if state.get("comments") or state.get("prior_rounds") or state.get("decision_conflicts"):
            raise ValueError("review comments exist; preserve the review round before rebuilding")


def canonical_citations(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Repair the bounded B4012021 report-page mismatch; never weaken validation."""
    from digitalmodel.cathodic_protection.b401_tables import B401_WIKI_PATH, B401_WIKI_PATH_2021
    records = copy.deepcopy(records)
    for record in records:
        if ((record["code_id"], record["publisher"], record["wiki_path"]) == (
            "dnv-rp-b401", "DNV", B401_WIKI_PATH
        ) and record["revision"] in ("2021", "2021-05")):
            record["wiki_path"] = B401_WIKI_PATH_2021
            record["revision"] = "2021-05"
    return records


def write_receipt(folder: Path) -> None:
    files = {p.name: digest(p) for p in sorted(folder.iterdir())
             if p.is_file() and p.name != "checksums.json"}
    write_json(folder / "checksums.json", files)


def verify_pack(folder: Path) -> None:
    """Verify every byte bound by the receipt, rejecting unsafe relative names."""
    files = json.loads((folder / "checksums.json").read_text("utf-8"))
    for name, expected in files.items():
        if Path(name).name != name or "/" in name or "\\" in name:
            raise ValueError("unsafe checksum filename")
        target = folder / name
        if not target.is_file():
            raise ValueError(f"missing pack file: {name}")
        if digest(target) != expected:
            raise ValueError(f"checksum mismatch: {name}")


def child_evidence(cfg: dict[str, Any]) -> list[dict[str, str]]:
    """Give each component, phase-family and retrofit its own result link."""
    assessment = cfg["results"].get("riser_base_assessment")
    if not assessment:
        return []
    children = []
    prefix = "/riser_base_assessment"
    for index, name in enumerate(assessment["components"]):
        token = name.replace("~", "~0").replace("/", "~1")
        children.append({"anchor": f"child-component-{index}", "label": name,
                         "result_pointer": f"{prefix}/components/{token}",
                         "input_pointer": f"{prefix}/components/{index}"})
    for index, family in enumerate(cfg["inputs"]["anode_families"]):
        token = family["name"].replace("~", "~0").replace("/", "~1")
        children.append({"anchor": f"child-family-{index}", "label": family["name"] + " family",
                         "result_pointer": f"/anode_families/{token}",
                         "input_pointer": f"/anode_families/{index}"})
    for index, phase in enumerate(assessment["phases"]):
        for number, name in enumerate(phase["families"]):
            token = name.replace("~", "~0").replace("/", "~1")
            children.append({"anchor": f"child-phase-{index}-family-{number}",
                             "label": f"{phase['name']} / {name}",
                             "result_pointer": f"{prefix}/phases/{index}/families/{token}",
                             "input_pointer": f"{prefix}/phases/{index}/families/{number}"})
    if assessment.get("retrofit"):
        children.append({"anchor": "child-retrofit", "label": "Retrofit disposition",
                         "result_pointer": f"{prefix}/retrofit",
                         "input_pointer": f"{prefix}/retrofit"})
    return children


def append_children(spec: Any, children: list[dict[str, str]], results: Any) -> None:
    from digitalmodel.reporting.spec import Section, TextBlock
    for child in children:
        value = json.dumps(resolve_pointer(results, child["result_pointer"]), indent=2)
        spec.appendices.append(Section(key=child["anchor"], title=child["label"], blocks=[
            TextBlock(markdown=f"Input pointer: `{child['input_pointer']}`\n\nResult pointer: `{child['result_pointer']}`\n\n```json\n{value}\n```"),
        ]))


def build_case(case: tuple[str, str, str], destination: Path,
               register: dict[str, Any]) -> dict[str, Any]:
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
    from digitalmodel.cathodic_protection.report_adapters import anode_design_report
    from digitalmodel.citations import Citation, validate_citation
    from digitalmodel.reporting.engine import write_report
    from cp_review_document import comment_seed, render_review, validate_comments
    from cp_portfolio_coverage import validate_mode_coverage
    case_id, title, filename = case
    cfg = load_fixture(filename)
    cfg["report"] = {"document": register[case_id]}
    original = copy.deepcopy(cfg)
    routed = run_cathodic_protection(cfg)
    validate_mode_coverage(case_id, routed)
    spec = anode_design_report(routed)
    source_citations = copy.deepcopy(spec.citations)
    spec.citations = canonical_citations(spec.citations)
    for record in spec.citations:
        validate_citation(Citation(**record))
    if not spec.citations and case_id != "S05":
        raise ValueError(f"missing citations for {case_id}")
    children = child_evidence(routed)
    append_children(spec, children, routed["results"])
    folder = destination / "cases" / case_id
    check_existing_pack(folder)
    folder.mkdir(parents=True, exist_ok=True)
    write_report(spec, folder, "report", pdf="off")
    page = render_review(spec, case_id)
    (folder / "report.html").write_text(page, encoding="utf-8", newline="\n")
    seed = comment_seed(page, case_id, "00")
    validate_comments(page, seed)
    write_json(folder / "report.comments.json", seed)
    write_json(folder / "results.json", routed["results"])
    write_json(folder / "source-citations.json", source_citations)
    write_changes(folder)
    (folder / "input.yml").write_text(yaml.safe_dump(original, sort_keys=False), encoding="utf-8", newline="\n")
    for child in children:
        resolve_pointer(routed["inputs"], child["input_pointer"])
        validate_child(child, routed["results"], page)
    row = case_receipt(case, routed, children)
    write_json(folder / "verification.json", row)
    write_json(folder / "release.json", {"source_class": "repository_regression",
               "input_fixture": filename, "input_sha256": digest(FIXTURES / filename),
               "private_benchmark_data": False})
    write_receipt(folder)
    verify_pack(folder)
    return row


def write_changes(folder: Path) -> None:
    (folder / "CHANGES.html").write_text(
        '<!doctype html><html lang="en"><meta charset="utf-8"><title>Report changes</title>'
        '<h1>Portfolio presentation changes</h1><p>Existing solver results retained. '
        'Added ten review sections, comment transfer controls, exact-file sidecar, '
        'review-only document identity and phase/component evidence anchors. '
        'B401:2021 citation labels are normalized to the owned May 2021 wiki page; '
        'original adapter labels remain in source-citations.json. '
        'No shared engine, template or numerical kernel changed.</p><p>'
        'Comprehensive visual/UI/print review deferred by owner. Engineer-of-record '
        'acceptance remains pending; calculation FAIL is retained. '
        '<a href="https://github.com/vamseeachanta/digitalmodel/issues/2284">Shared citation defect</a>.</p></html>', encoding="utf-8")


def case_receipt(case: tuple[str, str, str], cfg: Any, children: Any) -> dict[str, Any]:
    case_id, title, filename = case
    status = cfg["results"]["status"]
    return {"id": case_id, "title": title, "fixture": filename,
            "report": f"cases/{case_id}/report.html", "children": children,
            "route": cfg["inputs"]["calculation_type"],
            "calculation_result": status["result"], "use_status": status["use_status"],
            "pack_state": "verified", "verification": {"passed": True,
                "scope": "citation resolution, comment binding, child links and file checksums"},
            "engineering_review": "pending", "visual_review": "deferred_by_owner",
            "source_revision": SOURCE_REVISION,
            "source_class": "synthetic_repository_regression"}


def benchmark_rows(destination: Path) -> list[dict[str, Any]]:
    rows = []
    for case_id, title, limitation in BENCHMARKS:
        row = {"id": case_id, "title": title, "pack_state": "blocked",
               "reason": limitation, "private_data_release": "not_authorized",
               "report": None, "engineering_review": "pending",
               "public_source": "https://github.com/vamseeachanta/digitalmodel/issues/1852#issuecomment-5893200921"}
        folder = destination / "benchmarks" / case_id
        folder.mkdir(parents=True, exist_ok=True)
        write_json(folder / "status.json", row)
        rows.append(row)
    return rows


def write_index(destination: Path, rows: list[dict[str, Any]]) -> None:
    summary = coverage_summary(rows)
    write_json(destination / "coverage.json", {"summary": summary, "rows": rows,
        "excluded_routes": {"DNV_RP_B401_offshore_legacy": "Deprecated; no report builder",
                            "DNV_RP_F103_2010_legacy": "Deprecated; no report builder"},
        "compatibility_variants": ["DNV_RP_F103_2010", "ABS_gn_ships_2018_legacy"],
        "outside_structure_design_scope": ["experimental ICCP", "stray current", "galvanic", "survey/depletion assessment"],
        "phase_scope": "S09 wet-storage and operating ledgers cover the sea family only; other installed families have component-local results."})
    entries = []
    for row in rows:
        title = html.escape(row["title"])
        if row.get("report"):
            title = f'<a href="{row["report"]}">{title}</a>'
        entries.append(f"<tr><td>{row['id']}</td><td>{title}</td>"
                       f"<td>{row['pack_state']}</td><td>{html.escape(row.get('calculation_result', 'Not assessed'))}</td>"
                       f"<td>{html.escape(row.get('use_status', row.get('reason', '')))}</td></tr>")
    page = '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width">'
    page += '<title>CP module review portfolio</title><style>body{font:16px system-ui;max-width:1100px;margin:2rem auto;padding:1rem}td,th{padding:.6rem;border:1px solid #ccc;text-align:left}table{border-collapse:collapse}div{overflow:auto}</style>'
    page += '<h1>CP module review portfolio — incomplete</h1><p>Internal review drafts — not issued. Engineering acceptance pending. Comprehensive visual/UI/print review deferred by owner.</p>'
    page += f'<p>{summary["verified"]} of {summary["required"]} packs verified. Benchmark blockers do not count as completed reports.</p>'
    page += '<p>Calculation PASS does not establish route eligibility or engineering approval. Proposed reporting conventions apply only to this portfolio.</p>'
    page += '<div><table><tr><th>Case</th><th>Structure</th><th>Pack</th><th>Calculation</th><th>Use status / gap</th></tr>'
    page += ''.join(entries) + '</table></div><p><a href="coverage.json">Coverage and child evidence</a> · <a href="../reporting-roadmap.html">Roadmap</a></p></html>'
    (destination / "index.html").write_text(page, encoding="utf-8", newline="\n")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, help="New candidate directory, outside final reports")
    parser.add_argument("--publish-candidate", type=Path)
    parser.add_argument("--release-register", type=Path)
    parser.add_argument("--partial-review", action="store_true",
                        help="Publish explicitly incomplete internal review work; never goal completion")
    args = parser.parse_args()
    if args.publish_candidate:
        from cp_portfolio_storage import publish
        if not args.release_register:
            parser.error("Publication requires an exact artifact release register after agent review")
        release = json.loads(args.release_register.read_text("utf-8"))
        validate_candidate(args.publish_candidate)
        publish(args.publish_candidate, DESTINATION, release, partial_review=args.partial_review)
        return
    if args.output is None or args.output.resolve() == DESTINATION.resolve():
        parser.error("Build to a new --output candidate directory before review/publication")
    if args.output.exists() and any(args.output.iterdir()):
        parser.error("Candidate destination is not empty; existing work will not be overwritten")
    args.output.mkdir(parents=True, exist_ok=True)
    register = json.loads(Path(__file__).with_name("cp_portfolio_documents.json").read_text("utf-8"))
    write_json(args.output / "document-register.json", register)
    write_json(args.output / "build-environment.json", {
        package: version(package) for package in ("plotly", "jinja2", "pydantic", "PyYAML")
    })
    rows = [build_case(case, args.output, register) for case in CASES]
    rows += benchmark_rows(args.output)
    write_index(args.output, rows)
    print(json.dumps(coverage_summary(rows)))


def validate_candidate(candidate: Path) -> None:
    """Recheck the complete matrix and every claimed verified pack before release."""
    from cp_review_document import validate_comments
    from cp_portfolio_coverage import validate_mode_coverage
    coverage = json.loads((candidate / "coverage.json").read_text("utf-8"))
    if coverage_summary(coverage["rows"]) != coverage["summary"]:
        raise ValueError("coverage summary disagrees with row receipts")
    for row in coverage["rows"]:
        if row["pack_state"] != "verified":
            continue
        folder = candidate / "cases" / row["id"]
        verify_pack(folder)
        receipt = json.loads((folder / "verification.json").read_text("utf-8"))
        if receipt != row:
            raise ValueError("coverage row disagrees with pack receipt")
        page = (folder / "report.html").read_text("utf-8")
        validate_comments(page, json.loads((folder / "report.comments.json").read_text("utf-8")))
        cfg = yaml.safe_load((folder / "input.yml").read_text("utf-8"))
        cfg["results"] = json.loads((folder / "results.json").read_text("utf-8"))
        validate_mode_coverage(row["id"], cfg)
        for child in row["children"]:
            validate_child(child, cfg["results"], page)
            resolve_pointer(cfg["inputs"], child["input_pointer"])


if __name__ == "__main__":
    main()
