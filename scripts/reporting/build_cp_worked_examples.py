# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Refresh public presentation while retaining the original calculation records.

The synthetic cases are recalculated from their pack inputs and must equal the
stored results. Citation receipts retain their original provenance; this command
does not claim a fresh resolution of licensed sources. Output defaults to no
location: an explicit destination is required.
"""

from __future__ import annotations

import argparse
import copy
import json
import shutil
from pathlib import Path

import yaml

from build_cp_review_portfolio import (
    check_existing_pack, digest, load_fixture, verify_pack, write_json, write_receipt,
)
from build_reference_jacket_analysis import FIXTURE, WORKED_EXAMPLE_BASIS
from cp_review_document import comment_seed, render_bound, validate_comments
from cp_worked_example import CASES, public_spec
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report


ROOT = Path(__file__).resolve().parents[2]
REPORTS = ROOT / "docs/domains/cathodic_protection/reports"


def rebuild_case(source: Path, destination: Path, case_id: str) -> None:
    """Fail closed on changed calculation evidence or existing reviewer work."""
    check_existing_pack(source)
    cfg = yaml.safe_load((source / "input.yml").read_text())
    fixture = {"S02": "monopile.yml", "S03": "manifold.yml", "S06": "pipeline.yml"}[case_id]
    if cfg["inputs"] != load_fixture(fixture)["inputs"]:
        raise ValueError("input differs from the released synthetic fixture")
    expected = json.loads((source / "results.json").read_text())
    routed = run_cathodic_protection(copy.deepcopy(cfg))
    if routed["results"] != expected:
        raise ValueError("recalculated results differ from the retained evidence")
    spec = public_spec(anode_design_report(routed), case_id, expected)
    page = render_bound(spec, case_id, status="Worked example — issued for information")
    seed = comment_seed(page, case_id, spec.document.revision)
    validate_comments(page, seed)
    if destination.exists():
        check_existing_pack(destination)
    shutil.copytree(source, destination, dirs_exist_ok=True)
    (destination / "report.html").write_text(page, encoding="utf-8", newline="\n")
    write_json(destination / "report.comments.json", seed)
    manifest = json.loads((source / "report_manifest.json").read_text())
    manifest["document"] = {key: getattr(spec.document, key) for key in manifest["document"]}
    manifest["presentation"] = {
        "builder": "scripts/reporting/build_cp_worked_examples.py",
        "basis": "synthetic worked example; original numerical and citation provenance retained",
        "results_reproduced": True,
        "licensed_sources_resolved_again": False,
    }
    write_json(destination / "report_manifest.json", manifest)
    write_receipt(destination)
    verify_pack(destination)


def refresh_reference(source: Path, destination: Path) -> None:
    """Apply the generator's wording revision to a digest-bound issued report."""
    report = source / "cathodic-protection-analysis.html"
    evidence = json.loads((source / "cathodic-protection-analysis.citations.json").read_text())
    if digest(report) != evidence["report_sha256"] or digest(FIXTURE) != evidence["input_sha256"]:
        raise ValueError("reference report or input digest mismatch")
    page = report.read_text()
    prefix = "<p>The reference jacket at "
    if WORKED_EXAMPLE_BASIS not in page:
        if page.count(prefix) != 1:
            raise ValueError("reference summary is ambiguous")
        page = page.replace(prefix, "<p>" + WORKED_EXAMPLE_BASIS + "The reference jacket at ", 1)
    old_row = "<tr><td>0</td><td>2026-10-14</td><td>Issued for information.</td></tr>"
    new_row = "<tr><td>1</td><td>2026-11-05</td><td>Label the published-geometry worked example and its design limits.</td></tr>"
    if new_row not in page:
        if page.count(old_row) != 1:
            raise ValueError("reference revision history is ambiguous")
        page = page.replace(old_row, old_row + "\n            " + new_row, 1)
    page = page.replace("Revision 0 — Issued for information", "Revision 1 — Issued for information")
    destination.mkdir(parents=True, exist_ok=True)
    target = destination / report.name
    target.write_text(page, encoding="utf-8", newline="\n")
    evidence["report_sha256"] = digest(target)
    write_json(destination / "cathodic-protection-analysis.citations.json", evidence)


def build(destination: Path) -> None:
    for case_id in CASES:
        rebuild_case(REPORTS / "cases" / case_id, destination / "cases" / case_id, case_id)
    refresh_reference(REPORTS / "reference-jacket", destination / "reference-jacket")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--destination", required=True, type=Path)
    build(parser.parse_args().destination)
