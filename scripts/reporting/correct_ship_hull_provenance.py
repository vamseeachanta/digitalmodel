# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Correct a historical hull report without recalculating unqualified inputs."""

from __future__ import annotations

import argparse
import hashlib
import html
import json
from pathlib import Path
import re
import shutil
from typing import Any

import yaml

from build_cp_review_portfolio import (
    DESTINATION, FIXTURES, ROOT, check_existing_pack, digest, verify_pack,
    write_index, write_json, write_receipt,
)
from cp_portfolio_storage import tree_digest
from cp_review_document import comment_seed, validate_comments

TITLE = "Hull cathodic protection analysis"
REVISION = "01"
DATE = "2026-10-14"
CAPACITY_LOCATOR = (
    "ABS Guidance Notes on Cathodic Protection of Ships, December 2017, "
    "Section 3, Table 4, Typical Properties of Aluminum Anodes in Normal "
    "Operation Conditions"
)
DESCRIPTION = "Internal draft: clarify hull provenance and remove ignored capacity input"


def _replace_once(page: str, old: str, new: str) -> str:
    """Refuse an unexpected report layout instead of silently missing a correction."""
    if page.count(old) != 1:
        raise ValueError("historical report layout differs from the correction basis")
    return page.replace(old, new, 1)


def _provenance_section(provenance: dict[str, Any]) -> str:
    """Render the recorded provenance without reproducing source constants."""
    paragraphs = "".join(
        f"<p>{html.escape(str(provenance[key]))}</p>"
        for key in ("hull_area_basis", "capacity_basis", "limitations", "way_forward")
    )
    return (
        '<section id="source-provenance"><h2>Provenance note</h2>'
        f"{paragraphs}</section>\n"
    )


def _correct_page(page: str, provenance: dict[str, Any]) -> str:
    """Reissue the presentation while retaining numerical tables and prior history."""
    page = page.replace("CP module review: Ship hull", TITLE)
    page = _replace_once(page, "<td>Revision</td><td>00</td>", f"<td>Revision</td><td>{REVISION}</td>")
    page = _replace_once(page, "<td>Date</td><td>2026-10-03</td>", f"<td>Date</td><td>{DATE}</td>")
    page = page.replace("Rev 00", f"Rev {REVISION}")
    page, count = re.subn(
        r"<tr><td>inputs\.anode\.anode_current_capacity</td><td>[^<]+</td></tr>",
        "<tr><td>inputs.anode.alloy</td><td>A1</td></tr>", page,
    )
    if count != 1:
        raise ValueError("historical capacity input echo is missing or duplicated")
    history = re.search(r'    <div class="kv revhist">.*?</table>\s*</div>', page, re.S)
    if history is None:
        raise ValueError("historical revision history is missing")
    revised = _append_revision(history.group()).replace(
        '<div class="cap">Revision history</div>', "<h2>Revision history</h2>",
    )
    revised = '<section id="revision-history">' + revised + "</section>\n"
    page = page[:history.start()] + page[history.end():]
    summary = re.search(r'<section id="review-1">.*?</section>', page, re.S)
    if summary is None:
        raise ValueError("historical summary is missing")
    warning = (
        "<p>Source qualification is pending. Historical calculation results are "
        "retained without a numerical rebuild; calculation PASS does not establish "
        "design applicability or release rights. Client-derived inputs require "
        "a maintainer decision before further use.</p>\n"
    )
    revised_summary = summary.group().replace('<div class="prose">', '<div class="prose">' + warning)
    page = page[:summary.start()] + revised_summary + "\n" + revised + _provenance_section(provenance) + page[summary.end():]
    page = page.replace(
        "Use status: approved for client use subject to an engineer-of-record check of this deliverable.",
        "Use status: historical assessment retained; source qualification pending.",
    )
    page = _replace_once(
        page, '<li><a href="#review-2">Introduction</a></li>',
        '<li><a href="#revision-history">Revision history</a></li>\n'
        '<li><a href="#source-provenance">Provenance note</a></li>\n'
        '<li><a href="#review-2">Introduction</a></li>',
    )
    return _bind_review(_capacity_locator(page))


def _append_revision(history: str) -> str:
    """Append the correction row while preserving the original revision row."""
    if history.count("<td>00</td>") != 1 or "<td>01</td>" in history:
        raise ValueError("unexpected historical revision rows")
    return _replace_once(
        history, "</tbody>",
        f"<tr><td>{REVISION}</td><td>{DATE}</td><td>{DESCRIPTION}</td>"
        "<td></td><td>Pending</td><td>Pending</td></tr>\n</tbody>",
    )


def _capacity_locator(page: str) -> str:
    """Replace the licensed design-data capacity cell with its source locator."""
    page, count = re.subn(
        r"<tr><td>Anode current capacity</td><td>[^<]+</td><td>[^<]+</td></tr>",
        f"<tr><td>Anode current capacity</td><td>{CAPACITY_LOCATOR}</td><td>-</td></tr>",
        page,
    )
    if count != 1:
        raise ValueError("historical design-data capacity row is missing or duplicated")
    return page


def _repair_reissued_page(page: str) -> str:
    """Repair the earlier truncated history without adding another revision."""
    section = re.search(r'<section id="revision-history">.*?</section>', page, re.S)
    if section is None:
        raise ValueError("reissued revision history is missing")
    if "</table>" not in section.group():
        table = re.search(
            r"<table>\s*<thead><tr><th>Rev</th>.*?</table>\s*</div>",
            page, re.S,
        )
        if table is None or table.start() > section.start():
            raise ValueError("displaced historical revision table is missing")
        history_block = _append_revision(table.group())
        replacement = (
            '<section id="revision-history"><div class="kv revhist">'
            '<h2>Revision history</h2>' + history_block + "</section>"
        )
        page = page[:section.start()] + replacement + page[section.end():]
        page = page[:table.start()] + page[table.end():]
    history = re.search(r'<section id="revision-history">(.*?)</section>', page, re.S)
    if history is None or any(history.group(1).count(f"<td>{rev}</td>") != 1
                              for rev in ("00", "01")):
        raise ValueError("reissued revision history must contain both revisions")
    return _bind_review(_capacity_locator(page))


def _bind_review(page: str) -> str:
    """Rebind the existing review controls to the exact corrected report content."""
    page = "".join(line.rstrip() + "\n" for line in page.splitlines())
    pattern = r'<script id="cp-review-config" type="application/json">(.*?)</script>'
    match = re.search(pattern, page, re.S)
    if match is None:
        raise ValueError("historical review configuration is missing")
    config = json.loads(match.group(1))
    config["revision"] = REVISION
    additions = [
        {"id": "revision-history", "title": "Revision history"},
        {"id": "source-provenance", "title": "Provenance note"},
    ]
    existing = {section["id"] for section in config["sections"]}
    config["sections"][1:1] = [row for row in additions if row["id"] not in existing]
    core = re.sub(r"<!--CP_REVIEW_START-->.*?<!--CP_REVIEW_END-->", "", page, flags=re.S)
    config["content_sha256"] = hashlib.sha256(core.encode()).hexdigest()
    encoded = json.dumps(config, ensure_ascii=True).replace("<", "\\u003c")
    return page[:match.start(1)] + encoded + page[match.end(1):]


def _correct_input(folder: Path) -> None:
    """Correct the reproduction input and append its document history."""
    cfg = yaml.safe_load((folder / "input.yml").read_text("utf-8"))
    fixture = yaml.safe_load((FIXTURES / "ships.yml").read_text("utf-8"))
    cfg["inputs"]["anode"] = fixture["inputs"]["anode"]
    document = cfg["report"]["document"]
    document.update(title=TITLE, revision=REVISION, date=DATE)
    document["revision_history"].append({
        "rev": REVISION, "date": DATE, "description": DESCRIPTION,
        "checked": "Pending", "approved": "Pending",
    })
    (folder / "input.yml").write_text(yaml.safe_dump(cfg, sort_keys=False), "utf-8")


def correct_ship_provenance(destination: Path) -> None:
    """Correct only the hull pack and index; never run an unqualified calculation."""
    folder = destination / "cases/S04"
    check_existing_pack(folder)
    entry = json.loads((ROOT / "scripts/reporting/cp_portfolio_inputs.json").read_text())["ships.yml"]
    if entry["source_class"] != "client_derived_pending_release":
        raise ValueError("hull provenance correction requires pending source qualification")
    if digest(FIXTURES / "ships.yml") != entry["sha256"]:
        raise ValueError("hull provenance fixture digest mismatch")
    release = json.loads((folder / "release.json").read_text())
    original = (folder / "report.html").read_text("utf-8")
    if release.get("provenance_correction") == entry["provenance"]:
        page = _repair_reissued_page(original)
        if page == original:
            return
    else:
        page = _correct_page(original, entry["provenance"])
        _correct_input(folder)
    (folder / "report.html").write_text(page, "utf-8")
    write_json(folder / "report.comments.json", comment_seed(page, "S04", REVISION))
    release.update(
        source_class=entry["source_class"], private_benchmark_data="not_established",
        input_sha256=entry["sha256"], provenance_correction=entry["provenance"],
        numerical_rebuild="blocked_pending_source_qualification",
    )
    write_json(folder / "release.json", release)
    manifest = json.loads((folder / "report_manifest.json").read_text())
    manifest["document"].update(title=TITLE, revision=REVISION)
    manifest["provenance_correction"] = entry["provenance"]
    write_json(folder / "report_manifest.json", manifest)
    row = json.loads((folder / "verification.json").read_text())
    row.update(source_class=entry["source_class"], pack_state="blocked",
               use_status="source qualification and release pending",
               numerical_results="historical_unchanged")
    write_json(folder / "verification.json", row)
    register = json.loads((destination / "document-register.json").read_text())
    register["S04"] = yaml.safe_load((folder / "input.yml").read_text())["report"]["document"]
    write_json(destination / "document-register.json", register)
    coverage = json.loads((destination / "coverage.json").read_text())
    rows = [row if item["id"] == "S04" else item for item in coverage["rows"]]
    write_index(destination, rows)
    index = destination / "index.html"
    index.write_text(index.read_text().rstrip("\n") + "\n", "utf-8")
    write_receipt(folder)
    verify_pack(folder)
    validate_comments(page, json.loads((folder / "report.comments.json").read_text()))
    write_json(destination / "portfolio-receipt.json", tree_digest(destination))


def main() -> None:
    """Build a correction candidate outside the existing portfolio."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    output = args.output.resolve()
    if output.exists() or output.is_relative_to(ROOT.resolve()):
        parser.error("correction output must be a new directory outside the repository")
    shutil.copytree(DESTINATION, output)
    correct_ship_provenance(output)


if __name__ == "__main__":
    main()
