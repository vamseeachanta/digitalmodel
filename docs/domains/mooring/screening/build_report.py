# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Build a static-line analysis and a separate, unpublished website source."""
from __future__ import annotations

import argparse
import html
import json
import re
from html.parser import HTMLParser
from pathlib import Path

from run_screening import RECORD, digest, validate_evidence, write_json

ESC = html.escape


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render already escaped cells in a portable table."""
    heading = "".join(f"<th>{ESC(h)}</th>" for h in headers)
    body = "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
    return f'<table class="data-table"><thead><tr>{heading}</tr></thead><tbody>{body}</tbody></table>'


def number(value: float, locator: str, integer: bool = False) -> str:
    """Bind each displayed result to its record locator."""
    rendered = str(int(value)) if integer else f"{value:.1f}"
    return f'<span data-record="{ESC(locator)}" data-format="{"integer" if integer else "decimal"}">{rendered}</span>'


def lookup(result: dict, locator: str):
    """Resolve a dotted record locator, including array positions."""
    value = result
    for part in locator.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


class BoundValues(HTMLParser):
    """Read every numeric record binding from the generated page."""
    def __init__(self, result: dict):
        super().__init__()
        self.result = result
        self.expected = None
        self.count = 0

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if "data-record" in attributes:
            value = lookup(self.result, attributes["data-record"])
            self.expected = str(int(value)) if attributes["data-format"] == "integer" else f"{value:.1f}"
            self.count += 1
        if tag in {"script", "img", "iframe", "include", "link", "source", "video", "audio"}:
            if "src" in attributes or "href" in attributes:
                raise ValueError("Page resources must be self-contained")

    def handle_data(self, data):
        if self.expected is not None:
            if data != self.expected:
                raise ValueError("Page result differs from the run record")
            self.expected = None



class UnboundNumbers(HTMLParser):
    """Inventory numerical prose outside result bindings and inline styles."""
    def __init__(self):
        super().__init__()
        self.hidden = None
        self.bound = False
        self.texts = set()

    def handle_starttag(self, tag, attrs):
        if tag in {"style", "script"}:
            self.hidden = tag
        if "data-record" in dict(attrs):
            self.bound = True

    def handle_endtag(self, tag):
        if tag == self.hidden:
            self.hidden = None
        if tag == "span":
            self.bound = False

    def handle_data(self, data):
        if not self.hidden and not self.bound and re.search(r"\d", data):
            self.texts.add(data.strip())


def unbound_numbers(page: str) -> set[str]:
    """Return complete numerical text nodes rather than loose allowed tokens."""
    parser = UnboundNumbers()
    parser.feed(page)
    return parser.texts


def revision_section(revisions: dict, declared: str) -> str:
    """Build revision history from retained data, immediately after the summary."""
    rows = revisions["history"]
    if not rows or rows[-1]["revision"] != declared:
        raise ValueError("Newest revision does not match the declared report revision")
    return '<section class="section" id="revision-history"><h2>Revision history</h2>' + table(
        ["Revision", "Date", "What changed and why"],
        [[ESC(r["revision"]), ESC(r["date"]), ESC(r["change"])] for r in rows],
    ) + "</section>"


def summary_section(result: dict) -> str:
    """Distinguish independent solves from matrix labels and qualified cases."""
    counts = result["counts"]
    summary = '<section class="section" id="summary"><h2>Summary</h2><p>'
    summary += number(counts["unique_static_solves"], "counts.unique_static_solves", True)
    summary += ' homogeneous static line solutions are computed. '
    summary += number(counts["entries_with_static_line"], "counts.entries_with_static_line", True)
    summary += ' catenary matrix entries reuse these lines; '
    summary += number(counts["entries_not_reproduced"], "counts.entries_not_reproduced", True)
    summary += ' taut or hybrid entries are not reproduced. Qualified environmental cases: '
    summary += number(counts["qualified_environmental_cases"], "counts.qualified_environmental_cases", True)
    summary += '. Platform classes and sea-state labels do not add independent solves.</p></section>'
    return summary


def solutions_section(result: dict, basis: dict) -> str:
    """Display native geometry with record-bound unit conversions."""
    rows = []
    for index, solution in enumerate(result["solutions"]):
        prefix = f"solutions.{index}."
        rows.append([
            number(solution["water_depth_m"], prefix + "water_depth_m", True),
            number(solution["native"]["fairlead_tension_n"] / basis["assumptions"]["n_per_kn"],
                   prefix + "fairlead_tension_kn"),
            number(solution["native"]["horizontal_tension_n"] / basis["assumptions"]["n_per_kn"],
                   prefix + "horizontal_tension_kn"),
            number(solution["native"]["suspended_length_m"], prefix + "native.suspended_length_m"),
            number(solution["grounded_length_m"], prefix + "grounded_length_m"),
            number(solution["anchor_radius_m"], prefix + "anchor_radius_m"),
            number(solution["provided_static_mbl_to_tension"], prefix + "provided_static_mbl_to_tension"),
            "Not established",
        ])
    values = '<section class="section" id="computed-values"><h2>Computed static lines</h2>'
    values += '<p>The fairlead resultant is recomputed from the native horizontal and vertical components. '
    values += 'It is constrained by an assumed prescribed pretension, not an environmental peak. '
    values += 'The anchor radius is measured from a fixed fairlead and is not vessel offset.</p>'
    values += table(["Depth (m)", "Static resultant (kN)", "Horizontal tension (kN)",
                     "Suspended length (m)", "Grounded length (m)", "Anchor radius (m)",
                     "Empirical MBL / static tension (1)", "Required standard factor (1)"], rows)
    values += '<p>Table 1. Homogeneous inextensible catenary lines with a horizontal, frictionless seabed. '
    values += 'Empirical MBL ratios are provided without a standards acceptance verdict.</p></section>'
    return values


def assumptions_section(basis: dict) -> str:
    """State missing environmental and component evidence."""
    assumptions = '<section class="section"><h2>Input basis and assumptions</h2><ul>'
    assumptions += ''.join(f'<li>{ESC(text)}</li>' for text in basis["assumption_notes"])
    assumptions += '</ul><p>These assumptions are not stated on the source page. The generic wire property '
    assumptions += 'examples are held public-source data; no certified chain make-up is inferred. '
    assumptions += 'Operating and survival labels are retained in the record; numerical environmental inputs '
    assumptions += 'are not stated on the page and are not assigned.</p></section>'
    return assumptions


def removals_section(result: dict) -> str:
    """List unsupported quantities without retaining their illustrative values."""
    limits = '<section class="section" id="not-reproduced"><h2>Not reproduced</h2>'
    limits += table(["Removed quantity", "Reason"], [[ESC(r["quantity"]), ESC(r["reason"])]
                                                   for r in result["not_reproduced"]])
    limits += '<p>Table 2. Unsupported illustrative values are removed; no replacement is estimated.</p></section>'
    return limits


def criteria_section(basis: dict) -> str:
    """Explain mathematical criteria and the missing standards acceptance basis."""
    standards = '<section class="section"><h2>Criteria and way forward</h2>'
    standards += '<p>Each computed line has positive horizontal tension, suspended length no greater than '
    standards += 'the assumed total line length, and a recomputed resultant matching the prescribed force '
    standards += 'within the recorded numerical tolerance. These are mathematical checks, not code acceptance.</p>'
    standards += '<p>DNV-OS-E301, Position Mooring, edition July 2021: the repository registry identifies '
    standards += 'Section 2.2.3, intact quasi-static mooring; the governing clause and required factor are not independently '
    standards += 'qualified here. API RP 2SK, Design and Analysis of Stationkeeping Systems for Floating Structures: '
    standards += 'the design module identifies Section 5; the governing edition, clause title and required factor '
    standards += 'are not established. No licensed table values are reproduced or applied.</p>'
    standards += '<p>Environmental demand, actual component certificates, endpoint geometry and the governing '
    standards += 'code criteria are required before tension, offset, fatigue or station-keeping acceptance can be assessed.</p>'
    standards += f'<p>Run record: <a href="{ESC(basis["run_record_url"])}">inputs, native outputs and reproducible commands</a>.</p></section>'
    return standards


def sections(result: dict, basis: dict, revisions: dict) -> str:
    """Place retained revision history directly after the computed summary."""
    return (summary_section(result) + revision_section(revisions, result["revision"])
            + solutions_section(result, basis) + assumptions_section(basis)
            + removals_section(result) + criteria_section(basis))

def display_record(result: dict, basis: dict) -> dict:
    """Add display-unit conversions without changing the native result payload."""
    copy = json.loads(json.dumps(result))
    for solution in copy["solutions"]:
        solution["fairlead_tension_kn"] = solution["native"]["fairlead_tension_n"] / basis["assumptions"]["n_per_kn"]
        solution["horizontal_tension_kn"] = solution["native"]["horizontal_tension_n"] / basis["assumptions"]["n_per_kn"]
    return copy


def validate_page(page: str, result: dict, basis: dict, revisions: dict) -> int:
    """Require the complete expected results block and validate numeric bindings."""
    expected = sections(result, basis, revisions)
    if expected not in page:
        raise ValueError("Page differs from the computed results and removal list")
    allowed = unbound_numbers(expected) | {
        f"Revision {result['revision']}",
        f"Revision {result['revision']}; computed statics with assumed inputs",
    }
    if unbound_numbers(page) - allowed:
        raise ValueError("Unbound numerical claim outside the run record")
    parser = BoundValues(display_record(result, basis))
    parser.feed(page)
    if parser.count != len(result["solutions"]) * 7 + 4:
        raise ValueError("Unexpected result binding count")
    if parser.expected is not None:
        raise ValueError("Empty result binding")
    return parser.count


def report_page(content: str, revision: str) -> str:
    """Render the run record as a single file with inline styling."""
    return '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">' + \
        '<meta name="viewport" content="width=device-width,initial-scale=1">' + \
        '<title>Mooring static-line analysis</title><style>body{font:16px system-ui;' + \
        'margin:2rem;max-width:90rem}table{border-collapse:collapse;width:100%}' + \
        'td,th{border:1px solid #aaa;padding:.5rem;text-align:left}section{margin:2rem 0}' + \
        'th{background:#edf2f7}a{overflow-wrap:anywhere}</style></head><body>' + \
        f'<h1>Mooring static-line analysis</h1><p>Revision {ESC(revision)}</p>' + \
        content + '</body></html>\n'


def website_page(source: str, content: str, revision: str) -> str:
    """Retain source styling while replacing all original claims and result tables."""
    head = source.split("<body>", 1)[0]
    head = re.sub(r'<include\b[^>]*>\s*</include>', '', head)
    head = re.sub(r'<!-- Inter .*? -->', '', head)
    head = re.sub(r'<script\b.*?</script>', '', head, flags=re.DOTALL)
    head = re.sub(r'<meta\s+(?:name="description"|property="og:[^"]+")[^>]*>', '', head)
    head = re.sub(r'<title>.*?</title>', '<title>Mooring static-line analysis</title>', head)
    page = head + '<body><main id="main"><div class="report-header">' + \
        '<h1>Mooring static-line analysis</h1>' + \
        f'<p>Revision {ESC(revision)}; computed statics with assumed inputs</p></div>' + \
        '<div class="report-body">' + content + '</div></main></body></html>\n'
    return '\n'.join(line.rstrip(' \t') for line in page.splitlines()) + '\n'


def build(output_dir: Path, source: Path | None = None, website_out: Path | None = None) -> dict:
    """Build deterministic report, README and manifests from recorded output bytes."""
    basis = json.loads((RECORD / "basis.json").read_text(encoding="utf-8"))
    result = json.loads((output_dir / "results.json").read_text(encoding="utf-8"))
    revisions = json.loads((RECORD / "revisions.json").read_text(encoding="utf-8"))
    receipt = json.loads((RECORD / "execution.json").read_text(encoding="utf-8"))
    validate_evidence(RECORD / "criteria.json", receipt)
    content = sections(result, basis, revisions)
    page = report_page(content, result["revision"])
    bindings = validate_page(page, result, basis, revisions)
    (output_dir / "report.html").write_text(page, encoding="utf-8")
    readme = (RECORD / "README.template.md").read_text(encoding="utf-8")
    history = "## Revision history\n\n| Revision | Date | What changed and why |\n| --- | --- | --- |\n"
    history += "".join(f"| {r['revision']} | {r['date']} | {r['change']} |\n" for r in revisions["history"])
    readme = readme.replace("{revision_history}", history.rstrip()).replace("{revision}", result["revision"])
    (output_dir / "README.md").write_text(readme, encoding="utf-8")
    comparison = (RECORD / "comparison.template.md").read_text(encoding="utf-8")
    (output_dir / "comparison.md").write_text(comparison.replace("{revision_history}", history.rstrip()), encoding="utf-8")
    files = {name: {"sha256": digest(output_dir / name), "bytes": (output_dir / name).stat().st_size}
             for name in ["results.json", "report.html", "README.md", "comparison.md"]}
    manifest = {"revision": result["revision"], "criteria_revision": result["criteria_revision"],
                "module_commit": basis["module_commit"], "python_version": result["python_version"],
                "source_files": basis["source_files"], "execution": receipt,
                "counts": result["counts"], "numeric_bindings": bindings, "files": files,
                "record_inputs": {name: digest(RECORD / name) for name in [
                    "basis.json", "criteria.json", "revisions.json", "execution.json",
                    "run_screening.py", "build_report.py", "README.template.md", "comparison.template.md"]},
                "header_replacements": "none; native JSON contains no runtime paths or host headers"}
    write_json(output_dir / "manifest.json", manifest)
    if (source is None) != (website_out is None):
        raise ValueError("Website source and destination must be provided together")
    if source is not None:
        if digest(source) != basis["website_source_sha256"]:
            raise ValueError("Website source digest mismatch")
        prepared = website_page(source.read_text(encoding="utf-8"), content, result["revision"])
        validate_page(prepared, result, basis, revisions)
        destination = website_out / "content/demos"
        destination.mkdir(parents=True, exist_ok=True)
        (destination / "mooring.html").write_text(prepared, encoding="utf-8")
        (website_out / "SOURCE.txt").write_text(basis["website_commit"] + "\n", encoding="utf-8")
        (website_out / "CHANGES.md").write_text((output_dir / "comparison.md").read_text(encoding="utf-8"), encoding="utf-8")
        write_json(website_out / "manifest.json", {"source_commit": basis["website_commit"],
                   "source_sha256": basis["website_source_sha256"], "numeric_bindings": bindings,
                   "files": {str(p.relative_to(website_out)): {"sha256": digest(p), "bytes": p.stat().st_size}
                             for p in [destination / "mooring.html", website_out / "SOURCE.txt", website_out / "CHANGES.md"]}})
    return manifest


def main() -> None:
    """Build only into explicitly requested directories."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--website-source", type=Path)
    parser.add_argument("--website-out", type=Path)
    args = parser.parse_args()
    manifest = build(args.output_dir, args.website_source, args.website_out)
    print(f"report.html sha256={manifest['files']['report.html']['sha256']}")


if __name__ == "__main__":
    main()
