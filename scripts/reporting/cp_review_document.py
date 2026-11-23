"""Portfolio-only review presentation; never changes shared templates or results."""
from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path
from typing import Any

from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.spec import ReportSpec, Section, StatusBlock, TextBlock

STANDARD_REVISION = "c955b86159acc4f9e9f5964690c24dd1411e5b48"
SECTION_TITLES = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
DECISIONS = {"pending", "accept", "revise", "reject"}
_CONFIG = re.compile(r'<script id="cp-review-config" type="application/json">(.*?)</script>', re.S)


def _draft_control(spec: ReportSpec) -> None:
    document = spec.document
    histories = document.revision_history
    if not histories or any("draft" not in row.description.lower() for row in histories):
        raise ValueError("Every revision must explicitly identify an internal draft")
    signoffs = [document.checked_by, document.approved_by]
    signoffs.extend(value for row in histories for value in (row.checked, row.approved))
    if any(value.strip().lower() not in {"", "pending", "not assigned"} for value in signoffs):
        raise ValueError("Review portfolio must not carry completed human signoffs")


def _sections(spec: ReportSpec) -> list[Section]:
    source_links = "\n".join(f"- [{s.title}](#{s.key})" for s in spec.sections)
    findings = _findings(spec)
    texts = (
        "Internal review draft. No engineering acceptance is implied. Calculation "
        "PASS/FAIL and route use restrictions remain in the original adequacy records.\n\n"
        + findings + "\n\nIndependent engineering review is required before design use.",
        "This module regression report supports engineering review. It is not a client "
        "issue or an installation instruction. The proposed reporting standard is applied "
        "to this portfolio only; global adoption remains pending.",
        "The complete input echo and original design-basis tables below preserve the "
        "calculation inputs. Reproduction files are supplied alongside this report.",
        "The existing CP route calculates the results without portfolio-layer numerical "
        "changes. Original method notes and citations remain in the calculation appendices.",
        "The original numerical tables, status records and figures are retained in the "
        "following keyed calculation appendices:\n\n" + source_links,
        _findings(spec, numbered=True) + "\n\nA failed assessed arrangement remains FAIL; a recommended count "
        "is not independently accepted. Comparator class: source analytical criterion. "
        "Independent validation and numerical uncertainty bounds are not supplied; "
        "plausibility verdict: not evaluated. Review focus: qualify the physical inputs, "
        "criterion and anode arrangement. Preparer recommendation: resolve failures "
        "and obtain EOR review; reviewer decision remains pending.",
        "Comprehensive desktop/mobile, print and comment-control review is deferred by "
        "the owner. Independent engineering review, input qualification and EOR acceptance "
        "remain pending. Legacy or unevaluated limitations remain in the source records.",
        "Resolve the recorded adequacy failures and evidence gaps, qualify the input "
        "basis, and obtain independent engineering review before design use. Record "
        "comments and unresolved decisions in the matching JSON sidecar.",
        "Standards citations, edition labels and source provenance below remain the "
        "calculation basis. Presentation standard: proposed engineering reporting "
        f"conventions, revision `{STANDARD_REVISION}` (review draft).",
        "CP: cathodic protection. EOR: engineer of record. PASS/FAIL: satisfaction of "
        "the specified calculation checks, not engineering acceptance. A: ampere; "
        "kg: kilogram; m: metre; yr: year. Table units remain explicit in source records.",
    )
    return [Section(key=f"review-{i}", title=title, blocks=[TextBlock(markdown=text)])
            for i, (title, text) in enumerate(zip(SECTION_TITLES, texts), 1)]


def _findings(spec: ReportSpec, *, numbered: bool = False) -> str:
    findings: list[str] = []
    for section in spec.sections:
        for block in section.blocks:
            if isinstance(block, StatusBlock):
                prefix = f"## 6.{len(findings) + 1} {block.label}\n\n" if numbered else "- "
                findings.append(f"{prefix}{block.label}: {block.status}. {block.detail} "
                                f"Governing case: {block.governing_case or 'not specified'}. "
                                f"[Calculation evidence](#{section.key}).")
    return "\n\n".join(findings) or "No quantitative adequacy verdict is supplied by this adapter."


def _result_cards(spec: ReportSpec) -> list[Section]:
    cards = []
    for section in spec.sections:
        for index, block in enumerate(section.blocks):
            if not isinstance(block, StatusBlock):
                continue
            text = (f"Observed source verdict: {block.status}. Evidence: {block.detail}\n\n"
                    f"Governing case: {block.governing_case or 'not specified'}. "
                    f"[Source record](#{section.key}). Comparator class: source analytical "
                    "criterion; criterion values remain in the original check table. "
                    "No additional independent tolerance was supplied or selected.\n\n"
                    "Physical expectation: the qualified physical arrangement should "
                    "satisfy the declared check. Plausibility verdict: not evaluated; "
                    "independent comparator evidence is not supplied. Review focus: "
                    "check criterion applicability, assumptions and governing evidence. "
                    "Preparer recommendation: retain reported failures and use restrictions "
                    "until independently resolved. Reviewer assignment and decision: pending.")
            cards.append(Section(key=f"result-{section.key}-{index}",
                                 title=f"Result review: {block.label}",
                                 blocks=[TextBlock(markdown=text)]))
    return cards


def _review_controls() -> str:
    return '''<section id="cp-review-ui" aria-label="Report comments">
<h2>Review comments</h2><p>Select this HTML and load its matching JSON sidecar.
Re-select the HTML before each Save or Copy. Files remain on your computer.</p>
<label>Current HTML <input id="cp-html" type="file" accept=".html"></label>
<label>Load comments <input id="cp-load" type="file" accept=".json"></label>
<label>Reviewer <input id="cp-reviewer" maxlength="120"></label>
<label>Section <select id="cp-section"></select></label>
<button id="cp-quote" type="button">Capture selected report text</button>
<label>Quoted text <textarea id="cp-quoted" maxlength="10000" readonly></textarea></label>
<label>Decision <select id="cp-decision"><option>pending</option><option>accept</option>
<option>revise</option><option>reject</option></select></label>
<label>Comment <textarea id="cp-comment" maxlength="10000"></textarea></label>
<label>Disposition <select id="cp-disposition"><option>requiring a decision</option>
<option>incorporated</option><option>deferred</option></select></label>
<label>Disposition reason <input id="cp-reason" maxlength="2000"></label>
<button id="cp-add" type="button">Record comment and decision</button>
<button id="cp-save" type="button">Save JSON</button>
<button id="cp-copy" type="button">Copy JSON</button>
<p>Save As is used where supported; otherwise Save uses your browser's Downloads
destination. Re-load the saved JSON to verify the durable copy.</p>
<p id="cp-message" role="status">Not bound to a report file.</p>
<pre id="cp-preview"></pre></section>'''


def render_review(spec: ReportSpec, case_id: str) -> str:
    """Render ten review sections and preserve original evidence as appendices."""
    if not re.fullmatch(r"[A-Z][A-Z0-9_-]{0,39}", case_id):
        raise ValueError("case_id must be a bounded public identifier")
    _draft_control(spec)
    revised = spec.model_copy(deep=True)
    revised.sections = _sections(spec)
    revised.appendices = [*spec.sections, *spec.appendices, *_result_cards(spec)]
    reserved = {s.key for s in revised.sections}
    if any(s.key in reserved for s in revised.appendices):
        raise ValueError("Source section collides with review section IDs")
    rendered = render_html(revised, pdf_status="not generated; manual print review deferred")
    config = {"report_id": case_id, "revision": spec.document.revision,
              "sections": [{"id": s.key, "title": s.title}
                           for s in [*revised.sections, *revised.appendices]]}
    css = '''<style id="cp-review-layout">body.doc{min-width:0;width:100%}
    .wrap,main,section{min-width:0;max-width:100%;box-sizing:border-box}
    .tbl-wrap{max-width:100%;overflow-x:auto}#cp-review-ui{padding:24px;overflow-wrap:anywhere}
    #cp-review-ui label{display:block;margin:10px 0}#cp-review-ui pre{white-space:pre-wrap}
    #cp-review-ui input,#cp-review-ui select,#cp-review-ui textarea{max-width:100%}
    @media print{#cp-review-ui{display:none}.tbl-wrap{overflow:visible}}
    </style>'''
    core = rendered.replace("</head>", css + "</head>")
    config["content_sha256"] = hashlib.sha256(core.encode("utf-8")).hexdigest()
    encoded = json.dumps(config, ensure_ascii=True).replace("<", "\\u003c")
    script = Path(__file__).with_name("cp_review_ui.js").read_text(encoding="utf-8")
    controls = ('<!--CP_REVIEW_START-->' + _review_controls() +
                '<script id="cp-review-config" type="application/json">' + encoded +
                f'</script><script>{script}</script><!--CP_REVIEW_END-->')
    return core.replace("</body>", controls + "</body>")


def _config(html: str) -> dict[str, Any]:
    match = _CONFIG.search(html)
    if match is None:
        raise ValueError("Missing review configuration")
    return dict(json.loads(match.group(1)))


def comment_seed(html: str, case_id: str, revision: str) -> dict[str, Any]:
    """Bind a fresh pending review to the exact emitted UTF-8 HTML bytes."""
    config = _config(html)
    if (case_id, revision) != (config["report_id"], config["revision"]):
        raise ValueError("Report identity or revision mismatch")
    return {"schema_version": 1, "report_id": case_id, "revision": revision,
            "report_sha256": hashlib.sha256(html.encode("utf-8")).hexdigest(),
            "standard_revision": STANDARD_REVISION, "reviewer_assignment": "pending",
            "visual_review": "deferred by the owner", "comments": [], "prior_rounds": [],
            "decision_conflicts": [],
            "results": [{"id": section["id"], "decision": "pending"}
                        for section in config["sections"]]}


def validate_comments(html: str, sidecar: dict[str, Any]) -> None:
    """Reject stale/misbound sidecars and malformed review records."""
    config = _config(html)
    for key in ("report_id", "revision"):
        if sidecar.get(key) != config[key]:
            raise ValueError(f"Review {key} mismatch")
    if sidecar.get("report_sha256") != hashlib.sha256(html.encode("utf-8")).hexdigest():
        raise ValueError("Review HTML digest mismatch")
    if sidecar.get("schema_version") != 1 or sidecar.get("standard_revision") != STANDARD_REVISION:
        raise ValueError("Unknown review schema or standard revision")
    expected = {section["id"] for section in config["sections"]}
    rows = sidecar.get("results")
    if not isinstance(rows, list) or len(rows) != len(expected):
        raise ValueError("Review results do not cover all sections")
    if any(not isinstance(row, dict) or row.get("decision") not in DECISIONS for row in rows):
        raise ValueError("Invalid review decision")
    if {row.get("id") for row in rows} != expected:
        raise ValueError("Missing or duplicate review section")
    for field in ("comments", "prior_rounds", "decision_conflicts"):
        if not isinstance(sidecar.get(field), list):
            raise ValueError(f"Review {field} must be a list")
    for comment in sidecar["comments"]:
        if (not isinstance(comment, dict) or comment.get("section") not in expected
                or not isinstance(comment.get("text"), str)
                or not isinstance(comment.get("reviewer"), str)
                or not isinstance(comment.get("id"), str)
                or not isinstance(comment.get("quote"), str)
                or comment.get("disposition") not in {
                    "incorporated", "deferred", "requiring a decision"}
                or (comment.get("disposition") == "deferred" and not comment.get("reason"))):
            raise ValueError("Invalid review comment")
