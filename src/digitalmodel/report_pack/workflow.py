# ABOUTME: Routed workflow that renders a standard marine engineering report pack
# ABOUTME: (md + self-contained html + optional pdf + citations + provenance manifest).
"""Durable workflow: standard marine engineering report pack.

Renders a complete engineering report pack following the standard marine
engineering report structure: numbered document (``JOB-DOCTYPE-SEQ-REV``),
title/revision block with prepared/checked/approved rows, a fixed section
skeleton (introduction/scope -> references -> design basis -> methodology &
assumptions -> results -> conclusions & limitations), capital-lettered
appendices, a citation sidecar, and a report-layer provenance manifest.

Config schema (YAML basename ``report_pack``)::

    basename: report_pack
    report_pack:
      document:
        number: B0000-RPT-001-00     # JOB-DOCTYPE-SEQ-REV (rev suffix must match revision)
        revision: "00"
        title: Example Structure Spectral Fatigue Screening
        project: B0000               # job/project placeholder
        client: Example Client
        date: "2026-07-12"           # optional; omitted -> not rendered (deterministic)
        prepared_by: Author Placeholder
        checked_by: Checker Placeholder
        approved_by: Approver Placeholder
        revision_history:            # optional; defaults to single row from doc meta
          - {rev: "00", date: "2026-07-12", description: Issued for review,
             prepared: AP, checked: CP, approved: XP}
      sections:                      # ordered; each needs title + content|content_file
        - title: Introduction and Scope
          content: |
            Markdown body ...
        - title: Design Data and Basis
          content_file: relative/or/absolute.md
      results:                       # optional; rendered inside the Results section
        tables:
          - title: Per-sea-state damage
            csv: data/results.csv    # relative to the config file
        figures:
          - title: Damage histogram
            path: data/figure.png    # embedded as data URI in HTML
      appendices:                    # optional; lettered A, B, C ... (or explicit letter)
        - title: References
          content: ...
        - letter: B
          title: Calculation Records
          files: [data/results.csv]  # listed as appendix contents
      citations:                     # optional; emitted to <stem>_citations.json
        - code_id: DNV-RP-C203
          publisher: DNV
          revision: "2019"
          section: S-N curves in air, Table 2-1
          wiki_path: wikis/marine-engineering/wiki/standards/dnv-rp-c203.md
          note: S-N basis for the screening.
      manifest:                      # report-layer provenance manifest inputs
        issue: https://github.com/org/repo/issues/1
        parent_issue: ...            # optional
        project: B0000
        artifact_class: report-layer-output
        privacy_classification: internal
        publishability_decision: internal review only
        input_source_ids: [SRC-1]
        source_artifacts: {}         # optional pointers (licensed material: pointer-only)
        raw_output_path: repo:outputs/example
        final_output_path: repo:reports/example
        compute_environment: not-recorded   # optional; default "not-recorded"
      pdf: auto                      # auto (default) | off | require
      output_dir: results

Outputs (all under ``output_dir``): ``<stem>_report.md``, ``<stem>_report.html``
(self-contained -- no CDN scripts), ``<stem>_report.pdf`` (best-effort, see
below), ``<stem>_citations.json``, ``<stem>_manifest.json`` (file manifest) and
``report-layer-manifest.json`` (provenance manifest, required-field validated).

PDF rendering is optional by design (``pdf: auto``): the workflow tries, in
order, Playwright/Chromium, Microsoft Edge headless (``msedge --headless
--print-to-pdf`` -- the documented Windows fallback), then Chrome/Chromium
headless. If no renderer is available the pack is still emitted and
``pdf_status`` carries a clear message; ``pdf: require`` turns that into an
error, ``pdf: off`` skips the attempt entirely.

Determinism: no timestamps are generated -- dates render only when supplied in
config -- so re-running the same config produces byte-identical md/html/json.
"""

from __future__ import annotations

import base64
import csv
import html as _html
import json
import re
import shutil
import subprocess
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.citations.schema import Citation, CitationValidationError

REPO_ROOT = Path(__file__).resolve().parents[3]

EXECUTION_TOOL = "digitalmodel.report_pack.workflow"

#: JOB-DOCTYPE-SEQ-REV, doctype optional (e.g. B0000-001-00 or B0000-RPT-001-00).
DOC_NUMBER_RE = re.compile(r"^[A-Z]\d{3,4}(-[A-Z0-9]{1,8})?-\d{3}-\d{2}$")

APPENDIX_LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#: Report-layer manifest fields the caller must supply (provenance contract).
MANIFEST_REQUIRED_FIELDS = (
    "issue",
    "project",
    "artifact_class",
    "privacy_classification",
    "publishability_decision",
    "input_source_ids",
    "raw_output_path",
    "final_output_path",
)
#: Optional caller-supplied manifest fields carried through when present.
MANIFEST_OPTIONAL_FIELDS = ("parent_issue", "source_artifacts")

PDF_MODES = ("auto", "off", "require")

_CSS = """
body { margin: 0; background: #eef2f7; color: #172033;
       font-family: 'Segoe UI', system-ui, sans-serif; line-height: 1.55; }
.report-shell { max-width: 1080px; margin: 0 auto; padding: 34px 16px 50px; }
.report-page { background: #fff; border: 1px solid #d7dee8; border-radius: 10px;
               padding: 36px 44px; }
h1 { margin: 0 0 10px; font-size: 1.7rem; }
h2 { margin: 28px 0 10px; font-size: 1.2rem; border-bottom: 1px solid #d7dee8;
     padding-bottom: 4px; }
h3 { margin: 18px 0 8px; font-size: 1.02rem; }
table { width: 100%; border-collapse: collapse; margin: 12px 0; font-size: .92rem; }
th, td { border: 1px solid #d7dee8; padding: 7px 10px; text-align: left;
         vertical-align: top; }
th { background: #f2f6fb; }
pre { background: #f8fafc; border: 1px solid #d7dee8; border-radius: 6px;
      padding: 12px 14px; overflow-x: auto; }
.titleblock td:first-child { font-weight: 700; width: 220px; background: #f2f6fb; }
figure { margin: 16px 0; }
figure img { max-width: 100%; border: 1px solid #d7dee8; }
figcaption { color: #607085; font-size: .88rem; margin-top: 4px; }
""".strip()


class ReportPackConfigError(ValueError):
    """Raised when the report_pack config is structurally invalid."""


class ReportPackManifestError(ValueError):
    """Raised when the report-layer manifest inputs fail schema validation."""


# ---------------------------------------------------------------------------
# Router
# ---------------------------------------------------------------------------


def router(cfg: dict) -> dict:
    settings = cfg.get("report_pack") or {}
    if not isinstance(settings, dict) or not settings:
        raise ReportPackConfigError("report_pack settings block is required")

    config_dir = _config_dir(cfg)
    document = _validate_document(settings)
    sections = _validate_sections(settings, config_dir)
    results = _load_results(settings, config_dir)
    appendices = _validate_appendices(settings, config_dir)
    citations = _validate_citations(settings)
    manifest_inputs = _validate_manifest_inputs(settings)
    pdf_mode = _pdf_mode(settings)

    stem = _input_stem(cfg)
    out_dir = _output_dir(cfg, settings)
    out_dir.mkdir(parents=True, exist_ok=True)

    md_path = out_dir / f"{stem}_report.md"
    html_path = out_dir / f"{stem}_report.html"
    pdf_path = out_dir / f"{stem}_report.pdf"
    citations_path = out_dir / f"{stem}_citations.json"
    file_manifest_path = out_dir / f"{stem}_manifest.json"
    layer_manifest_path = out_dir / "report-layer-manifest.json"

    markdown = render_markdown(document, sections, results, appendices, citations)
    html_doc = render_html(document, sections, results, appendices, citations)
    md_path.write_text(markdown, encoding="utf-8", newline="\n")
    html_path.write_text(html_doc, encoding="utf-8", newline="\n")

    pdf_written, pdf_status = _render_pdf(html_path, pdf_path, pdf_mode)

    citations_payload = {"citations": [asdict(c) for c in citations]}
    _write_json(citations_path, citations_payload)

    emitted = [md_path, html_path, citations_path]
    if pdf_written:
        emitted.append(pdf_path)

    # Pack-relative names: the pack directory is the portable unit, so its
    # internal manifests must not leak machine-specific absolute paths.
    file_manifest = {
        "markdown_report": md_path.name,
        "html_report": html_path.name,
        "pdf_report": pdf_path.name if pdf_written else None,
        "pdf_status": pdf_status,
        "citation_sidecar": citations_path.name,
        "report_layer_manifest": layer_manifest_path.name,
    }
    _write_json(file_manifest_path, file_manifest)
    emitted.append(file_manifest_path)

    layer_manifest = build_report_layer_manifest(
        manifest_inputs,
        citation_evidence_manifest=citations_path.name,
        generated_manifest=file_manifest_path.name,
        files=sorted(p.name for p in emitted) + ["report-layer-manifest.json"],
    )
    _write_json(layer_manifest_path, layer_manifest)

    cfg["report_pack"] = {
        **settings,
        "document_number": document["number"],
        "markdown_report": _display_path(md_path),
        "html_report": _display_path(html_path),
        "pdf_report": _display_path(pdf_path) if pdf_written else None,
        "pdf_status": pdf_status,
        "citations_json": _display_path(citations_path),
        "file_manifest": _display_path(file_manifest_path),
        "report_layer_manifest": _display_path(layer_manifest_path),
    }
    return cfg


# ---------------------------------------------------------------------------
# Config validation
# ---------------------------------------------------------------------------


def _validate_document(settings: dict[str, Any]) -> dict[str, Any]:
    document = settings.get("document")
    if not isinstance(document, dict):
        raise ReportPackConfigError("report_pack.document block is required")
    for key in ("number", "revision", "title", "project", "client"):
        value = document.get(key)
        if not isinstance(value, str) or not value.strip():
            raise ReportPackConfigError(
                f"report_pack.document.{key} must be a non-empty string"
            )
    number = document["number"].strip()
    if not DOC_NUMBER_RE.match(number):
        raise ReportPackConfigError(
            "report_pack.document.number must match JOB-DOCTYPE-SEQ-REV "
            f"(e.g. B0000-RPT-001-00): got {number!r}"
        )
    revision = str(document["revision"]).strip()
    if not number.endswith(f"-{revision}"):
        raise ReportPackConfigError(
            f"document number {number!r} revision suffix must match "
            f"document.revision {revision!r}"
        )
    history = document.get("revision_history")
    if history is None:
        history = [
            {
                "rev": revision,
                "date": document.get("date", ""),
                "description": "Issued",
                "prepared": document.get("prepared_by", ""),
                "checked": document.get("checked_by", ""),
                "approved": document.get("approved_by", ""),
            }
        ]
    if not isinstance(history, list) or not all(isinstance(r, dict) for r in history):
        raise ReportPackConfigError(
            "report_pack.document.revision_history must be a list of mappings"
        )
    resolved = dict(document)
    resolved["number"] = number
    resolved["revision"] = revision
    resolved["revision_history"] = history
    return resolved


def _validate_sections(
    settings: dict[str, Any], config_dir: Path
) -> list[dict[str, str]]:
    raw = settings.get("sections")
    if not isinstance(raw, list) or not raw:
        raise ReportPackConfigError(
            "report_pack.sections must be a non-empty ordered list"
        )
    sections: list[dict[str, str]] = []
    for index, entry in enumerate(raw, start=1):
        if not isinstance(entry, dict):
            raise ReportPackConfigError(f"report_pack.sections[{index}] must be a mapping")
        title = entry.get("title")
        if not isinstance(title, str) or not title.strip():
            raise ReportPackConfigError(
                f"report_pack.sections[{index}].title must be a non-empty string"
            )
        content = _resolve_content(entry, config_dir, f"report_pack.sections[{index}]")
        sections.append({"title": title.strip(), "content": content})
    return sections


def _validate_appendices(
    settings: dict[str, Any], config_dir: Path
) -> list[dict[str, Any]]:
    raw = settings.get("appendices") or []
    if not isinstance(raw, list):
        raise ReportPackConfigError("report_pack.appendices must be a list")
    appendices: list[dict[str, Any]] = []
    used_letters: set[str] = set()
    auto_index = 0
    for index, entry in enumerate(raw, start=1):
        if not isinstance(entry, dict):
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}] must be a mapping"
            )
        title = entry.get("title")
        if not isinstance(title, str) or not title.strip():
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}].title must be a non-empty string"
            )
        letter = entry.get("letter")
        if letter is None:
            while APPENDIX_LETTERS[auto_index] in used_letters:
                auto_index += 1
            letter = APPENDIX_LETTERS[auto_index]
        letter = str(letter).strip().upper()
        if len(letter) != 1 or letter not in APPENDIX_LETTERS:
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}].letter must be a single letter A-Z"
            )
        if letter in used_letters:
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}] duplicate appendix letter {letter!r}"
            )
        used_letters.add(letter)
        content = ""
        if "content" in entry or "content_file" in entry:
            content = _resolve_content(
                entry, config_dir, f"report_pack.appendices[{index}]"
            )
        files = entry.get("files") or []
        if not isinstance(files, list):
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}].files must be a list"
            )
        if not content and not files:
            raise ReportPackConfigError(
                f"report_pack.appendices[{index}] needs content, content_file or files"
            )
        appendices.append(
            {
                "letter": letter,
                "title": title.strip(),
                "content": content,
                "files": [str(f) for f in files],
            }
        )
    return appendices


def _resolve_content(entry: dict[str, Any], config_dir: Path, label: str) -> str:
    content = entry.get("content")
    content_file = entry.get("content_file")
    if content is not None and content_file is not None:
        raise ReportPackConfigError(f"{label}: give content or content_file, not both")
    if content is not None:
        if not isinstance(content, str) or not content.strip():
            raise ReportPackConfigError(f"{label}.content must be a non-empty string")
        return content.strip()
    if content_file is not None:
        path = Path(content_file)
        if not path.is_absolute():
            path = config_dir / path
        if not path.is_file():
            raise ReportPackConfigError(f"{label}.content_file not found: {path}")
        return path.read_text(encoding="utf-8").strip()
    raise ReportPackConfigError(f"{label} needs content or content_file")


def _load_results(settings: dict[str, Any], config_dir: Path) -> dict[str, Any]:
    raw = settings.get("results") or {}
    if not isinstance(raw, dict):
        raise ReportPackConfigError("report_pack.results must be a mapping")
    tables = []
    for index, entry in enumerate(raw.get("tables") or [], start=1):
        if not isinstance(entry, dict) or not entry.get("csv"):
            raise ReportPackConfigError(
                f"report_pack.results.tables[{index}] needs a csv path"
            )
        title = str(entry.get("title", f"Results table {index}")).strip()
        csv_path = Path(entry["csv"])
        if not csv_path.is_absolute():
            csv_path = config_dir / csv_path
        if not csv_path.is_file():
            raise ReportPackConfigError(
                f"report_pack.results.tables[{index}].csv not found: {csv_path}"
            )
        with csv_path.open(newline="", encoding="utf-8") as stream:
            rows = list(csv.reader(stream))
        if len(rows) < 2:
            raise ReportPackConfigError(
                f"report_pack.results.tables[{index}].csv has no data rows: {csv_path}"
            )
        tables.append({"title": title, "header": rows[0], "rows": rows[1:],
                       "source": csv_path.name})
    figures = []
    for index, entry in enumerate(raw.get("figures") or [], start=1):
        if not isinstance(entry, dict) or not entry.get("path"):
            raise ReportPackConfigError(
                f"report_pack.results.figures[{index}] needs a path"
            )
        fig_path = Path(entry["path"])
        if not fig_path.is_absolute():
            fig_path = config_dir / fig_path
        if not fig_path.is_file():
            raise ReportPackConfigError(
                f"report_pack.results.figures[{index}].path not found: {fig_path}"
            )
        figures.append(
            {
                "title": str(entry.get("title", f"Figure {index}")).strip(),
                "path": fig_path,
            }
        )
    return {"tables": tables, "figures": figures}


def _validate_citations(settings: dict[str, Any]) -> list[Citation]:
    raw = settings.get("citations") or []
    if not isinstance(raw, list):
        raise ReportPackConfigError("report_pack.citations must be a list")
    citations: list[Citation] = []
    for index, entry in enumerate(raw, start=1):
        if not isinstance(entry, dict):
            raise ReportPackConfigError(
                f"report_pack.citations[{index}] must be a mapping"
            )
        try:
            citations.append(Citation(**entry))
        except (TypeError, CitationValidationError) as exc:
            raise ReportPackConfigError(
                f"report_pack.citations[{index}] invalid: {exc}"
            ) from exc
    return citations


def _validate_manifest_inputs(settings: dict[str, Any]) -> dict[str, Any]:
    manifest = settings.get("manifest")
    if not isinstance(manifest, dict):
        raise ReportPackManifestError(
            "report_pack.manifest block is required (report-layer provenance contract)"
        )
    missing = [f for f in MANIFEST_REQUIRED_FIELDS if not manifest.get(f)]
    if missing:
        raise ReportPackManifestError(
            "report_pack.manifest missing required field(s): " + ", ".join(missing)
        )
    input_source_ids = manifest["input_source_ids"]
    if not isinstance(input_source_ids, list) or not all(
        isinstance(i, str) and i.strip() for i in input_source_ids
    ):
        raise ReportPackManifestError(
            "report_pack.manifest.input_source_ids must be a list of non-empty strings"
        )
    source_artifacts = manifest.get("source_artifacts")
    if source_artifacts is not None and not isinstance(source_artifacts, dict):
        raise ReportPackManifestError(
            "report_pack.manifest.source_artifacts must be a mapping"
        )
    return manifest


def _pdf_mode(settings: dict[str, Any]) -> str:
    mode = str(settings.get("pdf", "auto")).strip().lower()
    if mode not in PDF_MODES:
        raise ReportPackConfigError(
            f"report_pack.pdf must be one of {PDF_MODES}: got {mode!r}"
        )
    return mode


def build_report_layer_manifest(
    manifest_inputs: dict[str, Any],
    *,
    citation_evidence_manifest: str,
    generated_manifest: str,
    files: list[str],
) -> dict[str, Any]:
    """Assemble the report-layer provenance manifest (required-field validated).

    Field set follows the report-layer contract: input source IDs, execution
    tool + version, compute environment, raw/final output paths, citation and
    generated manifests, privacy classification and publishability decision.
    """
    try:
        from digitalmodel import __version__ as tool_version
    except Exception:  # pragma: no cover - version metadata is optional
        tool_version = "unknown"
    manifest: dict[str, Any] = {"issue": manifest_inputs["issue"]}
    if manifest_inputs.get("parent_issue"):
        manifest["parent_issue"] = manifest_inputs["parent_issue"]
    manifest.update(
        {
            "project": manifest_inputs["project"],
            "artifact_class": manifest_inputs["artifact_class"],
            "privacy_classification": manifest_inputs["privacy_classification"],
            "publishability_decision": manifest_inputs["publishability_decision"],
            "input_source_ids": list(manifest_inputs["input_source_ids"]),
            "execution_tool": EXECUTION_TOOL,
            "tool_version": tool_version,
            "compute_environment": manifest_inputs.get(
                "compute_environment", "not-recorded"
            ),
            "source_artifacts": manifest_inputs.get("source_artifacts", {}),
            "raw_output_path": manifest_inputs["raw_output_path"],
            "final_output_path": manifest_inputs["final_output_path"],
            "citation_evidence_manifest": citation_evidence_manifest,
            "generated_manifest": generated_manifest,
            "files": sorted(set(files)),
        }
    )
    return manifest


# ---------------------------------------------------------------------------
# Markdown rendering
# ---------------------------------------------------------------------------


def render_markdown(
    document: dict[str, Any],
    sections: list[dict[str, str]],
    results: dict[str, Any],
    appendices: list[dict[str, Any]],
    citations: list[Citation],
) -> str:
    lines: list[str] = []
    lines.append(f"# {document['title']}")
    lines.append("")
    lines.append("## Title and revision block")
    lines.append("")
    lines.append("| Field | Value |")
    lines.append("|---|---|")
    lines.append(f"| Document number | {document['number']} |")
    lines.append(f"| Revision | {document['revision']} |")
    lines.append(f"| Title | {document['title']} |")
    lines.append(f"| Project | {document['project']} |")
    lines.append(f"| Client | {document['client']} |")
    if document.get("date"):
        lines.append(f"| Date | {document['date']} |")
    for role, key in (
        ("Prepared by", "prepared_by"),
        ("Checked by", "checked_by"),
        ("Approved by", "approved_by"),
    ):
        if document.get(key):
            lines.append(f"| {role} | {document[key]} |")
    lines.append("")
    lines.append("### Revision history")
    lines.append("")
    lines.append("| Rev | Date | Description | Prepared | Checked | Approved |")
    lines.append("|---|---|---|---|---|---|")
    for row in document["revision_history"]:
        lines.append(
            "| {rev} | {date} | {description} | {prepared} | {checked} | {approved} |".format(
                rev=row.get("rev", ""),
                date=row.get("date", ""),
                description=row.get("description", ""),
                prepared=row.get("prepared", ""),
                checked=row.get("checked", ""),
                approved=row.get("approved", ""),
            )
        )
    lines.append("")
    for number, section in enumerate(sections, start=1):
        lines.append(f"## {number}. {section['title']}")
        lines.append("")
        lines.append(section["content"])
        lines.append("")
        if _is_results_section(section["title"]):
            lines.extend(_markdown_results(results))
    if citations:
        lines.append("## References cited")
        lines.append("")
        for index, citation in enumerate(citations, start=1):
            note = f" {citation.note}" if citation.note else ""
            lines.append(
                f"{index}. **{citation.code_id}** — {citation.publisher}, "
                f"{citation.revision}, {citation.section}.{note}"
            )
        lines.append("")
    for appendix in appendices:
        lines.append(f"## Appendix {appendix['letter']} — {appendix['title']}")
        lines.append("")
        if appendix["content"]:
            lines.append(appendix["content"])
            lines.append("")
        if appendix["files"]:
            for item in appendix["files"]:
                lines.append(f"- `{item}`")
            lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def _markdown_results(results: dict[str, Any]) -> list[str]:
    lines: list[str] = []
    for table in results["tables"]:
        lines.append(f"### {table['title']}")
        lines.append("")
        lines.append("| " + " | ".join(table["header"]) + " |")
        lines.append("|" + "---|" * len(table["header"]))
        for row in table["rows"]:
            lines.append("| " + " | ".join(row) + " |")
        lines.append("")
        lines.append(f"Source: `{table['source']}`")
        lines.append("")
    for figure in results["figures"]:
        lines.append(f"![{figure['title']}]({figure['path'].name})")
        lines.append("")
    return lines


def _is_results_section(title: str) -> bool:
    return "result" in title.lower()


# ---------------------------------------------------------------------------
# HTML rendering (self-contained; no external scripts or stylesheets)
# ---------------------------------------------------------------------------


def render_html(
    document: dict[str, Any],
    sections: list[dict[str, str]],
    results: dict[str, Any],
    appendices: list[dict[str, Any]],
    citations: list[Citation],
) -> str:
    esc = _html.escape
    body: list[str] = []
    body.append(f"<h1>{esc(document['title'])}</h1>")
    body.append('<h2 id="title-block">Title and revision block</h2>')
    rows = [
        ("Document number", document["number"]),
        ("Revision", document["revision"]),
        ("Title", document["title"]),
        ("Project", document["project"]),
        ("Client", document["client"]),
    ]
    if document.get("date"):
        rows.append(("Date", document["date"]))
    for role, key in (
        ("Prepared by", "prepared_by"),
        ("Checked by", "checked_by"),
        ("Approved by", "approved_by"),
    ):
        if document.get(key):
            rows.append((role, document[key]))
    body.append('<table class="titleblock"><tbody>')
    for label, value in rows:
        body.append(f"<tr><td>{esc(label)}</td><td>{esc(str(value))}</td></tr>")
    body.append("</tbody></table>")
    body.append("<h3>Revision history</h3>")
    body.append(
        "<table><thead><tr><th>Rev</th><th>Date</th><th>Description</th>"
        "<th>Prepared</th><th>Checked</th><th>Approved</th></tr></thead><tbody>"
    )
    for row in document["revision_history"]:
        body.append(
            "<tr>"
            + "".join(
                f"<td>{esc(str(row.get(key, '')))}</td>"
                for key in ("rev", "date", "description", "prepared", "checked", "approved")
            )
            + "</tr>"
        )
    body.append("</tbody></table>")
    for number, section in enumerate(sections, start=1):
        body.append(f'<h2 id="section-{number}">{number}. {esc(section["title"])}</h2>')
        body.append(_markdown_to_html(section["content"]))
        if _is_results_section(section["title"]):
            body.extend(_html_results(results))
    if citations:
        body.append('<h2 id="references-cited">References cited</h2>')
        body.append("<ol>")
        for citation in citations:
            note = f" {esc(citation.note)}" if citation.note else ""
            body.append(
                f"<li><strong>{esc(citation.code_id)}</strong> — "
                f"{esc(citation.publisher)}, {esc(citation.revision)}, "
                f"{esc(citation.section)}.{note}</li>"
            )
        body.append("</ol>")
    for appendix in appendices:
        body.append(
            f'<h2 id="appendix-{appendix["letter"].lower()}">'
            f"Appendix {appendix['letter']} — {esc(appendix['title'])}</h2>"
        )
        if appendix["content"]:
            body.append(_markdown_to_html(appendix["content"]))
        if appendix["files"]:
            body.append("<ul>")
            for item in appendix["files"]:
                body.append(f"<li><code>{esc(item)}</code></li>")
            body.append("</ul>")
    body_html = "\n".join(body)
    return (
        "<!DOCTYPE html>\n"
        '<html lang="en">\n'
        "<head>\n"
        '<meta charset="utf-8">\n'
        '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
        f"<title>{esc(document['number'])} — {esc(document['title'])}</title>\n"
        f"<style>\n{_CSS}\n</style>\n"
        "</head>\n"
        "<body>\n"
        '<div class="report-shell"><div class="report-page">\n'
        f"{body_html}\n"
        "</div></div>\n"
        "</body>\n"
        "</html>\n"
    )


def _html_results(results: dict[str, Any]) -> list[str]:
    esc = _html.escape
    parts: list[str] = []
    for table in results["tables"]:
        parts.append(f"<h3>{esc(table['title'])}</h3>")
        parts.append(
            "<table><thead><tr>"
            + "".join(f"<th>{esc(col)}</th>" for col in table["header"])
            + "</tr></thead><tbody>"
        )
        for row in table["rows"]:
            parts.append(
                "<tr>" + "".join(f"<td>{esc(cell)}</td>" for cell in row) + "</tr>"
            )
        parts.append("</tbody></table>")
        parts.append(f"<p>Source: <code>{esc(table['source'])}</code></p>")
    for figure in results["figures"]:
        data_uri = _figure_data_uri(figure["path"])
        parts.append(
            "<figure>"
            f'<img src="{data_uri}" alt="{esc(figure["title"])}">'
            f"<figcaption>{esc(figure['title'])}</figcaption>"
            "</figure>"
        )
    return parts


_FIGURE_MIME = {
    ".png": "image/png",
    ".jpg": "image/jpeg",
    ".jpeg": "image/jpeg",
    ".svg": "image/svg+xml",
    ".gif": "image/gif",
}


def _figure_data_uri(path: Path) -> str:
    mime = _FIGURE_MIME.get(path.suffix.lower())
    if mime is None:
        raise ReportPackConfigError(
            f"unsupported figure format {path.suffix!r} (use png/jpg/svg/gif): {path}"
        )
    payload = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:{mime};base64,{payload}"


def _markdown_to_html(markdown: str) -> str:
    """Minimal, deterministic markdown-to-HTML for section bodies.

    Supports paragraphs, unordered lists (``- ``), fenced code blocks,
    ``**bold**``, ``*italic*`` and `` `code` `` spans. Section headings come
    from the config (section titles), not from the body, so heading syntax in
    the body is intentionally rendered as text.
    """
    esc = _html.escape
    blocks: list[str] = []
    lines = markdown.splitlines()
    index = 0
    while index < len(lines):
        line = lines[index]
        if not line.strip():
            index += 1
            continue
        if line.strip().startswith("```"):
            code: list[str] = []
            index += 1
            while index < len(lines) and not lines[index].strip().startswith("```"):
                code.append(lines[index])
                index += 1
            index += 1  # closing fence
            blocks.append(f"<pre>{esc(chr(10).join(code))}</pre>")
            continue
        if line.lstrip().startswith("- "):
            items: list[str] = []
            while index < len(lines) and lines[index].lstrip().startswith("- "):
                items.append(
                    f"<li>{_inline_md(lines[index].lstrip()[2:].strip())}</li>"
                )
                index += 1
            blocks.append("<ul>" + "".join(items) + "</ul>")
            continue
        paragraph: list[str] = []
        while index < len(lines) and lines[index].strip() and not (
            lines[index].lstrip().startswith("- ")
            or lines[index].strip().startswith("```")
        ):
            paragraph.append(lines[index].strip())
            index += 1
        blocks.append(f"<p>{_inline_md(' '.join(paragraph))}</p>")
    return "\n".join(blocks)


def _inline_md(text: str) -> str:
    escaped = _html.escape(text)
    escaped = re.sub(r"`([^`]+)`", r"<code>\1</code>", escaped)
    escaped = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", escaped)
    escaped = re.sub(r"(?<!\*)\*([^*]+)\*(?!\*)", r"<em>\1</em>", escaped)
    return escaped


# ---------------------------------------------------------------------------
# PDF rendering — best-effort renderer chain, fail-soft by default
# ---------------------------------------------------------------------------


def _render_pdf(html_path: Path, pdf_path: Path, mode: str) -> tuple[bool, str]:
    """Render ``html_path`` to ``pdf_path`` using the first available renderer.

    Renderer chain: Playwright/Chromium, then Microsoft Edge headless (the
    documented Windows fallback: ``msedge --headless --print-to-pdf=...``),
    then Chrome/Chromium headless. Returns ``(written, status_message)``.
    ``mode='off'`` skips entirely; ``mode='require'`` raises if every renderer
    is unavailable or fails; ``mode='auto'`` fails soft with a clear message.
    """
    if mode == "off":
        return False, "pdf rendering disabled (report_pack.pdf: off)"

    attempts: list[str] = []

    written = _pdf_via_playwright(html_path, pdf_path, attempts)
    if not written:
        written = _pdf_via_browser_cli(html_path, pdf_path, attempts)

    if written:
        return True, f"pdf rendered via {attempts[-1]}"
    message = (
        "PDF not rendered — no PDF renderer available on this host. "
        "Tried: " + "; ".join(attempts) + ". "
        "Install Playwright (pip install playwright && playwright install chromium) "
        "or ensure Microsoft Edge / Chrome is on PATH "
        "(Windows fallback: msedge --headless --print-to-pdf). "
        "The md/html pack is complete; PDFs are limited derivatives of the "
        "approved HTML source."
    )
    if mode == "require":
        raise RuntimeError(message)
    return False, message


def _pdf_via_playwright(html_path: Path, pdf_path: Path, attempts: list[str]) -> bool:
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        attempts.append("playwright (not installed)")
        return False
    try:
        with sync_playwright() as playwright:
            browser = playwright.chromium.launch(headless=True)
            page = browser.new_page()
            page.goto(html_path.resolve().as_uri(), wait_until="networkidle")
            page.emulate_media(media="print")
            page.pdf(path=str(pdf_path), format="A4", print_background=True)
            browser.close()
    except Exception as exc:  # pragma: no cover - browser availability varies
        attempts.append(f"playwright (failed: {exc})")
        return False
    attempts.append("playwright/chromium")
    return pdf_path.is_file()


_EDGE_DEFAULT_PATHS = (
    r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe",
    r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
)


def _pdf_via_browser_cli(html_path: Path, pdf_path: Path, attempts: list[str]) -> bool:
    candidates: list[tuple[str, str]] = []
    for name in ("msedge", "microsoft-edge", "chrome", "google-chrome", "chromium",
                 "chromium-browser"):
        located = shutil.which(name)
        if located:
            candidates.append((name, located))
    for default in _EDGE_DEFAULT_PATHS:
        if Path(default).is_file():
            candidates.append(("msedge", default))
            break
    if not candidates:
        attempts.append("edge/chrome headless (no browser executable found)")
        return False
    for name, executable in candidates:
        command = [
            executable,
            "--headless",
            "--disable-gpu",
            "--no-sandbox",
            f"--print-to-pdf={pdf_path.resolve()}",
            html_path.resolve().as_uri(),
        ]
        try:
            completed = subprocess.run(
                command, capture_output=True, timeout=120, check=False
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            attempts.append(f"{name} headless (failed: {exc})")
            continue
        if completed.returncode == 0 and pdf_path.is_file():
            attempts.append(f"{name} headless --print-to-pdf")
            return True
        attempts.append(f"{name} headless (exit {completed.returncode})")
    return False


# ---------------------------------------------------------------------------
# Path helpers (engine cfg conventions)
# ---------------------------------------------------------------------------


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "report_pack"))


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)


def _write_json(path: Path, payload: Any) -> None:
    path.write_text(
        json.dumps(payload, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
        newline="\n",
    )
