# ABOUTME: Routed workflow that renders a standard marine engineering report pack
# ABOUTME: (md + engine-rendered html + optional pdf + citations + provenance manifest).
"""Durable workflow: standard marine engineering report pack.

Renders a complete engineering report pack following the standard marine
engineering report structure: numbered document (``JOB-DOCTYPE-SEQ-REV``),
title/revision block with prepared/checked/approved rows, a fixed section
skeleton (introduction/scope -> references -> design basis -> methodology &
assumptions -> results -> conclusions & limitations), capital-lettered
appendices, a citation sidecar, and a report-layer provenance manifest.

This module is the YAML front end of the standard report engine (#2212): it
validates the config, builds a :class:`~digitalmodel.reporting.spec.ReportSpec`
(:func:`build_report_spec`) and lets :func:`digitalmodel.reporting.engine.render_html`
and :func:`digitalmodel.reporting.pdf.render_pdf` produce the HTML and PDF.
Only the markdown body and the two manifests are written here.

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
            units_row: true          # optional: second CSV row holds the units
          - title: Summary
            csv: data/summary.csv
            units: [-, t]            # optional: inline units, one per column
        figures:
          - title: Damage histogram
            path: data/figure.svg    # .svg inlined; png/jpg/gif embedded as data URI
      appendices:                    # optional; lettered A, B, C ... (or explicit letter)
        - title: References
          content: ...
        - letter: B
          title: Calculation Records
          files: [data/results.csv]  # listed as appendix contents
      citations:                     # optional; emitted to <stem>_citations.json
        - code_id: DNV-RP-C203       # each distinct code_id also becomes a
          publisher: DNV             # standard/edition chip in the HTML header
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
(self-contained -- rendered by the standard report engine, no CDN scripts),
``<stem>_report.pdf`` (best-effort, see below), ``<stem>_citations.json``,
``<stem>_manifest.json`` (file manifest) and ``report-layer-manifest.json``
(provenance manifest, required-field validated).

PDF rendering is optional by design (``pdf: auto``): the engine's chain tries,
in order, Playwright/Chromium, Microsoft Edge headless (``msedge --headless
--print-to-pdf`` -- the documented Windows fallback), then Chrome/Chromium
headless. If no renderer is available the pack is still emitted and
``pdf_status`` carries a clear message; ``pdf: require`` turns that into an
error, ``pdf: off`` skips the attempt entirely.

Determinism: no timestamps are generated -- dates render only when supplied in
config -- so re-running the same config produces byte-identical md/html/json.
"""

from __future__ import annotations

import csv
import hashlib
import json
import re
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Optional, cast

from digitalmodel.citations.schema import Citation, CitationValidationError
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.pdf import PDF_MODES, PdfMode, render_pdf
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.spec import (
    DOC_NUMBER_RE,
    MANIFEST_REQUIRED_FIELDS,
    DocumentMeta,
    FigureBlock,
    ReportSpec,
    RevisionRow,
    Section,
    StandardLabel,
    TableBlock,
    TextBlock,
)

REPO_ROOT = Path(__file__).resolve().parents[3]

EXECUTION_TOOL = "digitalmodel.report_pack.workflow"

#: JOB-DOCTYPE-SEQ-REV, doctype optional (e.g. B0000-001-00 or B0000-RPT-001-00).
#: Defined once in ``digitalmodel.reporting.spec``; re-exported here.

APPENDIX_LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#: Report-layer manifest fields the caller must supply (provenance contract).
#: Defined once in ``digitalmodel.reporting.spec``; re-exported here.
#: Optional caller-supplied manifest fields carried through when present.
MANIFEST_OPTIONAL_FIELDS = ("parent_issue", "source_artifacts")

#: Figure formats a pack may declare; ``.svg`` is inlined, the rest embedded.
FIGURE_SUFFIXES = (".png", ".jpg", ".jpeg", ".svg", ".gif")

_SVG_PROLOG_RE = re.compile(r"^\s*(<\?xml[^>]*\?>\s*)?(<!DOCTYPE[^>]*>\s*)?", re.S)


class ReportPackConfigError(ValueError):
    """Raised when the report_pack config is structurally invalid."""


class ReportPackManifestError(ValueError):
    """Raised when the report-layer manifest inputs fail schema validation."""


@dataclass(frozen=True)
class ReportPack:
    """The validated content of one ``report_pack`` config (see :func:`load_pack`)."""

    stem: str
    config_dir: Path
    output_dir: Path
    document: dict[str, Any]
    sections: list[dict[str, str]]
    results: dict[str, Any]
    appendices: list[dict[str, Any]]
    citations: list[Citation]
    manifest_inputs: dict[str, Any]
    pdf_mode: PdfMode


# ---------------------------------------------------------------------------
# Router
# ---------------------------------------------------------------------------


def router(cfg: dict) -> dict:
    pack = load_pack(cfg)
    settings = cfg["report_pack"]
    spec = build_report_spec(pack)

    out_dir = pack.output_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    stem = pack.stem

    md_path = out_dir / f"{stem}_report.md"
    html_path = out_dir / f"{stem}_report.html"
    pdf_path = out_dir / f"{stem}_report.pdf"
    citations_path = out_dir / f"{stem}_citations.json"
    file_manifest_path = out_dir / f"{stem}_manifest.json"
    layer_manifest_path = out_dir / "report-layer-manifest.json"

    markdown = render_markdown(
        pack.document, pack.sections, pack.results, pack.appendices, pack.citations
    )
    md_path.write_text(markdown, encoding="utf-8", newline="\n")
    html_path.write_text(render_html(spec), encoding="utf-8", newline="\n")

    pdf_status = render_pdf(html_path, pdf_path, pack.pdf_mode)
    pdf_written = pdf_status.rendered

    _write_json(citations_path, {"citations": spec.citations})

    emitted = [md_path, html_path, citations_path]
    if pdf_written:
        emitted.append(pdf_path)

    # Pack-relative names: the pack directory is the portable unit, so its
    # internal manifests must not leak machine-specific absolute paths.
    file_manifest = {
        "markdown_report": md_path.name,
        "html_report": html_path.name,
        "pdf_report": pdf_path.name if pdf_written else None,
        "pdf_status": pdf_status.message,
        "citation_sidecar": citations_path.name,
        "report_layer_manifest": layer_manifest_path.name,
    }
    _write_json(file_manifest_path, file_manifest)
    emitted.append(file_manifest_path)

    layer_manifest = build_report_layer_manifest(
        pack.manifest_inputs,
        citation_evidence_manifest=citations_path.name,
        generated_manifest=file_manifest_path.name,
        files=sorted(p.name for p in emitted) + ["report-layer-manifest.json"],
    )
    _write_json(layer_manifest_path, layer_manifest)

    cfg["report_pack"] = {
        **settings,
        "document_number": pack.document["number"],
        "markdown_report": _display_path(md_path),
        "html_report": _display_path(html_path),
        "pdf_report": _display_path(pdf_path) if pdf_written else None,
        "pdf_status": pdf_status.message,
        "citations_json": _display_path(citations_path),
        "file_manifest": _display_path(file_manifest_path),
        "report_layer_manifest": _display_path(layer_manifest_path),
    }
    return cfg


def load_pack(cfg: dict) -> ReportPack:
    """Validate the routed config and resolve every file it references."""
    settings = cfg.get("report_pack") or {}
    if not isinstance(settings, dict) or not settings:
        raise ReportPackConfigError("report_pack settings block is required")
    config_dir = _config_dir(cfg)
    return ReportPack(
        stem=_input_stem(cfg),
        config_dir=config_dir,
        output_dir=_output_dir(cfg, settings),
        document=_validate_document(settings),
        sections=_validate_sections(settings, config_dir),
        results=_load_results(settings, config_dir),
        appendices=_validate_appendices(settings, config_dir),
        citations=_validate_citations(settings),
        manifest_inputs=_validate_manifest_inputs(settings),
        pdf_mode=_pdf_mode(settings),
    )


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
        label = f"report_pack.results.tables[{index}]"
        if not isinstance(entry, dict) or not entry.get("csv"):
            raise ReportPackConfigError(f"{label} needs a csv path")
        title = str(entry.get("title", f"Results table {index}")).strip()
        csv_path = _resolve_file(entry["csv"], config_dir, f"{label}.csv")
        with csv_path.open(newline="", encoding="utf-8") as stream:
            rows = list(csv.reader(stream))
        units, rows = _table_units(entry, rows, label)
        if len(rows) < 2:
            raise ReportPackConfigError(f"{label}.csv has no data rows: {csv_path}")
        tables.append(
            {
                "title": title,
                "header": rows[0],
                "units": units,
                "rows": rows[1:],
                "source": csv_path.name,
                "identifier": _pack_relative(csv_path, config_dir),
                "digest": _content_digest(csv_path),
            }
        )
    figures = []
    for index, entry in enumerate(raw.get("figures") or [], start=1):
        label = f"report_pack.results.figures[{index}]"
        if not isinstance(entry, dict) or not entry.get("path"):
            raise ReportPackConfigError(f"{label} needs a path")
        fig_path = _resolve_file(entry["path"], config_dir, f"{label}.path")
        if fig_path.suffix.lower() not in FIGURE_SUFFIXES:
            raise ReportPackConfigError(
                f"unsupported figure format {fig_path.suffix!r} "
                f"(use png/jpg/svg/gif): {fig_path}"
            )
        figures.append(
            {
                "title": str(entry.get("title", f"Figure {index}")).strip(),
                "path": fig_path,
                "svg": _inline_svg(fig_path, label)
                if fig_path.suffix.lower() == ".svg"
                else None,
                "identifier": _pack_relative(fig_path, config_dir),
                "digest": _content_digest(fig_path),
            }
        )
    return {"tables": tables, "figures": figures}


def _table_units(
    entry: dict[str, Any], rows: list[list[str]], label: str
) -> tuple[Optional[list[str]], list[list[str]]]:
    """Resolve the optional units row: ``units_row: true`` (second CSV row) or
    an inline ``units:`` list. Returns ``(units, rows_without_units_row)``."""
    units_row = entry.get("units_row", False)
    inline = entry.get("units")
    if units_row and inline is not None:
        raise ReportPackConfigError(f"{label}: give units_row or units, not both")
    if units_row:
        if len(rows) < 2:
            raise ReportPackConfigError(f"{label}.csv has no units row")
        return [str(u) for u in rows[1]], [rows[0], *rows[2:]]
    if inline is not None:
        if not isinstance(inline, list) or not rows or len(inline) != len(rows[0]):
            raise ReportPackConfigError(
                f"{label}.units must be a list with one entry per column"
            )
        return [str(u) for u in inline], rows
    return None, rows


def _inline_svg(path: Path, label: str) -> str:
    """Read an SVG file for inline embedding (XML prolog dropped, no scripts)."""
    text = path.read_text(encoding="utf-8")
    body = _SVG_PROLOG_RE.sub("", text, count=1).strip()
    if not body.startswith("<svg"):
        raise ReportPackConfigError(f"{label}.path is not an SVG document: {path}")
    if "<script" in body.lower():
        raise ReportPackConfigError(
            f"{label}.path: SVG figures must not contain scripts: {path}"
        )
    return body


def _resolve_file(value: Any, config_dir: Path, label: str) -> Path:
    path = Path(str(value))
    if not path.is_absolute():
        path = config_dir / path
    if not path.is_file():
        raise ReportPackConfigError(f"{label} not found: {path}")
    return path


def _pack_relative(path: Path, config_dir: Path) -> str:
    """Portable identifier: relative to the config dir when inside it, else the name."""
    try:
        return path.resolve().relative_to(config_dir.resolve()).as_posix()
    except ValueError:
        return path.name


def _content_digest(path: Path) -> str:
    """sha256 of the file with CRLF normalised, so checkouts agree."""
    payload = path.read_bytes().replace(b"\r\n", b"\n")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


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


def _pdf_mode(settings: dict[str, Any]) -> PdfMode:
    mode = str(settings.get("pdf", "auto")).strip().lower()
    if mode not in PDF_MODES:
        raise ReportPackConfigError(
            f"report_pack.pdf must be one of {PDF_MODES}: got {mode!r}"
        )
    return cast(PdfMode, mode)


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
# Spec assembly (the engine renders the HTML/PDF from this)
# ---------------------------------------------------------------------------


def build_report_spec(pack: ReportPack) -> ReportSpec:
    """Map the validated pack onto the standard report engine's contract.

    ``document`` -> :class:`DocumentMeta`; each distinct cited ``code_id`` ->
    a :class:`StandardLabel` (edition = citation revision, provenance = wiki
    path); sections -> :class:`TextBlock` plus, in every section whose title
    contains "result", one :class:`TableBlock` per CSV and one
    :class:`FigureBlock` per figure (SVG inlined, rasters embedded); appendices
    keep their explicit letters. Provenance is declared from the manifest's
    input source IDs and source-artifact pointers plus every CSV/figure read
    (with a content digest); the engine refuses to render without one.
    """
    document = pack.document
    meta = DocumentMeta(
        number=document["number"],
        revision=document["revision"],
        title=str(document["title"]).strip(),
        project=str(document["project"]).strip(),
        client=str(document["client"]).strip(),
        date=str(document["date"]) if document.get("date") else None,
        prepared_by=str(document.get("prepared_by") or ""),
        checked_by=str(document.get("checked_by") or ""),
        approved_by=str(document.get("approved_by") or ""),
        revision_history=[
            RevisionRow(
                rev=str(row.get("rev", "")),
                date=str(row.get("date") or ""),
                description=str(row.get("description") or ""),
                prepared=str(row.get("prepared") or ""),
                checked=str(row.get("checked") or ""),
                approved=str(row.get("approved") or ""),
            )
            for row in document["revision_history"]
        ],
    )

    result_blocks = _result_blocks(pack.results)
    sections: list[Section] = []
    for number, section in enumerate(pack.sections, start=1):
        blocks: list[Any] = [TextBlock(markdown=section["content"])]
        if _is_results_section(section["title"]):
            blocks.extend(result_blocks)
        sections.append(
            Section(key=f"section-{number}", title=section["title"], blocks=blocks)
        )

    appendices: list[Section] = []
    for appendix in pack.appendices:
        blocks = []
        if appendix["content"]:
            blocks.append(TextBlock(markdown=appendix["content"]))
        if appendix["files"]:
            blocks.append(
                TextBlock(markdown="\n".join(f"- `{item}`" for item in appendix["files"]))
            )
        appendices.append(
            Section(
                key=f"appendix-{appendix['letter'].lower()}",
                title=appendix["title"],
                label=f"Appendix {appendix['letter']}",
                blocks=blocks,
            )
        )

    return ReportSpec(
        document=meta,
        standards=_standards_from_citations(pack.citations),
        citations=[asdict(citation) for citation in pack.citations],
        sections=sections,
        provenance=_provenance(pack),
        manifest=dict(pack.manifest_inputs),
        input_echo=_input_echo(pack),
        appendices=appendices,
    )


def _result_blocks(results: dict[str, Any]) -> list[Any]:
    blocks: list[Any] = []
    for table in results["tables"]:
        blocks.append(
            TableBlock(
                title=table["title"],
                columns=list(table["header"]),
                units=table["units"],
                rows=[list(row) for row in table["rows"]],
                source=table["source"],
            )
        )
    for index, figure in enumerate(results["figures"], start=1):
        if figure["svg"] is not None:
            blocks.append(
                FigureBlock(
                    title=figure["title"], figure_id=f"figure-{index}", svg=figure["svg"]
                )
            )
        else:
            blocks.append(
                FigureBlock(
                    title=figure["title"],
                    figure_id=f"figure-{index}",
                    image_path=str(figure["path"]),
                )
            )
    return blocks


def _standards_from_citations(citations: list[Citation]) -> list[StandardLabel]:
    """One header/footer chip per distinct cited code, first citation wins."""
    labels: list[StandardLabel] = []
    seen: set[str] = set()
    for citation in citations:
        if citation.code_id in seen:
            continue
        seen.add(citation.code_id)
        labels.append(
            StandardLabel(
                code_id=citation.code_id,
                edition=citation.revision,
                provenance=citation.wiki_path,
            )
        )
    return labels


def _provenance(pack: ReportPack) -> Provenance:
    provenance = Provenance()
    for source_id in pack.manifest_inputs["input_source_ids"]:
        provenance.add(
            "input_source", source_id, description="report_pack.manifest.input_source_ids"
        )
    for name, pointer in (pack.manifest_inputs.get("source_artifacts") or {}).items():
        provenance.add("source_artifact", str(pointer), description=str(name))
    for table in pack.results["tables"]:
        provenance.add(
            "csv", table["identifier"], digest=table["digest"], description=table["title"]
        )
    for figure in pack.results["figures"]:
        provenance.add(
            "figure",
            figure["identifier"],
            digest=figure["digest"],
            description=figure["title"],
        )
    return provenance


def _input_echo(pack: ReportPack) -> dict[str, Any]:
    """What the pack was built from (portable identifiers only, no host paths)."""
    manifest = pack.manifest_inputs
    return {
        "pdf": pack.pdf_mode,
        "results": {
            "tables": [t["identifier"] for t in pack.results["tables"]],
            "figures": [f["identifier"] for f in pack.results["figures"]],
        },
        "manifest": {
            "input_source_ids": list(manifest["input_source_ids"]),
            "source_artifacts": dict(manifest.get("source_artifacts") or {}),
            "raw_output_path": manifest["raw_output_path"],
            "final_output_path": manifest["final_output_path"],
        },
    }


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
        if table["units"]:
            lines.append("| " + " | ".join(table["units"]) + " |")
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


__all__ = [
    "APPENDIX_LETTERS",
    "DOC_NUMBER_RE",
    "EXECUTION_TOOL",
    "FIGURE_SUFFIXES",
    "MANIFEST_OPTIONAL_FIELDS",
    "MANIFEST_REQUIRED_FIELDS",
    "PDF_MODES",
    "ReportPack",
    "ReportPackConfigError",
    "ReportPackManifestError",
    "build_report_layer_manifest",
    "build_report_spec",
    "load_pack",
    "render_markdown",
    "render_pdf",
    "router",
]
