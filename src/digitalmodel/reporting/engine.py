#!/usr/bin/env python3
"""Standard report engine — renders a :class:`ReportSpec` to HTML (+ PDF).

ABOUTME: One Jinja2 template, one CSS, one PDF chain for every domain report
(#2212). ``render_html`` is pure (spec in, HTML out, no clock, no network);
``write_report`` adds the PDF attempt, the citations sidecar and a manifest.

The HTML is self-contained: plotly.js is inlined once, only when a figure
needs it, and there is never a ``<script src=``.
"""

from __future__ import annotations

import base64
import html as _html
import json
import mimetypes
import re
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterator, Mapping

from jinja2 import Environment, FileSystemLoader, select_autoescape
from markupsafe import Markup

from digitalmodel.reporting.calc_report import _DEFAULT_TEMPLATE, _template_parts
from digitalmodel.reporting.figures import inline_plotlyjs, plotly_div
from digitalmodel.reporting.pdf import PdfMode, PdfStatus, render_pdf
from digitalmodel.reporting.spec import FigureBlock, ReportSpec

EXECUTION_TOOL = "digitalmodel.reporting.engine"
BRAND = "digitalmodel"
ASSETS_DIR = Path(__file__).parent / "assets"
TEMPLATE_NAME = "standard_report.html.j2"
ENGINE_CSS = ASSETS_DIR / "standard_report.css"
APPENDIX_LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#: Footer text at HTML render time; the PDF is a derivative rendered afterwards
#: and its outcome is recorded in the manifest, never in the HTML.
PDF_STATUS_PLACEHOLDER = "derivative of this HTML; render status in the manifest"


@dataclass(frozen=True)
class ReportArtifacts:
    """Paths written by :func:`write_report`."""

    html_path: Path
    pdf_path: Path | None
    pdf_status: PdfStatus
    citations_json_path: Path
    manifest_path: Path


# ---------------------------------------------------------------------------
# Markdown subset (paragraphs, headings, lists, fenced code, inline marks)
# ---------------------------------------------------------------------------


_BULLET_RE = re.compile(r"^\s*[-*]\s+")
_NUMBERED_RE = re.compile(r"^\s*\d+[.)]\s+")
_HEADING_RE = re.compile(r"^#{1,3}\s")


def _inline_md(text: str) -> str:
    escaped = _html.escape(text)
    escaped = re.sub(r"`([^`]+)`", r"<code>\1</code>", escaped)
    escaped = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", escaped)
    escaped = re.sub(r"(?<!\*)\*([^*]+)\*(?!\*)", r"<em>\1</em>", escaped)
    return escaped


def _md_blocks(text: str) -> Iterator[list[str]]:
    block: list[str] = []
    in_fence = False
    for line in text.replace("\r\n", "\n").split("\n"):
        if line.startswith("```"):
            in_fence = not in_fence
            block.append(line)
            if not in_fence:
                yield block
                block = []
            continue
        if in_fence:
            block.append(line)
        elif line.strip():
            block.append(line)
        elif block:
            yield block
            block = []
    if block:
        yield block


def markdown_to_html(text: str) -> str:
    """Render the Markdown subset used by :class:`TextBlock` (escaped)."""
    parts: list[str] = []
    for block in _md_blocks(text):
        first = block[0]
        if first.startswith("```"):
            body = "\n".join(block[1:-1] if block[-1].startswith("```") else block[1:])
            parts.append(f"<pre><code>{_html.escape(body)}</code></pre>")
        elif _HEADING_RE.match(first):
            level = min(len(first) - len(first.lstrip("#")), 2) + 2
            parts.append(
                f"<h{level}>{_inline_md(first.lstrip('#').strip())}</h{level}>"
            )
        elif all(_BULLET_RE.match(line) for line in block):
            items = "".join(
                "<li>" + _inline_md(_BULLET_RE.sub("", line)) + "</li>"
                for line in block
            )
            parts.append(f"<ul>{items}</ul>")
        elif all(_NUMBERED_RE.match(line) for line in block):
            items = "".join(
                "<li>" + _inline_md(_NUMBERED_RE.sub("", line)) + "</li>"
                for line in block
            )
            parts.append(f"<ol>{items}</ol>")
        else:
            parts.append(f"<p>{_inline_md(' '.join(s.strip() for s in block))}</p>")
    return "\n".join(parts)


# ---------------------------------------------------------------------------
# Template helpers
# ---------------------------------------------------------------------------


def _format_cell(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "yes" if value else "no"
    if isinstance(value, float):
        return f"{value:g}" if abs(value) >= 1e-3 or value == 0 else f"{value:.3e}"
    return str(value)


def _flatten_echo(mapping: Mapping[str, Any], prefix: str = "") -> list[tuple[str, str]]:
    rows: list[tuple[str, str]] = []
    for key, value in mapping.items():
        dotted = f"{prefix}{key}"
        if isinstance(value, Mapping):
            rows.extend(_flatten_echo(value, f"{dotted}."))
        elif isinstance(value, (list, tuple)):
            rows.append((dotted, json.dumps(list(value), ensure_ascii=False)))
        else:
            rows.append((dotted, _format_cell(value)))
    return rows


def _image_data_uri(image_path: str) -> str:
    path = Path(image_path)
    if not path.is_file():
        raise FileNotFoundError(f"FigureBlock image not found: {image_path}")
    mime = mimetypes.guess_type(path.name)[0] or "application/octet-stream"
    payload = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:{mime};base64,{payload}"


def _figure_html(figures: list[FigureBlock]) -> dict[str, str]:
    rendered: dict[str, str] = {}
    for block in figures:
        if block.plotly is not None:
            rendered[block.figure_id] = plotly_div(block.plotly, block.figure_id)
        elif block.image_path is not None:
            rendered[block.figure_id] = _image_data_uri(block.image_path)
    return rendered


def _environment() -> Environment:
    env = Environment(
        loader=FileSystemLoader(str(ASSETS_DIR)),
        autoescape=select_autoescape(default=True, default_for_string=True),
        trim_blocks=False,
        lstrip_blocks=False,
        keep_trailing_newline=True,
    )
    env.filters["md"] = lambda text: Markup(markdown_to_html(text))
    env.filters["cell"] = _format_cell
    return env


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def render_html(spec: ReportSpec, *, pdf_status: str = PDF_STATUS_PLACEHOLDER) -> str:
    """Render the spec to a self-contained HTML document.

    Raises :class:`~digitalmodel.reporting.provenance.ProvenanceError` when the
    spec declares no data source. plotly.js is inlined only when a figure
    carries a Plotly payload.
    """
    spec.provenance.require()
    house_css, _script = _template_parts(_DEFAULT_TEMPLATE)
    template = _environment().get_template(TEMPLATE_NAME)
    return template.render(
        brand=BRAND,
        doc=spec.document,
        standards=spec.standards,
        citations=spec.citations,
        sections=spec.sections,
        appendices=spec.appendices,
        letters=APPENDIX_LETTERS,
        provenance=spec.provenance,
        input_echo=_flatten_echo(spec.input_echo),
        figure_html=_figure_html(spec.figure_blocks()),
        plotlyjs=inline_plotlyjs() if spec.has_plotly() else "",
        house_css=house_css,
        engine_css=ENGINE_CSS.read_text(encoding="utf-8"),
        execution_tool=EXECUTION_TOOL,
        tool_version=spec.tool_version,
        pdf_status=pdf_status,
    )


def _write_json(path: Path, payload: Any) -> None:
    path.write_text(
        json.dumps(payload, indent=2, ensure_ascii=False, sort_keys=True) + "\n",
        encoding="utf-8",
        newline="\n",
    )


def write_report(
    spec: ReportSpec,
    out_dir: Path | str,
    stem: str,
    pdf: PdfMode = "auto",
) -> ReportArtifacts:
    """Write ``<stem>.html``, ``<stem>.pdf`` (best effort), the citations
    sidecar and a manifest under ``out_dir``.

    The manifest lists artifact names (pack-relative, portable), the
    standards, the citation count and the PDF status; keys are sorted and no
    timestamp is generated.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    html_path = out_dir / f"{stem}.html"
    pdf_path = out_dir / f"{stem}.pdf"
    citations_path = out_dir / f"{stem}_citations.json"
    manifest_path = out_dir / f"{stem}_manifest.json"

    html_path.write_text(render_html(spec), encoding="utf-8", newline="\n")
    status = render_pdf(html_path, pdf_path, pdf)
    _write_json(citations_path, {"citations": spec.citations})

    manifest: dict[str, Any] = {
        "artifacts": {
            "html": html_path.name,
            "pdf": pdf_path.name if status.rendered else None,
            "citations": citations_path.name,
            "manifest": manifest_path.name,
        },
        "document": {
            "number": spec.document.number,
            "revision": spec.document.revision,
            "title": spec.document.title,
            "project": spec.document.project,
            "client": spec.document.client,
        },
        "standards": [s.model_dump() for s in spec.standards],
        "citations_count": len(spec.citations),
        "pdf": asdict(status),
        "execution_tool": EXECUTION_TOOL,
        "tool_version": spec.tool_version,
        "report_layer": spec.manifest or None,
    }
    _write_json(manifest_path, manifest)
    return ReportArtifacts(
        html_path=html_path,
        pdf_path=pdf_path if status.rendered else None,
        pdf_status=status,
        citations_json_path=citations_path,
        manifest_path=manifest_path,
    )


__all__ = [
    "EXECUTION_TOOL",
    "PDF_STATUS_PLACEHOLDER",
    "ReportArtifacts",
    "markdown_to_html",
    "render_html",
    "write_report",
]
