#!/usr/bin/env python3
"""Reporting block library — provenance + SSOT-view enforcement (#1019).

ABOUTME: Makes a report a *view* over a single source of truth, with
**mandatory provenance**. A report assembled through :func:`assemble_report`
must declare where its numbers came from (one or more :class:`DataSource`) or
rendering fails closed -- killing the hand-pasted-results anti-pattern. The
non-user-supplied :class:`~digitalmodel.common.assumption_ledger.AssumptionLedger`
renders as a first-class provenance block (the #622 "no silent assumptions"
surface), reusable by every reporter instead of being diffraction-only.
"""

from __future__ import annotations

import html
from pathlib import Path
from typing import TYPE_CHECKING, Any, List, Optional

from pydantic import Field

from digitalmodel.reporting._backbone import ReportBackbone, ReportSection
from digitalmodel.reporting._base import ReportDataModel
from digitalmodel.reporting._renderer import ReportRenderer

if TYPE_CHECKING:  # avoid importing the ledger at module load
    from digitalmodel.common.assumption_ledger import AssumptionLedger


class ProvenanceError(RuntimeError):
    """Raised when a report is assembled without declared provenance."""


class DataSource(ReportDataModel):
    """One declared origin of a report's data -- *where the numbers came from*.

    ``retrieved_at`` is caller-supplied (kept out of the library so reports stay
    deterministic and testable); pass an ISO timestamp when you have one.
    """

    kind: str = Field(
        description="Source category, e.g. 'spec', 'solver_queue', "
        "'analysis_store', 'file'."
    )
    identifier: str = Field(description="Path, run-id, or store key.")
    digest: Optional[str] = Field(
        default=None, description="Content hash, e.g. sha256."
    )
    retrieved_at: Optional[str] = None
    description: Optional[str] = None


class Provenance(ReportDataModel):
    """The set of sources a report is a view over."""

    sources: List[DataSource] = Field(default_factory=list)

    def add(
        self,
        kind: str,
        identifier: str,
        *,
        digest: Optional[str] = None,
        retrieved_at: Optional[str] = None,
        description: Optional[str] = None,
    ) -> "Provenance":
        """Append a source and return self (chainable)."""
        self.sources.append(
            DataSource(
                kind=kind,
                identifier=identifier,
                digest=digest,
                retrieved_at=retrieved_at,
                description=description,
            )
        )
        return self

    def require(self) -> "Provenance":
        """Enforce mandatory provenance; raise if no source is declared."""
        if not self.sources:
            raise ProvenanceError(
                "Report has no declared data source -- provenance is mandatory "
                "(#1019: a report is a view over a source, not a copy). Declare "
                "at least one DataSource."
            )
        return self


# ---------------------------------------------------------------------------
# Block builders (return complete ``<div class="section">`` fragments)
# ---------------------------------------------------------------------------


def provenance_block(provenance: Provenance, **_: Any) -> str:
    """Render the data-source declaration table (id=``provenance``)."""
    rows = []
    for src in provenance.sources:
        rows.append(
            "<tr>"
            f"<td>{html.escape(src.kind)}</td>"
            f"<td>{html.escape(src.identifier)}</td>"
            f"<td>{html.escape(src.digest or '')}</td>"
            f"<td>{html.escape(src.retrieved_at or '')}</td>"
            f"<td>{html.escape(src.description or '')}</td>"
            "</tr>"
        )
    body = "".join(rows) or (
        '<tr><td colspan="5"><em>No sources declared.</em></td></tr>'
    )
    return (
        '<div class="section" id="provenance">'
        "<h2>Data provenance</h2>"
        "<p>This report is a <strong>view</strong> over the sources below; "
        "values are read from them, not copied in.</p>"
        "<table><thead><tr><th>Kind</th><th>Identifier</th><th>Digest</th>"
        "<th>Retrieved</th><th>Description</th></tr></thead>"
        f"<tbody>{body}</tbody></table>"
        "</div>"
    )


def assumption_ledger_block(ledger: "AssumptionLedger", **_: Any) -> str:
    """Render the non-user-supplied assumption ledger (id=``assumptions``).

    Generalized from the diffraction reporter so any report can surface the
    no-silent-assumptions ledger. An empty ledger renders an explicit
    "every value was supplied" note rather than nothing.
    """
    if not ledger:
        return (
            '<div class="section" id="assumptions">'
            "<h2>Assumptions</h2>"
            "<p>No assumed values &mdash; every value was user-supplied.</p>"
            "</div>"
        )
    rows = []
    for record in ledger.rows():
        rows.append(
            "<tr>"
            f"<td>{html.escape(record.field)}</td>"
            f"<td>{html.escape(str(record.value))}</td>"
            f"<td>{html.escape(record.source.value)}</td>"
            f"<td>{html.escape(record.confidence.value)}</td>"
            f"<td>{html.escape(record.basis)}</td>"
            f"<td>{html.escape(str(record.reference or ''))}</td>"
            "</tr>"
        )
    return (
        '<div class="section" id="assumptions">'
        "<h2>Assumptions</h2>"
        "<p>Every value below was <strong>assumed</strong> by the resolver "
        "(not supplied) and is surfaced for review.</p>"
        "<table><thead><tr><th>Field</th><th>Value</th><th>Source</th>"
        "<th>Confidence</th><th>Basis</th><th>Reference</th></tr></thead>"
        f"<tbody>{''.join(rows)}</tbody></table>"
        "</div>"
    )


# ---------------------------------------------------------------------------
# Assembler — enforces mandatory provenance, appends provenance + ledger
# ---------------------------------------------------------------------------


def assemble_report(
    title: str,
    *,
    provenance: Provenance,
    sections: Optional[List[ReportSection]] = None,
    data: Any = None,
    ledger: Optional["AssumptionLedger"] = None,
    mode: str = "full",
    css: str = "",
    output_path: Optional[Path | str] = None,
    **builder_kwargs: Any,
) -> str | Path:
    """Assemble a provenance-gated report on the shared backbone.

    Fails closed via :meth:`Provenance.require` if no source is declared. Body
    ``sections`` (standard :class:`ReportSection` builders over ``data``) render
    first, then the mandatory **provenance** block, then the **assumption
    ledger** block (when a ledger is given). Returns the HTML string, or writes
    to ``output_path`` and returns the :class:`Path`.
    """
    provenance.require()

    body: List[str] = []
    if sections:
        backbone = ReportBackbone(title=title, sections=sections)
        body.extend(backbone.render(data, mode=mode, **builder_kwargs))
    body.append(provenance_block(provenance))
    if ledger is not None:
        body.append(assumption_ledger_block(ledger))

    renderer = ReportRenderer()
    html_doc = renderer.render_html(body, title=title, css=css)
    if output_path is not None:
        return renderer.write_html(html_doc, Path(output_path))
    return html_doc


__all__ = [
    "ProvenanceError",
    "DataSource",
    "Provenance",
    "provenance_block",
    "assumption_ledger_block",
    "assemble_report",
]
