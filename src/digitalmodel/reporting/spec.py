#!/usr/bin/env python3
"""Standard report contract — the typed spec every domain report is built from.

ABOUTME: Pydantic v2 models for the standard HTML/PDF reporting engine (#2212).
A domain adapter fills a :class:`ReportSpec` (document control, standards,
citations, sections of typed blocks, provenance, manifest, input echo) and the
engine renders it; no domain writes HTML.

Determinism rules: nothing here reads the clock. Dates render only when the
caller supplies them, so re-rendering the same spec is byte-identical.
"""

from __future__ import annotations

import re
from dataclasses import asdict
from typing import Annotated, Any, Literal, Mapping, Optional, Union

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator

from digitalmodel.citations.schema import Citation, CitationValidationError
from digitalmodel.reporting.provenance import Provenance

#: JOB-DOCTYPE-SEQ-REV, doctype optional (e.g. B0000-001-00 or B0000-RPT-001-00).
#: Single source of truth; ``report_pack.workflow`` re-exports it.
DOC_NUMBER_RE = re.compile(r"^[A-Z]\d{3,4}(-[A-Z0-9]{1,8})?-\d{3}-\d{2}$")

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

_HTML_ID_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_-]*$")


def _tool_version() -> str:
    try:
        from digitalmodel import __version__
    except Exception:  # pragma: no cover - version metadata is optional
        return "unknown"
    return str(__version__)


def _non_empty(value: str, name: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a non-empty string")
    return value.strip()


class _Strict(BaseModel):
    model_config = ConfigDict(extra="forbid")


# ---------------------------------------------------------------------------
# Document control
# ---------------------------------------------------------------------------


class RevisionRow(_Strict):
    """One row of the revision-history table."""

    rev: str
    date: str = ""
    description: str = ""
    prepared: str = ""
    checked: str = ""
    approved: str = ""


class DocumentMeta(_Strict):
    """Title-block metadata: ``JOB-DOCTYPE-SEQ-REV`` numbering and sign-off."""

    number: str
    revision: str
    title: str
    project: str
    client: str
    date: Optional[str] = None
    prepared_by: str = ""
    checked_by: str = ""
    approved_by: str = ""
    revision_history: list[RevisionRow] = Field(default_factory=list)

    @model_validator(mode="after")
    def _check_document(self) -> "DocumentMeta":
        for name in ("number", "revision", "title", "project", "client"):
            setattr(self, name, _non_empty(getattr(self, name), f"document.{name}"))
        if not DOC_NUMBER_RE.match(self.number):
            raise ValueError(
                "document.number must match JOB-DOCTYPE-SEQ-REV "
                f"(e.g. B0000-RPT-001-00): got {self.number!r}"
            )
        if not self.number.endswith(f"-{self.revision}"):
            raise ValueError(
                f"document number {self.number!r} revision suffix must match "
                f"document.revision {self.revision!r}"
            )
        if not self.revision_history:
            self.revision_history = [
                RevisionRow(
                    rev=self.revision,
                    date=self.date or "",
                    description="Issued",
                    prepared=self.prepared_by,
                    checked=self.checked_by,
                    approved=self.approved_by,
                )
            ]
        return self


class StandardLabel(_Strict):
    """A code or standard the report is checked against, with its edition."""

    code_id: str
    edition: str
    provenance: str = Field(
        description="Where the edition was taken from, e.g. a wiki path or "
        "'licensed copy'."
    )

    @field_validator("code_id", "edition", "provenance")
    @classmethod
    def _strip(cls, value: str) -> str:
        return _non_empty(value, "StandardLabel field")


# ---------------------------------------------------------------------------
# Blocks
# ---------------------------------------------------------------------------


class TextBlock(_Strict):
    """Prose, in a small Markdown subset (paragraphs, headings, lists, code)."""

    kind: Literal["text"] = "text"
    markdown: str


class TableBlock(_Strict):
    """A results table; ``units`` (when given) is one entry per column."""

    kind: Literal["table"] = "table"
    title: str
    columns: list[str]
    units: Optional[list[str]] = None
    rows: list[list[Any]]
    source: Optional[str] = None

    @model_validator(mode="after")
    def _check_shape(self) -> "TableBlock":
        if not self.columns:
            raise ValueError("TableBlock.columns must not be empty")
        if self.units is not None and len(self.units) != len(self.columns):
            raise ValueError(
                f"TableBlock {self.title!r}: units has {len(self.units)} entries "
                f"for {len(self.columns)} columns"
            )
        for index, row in enumerate(self.rows):
            if len(row) != len(self.columns):
                raise ValueError(
                    f"TableBlock {self.title!r}: row {index} has {len(row)} "
                    f"cells for {len(self.columns)} columns"
                )
        return self


class FigureBlock(_Strict):
    """A figure: an interactive Plotly figure dict, inline SVG, or an image."""

    kind: Literal["figure"] = "figure"
    title: str
    caption: str = ""
    plotly: Optional[dict[str, Any]] = None
    svg: Optional[str] = None
    image_path: Optional[str] = None
    figure_id: str

    @field_validator("figure_id")
    @classmethod
    def _check_id(cls, value: str) -> str:
        if not _HTML_ID_RE.match(value):
            raise ValueError(
                f"figure_id must be an HTML id (letters, digits, '-', '_'): "
                f"got {value!r}"
            )
        return value

    @model_validator(mode="after")
    def _check_payload(self) -> "FigureBlock":
        if self.plotly is None and self.svg is None and self.image_path is None:
            raise ValueError(
                f"FigureBlock {self.title!r} needs one of plotly, svg or image_path"
            )
        return self


class StatusBlock(_Strict):
    """A PASS/FAIL adequacy line; a FAIL must name its governing case."""

    kind: Literal["status"] = "status"
    label: str
    status: Literal["PASS", "FAIL"]
    governing_case: Optional[str] = None
    detail: str = ""

    @model_validator(mode="after")
    def _fail_needs_case(self) -> "StatusBlock":
        if self.status == "FAIL" and not (self.governing_case or "").strip():
            raise ValueError(
                f"StatusBlock {self.label!r}: FAIL requires governing_case"
            )
        return self


Block = Annotated[
    Union[TextBlock, TableBlock, FigureBlock, StatusBlock],
    Field(discriminator="kind"),
]


class Section(_Strict):
    """A titled, keyed section (or lettered appendix) of typed blocks."""

    key: str
    title: str
    subtitle: str = ""
    blocks: list[Block]

    @field_validator("key")
    @classmethod
    def _check_key(cls, value: str) -> str:
        if not _HTML_ID_RE.match(value):
            raise ValueError(
                f"Section.key must be an HTML id (letters, digits, '-', '_'): "
                f"got {value!r}"
            )
        return value

    @field_validator("blocks")
    @classmethod
    def _non_empty_blocks(cls, value: list[Any]) -> list[Any]:
        if not value:
            raise ValueError("Section.blocks must not be empty")
        return value


# ---------------------------------------------------------------------------
# The spec
# ---------------------------------------------------------------------------


def _coerce_citation(item: Any, index: int) -> dict[str, Any]:
    if isinstance(item, Citation):
        return asdict(item)
    if isinstance(item, Mapping):
        try:
            return asdict(Citation(**dict(item)))
        except (TypeError, CitationValidationError) as exc:
            raise ValueError(f"citations[{index}] invalid: {exc}") from exc
    raise ValueError(
        f"citations[{index}] must be a Citation or a mapping, got "
        f"{type(item).__name__}"
    )


class ReportSpec(BaseModel):
    """Everything the engine needs to render one report.

    ``citations`` accepts :class:`~digitalmodel.citations.schema.Citation`
    dataclasses (or mappings with the same fields) and stores them as plain
    dicts so the spec serialises to JSON unchanged. ``provenance`` must declare
    at least one source by render time (:meth:`Provenance.require`).
    ``manifest`` is validated against :data:`MANIFEST_REQUIRED_FIELDS` when
    supplied; an empty mapping means "no report-layer manifest".
    """

    model_config = ConfigDict(extra="forbid", arbitrary_types_allowed=True)

    document: DocumentMeta
    standards: list[StandardLabel] = Field(default_factory=list)
    citations: list[dict[str, Any]] = Field(default_factory=list)
    sections: list[Section]
    provenance: Provenance = Field(default_factory=Provenance)
    manifest: dict[str, Any] = Field(default_factory=dict)
    input_echo: dict[str, Any] = Field(default_factory=dict)
    appendices: list[Section] = Field(default_factory=list)
    tool_version: str = Field(default_factory=_tool_version)

    @field_validator("citations", mode="before")
    @classmethod
    def _coerce_citations(cls, value: Any) -> list[dict[str, Any]]:
        if value is None:
            return []
        if not isinstance(value, (list, tuple)):
            raise ValueError("citations must be a list")
        return [_coerce_citation(item, i) for i, item in enumerate(value)]

    @field_validator("manifest")
    @classmethod
    def _check_manifest(cls, value: dict[str, Any]) -> dict[str, Any]:
        if not value:
            return value
        missing = [f for f in MANIFEST_REQUIRED_FIELDS if not value.get(f)]
        if missing:
            raise ValueError(
                "manifest missing required field(s): " + ", ".join(missing)
            )
        ids = value["input_source_ids"]
        if not isinstance(ids, list) or not all(
            isinstance(i, str) and i.strip() for i in ids
        ):
            raise ValueError(
                "manifest.input_source_ids must be a list of non-empty strings"
            )
        return value

    @model_validator(mode="after")
    def _check_unique_ids(self) -> "ReportSpec":
        if not self.sections:
            raise ValueError("ReportSpec.sections must not be empty")
        keys: list[str] = []
        figure_ids: list[str] = []
        for section in [*self.sections, *self.appendices]:
            keys.append(section.key)
            for block in section.blocks:
                if isinstance(block, FigureBlock):
                    figure_ids.append(block.figure_id)
        for name, values in (("section keys", keys), ("figure_ids", figure_ids)):
            dupes = sorted({v for v in values if values.count(v) > 1})
            if dupes:
                raise ValueError(f"duplicate {name}: {', '.join(dupes)}")
        return self

    def figure_blocks(self) -> list[FigureBlock]:
        """All figures in document order (sections, then appendices)."""
        return [
            block
            for section in [*self.sections, *self.appendices]
            for block in section.blocks
            if isinstance(block, FigureBlock)
        ]

    def has_plotly(self) -> bool:
        """True when any figure carries a Plotly payload (inline JS needed)."""
        return any(block.plotly is not None for block in self.figure_blocks())


__all__ = [
    "DOC_NUMBER_RE",
    "MANIFEST_REQUIRED_FIELDS",
    "Block",
    "DocumentMeta",
    "FigureBlock",
    "ReportSpec",
    "RevisionRow",
    "Section",
    "StandardLabel",
    "StatusBlock",
    "TableBlock",
    "TextBlock",
]
