#!/usr/bin/env python3
"""Skeleton-first reporting (#1021).

ABOUTME: Instantiate a report *backbone* from a declarative skeleton at
engagement kickoff, so analysis **fills slots** instead of being written up
last. The skeleton (ordered sections, required blocks, acceptance criteria) is
the work breakdown — every analysis run targets a slot, and
:meth:`ReportSkeleton.completeness` says when the report is "complete" (all
required blocks filled + provenance present per #1019). There is never a
"now we write the report" phase.

Drives the shared backbone (#1018) + provenance (#1019): a skeleton renders to
HTML with empty slots shown as pending placeholders, and once content is
supplied for every required block (and a data source is declared) the report is
complete.
"""

from __future__ import annotations

import html
from pathlib import Path
from typing import Any, Dict, List, Optional

from pydantic import Field

from digitalmodel.reporting._backbone import ReportBackbone, ReportSection, SectionMode
from digitalmodel.reporting._base import ReportDataModel
from digitalmodel.reporting._renderer import ReportRenderer
from digitalmodel.reporting.provenance import Provenance, provenance_block

_MODE_BY_NAME = {m.value: m for m in SectionMode}


class BlockSpec(ReportDataModel):
    """One slot in a section the analysis must fill."""

    key: str
    label: str = ""
    required: bool = True
    description: str = ""


class SectionSpec(ReportDataModel):
    """One ordered section: an ordered set of block slots."""

    key: str
    label: str
    mode: str = SectionMode.ALWAYS.value
    blocks: List[BlockSpec] = Field(default_factory=list)

    def section_mode(self) -> SectionMode:
        try:
            return _MODE_BY_NAME[self.mode]
        except KeyError as exc:  # pragma: no cover - guarded by validation
            raise ValueError(
                f"Unknown section mode {self.mode!r}; "
                f"valid: {sorted(_MODE_BY_NAME)}"
            ) from exc


class Completeness(ReportDataModel):
    """Acceptance status of a skeleton against supplied content."""

    complete: bool
    required_total: int
    required_filled: int
    missing_blocks: List[str] = Field(default_factory=list)
    provenance_ok: bool = True

    def summary(self) -> str:
        status = "COMPLETE" if self.complete else "INCOMPLETE"
        prov = "ok" if self.provenance_ok else "MISSING"
        return (
            f"{status}: {self.required_filled}/{self.required_total} required "
            f"blocks filled, provenance {prov}"
            + (
                ""
                if not self.missing_blocks
                else " | missing: " + ", ".join(self.missing_blocks)
            )
        )


class ReportSkeleton(ReportDataModel):
    """A declarative report backbone created before the analysis runs."""

    title: str
    sections: List[SectionSpec]
    require_provenance: bool = True

    @classmethod
    def from_yaml(cls, path: str | Path) -> "ReportSkeleton":
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f) or {}
        return cls.model_validate(data)

    def required_block_keys(self) -> List[str]:
        return [b.key for s in self.sections for b in s.blocks if b.required]

    # -- rendering ----------------------------------------------------------

    def _section_builder(self, spec: SectionSpec):
        def build(content: Dict[str, str], **_: Any) -> str:
            parts = [
                f'<div class="section" id="{html.escape(spec.key)}">'
                f"<h2>{html.escape(spec.label)}</h2>"
            ]
            for block in spec.blocks:
                filled = (content or {}).get(block.key)
                if filled:
                    parts.append(
                        f'<div class="block" id="{html.escape(block.key)}">'
                        f"{filled}</div>"
                    )
                else:
                    tag = "required" if block.required else "optional"
                    label = html.escape(block.label or block.key)
                    note = (
                        f" &mdash; {html.escape(block.description)}"
                        if block.description
                        else ""
                    )
                    parts.append(
                        f'<div class="block pending {tag}" '
                        f'id="{html.escape(block.key)}">'
                        f"<h3>{label}</h3>"
                        f"<p><em>⏳ slot pending ({tag}){note}</em></p></div>"
                    )
            parts.append("</div>")
            return "".join(parts)

        return build

    def to_backbone(self) -> ReportBackbone:
        """Build a ReportBackbone whose sections render filled-or-pending slots."""
        return ReportBackbone(
            title=self.title,
            sections=[
                ReportSection(
                    s.key, s.label, s.section_mode(), self._section_builder(s)
                )
                for s in self.sections
            ],
        )

    def build_html(
        self,
        content: Optional[Dict[str, str]] = None,
        *,
        provenance: Optional[Provenance] = None,
        mode: str = "full",
        css: str = "",
        output_path: Optional[str | Path] = None,
    ) -> str | Path:
        """Render the skeleton (filled slots + pending placeholders) to HTML.

        Unlike :func:`provenance.assemble_report`, a skeleton may render before
        provenance exists (it is a kickoff draft); when ``provenance`` is given
        the provenance block is appended. Completeness is reported separately by
        :meth:`completeness`.
        """
        sections_html = self.to_backbone().render(content or {}, mode=mode)
        if provenance is not None and provenance.sources:
            sections_html.append(provenance_block(provenance))
        renderer = ReportRenderer()
        doc = renderer.render_html(sections_html, title=self.title, css=css)
        if output_path is not None:
            return renderer.write_html(doc, Path(output_path))
        return doc

    # -- acceptance ---------------------------------------------------------

    def completeness(
        self,
        content: Optional[Dict[str, str]] = None,
        *,
        provenance: Optional[Provenance] = None,
    ) -> Completeness:
        """Report whether every required block is filled and provenance exists."""
        content = content or {}
        required = self.required_block_keys()
        missing = [k for k in required if not content.get(k)]
        provenance_ok = (not self.require_provenance) or bool(
            provenance and provenance.sources
        )
        return Completeness(
            complete=(not missing) and provenance_ok,
            required_total=len(required),
            required_filled=len(required) - len(missing),
            missing_blocks=missing,
            provenance_ok=provenance_ok,
        )


__all__ = [
    "BlockSpec",
    "SectionSpec",
    "Completeness",
    "ReportSkeleton",
]
