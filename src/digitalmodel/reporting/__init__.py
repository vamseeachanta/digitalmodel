#!/usr/bin/env python3
"""digitalmodel.reporting — reusable report block library.

ABOUTME: A small, domain-agnostic toolkit for assembling self-contained HTML
reports from ordered, keyed section builders. Extracted from the diffraction
report generator (#1018) so other modules can reuse the same pattern.

Public API:
  * ``ReportDataModel``  — marker base for report envelope (pydantic) models.
  * ``ReportBlock``      — runtime-checkable protocol for block-shaped objects.
  * ``ReportBlockFn``    — ``Callable[..., str]`` alias (builders are functions).
  * ``SectionMode``      — ALWAYS / COMPACT_SKIP / BENCHMARK_ONLY.
  * ``ReportSection``    — a keyed builder with a render mode.
  * ``ReportBackbone``   — ordered sections -> list[str] of section HTML.
  * ``ReportRenderer``   — wrap sections in an HTML document + write to disk.
"""

from __future__ import annotations

from digitalmodel.reporting._backbone import ReportBackbone, ReportSection, SectionMode
from digitalmodel.reporting._base import ReportBlock, ReportBlockFn, ReportDataModel
from digitalmodel.reporting._renderer import ReportRenderer
from digitalmodel.reporting.provenance import (
    DataSource,
    Provenance,
    ProvenanceError,
    assemble_report,
    assumption_ledger_block,
    provenance_block,
)
from digitalmodel.reporting.skeleton import (
    BlockSpec,
    Completeness,
    ReportSkeleton,
    SectionSpec,
)

__all__ = [
    "ReportDataModel",
    "ReportBlock",
    "ReportBlockFn",
    "SectionMode",
    "ReportSection",
    "ReportBackbone",
    "ReportRenderer",
    # provenance / SSOT-view (#1019)
    "ProvenanceError",
    "DataSource",
    "Provenance",
    "provenance_block",
    "assumption_ledger_block",
    "assemble_report",
    # skeleton-first (#1021)
    "ReportSkeleton",
    "SectionSpec",
    "BlockSpec",
    "Completeness",
]
