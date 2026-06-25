#!/usr/bin/env python3
"""Reporting block library — section backbone.

ABOUTME: Declarative, ordered list of report sections with mode/benchmark
rules. Generalized from the diffraction report orchestration (#1018) so that
``render`` reproduces the diffraction section-ordering behaviour exactly while
staying domain-agnostic.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, Dict, List, Optional


class SectionMode(Enum):
    """How a section participates in rendering."""

    ALWAYS = "always"
    # Skipped when ``mode == "compact"``.
    COMPACT_SKIP = "compact_skip"
    # Rendered only when this section's benchmark fragment is present.
    BENCHMARK_ONLY = "benchmark_only"


@dataclass
class ReportSection:
    """One ordered section: a keyed builder plus its render mode.

    ``builder`` is any ``Callable[..., str]`` (a plain function or a callable
    block). It is invoked as ``builder(data, mode=..., benchmark_sections=...,
    **builder_kwargs)`` and is expected to return an HTML fragment (possibly
    the empty string, which is omitted from the output).
    """

    key: str
    label: str
    mode: SectionMode
    builder: Callable[..., str]


@dataclass
class ReportBackbone:
    """Ordered collection of report sections.

    ``render`` produces the list of section HTML strings, applying the
    per-section mode rules and — mirroring the diffraction orchestrator —
    injecting any benchmark fragment whose key matches a rendered section.
    """

    title: str
    sections: List[ReportSection]

    def render(
        self,
        data: Any,
        *,
        mode: str = "full",
        benchmark_sections: Optional[Dict[str, str]] = None,
        **builder_kwargs: Any,
    ) -> List[str]:
        """Return the ordered list of non-empty section HTML strings.

        Rules (mirroring the diffraction report generator):
          * ``COMPACT_SKIP`` sections are dropped when ``mode == "compact"``.
          * ``BENCHMARK_ONLY`` sections render only when a truthy benchmark
            fragment exists for their ``key`` (regardless of compact mode).
          * After any non-benchmark section whose ``key`` has a truthy
            benchmark fragment, that fragment is appended wrapped in
            ``<div class="section">...</div>``.
        """
        bm = benchmark_sections or {}
        out: List[str] = []

        for section in self.sections:
            if section.mode is SectionMode.COMPACT_SKIP and mode == "compact":
                continue

            if section.mode is SectionMode.BENCHMARK_ONLY:
                # Render only when this section's benchmark fragment exists.
                if not bm.get(section.key):
                    continue
                html = section.builder(
                    data, mode=mode, benchmark_sections=bm, **builder_kwargs
                )
                if html:
                    out.append(html)
                continue

            # ALWAYS, or COMPACT_SKIP while not in compact mode.
            html = section.builder(
                data, mode=mode, benchmark_sections=bm, **builder_kwargs
            )
            if html:
                out.append(html)

            # Benchmark injection: after a rendered section whose key has a
            # fragment, append that fragment as a standalone section.
            frag = bm.get(section.key)
            if frag:
                out.append(f'<div class="section">{frag}</div>')

        return out
