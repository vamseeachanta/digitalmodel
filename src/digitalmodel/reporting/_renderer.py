#!/usr/bin/env python3
"""Reporting block library — HTML document renderer.

ABOUTME: Wraps an ordered list of section HTML strings in a parameterized,
self-contained HTML document and writes it to disk. With the diffraction
report's exact CSS/head pieces it reproduces that document byte-for-byte
(#1018). PDF rendering is intentionally out of scope.
"""

from __future__ import annotations

from pathlib import Path
from typing import List, Optional


class ReportRenderer:
    """Assemble section HTML into a standalone HTML document and write it."""

    def render_html(
        self,
        sections: List[str],
        *,
        title: str,
        head_extra: str = "",
        css: str = "",
        plotlyjs_src: str = "",
        container_class: str = "container",
        container_max_width: Optional[str] = None,
    ) -> str:
        """Wrap ``sections`` in a complete ``<!DOCTYPE html>`` document.

        The template is intentionally parameterized so callers can reproduce
        an existing document wrapper exactly:

          * ``title`` populates ``<title>``.
          * ``plotlyjs_src`` is emitted on its own line in ``<head>`` (pass an
            empty string for no script — the line is still emitted, matching
            the diffraction wrapper).
          * ``head_extra`` is inserted immediately before ``<style>``.
          * ``css`` is placed verbatim between ``<style>`` and ``</style>``.
          * ``container_class`` / ``container_max_width`` control the wrapping
            ``<div>`` (an inline ``max-width`` is only added when provided).
        """
        body_inner = "".join(sections)
        container_style = (
            f' style="max-width: {container_max_width};"'
            if container_max_width
            else ""
        )
        return (
            "<!DOCTYPE html>\n"
            '<html lang="en">\n'
            "<head>\n"
            '<meta charset="utf-8">\n'
            f"<title>{title}</title>\n"
            f"{plotlyjs_src}\n"
            f"{head_extra}<style>\n"
            f"{css}</style>\n"
            "</head>\n"
            "<body>\n"
            f'<div class="{container_class}"{container_style}>\n'
            f"{body_inner}\n"
            "</div>\n"
            "</body>\n"
            "</html>"
        )

    def write_html(self, html: str, output_path: Path) -> Path:
        """Write ``html`` to ``output_path`` (utf-8), creating parent dirs."""
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(html, encoding="utf-8")
        return output_path
