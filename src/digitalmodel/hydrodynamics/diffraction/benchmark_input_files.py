"""Benchmark solver input file viewer HTML builder.

ABOUTME: build_input_files_html — scrollable input file previews for each
solver. Split from benchmark_input_reports.py (WRK-593 God Object split).

Depends on benchmark_input_comparison.build_semantic_equivalence_html and
benchmark_helpers._FILE_DESCRIPTIONS.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Optional

from loguru import logger

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    _FILE_DESCRIPTIONS,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_input_comparison import (
    build_semantic_equivalence_html,
)


def build_input_files_html(
    solver_names: List[str],
    solver_metadata: Dict[str, Dict[str, Any]],
) -> str:
    """Render scrollable input file previews for each solver."""
    max_lines = 2000
    file_entries: List[tuple] = []

    for solver in solver_names:
        meta = solver_metadata.get(solver, {})
        input_file = meta.get("input_file")
        if not input_file:
            continue

        file_path = Path(input_file)
        if not file_path.exists():
            logger.warning(
                f"Input file for solver '{solver}' not found: {input_file}"
            )
            continue

        content: Optional[str] = None
        for encoding in ("utf-8", "latin-1"):
            try:
                content = file_path.read_text(encoding=encoding)
                break
            except (UnicodeDecodeError, OSError):
                continue

        if content is None:
            logger.warning(
                f"Could not read input file for solver '{solver}': "
                f"{input_file}"
            )
            continue

        lines = content.splitlines()
        truncated = len(lines) > max_lines
        if truncated:
            lines = lines[:max_lines]
        content = "\n".join(lines)

        file_entries.append(
            (solver, str(file_path), content, truncated, len(lines))
        )

    if not file_entries:
        return ""

    semantic_html = build_semantic_equivalence_html(
        solver_names, solver_metadata,
    )

    parts: List[str] = ["<h2>Input Files</h2>"]
    if semantic_html:
        parts.append(semantic_html)

    for idx, (solver, path_str, content, truncated, n_lines) in enumerate(
        file_entries
    ):
        safe_solver = html_mod.escape(solver)
        safe_path = html_mod.escape(path_str)
        safe_content = html_mod.escape(content)
        textarea_id = f"file_content_{idx}"

        line_spans: List[str] = []
        for line in content.splitlines():
            line_spans.append(
                f'<span class="line">{html_mod.escape(line)}</span>'
            )
        numbered_content = "\n".join(line_spans)

        truncation_note = ""
        if truncated:
            truncation_note = (
                f'<div style="padding:0.4em 1em;background:#fef9e7;'
                f'border:1px solid #ddd;border-top:none;font-size:0.8em;'
                f'color:#888;font-style:italic;">'
                f"Showing first {n_lines} lines (file truncated)"
                f"</div>"
            )

        description = _FILE_DESCRIPTIONS.get(solver, "")
        desc_html = ""
        if description:
            desc_html = (
                f'<p style="margin:0.3em 0 0.5em;font-size:12px;'
                f'color:#64748b;max-width:700px;">'
                f'{html_mod.escape(description)}</p>'
            )

        parts.append(f"""\
<h3 style="margin-top:1.5em;margin-bottom:0.2em;">{safe_solver}</h3>
{desc_html}
<div class="file-viewer">
  <div class="file-viewer-header">
    <div>
      <span class="solver-label">{safe_solver}</span>
      <span class="file-path">{safe_path}</span>
    </div>
    <button onclick="openFileWindow_{idx}()">Open in New Window</button>
  </div>
  <div class="file-content">
    <pre>{numbered_content}</pre>
  </div>
  {truncation_note}
  <textarea id="{textarea_id}" style="display:none;">{safe_content}</textarea>
  <script>
    function openFileWindow_{idx}() {{
      var ta = document.getElementById('{textarea_id}');
      var w = window.open('', '_blank');
      w.document.write(
        '<html><head><title>{safe_solver} - {safe_path}</title>' +
        '<style>body{{font-family:"SF Mono","Cascadia Code","Consolas",' +
        'monospace;white-space:pre;margin:1em;font-size:13px;' +
        'line-height:1.5;tab-size:4;}}</style></head><body>' +
        ta.value.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;') +
        '</body></html>'
      );
      w.document.close();
    }}
  </script>
</div>""")

    return "\n".join(parts)
