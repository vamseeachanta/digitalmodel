"""Mechanism-specific offline sections for FFSReport (no inferred metal-loss data)."""
from __future__ import annotations

import html
import math
from datetime import datetime, timezone
from pathlib import Path


def finite_number(block: dict, key: str, *, default=None, positive=True) -> float:
    """Read an explicit, finite engineering scalar; reject booleans and unknowns."""
    value = block.get(key, default)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{key} must be a finite number")
    value = float(value)
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(f"{key} must be {'positive' if positive else 'nonnegative'} and finite")
    return value


def generate_screening_html(component_id, title, decision, sections, limitations):
    """Render calculated metrics without borrowing unrelated Part 4/5 criteria."""
    from .ffs_report import FFSReport

    def escape(value):
        return html.escape("not evaluated" if value is None else str(value))
    parts = [FFSReport._html_head(str(component_id), datetime.now(timezone.utc).date().isoformat()),
             f"<h2>{escape(title)}</h2>",
             f"<p>Screening verdict: <strong>{escape(decision['verdict'])}</strong></p>",
             f"<p>{escape(decision['governing_criterion'])}</p>",
             "<p>Published-case validation: not evaluated. Formula checks only.</p>"]
    for heading, metrics in sections.items():
        rows = ''.join(f"<tr><td>{escape(k)}</td><td>{escape(v)}</td></tr>"
                       for k, v in metrics.items())
        parts.append(f"<h3>{escape(heading)}</h3><table>"
                     f"<tr><th>Quantity (units in name)</th><th>Value</th></tr>{rows}</table>")
    items = ''.join(f"<li>{escape(item)}</li>" for item in limitations)
    parts.append(f"<h3>Method and applicability limits</h3><ul>{items}</ul>"
                 "<p>Engineering screening; competent review and applicable "
                 "code qualification remain required.</p></body></html>")
    return '\n'.join(parts)


def store_report(result, block, filename, root_folder=None):
    """Persist only when an output directory is supplied; expose path for readback."""
    if block.get("output_dir"):
        output = Path(block["output_dir"])
        if root_folder is not None and not output.is_absolute():
            output = Path(root_folder) / output
        output.mkdir(parents=True, exist_ok=True)
        path = output / filename
        path.write_text(result["report_html"], encoding="utf-8")
        result["report_path"] = str(path)
    return result
