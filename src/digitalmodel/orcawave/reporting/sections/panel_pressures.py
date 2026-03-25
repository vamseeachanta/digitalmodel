"""Section 5: Panel geometry summary and pressure output status."""
from __future__ import annotations

import numpy as np

from ..config import PanelPressuresConfig


def build_panel_pressures(diff, config: PanelPressuresConfig) -> str:
    """Build HTML for panel pressures section."""
    try:
        areas, bodies = [], set()
        for panel in diff.panelGeometry:
            areas.append(float(panel["area"]))
            obj = panel.get("objectName", "")
            if obj:
                bodies.add(obj)
    except Exception as exc:
        return f"<div class='alert alert-warning'>Panel geometry unavailable: {exc}</div>"

    arr = np.array(areas) if areas else np.array([0.0])
    bodies_str = ", ".join(sorted(bodies)) if bodies else "—"
    rows_data = [
        ("Total panels", str(len(areas))),
        ("Total wetted area (m²)", f"{arr.sum():.2f}"),
        ("Panel area — min (m²)", f"{arr.min():.4g}"),
        ("Panel area — max (m²)", f"{arr.max():.4g}"),
        ("Panel area — mean (m²)", f"{arr.mean():.4g}"),
        ("Bodies in mesh", bodies_str),
    ]
    tr_rows = "".join(
        f"<tr><th scope='row'>{lbl}</th><td>{val}</td></tr>"
        for lbl, val in rows_data
    )
    table_html = (
        "<div class='table-responsive' style='max-width:600px'>"
        "<table class='table table-sm table-bordered'>"
        "<thead class='table-light'><tr><th>Property</th><th>Value</th></tr></thead>"
        f"<tbody>{tr_rows}</tbody></table></div>"
    )
    pressure_note = _pressure_note(diff)
    return (
        "<div class='card mb-4'>"
        "<div class='card-header'><strong>Panel Geometry Summary</strong></div>"
        f"<div class='card-body'>{table_html}{pressure_note}</div></div>"
    )


def _pressure_note(diff) -> str:
    try:
        available = bool(getattr(diff, "OutputPanelPressures", False))
    except Exception:
        available = False
    if available:
        return (
            "<p class='mt-2'><span class='badge bg-success'>Panel pressures: enabled</span>"
            " Access via <code>diff.PanelPressureResults</code>.</p>"
        )
    return (
        "<p class='mt-2'><span class='badge bg-secondary'>Panel pressures: not output</span>"
        " Re-run with <em>Output panel pressures</em> enabled.</p>"
    )
