"""Benchmark RAO amplitude/phase summary computation and HTML rendering.

ABOUTME: compute_amplitude_summary, compute_phase_summary, build_summary_table,
and render_html_with_table. Split from benchmark_rao_plots.py (WRK-593).

Depends on benchmark_rao_helpers for get_heading_indices + get_x_values.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Literal, Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_helpers import (
    get_heading_indices,
    get_x_values,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    RAOComponent,
)


def compute_amplitude_summary(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]],
) -> List[Dict[str, Any]]:
    """Per-DOF amplitude summary rows for the comparison table."""
    sections: List[Dict[str, Any]] = []
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        solver_peaks: Dict[str, Dict[float, float]] = {}
        for solver in solver_names:
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            h_indices = get_heading_indices(comp, headings)
            periods = comp.frequencies.periods
            solver_peaks[solver] = {}
            for hi in h_indices:
                mag = comp.magnitude[:, hi]
                peak_idx = int(np.argmax(mag))
                heading_val = float(comp.headings.values[hi])
                solver_peaks[solver][heading_val] = float(mag[peak_idx])
                rows.append({
                    "heading": f"{heading_val:.0f}",
                    "solver": solver,
                    "peak_amp": f"{mag[peak_idx]:.4g}",
                    "peak_period": f"{periods[peak_idx]:.2f}",
                    "long_period_amp": f"{mag[0]:.4g}",
                })
        ref_solver = solver_names[0]
        for row in rows:
            h_val = float(row["heading"])
            ref_peak = solver_peaks[ref_solver].get(h_val, 0.0)
            cur_peak = solver_peaks[row["solver"]].get(h_val, 0.0)
            if ref_peak > 1e-12:
                diff = abs(cur_peak - ref_peak) / ref_peak * 100
                row["diff_pct"] = f"{diff:.1f}"
            else:
                row["diff_pct"] = "-"
        sections.append({
            "dof": dof.name.capitalize(),
            "unit": _AMPLITUDE_UNITS[dof],
            "rows": rows,
        })
    return sections


def compute_phase_summary(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]],
) -> List[Dict[str, Any]]:
    """Per-DOF phase summary rows for the comparison table."""
    sections: List[Dict[str, Any]] = []
    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        rows: List[Dict[str, Any]] = []
        solver_phase_at_peak: Dict[str, Dict[float, float]] = {}
        for solver in solver_names:
            comp: RAOComponent = getattr(
                solver_results[solver].raos, dof_name,
            )
            h_indices = get_heading_indices(comp, headings)
            solver_phase_at_peak[solver] = {}
            for hi in h_indices:
                mag = comp.magnitude[:, hi]
                phase = comp.phase[:, hi]
                peak_idx = int(np.argmax(mag))
                heading_val = float(comp.headings.values[hi])
                phase_peak = float(phase[peak_idx])
                solver_phase_at_peak[solver][heading_val] = phase_peak
                rows.append({
                    "heading": f"{heading_val:.0f}",
                    "solver": solver,
                    "phase_at_peak": f"{phase_peak:.1f}",
                    "long_period_phase": f"{float(phase[0]):.1f}",
                })
        ref_solver = solver_names[0]
        for row in rows:
            h_val = float(row["heading"])
            ref_ph = solver_phase_at_peak[ref_solver].get(h_val, 0.0)
            cur_ph = solver_phase_at_peak[row["solver"]].get(h_val, 0.0)
            row["phase_diff"] = f"{abs(cur_ph - ref_ph):.1f}"
        sections.append({
            "dof": dof.name.capitalize(),
            "rows": rows,
        })
    return sections


def build_summary_table(
    sections: List[Dict[str, Any]],
    mode: Literal["Amplitude", "Phase"],
) -> str:
    """Render summary *sections* as an HTML string of tables."""
    parts: List[str] = []
    for sec in sections:
        dof_label = html_mod.escape(sec["dof"])
        unit = html_mod.escape(sec.get("unit", ""))
        heading_text = (
            f"{dof_label} ({unit})" if unit else dof_label
        )
        parts.append(f"<h3>{heading_text}</h3>")
        parts.append("<table>")
        if mode == "Amplitude":
            parts.append(
                "<tr><th>Hdg</th><th>Solver</th>"
                "<th>Peak&nbsp;Amp</th><th>Peak&nbsp;T(s)</th>"
                "<th>LP&nbsp;Amp</th><th>Diff(%)</th></tr>"
            )
            for r in sec["rows"]:
                parts.append(
                    f"<tr><td>{r['heading']}</td>"
                    f"<td>{html_mod.escape(r['solver'])}</td>"
                    f"<td>{r['peak_amp']}</td>"
                    f"<td>{r['peak_period']}</td>"
                    f"<td>{r['long_period_amp']}</td>"
                    f"<td>{r['diff_pct']}</td></tr>"
                )
        else:
            parts.append(
                "<tr><th>Hdg</th><th>Solver</th>"
                "<th>Phase@Peak</th><th>LP&nbsp;Phase</th>"
                "<th>Diff(deg)</th></tr>"
            )
            for r in sec["rows"]:
                parts.append(
                    f"<tr><td>{r['heading']}</td>"
                    f"<td>{html_mod.escape(r['solver'])}</td>"
                    f"<td>{r['phase_at_peak']}</td>"
                    f"<td>{r['long_period_phase']}</td>"
                    f"<td>{r['phase_diff']}</td></tr>"
                )
        parts.append("</table>")
    return "\n".join(parts)


def render_html_with_table(
    fig,
    summary: List[Dict[str, Any]],
    filename: str,
    mode: Literal["Amplitude", "Phase"],
    output_dir: Path,
) -> Path:
    """Write an HTML page with Plotly plot (left) and table (right)."""
    plot_html = fig.to_html(
        full_html=False, include_plotlyjs="cdn",
    )
    table_html = build_summary_table(summary, mode)
    title = html_mod.escape(
        fig.layout.title.text if fig.layout.title.text else filename,
    )
    page = f"""\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<title>{title}</title>
<style>
  body {{ margin:0; font-family: Arial, Helvetica, sans-serif; }}
  .grid {{ display:grid; grid-template-columns:70% 30%; height:100vh; }}
  .plot {{ overflow:auto; }}
  .table-panel {{
    overflow:auto; padding:8px; background:#fafafa;
    border-left:1px solid #ddd; font-size:12px;
  }}
  .table-panel h3 {{ margin:12px 0 4px; font-size:13px; }}
  .table-panel table {{
    border-collapse:collapse; width:100%; margin-bottom:8px;
  }}
  .table-panel th, .table-panel td {{
    border:1px solid #ccc; padding:3px 5px; text-align:right;
  }}
  .table-panel th {{ background:#e8e8e8; font-weight:600; }}
  .table-panel td:first-child, .table-panel td:nth-child(2) {{
    text-align:left;
  }}
</style>
</head>
<body>
<div class="grid">
  <div class="plot">{plot_html}</div>
  <div class="table-panel">{table_html}</div>
</div>
</body>
</html>"""
    path = output_dir / f"{filename}.html"
    path.write_text(page, encoding="utf-8")
    return path
