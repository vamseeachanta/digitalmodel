"""Benchmark pairwise correlation, per-DOF report sections, and data tables.

ABOUTME: Module-level functions for pairwise solver correlation heatmaps,
per-DOF report sections with inline plots + commentary, hydrodynamic
coefficient matrices, and raw RAO data tables.
Extracted from BenchmarkPlotter as part of WRK-592 God Object split.

Per-DOF table/section functions moved to benchmark_dof_tables and
benchmark_dof_sections as part of WRK-593. Re-exported here for backward compat.
"""
from __future__ import annotations

import html as html_mod
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    DOF_ORDER,
    _AMPLITUDE_UNITS,
    _is_phase_at_negligible_amplitude,
    generate_dof_observations,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_rao_plots import (
    add_solver_traces,
    apply_layout,
    get_heading_indices,
    get_significant_heading_indices,
    get_x_values,
    get_solver_style,
    save_figure,
    x_axis_label,
)
from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    rad_per_s_to_period_s,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    RAOComponent,
)

# --- backward-compat re-exports for functions moved to sub-modules ---
from digitalmodel.hydrodynamics.diffraction.benchmark_dof_tables import (  # noqa: F401
    _compute_dof_amplitude_rows,
    _compute_dof_phase_rows,
    _build_solver_column_table,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_dof_sections import (  # noqa: F401
    _add_phase_annotations,
    build_dof_report_sections,
)


# ------------------------------------------------------------------
# Pairwise correlation heatmap
# ------------------------------------------------------------------

def plot_pairwise_correlation_heatmap(
    report: BenchmarkReport,
    output_dir: Path,
) -> Path:
    """Heatmap of mean pairwise magnitude correlation across all DOFs."""
    n = len(report.solver_names)
    matrix = np.ones((n, n), dtype=float)

    name_to_idx = {
        name: idx for idx, name in enumerate(report.solver_names)
    }

    for pair_key, pw_result in report.pairwise_results.items():
        rao_comps = pw_result.rao_comparisons
        corrs = [
            c.magnitude_stats.correlation for c in rao_comps.values()
        ]
        mean_corr = float(np.mean(corrs))
        i = name_to_idx[pw_result.solver_a]
        j = name_to_idx[pw_result.solver_b]
        matrix[i, j] = mean_corr
        matrix[j, i] = mean_corr

    fig = go.Figure(
        data=go.Heatmap(
            z=matrix,
            x=report.solver_names,
            y=report.solver_names,
            colorscale="RdYlGn",
            zmin=0.0,
            zmax=1.0,
            text=np.round(matrix, 3),
            texttemplate="%{text:.3f}",
        )
    )
    apply_layout(fig, "Pairwise Solver Correlation")
    fig.update_layout(height=500)
    return save_figure(fig, "benchmark_heatmap", output_dir)


# ------------------------------------------------------------------
# 6x6 coefficient matrix rendering
# ------------------------------------------------------------------

def render_6x6_matrix(
    corr_dict: dict,
    labels: List[str],
) -> str:
    """Render a 6x6 correlation matrix as an HTML table."""
    rows: List[str] = ['<table class="solver-table" '
                       'style="width:auto;max-width:600px;">']
    rows.append("<tr><th></th>")
    for label in labels:
        rows.append(f"<th>{label[:2]}</th>")
    rows.append("</tr>")

    for i, row_label in enumerate(labels, start=1):
        rows.append(f"<tr><td style='font-weight:600;text-align:left;'>"
                    f"{row_label}</td>")
        for j in range(1, 7):
            val = corr_dict.get((i, j), corr_dict.get(f"{i},{j}", None))
            if val is None:
                rows.append("<td>-</td>")
                continue
            if val >= 0.999:
                bg = "#d5f5e3"
            elif val >= 0.99:
                bg = "#fef9e7"
            else:
                bg = "#fadbd8"
            rows.append(
                f'<td style="background:{bg};text-align:center;">'
                f"{val:.6f}</td>"
            )
        rows.append("</tr>")
    rows.append("</table>")
    return "\n".join(rows)


def build_hydro_coefficients_html(
    report: BenchmarkReport,
) -> str:
    """Build 6x6 added-mass and damping correlation matrices."""
    if not report.pairwise_results:
        return ""

    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    parts: List[str] = [
        "<h2>Hydrodynamic Coefficients</h2>"
        "<p>Pairwise correlation of frequency-dependent added-mass "
        "and radiation-damping matrices (6&times;6 DOF). Values "
        "near 1.000 indicate identical coefficients.</p>",
    ]

    for pair_key, pair_result in report.pairwise_results.items():
        parts.append(
            f'<h3 style="font-size:0.95em;color:#555;">'
            f"{html_mod.escape(pair_key)}</h3>"
        )
        for matrix_name, corr_dict in [
            ("Added Mass", pair_result.added_mass_correlations),
            ("Radiation Damping", pair_result.damping_correlations),
        ]:
            parts.append(
                f'<h4 style="margin:0.8em 0 0.3em;">{matrix_name} '
                f"Correlation</h4>"
            )
            parts.append(render_6x6_matrix(corr_dict, dof_labels))

    return "\n".join(parts)


def build_coupling_heatmap_html(
    output_dir: Path,
    am_corr: list[list[float]],
    damp_corr: list[list[float]],
    body_i_name: str,
    body_j_name: str,
) -> Path:
    """Render 6x6 correlation heatmaps for coupling matrices."""
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]

    def _to_dict(matrix):
        d = {}
        for i in range(6):
            for j in range(6):
                d[(i + 1, j + 1)] = matrix[i][j]
        return d

    am_dict = _to_dict(am_corr)
    damp_dict = _to_dict(damp_corr)

    am_table = render_6x6_matrix(am_dict, dof_labels)
    damp_table = render_6x6_matrix(damp_dict, dof_labels)

    title = f"Coupling: {body_i_name} \u2194 {body_j_name}"
    safe_i = "".join(c for c in body_i_name if c.isalnum() or c in "_-")
    safe_j = "".join(c for c in body_j_name if c.isalnum() or c in "_-")
    filename = f"coupling_{safe_i}_{safe_j}".lower()

    page = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<title>{html_mod.escape(title)}</title>
<style>
  body {{ margin:20px; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; }}
  h1 {{ font-size: 1.5em; margin-bottom: 0.5em; }}
  h2 {{ font-size: 1.2em; margin-top: 1.5em; color: #555; }}
  .container {{ max-width: 800px; margin: 0 auto; }}
  table {{ border-collapse: collapse; width: 100%; max-width: 600px; margin-bottom: 20px; }}
  th, td {{ border: 1px solid #ddd; padding: 8px; text-align: center; font-size: 0.9em; }}
  th {{ background-color: #f2f2f2; }}
  .solver-table td {{ padding: 6px; }}
</style>
</head>
<body>
<div class="container">
  <h1>{html_mod.escape(title)}</h1>
  <p>Correlation of frequency-dependent coupling coefficients between .owd and spec.yml results.
     Values near 1.000 indicate identical coefficients.</p>

  <h2>Added Mass Coupling</h2>
  {am_table}

  <h2>Radiation Damping Coupling</h2>
  {damp_table}
</div>
</body>
</html>"""

    path = output_dir / f"{filename}.html"
    path.write_text(page, encoding="utf-8")
    return path


# ------------------------------------------------------------------
# Raw RAO data tables
# ------------------------------------------------------------------

def build_raw_rao_data_html(
    solver_results: Dict[str, Any],
    solver_names: List[str],
    headings: Optional[List[float]] = None,
) -> str:
    """Build collapsible tables of raw RAO magnitude+phase per DOF."""
    parts: List[str] = [
        "<h2>Raw RAO Data</h2>"
        "<p>Full frequency-by-heading magnitude and phase values "
        "for each DOF. Expand each section to inspect.</p>",
    ]

    for dof in DOF_ORDER:
        dof_name = dof.name.lower()
        dof_cap = dof.name.capitalize()
        unit = _AMPLITUDE_UNITS[dof]

        first_comp: RAOComponent = getattr(
            solver_results[solver_names[0]].raos, dof_name,
        )
        h_indices = get_heading_indices(first_comp, headings)
        periods = first_comp.frequencies.periods

        parts.append(
            f"<details><summary><strong>{dof_cap}</strong> "
            f"({len(periods)} freq &times; {len(h_indices)} hdg)"
            f"</summary>"
        )

        tbl: List[str] = ['<div style="overflow-x:auto;">'
                          '<table class="solver-table">']
        tbl.append("<tr><th>T (s)</th>")
        for hi in h_indices:
            hdg_val = first_comp.headings.values[hi]
            for solver in solver_names:
                short = (
                    solver.split("(")[-1].rstrip(")")
                    if "(" in solver else solver
                )
                tbl.append(
                    f"<th>{hdg_val:.0f}&deg; {short}<br>"
                    f"Mag ({unit})</th>"
                    f"<th>{hdg_val:.0f}&deg; {short}<br>"
                    f"Phase (&deg;)</th>"
                )
        tbl.append("</tr>")

        n_freq = min(len(periods), 100)
        for fi in range(n_freq):
            tbl.append(f"<tr><td>{periods[fi]:.4f}</td>")
            for hi in h_indices:
                for solver in solver_names:
                    comp: RAOComponent = getattr(
                        solver_results[solver].raos, dof_name,
                    )
                    mag_val = float(comp.magnitude[fi, hi])
                    phase_val = float(comp.phase[fi, hi])
                    tbl.append(
                        f"<td>{mag_val:.6g}</td>"
                        f"<td>{phase_val:.1f}</td>"
                    )
            tbl.append("</tr>")

        if len(periods) > 100:
            tbl.append(
                f'<tr><td colspan="99" style="text-align:center;'
                f'font-style:italic;">... truncated at 100 of '
                f'{len(periods)} frequencies</td></tr>'
            )

        tbl.append("</table></div>")
        parts.append("\n".join(tbl))
        parts.append("</details>")

    return "\n".join(parts)
