# ABOUTME: Interactive S-N curve comparison HTML report with Plotly overlay plots
# ABOUTME: Compares curves across DNV, API, BS, AWS standards with design factor bands

"""
S-N Curve Comparison Report

Generates an interactive HTML report comparing S-N curves across standards.
Features:
- Multi-standard overlay on log-log plot (Plotly)
- Design factor bands (DFF = 1, 2, 3, 10)
- Curve parameter comparison table
- Self-contained HTML with Plotly CDN
"""

import math
import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np

from digitalmodel.structural.fatigue.sn_curves import (
    PowerLawSNCurve,
    StandardSNCurves,
)

logger = logging.getLogger(__name__)


# Color palette for standards
STANDARD_COLORS = {
    "DNV": "#1f77b4",
    "API": "#ff7f0e",
    "BS": "#2ca02c",
    "AWS": "#d62728",
}

# Dash patterns per curve class rank
DASH_PATTERNS = ["solid", "dash", "dot", "dashdot", "longdash", "longdashdot"]

# Design factor band definitions
DFF_BANDS = [
    {"dff": 1, "label": "DFF = 1 (Mean)", "opacity": 0.0},
    {"dff": 2, "label": "DFF = 2", "opacity": 0.08},
    {"dff": 3, "label": "DFF = 3", "opacity": 0.12},
    {"dff": 10, "label": "DFF = 10", "opacity": 0.18},
]


def _curve_points(
    curve: PowerLawSNCurve,
    n_min: float = 1e3,
    n_max: float = 1e9,
    n_points: int = 200,
) -> Tuple[np.ndarray, np.ndarray]:
    """Generate (N, S) arrays for an S-N curve."""
    N = np.logspace(math.log10(n_min), math.log10(n_max), n_points)
    S = curve.get_stress_range(N)
    return N, S


def _build_plotly_html(traces_json: str, layout_json: str) -> str:
    """Build a Plotly chart div from trace and layout JSON."""
    return f"""<div id="sn-chart" style="width:100%;height:700px;"></div>
<script>
Plotly.newPlot('sn-chart', {traces_json}, {layout_json}, {{responsive: true}});
</script>"""


def generate_sn_comparison_report(
    curves: List[Tuple[str, str]],
    title: str = "S-N Curve Comparison Report",
    show_dff_bands: bool = True,
    n_min: float = 1e3,
    n_max: float = 1e9,
    output_path: Optional[str] = None,
) -> str:
    """Generate an interactive HTML S-N curve comparison report.

    Args:
        curves: List of (standard, curve_class) tuples, e.g.
                [("DNV", "D"), ("API", "X"), ("BS", "D")].
        title: Report title.
        show_dff_bands: Whether to show design factor bands.
        n_min: Minimum cycles for plot range.
        n_max: Maximum cycles for plot range.
        output_path: If provided, write HTML to this path.

    Returns:
        Complete HTML string.
    """
    import json

    # Load curves and generate data
    curve_data = []
    for standard, curve_class in curves:
        try:
            sn = StandardSNCurves.get_curve(standard, curve_class)
            N, S = _curve_points(sn, n_min, n_max)
            curve_data.append({
                "standard": standard,
                "curve_class": curve_class,
                "name": f"{standard} {curve_class}",
                "curve": sn,
                "N": N,
                "S": S,
                "A": sn.A,
                "m": sn.m,
                "fatigue_limit": sn.fatigue_limit,
            })
        except ValueError as e:
            logger.warning("Skipping %s-%s: %s", standard, curve_class, e)

    if not curve_data:
        return "<html><body><h1>No valid curves found</h1></body></html>"

    # Build Plotly traces
    traces = []

    # DFF bands (using first curve as reference)
    if show_dff_bands:
        ref = curve_data[0]
        for band in DFF_BANDS:
            if band["opacity"] > 0:
                S_dff = ref["S"] / band["dff"]
                traces.append({
                    "x": ref["N"].tolist(),
                    "y": S_dff.tolist(),
                    "mode": "lines",
                    "name": band["label"],
                    "line": {"color": "gray", "width": 1, "dash": "dot"},
                    "opacity": 0.5,
                    "showlegend": True,
                })

    # S-N curve traces
    standard_idx = {}
    for cd in curve_data:
        std = cd["standard"]
        if std not in standard_idx:
            standard_idx[std] = 0
        else:
            standard_idx[std] += 1

        color = STANDARD_COLORS.get(std, "#7f7f7f")
        dash = DASH_PATTERNS[standard_idx[std] % len(DASH_PATTERNS)]

        traces.append({
            "x": cd["N"].tolist(),
            "y": cd["S"].tolist(),
            "mode": "lines",
            "name": cd["name"],
            "line": {"color": color, "width": 2.5, "dash": dash},
            "hovertemplate": (
                f"{cd['name']}<br>"
                "N = %{x:.2e}<br>"
                "S = %{y:.1f} MPa<extra></extra>"
            ),
        })

        # Fatigue limit horizontal line
        if cd["fatigue_limit"] > 0:
            traces.append({
                "x": [n_min, n_max],
                "y": [cd["fatigue_limit"], cd["fatigue_limit"]],
                "mode": "lines",
                "name": f"{cd['name']} limit ({cd['fatigue_limit']:.0f} MPa)",
                "line": {"color": color, "width": 1, "dash": "dot"},
                "showlegend": False,
            })

    layout = {
        "title": {"text": title, "font": {"size": 16}},
        "xaxis": {
            "title": "Number of Cycles (N)",
            "type": "log",
            "showgrid": True,
            "gridcolor": "#e0e0e0",
        },
        "yaxis": {
            "title": "Stress Range (MPa)",
            "type": "log",
            "showgrid": True,
            "gridcolor": "#e0e0e0",
        },
        "template": "plotly_white",
        "hovermode": "closest",
        "legend": {
            "x": 0.01, "y": 0.01,
            "xanchor": "left", "yanchor": "bottom",
            "bgcolor": "rgba(255,255,255,0.9)",
            "bordercolor": "#ccc", "borderwidth": 1,
        },
        "width": 1000,
        "height": 700,
    }

    traces_json = json.dumps(traces)
    layout_json = json.dumps(layout)

    # Build parameter table
    table_rows = ""
    for cd in curve_data:
        log_a = math.log10(cd["A"]) if cd["A"] > 0 else 0
        table_rows += f"""<tr>
            <td>{cd['standard']}</td>
            <td>{cd['curve_class']}</td>
            <td>{cd['A']:.3e}</td>
            <td>{log_a:.3f}</td>
            <td>{cd['m']:.1f}</td>
            <td>{cd['fatigue_limit']:.1f}</td>
        </tr>"""

    # Compute allowable cycles at reference stresses for comparison
    ref_stresses = [50, 100, 150, 200, 300]
    comparison_rows = ""
    for stress in ref_stresses:
        row = f"<tr><td>{stress}</td>"
        for cd in curve_data:
            n_allow = cd["curve"].get_allowable_cycles(float(stress))
            if isinstance(n_allow, np.ndarray):
                n_allow = float(n_allow)
            if n_allow == float("inf") or n_allow > 1e15:
                row += "<td>&infin;</td>"
            else:
                row += f"<td>{n_allow:.2e}</td>"
        row += "</tr>"
        comparison_rows += row

    comparison_headers = "".join(
        f"<th>{cd['name']}</th>" for cd in curve_data
    )

    html = f"""<!DOCTYPE html>
<html>
<head>
    <title>{title}</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        body {{ font-family: 'Segoe UI', Arial, sans-serif; margin: 30px; color: #333; }}
        h1 {{ color: #1a1a2e; border-bottom: 2px solid #4472C4; padding-bottom: 10px; }}
        h2 {{ color: #4472C4; margin-top: 30px; }}
        table {{ border-collapse: collapse; width: 100%; margin: 15px 0; }}
        th {{ background-color: #4472C4; color: white; padding: 10px; text-align: right; }}
        td {{ border: 1px solid #ddd; padding: 8px; text-align: right; font-family: monospace; }}
        tr:nth-child(even) {{ background-color: #f8f9fa; }}
        th:first-child, td:first-child {{ text-align: left; }}
        .summary {{ background: #f0f4f8; border-radius: 8px; padding: 15px; margin: 15px 0; }}
        .summary p {{ margin: 5px 0; }}
        @media print {{ body {{ margin: 10px; }} }}
    </style>
</head>
<body>
    <h1>{title}</h1>

    <div class="summary">
        <p><strong>Curves compared:</strong> {len(curve_data)}</p>
        <p><strong>Standards:</strong> {', '.join(sorted(set(cd['standard'] for cd in curve_data)))}</p>
        <p><strong>Cycle range:</strong> {n_min:.0e} to {n_max:.0e}</p>
    </div>

    <h2>S-N Curve Overlay</h2>
    {_build_plotly_html(traces_json, layout_json)}

    <h2>Curve Parameters</h2>
    <table>
        <tr>
            <th>Standard</th>
            <th>Class</th>
            <th>A (intercept)</th>
            <th>log<sub>10</sub>(A)</th>
            <th>m (slope)</th>
            <th>Fatigue Limit (MPa)</th>
        </tr>
        {table_rows}
    </table>

    <h2>Allowable Cycles Comparison</h2>
    <table>
        <tr>
            <th>Stress Range (MPa)</th>
            {comparison_headers}
        </tr>
        {comparison_rows}
    </table>
</body>
</html>"""

    if output_path:
        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(html)
        logger.info("S-N comparison report written to %s", output_path)

    return html
