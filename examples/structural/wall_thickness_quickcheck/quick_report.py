# ABOUTME: Pure HTML/Plotly renderer for the wall-thickness quickcheck cache.
# ABOUTME: Builds offline self-contained report without importing the calculation engine.
"""Report rendering for ``quick_check.py`` cached payloads."""

from __future__ import annotations

import html
import math
from pathlib import Path
from typing import Any


def is_propagation_check(
    code_label: str,
    check_name: str,
    propagation_checks: dict[str, set[str]],
) -> bool:
    return check_name in propagation_checks.get(code_label, set())


def find_sweep_point(payload: dict[str, Any], wall_mm: float) -> dict[str, Any]:
    for point in payload["sweep"]:
        if math.isclose(point["wall_thickness_mm"], wall_mm, abs_tol=0.001):
            return point
    raise KeyError(f"No sweep point for wall {wall_mm:.3f} mm")


def code_data(point: dict[str, Any], code_label: str) -> dict[str, Any]:
    return [item for item in point["codes"] if item["code_label"] == code_label][0]


def check_rows_html(
    point: dict[str, Any],
    include_propagation: bool,
    propagation_checks: dict[str, set[str]],
) -> str:
    rows = []
    for code in point["codes"]:
        code_label = code["code_label"]
        for check_name, util in code["checks"].items():
            included = include_propagation or not is_propagation_check(
                code_label, check_name, propagation_checks
            )
            status = "PASS" if util <= 1.0 else "FAIL"
            rows.append(
                "<tr>"
                f"<td>{html.escape(code_label)}</td><td>{html.escape(check_name)}</td>"
                f"<td>{util:.3f}</td><td>{'yes' if included else 'reported only'}</td>"
                f"<td class=\"{'pass' if util <= 1.0 else 'fail'}\">{status}</td>"
                "</tr>"
            )
    return "\n".join(rows)


def build_plot_html(payload: dict[str, Any]) -> str:
    import plotly.graph_objects as go
    import plotly.io as pio
    from plotly.subplots import make_subplots

    sweep = payload["sweep"]
    selected = payload["selection"]["with_arrestors"]["selected_standard_wall_mm"]
    nonstandard = payload["selection"]["with_arrestors"]["nonstandard_minimum_wall_mm"]
    selected_point = find_sweep_point(payload, selected)
    fig = make_subplots(
        rows=2,
        cols=1,
        subplot_titles=("Utilization Sweep", "Selected Wall Utilizations"),
        vertical_spacing=0.16,
    )

    for code_label in payload["case"]["codes"]:
        for check_name in code_data(sweep[0], code_label)["checks"]:
            x_vals, y_vals, marker_sizes = [], [], []
            for point in sweep:
                data = code_data(point, code_label)
                x_vals.append(point["wall_thickness_mm"])
                y_vals.append(data["checks"][check_name])
                marker_sizes.append(8 if point["is_standard"] else 0)
            fig.add_trace(
                go.Scatter(
                    x=x_vals,
                    y=y_vals,
                    mode="lines+markers",
                    name=f"{code_label} {check_name}",
                    marker={"size": marker_sizes, "symbol": "diamond"},
                    hovertemplate=(
                        "Wall %{x:.3f} mm<br>U=%{y:.4f}"
                        "<extra>%{fullData.name}</extra>"
                    ),
                ),
                row=1,
                col=1,
            )

    for code in selected_point["codes"]:
        fig.add_trace(
            go.Bar(
                x=list(code["checks"].keys()),
                y=list(code["checks"].values()),
                name=f"{code['code_label']} at selected wall",
                offsetgroup=code["code_label"],
                hovertemplate="%{x}<br>U=%{y:.4f}<extra>%{fullData.name}</extra>",
            ),
            row=2,
            col=1,
        )

    fig.add_hline(y=1.0, line_dash="dash", line_color="#b00020", row=1, col=1)
    fig.add_hline(y=1.0, line_dash="dash", line_color="#b00020", row=2, col=1)
    fig.add_vline(x=nonstandard, line_dash="dot", line_color="#424242", row=1, col=1)
    fig.add_vline(x=selected, line_dash="dash", line_color="#0b6e4f", row=1, col=1)
    fig.update_xaxes(title_text="Wall thickness (mm)", row=1, col=1)
    fig.update_yaxes(title_text="Utilization", row=1, col=1)
    fig.update_xaxes(title_text="Check", row=2, col=1)
    fig.update_yaxes(title_text="Utilization", row=2, col=1)
    fig.update_layout(
        template="plotly_white",
        height=950,
        barmode="group",
        legend={"orientation": "h", "y": -0.18},
        margin={"l": 70, "r": 40, "t": 70, "b": 160},
    )
    return pio.to_html(fig, full_html=False, include_plotlyjs="inline")


def write_report(
    payload: dict[str, Any],
    output_path: Path,
    propagation_checks: dict[str, set[str]],
) -> Path:
    selected = payload["selection"]["with_arrestors"]
    selected_point = find_sweep_point(payload, selected["selected_standard_wall_mm"])
    without = payload["selection"]["without_arrestors"]
    sizing = payload["buckle_arrestor_sizing"]
    assumptions = payload["case"]
    report = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Wall Thickness Quick Check</title>
<style>
body {{ font-family: Arial, sans-serif; margin: 24px auto; max-width: 1180px; color: #222; }}
h1, h2 {{ color: #102a43; }}
table {{ border-collapse: collapse; width: 100%; margin: 12px 0 24px; }}
th, td {{ border-bottom: 1px solid #d9e2ec; padding: 8px 10px; text-align: left; }}
th {{ background: #243b53; color: white; }}
.pass {{ color: #1b7f3a; font-weight: bold; }}
.fail {{ color: #b00020; font-weight: bold; }}
.callout {{ background: #f0f4f8; border-left: 4px solid #0b6e4f; padding: 12px 16px; }}
</style>
</head>
<body>
<h1>Pipeline Wall Thickness Quick Check</h1>
<h2>Assumptions</h2>
<table>
<tr><th>Input</th><th>Value</th></tr>
<tr><td>Case</td><td>{html.escape(assumptions['label'])}</td></tr>
<tr><td>Pressure basis</td><td>{html.escape(assumptions['pressure_basis'])}; quoted 150 bar is design pressure</td></tr>
<tr><td>Buckle arrestors</td><td>{html.escape(assumptions['buckle_arrestors'])}; propagation reported but excluded from headline</td></tr>
<tr><td>OD</td><td>{assumptions['geometry']['outer_diameter'] * 1000:.1f} mm (12.75 in)</td></tr>
<tr><td>Corrosion allowance</td><td>{assumptions['geometry']['corrosion_allowance'] * 1000:.1f} mm</td></tr>
<tr><td>Material</td><td>{html.escape(assumptions['material']['grade'])}, SMYS {assumptions['material']['smys']/1e6:.0f} MPa, SMTS {assumptions['material']['smts']/1e6:.0f} MPa</td></tr>
<tr><td>External pressure</td><td>{payload['external_pressure_mpa']:.3f} MPa at {assumptions['water_depth_m']:.0f} m</td></tr>
<tr><td>Load premises</td><td>{html.escape(payload.get('load_premises', ''))}</td></tr>
</table>
<h2>Results</h2>
<div class="callout">
Headline wall: <strong>{selected['selected_standard_wall_in']:.3f} in / {selected['selected_standard_wall_mm']:.3f} mm</strong>
({html.escape(selected['selected_standard_label'])}). Governing check:
<strong>{html.escape(selected['governing_check'])}</strong> U={selected['governing_utilisation']:.3f}.
Calculated non-standard minimum: {selected['nonstandard_minimum_wall_mm']:.3f} mm.
</div>
<table>
<tr><th>Code</th><th>Check</th><th>Utilization</th><th>Included in Headline</th><th>Status</th></tr>
{check_rows_html(selected_point, False, propagation_checks)}
</table>
<h2>Propagation Buckling Branch</h2>
<p>Without arrestors, propagation buckling is included in governing selection.
The propagation-governed non-standard minimum is {without['nonstandard_minimum_wall_mm']:.3f} mm;
the selected standard wall is {without['selected_standard_wall_in']:.3f} in /
{without['selected_standard_wall_mm']:.3f} mm ({html.escape(without['selected_standard_label'])}),
governed by {html.escape(without['governing_check'])} at U={without['governing_utilisation']:.3f}.</p>
<h2>Buckle Arrestor Sizing</h2>
<table>
<tr><th>Dimension</th><th>Value</th></tr>
<tr><td>Type</td><td>{html.escape(sizing['type'])}</td></tr>
<tr><td>Selected pipe wall</td><td>{sizing['pipe_wall_mm']:.3f} mm</td></tr>
<tr><td>Arrestor wall thickness</td><td>{sizing['arrestor_wall_mm']:.3f} mm</td></tr>
<tr><td>Additional wall</td><td>{sizing['additional_wall_mm']:.3f} mm</td></tr>
<tr><td>Length</td><td>{sizing['arrestor_length_m']:.3f} m ({html.escape(sizing['length_basis'])})</td></tr>
<tr><td>Governing crossover criterion</td><td>{html.escape(sizing['governing_code'])}; crossover pressure {sizing['crossover_pressure_mpa']:.3f} MPa</td></tr>
</table>
<h2>Interactive Plot</h2>
{build_plot_html(payload)}
</body>
</html>
"""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(report, encoding="utf-8")
    return output_path
