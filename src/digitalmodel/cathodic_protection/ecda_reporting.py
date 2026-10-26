# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Interactive, self-contained cathodic protection survey analysis."""

from __future__ import annotations

from html import escape
from pathlib import Path

import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.cathodic_protection.ecda import ECDAResult

CLASS_COLORS = {
    "high priority": "#b2182b",
    "further assessment": "#a6611a",
    "conflicting signals": "#2166ac",
}


def build_chainage_chart(result: ECDAResult) -> go.Figure:
    """Stack raw survey readings on a shared axis with ranked anomaly markers."""
    fig = make_subplots(
        rows=4, cols=1, shared_xaxes=True, vertical_spacing=0.08,
        subplot_titles=("CIS", "DCVG", "PCM", "ACVG"),
        specs=[[{}], [{}], [{"secondary_y": True}], [{}]],
    )
    criteria = result.criteria
    potential = criteria["cis_potential"]
    series = [
        ("cis", f"CIS {potential.upper()}", "potential_V", 1, "lines+markers"),
        ("dcvg", "DCVG", "ir_drop_percentage", 2, "markers"),
        ("pcm_current", "PCM", "signal_current_mA", 3, "lines+markers"),
        ("acvg", "ACVG", "reading", 4, "markers"),
    ]
    for name, label, field, row, mode in series:
        points = result.surveys[name]
        hover = [escape(str({k: v for k, v in point.items() if k != "distance_m"})) for point in points]
        fig.add_trace(go.Scatter(
            x=[p["distance_m"] for p in points], y=[p[field] for p in points],
            name=label, mode=mode, connectgaps=False, text=hover,
            hovertemplate="Chainage %{x:.1f} m<br>Reading %{y:.1f}<br>%{text}<extra>%{fullData.name}</extra>",
        ), row=row, col=1)
    segments = result.surveys["pcm"]
    fig.add_trace(go.Scatter(
        x=[p["distance_m"] for p in segments], y=[p["loss_per_m"] for p in segments],
        name="PCM segment loss", mode="markers", marker_symbol="diamond",
        customdata=[[p["start_m"], p["end_m"]] for p in segments],
        hovertemplate="Segment %{customdata[0]:.1f}–%{customdata[1]:.1f} m<br>Loss %{y:.1f}<extra>PCM</extra>",
    ), row=3, col=1, secondary_y=True)
    for threshold in (criteria["cis_protection_criterion_V"], criteria["cis_overprotection_limit_V"]):
        fig.add_hline(y=threshold, row=1, col=1, line_dash="dot")
    fig.add_hline(y=criteria["dcvg_min_ir_percentage"], row=2, col=1, line_dash="dot")
    fig.add_hline(y=criteria["pcm"]["loss_threshold"], row=3, col=1, secondary_y=True, line_dash="dot")
    for key in ("moderate_min", "severe_min", "very_severe_min"):
        fig.add_hline(y=criteria["acvg"][key], row=4, col=1, line_dash="dot")
    for classification, color in CLASS_COLORS.items():
        anomalies = [a for a in result.anomalies if a.classification == classification]
        # Legend swatches describe vertical guides without inventing survey values.
        fig.add_trace(go.Scatter(
            x=[None], y=[None], mode="lines", line=dict(color=color, dash="dash"),
            name=classification, hoverinfo="skip",
        ), row=1, col=1)
        for anomaly in anomalies:
            for row in range(1, 5):
                fig.add_vline(x=anomaly.chainage_m, row=row, col=1, line_color=color, line_dash="dash")
            fig.add_annotation(
                x=anomaly.chainage_m, y=1.02, xref="x", yref="y domain",
                text=f"Dig {anomaly.rank}: {classification}", showarrow=False,
                font=dict(color=color), textangle=-25,
            )
    fig.update_yaxes(title_text="Potential [V vs CSE]", row=1, col=1)
    fig.update_yaxes(title_text="IR drop [%]", row=2, col=1)
    fig.update_yaxes(title_text="Current [mA]", row=3, col=1, secondary_y=False)
    fig.update_yaxes(title_text=f"Loss [{criteria['pcm']['loss_unit']}]", row=3, col=1, secondary_y=True)
    fig.update_yaxes(title_text=f"Reading [{criteria['acvg']['unit']}]", row=4, col=1)
    fig.update_xaxes(title_text="Chainage [m]", row=4, col=1)
    fig.update_layout(
        height=1150, title="Cathodic protection analysis — integrated surveys",
        hovermode="x unified", template="plotly_white", margin=dict(t=130),
        meta={"criteria": criteria, "criteria_note": result.criteria_note},
    )
    return fig


def chainage_html(result: ECDAResult) -> str:
    """Render raw surveys, criteria and ranked dig evidence without network assets."""
    chart = str(build_chainage_chart(result).to_html(
        full_html=False, include_plotlyjs=True,
        config={"scrollZoom": True, "displaylogo": False},
    ))
    criteria_rows = []
    for key, value in result.criteria.items():
        nested = value if isinstance(value, dict) else {"": value}
        for child, setting in nested.items():
            label = f"{key}.{child}" if child else key
            criteria_rows.append(f"<tr><td>{escape(label)}</td><td>{escape(str(setting))}</td></tr>")
    table = str(result.to_dataframe().to_html(index=False, escape=True, float_format=lambda x: f"{x:.1f}"))
    return (
        '<!doctype html><html lang="en"><head><meta charset="utf-8">'
        '<title>Cathodic protection analysis — integrated surveys</title>'
        '<style>body{font-family:sans-serif;margin:2em}table{border-collapse:collapse}'
        'td,th{border:1px solid #bbb;padding:.4em} .digs{overflow-x:auto}</style></head><body>'
        '<h1>Cathodic protection analysis — integrated surveys</h1>'
        '<h2>Summary</h2>'
        f'<p>{len(result.anomalies)} candidate assessment locations. '
        'Review high priority locations first, investigate conflicting signals and confirm '
        'survey coverage before preparing an excavation plan. Missing evidence is unknown. '
        f'Criteria: {escape(result.criteria_note)}. No compliance verdict is implied.</p>'
        '<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>'
        '<tr><td>0</td><td>2026-10-26</td><td>Initial configurable survey analysis for assessment planning.</td></tr>'
        '<tr><td>1</td><td>2026-10-26</td><td>Require explicit criteria and retain their basis '
        'to prevent implicit demonstration priorities.</td></tr></table>'
        '<h2>Criteria</h2><p>All criteria and their provenance are supplied explicitly. '
        'Engineering criteria require recorded confirmation; demonstration criteria '
        'have no established field basis. Source references are retained as supplied '
        'and their applicability is not independently established by this analysis. '
        'Chainage groups span no more than the alignment tolerance from their earliest '
        'observation. PCM uses segment midpoints; endpoints remain in the evidence. '
        'An observation is used once; each survey contributes at most one positive vote. '
        'Protected CIS alongside positive evidence fires cis_disagrees. Otherwise '
        'corroborated_anomaly requires the configured survey count and CIS requirement; '
        'remaining positive groups fire isolated_anomaly. Normal-only groups are omitted. '
        'Ranking follows high priority, further assessment, conflicting signals; within '
        'each class more positive survey types rank first, then earlier chainage. '
        'No physical conversion is inferred for ACVG relative readings.</p>'
        '<table><tr><th>Input</th><th>Value</th></tr>' + ''.join(criteria_rows) + '</table>'
        '<h2>Chainage chart</h2><p>Dashed vertical guides mark anomaly classes; dig ranks '
        'appear above CIS. Dotted horizontal guides show configured thresholds. '
        'Zoom, pan and hover to inspect evidence.</p>' + chart +
        '<h2>Ranked dig list</h2><div class="digs">' + table + '</div>'
        '<h2>Limits and way forward</h2><p>These are survey correlations, not confirmed '
        'metal loss or remaining strength. Current gains are retained as negative loss; '
        'check transmitter position and survey direction before interpreting PCM. '
        'Confirm chainage registration, instrument units, criteria and missing coverage '
        'with the maintainer before field assessment.</p></body></html>'
    )


def write_chainage_html(result: ECDAResult, path: str | Path) -> None:
    """Write a self-contained interactive HTML analysis."""
    Path(path).write_text(chainage_html(result), encoding="utf-8", newline="\n")
