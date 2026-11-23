"""Section 4: Mean drift table and optional polar plot."""
from __future__ import annotations

import numpy as np

from ..config import MeanDriftConfig

_DOF_LABELS = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
_DRIFT_ATTRS = ["meanDriftLoadControlSurface", "meanDriftLoadPressureIntegration"]


def build_mean_drift(diff, config: MeanDriftConfig) -> str:
    """Build HTML for mean drift table and optional polar plot section."""
    drift, source = None, None
    for attr in _DRIFT_ATTRS:
        try:
            data = getattr(diff, attr)
            if data is not None:
                drift, source = np.array(data), attr
                break
        except Exception:
            continue
    if drift is None:
        return "<div class='alert alert-info'>Mean drift not available.</div>"

    try:
        import plotly.graph_objects as go  # type: ignore
    except ImportError:
        return "<p class='section-placeholder'>Plotly required for mean drift.</p>"

    freqs_hz = np.array(diff.frequencies)
    sort_idx = np.argsort(freqs_hz)
    periods = np.where(freqs_hz[sort_idx] > 0, 1.0 / freqs_hz[sort_idx], np.inf)
    headings = list(diff.headings)
    drift_s = drift[:, sort_idx, :]   # (nheading, nfreq, 6), ascending freq

    table_html = _peak_table(drift_s, periods)
    polar_html = _polar_plot(drift_s, periods, headings, go) if config.include_polar else ""
    return (
        f"<div class='card mb-4'>"
        f"<div class='card-header'><strong>Mean Drift ({source})</strong></div>"
        f"<div class='card-body'>{table_html}{polar_html}</div></div>"
    )


def _peak_table(drift, periods) -> str:
    valid = np.isfinite(periods)
    peak_period = float(periods[valid][-1]) if np.any(valid) else float("nan")
    peak_i = np.where(valid)[0][-1] if np.any(valid) else 0
    rows = "".join(
        f"<tr><td>{lbl}</td>"
        f"<td>{float(np.max(np.abs(drift[:, peak_i, i]))):.4g}</td></tr>"
        for i, lbl in enumerate(_DOF_LABELS)
    )
    return (
        f"<p>Values at T = {peak_period:.2f} s (max over headings)</p>"
        "<div class='table-responsive' style='max-width:480px'>"
        "<table class='table table-sm table-bordered'>"
        "<thead class='table-light'><tr><th>DOF</th><th>Peak drift</th></tr></thead>"
        f"<tbody>{rows}</tbody></table></div>"
    )


def _polar_plot(drift, periods, headings, go) -> str:
    valid = np.isfinite(periods)
    if not np.any(valid):
        return ""
    period_idx = int(np.argmax(
        np.sqrt(drift[:, valid, 0] ** 2 + drift[:, valid, 1] ** 2).sum(axis=0)
    ))
    sel_period = float(periods[valid][period_idx])
    fig = go.Figure()
    for dof_i, dof_name in enumerate(["Surge", "Sway"]):
        vals = np.abs(drift[:, valid, dof_i][:, period_idx])
        fig.add_trace(go.Scatterpolar(
            r=np.append(vals, vals[0]),
            theta=np.append(headings, headings[0]),
            mode="lines+markers", name=dof_name,
        ))
    fig.update_layout(
        title=f"Mean Drift — T = {sel_period:.2f} s",
        height=400, margin=dict(l=40, r=40, t=60, b=40),
    )
    return fig.to_html(include_plotlyjs=False, full_html=False)
