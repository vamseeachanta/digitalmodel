"""Section 7: QTF magnitude heatmap."""
from __future__ import annotations

import numpy as np

from ..config import QTFHeatmapConfig


def build_qtf_heatmap(diff, config: QTFHeatmapConfig) -> str:
    """Build HTML for QTF heatmap section.

    Gracefully skips if QTF results are unavailable on the diff object.
    """
    if not _qtf_results_available(diff):
        return (
            "<div class='card mb-4'>"
            "<div class='card-header'><strong>QTF Analysis</strong></div>"
            "<div class='card-body'><div class='alert alert-secondary mb-0'>"
            "QTF results not available in this .owr file — section skipped."
            "</div></div></div>"
        )
    try:
        import plotly.graph_objects as go  # type: ignore
    except ImportError:
        return "<p class='section-placeholder'>Plotly required for QTF heatmap.</p>"
    try:
        return _build_card(diff, config, go)
    except Exception as exc:
        return f"<div class='alert alert-warning'>QTF heatmap failed: {exc}</div>"


def _qtf_results_available(diff) -> bool:
    try:
        return diff.QTFResults is not None
    except Exception:
        return False


def _build_card(diff, config: QTFHeatmapConfig, go) -> str:
    qtf_arr = np.array(diff.QTFResults)
    freqs = np.array(diff.QTFAngularFrequencies)   # rad/s
    headings = list(diff.headings)

    head_idx = int(np.argmin([abs(h - 180.0) for h in headings])) if headings else 0

    # Extract |QTF| for surge — handle 3-D and 4-D shapes
    if qtf_arr.ndim == 4:
        mat = np.abs(qtf_arr[head_idx, :, :, 0])
    elif qtf_arr.ndim == 3:
        mat = np.abs(qtf_arr[:, :, 0])
    else:
        mat = np.abs(qtf_arr)

    # Apply max_delta_omegas masking
    max_do = config.max_delta_omegas
    if max_do is not None and mat.ndim == 2 and mat.shape[0] == mat.shape[1]:
        n = mat.shape[0]
        idx = np.abs(np.subtract.outer(np.arange(n), np.arange(n))) > max_do
        mat = mat.astype(float).copy()
        mat[idx] = np.nan

    periods = [f"{2*np.pi/f:.1f}" if f > 0 else "inf" for f in freqs]
    fig = go.Figure(go.Heatmap(
        z=mat, x=periods, y=periods,
        colorscale="Viridis", colorbar=dict(title="|QTF|"),
    ))
    fig.update_layout(
        xaxis_title="T₁ (s)", yaxis_title="T₂ (s)",
        title=f"QTF Magnitude — Surge, heading {headings[head_idx]:.0f}°"
              f" (max Δω = {max_do})",
        height=500, margin=dict(l=80, r=20, t=60, b=80),
    )
    fig_html = fig.to_html(include_plotlyjs=False, full_html=False)
    return (
        "<div class='card mb-4'>"
        "<div class='card-header'><strong>QTF Heatmap</strong></div>"
        f"<div class='card-body'>{fig_html}</div></div>"
    )
