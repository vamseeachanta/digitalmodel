"""Section 6: Multi-body coupling matrix heatmap."""
from __future__ import annotations

import numpy as np

from ..config import MultiBodyConfig

_DOF_SHORT = ["Su", "Sw", "He", "Ro", "Pi", "Ya"]


def build_multi_body(diff, config: MultiBodyConfig) -> str:
    """Build HTML for multi-body coupling matrix section."""
    try:
        am = np.array(diff.addedMass)  # (nfreq, 6N, 6N)
    except Exception as exc:
        return f"<div class='alert alert-warning'>Added mass unavailable: {exc}</div>"

    n_bodies = am.shape[1] // 6
    if n_bodies < 2:
        return (
            "<div class='card mb-4'>"
            "<div class='card-header'><strong>Multi-Body Coupling</strong></div>"
            "<div class='card-body'><p class='text-muted'>"
            "Single body — no coupling terms.</p></div></div>"
        )
    try:
        import plotly.graph_objects as go  # type: ignore
    except ImportError:
        return "<p class='section-placeholder'>Plotly required for coupling heatmap.</p>"

    freqs_hz = np.array(diff.frequencies)
    sort_idx = np.argsort(freqs_hz)
    mid_idx = len(sort_idx) // 2
    sel_period = 1.0 / freqs_hz[sort_idx][mid_idx] if freqs_hz[sort_idx][mid_idx] > 0 else 0
    mat = am[sort_idx][mid_idx]  # (6N, 6N)

    dof_labels = [f"B{b+1}-{s}" for b in range(n_bodies) for s in _DOF_SHORT]
    fig = go.Figure(go.Heatmap(
        z=mat, x=dof_labels, y=dof_labels,
        colorscale="RdBu", zmid=0, colorbar=dict(title="kg"),
    ))
    for b in range(1, n_bodies):
        bnd = b * 6 - 0.5
        fig.add_shape(type="line", x0=bnd, x1=bnd, y0=-0.5, y1=6*n_bodies-0.5,
                      line=dict(color="black", width=1))
        fig.add_shape(type="line", y0=bnd, y1=bnd, x0=-0.5, x1=6*n_bodies-0.5,
                      line=dict(color="black", width=1))
    fig.update_layout(
        title=f"Added Mass Matrix at T = {sel_period:.2f} s",
        height=500, margin=dict(l=120, r=20, t=60, b=120),
    )

    rows = "".join(
        f"<tr><td>B{b1+1}–B{b2+1}</td>"
        f"<td>{float(np.sqrt(np.mean(mat[b1*6:b1*6+6, b2*6:b2*6+6]**2))):.4g}</td></tr>"
        for b1 in range(n_bodies) for b2 in range(b1 + 1, n_bodies)
    )
    coupling_html = (
        "<h6 class='mt-3'>Off-diagonal coupling RMS (kg)</h6>"
        "<div class='table-responsive' style='max-width:400px'>"
        "<table class='table table-sm table-bordered'>"
        "<thead class='table-light'><tr><th>Body pair</th><th>RMS</th></tr></thead>"
        f"<tbody>{rows}</tbody></table></div>"
    )
    fig_html = fig.to_html(include_plotlyjs=False, full_html=False)
    return (
        "<div class='card mb-4'>"
        "<div class='card-header'><strong>Multi-Body Coupling Matrix</strong></div>"
        f"<div class='card-body'>{fig_html}{coupling_html}</div></div>"
    )
