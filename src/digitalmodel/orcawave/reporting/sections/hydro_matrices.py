"""Section 3: Added mass and damping matrices — diagonal line plots."""
from __future__ import annotations

import numpy as np

from ..config import HydroMatricesConfig

_DOF_LABELS = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
_MAX_BODIES = 2


def build_hydro_matrices(diff, config: HydroMatricesConfig) -> str:
    """Build HTML for added mass / damping diagonal plots section."""
    try:
        import plotly.graph_objects as go  # type: ignore
    except ImportError:
        return "<p class='section-placeholder'>Plotly required for hydro matrices.</p>"

    try:
        am = np.array(diff.addedMass)   # (nfreq, 6N, 6N)
        dm = np.array(diff.damping)     # (nfreq, 6N, 6N)
        freqs_hz = np.array(diff.frequencies)   # Hz, descending
    except Exception as exc:
        return (
            f"<div class='alert alert-warning'>"
            f"Hydrodynamic matrices unavailable: {exc}</div>"
        )

    sort_idx = np.argsort(freqs_hz)
    freqs_sorted = freqs_hz[sort_idx]
    periods = np.where(freqs_sorted > 0, 1.0 / freqs_sorted, np.inf)
    am_sorted = am[sort_idx]
    dm_sorted = dm[sort_idx]

    n_bodies = min(am.shape[1] // 6, _MAX_BODIES)
    valid = np.isfinite(periods)

    cards = []
    for body_idx in range(n_bodies):
        r0 = body_idx * 6
        body_label = f"Body {body_idx + 1}" if n_bodies > 1 else "Body"
        cards.append(_build_matrix_card(
            am_sorted, dm_sorted, periods, valid, r0, body_label, go
        ))

    return "\n".join(cards)


def _build_matrix_card(am, dm, periods, valid, r0, body_label, go) -> str:
    """Build a Bootstrap card with added mass and damping subplots for one body."""
    from plotly.subplots import make_subplots  # type: ignore

    fig = make_subplots(
        rows=2, cols=3,
        subplot_titles=[f"{lbl} diag" for lbl in _DOF_LABELS],
        shared_xaxes=True,
    )

    for col_idx, lbl in enumerate(_DOF_LABELS):
        row_pos = col_idx // 3 + 1
        col_pos = col_idx % 3 + 1
        dof = r0 + col_idx
        am_diag = am[:, dof, dof][valid]
        dm_diag = dm[:, dof, dof][valid]
        t = periods[valid]

        fig.add_trace(
            go.Scatter(x=t, y=am_diag, mode="lines", name=f"A{col_idx+1}{col_idx+1}",
                       showlegend=(col_idx == 0)),
            row=row_pos, col=col_pos,
        )
        # Damping as dashed overlay on same axes, secondary y not needed
        fig.add_trace(
            go.Scatter(x=t, y=dm_diag, mode="lines",
                       name=f"B{col_idx+1}{col_idx+1}",
                       line=dict(dash="dash"), showlegend=(col_idx == 0)),
            row=row_pos, col=col_pos,
        )

    fig.update_layout(
        height=480,
        margin=dict(l=50, r=20, t=60, b=50),
        legend=dict(orientation="h", y=-0.15),
    )
    fig.update_xaxes(title_text="Period (s)")

    fig_html = fig.to_html(include_plotlyjs=False, full_html=False)
    return (
        f"<div class='card mb-4'>"
        f"<div class='card-header'><strong>{body_label} — Added Mass (solid) &amp; "
        f"Radiation Damping (dashed) diagonal terms</strong></div>"
        f"<div class='card-body'>{fig_html}</div>"
        f"</div>"
    )
