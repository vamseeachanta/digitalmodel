"""Section 2: RAO plots per DOF using Plotly interactive charts."""
from __future__ import annotations

import math

import numpy as np

from ..config import RAOPlotsConfig

# DOF index mapping (0-based into the 6-DOF block per body)
_DOF_LABELS = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
_DOF_UNITS = {
    "surge": "m/m",
    "sway": "m/m",
    "heave": "m/m",
    "roll": "deg/m",
    "pitch": "deg/m",
    "yaw": "deg/m",
}
_ROTATIONAL_DOFS = {"roll", "pitch", "yaw"}


def build_rao_plots(diff, config: RAOPlotsConfig, include_plotlyjs: str = "cdn") -> str:
    """Build tabbed Plotly RAO plots (one tab per DOF).

    Args:
        diff: OrcFxAPI.Diffraction result object.
        config: RAOPlotsConfig controlling which DOFs and headings to plot.
        include_plotlyjs: Plotly JS inclusion strategy passed to fig.to_html().

    Returns:
        HTML string containing tabbed Plotly figures.
    """
    try:
        import plotly.graph_objects as go  # type: ignore
    except ImportError:
        return "<p class='section-placeholder'>Plotly is required for RAO plots.</p>"

    # --- Frequencies: Hz descending -> periods ascending ---
    freqs_hz = np.array(diff.frequencies)  # descending
    sort_idx = np.argsort(freqs_hz)  # ascending freq indices
    freqs_sorted = freqs_hz[sort_idx]
    periods = np.where(freqs_sorted > 0, 1.0 / freqs_sorted, np.inf)

    # --- RAOs: shape (nheading, nfreq, 6*nbodies), complex ---
    raos_raw = np.array(diff.displacementRAOs)  # complex
    nheadings, nfreq, ndof_total = raos_raw.shape
    n_bodies = ndof_total // 6

    # Re-order frequencies to ascending
    raos_sorted = raos_raw[:, sort_idx, :]  # (nheading, nfreq, 6*nbodies)

    # --- Headings filter ---
    all_headings = list(diff.headings)
    if config.headings is not None:
        heading_indices = [
            i for i, h in enumerate(all_headings) if h in config.headings
        ]
    else:
        heading_indices = list(range(nheadings))

    # --- DOFs filter ---
    enabled_dofs = [d.lower() for d in config.dofs]
    dof_indices = [
        i for i, label in enumerate(_DOF_LABELS) if label in enabled_dofs
    ]

    # Build one figure per DOF (first body only for multi-body)
    tab_ids: list[str] = []
    tab_labels: list[str] = []
    figures_html: list[str] = []

    plotlyjs_included = False  # include JS only in the first figure

    for dof_idx in dof_indices:
        dof_name = _DOF_LABELS[dof_idx]
        unit = _DOF_UNITS[dof_name]
        is_rotational = dof_name in _ROTATIONAL_DOFS

        fig = go.Figure()

        for hi in heading_indices:
            heading_deg = all_headings[hi]
            rao_mag = np.abs(raos_sorted[hi, :, dof_idx])
            if is_rotational:
                rao_mag = np.degrees(rao_mag)

            # Filter out infinite periods
            valid = np.isfinite(periods)
            fig.add_trace(
                go.Scatter(
                    x=periods[valid],
                    y=rao_mag[valid],
                    mode="lines+markers",
                    name=f"{heading_deg:.0f}°",
                )
            )

            if config.include_phase:
                phase = np.angle(raos_sorted[hi, :, dof_idx], deg=True)
                fig.add_trace(
                    go.Scatter(
                        x=periods[valid],
                        y=phase[valid],
                        mode="lines",
                        name=f"{heading_deg:.0f}° phase",
                        line=dict(dash="dot"),
                        visible="legendonly",
                    )
                )

        fig.update_layout(
            xaxis_title="Period (s)",
            yaxis_title=f"RAO magnitude ({unit})",
            legend_title="Heading",
            margin=dict(l=50, r=20, t=40, b=50),
            height=420,
        )

        js_mode = include_plotlyjs if not plotlyjs_included else False
        fig_html = fig.to_html(include_plotlyjs=js_mode, full_html=False)
        plotlyjs_included = True

        tab_id = f"rao-tab-{dof_name}"
        tab_ids.append(tab_id)
        tab_labels.append(dof_name.capitalize())
        figures_html.append(fig_html)

    return _render_tabs(tab_ids, tab_labels, figures_html)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _render_tabs(
    tab_ids: list[str],
    tab_labels: list[str],
    contents: list[str],
) -> str:
    """Render Bootstrap 5 tab navigation wrapping Plotly figures."""
    if not tab_ids:
        return "<p class='section-placeholder'>No DOFs enabled for RAO plots.</p>"

    nav_items = []
    pane_items = []

    for i, (tid, label, content) in enumerate(zip(tab_ids, tab_labels, contents)):
        active = "active" if i == 0 else ""
        selected = "true" if i == 0 else "false"
        show_active = "show active" if i == 0 else ""

        nav_items.append(
            f'<li class="nav-item" role="presentation">'
            f'<button class="nav-link {active}" id="{tid}-btn" '
            f'data-bs-toggle="tab" data-bs-target="#{tid}" '
            f'type="button" role="tab" aria-selected="{selected}">'
            f'{label}</button></li>'
        )
        pane_items.append(
            f'<div class="tab-pane fade {show_active}" id="{tid}" '
            f'role="tabpanel" aria-labelledby="{tid}-btn">{content}</div>'
        )

    nav_html = (
        '<ul class="nav nav-tabs" role="tablist">\n'
        + "\n".join(nav_items)
        + "\n</ul>"
    )
    panes_html = (
        '<div class="tab-content">\n'
        + "\n".join(pane_items)
        + "\n</div>"
    )
    return nav_html + "\n" + panes_html
