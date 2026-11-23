"""
ABOUTME: Plotly graph generation for RAO lookup results — Phase 4 of WRK-043.

Provides client-facing interactive figures for inspecting and comparing
hull RAO data stored in a RAODatabase.

Functions
---------
per_hull_rao_plot(entry)
    Six-subplot figure showing RAO amplitude vs frequency for all DOFs,
    at all available wave heading directions.

comparison_plot(entries)
    Overlaid RAO curves for multiple hull variants (heave DOF by default,
    with hover info showing hull parameters).

parameter_sweep_plot(entries, param_name, dof)
    A single figure showing how RAO changes with a swept parameter.

export_html(fig, path)
    Write an interactive HTML file.

export_png(fig, path)
    Write a static PNG (requires kaleido).
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

import numpy as np

if TYPE_CHECKING:
    import plotly.graph_objects as go


# ---------------------------------------------------------------------------
# DOF metadata
# ---------------------------------------------------------------------------

_DOF_NAMES = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
_DOF_UNITS = ["m/m", "m/m", "m/m", "deg/m", "deg/m", "deg/m"]

# Default heading index to use when a single heading is plotted
_DEFAULT_DIR_IDX = 0


# ---------------------------------------------------------------------------
# Public figures
# ---------------------------------------------------------------------------


def per_hull_rao_plot(entry: "RAODatabaseEntry") -> "go.Figure":
    """Return a 6-subplot figure of RAO amplitudes for all DOFs.

    Each subplot shows amplitude vs frequency (rad/s) for every wave
    heading direction present in the RAOData.  The variation ID is used
    as the figure title.

    Parameters
    ----------
    entry:
        A ``RAODatabaseEntry`` containing RAOData.

    Returns
    -------
    plotly.graph_objects.Figure
    """
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots

    rao = entry.rao_data
    n_dirs = len(rao.directions)

    fig = make_subplots(
        rows=2,
        cols=3,
        subplot_titles=_DOF_NAMES,
        shared_xaxes=False,
    )

    colors = _direction_colors(n_dirs)

    for dof_idx in range(6):
        row = dof_idx // 3 + 1
        col = dof_idx % 3 + 1

        for dir_idx in range(n_dirs):
            direction = rao.directions[dir_idx]
            amp = rao.amplitudes[:, dir_idx, dof_idx]

            trace = go.Scatter(
                x=rao.frequencies,
                y=amp,
                mode="lines",
                name=f"{direction:.0f}°",
                line={"color": colors[dir_idx]},
                showlegend=(dof_idx == 0),
                legendgroup=f"dir_{dir_idx}",
                hovertemplate=(
                    f"Dir: {direction:.0f}°<br>"
                    "Freq: %{x:.3f} rad/s<br>"
                    "Amp: %{y:.4f}<extra></extra>"
                ),
            )
            fig.add_trace(trace, row=row, col=col)

        fig.update_xaxes(title_text="Freq (rad/s)", row=row, col=col)
        fig.update_yaxes(
            title_text=_DOF_UNITS[dof_idx], row=row, col=col
        )

    fig.update_layout(
        title_text=f"RAO — {entry.variation_id}",
        height=600,
        legend_title="Heading",
    )
    return fig


def comparison_plot(entries: list["RAODatabaseEntry"]) -> "go.Figure":
    """Return an overlaid RAO comparison figure for multiple hull variants.

    Shows heave (DOF index 2) amplitude vs frequency for each entry at
    the first available heading direction.  Hover text includes the hull
    parameter values.

    Parameters
    ----------
    entries:
        List of ``RAODatabaseEntry`` objects to compare.

    Returns
    -------
    plotly.graph_objects.Figure
    """
    import plotly.graph_objects as go

    fig = go.Figure()

    for entry in entries:
        rao = entry.rao_data
        params_str = ", ".join(
            f"{k}={v:.3g}" for k, v in sorted(entry.hull_params.items())
        )

        # Show all DOFs as separate traces, heave highlighted
        for dof_idx in range(6):
            amp = rao.amplitudes[:, _DEFAULT_DIR_IDX, dof_idx]
            dof_name = _DOF_NAMES[dof_idx]
            is_heave = dof_idx == 2

            fig.add_trace(
                go.Scatter(
                    x=rao.frequencies,
                    y=amp,
                    mode="lines",
                    name=f"{entry.variation_id} / {dof_name}",
                    visible=True if is_heave else "legendonly",
                    hovertemplate=(
                        f"ID: {entry.variation_id}<br>"
                        f"DOF: {dof_name}<br>"
                        f"Params: {params_str}<br>"
                        "Freq: %{x:.3f} rad/s<br>"
                        "Amp: %{y:.4f}<extra></extra>"
                    ),
                )
            )

    fig.update_layout(
        title_text="RAO Comparison",
        xaxis_title="Frequency (rad/s)",
        yaxis_title="Amplitude",
        legend_title="ID / DOF",
    )
    return fig


def parameter_sweep_plot(
    entries: list["RAODatabaseEntry"],
    param_name: str,
    dof: int = 2,
) -> "go.Figure":
    """Return a figure showing RAO variation with a swept parameter.

    One trace per entry, coloured by parameter value.  The legend shows
    the parametric value and the trace name is the variation ID.

    Parameters
    ----------
    entries:
        Ordered list of ``RAODatabaseEntry`` objects (ideally varying
        only the ``param_name`` dimension).
    param_name:
        The hull parameter key being swept (used in legend and title).
    dof:
        DOF index to plot (0=surge … 5=yaw).

    Returns
    -------
    plotly.graph_objects.Figure
    """
    import plotly.graph_objects as go

    dof_label = _DOF_NAMES[dof] if 0 <= dof < 6 else f"DOF_{dof}"
    fig = go.Figure()

    param_values = [
        e.hull_params.get(param_name, float("nan")) for e in entries
    ]

    # Normalise param values for colour mapping (0..1)
    finite_vals = [v for v in param_values if not np.isnan(v)]
    vmin = min(finite_vals) if finite_vals else 0.0
    vmax = max(finite_vals) if finite_vals else 1.0
    vrange = (vmax - vmin) if vmax != vmin else 1.0

    colors = _sequential_colors(len(entries))

    for i, entry in enumerate(entries):
        rao = entry.rao_data
        amp = rao.amplitudes[:, _DEFAULT_DIR_IDX, dof]
        pval = param_values[i]
        legend_label = (
            f"{param_name}={pval:.3g}" if not np.isnan(pval) else entry.variation_id
        )

        fig.add_trace(
            go.Scatter(
                x=rao.frequencies,
                y=amp,
                mode="lines",
                name=legend_label,
                line={"color": colors[i]},
                hovertemplate=(
                    f"ID: {entry.variation_id}<br>"
                    f"{param_name}: {pval:.4g}<br>"
                    "Freq: %{x:.3f} rad/s<br>"
                    "Amp: %{y:.4f}<extra></extra>"
                ),
            )
        )

    fig.update_layout(
        title_text=f"RAO Sweep — {param_name} / {dof_label}",
        xaxis_title="Frequency (rad/s)",
        yaxis_title=f"{dof_label} Amplitude",
        legend_title=param_name,
    )
    return fig


# ---------------------------------------------------------------------------
# Export helpers
# ---------------------------------------------------------------------------


def export_html(fig: "go.Figure", path: str | Path) -> Path:
    """Write an interactive HTML file for the figure.

    Creates parent directories if they do not exist.

    Parameters
    ----------
    fig:
        A plotly Figure object.
    path:
        Destination file path (should end in ``.html``).

    Returns
    -------
    pathlib.Path
        Resolved absolute path of the written file.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.write_html(str(path), include_plotlyjs="cdn")
    return path.resolve()


def export_png(fig: "go.Figure", path: str | Path,
               width: int = 1200, height: int = 800) -> Path:
    """Write a static PNG image for the figure.

    Requires the ``kaleido`` package to be installed.

    Parameters
    ----------
    fig:
        A plotly Figure object.
    path:
        Destination file path (should end in ``.png``).
    width:
        Image width in pixels.
    height:
        Image height in pixels.

    Returns
    -------
    pathlib.Path
        Resolved absolute path of the written file.

    Raises
    ------
    ImportError
        If ``kaleido`` is not installed.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.write_image(str(path), width=width, height=height)
    return path.resolve()


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _direction_colors(n: int) -> list[str]:
    """Return n distinct CSS colour strings for heading directions."""
    _palette = [
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
        "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    ]
    if n <= len(_palette):
        return _palette[:n]
    # Repeat palette if more directions than colours
    return [_palette[i % len(_palette)] for i in range(n)]


def _sequential_colors(n: int) -> list[str]:
    """Return n colours from a blue-to-red sequential scale."""
    if n == 0:
        return []
    if n == 1:
        return ["#1f77b4"]
    import colorsys

    colors = []
    for i in range(n):
        # Hue from 0.67 (blue) to 0.0 (red)
        hue = 0.67 * (1.0 - i / (n - 1))
        r, g, b = colorsys.hsv_to_rgb(hue, 0.85, 0.9)
        colors.append(
            f"#{int(r*255):02x}{int(g*255):02x}{int(b*255):02x}"
        )
    return colors


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "per_hull_rao_plot",
    "comparison_plot",
    "parameter_sweep_plot",
    "export_html",
    "export_png",
]
