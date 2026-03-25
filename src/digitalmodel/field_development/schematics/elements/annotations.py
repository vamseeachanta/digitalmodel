# ABOUTME: Annotation helpers — scale bar, depth label, field name, north arrow
"""
Annotation helpers for field development schematics.

All position arguments are in axes-fraction coordinates [0, 1].
"""

from __future__ import annotations

from typing import Any

import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
from matplotlib.text import Text
from matplotlib.lines import Line2D


def add_scale_bar(
    ax: Axes,
    x: float,
    y: float,
    length_km: float,
    bar_frac: float = 0.12,
    color: str = "black",
    fontsize: float = 7.0,
) -> list[Any]:
    """Draw a simple horizontal scale bar at axes-fraction position (x, y).

    Parameters
    ----------
    ax : Axes
        Target axes.
    x, y : float
        Left anchor position in axes-fraction space.
    length_km : float
        Real-world distance the bar represents (km).
    bar_frac : float
        Bar length as fraction of axes width.
    color : str
        Bar and label colour.
    fontsize : float
        Label font size in points.

    Returns
    -------
    list of artists
    """
    # Horizontal bar
    bar = ax.annotate(
        "",
        xy=(x + bar_frac, y),
        xytext=(x, y),
        xycoords="axes fraction",
        textcoords="axes fraction",
        arrowprops=dict(
            arrowstyle="<->",
            color=color,
            lw=1.2,
        ),
    )
    # Label
    label = ax.text(
        x + bar_frac / 2,
        y + 0.012,
        f"{length_km:.0f} km",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=fontsize,
        color=color,
    )
    return [bar, label]


def add_depth_label(
    ax: Axes,
    x: float,
    y: float,
    depth_m: float,
    color: str = "#1a5276",
    fontsize: float = 8.0,
) -> Text:
    """Add a water-depth annotation at axes-fraction position (x, y).

    Parameters
    ----------
    ax : Axes
        Target axes.
    x, y : float
        Position in axes-fraction space.
    depth_m : float
        Water depth in metres.
    color : str
        Text colour.
    fontsize : float
        Font size in points.

    Returns
    -------
    matplotlib.text.Text
    """
    txt = ax.text(
        x,
        y,
        f"WD: {depth_m:.0f} m",
        transform=ax.transAxes,
        ha="center",
        va="center",
        fontsize=fontsize,
        color=color,
        style="italic",
    )
    return txt


def add_field_name_label(
    ax: Axes,
    name: str,
    x: float = 0.5,
    y: float = 0.95,
    color: str = "black",
    fontsize: float = 11.0,
    fontweight: str = "bold",
) -> Text:
    """Add a field name title label at axes-fraction position (x, y).

    Parameters
    ----------
    ax : Axes
        Target axes.
    name : str
        Field name string.
    x, y : float
        Position in axes-fraction space.
    """
    txt = ax.text(
        x,
        y,
        name,
        transform=ax.transAxes,
        ha="center",
        va="top",
        fontsize=fontsize,
        fontweight=fontweight,
        color=color,
    )
    return txt


def add_north_arrow(
    ax: Axes,
    x: float = 0.92,
    y: float = 0.88,
    length: float = 0.055,
    color: str = "black",
    fontsize: float = 8.0,
) -> Any:
    """Draw a simple north-pointing arrow at axes-fraction position (x, y).

    Parameters
    ----------
    ax : Axes
        Target axes.
    x, y : float
        Base of the arrow in axes-fraction space.
    length : float
        Arrow length in axes-fraction units.
    """
    arrow = ax.annotate(
        "N",
        xy=(x, y + length),
        xytext=(x, y),
        xycoords="axes fraction",
        textcoords="axes fraction",
        fontsize=fontsize,
        ha="center",
        va="bottom",
        fontweight="bold",
        color=color,
        arrowprops=dict(
            arrowstyle="-|>",
            color=color,
            lw=1.4,
        ),
    )
    return arrow
