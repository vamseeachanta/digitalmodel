# ABOUTME: Seabed line and water-column gradient drawing helpers
"""
Seabed element drawing for field development schematics.

Provides a hatched seabed line and a blue-gradient water column fill.
"""

from __future__ import annotations

from typing import Any

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from matplotlib.axes import Axes
from matplotlib.lines import Line2D

# Hatch pattern for the seabed sediment fill
SEABED_HATCH: str = "///"

_WATER_ALPHA_TOP: float = 0.15
_WATER_ALPHA_BOTTOM: float = 0.45
_SEABED_COLOUR: str = "#b5a07a"
_SEABED_LINE_COLOUR: str = "#6b4f2a"


def compute_water_column_height(
    water_depth_m: float,
    fig_height_m: float,
    min_frac: float = 0.15,
    max_frac: float = 0.75,
) -> float:
    """Map water depth to an axes-fraction height for the water column.

    A shallow field (100 m) gets a smaller fraction than a deep one (3000 m),
    clamped to [min_frac, max_frac].

    Parameters
    ----------
    water_depth_m : float
        Actual water depth.
    fig_height_m : float
        Notional total vertical extent of the diagram in metres.
    min_frac, max_frac : float
        Output range clamp.

    Returns
    -------
    float
        Axes-fraction height of the water column.
    """
    raw = water_depth_m / fig_height_m
    return float(np.clip(raw, min_frac, max_frac))


def draw_seabed_line(
    ax: Axes,
    y_seabed: float,
    x_start: float = 0.0,
    x_end: float = 1.0,
    color: str = _SEABED_LINE_COLOUR,
    linewidth: float = 2.0,
    add_water_fill: bool = True,
    add_seabed_fill: bool = True,
) -> Any:
    """Draw a horizontal seabed line with optional water and sediment fills.

    Parameters
    ----------
    ax : Axes
        Target axes (must be using axes-fraction transform).
    y_seabed : float
        Y position of the seabed in axes-fraction space.
    x_start, x_end : float
        Horizontal extent of the seabed line.
    color : str
        Seabed line colour.
    linewidth : float
        Seabed line width.
    add_water_fill : bool
        Draw a blue gradient rectangle above the seabed.
    add_seabed_fill : bool
        Draw a hatched rectangle below the seabed.

    Returns
    -------
    Line2D
        The seabed line artist.
    """
    transform = ax.transAxes

    if add_water_fill:
        water_rect = mpatches.FancyBboxPatch(
            (x_start, y_seabed),
            x_end - x_start,
            1.0 - y_seabed,
            boxstyle="square,pad=0.0",
            facecolor="#c6e2f5",
            edgecolor="none",
            alpha=0.35,
            transform=transform,
            zorder=0,
        )
        ax.add_patch(water_rect)

    if add_seabed_fill:
        sed_rect = mpatches.FancyBboxPatch(
            (x_start, 0.0),
            x_end - x_start,
            y_seabed,
            boxstyle="square,pad=0.0",
            facecolor=_SEABED_COLOUR,
            edgecolor="none",
            hatch=SEABED_HATCH,
            alpha=0.6,
            transform=transform,
            zorder=0,
        )
        ax.add_patch(sed_rect)

    # Seabed line on top
    (line,) = ax.plot(
        [x_start, x_end],
        [y_seabed, y_seabed],
        color=color,
        linewidth=linewidth,
        transform=transform,
        zorder=1,
        solid_capstyle="butt",
    )
    return line
