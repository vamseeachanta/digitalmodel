# ABOUTME: Matplotlib patch factories for field development schematic icons
"""
Icon patch factories for field development schematics.

All coordinates are in axes-fraction space [0, 1] x [0, 1].
Callers add the returned patches to a matplotlib Axes via ax.add_patch().
"""

from __future__ import annotations

from matplotlib.patches import FancyBboxPatch, Circle, FancyArrowPatch, Polygon
import matplotlib.patches as mpatches
import numpy as np

# ---------------------------------------------------------------------------
# Dimension constants (axes-fraction units)
# ---------------------------------------------------------------------------

FPSO_WIDTH: float = 0.14
FPSO_HEIGHT: float = 0.04
TEMPLATE_WIDTH: float = 0.06
TEMPLATE_HEIGHT: float = 0.025
WELL_RADIUS: float = 0.008

# Colour palette for a clean technical illustration style
_COLOUR_WATER = "#c6e2f5"
_COLOUR_FPSO = "#4a7fa5"
_COLOUR_PLATFORM = "#5c6e82"
_COLOUR_TEMPLATE = "#2c7a4b"
_COLOUR_WELL = "#e07b39"


def make_fpso_patch(
    cx: float,
    cy: float,
    facecolor: str = _COLOUR_FPSO,
    edgecolor: str = "black",
    linewidth: float = 1.2,
    **kwargs,
) -> mpatches.Patch:
    """Return a rounded rectangle representing an FPSO vessel.

    Parameters
    ----------
    cx, cy : float
        Centre coordinates in axes-fraction space.
    facecolor, edgecolor : str
        Fill and outline colours.
    linewidth : float
        Outline line width.
    **kwargs : any
        Forwarded to FancyBboxPatch.

    Returns
    -------
    matplotlib.patches.Patch
    """
    x = cx - FPSO_WIDTH / 2.0
    y = cy - FPSO_HEIGHT / 2.0
    patch = FancyBboxPatch(
        (x, y),
        FPSO_WIDTH,
        FPSO_HEIGHT,
        boxstyle="round,pad=0.004",
        facecolor=facecolor,
        edgecolor=edgecolor,
        linewidth=linewidth,
        **kwargs,
    )
    return patch


def make_platform_patch(
    cx: float,
    cy: float,
    facecolor: str = _COLOUR_PLATFORM,
    edgecolor: str = "black",
    linewidth: float = 1.2,
    **kwargs,
) -> mpatches.Patch:
    """Return a trapezoidal polygon representing a fixed/jacket platform.

    Parameters
    ----------
    cx, cy : float
        Centre of the base of the platform in axes-fraction space.
    """
    # Jacket-style trapezoid: wider at base, narrower at top
    w_base = FPSO_WIDTH * 0.7
    w_top = FPSO_WIDTH * 0.35
    h = FPSO_HEIGHT * 1.8
    verts = np.array([
        [cx - w_base / 2, cy],
        [cx + w_base / 2, cy],
        [cx + w_top / 2, cy + h],
        [cx - w_top / 2, cy + h],
    ])
    patch = Polygon(
        verts,
        closed=True,
        facecolor=facecolor,
        edgecolor=edgecolor,
        linewidth=linewidth,
        **kwargs,
    )
    return patch


def make_template_patch(
    cx: float,
    cy: float,
    facecolor: str = _COLOUR_TEMPLATE,
    edgecolor: str = "black",
    linewidth: float = 1.0,
    label: str | None = None,
    **kwargs,
) -> mpatches.Patch:
    """Return a rectangle representing a subsea template/manifold.

    Parameters
    ----------
    cx, cy : float
        Centre coordinates in axes-fraction space.
    label : str, optional
        Text label (stored as patch label for callers to retrieve).
    """
    x = cx - TEMPLATE_WIDTH / 2.0
    y = cy - TEMPLATE_HEIGHT / 2.0
    patch = FancyBboxPatch(
        (x, y),
        TEMPLATE_WIDTH,
        TEMPLATE_HEIGHT,
        boxstyle="square,pad=0.0",
        facecolor=facecolor,
        edgecolor=edgecolor,
        linewidth=linewidth,
        label=label or "",
        **kwargs,
    )
    return patch


def make_well_symbol(
    cx: float,
    cy: float,
    facecolor: str = _COLOUR_WELL,
    edgecolor: str = "black",
    linewidth: float = 0.8,
    **kwargs,
) -> Circle:
    """Return a Circle representing a single well slot.

    Parameters
    ----------
    cx, cy : float
        Centre coordinates in axes-fraction space.
    """
    patch = Circle(
        (cx, cy),
        radius=WELL_RADIUS,
        facecolor=facecolor,
        edgecolor=edgecolor,
        linewidth=linewidth,
        **kwargs,
    )
    return patch
