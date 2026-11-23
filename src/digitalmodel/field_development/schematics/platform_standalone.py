# ABOUTME: PlatformSchematic — fixed/jacket platform stand-alone layout diagram
"""
PlatformSchematic

Renders a side-view schematic of a stand-alone platform development:
  - Jacket structure rising from seabed to surface
  - Well conductors through the jacket
  - Optional satellite subsea wells with short tiebacks
  - Export pipeline to edge of diagram
  - Annotations: field name, water depth, scale bar
"""

from __future__ import annotations

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from matplotlib.figure import Figure

from .elements.icons import (
    make_platform_patch,
    make_well_symbol,
    TEMPLATE_HEIGHT,
    TEMPLATE_WIDTH,
    WELL_RADIUS,
)
from .elements.annotations import (
    add_scale_bar,
    add_depth_label,
    add_field_name_label,
    add_north_arrow,
)
from .elements.seabed import draw_seabed_line, compute_water_column_height

_SURFACE_Y = 0.88
_HOST_X = 0.5
_MAX_CONDUCTORS = 8


class PlatformSchematic:
    """Generates a stand-alone platform layout schematic.

    Parameters
    ----------
    water_depth_m : float
        Water depth in metres.
    n_wells : int
        Number of platform well conductors (capped at 8).
    host_type : str
        ``'fixed'``, ``'jacket'``, ``'semi'``, or ``'TLP'``.
    n_satellite_wells : int
        Number of satellite subsea wells (0 = none).
    satellite_tieback_km : float
        Tieback distance for satellites in km.
    """

    def __init__(
        self,
        water_depth_m: float,
        n_wells: int,
        host_type: str,
        n_satellite_wells: int = 0,
        satellite_tieback_km: float = 3.0,
    ) -> None:
        self.water_depth_m = float(water_depth_m)
        self.n_wells = min(max(n_wells, 1), _MAX_CONDUCTORS)
        self.host_type = host_type
        self.n_satellite_wells = max(n_satellite_wells, 0)
        self.satellite_tieback_km = float(satellite_tieback_km)
        self.seabed_y = self._compute_seabed_y()

    def _compute_seabed_y(self) -> float:
        frac = compute_water_column_height(
            water_depth_m=self.water_depth_m,
            fig_height_m=max(self.water_depth_m * 1.25, 200.0),
        )
        return float(np.clip(1.0 - frac, 0.18, 0.65))

    def render(
        self,
        field_name: str,
        figsize: tuple[float, float] = (12, 7),
        facecolor: str = "#f0f4f8",
    ) -> Figure:
        """Render the platform schematic.

        Parameters
        ----------
        field_name : str
            Field name label.
        figsize : tuple
            Figure size in inches.
        facecolor : str
            Background colour.

        Returns
        -------
        matplotlib.figure.Figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        fig.patch.set_facecolor(facecolor)
        ax.set_facecolor(facecolor)
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis("off")

        # Seabed
        draw_seabed_line(ax, y_seabed=self.seabed_y, x_start=0.05, x_end=0.95)

        # Platform (jacket) body — a vertical rectangle from seabed to surface
        jacket_w = 0.06
        jacket_h = _SURFACE_Y - self.seabed_y
        jacket_rect = mpatches.FancyBboxPatch(
            (_HOST_X - jacket_w / 2, self.seabed_y),
            jacket_w,
            jacket_h,
            boxstyle="square,pad=0.0",
            facecolor="#5c6e82",
            edgecolor="black",
            linewidth=1.2,
            transform=ax.transAxes,
            zorder=3,
        )
        ax.add_patch(jacket_rect)

        # Well conductors through jacket
        spacing = jacket_w / (self.n_wells + 1)
        for i in range(self.n_wells):
            wx = _HOST_X - jacket_w / 2 + spacing * (i + 1)
            ax.plot(
                [wx, wx],
                [self.seabed_y - 0.05, _SURFACE_Y],
                color="#b7950b",
                linewidth=0.8,
                linestyle="-",
                transform=ax.transAxes,
                zorder=2,
            )

        # Platform topsides symbol on top of jacket
        topsides = mpatches.FancyBboxPatch(
            (_HOST_X - 0.07, _SURFACE_Y),
            0.14,
            0.04,
            boxstyle="round,pad=0.003",
            facecolor="#5c6e82",
            edgecolor="black",
            linewidth=1.2,
            transform=ax.transAxes,
            zorder=4,
        )
        ax.add_patch(topsides)

        # Export pipeline (horizontal line from jacket base to right edge)
        ax.plot(
            [_HOST_X + jacket_w / 2, 0.92],
            [self.seabed_y + 0.02, self.seabed_y + 0.02],
            color="#e07b39",
            linewidth=1.4,
            transform=ax.transAxes,
            zorder=2,
        )
        ax.text(
            0.93,
            self.seabed_y + 0.03,
            "Export",
            transform=ax.transAxes,
            ha="left",
            va="bottom",
            fontsize=6.5,
            color="#e07b39",
        )

        # Optional satellite wells
        if self.n_satellite_wells > 0:
            self._draw_satellite_wells(ax)

        # Annotations
        add_field_name_label(ax, name=field_name, x=0.5, y=0.97)
        add_depth_label(
            ax, x=0.08, y=(self.seabed_y + _SURFACE_Y) / 2.0,
            depth_m=self.water_depth_m,
        )
        add_north_arrow(ax, x=0.93, y=0.88)
        add_scale_bar(ax, x=0.06, y=0.04, length_km=max(self.satellite_tieback_km, 1.0))
        ax.text(
            _HOST_X,
            _SURFACE_Y + 0.06,
            self.host_type.upper(),
            transform=ax.transAxes,
            ha="center",
            va="bottom",
            fontsize=7.5,
            color="#1a5276",
            fontweight="bold",
        )

        fig.tight_layout(pad=0.5)
        return fig

    def _draw_satellite_wells(self, ax: Any) -> None:
        """Draw satellite subsea well symbols to the left of the platform."""
        x_sat_start = 0.15
        x_sat_end = _HOST_X - 0.10
        xs = np.linspace(x_sat_start, x_sat_end, self.n_satellite_wells)
        y_sat = self.seabed_y - 0.03
        for sx in xs:
            w_patch = make_well_symbol(cx=sx, cy=y_sat)
            w_patch.set_transform(ax.transAxes)
            ax.add_patch(w_patch)
            # Short tieback line
            ax.plot(
                [sx, _HOST_X - 0.03],
                [y_sat, self.seabed_y + 0.01],
                color="#e07b39",
                linewidth=1.0,
                linestyle="--",
                transform=ax.transAxes,
                zorder=2,
            )


# Type alias to avoid bare Any in the helper
from typing import Any
