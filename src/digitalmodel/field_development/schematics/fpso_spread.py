# ABOUTME: FpsoSpreadSchematic — FPSO with spread mooring and subsea production system
"""
FpsoSpreadSchematic

Renders a plan-view schematic of an FPSO with spread mooring:
  - FPSO vessel at centre
  - Mooring lines radiating out at evenly distributed angles
  - Subsea production wells shown as icons below
  - Umbilical / flow line connections
  - Annotations: field name, water depth, north arrow
"""

from __future__ import annotations

import math
from typing import Any

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from matplotlib.figure import Figure

from .elements.icons import (
    make_fpso_patch,
    make_well_symbol,
    FPSO_WIDTH,
    FPSO_HEIGHT,
    WELL_RADIUS,
)
from .elements.annotations import (
    add_depth_label,
    add_field_name_label,
    add_north_arrow,
)
from .elements.seabed import draw_seabed_line, compute_water_column_height

_FPSO_CX = 0.5
_FPSO_CY = 0.60
_MOORING_REACH = 0.28   # mooring line reach in axes fraction
_WELL_REACH = 0.22      # subsea well reach below FPSO


class FpsoSpreadSchematic:
    """Generates an FPSO spread mooring + SPS schematic.

    This renders a combined side-view schematic showing:
    - The FPSO at the surface
    - Spread mooring lines angling down to the seabed
    - Subsea production wells on the seabed

    Parameters
    ----------
    water_depth_m : float
        Water depth in metres.
    n_mooring_lines : int
        Total number of mooring lines (evenly distributed).
    n_subsea_wells : int
        Number of subsea production wells.
    """

    def __init__(
        self,
        water_depth_m: float,
        n_mooring_lines: int,
        n_subsea_wells: int,
    ) -> None:
        self.water_depth_m = float(water_depth_m)
        self.n_mooring_lines = max(n_mooring_lines, 3)
        self.n_subsea_wells = max(n_subsea_wells, 1)
        self.seabed_y = self._compute_seabed_y()

    def _compute_seabed_y(self) -> float:
        frac = compute_water_column_height(
            water_depth_m=self.water_depth_m,
            fig_height_m=max(self.water_depth_m * 1.25, 800.0),
        )
        return float(np.clip(1.0 - frac, 0.15, 0.55))

    def compute_mooring_angles(self) -> list[float]:
        """Compute evenly distributed mooring line angles in degrees (0=right).

        Returns
        -------
        list of float
            Angles in degrees, starting from 0, spaced by 360/n_mooring_lines.
        """
        step = 360.0 / self.n_mooring_lines
        return [i * step for i in range(self.n_mooring_lines)]

    def render(
        self,
        field_name: str,
        figsize: tuple[float, float] = (12, 7),
        facecolor: str = "#f0f4f8",
    ) -> Figure:
        """Render the FPSO spread mooring schematic.

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

        # Water column + seabed
        draw_seabed_line(ax, y_seabed=self.seabed_y, x_start=0.05, x_end=0.95)

        # FPSO at surface
        fpso_cy = _FPSO_CY + (0.88 - _FPSO_CY) * 0.5  # place near surface
        fpso_patch = make_fpso_patch(cx=_FPSO_CX, cy=fpso_cy)
        fpso_patch.set_transform(ax.transAxes)
        ax.add_patch(fpso_patch)
        ax.text(
            _FPSO_CX,
            fpso_cy + FPSO_HEIGHT,
            "FPSO",
            transform=ax.transAxes,
            ha="center",
            va="bottom",
            fontsize=8.0,
            fontweight="bold",
            color="#1a5276",
        )

        # Mooring lines — angling from FPSO down to seabed
        angles_deg = self.compute_mooring_angles()
        for angle_deg in angles_deg:
            angle_rad = math.radians(angle_deg)
            # Project outward and downward for side-view representation
            dx = _MOORING_REACH * math.cos(angle_rad) * 0.6
            dy = -(fpso_cy - self.seabed_y) * 0.85
            # Anchor point on seabed
            ax_end = _FPSO_CX + dx
            ay_end = self.seabed_y + 0.01
            ax.plot(
                [_FPSO_CX, ax_end],
                [fpso_cy - FPSO_HEIGHT / 2, ay_end],
                color="#7f8c8d",
                linewidth=0.9,
                linestyle="-",
                alpha=0.7,
                transform=ax.transAxes,
                zorder=2,
            )
            # Anchor symbol
            ax.plot(
                ax_end,
                ay_end,
                marker="v",
                markersize=4,
                color="#7f8c8d",
                transform=ax.transAxes,
                zorder=3,
            )

        # Subsea production wells below FPSO on seabed
        spacing = 0.06
        total_w = (self.n_subsea_wells - 1) * spacing
        for i in range(self.n_subsea_wells):
            wx = _FPSO_CX - total_w / 2 + i * spacing
            wy = self.seabed_y - 0.025
            w_patch = make_well_symbol(cx=wx, cy=wy)
            w_patch.set_transform(ax.transAxes)
            ax.add_patch(w_patch)
            # Riser/umbilical from well to FPSO
            ax.plot(
                [wx, _FPSO_CX],
                [wy + WELL_RADIUS, fpso_cy - FPSO_HEIGHT / 2],
                color="#e07b39",
                linewidth=0.9,
                linestyle="--",
                alpha=0.8,
                transform=ax.transAxes,
                zorder=2,
            )

        # Surface line
        ax.plot(
            [0.05, 0.95],
            [fpso_cy - FPSO_HEIGHT * 0.7, fpso_cy - FPSO_HEIGHT * 0.7],
            color="#85c1e9",
            linewidth=1.0,
            linestyle="--",
            transform=ax.transAxes,
            zorder=1,
            alpha=0.6,
        )

        # Annotations
        add_field_name_label(ax, name=field_name, x=0.5, y=0.97)
        add_depth_label(
            ax, x=0.08, y=(self.seabed_y + fpso_cy) / 2.0,
            depth_m=self.water_depth_m,
        )
        add_north_arrow(ax, x=0.93, y=0.88)

        fig.tight_layout(pad=0.5)
        return fig
