# ABOUTME: SubseaTiebackSchematic — subsea template + flowline + host layout diagram
"""
SubseaTiebackSchematic

Renders a side-view schematic of a subsea tieback development:
  - Water column with depth gradient
  - Seabed with hatch fill
  - N subsea templates, each with M well symbols
  - Flowlines from each template up to the host facility
  - Host facility (FPSO, TLP, SPAR, or fixed platform) at the surface
  - Annotations: field name, water depth, scale bar, north arrow
"""

from __future__ import annotations

import math
from typing import Any

import matplotlib
matplotlib.use("Agg")  # non-interactive backend for file output
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
from matplotlib.figure import Figure

from .elements.icons import (
    make_fpso_patch,
    make_platform_patch,
    make_template_patch,
    make_well_symbol,
    TEMPLATE_WIDTH,
    TEMPLATE_HEIGHT,
    WELL_RADIUS,
)
from .elements.annotations import (
    add_scale_bar,
    add_depth_label,
    add_field_name_label,
    add_north_arrow,
)
from .elements.seabed import draw_seabed_line, compute_water_column_height

_MAX_WELLS = 8

# Canvas layout constants (axes-fraction)
_SURFACE_Y = 0.88          # y position of the surface / host waterline
_SEABED_Y_SHALLOW = 0.60   # seabed y for very shallow fields
_SEABED_Y_DEEP = 0.18      # seabed y for very deep fields
_HOST_X = 0.82             # x position of the host facility
_TEMPLATE_Y_OFFSET = -0.04 # templates sit slightly below seabed line centre
_WELL_Y_BELOW = 0.04       # wells hang below their template


class SubseaTiebackSchematic:
    """Generates a subsea tieback layout schematic.

    Parameters
    ----------
    n_templates : int
        Number of subsea templates (1 – 6 drawn; capped at 6).
    water_depth_m : float
        Water depth in metres.
    flowline_length_km : float
        Approximate flowline length in km (drives x-axis scale annotation).
    host_type : str
        ``'FPSO'``, ``'TLP'``, ``'SPAR'``, or ``'fixed'``.
    """

    def __init__(
        self,
        n_templates: int,
        water_depth_m: float,
        flowline_length_km: float,
        host_type: str,
    ) -> None:
        self.n_templates = max(1, min(n_templates, 6))
        self.water_depth_m = float(water_depth_m)
        self.flowline_length_km = float(flowline_length_km)
        self.host_type = host_type.upper() if host_type.lower() != "fixed" else "fixed"
        self.x_scale_factor = max(flowline_length_km, 1.0)
        self.seabed_y = self._compute_seabed_y()

    # ------------------------------------------------------------------
    # Layout helpers
    # ------------------------------------------------------------------

    def _compute_seabed_y(self) -> float:
        """Return the seabed y position in axes-fraction space."""
        frac = compute_water_column_height(
            water_depth_m=self.water_depth_m,
            fig_height_m=max(self.water_depth_m * 1.25, 500.0),
        )
        # frac is fraction of axes occupied by water → seabed is at (1 - frac)
        return float(np.clip(1.0 - frac, _SEABED_Y_DEEP, _SEABED_Y_SHALLOW))

    def compute_template_positions(self) -> list[tuple[float, float]]:
        """Compute evenly-spaced template (x, y) positions in axes-fraction space.

        Returns
        -------
        list of (x, y) tuples
        """
        x_start = 0.12
        x_end = _HOST_X - 0.10
        positions = []
        if self.n_templates == 1:
            xs = [0.5 * (x_start + x_end)]
        else:
            xs = list(np.linspace(x_start, x_end, self.n_templates))
        y = self.seabed_y + _TEMPLATE_Y_OFFSET
        for x in xs:
            positions.append((float(x), float(y)))
        return positions

    def compute_well_positions(
        self,
        template_pos: tuple[float, float],
        n_wells: int,
    ) -> list[tuple[float, float]]:
        """Compute well symbol positions relative to a template.

        Well count is clamped to _MAX_WELLS.

        Parameters
        ----------
        template_pos : (x, y)
            Template centre in axes-fraction space.
        n_wells : int
            Requested number of wells.

        Returns
        -------
        list of (x, y) tuples
        """
        n_wells = min(n_wells, _MAX_WELLS)
        cx, cy = template_pos
        spacing = WELL_RADIUS * 3.0
        y_well = cy - TEMPLATE_HEIGHT / 2.0 - _WELL_Y_BELOW
        total_width = (n_wells - 1) * spacing
        xs = [cx - total_width / 2.0 + i * spacing for i in range(n_wells)]
        return [(float(x), float(y_well)) for x in xs]

    # ------------------------------------------------------------------
    # Render
    # ------------------------------------------------------------------

    def render(
        self,
        field_name: str,
        n_wells: int = 3,
        figsize: tuple[float, float] = (12, 7),
        facecolor: str = "#f0f4f8",
    ) -> Figure:
        """Render the schematic and return a matplotlib Figure.

        Parameters
        ----------
        field_name : str
            Field name displayed as the diagram title.
        n_wells : int
            Wells per template (1–8, clamped).
        figsize : tuple
            Figure size in inches.
        facecolor : str
            Figure background colour.

        Returns
        -------
        matplotlib.figure.Figure
        """
        n_wells = min(max(n_wells, 1), _MAX_WELLS)

        fig, ax = plt.subplots(figsize=figsize)
        fig.patch.set_facecolor(facecolor)
        ax.set_facecolor(facecolor)
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis("off")

        # 1. Water column + seabed
        draw_seabed_line(
            ax,
            y_seabed=self.seabed_y,
            x_start=0.05,
            x_end=0.95,
        )

        # 2. Templates + wells + flowlines
        template_positions = self.compute_template_positions()
        host_x = _HOST_X
        host_y = _SURFACE_Y

        for idx, (tx, ty) in enumerate(template_positions):
            # Flowline from template to host (curved line in axes coords)
            self._draw_flowline(ax, tx, ty, host_x, host_y)

            # Template rectangle
            t_patch = make_template_patch(cx=tx, cy=ty)
            ax.add_patch(
                _to_axes_patch(t_patch, ax)
            )

            # Template label
            ax.text(
                tx,
                ty + TEMPLATE_HEIGHT,
                f"T{idx + 1}",
                transform=ax.transAxes,
                ha="center",
                va="bottom",
                fontsize=6.5,
                color="white",
            )

            # Wells
            well_positions = self.compute_well_positions((tx, ty), n_wells)
            for wx, wy in well_positions:
                w_patch = make_well_symbol(cx=wx, cy=wy)
                ax.add_patch(_to_axes_patch(w_patch, ax))
                # Vertical conductor from template to well
                ax.plot(
                    [wx, wx],
                    [ty - TEMPLATE_HEIGHT / 2.0, wy + WELL_RADIUS],
                    color="#6b4f2a",
                    linewidth=0.8,
                    transform=ax.transAxes,
                    zorder=2,
                )

        # 3. Host facility
        self._draw_host(ax, host_x, host_y)

        # 4. Surface line
        ax.plot(
            [0.05, 0.95],
            [_SURFACE_Y + 0.01, _SURFACE_Y + 0.01],
            color="#85c1e9",
            linewidth=1.0,
            linestyle="--",
            transform=ax.transAxes,
            zorder=1,
            alpha=0.6,
        )

        # 5. Annotations
        add_field_name_label(ax, name=field_name, x=0.5, y=0.97)
        add_depth_label(ax, x=0.08, y=(self.seabed_y + _SURFACE_Y) / 2.0,
                        depth_m=self.water_depth_m)
        add_north_arrow(ax, x=0.93, y=0.88)
        add_scale_bar(ax, x=0.06, y=0.04,
                      length_km=self.flowline_length_km)

        # Host type label
        ax.text(
            host_x,
            _SURFACE_Y - 0.04,
            self.host_type,
            transform=ax.transAxes,
            ha="center",
            va="top",
            fontsize=7.5,
            color="#1a5276",
            fontweight="bold",
        )

        fig.tight_layout(pad=0.5)
        return fig

    # ------------------------------------------------------------------
    # Private drawing helpers
    # ------------------------------------------------------------------

    def _draw_flowline(
        self,
        ax: Any,
        tx: float,
        ty: float,
        hx: float,
        hy: float,
    ) -> None:
        """Draw a curved flowline from template (tx, ty) to host (hx, hy)."""
        xs = np.array([tx, tx + (hx - tx) * 0.5, hx])
        ys = np.array([ty, self.seabed_y - 0.02, hy - 0.03])
        from scipy.interpolate import make_interp_spline
        try:
            spline = make_interp_spline(
                [0, 0.5, 1], np.column_stack([xs, ys]), k=2
            )
            t_fine = np.linspace(0, 1, 60)
            pts = spline(t_fine)
            ax.plot(
                pts[:, 0],
                pts[:, 1],
                color="#e07b39",
                linewidth=1.4,
                transform=ax.transAxes,
                zorder=2,
            )
        except ImportError:
            # Fallback: straight line if scipy unavailable
            ax.plot(
                [tx, hx],
                [ty, hy],
                color="#e07b39",
                linewidth=1.4,
                transform=ax.transAxes,
                zorder=2,
            )

    def _draw_host(self, ax: Any, cx: float, cy: float) -> None:
        """Draw the appropriate host facility symbol."""
        if self.host_type in ("FPSO",):
            patch = make_fpso_patch(cx=cx, cy=cy)
        elif self.host_type in ("TLP", "SPAR"):
            patch = make_platform_patch(cx=cx, cy=cy - 0.02)
        else:
            patch = make_platform_patch(cx=cx, cy=cy - 0.02)
        ax.add_patch(_to_axes_patch(patch, ax))

        # Riser line from host down to seabed level
        ax.plot(
            [cx, cx],
            [cy - 0.02, self.seabed_y],
            color="#7f8c8d",
            linewidth=1.0,
            linestyle=":",
            transform=ax.transAxes,
            zorder=2,
        )


def _to_axes_patch(patch: mpatches.Patch, ax: Any) -> mpatches.Patch:
    """Set the transform of patch to axes-fraction space."""
    patch.set_transform(ax.transAxes)
    return patch
