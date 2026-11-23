#!/usr/bin/env python3
"""
ABOUTME: Partial-fill initialisation helpers for VOF (interFoam) tank cases
(#659). Turns a fractional fill level into a ``system/setFieldsDict`` that fills
``alpha.water`` from the tank bottom up to the fill height, and snaps that height
onto a mesh cell face so the still-water free surface never bisects a cell.

VOF gotcha
----------
interFoam represents the interface with the phase fraction ``alpha.water``. If
the still-water level ``h = fill_level * tank_height`` lands in the interior of a
cell, that cell starts at a fractional alpha and the sharpened interface snaps to
the nearest face on the first step, perturbing the initial condition (and the
initial volume). To keep the interface exactly on a face we require ``h`` to be an
integer number of vertical cells; :func:`snap_fill_to_cell_face` rounds to the
nearest face and warns when it had to move the requested fill level.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import List, Sequence, Tuple

from loguru import logger

Vec3 = Tuple[float, float, float]


# ---------------------------------------------------------------------------
# Snapping the free surface onto a cell face
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class FillSnap:
    """Result of snapping a fractional fill level onto a mesh cell face.

    Attributes:
        requested_fill_level: The fill level the caller asked for (fraction 0-1).
        fill_level: The snapped fill level actually used (fraction 0-1).
        fill_height: Still-water fill height ``h`` (m) = ``fill_level*tank_height``.
        n_fill_cells: Number of vertical cells filled with liquid.
        n_cells: Total vertical cell count.
        tank_height: Internal tank height (m) in the vertical direction.
        cell_height: Vertical cell size ``tank_height / n_cells`` (m).
        adjusted: True if snapping moved the requested fill level.
    """

    requested_fill_level: float
    fill_level: float
    fill_height: float
    n_fill_cells: int
    n_cells: int
    tank_height: float
    cell_height: float
    adjusted: bool

    @property
    def adjustment(self) -> float:
        """Signed change applied to the fill level by snapping (snapped - requested)."""
        return self.fill_level - self.requested_fill_level


def snap_fill_to_cell_face(
    tank_height: float,
    n_cells: int,
    fill_level: float,
    *,
    warn: bool = True,
) -> FillSnap:
    """Snap a fractional fill level so the free surface lands on a cell face.

    Args:
        tank_height: Internal tank height (m) in the vertical direction (> 0).
        n_cells: Number of mesh cells across the tank height (>= 1).
        fill_level: Requested fill level as a fraction of ``tank_height`` (0-1).
        warn: Emit a ``logger.warning`` when the fill level had to be adjusted.

    Returns:
        A :class:`FillSnap` with the snapped fill level, fill height and the
        integer number of filled cells.

    Raises:
        ValueError: If ``tank_height`` <= 0, ``n_cells`` < 1, or ``fill_level``
            is outside ``[0, 1]``.
    """
    if tank_height <= 0.0:
        raise ValueError(f"tank_height must be positive, got {tank_height}")
    if n_cells < 1:
        raise ValueError(f"n_cells must be >= 1, got {n_cells}")
    if not 0.0 <= fill_level <= 1.0:
        raise ValueError(f"fill_level must be in [0, 1], got {fill_level}")

    dy = tank_height / n_cells
    n_fill = int(round(fill_level * n_cells))
    n_fill = max(0, min(n_cells, n_fill))
    h = n_fill * dy
    snapped_level = h / tank_height
    adjusted = abs(snapped_level - fill_level) > 1e-9

    if adjusted and warn:
        logger.warning(
            "Partial-fill level {:.4f} snapped to {:.4f} "
            "({} of {} vertical cells, h={:.4f} m) so the free surface lands on "
            "a cell face; increase the vertical cell count for a finer fill.",
            fill_level,
            snapped_level,
            n_fill,
            n_cells,
            h,
        )

    return FillSnap(
        requested_fill_level=fill_level,
        fill_level=snapped_level,
        fill_height=h,
        n_fill_cells=n_fill,
        n_cells=n_cells,
        tank_height=tank_height,
        cell_height=dy,
        adjusted=adjusted,
    )


# ---------------------------------------------------------------------------
# setFieldsDict geometry + rendering
# ---------------------------------------------------------------------------


def partial_fill_box(
    min_coords: Sequence[float],
    max_coords: Sequence[float],
    fill_height: float,
    *,
    vertical_axis: int = 2,
    pad: float | None = None,
) -> Tuple[Vec3, Vec3]:
    """Corner points of a ``boxToCell`` that fills the tank to ``fill_height``.

    The box spans the full tank extent (with a small pad) in the two horizontal
    directions and runs from below the tank floor up to ``fill_height`` along the
    vertical axis, so every cell whose centre is below the still-water level is
    initialised to liquid.

    Args:
        min_coords: ``[xmin, ymin, zmin]`` of the tank bounding box (m).
        max_coords: ``[xmax, ymax, zmax]`` of the tank bounding box (m).
        fill_height: Still-water fill height ``h`` (m) measured from the floor.
        vertical_axis: Index of the vertical axis (0=x, 1=y, 2=z).
        pad: Horizontal/below-floor margin (m); defaults to the largest tank
            extent so the box comfortably brackets the mesh.

    Returns:
        ``(box_min, box_max)`` corner tuples for the ``boxToCell`` region.
    """
    if vertical_axis not in (0, 1, 2):
        raise ValueError(f"vertical_axis must be 0, 1 or 2, got {vertical_axis}")
    lo = [float(c) for c in min_coords]
    hi = [float(c) for c in max_coords]
    if pad is None:
        pad = max(hi[i] - lo[i] for i in range(3))

    box_min: List[float] = [0.0, 0.0, 0.0]
    box_max: List[float] = [0.0, 0.0, 0.0]
    for ax in range(3):
        if ax == vertical_axis:
            box_min[ax] = lo[ax] - pad
            box_max[ax] = lo[ax] + fill_height
        else:
            box_min[ax] = lo[ax] - pad
            box_max[ax] = hi[ax] + pad
    return (box_min[0], box_min[1], box_min[2]), (box_max[0], box_max[1], box_max[2])


def _fmt(x: float) -> str:
    return f"{x:.6g}"


def _vec(v: Sequence[float]) -> str:
    return f"({_fmt(v[0])} {_fmt(v[1])} {_fmt(v[2])})"


def render_set_fields_dict_body(
    box_min: Sequence[float],
    box_max: Sequence[float],
    *,
    field: str = "alpha.water",
    inside_value: float = 1.0,
    default_value: float = 0.0,
) -> str:
    """Return ``setFieldsDict`` entries (no FoamFile header) for a partial fill.

    The domain defaults to ``default_value`` (air) and a single ``boxToCell``
    sets ``inside_value`` (liquid) inside the fill box.
    """
    return (
        f"defaultFieldValues ( volScalarFieldValue {field} {_fmt(default_value)} );\n"
        "regions\n"
        "(\n"
        "    boxToCell\n"
        "    {\n"
        f"        box {_vec(box_min)} {_vec(box_max)};\n"
        f"        fieldValues ( volScalarFieldValue {field} {_fmt(inside_value)} );\n"
        "    }\n"
        ");\n"
    )
