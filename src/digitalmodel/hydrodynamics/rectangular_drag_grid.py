"""Distribute rectangular-body projected drag areas over a regular planar grid.

Inputs are SI lengths (m), a dimensionless open-area fraction and explicit
integer cell counts. Coordinates are cell centres, with the grid centred at
x=y=z=0; x follows length and y follows width. Cells are ordered by increasing
y, then increasing x. Only boundary columns carry x-normal drag area; only
boundary rows carry y-normal drag area. Both opposed vertical faces are
represented, so their sums are twice the corresponding single-face projection.
The horizontal projection is represented once, reduced by its open-area fraction.

Method: dx=L/nx, dy=W/ny; centres are (-L/2+(i+1/2)dx,
-W/2+(j+1/2)dy,0). Boundary face areas are WH/ny and LH/nx;
each horizontal area is LW(1-p)/(nx*ny). This is geometric distribution only,
not a fluid-force model. Coefficients are computed by the existing rectangular
hydrodynamics implementation, not by this module.

Source: a legacy engineering workbook. Its companion hydrodynamic methods cite
DNV-RP-N103 (July 2017), appendix B, items 4.6.2.2 and 4.6.2.4; appendix A,
4.6.3.3 and section 4.6.4. For this grid rule: clause not stated in source.
No standard qualification of the distribution is established by that citation.

Input gaps: element counts require caller justification; the source's automatic
size cutoffs have no stated basis and are not encoded. No drag-table constants,
fit coefficients or fluid properties are assumed. Limits: at least two cells
per axis, positive finite representable geometry, horizontal projection only.
Subnormal lengths, spacings and positive areas are unsupported.
Deployment rotation, coefficients, mass/inertia distribution, solver input-file
generation and environmental loading are left to the existing application.
"""

from dataclasses import dataclass
import math
import sys


def _finite_number(name: str, value: float) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f'{name} must be a finite number')
    try:
        finite = math.isfinite(value)
    except OverflowError:
        finite = False
    if not finite:
        raise ValueError(f'{name} must be a finite number')


@dataclass(frozen=True)
class RectangularDragGrid:
    """Required geometry in m, dimensionless perforation and integer counts."""

    length_m: float
    width_m: float
    height_m: float
    perforation_fraction: float
    elements_x_count: int
    elements_y_count: int

    def __post_init__(self) -> None:
        for name in ('length_m', 'width_m', 'height_m'):
            value = getattr(self, name)
            _finite_number(name, value)
            if value <= 0:
                raise ValueError(f'{name} must be positive')
        _finite_number('perforation_fraction', self.perforation_fraction)
        if not 0 <= self.perforation_fraction <= 1:
            raise ValueError('perforation_fraction must be between zero and one')
        for name in ('elements_x_count', 'elements_y_count'):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, int) or value < 2:
                raise ValueError(f'{name} must be an integer of at least two')


@dataclass(frozen=True)
class DragElement:
    """One cell's position in m and projected areas in m2."""

    element_number: int
    x_m: float
    y_m: float
    z_m: float
    area_x_m2: float
    area_y_m2: float
    area_z_m2: float


def distribute_rectangular_drag(grid: RectangularDragGrid) -> tuple[DragElement, ...]:
    """Return cell centres and areas using the geometry formulas above.

    Counts are supplied rather than selected from undocumented size thresholds.
    The two-face vertical-area convention shall be retained by consumers;
    summing these areas as a single-body projected area would double it.
    """
    nx, ny = grid.elements_x_count, grid.elements_y_count
    try:
        length, width, height = map(float, (grid.length_m, grid.width_m, grid.height_m))
        dx, dy = length / nx, width / ny
        face_x = width * height
        face_y = length * height
        face_z = length * width
        ax, ay = face_x / ny, face_y / nx
        az = face_z * (1 - grid.perforation_fraction) / (nx * ny)
    except OverflowError as exc:
        raise ValueError('grid geometry must be representable') from exc
    # The normal-number floor is a floating-point property, not a design limit.
    # Reject subnormal partitions whose rounding can collapse centres or areas.
    if any(not math.isfinite(v) or v < sys.float_info.min
           for v in (length, width, height, dx, dy, face_x, face_y, face_z,
                     ax, ay)):
        raise ValueError('grid geometry must be representable')
    if not math.isfinite(az) or (az < sys.float_info.min and grid.perforation_fraction < 1):
        raise ValueError('grid horizontal area must be representable')
    elements = tuple(
        DragElement(
            element_number=j * nx + i + 1,
            x_m=-length / 2 + (i + 0.5) * dx,
            y_m=-width / 2 + (j + 0.5) * dy,
            z_m=0.0,
            area_x_m2=ax if i in (0, nx - 1) else 0.0,
            area_y_m2=ay if j in (0, ny - 1) else 0.0,
            area_z_m2=az,
        )
        for j in range(ny) for i in range(nx)
    )
    # Validate the returned rounded values, rather than ideal face totals.
    try:
        totals = [math.fsum(getattr(e, name) for e in elements)
                  for name in ('area_x_m2', 'area_y_m2', 'area_z_m2')]
    except OverflowError as exc:
        raise ValueError('distributed area totals must be representable') from exc
    if any(not math.isfinite(total) for total in totals):
        raise ValueError('distributed area totals must be representable')
    return elements
