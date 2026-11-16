"""Rectangular drag areas and element centres with caller-supplied masks.

Method extracted from a legacy engineering workbook. For column i and row j,
centres are L*((i+1/2)/nx-1/2), W*((j+1/2)/ny-1/2), z=0; areas are
mask_x*Ax/ny, mask_y*Ay/nx, and Az_gross*(1-p)/(nx*ny). SI units only.
The x direction follows length and y follows width. The origin is the plate
centre; elements are returned row by row, x increasing first, k=j*nx+i.

The source names DNV-RP-N103 (July 2017), Appendix B for drag, Appendix A
for added mass, 4.6.3.3 for a three-dimensional correction and section 4.6.4
for perforation. For this discretization method: clause not stated in source.
The equations here distribute supplied areas; they do not calculate forces.

Input gaps: grid count selection, lateral masks and their physical basis are
caller responsibilities; no empirical thresholds or lookup tables are supplied.
Lateral totals are Ax*sum(mask_x)/ny and Ay*sum(mask_y)/nx. Opposite face
masks can duplicate projected area, so masks are never normalized implicitly.
Only the vertical total is constrained to Az_gross*(1-p). No convergence,
shielding, tilt, sea-state response, mass distribution or model export is
established. Hydrodynamic coefficients remain in the existing rectangular
hydrodynamics solver; its physical applicability requires separate assessment.
"""
from dataclasses import dataclass
import math
from numbers import Real


def _nonnegative_number(name: str, value: float) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite nonnegative number")
    try:
        value = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{name} exceeds finite arithmetic range") from exc
    if not math.isfinite(value) or value < 0:
        raise ValueError(f"{name} must be a finite nonnegative number")
    return value


@dataclass(frozen=True)
class RectangularDragGrid:
    """Required SI geometry/areas, dimensionless counts, fraction and masks.

    x/y areas are already effective areas; perforation is applied only to the
    gross z area. Each mask is a dimensionless weight in [0,1] per element.
    """

    length_m: float
    width_m: float
    count_x: int
    count_y: int
    drag_area_x_m2: float
    drag_area_y_m2: float
    gross_drag_area_z_m2: float
    perforation_fraction: float
    mask_x: tuple[float, ...]
    mask_y: tuple[float, ...]

    def __post_init__(self) -> None:
        for name in ("count_x", "count_y"):
            count = getattr(self, name)
            if isinstance(count, bool) or not isinstance(count, int) or count <= 0:
                raise ValueError(f"{name} must be a positive integer")
        for name in ("length_m", "width_m", "drag_area_x_m2", "drag_area_y_m2",
                     "gross_drag_area_z_m2", "perforation_fraction"):
            object.__setattr__(self, name, _nonnegative_number(name, getattr(self, name)))
        if self.length_m == 0 or self.width_m == 0:
            raise ValueError("length_m and width_m must be positive")
        if self.perforation_fraction > 1:
            raise ValueError("perforation_fraction must be in [0,1]")
        for name in ("mask_x", "mask_y"):
            mask = getattr(self, name)
            if not isinstance(mask, (tuple, list)) or len(mask) != self.count_x * self.count_y:
                raise ValueError(f"{name} requires one entry per element")
            frozen_mask = tuple(_nonnegative_number(name, value) for value in mask)
            if any(value > 1 for value in frozen_mask):
                raise ValueError(f"{name} entries must be in [0,1]")
            object.__setattr__(self, name, frozen_mask)


@dataclass(frozen=True)
class DragElement:
    """A row-major plate-centre position and allocated drag areas in SI."""

    x_m: float
    y_m: float
    z_m: float
    drag_area_x_m2: float
    drag_area_y_m2: float
    drag_area_z_m2: float


def _scaled_area(area_m2: float, weight: float, count: int) -> float:
    value = area_m2 * weight / count
    if not math.isfinite(value) or (area_m2 > 0 and weight > 0 and value == 0):
        raise ValueError("positive allocated area is not representable")
    return value


def _centre(index: int, count: int, length_m: float) -> float:
    factor = (index + 0.5) / count - 0.5
    value = factor * length_m
    if not math.isfinite(value) or (factor != 0 and value == 0):
        raise ValueError("element position is not representable")
    return value


def _axis_centres(count: int, length_m: float) -> tuple[float, ...]:
    if length_m / count == 0:
        raise ValueError("positive cell spacing is not representable")
    centres = tuple(_centre(i, count, length_m) for i in range(count))
    if any(right <= left for left, right in zip(centres, centres[1:])):
        raise ValueError("adjacent element centres must remain distinct")
    return centres


def rectangular_drag_elements(grid: RectangularDragGrid) -> tuple[DragElement, ...]:
    """Allocate supplied areas with the module equations; no implicit masks."""
    count = grid.count_x * grid.count_y
    xs = _axis_centres(grid.count_x, grid.length_m)
    ys = _axis_centres(grid.count_y, grid.width_m)
    area_z = _scaled_area(grid.gross_drag_area_z_m2, 1-grid.perforation_fraction, count)
    elements = []
    for j in range(grid.count_y):
        for i in range(grid.count_x):
            k = j * grid.count_x + i
            elements.append(DragElement(
                x_m=xs[i],
                y_m=ys[j],
                z_m=0.0,
                drag_area_x_m2=_scaled_area(grid.drag_area_x_m2, grid.mask_x[k], grid.count_y),
                drag_area_y_m2=_scaled_area(grid.drag_area_y_m2, grid.mask_y[k], grid.count_x),
                drag_area_z_m2=area_z,
            ))
    return tuple(elements)
