"""Regular rectangular-body surface cells, recovered from a legacy engineering workbook.

Inputs are SI dimensions (m), explicit integer cell counts, and a dimensionless
horizontal perforation fraction. Results are cell centres (m) relative to the
centre of the top plate, with z=0, and directional drag areas (m2). Counts are
caller choices: no empirical mesh-size selection rule is supplied.

For nx by ny cells, centres are ((i+1/2)/nx-1/2)*L and
((j+1/2)/ny-1/2)*W. At each opposing x edge, area_x=W*H/ny;
at each opposing y edge, area_y=L*H/nx; interior side areas are zero.
Every cell has area_z=L*W*(1-p)/(nx*ny). Thus total x and y areas are
TWICE their single projected areas, while z area is counted once. This is a
surface allocation convention; it does not establish a global drag-force
coefficient or a physically qualified mesh. Counts must both be at least two.

Reuse: translational drag/added mass, rotational drag moments, displaced-water
inertia and rotational coefficient fitting remain in
``infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular``.
That solver takes kg, m, kg/m3 and fractional perforation. Its hydrodynamic
inertia output uses tonne.m2; its structural inertia output uses kg.m2.
Interpolation uses caller-supplied tables. No source tables are embedded here.

Source-stated method references: DNV-RP-N103, July 2017, Appendix B,
4.6.2.2 and 4.6.2.4 for drag; Appendix A, 4.6.3.3 and section 4.6.4
for translational added mass and perforation. These citations describe the
source method, not independent verification of the standard text.
Surface discretization: clause not stated in source. The source also names
an OrcaFlex manual rectangular-box method; clause not stated in source.

Input gaps: grid-selection thresholds and a fixed row count have no stated
basis, so counts are required arguments. Rotational polynomial provenance is
incomplete. Existing solver fits and source rounded polynomials differ; existing
intrinsic box inertia and the source datum-dependent parallel-axis expression
also differ. A centre-of-gravity coordinate does not define mass distribution.
No engineering equivalence to those source expressions is asserted.

Limits/what is left: uniform, axis-aligned rectangular surface allocation only;
no rotation, shielding, side perforation, mesh convergence criterion, distributed
added mass, model export, or macro execution. Existing methods are reused
without altering their conventions. Unit conversion belongs at the boundary
through existing repository converters. No numerical coefficients are selected
by this module.
"""
from __future__ import annotations

from dataclasses import dataclass
import math
from numbers import Real


def _finite_number(value: float, name: str, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f'{name} must be a finite number')
    try:
        valid = math.isfinite(value) and (not positive or value > 0)
    except (OverflowError, TypeError):
        valid = False
    if not valid:
        raise ValueError(f'{name} must be finite' + (' and positive' if positive else ''))


@dataclass(frozen=True)
class RectangularSurfaceGrid:
    """Required geometry, cell counts and horizontal opening fraction."""

    length_m: float
    width_m: float
    height_m: float
    x_count: int
    y_count: int
    perforation_fraction: float

    def __post_init__(self) -> None:
        for name in ('length_m', 'width_m', 'height_m'):
            _finite_number(getattr(self, name), name, positive=True)
        _finite_number(self.perforation_fraction, 'perforation_fraction')
        if not 0 <= self.perforation_fraction <= 1:
            raise ValueError('perforation_fraction must lie in [0, 1]')
        for name in ('x_count', 'y_count'):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, int) or value < 2:
                raise ValueError(f'{name} must be an integer at least two')


@dataclass(frozen=True)
class SurfaceCell:
    """Cell centre and directional areas; side areas include opposing faces."""

    x_m: float
    y_m: float
    z_m: float
    area_x_m2: float
    area_y_m2: float
    area_z_m2: float


def rectangular_surface_cells(geometry: RectangularSurfaceGrid) -> tuple[SurfaceCell, ...]:
    """Return cells in row-major order, x increasing first, then y.

    Arithmetic that cannot retain positive finite areas or distinct cell centres
    raises ValueError. See the module formula and opposing-face convention.
    """
    nx, ny = geometry.x_count, geometry.y_count
    try:
        dx, dy = geometry.length_m / nx, geometry.width_m / ny
        ax = geometry.width_m * geometry.height_m / ny
        ay = geometry.length_m * geometry.height_m / nx
        full_az = geometry.length_m * geometry.width_m / nx / ny
        az = full_az * (1 - geometry.perforation_fraction)
        derived = (dx, dy, ax, ay, full_az)
        if not all(math.isfinite(v) and v > 0 for v in derived):
            raise ValueError('geometry is not representable at this cell resolution')
        if az == 0 and geometry.perforation_fraction < 1:
            raise ValueError('perforated area is not representable')
        xs = tuple((i - (nx - 1) / 2) * dx for i in range(nx))
        ys = tuple((j - (ny - 1) / 2) * dy for j in range(ny))
        for coordinates in (xs, ys):
            if (not all(math.isfinite(v) for v in coordinates)
                    or any(b <= a for a, b in zip(coordinates, coordinates[1:]))):
                raise ValueError('cell centres are not representable as distinct values')
    except OverflowError as exc:
        raise ValueError('geometry is not representable at this cell resolution') from exc
    return tuple(
        SurfaceCell(
            x_m=xs[i],
            y_m=ys[j],
            z_m=0.0,
            area_x_m2=ax if i in (0, nx - 1) else 0.0,
            area_y_m2=ay if j in (0, ny - 1) else 0.0,
            area_z_m2=az,
        )
        for j in range(ny) for i in range(nx)
    )
