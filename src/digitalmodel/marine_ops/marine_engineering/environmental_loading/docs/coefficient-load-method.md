# Signed environmental coefficient loads

The existing `OCIMFDatabase` and `EnvironmentalForces` in `ocimf.py` compute signed wind and current forces and yaw moments from tabulated coefficients.

Supply speed in m/s, density in kg/m³, projected frontal and lateral areas in m²,
and reference length in m. The coefficient database uses heading in degrees and
displacement in tonnes. Coefficients are dimensionless signed values. Supply
all projected areas and both densities explicitly; the class defaults and
estimated areas are simplified estimates; supply measured projected areas for design use.

For each wind or current component, dynamic pressure is `q = rho * speed² / 2`.
The method returns `Fx = q * frontal_area * Cx`,
`Fy = q * lateral_area * Cy` in N and
`Mz = q * lateral_area * length * Cm` in N·m. Wind and current use their own
headings and speeds. Their three signed components are summed independently.
Regular coefficient surfaces use bilinear interpolation; a single populated
axis uses piecewise linear interpolation. Unit conversion at the boundary can
use the existing `digitalmodel.units.Q_` registry; calculations remain in SI.

Coefficient tables are commonly taken from OCIMF publications (MEG 3, MEG 4).
No edition, clause or table is cited or checked here; callers supply qualified coefficients.

Input gaps and limits:

- Coefficient provenance, edition, validity ranges, heading/force sign convention
  and choice of reference areas and length require caller evidence.
- The existing database folds headings to 0–180 degrees without reversing
  lateral force or yaw sign. Resolve the physical quadrant convention upstream.
- Out-of-range queries warn and clamp (no extrapolation).
- The database checks required columns but does not comprehensively reject
  nonfinite data, conflicting rows, or physically invalid geometry and speeds.
  Required-column checks do not establish full physical input validation.
- The database is indexed by displacement in tonnes; tables indexed by another
  parameter (for example water-depth/draft ratio) must be selected or
  interpolated by the caller before use.
- No missing density, coefficient or threshold is inferred.
