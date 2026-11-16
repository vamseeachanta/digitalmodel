# Rectangular-body hydrodynamic method

The existing `DNVRPH103_hydrodynamics_rectangular` class computes translational
added mass and drag, rotational drag area moments, hydrodynamic inertia,
rotational added-inertia coefficients and homogeneous-box mass inertia.
Regression tests use illustrative example inputs and synthetic curves.
No coefficient table or operating case from a specific project is included.

Inputs are explicit configuration entries: `inputs.l`, `w`, `h` in metres,
`m_air` and `m_water` in kilograms, `rho_water` in kg/m3, per-axis
`perforation_ratio` as fractions and `cog` in metres. Displaced volume is
`(m_air-m_water)/rho_water`. Coefficient curves under `look_up` are caller
supplied. Existing converters in
`digitalmodel.hydrodynamics.diffraction.diffraction_units` convert tonnes,
density and inertia at the boundary. Internal masses and intrinsic inertia
use kg and kg.m2. The existing `get_hydrodynamic_intertia` method specifically
returns t.m2; consumers must preserve that output-unit distinction.

For each motion axis, transverse dimensions are sorted as `a <= b`, with axial
length `c`. Linear interpolation supplies the plate added-mass coefficient
`C(b/a)`. Reference volume is `pi*a**2*b/4`; the first added-mass estimate is
`rho*C*volume`. With `lambda=sqrt(a*b)/(c+sqrt(a*b))`, its three-dimensional
multiplier is `1+sqrt((1-lambda**2)/(2*(1+lambda**2)))`. The perforation method
applies an unchanged low-porosity branch, a cosine branch and an exponential
branch, using fractional porosity. Normalized added mass is divided by
displaced-fluid mass. Drag area is `a*b*(1-p)`. Drag uses the plate curve for
`c/sqrt(a*b)<1` and the prism curve otherwise; splash drag is
`max(3*deep_drag, 2.5)`.

Rotational drag area moments combine two faces; for example,
`Mx=(Cy*l*h**4+Cz*l*w**4)/32` in m5. Hydrodynamic inertia uses displaced mass
and the box transverse squared dimensions divided by twelve. Rotational
added-inertia coefficients use sixth-degree fits to caller-provided upper
and lower curves with equivalent radius `sqrt(transverse_area/pi)` and
half the axial length. The implemented branch switches at radius/half-length
`1.6`, applying the lower-curve geometric transform at or above that boundary.
Intrinsic box inertia is `mass*(edge_1**2+edge_2**2)/12` about the mass centre;
it does not acquire a parallel-axis addition when the coordinate origin moves.
Explicit principal moments can replace the box approximation and are validated
for finite positive values and triangle inequalities.

Cited attributions are DNV-RP-N103, July 2017:
appendix B for steady drag, 4.6.2.2 and 4.6.2.4 for the oscillatory
multiplier/floor rationale, appendix A for rectangular-plate added mass,
4.6.3.3 for the three-dimensional correction and section 4.6.4 for
perforation. These are source attributions, not a fresh verification of the
standard or a claim that every configuration is within its scope.

Input gaps and limits:

- Rotational fit data provenance, coefficient qualification and approximation
  accuracy are not established: no governing clause is cited. Curves remain
  required caller inputs to those methods; no new default curve is introduced.
- The 1.6 branch threshold in `get_newman_ca` has no cited basis; physical intent
  requires an engineering ruling.
- For the splash multiplier and floor, the cited clauses give a rationale;
  application to a particular geometry or flow condition still requires qualification.
- A prism-curve lookup outside the supplied prism curve range can return an error
  where the existing method selects a plate curve. Equality between the methods is not
  established in that case. Existing linear lookup interpolation rejects
  extrapolation; rotational polynomial fits do not enforce a domain guard.
- Box inertia is returned about the mass centre without a parallel-axis term;
  callers needing inertia about another origin must apply the shift and establish
  the mass distribution.
- Core geometry, displacement, density and porosity methods do not provide a
  complete validation boundary. Callers must establish positive finite geometry
  and density, positive displacement and a qualified porosity range.
- Deployment rotations, element partitioning, document checks, metadata,
  serialization and macro-driven model generation are outside these regressions.
  No automation or licensed solver is run.
