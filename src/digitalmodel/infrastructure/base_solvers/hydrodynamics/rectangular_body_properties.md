# Rectangular-body property methods

The existing `DNVRPH103_hydrodynamics_rectangular` class in
`code_dnvrph103_hydrodynamics_rectangular.py` computes rectangular-body drag,
translational added mass, rotational drag area moments, added-inertia
coefficients, homogeneous-box mass inertia and corner coordinates. Independent
arithmetic tests are provided in
`tests/hydrodynamics/test_rectangular_body_hand_checks.py`. No additional
implementation or change to existing calculation behavior is introduced.

## Inputs and units

`inputs.l`, `inputs.w` and `inputs.h` are the local x/y/z dimensions in metres;
`inputs.m_air` and `inputs.m_water` are equivalent masses in kg. `rho_water`
is in kg/m³. The displaced volume is the mass difference divided by density,
rather than the gross box volume. Per-axis `perforation_ratio` values are
dimensionless fractions. Lookup ratios and coefficients are supplied by the
caller; the tests use invented analytic samples labelled illustrative example.
Mass/density boundaries can be converted through `digitalmodel.units.Q_`.

Translational results contain added masses in kg, projected areas in m² and
dimensionless drag/added-mass coefficients. Mass inertia is returned in kg·m².
The existing rotational hydrodynamic inertia method returns tonne·m²; it has an
internal mass conversion. Rotational drag area moments are in m⁵ and rotational
coefficients are dimensionless. These return units require separate treatment.

## Method and source-stated references

References below are those stated in a legacy engineering calculation. They are
traceability references, rather than an independent verification of standard
content, applicability or compliance. The existing implementation retains its
historical H103 name; no equivalence of standard editions is established.

| Property | Existing calculation | Reference stated in source |
|---|---|---|
| Steady drag | Piecewise-linear interpolation of supplied shape coefficients; projected face area is reduced by its supplied perforation fraction | DNV-RP-N103, July 2017, Appendix B: square rod parallel to flow; rectangular plate normal to flow direction |
| Oscillatory drag | Maximum of three times steady drag and the existing coefficient floor | DNV-RP-N103, items 4.6.2.2 and 4.6.2.4 |
| Plate added mass | `C * rho * pi * a² * b / 4`, with sorted face edges `a <= b` and interpolated `C` | DNV-RP-N103, Appendix A: rectangular plates |
| Finite body correction | Multiply plate added mass by `1 + sqrt((1 - lambda²)/(2*(1 + lambda²)))`, where `lambda = sqrt(a*b)/(c + sqrt(a*b))` and `c` is streamwise depth | DNV-RP-N103, 4.6.3.3; horizontal application is an adaptation stated in source |
| Perforated added mass | Existing piecewise unity, cosine and exponential reduction | DNV-RP-N103, section 4.6.4 |
| Rotational drag | Sum of the transverse face contributions `Cd * edge * lever_edge⁴ / 32` | Clause not stated in source |
| Rotational added inertia | Equivalent ellipsoid geometry and an existing sixth-order fit to caller-supplied upper/lower curve samples | Clause not stated in source |
| Rigid-body and displaced-fluid inertia | Homogeneous-box transverse squared-edge sum multiplied by mass and divided by twelve | Clause not stated in source |

Table 1. Property methods and references as stated in the source.

The finite-body correction is applied on each axis by the existing solver.
Its drag branch selects a plate when `c/sqrt(a*b) < 1` and a prism otherwise.
That automatic geometry branch is distinct from selecting shape-specific
curves for particular directions. Positive dimensions alone do not establish
that the chosen approximation applies.

## Limits and input gaps

The source evaluates printed, rounded rotational polynomials; the existing
class fits supplied curve ordinates. Exact numerical equivalence is not
established. The source also adds a parallel-axis term to its box inertia,
whereas the existing class returns the box estimate at the centre of mass.
The physical mass distribution and reference point require independent
resolution before substituting one result for the other.

No source clause is stated for the rotational polynomial coefficients,
rotational drag expression, inertia assumptions or section-mesh rules. The
underlying rotational curve ordinates are an input gap. No new coefficients,
lookup defaults or empirical constants are supplied by these additions.
The original standard text and the engineering validity of horizontal use of
the finite-body correction remain unverified.

The existing calculator lacks comprehensive positive-finite geometry,
displacement and perforation validation. Its lookup interpolator rejects
out-of-domain queries. Supplied-inertia validation is covered by
`tests/hydrodynamics/test_rectangular_buoy_inertia.py`. These existing checks
do not establish physical applicability. Caller validation and qualified
coefficient data remain necessary.

The public tests characterize arithmetic using independent hand formulas,
symmetry, limiting cases, branch boundaries and existing rejection behavior.
They do not constitute a force benchmark or standards qualification.
Section discretization, orientation-dependent model preparation, input-status
forms, text serialization, macro-driven automation and model execution remain
outside these additions. No source tables, private inputs or cached results
are included.
