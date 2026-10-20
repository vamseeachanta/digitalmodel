# Wave-scaled free-surface refinement

The hull case builder can hold its finest in-plane resolution over a stated
wake length. Set `HullCaseConfig.free_surface_wake_wavelengths` to the required
number of wavelengths aft of the actual stern. The same speed now determines
both cell size and the extent where that cell size is provided. The criterion
has no assumed minimum: omitting it records `NOT_EVALUATED` in
`mesh.free_surface.extent` in `case_provenance.json` and retains the existing
hull refinement. A production analysis must supply and justify its criterion.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-20 | Added wavelength-scaled coverage, a worked example and limits to make the resolution domain explicit. |

## Inputs and method

Inputs are the existing hull manifest, speed, domain dimensions, cell-resolution
criterion and the additional wake length in wavelengths. The geometry uses the
manifest coordinates; it is not translated to a presumed origin.

The public paper [Ship wakes: Kelvin or Mach angle? (2013), pp. 1–2](https://arxiv.org/abs/1304.2653)
gives the transverse deep-water wavelength and Kelvin envelope:

\[
\lambda = \frac{2\pi U^2}{g},\qquad
\sin\alpha_K = \frac{1}{3},\qquad \tan\alpha_K=\frac{1}{\sqrt{8}}.
\]

For the caller's requested count \(N\), stern coordinate \(x_s\), bow coordinate
\(x_b\), and hull half width \(b\), the refined region reaches
\(x_a=x_s-N\lambda\). Its lateral boundary is
\(y_{\min}(x)=-b-(x_b-x)/\sqrt{8}\), from bow to aft end. This is a tapered
Kelvin envelope with finite width at the bow, covering waves originating along
the hull. The angle is approximately 19.5 degrees; the exact trigonometric
relation is used in the calculation. The wavelength uses the builder's existing
standard gravity, consistently with its Froude number.

Each stage selects the union of its original hull box and a closed trapezoidal
prism. The prism has the same vertical limits as that stage's box, so every
stage covers the requested wake and the selections stay nested. The emitted
`surfaceToCell` action includes inside cells and intersecting cells; it adds to
the hull selection instead of replacing it. Its syntax follows the public
[OpenFOAM 10 (2022) source, dictionary constructor](https://github.com/OpenFOAM/OpenFOAM-10/blob/master/src/meshTools/sets/cellSources/surfaceToCell/surfaceToCell.C).

The builder refuses a criterion that crosses the outlet or lateral boundary
before writing files. The error names `downstream_lpp` or `lateral_lpp`. It
does not enlarge a caller-stated domain. The cell budget uses nested enclosing
boxes of the unions: an upper bound within the existing uniform-z cell
estimate. This can refuse a cap that a more exact polygon-volume estimate could
satisfy. The original hull boxes and achieved cell size remain in provenance;
the additional extent entry records the criterion, achieved reach, geometry,
source, budget basis and limits.

## Worked example

Run `PYTHONPATH=src python examples/openfoam/wave_extent.py` in an environment
with the package dependencies. This only derives inputs; it does not read an
STL, mesh or start a solver. All dimensions and criteria are synthetic inputs:
length 6.0 m, beam 0.9 m, draft 0.3 m, stern at −0.05 m, bow at 6.15 m, and
two wavelengths required aft of the stern. They are not recommended limits.

| Speed (m/s) | Wavelength (m) | Wake aft of stern (m) | Aft half width (m) | Achieved wavelengths |
|---|---|---|---|---|
| 1.0 | 0.6 | 1.3 | 3.1 | 2.0 |
| 2.0 | 2.6 | 5.1 | 4.5 | 2.0 |

Hand calculation for the first row: substitute \(U=1\) and \(N=2\), giving
\(\lambda=2\pi/g\), aft reach \(4\pi/g\), and half width
\(0.45+(6.20+4\pi/g)/\sqrt{8}\). Doubling speed quadruples wavelength and
reach while the hull stays fixed. Tests use these substitutions independently
of the production helpers, and inspect all emitted prism surfaces and stages.

## Limits and way forward

This is a pre-mesh geometric coverage check under linear deep-water theory.
It does not establish finite-depth applicability, sufficient vertical wave
amplitude coverage, mesh quality, absorption, radiation or resistance accuracy.
The selected region may grow to include whole cells at its boundary. Native
meshing and wave-propagation studies are still required before using results
as an accuracy claim. Missing wake criteria remain `NOT_EVALUATED`; they are
not silently promoted to a coverage pass. Related work on layer sizing,
absorption and vertical refinement transitions remains separate.
