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
| 2 | 2026-10-22 | Corrected selection seeds and depth applicability; aligned references with the template version and quantified estimated cell cost. |

## Inputs and method

Inputs are the existing hull manifest, speed, domain dimensions, cell-resolution
criterion and the additional wake length in wavelengths. The geometry uses the
manifest coordinates; it is not translated to a presumed origin.

Rabaud & Moisy, [Ship wakes: Kelvin or Mach angle?, Phys. Rev. Lett. 110,
214503 (2013), pp. 1–2](https://arxiv.org/abs/1304.2653), gives the transverse
deep-water wavelength and discusses the Kelvin envelope:

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
standard gravity, consistently with its Froude number. The Kelvin angle is a
bounding modelling choice, rather than a prediction of the dominant wake angle:
Rabaud & Moisy find that the dominant angle narrows at high Froude number.
The classical construction is also given in Newman, *Marine Hydrodynamics*,
40th anniversary edition (2018), section 6.10.

The domain water depth is \(h=z_{waterline}-z_{bottom}\). The builder records
`water_depth_m` and `depth_wavelength_ratio` and screens for
\(h/\lambda \geq 0.5\), following Newman, sections 6.2–6.3, pp. 253–255
([open edition](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/11452/11452.pdf?dl=1)).
When \(h<\lambda/2\), requested geometry is still emitted, but extent status is
`NOT_EVALUATED` with a finite-depth reason. A `PASS` therefore requires both
geometric fit and this depth screen; no finite-depth wavelength is calculated.

Each stage selects the union of its original hull box and a closed trapezoidal
prism. The prism has the same vertical limits as that stage's box, so every
stage covers the requested wake and the selections stay nested. The emitted
`surfaceToCell` action includes inside cells and intersecting cells; it adds to
the hull selection instead of replacing it. Its syntax follows the public
[OpenFOAM v2312 source, dictionary constructor, lines 431–461](https://api.openfoam.com/2312/surfaceToCell_8C_source.html#l00431),
matching the templates. The outside seed lies ahead of the prism and inside
a mesh cell. Its coordinates use the emitted divisions, with fractional cell
offsets from the inlet, side and lower free-surface band plane to avoid
background and dyadic refinement faces, including the waterline.

The builder refuses a criterion that crosses the outlet or lateral boundary
before writing files. The error names `downstream_lpp` or `lateral_lpp`. It
does not enlarge a caller-stated domain. The cell budget uses nested enclosing
boxes of the unions within the existing uniform-z cell estimate.
This is an estimate, not an upper bound on native mesh cell counts:
`includeCut true` can add cells up to one cell beyond the prism in every
direction, including z, and the budget boxes do not pad for that growth.
The enclosing boxes can also overestimate the polygon volume and refuse a cap
that a more exact polygon-volume estimate could satisfy. The original hull
boxes and achieved cell size remain in provenance;
the additional extent entry records the criterion, achieved reach, geometry,
source, budget basis and limits.

## Worked example

Run `PYTHONPATH=src python examples/openfoam/wave_extent.py` in an environment
with the package dependencies. This only derives inputs; it does not read an
STL, mesh or start a solver. All dimensions and criteria are synthetic inputs:
length 6.0 m, beam 0.9 m, draft 0.3 m, stern at −0.05 m, bow at 6.15 m, and
two wavelengths required aft of the stern. They are not recommended limits.

| Speed (m/s) | Wavelength (m) | Wake aft of stern (m) | Aft half width (m) | Achieved wavelengths | Depth / wavelength |
|---|---|---|---|---|---|
| 1.0 | 0.6 | 1.3 | 3.1 | 2.0 | 23.9 |
| 2.0 | 2.6 | 5.1 | 4.5 | 2.0 | 6.0 |

Table 1. Synthetic geometry with N = 2.0 as a caller modelling choice, not an
acceptance recommendation. Both depths satisfy the half-wavelength screen.

| Speed (m/s) | Hull-only estimate (million cells) | With wake (million cells) | Cost ratio |
|---|---|---|---|
| 1.0 | 5.7 | 19.1 | 3.3 |
| 2.0 | 1.6 | 10.8 | 6.7 |

Table 2. Estimated cell cost for the same synthetic geometry and caller choice
N = 2.0. Ratios use the unrounded estimates. Every stage's wave prism spans
its hull box's full z-band, which increases cost even away from the waterline.
These are pre-mesh estimates; cut-cell growth is excluded.

Hand calculation for the first row: substitute \(U=1\) and \(N=2\), giving
\(\lambda=2\pi/g\), aft reach \(4\pi/g\), and half width
\(0.45+(6.20+4\pi/g)/\sqrt{8}\). Doubling speed quadruples wavelength and
reach while the hull stays fixed. Tests use these substitutions independently
of the production helpers, and inspect all emitted prism surfaces and stages.

## Limits and way forward

This is a pre-mesh geometric coverage check under linear deep-water theory.
The depth screen does not establish finite-depth wave propagation, sufficient
vertical wave amplitude coverage, mesh quality, absorption, radiation or
resistance accuracy.
The selected region may grow to include whole cells at its boundary. Native
meshing and wave-propagation studies are still required before using results
as an accuracy claim. Missing wake criteria remain `NOT_EVALUATED`; they are
not silently promoted to a coverage pass. Related work on layer sizing,
absorption and vertical refinement transitions remains separate.
