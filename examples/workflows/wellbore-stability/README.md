# Wellbore stability screening

Run from the repository root with the project's Python environment:

```sh
PYTHONPATH=src uv run python -m digitalmodel.well.drilling.wellbore_stability \
  examples/workflows/wellbore-stability/input.yml \
  --output-dir results/wellbore-stability
```

This standalone entry point uses NumPy and YAML and does not import the engine
or a licensed solver. The example is a synthetic two-depth profile. This is
screening only. Every output calculation path carries `NOT_VALIDATED`; the
two reference tests below qualify only their specified numerical comparisons.

`summary.json` contains inputs, assumptions, numerical resolutions, nominal
bounds, four-curve percentile data, quantile-constrained windows and trajectory data.
`window.json` and `polar.json` expose the same subsets for plotting. They contain
no local environment paths. Pressures are psi, TVD is feet, EMW is ppg using
`pressure / (0.052 * TVD)`. The pressure datum is surface gauge pressure for an
onshore single fluid column; offshore datums need a separate qualified model.

## Model and conventions

Compression is positive. Sv is a vertical principal stress; SHmax and Shmin
are horizontal principal stresses, with SHmax >= Shmin. The geographical frame
is North/East/down. Azimuths run clockwise from north; inclination runs from
down vertical (0 through 180 degrees). The right-handed well frame has z along
the trajectory, x toward the high side and y horizontal to the right. At a
vertical well, x follows the input well azimuth. Wall theta runs from x toward y.
Stress rotation preserves all three total principal stresses.

Effective far-field stress is `s = S - Pp I`, with unit Biot coefficient. At the
impermeable circular wall, using `dP = Pmud - Pp`:

```text
A = (sxx - syy) cos(2 theta) + 2 sxy sin(2 theta)
radial = dP
hoop = sxx + syy - 2 A - dP
axial = szz - 2 nu A
hoop_axial_shear = 2 (syz cos(theta) - sxz sin(theta))
radial_hoop_shear = radial_axial_shear = 0
```

The eigenvalues of the complete symmetric wall tensor give sigma1 >= sigma2
>= sigma3. Shear failure margins are strength minus demand:

```text
Mohr-Coulomb: UCS + q sigma3 - sigma1,  q = (1 + sin(phi))/(1 - sin(phi))
Mogi-Coulomb: a + b (sigma1 + sigma3)/2 - tau_oct
tau_oct = sqrt((sigma1-sigma2)^2 + (sigma2-sigma3)^2 + (sigma3-sigma1)^2)/3
a = sqrt(2)/3 UCS (1 - sin(phi)),  b = 2 sqrt(2)/3 sin(phi)
```

Mogi parameters are inferred from conventional triaxial equivalence, rather
than calibrated true-triaxial tests. Both criteria, including their extension
to tensile confinement, remain screening assumptions.
The mapping follows by setting sigma2 = sigma3, so that
`tau_oct = sqrt(2)/3 * (sigma1 - sigma3)`, then substituting the
Mohr-Coulomb triaxial relation `sigma1 = UCS + q * sigma3` and matching
the constant and confinement coefficients. Numerically singular friction
angles whose sine rounds to one are rejected before either criterion runs.

Fracture initiation occurs when the least eigenvalue of the hoop/axial block
reaches minus tensile strength. Radial support is a boundary traction and is
excluded from this drilling-induced tensile-fracture test. The model includes
hoop/axial shear. Its Schur-complement root is evaluated at each wall angle;
an axial stress already at or below the tensile limit produces an explicit
`ALREADY_TENSILE_AT_ZERO` result. Initiation is distinct from propagation or
loss pressure. Shmin is a separate, conservative upper constraint.

## Collapse search and numerical limits

Wall eigenvalues repeat every 180 degrees. `angular_samples` covers that
half-circle uniformly, without duplicating its endpoint. The allowed breakout
width is the aggregate failed angular extent per half-circle, equal to one
lobe's width for the usual two-lobe vertical breakout. The supported range is
0 to 60 degrees, a deliberately narrow elastic-screening limit.

At every wall angle, the solver scans pressures from Pp to tensile initiation
and refines the first transition from shear failure to stability. Collapse is
the pressure by which all but the allowed number of angular nodes have first
stabilized. Its floor is Pp; `FLOOR_AT_PORE_PRESSURE` does not establish the
true collapse threshold below Pp. The width allowance is rounded down to whole
angular nodes. Pressure roots are refined within 1e-6 psi, independently of
the much coarser angular and pressure scanning resolutions.

A separate `shear_upper` marks the first renewed local shear failure after
initial stability. It conservatively permits no renewed shear even when a
nonzero initial breakout width is allowed. This upper bound may be below the
collapse bound: the implementation preserves both numbers and reports the
window as infeasible. `stable_pressure_intervals_psi` separately records the
intervals satisfying the actual sampled width criterion.

If no recurrence is found, `shear_upper` retains the tensile search ceiling
and reports `NO_SHEAR_RECURRENCE_BEFORE_TENSILE_CEILING`; it does not claim
a shear root was found there.

A scan can miss a stable interval narrower than its pressure spacing. If too
many wall points never stabilize at the sampled pressures, collapse is null
with `NO_FEASIBLE_SAMPLED_PRESSURE`. This is not a proof that no solution exists.
If fracture is at or below Pp, collapse is null with that explicit reason.
Increase angular and pressure samples and check convergence before relying on
any screen. Deviated cases, nonzero breakout allowance and Mogi-Coulomb remain
`NOT_VALIDATED`, including when mathematical regression tests pass.

## Uncertainty and plotting

The four curves are pore pressure, collapse, tensile initiation, and Shmin.
Each includes P10/P50/P90 in psi and EMW, with the same low/median/high meanings
as the pore-pressure module. Here bands are empirical quantiles of seeded
normal input draws; they are not the pore module's analytical normal-output
quantiles. `ensemble.sigma` specifies independent input standard deviations
in psi or degrees, except dimensionless Poisson ratio. Unspecified sigmas are
zero. All four curves share each realization; identical standardized draws
are reused at every depth. This imposes perfect depth correlation per input
field and assumes independence between fields. It is an explicit assumption,
not measured geological uncertainty. Invalid physical draws raise an error
without clipping or rejection sampling.

Unresolved draws are counted and invalidate the affected bands; they are never
discarded to obtain apparently better quantiles. Infeasible draws are counted
separately. The lower limit is max(pore P90, collapse P90). The ceiling
is P10 of the per-draw minimum of tensile initiation, Shmin and the additional
shear upper bound. These marginal quantile limits are not a joint 80% safety
guarantee. No operational margin, ECD or mud recommendation is provided.

Polar rows use nominal inputs at the selected profile index. The equal-angle
stereographic radius is `tan(inclination/2)`; azimuth is clockwise from north.
Use inclinations below 180 degrees for a finite projection. Each row includes
collapse, fracture, shear upper, feasibility, and numerical/validation status.
No uncertainty band is implied for polar data.

## Published worked-example checks and citations

The selected open textbook is [*Introduction to Energy Geomechanics*](https://dnicolasespinoza.github.io/IPG.html), University
of Texas at Austin, Department of Petroleum and Geosystems Engineering,
edition dated August 28, 2024, copyright 2021. Its published pagination is
numbered HTML pages. The following citations identify those web pages,
sections, problem numbers and equation numbers; no printed page numbers are
asserted. Only numerical inputs and reference results are retained here.
The title page and both worked examples were checked from public HTML on
2026-10-17; all returned HTTP 200. `sources.yaml` records the locators,
response digests, retrieval times, assumptions and tolerances. The two open
worked problems are the narrower interpretation of the issue's textbook and
page-citation requirement; no access to a licensed textbook was assumed.

| Check | Published page and locator | Published result | Test tolerance |
|---|---|---|---|
| Collapse initiation | [Web page node46.html](https://dnicolasespinoza.github.io/node46.html), section 6.3, Problem 6.1, equation 6.9 | 4,279 psi | 2 psi absolute |
| Tensile initiation | [Web page node47.html](https://dnicolasespinoza.github.io/node47.html), section 6.4, Problem 6.3, equation 6.18, zero thermal stress | 4,320 psi | 0.01 psi absolute |

Both worked problems use TVD 7,000 ft, total SHmax 6,300 psi, total Shmin
4,300 psi, hydrostatic Pp 3,080 psi, UCS 3,500 psi, internal friction
coefficient 0.6 (`phi = atan(0.6)`), and tensile strength 800 psi. They are two
separate worked problems sharing one input set, rather than independent
datasets. The collapse text rounds q to 3.12; the code retains full precision
and gives about 4,279.20 psi. The quoted pressure is the reference: the text's
rounded water-density conversion differs from the repository's 0.052 EMW
constant, so its ppg values are not used as exact reference values.

The source's closed forms assume the governing hoop/radial stress ordering.
The tests explicitly complete the omitted Sv and Poisson ratio as 7,000 psi
and 0.25, respectively, and use a vertical well. The full wall-tensor screen
also finds renewed shear near 3,706 psi at another sector, before collapse is
relieved near 4,279 psi. Thus reproducing these two published pressure bounds
does not establish a feasible mud window or validate a general 3D model.
At that other sector, effective hoop stress is `440 - (Pmud - 3080)` psi
and axial stress is 2,920 psi; the latter governs as the major principal
stress. Its shear recurrence solves `3500 + q * hoop = 2920`.

Additional mathematical checks include the vertical horizontally isotropic
closed forms, stress-rotation invariants, an independent 45-degree wall-stress
calculation including nonzero hoop/axial shear, azimuth invariance, an allowed
breakout edge equation, triaxial Mogi equivalence, and high-pressure shear
reversal. They are regression checks, not published-example qualification of
the deviated or Mogi paths.

Method references:

- *Introduction to Energy Geomechanics*, same edition, [web page node49.html](https://dnicolasespinoza.github.io/node49.html), section 6.6.2, equations 6.19-6.21 and Figure 6.22: geographical/well transformation and wall stresses. The frame here uses the high side for x; the source uses the deepest side. The stated basis and wall-angle convention control the component signs.
- Method lead only, `NOT_VALIDATED`: *An analytical solution to wellbore stability using Mogi-Coulomb failure criterion*, [DOI 10.1016/j.jrmge.2019.03.004](https://doi.org/10.1016/j.jrmge.2019.03.004). The DOI redirect confirmed the title, but the publisher article returned HTTP 403 on 2026-10-17. Its equations were not checked and supply no validation claim. The algebraic mapping above and regression tests do not replace a published Mogi worked example.

Single-plane weakness, chemical/time effects, thermal stresses, poroelastic
diffusion, plastic redistribution and fracture propagation stay out of scope
and appear in `not_covered`. Independent review and the family QA gate must
be assessed separately from the mathematical checks. Second-agent code review
passed after resolving the near-90-degree friction-angle singularity. Formal
family clearance remains pending: the configured `tests-marine-ops-other` gate
includes an unrelated diffraction import chain that probes a licensed solver
binding. It was not run under this job's prohibition on those imports. The
complete `tests/well` suite supplies module regression evidence only.
