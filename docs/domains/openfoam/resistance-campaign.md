# Resistance campaign analysis

## Summary

The offline campaign evaluator adds condition-matched, two-sided multi-Froude
scoring and a guarded three-grid study for #2022. It reads declared coefficients
and evidence from JSON. It does not establish an experimental resistance result.
The single-point validation prerequisite in #1173 remains unresolved; a real
campaign must remain `NOT_EVALUATED` until that evidence is available.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Added offline sweep and grid analysis with a synthetic example to make the deferred evidence requirements executable. |
| 2 | 2026-10-25 | Corrected inclusive sweep boundaries and rejected malformed JSON structures to preserve the documented comparison and exit-code contracts. |

## Scope and inputs

Slice 1 checks the prerequisite, source declarations, reference qualification,
physical conditions and complete Froude coverage. Slice 2 scores signed total
coefficient errors and adjacent changes against explicitly supplied symmetric
limits. Slice 3 evaluates a settled, systematic, equal-ratio three-grid sequence
and gates its medium-grid GCI. The JSON command and worked example exercise all
three slices without starting a solver.

The implementation is
`src/digitalmodel/solvers/openfoam/resistance_campaign.py` and reuses
`grid_convergence.py` for the supported Richardson calculation. Existing
single-point validation and fixture values are unchanged.

Each `reference` and `computed` series has a `source`, a `condition` object, and
`points`. Conditions contain `body_condition`, `appendages`,
`wetted_surface_m2`, `lpp_m`, `density_kg_m3` and `viscosity_m2_s`. Points contain
positive finite `froude`, `reynolds` and `ct`. Computed points also require
`settled: true`, backed by the cited force-history assessment. The reference
requires `qualified: true`, backed by an opened, condition-matched source.

Reynolds number belongs to each speed, not to a campaign-wide scalar. A
campaign with fixed viscosity and length changes Reynolds number with speed.
No interpolation, automatic renormalisation or rounding of condition fields is
performed. Exact equality rejects rounding differences: convert units and reconcile source
precision before supplying data. Duplicate speeds and invalid numerical fields
are input errors; missing evidence and incomparable conditions are unevaluated.
The JSON root, series, conditions, prerequisite, limits, grid entries and points
must be objects; `points` and `grids` must be lists of objects. Explicit nulls or
other container types are invalid input, checked before evidence evaluation.
Omitted top-level evidence sections retain the unevaluated behavior.

`prerequisite` contains `status: PASS` and a `source` pointing to the validated
single-point result. A declaration alone does not authenticate that result.
`limits` contains a nonempty `source` and nonnegative `ct_fraction`,
`increment_fraction` and `gci_medium_pct`. There are no engineering defaults.

Each entry in `grids` carries its own `condition`, `source`, nonempty
`mesh_family`, representative `h_m`, and `point` with the same fields as a
computed point. The three grids must share conditions, speed, Reynolds number,
mesh family and documented settling. The family declaration requires review of
the actual mesh construction; a shared string cannot prove systematic refinement.

## Method and sources

The signed point error is

`e_i = (Ct_computed,i - Ct_reference,i) / Ct_reference,i`.

The signed adjacent-increment error is

`d_i = [(Ct_computed,i+1 - Ct_computed,i) - (Ct_reference,i+1 - Ct_reference,i)]
/ max(Ct_reference,i, Ct_reference,i+1)`.

Both must satisfy `-limit <= error <= limit` at every point or interval.
The inclusive comparison permits an absolute roundoff allowance of
`8 * ulp(1.0)` in the dimensionless error (eight binary64 machine epsilons),
with no relative allowance. This implementation definition (2026) accommodates
the subtractions and division at decimal endpoints; it is not an engineering
acceptance tolerance. Errors beyond `limit + 8 * ulp(1.0)` fail, including when
the supplied limit is zero. Diagnostic errors remain unrounded.
These are explicit campaign metric definitions developed here (2026), following
the two-sided and condition-matching requirements of #2022. The increment scale
remains defined when a reference increment vanishes or changes sign. It measures
shape on the local coefficient scale, rather than claiming a relative slope
error near an extremum. Limits require a documented intended-use basis; no
experimental acceptance percentage is inferred from the metric definition.

For grids ordered fine, medium, coarse, let `r = h2/h1 = h3/h2`,
`epsilon21 = Ct2-Ct1`, and `epsilon32 = Ct3-Ct2`. A positive-order monotonic
estimate requires `0 < epsilon21/epsilon32 < 1`. Then

- `p = ln(epsilon32/epsilon21) / ln(r)`;
- `Ct_ext = Ct1 - epsilon21/(r^p - 1)`;
- `GCI_fine_pct = 100 Fs |epsilon21/Ct1|/(r^p - 1)`;
- `GCI_medium_pct = 100 Fs |epsilon32/Ct2|/(r^p - 1)`.

The source is [NASA, Examining Spatial (Grid) Convergence, updated 2021](https://www.grc.nasa.gov/www/wind/valid/tutorial/spatconv.html),
sections Order of Grid Convergence, Richardson Extrapolation, and Grid
Convergence Index. The same page recommends `Fs = 1.25` for three or more grids
and minimum `r >= 1.1` in Grid Considerations. It was opened successfully on
2026-10-19. The factor 100 converts a fraction to percent.

The ratio equality uses relative roundoff allowance `1e-12` solely for floating
point arithmetic (implementation definition, 2026); it is not a mesh-quality or
engineering tolerance. Unequal ratios are outside this implementation's supported
method. Identical, oscillatory and divergent sequences return `NOT_EVALUATED`
without a Richardson estimate. A missing GCI limit still permits diagnostics,
while its acceptance status remains unevaluated. A triplet supports numerical
evidence at that speed only, not grid convergence across an entire sweep.

The reported asymptotic ratio comes from the existing helper's absolute-error
normalisation; it is not an independent asymptotic-range acceptance test. Neither
GCI nor an observed order proves experimental validity, domain independence,
temporal accuracy or boundary-layer resolution.

## Worked example

The input is [resistance_campaign.json](../../../examples/openfoam/resistance_campaign.json).
All of its values are synthetic arithmetic definitions in this section (2026),
not measurements, fluid properties or standard limits. The condition fields
exercise matching only. The example's prerequisite and qualified-reference
declarations are synthetic too, and cannot qualify the outstanding real study.

At Froude inputs 0.1 and 0.2, reference coefficients are 1 and 2, with Reynolds
inputs 1 and 2. Computed coefficients are 1.01 and 2.02. Both point errors are
+1.0%; the increment error is `(1.01-1)/2`, or +0.5%. The example defines
limits of 5.0% for points and 1.0% for increments, so both checks pass.
Its condition defines unit-test metadata: free heave/pitch, rudder, and numerical
value 2 for area, length, density and viscosity in the schema's SI units.
Those metadata values are not presented as a physically realised flow.

For `h = 1, 2, 4`, define the arithmetic sequence `Ct(h) = 1 + 0.01 h²`:
fine/medium/coarse coefficients are 1.01, 1.04 and 1.16. Differences are 0.03
and 0.12; their ratio gives `p = 2`. The extrapolated coefficient is 1.
Fine GCI is `1.25/1.01` percent, or 1.2%; medium GCI is `5/1.04` percent, or
4.8%. The example defines a medium-grid limit of 5.0%, so this check passes.
Tightening that supplied limit to 2.0% fails the medium-grid check even though
the fine-grid GCI is smaller than the limit. Comparisons use unrounded values.

Run from the repository root, with the output directory pointing to scratch:

```sh
PYTHONPATH=src uv run --no-project --with loguru --with numpy --with scipy \
  --with pydantic python -m digitalmodel.solvers.openfoam.resistance_campaign \
  examples/openfoam/resistance_campaign.json --output "$TMPDIR/campaign.json"
```

The result is `PASS` for the synthetic campaign. Output retains the input
evidence declarations and unrounded machine-readable diagnostics. Displayed
comparisons above use one decimal place. Exit codes are 0 for pass, 1 for fail,
and 2 for unevaluated or invalid input. A failed gate takes precedence over an
unevaluated gate in the combined status; each component retains its own status.

The regression suite also reproduces the NASA page's published inlet example
as a mathematical check, without treating it as resistance data. Its expected
order, extrapolation and GCI values come from the opened published example;
the synthetic tests contain independent hand calculations in their docstrings.

## Evidence limits and follow-up

The existing experimental fixture distinguishes fixed bare-body and free
heave/pitch with-rudder campaigns. They must never share a score. The fixture's
free-condition series has a single campaign Reynolds scalar and lacks qualified,
opened evidence in the retained study; this evaluator does not automatically
promote it into a validated multi-speed reference.

Actual multi-speed results, opened matched experimental tables with per-speed
conditions, qualified single-point validation, and a completed fine mesh remain
follow-up. Free heave/pitch requires the moving-mesh formulation noted in #2022;
this change neither implements nor claims that solver campaign. The existing
study lacks a completed third full mesh and states that experimental comparison
is unavailable in `docs/domains/openfoam/calm-water-resistance-study.html`,
sections Numerical and experimental qualification and Limitations.

Until those records exist, supply the real prerequisite as `NOT_EVALUATED`.
The executable example demonstrates the method and answer for synthetic inputs
only. No solver runs or experimental coefficients are added by this change.
