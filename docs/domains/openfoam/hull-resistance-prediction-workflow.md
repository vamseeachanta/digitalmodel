# Hull resistance prediction workflow

## Summary

The workflow connects normalised hull geometry, case preparation, the existing
meshing and towing runner, signed force reduction and a matched grid study.
It reports numerical diagnostics and explicitly unevaluated qualification checks.
No experimental validation or benchmark-transfer claim follows from these checks.
Refs #2023; benchmark evidence remains a prerequisite for engineering use.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-19 | Added case-bound force reduction and grid assessment to make the existing geometry and solver components reproducible together. |
| 1 | 2026-10-25 | Refused unsupported full-domain preparation and withheld grid estimates for unequal averaging coverage to prevent force-scaling and comparison errors. |

## Inputs and contract

The entry point is
`digitalmodel.solvers.openfoam.resistance_campaign`.

1. Ingest geometry with `hull_ingest.ingest_triangles` or `ingest_3dm`.
   Explicitly declare units, forward direction and draft when these cannot be
   established from the geometry. The existing ingestion contract supplies metres,
   x forward, y port, z upward and an aft-perpendicular keel origin. Closed geometry
   and an external wetted area are prerequisites. A waterline length used as an
   Lpp proxy remains an ingestion assumption, not a measured perpendicular spacing.
2. Pass the resulting `HullManifest` and normalised STL to `HullCaseConfig`.
   Specify fluid density and viscosity or Reynolds number. Existing case-builder
   defaults for mesh, turbulence, gravity and domain sizing remain declared model
   settings; this workflow does not qualify those defaults for a new hull.
3. Call `prepare_resistance_case(config, output)` for each mesh level.
   It refuses an existing case directory. `resistance_input.json` records the
   physical condition, union reference area, geometry digests, numerical schemes
   and input-file digests. Domain factors and turbulence settings must agree
   across levels. Case names, ranks and mesh sizing may differ.
   Preparation requires `half_domain=True`: the builder emits a symmetry
   half-domain, so full-domain requests are refused before writing a case.
4. To connect preparation to execution, call
   `run_resistance_case(config, output, timeout_seconds=budget, dry_run=True)`.
   An explicit positive budget is required even in dry mode. Set `dry_run=False`
   only when execution is intended. The existing runner uses the complete
   feature extraction, background mesh, refinement, layer, field-restoration,
   decomposition and towing sequence. A dry run creates no force result.
5. Supply each completed case as
   `ResistanceEvidence(case_dir, force_file, cells, spacing_m)`.
   Select the force history explicitly: restarts and multiple output folders are
   not guessed. Cell counts and a representative spacing must come from the
   executed meshes. The same spacing definition and grid generation method must
   apply throughout the study; total cell counts alone do not establish a linear
   refinement ratio for a locally refined mesh [2].
6. Call `assess_resistance_campaign(evidence, policy)`. `CampaignPolicy` requires
   the averaging time span, minimum sample count, maximum relative coefficient
   span, force-balance tolerance, spacing-ratio equality tolerance and a source
   describing their basis. There are no inherited acceptance limits.

Receipts detect changed input bytes; they are not authenticated attestations.
Geometry measurement remains the ingestion stage's responsibility. The caller
must associate force histories and mesh measurements with the correct case.
Generated result files are written only by the caller or the worked example.

## Method and status interpretation

The supported force format is the nonporous OpenFOAM v2312 `force.dat` layout:
time, total vector, pressure vector and viscous vector [1]. The header is required.
Unknown layouts, malformed rows, nonfinite numbers and nonincreasing time
coordinates are refused. Floating time coordinates are retained; the window is a
time span, or an iteration span for local time stepping, rather than a row count.

The generated case uses negative-x flow. With full-body wetted area S, the
reduction uses `R = -F_x` for a full domain and `R = -2 F_x` for a reflected half
domain. The coefficient definition is `Ct = R / (rho S U² / 2)`, with the same
normalisation for `Cp` and `Cv`; these are equations (1)-(2) in the existing
[calm-water resistance study](calm-water-resistance-study.html).
The sign is fixed by the case convention and is never inferred from the answer.
Force decomposition is checked per sample, so cancellation between bad rows
cannot hide an imbalance. A failed reduction check produces `FAIL`.

The relative span is `(max(Ct)-min(Ct))/abs(mean(Ct))`. It is a descriptive
stationarity check against the caller's declared policy, not a statistical
uncertainty interval. An insufficient sample count gives `NOT_EVALUATED`.
Every grid level must satisfy the policy before a spatial estimate is reported.
Final averaging times must match across levels. The first retained sample time
must also match: equal end times with different starts give `NOT_EVALUATED`
and no GCI, even when every history meets the minimum sample count. Coverage
is compared after selecting the declared window; earlier discarded samples do
not affect the comparison. Matching coverage does not establish stationarity
beyond the declared policy or qualify the chosen averaging duration.

Exactly three distinct matched cases are required for an observed-order estimate.
For fine-to-coarse spacings h1, h2, h3 with equal ratios r, define
`d21 = Ct2-Ct1`, `d32 = Ct3-Ct2`, `p = log(d32/d21)/log(r)` and
`error = d21/(r^p-1)`. The extrapolation is `Ct1-error`, and the absolute
fine-grid GCI is `1.25 |error|` [2]. The minimum refinement ratio is 1.1 [2].
Equality of the two ratios is assessed using the caller's declared tolerance.
Repeated spacings, unsuitable ratios, nonmonotonic or degenerate sequences, and
studies with fewer or more than three levels return `NOT_EVALUATED`; no assumed
order or oscillatory half-range is substituted. The single-term asymptotic error
model remains an assumption, and its fit to three values is not independent
confirmation that the asymptotic range has been reached [2].

`checks.numerical_evidence = PASS` means only that the stated force-history and
grid checks ran and met their specified conditions. `qualification`, `validation`
and `empirical_screening` remain `NOT_EVALUATED`: no temporal or domain sensitivity,
near-wall evidence, experimental comparison or sourced empirical band is supplied.
No combined numerical uncertainty or accuracy bound is asserted.

## Worked example

Run the example with a new output directory outside the source tree:

```sh
PYTHONPATH=src python examples/openfoam/hull_resistance_prediction.py "$OUTPUT_DIR"
```

All example dimensions, forces and assessment settings are invented arithmetic
inputs. They are neither measured resistance data nor recommended design settings.
The rectangular hull is 4.0 m long, 1.0 m wide and 1.0 m deep, at 0.6 m draft.
Its wetted area is `4×1 + 2×(4+1)×0.6 = 10.0 m²` and displacement is
`4×1×0.6 = 2.4 m³`. With density 1000.0 kg/m³ and speed 2.0 m/s, the
coefficient denominator is 20000.0 N.

| Representative spacing (m) | Synthetic half-body force (N) | Ct |
| --- | --- | --- |
| 1.0 | -10.0 | 1.0 × 10⁻³ |
| 2.0 | -13.0 | 1.3 × 10⁻³ |
| 4.0 | -25.0 | 2.5 × 10⁻³ |

These values follow `Ct = 9.0×10⁻⁴ + 1.0×10⁻⁴ h²`. The independent answers are
observed order 2.0, extrapolated coefficient 9.0 × 10⁻⁴ and absolute GCI
1.3 × 10⁻⁴ when displayed to one decimal place in scientific notation.
The executable test checks the unrounded hand calculation. The example writes
normalised geometry, three prepared cases, synthetic force histories and
`prediction.json`; it starts no solver. The example spacings and cell counts
demonstrate the arithmetic rather than a practical resistance discretisation.

## Later slices and known limits

The three delivered slices are case provenance and runner integration, strict
signed reduction, and matched three-grid assessment with the example. The original
issue's wider engineering qualification remains open: a passing matched benchmark,
measured near-wall resolution, temporal and domain sensitivity, a sourced empirical
comparison, free sinkage and trim, speed sweeps, and full-scale powering are not
established here. Existing benchmark templates and scorers are unchanged.

## Sources

1. [OpenFOAM v2312 source (2023), forces.C](https://api.openfoam.com/2312/forces_8C_source.html),
   `writeIntegratedDataFileHeader`, lines 429-450, and
   `writeIntegratedDataFile`, lines 479-499. Defines total/pressure/viscous ordering
   and the optional porous component, which this adapter refuses.
2. [NASA (2021), Examining Spatial (Grid) Convergence](https://www.grc.nasa.gov/www/wind/valid/tutorial/spatconv.html),
   “Grid Considerations for a Grid Convergence Study”, “Order of Grid Convergence”,
   “Richardson Extrapolation” and “Grid Convergence Index”. Supplies the
   equal-ratio observed-order method, minimum ratio and GCI safety factor.
