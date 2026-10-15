# OpenFOAM module knowledge pack

| Header | Value |
|---|---|
| Module | `solvers/openfoam` |
| Repository | `digitalmodel` |
| Path | `src/digitalmodel/solvers/openfoam` |
| Pack revision | 0 |
| Code revision | `4a96969590e625fbd00392851d0a4dd29b24c4f9` |
| Analysis family | CF (CFD) |
| Quality manager role | CFD quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The main path parses authored case requests, writes OpenFOAM dictionaries and initial fields without a solver installation, and supplies execution plans and offline reduction of force histories. [src/digitalmodel/solvers/openfoam/case_definition.py:L378-L389] [src/digitalmodel/solvers/openfoam/case_builder.py:L73-L82] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L279-L305]

The existing `solvers` routing domain has maturity `beta`; this is a domain designation, not a measured validation rate. [docs/registry/module-routing.yaml:L255-L260]

Scope covers the generic builder, rectangular free-decay theory checks, resistance reduction and the six incident classes in section 6; separate hull and validation builders retain their own dictionaries. [src/digitalmodel/solvers/openfoam/case_builder.py:L209-L242] [src/digitalmodel/solvers/openfoam/hull_double_body.py:L11-L42] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_dicts.py:L25-L29]

Referent-free resistance results describe numerical uncertainty and plausibility, without establishing modelling error or measured resistance accuracy. [docs/domains/openfoam/referent-free-resistance-validation-2026-08-19.md:L12-L27]

## 2. Inputs and outputs

Canonical schema-v1 fields below are required unless marked optional; the legacy route supplies different defaults. [src/digitalmodel/solvers/openfoam/case_definition.py:L324-L375]

| Input | Unit | Range / default | Consumer and evidence |
|---|---|---|---|
| `case_definition.schema_version`, `kind` | 1 | `1`, `authored`; `prebuilt` is reserved in this parser | `_parse_canonical`; [src/digitalmodel/solvers/openfoam/case_definition.py:L324-L352] |
| `authored.case_type`, `name`, `solver` | 1 | Type from enum; portable name; solver string required | `_parse_authored`; builder subsequently requires a declared solver contract; [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L101-L106] [src/digitalmodel/solvers/openfoam/models.py:L20-L27] [src/digitalmodel/solvers/openfoam/case_definition.py:L277-L293] [src/digitalmodel/solvers/openfoam/solver_contracts.py:L165-L177] |
| `domain.min_coords_m`, `max_coords_m` | m | Three finite coordinates; each maximum exceeds minimum; no canonical default | `_parse_domain`; [src/digitalmodel/solvers/openfoam/case_definition.py:L132-L143] [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L75-L110] |
| `domain.n_cells` | cells | Three positive integers; booleans rejected; no canonical default | `_parse_domain`; [src/digitalmodel/solvers/openfoam/case_definition.py:L132-L143] [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L66-L110] |
| `time.start_time_s`, `end_time_s`, `delta_t_s` | s | Finite; end > start; step > 0; all required | `_parse_time`; steady hull time has an iteration interpretation described below; [src/digitalmodel/solvers/openfoam/case_definition.py:L146-L168] [src/digitalmodel/solvers/openfoam/hull_double_body.py:L92-L97] |
| `time.write_interval_steps`, `purge_write` | steps / writes | Positive integer interval; integer purge count, with no nonnegative bound enforced | `_parse_time`; [src/digitalmodel/solvers/openfoam/case_definition.py:L160-L168] |
| `time.adjustable_time_step`, `max_co` | boolean / 1 | Boolean and finite real; parser does not require positive `max_co` | `_parse_time`; [src/digitalmodel/solvers/openfoam/case_definition.py:L164-L168] [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L58-L87] |
| Optional `motion.type`, `amplitude`, `amplitude_unit`, `period_s` | enum, m or deg, s | Positive amplitude and period; rotation requires `deg`, translation requires `m` | `_parse_motion`; [src/digitalmodel/solvers/openfoam/case_definition.py:L171-L221] |
| `motion.origin_m`, `phase_shift_s` | m, s | Origin required for rotation and forbidden for translation; phase forbidden for rotation and defaults to 0 s for translation | Motion parser; [src/digitalmodel/solvers/openfoam/case_definition.py:L201-L221] |
| Optional `fill.level` | 1 | 0 to 1 inclusive; requires multiphase and `execution.run_set_fields=true` | `_parse_fill`, `_validate_combinations`; [src/digitalmodel/solvers/openfoam/case_definition.py:L302-L321] |
| Optional `function_objects` | configuration | Without the block: no taps, `timeStep`, interval 1; a supplied block requires taps, write control and positive interval | `_optional_function_objects`, `_parse_function_objects`; [src/digitalmodel/solvers/openfoam/case_definition.py:L224-L253] [src/digitalmodel/solvers/openfoam/case_definition.py:L296-L299] |
| `pressure_taps[].name`, `location_m`, `fields` | 1, m, field names | Each tap requires all three; finite 3-vector and nonempty field sequence | `_parse_tap`; [src/digitalmodel/solvers/openfoam/case_definition.py:L255-L264] [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L108-L116] |
| `execution.mesh_utility`, `run_snappy`, `run_set_fields`, `to_vtk`, `timeout_seconds`, `dry_run` | name, booleans, s | All required in canonical form; timeout positive integer | `_parse_execution`; [src/digitalmodel/solvers/openfoam/case_definition.py:L115-L129] |
| Legacy execution defaults | name, boolean, s | `blockMesh`, no snappy, no setFields, VTK enabled, timeout 7200 s, dry run false | `_parse_legacy`; [src/digitalmodel/solvers/openfoam/case_definition.py:L355-L375] |
| Rectangular free-decay geometry and sampling | m, s, steps | Breadth 1 m, height 0.6 m, fill 0.5, 100 cells per breadth, perturbation 0.02 m, step 0.002 s, end 16 s, stride 10 | `SloshingFreeDecayConfig`; [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py:L38-L74] |
| Hull manifest, surface, speed, ranks | JSON, STL, m/s, processes | `HullCaseConfig` requires all four; density and viscosity are parameters, not manifest-derived fluid properties | Separate hull builder; [src/digitalmodel/solvers/openfoam/hull_case.py:L98-L127] |

The generic block mesh is right handed with z upward, with vertices at the supplied minimum and maximum coordinates; gravity is negative z, and fill is measured from the domain bottom. [src/digitalmodel/solvers/openfoam/models.py:L265-L284] [src/digitalmodel/solvers/openfoam/case_builder.py:L151-L164] [src/digitalmodel/solvers/openfoam/case_builder.py:L311-L316]

The rectangular validation mesh instead uses x for breadth, y for height, origin at a bottom corner, and a thin empty z direction; its gravity points downward in y. [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_dicts.py:L36-L63] [tests/solvers/openfoam/validation/test_sloshing_2d.py:L126-L132]

Regular-wave direction is measured from x in degrees and phase in radians; sloshing coupling defines positive coefficients as opposing motion, with roll amplitude in radians. [src/digitalmodel/solvers/openfoam/wave_models.py:L54-L68] [src/digitalmodel/solvers/openfoam/sloshing_coupling.py:L17-L35]

| Output | Unit / format | Interpretation and source |
|---|---|---|
| `system/`, `constant/`, `0/` | OpenFOAM text dictionaries and fields | `OpenFOAMCaseBuilder.build` creates the three directories; [src/digitalmodel/solvers/openfoam/case_builder.py:L104-L129] |
| `system/blockMeshDict` | m, cells | `convertToMeters 1`; single hex block, uniform grading, fixed patch topology; [src/digitalmodel/solvers/openfoam/block_mesh.py:L14-L76] |
| `0/U`, `0/alpha.water` | m/s, 1 | Velocity and VOF fraction dimension vectors are emitted explicitly; [src/digitalmodel/solvers/openfoam/initial_fields.py:L97-L107] [src/digitalmodel/solvers/openfoam/initial_fields.py:L176-L186] |
| Single-phase `0/p` | m²/s² (`p/ρ`) | `write_pressure_field` enforces `[0 2 -2 0 0 0 0]` for simpleFoam/pimpleFoam; [src/digitalmodel/solvers/openfoam/initial_fields.py:L110-L143] |
| Multiphase `0/p_rgh` | Pa | Dynamic reduced pressure `p − ρgh`, enforced `[1 -1 -2 0 0 0 0]`; [src/digitalmodel/solvers/openfoam/initial_fields.py:L110-L143] |
| `system/setFieldsDict` | m and dimensionless fraction | Generic fill snaps to a vertical cell face; water fraction 1 below, 0 above; [src/digitalmodel/solvers/openfoam/case_builder.py:L148-L168] |
| Free-decay analysis dictionary | Hz, m, 1 | Refined/raw frequency, sample rate, relative error, geometry and tolerance flag; [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L156-L211] |
| Force reduction dictionary | N, iteration coordinate, % | Total x force, pressure/viscous components, cycle change, central mean and settling metadata; [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L36-L45] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L279-L305] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L384-L388] |
| Three-grid report | m, dimensionless coefficient, % | Fine/medium/coarse sizes, observed order, extrapolate, GCI and convergence flags; [src/digitalmodel/solvers/openfoam/grid_convergence.py:L40-L54] [src/digitalmodel/solvers/openfoam/grid_convergence.py:L131-L149] |

## 3. Method and governing standards

| Family / choice | Emitted algorithm and numerical scheme | Evidence |
|---|---|---|
| Generic wave loading, greenwater, sloshing | `interFoam`, multiphase, PIMPLE, Euler time derivative | [src/digitalmodel/solvers/openfoam/models.py:L292-L305] [src/digitalmodel/solvers/openfoam/solver_contracts.py:L105-L127] |
| Generic steady current | `simpleFoam`, single phase, SIMPLE, `steadyState` | [src/digitalmodel/solvers/openfoam/models.py:L292-L305] [src/digitalmodel/solvers/openfoam/solver_contracts.py:L145-L161] |
| Generic VIV; transient current setup | `pimpleFoam`, single phase, PIMPLE, `backward` | [src/digitalmodel/solvers/openfoam/models.py:L292-L299] [src/digitalmodel/solvers/openfoam/marine_solvers.py:L78-L109] [src/digitalmodel/solvers/openfoam/solver_contracts.py:L128-L144] |
| Generic momentum / turbulence | Single-phase `div(phi,U)` linearUpwind; VOF `div(rhoPhi,U)` linearUpwind, alpha vanLeer, interface compression linear; k/omega/epsilon upwind | [src/digitalmodel/solvers/openfoam/solver_contracts.py:L47-L71] |
| Generic spatial operators | Gauss linear gradient, corrected Laplacian and normal gradient, linear interpolation; wall distance `meshWave` for nonlaminar models | [src/digitalmodel/solvers/openfoam/solver_contracts.py:L190-L239] |
| Turbulence selection | Default kOmegaSST; laminar, kEpsilon and LES Smagorinsky are configurable; serialization uses RAS or LES, and laminar emits no turbulence initial fields | [src/digitalmodel/solvers/openfoam/models.py:L46-L52] [src/digitalmodel/solvers/openfoam/models.py:L92-L124] [src/digitalmodel/solvers/openfoam/initial_fields.py:L146-L164] |
| Rectangular free decay | Own interFoam dictionaries: Euler, VOF momentum and alpha schemes, PIMPLE; small cosine initial disturbance and uniformly sampled FFT analysis | [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_dicts.py:L101-L144] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py:L40-L61] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L156-L180] |
| Hull free surface / double body | Separate template trees: free-surface interFoam with localEuler; double-body simpleFoam with steadyState, one phase, waterline symmetry and no gravity | [src/digitalmodel/solvers/openfoam/hull_double_body.py:L3-L25] |

Linear rectangular theory uses `ωₙ² = g k tanh(kh)`, `k = nπ/L`, `f = ω/(2π)` in `prismatic_tank_natural_frequency`; positive length and depth and mode ≥ 1 are required. [src/digitalmodel/solvers/openfoam/spectral_analysis.py:L316-L354]

`analyze_free_decay` compares the refined FFT frequency against that closed form, using relative error ≤ 5.0 %; the frequency extraction assumes uniform sampling. [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L156-L180] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L190-L211] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py:L26-L28]

The generic VOF dictionary contract identifies the OpenFOAM v2312 dam-break tutorial as its reference, while the builder's decorative header says v2306; neither header demonstrates solver compatibility. [src/digitalmodel/solvers/openfoam/solver_contracts.py:L42-L45] [src/digitalmodel/solvers/openfoam/case_builder.py:L40-L54]

The grid-convergence implementation names ASME V&V 20 and Roache, but does not identify an edition or clause; clause text not checked against the licensed copy, and no standard table values are reproduced here. [src/digitalmodel/solvers/openfoam/grid_convergence.py:L1-L20]

The referent-free design note names ITTC 7.5-03-01-01 for iterative uncertainty; its cited edition and clause title need confirmation before a standards-conformance claim. [docs/domains/openfoam/referent-free-resistance-validation-2026-08-19.md:L63-L98]

`_provenance` in the wave-tank builder constructs a citation list; the generic builder writes dictionaries rather than a calculation `Citation` sidecar. [src/digitalmodel/solvers/openfoam/validation/wave_tank.py:L544-L570] [src/digitalmodel/solvers/openfoam/case_builder.py:L104-L129]

UNVERIFIED: Standards editions, clause titles, and calculation-citation coverage across all separate builders remain unchecked; issue pending (4).

## 4. Validated worked examples and regression examples

The validated entries below check the analytical frequency implementation against independently specified closed-form targets; they do not rerun CFD or validate pressure loads. [tests/solvers/openfoam/validation/test_sloshing_2d.py:L44-L70]

| ID / classification | Source and input | Expected value and tolerance | Existing test node ID | Offline result at the pin |
|---|---|---|---|---|
| V1 / validated analytical check | Linear rectangular theory; `SloshingFreeDecayConfig`, breadth 1 m, height 0.6 m, fill 0.5; test parameters are the input record | 0.758 Hz ± 0.005 Hz; corresponding closed-form period 1.3192 s; bound applies to frequency | `tests/solvers/openfoam/validation/test_sloshing_2d.py::test_analytical_frequency_across_fill_range[0.6-0.5-0.758]` | PASS before and after; [tests/solvers/openfoam/validation/test_sloshing_2d.py:L57-L70] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py:L65-L74] |
| V2 / validated analytical check | Same closed form; breadth 1 m, height 1 m, fill 0.7; test parameters are the input record | 0.8726 Hz ± 0.005 Hz; corresponding closed-form period 1.1460 s; bound applies to frequency | `tests/solvers/openfoam/validation/test_sloshing_2d.py::test_analytical_frequency_across_fill_range[1.0-0.7-0.8726]` | PASS before and after; [tests/solvers/openfoam/validation/test_sloshing_2d.py:L57-L70] |
| R1 / regression | Synthetic `force.dat` written by the test; no measured resistance referent | `referent_loaded=false`, zero calls to a referent loader; exact boolean contract | `tests/solvers/openfoam/validation/test_referent_free_resistance.py::test_referent_free_run_never_loads_a_referent` | PASS before and after; [tests/solvers/openfoam/validation/test_referent_free_resistance.py:L385-L418] |
| R2 / regression | Synthetic `force.dat`; referent-free manifest | Zero aggregate pass claims; `validation.available=false`; exact manifest contract | `tests/solvers/openfoam/validation/test_referent_free_resistance.py::test_manifest_carries_no_aggregate_pass_and_no_validation_claim` | PASS before and after; [tests/solvers/openfoam/validation/test_referent_free_resistance.py:L444-L454] |

Independent arithmetic uses the declared spectral gravity 9.80665 m/s²; generic CFD gravity is 9.81 m/s². [src/digitalmodel/solvers/openfoam/spectral_analysis.py:L18-L19] [src/digitalmodel/solvers/openfoam/case_builder.py:L311-L316]

For a period comparison use `T = 1/f`; the frequency gate is defined on frequency, so a frequency tolerance must not silently become an identical period tolerance. [src/digitalmodel/solvers/openfoam/spectral_analysis.py:L68-L74] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L199-L208]

The natural-frequency battery is tracked by #1430; the local analytical tests retain the two fill-depth targets above, while their comments describe historical solver measurements rather than a solver run performed for this pack. [tests/solvers/openfoam/validation/test_sloshing_2d.py:L52-L70]

The referent-free resistance record explains why a numerical uncertainty band cannot establish measured resistance accuracy; R1 and R2 exercise that distinction without reproducing hull-specific results. [docs/domains/openfoam/referent-free-resistance-validation-2026-08-19.md:L12-L27] [tests/solvers/openfoam/validation/test_referent_free_resistance.py:L385-L454]

UNVERIFIED: The private tank-sloshing result has no dataset identifier supplied for this pack; sidecar entry: to be created; no private values or identities are included; #1528.

Offline evidence record: run `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/openfoam -q -rA -p no:cacheprovider --basetemp=<scratch>` at `4a96969590e625fbd00392851d0a4dd29b24c4f9`, with bytecode disabled and caches in scratch; BASE and AFTER each collected 1584 tests: 1561 passed, 7 failed, 16 skipped, with 13 of the skips classified NOT_RUN for unavailable solver utilities or tutorial fixtures and 3 phase-inapplicable skips retained as skipped; these are execution results recorded for this pack, not a solver-validation claim. [tests/solvers/openfoam/validation/conftest.py:L11-L19]

## 5. QA gates

Apply G0 dictionary/BC/dimension checks, G1 mesh checks, G2 hydrostatic initialisation, G3 a short run with a Courant limit, G4 NOT_APPLICABLE for CFD, and G5 an evidence appendix (AQG r0, sections 9–10, OpenFOAM family row).

Checks below are proposed repeatable acceptance checks; a source identifies an existing function, test, or criterion, not a claim that a physical case passed it. [src/digitalmodel/solvers/openfoam/doctor.py:L77-L85] [src/digitalmodel/solvers/openfoam/grid_convergence.py:L11-L20]

| Check / gate | Module-specific check and numeric criterion (unit) | Evidence / execution status and source |
|---|---|---|
| CF0a / G0 | `parse_case_request`: 0 accepted unknown keys; schema version = 1 (count, dimensionless) | Offline parser tests; [src/digitalmodel/solvers/openfoam/_case_definition_validation.py:L34-L49] [src/digitalmodel/solvers/openfoam/case_definition.py:L324-L352] |
| CF0b / G0 | `write_pressure_field`: 0 mismatches between field name and dimension vector (fields) | Offline `test_case_builder_preserves_pressure_name_dimension_pairing`; [tests/solvers/openfoam/test_initial_fields.py:L52-L69] |
| CF0c / G0 | Inspect mesh patch types and `U/k/omega/nut`: 0 no-slip wall patches with incompatible topology/BCs (patches) | Manual generated-dictionary comparison; no automated closed-outlet consistency check identified; #1979; [src/digitalmodel/solvers/openfoam/block_mesh.py:L43-L57] [src/digitalmodel/solvers/openfoam/initial_fields.py:L146-L164] |
| CF0d / G0 | Inspect wave-properties branches: 0 absorption configurations without an explicit applicability disposition (configurations) | Gap: no `kh` guard in the template; requires a sourced regime criterion before use; #2043; [src/digitalmodel/solvers/openfoam/validation/wave_tank.py:L300-L318] [src/digitalmodel/solvers/openfoam/validation/wave_excited_body.py:L522-L540] |
| CF1a / G1 | `run_doctor`: 0 FAIL diagnostics; target OpenFOAM version recorded; actual `checkMesh`: 0 failed mesh checks and 1 `Mesh OK` verdict (counts) | Program-specific load route is the generated case through `blockMesh` / optional snappy path and `checkMesh` in the target installation, not Python import; solver route NOT_RUN; doctor alone checks availability; [src/digitalmodel/solvers/openfoam/doctor.py:L77-L135] [src/digitalmodel/solvers/openfoam/runner.py:L418-L432] [scripts/cfd/stage45_driver.sh:L194-L205] |
| CF1b / G1 | MPI ranks ≤ available physical cores for physical-core benchmark cases (processes); 0 oversubscription errors | Compare resolved benchmark receipt to physical-core inventory; no MPI run here; #2320; [src/digitalmodel/solvers/benchmark/runner.py:L273-L283] |
| CF2a / G2 | `extract_time_history`: maximum mass residual / maximum liquid mass ≤ 0.001 = 0.1 % (kg/kg) | Gap in the four audit/readiness entry points; a separate `time_history` helper supplies the mass-balance flag; physical initialisation NOT_RUN; this does not prove hydrostatic pressure balance; [src/digitalmodel/solvers/openfoam/time_history.py:L579-L594] [src/digitalmodel/solvers/openfoam/time_history.py:L761-L775] [src/digitalmodel/solvers/openfoam/time_history.py:L813-L834] |
| CF2b / G2 | `analyze_free_decay`: absolute frequency error / analytical frequency ≤ 5.0 % (Hz/Hz) | V1/V2 check the analytical component only; measured-frequency gate NOT_RUN; [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_config.py:L26-L28] [src/digitalmodel/solvers/openfoam/validation/sloshing_2d_analysis.py:L190-L211] |
| CF3a / G3 | Generic VOF final pressure residual ≤ 1e-7 and U residual ≤ 1e-6 (dimensionless); final relative tolerance = 0; generic SIMPLE residualControl = 1e-4 (dimensionless) | These are template criteria, not measured convergence; hand inspection of residual log needed, automated convergence-audit residual gate is a gap; NOT_RUN; [src/digitalmodel/solvers/openfoam/templates/__init__.py:L93-L119] [src/digitalmodel/solvers/openfoam/templates/__init__.py:L147-L158] |
| CF3b / G3 | `force_cycle_average.analyse`: decaying envelope (newest/previous < 0.8, N/N) AND either cycle change < 1.0 % or estimator disagreement ≤ 2.0 % (N/N) | Report `settling_verdict`, cycles, envelope and central mean uncertainty separately; offline synthetic tests pass; physical force history NOT_RUN; [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L234-L240] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L279-L300] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L375-L388] |
| CF3c / G3 | `grid_convergence` / `gci_report`: exactly 3 increasing mesh sizes (m), medium-grid GCI ≤ 1.0 % for the stated calm-water target | Offline arithmetic tests pass; no physical mesh study; function reports GCI rather than enforcing that target; [src/digitalmodel/solvers/openfoam/grid_convergence.py:L18-L20] [src/digitalmodel/solvers/openfoam/grid_convergence.py:L60-L104] [src/digitalmodel/solvers/openfoam/grid_convergence.py:L131-L149] |
| CF3d / G3 | Short-run stop-time error ≤ 10^(-timePrecision) s; observed Co ≤ configured `maxCo` (1); 0 solver fatal/divergence markers | `smoke._require_final_time` and `_reject_stage_failure`; VOF builder bounds `maxAlphaCo=maxCo` and `maxDeltaT`; actual short run NOT_RUN, automatic Courant-history and time-step-independence scoring remain gaps; [src/digitalmodel/solvers/openfoam/smoke.py:L360-L384] [src/digitalmodel/solvers/openfoam/case_builder.py:L175-L183] |
| CF5a / G5 | `sha256_manifest`: 0 changed input digests between evidence and recomputation (files); 0 missing required gate dispositions (records) | Manifest function supplies input hashes; full signed AQG appendix integration remains a gap; [src/digitalmodel/solvers/openfoam/smoke.py:L58-L68]; AQG r0, section 9 |

G4 is NOT_APPLICABLE because the OpenFOAM family row assigns mesh checks to G1 rather than the diffraction gate (AQG r0, section 10).

Specific gaps: `doctor` reports tool availability, not actual mesh metrics or y+; the hull builder's y+ target is a design input, while the collector explicitly avoids inventing a post-hoc pass band; no quantitative y+ acceptance band is claimed here. [src/digitalmodel/solvers/openfoam/doctor.py:L88-L135] [src/digitalmodel/solvers/openfoam/hull_case.py:L109-L110] [scripts/cfd/yplus_after.sh:L1-L22]

Force-balance closure, hydrostatic pressure-balance acceptance, time-step refinement, and a unified residual/mass/force/mesh/y+ gate record need additional evidence and implementation; issue pending (3); the force-cycle, grid and readiness functions expose separate quantities rather than a complete CFD gate engine. [src/digitalmodel/solvers/openfoam/convergence_audit.py:L27-L65] [src/digitalmodel/solvers/openfoam/grid_convergence.py:L131-L149] [src/digitalmodel/solvers/openfoam/doctor.py:L77-L85]

## 6. Known failure modes and pitfalls

| Symptom / cause | Detection and present evidence | Incident |
|---|---|---|
| Referent-free output mistaken for measured accuracy | R1/R2: zero validation claims; numerical uncertainty excludes modelling error | #2023; [tests/solvers/openfoam/validation/test_referent_free_resistance.py:L444-L465] |
| Absorber selected outside a justified depth regime without warning | CF0d; the unconditional shallow-water template remains visible; no automated `kh` detection check exists | #2043; [src/digitalmodel/solvers/openfoam/validation/wave_tank.py:L300-L318] [src/digitalmodel/solvers/openfoam/validation/wave_excited_body.py:L522-L540] |
| Closed outlet uses wall momentum but free-patch turbulence / topology | CF0c manual cross-field and topology check; no automated detection check exists; generic outlet is still a `patch`, and turbulence fields have their own writer | #1979; [src/digitalmodel/solvers/openfoam/block_mesh.py:L43-L57] [src/digitalmodel/solvers/openfoam/initial_fields.py:L146-L164] |
| Single-phase case emits dynamic pressure dimensions | CF0b; at the pin the pressure-name and dimension pair is branched together and exercised by offline tests | #2026; [src/digitalmodel/solvers/openfoam/initial_fields.py:L110-L143] [tests/solvers/openfoam/test_initial_fields.py:L19-L49] |
| Mesh bounding-box text triggers solver divergence classification | CF3d; at the pin smoke enables divergence checking only for solver stage index 2; fatal and nonzero results still fail every stage | #2021; [src/digitalmodel/solvers/openfoam/smoke.py:L250-L256] [src/digitalmodel/solvers/openfoam/smoke.py:L360-L370] [tests/solvers/openfoam/test_smoke.py:L536-L584] |
| A validation case lacks a required divergence entry or body mesh | CF1a must include an actual program load; the cylinder builder explicitly describes a generic box without a cylinder; static assembly is insufficient | #1261; [src/digitalmodel/solvers/openfoam/validation/cylinder.py:L140-L154]; generic single-phase stress key: [src/digitalmodel/solvers/openfoam/solver_contracts.py:L55-L59] |
| MPI requests logical cores while the runtime offers physical-core slots | CF1b; benchmark resolves physical-core limits separately at the pin; no live MPI result is claimed | #2320; [src/digitalmodel/solvers/benchmark/runner.py:L273-L283] |
| Two force reducers use different settling criteria | CF3b; `audit_one` classifies from cycles and a fitted amplitude, without the `analyse` envelope requirement; no parity detection test identified | issue pending (2); [src/digitalmodel/solvers/openfoam/convergence_audit.py:L41-L64] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L384-L387] |
| Warm-start fallback writes outside a supplied case tree | Six baseline tests fail before their intended assertions complete with a read-only-filesystem error; `_campaign` can select a home-relative default; no code changes here | #2052; [src/digitalmodel/solvers/openfoam/warm_start/cli.py:L88-L125] [tests/solvers/openfoam/test_warm_start.py:L330-L361] |
| Truncated status text cannot be decoded as UTF-8 | Baseline `test_statusline_never_exceeds_width[72]` raises `UnicodeDecodeError`; existing shell truncation and locale handling require investigation | issue pending (1); [tests/solvers/openfoam/test_cfd_status_tools.py:L23-L55] [scripts/cfd/status/cfd_statusline.sh:L56-L60] |

Dates for these incident entries are intentionally tied to the pinned source evidence rather than an inferred discovery date. [src/digitalmodel/solvers/openfoam/initial_fields.py:L110-L143]

## 7. Lessons log

| Date | Entry | Lesson and pack change | Evidence / shared entry |
|---|---|---|---|
| 2026-10-15 | Initial revision | Keep pressure field names and dimensions together; section 2 records both families | [src/digitalmodel/solvers/openfoam/initial_fields.py:L127-L143]; shared entry pending, issue pending (4) |
| 2026-10-15 | Initial revision | Keep settling, precision and resistance validation as distinct statements; sections 4–6 carry the limitation | [docs/domains/openfoam/force_convergence.md:L29-L54]; shared entry pending, issue pending (4) |

## 8. Ownership

Proposed review responsibility: CFD quality manager; escalation to the maintainer for missing criteria, conflicting solver contracts or unsupported physics assumptions (AQG r0, section 11, independent family sign-off).

UNVERIFIED: A named deputy role, producing-desk assignment and shared-log entry identifiers are not established in the cited module sources; maintainers to assign them; issue pending (4).

## 9. Dependencies and parallel implementations

The generic builder consumes `OpenFOAMCase`, pressure taps, motion and fill renderers; the hull builder consumes a hull manifest and separate surface regions. [src/digitalmodel/solvers/openfoam/case_builder.py:L322-L330] [src/digitalmodel/solvers/openfoam/case_builder.py:L148-L168] [src/digitalmodel/solvers/openfoam/case_builder.py:L196-L203] [src/digitalmodel/solvers/openfoam/case_builder.py:L269-L277] [src/digitalmodel/solvers/openfoam/hull_case.py:L398-L414]

The sloshing coupling consumer converts sweep harmonics into added roll inertia and damping with one-way coupling and a stated sign convention; it is not a full two-way fluid/structure solution. [src/digitalmodel/solvers/openfoam/sloshing_coupling.py:L10-L40]

Mesh identity has independent shell and Python implementations, with a cross-implementation fixture test; changes to identity rules must preserve their agreement. [docs/domains/openfoam/mesh_store_case_layout.md:L54-L72]

Warm-start field reuse depends on source-time selection, reconstruction and decomposition compatibility; its relaunched chain must retain the copied fields. [src/digitalmodel/solvers/openfoam/warm_start/cli.py:L35-L85]

Repository-wide method search, including `scripts/`, found these other implementations; they are dependency leads, not independent validation evidence. [src/digitalmodel/hydrodynamics/sloshing.py:L95-L117] [scripts/capabilities/build_sloshing_explorer.py:L63-L70]

| Method | Other implementation / relationship | Evidence |
|---|---|---|
| Rectangular sloshing frequency | `hydrodynamics/sloshing.py` repeats linear tank theory; capability generator evaluates the dimensionless curve | [src/digitalmodel/hydrodynamics/sloshing.py:L95-L117] [scripts/capabilities/build_sloshing_explorer.py:L63-L70] |
| Linear dispersion | `motion_forecast/reconstruct.py` uses a bracketed root; pipeline free-span wave velocity has a vector solver | [src/digitalmodel/motion_forecast/reconstruct.py:L24-L47] [src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py:L147-L185] |
| Rectangular CFD benchmark driver | `run_sloshing_benchmark.py` records the analytical relation in manifest metadata | [scripts/cfd/run_sloshing_benchmark.py:L240-L254] |
| Force settling | `convergence_audit.audit_one` wraps `force_cycle_average.analyse` but computes its own verdict | [src/digitalmodel/solvers/openfoam/convergence_audit.py:L27-L65] |
| Grid uncertainty | `validation.referent_free_resistance` has a separate uncertainty and convergence-classification path | [src/digitalmodel/solvers/openfoam/validation/referent_free_resistance.py:L95-L114] |

The registry describes the broader `solvers` domain; upstream/downstream inventory-row mapping and the complete target-version matrix remain open documentation work, issue pending (4). [docs/registry/module-routing.yaml:L255-L260]

## 10. Coverage and open items

Coverage record: main-path source ranges cited in sections 1–6 were read; all tests under `tests/solvers/openfoam/`, including `validation/`, were attempted before and after, with solver guards retained; the table below distinguishes remaining source coverage from a test pass. [tests/solvers/openfoam/validation/conftest.py:L11-L19] [tests/solvers/openfoam/validation/test_sloshing_2d.py:L1-L10]

| Item | Disposition / responsible role | Tracking |
|---|---|---|
| Standards and source verification | UNVERIFIED statement in section 3: confirm editions, clause titles and calculation-citation sidecars; resolve decorative v2306 versus v2312 reference before claiming a supported version; maintainer | issue pending (4); [src/digitalmodel/solvers/openfoam/case_builder.py:L40-L54] [src/digitalmodel/solvers/openfoam/solver_contracts.py:L42-L45] |
| Private tank result | UNVERIFIED statement in section 4: dataset ID absent; sidecar entry: to be created; no private repository read and no result validated; maintainer | #1528 |
| Review and shared-log assignments | UNVERIFIED statement in section 8: deputy role, producing desks and shared entries unassigned; maintainer | issue pending (4) |
| Extended coverage | UNVERIFIED: complete per-field hull/3D/RAO/sweep/warm-start applicability and solver-version/inventory mapping are not established by this main-path pack; maintainer | issue pending (4); [src/digitalmodel/solvers/openfoam/hull_case.py:L113-L145] [src/digitalmodel/solvers/openfoam/sloshing_coupling.py:L37-L40] |
| CFD gate integration | y+, hydrostatic pressure balance, force balance, time-step refinement, residual-history and signed evidence-appendix integration lack a complete executable acceptance route; CFD quality manager | issue pending (3); [src/digitalmodel/solvers/openfoam/doctor.py:L77-L85] [src/digitalmodel/solvers/openfoam/convergence_audit.py:L27-L65] |
| Force-audit parity | Align the audit verdict with the decaying-envelope criterion, or state a separate meaning; maintainer | issue pending (2); [src/digitalmodel/solvers/openfoam/convergence_audit.py:L41-L64] [src/digitalmodel/solvers/openfoam/force_cycle_average.py:L384-L387] |
| Status UTF-8 failure | Baseline width-72 decoding failure; maintainer | issue pending (1); [tests/solvers/openfoam/test_cfd_status_tools.py:L49-L55] |
| Warm-start baseline failures | Six tests hit a non-case fallback output location; maintainer; no alternate write route attempted | #2052; [src/digitalmodel/solvers/openfoam/warm_start/cli.py:L88-L125] |
| Physical worked examples | No solver launched; natural-frequency battery measurements and solver-only cases not independently reproduced; CFD quality manager | #1430; [tests/solvers/openfoam/validation/test_sloshing_2d.py:L1-L10] |
| Incident follow-through | Absorber regime, outlet topology/turbulence and actual cylinder load still require solver evidence or new checks; CFD quality manager | #2043, #1979, #1261; [src/digitalmodel/solvers/openfoam/validation/wave_tank.py:L300-L318] [src/digitalmodel/solvers/openfoam/block_mesh.py:L43-L57] [src/digitalmodel/solvers/openfoam/validation/cylinder.py:L140-L154] |

Private reference pages retrieved through the read-only API, by path only: `wikis/engineering/wiki/tools/openfoam/boundary-layers-and-yplus.md`, `case-orchestration.md`, `case-types-and-solvers.md`, `cost-to-information.md`, `function-objects.md`, `resistance-decomposition-strategy.md`, `signals-that-lie.md`, `snappyhexmesh.md`; no private text is used as public engineering evidence; independent public-source verification belongs to issue pending (4).
