# Hydrodynamics module knowledge pack

| Header | Value |
|---|---|
| Module | hydrodynamics (top level) |
| Repository | digitalmodel |
| Path | `src/digitalmodel/hydrodynamics/` |
| Pack revision | 1, 2026-10-17 |
| Code revision | `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| QM role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The top-level package exposes wave spectra, coefficient storage, environmental loading, interpolation, spectral response and analytical sloshing; the routing registry describes the hydrodynamics domain as production, which is a routing classification rather than an independent validation result. [src/digitalmodel/hydrodynamics/__init__.py:L8-L35] [docs/registry/module-routing.yaml:L143-L155]

Scope is the Python files directly in the header path; sub-packages have separate implementation boundaries and are inventoried as dependencies in section 9. The propeller-rudder workflow delegates its calculations to two existing force models and writes CSV/JSON results. [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L1-L13] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L241-L311]

Analytical tank periods are a screening calculation; the oval approximation has a documented mid-fill limitation, and impact sloshing needs a different analysis. [src/digitalmodel/hydrodynamics/sloshing.py:L5-L12] [src/digitalmodel/hydrodynamics/sloshing.py:L175-L190]

U-tube dynamics use fitted inertia and loss parameters; the source withdraws an earlier modal explanation for short-period error and requires natural-period calibration instead of forced-peak calibration. No accuracy percentage from that narrative is adopted as an independently checked result here. [src/digitalmodel/hydrodynamics/utube_tank.py:L3-L33]

## 2. Inputs and outputs

Input contracts below describe implemented validation, including its omissions; defaults are code defaults, not design-basis recommendations. [src/digitalmodel/hydrodynamics/models.py:L45-L51] [src/digitalmodel/hydrodynamics/models.py:L172-L181]

| Input | Unit and implemented range | Default / source | Evidence |
|---|---|---|---|
| `WaveParameters` | Hs m, Tp s, gamma dimensionless: positive; frequency endpoints rad/s: ordered; sample count has no explicit bound check | Angular-frequency band 0.02–2.0 rad/s and 100 samples; gamma uses its declared dataclass default; swell fields do not add dispatch support | [src/digitalmodel/hydrodynamics/models.py:L150-L196] [src/digitalmodel/hydrodynamics/wave_spectra.py:L228-L262] |
| `VesselProperties` | Length, beam, draft m and displacement tonnes: positive; optional areas m² | Optional geometry and projected areas default to absent | [src/digitalmodel/hydrodynamics/models.py:L85-L117] |
| `EnvironmentalConditions` | Speed m/s: nonnegative; density kg/m³: positive; heading degrees from bow | Calm speeds and zero headings; stored wave direction uses head seas as zero | [src/digitalmodel/hydrodynamics/models.py:L216-L248] |
| Matrix database | Frequency rad/s nonnegative; matrix shape exactly 6 by 6; type added mass or damping | Caller supplies coefficient units; no entry-by-entry mixed-unit metadata | [src/digitalmodel/hydrodynamics/models.py:L34-L51] [src/digitalmodel/hydrodynamics/coefficient_database.py:L28-L74] |
| `RAOData` | Frequencies rad/s, directions degrees; amplitude/phase shapes n-frequency by n-direction by 6 | Phase returned in degrees; no default amplitude data; nearest coordinate lookup | [src/digitalmodel/hydrodynamics/models.py:L269-L322] |
| Propulsion and rudder | Diameter and offset m, area m², shaft speed rev/s, ship speed m/s or kn, angles degrees | Workflow rejects nonfinite numeric entries and nonpositive dimensions; speed units explicit | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L17-L43] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L56-L75] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L170-L216] |
| Sloshing | Dimensions m positive; fill fraction strictly between zero and one; periods s | Mode count and gravity are parameters; cylindrical modes bounded above by stored root count | [src/digitalmodel/hydrodynamics/sloshing.py:L100-L151] [src/digitalmodel/hydrodynamics/sloshing.py:L175-L217] |
| U-tube | Geometry m; conduit area m²; forcing period s; roll amplitude radians in trigonometric calls | Gravity and density are explicit parameters with module defaults; geometry positivity checked | [src/digitalmodel/hydrodynamics/utube_tank.py:L42-L76] [src/digitalmodel/hydrodynamics/utube_tank.py:L316-L360] |
| Wang interaction | Consistent length, area, density and velocity system; SI yields N and N·m | Stagger is midship-to-midship, separation centreline-to-centreline; depth absent selects deep water | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L20-L61] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L317-L341] |

| Output | Units, frame and serialization | Evidence |
|---|---|---|
| Spectrum and statistics | Angular frequency rad/s; density m²·s; periods s; CSV also contains Hz | [src/digitalmodel/hydrodynamics/wave_spectra.py:L60-L72] [src/digitalmodel/hydrodynamics/wave_spectra.py:L363-L386] [src/digitalmodel/hydrodynamics/cli.py:L89-L98] |
| Wind/current/combined loading | Surge/sway N, yaw N·m; direction degrees from bow; resultant via atan2 | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L90-L107] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L166-L187] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L220-L238] |
| Rudder forces and workflow tables | Surge forward positive, sway port positive, yaw bow-to-port positive; N and N·m; deterministic CSV/JSON | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L37-L43] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L77-L86] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L278-L311] |
| Coefficients / RAOs | JSON storage retains frequency labels; interpolation returns amplitudes and raw phases, without phase unwrapping | [src/digitalmodel/hydrodynamics/coefficient_database.py:L266-L302] [src/digitalmodel/hydrodynamics/interpolator.py:L90-L110] [src/digitalmodel/hydrodynamics/interpolator.py:L134-L163] |
| Motion response | Squared RAO times wave spectrum; significant single amplitude in the selected RAO units; exceedance dimensionless; operability percent | [src/digitalmodel/hydrodynamics/seakeeping.py:L30-L53] [src/digitalmodel/hydrodynamics/seakeeping.py:L85-L126] [src/digitalmodel/hydrodynamics/seakeeping.py:L207-L231] |
| Tank results | Sloshing natural/convective periods s and resonance flags; U-tube tank moment and transferred-volume phasors | [src/digitalmodel/hydrodynamics/sloshing.py:L67-L92] [src/digitalmodel/hydrodynamics/utube_tank.py:L83-L94] [src/digitalmodel/hydrodynamics/utube_tank.py:L350-L361] |

The six matrix coordinates are surge, sway, heave, roll, pitch and yaw about X, Y and Z; no common global origin or cross-format phase-conversion contract is established by those enums. [src/digitalmodel/hydrodynamics/models.py:L23-L30] [src/digitalmodel/hydrodynamics/models.py:L269-L322]

## 3. Method and governing standards

The table records references actually named by implementation or repository documentation; it does not certify a standard implementation. For every licensed standard below, clause text not checked against the licensed copy. [src/digitalmodel/hydrodynamics/wave_spectra.py:L12-L22] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L12-L20]

| Method / implementing definitions | Reference, edition and clause status | Evidence |
|---|---|---|
| `WaveSpectra.jonswap`, `pierson_moskowitz`, `bretschneider`, `issc`, `_normalize_to_significant_height` | DNV-RP-C205, Environmental conditions and environmental loads; code names sections 3.5.2 and 3.5.1 without edition or clause titles; normalize discrete integrated energy to requested height | [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L96] [src/digitalmodel/hydrodynamics/wave_spectra.py:L98-L213] |
| `WaveSpectra.spectral_moment`, `peak_frequency_from_spectrum`, `WaveParameters.zero_crossing_period`, `significant_height_from_spectrum`, `zero_crossing_period_from_spectrum`, `spectrum_statistics`; `seakeeping.compute_response_spectrum`, `spectral_moments`, `significant_amplitude`, `motion_exceedance`, `operability_analysis` | Trapezoidal moments, narrow-band Rayleigh model and first-criterion operability; DNV-RP-C205 and Journée & Massie, Offshore Hydromechanics, chapter 6 are named, with no standard edition | [src/digitalmodel/hydrodynamics/wave_spectra.py:L264-L386] [src/digitalmodel/hydrodynamics/models.py:L183-L196] [src/digitalmodel/hydrodynamics/seakeeping.py:L16-L19] [src/digitalmodel/hydrodynamics/seakeeping.py:L56-L231] |
| `OCIMFLoading.wind_load`, `current_load`, `combined_environmental_load`, `estimate_wind_area`, `newman_drift_force` | OCIMF, Mooring Equipment Guidelines, fourth edition, Environmental Forces; code references sections 3.3 and 3.2, without individual titles; empirical simplifications and estimated areas rather than chart interpolation | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L12-L20] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L28-L107] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L109-L239] [src/digitalmodel/hydrodynamics/ocimf_loading.py:L241-L313] |
| `CoefficientDatabase._interpolate_matrix`, `get_infinite_frequency_added_mass`; `CoefficientsInterpolator._interpolate_values`, `interpolate_rao_1d`, `interpolate_rao_2d` | Linear matrix interpolation with endpoint clamping; highest sampled added mass as infinite-frequency approximation; cubic interpolation downgrades on sparse grids, and RAOs extrapolate | [src/digitalmodel/hydrodynamics/coefficient_database.py:L143-L199] [src/digitalmodel/hydrodynamics/coefficient_database.py:L242-L264] [src/digitalmodel/hydrodynamics/interpolator.py:L26-L57] [src/digitalmodel/hydrodynamics/interpolator.py:L68-L163] |
| `kt_from_poly`, `soding_forces`, `actuator_disk_velocity`, `flat_plate_rudder_cl`, `ad_flat_plate_forces` | McTaggart (2005), sections 7–9; Carlton (2007), chapter 5; Molland & Turnock (2007); polynomial thrust, interaction force and capped lifting-line approximation | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L1-L9] [src/digitalmodel/hydrodynamics/propeller_rudder.py:L46-L205] |
| Propeller test / scale-correction documentation | ITTC procedures: Open Water Test (2014), Guidelines for Rudder and Nozzle Interaction with Propeller (2017), 1978 ITTC Performance Prediction Method (revised 2017); these are documentation references, not implemented test procedures | [docs/domains/hydrodynamics/propeller-rudder-literature.md:L409-L450] [docs/domains/hydrodynamics/propeller-rudder-literature.md:L659-L666] |
| `rectangular_tank_periods`, `cylindrical_tank_periods`, `_ellipse_equivalent_rectangle`, `oval_tank_periods`, `api650_convective_period`, `resonance_check` | Faltinsen & Timokha, Sloshing (2009), linear modal theory; API STD 650, edition unspecified, Annex E, Seismic Design of Storage Tanks; equivalent rectangle and fractional period proximity | [src/digitalmodel/hydrodynamics/sloshing.py:L38-L44] [src/digitalmodel/hydrodynamics/sloshing.py:L100-L233] |
| `natural_period`, `calibrate_effective_conduit_length`, `equivalent_damping`, `_forced_volume_response`, `_prismatic_properties`, `tank_moment`, `_quasi_static_moment_per_radian`, `coupled_roll_response`, `optimize_conduit_area` | Rigid-column inertia, fitted quadratic loss, harmonic balance, prismatic moments and bounded area grid; calibration is repository evidence, not an independent published validation | [src/digitalmodel/hydrodynamics/utube_tank.py:L111-L199] [src/digitalmodel/hydrodynamics/utube_tank.py:L202-L511] |
| Wang `_surge_deep`, `_sway_deep`, `_yaw_deep`, `_depth_sum`, `_scalar_deep`, `_integrate_legacy`, `wang_forces_scalar` | Wang (1975), Dynamic Effects of Ship Passage on Moored Vessels; Varyani, Krishnankutty & Vantorre (2003), equations 2.4–2.9 as named in source; parabolic areas, vectorized Gauss-Legendre and separate adaptive Gauss-Lobatto path | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L1-L78] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L250-L314] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L447-L540] |
| Floating-wind documentation only | DNV-RP-0286, Coupled analysis of floating wind turbines, 2019-05 amended 2021-10, Load cases set-up and data analysis; DNV-ST-0119, Floating wind turbines, 2025-12, clause not established; IEC 61400-3-2:2025, edition 1.0, clause titles not established in the note | [docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md:L43-L53] [docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md:L121-L124] [docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md:L144-L149] |

Exact edition-to-clause agreement and standard-derived output citation sidecars remain unestablished; documentation supplies edition metadata where the calculation source does not, rather than demonstrating that its formulas use that edition. This is a traceability gap, not evidence of a confirmed edition conflict; see section 10, issue pending (1). [src/digitalmodel/hydrodynamics/models.py:L34-L81] [src/digitalmodel/hydrodynamics/wave_spectra.py:L71-L96] [docs/standards/hydrodynamics-inventory.yaml:L283-L312]

## 4. Validated worked examples and regression examples

Run evidence is recorded at the header code revision on the uncommitted documentation tree; PASS below means the exact existing node passed in both the baseline and repeated offline run. Closed-form identities supply independent expectations for V1–V3; repository golden values and same-implementation consistency checks are classified as regression evidence. [tests/hydrodynamics/test_seakeeping.py:L34-L40] [tests/hydrodynamics/test_seakeeping.py:L68-L89] [tests/hydrodynamics/test_seakeeping.py:L149-L154]

| ID / class | Inputs and independent expectation or regression basis | Tolerance and basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| V1 validated, closed form | Unit flat density on angular-frequency band gives variance 1.9 m² by interval integration | 2.0 % relative, existing assertion | `tests/hydrodynamics/test_seakeeping.py::TestSpectralMoments::test_m0_of_flat_spectrum` | PASS; [tests/hydrodynamics/test_seakeeping.py:L34-L40] [tests/hydrodynamics/test_seakeeping.py:L85-L89] |
| V2 validated, closed form | Constant RAO 2 m/m gives response density four times the input by squared transfer | Relative 1e-7, absolute 0 m²·s, existing NumPy comparison | `tests/hydrodynamics/test_seakeeping.py::TestComputeResponseSpectrum::test_scaling_by_rao_squared` | PASS; [tests/hydrodynamics/test_seakeeping.py:L68-L72] |
| V3 validated, closed form | Variance 2 m² and threshold 2 m give Rayleigh exceedance exp(-1), dimensionless | Relative 1e-6, existing assertion | `tests/hydrodynamics/test_seakeeping.py::TestMotionExceedance::test_known_rayleigh_value` | PASS; [tests/hydrodynamics/test_seakeeping.py:L149-L154] |
| R1 regression | Rectangular period golden value; no independently opened reference for its stored numeric target | Absolute 0.001 s, stored assertion | `tests/hydrodynamics/test_sloshing.py::test_rectangular_fundamental_period_golden` | PASS; [tests/hydrodynamics/test_sloshing.py:L12-L17] |
| R2 regression | Spectrum statistics recover requested height; discrete normalization is part of the implementation itself | Absolute 0.5 m, stored assertion | `tests/hydrodynamics/test_hydrodynamics_unit.py::TestWaveSpectra::test_spectral_moments` | PASS; [tests/hydrodynamics/test_hydrodynamics_unit.py:L179-L192] |
| R3 regression | Opposite rudder angles give opposite sway/yaw; identifying fixture values are omitted | Relative 1e-10, dimensionless force/moment ratios | `tests/test_propeller_rudder_interaction.py::TestSodingForces::test_symmetric_rudder_angles` | PASS; [tests/test_propeller_rudder_interaction.py:L96-L101] |
| R4 regression workflow | Registered CSV/JSON sweep delegates to the existing force model; stored workflow force values are regression expectations | Four rows and stored force/moment assertions; no independent reference | `tests/workflows/test_durable_workflows.py::test_workflow_registry[propeller-rudder-interaction]` | PASS offline at the code pin (2026-10-17), 1 passed / 0 failed / 0 skipped; licensed-interface imports blocked before collection; [docs/registry/workflows.yaml:L1117-L1126] [tests/workflows/test_durable_workflows.py:L1406-L1437] [src/digitalmodel/engine.py:L28-L34] [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L5] |

No calibrated U-tube result or historical passing-ship fixture is promoted to independent validation; their evidence remains repository regression evidence and its public reuse basis needs review. [src/digitalmodel/hydrodynamics/utube_tank.py:L3-L33] [tests/hydrodynamics/test_wang_passing_ship.py:L1-L24]

## 5. QA gates

AQG r0, sections 9–11, assigns Python calculations G0 inputs/units/citations, G1 imports/tests, G2 hand or published check, and G5 evidence; G3 and G4 are not applicable to this calculation scope. Checks below are executable existing test nodes or specified static inspections; a PASS does not close the unresolved failure modes in section 6.

| Gate | Applicability | Check IDs and numerical criterion | Evidence route |
|---|---|---|---|
| G0 | Applicable | C01 response input lengths: 1 mismatched case rejected, 0 accepted mismatches (count); C02 invalid tank geometry/dimensions: 3 rejected cases, 0 unexpected accepts (count); C03 standards traceability: 0 unresolved edition/sidecar gaps (count), currently OPEN | Existing negative tests and section-3 reference audit |
| G1 | Applicable | C04 offline test selection: 0 new failures (count) between baseline and repeated run; C05 CLI output: 1 CSV created and exit code 0 (count) | Python imports occur through test collection; console CLI is the target program for these text outputs, with no solver deck loading claim |
| G2 | Applicable | C06 flat-band m0 error ≤2.0 %; C07 Rayleigh probability relative error ≤1e-6 (dimensionless); C08 RAO-squared response relative error ≤1e-7 (dimensionless) | V1, V3, V2 test nodes respectively |
| G3 | NOT_APPLICABLE | 0 time-domain solver runs required (count); top-level calculations do not constitute solver dynamics acceptance | AQG r0 section 10, Python calculations |
| G4 | NOT_APPLICABLE | 0 diffraction mesh acceptances claimed (count); imported coefficient storage does not qualify a mesh or diffraction solve | AQG r0 section 10, Python calculations |
| G5 | Applicable | C09 evidence completeness: 7 example rows, 0 missing node/results (count); 0 unresolved citation ranges (count); 0 unlisted sweep candidates (count) | Sections 4, 6 and 9; citation resolver and test-result logs retained for review |

| Check | Executable route / target quantity | Pass evidence or open limit |
|---|---|---|
| C01 | `tests/hydrodynamics/test_seakeeping.py::TestComputeResponseSpectrum::test_mismatched_lengths_raises` | PASS; [tests/hydrodynamics/test_seakeeping.py:L74-L76] |
| C02 | `tests/hydrodynamics/test_sloshing.py::test_input_validation` | PASS; [tests/hydrodynamics/test_sloshing.py:L104-L110] |
| C03 | Read every standards-reference and return payload in the section-3 implementing definitions; require explicit edition, clause title and applicable citation sidecar | OPEN: issue pending (1); no automated detection check exists; [src/digitalmodel/hydrodynamics/wave_spectra.py:L71-L96] [src/digitalmodel/hydrodynamics/models.py:L71-L81] |
| C04 | `PYTHONPATH=src uv run --no-sources python -m pytest <listed paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; repeated identical selections, no addopts override | Result counts and exceptions are in section 10; tests import the calculation package directly, without engine or licensed-interface imports; [tests/hydrodynamics/test_hydrodynamics_unit.py:L12-L25] |
| C05 | `tests/hydrodynamics/test_hydrodynamics_cli.py::TestSpectrumCommand::test_spectrum_with_output` | Existing CSV output assertion; [tests/hydrodynamics/test_hydrodynamics_cli.py:L63-L90] |
| C06 | V1 node in section 4, `spectral_moments` returning variance in m² | PASS; [tests/hydrodynamics/test_seakeeping.py:L85-L89] |
| C07 | V3 node in section 4, `motion_exceedance` returning dimensionless probability | PASS; [tests/hydrodynamics/test_seakeeping.py:L149-L154] |
| C08 | V2 node in section 4, `compute_response_spectrum` returning m²·s | PASS; [tests/hydrodynamics/test_seakeeping.py:L68-L72] |
| C09 | Resolve each `[path:Lstart-Lend]` at the code pin, reconcile each section-4 node with test logs, and reconcile each sweep row against the exact commands below | 0 missing references required; workflow R4 records offline PASS only, with no solver acceptance claim; [docs/registry/workflows.yaml:L1117-L1126] |

## 6. Known failure modes and pitfalls

sweep: 224 found

The count includes every location in the candidate inventory below. A candidate is a reviewed default, conversion, exception, unit assumption or fallback, not automatically a confirmed defect. Rows marked “no detection check exists” require the cited issue or draft; benign serialization and accumulator rows remain visible rather than being silently discarded.

Exact lexical sweep (direct files only, because sub-packages are excluded):

```sh
rg -n --max-depth 1 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|density|gravity|cache|save|write' src/digitalmodel/hydrodynamics -g '*.py'
```

The lexical sweep was supplemented with Python AST enumeration of exception handlers, continue statements, empty literal returns, function defaults, zero/epoch constants, numeric conversions, mapping defaults, min/max clamps, conditional fallbacks, zero-array construction and persistence calls. Unit and iterative-convergence paths were read manually. The following rows record each location; inherited defaults repeated by different entry points have separate rows.

| ID | Symptom / interpretation at pin | Detection and follow-up | Evidence |
|---|---|---|---|
| S001 | exception path: ImportError suppresses optional solver exports; transitive import errors can look like absence | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/__init__.py:L42-L42] |
| S002 | exception path: Exception is reported and exits nonzero; reviewed, not swallowed | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L100-L100] |
| S003 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L152-L152] |
| S004 | exception path: Exception is reported and exits nonzero; reviewed, not swallowed | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L158-L158] |
| S005 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L210-L210] |
| S006 | exception path: Exception is reported and exits nonzero; reviewed, not swallowed | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L216-L216] |
| S007 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L278-L278] |
| S008 | exception path: Exception is reported and exits nonzero; reviewed, not swallowed | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/cli.py:L285-L285] |
| S009 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L80-L80] |
| S010 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L81-L81] |
| S011 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L103-L103] |
| S012 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L205-L205] |
| S013 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L283-L283] |
| S014 | exception path: Cholesky failure becomes False; caller must inspect physical-validity flag | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L350-L350] |
| S015 | conditional fallback, numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L377-L377] |
| S016 | conditional fallback, numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L378-L378] |
| S017 | conditional fallback, numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L381-L381] |
| S018 | conditional fallback, numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L382-L382] |
| S019 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L73-L73] |
| S020 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L117-L117] |
| S021 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L169-L169] |
| S022 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L188-L188] |
| S023 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L189-L189] |
| S024 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L214-L214] |
| S025 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L233-L233] |
| S026 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L272-L272] |
| S027 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L25-L25] |
| S028 | exception path: Cholesky failure becomes False; caller must inspect physical-validity flag | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L64-L64] |
| S029 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L74-L74] |
| S030 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L75-L75] |
| S031 | conditional fallback, numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L76-L76] |
| S032 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L225-L225] |
| S033 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L228-L228] |
| S034 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L229-L229] |
| S035 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L232-L232] |
| S036 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L233-L233] |
| S037 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L32-L32] |
| S038 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L33-L33] |
| S039 | conditional fallback: Absent or zero freeboard silently becomes a beam-based estimate | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L63-L63] |
| S040 | conditional fallback: Absent or zero freeboard silently becomes a beam-based estimate | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L70-L70] |
| S041 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L102-L102] |
| S042 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L103-L103] |
| S043 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L104-L104] |
| S044 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L113-L113] |
| S045 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L114-L114] |
| S046 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L182-L182] |
| S047 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L183-L183] |
| S048 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L184-L184] |
| S049 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L233-L233] |
| S050 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L234-L234] |
| S051 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L235-L235] |
| S052 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L236-L236] |
| S053 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L237-L237] |
| S054 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L244-L244] |
| S055 | conditional fallback: Absent or zero freeboard silently becomes a beam-based estimate | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L257-L257] |
| S056 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L273-L273] |
| S057 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L308-L308] |
| S058 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L309-L309] |
| S059 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L34-L34] |
| S060 | conditional fallback: Near-zero advance-speed denominator/inflow is floored; limiting-regime applicability is unqualified | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L89-L89] |
| S061 | clamp or selection: Rudder-angle clipping is silent and can hide operation beyond capped lift model | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L144-L144] |
| S062 | clamp or selection: Near-zero advance-speed denominator/inflow is floored; limiting-regime applicability is unqualified | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L180-L180] |
| S063 | conditional fallback: Near-zero advance-speed denominator/inflow is floored; limiting-regime applicability is unqualified | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L187-L187] |
| S064 | clamp or selection: Rudder-angle clipping is silent and can hide operation beyond capped lift model | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L194-L194] |
| S065 | numeric conversion: External float conversion rejects localized separators; nonfinite values are then rejected | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L57-L57] |
| S066 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L157-L157] |
| S067 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L179-L179] |
| S068 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L188-L188] |
| S069 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L218-L218] |
| S070 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L225-L225] |
| S071 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L234-L234] |
| S072 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L281-L281] |
| S073 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L306-L306] |
| S074 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L318-L318] |
| S075 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L325-L325] |
| S076 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L59-L59] |
| S077 | conditional fallback: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L76-L76] |
| S078 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L81-L81] |
| S079 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L134-L134] |
| S080 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L135-L135] |
| S081 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L174-L174] |
| S082 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L175-L175] |
| S083 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L185-L185] |
| S084 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L186-L186] |
| S085 | conditional fallback: Empty/zero-weight scatter silently reports zero operability | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L226-L226] |
| S086 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L101-L101] |
| S087 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L122-L122] |
| S088 | clamp or selection: Near-empty/full fill is clamped within the equivalent-rectangle calculation | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L166-L166] |
| S089 | clamp or selection: Near-empty/full fill is clamped within the equivalent-rectangle calculation | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L168-L168] |
| S090 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L176-L176] |
| S091 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L208-L208] |
| S092 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L221-L221] |
| S093 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L244-L244] |
| S094 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L245-L245] |
| S095 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L246-L246] |
| S096 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L247-L247] |
| S097 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L248-L248] |
| S098 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L249-L249] |
| S099 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L286-L286] |
| S100 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L292-L292] |
| S101 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L294-L294] |
| S102 | mapping default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L295-L295] |
| S103 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L116-L116] |
| S104 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L117-L117] |
| S105 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L142-L142] |
| S106 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L168-L168] |
| S107 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L169-L169] |
| S108 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L170-L170] |
| S109 | clamp or selection: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L250-L250] |
| S110 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L322-L322] |
| S111 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L323-L323] |
| S112 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L324-L324] |
| S113 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L325-L325] |
| S114 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L386-L386] |
| S115 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L387-L387] |
| S116 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L388-L388] |
| S117 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L389-L389] |
| S118 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L390-L390] |
| S119 | clamp or selection: Finite forcing reference or amplitude floor influences nonlinear roll transfer | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L418-L418] |
| S120 | clamp or selection: Finite forcing reference or amplitude floor influences nonlinear roll transfer | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L439-L439] |
| S121 | clamp or selection: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L442-L442] |
| S122 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L455-L455] |
| S123 | clamp or selection: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L469-L469] |
| S124 | clamp or selection: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L488-L488] |
| S125 | zero output or persistence: Array initialization or persistence can retain an empty payload; no nonempty guard at this site | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L308-L308] |
| S126 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L340-L340] |
| S127 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L351-L351] |
| S128 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L352-L352] |
| S129 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L353-L353] |
| S130 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L369-L369] |
| S131 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L370-L370] |
| S132 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L371-L371] |
| S133 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L387-L387] |
| S134 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L388-L388] |
| S135 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L389-L389] |
| S136 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L405-L405] |
| S137 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L406-L406] |
| S138 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L407-L407] |
| S139 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L453-L453] |
| S140 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L468-L468] |
| S141 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L476-L476] |
| S142 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L497-L497] |
| S143 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L500-L500] |
| S144 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L521-L521] |
| S145 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L522-L522] |
| S146 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L531-L531] |
| S147 | numeric default or accumulator: Zero accumulator, enum index or declared calm/heading default; review caller intent | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L534-L534] |
| S148 | zero output or persistence: Nonpositive target/integrated energy silently becomes a zero spectrum | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L38-L38] |
| S149 | zero output or persistence: Nonpositive target/integrated energy silently becomes a zero spectrum | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L42-L42] |
| S150 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L50-L50] |
| S151 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L51-L51] |
| S152 | argument default: Fixed upper frequency can truncate high moments despite height normalization | no detection check exists for tail convergence; #2613 | [src/digitalmodel/hydrodynamics/wave_spectra.py:L52-L52] |
| S153 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L53-L53] |
| S154 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L102-L102] |
| S155 | argument default: Fixed upper frequency can truncate high moments despite height normalization | no detection check exists for tail convergence; #2613 | [src/digitalmodel/hydrodynamics/wave_spectra.py:L103-L103] |
| S156 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L104-L104] |
| S157 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L143-L143] |
| S158 | argument default: Fixed upper frequency can truncate high moments despite height normalization | no detection check exists for tail convergence; #2613 | [src/digitalmodel/hydrodynamics/wave_spectra.py:L144-L144] |
| S159 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L145-L145] |
| S160 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L180-L180] |
| S161 | argument default: Fixed upper frequency can truncate high moments despite height normalization | no detection check exists for tail convergence; #2613 | [src/digitalmodel/hydrodynamics/wave_spectra.py:L181-L181] |
| S162 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L182-L182] |
| S163 | argument default: Explicit default/fallback can change results when input is omitted; retain its declared contract | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L268-L268] |
| S164 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L285-L285] |
| S165 | numeric conversion: Internal numeric conversion/serialization; does not parse localized external text | no detection check exists in this pack for this site; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L286-L286] |
| S166 | Optional import block exposes capability only when imports succeed; no exception detail is retained | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/__init__.py:L36-L43] |
| S167 | CLI spectrum defaults and Click numeric parsing assume a dot decimal and angular-frequency SI labels | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/cli.py:L34-L45] |
| S168 | Wind CLI numeric parsing and zero heading default carry no speed/density range guard before direct loading call | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/cli.py:L105-L114] |
| S169 | Current CLI numeric parsing and zero heading default carry no nonnegative speed guard before direct loading call | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/cli.py:L163-L172] |
| S170 | Combined CLI calm-speed and heading defaults can hide omitted environment inputs | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/cli.py:L221-L228] |
| S171 | Empty coefficient database is a legitimate initial state, but save does not require populated results | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L24-L26] |
| S172 | Rounded frequency keys can overwrite close distinct frequencies while preserving both frequency-list entries | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L67-L74] |
| S173 | Out-of-range matrix frequency silently clamps to an endpoint | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L169-L177] |
| S174 | Highest sampled added mass is returned as infinite-frequency approximation without a convergence test | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L242-L264] |
| S175 | JSON import replaces database without validating shape, units or finite matrix values | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/coefficient_database.py:L285-L302] |
| S176 | Sparse cubic request silently downgrades to linear | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/interpolator.py:L26-L33] |
| S177 | Single source frequency returns constant data; larger grids extrapolate outside bounds | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/interpolator.py:L44-L55] |
| S178 | Nearest direction ignores circular wrap and phase interpolation ignores angular discontinuities | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/interpolator.py:L90-L110] |
| S179 | Two-dimensional interpolation extrapolates raw amplitudes and phases outside source grid | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/interpolator.py:L138-L153] |
| S180 | Coefficient units and frequency finiteness are not validated by matrix shape and nonnegative checks | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L34-L51] |
| S181 | Dimensional geometry/tonnage/area assumptions are declared in field comments; optional dimensions have no positivity validation | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L92-L117] |
| S182 | Wetted-surface estimate is a simplified geometric assumption | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L130-L133] |
| S183 | Wave defaults include a fixed frequency band and no positive sample-count/finite-input validation | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L156-L181] |
| S184 | Approximate zero-crossing period always applies the same peak-period ratio irrespective of spectrum selection | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/models.py:L189-L192] |
| S185 | Environment uses fixed reference densities and calm/heading defaults; no finite-value checks | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L225-L248] |
| S186 | RAO units/phases/shape are declared but coordinate monotonicity, finite values and circular nearest lookup are not checked | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L269-L322] |
| S187 | Built-in geometry templates are convenience data, not an approved design basis | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/models.py:L325-L351] |
| S188 | Fixed reference densities are used when caller omits density | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L23-L26] |
| S189 | Simplified wind coefficients and moment reference do not establish chart-based OCIMF accuracy | no detection check exists for this edge case; issue pending (1) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L75-L99] |
| S190 | Current-load empirical direction switch can introduce a force jump at quadrant boundaries | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L151-L179] |
| S191 | Drift estimate uses empirical coefficients; computed wavelength/wavenumber do not enter returned forces | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L268-L305] |
| S192 | Low-level dataclasses accept unvalidated propulsion geometry, fractions and reference density | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L17-L34] |
| S193 | Engine-off primary interaction returns zero, not the residual bare-rudder force | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L78-L80] |
| S194 | Strong braking returns zero; the exact square-root boundary remains singular | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L92-L96] |
| S195 | Fallback slipstream distance is fixed from propeller diameter; low-speed guards omit powered bollard acceleration | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L172-L195] |
| S196 | Workflow checks finite fractions but does not bound wake/thrust deduction to their physical intervals | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L170-L188] |
| S197 | Empty output-format list is accepted; output manifest can be empty without an error | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L218-L221] |
| S198 | Zip truncates unequal manually constructed speed arrays; validator-generated arrays have equal lengths | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L241-L264] |
| S199 | Writer accepts empty result rows and unsupported direct-call table formats without validation | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L278-L311] |
| S200 | Response assumes same frequency alignment and real RAO magnitudes; only length is checked | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/seakeeping.py:L30-L53] |
| S201 | Spectral integration assumes ordered angular-frequency coordinates and matching finite density values | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/seakeeping.py:L56-L82] |
| S202 | Rayleigh exceedance assumes narrow-band Gaussian peaks and squares even a negative threshold | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L107-L126] |
| S203 | Only first criterion is used; additional DOF criteria are silently ignored | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L171-L175] |
| S204 | Operability regenerates a uniform grid from endpoints and count; nonuniform RAO coordinate association is lost | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/seakeeping.py:L184-L209] |
| S205 | Fixed gravity and stored Bessel roots define units and finite cylindrical mode capacity | no detection check exists for this edge case; issue pending (1) | [src/digitalmodel/hydrodynamics/sloshing.py:L53-L58] |
| S206 | Nonpositive rectangular mode count returns an empty list; screen later indexes first period | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/sloshing.py:L109-L118] |
| S207 | Cylindrical slicing accepts zero or negative mode count, yielding empty or shortened output | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/sloshing.py:L130-L139] |
| S208 | Oval mode count is not bounded below; zero modes silently return an empty list | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/sloshing.py:L192-L204] |
| S209 | Nonpositive excitation periods are silently excluded from resonance matches; tolerance has no bound check | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L220-L233] |
| S210 | Convective period uses its own fixed gravity; absent excitations produce a false resonance flag | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/sloshing.py:L256-L266] |
| S211 | Fitted length, loss and area exponent plus reference gravity/density are defaults, not transferable validation data | no detection check exists for this edge case; issue pending (4) | [src/digitalmodel/hydrodynamics/utube_tank.py:L42-L48] |
| S212 | Harmonic-balance loop returns after iteration limit without reporting nonconvergence | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L250-L283] |
| S213 | Roll amplitude is passed to radian trigonometric functions; density and gravity must use consistent SI units | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/utube_tank.py:L316-L360] |
| S214 | Nonlinear roll iteration returns after its limit without a convergence-status field | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L415-L445] |
| S215 | Optimum is the best sampled grid point over a fixed forcing band, without a grid-convergence estimate | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/utube_tank.py:L448-L511] |
| S216 | Passing-force inputs require a consistent unit system and source-specific sign convention | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L56-L61] |
| S217 | Finite image count and quadrature nodes are fixed defaults, with no automatic convergence refinement | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L108-L115] |
| S218 | Only quadrature nodes/weights are cached; empty result data are not cached | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L204-L207] |
| S219 | Negative image count can create an empty image list and hence zero finite-depth forces | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L284-L291] |
| S220 | Small initial panel estimates are silently omitted from adaptive integration; relative error budget is local | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L468-L483] |
| S221 | Zero-spectrum fallback can hide an invalid frequency grid or an underflowed positive-energy spectrum | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L35-L44] |
| S222 | Moment quadrature assumes ordered, finite, consistently scaled coordinates and density values | no detection check exists for this edge case; issue pending (3) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L264-L286] |
| S223 | Zero spectral energy produces an undefined zero-crossing ratio without an explicit guard | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L325-L328] |
| S224 | Zero moments and finite-band tail truncation can invalidate period/width statistics | no detection check exists for this edge case; issue pending (2) | [src/digitalmodel/hydrodynamics/wave_spectra.py:L363-L374] |

All sweep rows are an inspection record dated 2026-10-16. No executable `continue`, literal `return None`, empty-list/dict literal error return or epoch-year fallback occurred in the direct Python files; zero-array fallbacks, indirect empty-list results, and empty persisted payload risks are included above. CLI exceptions terminate explicitly; Cholesky exceptions return diagnostic flags rather than silently accepting a matrix. [src/digitalmodel/hydrodynamics/cli.py:L100-L102] [src/digitalmodel/hydrodynamics/models.py:L59-L65] [src/digitalmodel/hydrodynamics/coefficient_database.py:L346-L351]

| Incident / confirmed limitation | Detection and disposition | Evidence |
|---|---|---|
| #2613: spectrum tail / period-moment convergence | No detection check exists for automatic tail convergence; C06 tests variance, not tail fidelity | [src/digitalmodel/hydrodynamics/wave_spectra.py:L98-L137] [src/digitalmodel/hydrodynamics/wave_spectra.py:L308-L328] |
| #2543: normalization defect in other calculation paths | Not the same denominator defect here: discrete normalization multiplies by target/current energy; C04 covers the existing normalization regression tests, not other packages | [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [tests/hydrodynamics/test_hydrodynamics_unit.py:L179-L192] |
| #2526: offline collection crosses licensed-interface imports | R4 PASS offline with the optional interface blocked before collection; no detection check exists in the workflow test to avoid engine-level imports; this issue tracks the guarded import only | [tests/workflows/test_durable_workflows.py:L13-L13] [src/digitalmodel/engine.py:L28-L34] [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L5] |

## 7. Lessons log

| Date | Entry | Lesson and pack change | Evidence | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Initial pack | Discrete height normalization does not establish tail convergence; retain period-moment limitation and separate regression from independent identities | [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [src/digitalmodel/hydrodynamics/wave_spectra.py:L308-L328] | Not assigned; #2613 |
| 2026-10-16 | Initial pack | Low-level force helpers and the workflow validator have different input guarantees; document both contracts | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L17-L34] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L141-L216] | Not assigned; issue pending (3) |

## 8. Maintainer

Declared review route for this pack: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing function: calculation documentation. Ambiguous method classification and tolerance/edition changes go to the family quality manager; cross-family questions go to the maintainer. These are role assignments for review, not a claim that review or clearance occurred.

## 9. Dependencies and implementation inventory

NumPy supplies array calculations, SciPy supplies RAO interpolation, Click supplies the command interface, and pandas supplies the spectrum CSV table. These are calculation dependencies, without a solver-version qualification. [src/digitalmodel/hydrodynamics/wave_spectra.py:L7-L9] [src/digitalmodel/hydrodynamics/interpolator.py:L7-L10] [src/digitalmodel/hydrodynamics/cli.py:L7-L20]

Every direct-source mention of AQWA/OrcaWave is recorded here as text only: coefficient storage describes data originating from AQWA, WAMIT or OrcaWave; analytical sloshing describes coupling outside diffraction scope. Neither location launches or loads those programs. [src/digitalmodel/hydrodynamics/coefficient_database.py:L14-L21] [src/digitalmodel/hydrodynamics/sloshing.py:L5-L12]

The registered propeller-rudder workflow routes through the engine and writes a CSV and JSON table; offline marking in the registry does not prevent its test module from importing the engine. [docs/registry/workflows.yaml:L1117-L1126] [tests/workflows/test_durable_workflows.py:L13-L13] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L324-L338]

Sub-package boundaries observed at the pin: `aqwa`, `bemrosetta`, `capytaine`, `diffraction`, `hull_library`, `parametric_hull_analysis`, `passing_ship`, `planing_hull`, `rao_analysis`, `rao_tabulation`, `river_currents`, `wave_spectrum`, `well_datasets`; their internal implementation and tests are excluded from this top-level pack. [src/digitalmodel/hydrodynamics/aqwa/__init__.py:L1-L8] [src/digitalmodel/hydrodynamics/bemrosetta/__init__.py:L1-L8] [src/digitalmodel/hydrodynamics/capytaine/__init__.py:L1-L8] [src/digitalmodel/hydrodynamics/diffraction/__init__.py:L1-L8] [src/digitalmodel/hydrodynamics/hull_library/__init__.py:L1-L8]

### Map revision and counted families

Map revision: `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23` (the pack code pin); data/code-search revision named by the map: `270c8759c374b67a7f032953e7fcdb14e0224461`. All map citations below resolve at the pack code pin. [docs/registry/implementation-map.yaml:L1-L4]

| Family | In-package implementation rows | Full map inventory rows | Counted search |
|---|---|---|---|
| `wave-spectra` | 11 | 141 | Counted; [docs/registry/implementation-map.yaml:L13593-L13594] |
| `rao-readers-and-orcaflex-converters` | 6 | 295 | Counted; [docs/registry/implementation-map.yaml:L9509-L9510] |
| `wind-and-current-force-routes` | 3 | 135 | Counted; [docs/registry/implementation-map.yaml:L15393-L15394] |
| `linear-wave-kinematics` | 1 | 21 | Additional family, inventory only; [docs/registry/implementation-map.yaml:L26253-L26254] |

Families are ranked by direct-file row count, with map order preserved on ties. For each counted family, every `search_signals.pattern` was passed unchanged to Python `re.search`, case-sensitive. The exclusion compares each pattern against `hydrodynamics`, the package path below `src/digitalmodel/`, case-insensitively; no pattern matched that path, so no signal was omitted. Each line hit was assigned to its smallest enclosing function or class, or the module definition; docstring-only matches are not independent implementations.

| Counted family | Hit definitions | Classification |
|---|---|---|
| `wave-spectra` | 27 | Every computing definition is in section 3 or the inventory below; remaining hits are containers, initialization, exports, prose or delegates; 0 further unlisted implementations |
| `rao-readers-and-orcaflex-converters` | 7 | Every computing definition is in section 3 or the inventory below; remaining hits are containers, initialization, exports, prose or delegates; 0 further unlisted implementations |
| `wind-and-current-force-routes` | 15 | Every computing definition is in section 3 or the inventory below; remaining hits are containers, initialization, exports, prose or delegates; 0 further unlisted implementations |

Definition-level dispositions (C = computation named in section 3/inventory; H = helper/data/export; P = module prose/imports or class prose, not another algorithm):

| Family | Definition at pin | Disposition | Evidence |
|---|---|---|---|
| `wave-spectra` | `__init__.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/__init__.py:L1-L1] |
| `wave-spectra` | `cli.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/cli.py:L1-L1] |
| `wave-spectra` | `cli.py` / `spectrum_cmd` | H | [src/digitalmodel/hydrodynamics/cli.py:L46-L102] |
| `wave-spectra` | `models.py` / `WaveSpectrumType` | H | [src/digitalmodel/hydrodynamics/models.py:L13-L20] |
| `wave-spectra` | `models.py` / `WaveParameters` | H | [src/digitalmodel/hydrodynamics/models.py:L150-L212] |
| `wave-spectra` | `models.py` / `zero_crossing_period` | C | [src/digitalmodel/hydrodynamics/models.py:L189-L192] |
| `wave-spectra` | `models.py` / `frequency_array` | H | [src/digitalmodel/hydrodynamics/models.py:L194-L196] |
| `wave-spectra` | `models.py` / `to_dict` | H | [src/digitalmodel/hydrodynamics/models.py:L198-L212] |
| `wave-spectra` | `seakeeping.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/seakeeping.py:L1-L1] |
| `wave-spectra` | `seakeeping.py` / `compute_response_spectrum` | C | [src/digitalmodel/hydrodynamics/seakeeping.py:L30-L53] |
| `wave-spectra` | `seakeeping.py` / `spectral_moments` | C | [src/digitalmodel/hydrodynamics/seakeeping.py:L56-L82] |
| `wave-spectra` | `seakeeping.py` / `motion_exceedance` | C | [src/digitalmodel/hydrodynamics/seakeeping.py:L107-L126] |
| `wave-spectra` | `seakeeping.py` / `operability_analysis` | C | [src/digitalmodel/hydrodynamics/seakeeping.py:L129-L231] |
| `wave-spectra` | `wave_spectra.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/wave_spectra.py:L1-L1] |
| `wave-spectra` | `wave_spectra.py` / `WaveSpectra` | P | [src/digitalmodel/hydrodynamics/wave_spectra.py:L12-L386] |
| `wave-spectra` | `wave_spectra.py` / `__init__` | H | [src/digitalmodel/hydrodynamics/wave_spectra.py:L25-L27] |
| `wave-spectra` | `wave_spectra.py` / `_normalize_to_significant_height` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] |
| `wave-spectra` | `wave_spectra.py` / `jonswap` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L46-L96] |
| `wave-spectra` | `wave_spectra.py` / `pierson_moskowitz` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L98-L137] |
| `wave-spectra` | `wave_spectra.py` / `bretschneider` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L139-L174] |
| `wave-spectra` | `wave_spectra.py` / `issc` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L176-L213] |
| `wave-spectra` | `wave_spectra.py` / `generate_spectrum` | H | [src/digitalmodel/hydrodynamics/wave_spectra.py:L215-L262] |
| `wave-spectra` | `wave_spectra.py` / `spectral_moment` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L264-L286] |
| `wave-spectra` | `wave_spectra.py` / `significant_height_from_spectrum` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L288-L306] |
| `wave-spectra` | `wave_spectra.py` / `zero_crossing_period_from_spectrum` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L308-L328] |
| `wave-spectra` | `wave_spectra.py` / `peak_frequency_from_spectrum` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L330-L346] |
| `wave-spectra` | `wave_spectra.py` / `spectrum_statistics` | C | [src/digitalmodel/hydrodynamics/wave_spectra.py:L348-L386] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `__init__` | H | [src/digitalmodel/hydrodynamics/interpolator.py:L22-L24] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `load_raos` | H | [src/digitalmodel/hydrodynamics/interpolator.py:L59-L66] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `interpolate_rao_1d` | C | [src/digitalmodel/hydrodynamics/interpolator.py:L68-L110] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `interpolate_rao_2d` | C | [src/digitalmodel/hydrodynamics/interpolator.py:L112-L163] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `interpolate_all_dofs` | H | [src/digitalmodel/hydrodynamics/interpolator.py:L165-L207] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `extract_rao_at_frequency` | C | [src/digitalmodel/hydrodynamics/interpolator.py:L230-L267] |
| `rao-readers-and-orcaflex-converters` | `interpolator.py` / `extract_rao_at_direction` | C | [src/digitalmodel/hydrodynamics/interpolator.py:L269-L306] |
| `wind-and-current-force-routes` | `__init__.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/__init__.py:L1-L1] |
| `wind-and-current-force-routes` | `cli.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/cli.py:L1-L1] |
| `wind-and-current-force-routes` | `cli.py` / `ocimf_wind_cmd` | H | [src/digitalmodel/hydrodynamics/cli.py:L115-L160] |
| `wind-and-current-force-routes` | `cli.py` / `ocimf_current_cmd` | H | [src/digitalmodel/hydrodynamics/cli.py:L173-L218] |
| `wind-and-current-force-routes` | `cli.py` / `combined_env_cmd` | H | [src/digitalmodel/hydrodynamics/cli.py:L229-L287] |
| `wind-and-current-force-routes` | `models.py` / `VesselProperties` | H | [src/digitalmodel/hydrodynamics/models.py:L85-L146] |
| `wind-and-current-force-routes` | `models.py` / `EnvironmentalConditions` | H | [src/digitalmodel/hydrodynamics/models.py:L216-L265] |
| `wind-and-current-force-routes` | `models.py` / `__post_init__` | H | [src/digitalmodel/hydrodynamics/models.py:L239-L248] |
| `wind-and-current-force-routes` | `models.py` / `to_dict` | H | [src/digitalmodel/hydrodynamics/models.py:L250-L265] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `<module>` | P | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L1-L1] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `OCIMFLoading` | P | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L12-L313] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `__init__` | H | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L23-L26] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `wind_load` | C | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L28-L107] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `current_load` | C | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L109-L187] |
| `wind-and-current-force-routes` | `ocimf_loading.py` / `combined_environmental_load` | C | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L189-L239] |

A8 result: PASS, 0 further unlisted computing definitions across the three counted families. Inventory includes every map row for all four module families, including rows outside the package and in excluded sub-packages; absence of a map entry does not establish absence of code.

### Repository-wide additional search and map follow-up

The same exact signal sets were searched over all tracked Python files, including `scripts/`, without importing them; this wider search is a discovery inventory and does not change the package A8 result.

| Family | Matching files | Matching lines | Matching scripts files |
|---|---|---|---|
| `wave-spectra` | 494 | 3237 | 60 |
| `rao-readers-and-orcaflex-converters` | 410 | 2151 | 65 |
| `wind-and-current-force-routes` | 295 | 1985 | 23 |
| `linear-wave-kinematics` | 848 | 6478 | 64 |

Map follow-up: add the direct environmental-load calculators, spectral-moment/peak helpers and response helpers presently covered by section 3 but absent as map rows; add method families with in-package rows for propeller-rudder interaction, U-tube dynamics and the full analytical sloshing screen; add the separate Wang production and scalar implementations. External discovery candidates require classification under #1803. These gaps do not add counted families before the map contains their in-package rows. [src/digitalmodel/hydrodynamics/ocimf_loading.py:L28-L313] [src/digitalmodel/hydrodynamics/wave_spectra.py:L264-L346] [src/digitalmodel/hydrodynamics/propeller_rudder.py:L46-L205] [src/digitalmodel/hydrodynamics/utube_tank.py:L111-L511] [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L250-L540]

### Complete map-family row inventory

Each row retains its ordinal within the named family's implementation list and its declared relation. A source path is an inventory locator, not a claim that this pack executed it. Identifying discovery names, where present, are replaced by their map ordinal and cited map location; codenaming remains open under issue pending (6).

| Family / row | Path | Definition | Relation | Map evidence |
|---|---|---|---|---|
| `wave-spectra` / 1 | `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | independent | [docs/registry/implementation-map.yaml:L13600-L13606] |
| `wave-spectra` / 2 | `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | independent | [docs/registry/implementation-map.yaml:L13612-L13618] |
| `wave-spectra` / 3 | `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | example | [docs/registry/implementation-map.yaml:L13624-L13630] |
| `wave-spectra` / 4 | `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | example | [docs/registry/implementation-map.yaml:L13636-L13642] |
| `wave-spectra` / 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example | [docs/registry/implementation-map.yaml:L13648-L13654] |
| `wave-spectra` / 6 | `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | example | [docs/registry/implementation-map.yaml:L13663-L13669] |
| `wave-spectra` / 7 | `scripts/_seed_eq_mock.py` | `mock_time_series` | independent | [docs/registry/implementation-map.yaml:L13675-L13681] |
| `wave-spectra` / 8 | `scripts/benchmark/extract_pdf_plots.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13687-L13693] |
| `wave-spectra` / 9 | `scripts/benchmark/validate_owd_vs_spec.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13698-L13704] |
| `wave-spectra` / 10 | `scripts/extract_property_inventory.py` | `build_dormancy_rules` | independent | [docs/registry/implementation-map.yaml:L13709-L13715] |
| `wave-spectra` / 11 | `scripts/generate_env_files.py` | `create_wave_file` | independent | [docs/registry/implementation-map.yaml:L13721-L13727] |
| `wave-spectra` / 12 | `scripts/generate_env_files.py` | `main` | independent | [docs/registry/implementation-map.yaml:L13733-L13739] |
| `wave-spectra` / 13 | `scripts/ingest_orcina_help.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13747-L13753] |
| `wave-spectra` / 14 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13758-L13764] |
| `wave-spectra` / 15 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13769-L13775] |
| `wave-spectra` / 16 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13780-L13786] |
| `wave-spectra` / 17 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13791-L13797] |
| `wave-spectra` / 18 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13802-L13808] |
| `wave-spectra` / 19 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13813-L13819] |
| `wave-spectra` / 20 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13824-L13830] |
| `wave-spectra` / 21 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13835-L13841] |
| `wave-spectra` / 22 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13846-L13852] |
| `wave-spectra` / 23 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13857-L13863] |
| `wave-spectra` / 24 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13868-L13874] |
| `wave-spectra` / 25 | `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | independent | [docs/registry/implementation-map.yaml:L13879-L13885] |
| `wave-spectra` / 26 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L13891-L13897] |
| `wave-spectra` / 27 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent | [docs/registry/implementation-map.yaml:L13903-L13909] |
| `wave-spectra` / 28 | `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | independent | [docs/registry/implementation-map.yaml:L13915-L13921] |
| `wave-spectra` / 29 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | independent | [docs/registry/implementation-map.yaml:L13927-L13933] |
| `wave-spectra` / 30 | `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | independent | [docs/registry/implementation-map.yaml:L13939-L13945] |
| `wave-spectra` / 31 | `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | independent | [docs/registry/implementation-map.yaml:L13951-L13957] |
| `wave-spectra` / 32 | `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | independent | [docs/registry/implementation-map.yaml:L13965-L13971] |
| `wave-spectra` / 33 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | independent | [docs/registry/implementation-map.yaml:L13977-L13983] |
| `wave-spectra` / 34 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | independent | [docs/registry/implementation-map.yaml:L13989-L13995] |
| `wave-spectra` / 35 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | independent | [docs/registry/implementation-map.yaml:L14005-L14011] |
| `wave-spectra` / 36 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | independent | [docs/registry/implementation-map.yaml:L14018-L14024] |
| `wave-spectra` / 37 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | independent | [docs/registry/implementation-map.yaml:L14031-L14037] |
| `wave-spectra` / 38 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | wrapper | [docs/registry/implementation-map.yaml:L14044-L14050] |
| `wave-spectra` / 39 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | independent | [docs/registry/implementation-map.yaml:L14061-L14067] |
| `wave-spectra` / 40 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | independent | [docs/registry/implementation-map.yaml:L14073-L14079] |
| `wave-spectra` / 41 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | independent | [docs/registry/implementation-map.yaml:L14085-L14091] |
| `wave-spectra` / 42 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L14097-L14103] |
| `wave-spectra` / 43 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | independent | [docs/registry/implementation-map.yaml:L14111-L14117] |
| `wave-spectra` / 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | independent | [docs/registry/implementation-map.yaml:L14123-L14129] |
| `wave-spectra` / 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | independent | [docs/registry/implementation-map.yaml:L14135-L14141] |
| `wave-spectra` / 46 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | independent | [docs/registry/implementation-map.yaml:L14147-L14153] |
| `wave-spectra` / 47 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent | [docs/registry/implementation-map.yaml:L14159-L14165] |
| `wave-spectra` / 48 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | independent | [docs/registry/implementation-map.yaml:L14173-L14179] |
| `wave-spectra` / 49 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | independent | [docs/registry/implementation-map.yaml:L14187-L14193] |
| `wave-spectra` / 50 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14200-L14206] |
| `wave-spectra` / 51 | `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | copy | [docs/registry/implementation-map.yaml:L14214-L14220] |
| `wave-spectra` / 52 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L14226-L14232] |
| `wave-spectra` / 53 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent | [docs/registry/implementation-map.yaml:L14238-L14244] |
| `wave-spectra` / 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L14218-L14231] |
| `wave-spectra` / 55 | `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | independent | [docs/registry/implementation-map.yaml:L14262-L14268] |
| `wave-spectra` / 56 | `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | independent | [docs/registry/implementation-map.yaml:L14276-L14282] |
| `wave-spectra` / 57 | `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | independent | [docs/registry/implementation-map.yaml:L14288-L14294] |
| `wave-spectra` / 58 | `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14302-L14308] |
| `wave-spectra` / 59 | `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | independent | [docs/registry/implementation-map.yaml:L14316-L14322] |
| `wave-spectra` / 60 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | independent | [docs/registry/implementation-map.yaml:L14329-L14335] |
| `wave-spectra` / 61 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | independent | [docs/registry/implementation-map.yaml:L14343-L14349] |
| `wave-spectra` / 62 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | independent | [docs/registry/implementation-map.yaml:L14357-L14363] |
| `wave-spectra` / 63 | `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | independent | [docs/registry/implementation-map.yaml:L14369-L14375] |
| `wave-spectra` / 64 | `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | independent | [docs/registry/implementation-map.yaml:L14381-L14387] |
| `wave-spectra` / 65 | `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | independent | [docs/registry/implementation-map.yaml:L14393-L14399] |
| `wave-spectra` / 66 | `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | wrapper | [docs/registry/implementation-map.yaml:L14409-L14415] |
| `wave-spectra` / 67 | `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | independent | [docs/registry/implementation-map.yaml:L14422-L14428] |
| `wave-spectra` / 68 | `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | independent | [docs/registry/implementation-map.yaml:L14434-L14440] |
| `wave-spectra` / 69 | `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | independent | [docs/registry/implementation-map.yaml:L14446-L14452] |
| `wave-spectra` / 70 | `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | independent | [docs/registry/implementation-map.yaml:L14460-L14466] |
| `wave-spectra` / 71 | `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | independent | [docs/registry/implementation-map.yaml:L14478-L14484] |
| `wave-spectra` / 72 | `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | independent | [docs/registry/implementation-map.yaml:L14490-L14496] |
| `wave-spectra` / 73 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | independent | [docs/registry/implementation-map.yaml:L14502-L14508] |
| `wave-spectra` / 74 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | independent | [docs/registry/implementation-map.yaml:L14514-L14520] |
| `wave-spectra` / 75 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | wrapper | [docs/registry/implementation-map.yaml:L14526-L14532] |
| `wave-spectra` / 76 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | independent | [docs/registry/implementation-map.yaml:L14540-L14546] |
| `wave-spectra` / 77 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | copy | [docs/registry/implementation-map.yaml:L14552-L14558] |
| `wave-spectra` / 78 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | independent | [docs/registry/implementation-map.yaml:L14564-L14570] |
| `wave-spectra` / 79 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | independent | [docs/registry/implementation-map.yaml:L14578-L14584] |
| `wave-spectra` / 80 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | independent | [docs/registry/implementation-map.yaml:L14592-L14598] |
| `wave-spectra` / 81 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | copy | [docs/registry/implementation-map.yaml:L14606-L14612] |
| `wave-spectra` / 82 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | copy | [docs/registry/implementation-map.yaml:L14618-L14624] |
| `wave-spectra` / 83 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | independent | [docs/registry/implementation-map.yaml:L14630-L14636] |
| `wave-spectra` / 84 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent | [docs/registry/implementation-map.yaml:L14642-L14648] |
| `wave-spectra` / 85 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | independent | [docs/registry/implementation-map.yaml:L14656-L14662] |
| `wave-spectra` / 86 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14668-L14674] |
| `wave-spectra` / 87 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14680-L14686] |
| `wave-spectra` / 88 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14692-L14698] |
| `wave-spectra` / 89 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14704-L14710] |
| `wave-spectra` / 90 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | independent | [docs/registry/implementation-map.yaml:L14716-L14722] |
| `wave-spectra` / 91 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | independent | [docs/registry/implementation-map.yaml:L14728-L14734] |
| `wave-spectra` / 92 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | independent | [docs/registry/implementation-map.yaml:L14740-L14746] |
| `wave-spectra` / 93 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | independent | [docs/registry/implementation-map.yaml:L14752-L14758] |
| `wave-spectra` / 94 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | independent | [docs/registry/implementation-map.yaml:L14764-L14770] |
| `wave-spectra` / 95 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14776-L14782] |
| `wave-spectra` / 96 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | independent | [docs/registry/implementation-map.yaml:L14792-L14798] |
| `wave-spectra` / 97 | `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | independent | [docs/registry/implementation-map.yaml:L14807-L14813] |
| `wave-spectra` / 98 | `src/digitalmodel/workflows/installation_full_report.py` | `_design` | independent | [docs/registry/implementation-map.yaml:L14819-L14825] |
| `wave-spectra` / 99 | `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | independent | [docs/registry/implementation-map.yaml:L14831-L14837] |
| `wave-spectra` / 100 | `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | independent | [docs/registry/implementation-map.yaml:L14843-L14849] |
| `wave-spectra` / 101 | `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | independent | [docs/registry/implementation-map.yaml:L14855-L14861] |
| `wave-spectra` / 102 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | independent | [docs/registry/implementation-map.yaml:L14867-L14873] |
| `wave-spectra` / 103 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L14879-L14885] |
| `wave-spectra` / 104 | `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14893-L14899] |
| `wave-spectra` / 105 | `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14907-L14913] |
| `wave-spectra` / 106 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L14921-L14927] |
| `wave-spectra` / 107 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14933-L14939] |
| `wave-spectra` / 108 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | test_helper | [docs/registry/implementation-map.yaml:L14945-L14951] |
| `wave-spectra` / 109 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | test_helper | [docs/registry/implementation-map.yaml:L14957-L14963] |
| `wave-spectra` / 110 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | test_helper | [docs/registry/implementation-map.yaml:L14969-L14975] |
| `wave-spectra` / 111 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | test_helper | [docs/registry/implementation-map.yaml:L14981-L14987] |
| `wave-spectra` / 112 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | test_helper | [docs/registry/implementation-map.yaml:L14993-L14999] |
| `wave-spectra` / 113 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper | [docs/registry/implementation-map.yaml:L15005-L15011] |
| `wave-spectra` / 114 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper | [docs/registry/implementation-map.yaml:L15017-L15023] |
| `wave-spectra` / 115 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper | [docs/registry/implementation-map.yaml:L15029-L15035] |
| `wave-spectra` / 116 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper | [docs/registry/implementation-map.yaml:L15041-L15047] |
| `wave-spectra` / 117 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper | [docs/registry/implementation-map.yaml:L15053-L15059] |
| `wave-spectra` / 118 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper | [docs/registry/implementation-map.yaml:L15065-L15071] |
| `wave-spectra` / 119 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper | [docs/registry/implementation-map.yaml:L15077-L15083] |
| `wave-spectra` / 120 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper | [docs/registry/implementation-map.yaml:L15089-L15095] |
| `wave-spectra` / 121 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper | [docs/registry/implementation-map.yaml:L15101-L15107] |
| `wave-spectra` / 122 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper | [docs/registry/implementation-map.yaml:L15113-L15119] |
| `wave-spectra` / 123 | `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | test_helper | [docs/registry/implementation-map.yaml:L15125-L15131] |
| `wave-spectra` / 124 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | test_helper | [docs/registry/implementation-map.yaml:L15137-L15143] |
| `wave-spectra` / 125 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | test_helper | [docs/registry/implementation-map.yaml:L15149-L15155] |
| `wave-spectra` / 126 | `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | test_helper | [docs/registry/implementation-map.yaml:L15161-L15167] |
| `wave-spectra` / 127 | `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | test_helper | [docs/registry/implementation-map.yaml:L15173-L15179] |
| `wave-spectra` / 128 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | test_helper | [docs/registry/implementation-map.yaml:L15185-L15191] |
| `wave-spectra` / 129 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15199-L15205] |
| `wave-spectra` / 130 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15211-L15217] |
| `wave-spectra` / 131 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15223-L15229] |
| `wave-spectra` / 132 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | test_helper | [docs/registry/implementation-map.yaml:L15235-L15241] |
| `wave-spectra` / 133 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | test_helper | [docs/registry/implementation-map.yaml:L15247-L15253] |
| `wave-spectra` / 134 | `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | test_helper | [docs/registry/implementation-map.yaml:L15259-L15265] |
| `wave-spectra` / 135 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L15271-L15277] |
| `wave-spectra` / 136 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L15285-L15291] |
| `wave-spectra` / 137 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper | [docs/registry/implementation-map.yaml:L15299-L15305] |
| `wave-spectra` / 138 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper | [docs/registry/implementation-map.yaml:L15315-L15321] |
| `wave-spectra` / 139 | `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L15329-L15335] |
| `wave-spectra` / 140 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` | caller | [docs/registry/implementation-map.yaml:L15343-L15350] |
| `wave-spectra` / 141 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range.ordinate` | independent | [docs/registry/implementation-map.yaml:L15362-L15369] |
| `rao-readers-and-orcaflex-converters` / 1 | `docs/domains/diffraction/examples/orcawave_conversion_example.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9516-L9522] |
| `rao-readers-and-orcaflex-converters` / 2 | `docs/domains/orcawave/L01_aqwa_benchmark/compare_model_inputs.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9528-L9534] |
| `rao-readers-and-orcaflex-converters` / 3 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` | `extract_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L9540-L9546] |
| `rao-readers-and-orcaflex-converters` / 4 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` | `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9552-L9558] |
| `rao-readers-and-orcaflex-converters` / 5 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9564-L9570] |
| `rao-readers-and-orcaflex-converters` / 6 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9579-L9585] |
| `rao-readers-and-orcaflex-converters` / 7 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` | `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9590-L9596] |
| `rao-readers-and-orcaflex-converters` / 8 | `docs/domains/orcawave/L01_aqwa_benchmark/monitor_orcawave_run.py` | `monitor_progress` | independent | [docs/registry/implementation-map.yaml:L9602-L9608] |
| `rao-readers-and-orcaflex-converters` / 9 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` | `extract_aqwa_data` | independent | [docs/registry/implementation-map.yaml:L9614-L9620] |
| `rao-readers-and-orcaflex-converters` / 10 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` | `extract_orcawave_data` | independent | [docs/registry/implementation-map.yaml:L9626-L9632] |
| `rao-readers-and-orcaflex-converters` / 11 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9638-L9644] |
| `rao-readers-and-orcaflex-converters` / 12 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` | `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9653-L9659] |
| `rao-readers-and-orcaflex-converters` / 13 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9665-L9671] |
| `rao-readers-and-orcaflex-converters` / 14 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` | `extract_orcawave_raos` | independent | [docs/registry/implementation-map.yaml:L9679-L9685] |
| `rao-readers-and-orcaflex-converters` / 15 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9691-L9697] |
| `rao-readers-and-orcaflex-converters` / 16 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9705-L9711] |
| `rao-readers-and-orcaflex-converters` / 17 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9716-L9722] |
| `rao-readers-and-orcaflex-converters` / 18 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` | `extract_orcawave_motion_raos` | independent | [docs/registry/implementation-map.yaml:L9728-L9734] |
| `rao-readers-and-orcaflex-converters` / 19 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9740-L9746] |
| `rao-readers-and-orcaflex-converters` / 20 | `docs/domains/orcawave/L01_aqwa_benchmark/validate_matched_model.py` | `main` | independent | [docs/registry/implementation-map.yaml:L9754-L9760] |
| `rao-readers-and-orcaflex-converters` / 21 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py` | `run_orcawave_diffraction` | independent | [docs/registry/implementation-map.yaml:L9766-L9772] |
| `rao-readers-and-orcaflex-converters` / 22 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9778-L9784] |
| `rao-readers-and-orcaflex-converters` / 23 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` | `_check_raos_finite` | independent | [docs/registry/implementation-map.yaml:L9789-L9795] |
| `rao-readers-and-orcaflex-converters` / 24 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` | `_check_heave_rao_long_period` | independent | [docs/registry/implementation-map.yaml:L9801-L9807] |
| `rao-readers-and-orcaflex-converters` / 25 | `examples/domains/core/cache_example.py` | `example_realistic_usage.load_hydrodynamic_database` | example | [docs/registry/implementation-map.yaml:L9813-L9819] |
| `rao-readers-and-orcaflex-converters` / 26 | `examples/domains/validation/validation_pipeline_example.py` | `example_rao_polar_data_validation` | example | [docs/registry/implementation-map.yaml:L9827-L9833] |
| `rao-readers-and-orcaflex-converters` / 27 | `scripts/benchmark/compare_hydro_properties.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9839-L9845] |
| `rao-readers-and-orcaflex-converters` / 28 | `scripts/benchmark/compare_hydro_properties.py` | `extract_aqwa_damping` | independent | [docs/registry/implementation-map.yaml:L9850-L9856] |
| `rao-readers-and-orcaflex-converters` / 29 | `scripts/benchmark/compare_hydro_properties.py` | `extract_aqwa_added_mass` | independent | [docs/registry/implementation-map.yaml:L9862-L9868] |
| `rao-readers-and-orcaflex-converters` / 30 | `scripts/benchmark/compare_hydro_properties.py` | `parse_args` | independent | [docs/registry/implementation-map.yaml:L9874-L9880] |
| `rao-readers-and-orcaflex-converters` / 31 | `scripts/benchmark/digitize_validation_plots.py` | `parse_args` | independent | [docs/registry/implementation-map.yaml:L9886-L9892] |
| `rao-readers-and-orcaflex-converters` / 32 | `scripts/benchmark/probe_owd_content.py` | `_extract_core_data` | independent | [docs/registry/implementation-map.yaml:L9898-L9904] |
| `rao-readers-and-orcaflex-converters` / 33 | `scripts/benchmark/probe_owd_content.py` | `probe_body_details` | independent | [docs/registry/implementation-map.yaml:L9910-L9916] |
| `rao-readers-and-orcaflex-converters` / 34 | `scripts/benchmark/qtf_postprocessing.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9922-L9928] |
| `rao-readers-and-orcaflex-converters` / 35 | `scripts/benchmark/regenerate_barge_benchmark.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9933-L9939] |
| `rao-readers-and-orcaflex-converters` / 36 | `scripts/benchmark/regenerate_ship_benchmark.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9944-L9950] |
| `rao-readers-and-orcaflex-converters` / 37 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L9955-L9961] |
| `rao-readers-and-orcaflex-converters` / 38 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `_extract_correlations_from_json` | independent | [docs/registry/implementation-map.yaml:L9966-L9972] |
| `rao-readers-and-orcaflex-converters` / 39 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `_extract_correlations_from_runner` | independent | [docs/registry/implementation-map.yaml:L9978-L9984] |
| `rao-readers-and-orcaflex-converters` / 40 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `_run_ship_benchmark` | independent | [docs/registry/implementation-map.yaml:L9990-L9996] |
| `rao-readers-and-orcaflex-converters` / 41 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `_run_spar_benchmark` | independent | [docs/registry/implementation-map.yaml:L10005-L10011] |
| `rao-readers-and-orcaflex-converters` / 42 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10018-L10024] |
| `rao-readers-and-orcaflex-converters` / 43 | `scripts/benchmark/run_3way_benchmark.py` | `run_orcawave` | independent | [docs/registry/implementation-map.yaml:L10033-L10039] |
| `rao-readers-and-orcaflex-converters` / 44 | `scripts/benchmark/run_3way_benchmark.py` | `run_aqwa` | independent | [docs/registry/implementation-map.yaml:L10047-L10053] |
| `rao-readers-and-orcaflex-converters` / 45 | `scripts/benchmark/run_3way_benchmark.py` | `_extract_from_owr` | independent | [docs/registry/implementation-map.yaml:L10061-L10067] |
| `rao-readers-and-orcaflex-converters` / 46 | `scripts/benchmark/run_3way_benchmark.py` | `_extract_from_aqwa_lis` | independent | [docs/registry/implementation-map.yaml:L10073-L10079] |
| `rao-readers-and-orcaflex-converters` / 47 | `scripts/benchmark/run_3way_benchmark.py` | `_parse_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L10088-L10094] |
| `rao-readers-and-orcaflex-converters` / 48 | `scripts/benchmark/run_3way_benchmark.py` | `_parse_aqwa_matrix_section` | independent | [docs/registry/implementation-map.yaml:L10100-L10106] |
| `rao-readers-and-orcaflex-converters` / 49 | `scripts/benchmark/run_spar_benchmark.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10112-L10118] |
| `rao-readers-and-orcaflex-converters` / 50 | `scripts/benchmark/run_spar_benchmark.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10123-L10129] |
| `rao-readers-and-orcaflex-converters` / 51 | `scripts/benchmark/run_validation_benchmark.py` | `_run_orcawave_for_case` | independent | [docs/registry/implementation-map.yaml:L10135-L10141] |
| `rao-readers-and-orcaflex-converters` / 52 | `scripts/benchmark/run_validation_benchmark.py` | `_load_wamit_reference` | independent | [docs/registry/implementation-map.yaml:L10147-L10153] |
| `rao-readers-and-orcaflex-converters` / 53 | `scripts/benchmark/solve_owd_extract.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10159-L10165] |
| `rao-readers-and-orcaflex-converters` / 54 | `scripts/benchmark/solve_owd_extract.py` | `solve_and_extract` | independent | [docs/registry/implementation-map.yaml:L10170-L10176] |
| `rao-readers-and-orcaflex-converters` / 55 | `scripts/benchmark/validate_owd_vs_spec.py` | `_extract_from_diffraction` | independent | [docs/registry/implementation-map.yaml:L10182-L10188] |
| `rao-readers-and-orcaflex-converters` / 56 | `scripts/benchmark/validate_owd_vs_spec.py` | `solve_owd` | independent | [docs/registry/implementation-map.yaml:L10194-L10200] |
| `rao-readers-and-orcaflex-converters` / 57 | `scripts/benchmark/validate_owd_vs_spec.py` | `solve_spec` | independent | [docs/registry/implementation-map.yaml:L10208-L10214] |
| `rao-readers-and-orcaflex-converters` / 58 | `scripts/benchmark/validate_owd_vs_spec.py` | `_solve_spec_via_orcfxapi` | independent | [docs/registry/implementation-map.yaml:L10223-L10229] |
| `rao-readers-and-orcaflex-converters` / 59 | `scripts/build_results_workbook.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10236-L10242] |
| `rao-readers-and-orcaflex-converters` / 60 | `scripts/populate_rao_database.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10247-L10253] |
| `rao-readers-and-orcaflex-converters` / 61 | `scripts/python/digitalmodel/legacy/aqwa_data_helper.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10261-L10267] |
| `rao-readers-and-orcaflex-converters` / 62 | `scripts/python/digitalmodel/legacy/check_aqwa_heading_fix.py` | `test_actual_file_counts` | independent | [docs/registry/implementation-map.yaml:L10273-L10279] |
| `rao-readers-and-orcaflex-converters` / 63 | `scripts/python/digitalmodel/legacy/check_aqwa_parser_fix.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10285-L10291] |
| `rao-readers-and-orcaflex-converters` / 64 | `scripts/python/digitalmodel/legacy/diagnose_rao_parsing.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10297-L10303] |
| `rao-readers-and-orcaflex-converters` / 65 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` | `parse_rao_section_simple` | independent | [docs/registry/implementation-map.yaml:L10308-L10314] |
| `rao-readers-and-orcaflex-converters` / 66 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10320-L10326] |
| `rao-readers-and-orcaflex-converters` / 67 | `scripts/python/digitalmodel/legacy/fix_rao_plots.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10334-L10340] |
| `rao-readers-and-orcaflex-converters` / 68 | `scripts/python/digitalmodel/legacy/fix_rao_plots_standalone.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10346-L10352] |
| `rao-readers-and-orcaflex-converters` / 69 | `scripts/python/digitalmodel/legacy/generate_rao_qa_report.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10357-L10363] |
| `rao-readers-and-orcaflex-converters` / 70 | `scripts/python/digitalmodel/legacy/regenerate_rao_plots.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10369-L10375] |
| `rao-readers-and-orcaflex-converters` / 71 | `scripts/python/digitalmodel/legacy/simple_rao_plot.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L10381-L10387] |
| `rao-readers-and-orcaflex-converters` / 72 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` | `test_rao_section` | independent | [docs/registry/implementation-map.yaml:L10392-L10398] |
| `rao-readers-and-orcaflex-converters` / 73 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10404-L10410] |
| `rao-readers-and-orcaflex-converters` / 74 | `scripts/rao_quality_check.py` | `parse_args` | independent | [docs/registry/implementation-map.yaml:L10416-L10422] |
| `rao-readers-and-orcaflex-converters` / 75 | `scripts/rao_quality_check.py` | `process_file` | independent | [docs/registry/implementation-map.yaml:L10428-L10434] |
| `rao-readers-and-orcaflex-converters` / 76 | `scripts/run_benchmark_ship_raos.py` | `create_diffraction_results` | independent | [docs/registry/implementation-map.yaml:L10440-L10446] |
| `rao-readers-and-orcaflex-converters` / 77 | `scripts/run_benchmark_ship_raos.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10452-L10458] |
| `rao-readers-and-orcaflex-converters` / 78 | `scripts/run_semisub_quality_check.py` | `main` | independent | [docs/registry/implementation-map.yaml:L10466-L10472] |
| `rao-readers-and-orcaflex-converters` / 79 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` | `GenericRAOClient._generate_generic_raos` | independent | [docs/registry/implementation-map.yaml:L10478-L10484] |
| `rao-readers-and-orcaflex-converters` / 80 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` | `GenericRAOClient.interpolate_raos` | independent | [docs/registry/implementation-map.yaml:L10490-L10496] |
| `rao-readers-and-orcaflex-converters` / 81 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` | `GenericRAOClient.stream_raos_orcaflex` | independent | [docs/registry/implementation-map.yaml:L10502-L10508] |
| `rao-readers-and-orcaflex-converters` / 82 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` | `GenericRAOClient.query_by_date` | caller | [docs/registry/implementation-map.yaml:L10514-L10520] |
| `rao-readers-and-orcaflex-converters` / 83 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` | `VesselClient._initialize_clients` | independent | [docs/registry/implementation-map.yaml:L10528-L10534] |
| `rao-readers-and-orcaflex-converters` / 84 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_vessel_type` | independent | [docs/registry/implementation-map.yaml:L10540-L10546] |
| `rao-readers-and-orcaflex-converters` / 85 | `src/digitalmodel/drilling_riser/global_model/raos.py` | `parse_direction_grouped_rao_text` | independent | [docs/registry/implementation-map.yaml:L10552-L10558] |
| `rao-readers-and-orcaflex-converters` / 86 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` | `AqwaRAOs.get_natural_period_from_lis_file` | independent | [docs/registry/implementation-map.yaml:L10564-L10570] |
| `rao-readers-and-orcaflex-converters` / 87 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` | `AqwaRAOs.get_percent_critical_damping` | independent | [docs/registry/implementation-map.yaml:L10576-L10582] |
| `rao-readers-and-orcaflex-converters` / 88 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` | `AqwaRAOs.get_absolute_damping` | independent | [docs/registry/implementation-map.yaml:L10588-L10594] |
| `rao-readers-and-orcaflex-converters` / 89 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` | `AqwaRAOs.get_raos_by_cfg` | independent | [docs/registry/implementation-map.yaml:L10600-L10606] |
| `rao-readers-and-orcaflex-converters` / 90 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` | `create_parser` | independent | [docs/registry/implementation-map.yaml:L10612-L10618] |
| `rao-readers-and-orcaflex-converters` / 91 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` | `list_methods` | independent | [docs/registry/implementation-map.yaml:L10624-L10630] |
| `rao-readers-and-orcaflex-converters` / 92 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` | `AqwaReader.get_change_of_data_result_group` | independent | [docs/registry/implementation-map.yaml:L10636-L10642] |
| `rao-readers-and-orcaflex-converters` / 93 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` | `AqwaReader.get_property_args_array` | independent | [docs/registry/implementation-map.yaml:L10648-L10654] |
| `rao-readers-and-orcaflex-converters` / 94 | `src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py` | `ViscousDampingDetermination._process_case` | independent | [docs/registry/implementation-map.yaml:L10660-L10666] |
| `rao-readers-and-orcaflex-converters` / 95 | `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` | `get_module_info` | independent | [docs/registry/implementation-map.yaml:L10672-L10678] |
| `rao-readers-and-orcaflex-converters` / 96 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | `convert` | independent | [docs/registry/implementation-map.yaml:L10684-L10690] |
| `rao-readers-and-orcaflex-converters` / 97 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | `_export_raos_csv` | independent | [docs/registry/implementation-map.yaml:L10700-L10706] |
| `rao-readers-and-orcaflex-converters` / 98 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` | `status` | independent | [docs/registry/implementation-map.yaml:L10712-L10718] |
| `rao-readers-and-orcaflex-converters` / 99 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | `OrcaFlexConverter._call_orcaflex_exporter` | independent | [docs/registry/implementation-map.yaml:L10726-L10732] |
| `rao-readers-and-orcaflex-converters` / 100 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | `OrcaFlexConverter._build_vessel_yaml` | independent | [docs/registry/implementation-map.yaml:L10738-L10744] |
| `rao-readers-and-orcaflex-converters` / 101 | `src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py` | `DATHandler._parse_aqwa_deck` | independent | [docs/registry/implementation-map.yaml:L10750-L10756] |
| `rao-readers-and-orcaflex-converters` / 102 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` | `AQWAParser.supported_extensions` | independent | [docs/registry/implementation-map.yaml:L10762-L10768] |
| `rao-readers-and-orcaflex-converters` / 103 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` | `AQWAParser.parse` | independent | [docs/registry/implementation-map.yaml:L10776-L10782] |
| `rao-readers-and-orcaflex-converters` / 104 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` | `AQWAParser._extract_raos` | independent | [docs/registry/implementation-map.yaml:L10790-L10796] |
| `rao-readers-and-orcaflex-converters` / 105 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._extract_rao_data` | caller | [docs/registry/implementation-map.yaml:L10802-L10808] |
| `rao-readers-and-orcaflex-converters` / 106 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._extract_added_mass_data` | caller | [docs/registry/implementation-map.yaml:L10816-L10822] |
| `rao-readers-and-orcaflex-converters` / 107 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._extract_damping_data` | caller | [docs/registry/implementation-map.yaml:L10829-L10835] |
| `rao-readers-and-orcaflex-converters` / 108 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._build_rao_set` | independent | [docs/registry/implementation-map.yaml:L10842-L10848] |
| `rao-readers-and-orcaflex-converters` / 109 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._build_added_mass_set` | independent | [docs/registry/implementation-map.yaml:L10854-L10860] |
| `rao-readers-and-orcaflex-converters` / 110 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` | `AQWAConverter._build_damping_set` | independent | [docs/registry/implementation-map.yaml:L10866-L10872] |
| `rao-readers-and-orcaflex-converters` / 111 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.extract_frequencies_and_periods` | independent | [docs/registry/implementation-map.yaml:L10878-L10884] |
| `rao-readers-and-orcaflex-converters` / 112 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.extract_headings` | independent | [docs/registry/implementation-map.yaml:L10892-L10898] |
| `rao-readers-and-orcaflex-converters` / 113 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser._table_span` | independent | [docs/registry/implementation-map.yaml:L10906-L10912] |
| `rao-readers-and-orcaflex-converters` / 114 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.parse_added_mass_table` | caller | [docs/registry/implementation-map.yaml:L10918-L10924] |
| `rao-readers-and-orcaflex-converters` / 115 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.parse_damping_table` | caller | [docs/registry/implementation-map.yaml:L10934-L10940] |
| `rao-readers-and-orcaflex-converters` / 116 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.parse_rao_table` | independent | [docs/registry/implementation-map.yaml:L10949-L10955] |
| `rao-readers-and-orcaflex-converters` / 117 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `AQWALISParser.parse_all` | wrapper | [docs/registry/implementation-map.yaml:L10963-L10969] |
| `rao-readers-and-orcaflex-converters` / 118 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `parse_aqwa_lis_file` | independent | [docs/registry/implementation-map.yaml:L10980-L10986] |
| `rao-readers-and-orcaflex-converters` / 119 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `extract_aqwa_frequencies` | independent | [docs/registry/implementation-map.yaml:L10992-L10998] |
| `rao-readers-and-orcaflex-converters` / 120 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `extract_aqwa_headings` | independent | [docs/registry/implementation-map.yaml:L11004-L11010] |
| `rao-readers-and-orcaflex-converters` / 121 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `extract_aqwa_added_mass` | independent | [docs/registry/implementation-map.yaml:L11016-L11022] |
| `rao-readers-and-orcaflex-converters` / 122 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `extract_aqwa_damping` | independent | [docs/registry/implementation-map.yaml:L11028-L11034] |
| `rao-readers-and-orcaflex-converters` / 123 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` | `extract_aqwa_raos` | independent | [docs/registry/implementation-map.yaml:L11040-L11046] |
| `rao-readers-and-orcaflex-converters` / 124 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_result_extractor.py` | `AQWAResultExtractor.extract` | independent | [docs/registry/implementation-map.yaml:L11052-L11058] |
| `rao-readers-and-orcaflex-converters` / 125 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py` | `AQWARunner._capture_lis_file` | independent | [docs/registry/implementation-map.yaml:L11064-L11070] |
| `rao-readers-and-orcaflex-converters` / 126 | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | `convert_aqwa` | independent | [docs/registry/implementation-map.yaml:L11076-L11082] |
| `rao-readers-and-orcaflex-converters` / 127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` | `create_parser` | independent | [docs/registry/implementation-map.yaml:L11090-L11096] |
| `rao-readers-and-orcaflex-converters` / 128 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | `OrcaFlexExporter.export_raos_csv` | independent | [docs/registry/implementation-map.yaml:L11102-L11108] |
| `rao-readers-and-orcaflex-converters` / 129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | `OrcaFlexExporter._write_rao_sheet` | independent | [docs/registry/implementation-map.yaml:L11114-L11120] |
| `rao-readers-and-orcaflex-converters` / 130 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | `OrcaFlexExporter._write_discretization_sheet` | independent | [docs/registry/implementation-map.yaml:L11126-L11132] |
| `rao-readers-and-orcaflex-converters` / 131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | `OrcaWaveRunner._execute_via_api` | independent | [docs/registry/implementation-map.yaml:L11138-L11144] |
| `rao-readers-and-orcaflex-converters` / 132 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | `OrcaWaveRunner._read_live_attrs` | independent | [docs/registry/implementation-map.yaml:L11150-L11156] |
| `rao-readers-and-orcaflex-converters` / 133 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | `OrcaWaveRunner._build_results_from_object` | independent | [docs/registry/implementation-map.yaml:L11164-L11170] |
| `rao-readers-and-orcaflex-converters` / 134 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_test_utilities.py` | `MockDataGenerator.create_mock_vessel` | independent | [docs/registry/implementation-map.yaml:L11176-L11182] |
| `rao-readers-and-orcaflex-converters` / 135 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` | `convert_orcawave_xlsx_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L11188-L11194] |
| `rao-readers-and-orcaflex-converters` / 136 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L11203-L11209] |
| `rao-readers-and-orcaflex-converters` / 137 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` | `PolarsExporter.export_all_csv` | independent | [docs/registry/implementation-map.yaml:L11214-L11220] |
| `rao-readers-and-orcaflex-converters` / 138 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` | `PolarsExporter._build_rao_records` | independent | [docs/registry/implementation-map.yaml:L11226-L11232] |
| `rao-readers-and-orcaflex-converters` / 139 | `src/digitalmodel/hydrodynamics/diffraction/report_generator.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L11238-L11244] |
| `rao-readers-and-orcaflex-converters` / 140 | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` | `OrcaWaveInputParser.parse` | independent | [docs/registry/implementation-map.yaml:L11249-L11255] |
| `rao-readers-and-orcaflex-converters` / 141 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` | `OrcaWaveConverter._extract_rao_data` | caller | [docs/registry/implementation-map.yaml:L11261-L11267] |
| `rao-readers-and-orcaflex-converters` / 142 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` | `OrcaWaveConverter._build_rao_set` | independent | [docs/registry/implementation-map.yaml:L11275-L11281] |
| `rao-readers-and-orcaflex-converters` / 143 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` | `OrcaWaveConverter._build_added_mass_set` | independent | [docs/registry/implementation-map.yaml:L11287-L11293] |
| `rao-readers-and-orcaflex-converters` / 144 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` | `OrcaWaveConverter._build_damping_set` | independent | [docs/registry/implementation-map.yaml:L11299-L11305] |
| `rao-readers-and-orcaflex-converters` / 145 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` | `OrcaWaveDataExtractor.extract_rao_frequencies` | independent | [docs/registry/implementation-map.yaml:L11311-L11317] |
| `rao-readers-and-orcaflex-converters` / 146 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` | `OrcaWaveDataExtractor.extract_rao_headings` | independent | [docs/registry/implementation-map.yaml:L11323-L11329] |
| `rao-readers-and-orcaflex-converters` / 147 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` | `extract_all_rao_data` | independent | [docs/registry/implementation-map.yaml:L11335-L11341] |
| `rao-readers-and-orcaflex-converters` / 148 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` | `WamitReferenceLoader.from_owd` | independent | [docs/registry/implementation-map.yaml:L11347-L11353] |
| `rao-readers-and-orcaflex-converters` / 149 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` | `WamitReferenceLoader.from_yaml` | independent | [docs/registry/implementation-map.yaml:L11361-L11367] |
| `rao-readers-and-orcaflex-converters` / 150 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` | `WamitReferenceLoader._build_results` | independent | [docs/registry/implementation-map.yaml:L11375-L11381] |
| `rao-readers-and-orcaflex-converters` / 151 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` | `_attempt_rao_lookup` | independent | [docs/registry/implementation-map.yaml:L11389-L11395] |
| `rao-readers-and-orcaflex-converters` / 152 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` | `setup_hull_analysis` | caller | [docs/registry/implementation-map.yaml:L11401-L11407] |
| `rao-readers-and-orcaflex-converters` / 153 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` | `HullCatalog.compute_motions` | independent | [docs/registry/implementation-map.yaml:L11415-L11421] |
| `rao-readers-and-orcaflex-converters` / 154 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` | `HullCatalog.compute_accelerations` | independent | [docs/registry/implementation-map.yaml:L11427-L11433] |
| `rao-readers-and-orcaflex-converters` / 155 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` | `RAODatabase.save_to_disk` | independent | [docs/registry/implementation-map.yaml:L11439-L11445] |
| `rao-readers-and-orcaflex-converters` / 156 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` | `RAODatabase.load_from_disk` | independent | [docs/registry/implementation-map.yaml:L11451-L11457] |
| `rao-readers-and-orcaflex-converters` / 157 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` | `PipelineXlsxReader.read_rao_data` | independent | [docs/registry/implementation-map.yaml:L11463-L11469] |
| `rao-readers-and-orcaflex-converters` / 158 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` | `NativeXlsxReader.read_rao_data` | independent | [docs/registry/implementation-map.yaml:L11475-L11481] |
| `rao-readers-and-orcaflex-converters` / 159 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` | `xlsx_to_rao_data` | independent | [docs/registry/implementation-map.yaml:L11487-L11493] |
| `rao-readers-and-orcaflex-converters` / 160 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` | `xlsx_to_hydro_coefficients` | independent | [docs/registry/implementation-map.yaml:L11499-L11505] |
| `rao-readers-and-orcaflex-converters` / 161 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` | `populate_database_from_xlsx` | independent | [docs/registry/implementation-map.yaml:L11511-L11517] |
| `rao-readers-and-orcaflex-converters` / 162 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_comparison.py` | `compare_box_exports` | independent | [docs/registry/implementation-map.yaml:L11525-L11531] |
| `rao-readers-and-orcaflex-converters` / 163 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `_read_units` | independent | [docs/registry/implementation-map.yaml:L11537-L11543] |
| `rao-readers-and-orcaflex-converters` / 164 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `_same_content` | independent | [docs/registry/implementation-map.yaml:L11549-L11555] |
| `rao-readers-and-orcaflex-converters` / 165 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `store_fixture_entry` | caller | [docs/registry/implementation-map.yaml:L11561-L11567] |
| `rao-readers-and-orcaflex-converters` / 166 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `populate_fixture_database` | independent | [docs/registry/implementation-map.yaml:L11575-L11581] |
| `rao-readers-and-orcaflex-converters` / 167 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `_verify_readback` | caller | [docs/registry/implementation-map.yaml:L11591-L11597] |
| `rao-readers-and-orcaflex-converters` / 168 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` | `regenerate_fixture_database` | independent | [docs/registry/implementation-map.yaml:L11604-L11610] |
| `rao-readers-and-orcaflex-converters` / 169 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` | `RaoRegistry.register_rao` | caller | [docs/registry/implementation-map.yaml:L11620-L11626] |
| `rao-readers-and-orcaflex-converters` / 170 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` | `RaoRegistry._store_content` | independent | [docs/registry/implementation-map.yaml:L11634-L11640] |
| `rao-readers-and-orcaflex-converters` / 171 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` | `RaoRegistry.load_rao_data` | caller | [docs/registry/implementation-map.yaml:L11649-L11655] |
| `rao-readers-and-orcaflex-converters` / 172 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` | `_read_payload` | independent | [docs/registry/implementation-map.yaml:L11663-L11669] |
| `rao-readers-and-orcaflex-converters` / 173 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.load_raos` | independent | [docs/registry/implementation-map.yaml:L11675-L11681] |
| `rao-readers-and-orcaflex-converters` / 174 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.interpolate_rao_1d` | independent | [docs/registry/implementation-map.yaml:L11687-L11693] |
| `rao-readers-and-orcaflex-converters` / 175 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.interpolate_rao_2d` | independent | [docs/registry/implementation-map.yaml:L11699-L11705] |
| `rao-readers-and-orcaflex-converters` / 176 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.interpolate_all_dofs` | caller | [docs/registry/implementation-map.yaml:L11711-L11717] |
| `rao-readers-and-orcaflex-converters` / 177 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.extract_rao_at_frequency` | independent | [docs/registry/implementation-map.yaml:L11725-L11731] |
| `rao-readers-and-orcaflex-converters` / 178 | `src/digitalmodel/hydrodynamics/interpolator.py` | `CoefficientsInterpolator.extract_rao_at_direction` | independent | [docs/registry/implementation-map.yaml:L11737-L11743] |
| `rao-readers-and-orcaflex-converters` / 179 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` | `run_parametric_sweep` | independent | [docs/registry/implementation-map.yaml:L11749-L11755] |
| `rao-readers-and-orcaflex-converters` / 180 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` | `_to_rao_db_entry` | independent | [docs/registry/implementation-map.yaml:L11763-L11769] |
| `rao-readers-and-orcaflex-converters` / 181 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.read_orcaflex_displacement_raos` | independent | [docs/registry/implementation-map.yaml:L11775-L11781] |
| `rao-readers-and-orcaflex-converters` / 182 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.get_rao_data_for_vessel` | independent | [docs/registry/implementation-map.yaml:L11787-L11793] |
| `rao-readers-and-orcaflex-converters` / 183 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L11799-L11805] |
| `rao-readers-and-orcaflex-converters` / 184 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` | `_database` | independent | [docs/registry/implementation-map.yaml:L11813-L11819] |
| `rao-readers-and-orcaflex-converters` / 185 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` | `_rao_array` | independent | [docs/registry/implementation-map.yaml:L11825-L11831] |
| `rao-readers-and-orcaflex-converters` / 186 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` | `_interpolated_rows` | caller | [docs/registry/implementation-map.yaml:L11837-L11843] |
| `rao-readers-and-orcaflex-converters` / 187 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` | `_check_query_bounds` | independent | [docs/registry/implementation-map.yaml:L11851-L11857] |
| `rao-readers-and-orcaflex-converters` / 188 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `_extract_rao_dict` | independent | [docs/registry/implementation-map.yaml:L11863-L11869] |
| `rao-readers-and-orcaflex-converters` / 189 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `load_rao_set_from_artifact` | independent | [docs/registry/implementation-map.yaml:L11875-L11881] |
| `rao-readers-and-orcaflex-converters` / 190 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent | [docs/registry/implementation-map.yaml:L11889-L11895] |
| `rao-readers-and-orcaflex-converters` / 191 | `src/digitalmodel/marine_ops/installation/models.py` | `Vessel.has_complete_raos` | independent | [docs/registry/implementation-map.yaml:L11901-L11907] |
| `rao-readers-and-orcaflex-converters` / 192 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` | `AQWAEnhancedParser.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L11913-L11919] |
| `rao-readers-and-orcaflex-converters` / 193 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` | `AQWAEnhancedParser._parse_rao_section_enhanced` | independent | [docs/registry/implementation-map.yaml:L11927-L11933] |
| `rao-readers-and-orcaflex-converters` / 194 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` | `AQWAEnhancedParser._extract_section_lines` | independent | [docs/registry/implementation-map.yaml:L11939-L11945] |
| `rao-readers-and-orcaflex-converters` / 195 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` | `AQWAEnhancedParser.extract_drag_table` | caller | [docs/registry/implementation-map.yaml:L11951-L11957] |
| `rao-readers-and-orcaflex-converters` / 196 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` | `AQWAEnhancedParser.export_to_csv` | independent | [docs/registry/implementation-map.yaml:L11965-L11971] |
| `rao-readers-and-orcaflex-converters` / 197 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` | `AQWAReader.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L11977-L11983] |
| `rao-readers-and-orcaflex-converters` / 198 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` | `AQWAReader._parse_rao_section` | independent | [docs/registry/implementation-map.yaml:L11991-L11997] |
| `rao-readers-and-orcaflex-converters` / 199 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` | `AQWAReader._extract_last_rao_section` | independent | [docs/registry/implementation-map.yaml:L12003-L12009] |
| `rao-readers-and-orcaflex-converters` / 200 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` | `AQWAReader._parse_frequencies` | independent | [docs/registry/implementation-map.yaml:L12015-L12021] |
| `rao-readers-and-orcaflex-converters` / 201 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` | `AQWAReader._parse_headings` | independent | [docs/registry/implementation-map.yaml:L12027-L12033] |
| `rao-readers-and-orcaflex-converters` / 202 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` | `AQWAReaderFixed.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L12039-L12045] |
| `rao-readers-and-orcaflex-converters` / 203 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` | `AQWAReaderFixed._parse_rao_section_fixed` | independent | [docs/registry/implementation-map.yaml:L12053-L12059] |
| `rao-readers-and-orcaflex-converters` / 204 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader.parse_yml_file` | caller | [docs/registry/implementation-map.yaml:L12065-L12071] |
| `rao-readers-and-orcaflex-converters` / 205 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader._parse_orcaflex_rao_data` | wrapper | [docs/registry/implementation-map.yaml:L12079-L12085] |
| `rao-readers-and-orcaflex-converters` / 206 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader._extract_headings` | independent | [docs/registry/implementation-map.yaml:L12096-L12102] |
| `rao-readers-and-orcaflex-converters` / 207 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader._extract_frequencies` | independent | [docs/registry/implementation-map.yaml:L12108-L12114] |
| `rao-readers-and-orcaflex-converters` / 208 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader._fill_rao_values` | independent | [docs/registry/implementation-map.yaml:L12120-L12126] |
| `rao-readers-and-orcaflex-converters` / 209 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` | `OrcaFlexReader._parse_table_format` | independent | [docs/registry/implementation-map.yaml:L12132-L12138] |
| `rao-readers-and-orcaflex-converters` / 210 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` | `AQWALISParser.parse_lis_file` | caller | [docs/registry/implementation-map.yaml:L12144-L12150] |
| `rao-readers-and-orcaflex-converters` / 211 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` | `AQWALISParser._extract_displacement_raos` | caller | [docs/registry/implementation-map.yaml:L12160-L12166] |
| `rao-readers-and-orcaflex-converters` / 212 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` | `AQWALISParser._extract_velocity_raos` | caller | [docs/registry/implementation-map.yaml:L12174-L12180] |
| `rao-readers-and-orcaflex-converters` / 213 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` | `AQWALISParser._extract_acceleration_raos` | caller | [docs/registry/implementation-map.yaml:L12187-L12193] |
| `rao-readers-and-orcaflex-converters` / 214 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` | `AQWALISParser._parse_rao_section` | independent | [docs/registry/implementation-map.yaml:L12200-L12206] |
| `rao-readers-and-orcaflex-converters` / 215 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser.parse_yml_file` | caller | [docs/registry/implementation-map.yaml:L12212-L12218] |
| `rao-readers-and-orcaflex-converters` / 216 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser._navigate_to_draught` | independent | [docs/registry/implementation-map.yaml:L12227-L12233] |
| `rao-readers-and-orcaflex-converters` / 217 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser._parse_orcaflex_rao_structure` | caller | [docs/registry/implementation-map.yaml:L12239-L12245] |
| `rao-readers-and-orcaflex-converters` / 218 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser._extract_headings` | copy | [docs/registry/implementation-map.yaml:L12255-L12261] |
| `rao-readers-and-orcaflex-converters` / 219 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser._extract_frequencies` | independent | [docs/registry/implementation-map.yaml:L12267-L12273] |
| `rao-readers-and-orcaflex-converters` / 220 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` | `OrcaFlexYMLParser._fill_rao_values` | independent | [docs/registry/implementation-map.yaml:L12279-L12285] |
| `rao-readers-and-orcaflex-converters` / 221 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` | `RAOInterpolator.interpolate_2d` | independent | [docs/registry/implementation-map.yaml:L12291-L12297] |
| `rao-readers-and-orcaflex-converters` / 222 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` | `RAOInterpolator.interpolate_1d_frequency` | independent | [docs/registry/implementation-map.yaml:L12303-L12309] |
| `rao-readers-and-orcaflex-converters` / 223 | `Discovery file with identifying name` | `Definition identified by map row ordinal` | independent | [docs/registry/implementation-map.yaml:L12315-L12321] |
| `rao-readers-and-orcaflex-converters` / 224 | `Discovery file with identifying name` | `Definition identified by map row ordinal` | independent | [docs/registry/implementation-map.yaml:L12327-L12333] |
| `rao-readers-and-orcaflex-converters` / 225 | `Discovery file with identifying name` | `Definition identified by map row ordinal` | independent | [docs/registry/implementation-map.yaml:L12339-L12345] |
| `rao-readers-and-orcaflex-converters` / 226 | `Discovery file with identifying name` | `Definition identified by map row ordinal` | independent | [docs/registry/implementation-map.yaml:L12351-L12357] |
| `rao-readers-and-orcaflex-converters` / 227 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` | `RAODataValidators._detect_phase_convention` | caller | [docs/registry/implementation-map.yaml:L12363-L12369] |
| `rao-readers-and-orcaflex-converters` / 228 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` | `RAODataValidators._find_phase_convention_in_data` | independent | [docs/registry/implementation-map.yaml:L12377-L12383] |
| `rao-readers-and-orcaflex-converters` / 229 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` | `RAODataValidators._extract_orcaflex_raos` | independent | [docs/registry/implementation-map.yaml:L12389-L12395] |
| `rao-readers-and-orcaflex-converters` / 230 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `RAOReaderError.get_user_message` | independent | [docs/registry/implementation-map.yaml:L12401-L12407] |
| `rao-readers-and-orcaflex-converters` / 231 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader.read` | caller | [docs/registry/implementation-map.yaml:L12413-L12419] |
| `rao-readers-and-orcaflex-converters` / 232 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader.read_aqwa_lis` | wrapper | [docs/registry/implementation-map.yaml:L12428-L12434] |
| `rao-readers-and-orcaflex-converters` / 233 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader._detect_format` | independent | [docs/registry/implementation-map.yaml:L12442-L12448] |
| `rao-readers-and-orcaflex-converters` / 234 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader._read_aqwa_lis` | independent | [docs/registry/implementation-map.yaml:L12454-L12460] |
| `rao-readers-and-orcaflex-converters` / 235 | `src/digitalmodel/marine_ops/marine_analysis/visualization/rao_plotter.py` | `main` | independent | [docs/registry/implementation-map.yaml:L12466-L12472] |
| `rao-readers-and-orcaflex-converters` / 236 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` | `read_rao_workbook` | independent | [docs/registry/implementation-map.yaml:L12478-L12484] |
| `rao-readers-and-orcaflex-converters` / 237 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` | `real_rao_datasets` | independent | [docs/registry/implementation-map.yaml:L12490-L12496] |
| `rao-readers-and-orcaflex-converters` / 238 | `src/digitalmodel/marine_ops/vessel_db/promote.py` | `promote` | independent | [docs/registry/implementation-map.yaml:L12504-L12510] |
| `rao-readers-and-orcaflex-converters` / 239 | `src/digitalmodel/motion_forecast/rao_adapter.py` | `GridRAO.from_file` | independent | [docs/registry/implementation-map.yaml:L12516-L12522] |
| `rao-readers-and-orcaflex-converters` / 240 | `src/digitalmodel/orcawave/reporting/sections/qa_summary.py` | `_run_checks` | independent | [docs/registry/implementation-map.yaml:L12530-L12536] |
| `rao-readers-and-orcaflex-converters` / 241 | `src/digitalmodel/parametric/generate.py` | `_rao_interpolator` | independent | [docs/registry/implementation-map.yaml:L12542-L12548] |
| `rao-readers-and-orcaflex-converters` / 242 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcawave` | independent | [docs/registry/implementation-map.yaml:L12556-L12562] |
| `rao-readers-and-orcaflex-converters` / 243 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_aqwa` | independent | [docs/registry/implementation-map.yaml:L12568-L12574] |
| `rao-readers-and-orcaflex-converters` / 244 | `src/digitalmodel/solvers/orcaflex/library_generator.py` | `csv_row_to_vessel_type` | independent | [docs/registry/implementation-map.yaml:L12583-L12589] |
| `rao-readers-and-orcaflex-converters` / 245 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py` | `VesselTypeBuilder.build` | independent | [docs/registry/implementation-map.yaml:L12595-L12601] |
| `rao-readers-and-orcaflex-converters` / 246 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` | `VesselRouter.from_hull_catalog` | independent | [docs/registry/implementation-map.yaml:L12607-L12613] |
| `rao-readers-and-orcaflex-converters` / 247 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` | `LoadVessel.aqwa_group_item` | independent | [docs/registry/implementation-map.yaml:L12621-L12627] |
| `rao-readers-and-orcaflex-converters` / 248 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter.convert_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L12633-L12639] |
| `rao-readers-and-orcaflex-converters` / 249 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter._format_load_raos` | independent | [docs/registry/implementation-map.yaml:L12647-L12653] |
| `rao-readers-and-orcaflex-converters` / 250 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter._save_compact_format` | independent | [docs/registry/implementation-map.yaml:L12659-L12665] |
| `rao-readers-and-orcaflex-converters` / 251 | `src/digitalmodel/solvers/smoke/probes.py` | `check_aqwa` | independent | [docs/registry/implementation-map.yaml:L12671-L12677] |
| `rao-readers-and-orcaflex-converters` / 252 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` | `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | independent | [docs/registry/implementation-map.yaml:L12683-L12689] |
| `rao-readers-and-orcaflex-converters` / 253 | `tests/drilling_riser/global_model/test_nonlinear_vessel_foundation.py` | `_vessel_motion` | test_helper | [docs/registry/implementation-map.yaml:L12697-L12703] |
| `rao-readers-and-orcaflex-converters` / 254 | `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L12711-L12717] |
| `rao-readers-and-orcaflex-converters` / 255 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` | `_FakeLISParser.parse_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12722-L12728] |
| `rao-readers-and-orcaflex-converters` / 256 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` | `_run_case` | test_helper | [docs/registry/implementation-map.yaml:L12734-L12740] |
| `rao-readers-and-orcaflex-converters` / 257 | `tests/hydrodynamics/bemrosetta/conftest.py` | `sample_aqwa_file` | test_helper | [docs/registry/implementation-map.yaml:L12746-L12752] |
| `rao-readers-and-orcaflex-converters` / 258 | `tests/hydrodynamics/bemrosetta/test_cli.py` | `temp_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12760-L12766] |
| `rao-readers-and-orcaflex-converters` / 259 | `tests/hydrodynamics/bemrosetta/test_parsers.py` | `temp_lis_file` | test_helper | [docs/registry/implementation-map.yaml:L12774-L12780] |
| `rao-readers-and-orcaflex-converters` / 260 | `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py` | `_load_reference_data` | test_helper | [docs/registry/implementation-map.yaml:L12788-L12794] |
| `rao-readers-and-orcaflex-converters` / 261 | `tests/hydrodynamics/diffraction/conftest.py` | `l00_owr_path` | test_helper | [docs/registry/implementation-map.yaml:L12800-L12806] |
| `rao-readers-and-orcaflex-converters` / 262 | `tests/hydrodynamics/diffraction/conftest.py` | `l01_owr_path` | test_helper | [docs/registry/implementation-map.yaml:L12814-L12820] |
| `rao-readers-and-orcaflex-converters` / 263 | `tests/hydrodynamics/diffraction/test_aqwa_lis_coefficient_grid.py` | `_synthetic` | test_helper | [docs/registry/implementation-map.yaml:L12828-L12834] |
| `rao-readers-and-orcaflex-converters` / 264 | `tests/hydrodynamics/diffraction/test_aqwa_parser.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L12840-L12846] |
| `rao-readers-and-orcaflex-converters` / 265 | `tests/hydrodynamics/diffraction/test_aqwa_runner.py` | `_patch_extract` | test_helper | [docs/registry/implementation-map.yaml:L12851-L12857] |
| `rao-readers-and-orcaflex-converters` / 266 | `tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py` | `_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12863-L12869] |
| `rao-readers-and-orcaflex-converters` / 267 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` | `_make_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12875-L12881] |
| `rao-readers-and-orcaflex-converters` / 268 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` | `_make_fake_diffraction._install` | test_helper | [docs/registry/implementation-map.yaml:L12887-L12893] |
| `rao-readers-and-orcaflex-converters` / 269 | `tests/hydrodynamics/diffraction/test_orcawave_runner_units_boundary.py` | `_fake_diffraction` | test_helper | [docs/registry/implementation-map.yaml:L12899-L12905] |
| `rao-readers-and-orcaflex-converters` / 270 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` | `_full_basic` | test_helper | [docs/registry/implementation-map.yaml:L12911-L12917] |
| `rao-readers-and-orcaflex-converters` / 271 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` | `_benchmark` | test_helper | [docs/registry/implementation-map.yaml:L12923-L12929] |
| `rao-readers-and-orcaflex-converters` / 272 | `tests/hydrodynamics/diffraction/test_solver_orcawave_data_extraction.py` | `TestValidateDataConsistency.test_mismatched_frequencies_logged` | test_helper | [docs/registry/implementation-map.yaml:L12935-L12941] |
| `rao-readers-and-orcaflex-converters` / 273 | `tests/hydrodynamics/diffraction/test_solver_report_extractors.py` | `_mock_diffraction_object` | test_helper | [docs/registry/implementation-map.yaml:L12947-L12953] |
| `rao-readers-and-orcaflex-converters` / 274 | `tests/hydrodynamics/hull_library/test_rao_fixture_metadata.py` | `repository_pair` | test_helper | [docs/registry/implementation-map.yaml:L12959-L12965] |
| `rao-readers-and-orcaflex-converters` / 275 | `tests/hydrodynamics/hull_library/test_rao_fixture_population.py` | `test_ship_periods_convert_and_responses_stay_aligned` | test_helper | [docs/registry/implementation-map.yaml:L12974-L12980] |
| `rao-readers-and-orcaflex-converters` / 276 | `tests/hydrodynamics/hull_library/test_rao_lookup_plots.py` | `_make_entry` | test_helper | [docs/registry/implementation-map.yaml:L12987-L12993] |
| `rao-readers-and-orcaflex-converters` / 277 | `tests/hydrodynamics/hull_library/test_rao_registry.py` | `TestPanelCatalogRaoRoundTrip.test_csv_export_with_raos` | test_helper | [docs/registry/implementation-map.yaml:L12999-L13005] |
| `rao-readers-and-orcaflex-converters` / 278 | `tests/hydrodynamics/rao_analysis/test_rao_analysis.py` | `run_rao_analysis` | test_helper | [docs/registry/implementation-map.yaml:L13011-L13017] |
| `rao-readers-and-orcaflex-converters` / 279 | `tests/hydrodynamics/test_rao_tabulation_workflow.py` | `_base_cfg` | test_helper | [docs/registry/implementation-map.yaml:L13023-L13029] |
| `rao-readers-and-orcaflex-converters` / 280 | `tests/marine_ops/installation/conftest.py` | `vessel` | test_helper | [docs/registry/implementation-map.yaml:L13035-L13041] |
| `rao-readers-and-orcaflex-converters` / 281 | `tests/marine_ops/installation/test_crane_tip_motion.py` | `TestCraneTipRAOs.test_heave_transfer_formula` | test_helper | [docs/registry/implementation-map.yaml:L13049-L13055] |
| `rao-readers-and-orcaflex-converters` / 282 | `tests/marine_ops/marine_analysis/test_rao_quality_checks.py` | `create_synthetic_orcaflex_rao_data` | test_helper | [docs/registry/implementation-map.yaml:L13063-L13069] |
| `rao-readers-and-orcaflex-converters` / 283 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` | `RAOVerification._find_test_files` | test_helper | [docs/registry/implementation-map.yaml:L13075-L13081] |
| `rao-readers-and-orcaflex-converters` / 284 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` | `RAOVerification.verify_file` | test_helper | [docs/registry/implementation-map.yaml:L13087-L13093] |
| `rao-readers-and-orcaflex-converters` / 285 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` | `RAOVerification._extract_unified_data` | test_helper | [docs/registry/implementation-map.yaml:L13102-L13108] |
| `rao-readers-and-orcaflex-converters` / 286 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` | `RAOVerification._extract_legacy_data` | test_helper | [docs/registry/implementation-map.yaml:L13114-L13120] |
| `rao-readers-and-orcaflex-converters` / 287 | `tests/orcawave/conftest.py` | `mock_diff_single_body` | test_helper | [docs/registry/implementation-map.yaml:L13126-L13132] |
| `rao-readers-and-orcaflex-converters` / 288 | `tests/orcawave/conftest.py` | `mock_diff_two_body` | test_helper | [docs/registry/implementation-map.yaml:L13140-L13146] |
| `rao-readers-and-orcaflex-converters` / 289 | `tests/orcawave/test_builder_orchestration.py` | `owr_path` | test_helper | [docs/registry/implementation-map.yaml:L13154-L13160] |
| `rao-readers-and-orcaflex-converters` / 290 | `tests/orcawave/test_report_builder.py` | `builder` | test_helper | [docs/registry/implementation-map.yaml:L13168-L13174] |
| `rao-readers-and-orcaflex-converters` / 291 | `tests/postprocess/test_build_results_workbook.py` | `_orcawave_dir` | test_helper | [docs/registry/implementation-map.yaml:L13182-L13188] |
| `rao-readers-and-orcaflex-converters` / 292 | `tests/solver/smoke_test.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L13194-L13200] |
| `rao-readers-and-orcaflex-converters` / 293 | `tests/solver/smoke_test.py` | `promote_committed_artifacts` | test_helper | [docs/registry/implementation-map.yaml:L13205-L13211] |
| `rao-readers-and-orcaflex-converters` / 294 | `tests/solver/smoke_test.py` | `run_l00_smoke_test` | test_helper | [docs/registry/implementation-map.yaml:L13217-L13223] |
| `rao-readers-and-orcaflex-converters` / 295 | `tests/solver/smoke_test.py` | `run_l01_smoke_test` | test_helper | [docs/registry/implementation-map.yaml:L13229-L13235] |
| `wind-and-current-force-routes` / 1 | `examples/demos/gtm/demo_03_deepwater_mudmat_installation.py` | `calc_splash_zone` | example | [docs/registry/implementation-map.yaml:L15400-L15406] |
| `wind-and-current-force-routes` / 2 | `examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py` | `calc_phase_3_splash` | example | [docs/registry/implementation-map.yaml:L15410-L15416] |
| `wind-and-current-force-routes` / 3 | `examples/demos/gtm/results_store_demo05.py` | `<module>` | example | [docs/registry/implementation-map.yaml:L15420-L15426] |
| `wind-and-current-force-routes` / 4 | `examples/demos/gtm/tests/test_demo_05_results_store.py` | `_good_case` | example | [docs/registry/implementation-map.yaml:L15429-L15435] |
| `wind-and-current-force-routes` / 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | example | [docs/registry/implementation-map.yaml:L15439-L15445] |
| `wind-and-current-force-routes` / 6 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example | [docs/registry/implementation-map.yaml:L15449-L15455] |
| `wind-and-current-force-routes` / 7 | `examples/domains/ocimf/ocimf_demo.py` | `main` | example | [docs/registry/implementation-map.yaml:L15461-L15467] |
| `wind-and-current-force-routes` / 8 | `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | example | [docs/registry/implementation-map.yaml:L15471-L15477] |
| `wind-and-current-force-routes` / 9 | `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | example | [docs/registry/implementation-map.yaml:L15481-L15487] |
| `wind-and-current-force-routes` / 10 | `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | example | [docs/registry/implementation-map.yaml:L15491-L15497] |
| `wind-and-current-force-routes` / 11 | `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | example | [docs/registry/implementation-map.yaml:L15501-L15507] |
| `wind-and-current-force-routes` / 12 | `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | example | [docs/registry/implementation-map.yaml:L15511-L15517] |
| `wind-and-current-force-routes` / 13 | `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | example | [docs/registry/implementation-map.yaml:L15521-L15527] |
| `wind-and-current-force-routes` / 14 | `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | example | [docs/registry/implementation-map.yaml:L15531-L15537] |
| `wind-and-current-force-routes` / 15 | `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | example | [docs/registry/implementation-map.yaml:L15541-L15547] |
| `wind-and-current-force-routes` / 16 | `scripts/capabilities/build_onepagers.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L15551-L15557] |
| `wind-and-current-force-routes` / 17 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L15560-L15566] |
| `wind-and-current-force-routes` / 18 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | independent | [docs/registry/implementation-map.yaml:L15569-L15575] |
| `wind-and-current-force-routes` / 19 | `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L15579-L15585] |
| `wind-and-current-force-routes` / 20 | `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | independent | [docs/registry/implementation-map.yaml:L15588-L15594] |
| `wind-and-current-force-routes` / 21 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | independent | [docs/registry/implementation-map.yaml:L15598-L15604] |
| `wind-and-current-force-routes` / 22 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | independent | [docs/registry/implementation-map.yaml:L15608-L15614] |
| `wind-and-current-force-routes` / 23 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | independent | [docs/registry/implementation-map.yaml:L15618-L15624] |
| `wind-and-current-force-routes` / 24 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | independent | [docs/registry/implementation-map.yaml:L15630-L15636] |
| `wind-and-current-force-routes` / 25 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | independent | [docs/registry/implementation-map.yaml:L15640-L15646] |
| `wind-and-current-force-routes` / 26 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | independent | [docs/registry/implementation-map.yaml:L15650-L15656] |
| `wind-and-current-force-routes` / 27 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | independent | [docs/registry/implementation-map.yaml:L15660-L15666] |
| `wind-and-current-force-routes` / 28 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | independent | [docs/registry/implementation-map.yaml:L15670-L15676] |
| `wind-and-current-force-routes` / 29 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L15685-L15691] |
| `wind-and-current-force-routes` / 30 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | independent | [docs/registry/implementation-map.yaml:L15694-L15700] |
| `wind-and-current-force-routes` / 31 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | independent | [docs/registry/implementation-map.yaml:L15704-L15710] |
| `wind-and-current-force-routes` / 32 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | independent | [docs/registry/implementation-map.yaml:L15714-L15720] |
| `wind-and-current-force-routes` / 33 | `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | independent | [docs/registry/implementation-map.yaml:L15724-L15730] |
| `wind-and-current-force-routes` / 34 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | independent | [docs/registry/implementation-map.yaml:L15734-L15740] |
| `wind-and-current-force-routes` / 35 | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | independent | [docs/registry/implementation-map.yaml:L15744-L15750] |
| `wind-and-current-force-routes` / 36 | `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | independent | [docs/registry/implementation-map.yaml:L15754-L15760] |
| `wind-and-current-force-routes` / 37 | `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | independent | [docs/registry/implementation-map.yaml:L15764-L15770] |
| `wind-and-current-force-routes` / 38 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | independent | [docs/registry/implementation-map.yaml:L15774-L15780] |
| `wind-and-current-force-routes` / 39 | `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L15784-L15790] |
| `wind-and-current-force-routes` / 40 | `src/digitalmodel/engine.py` | `engine` | caller | [docs/registry/implementation-map.yaml:L15794-L15800] |
| `wind-and-current-force-routes` / 41 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | independent | [docs/registry/implementation-map.yaml:L15807-L15813] |
| `wind-and-current-force-routes` / 42 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | independent | [docs/registry/implementation-map.yaml:L15817-L15823] |
| `wind-and-current-force-routes` / 43 | `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | independent | [docs/registry/implementation-map.yaml:L15827-L15833] |
| `wind-and-current-force-routes` / 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | independent | [docs/registry/implementation-map.yaml:L15837-L15843] |
| `wind-and-current-force-routes` / 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | independent | [docs/registry/implementation-map.yaml:L15847-L15853] |
| `wind-and-current-force-routes` / 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | independent | [docs/registry/implementation-map.yaml:L15857-L15863] |
| `wind-and-current-force-routes` / 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | independent | [docs/registry/implementation-map.yaml:L15867-L15873] |
| `wind-and-current-force-routes` / 48 | `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | independent | [docs/registry/implementation-map.yaml:L15877-L15883] |
| `wind-and-current-force-routes` / 49 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | independent | [docs/registry/implementation-map.yaml:L15887-L15893] |
| `wind-and-current-force-routes` / 50 | `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | independent | [docs/registry/implementation-map.yaml:L15897-L15903] |
| `wind-and-current-force-routes` / 51 | `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | independent | [docs/registry/implementation-map.yaml:L15907-L15913] |
| `wind-and-current-force-routes` / 52 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | independent | [docs/registry/implementation-map.yaml:L15917-L15923] |
| `wind-and-current-force-routes` / 53 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | independent | [docs/registry/implementation-map.yaml:L15927-L15933] |
| `wind-and-current-force-routes` / 54 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | independent | [docs/registry/implementation-map.yaml:L15937-L15943] |
| `wind-and-current-force-routes` / 55 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | independent | [docs/registry/implementation-map.yaml:L15947-L15953] |
| `wind-and-current-force-routes` / 56 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L15957-L15963] |
| `wind-and-current-force-routes` / 57 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | copy | [docs/registry/implementation-map.yaml:L15966-L15972] |
| `wind-and-current-force-routes` / 58 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | copy | [docs/registry/implementation-map.yaml:L15976-L15982] |
| `wind-and-current-force-routes` / 59 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | copy | [docs/registry/implementation-map.yaml:L15988-L15994] |
| `wind-and-current-force-routes` / 60 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | copy | [docs/registry/implementation-map.yaml:L15998-L16004] |
| `wind-and-current-force-routes` / 61 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | copy | [docs/registry/implementation-map.yaml:L16010-L16016] |
| `wind-and-current-force-routes` / 62 | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | independent | [docs/registry/implementation-map.yaml:L16020-L16026] |
| `wind-and-current-force-routes` / 63 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | copy | [docs/registry/implementation-map.yaml:L16030-L16036] |
| `wind-and-current-force-routes` / 64 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | copy | [docs/registry/implementation-map.yaml:L16040-L16046] |
| `wind-and-current-force-routes` / 65 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | copy | [docs/registry/implementation-map.yaml:L16050-L16056] |
| `wind-and-current-force-routes` / 66 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | copy | [docs/registry/implementation-map.yaml:L16060-L16066] |
| `wind-and-current-force-routes` / 67 | `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L16075-L16081] |
| `wind-and-current-force-routes` / 68 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | independent | [docs/registry/implementation-map.yaml:L16084-L16090] |
| `wind-and-current-force-routes` / 69 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | independent | [docs/registry/implementation-map.yaml:L16094-L16100] |
| `wind-and-current-force-routes` / 70 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | independent | [docs/registry/implementation-map.yaml:L16104-L16110] |
| `wind-and-current-force-routes` / 71 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | independent | [docs/registry/implementation-map.yaml:L16114-L16120] |
| `wind-and-current-force-routes` / 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | copy | [docs/registry/implementation-map.yaml:L16127-L16133] |
| `wind-and-current-force-routes` / 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | copy | [docs/registry/implementation-map.yaml:L16137-L16143] |
| `wind-and-current-force-routes` / 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | copy | [docs/registry/implementation-map.yaml:L16117-L16126] |
| `wind-and-current-force-routes` / 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | independent | [docs/registry/implementation-map.yaml:L16127-L16139] |
| `wind-and-current-force-routes` / 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database` | independent | [docs/registry/implementation-map.yaml:L16140-L16149] |
| `wind-and-current-force-routes` / 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | independent | [docs/registry/implementation-map.yaml:L16179-L16185] |
| `wind-and-current-force-routes` / 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | independent | [docs/registry/implementation-map.yaml:L16189-L16195] |
| `wind-and-current-force-routes` / 79 | `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | independent | [docs/registry/implementation-map.yaml:L16199-L16205] |
| `wind-and-current-force-routes` / 80 | `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | independent | [docs/registry/implementation-map.yaml:L16209-L16215] |
| `wind-and-current-force-routes` / 81 | `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | independent | [docs/registry/implementation-map.yaml:L16219-L16225] |
| `wind-and-current-force-routes` / 82 | `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | independent | [docs/registry/implementation-map.yaml:L16229-L16235] |
| `wind-and-current-force-routes` / 83 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | independent | [docs/registry/implementation-map.yaml:L16239-L16245] |
| `wind-and-current-force-routes` / 84 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | independent | [docs/registry/implementation-map.yaml:L16249-L16255] |
| `wind-and-current-force-routes` / 85 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | independent | [docs/registry/implementation-map.yaml:L16259-L16265] |
| `wind-and-current-force-routes` / 86 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | independent | [docs/registry/implementation-map.yaml:L16269-L16275] |
| `wind-and-current-force-routes` / 87 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | caller | [docs/registry/implementation-map.yaml:L16281-L16287] |
| `wind-and-current-force-routes` / 88 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | independent | [docs/registry/implementation-map.yaml:L16294-L16300] |
| `wind-and-current-force-routes` / 89 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | independent | [docs/registry/implementation-map.yaml:L16305-L16311] |
| `wind-and-current-force-routes` / 90 | `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | independent | [docs/registry/implementation-map.yaml:L16315-L16321] |
| `wind-and-current-force-routes` / 91 | `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | independent | [docs/registry/implementation-map.yaml:L16325-L16331] |
| `wind-and-current-force-routes` / 92 | `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | independent | [docs/registry/implementation-map.yaml:L16335-L16341] |
| `wind-and-current-force-routes` / 93 | `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | independent | [docs/registry/implementation-map.yaml:L16345-L16351] |
| `wind-and-current-force-routes` / 94 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | independent | [docs/registry/implementation-map.yaml:L16355-L16361] |
| `wind-and-current-force-routes` / 95 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | independent | [docs/registry/implementation-map.yaml:L16365-L16371] |
| `wind-and-current-force-routes` / 96 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | independent | [docs/registry/implementation-map.yaml:L16375-L16381] |
| `wind-and-current-force-routes` / 97 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | caller | [docs/registry/implementation-map.yaml:L16385-L16391] |
| `wind-and-current-force-routes` / 98 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | caller | [docs/registry/implementation-map.yaml:L16397-L16403] |
| `wind-and-current-force-routes` / 99 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | independent | [docs/registry/implementation-map.yaml:L16409-L16415] |
| `wind-and-current-force-routes` / 100 | `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | independent | [docs/registry/implementation-map.yaml:L16419-L16425] |
| `wind-and-current-force-routes` / 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | independent | [docs/registry/implementation-map.yaml:L16429-L16435] |
| `wind-and-current-force-routes` / 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | independent | [docs/registry/implementation-map.yaml:L16439-L16445] |
| `wind-and-current-force-routes` / 103 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | independent | [docs/registry/implementation-map.yaml:L16449-L16455] |
| `wind-and-current-force-routes` / 104 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L16459-L16465] |
| `wind-and-current-force-routes` / 105 | `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | test_helper | [docs/registry/implementation-map.yaml:L16469-L16475] |
| `wind-and-current-force-routes` / 106 | `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | test_helper | [docs/registry/implementation-map.yaml:L16478-L16484] |
| `wind-and-current-force-routes` / 107 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | test_helper | [docs/registry/implementation-map.yaml:L16488-L16494] |
| `wind-and-current-force-routes` / 108 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | test_helper | [docs/registry/implementation-map.yaml:L16498-L16504] |
| `wind-and-current-force-routes` / 109 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | test_helper | [docs/registry/implementation-map.yaml:L16508-L16514] |
| `wind-and-current-force-routes` / 110 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L16518-L16524] |
| `wind-and-current-force-routes` / 111 | `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | test_helper | [docs/registry/implementation-map.yaml:L16529-L16535] |
| `wind-and-current-force-routes` / 112 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | test_helper | [docs/registry/implementation-map.yaml:L16539-L16545] |
| `wind-and-current-force-routes` / 113 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | test_helper | [docs/registry/implementation-map.yaml:L16550-L16556] |
| `wind-and-current-force-routes` / 114 | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | test_helper | [docs/registry/implementation-map.yaml:L16560-L16566] |
| `wind-and-current-force-routes` / 115 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | test_helper | [docs/registry/implementation-map.yaml:L16570-L16576] |
| `wind-and-current-force-routes` / 116 | `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | test_helper | [docs/registry/implementation-map.yaml:L16580-L16586] |
| `wind-and-current-force-routes` / 117 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | test_helper | [docs/registry/implementation-map.yaml:L16590-L16596] |
| `wind-and-current-force-routes` / 118 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | test_helper | [docs/registry/implementation-map.yaml:L16601-L16607] |
| `wind-and-current-force-routes` / 119 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | test_helper | [docs/registry/implementation-map.yaml:L16612-L16618] |
| `wind-and-current-force-routes` / 120 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | test_helper | [docs/registry/implementation-map.yaml:L16623-L16629] |
| `wind-and-current-force-routes` / 121 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | test_helper | [docs/registry/implementation-map.yaml:L16634-L16640] |
| `wind-and-current-force-routes` / 122 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | test_helper | [docs/registry/implementation-map.yaml:L16645-L16651] |
| `wind-and-current-force-routes` / 123 | `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | test_helper | [docs/registry/implementation-map.yaml:L16656-L16662] |
| `wind-and-current-force-routes` / 124 | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | test_helper | [docs/registry/implementation-map.yaml:L16666-L16672] |
| `wind-and-current-force-routes` / 125 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | test_helper | [docs/registry/implementation-map.yaml:L16676-L16682] |
| `wind-and-current-force-routes` / 126 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | test_helper | [docs/registry/implementation-map.yaml:L16686-L16692] |
| `wind-and-current-force-routes` / 127 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | test_helper | [docs/registry/implementation-map.yaml:L16696-L16702] |
| `wind-and-current-force-routes` / 128 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | test_helper | [docs/registry/implementation-map.yaml:L16706-L16712] |
| `wind-and-current-force-routes` / 129 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | test_helper | [docs/registry/implementation-map.yaml:L16716-L16722] |
| `wind-and-current-force-routes` / 130 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | test_helper | [docs/registry/implementation-map.yaml:L16726-L16732] |
| `wind-and-current-force-routes` / 131 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L16736-L16742] |
| `wind-and-current-force-routes` / 132 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` | independent | [docs/registry/implementation-map.yaml:L16748-L16755] |
| `wind-and-current-force-routes` / 133 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `lift_force_per_meter` | independent | [docs/registry/implementation-map.yaml:L16759-L16766] |
| `wind-and-current-force-routes` / 134 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lift_force_per_meter` | independent | [docs/registry/implementation-map.yaml:L16770-L16778] |
| `wind-and-current-force-routes` / 135 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `inertia_force_per_meter` | independent | [docs/registry/implementation-map.yaml:L16782-L16788] |
| `linear-wave-kinematics` / 1 | `src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py` | `wavenumbers` | independent | [docs/registry/implementation-map.yaml:L26260-L26266] |
| `linear-wave-kinematics` / 2 | `src/digitalmodel/hydrodynamics/diffraction/analytical_cylinder.py` | `wavenumbers.propagating` | independent | [docs/registry/implementation-map.yaml:L26270-L26276] |
| `linear-wave-kinematics` / 3 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py` | `wave_number` | independent | [docs/registry/implementation-map.yaml:L26280-L26286] |
| `linear-wave-kinematics` / 4 | `src/digitalmodel/motion_forecast/reconstruct.py` | `wavenumber` | independent | [docs/registry/implementation-map.yaml:L26290-L26296] |
| `linear-wave-kinematics` / 5 | `src/digitalmodel/motion_forecast/reconstruct.py` | `wavenumber.f` | independent | [docs/registry/implementation-map.yaml:L26300-L26306] |
| `linear-wave-kinematics` / 6 | `src/digitalmodel/solvers/openfoam/validation/wave_tank.py` | `dispersion_wavenumber` | independent | [docs/registry/implementation-map.yaml:L26310-L26316] |
| `linear-wave-kinematics` / 7 | `src/digitalmodel/solvers/openfoam/wave_models.py` | `_solve_dispersion` | independent | [docs/registry/implementation-map.yaml:L26320-L26326] |
| `linear-wave-kinematics` / 8 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `solve_dispersion` | independent | [docs/registry/implementation-map.yaml:L26330-L26336] |
| `linear-wave-kinematics` / 9 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `velocity_transfer` | independent | [docs/registry/implementation-map.yaml:L26340-L26346] |
| `linear-wave-kinematics` / 10 | `tests/hydrodynamics/diffraction/test_analytical_cylinder.py` | `test_propagating_root_satisfies_dispersion_relation` | test_helper | [docs/registry/implementation-map.yaml:L26350-L26356] |
| `linear-wave-kinematics` / 11 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_deep_water_limit` | test_helper | [docs/registry/implementation-map.yaml:L26362-L26368] |
| `linear-wave-kinematics` / 12 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_dispersion_relation_satisfied` | test_helper | [docs/registry/implementation-map.yaml:L26374-L26380] |
| `linear-wave-kinematics` / 13 | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py` | `TestWaveNumber.test_scalar_input` | test_helper | [docs/registry/implementation-map.yaml:L26385-L26391] |
| `linear-wave-kinematics` / 14 | `tests/motion_forecast/test_reconstruct.py` | `test_finite_depth_wavenumber_reduces_to_deep` | test_helper | [docs/registry/implementation-map.yaml:L26396-L26402] |
| `linear-wave-kinematics` / 15 | `tests/motion_forecast/test_reconstruct.py` | `test_finite_depth_dispersion_holds_in_shallow_water` | test_helper | [docs/registry/implementation-map.yaml:L26406-L26412] |
| `linear-wave-kinematics` / 16 | `tests/solvers/openfoam/validation/test_wave_tank.py` | `test_dispersion_satisfies_relation` | test_helper | [docs/registry/implementation-map.yaml:L26416-L26422] |
| `linear-wave-kinematics` / 17 | `tests/solvers/openfoam/validation/test_wave_tank.py` | `test_dispersion_deep_water_limit` | test_helper | [docs/registry/implementation-map.yaml:L26426-L26432] |
| `linear-wave-kinematics` / 18 | `src/digitalmodel/hydrodynamics/planing_hull/solver.py` | `PlaningMotionSolver._wave_number` | independent | [docs/registry/implementation-map.yaml:L26436-L26444] |
| `linear-wave-kinematics` / 19 | `src/digitalmodel/solvers/openfoam/hull_free_surface.py` | `deep_water_wavelength` | independent | [docs/registry/implementation-map.yaml:L26448-L26455] |
| `linear-wave-kinematics` / 20 | `src/digitalmodel/solvers/openfoam/validation/wave_excited_body_rao.py` | `group_velocity` | independent | [docs/registry/implementation-map.yaml:L26459-L26468] |
| `linear-wave-kinematics` / 21 | `src/digitalmodel/hydrodynamics/sloshing.py` | `oval_tank_periods` | independent | [docs/registry/implementation-map.yaml:L26475-L26483] |

## 10. Coverage and open items

### Files read and test selections

The top-level source inventory below is complete for direct Python files at the pin; no implementation inside a sub-package is covered by that statement.

| File read | Pin evidence |
|---|---|
| `__init__.py` | [src/digitalmodel/hydrodynamics/__init__.py:L1-L71] |
| `cli.py` | [src/digitalmodel/hydrodynamics/cli.py:L1-L296] |
| `coefficient_database.py` | [src/digitalmodel/hydrodynamics/coefficient_database.py:L1-L384] |
| `interpolator.py` | [src/digitalmodel/hydrodynamics/interpolator.py:L1-L306] |
| `models.py` | [src/digitalmodel/hydrodynamics/models.py:L1-L373] |
| `ocimf_loading.py` | [src/digitalmodel/hydrodynamics/ocimf_loading.py:L1-L313] |
| `propeller_rudder.py` | [src/digitalmodel/hydrodynamics/propeller_rudder.py:L1-L205] |
| `propeller_rudder_workflow.py` | [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L1-L338] |
| `seakeeping.py` | [src/digitalmodel/hydrodynamics/seakeeping.py:L1-L231] |
| `sloshing.py` | [src/digitalmodel/hydrodynamics/sloshing.py:L1-L304] |
| `utube_tank.py` | [src/digitalmodel/hydrodynamics/utube_tank.py:L1-L529] |
| `wang_passing_ship.py` | [src/digitalmodel/hydrodynamics/wang_passing_ship.py:L1-L540] |
| `wave_spectra.py` | [src/digitalmodel/hydrodynamics/wave_spectra.py:L1-L386] |

Core documentation inspected for reference navigation: research academic and industry source inventories, the hydrodynamics standards inventory, propeller-rudder literature/method-selection/research notes, floating-wind load-case navigation, routing and workflow registries. The broader Markdown discovery list below is a discovery record, not a claim that every document has had a technical review. [docs/research/hydrodynamics-academic-references.md:L1-L12] [docs/research/hydrodynamics-industry-practice.md:L1-L12] [docs/standards/hydrodynamics-inventory.yaml:L1-L12] [docs/domains/hydrodynamics/propeller-rudder-research-note-2026-10.md:L3-L7] [docs/registry/workflows.yaml:L1117-L1126]

Offline commands use `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; selection B additionally uses `--with pytest-benchmark`, a declared test dependency. No pytest addopts override is used. Paths for temporary test copies retain their repository-relative node IDs; the files are copied without modification because their output fixtures write beside `__file__`. [pyproject.toml:L175-L187] [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L26-L31]

R4 reruns used Python 3.12.13 and pytest 8.4.2 at the exact code pin. Before pytest collection, `sys.modules` entries for `OrcFxAPI`, `OrcaFlex` and `OrcaWave` were set to `None` so no licensed interface could load. Pytest plugin autoload was disabled; `pytest_asyncio.plugin` was explicitly enabled. Command arguments: `tests/workflows/test_durable_workflows.py::test_workflow_registry[propeller-rudder-interaction] -q -p no:cacheprovider -p pytest_asyncio.plugin --basetemp=<scratch>`. Both runs passed; these reruns supplement the unchanged A-D evidence. [tests/workflows/test_durable_workflows.py:L57-L62] [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L5]

| Selection | Baseline run / passed / failed / skipped / NOT_RUN | Repeated run / passed / failed / skipped / NOT_RUN | Code revision |
|---|---|---|---|
| A: 14 core and documentation files listed below | 313 / 313 / 0 / 0 / 0 | 313 / 313 / 0 / 0 / 0 | Header pin |
| B: 8 supporting calculation/benchmark/routing files listed below | 161 / 161 / 0 / 0 / 0 | 161 / 161 / 0 / 0 / 0 | Header pin |
| C: 3 integration files in temporary copies | 21 / 21 / 0 / 0 / 0 | 21 / 21 / 0 / 0 / 0 | Header pin |
| D: `tests/contracts` domain | 484 / 483 / 1 / 0 / 0 | 484 / 483 / 1 / 0 / 0 | Header pin |
| Registered propeller-rudder workflow (2026-10-17 offline reruns) | 1 / 1 / 0 / 0 / 0 | 1 / 1 / 0 / 0 / 0 | Header pin |

Baseline exception: `tests/contracts/test_string_addressed_dependencies.py::test_the_three_known_defects_are_still_derived` fails at line 632 with `engine='xlsxwriter' (#1906) no longer derived — the scanner regressed`. The assertion and scanned Python/TOML files are unchanged from the pin. No open issue specific to this assertion was found in the tracking search on 2026-10-17; issue pending (7). This pack does not alter its result. [tests/contracts/test_string_addressed_dependencies.py:L620-L644]

| Selection | Test files |
|---|---|
| A | `tests/hydrodynamics/test_hydrodynamics_unit.py`; [tests/hydrodynamics/test_hydrodynamics_unit.py:L1-L5] |
| A | `tests/hydrodynamics/test_hydrodynamics_cli.py`; [tests/hydrodynamics/test_hydrodynamics_cli.py:L1-L5] |
| A | `tests/hydrodynamics/test_seakeeping.py`; [tests/hydrodynamics/test_seakeeping.py:L1-L5] |
| A | `tests/hydrodynamics/test_sloshing.py`; [tests/hydrodynamics/test_sloshing.py:L1-L5] |
| A | `tests/hydrodynamics/test_utube_tank.py`; [tests/hydrodynamics/test_utube_tank.py:L1-L5] |
| A | `tests/hydrodynamics/test_wang_passing_ship.py`; [tests/hydrodynamics/test_wang_passing_ship.py:L1-L5] |
| A | `tests/test_propeller_rudder_interaction.py`; [tests/test_propeller_rudder_interaction.py:L1-L5] |
| A | `tests/unit/hydrodynamics/test_wave_spectra_extended.py`; [tests/unit/hydrodynamics/test_wave_spectra_extended.py:L1-L5] |
| A | `tests/specialized/cli/test_hydrodynamics_cli.py`; [tests/specialized/cli/test_hydrodynamics_cli.py:L1-L5] |
| A | `tests/docs/test_hydrodynamics_academic_references.py`; [tests/docs/test_hydrodynamics_academic_references.py:L1-L5] |
| A | `tests/docs/test_hydrodynamics_standards_inventory.py`; [tests/docs/test_hydrodynamics_standards_inventory.py:L1-L5] |
| A | `tests/docs/test_propeller_rudder_research_note.py`; [tests/docs/test_propeller_rudder_research_note.py:L1-L5] |
| A | `tests/docs/test_propeller_rudder_method_selection.py`; [tests/docs/test_propeller_rudder_method_selection.py:L1-L5] |
| A | `tests/docs/test_hydrodynamics_industry_practice.py`; [tests/docs/test_hydrodynamics_industry_practice.py:L1-L5] |
| B | `tests/marine_ops/marine_engineering/test_wave_spectra.py`; [tests/marine_ops/marine_engineering/test_wave_spectra.py:L1-L5] |
| B | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py`; [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L1-L5] |
| B | `tests/marine_ops/marine_engineering/legacy/test_validation.py`; [tests/marine_ops/marine_engineering/legacy/test_validation.py:L1-L5] |
| B | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py`; [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L1-L5] |
| B | `tests/unit/hydrodynamics/test_rao_analysis.py`; [tests/unit/hydrodynamics/test_rao_analysis.py:L1-L5] |
| B | `tests/test_maneuvering_envelope.py`; [tests/test_maneuvering_envelope.py:L1-L5] |
| B | `tests/motion_forecast/test_validation.py`; [tests/motion_forecast/test_validation.py:L1-L5] |
| B | `tests/docs/test_digitalmodel_routing_contract.py`; [tests/docs/test_digitalmodel_routing_contract.py:L1-L5] |
| C | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py`; [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L1-L5] |
| C | `tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py`; [tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py:L1-L5] |
| C | `tests/marine_ops/marine_engineering/integration/test_end_to_end_workflow.py`; [tests/marine_ops/marine_engineering/integration/test_end_to_end_workflow.py:L1-L5] |

The historical “about 414 tests” lead is not used as a verified count: the recorded selections total 980 collected items per pass, including supporting documentation and repository contracts; 979 passed and one contract failed at baseline. Sub-package suites and unrelated consumers are outside this total. [tests/hydrodynamics/test_hydrodynamics_unit.py:L1-L5] [tests/docs/test_digitalmodel_routing_contract.py:L1-L12] [tests/contracts/test_string_addressed_dependencies.py:L620-L644]

### Open items

| Item | Status, limitation and required follow-up | Responsible role / tracking |
|---|---|---|
| Standards editions, clauses and output citations | UNVERIFIED: exact edition-to-clause mapping and output sidecars are not established; standard clause text was not read at a licensed location; no confirmed edition conflict is claimed | Maintainer; issue pending (1), related #1832 and #1823; [src/digitalmodel/hydrodynamics/wave_spectra.py:L71-L96] [src/digitalmodel/hydrodynamics/models.py:L71-L81] |
| Silent numeric fallbacks and missing edge checks | Reviewed sweep rows identify unqualified zero results, clamping, first-criterion selection, nonuniform grid association, and iteration-limit fallthrough; no detection check exists for the listed edge cases | Maintainer; issue pending (2); [src/digitalmodel/hydrodynamics/seakeeping.py:L171-L226] [src/digitalmodel/hydrodynamics/utube_tank.py:L415-L445] |
| Input/unit/data-integrity validation | Reviewed sweep rows identify finite-value, mode-count, localized-number parsing, rounded-key and empty-output checks that are missing | Maintainer; issue pending (3); [src/digitalmodel/hydrodynamics/coefficient_database.py:L67-L74] [src/digitalmodel/hydrodynamics/propeller_rudder_workflow.py:L56-L60] [src/digitalmodel/hydrodynamics/sloshing.py:L109-L139] |
| Calibration and historical reference eligibility | UNVERIFIED: independent provenance and public reuse eligibility of stored U-tube calibration and historical fixture values are not established by this pack; no such values are reproduced; sidecar entry: to be created if private provenance is needed | Maintainer; issue pending (4), related #1909 and #2017; [tests/hydrodynamics/test_utube_tank.py:L1-L18] [tests/hydrodynamics/test_wang_passing_ship.py:L1-L24] |
| Documentation breadth and codenaming | UNVERIFIED: technical claims in the broader discovery corpus remain unaudited; identifying discovery paths are represented only by counts, and require codenames before publication | Maintainer; issue pending (6), related #1802; discovery inventory below |
| Map follow-up | Add absent method families and missing direct implementation rows named in section 9; classify external search hits without changing this pin's A8 result | Maintainer; #1803; [src/digitalmodel/hydrodynamics/propeller_rudder.py:L46-L205] [src/digitalmodel/hydrodynamics/utube_tank.py:L111-L511] |
| Frequency-tail accuracy | Default-band height normalization is not a convergence check for spectral moments or zero-crossing periods | Maintainer; #2613; [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [src/digitalmodel/hydrodynamics/wave_spectra.py:L308-L328] |
| Engine-routed offline test | PASS offline at the code pin on 2026-10-17 in both runs (1 passed each); the optional licensed-interface import was blocked before collection; no licensed interface was loaded | Maintainer; #2526 tracks the guarded import only; [tests/workflows/test_durable_workflows.py:L13-L13] [src/digitalmodel/solvers/orcaflex/all_vars.py:L1-L5] |
| Baseline contract | Unchanged dependency-scanner assertion fails; compare the repeated run before accepting documentation changes | Maintainer; issue pending (7), no open tracker identified; [tests/contracts/test_string_addressed_dependencies.py:L620-L644] |

### Broader documentation discovery

Discovery command: `rg -l -i 'hydrodynamics|propeller.rudder' docs --glob '*.md'`; the list contains documents that mention the domain, including other packages' plans and packs. Discovery entries are follow-up locators, not adopted engineering claims; issue pending (6) covers technical review and codenaming.

| Discovery entry | Location |
|---|---|
| 2 | `docs/data/OCIMF_CORPUS_README.md` |
| 3 | `docs/migration/legacy-batch-workflows.md` |
| 4 | `docs/registry/implementation-map.md` |
| 5 | `docs/cad-inventory/README.md` |
| 6 | `docs/reviews/hull-library-cross-review.md` |
| 7 | `docs/spikes/2026-09-25-hullprod-curvature-screening/README.md` |
| 8 | `docs/maps/digitalmodel-operator-map.md` |
| 9 | `docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md` |
| 10 | `docs/field-development/subsea-production-systems-mapping.md` |
| 11 | `docs/plans/2026-06-29-cfd-capability-assessment.md` |
| 12 | `docs/capability-map/capabilities-ia-spec-1444.md` |
| 13 | `docs/plans/2026-09-27-issue-2239-analytical-resistance-methods.md` |
| 14 | `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` |
| 15 | `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` |
| 16 | `docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md` |
| 17 | `docs/domains/development/test_validation_patterns.md` |
| 18 | `docs/plans/2026-05-05-issue-552-aqwa-crlf-lf-alignment.md` |
| 19 | `docs/api/cfd/cases/wave_excited_body/README.md` |
| 20 | `docs/domains/development/test_summary_with_validations.md` |
| 21 | `docs/plans/2026-05-05-issue-577-safety-case-mah-alarp.md` |
| 22 | `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md` |
| 23 | `docs/plans/2026-05-05-issue-278-4-quadrant-propeller-data.md` |
| 24 | `docs/research/hydrodynamics-academic-references.md` |
| 25 | `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md` |
| 26 | `docs/api/cfd/cases/floating_body_decay/README.md` |
| 27 | `docs/research/mooring-industry-practice.md` |
| 28 | `docs/research/hydrodynamics-industry-practice.md` |
| 29 | `docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md` |
| 30 | `docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md` |
| 31 | `docs/domains/domains_index.md` |
| 32 | `docs/plans/2026-09-27-issue-2191-parametric-monohull-form.md` |
| 33 | `docs/research/subsea-pipelines-academic-references.md` |
| 34 | `docs/research/subsea-pipelines-industry-practice.md` |
| 35 | `docs/domains/mooring/CALM_BUOY_PROJECT_WORKFLOW.md` |
| 36 | `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` |
| 37 | `docs/vision/CALCULATIONS-VISION.md` |
| 38 | `docs/plans/parametrics-preparedness.md` |
| 39 | `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` |
| 40 | `docs/domains/_index.md` |
| 41 | `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` |
| 42 | `docs/plans/2026-09-27-issue-2241-end-closure.md` |
| 43 | `docs/plans/2026-09-09-issue-2051-batch-reliability.md` |
| 44 | `docs/domains/orcawave/intended-api/to-orcaflex.md` |
| 45 | `docs/plans/2026-05-05-issue-493-river-current-profiles.md` |
| 46 | Discovery locator with restricted wording; see issue pending (6) |
| 47 | `docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md` |
| 48 | `docs/domains/orcawave/intended-api/aqwa-benchmark.md` |
| 49 | `docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md` |
| 50 | `docs/plans/reviews/2319/code-review.md` |
| 51 | `docs/domains/orcawave/intended-api/README.md` |
| 52 | `docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md` |
| 53 | `docs/plans/2026-05-05-issue-276-propeller-rudder-method-selection.md` |
| 54 | `docs/plans/2026-05-05-issue-487-towing-marine-operations.md` |
| 55 | `docs/plans/2026-09-27-issue-2241-winding.md` |
| 56 | `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` |
| 57 | `docs/domains/orcawave/LICENSED_E2E_ACCEPTANCE.md` |
| 58 | `docs/plans/parametric-refresh/adding-an-atlas.md` |
| 59 | `docs/plans/2026-08-04-issue-1965-published-provenance-paths.md` |
| 60 | `docs/plans/2026-09-27-issue-2253-quad-triangulation.md` |
| 61 | `docs/domains/standards/RAO_PHASE_CONVENTIONS.md` |
| 62 | `docs/plans/2026-05-05-issue-491-rov-intervention-modeling.md` |
| 63 | `docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md` |
| 64 | `docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md` |
| 65 | `docs/plans/2026-05-15-issue-610-orcawave-arbitrary-mesh-acceptance-test.md` |
| 66 | `docs/plans/2026-05-05-issue-283-passing-ship-mooring-response.md` |
| 67 | `docs/plans/2026-05-05-issue-506-passing-ship-jumper-installation-schemas.md` |
| 68 | `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` |
| 69 | `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md` |
| 70 | `docs/plans/2026-08-05-issue-1968-mpi-resume-decomposition-validation.md` |
| 71 | `docs/plans/2026-09-29-issue-2272-a1-dxf-lines-reader.md` |
| 72 | `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` |
| 73 | `docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md` |
| 74 | `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-physics.md` |
| 75 | `docs/domains/manoeuvring-rudder-reference.md` |
| 76 | `docs/plans/evidence/2026-09-12-issue-1633-claude-continuation.md` |
| 77 | `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` |
| 78 | `docs/plans/2026-05-05-issue-490-capping-stack-analysis.md` |
| 79 | `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-evidence.md` |
| 80 | `docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md` |
| 81 | `docs/plans/evidence/2026-09-12-issue-1633-claude-r2.md` |
| 82 | `docs/plans/2026-09-26-issue-2190-brep-curvature-signature.md` |
| 83 | `docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md` |
| 84 | `docs/domains/README.md` |
| 85 | `docs/plans/2026-05-05-issue-464-capytaine-bem-module.md` |
| 86 | `docs/plans/evidence/2026-09-12-issue-2094-claude-r1.md` |
| 87 | `docs/domains/data/DATA_ARCHITECTURE_IMPROVEMENTS_COMPLETE.md` |
| 88 | `docs/plans/2026-08-04-issue-1959-interfoam-runnable-case.md` |
| 89 | `docs/plans/2026-08-05-issue-1970-artifact-generation-commit.md` |
| 90 | `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-testdesign.md` |
| 91 | `docs/plans/evidence/2026-09-12-issue-2094-claude-r2.md` |
| 92 | `docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md` |
| 93 | `docs/domains/infrastructure/cache_implementation_summary.md` |
| 94 | `docs/domains/infrastructure/SKILL_VERSIONING.md` |
| 95 | `docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md` |
| 96 | `docs/domains/submarine-cables/tdp_compression.md` |
| 97 | `docs/domains/orcawave/README.md` |
| 98 | `docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md` |
| 99 | `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` |
| 100 | Discovery locator with restricted wording; see issue pending (6) |
| 101 | Discovery locator with restricted wording; see issue pending (6) |
| 102 | `docs/domains/cathodic_protection/examples/calc-009-dnv-b401-2005-fsr-temporary.md` |
| 103 | `docs/domains/cathodic_protection/_index.md` |
| 104 | `docs/domains/cathodic_protection/examples/calc-004-dnvgl-b401-subsea-riser-base.md` |
| 105 | `docs/domains/cathodic_protection/examples/calc-010-dnv-b401-2021-fst-design-philosophy.md` |
| 106 | `docs/domains/charts/phase2/README.md` |
| 107 | `docs/domains/cathodic_protection/examples/example-02-ship-hull-abs-gn-ships-2017.md` |
| 108 | `docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md` |
| 109 | `docs/domains/cathodic_protection/examples/example-01-pipeline-dnv-f103-2010.md` |
| 110 | `docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md` |
| 111 | `docs/domains/testing/engine_test_implementation_plan.md` |
| 112 | `docs/domains/cathodic_protection/examples/calc-003-dnvgl-f103-2016-flowline-flet-to-flet.md` |
| 113 | `docs/domains/cathodic_protection/examples/calc-011-abs-gn-ships-2017-fst-cp-specification.md` |
| 114 | `docs/domains/testing/CLI_TESTING_PHASE2_SESSION_SUMMARY.md` |
| 115 | `docs/domains/cathodic_protection/examples/calc-008-dnv-b401-2005-slhr-deepwater.md` |
| 116 | `docs/domains/project-docs/MODULE_SURVEY.md` |
| 117 | `docs/domains/orcaflex/examples/catalog/complete_catalog.md` |
| 118 | `docs/domains/cathodic_protection/examples/example-03-platform-dnv-b401-offshore.md` |
| 119 | `docs/domains/marine-engineering/marine_excel_analysis_report.md` |
| 120 | `docs/domains/openfoam/user_manual/openFOAM.md` |
| 121 | `docs/domains/cathodic_protection/examples/calc-005-dnvgl-b401-riser-base-foundation.md` |
| 122 | `docs/domains/cathodic_protection/examples/calc-001-abs-gn-ships-2017-fst-hull.md` |
| 123 | `docs/domains/marine-engineering/marine-engineering-excel-mapping.md` |
| 124 | `docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md` |
| 125 | `docs/domains/marine-engineering/marine_3d_models_catalog.md` |
| 126 | `docs/domains/cathodic_protection/examples/example-03-platform-abs-offshore-2018.md` |
| 127 | `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` |
| 128 | `docs/domains/marine-engineering/marine_excel_analysis_summary.md` |
| 129 | `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` |
| 130 | `docs/domains/marine-engineering/python-code-passing-ship/methodology.md` |
| 131 | `docs/domains/project-docs/PROJECT_YAML_SYSTEM_SUMMARY.md` |
| 132 | `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` |
| 133 | `docs/domains/project-docs/IMPLEMENTATION_SUMMARY_PROPAGATION.md` |
| 134 | `docs/domains/hull_library/dxf-lines-reader.md` |
| 135 | `docs/domains/hull_library/curvature-screening.md` |
| 136 | `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md` |
| 137 | `docs/domains/hull_library/parametric-form.md` |
| 138 | `docs/domains/hull_library/hull_scaling_guide.md` |
| 139 | `docs/domains/orcaflex/property_routing/routing_map.md` |
| 140 | `docs/domains/project-docs/phase2_data_extraction_plan.md` |
| 141 | `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` |
| 142 | `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` |
| 143 | `docs/domains/project-docs/README.md` |
| 144 | `docs/domains/diffraction/examples/ORCAWAVE_TEST_RESULTS.md` |
| 145 | `docs/domains/diffraction/examples/README.md` |
| 146 | `docs/domains/diffraction/tutorials/02_aqwa_conversion_example.md` |
| 147 | `docs/domains/diffraction/PHASE_2_COMPLETION.md` |
| 148 | `docs/domains/hydrodynamics/wang-passing-ship-forces.md` |
| 149 | `docs/domains/diffraction/README.md` |
| 150 | `docs/domains/hydrodynamics/propeller-rudder-literature.md` |
| 151 | `docs/domains/hydrodynamics/propeller-rudder-research-note-2026-10.md` |
| 152 | `docs/domains/hydrodynamics/2026-10-13-fw-loadcases.md` |
| 153 | `docs/domains/hydrodynamics/morison-elements-assessment.md` |
| 154 | `docs/domains/hydrodynamics/bemrosetta/README.md` |
| 155 | `docs/domains/hydrodynamics/references.md` |
| 156 | `docs/domains/hydrodynamics/propeller-rudder-method-selection.md` |
| 157 | `docs/domains/hydrodynamics/propeller-4-quadrant-data.md` |
| 158 | `docs/domains/orcaflex/library/model_library/frps_ssr_global_riser/README.md` |

1 discovery files with identifying names are omitted from the path list; issue pending (6). Map-row inventory separately retains redacted row locators so no implementation row is dropped.

## Depth-classification update (#1704)

Parametric hull analysis uses the cited PIANC 3.0/1.5/1.2 bands, with equality
assigned to the shallower class locally. Bank-clearance x1.2/x1.5 depth
multipliers remain uncited engineering assumptions. See the module
[depth-screening note](../parametric_hull_analysis/README.md) for source
locators, changed behaviour and published turning-diameter trend values.
