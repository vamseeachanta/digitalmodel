# Parametric hull analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/parametric_hull_analysis |
| Repository | digitalmodel |
| Package | `src/digitalmodel/hydrodynamics/parametric_hull_analysis` |
| Pack revision | 0 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | diffraction (DF) |
| Quality role | diffraction quality manager |
| Status | DRAFT |

Source citations resolve at the code revision above, except implementation-map citations in section 9, which resolve at its explicitly stated map revision.

## 1. Purpose and scope

The main sweep generates hull variants, exports GDF meshes, delegates BEM and RAO calculation, and stores a comparison database; the passing-ship route delegates force histories to a separate calculator. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L101-L160] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L86-L157]

The package also implements encounter-frequency remapping, coefficient corrections, shallow-water comparisons, bank-effect screening and charts. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L138-L148] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L310-L338] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L124-L163] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L245-L317] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L66-L93]

The registry assigns production maturity to the hydrodynamics domain; it does not assign a separate maturity to this package at the pin. [docs/registry/module-routing.yaml:L156-L174]

`forward_speed_ms` is recorded in the sweep result but no correction function is called between the stationary BEM solve and RAO storage; resistance is not computed along that path. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L119-L160]

UNVERIFIED: physical accuracy of the complete swept-hull pipeline and its production qualification; #1705 records the missing end-to-end validation.

## 2. Inputs and outputs

Input defaults describe code behavior, not approved design values or licensed-standard tables.

| Input | Unit | Range enforced / default | Source |
| --- | --- | --- | --- |
| `base_hull_id`, `ranges`, `fixed_params` | identifier; parameter dependent | Required ID; empty mappings | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L43-L52] |
| `t_min`, `t_max`, `n_periods` | s, s, count | Defaults 3.0, 25.0, 30; no positivity/order constraints here | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L54-L57] |
| `headings_deg`, `water_depth`, `rho` | degree, m, kg/m³ | Five heading defaults; infinite depth; density 1025.0 passed downstream | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L58-L64] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L226-L237] |
| `target_panels`, `symmetry`, `n_jobs` | panels, boolean, processes | 100–10000 / 1000; true; at least 1 / 1 | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L66-L71] |
| `forward_speed_ms` | m/s | Optional, default None; copied into metadata | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L73-L76] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L139-L154] |
| Separation, passing speed, depth axes | m, m/s, m | Required lists; no item bounds here; stagger count at least 10 / 50; parallel true | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L79-L94] |
| `wave_number` inputs | rad/s, m, m/s² | Infinite-depth default, gravity 9.81; 50 Newton iterations; tolerance 1e-10 | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L30-L63] |
| Bank-effect arguments | m/s, m, m², m, category | Beam and length default 1.0 m; density default 1025.0 kg/m³ | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L196-L243] |

| Output | Unit / representation | Behavior | Source |
| --- | --- | --- | --- |
| Sweep entries and RAO database | Geometry in m; upstream BEM/RAO objects | One result per generated profile; GDF under `meshes/`; Parquet database attempted | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L101-L178] |
| Sweep DataFrame | Geometry in m; peak RAO follows upstream units | Peak over frequencies and headings; absent RAO omits peak columns | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L181-L218] |
| Passing-ship entries / DataFrame | Surge and sway N; yaw N·m (declared) | Cartesian scenario product; peaks or NaNs; threshold boolean | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L119-L131] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L96-L157] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L230-L255] |
| Shallow-water comparison | Dimensionless ratios and error; depth label | Mean heave added-mass ratio; missing values can return NaNs | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L124-L163] |
| BankEffectResult / clearance | N, N·m (declared), m | Formula-based screening and clearance lookup; dimensional concern in section 6 | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L245-L263] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L300-L317] |
| Figures | Matplotlib Figure; PNG or PDF | Agg backend; `save_figure` writes and closes the figure | [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L13-L18] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L408-L414] |

The body builder assumes center of mass `(L/2, 0, -T/2)` in metres and requests all DOFs; this is an assigned mass-center model, not an independently established equilibrium. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L247-L263]

The encounter formula uses angular frequency in rad/s, speed in m/s and headings in radians; the docstring calls zero heading head seas but its formula and tests put increasing encounter frequency at pi. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L105-L148] [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L108-L139]

UNVERIFIED: complete GDF coordinate-origin, RAO phase/sign and rotational-amplitude conventions across the upstream exporters and database; issue pending (1).

## 3. Method and governing standards

The following inventory describes implementation and source attributions; it does not validate licensed clause content.

| Method / implementation | Calculation or delegation | Reference and qualification |
| --- | --- | --- |
| `run_parametric_sweep`, `_build_wave_conditions`, `_build_solver_config`, `_profile_to_body_config` | Hull space → mesh → GDF → BEM → RAO → database; wave and solver configurations are adapters | Manifest attributes BEM and RAO to DNV-RP-C205, Environmental Conditions and Environmental Loads, 2021; clause titles are not recorded for the BEM/RAO entries; clause text not checked against the licensed copy. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/manifest.yaml:L6-L16] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L58-L178] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L226-L263] |
| `wave_number` | Deep-water dispersion closed form; finite-depth Newton iteration with positive clipping | Code-stated linear dispersion relation; residual tested independently of the iteration. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L37-L90] [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L64-L71] |
| `encounter_frequency` | Frequency minus wave number × speed × heading cosine | Code attributes DNV-RP-C205, Environmental Conditions and Environmental Loads, 2021, Forward Speed Effects; clause text not checked against the licensed copy; heading documentation conflicts with tests. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L9-L12] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L105-L148] [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L108-L139] |
| `correct_rao_for_speed` | Retains initial monotonic region, interpolates amplitude and phase, fills outside it with zeros | Same Forward Speed Effects attribution; no independent remapping benchmark is present in its two tests. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L190-L261] [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L262-L317] |
| `strip_theory_speed_correction` | Copies A/B, updates selected off-diagonal couplings, skips near-zero frequency; `length_bp` is unused by the body | Salvesen, Tuck and Faltinsen, Ship Motions and Sea Loads (1970), as named in source; derivation not checked; DNV Forward Speed Effects attribution also appears. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L269-L338] |
| `classify_depth` | Ratio-based categories; every non-finite depth becomes deep | DNV-RP-C205 and PIANC 121 named without edition/clause at the classification; licensed attribution remains unverified. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L21-L27] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L151-L175] |
| `dnv_shallow_water_factor`, `validate_shallow_water_results` | Empirical fits and mean shallow/deep heave ratio; comparison uses the package's own fit | DNV-RP-C205, Environmental Conditions and Environmental Loads, 2021, Shallow water added mass correction (code-stated title); clause text not checked against the licensed copy; no tabulated values reproduced here. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L34-L86] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L89-L163] |
| `pianc_bank_suction_force`, `pianc_bank_clearance_width` | Empirical suction model, assumed yaw lever arm, clearance lookup and depth multiplier | PIANC Report 121, Harbour Approach Channels, 2014; clause/table titles not recorded in source; clause text not checked against the licensed copy. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L196-L317] |
| `run_passing_ship_sweep`, `hull_profile_to_vessel_config` | Delegates force histories, stores their peaks; profile adapter supplies a default coefficient | Wang (1975) slender-body theory as attributed by the module; full publication and equation are not recorded here. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L1-L7] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L33-L157] |
| `_to_rao_db_entry`, `_extract_hull_params`, `_parse_variation_id` | Converts data and parses numeric identifier tokens; not a second BEM solver | Missing RAO fields become zeros; invalid parameter text is ignored. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L266-L310] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L263-L275] |
| `sweep_to_dataframe`, `passing_ship_to_dataframe`, `peak_force_envelope`, `pianc_operability_check` | Export, pivot maxima, absolute sway/yaw thresholds | Data reduction rather than independent force/RAO computation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L181-L218] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L165-L255] |
| Chart functions | Plot stored values; `depth_sensitivity_plot` computes a ratio of means and calls the existing fit | Plot ratio-of-means differs from validator mean-of-ratios for nonuniform spectra. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L165-L250] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L138-L156] |

The manifest and dated forward-speed/shallow-water source references use the same 2021 DNV edition; classification leaves the edition unspecified, and the earlier plan requests a different shallow-water clause without an edition. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/manifest.yaml:L6-L9] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L9-L12] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L58-L60] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L151-L164] [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L21-L21] [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L40-L43]

UNVERIFIED: licensed-standard attribution, empirical-fit validity, publication details for the correction methods, and Citation sidecar compliance; no Citation is emitted by these formula bodies; issue pending (1). [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L138-L148] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L310-L338] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L62-L86] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L245-L263]

## 4. Validated worked examples

Analytical examples validate a closed-form limit or invariant only; they do not qualify a swept hull, empirical correction or solver result. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L34-L39] [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L141-L147]

| Case / kind | Independent basis, input and expected value | Tolerance / basis | Existing test node | Offline record |
| --- | --- | --- | --- | --- |
| Deep dispersion / validated | Closed-form deep-water dispersion; inline frequencies; expected k = omega²/g in 1/m | Relative 1e-10, test assertion; analytical-limit scope | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestWaveNumber::test_deep_water_limit` | PASS at BASE and AFTER; [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L34-L39] |
| Stationary encounter / validated | Closed-form zero-speed invariant; inline headings and frequencies; expected frequency change 0 rad/s | Absolute 1e-12 rad/s, test assertion | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestEncounterFrequency::test_zero_speed_no_change` | PASS at BASE and AFTER; [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L141-L147] |
| Strip identity / regression | Synthetic A/B matrices; expected elementwise unchanged matrices at zero speed; no independent physical benchmark | Exact equality, stored matrix units | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestStripTheorySpeedCorrection::test_zero_speed_unchanged` | PASS at BASE and AFTER; [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L184-L198] |
| Synthetic added-mass ratio / regression | Inline arrays; expected shallow/deep ratio 1.5 dimensionless; synthetic data, not solver validation | Relative 1e-10, test assertion | `tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py::TestValidateShallowWaterResults::test_ratio_increases_for_amplified_shallow` | PASS at BASE and AFTER; [tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py:L144-L163] |
| Failed force history / regression | Mock RuntimeError; expected 1 row with 3 NaN force/moment fields, rather than a physical force | Exact row count and NaN predicates | `tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py::TestRunPassingShipSweep::test_sweep_handles_calculation_failure` | PASS at BASE and AFTER; [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L340-L361] |

Offline execution record: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/hydrodynamics/parametric_hull_analysis -q -rA -p no:cacheprovider --basetemp=<scratch>`; full-path executable form, no addopts override, one process; code pin in header; BASE 126 run / 126 passed / 0 failed / 0 skipped / 0 NOT_RUN; AFTER 126 run / 126 passed / 0 failed / 0 skipped / 0 NOT_RUN on the uncommitted documentation tree.

The force-trend tests construct their own speed/separation laws rather than exercise a physical passing-ship reference; their names are not evidence of an independently validated solver. [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L56-L86] [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L369-L433]

## 5. QA gates

Use AQG r0, sections 3–10, as the gate policy; the following checks are executable evidence fragments rather than a signed analysis release.

| Gate | Applicability and program route | Package check / numeric pass criterion and unit | Evidence and current disposition |
| --- | --- | --- | --- |
| G0 | Applicable: scalar units, geometry and GDF inputs; no OrcaWave data or AQWA deck is written by the shown sweep | C01: `wave_number(0)` gives 0 within 1e-10 1/m | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestWaveNumber::test_zero_omega`; PASS; input-schema/mesh audit remains #1705. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L79-L82] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L108-L123] |
| G1 | Applicable: Python imports/tests; real target load is Capytaine via body config → `run_bem_analysis(..., compute_hydrostatics=True)` → `compute_rao`; OrcaWave/AQWA load routes NOT_APPLICABLE because no target decks are generated | C02: complete module suite, 126 passed tests, 0 failed tests | Offline PASS; Capytaine solver load NOT_RUN, solver execution excluded; version and mesh-load warnings unmeasured, #1705. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L116-L126] |
| G2 | NOT_APPLICABLE: diffraction sweep does not solve mooring statics | No statics load route | BEM and RAO calls only. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L119-L126] |
| G3 | NOT_APPLICABLE: package generates frequency-domain responses, not time-domain motion dynamics | No dynamics acceptance check | Passing-ship time histories are force inputs, not mooring dynamic responses. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L124-L154] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L119-L126] |
| G4 | Applicable: mesh, hydrostatics and response QA for BEM; independent residual check is a limited analytical anchor | C03: `wave_number` at 20.0 m satisfies dispersion with relative residual ≤1e-9 (dimensionless; both sides s⁻²) | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestWaveNumber::test_dispersion_relation_satisfied`; PASS. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L64-L71] |
| G4 | Applicable: zero-speed coefficient transformation | C04: `strip_theory_speed_correction` changes 0 matrix elements at zero m/s; exact equality in input coefficient units | `tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py::TestStripTheorySpeedCorrection::test_zero_speed_unchanged`; PASS regression only. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L184-L198] |
| G4 | Applicable: explicit missing-result sentinel | C05: missing added mass produces exactly 1 NaN ratio (dimensionless), never a finite validation ratio | `tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py::TestValidateShallowWaterResults::test_none_added_mass_returns_nan`; PASS sentinel behavior, not rejection. [tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py:L178-L185] |
| G4 | Applicable: passing-ship failure preservation | C06: 1 failed case retains 1 row with 3 NaN output fields (N, N, N·m) | `tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py::TestRunPassingShipSweep::test_sweep_handles_calculation_failure`; PASS regression only. [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L343-L361] |
| G0 | Applicable: parameter provenance | C07: malformed numeric parameter is omitted; exactly 1 surviving `beam` parameter equals 40.0 m | `tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py::TestParseVariationId::test_non_numeric_param_ignored`; PASS detects fallback, not its absence. [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L110-L114] |
| G0 | Applicable: hull coefficient fallback | C08: missing block coefficient becomes 0.80 dimensionless within pytest.approx default tolerance | `tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py::TestHullProfileToVesselConfig::test_none_block_coefficient_defaults`; PASS regression only. [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L138-L142] |
| G4 | Applicable: omitted chart data | C09: absent RAO still returns 1 Figure object (count), rather than rejecting the omitted result | `tests/hydrodynamics/parametric_hull_analysis/test_charts.py::TestRaoComparisonGrid::test_no_rao_data_gracefully_handled`; PASS documents omission. [tests/hydrodynamics/parametric_hull_analysis/test_charts.py:L195-L210] |
| G5 | Applicable: evidence export before reporting | C10: `save_figure` creates 1 PNG file with size >0 bytes | `tests/hydrodynamics/parametric_hull_analysis/test_charts.py::TestSaveFigure::test_save_png`; PASS export fragment; full signed fingerprinted QA appendix has no detection check, issue pending (4). [tests/hydrodynamics/parametric_hull_analysis/test_charts.py:L365-L371] |
| G4 | Applicable to every saved `meshes/<variation_id>.gdf` and its assigned mass model | C11: hand integrate the closed GDF mesh for volume V; compare abs(rho × V − mass)/mass ≤0.01 dimensionless; zero nonpositive-area panels (count); waterline abs(z) ≤0.001 m; criteria from AQG r0 sections 8.1, 8.5–8.6 | NOT_RUN: no solver-generated mesh or independent mass model was produced; #1705. The package supplies the GDF route and assumed centre, not these evidence records. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L108-L123] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L259-L263] |
| G5 | Applicable to each reported sweep | C12: hand recompute SHA-256 for every declared input and figure; require 0 missing or mismatched files (count) and 0 unrecorded required gates (count); use the saved GDF locations and figure outputs; criteria from AQG r0 sections 4.7 and 9 | NOT_RUN: no analysis QA manifest or signed appendix was generated; issue pending (4). The result metadata and figure writer supply only fragments. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L149-L154] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L408-L414] |

Check count: G0 = 3, G1 = 1, G2 = 0, G3 = 0, G4 = 6, G5 = 2; 12 checks total; C01–C10 exercised offline, C11–C12 defined for a future analysis and NOT_RUN here.

Before an actual sweep is accepted, retain the AQG r0 G4 mesh, mass/buoyancy, centres, waterline, normals and irregular-frequency records and G5 fingerprinted appendix; these records are not generated or checked by the sweep's shown solve/store sequence. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L108-L173]

## 6. Known failure modes and pitfalls

sweep: 72 found

Sweep scope: every package Python file; classify matches rather than treating plotting constants or numeric casts of computed arrays as external text parsing.

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\.get\(|float\(|int\(|or 0|0\.05|0\.5' src/digitalmodel/hydrodynamics/parametric_hull_analysis
rg -n 'zeros|full|nan|isfinite|maximum|tol|max_iter|rho|g:|speed|degree|rad|phase|cache|save|return' src/digitalmodel/hydrodynamics/parametric_hull_analysis
```

Rows include intentional fallbacks and unit assumptions; a regression test that confirms fallback behavior is not a fail-closed engineering safeguard.

| Site / symptom and cause at the pin | Detection check | Incident / follow-up |
| --- | --- | --- |
| Database save exception becomes a warning and a successful return. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L172-L173] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Invalid external identifier number is discarded. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L280-L283] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Invalid external identifier number is discarded. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L271-L274] | C07 (confirms discard) | issue pending (2); observed at code pin, 2026-10-18 |
| Force-history exception becomes NaN peaks and processing continues. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L134-L140] | C06 (confirms sentinel) | issue pending (2); observed at code pin, 2026-10-18 |
| Missing frequency/amplitude returns the input unchanged. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L201-L203] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| No positive encounter value uses an arbitrary positive grid minimum. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L214-L216] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Amplitude output initializes to zeros; incomplete headings can remain zero. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L219-L219] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Phase output initializes to zeros without completeness status. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L220-L220] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| First non-increasing encounter value discards the remaining frequency tail. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L228-L230] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Fewer than two retained points silently skips a heading. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L233-L234] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Missing phase becomes a zero vector. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L238-L238] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Amplitude interpolation outside coverage uses zeros. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L240-L243] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Phase interpolation outside coverage uses zeros. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L244-L247] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Corrected complex RAO and dataset are discarded as None. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L253-L260] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Near-zero speed returns unchanged coefficient copies. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L313-L314] | C04 (zero-speed case) | issue pending (2); observed at code pin, 2026-10-18 |
| Near-zero frequency skips all coupling corrections at that index. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L321-L322] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Small derivative produces zero update; positivity clipping may hide invalid inputs. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L80-L85] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Newton iteration returns k without reporting exhaustion or residual. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L75-L90] | C03 (ordinary positive input only) | issue pending (2); observed at code pin, 2026-10-18 |
| NaN and either infinite depth are all classified deep. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L165-L166] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Positive draft is documented but division/classification does not enforce it. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L167-L175] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Deep-depth cutoff returns a fixed neutral factor. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L62-L63] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Out-of-range depth ratio clamps instead of reporting invalid geometry. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L65-L66] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Unknown DOF logs and substitutes heave coefficients. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L78-L81] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Missing added mass returns NaN ratio/error in an otherwise normal dictionary. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L130-L136] | C05 (ratio sentinel only) | issue pending (2); observed at code pin, 2026-10-18 |
| Near-zero deep heave samples are removed from the ratio average. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L143-L144] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| No retained deep samples returns NaN ratio/error. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L145-L151] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Nonpositive beam substitutes zero bank proximity and force. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L248-L248] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Missing clearance-table key substitutes a fixed fraction. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L308-L308] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Depth class changes clearance by fixed multipliers without evidence record. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L312-L315] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Missing or zero block coefficient is replaced by a default. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L48-L54] | C08 (None input only) | issue pending (2); observed at code pin, 2026-10-18 |
| Every non-finite depth is sent downstream as None. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L115-L117] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Unknown variation returns three empty DataFrames. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L202-L209] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Pivot maximum aggregates across unfiltered water depths and drops NaN groups. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L219-L224] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Absent forward speed is stored as zero; nonzero speed is only recorded here. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L149-L149] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Result speed defaults to zero regardless of result provenance. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L115-L115] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Missing RAO/amplitude/DOF names or excess DOF indices omits peak columns. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L204-L210] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Missing panel/time metadata becomes None via dict.get. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L213-L214] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Absent RAO headings become zero heading. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L299-L299] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Absent RAO amplitudes become a zero response array. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L300-L300] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Absent RAO phases become a zero phase array. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L301-L301] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| RAO plot selects heading index zero by default. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L33-L33] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| RAO plot skips missing result/amplitude. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L68-L69] | C09 (confirms omission) | issue pending (3); observed at code pin, 2026-10-18 |
| RAO plot omits missing/unknown/out-of-range DOFs. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L72-L81] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| RAO plot clamps requested upper heading to last available heading. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L83-L84] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Parameter plot selects heading index zero by default. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L101-L101] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Parameter plot skips absent parameter. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L131-L133] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Parameter plot skips missing RAO/amplitude. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L135-L137] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Parameter plot skips unknown DOF. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L139-L146] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Parameter plot clamps heading; nanmax ignores invalid samples. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L148-L149] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Depth plot returns a message-only figure without deep reference. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L193-L197] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Unknown depth-plot DOF defaults to heave index. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L200-L205] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Missing deep BEM returns an empty figure. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L209-L210] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Depth plot skips non-finite depth and empty entries. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L218-L219] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Depth plot skips missing shallow BEM. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L224-L225] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Depth plot silently omits ratio when mean deep mass is near zero. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L228-L231] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Contour returns a message-only figure without data. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L288-L292] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Unknown contour component substitutes sway. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L294-L299] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Insufficient contour grid returns a message without force contours. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L315-L322] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Contour aggregates depths with max if no depth filter is supplied, despite first-depth wording. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L305-L307] | no detection check exists | issue pending (3); observed at code pin, 2026-10-18 |
| Angular frequency, depth and gravity assume stated SI units with no dimensional validation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L30-L63] | C01 / C03 cover ordinary input only | issue pending (1); observed at code pin, 2026-10-18 |
| Heading/sign description conflicts with formula and tests. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L105-L148] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Fixed six-DOF indices and array units; unused normalization length. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L269-L338] | C04 covers zero-speed identity only | issue pending (1); observed at code pin, 2026-10-18 |
| Declared N output has force-per-length dimensions under documented SI arguments; yaw inherits this concern. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L245-L254] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Speed interpreted as knots, unlike suction function m/s. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L300-L317] | no detection check exists | issue pending (1); observed at code pin, 2026-10-18 |
| Center of mass assumes mid-length and half-draft without supplied mass model. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L259-L263] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Configured stagger count and parallel flag are not forwarded to the history call. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L124-L133] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Wave limits, period count, heading and density use defaults without field bounds. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L54-L64] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Scenario lists accept empty or physically invalid values at this layer. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L82-L94] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Empty sweep still attempts database persistence without an empty-result safeguard. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L166-L173] | no detection check exists | #1705; observed at code pin, 2026-10-18 |
| Absent headings assumes one heading while passing None to encounter mapping. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L205-L211] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Zero output-count input falls back to input frequency count. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L208-L208] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |
| Nonpositive encounter frequencies produce infinite period values. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L250-L250] | no detection check exists | issue pending (2); observed at code pin, 2026-10-18 |

Numeric casts outside identifier parsing consume computed NumPy/scalar values, not locale-formatted external text; neither a 1970 fallback nor an empty-result cache writer occurs in these bodies; persistence attempts the database save after the loop even when no results were accumulated. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L214-L247] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L153-L163] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L166-L178]

#1704 is a historical missing-test lead, not a confirmed runtime incident: four test modules now contain the checks referenced above; #1705 remains a missing end-to-end qualification, with no solver-free full-pipeline detection check. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L31-L39] [tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py:L34-L41] [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L288-L316] [tests/hydrodynamics/parametric_hull_analysis/test_charts.py:L151-L165]

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial documentation | Distinguish synthetic force trends from executed physical calculations; classify these as regression evidence | [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L56-L86] [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L369-L433] | Not created; issue pending (4) |

## 8. Ownership

Review assignment: diffraction quality manager; deputy: a maintainer-designated independent diffraction reviewer; producing discipline: hydrodynamics. Escalate disputed method classification, missing sources, conventions or tolerance changes to that quality role; the maintainer handles cross-module scope. Review and clearance remain pending.

## 9. Dependencies

The upstream hull-library route supplies parametric space, meshes, GDF export and the RAO database; the passing-ship route supplies vessel/environment configuration, calculator and force histories; downstream tables and charts consume the stored result containers. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L71-L160] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L115-L133] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L66-L87]

Capytaine dependency version: the declared optional dependency is 3.0.0 and the adapter requires major version at least 3; no solver version was queried or run. [pyproject.toml:L105-L108] [src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py:L26-L48]

Every source occurrence of Capytaine, including documentation, annotations, labels and result keys, is inventoried below; occurrences were read as text, with no external solver import.

| File | Lines containing the dependency name, case-insensitive | Role and evidence |
| --- | --- | --- |
| `__init__.py` | 4 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/__init__.py:L4-L4] |
| `charts.py` | 172, 213, 234 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L172-L172] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L213-L213] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L234-L234] |
| `forward_speed.py` | 6, 171, 194 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L6-L6] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L171-L171] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L194-L194] |
| `manifest.yaml` | 14, 44 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/manifest.yaml:L14-L14] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/manifest.yaml:L44-L44] |
| `models.py` | 112, 113 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L112-L112] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L113-L113] |
| `passing_ship_sweep.py` | 5 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L5-L5] |
| `shallow_water.py` | 4, 7, 41, 95, 98, 104, 106, 115, 120, 132, 147, 154, 156, 159 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L4-L4] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L7-L7] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L41-L41] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L95-L95] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L98-L98] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L104-L104] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L106-L106] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L115-L115] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L120-L120] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L132-L132] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L147-L147] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L154-L154] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L156-L156] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L159-L159] |
| `sweep.py` | 3, 56, 66, 228, 242, 249, 296 | Dependency text locations. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L3-L3] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L56-L56] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L66-L66] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L228-L228] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L242-L242] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L249-L249] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L296-L296] |

### Implementation-map audit

Map ref: `origin/codex/implmap-work`; map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; all `docs/registry/implementation-map.yaml` citations in this section resolve at that map commit. [docs/registry/implementation-map.yaml:L1-L3]

| Counted rank | Family | In-package rows | Entire family inventory |
| --- | --- | --- | --- |
| 1 | `rao-readers-and-orcaflex-converters` | 2: `run_parametric_sweep`, `_to_rao_db_entry` | 295 rows, enumerated below. [docs/registry/implementation-map.yaml:L11749-L11777] |
| 2 | `linear-wave-kinematics` | 1: `wave_number` | 21 rows, enumerated below. [docs/registry/implementation-map.yaml:L26255-L26306] |

Both map families are counted, ordered by their in-package row counts. Their complete 316-row inventories follow, including external definitions; the ordinal is the implementation's position within that family's YAML list. External paths and symbols remain at their exact cited map rows instead of copying identifying discovery text into this pack.

Search procedure: read package `.py` blobs at the header pin, apply Python `re.finditer` with case-sensitive patterns exactly as supplied by this map revision, and assign hits to the smallest enclosing AST function/class, otherwise one module-level definition per file. Before searching, omit a signal if Python `re.search(pattern, package_path_below_src, re.I)` matches the package path.

#### Signals: `rao-readers-and-orcaflex-converters`

- `algorithm`: `\.lis\b|\.hyd\b|\.owr\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\.content|_table_span|parse_aqwa_lis_file|AQWALISParser` — searched exactly as written.
- `vocabulary`: `\bRAO\b|\braos?\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine` — omitted: vocabulary matches hydrodynamics through HydroD when comparing the package path case-insensitively.
- `constants_or_keys`: `ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection` — searched exactly as written.

#### Signals: `linear-wave-kinematics`

- `algorithm`: `\b(?:tanh|sinh|cosh)\s*\(|solve_dispersion|wave_number|wavenumber|velocity_transfer` — searched exactly as written.
- `vocabulary`: `wave.kinematics|dispersion|orbital.velocity|Airy` — searched exactly as written.
- `constants_or_keys`: `water_depth|omega|seabed|wave_number|wavenumber` — searched exactly as written.

The RAO search yields only its two listed sweep definitions; the wave search yields 21 enclosing definitions, classified below. All computation-bearing package hits are already listed in section 3 or the map inventory: 0 further second implementations, so the counted-family audit passes. Helpers and unrelated calculations do not become wave solvers merely by accepting depth or frequency.

| Wave-search hit | Classification and source evidence |
| --- | --- |
| `__init__.py` / `<module>` | Imports, exports or section commentary, no dispersion computation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/__init__.py:L22-L63] |
| `charts.py` / `depth_sensitivity_plot` | Plot/helper; ratio of stored data listed in section 3. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L177-L177] |
| `charts.py` / `passing_ship_contour` | Plot/helper; result filtering. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L262-L326] |
| `forward_speed.py` / `wave_number` | Mapped implementation; Newton dispersion solve. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L30-L78] |
| `forward_speed.py` / `encounter_frequency` | Computes encounter transform, listed in section 3; delegates wave-number computation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L99-L148] |
| `forward_speed.py` / `<module>` | Imports, exports or section commentary, no dispersion computation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L26-L343] |
| `forward_speed.py` / `correct_rao_for_speed` | RAO remapping listed in section 3; delegates encounter and wave-number computation. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L159-L256] |
| `forward_speed.py` / `strip_theory_speed_correction` | Coefficient correction listed in section 3; not a dispersion solve. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L272-L320] |
| `models.py` / `SweepConfig` | Data configuration. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L61-L61] |
| `models.py` / `PassingShipSweepConfig` | Data configuration. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L88-L88] |
| `models.py` / `PassingShipSweepEntry` | Data container. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L127-L127] |
| `models.py` / `BankEffectResult` | Data container. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L143-L143] |
| `models.py` / `classify_depth` | Depth classification listed in section 3. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L151-L167] |
| `passing_ship_sweep.py` / `run_passing_ship_sweep` | Delegates histories; depth scenario orchestration. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L69-L149] |
| `passing_ship_sweep.py` / `passing_ship_to_dataframe` | Export helper. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L171-L179] |
| `shallow_water.py` / `validate_shallow_water_results` | Ratio comparison listed in section 3, not wave kinematics. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L93-L162] |
| `shallow_water.py` / `pianc_bank_suction_force` | Bank formula listed in section 3, not wave kinematics. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L198-L262] |
| `shallow_water.py` / `pianc_bank_clearance_width` | Lookup listed in section 3, not wave kinematics. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L269-L311] |
| `sweep.py` / `run_parametric_sweep` | Orchestrates mapped calls; does not implement dispersion. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L129-L129] |
| `sweep.py` / `_build_wave_conditions` | Configuration adapter. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L235-L235] |
| `sweep.py` / `_to_rao_db_entry` | Mapped serialization adapter. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L298-L298] |

Repository-wide search (including `scripts/`): `git grep -n -E 'def (wave_number|wavenumbers|wavenumber|_solve_dispersion|dispersion_wavenumber|encounter_frequency|correct_rao_for_speed|strip_theory_speed_correction|dnv_shallow_water_factor|pianc_bank_suction_force|pianc_bank_clearance_width|run_parametric_sweep|run_passing_ship_sweep)\b' <code-pin> -- '*.py'` returned 31 definition lines; `git grep -n -E 'read_rao|parse_rao|read_wamit|read_hyd' <code-pin> -- '*.py'` returned 75 lines; these are discovery searches rather than the counted-package grep.

An additional external dispersion implementation appears in `hydrodynamics/diffraction/long_wave_limits.py::wavenumber`; it is a map follow-up rather than an in-package miss. Another encounter transform appears in naval architecture. [src/digitalmodel/hydrodynamics/diffraction/long_wave_limits.py:L36-L61] [src/digitalmodel/naval_architecture/seakeeping.py:L126-L143]

Map follow-up: add or reconcile encounter-frequency remapping, strip-theory coupling corrections, depth classification, shallow-water fits/ratios, bank suction/clearance and passing-ship force families, which have no in-package implementation rows in this map; reconcile the external dispersion gap; issue pending (5). These families start counting only after the map adds rows within the package. Source methods are listed in section 3. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L98-L338] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L34-L317] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L62-L157]

### Map inventory: `rao-readers-and-orcaflex-converters`

| Map ordinal | Implementation locator |
| --- | --- |
| 1 | External definition 1; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | External definition 2; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | External definition 3; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | External definition 4; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | External definition 5; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | External definition 6; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | External definition 7; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | External definition 8; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | External definition 9; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | External definition 10; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | External definition 11; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | External definition 12; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | External definition 13; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | External definition 14; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | External definition 15; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | External definition 16; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | External definition 17; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | External definition 18; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | External definition 19; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | External definition 20; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | External definition 21; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | External definition 22; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | External definition 23; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | External definition 24; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | External definition 25; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | External definition 26; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | External definition 27; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | External definition 28; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | External definition 29; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | External definition 30; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | External definition 31; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | External definition 32; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | External definition 33; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | External definition 34; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | External definition 35; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | External definition 36; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | External definition 37; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | External definition 38; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | External definition 39; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | External definition 40; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | External definition 41; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | External definition 42; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | External definition 43; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | External definition 44; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | External definition 45; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | External definition 46; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | External definition 47; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | External definition 48; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | External definition 49; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | External definition 50; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | External definition 51; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | External definition 52; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | External definition 53; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | External definition 54; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | External definition 55; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | External definition 56; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | External definition 57; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | External definition 58; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | External definition 59; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | External definition 60; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | External definition 61; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | External definition 62; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | External definition 63; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | External definition 64; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | External definition 65; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | External definition 66; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | External definition 67; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | External definition 68; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | External definition 69; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | External definition 70; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | External definition 71; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | External definition 72; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | External definition 73; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | External definition 74; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | External definition 75; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | External definition 76; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | External definition 77; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | External definition 78; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | External definition 79; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | External definition 80; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | External definition 81; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | External definition 82; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | External definition 83; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | External definition 84; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | External definition 85; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | External definition 86; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | External definition 87; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | External definition 88; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | External definition 89; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | External definition 90; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | External definition 91; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | External definition 92; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | External definition 93; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | External definition 94; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | External definition 95; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | External definition 96; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | External definition 97; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | External definition 98; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | External definition 99; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | External definition 100; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | External definition 101; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | External definition 102; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | External definition 103; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | External definition 104; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | External definition 105; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | External definition 106; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | External definition 107; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | External definition 108; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | External definition 109; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | External definition 110; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | External definition 111; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | External definition 112; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | External definition 113; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | External definition 114; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | External definition 115; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | External definition 116; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | External definition 117; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | External definition 118; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | External definition 119; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | External definition 120; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | External definition 121; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | External definition 122; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | External definition 123; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | External definition 124; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | External definition 125; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | External definition 126; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | External definition 127; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | External definition 128; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | External definition 129; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | External definition 130; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | External definition 131; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | External definition 132; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | External definition 133; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | External definition 134; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | External definition 135; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | External definition 136; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | External definition 137; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | External definition 138; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | External definition 139; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | External definition 140; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | External definition 141; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | External definition 142; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | External definition 143; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | External definition 144; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | External definition 145; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | External definition 146; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | External definition 147; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | External definition 148; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | External definition 149; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | External definition 150; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | External definition 151; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | External definition 152; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | External definition 153; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | External definition 154; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | External definition 155; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | External definition 156; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | External definition 157; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | External definition 158; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | External definition 159; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | External definition 160; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | External definition 161; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | External definition 162; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | External definition 163; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | External definition 164; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | External definition 165; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | External definition 166; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | External definition 167; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | External definition 168; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | External definition 169; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | External definition 170; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | External definition 171; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | External definition 172; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | External definition 173; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | External definition 174; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | External definition 175; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | External definition 176; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | External definition 177; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | External definition 178; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` — `run_parametric_sweep`. [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` — `_to_rao_db_entry`. [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | External definition 181; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | External definition 182; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | External definition 183; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | External definition 184; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | External definition 185; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | External definition 186; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | External definition 187; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | External definition 188; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | External definition 189; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | External definition 190; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | External definition 191; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | External definition 192; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | External definition 193; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | External definition 194; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | External definition 195; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | External definition 196; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | External definition 197; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | External definition 198; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | External definition 199; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | External definition 200; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | External definition 201; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | External definition 202; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | External definition 203; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | External definition 204; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | External definition 205; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | External definition 206; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | External definition 207; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | External definition 208; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | External definition 209; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | External definition 210; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | External definition 211; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | External definition 212; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | External definition 213; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | External definition 214; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | External definition 215; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | External definition 216; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | External definition 217; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | External definition 218; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | External definition 219; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | External definition 220; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | External definition 221; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | External definition 222; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | External definition 223; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | External definition 224; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | External definition 225; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | External definition 226; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | External definition 227; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | External definition 228; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | External definition 229; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | External definition 230; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | External definition 231; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | External definition 232; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | External definition 233; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | External definition 234; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | External definition 235; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | External definition 236; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | External definition 237; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | External definition 238; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | External definition 239; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | External definition 240; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | External definition 241; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | External definition 242; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | External definition 243; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | External definition 244; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | External definition 245; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | External definition 246; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | External definition 247; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | External definition 248; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | External definition 249; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | External definition 250; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | External definition 251; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | External definition 252; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | External definition 253; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | External definition 254; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | External definition 255; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | External definition 256; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | External definition 257; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | External definition 258; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | External definition 259; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | External definition 260; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | External definition 261; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | External definition 262; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | External definition 263; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | External definition 264; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | External definition 265; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | External definition 266; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | External definition 267; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | External definition 268; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | External definition 269; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | External definition 270; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | External definition 271; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | External definition 272; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | External definition 273; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | External definition 274; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | External definition 275; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | External definition 276; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | External definition 277; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | External definition 278; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | External definition 279; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | External definition 280; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | External definition 281; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | External definition 282; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | External definition 283; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | External definition 284; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | External definition 285; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | External definition 286; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | External definition 287; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | External definition 288; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | External definition 289; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | External definition 290; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | External definition 291; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | External definition 292; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | External definition 293; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | External definition 294; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | External definition 295; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L13229-L13240] |

### Map inventory: `linear-wave-kinematics`

| Map ordinal | Implementation locator |
| --- | --- |
| 1 | External definition 1; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26260-L26269] |
| 2 | External definition 2; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26270-L26279] |
| 3 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py` — `wave_number`. [docs/registry/implementation-map.yaml:L26280-L26289] |
| 4 | External definition 4; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26290-L26299] |
| 5 | External definition 5; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26300-L26309] |
| 6 | External definition 6; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26310-L26319] |
| 7 | External definition 7; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26320-L26329] |
| 8 | External definition 8; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26330-L26339] |
| 9 | External definition 9; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26340-L26349] |
| 10 | External definition 10; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26350-L26361] |
| 11 | External definition 11; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26362-L26373] |
| 12 | External definition 12; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26374-L26384] |
| 13 | External definition 13; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26385-L26395] |
| 14 | External definition 14; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26396-L26405] |
| 15 | External definition 15; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26406-L26415] |
| 16 | External definition 16; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26416-L26425] |
| 17 | External definition 17; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26426-L26435] |
| 18 | External definition 18; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26436-L26447] |
| 19 | External definition 19; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26448-L26458] |
| 20 | External definition 20; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26459-L26474] |
| 21 | External definition 21; path and symbol at cited map row. [docs/registry/implementation-map.yaml:L26475-L26486] |

## 10. Coverage and open items

Read coverage: all 7 package Python files (`__init__.py`, `models.py`, `sweep.py`, `forward_speed.py`, `shallow_water.py`, `passing_ship_sweep.py`, `charts.py`) and `manifest.yaml`; the package has no child source package or README at the code pin. [src/digitalmodel/hydrodynamics/parametric_hull_analysis/__init__.py:L1-L84] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/models.py:L1-L187] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py:L1-L316] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py:L1-L346] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/shallow_water.py:L1-L325] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/passing_ship_sweep.py:L1-L284] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/charts.py:L1-L441] [src/digitalmodel/hydrodynamics/parametric_hull_analysis/manifest.yaml:L1-L46]

Test read coverage: all four discovered test modules and the empty package initializer; 26 forward-speed, 34 shallow-water, 34 passing-ship and 32 chart tests, total 126, counted from executed pytest node results; no package-local tests were found. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L1-L317] [tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py:L1-L356] [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L1-L433] [tests/hydrodynamics/parametric_hull_analysis/test_charts.py:L1-L407]

Documentation discovery: `rg -l -i 'parametric[ _-]hull[ _-]analysis|parametric_hull_analysis' docs` found the two implementation-map documents, the two dated plans and the curvature-screening evaluation below; historical plans and external evaluation figures are not validation evidence for this package. [docs/registry/implementation-map.md:L20-L29] [docs/plans/2026-05-05-issue-466-parametric-hull-analysis.md:L1-L19] [docs/plans/2026-05-05-issue-283-passing-ship-mooring-response.md:L1-L13] [docs/domains/hull_library/hullprod-curvature-screening-evaluation.md:L1-L12]

| Open item / coverage limitation | Responsible discipline | Tracking |
| --- | --- | --- |
| Claim from section 1: complete swept-hull physical qualification; independent worked hull benchmark and full pipeline execution absent from this evidence set | Hydrodynamics | #1705; benchmark programme #1839 |
| Claim from section 2: coordinate origin, phase/sign and rotational units across GDF and upstream database | Hydrodynamics | issue pending (1) |
| Claim from section 3: licensed attribution, empirical coefficients and fits, correction derivations and Citation emission; reconcile the heading documentation and bank-force dimensions | Hydrodynamics | issue pending (1) |
| Numeric parsing, omitted results, convergence/completeness status and ignored configuration controls in section 6 | Hydrodynamics | issue pending (2); persistence/full sweep under #1705 |
| Plot omission, mixed-depth envelopes and ratio-of-means versus mean-of-ratios; add explicit completeness checks | Hydrodynamics | issue pending (3) |
| Full G0/G1/G4 solver/mesh qualification, mass model, data fingerprint and independent response benchmark | Hydrodynamics | #1705; #1839 |
| G5 generated signed appendix and shared lessons-log entry are not delivered by this package | Hydrodynamics | issue pending (4) |
| Method-family inventory extensions and external map gaps from section 9 | Hydrodynamics | issue pending (5) |
| Historical missing-tests claim is superseded by measured 126-case offline coverage, while independent benchmark coverage remains open | Hydrodynamics | #1704; #1839 |

No private sidecar was read or created; no client-derived values enter the examples. The only examples described here use inline arrays and mocks. [tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py:L34-L39] [tests/hydrodynamics/parametric_hull_analysis/test_passing_ship_sweep.py:L56-L86]
