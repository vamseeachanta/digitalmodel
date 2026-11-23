# Motion forecast knowledge pack

| Header | Value |
| --- | --- |
| Module | `motion_forecast` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/motion_forecast/` |
| Pack revision | 0 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations (EC), AQG r0 Python calculations |
| Quality manager | Engineering calculations family quality manager |
| Status | DRAFT |

Repository citations refer to the code revision above; section 9 separately pins the implementation-map snapshot used for reconciliation.

## 1. Purpose and scope

The core superposes phased incident-wave components through a complex response amplitude operator (RAO), propagating the wave phase from a reference position to the asset and returning six motion histories. [src/digitalmodel/motion_forecast/reconstruct.py:L68-L146]

The package also computes point-of-interest vertical motion, governing magnitudes, rolling decisions, measured-motion reconciliation, pooled forecast skill, affine output corrections, operability fractions and a lumped dynamic-tension screen. [src/digitalmodel/motion_forecast/reconstruct.py:L149-L170] [src/digitalmodel/motion_forecast/derived.py:L36-L85] [src/digitalmodel/motion_forecast/decision.py:L63-L106] [src/digitalmodel/motion_forecast/reconcile.py:L44-L155] [src/digitalmodel/motion_forecast/skill.py:L65-L115] [src/digitalmodel/motion_forecast/recalibrate.py:L58-L120] [src/digitalmodel/motion_forecast/feedback.py:L26-L56] [src/digitalmodel/motion_forecast/top_tension.py:L143-L182]

The routing registry records maturity as beta. [docs/registry/module-routing.yaml:L241-L246]

Synthetic linear-wave synthesis is implemented; radar/lidar reconstruction and nonlinear wave evolution are explicitly outside that implementation. [src/digitalmodel/motion_forecast/wave_forecast.py:L1-L13]

The tension calculation is a linear lumped screen: slack onset invalidates its amplification factor, and it does not resolve snatch impact or distributed line dynamics. [src/digitalmodel/motion_forecast/top_tension.py:L14-L18] [src/digitalmodel/motion_forecast/top_tension.py:L173-L182]

## 2. Inputs and outputs

### Inputs

| Input | Unit and convention | Range enforced / default | Source |
| --- | --- | --- | --- |
| `WaveComponent(omega, amplitude, phase, heading)` | rad/s, m, rad, deg; cosine time convention, heading going-to from +x | Dataclass has no numerical validator; heading defaults to zero | [src/digitalmodel/motion_forecast/models.py:L30-L50] |
| `WaveForecast` | Horizon and origin time s; reference `(x,y)` m; depth m | Origin and reference default to zero; depth `None` selects deep water | [src/digitalmodel/motion_forecast/models.py:L53-L65] [src/digitalmodel/motion_forecast/reconstruct.py:L25-L40] |
| `rao.H(dof, omega, heading)` | Translation m/m; rotation deg/m; canonical phase lead | Analytic missing DOF gives zero; grid interpolation clamps rather than wraps headings | [src/digitalmodel/motion_forecast/rao_adapter.py:L27-L55] [src/digitalmodel/motion_forecast/rao_adapter.py:L103-L115] |
| `asset_location`, `dt`, `t_grid`, `dofs` | Position m, times s; same horizontal frame as wave reference | Position zero; `dt=0.2` s; default six DOFs; explicit times outside horizon raise | [src/digitalmodel/motion_forecast/reconstruct.py:L68-L116] |
| Synthetic sea | `hs` m, `tp` s, `gamma` dimensionless, heading deg | 48 components, 90 s horizon, seeded phases; numerical domain not validated here | [src/digitalmodel/motion_forecast/wave_source.py:L49-L102] |
| Directional sea | Same sea units; aperture m, spread exponent dimensionless | 32 frequencies, 7 headings; aperture absent selects coherence proxy; nonpositive aperture raises | [src/digitalmodel/motion_forecast/wave_forecast.py:L67-L93] [src/digitalmodel/motion_forecast/wave_forecast.py:L111-L156] |
| `Criterion` / YAML | Governing quantity unit string, caution and limit in that unit; offset m; alpha dimensionless | Enforces known governing key, positive caution no larger than limit, three offset coordinates; alpha defaults to unity, basis to empty text | [src/digitalmodel/motion_forecast/criteria.py:L24-L94] |
| Measured CSV | `t,surge,sway,heave,roll,pitch,yaw`; absolute s, m, deg | Extra columns ignored; required names, ascending nonempty clock and matching array shapes checked | [src/digitalmodel/motion_forecast/measured_source.py:L57-L72] [src/digitalmodel/motion_forecast/measured.py:L29-L52] |
| `LumpedLine` | Payload/added mass kg; EA N; length m; static tension N; damping ratio dimensionless | Positive payload, EA, length, tension; nonnegative added mass; damping strictly between zero and unity | [src/digitalmodel/motion_forecast/top_tension.py:L30-L77] |
| Workflow configuration | `sea`, `rao`, `asset`, optional operation/measured/skill | Empty configuration supplies synthetic sea and analytic demo RAO; reference location, depth and origin time are not forwarded by either sea builder | [src/digitalmodel/motion_forecast/workflow.py:L62-L124] |

Internal phase is `omega*t + phase + arg(H)`, with a negative projected spatial phase; rotations are converted from degrees to radians for the point-of-interest lever arm `heave - rx*pitch + ry*roll`, and `rz` contributes no vertical term. [src/digitalmodel/motion_forecast/conventions.py:L12-L18] [src/digitalmodel/motion_forecast/reconstruct.py:L118-L136] [src/digitalmodel/motion_forecast/reconstruct.py:L149-L165]

### Outputs

| Output | Unit / interpretation | Source |
| --- | --- | --- |
| `MotionForecast.t`, `.dof`, `.significant()`, `.max_abs()` | Absolute s, translations m, rotations deg; significant amplitude is twice population standard deviation | [src/digitalmodel/motion_forecast/models.py:L74-L99] |
| `GoverningSeries`, `RollingDecision` | Nonnegative magnitude in m, m/s or deg; first sample is forecast now; lead times s or `None` for no crossing | [src/digitalmodel/motion_forecast/derived.py:L24-L85] [src/digitalmodel/motion_forecast/decision.py:L40-L105] |
| `MeasuredDecision`, seam offsets, overlap errors | Latest sample is measured now; errors in DOF units, correlation dimensionless or `None` | [src/digitalmodel/motion_forecast/measured.py:L54-L60] [src/digitalmodel/motion_forecast/reconcile.py:L44-L155] |
| `AggregateSkill`, correction, holdout | RMSE/bias in DOF units; sample count, correlation median/IQR; gain dimensionless and bias in DOF units | [src/digitalmodel/motion_forecast/skill.py:L85-L115] [src/digitalmodel/motion_forecast/recalibrate.py:L28-L42] [src/digitalmodel/motion_forecast/recalibrate.py:L87-L120] |
| `OperabilitySummary` | Fractions dimensionless; longest run counts samples, not duration | [src/digitalmodel/motion_forecast/feedback.py:L16-L56] |
| `TensionResult` | Tension/minimum N; peak/static amplification dimensionless; slack and validity flags | [src/digitalmodel/motion_forecast/top_tension.py:L102-L111] [src/digitalmodel/motion_forecast/top_tension.py:L173-L182] |
| `router(cfg)` | Mutated configuration containing arrays as lists and optional decision, measured and skill summaries | [src/digitalmodel/motion_forecast/workflow.py:L112-L204] |

## 3. Method and governing standards

| Method / implementing definition | Evidence and boundary |
| --- | --- |
| Linear dispersion, `wavenumber` and nested residual `wavenumber.f` | Deep-water algebra or bracketed finite-depth bisection; nonpositive frequency gives zero and invalid depth falls back to deep water. [src/digitalmodel/motion_forecast/reconstruct.py:L25-L65] |
| Phased superposition, `reconstruct_motion`; free surface, `surface_elevation` | Component cosine sum with directional projection; the surface evaluator delegates dispersion to `wavenumber`. [src/digitalmodel/motion_forecast/reconstruct.py:L118-L146] [src/digitalmodel/motion_forecast/wave_forecast.py:L161-L177] |
| RAO conversion, `normalize_phase_deg`, `heading_going_to_deg`, `GridRAO.__init__`, `_interp_complex` and nested `_interp_freq` | Phase sign normalization, sorting and complex bilinear interpolation; source sign correctness needs independent evidence. [src/digitalmodel/motion_forecast/conventions.py:L40-L74] [src/digitalmodel/motion_forecast/rao_adapter.py:L27-L100] |
| Spectrum, `jonswap_spectrum`; long/directional component generation | Spectrum numerically normalized to requested height; amplitudes follow spectral-bin variance and directional weights. [src/digitalmodel/motion_forecast/wave_source.py:L21-L46] [src/digitalmodel/motion_forecast/wave_source.py:L79-L102] [src/digitalmodel/motion_forecast/wave_forecast.py:L128-L156] |
| `_moments`, `coherence_horizon`, `dpz_horizon`, `directional_spread` | Discrete energy moments; clamped bandwidth proxy; energetic-band deep-water group-arrival spread; normalized cosine directional weights. [src/digitalmodel/motion_forecast/wave_forecast.py:L29-L108] |
| `generic_vessel_rao` and nested `_mech`, `_rolloff`, `heave`, `pitch`, `roll` | Demo response uses oscillator transfer and its own deep-water `omega*omega/g` in `_rolloff`, `pitch` and `roll`; these are additional algebraic wave-number evaluations, not independent finite-depth solvers or hand-check references. [src/digitalmodel/motion_forecast/workflow.py:L30-L59] |
| Point kinematics and numerical differentiation | Small-angle rigid-body lever arm and `numpy.gradient`; no spectral differentiation is implemented here despite the historical plan. [src/digitalmodel/motion_forecast/reconstruct.py:L149-L170] [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L36-L38] |
| Decision taxonomy and rolling crossing | Delegates classification/crossing to `decision_spine`; nonfinite governing values force present NO_GO. [src/digitalmodel/motion_forecast/decision.py:L23-L35] [src/digitalmodel/motion_forecast/decision.py:L76-L105] |
| Reconciliation, skill, recalibration, operability | Linear time interpolation, pooled residual statistics, affine least squares and state-count fractions; affine correction cannot establish improved phase prediction. [src/digitalmodel/motion_forecast/reconcile.py:L27-L110] [src/digitalmodel/motion_forecast/skill.py:L49-L115] [src/digitalmodel/motion_forecast/recalibrate.py:L1-L13] [src/digitalmodel/motion_forecast/recalibrate.py:L58-L120] [src/digitalmodel/motion_forecast/feedback.py:L26-L56] |
| Tension, `LumpedLine`, transmissibility functions, `_newmark`, `dynamic_tension` | Base-excited mass/spring/damper calculation with Newmark integration, refinement and slack flag; mechanical closed forms are test oracles, not confirmation of a licensed standard. [src/digitalmodel/motion_forecast/top_tension.py:L1-L18] [src/digitalmodel/motion_forecast/top_tension.py:L67-L182] |

DNV-RP-N103 is named for splash-zone screening in the tension docstring, the crane criterion basis and the derived-quantity plan; none of those occurrences specifies an edition, clause or table. [src/digitalmodel/motion_forecast/top_tension.py:L1-L18] [src/digitalmodel/motion_forecast/criteria_defaults.yml:L1-L16] [docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md:L20-L27]

DNV-ST-N001, DNV-ST-0358 and CAP 437 are also named as criterion bases without editions or clauses; defaults are explicitly indicative screening data. No criterion numbers are reproduced here as licensed-standard requirements. [src/digitalmodel/motion_forecast/criteria_defaults.yml:L1-L34]

UNVERIFIED: U1 — the AQWA-to-canonical phase sign agrees with matched physical data; the implementation itself records this as pending validation. Issue pending (1). [src/digitalmodel/motion_forecast/conventions.py:L17-L22]

UNVERIFIED: U2 — standards compliance, including the edition, clause and independent provenance of screening criteria and the tension method; clause text not checked against the licensed copy. No standards `Citation` sidecar is returned by the criterion loader or tension calculation. Issue pending (1). [src/digitalmodel/motion_forecast/criteria.py:L58-L94] [src/digitalmodel/motion_forecast/top_tension.py:L143-L182]

UNVERIFIED: U3 — operational prediction validity of either horizon model; the bandwidth function calls itself indicative, and the group-velocity docstring supplies only an author/year reference without a publication locator. Issue pending (1); field qualification tracked by #1467. [src/digitalmodel/motion_forecast/wave_forecast.py:L39-L79]

## 4. Validated worked examples

Closed-form examples below validate the indicated algebra or numerical identity only; they do not qualify physical forecast performance or the standards named in section 3. Input fixtures are constructed in their test bodies. [tests/motion_forecast/test_reconstruct.py:L26-L100] [tests/motion_forecast/test_consistency.py:L23-L47] [tests/motion_forecast/test_reconcile.py:L73-L88] [tests/motion_forecast/test_top_tension.py:L45-L49]

| Case | Independent expected value and unit | Tolerance / basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 regular wave | Closed-form heave `1.3*0.8*cos(0.7*t+0.4+0.5)` m | `numpy.allclose`, absolute term 1e-6 m plus default relative term; signed phase guard | `tests/motion_forecast/test_reconstruct.py::test_regular_wave_amplitude_and_phase` | PASS; [tests/motion_forecast/test_reconstruct.py:L26-L32] |
| V2 spatial propagation | Closed-form heave `cos(0.9*t-(0.9**2/GRAVITY)*120)` m | `allclose`, absolute term 1e-6 m plus default relative term | `tests/motion_forecast/test_reconstruct.py::test_spatial_phase_transfer_term` | PASS; [tests/motion_forecast/test_reconstruct.py:L35-L47] |
| V3 finite-depth identity | Residual `GRAVITY*k*tanh(k*h)-omega**2` in rad²/s²; `omega=0.3` rad/s, `h=2` m | Relative dispersion error 1e-6; positive k and deep-water lower bound | `tests/motion_forecast/test_reconstruct.py::test_finite_depth_dispersion_holds_in_shallow_water[2.0-0.3]` | PASS; [tests/motion_forecast/test_reconstruct.py:L73-L82] |
| V4 lever arm | Closed-form `heave-40*deg2rad(pitch)` m | `allclose`, absolute term 1e-9 m plus default relative term | `tests/motion_forecast/test_reconstruct.py::test_vertical_motion_lever_arm` | PASS; [tests/motion_forecast/test_reconstruct.py:L85-L100] |
| V5 derived peaks | Closed-form excursion `1.4*0.8` m and velocity `1.4*0.8*0.7` m/s | Absolute excursion error <1e-4 m; velocity relative error <0.5 %; numerical gradient/sample error | `tests/motion_forecast/test_consistency.py::test_single_component_derived_peaks_match_closed_form` | PASS; [tests/motion_forecast/test_consistency.py:L34-L47] |
| V6 resampled error | Linear signals on different grids: RMSE/bias 0.25 m, correlation unity | Absolute error 1e-9 m and 1e-9 dimensionless; linear interpolation identity | `tests/motion_forecast/test_reconcile.py::test_overlap_error_resamples_across_different_grids` | PASS; [tests/motion_forecast/test_reconcile.py:L73-L88] |
| V7 resonant displacement ratio | Independent algebra `sqrt(1+4*zeta**2)/(2*zeta)` dimensionless at `omega=wn`, `zeta=0.1` | Relative error 1e-9 dimensionless; base-excitation identity | `tests/motion_forecast/test_top_tension.py::test_displacement_transmissibility_at_resonance` | PASS; [tests/motion_forecast/test_top_tension.py:L20-L23] [tests/motion_forecast/test_top_tension.py:L45-L49] |

### Regression examples

| Case | Input / expected behavior | Existing test node ID | Offline result and limit |
| --- | --- | --- | --- |
| R1 hand-authored RAO file | `tests/motion_forecast/fixtures/orcaflex_rao_min.yml`; heave 0.8 m/m and phase 30 deg at the selected node | `tests/motion_forecast/test_rao_file.py::test_from_file_parses_known_node_values` | PASS; schema/adapter regression, not independent solver data. [tests/motion_forecast/test_rao_file.py:L1-L5] [tests/motion_forecast/test_rao_file.py:L22-L33] |
| R2 spectral bookkeeping | Shared synthetic spectrum and transfer, compare component variance with spectral integration | `tests/motion_forecast/test_validation.py::test_analytic_variance_matches_oracle_m0` | PASS; relative tolerance 2.0 %, m²; shares spectrum and transfer on both sides. [tests/motion_forecast/test_validation.py:L5-L10] [tests/motion_forecast/test_validation.py:L47-L64] |
| R3 realized variance | Seeded 1500 s record compared with its component variance | `tests/motion_forecast/test_validation.py::test_realized_record_variance_close_to_analytic` | PASS; relative tolerance 12.0 %, m²; no independent physical truth. [tests/motion_forecast/test_validation.py:L67-L77] |
| R4 horizon formula | Two-component aperture case, expected `2*200*(1-0.5)/GRAVITY` s | `tests/motion_forecast/test_wave_forecast.py::test_dpz_horizon_matches_formula` | PASS; absolute tolerance 1e-9 s; checks selected formula, not physical forecast accuracy. [tests/motion_forecast/test_wave_forecast.py:L51-L55] |
| R5 workflow | Synthetic sea, demo analytic RAO, six aligned DOF arrays and 32 components | `tests/motion_forecast/test_workflow.py::test_router_produces_results` | PASS; serialization/dispatch regression. [tests/motion_forecast/test_workflow.py:L11-L26] |

## 5. QA gates

Apply AQG r0, section 10, Python calculations, with G5 from section 9. These are pack check requirements, not a claim of independent clearance.

| Gate | Applicability | Module-specific checks |
| --- | --- | --- |
| G0 | Applicable: inputs, units and reference provenance | C01–C03; external reference qualification remains U1–U3 |
| G1 | Applicable: Python import and offline execution; no target-program writer is implemented in this package | C04–C06; file route reads an RAO through `GridRAO.from_file`, then wraps the parsed displacement data. [src/digitalmodel/motion_forecast/rao_adapter.py:L85-L100] |
| G2 | Applicable: hand/closed-form and regression checks | C07–C14, C16–C21 |
| G3 | NOT_APPLICABLE: short licensed dynamics is outside this Python calculation gate set | AQG r0 section 10, Python calculations |
| G4 | NOT_APPLICABLE: diffraction mesh qualification is outside this Python calculation gate set | AQG r0 section 10, Python calculations |
| G5 | Applicable: preserve the calculation evidence and qualification boundary | C15; production evidence packaging remains issue pending (5) |

Run a check through its cited test function, with the file path prefixed by `tests/motion_forecast/`; the complete test file inventory is in section 10. Numerical tolerances below reproduce the tests, including their relative terms where applicable.

| ID / gate | Function/output checked and executable evidence | Numeric pass criterion and unit |
| --- | --- | --- |
| C01 / G0 | `load_criteria`; `test_criteria.py` | 0 accepted malformed cases in the five rejection tests; positive ordered thresholds and 3 offset coordinates in the defaults test. [tests/motion_forecast/test_criteria.py:L8-L71] |
| C02 / G0 | `MeasuredMotion`; `test_measured.py` | 0 accepted nonmonotonic clocks, missing-DOF or wrong-shape records; trailing window begins at 140 s and now is 160 s in the fixture. [tests/motion_forecast/test_measured.py:L17-L54] |
| C03 / G0 | `from_csv`; `test_measured_source.py` | Round-trip `allclose` absolute term 1e-6 s/m plus default relative term; single-row length 1 sample; 0 missing-column records accepted. [tests/motion_forecast/test_measured_source.py:L37-L64] |
| C04 / G1 | Public imports and all `tests/motion_forecast/` tests | 123 passed test cases, 0 failures, 0 skips at the recorded code pin; keep collection and outcome counts together. |
| C05 / G1 | `GridRAO.from_file`, `GridRAO.H`; R1 and `test_aqwa_source_normalises_phase_sign` | Heave amplitude absolute error ≤1e-9 m/m, phase absolute error ≤1e-9 deg plus pytest default relative terms; sign-switch expectation is regression only. [tests/motion_forecast/test_rao_file.py:L26-L40] |
| C06 / G1 | `router`; `test_workflow.py` | 6 DOF arrays, 32 long-crested components; 120 directional components in the directional fixture; horizon between 15 and 120 s there; invalid forecaster raises. [tests/motion_forecast/test_workflow.py:L11-L26] [tests/motion_forecast/test_workflow.py:L136-L167] |
| C07 / G2 | `reconstruct_motion`; V1 and V2 | `allclose` absolute term 1e-6 m plus default relative term on both signed histories. [tests/motion_forecast/test_reconstruct.py:L26-L47] |
| C08 / G2 | `wavenumber`; all finite-depth parameter cases | 16 cases: positive k in rad/m; dispersion relative error ≤1e-6 dimensionless; deep-water lower bound to 1e-12 rad/m. [tests/motion_forecast/test_reconstruct.py:L73-L82] |
| C09 / G2 | `vertical_motion_at`, governing magnitudes; V4, V5 and `test_derived.py` | Lever-arm `allclose` absolute term 1e-9 m; excursion error <1e-4 m; velocity relative error <0.5 %; linear velocity 0.5 m/s with absolute term 1e-9 m/s. [tests/motion_forecast/test_reconstruct.py:L85-L100] [tests/motion_forecast/test_consistency.py:L34-L47] [tests/motion_forecast/test_derived.py:L27-L63] |
| C10 / G2 | `displacement_transmissibility`; V7 | Relative error ≤1e-9 dimensionless against direct resonant algebra. [tests/motion_forecast/test_top_tension.py:L45-L49] |
| C11 / G2 | `overlap_error`, `seam_offset`; V6 and `test_reconcile.py` | RMSE/bias absolute error ≤1e-9 m, correlation error ≤1e-9 dimensionless; 0 out-of-range/no-overlap/short-overlap records accepted by the rejection fixtures. [tests/motion_forecast/test_reconcile.py:L49-L97] [tests/motion_forecast/test_reconcile.py:L113-L124] |
| C12 / G2 | `_newmark`, `dynamic_tension`; `test_top_tension.py` | Off-resonance displacement and force relative errors ≤0.5 % (m and N); resonant displacement ≤1.0 %; slow-motion amplification absolute term 0.005 dimensionless; slack case minimum ≤0 N and validity false. Oracles here reuse production transmissibility functions. [tests/motion_forecast/test_top_tension.py:L52-L108] |
| C13 / G2 | `synthesize_forecast`, spectral bookkeeping and realized variance; `test_validation.py`, R2/R3 | Recovered height relative error ≤1.0 % m; bookkeeping variance ≤2.0 % m²; realized variance ≤12.0 % m². [tests/motion_forecast/test_validation.py:L39-L77] |
| C14 / G2 | `coherence_horizon`, `dpz_horizon`, directional weights and `surface_elevation`; `test_wave_forecast.py` | Monochromatic horizon 120 s; 0 zero-aperture records accepted; R4 absolute error 1e-9 s; weights sum to 1 dimensionless; directional height relative error ≤2.0 % m and mean heading absolute error ≤1e-6 deg plus default relative terms; single-component surface absolute term 1e-9 m. [tests/motion_forecast/test_wave_forecast.py:L25-L104] |
| C15 / G5 | Pack evidence audit: resolve citations; match every section-4 node against the passing test log; compare file digests and before/after counts | 0 unresolved citations, 0 missing example outcomes, 0 changed source-file digests, 0 changed module outcomes, 0 missing G0–G5 rows; count units. Production input fingerprints and generated appendix are follow-up, not supplied by this check. |
| C16 / G2 | `aggregate_skill`, `error_vs_lead_time`; `test_skill.py` | Pool 100 samples, RMSE `sqrt((10+90*9)/100)` m with relative error 1e-9; one dropout leaves 80 samples and RMSE 1 m; 0 empty batches accepted. [tests/motion_forecast/test_skill.py:L35-L73] |
| C17 / G2 | `fit_correction`, `holdout_report`; `test_recalibrate.py` | Injected gain 2 dimensionless and bias 0.5 m recovered to absolute term 1e-9; exact held-out RMSE absolute term 1e-9 m; noise-floor error relative term 30.0 % and correlation change absolute term 1e-9 dimensionless; constant sway uses gain 1 and bias 0.3 m. [tests/motion_forecast/test_recalibrate.py:L33-L79] |
| C18 / G2 | `rolling_decision`, `measured_status`; `test_decision.py`, `test_reconcile.py` | Linear crossing-time errors <0.02 s; already-breached lead time 0 s; 0 GO outcomes for tested NaN and downward-velocity breaches; equality remains below strict breach. [tests/motion_forecast/test_decision.py:L28-L96] [tests/motion_forecast/test_reconcile.py:L105-L139] |
| C19 / G2 | `SyntheticMMS`; `test_measured_source.py` | 0 differences for repeated seeded noisy arrays; noiseless `allclose` against truth; 0 identical-to-truth noisy fixtures. [tests/motion_forecast/test_measured_source.py:L21-L34] |
| C20 / G2 | Phase/heading normalization; `test_conventions.py` | AQWA +90 deg becomes -90 deg; 30 deg coming-from becomes 210 deg going-to; normalized synthetic transfer equality and unit amplitude under NumPy defaults. [tests/motion_forecast/test_conventions.py:L18-L60] |
| C21 / G2 | `operability_summary`; `test_feedback.py` | 6 samples, GO-only fraction 4/6, longest run 2 samples; including marginal fraction 5/6, longest run 3 samples; 0 empty sequences accepted. [tests/motion_forecast/test_feedback.py:L11-L29] |

## 6. Known failure modes and pitfalls

sweep: 57 found

The following 57 rows inventory fallbacks, omission paths, external conversions, unit assumptions and review-relevant defaults; a row describes the cited behavior rather than declaring every default a defect. The only `except` in the package rethrows with context, and its only `continue` skips an exactly zero transfer. [src/digitalmodel/motion_forecast/criteria.py:L64-L79] [src/digitalmodel/motion_forecast/reconstruct.py:L131-L136]

Exact package-wide text sweep, followed by inspection of every Python file, all parameter defaults, `numpy.asarray`/`astype` ingest and the criteria YAML:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|astype|errstate|isfinite|np\.interp|\.get\(' src/digitalmodel/motion_forecast --glob '*.py'
rg -n 'unit|degrees|radian|metre|meter|seconds|default|cache|write|open\(' src/digitalmodel/motion_forecast
```

| ID | Symptom / cause at the code pin | Detection / disposition |
| --- | --- | --- |
| S01 | Empty finite skill pool reports zero RMSE. [src/digitalmodel/motion_forecast/skill.py:L109-L113] | no detection check exists; issue pending (3) |
| S02 | Empty finite skill pool reports zero bias. [src/digitalmodel/motion_forecast/skill.py:L109-L113] | no detection check exists; issue pending (3) |
| S03 | Nonfinite residual/lead samples are silently removed before pooling. [src/digitalmodel/motion_forecast/skill.py:L49-L62] | C16 covers one dropout; all-dropout detection absent; issue pending (3) |
| S04 | Empty record pool returns two empty arrays. [src/digitalmodel/motion_forecast/skill.py:L49-L58] | no detection check exists for direct empty pool; issue pending (3) |
| S05 | No finite leads returns an empty error-curve mapping. [src/digitalmodel/motion_forecast/skill.py:L72-L74] | no detection check exists; issue pending (3) |
| S06 | Empty lead bins are omitted from the curve. [src/digitalmodel/motion_forecast/skill.py:L75-L82] | no detection check exists for missing bins; issue pending (3) |
| S07 | Missing correlations are excluded, with absent median/IQR represented as `None`. [src/digitalmodel/motion_forecast/skill.py:L101-L113] | no detection check exists for correlation distribution completeness; issue pending (3) |
| S08 | Calibration removes nonfinite forecast/measured pairs without a minimum retained count. [src/digitalmodel/motion_forecast/recalibrate.py:L45-L55] | no detection check exists; issue pending (3) |
| S09 | Almost constant forecast falls back to unit gain and bias-only fit. [src/digitalmodel/motion_forecast/recalibrate.py:L58-L78] | C17 constant-sway regression; intentional guard |
| S10 | Undefined calibration correlation returns `None`. [src/digitalmodel/motion_forecast/recalibrate.py:L81-L84] | no detection check exists for direct constant-array `_corr`; issue pending (3) |
| S11 | Holdout defaults to half the records, rounds/clamps the split and does not validate the fraction range. [src/digitalmodel/motion_forecast/recalibrate.py:L95-L107] | C17 exercises default split; no invalid-fraction detection; issue pending (3) |
| S12 | Zero-variance overlap correlation becomes `None`. [src/digitalmodel/motion_forecast/reconcile.py:L99-L109] | C11 constant-signal regression; intentional undefined statistic |
| S13 | Nonfinite or nonpositive depth silently selects deep-water dispersion. [src/digitalmodel/motion_forecast/reconstruct.py:L38-L40] | no detection check exists; issue pending (2) |
| S14 | Nonpositive angular frequency returns zero wave number. [src/digitalmodel/motion_forecast/reconstruct.py:L36-L37] | no detection check exists for invalid frequency; issue pending (2) |
| S15 | Exactly zero RAO magnitude is skipped in superposition. [src/digitalmodel/motion_forecast/reconstruct.py:L122-L136] | C07 uses absent analytic DOFs; intentional zero contribution |
| S16 | Missing analytic DOF silently means zero transfer. [src/digitalmodel/motion_forecast/rao_adapter.py:L103-L115] | C07 exercises sparse analytic RAO; no unknown-DOF rejection; issue pending (2) |
| S17 | Frequency interpolation outside the grid uses endpoint values. [src/digitalmodel/motion_forecast/rao_adapter.py:L27-L42] | no detection check exists for extrapolation bounds; issue pending (2) |
| S18 | Heading is clipped to endpoints without periodic wrap. [src/digitalmodel/motion_forecast/rao_adapter.py:L44-L50] | no detection check exists for wrap/bounds; issue pending (2) |
| S19 | Repeated heading bracket uses zero interpolation weight. [src/digitalmodel/motion_forecast/rao_adapter.py:L51-L55] | no detection check exists for duplicate axes; issue pending (2) |
| S20 | Invalid/nonpositive-frequency spectral bins become zero after suppressed floating-point warnings. [src/digitalmodel/motion_forecast/wave_source.py:L29-L40] | no detection check exists for invalid spectral input; issue pending (2) |
| S21 | Nonpositive integrated spectrum bypasses normalization. [src/digitalmodel/motion_forecast/wave_source.py:L41-L46] | C13 checks physical inputs; no degenerate-input detection; issue pending (2) |
| S22 | Nonpositive moments return maximum coherence horizon. [src/digitalmodel/motion_forecast/wave_forecast.py:L54-L56] | no detection check exists for empty/zero-energy coherence; issue pending (2) |
| S23 | Near-zero spectral width returns maximum coherence horizon. [src/digitalmodel/motion_forecast/wave_forecast.py:L57-L59] | C14 monochromatic regression; proxy limit, U3 |
| S24 | Coherence horizon defaults/clamps to configured rails with a default scale factor. [src/digitalmodel/motion_forecast/wave_forecast.py:L39-L64] | C14 clamp test; external qualification issue pending (1) |
| S25 | DPZ selects the highest-energy band until the default fraction is reached; fraction is not range-validated. [src/digitalmodel/motion_forecast/wave_forecast.py:L67-L93] | C14 checks default two-component algebra; no invalid-fraction detection; issue pending (2) |
| S26 | Zero or negative direction count selects one mean heading. [src/digitalmodel/motion_forecast/wave_forecast.py:L134-L139] | C14 tests count one; no negative-count detection; issue pending (2) |
| S27 | Unknown RAO source takes the analytic path. [src/digitalmodel/motion_forecast/workflow.py:L62-L72] | no detection check exists; issue pending (2) |
| S28 | Unknown file-format value selects the OrcaFlex phase sign. [src/digitalmodel/motion_forecast/workflow.py:L64-L67] | no detection check exists; issue pending (2) |
| S29 | Missing source/format/preset selects analytic/default-format/demo preset. [src/digitalmodel/motion_forecast/workflow.py:L62-L72] | C06 default workflow; provenance qualification issue pending (4) |
| S30 | Empty sea mapping selects a default synthetic sea and forecaster. [src/digitalmodel/motion_forecast/workflow.py:L75-L94] [src/digitalmodel/motion_forecast/workflow.py:L114-L120] | C06 partial defaults; no empty-config qualification check; issue pending (4) |
| S31 | Synthetic route converts height, period, gamma, heading and horizon using `float`; component count and seed use `int`. [src/digitalmodel/motion_forecast/workflow.py:L85-L94] | no detection check exists for locale-formatted or nonfinite input, fractional count truncation; issue pending (2) |
| S32 | Directional route converts sea, spread, count, aperture and seed fields; mean heading/count have nested fallbacks. [src/digitalmodel/motion_forecast/workflow.py:L98-L108] | no detection check exists for locale/nonfinite/count coercion; issue pending (2) |
| S33 | Sea builders omit depth, phase-reference location and origin time from their calls. [src/digitalmodel/motion_forecast/workflow.py:L85-L108] | no detection check exists; issue pending (2) |
| S34 | Asset location defaults to zero and timestep to a numeric default coerced by `float`. [src/digitalmodel/motion_forecast/workflow.py:L121-L124] | C06 physical fixture; no invalid-timestep/unit detection; issue pending (2) |
| S35 | Falsy operation omits both decision modes. [src/digitalmodel/motion_forecast/workflow.py:L136-L150] [src/digitalmodel/motion_forecast/workflow.py:L178-L185] | C06 without-operation regression; output provenance issue pending (4) |
| S36 | Falsy measured mapping omits measured ingest entirely. [src/digitalmodel/motion_forecast/workflow.py:L164-L177] | no detection check exists for empty measured spec; issue pending (4) |
| S37 | Measured now outside the new forecast silently omits reconciliation; inside it, default synthetic sea can still be compared. [src/digitalmodel/motion_forecast/workflow.py:L114-L124] [src/digitalmodel/motion_forecast/workflow.py:L186-L189] | no detection check exists for scenario identity or omitted reconciliation; issue pending (4) |
| S38 | Falsy/empty skill records omit summary; optional result keys are not cleared when configuration is reused. [src/digitalmodel/motion_forecast/workflow.py:L134-L204] | no detection check exists for repeated-call stale output; issue pending (4) |
| S39 | Criterion numeric text converts via `float` without locale or finite/unit compatibility checks. [src/digitalmodel/motion_forecast/criteria.py:L64-L78] | C01 malformed cases only; no locale/unit/finite detection; issue pending (2) |
| S40 | Criterion alpha defaults to unity; basis defaults to empty text and neither is qualified by `_validate`. [src/digitalmodel/motion_forecast/criteria.py:L44-L55] [src/digitalmodel/motion_forecast/criteria.py:L69-L78] | C01 alpha default; provenance issue pending (1) |
| S41 | Criterion parse exception is rethrown with key context, not swallowed. [src/digitalmodel/motion_forecast/criteria.py:L76-L79] | C01 missing-key rejection; no silent failure |
| S42 | Measured CSV assumes comma delimiter, numeric dot-decimal values, absolute seconds, translations m and rotations deg; unused columns ignored. [src/digitalmodel/motion_forecast/measured_source.py:L57-L72] [src/digitalmodel/motion_forecast/measured.py:L29-L34] | C03 normal CSV; no locale/missing-cell unit qualification; issue pending (2) |
| S43 | Synthetic sensor defaults to no noise; nonpositive noise also adds zero, with float/int coercion of noise/seed. [src/digitalmodel/motion_forecast/measured_source.py:L32-L54] | C19 noiseless/seeded tests; no negative/nonfinite-noise check; issue pending (2) |
| S44 | Component heading defaults to zero; phase/frequency/amplitude units rely on caller. [src/digitalmodel/motion_forecast/models.py:L30-L50] | C07/C20 verify convention examples; no caller-unit validation; issue pending (2) |
| S45 | Forecast origin/reference default to zero and depth to absent; no model validator qualifies clock or units. [src/digitalmodel/motion_forecast/models.py:L53-L65] | C07/C08 exercise explicit assumptions; no nonfinite model detection; issue pending (2) |
| S46 | Both RAO `H` methods default heading to zero; arrays are interpreted as rad/s, deg, m/m or deg/m. [src/digitalmodel/motion_forecast/rao_adapter.py:L61-L83] [src/digitalmodel/motion_forecast/rao_adapter.py:L99-L115] | C05 physical node; input-unit qualification issue pending (2) |
| S47 | Zero reconstruction asset offset and default timestep select generated grid; count rounded and floored to at least two samples. [src/digitalmodel/motion_forecast/reconstruct.py:L68-L75] [src/digitalmodel/motion_forecast/reconstruct.py:L102-L116] | C07 horizon rejection; no negative/nonfinite dt/horizon check; issue pending (2) |
| S48 | Governing offset defaults to zero; lever arm assumes small-angle radian conversion and ignores vertical offset. [src/digitalmodel/motion_forecast/derived.py:L75-L85] [src/digitalmodel/motion_forecast/reconstruct.py:L149-L165] | C09 checks origin/lever arm; geometry qualification issue pending (2) |
| S49 | Both sea generators default heading/reference/origin to zero, depth to absent and phase seed to fixed; frequency grids assume angular-frequency spectra. [src/digitalmodel/motion_forecast/wave_source.py:L49-L102] [src/digitalmodel/motion_forecast/wave_forecast.py:L111-L156] | C13/C14 energy and seed fixtures; no complete input-domain detection; issue pending (2) |
| S50 | Forecast current is first sample and measured current is last; accidental measured input to rolling decision uses the wrong now. [src/digitalmodel/motion_forecast/measured.py:L8-L15] [src/digitalmodel/motion_forecast/decision.py:L76-L85] | C02/C18 verify intended entry points; no wrong-type rejection; issue pending (2) |
| S51 | Zero crossing lead denotes an already-breached or nonfinite forecast; `None` denotes no future breach. [src/digitalmodel/motion_forecast/decision.py:L76-L105] | C18 zero/None/NaN cases; fail-closed behavior |
| S52 | Measured numeric arrays permit nonfinite motion samples while enforcing clock order and shape. [src/digitalmodel/motion_forecast/measured.py:L40-L52] | C18 latest-NaN rejects GO; downstream missing-data behavior issue pending (3) |
| S53 | Tension units are kg, N and m without automatic conversion; top offset defaults to zero and the integrator starts in quasi-equilibrium with zero-initialized arrays. [src/digitalmodel/motion_forecast/top_tension.py:L30-L65] [src/digitalmodel/motion_forecast/top_tension.py:L114-L125] [src/digitalmodel/motion_forecast/top_tension.py:L143-L170] | C10/C12 unit-consistent fixtures; no nonfinite parameter/interior-time-order detection; issue pending (2) |
| S54 | Demo RAO embeds fixed periods, damping, gains and deep-water rolloff; it is explicitly not a real asset model. [src/digitalmodel/motion_forecast/workflow.py:L30-L59] | C06 checks workflow, not asset qualification; issue pending (1) |
| S55 | Marginal decisions default to inoperable; operable-run accumulators start at zero and measure sample counts. [src/digitalmodel/motion_forecast/feedback.py:L26-L56] | C21 exact fractions/run lengths; duration weighting is not implemented |
| S56 | Overlap interpolation assumes shared absolute seconds, allows only a bounded time query and defaults to at least three overlap samples. [src/digitalmodel/motion_forecast/reconcile.py:L27-L41] [src/digitalmodel/motion_forecast/reconcile.py:L61-L88] | C11 tests bounds and differing grids; scenario provenance issue pending (4) |
| S57 | No empty-result cache write occurs: the package reads criterion text/CSV and mutates in-memory outputs rather than writing a cache. [src/digitalmodel/motion_forecast/criteria.py:L83-L94] [src/digitalmodel/motion_forecast/measured_source.py:L57-L72] [src/digitalmodel/motion_forecast/workflow.py:L112-L204] | C15 source audit; no cache path to execute |

### Incident and reference leads

| Lead | Confirmed scope / detection |
| --- | --- |
| #1358 | Historical shallow-water fixed-point iteration failure is described in the existing test; current bisection is guarded by C08. The open feature issue is not evidence that current tests fail. [tests/motion_forecast/test_reconstruct.py:L73-L82] [src/digitalmodel/motion_forecast/reconstruct.py:L25-L65] |
| #1359 | Kinematic top-connection velocity remains a governing quantity, distinct from the lumped tension output; C09 and C12 check the separate paths. [src/digitalmodel/motion_forecast/derived.py:L61-L68] [src/digitalmodel/motion_forecast/top_tension.py:L143-L182] |
| #1360 / #1367 | Affine correction cannot establish phase improvement; measured and forecast now differ. C17/C18 exercise intended semantics; missing-data and scenario-identity gaps are S01–S12 and S37. [src/digitalmodel/motion_forecast/recalibrate.py:L1-L13] [src/digitalmodel/motion_forecast/measured.py:L8-L15] |
| #2107 | Map flags the RAO file wrapper as not assessed for an external parser-grid defect; this wrapper delegates to the marine-analysis reader, so no direct attribution to this package is established. C05 covers a small synthetic file only; no detection check exists for full external-grid truncation through this route. [src/digitalmodel/motion_forecast/rao_adapter.py:L91-L97] |
| #2543 | The map's spectrum-normalization lead is not a confirmed defect in this package: `jonswap_spectrum` explicitly renormalizes by multiplication. C13 tests its energy; no claim about other packages follows. [src/digitalmodel/motion_forecast/wave_source.py:L40-L46] [tests/motion_forecast/test_validation.py:L39-L64] |

## 7. Lessons log

Append future lessons without rewriting earlier entries; use a neutral evidence identifier rather than an execution identifier.

| Date | Evidence identifier | Lesson | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Phase-versus-energy checks | Spectral bookkeeping shares the source spectrum and transfer and cannot detect signed-phase errors; retain regular-wave signed checks. [tests/motion_forecast/test_validation.py:L5-L14] [tests/motion_forecast/test_reconstruct.py:L1-L5] | Sections 4–6 distinguish algebra, regression and physical qualification | Not assigned; issue pending (5) |
| 2026-10-18 | Missing-observation audit | Empty retained skill samples can yield zero errors; sample count must accompany skill. [src/digitalmodel/motion_forecast/skill.py:L100-L113] | S01–S07 and follow-up | Not assigned; issue pending (3) |

## 8. Ownership

Review roles for this pack: engineering calculations family quality manager, with an engineering calculations deputy quality manager. The maintainer assigns the deputy and the independent reviewer; assignment and shared-log registration remain issue pending (5).

The producing responsibility is calculation documentation and evidence collection. Ambiguous conventions, reference editions, criteria or failed checks go to the family quality manager; cross-family conflicts go to the maintainers. These are review requirements, not a record of completed review. Status remains DRAFT pending an independent provider's clearance.

## 9. Dependencies and implementation reconciliation

### Runtime and consumer boundaries

| Dependency / consumer | Route and evidence |
| --- | --- |
| NumPy | Dispersion, interpolation, superposition and numerical differentiation use array operations. [src/digitalmodel/motion_forecast/reconstruct.py:L25-L65] [src/digitalmodel/motion_forecast/reconstruct.py:L118-L170] |
| YAML / packaged criteria | `load_criteria` reads bundled or supplied YAML, then validates criterion records. [src/digitalmodel/motion_forecast/criteria.py:L58-L94] |
| Marine-analysis RAO reader | `GridRAO.from_file` delegates to `read_rao_file`, then requires displacement data. [src/digitalmodel/motion_forecast/rao_adapter.py:L85-L97] |
| Decision spine / installation taxonomy | Classification/crossing is re-exported from the shared spine, while result and state types come from the installation package. [src/digitalmodel/motion_forecast/decision.py:L17-L35] |
| Spectral oracle in tests | `compute_response_spectrum` and `spectral_moments` are used for regression bookkeeping, not signed phase validation. [tests/motion_forecast/test_validation.py:L5-L14] [tests/motion_forecast/test_validation.py:L47-L64] |
| ROV operability consumer | Imports motion reconstruction and evaluates vertical velocity at the deployment offset. [src/digitalmodel/subsea/rov/seastate_limits.py:L251-L317] |
| Capability-page implementation | Separate JavaScript spectrum, oscillator, motion, governing-velocity and verdict computations exist outside the package; they are map-follow-up findings, not counted-package misses. [docs/api/hydro/short-horizon-motion-forecast.html:L276-L340] [docs/api/hydro/short-horizon-motion-forecast.html:L424-L444] |

Every source occurrence naming the two RAO solver formats is inventoried below as a textual format/convention dependency; no installed solver version is established by these references. [src/digitalmodel/motion_forecast/conventions.py:L1-L22] [src/digitalmodel/motion_forecast/workflow.py:L8-L14]

| Source occurrence | Dependency meaning |
| --- | --- |
| `conventions.py` lines 4–5 | AQWA/OrcaFlex parser names and convention comparison. [src/digitalmodel/motion_forecast/conventions.py:L3-L7] |
| `conventions.py` line 10 | Direction of source normalization. [src/digitalmodel/motion_forecast/conventions.py:L9-L10] |
| `conventions.py` lines 17–18 | Canonical lead versus assumed lag. [src/digitalmodel/motion_forecast/conventions.py:L17-L18] |
| `conventions.py` line 20 | AQWA sign explicitly awaits matched-data validation. [src/digitalmodel/motion_forecast/conventions.py:L20-L22] |
| `conventions.py` lines 35–36 | Source enum values. [src/digitalmodel/motion_forecast/conventions.py:L32-L37] |
| `conventions.py` lines 43–44 | Source phase-sign table. [src/digitalmodel/motion_forecast/conventions.py:L40-L46] |
| `workflow.py` line 12 | Configuration documents two source formats. [src/digitalmodel/motion_forecast/workflow.py:L8-L14] |
| `workflow.py` lines 65–66 | Default file format and source selection. [src/digitalmodel/motion_forecast/workflow.py:L62-L67] |

### Map pins, inventory rule and search

Implementation-map ref: `origin/codex/implmap-work`, snapshot `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Only citations to `docs/registry/implementation-map.yaml` resolve at that snapshot, rather than the code pin. [docs/registry/implementation-map.yaml:L1-L7]

Each inventory row below identifies one implementation by its ordinal within that family's snapshot list and a direct map-record citation. Outside-package names are not transcribed; the ordinal and source locator retain exact identity without reproducing identifying discovery names. These are map records, not endorsements of independent validation or of the map's relationship classification.

| Counted order | Map family | In-package rows | Complete family inventory rows |
| --- | --- | --- | --- |
| 1 | `wave-spectra` | 3 | 141 |
| 2 | `linear-wave-kinematics` | 2 | 21 |
| 3 | `rao-readers-and-orcaflex-converters` | 1 | 295 |

Counts are obtained by selecting map implementation paths below the package header path; all three families are counted. Ties, if present in a later revision, preserve map order. Full inventories and per-definition signal hits follow.

Search procedure: use each counted family's `search_signals[*].pattern` unchanged with Python `re.search`, case-sensitive, against every line of each package `.py` at the code pin. Remove a signal only when it matches `motion_forecast` under `re.IGNORECASE`; none of these signals is removed. Assign a line hit to its narrowest enclosing AST function/class, with all remaining hits assigned to one module-level definition per file. Do not search tests as package implementations.

```python
# Read the exact snapshots named above through git show; no solver import.
signals = [s['pattern'] for s in family['search_signals']
           if not re.search(s['pattern'], 'motion_forecast', re.IGNORECASE)]
for line_number, line in enumerate(source.splitlines(), 1):
    if any(re.search(pattern, line) for pattern in signals):
        # assign to the narrowest enclosing AST FunctionDef / ClassDef
        record_hit(line_number)
```

| Family | Matched enclosing definitions | Matched lines | Further unlisted implementations after sections 3 and 9 |
| --- | --- | --- | --- |
| RAO readers/converters | 17 | 40 | 0 |
| Wave spectra | 10 | 18 | 0 |
| Linear wave kinematics | 31 | 99 | 0 |

The direct deep-water evaluations in the demo response are explicitly inventoried in section 3; the spectrum branch delegates to the listed JONSWAP function, and other hits are data, documentation, helpers or separately described methods. The detailed hit classifications below support a summed further-implementation count of zero.

Whole-repository discovery also includes scripts, using `rg -n -i 'rao|jonswap|wavenumber|dispersion|newmark|motion_forecast' scripts --glob '*.py' --files-with-matches`, and the map's complete inventories include implementations outside the package. Such discoveries are map follow-up, not changes to the counted-package result.

### Map follow-up

Add or reconcile method families for complex interpolation/normalization, phased motion superposition, coherence and aperture horizons, rigid-body governing transforms, decision crossing, measured-time reconciliation, pooled skill, affine correction, operability fractions and lumped Newmark tension. Include the demo's own deep-water expressions and the independent capability-page implementations. Issue pending (6); reference definitions and boundaries are in section 3 and the dependency table above.

### Inventory: `rao-readers-and-orcaflex-converters`

| Snapshot row | Package membership | Record at map pin |
| --- | --- | --- |
| 1 | Outside package | [docs/registry/implementation-map.yaml:L9516-L9526] |
| 2 | Outside package | [docs/registry/implementation-map.yaml:L9528-L9538] |
| 3 | Outside package | [docs/registry/implementation-map.yaml:L9540-L9550] |
| 4 | Outside package | [docs/registry/implementation-map.yaml:L9552-L9562] |
| 5 | Outside package | [docs/registry/implementation-map.yaml:L9564-L9574] |
| 6 | Outside package | [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | Outside package | [docs/registry/implementation-map.yaml:L9590-L9600] |
| 8 | Outside package | [docs/registry/implementation-map.yaml:L9602-L9612] |
| 9 | Outside package | [docs/registry/implementation-map.yaml:L9614-L9624] |
| 10 | Outside package | [docs/registry/implementation-map.yaml:L9626-L9636] |
| 11 | Outside package | [docs/registry/implementation-map.yaml:L9638-L9648] |
| 12 | Outside package | [docs/registry/implementation-map.yaml:L9653-L9663] |
| 13 | Outside package | [docs/registry/implementation-map.yaml:L9665-L9675] |
| 14 | Outside package | [docs/registry/implementation-map.yaml:L9679-L9689] |
| 15 | Outside package | [docs/registry/implementation-map.yaml:L9691-L9701] |
| 16 | Outside package | [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | Outside package | [docs/registry/implementation-map.yaml:L9716-L9726] |
| 18 | Outside package | [docs/registry/implementation-map.yaml:L9728-L9738] |
| 19 | Outside package | [docs/registry/implementation-map.yaml:L9740-L9750] |
| 20 | Outside package | [docs/registry/implementation-map.yaml:L9754-L9764] |
| 21 | Outside package | [docs/registry/implementation-map.yaml:L9766-L9776] |
| 22 | Outside package | [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | Outside package | [docs/registry/implementation-map.yaml:L9789-L9799] |
| 24 | Outside package | [docs/registry/implementation-map.yaml:L9801-L9811] |
| 25 | Outside package | [docs/registry/implementation-map.yaml:L9813-L9823] |
| 26 | Outside package | [docs/registry/implementation-map.yaml:L9827-L9837] |
| 27 | Outside package | [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | Outside package | [docs/registry/implementation-map.yaml:L9850-L9860] |
| 29 | Outside package | [docs/registry/implementation-map.yaml:L9862-L9872] |
| 30 | Outside package | [docs/registry/implementation-map.yaml:L9874-L9884] |
| 31 | Outside package | [docs/registry/implementation-map.yaml:L9886-L9896] |
| 32 | Outside package | [docs/registry/implementation-map.yaml:L9898-L9908] |
| 33 | Outside package | [docs/registry/implementation-map.yaml:L9910-L9920] |
| 34 | Outside package | [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | Outside package | [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | Outside package | [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | Outside package | [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | Outside package | [docs/registry/implementation-map.yaml:L9966-L9976] |
| 39 | Outside package | [docs/registry/implementation-map.yaml:L9978-L9988] |
| 40 | Outside package | [docs/registry/implementation-map.yaml:L9990-L10000] |
| 41 | Outside package | [docs/registry/implementation-map.yaml:L10005-L10015] |
| 42 | Outside package | [docs/registry/implementation-map.yaml:L10018-L10028] |
| 43 | Outside package | [docs/registry/implementation-map.yaml:L10033-L10043] |
| 44 | Outside package | [docs/registry/implementation-map.yaml:L10047-L10057] |
| 45 | Outside package | [docs/registry/implementation-map.yaml:L10061-L10071] |
| 46 | Outside package | [docs/registry/implementation-map.yaml:L10073-L10083] |
| 47 | Outside package | [docs/registry/implementation-map.yaml:L10088-L10098] |
| 48 | Outside package | [docs/registry/implementation-map.yaml:L10100-L10110] |
| 49 | Outside package | [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | Outside package | [docs/registry/implementation-map.yaml:L10123-L10133] |
| 51 | Outside package | [docs/registry/implementation-map.yaml:L10135-L10145] |
| 52 | Outside package | [docs/registry/implementation-map.yaml:L10147-L10157] |
| 53 | Outside package | [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | Outside package | [docs/registry/implementation-map.yaml:L10170-L10180] |
| 55 | Outside package | [docs/registry/implementation-map.yaml:L10182-L10192] |
| 56 | Outside package | [docs/registry/implementation-map.yaml:L10194-L10204] |
| 57 | Outside package | [docs/registry/implementation-map.yaml:L10208-L10218] |
| 58 | Outside package | [docs/registry/implementation-map.yaml:L10223-L10233] |
| 59 | Outside package | [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | Outside package | [docs/registry/implementation-map.yaml:L10247-L10257] |
| 61 | Outside package | [docs/registry/implementation-map.yaml:L10261-L10271] |
| 62 | Outside package | [docs/registry/implementation-map.yaml:L10273-L10283] |
| 63 | Outside package | [docs/registry/implementation-map.yaml:L10285-L10295] |
| 64 | Outside package | [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | Outside package | [docs/registry/implementation-map.yaml:L10308-L10318] |
| 66 | Outside package | [docs/registry/implementation-map.yaml:L10320-L10330] |
| 67 | Outside package | [docs/registry/implementation-map.yaml:L10334-L10344] |
| 68 | Outside package | [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | Outside package | [docs/registry/implementation-map.yaml:L10357-L10367] |
| 70 | Outside package | [docs/registry/implementation-map.yaml:L10369-L10379] |
| 71 | Outside package | [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | Outside package | [docs/registry/implementation-map.yaml:L10392-L10402] |
| 73 | Outside package | [docs/registry/implementation-map.yaml:L10404-L10414] |
| 74 | Outside package | [docs/registry/implementation-map.yaml:L10416-L10426] |
| 75 | Outside package | [docs/registry/implementation-map.yaml:L10428-L10438] |
| 76 | Outside package | [docs/registry/implementation-map.yaml:L10440-L10450] |
| 77 | Outside package | [docs/registry/implementation-map.yaml:L10452-L10462] |
| 78 | Outside package | [docs/registry/implementation-map.yaml:L10466-L10476] |
| 79 | Outside package | [docs/registry/implementation-map.yaml:L10478-L10488] |
| 80 | Outside package | [docs/registry/implementation-map.yaml:L10490-L10500] |
| 81 | Outside package | [docs/registry/implementation-map.yaml:L10502-L10512] |
| 82 | Outside package | [docs/registry/implementation-map.yaml:L10514-L10524] |
| 83 | Outside package | [docs/registry/implementation-map.yaml:L10528-L10538] |
| 84 | Outside package | [docs/registry/implementation-map.yaml:L10540-L10550] |
| 85 | Outside package | [docs/registry/implementation-map.yaml:L10552-L10562] |
| 86 | Outside package | [docs/registry/implementation-map.yaml:L10564-L10574] |
| 87 | Outside package | [docs/registry/implementation-map.yaml:L10576-L10586] |
| 88 | Outside package | [docs/registry/implementation-map.yaml:L10588-L10598] |
| 89 | Outside package | [docs/registry/implementation-map.yaml:L10600-L10610] |
| 90 | Outside package | [docs/registry/implementation-map.yaml:L10612-L10622] |
| 91 | Outside package | [docs/registry/implementation-map.yaml:L10624-L10634] |
| 92 | Outside package | [docs/registry/implementation-map.yaml:L10636-L10646] |
| 93 | Outside package | [docs/registry/implementation-map.yaml:L10648-L10658] |
| 94 | Outside package | [docs/registry/implementation-map.yaml:L10660-L10670] |
| 95 | Outside package | [docs/registry/implementation-map.yaml:L10672-L10682] |
| 96 | Outside package | [docs/registry/implementation-map.yaml:L10684-L10694] |
| 97 | Outside package | [docs/registry/implementation-map.yaml:L10700-L10710] |
| 98 | Outside package | [docs/registry/implementation-map.yaml:L10712-L10722] |
| 99 | Outside package | [docs/registry/implementation-map.yaml:L10726-L10736] |
| 100 | Outside package | [docs/registry/implementation-map.yaml:L10738-L10748] |
| 101 | Outside package | [docs/registry/implementation-map.yaml:L10750-L10760] |
| 102 | Outside package | [docs/registry/implementation-map.yaml:L10762-L10772] |
| 103 | Outside package | [docs/registry/implementation-map.yaml:L10776-L10786] |
| 104 | Outside package | [docs/registry/implementation-map.yaml:L10790-L10800] |
| 105 | Outside package | [docs/registry/implementation-map.yaml:L10802-L10812] |
| 106 | Outside package | [docs/registry/implementation-map.yaml:L10816-L10826] |
| 107 | Outside package | [docs/registry/implementation-map.yaml:L10829-L10839] |
| 108 | Outside package | [docs/registry/implementation-map.yaml:L10842-L10852] |
| 109 | Outside package | [docs/registry/implementation-map.yaml:L10854-L10864] |
| 110 | Outside package | [docs/registry/implementation-map.yaml:L10866-L10876] |
| 111 | Outside package | [docs/registry/implementation-map.yaml:L10878-L10888] |
| 112 | Outside package | [docs/registry/implementation-map.yaml:L10892-L10902] |
| 113 | Outside package | [docs/registry/implementation-map.yaml:L10906-L10916] |
| 114 | Outside package | [docs/registry/implementation-map.yaml:L10918-L10928] |
| 115 | Outside package | [docs/registry/implementation-map.yaml:L10934-L10944] |
| 116 | Outside package | [docs/registry/implementation-map.yaml:L10949-L10959] |
| 117 | Outside package | [docs/registry/implementation-map.yaml:L10963-L10973] |
| 118 | Outside package | [docs/registry/implementation-map.yaml:L10980-L10990] |
| 119 | Outside package | [docs/registry/implementation-map.yaml:L10992-L11002] |
| 120 | Outside package | [docs/registry/implementation-map.yaml:L11004-L11014] |
| 121 | Outside package | [docs/registry/implementation-map.yaml:L11016-L11026] |
| 122 | Outside package | [docs/registry/implementation-map.yaml:L11028-L11038] |
| 123 | Outside package | [docs/registry/implementation-map.yaml:L11040-L11050] |
| 124 | Outside package | [docs/registry/implementation-map.yaml:L11052-L11062] |
| 125 | Outside package | [docs/registry/implementation-map.yaml:L11064-L11074] |
| 126 | Outside package | [docs/registry/implementation-map.yaml:L11076-L11086] |
| 127 | Outside package | [docs/registry/implementation-map.yaml:L11090-L11100] |
| 128 | Outside package | [docs/registry/implementation-map.yaml:L11102-L11112] |
| 129 | Outside package | [docs/registry/implementation-map.yaml:L11114-L11124] |
| 130 | Outside package | [docs/registry/implementation-map.yaml:L11126-L11136] |
| 131 | Outside package | [docs/registry/implementation-map.yaml:L11138-L11148] |
| 132 | Outside package | [docs/registry/implementation-map.yaml:L11150-L11160] |
| 133 | Outside package | [docs/registry/implementation-map.yaml:L11164-L11174] |
| 134 | Outside package | [docs/registry/implementation-map.yaml:L11176-L11186] |
| 135 | Outside package | [docs/registry/implementation-map.yaml:L11188-L11198] |
| 136 | Outside package | [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | Outside package | [docs/registry/implementation-map.yaml:L11214-L11224] |
| 138 | Outside package | [docs/registry/implementation-map.yaml:L11226-L11236] |
| 139 | Outside package | [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | Outside package | [docs/registry/implementation-map.yaml:L11249-L11259] |
| 141 | Outside package | [docs/registry/implementation-map.yaml:L11261-L11271] |
| 142 | Outside package | [docs/registry/implementation-map.yaml:L11275-L11285] |
| 143 | Outside package | [docs/registry/implementation-map.yaml:L11287-L11297] |
| 144 | Outside package | [docs/registry/implementation-map.yaml:L11299-L11309] |
| 145 | Outside package | [docs/registry/implementation-map.yaml:L11311-L11321] |
| 146 | Outside package | [docs/registry/implementation-map.yaml:L11323-L11333] |
| 147 | Outside package | [docs/registry/implementation-map.yaml:L11335-L11345] |
| 148 | Outside package | [docs/registry/implementation-map.yaml:L11347-L11357] |
| 149 | Outside package | [docs/registry/implementation-map.yaml:L11361-L11371] |
| 150 | Outside package | [docs/registry/implementation-map.yaml:L11375-L11385] |
| 151 | Outside package | [docs/registry/implementation-map.yaml:L11389-L11399] |
| 152 | Outside package | [docs/registry/implementation-map.yaml:L11401-L11411] |
| 153 | Outside package | [docs/registry/implementation-map.yaml:L11415-L11425] |
| 154 | Outside package | [docs/registry/implementation-map.yaml:L11427-L11437] |
| 155 | Outside package | [docs/registry/implementation-map.yaml:L11439-L11449] |
| 156 | Outside package | [docs/registry/implementation-map.yaml:L11451-L11461] |
| 157 | Outside package | [docs/registry/implementation-map.yaml:L11463-L11473] |
| 158 | Outside package | [docs/registry/implementation-map.yaml:L11475-L11485] |
| 159 | Outside package | [docs/registry/implementation-map.yaml:L11487-L11497] |
| 160 | Outside package | [docs/registry/implementation-map.yaml:L11499-L11509] |
| 161 | Outside package | [docs/registry/implementation-map.yaml:L11511-L11521] |
| 162 | Outside package | [docs/registry/implementation-map.yaml:L11525-L11535] |
| 163 | Outside package | [docs/registry/implementation-map.yaml:L11537-L11547] |
| 164 | Outside package | [docs/registry/implementation-map.yaml:L11549-L11559] |
| 165 | Outside package | [docs/registry/implementation-map.yaml:L11561-L11571] |
| 166 | Outside package | [docs/registry/implementation-map.yaml:L11575-L11585] |
| 167 | Outside package | [docs/registry/implementation-map.yaml:L11591-L11601] |
| 168 | Outside package | [docs/registry/implementation-map.yaml:L11604-L11614] |
| 169 | Outside package | [docs/registry/implementation-map.yaml:L11620-L11630] |
| 170 | Outside package | [docs/registry/implementation-map.yaml:L11634-L11644] |
| 171 | Outside package | [docs/registry/implementation-map.yaml:L11649-L11659] |
| 172 | Outside package | [docs/registry/implementation-map.yaml:L11663-L11673] |
| 173 | Outside package | [docs/registry/implementation-map.yaml:L11675-L11685] |
| 174 | Outside package | [docs/registry/implementation-map.yaml:L11687-L11697] |
| 175 | Outside package | [docs/registry/implementation-map.yaml:L11699-L11709] |
| 176 | Outside package | [docs/registry/implementation-map.yaml:L11711-L11721] |
| 177 | Outside package | [docs/registry/implementation-map.yaml:L11725-L11735] |
| 178 | Outside package | [docs/registry/implementation-map.yaml:L11737-L11747] |
| 179 | Outside package | [docs/registry/implementation-map.yaml:L11749-L11759] |
| 180 | Outside package | [docs/registry/implementation-map.yaml:L11763-L11773] |
| 181 | Outside package | [docs/registry/implementation-map.yaml:L11775-L11785] |
| 182 | Outside package | [docs/registry/implementation-map.yaml:L11787-L11797] |
| 183 | Outside package | [docs/registry/implementation-map.yaml:L11799-L11809] |
| 184 | Outside package | [docs/registry/implementation-map.yaml:L11813-L11823] |
| 185 | Outside package | [docs/registry/implementation-map.yaml:L11825-L11835] |
| 186 | Outside package | [docs/registry/implementation-map.yaml:L11837-L11847] |
| 187 | Outside package | [docs/registry/implementation-map.yaml:L11851-L11861] |
| 188 | Outside package | [docs/registry/implementation-map.yaml:L11863-L11873] |
| 189 | Outside package | [docs/registry/implementation-map.yaml:L11875-L11885] |
| 190 | Outside package | [docs/registry/implementation-map.yaml:L11889-L11899] |
| 191 | Outside package | [docs/registry/implementation-map.yaml:L11901-L11911] |
| 192 | Outside package | [docs/registry/implementation-map.yaml:L11913-L11923] |
| 193 | Outside package | [docs/registry/implementation-map.yaml:L11927-L11937] |
| 194 | Outside package | [docs/registry/implementation-map.yaml:L11939-L11949] |
| 195 | Outside package | [docs/registry/implementation-map.yaml:L11951-L11961] |
| 196 | Outside package | [docs/registry/implementation-map.yaml:L11965-L11975] |
| 197 | Outside package | [docs/registry/implementation-map.yaml:L11977-L11987] |
| 198 | Outside package | [docs/registry/implementation-map.yaml:L11991-L12001] |
| 199 | Outside package | [docs/registry/implementation-map.yaml:L12003-L12013] |
| 200 | Outside package | [docs/registry/implementation-map.yaml:L12015-L12025] |
| 201 | Outside package | [docs/registry/implementation-map.yaml:L12027-L12037] |
| 202 | Outside package | [docs/registry/implementation-map.yaml:L12039-L12049] |
| 203 | Outside package | [docs/registry/implementation-map.yaml:L12053-L12063] |
| 204 | Outside package | [docs/registry/implementation-map.yaml:L12065-L12075] |
| 205 | Outside package | [docs/registry/implementation-map.yaml:L12079-L12089] |
| 206 | Outside package | [docs/registry/implementation-map.yaml:L12096-L12106] |
| 207 | Outside package | [docs/registry/implementation-map.yaml:L12108-L12118] |
| 208 | Outside package | [docs/registry/implementation-map.yaml:L12120-L12130] |
| 209 | Outside package | [docs/registry/implementation-map.yaml:L12132-L12142] |
| 210 | Outside package | [docs/registry/implementation-map.yaml:L12144-L12154] |
| 211 | Outside package | [docs/registry/implementation-map.yaml:L12160-L12170] |
| 212 | Outside package | [docs/registry/implementation-map.yaml:L12174-L12184] |
| 213 | Outside package | [docs/registry/implementation-map.yaml:L12187-L12197] |
| 214 | Outside package | [docs/registry/implementation-map.yaml:L12200-L12210] |
| 215 | Outside package | [docs/registry/implementation-map.yaml:L12212-L12222] |
| 216 | Outside package | [docs/registry/implementation-map.yaml:L12227-L12237] |
| 217 | Outside package | [docs/registry/implementation-map.yaml:L12239-L12249] |
| 218 | Outside package | [docs/registry/implementation-map.yaml:L12255-L12265] |
| 219 | Outside package | [docs/registry/implementation-map.yaml:L12267-L12277] |
| 220 | Outside package | [docs/registry/implementation-map.yaml:L12279-L12289] |
| 221 | Outside package | [docs/registry/implementation-map.yaml:L12291-L12301] |
| 222 | Outside package | [docs/registry/implementation-map.yaml:L12303-L12313] |
| 223 | Outside package | [docs/registry/implementation-map.yaml:L12315-L12325] |
| 224 | Outside package | [docs/registry/implementation-map.yaml:L12327-L12337] |
| 225 | Outside package | [docs/registry/implementation-map.yaml:L12339-L12349] |
| 226 | Outside package | [docs/registry/implementation-map.yaml:L12351-L12361] |
| 227 | Outside package | [docs/registry/implementation-map.yaml:L12363-L12373] |
| 228 | Outside package | [docs/registry/implementation-map.yaml:L12377-L12387] |
| 229 | Outside package | [docs/registry/implementation-map.yaml:L12389-L12399] |
| 230 | Outside package | [docs/registry/implementation-map.yaml:L12401-L12411] |
| 231 | Outside package | [docs/registry/implementation-map.yaml:L12413-L12423] |
| 232 | Outside package | [docs/registry/implementation-map.yaml:L12428-L12438] |
| 233 | Outside package | [docs/registry/implementation-map.yaml:L12442-L12452] |
| 234 | Outside package | [docs/registry/implementation-map.yaml:L12454-L12464] |
| 235 | Outside package | [docs/registry/implementation-map.yaml:L12466-L12476] |
| 236 | Outside package | [docs/registry/implementation-map.yaml:L12478-L12488] |
| 237 | Outside package | [docs/registry/implementation-map.yaml:L12490-L12500] |
| 238 | Outside package | [docs/registry/implementation-map.yaml:L12504-L12514] |
| 239 | `rao_adapter.py` / `GridRAO.from_file` | [docs/registry/implementation-map.yaml:L12516-L12526] |
| 240 | Outside package | [docs/registry/implementation-map.yaml:L12530-L12540] |
| 241 | Outside package | [docs/registry/implementation-map.yaml:L12542-L12552] |
| 242 | Outside package | [docs/registry/implementation-map.yaml:L12556-L12566] |
| 243 | Outside package | [docs/registry/implementation-map.yaml:L12568-L12578] |
| 244 | Outside package | [docs/registry/implementation-map.yaml:L12583-L12593] |
| 245 | Outside package | [docs/registry/implementation-map.yaml:L12595-L12605] |
| 246 | Outside package | [docs/registry/implementation-map.yaml:L12607-L12617] |
| 247 | Outside package | [docs/registry/implementation-map.yaml:L12621-L12631] |
| 248 | Outside package | [docs/registry/implementation-map.yaml:L12633-L12643] |
| 249 | Outside package | [docs/registry/implementation-map.yaml:L12647-L12657] |
| 250 | Outside package | [docs/registry/implementation-map.yaml:L12659-L12669] |
| 251 | Outside package | [docs/registry/implementation-map.yaml:L12671-L12681] |
| 252 | Outside package | [docs/registry/implementation-map.yaml:L12683-L12693] |
| 253 | Outside package | [docs/registry/implementation-map.yaml:L12697-L12707] |
| 254 | Outside package | [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | Outside package | [docs/registry/implementation-map.yaml:L12722-L12732] |
| 256 | Outside package | [docs/registry/implementation-map.yaml:L12734-L12744] |
| 257 | Outside package | [docs/registry/implementation-map.yaml:L12746-L12756] |
| 258 | Outside package | [docs/registry/implementation-map.yaml:L12760-L12770] |
| 259 | Outside package | [docs/registry/implementation-map.yaml:L12774-L12784] |
| 260 | Outside package | [docs/registry/implementation-map.yaml:L12788-L12798] |
| 261 | Outside package | [docs/registry/implementation-map.yaml:L12800-L12810] |
| 262 | Outside package | [docs/registry/implementation-map.yaml:L12814-L12824] |
| 263 | Outside package | [docs/registry/implementation-map.yaml:L12828-L12838] |
| 264 | Outside package | [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | Outside package | [docs/registry/implementation-map.yaml:L12851-L12861] |
| 266 | Outside package | [docs/registry/implementation-map.yaml:L12863-L12873] |
| 267 | Outside package | [docs/registry/implementation-map.yaml:L12875-L12885] |
| 268 | Outside package | [docs/registry/implementation-map.yaml:L12887-L12897] |
| 269 | Outside package | [docs/registry/implementation-map.yaml:L12899-L12909] |
| 270 | Outside package | [docs/registry/implementation-map.yaml:L12911-L12921] |
| 271 | Outside package | [docs/registry/implementation-map.yaml:L12923-L12933] |
| 272 | Outside package | [docs/registry/implementation-map.yaml:L12935-L12945] |
| 273 | Outside package | [docs/registry/implementation-map.yaml:L12947-L12957] |
| 274 | Outside package | [docs/registry/implementation-map.yaml:L12959-L12969] |
| 275 | Outside package | [docs/registry/implementation-map.yaml:L12974-L12984] |
| 276 | Outside package | [docs/registry/implementation-map.yaml:L12987-L12997] |
| 277 | Outside package | [docs/registry/implementation-map.yaml:L12999-L13009] |
| 278 | Outside package | [docs/registry/implementation-map.yaml:L13011-L13021] |
| 279 | Outside package | [docs/registry/implementation-map.yaml:L13023-L13033] |
| 280 | Outside package | [docs/registry/implementation-map.yaml:L13035-L13045] |
| 281 | Outside package | [docs/registry/implementation-map.yaml:L13049-L13059] |
| 282 | Outside package | [docs/registry/implementation-map.yaml:L13063-L13073] |
| 283 | Outside package | [docs/registry/implementation-map.yaml:L13075-L13085] |
| 284 | Outside package | [docs/registry/implementation-map.yaml:L13087-L13097] |
| 285 | Outside package | [docs/registry/implementation-map.yaml:L13102-L13112] |
| 286 | Outside package | [docs/registry/implementation-map.yaml:L13114-L13124] |
| 287 | Outside package | [docs/registry/implementation-map.yaml:L13126-L13136] |
| 288 | Outside package | [docs/registry/implementation-map.yaml:L13140-L13150] |
| 289 | Outside package | [docs/registry/implementation-map.yaml:L13154-L13164] |
| 290 | Outside package | [docs/registry/implementation-map.yaml:L13168-L13178] |
| 291 | Outside package | [docs/registry/implementation-map.yaml:L13182-L13192] |
| 292 | Outside package | [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | Outside package | [docs/registry/implementation-map.yaml:L13205-L13215] |
| 294 | Outside package | [docs/registry/implementation-map.yaml:L13217-L13227] |
| 295 | Outside package | [docs/registry/implementation-map.yaml:L13229-L13239] |

### Inventory: `wave-spectra`

| Snapshot row | Package membership | Record at map pin |
| --- | --- | --- |
| 1 | Outside package | [docs/registry/implementation-map.yaml:L13600-L13610] |
| 2 | Outside package | [docs/registry/implementation-map.yaml:L13612-L13622] |
| 3 | Outside package | [docs/registry/implementation-map.yaml:L13624-L13634] |
| 4 | Outside package | [docs/registry/implementation-map.yaml:L13636-L13646] |
| 5 | Outside package | [docs/registry/implementation-map.yaml:L13648-L13658] |
| 6 | Outside package | [docs/registry/implementation-map.yaml:L13663-L13673] |
| 7 | Outside package | [docs/registry/implementation-map.yaml:L13675-L13685] |
| 8 | Outside package | [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | Outside package | [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | Outside package | [docs/registry/implementation-map.yaml:L13709-L13719] |
| 11 | Outside package | [docs/registry/implementation-map.yaml:L13721-L13731] |
| 12 | Outside package | [docs/registry/implementation-map.yaml:L13733-L13743] |
| 13 | Outside package | [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | Outside package | [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | Outside package | [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | Outside package | [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | Outside package | [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | Outside package | [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | Outside package | [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | Outside package | [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | Outside package | [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | Outside package | [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | Outside package | [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | Outside package | [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | Outside package | [docs/registry/implementation-map.yaml:L13879-L13889] |
| 26 | Outside package | [docs/registry/implementation-map.yaml:L13891-L13901] |
| 27 | Outside package | [docs/registry/implementation-map.yaml:L13903-L13913] |
| 28 | Outside package | [docs/registry/implementation-map.yaml:L13915-L13925] |
| 29 | Outside package | [docs/registry/implementation-map.yaml:L13927-L13937] |
| 30 | Outside package | [docs/registry/implementation-map.yaml:L13939-L13949] |
| 31 | Outside package | [docs/registry/implementation-map.yaml:L13951-L13961] |
| 32 | Outside package | [docs/registry/implementation-map.yaml:L13965-L13975] |
| 33 | Outside package | [docs/registry/implementation-map.yaml:L13977-L13987] |
| 34 | Outside package | [docs/registry/implementation-map.yaml:L13989-L13999] |
| 35 | Outside package | [docs/registry/implementation-map.yaml:L14005-L14015] |
| 36 | Outside package | [docs/registry/implementation-map.yaml:L14018-L14028] |
| 37 | Outside package | [docs/registry/implementation-map.yaml:L14031-L14041] |
| 38 | Outside package | [docs/registry/implementation-map.yaml:L14044-L14054] |
| 39 | Outside package | [docs/registry/implementation-map.yaml:L14061-L14071] |
| 40 | Outside package | [docs/registry/implementation-map.yaml:L14073-L14083] |
| 41 | Outside package | [docs/registry/implementation-map.yaml:L14085-L14095] |
| 42 | Outside package | [docs/registry/implementation-map.yaml:L14097-L14107] |
| 43 | Outside package | [docs/registry/implementation-map.yaml:L14111-L14121] |
| 44 | Outside package | [docs/registry/implementation-map.yaml:L14123-L14133] |
| 45 | Outside package | [docs/registry/implementation-map.yaml:L14135-L14145] |
| 46 | Outside package | [docs/registry/implementation-map.yaml:L14147-L14157] |
| 47 | Outside package | [docs/registry/implementation-map.yaml:L14159-L14169] |
| 48 | Outside package | [docs/registry/implementation-map.yaml:L14173-L14183] |
| 49 | Outside package | [docs/registry/implementation-map.yaml:L14187-L14197] |
| 50 | Outside package | [docs/registry/implementation-map.yaml:L14200-L14210] |
| 51 | Outside package | [docs/registry/implementation-map.yaml:L14214-L14224] |
| 52 | Outside package | [docs/registry/implementation-map.yaml:L14226-L14236] |
| 53 | Outside package | [docs/registry/implementation-map.yaml:L14238-L14248] |
| 54 | Outside package | [docs/registry/implementation-map.yaml:L14250-L14260] |
| 55 | Outside package | [docs/registry/implementation-map.yaml:L14262-L14272] |
| 56 | Outside package | [docs/registry/implementation-map.yaml:L14276-L14286] |
| 57 | `wave_forecast.py` / `synthesize_directional_forecast` | [docs/registry/implementation-map.yaml:L14288-L14298] |
| 58 | `wave_source.py` / `jonswap_spectrum` | [docs/registry/implementation-map.yaml:L14302-L14312] |
| 59 | `wave_source.py` / `synthesize_forecast` | [docs/registry/implementation-map.yaml:L14316-L14326] |
| 60 | Outside package | [docs/registry/implementation-map.yaml:L14329-L14339] |
| 61 | Outside package | [docs/registry/implementation-map.yaml:L14343-L14353] |
| 62 | Outside package | [docs/registry/implementation-map.yaml:L14357-L14367] |
| 63 | Outside package | [docs/registry/implementation-map.yaml:L14369-L14379] |
| 64 | Outside package | [docs/registry/implementation-map.yaml:L14381-L14391] |
| 65 | Outside package | [docs/registry/implementation-map.yaml:L14393-L14403] |
| 66 | Outside package | [docs/registry/implementation-map.yaml:L14409-L14419] |
| 67 | Outside package | [docs/registry/implementation-map.yaml:L14422-L14432] |
| 68 | Outside package | [docs/registry/implementation-map.yaml:L14434-L14444] |
| 69 | Outside package | [docs/registry/implementation-map.yaml:L14446-L14456] |
| 70 | Outside package | [docs/registry/implementation-map.yaml:L14460-L14470] |
| 71 | Outside package | [docs/registry/implementation-map.yaml:L14478-L14488] |
| 72 | Outside package | [docs/registry/implementation-map.yaml:L14490-L14500] |
| 73 | Outside package | [docs/registry/implementation-map.yaml:L14502-L14512] |
| 74 | Outside package | [docs/registry/implementation-map.yaml:L14514-L14524] |
| 75 | Outside package | [docs/registry/implementation-map.yaml:L14526-L14536] |
| 76 | Outside package | [docs/registry/implementation-map.yaml:L14540-L14550] |
| 77 | Outside package | [docs/registry/implementation-map.yaml:L14552-L14562] |
| 78 | Outside package | [docs/registry/implementation-map.yaml:L14564-L14574] |
| 79 | Outside package | [docs/registry/implementation-map.yaml:L14578-L14588] |
| 80 | Outside package | [docs/registry/implementation-map.yaml:L14592-L14602] |
| 81 | Outside package | [docs/registry/implementation-map.yaml:L14606-L14616] |
| 82 | Outside package | [docs/registry/implementation-map.yaml:L14618-L14628] |
| 83 | Outside package | [docs/registry/implementation-map.yaml:L14630-L14640] |
| 84 | Outside package | [docs/registry/implementation-map.yaml:L14642-L14652] |
| 85 | Outside package | [docs/registry/implementation-map.yaml:L14656-L14666] |
| 86 | Outside package | [docs/registry/implementation-map.yaml:L14668-L14678] |
| 87 | Outside package | [docs/registry/implementation-map.yaml:L14680-L14690] |
| 88 | Outside package | [docs/registry/implementation-map.yaml:L14692-L14702] |
| 89 | Outside package | [docs/registry/implementation-map.yaml:L14704-L14714] |
| 90 | Outside package | [docs/registry/implementation-map.yaml:L14716-L14726] |
| 91 | Outside package | [docs/registry/implementation-map.yaml:L14728-L14738] |
| 92 | Outside package | [docs/registry/implementation-map.yaml:L14740-L14750] |
| 93 | Outside package | [docs/registry/implementation-map.yaml:L14752-L14762] |
| 94 | Outside package | [docs/registry/implementation-map.yaml:L14764-L14774] |
| 95 | Outside package | [docs/registry/implementation-map.yaml:L14776-L14786] |
| 96 | Outside package | [docs/registry/implementation-map.yaml:L14792-L14802] |
| 97 | Outside package | [docs/registry/implementation-map.yaml:L14807-L14817] |
| 98 | Outside package | [docs/registry/implementation-map.yaml:L14819-L14829] |
| 99 | Outside package | [docs/registry/implementation-map.yaml:L14831-L14841] |
| 100 | Outside package | [docs/registry/implementation-map.yaml:L14843-L14853] |
| 101 | Outside package | [docs/registry/implementation-map.yaml:L14855-L14865] |
| 102 | Outside package | [docs/registry/implementation-map.yaml:L14867-L14877] |
| 103 | Outside package | [docs/registry/implementation-map.yaml:L14879-L14889] |
| 104 | Outside package | [docs/registry/implementation-map.yaml:L14893-L14903] |
| 105 | Outside package | [docs/registry/implementation-map.yaml:L14907-L14917] |
| 106 | Outside package | [docs/registry/implementation-map.yaml:L14921-L14931] |
| 107 | Outside package | [docs/registry/implementation-map.yaml:L14933-L14943] |
| 108 | Outside package | [docs/registry/implementation-map.yaml:L14945-L14955] |
| 109 | Outside package | [docs/registry/implementation-map.yaml:L14957-L14967] |
| 110 | Outside package | [docs/registry/implementation-map.yaml:L14969-L14979] |
| 111 | Outside package | [docs/registry/implementation-map.yaml:L14981-L14991] |
| 112 | Outside package | [docs/registry/implementation-map.yaml:L14993-L15003] |
| 113 | Outside package | [docs/registry/implementation-map.yaml:L15005-L15015] |
| 114 | Outside package | [docs/registry/implementation-map.yaml:L15017-L15027] |
| 115 | Outside package | [docs/registry/implementation-map.yaml:L15029-L15039] |
| 116 | Outside package | [docs/registry/implementation-map.yaml:L15041-L15051] |
| 117 | Outside package | [docs/registry/implementation-map.yaml:L15053-L15063] |
| 118 | Outside package | [docs/registry/implementation-map.yaml:L15065-L15075] |
| 119 | Outside package | [docs/registry/implementation-map.yaml:L15077-L15087] |
| 120 | Outside package | [docs/registry/implementation-map.yaml:L15089-L15099] |
| 121 | Outside package | [docs/registry/implementation-map.yaml:L15101-L15111] |
| 122 | Outside package | [docs/registry/implementation-map.yaml:L15113-L15123] |
| 123 | Outside package | [docs/registry/implementation-map.yaml:L15125-L15135] |
| 124 | Outside package | [docs/registry/implementation-map.yaml:L15137-L15147] |
| 125 | Outside package | [docs/registry/implementation-map.yaml:L15149-L15159] |
| 126 | Outside package | [docs/registry/implementation-map.yaml:L15161-L15171] |
| 127 | Outside package | [docs/registry/implementation-map.yaml:L15173-L15183] |
| 128 | Outside package | [docs/registry/implementation-map.yaml:L15185-L15195] |
| 129 | Outside package | [docs/registry/implementation-map.yaml:L15199-L15209] |
| 130 | Outside package | [docs/registry/implementation-map.yaml:L15211-L15221] |
| 131 | Outside package | [docs/registry/implementation-map.yaml:L15223-L15233] |
| 132 | Outside package | [docs/registry/implementation-map.yaml:L15235-L15245] |
| 133 | Outside package | [docs/registry/implementation-map.yaml:L15247-L15257] |
| 134 | Outside package | [docs/registry/implementation-map.yaml:L15259-L15269] |
| 135 | Outside package | [docs/registry/implementation-map.yaml:L15271-L15281] |
| 136 | Outside package | [docs/registry/implementation-map.yaml:L15285-L15295] |
| 137 | Outside package | [docs/registry/implementation-map.yaml:L15299-L15309] |
| 138 | Outside package | [docs/registry/implementation-map.yaml:L15315-L15325] |
| 139 | Outside package | [docs/registry/implementation-map.yaml:L15329-L15339] |
| 140 | Outside package | [docs/registry/implementation-map.yaml:L15343-L15353] |
| 141 | Outside package | [docs/registry/implementation-map.yaml:L15362-L15372] |

### Inventory: `linear-wave-kinematics`

| Snapshot row | Package membership | Record at map pin |
| --- | --- | --- |
| 1 | Outside package | [docs/registry/implementation-map.yaml:L26260-L26269] |
| 2 | Outside package | [docs/registry/implementation-map.yaml:L26270-L26279] |
| 3 | Outside package | [docs/registry/implementation-map.yaml:L26280-L26289] |
| 4 | `reconstruct.py` / `wavenumber` | [docs/registry/implementation-map.yaml:L26290-L26299] |
| 5 | `reconstruct.py` / `wavenumber.f` | [docs/registry/implementation-map.yaml:L26300-L26309] |
| 6 | Outside package | [docs/registry/implementation-map.yaml:L26310-L26319] |
| 7 | Outside package | [docs/registry/implementation-map.yaml:L26320-L26329] |
| 8 | Outside package | [docs/registry/implementation-map.yaml:L26330-L26339] |
| 9 | Outside package | [docs/registry/implementation-map.yaml:L26340-L26349] |
| 10 | Outside package | [docs/registry/implementation-map.yaml:L26350-L26360] |
| 11 | Outside package | [docs/registry/implementation-map.yaml:L26362-L26372] |
| 12 | Outside package | [docs/registry/implementation-map.yaml:L26374-L26384] |
| 13 | Outside package | [docs/registry/implementation-map.yaml:L26385-L26395] |
| 14 | Outside package | [docs/registry/implementation-map.yaml:L26396-L26405] |
| 15 | Outside package | [docs/registry/implementation-map.yaml:L26406-L26415] |
| 16 | Outside package | [docs/registry/implementation-map.yaml:L26416-L26425] |
| 17 | Outside package | [docs/registry/implementation-map.yaml:L26426-L26435] |
| 18 | Outside package | [docs/registry/implementation-map.yaml:L26436-L26446] |
| 19 | Outside package | [docs/registry/implementation-map.yaml:L26448-L26458] |
| 20 | Outside package | [docs/registry/implementation-map.yaml:L26459-L26469] |
| 21 | Outside package | [docs/registry/implementation-map.yaml:L26475-L26485] |

### Signal audit: `rao-readers-and-orcaflex-converters`

| Enclosing definition | Matched lines | Classification |
| --- | --- | --- |
| `__init__.py` / `<module>` | 4; [src/digitalmodel/motion_forecast/__init__.py:L4-L4] | Imports or module documentation; no independent calculation |
| `conventions.py` / `<module>` | 1,3,5,10,13,17,18,20,40,44; [src/digitalmodel/motion_forecast/conventions.py:L1-L44] | Imports or module documentation; no independent calculation |
| `conventions.py` / `RAOSource` | 33,36; [src/digitalmodel/motion_forecast/conventions.py:L33-L36] | Data/representation or documentation; no further family result |
| `conventions.py` / `normalize_phase_deg` | 50,55; [src/digitalmodel/motion_forecast/conventions.py:L50-L55] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `models.py` / `<module>` | 5; [src/digitalmodel/motion_forecast/models.py:L5-L5] | Imports or module documentation; no independent calculation |
| `rao_adapter.py` / `<module>` | 1; [src/digitalmodel/motion_forecast/rao_adapter.py:L1-L1] | Imports or module documentation; no independent calculation |
| `rao_adapter.py` / `_interp_complex` | 44; [src/digitalmodel/motion_forecast/rao_adapter.py:L44-L44] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `GridRAO.__init__` | 61,70,71,72,80; [src/digitalmodel/motion_forecast/rao_adapter.py:L61-L80] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `GridRAO.from_unified` | 88; [src/digitalmodel/motion_forecast/rao_adapter.py:L88-L88] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `GridRAO.from_file` | 94,96; [src/digitalmodel/motion_forecast/rao_adapter.py:L94-L96] | Listed map row; also covered in section 3 |
| `recalibrate.py` / `<module>` | 13; [src/digitalmodel/motion_forecast/recalibrate.py:L13-L13] | Imports or module documentation; no independent calculation |
| `reconstruct.py` / `<module>` | 5; [src/digitalmodel/motion_forecast/reconstruct.py:L5-L5] | Imports or module documentation; no independent calculation |
| `reconstruct.py` / `reconstruct_motion` | 70,83,98,99,132; [src/digitalmodel/motion_forecast/reconstruct.py:L70-L132] | Delegates dispersion and RAO transfer; superposition covered in section 3 |
| `workflow.py` / `<module>` | 11; [src/digitalmodel/motion_forecast/workflow.py:L11-L11] | Imports or module documentation; no independent calculation |
| `workflow.py` / `generic_vessel_rao` | 31; [src/digitalmodel/motion_forecast/workflow.py:L31-L31] | Demo/oscillator response, covered in section 3; not a reader, spectrum or wave-dispersion solver |
| `workflow.py` / `_build_rao` | 66,71; [src/digitalmodel/motion_forecast/workflow.py:L66-L71] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `workflow.py` / `router` | 116,120,123; [src/digitalmodel/motion_forecast/workflow.py:L116-L123] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |

### Signal audit: `wave-spectra`

| Enclosing definition | Matched lines | Classification |
| --- | --- | --- |
| `__init__.py` / `<module>` | 70,88; [src/digitalmodel/motion_forecast/__init__.py:L70-L88] | Imports or module documentation; no independent calculation |
| `rao_adapter.py` / `GridRAO.__init__` | 82; [src/digitalmodel/motion_forecast/rao_adapter.py:L82-L82] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `wave_forecast.py` / `<module>` | 24; [src/digitalmodel/motion_forecast/wave_forecast.py:L24-L24] | Imports or module documentation; no independent calculation |
| `wave_forecast.py` / `dpz_horizon` | 76; [src/digitalmodel/motion_forecast/wave_forecast.py:L76-L76] | Energy moments/horizon calculation, covered in section 3; not another spectrum evaluation |
| `wave_forecast.py` / `synthesize_directional_forecast` | 132; [src/digitalmodel/motion_forecast/wave_forecast.py:L132-L132] | Listed map row; also covered in section 3 |
| `wave_source.py` / `<module>` | 3,9; [src/digitalmodel/motion_forecast/wave_source.py:L3-L9] | Imports or module documentation; no independent calculation |
| `wave_source.py` / `jonswap_spectrum` | 21,24,31,36,38,39; [src/digitalmodel/motion_forecast/wave_source.py:L21-L39] | Listed map row; also covered in section 3 |
| `wave_source.py` / `synthesize_forecast` | 68,85; [src/digitalmodel/motion_forecast/wave_source.py:L68-L85] | Listed map row; also covered in section 3 |
| `workflow.py` / `generic_vessel_rao._rolloff` | 46; [src/digitalmodel/motion_forecast/workflow.py:L46-L46] | Own deep-water expression, explicitly listed in section 3; no further miss |
| `workflow.py` / `_build_forecast` | 78; [src/digitalmodel/motion_forecast/workflow.py:L78-L78] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |

### Signal audit: `linear-wave-kinematics`

| Enclosing definition | Matched lines | Classification |
| --- | --- | --- |
| `__init__.py` / `<module>` | 52,87; [src/digitalmodel/motion_forecast/__init__.py:L52-L87] | Imports or module documentation; no independent calculation |
| `conventions.py` / `<module>` | 12,13; [src/digitalmodel/motion_forecast/conventions.py:L12-L13] | Imports or module documentation; no independent calculation |
| `models.py` / `WaveComponent` | 36,42,47; [src/digitalmodel/motion_forecast/models.py:L36-L47] | Data/representation or documentation; no further family result |
| `models.py` / `WaveForecast` | 65; [src/digitalmodel/motion_forecast/models.py:L65-L65] | Data/representation or documentation; no further family result |
| `models.py` / `WaveForecast.frequencies` | 68; [src/digitalmodel/motion_forecast/models.py:L68-L68] | Data/representation or documentation; no further family result |
| `rao_adapter.py` / `<module>` | 1; [src/digitalmodel/motion_forecast/rao_adapter.py:L1-L1] | Imports or module documentation; no independent calculation |
| `rao_adapter.py` / `_interp_complex` | 29; [src/digitalmodel/motion_forecast/rao_adapter.py:L29-L29] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `_interp_complex._interp_freq` | 37,38; [src/digitalmodel/motion_forecast/rao_adapter.py:L37-L38] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `GridRAO.H` | 99,100; [src/digitalmodel/motion_forecast/rao_adapter.py:L99-L100] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `rao_adapter.py` / `AnalyticRAO` | 104; [src/digitalmodel/motion_forecast/rao_adapter.py:L104-L104] | Data/representation or documentation; no further family result |
| `rao_adapter.py` / `AnalyticRAO.H` | 113,115; [src/digitalmodel/motion_forecast/rao_adapter.py:L113-L115] | Normalization, adapter, interpolation or workflow helper; covered in section 3 |
| `reconstruct.py` / `<module>` | 5,7,8,9; [src/digitalmodel/motion_forecast/reconstruct.py:L5-L9] | Imports or module documentation; no independent calculation |
| `reconstruct.py` / `wavenumber` | 25,26,28,29,31,33,36,38,39,46,52,57,64; [src/digitalmodel/motion_forecast/reconstruct.py:L25-L64] | Listed map row; also covered in section 3 |
| `reconstruct.py` / `wavenumber.f` | 43; [src/digitalmodel/motion_forecast/reconstruct.py:L43-L43] | Listed map row; also covered in section 3 |
| `reconstruct.py` / `reconstruct_motion` | 84,125,130,132,143; [src/digitalmodel/motion_forecast/reconstruct.py:L84-L143] | Delegates dispersion and RAO transfer; superposition covered in section 3 |
| `top_tension.py` / `displacement_transmissibility` | 84,87,88; [src/digitalmodel/motion_forecast/top_tension.py:L84-L88] | Demo/oscillator response, covered in section 3; not a reader, spectrum or wave-dispersion solver |
| `top_tension.py` / `force_transmissibility` | 92,96,97,99; [src/digitalmodel/motion_forecast/top_tension.py:L92-L99] | Demo/oscillator response, covered in section 3; not a reader, spectrum or wave-dispersion solver |
| `wave_forecast.py` / `<module>` | 23; [src/digitalmodel/motion_forecast/wave_forecast.py:L23-L23] | Imports or module documentation; no independent calculation |
| `wave_forecast.py` / `_moments` | 30,31; [src/digitalmodel/motion_forecast/wave_forecast.py:L30-L31] | Energy moments/horizon calculation, covered in section 3; not another spectrum evaluation |
| `wave_forecast.py` / `coherence_horizon` | 60,61; [src/digitalmodel/motion_forecast/wave_forecast.py:L60-L61] | Energy moments/horizon calculation, covered in section 3; not another spectrum evaluation |
| `wave_forecast.py` / `dpz_horizon` | 75,92; [src/digitalmodel/motion_forecast/wave_forecast.py:L75-L92] | Energy moments/horizon calculation, covered in section 3; not another spectrum evaluation |
| `wave_forecast.py` / `synthesize_directional_forecast` | 118,123,130,131,132,145,147,155; [src/digitalmodel/motion_forecast/wave_forecast.py:L118-L155] | Listed map row; also covered in section 3 |
| `wave_forecast.py` / `surface_elevation` | 165,173,176; [src/digitalmodel/motion_forecast/wave_forecast.py:L165-L176] | Delegates dispersion and RAO transfer; superposition covered in section 3 |
| `wave_source.py` / `<module>` | 8; [src/digitalmodel/motion_forecast/wave_source.py:L8-L8] | Imports or module documentation; no independent calculation |
| `wave_source.py` / `jonswap_spectrum` | 22,24,26,29,31,35,36,38,40,43; [src/digitalmodel/motion_forecast/wave_source.py:L22-L43] | Listed map row; also covered in section 3 |
| `wave_source.py` / `synthesize_forecast` | 57,59,73,80,82,83,85,86,92,101; [src/digitalmodel/motion_forecast/wave_source.py:L57-L101] | Listed map row; also covered in section 3 |
| `workflow.py` / `generic_vessel_rao._mech` | 39,41; [src/digitalmodel/motion_forecast/workflow.py:L39-L41] | Demo/oscillator response, covered in section 3; not a reader, spectrum or wave-dispersion solver |
| `workflow.py` / `generic_vessel_rao._rolloff` | 44,45; [src/digitalmodel/motion_forecast/workflow.py:L44-L45] | Own deep-water expression, explicitly listed in section 3; no further miss |
| `workflow.py` / `generic_vessel_rao.heave` | 48,49; [src/digitalmodel/motion_forecast/workflow.py:L48-L49] | Demo/oscillator response, covered in section 3; not a reader, spectrum or wave-dispersion solver |
| `workflow.py` / `generic_vessel_rao.pitch` | 51,52,53; [src/digitalmodel/motion_forecast/workflow.py:L51-L53] | Own deep-water expression, explicitly listed in section 3; no further miss |
| `workflow.py` / `generic_vessel_rao.roll` | 55,56,57; [src/digitalmodel/motion_forecast/workflow.py:L55-L57] | Own deep-water expression, explicitly listed in section 3; no further miss |

## 10. Coverage and open items

### Files read and test coverage

All 18 Python files under `src/digitalmodel/motion_forecast/` were read: `__init__.py`, `conventions.py`, `criteria.py`, `decision.py`, `derived.py`, `feedback.py`, `measured.py`, `measured_source.py`, `models.py`, `rao_adapter.py`, `recalibrate.py`, `reconcile.py`, `reconstruct.py`, `skill.py`, `top_tension.py`, `wave_forecast.py`, `wave_source.py`, `workflow.py`; the packaged criteria YAML was also read. The implementing definitions are cited throughout sections 1–6 and 9.

The primary offline inventory is 123 test cases in 17 files, rather than the earlier inventory estimate. All passed at the code pin; no module case was skipped or NOT_RUN. No solver execution or licensed interface was used. Baseline and final run counts and evidence digests accompany the change.

| Test file, all under `tests/motion_forecast/` | Scope / evidence |
| --- | --- |
| `test_consistency.py` | Derived closed-form peaks. [tests/motion_forecast/test_consistency.py:L23-L47] |
| `test_conventions.py` | Synthetic source-sign and heading checks. [tests/motion_forecast/test_conventions.py:L18-L60] |
| `test_criteria.py` | Defaults, supplied YAML and rejection checks. [tests/motion_forecast/test_criteria.py:L8-L71] |
| `test_decision.py` | Strict boundaries, lead times, NaN and downward magnitude. [tests/motion_forecast/test_decision.py:L28-L96] |
| `test_derived.py` | Kinematics, magnitudes and dispatch. [tests/motion_forecast/test_derived.py:L27-L63] |
| `test_feedback.py` | Fractions and sample runs. [tests/motion_forecast/test_feedback.py:L11-L29] |
| `test_measured.py` | Clock, window, shapes and governing interoperability. [tests/motion_forecast/test_measured.py:L17-L54] |
| `test_measured_source.py` | Seeded synthetic sensor and CSV fixtures. [tests/motion_forecast/test_measured_source.py:L21-L64] |
| `test_rao_file.py` | Hand-authored file and complex transfer. [tests/motion_forecast/test_rao_file.py:L22-L56] |
| `test_recalibrate.py` | Affine fit, holdout, noise and variance guard. [tests/motion_forecast/test_recalibrate.py:L33-L79] |
| `test_reconcile.py` | Alignment, residual metrics, bounds and latest measured state. [tests/motion_forecast/test_reconcile.py:L36-L139] |
| `test_reconstruct.py` | Phase, depth, horizon and lever arm. [tests/motion_forecast/test_reconstruct.py:L26-L100] |
| `test_skill.py` | Overlap, pooled errors and partial dropout. [tests/motion_forecast/test_skill.py:L28-L73] |
| `test_top_tension.py` | Derived properties, response identities, integration, slack and invalid-line guards. [tests/motion_forecast/test_top_tension.py:L38-L120] |
| `test_validation.py` | Energy and variance regression. [tests/motion_forecast/test_validation.py:L39-L77] |
| `test_wave_forecast.py` | Horizons, spread, surface and source interface. [tests/motion_forecast/test_wave_forecast.py:L25-L116] |
| `test_workflow.py` | Serialization and optional routes; dispatch checked as source text. [tests/motion_forecast/test_workflow.py:L11-L177] |

Documentation discovery used `rg -l -i 'motion.forecast' docs`; the following existing files were inspected: `docs/UNITS.md`, `docs/maps/digitalmodel-operator-map.md`, `docs/registry/module-routing.yaml`, `docs/registry/implementation-map.yaml`, `docs/registry/implementation-map.md`, `docs/capability-map/capability-cards.json`, `docs/capability-map/capabilities-sections.yml`, `docs/api/_assets/LAYOUT.md`, `docs/api/hydro/short-horizon-motion-forecast.html`, and the six plan documents listed below. Discovery records are coverage metadata, not validation claims.

| Plan document | Reading boundary |
| --- | --- |
| `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md` | Historical reconstruction intent; code supersedes unfinished checklist claims. [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L29-L42] |
| `docs/plans/2026-07-04-issue-1357-wave-forecast.md` | Horizon/synthesis intent; source distinguishes coherence proxy and DPZ. [src/digitalmodel/motion_forecast/wave_forecast.py:L39-L79] |
| `docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md` | Governing quantities and criterion basis. [docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md:L20-L27] |
| `docs/plans/2026-07-04-issue-1360-learning-loop.md` | Historical learning-loop intent; output correction boundary is authoritative in code. [src/digitalmodel/motion_forecast/recalibrate.py:L1-L13] |
| `docs/plans/2026-07-04-issue-1367-mms-ingest-measured-mode.md` | Shared-clock/measured-mode design; scenario-identity gap is S37. [src/digitalmodel/motion_forecast/workflow.py:L186-L189] |
| `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | Packaged-data-loader precedent only. [docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md:L87-L91] |

The related ROV consumer suite also passed 22 cases; its RAO path exercises this package through the public imports and deployment-point velocity route. [tests/subsea/rov/test_seastate_limits.py:L134-L175]

### Open-item register

Each draft reference below has a matching issue draft; the maintainer replaces draft references with issue numbers before independent clearance.

| Item | Coverage gap / action | Responsible role | Tracking |
| --- | --- | --- | --- |
| U1 | Matched-data AQWA phase convention qualification; synthetic consistency is insufficient. [src/digitalmodel/motion_forecast/conventions.py:L17-L22] | Calculations quality manager | issue pending (1) |
| U2 | Standards editions, clauses, criterion provenance and calculation citation sidecars; clause text not checked against the licensed copy. [src/digitalmodel/motion_forecast/criteria_defaults.yml:L1-L34] [src/digitalmodel/motion_forecast/top_tension.py:L1-L18] | Calculations quality manager | issue pending (1) |
| U3 | Horizon publication locator and operational predictability qualification. [src/digitalmodel/motion_forecast/wave_forecast.py:L39-L79] | Calculations quality manager | issue pending (1), #1467 |
| Input domains | S13–S49 and S53: finite values, locale, units, enum validation, interpolation bounds and forwarded sea metadata | Maintainer | issue pending (2) |
| Missing observations | S01–S12 and S52: unavailable skill, filtering, fit sample counts, bin completeness and split validation | Maintainer | issue pending (3), #1360 |
| Scenario provenance | S29–S38 and S56: scenario identity, optional output absence and reused-configuration state | Maintainer | issue pending (4), #1367 |
| Evidence lifecycle | G5 production fingerprints/appendix, deputy assignment and shared-log registration | Maintainer | issue pending (5) |
| Map follow-up | Add remaining section-3 method families, demo direct expressions and external page implementations before counting their package rows | Maintainer | issue pending (6) |
| Physical examples | No measured/public benchmark qualification is established by the synthetic tests; add field-independent reference examples and measured skill evidence | Calculations quality manager | #1467, #1358 |
| Nonlinear/time-domain extension | IRF/Cummins remains explicitly deferred in the historical design; no claim of this functionality. [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L40-L42] | Maintainer | #1358 |
| Sensor integration | Hardware protocols/time synchronization and nonlinear wave reconstruction remain outside the implemented synthetic seams. [src/digitalmodel/motion_forecast/measured_source.py:L7-L12] [src/digitalmodel/motion_forecast/wave_forecast.py:L1-L13] | Maintainer | #1367, #1357 |
| External parser lead | Assess complete-grid behavior through the actual delegated reader rather than attributing a different parser's defect to this wrapper. [src/digitalmodel/motion_forecast/rao_adapter.py:L91-L97] | Maintainer | #2107 |

No private sidecar entry is named or required for the synthetic values reproduced here; no private data was used. Licensed standards were not opened, and no licensed tabulated value is asserted by this pack.
