# River currents module knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/river_currents |
| Repository | digitalmodel |
| Package | src/digitalmodel/hydrodynamics/river_currents |
| Pack revision | 0 |
| Code revision | 2aa19e88306d9b4294e49a2b7849b07ae036b205 |
| Analysis family | Engineering calculations; EC; AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package supplies bed-referenced vertical profiles, assumed bank and bend corrections, probability-plot extreme fits, steady draft-integrated loads and synthetic analysis generation. [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L4-L22]

The routing registry assigns production maturity to the hydrodynamics domain; that is a routing classification, rather than evidence of calibrated river navigation or towing accuracy. [docs/registry/module-routing.yaml:L171-L176] [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L20-L24] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L8-L27]

Tidal reversal, unsteady flow, calibrated hull coefficients, tug resistance, available thrust at speed and passing-vessel interaction remain outside the stated scope (#2000, #2002). [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L20-L22]

## 2. Inputs and outputs

Inputs use SI quantities and explicit bed-referenced heights; the force direction is relative to the bow, positive towards positive sway, with collinear vessel speed subtracted from the profile. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L58-L66] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L56]

| Input / consuming function | Unit | Domain and default | Source and evidence |
| --- | --- | --- | --- |
| `height_above_bed`, `total_depth` / power profile | m | Scalar or array; depth > 0; heights clipped to [0,H]; required | Caller; [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L81] |
| `surface_velocity`, `exponent_m` / power profile and average | m/s; dimensionless | Surface velocity required; exponent > 0, default 6.0; finite velocity not enforced | Caller; [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L81] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L84-L114] |
| `shear_velocity`, `roughness_height`, `von_karman` / log profile and average | m/s; m; dimensionless | Positive; first two required; kappa default 0.41; average requires H > roughness | Caller; [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L117-L163] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L166-L211] |
| `n_points`, `model` / `river_velocity_profile` | count; selector | At least 2; default power law; surface 1.0 m/s, exponent 6.0, shear 0.1 m/s, roughness 0.01 m | Caller; [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L214-L262] |
| `position`, `width`, `retardation`, `outer_bias` | m; m; dimensionless; dimensionless | Required; left bank position in [0,width]; retardation [0,1]; bias [0,1); right bank is outer bank | Caller; [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L37-L83] |
| Width, depth, beam, draft / `blockage_factor` | m | Required, finite positive; beam < width; draft < depth | Caller; [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L62-L71] |
| Speed, width, radius, gravity / `bend_superelevation` | m/s; m; m; m/s² | Required; finite speed; positive geometry/gravity; centreline radius > half-width | Caller; [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L86-L105] |
| Profile and correction factor / `corrected_profile` | m/s callback; dimensionless | Required; finite nonnegative factor; callback must yield finite speed | Caller; [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L108-L120] |
| Speed/discharge samples; annual maxima; period; sample rate | m/s or m³/s; years; count/year | At least 3 finite nonconstant observations; Weibull positive, Gumbel nonnegative; period > 1; positive rate default 1 | Caller; [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L30-L105] |
| Seasonal values and labels | Input quantity unit; text | Equal nonempty lengths; finite nonnegative values and nonempty strings | Caller; [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L108-L118] |
| Vessel, water depth, profile / `current_load` | Geometry m; depth m; callback m/s | Required; positive vessel length/beam/draft; depth > draft | Caller; [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82] |
| Direction, density, vessel speed, integration slices | degrees; kg/m³; m/s; count | Defaults 0, 1000, 0, 1000; finite direction/speed; positive density; positive integer slices excluding bool | Caller; [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82] |
| Loads, available pull, efficiency, reserve, yaw capacity | N and N·m; N; dimensionless; dimensionless; N·m | Finite loads; nonnegative capacity; efficiency (0,1]; reserve ≥ 1; yaw capacity defaults None | Caller; [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L85-L110] |
| Basis JSON / `build` | SI by keys | Explicit basis and output locations; revision history required; synthetic basis revision 2 | [examples/hydrodynamics/river_currents/basis.json:L1-L15] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L186-L210] |

| Output | Unit / convention | Evidence |
| --- | --- | --- |
| Profile speeds and depth averages | m/s; scalar or array matching profile input; sampled heights start at bed 0 and end at H | [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L65-L81] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L240-L262] |
| Lateral, blockage, bend factors; corrected profile | Dimensionless factors; callback m/s | [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L44-L120] |
| Superelevation | m, outer minus inner water level; speed sign does not alter rise | [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L86-L105] |
| Weibull/Gumbel fit and return levels | Shape dimensionless; scale/location/return level in input series unit | [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L56-L105] |
| Seasonal summary | Count, mean and maximum per supplied season; no annualisation | [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L108-L118] |
| `Fx_surge_N`, `Fy_sway_N`, `Mz_yaw_Nm` | N, N, N·m; signed profile integration | [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L65-L82] |
| Towing assessment | Required/available N, absolute yaw N·m, boolean yaw/go comparisons | [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L105-L110] |
| Analysis HTML, results JSON, manifest JSON | HTML embeds SVG; JSON carries SI keys; manifest hashes basis and output bytes | [src/digitalmodel/hydrodynamics/river_currents/examples.py:L79-L95] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L186-L201] |

There is no time or phase input in the steady force API; the sign is carried by `speed*abs(speed)` and cardinal-axis components are zeroed at a trigonometric tolerance of 1e-12 dimensionless. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82]

## 3. Method and governing standards

| Method / implementation | Basis and limits |
| --- | --- |
| `power_law_profile`; `power_law_depth_averaged` | `u = u_s*(clip(z,0,H)/H)**(1/m)`; average `u_s*m/(m+1)`; docstring attributes the law to Chow (1959), Open-Channel Hydraulics, and Chen (1991), Unified theory on power laws for flow resistance; no equation locator is given. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L13-L20] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L71-L114] |
| `log_law_profile`; `log_law_depth_averaged` | `u = (u_star/kappa)*ln(max(z,z0)/z0)`, clipped nonnegative; average `(u_star/(kappa*H))*(H*ln(H/z0)-(H-z0))`; docstring names Julien (2002), River Mechanics, chapter 6. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L18-L20] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L149-L211] |
| `river_velocity_profile` | Samples the chosen shipped law with `linspace`; delegates rather than implementing another profile law. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L247-L262] |
| `lateral_factor`; `blockage_factor`; `bend_factor` | Assumed quadratic `1-r+6*r*x*(1-x)` and linear bias `1+a*(2*x-1)`; continuity factor `1/(1-beam*draft/(width*depth))`; docstring cites NASA Glenn, Conservation of Mass, for continuity only. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L8-L24] [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L44-L83] |
| `bend_superelevation` | Constant-speed radial/hydrostatic balance integrated across the bend, implemented as `speed**2/gravity*log1p(width/(radius-width/2))`; docstring names OpenStax University Physics Volume 1 (2016), equations 6.3 and 14.4; neither secondary flow nor friction is resolved. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L86-L105] |
| `_series`; `_regression`; `_annual_log_probability`; `fit_weibull`; `WeibullFit.return_level`; `fit_gumbel`; `GumbelFit.return_level`; `seasonal_summary` | Probability-plot least squares at `(rank-0.5)/n`, using the CDF transforms attributed to NIST/SEMATECH e-Handbook sections 1.3.6.6.8 and 1.3.6.6.16; Weibull annual probability is `F(x)**rate=1-1/T`; Gumbel uses one maximum/year; summaries group supplied labels. [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L8-L20] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L30-L118] |
| `shallow_drag_factor`; `current_load` | Full-width slot hypothesis `[H/(H-T)]**2`; midpoint mean of signed relative `u*abs(u)` multiplies `OCIMFLoading.current_load(vessel,1,direction,density)`; ignores side bypass and is uncalibrated. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L8-L27] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L39-L82] |
| `towing_assessment` | Required nominal pull `hypot(Fx,Fy)*reserve/efficiency`; absolute yaw compared separately; absent yaw capacity accepts only zero moment. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L85-L110] |
| `examples.compute`; `profile_svg`; `render`; `build` | Orchestration, inline visualization, rendering and byte hashing around the listed calculation functions; the discharge-to-surface conversion is `Q/(W*H)*(m+1)/m`. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L29-L95] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L103-L201] |

DNV-RP-C205, Environmental conditions and environmental loads, is named as an open-ocean contrast in the package and profile docstrings; those uses supply neither edition nor clause title. [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L4-L10] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L7-L20]

The historical plan names DNV-RP-C205 §3.4 and proposed citation hooks; it supplies no edition or clause title, while the shipped calculation functions return numeric values, arrays and dictionaries without a `Citation` sidecar. Clause text not checked against the licensed copy; no edition mismatch can be established when neither use identifies an edition. [docs/plans/2026-05-05-issue-493-river-current-profiles.md:L18-L33] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L76-L81] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L92-L118] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L80-L110]

The upstream load function names OCIMF MEG4 Section 3.2, but its approximate coefficient formulas are code, not a checked licensed coefficient table; no tabulated standard values are reproduced here. [src/digitalmodel/hydrodynamics/ocimf_loading.py:L109-L186] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L16-L22]

UNVERIFIED: DNV-RP-C205 edition, clause title and applicability, and the accuracy of the bibliographic attributions above against the original publications; issue pending (2).

## 4. Validated worked examples

The four analytical cases below validate mathematical identities for synthetic inputs; they do not validate calibrated river hydraulics or operational towing limits. [tests/hydrodynamics/river_currents/test_profiles.py:L45-L56] [tests/hydrodynamics/river_currents/test_profiles.py:L100-L110] [tests/hydrodynamics/river_currents/test_bank_effects.py:L27-L41] [tests/hydrodynamics/river_currents/test_extensions.py:L141-L152]

Result record: all six listed node IDs passed at the code revision in the header, both before and after documentation edits; inputs are inline unless a file is listed. The complete module command collected and passed 81 cases, with 0 failed, 0 skipped and 0 NOT_RUN; this replaces the inventory lead of 16 tests. [tests/hydrodynamics/river_currents/test_profiles.py:L21-L149] [tests/hydrodynamics/river_currents/test_bank_effects.py:L27-L108] [tests/hydrodynamics/river_currents/test_extensions.py:L14-L302] [tests/hydrodynamics/river_currents/test_ci_gate.py:L16-L22]

Command form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/hydrodynamics/river_currents/ -q -rA -p no:cacheprovider --basetemp=<scratch-directory>`; the environment is in scratch, and no solver is needed by these tests. [tests/hydrodynamics/river_currents/test_profiles.py:L3-L12] [tests/hydrodynamics/river_currents/test_extensions.py:L6-L11] [tests/hydrodynamics/river_currents/test_bank_effects.py:L17-L24]

| Case / class | Independent expected value, unit and tolerance basis | Existing node ID / offline result |
| --- | --- | --- |
| V1 power-law depth average / validated | Inline H=10 m, surface=2 m/s, m=6; closed-form integral `2*6/7` m/s; relative 1e-6 against hand value, 1e-4 against trapezoidal quadrature | `tests/hydrodynamics/river_currents/test_profiles.py::test_power_law_depth_averaged_matches_analytical_integral` — PASS / PASS. [tests/hydrodynamics/river_currents/test_profiles.py:L45-L56] |
| V2 log-law depth average / validated | Inline H=10 m, shear=0.1 m/s, roughness=0.01 m, kappa=0.41; independent integral `(0.1/(0.41*10))*(10*ln(1000)-(10-0.01))` m/s; relative 1e-6 against hand value, 1e-4 against quadrature | `tests/hydrodynamics/river_currents/test_profiles.py::test_log_law_depth_averaged_matches_analytical_and_numerical` — PASS / PASS. [tests/hydrodynamics/river_currents/test_profiles.py:L100-L110] |
| V3 bend radial balance / validated | Inline U=2 m/s, gravity=10 m/s², width=100 m, centreline radius=200 m; independent Decimal evaluation `(2/5)*ln(5/3)` m at 60 decimal digits; error ≤ 1 ULP of expected m, a floating-point criterion | `tests/hydrodynamics/river_currents/test_bank_effects.py::test_representative_bend_against_hand_integral` — PASS / PASS. [tests/hydrodynamics/river_currents/test_bank_effects.py:L27-L41] |
| V4 draft integration / validated | Inline vessel from `_vessel`, H=3 m and surface=2 m/s; hand integral of `u²` on [1,3] m, multiplied by the code's unit-load factor; relative force error ≤ 1e-6 dimensionless with 1000 slices; validates quadrature, not coefficients | `tests/hydrodynamics/river_currents/test_extensions.py::test_depth_integrated_power_law_against_analytical_integral` — PASS / PASS. [tests/hydrodynamics/river_currents/test_extensions.py:L118-L121] [tests/hydrodynamics/river_currents/test_extensions.py:L141-L152] |
| R1 deterministic analysis / regression | `examples/hydrodynamics/river_currents/basis.json`, revision 2; analytic assertions plus exact agreement of generated HTML/JSON/manifest with committed bytes; tolerances include 1e-6 relative force error and 0 byte differences | `tests/hydrodynamics/river_currents/test_extensions.py::test_worked_examples_and_deterministic_report` — PASS / PASS. [examples/hydrodynamics/river_currents/basis.json:L1-L15] [tests/hydrodynamics/river_currents/test_extensions.py:L193-L228] |
| R2 slot sensitivity / regression | Same basis; full-width-slot factor 9 dimensionless; demand without slot 100.4 kN ±0.05 kN; verdict reverses for the given capacity; checks the explicit hypothesis rather than field accuracy | `tests/hydrodynamics/river_currents/test_extensions.py::test_towing_verdict_reverses_without_slot_multiplier` — PASS / PASS. [tests/hydrodynamics/river_currents/test_extensions.py:L231-L248] |

## 5. QA gates

Gate allocation follows AQG r0, section 10, Python calculations, and section 9 for G5; criteria below are pack checks, not design acceptance tolerances. (AQG r0, Analysis QA gates, revision 0, sections 9–10.)

| Gate | Applicability | Check IDs |
| --- | --- | --- |
| G0 | Applicable: explicit quantities, input guards and provenance | C01–C04 |
| G1 | Applicable: Python import/test route; no vendor program output format is written by this package | C05 |
| G2 | Applicable: independent mathematical checks for selected synthetic cases | C06–C09 |
| G3 | NOT_APPLICABLE: no time-domain solver in the steady calculation API | [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82] |
| G4 | NOT_APPLICABLE: no diffraction mesh or radiation solver in these calculation paths | [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L44-L120] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L92-L118] |
| G5 | Applicable: reproducible analysis output and traceable evidence; full signed gate appendix remains an open item | C10–C12 |

For checks referencing tests, run the whole module command in section 4 and inspect the corresponding node result; the following additional nodes all occur in that command. [tests/hydrodynamics/river_currents/test_extensions.py:L49-L115] [tests/hydrodynamics/river_currents/test_extensions.py:L155-L190] [tests/hydrodynamics/river_currents/test_ci_gate.py:L16-L22]

| Check | Gate | Executable check and numeric pass criterion / unit | Evidence and scope |
| --- | --- | --- | --- |
| C01 | G0 | Run `test_power_law_invalid_inputs`, `test_log_law_invalid_inputs`, `test_river_velocity_profile_invalid`; 0 unexpected acceptances of their enumerated invalid inputs, count | Existing guards only; non-finite inputs and log sampler depth remain uncovered. [tests/hydrodynamics/river_currents/test_profiles.py:L65-L69] [tests/hydrodynamics/river_currents/test_profiles.py:L113-L119] [tests/hydrodynamics/river_currents/test_profiles.py:L145-L149] |
| C02 | G0 | Run all `test_bank_invalid`, `test_invalid_positive_inputs`, `test_nonfinite_speed`, `test_inner_bank_must_have_positive_radius` and `test_invalid_loading` parameters; 0 unexpected acceptances, count | Finite channel/force input rejection evidence. [tests/hydrodynamics/river_currents/test_extensions.py:L49-L62] [tests/hydrodynamics/river_currents/test_extensions.py:L181-L190] [tests/hydrodynamics/river_currents/test_bank_effects.py:L87-L108] |
| C03 | G0 | Run all `test_extreme_invalid_series` and `test_extreme_invalid_return_period` parameters; 0 unexpected acceptances, count | Does not establish independence, units, stationarity or locale parsing. [tests/hydrodynamics/river_currents/test_extensions.py:L99-L115] |
| C04 | G0 | `test_revision_must_match_latest_record`: 0 accepted revision mismatches, count; R1: 0 mismatched basis digests, count | JSON source fingerprint and revision evidence. [tests/hydrodynamics/river_currents/test_extensions.py:L220-L223] [tests/hydrodynamics/river_currents/test_extensions.py:L294-L302] |
| C05 | G1 | Whole module command: 81 passed and 0 failed, count; `test_hosted_gate_includes_river_currents`: 0 missing test-root entries, count | Python imports, offline execution and gate inclusion; no licensed program load route applies. [tests/hydrodynamics/river_currents/test_ci_gate.py:L16-L22] [tests/hydrodynamics/river_currents/test_profiles.py:L3-L12] |
| C06 | G2 | V1 and V2; relative errors ≤ 1e-6 for hand reference and ≤ 1e-4 for quadrature, dimensionless | Closed-form integration evidence. [tests/hydrodynamics/river_currents/test_profiles.py:L45-L56] [tests/hydrodynamics/river_currents/test_profiles.py:L100-L110] |
| C07 | G2 | V3 plus `test_gentle_bend_retains_nonzero_superelevation`, `test_large_radii_do_not_overflow_outer_bank_sum`, all `test_speed_sign_and_still_water` cases; absolute error ≤ 1 ULP(expected) m | Independent Decimal/Taylor references and cancellation/overflow cases. [tests/hydrodynamics/river_currents/test_bank_effects.py:L27-L84] |
| C08 | G2 | V4 relative force error ≤ 1e-6 dimensionless; `test_signed_profile_relative_motion_and_oblique_loads` reversed loads agree within pytest default relative 1e-6, with absolute floor 1e-12 N or N·m | Mathematical integration and sign evidence; calibration excluded. [tests/hydrodynamics/river_currents/test_extensions.py:L141-L164] |
| C09 | G2 | `test_lateral_magnitude_and_discharge_conservation`: Simpson mean exactly 1 dimensionless; `test_weibull_known_quantile_series`, `test_gumbel_known_annual_maxima_series`: expected parameters/quantiles within pytest default relative 1e-6, absolute floor 1e-12 in their units | Synthetic algebraic consistency, not observational validation. [tests/hydrodynamics/river_currents/test_extensions.py:L14-L22] [tests/hydrodynamics/river_currents/test_extensions.py:L65-L87] |
| C10 | G5 | R1: 0 differing output bytes and 0 digest mismatches, counts | Deterministic page/results/manifest evidence. [tests/hydrodynamics/river_currents/test_extensions.py:L213-L228] |
| C11 | G5 | R2: demand without slot within ±0.05 kN of 100.4 kN, correct two boolean decisions (2/2 count); summary contains the reversal | Exposes model sensitivity; not operational acceptance. [tests/hydrodynamics/river_currents/test_extensions.py:L231-L248] |
| C12 | G5 | `test_towing_summary_without_verdict_reversal`, `test_without_slot_verdict_requires_pull_and_yaw`, `test_towing_pull_and_yaw_limit`: 0 incorrect force/yaw/summary decisions, count | Includes absent yaw-capacity handling and boundary comparisons. [tests/hydrodynamics/river_currents/test_extensions.py:L167-L178] [tests/hydrodynamics/river_currents/test_extensions.py:L251-L291] |

## 6. Known failure modes and pitfalls

sweep: 27 found

Sweep record: every package Python file was read; the command below searches error swallowing, skipped records, null/empty error returns, zero/epoch defaults, numeric conversions, unit assumptions and writes. There are no `except` handlers, `continue` statements, `return None`, error-path `return []`/`return {}`, epoch defaults or empty-result cache writes in the package; local empty accumulators are not error fallbacks. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L27-L120] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L23-L118] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L30-L110] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L23-L262] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L13-L214]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(?:int|float)\(|\b1970\b|\b(?:0|0\.0|0\.05)\b|default|clamp|assum|unit|cache|write' src/digitalmodel/hydrodynamics/river_currents --glob '*.py'
```

Rows group occurrences sharing a function and symptom; finding IDs refer to this source audit, not observed field incidents. Dates refer to this audit (2026-10-26); linked issues are follow-up records rather than evidence that an incident occurred. [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L20-L22]

| Finding | Symptom / cause and pinned location | Detection / tracking |
| --- | --- | --- |
| S01 | NaN depth/exponent or speed can produce non-finite power-profile results; comparisons reject nonpositive inputs without a finite check; the depth average can ignore NaN depth because it cancels. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L71-L114] | no detection check exists; issue pending (1) |
| S02 | NaN shear/roughness/kappa or depth can pass the log-law nonpositive comparisons and propagate non-finite values. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L149-L163] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L199-L211] | no detection check exists; issue pending (1) |
| S03 | Log-law sampler accepts nonpositive total depth: it constructs heights without checking depth and calls the depth-free log function. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L247-L262] | no detection check exists; issue pending (1) |
| S04 | Power-profile heights silently clamp below bed or above surface to [0,H]. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L76-L81] | C01 tests invalid parameters, but no detection check exists for rejected out-of-range heights; issue pending (1) |
| S05 | Log heights below roughness silently become zero velocity; no upper water surface is supplied to this function. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L156-L163] | no detection check exists for unintended clipping; issue pending (1) |
| S06 | Power-profile exponent defaults to 6.0, including the depth-average API; these are user model assumptions rather than calibrated selections. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L63] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L84-L102] | C06; calibration remains #2000 |
| S07 | Log-profile and average kappa default to the module constant; positive checks do not establish applicability to supplied roughness. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L31-L32] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L117-L139] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L166-L191] | C06; source applicability issue pending (2) |
| S08 | Sampler defaults select power law, unit surface speed, exponent, shear and roughness without design-basis metadata. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L214-L238] | C01; provenance issue pending (2) |
| S09 | `_as_array`, scalar returns and sampler output cast to float; no locale normalisation or dimensional type is attached. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L35-L36] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L79-L81] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L161-L163] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L208-L211] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L262-L262] | no detection check exists for mixed units or locale ambiguity; issue pending (1) |
| S10 | Lateral quadratic is an assumed retardation distribution; zero bank speed at full retardation is permitted. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L44-L59] | C02, C09; field calibration #2000 |
| S11 | Blockage assumes rectangular channel and obstruction areas; invalid area relationships raise rather than returning an empty result. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L62-L71] | C02; model applicability #2000 |
| S12 | Outer-bank velocity bias is independent of superelevation; treating it as a predicted bend velocity would overstate the method. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L74-L83] | C02; calibration #2000 |
| S13 | Superelevation assumes constant speed and positive inner radius; ignores friction and redistribution; speed zero is a physical zero, not an error fallback. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L86-L105] | C02, C07; physical calibration #2000 |
| S14 | Corrected profile casts callback speed to float and permits zero factor; a non-finite callback raises, but no unit compatibility is verified. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L108-L120] | no detection check exists for callback units or zero-factor intent; issue pending (1) |
| S15 | `_series` casts external numeric text with `float`; comma-separated text raises, dot text is interpreted as decimal, and series units are not converted. [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L30-L38] | C03 covers invalid series, but no detection check exists for locale/units; issue pending (1) |
| S16 | Annual probability and Weibull return-level rate default to one independent sample/year; the fitter cannot establish independence or the rate from observations. [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L14-L20] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L49-L71] | C03, C09; sampling basis #2000 |
| S17 | Plotting positions and zero-location Weibull are fixed assumptions; no uncertainty interval, trend adjustment or automatic bank correction is computed. [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L14-L20] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L92-L105] | C09; observational suitability #2000 |
| S18 | Seasonal zero values are accepted, `float(value)` is stored and labels are grouped as supplied; no season vocabulary or unit metadata is checked. [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L108-L118] | no detection check exists for label/unit mismatch; issue pending (1) |
| S19 | Full-width slot drag diverges towards grounding, ignores side bypass and is always applied by `current_load`. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L8-L14] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L39-L60] | C02, C11; calibration #2002 |
| S20 | Density defaults to synthetic freshwater, direction/speed default to zero and slice count defaults to 1000; the API has no dimensional conversion. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L64] | C02, C08; design basis #2002 |
| S21 | Cardinal components are set to zero at a fixed trigonometric threshold; callback/output float conversions require supplied SI units; signed speed is accumulated, with no skipped records. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L65-L82] | C08; units issue pending (1) |
| S22 | Missing yaw capacity is not unlimited capacity: only exactly zero moment passes; force and yaw must both pass. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L85-L110] | C12; operational capacity basis #2002 |
| S23 | Example `compute` assumes SI-keyed data and separately applies section blockage plus the unvalidated local slot hypothesis; its no-slot branch treats zero moment specially when yaw capacity is absent. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L29-L76] | C11, C12; joint calibration #2002 |
| S24 | `profile_svg` assumes positive depth and nonzero positive surface speed; division by zero surface speed can embed `nan` coordinates; fixed sample count and axis origin are presentation assumptions. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L79-L95] | no detection check exists for invalid SVG coordinates; issue pending (1) |
| S25 | `_rows` and `render` round displayed values to one decimal while decisions retain full precision; presentation must not become input to force/yaw decisions. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L98-L144] | C10–C12; evidence completeness issue pending (4) |
| S26 | `build` writes basis/output hashes but no code/environment fingerprint or signed gate appendix; these are artifact writes, not empty-result cache writes. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L186-L201] | C04, C10 cover existing hashes; no detection check exists for a missing complete gate appendix; issue pending (4) |
| S27 | The example calculation accepts a basis exponent, but the SVG calls the sampler without it and therefore uses the default power-law exponent; a changed exponent can make the picture disagree with the loaded profile. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L49-L57] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L79-L83] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L214-L223] | no detection check exists for exponent agreement; issue pending (1) |

Additional provenance pitfall: the historical plan proposes an exponential profile, different default exponent, and a different bank-correction route; current code uses power/log functions and continuity corrections, so plan assertions are not current implementation evidence. [docs/plans/2026-05-05-issue-493-river-current-profiles.md:L18-L24] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L49] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L117-L127] [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L62-L71]

## 7. Lessons log

| Date | Record | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-26 | Initial documentation | A deterministic synthetic decision can reverse when an unvalidated multiplier is removed; retain the sensitivity alongside the decision. [tests/hydrodynamics/river_currents/test_extensions.py:L231-L248] | R2, C11 and S19 separate mathematical consistency from calibration | Not assigned; issue pending (4) |
| 2026-10-26 | Initial documentation | The bend integral needs cancellation-resistant evaluation; the gentle-bend and overflowing-outer-radius cases independently check it. [tests/hydrodynamics/river_currents/test_bank_effects.py:L44-L68] | C07 preserves both numerical edge cases | Not assigned; issue pending (4) |

## 8. Ownership

Review assignment: engineering calculations quality manager; deputy role: engineering calculations deputy quality manager; producing role: calculation documentation contributor. Escalate method applicability, tolerances and conflicting classifications to the family quality manager; the maintainer handles remaining coverage and appointment decisions. (AQG r0, Analysis QA gates, revision 0, section 11.)

UNVERIFIED: named appointments for the quality and deputy roles, and the shared-log destination, are not established by package evidence; issue pending (4).

## 9. Dependencies and implementation inventory

| Dependency / interface | Role and limit |
| --- | --- |
| NumPy | Array conversion, clipping, logarithms and sampled heights in the profile functions. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L23-L36] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L156-L163] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L247-L262] |
| Python standard library | `math`, dataclasses, sequences and callbacks implement the bank/extreme/force core; JSON, hashing and filesystem output implement the analysis builder. [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L27-L28] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L23-L27] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L13-L26] |
| `VesselProperties` | Shared vessel data include metre geometry and displacement in tonnes, with positive geometry validation. [src/digitalmodel/hydrodynamics/models.py:L84-L120] |
| `OCIMFLoading.current_load` | Existing approximate unit-speed load route, called by the draft integrator rather than copied into it. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L58-L82] |
| Synthetic basis and committed analysis artifacts | Example data revision 2; `build` hashes the supplied basis and generated output bytes. [examples/hydrodynamics/river_currents/basis.json:L1-L15] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L186-L201] |
| Towing consumer | Historical towing plan proposes consuming a callable river profile; this is proposed integration, not proof of a current production consumer. [docs/plans/2026-05-05-issue-487-towing-marine-operations.md:L20-L24] [docs/plans/2026-05-05-issue-487-towing-marine-operations.md:L35-L38] |

Implementation-map audit record: branch `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; 13 map families inspected; 0 implementation rows have a `path` inside the header package; no subpackage exclusions. The section-9 inventory is therefore empty and the counted-family grep is `NOT_APPLICABLE`, because no map family has an in-package implementation row. This result concerns the specified map revision, not later map content.

Reproduction: load that map revision with `yaml.safe_load`, iterate `families` in file order, and retain each `implementations` row whose `path.startswith('src/digitalmodel/hydrodynamics/river_currents/')`; the retained row count is 0. Rejected candidates do not constitute implementation rows. If rows are later added, rank families by in-package row count (stable map order for ties), take up to three, search pinned package Python files with each exact Python `re` signal case-sensitively, omit signals matching `hydrodynamics/river_currents` case-insensitively, and classify each hit by enclosing definition; wrappers, data and docstrings are helpers. At this audit revision the summed additional-implementation count is 0 across 0 counted families.

Map follow-up: register the section-3 families for bed-referenced power/log laws and depth averages, lateral/blockage/bend corrections, probability-plot Weibull/Gumbel fits and annual return levels, seasonal summaries, signed draft integration, slot screening and pull/yaw capacity checks; add explicit package rows to the existing wind/current family before treating that family as counted; issue pending (3), with supplemental extreme statistics tracked by #2620. [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L262] [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L44-L120] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L30-L118] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L39-L110]

Further repository search, including `scripts/`, used the following expression on all tracked Python files; these findings are map follow-up, not counted-family misses:

```sh
git grep -n -E 'power_law_profile|log_law_profile|fit_weibull|fit_gumbel|bend_superelevation|shallow_drag_factor|river_velocity_profile|class OCIMFLoading|def current_load|def calculate_current_forces' <code-revision> -- '*.py'
```

| Further implementation / route | Classification and evidence |
| --- | --- |
| `orcaflex/postprocessor.py` fit functions | Further extreme-fit implementations outside this package: maximum-likelihood Gumbel and three-parameter Weibull differ from the probability-plot estimators; map follow-up at the selected revision, #2620. [src/digitalmodel/orcaflex/postprocessor.py:L177-L221] [src/digitalmodel/orcaflex/postprocessor.py:L223-L268] |
| `subsea/pipeline/free_span/weibull_current.py` | Further piecewise tail fit using supplied exceedance probabilities; not the package's observation-rank fit; map follow-up at the selected revision, #2620. [src/digitalmodel/subsea/pipeline/free_span/weibull_current.py:L51-L115] |
| `orcaflex/environment.py` | Further depth-below-surface profile generator; keep its coordinate convention separate from the bed-referenced API; map follow-up, issue pending (3). [src/digitalmodel/orcaflex/environment.py:L63-L120] |
| `hydrodynamics/ocimf_loading.py` | Listed upstream force route, delegated to by this package; helper relationship at the package boundary. [src/digitalmodel/hydrodynamics/ocimf_loading.py:L109-L186] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L65-L82] |
| Both marine-operations environmental-loading packages | Further current-force methods outside the package; implementations require separate map reconciliation, issue pending (3). [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L595-L655] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L609-L669] |
| Package re-exports, `corrected_profile`, sampled generator and example orchestration | Helpers delegating to section-3 implementations, not additional unlisted implementations. [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L25-L55] [src/digitalmodel/hydrodynamics/river_currents/bank_effects.py:L108-L120] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L247-L262] [src/digitalmodel/hydrodynamics/river_currents/examples.py:L29-L76] |

## 10. Coverage and open items

Read-set record: all six package Python files were read in full (`__init__.py`, `profiles.py`, `bank_effects.py`, `extremes.py`, `vessel_forces.py`, `examples.py`), plus all four test files (`test_profiles.py`, `test_bank_effects.py`, `test_extensions.py`, `test_ci_gate.py`); there are no nested code subpackages in this pin. The source and test citations in sections 1–9 identify their implementation and assertion ranges.

Documentation discovery record: `rg -l -i 'river.currents|river.current profile|river_velocity_profile|bend_superelevation|shallow_drag_factor' docs -g '*.md'` found the three Markdown documents listed below; no package README exists at the code pin. The basis and generated HTML/results/manifest were also read; regression R1 compares all generated artifact bytes. [tests/hydrodynamics/river_currents/test_extensions.py:L193-L228]

| Discovered documentation | Treatment |
| --- | --- |
| `docs/plans/2026-05-05-issue-493-river-current-profiles.md` | Historical proposal, includes unrealised interfaces and a fixture with an identifying name; no identifying fixture name or proposed field-derived values are repeated here; codenaming and current-status follow-up, issue pending (2). [docs/plans/2026-05-05-issue-493-river-current-profiles.md:L16-L24] |
| `docs/plans/2026-05-05-issue-487-towing-marine-operations.md` | Proposed consumer, not a current call contract; source-status follow-up, issue pending (2). [docs/plans/2026-05-05-issue-487-towing-marine-operations.md:L20-L24] |
| `docs/field-development/subsea-production-systems-mapping.md` | Historical no-coverage statement conflicts with the current profile functions; current-status follow-up, issue pending (2). [docs/field-development/subsea-production-systems-mapping.md:L97-L108] [src/digitalmodel/hydrodynamics/river_currents/profiles.py:L39-L81] |

The calculation citation contract was readable during preparation; method provenance remains bibliographic/docstring evidence rather than emitted `Citation` records. The builder's manifest records basis and output hashes only. [src/digitalmodel/hydrodynamics/river_currents/examples.py:L186-L201]

| Open item | Responsible role | Tracking and completion criterion |
| --- | --- | --- |
| Non-finite inputs, log sampler depth, zero-speed SVG and explicit units/locale policy (S01–S05, S09, S14–S15, S18, S21, S24, S27) | Maintainer | issue pending (1); add failing-then-passing invalid-input and output-finiteness tests without changing valid SI results |
| UNVERIFIED source applicability and exact locators; no checked original publications or licensed clause text; no established DNV edition mismatch; missing Citation sidecars, stale plans and 1 discovery file with an identifying fixture name | Maintainer | issue pending (2); verify editions, clause titles and source applicability, reconcile plan status and codename the fixture lead |
| Empty map inventory at the selected revision and method families / outside-package routes in section 9 | Maintainer | #2620 for supplemental extreme statistics; issue pending (3) for remaining methods; add package rows, review all algorithm definitions and retain separate fitting/coordinate conventions |
| UNVERIFIED named quality/deputy appointments and shared-log destination; signed QA appendix and code/environment fingerprints absent from example output | Maintainer | issue pending (4); assign review roles and shared log; add traceable gate evidence with 0 missing required records |
| Tidal reversal, unsteady hydraulics and measured extreme-fit uncertainty / independence | Maintainer | #2000; evidence required before extending current applicability. [src/digitalmodel/hydrodynamics/river_currents/__init__.py:L20-L22] [src/digitalmodel/hydrodynamics/river_currents/extremes.py:L14-L20] |
| Calibrated shallow-water/hull coefficients, joint confinement/slot use, tug resistance/thrust and passing-vessel interaction | Maintainer | #2002; no operational accuracy claim until an independent calibrated benchmark is checked. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L8-L27] [examples/hydrodynamics/river_currents/basis.json:L130-L137] |

No client-derived data are included; the basis explicitly declares synthetic inputs and no measured record or hull calibration. A private sidecar is unnecessary for these synthetic cases. [examples/hydrodynamics/river_currents/basis.json:L15-L15]
