# Marine operations: top-level knowledge pack

| Header | Value |
|---|---|
| Module / repository | `marine_ops` / `digitalmodel` |
| Package path | `src/digitalmodel/marine_ops/` |
| Scope | Direct Python files; sub-packages retain separate qualification |
| Pack revision / status | 1 / DRAFT — unverified screening inputs |
| Current code reference | Functions in the same Git revision as this pack |
| Prior discovery code pin | `7aad603be1c6136106dbb2381ce5096b050654f8` |
| Prior implementation-map pin | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Analysis family | Engineering calculations |
| Required reviewer | Independent engineering calculations quality manager |
| Deputy and shared registration | [2771](https://github.com/vamseeachanta/digitalmodel/issues/2771) |

## 1. Purpose and scope

`operation_envelope()` returns alpha-reduced **forecast Hs limits** for linear
spectral screening, taking the minimum across every requested motion criterion
and heading. `envelope_for_operations()` applies that calculation to each key.
The catalogue is unverified screening input, not a standards-derived design
allowable. The [calculation basis](envelope-basis.md) defines equations B1–B5 and
table B6. Audit disposition: [2747](https://github.com/vamseeachanta/digitalmodel/issues/2747).

## 2. Inputs and outputs

`operation_envelope()` validates at calculation entry, including custom
`Operation` objects. Unknown DOFs, missing required components, empty grids,
non-finite arrays, negative magnitudes and inconsistent shapes raise `ValueError`.
Magnitude shape is frequency by heading; ambiguous square grids are rejected.
Frequency units shall be rad/s; magnitude units shall be m/m for translations
and deg/m for rotations; criterion units shall be m or deg respectively.
Declared units survive `RAOSet.from_dict()` and are rejected rather than converted.

Alpha shall be in `(0,1]`; limits, Tp, reference Hs and cap shall be finite and
positive; gamma shall be finite and at least 1. `None` selects default Tp or
headings, but an empty supplied grid raises. The default Tp grid remains
4–20 s in 0.5 s steps, reference Hs 1 m, gamma 3.3 and cap 20 m.
These are numerical screening defaults, not a qualification envelope.

`EnvelopePoint` carries `evaluated`, `status`, `extrapolated_energy_fraction`
and `captured_variance_fraction`. An evaluated cap is labelled `capped`, with
DOF `cap` and null heading. `hs_forecast_limit_m` names the criterion explicitly;
`hs_limit_m` remains a compatibility alias. `EnvelopeResult` carries
`criterion_type`, `alpha_source` and `citation_sidecar`. Both catalogue and
custom inputs remain `unverified_screening`; no table or clause is invented.

`EnvelopeResult.require_evaluated()`, `to_dict()`, `hs_limit_at()`,
`plot_envelopes()` and the installation pamphlet `compute_envelopes()` reject
unevaluated results. Lookup at an absent period returns `None` for valid results.
No rounding is applied to the calculation result; presentation controls rounding.

## 3. Method and governing basis

Equations B1 and B2 define response integration and linear forecast scaling.
`envelope_numerics.spectrum()` uses a peak-scaled `.01–8 omega_peak` grid and
records the pre-normalization variance fraction (B3), which shall exceed 99%.
`extrapolated_fraction()` integrates boundary cells and rejects RAOs covering
less than 99% of retained wave energy (B4). The maximum missing fraction over
required DOFs is reported; endpoint continuation is limited to that fraction.
`heading_column()` uses circular distance and half the smallest observed step
(B5). Symmetry is not inferred. Unsupported directions raise.

Catalogue values are retained unchanged in table B6. DNV compliance is not
established: alpha duration/design-Hs/forecast-table provenance remains open
under [2769](https://github.com/vamseeachanta/digitalmodel/issues/2769).
The upstream spectrum's DNV-RP-C205 section 3.5.2 attribution is not independently
qualified here. The basis supplies explicit equations and public corroboration,
not an assertion that a licensed standard was checked.

## 4. Worked examples and executable tests

All fixtures are synthetic arrays. Unit heave gain gives significant amplitude
0.5 m at Hs=1 m, hence forecast limits 5.4, 3.6, 2.55 and 4.5 m for the four
catalogue operations. Gain two at 90 degrees gives 2.7 m for transit. The
frequency-dependent PM low-pass case gives 10.0873 m at Tp=4 s, within 1% of
the independently integrated value in B3. This is a quadrature check, not a
measured-response qualification.

Dedicated run: `PYTHONPATH=src python -m pytest tests/marine_ops/test_operation_envelope.py tests/marine_ops/test_envelope_validation.py tests/marine_ops/test_envelope_pack.py`.
The environment shall prevent commercial solver API loading. No native solve
is needed. A broader pamphlet test with an insufficient RAO band shall fail
loudly until qualified wider-band input is supplied; numerical gates shall not
be relaxed to preserve a historical output.

## 5. QA gates

G0 covers input, unit and citation-status checks; G1 covers package calculation
entry; G2 covers independent numbers; G5 covers result serialization and plots.
G3 (time simulation) and G4 (native mesh/solver execution) do not apply to this
spectral consumer. Every check below calls or asserts a named package function
or output. Test names resolve in the dedicated files above.

| Check ID | Package function or output | Pass criterion | Executable check |
|---|---|---|---|
| C0-1 / G0 | `available_operations()` | Exactly four specified keys | `test_available_operations_are_the_four_requested` |
| C0-2 / G0 | `resolve_operation()` | Unknown key raises KeyError | `test_unknown_operation_raises` |
| C0-3 / G0 | `OPERATIONS` limits and `EnvelopeResult.to_dict()` | 12 limits, zero unit mismatches; explicit screening and forecast fields | `test_forecast_sidecar_and_catalogue_units` |
| C0-4 / G0 | `RAOSet.from_dict()` and `operation_envelope()` | rad/m declaration preserved then rejected; Hz rejected | `test_declared_units_survive_roundtrip_and_are_rejected` |
| C0-5 / G0 | `operation_envelope()` | Invalid units, DOFs, ranges, grids and non-finite arrays raise | `test_invalid_criterion`, `test_invalid_calculation_inputs`, `test_invalid_alpha`, `test_invalid_limit`, `test_nonfinite_rao_is_not_silently_dropped`, `test_bad_or_ambiguous_shape` |
| C1-1 / G1 | `operation_envelope()` point status | Evaluated full-band result 5.4 m, relative error at most 0.1% | `test_full_band_retains_hand_value_and_records_energy` |
| C2-1 / G2 | `operation_envelope()` constant gain | 5.4 m, relative error at most 0.1% | `test_constant_heave_rao_gives_closed_form_limit` |
| C2-2 / G2 | `operation_envelope()` scaling | Ratios 2 and 1, relative error at most 0.1% | `test_limit_scales_linearly_with_criterion_and_alpha` |
| C2-3 / G2 | `operation_envelope()` governing heading | 2.7 m at 90 degrees; circular 350 selects zero; unsupported 270 raises | `test_worst_case_heading_governs`, `test_circular_heading_and_incomplete_heading_domain` |
| C2-4 / G2 | `operation_envelope()` ordering | Heavy-lift limit below transit | `test_stricter_operation_gives_lower_envelope` |
| C2-5 / G2 | `operation_envelope()` frequency-dependent response | 10.0873 m within 1%; captured variance 0.99969487 within 1e-5 | `test_short_period_low_pass_hand_integral` |
| C2-6 / G2 | `operation_envelope()` coverage | 0.2–1.2 rad/s at Tp=4 s raises | `test_inadequate_band_is_rejected` |
| C5-1 / G5 | `EnvelopeResult.to_dict()` and `hs_limit_at()` | Independent 3.6 m within 0.1%, expected keys/values, absent 11 s returns None | `test_envelope_to_dict_roundtrips_points` |
| C5-2 / G5 | `envelope_for_operations()` and `plot_envelopes()` | 5.4/3.6/2.55/4.5 m within 0.1%, heave governs each, one image exists | `test_envelope_for_all_operations_and_plot` |
| C5-3 / G5 | `EnvelopeResult`, `plot_envelopes()`, `compute_envelopes()` | Unevaluated points rejected by each consumer | `test_consumers_reject_unevaluated_point` |

The following historical inventory retains the original source pins and search
evidence. Its pre-fix symptoms are not current runtime claims. Current behavior
and gates are defined in sections 1–5; remaining work is tracked by the concrete
issues in the final register.

## 6. Historical failure inventory (revision 0)

sweep: 16 found

Sweep scope: the two direct-package Python files; sub-packages are excluded from this pack's failure inventory. The following command is reproduced exactly for reruns; all matching paths were inspected, including non-error sentinels and numeric casts.

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|\b(int|float)\(|unit|cache|default' src/digitalmodel/marine_ops/*.py
```

| ID | Symptom / cause at the code pin | Detection check in section 5 | Disposition |
|---|---|---|---|
| S01 | `hs_limit_at()` returns `None` for an absent period; strict lookup threshold can surprise off-grid callers. [src/digitalmodel/marine_ops/operation_envelope.py:L145-L149] | C5-1 covers the present period only; no detection check exists for absent/off-grid periods | Intended sentinel; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) for missing negative coverage |
| S02 | Directly constructed `EnvelopeResult` defaults to an empty point list; serialization accepts it without qualification. [src/digitalmodel/marine_ops/operation_envelope.py:L138-L165] | no detection check exists | Empty-result validity; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S03 | Empty `tp_range_s` silently selects the full default grid via `or`. [src/digitalmodel/marine_ops/operation_envelope.py:L184-L190] | no detection check exists | Input intent lost; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S04 | No candidate below the cap leaves capped Hs, `none` DOF and NaN heading; a large physical limit and an unevaluated point share this state. [src/digitalmodel/marine_ops/operation_envelope.py:L190-L214] | no detection check exists | Missing coverage/status distinction; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S05 | Missing or misspelled DOF resolves to `None` and is skipped. [src/digitalmodel/marine_ops/operation_envelope.py:L194-L197] [src/digitalmodel/marine_ops/operation_envelope.py:L265-L266] | no detection check exists | Partial criteria silently accepted; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S06 | Nonresponding DOF is skipped when response is nonpositive. [src/digitalmodel/marine_ops/operation_envelope.py:L198-L201] | C2-1 covers intended zero-response axes only; no detection check exists for invalid response | Preserve valid zero response, distinguish invalid data; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S07 | Nonpositive zeroth moment becomes `0.0`, obscuring a negative variance as a nonresponding axis. [src/digitalmodel/marine_ops/operation_envelope.py:L283-L288] | no detection check exists | Numerical validation gap; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S08 | All plotting import exceptions become `None`; initialization errors and an absent optional dependency are indistinguishable. [src/digitalmodel/marine_ops/operation_envelope.py:L237-L243] | C5-2 covers successful plotting only; no detection check exists for the fallback | Observable fallback/status gap; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S09 | Empty plot input leaves an empty metadata string and generates an empty figure. [src/digitalmodel/marine_ops/operation_envelope.py:L247-L260] | no detection check exists | Empty-result validity; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S10 | `operations=[]` returns `{}`; repeated operation keys overwrite earlier results. [src/digitalmodel/marine_ops/operation_envelope.py:L220-L231] | C5-2 covers four distinct default keys only; no detection check exists for empty/duplicate keys | Explicit empty semantics and duplicate detection needed; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S11 | External frequency, heading and magnitude arrays are cast to float without locale-aware parsing; malformed text raises and non-finite values are not rejected locally. [src/digitalmodel/marine_ops/operation_envelope.py:L269-L280] | no detection check exists | No thousands/decimal separator contract; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S12 | Requested heading uses nearest absolute angular difference, without circular wrap or a maximum mismatch check. [src/digitalmodel/marine_ops/operation_envelope.py:L275-L280] | C2-3 covers an exact available heading only; no detection check exists for wrap/off-grid targets | Heading coverage limit; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S13 | Frequencies outside the supplied RAO band use endpoint-clamped magnitude. [src/digitalmodel/marine_ops/operation_envelope.py:L273-L280] | no detection check exists | Extrapolation hidden by interpolation; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S14 | Unit strings, origin and phase are not checked by the response calculation. [src/digitalmodel/marine_ops/operation_envelope.py:L46-L63] [src/digitalmodel/marine_ops/operation_envelope.py:L269-L288] | no detection check exists | U1; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S15 | Limit, alpha, reference Hs, gamma and cap have no local range validation; float conversion and rounding of output do not reject NaN. [src/digitalmodel/marine_ops/operation_envelope.py:L46-L63] [src/digitalmodel/marine_ops/operation_envelope.py:L168-L214] | C2-2 covers valid scaling only; no detection check exists for invalid ranges | Numeric acceptance gap; [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| S16 | Unknown catalogue key is caught and re-raised with context, rather than swallowed; direct `Operation` objects bypass catalogue checks. [src/digitalmodel/marine_ops/operation_envelope.py:L119-L127] | C0-2 detects unknown-key rejection; no detection check exists for malformed objects | Re-raise is intentional; object validation gap, [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |

No cache write or epoch fallback occurs in the scoped source; the entire implementation consists of the catalogue, result objects, spectral envelope helpers and plotting function. [src/digitalmodel/marine_ops/__init__.py:L1-L1] [src/digitalmodel/marine_ops/operation_envelope.py:L1-L288]

| Additional limitation | Detection / issue | Evidence |
|---|---|---|
| Standards basis without edition, clause or sidecar | no detection check exists; U2; [2769](https://github.com/vamseeachanta/digitalmodel/issues/2769) | [src/digitalmodel/marine_ops/operation_envelope.py:L55-L112] [src/digitalmodel/marine_ops/operation_envelope.py:L151-L165] |
| Vessel-level screening is not a crane-tip or relative-motion check | C2-1 verifies only constant-gain vessel response; no detection check exists for geometry-dependent correctness; #1359 | [src/digitalmodel/marine_ops/operation_envelope.py:L21-L24] [docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md:L10-L23] |
| Upstream normalization defect must not be transferred between implementations | Top-level path calls normalized `WaveSpectra.jonswap`; the installation-local spectrum is a different implementation; #2543 remains an adjacent-method caution | [src/digitalmodel/marine_ops/operation_envelope.py:L184-L190] [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [src/digitalmodel/hydrodynamics/wave_spectra.py:L93-L96] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L171-L201] |
| QA provenance and signed appendix not produced by `to_dict` | C5-1 checks only point serialization; no detection check exists for appendix completeness; #1556 | [src/digitalmodel/marine_ops/operation_envelope.py:L151-L165] |

No numbered incident was specified for the top-level scope; issue searches were discovery leads, and the rows above distinguish confirmed local behavior from adjacent-method cautions.

## 7. Lessons log

Append-only log; future entries shall retain evidence and issue references without including execution identifiers.

| Date | Entry | Lesson / evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Initial pack revision | A capped limit can coexist with an unset governing DOF and NaN heading; it is not proof of evaluated motion coverage. [src/digitalmodel/marine_ops/operation_envelope.py:L190-L214] | Sections 2, 5 and 6 distinguish smoke checks from qualification | Not registered; [2771](https://github.com/vamseeachanta/digitalmodel/issues/2771) |

## 8. Maintenance and review

Required reviewer: engineering calculations family quality manager, on a provider independent of the producer. Required deputy role: deputy engineering calculations quality manager; the assignment remains unverified. Required producing discipline: marine operations calculations. Ambiguous criteria, input conventions and tolerances shall be referred to the family quality manager; cross-family scope shall be referred to the maintainers.

UNVERIFIED: U4 — a deputy quality manager assignment and shared-log entry are not established in this repository; nomination and registration remain required, [2771](https://github.com/vamseeachanta/digitalmodel/issues/2771).

## 9. Dependencies and other implementations

### Direct dependencies and scope boundary

The spectral inputs flow through `WaveSpectra.jonswap`, `compute_response_spectrum`, `spectral_moments` and `significant_amplitude`; the RAO container provides magnitude, frequency and heading arrays. [src/digitalmodel/marine_ops/operation_envelope.py:L184-L202] [src/digitalmodel/marine_ops/operation_envelope.py:L269-L288] [src/digitalmodel/hydrodynamics/diffraction/output_schemas.py:L116-L147]

The installation-pamphlet calculation is a downstream wrapper: `compute_envelopes` builds an RAO set, delegates to `operation_envelope` and serializes its results. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L365-L394]

Excluded sub-packages, each retaining its own scope: `artificial_lift`, `ct_hydraulics`, `fowt_quasi_static`, `installation`, `marine_analysis`, `marine_engineering`, `reservoir` and `vessel_db`. Their package evidence files and existing pack locators are listed below; no solver version is qualified by the direct-package tests.

| Sub-package evidence file | Scope evidence |
|---|---|
| `src/digitalmodel/marine_ops/artificial_lift/README.md` | [src/digitalmodel/marine_ops/artificial_lift/README.md:L1-L3] |
| `src/digitalmodel/marine_ops/ct_hydraulics/__init__.py` | [src/digitalmodel/marine_ops/ct_hydraulics/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/fowt_quasi_static/__init__.py` | [src/digitalmodel/marine_ops/fowt_quasi_static/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/installation/__init__.py` | [src/digitalmodel/marine_ops/installation/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/marine_analysis/__init__.py` | [src/digitalmodel/marine_ops/marine_analysis/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/marine_engineering/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/reservoir/__init__.py` | [src/digitalmodel/marine_ops/reservoir/__init__.py:L1-L3] |
| `src/digitalmodel/marine_ops/vessel_db/__init__.py` | [src/digitalmodel/marine_ops/vessel_db/__init__.py:L1-L3] |

### Counted family and exact search

Map source: `docs/registry/implementation-map.yaml` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, with embedded data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; row identities and relation labels below are map inventory, not independent numerical qualification.

`wave-spectra` is the only family with direct-package rows: 2 in-package rows, ranked first; the complete family has 141 inventory rows. No tie or three-family truncation is needed. [docs/registry/implementation-map.yaml:L14262-L14288]

All three `search_signals` are retained: none matches `marine_ops` when the package path is compared case-insensitively. The grep itself uses case-sensitive Python `re.search` on the two `.py` files directly in the package, at the code pin. The following are the exact pattern strings from the map, without normalization. [docs/registry/implementation-map.yaml:L13593-L13599]

```text
jonswap|pierson.moskowitz|bretschneider|ochi.hubble|torsethaugen|gaussian.swell|\bISSC\b|spectral_moment|normalize_to_significant|spectral_density|gamma\s*\*\*|S_PM|S_pm|np\.exp|math\.exp|spectral_density|spectrum\s*\*|sigma\s*=
spectrum|spectral.density|wave.spectra|JONSWAP
0\.07|0\.09|0\.287|peak_enhancement
```

| Enclosing definition | Matching lines at code pin | Classification / miss count |
|---|---|---|
| Module-level statements | 13, 36, 38, 40 | Docstring and imports; no computation, 0 misses. [src/digitalmodel/marine_ops/operation_envelope.py:L8-L40] |
| `operation_envelope` | 190 | Listed implementation, 0 misses. [src/digitalmodel/marine_ops/operation_envelope.py:L168-L217] |
| `_significant_response` | 284–285 | Listed implementation; delegates response integration, 0 misses. [src/digitalmodel/marine_ops/operation_envelope.py:L283-L288] |

A8 result: complete map inventory below, 1 counted family, 2 in-scope implementation rows and 0 further second implementations, satisfying the at-most-1 criterion. Any defect-classification disagreement shall be resolved by the family quality manager.

### Complete mapped family inventory

The following table retains every row in map order, including tests, examples, helpers and out-of-package implementations; relation means the map's recorded classification and does not establish interchangeability.

| Row | Mapped path | Symbol (`<module>` denotes statements) | Map relation / package scope | Map evidence at map revision |
|---|---|---|---|---|
| 1 | `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | independent; outside scope | [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | independent; outside scope | [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | example; outside scope | [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | example; outside scope | [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example; outside scope | [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | example; outside scope | [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | `scripts/_seed_eq_mock.py` | `mock_time_series` | independent; outside scope | [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | `scripts/benchmark/extract_pdf_plots.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | `scripts/benchmark/validate_owd_vs_spec.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | `scripts/extract_property_inventory.py` | `build_dormancy_rules` | independent; outside scope | [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | `scripts/generate_env_files.py` | `create_wave_file` | independent; outside scope | [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | `scripts/generate_env_files.py` | `main` | independent; outside scope | [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | `scripts/ingest_orcina_help.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `<module>` | independent; outside scope | [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | independent; outside scope | [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | independent; outside scope | [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | independent; outside scope | [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | independent; outside scope | [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | independent; outside scope | [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | independent; outside scope | [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | independent; outside scope | [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | independent; outside scope | [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | independent; outside scope | [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | independent; outside scope | [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | independent; outside scope | [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | wrapper; outside scope | [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | independent; outside scope | [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | independent; outside scope | [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | independent; outside scope | [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | independent; outside scope | [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | independent; outside scope | [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | independent; outside scope | [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent; outside scope | [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | independent; outside scope | [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | independent; outside scope | [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | copy; outside scope | [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14250-L14261] |
| 55 | `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | independent; in scope | [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | independent; in scope | [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | independent; outside scope | [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | independent; outside scope | [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | independent; outside scope | [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | independent; outside scope | [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | independent; outside scope | [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | independent; outside scope | [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | independent; outside scope | [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | wrapper; outside scope | [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | independent; outside scope | [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | independent; outside scope | [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | independent; outside scope | [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | independent; outside scope | [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | independent; outside scope | [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | independent; outside scope | [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | wrapper; outside scope | [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | independent; outside scope | [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | copy; outside scope | [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | independent; outside scope | [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | independent; outside scope | [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | independent; outside scope | [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | copy; outside scope | [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | copy; outside scope | [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | independent; outside scope | [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent; outside scope | [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | independent; outside scope | [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent; outside scope | [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent; outside scope | [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent; outside scope | [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent; outside scope | [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | independent; outside scope | [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | independent; outside scope | [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | independent; outside scope | [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | independent; outside scope | [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | independent; outside scope | [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | independent; outside scope | [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | independent; outside scope | [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | independent; outside scope | [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | `src/digitalmodel/workflows/installation_full_report.py` | `_design` | independent; outside scope | [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | independent; outside scope | [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | independent; outside scope | [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | independent; outside scope | [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | independent; outside scope | [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | independent; outside scope | [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | test_helper; outside scope | [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` | caller; outside scope | [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range.ordinate` | independent; outside scope | [docs/registry/implementation-map.yaml:L15362-L15377] |

### Repository-wide discovery and map follow-up

Repository search included `scripts/`, source, tests and docs, using the following command without importing or executing discovered implementations:

```bash
rg -n 'operation_envelope|operability envelope' scripts src --glob '*.py'
```

| Additional route | Classification / consequence |
|---|---|
| `installation.operability.hs_limit_for_criterion`, `compute_operability` | Independent Hs search and crane-tip criteria aggregation in an excluded sub-package; not the same response/criteria contract. [src/digitalmodel/marine_ops/installation/operability.py:L31-L75] [src/digitalmodel/marine_ops/installation/operability.py:L108-L176] |
| `drilling_riser.operability.significant_wave_height_limit` | Independent scalar response-to-limit formula outside the package; convention equivalence is not established. [src/digitalmodel/drilling_riser/operability.py:L10-L32] |
| `OperabilityAnalyzer.generate_operability_envelope` | Polar tension plotting against load limits outside the package, rather than an Hs–Tp spectral limit solver. [src/digitalmodel/solvers/orcaflex/operability_analysis.py:L83-L122] |
| `scripts/drilling_riser/build_operability_explorer.py`, `build_results` | Delegates per-cell decisions to `screen_operability` and serializes the atlas; presentation helper, not another spectral-envelope solver. [scripts/drilling_riser/build_operability_explorer.py:L43-L80] |

Map follow-up, [2770](https://github.com/vamseeachanta/digitalmodel/issues/2770): add or classify envelope criterion aggregation, RAO heading/frequency interpolation, result lookup and envelope presentation families. The latter helper methods and map families with zero direct-package rows are not counted by A8; independent methods outside the package are map follow-up, not A8 misses. [src/digitalmodel/marine_ops/operation_envelope.py:L145-L165] [src/digitalmodel/marine_ops/operation_envelope.py:L190-L214] [src/digitalmodel/marine_ops/operation_envelope.py:L234-L280]

## 10. Coverage and open items

### Read coverage

Direct Python files read in full: `__init__.py` and `operation_envelope.py`; dedicated tests read in full: `tests/marine_ops/test_operation_envelope.py`. [src/digitalmodel/marine_ops/__init__.py:L1-L1] [src/digitalmodel/marine_ops/operation_envelope.py:L1-L288] [tests/marine_ops/test_operation_envelope.py:L1-L148]

Dependency excerpts, downstream calculation, adjacent implementation routes and mapped rows are cited in sections 2, 3 and 9; reading an inventory row does not establish numerical correctness of that implementation.

The documentation discovery command was `rg -l 'marine_ops|operation_envelope|OperationEnvelope' docs --glob '*.md'`; the package README/doc discovery also enumerated sub-package READMEs and their existing packs. Identifying locators are withheld for codenaming under [2770](https://github.com/vamseeachanta/digitalmodel/issues/2770); no client-derived values or private sidecar content are included.

| Documentation file read or discovered | Evidence at code pin |
|---|---|
| `docs/RELOCATED-DATA.md` | [docs/RELOCATED-DATA.md:L1-L1] |
| `docs/data/OCIMF_CORPUS_README.md` | [docs/data/OCIMF_CORPUS_README.md:L1-L1] |
| `docs/domains/artificial_lift/rod-pump-rp11l.md` | [docs/domains/artificial_lift/rod-pump-rp11l.md:L1-L1] |
| `docs/domains/cathodic_protection/_index.md` | [docs/domains/cathodic_protection/_index.md:L1-L1] |
| `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` | [docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md:L1-L1] |
| `docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md` | [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L1-L1] |
| `docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md` | [docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md:L1-L1] |
| `docs/domains/marine-engineering/RAO_EXTRACTION_VERIFICATION.md` | [docs/domains/marine-engineering/RAO_EXTRACTION_VERIFICATION.md:L1-L1] |
| `docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md` | [docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md:L1-L1] |
| `docs/domains/marine-engineering/marine_excel_analysis_report.md` | [docs/domains/marine-engineering/marine_excel_analysis_report.md:L1-L1] |
| `docs/domains/marine-engineering/marine_excel_analysis_summary.md` | [docs/domains/marine-engineering/marine_excel_analysis_summary.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/DOCUMENTATION_CORRECTIONS.md` | [docs/domains/marine-engineering/python-code-passing-ship/DOCUMENTATION_CORRECTIONS.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/README.md` | [docs/domains/marine-engineering/python-code-passing-ship/README.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/api_reference.md` | [docs/domains/marine-engineering/python-code-passing-ship/api_reference.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/configuration_guide.md` | [docs/domains/marine-engineering/python-code-passing-ship/configuration_guide.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/integration_guide.md` | [docs/domains/marine-engineering/python-code-passing-ship/integration_guide.md:L1-L1] |
| `docs/domains/marine-engineering/unified_rao_reader_guide.md` | [docs/domains/marine-engineering/unified_rao_reader_guide.md:L1-L1] |
| `docs/domains/marine_ops/ct_hydraulics/README.md` | [docs/domains/marine_ops/ct_hydraulics/README.md:L1-L1] |
| `docs/domains/marine_ops/marine_analysis/README.md` | [docs/domains/marine_ops/marine_analysis/README.md:L1-L1] |
| `docs/domains/marine_ops/marine_engineering/README.md` | [docs/domains/marine_ops/marine_engineering/README.md:L1-L1] |
| `docs/domains/mooring/MOORING_LINE_TYPES.md` | [docs/domains/mooring/MOORING_LINE_TYPES.md:L1-L1] |
| `docs/domains/orcaflex/subsea/jumper/installation/README.md` | [docs/domains/orcaflex/subsea/jumper/installation/README.md:L1-L1] |
| `docs/domains/plate-buckling/buckling-code-review-2026-06-26.md` | [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L1-L1] |
| `docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md:L1-L1] |
| `docs/domains/project-docs/MIGRATION_GUIDE.md` | [docs/domains/project-docs/MIGRATION_GUIDE.md:L1-L1] |
| `docs/domains/project-docs/REORGANIZATION_SUMMARY.md` | [docs/domains/project-docs/REORGANIZATION_SUMMARY.md:L1-L1] |
| `docs/domains/project-docs/phase1-api-reference.md` | [docs/domains/project-docs/phase1-api-reference.md:L1-L1] |
| `docs/domains/project-docs/phase1-implementation-report.md` | [docs/domains/project-docs/phase1-implementation-report.md:L1-L1] |
| `docs/domains/risers/scr-slwr-coverage-2026-10.md` | [docs/domains/risers/scr-slwr-coverage-2026-10.md:L1-L1] |
| `docs/domains/tubulars-casing-rod-validation-2026-06-28.md` | [docs/domains/tubulars-casing-rod-validation-2026-06-28.md:L1-L1] |
| `docs/maps/digitalmodel-operator-map.md` | [docs/maps/digitalmodel-operator-map.md:L1-L1] |
| `docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md` | [docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md:L1-L1] |
| `docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md` | [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L1-L1] |
| `docs/plans/2026-05-05-issue-471-jumper-installation-pipeline-story.md` | [docs/plans/2026-05-05-issue-471-jumper-installation-pipeline-story.md:L1-L1] |
| `docs/plans/2026-05-05-issue-472-go-no-go-decision-logic.md` | [docs/plans/2026-05-05-issue-472-go-no-go-decision-logic.md:L1-L1] |
| `docs/plans/2026-05-05-issue-475-jumper-lift-pytest-suite.md` | [docs/plans/2026-05-05-issue-475-jumper-lift-pytest-suite.md:L1-L1] |
| `docs/plans/2026-05-05-issue-478-orcaflex-modular-generator-integration.md` | [docs/plans/2026-05-05-issue-478-orcaflex-modular-generator-integration.md:L1-L1] |
| `docs/plans/2026-05-05-issue-479-jumper-html-pdf-report-renderer.md` | [docs/plans/2026-05-05-issue-479-jumper-html-pdf-report-renderer.md:L1-L1] |
| `docs/plans/2026-05-05-issue-480-plet-plem-jumper-segments.md` | [docs/plans/2026-05-05-issue-480-plet-plem-jumper-segments.md:L1-L1] |
| `docs/plans/2026-05-05-issue-481-plet-plem-workbook-conversion.md` | [docs/plans/2026-05-05-issue-481-plet-plem-workbook-conversion.md:L1-L1] |
| `docs/plans/2026-05-05-issue-487-towing-marine-operations.md` | [docs/plans/2026-05-05-issue-487-towing-marine-operations.md:L1-L1] |
| `docs/plans/2026-05-05-issue-504-buoys-builder-refactor.md` | [docs/plans/2026-05-05-issue-504-buoys-builder-refactor.md:L1-L1] |
| `docs/plans/2026-05-05-issue-506-passing-ship-jumper-installation-schemas.md` | [docs/plans/2026-05-05-issue-506-passing-ship-jumper-installation-schemas.md:L1-L1] |
| `docs/plans/2026-05-05-issue-554-catenary-solver-bracketing-sinh-overflow.md` | [docs/plans/2026-05-05-issue-554-catenary-solver-bracketing-sinh-overflow.md:L1-L1] |
| `docs/plans/2026-05-05-issue-555-chain-database-diameters-slice.md` | [docs/plans/2026-05-05-issue-555-chain-database-diameters-slice.md:L1-L1] |
| `docs/plans/2026-05-05-issue-556-ocimf-coefficients-interpolation.md` | [docs/plans/2026-05-05-issue-556-ocimf-coefficients-interpolation.md:L1-L1] |
| `docs/plans/2026-05-05-issue-557-ocimf-boundary-warnings.md` | [docs/plans/2026-05-05-issue-557-ocimf-boundary-warnings.md:L1-L1] |
| `docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md` | [docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md:L1-L1] |
| `docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md` | [docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md:L1-L1] |
| `docs/plans/2026-05-05-issue-560-rao-coupling-terms-affect-response.md` | [docs/plans/2026-05-05-issue-560-rao-coupling-terms-affect-response.md:L1-L1] |
| `docs/plans/2026-05-05-issue-561-ocimf-mooring-combined-environmental.md` | [docs/plans/2026-05-05-issue-561-ocimf-mooring-combined-environmental.md:L1-L1] |
| `docs/plans/2026-05-05-issue-562-marine-eng-perf-numpy-bool-json.md` | [docs/plans/2026-05-05-issue-562-marine-eng-perf-numpy-bool-json.md:L1-L1] |
| `docs/plans/2026-05-05-issue-563-marine-eng-perf-ocimf-database.md` | [docs/plans/2026-05-05-issue-563-marine-eng-perf-ocimf-database.md:L1-L1] |
| `docs/plans/2026-05-05-issue-564-ocimf-mooring-environmental-forces-total.md` | [docs/plans/2026-05-05-issue-564-ocimf-mooring-environmental-forces-total.md:L1-L1] |
| `docs/plans/2026-05-05-issue-565-hydro-coefficients-csv-to-visualization.md` | [docs/plans/2026-05-05-issue-565-hydro-coefficients-csv-to-visualization.md:L1-L1] |
| `docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md` | [docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md:L1-L1] |
| `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md` | [docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md:L1-L1] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L1-L1] |
| `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md` | [docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md:L1-L1] |
| `docs/plans/2026-05-05-issue-582-api-rp-2i-marine-inspection.md` | [docs/plans/2026-05-05-issue-582-api-rp-2i-marine-inspection.md:L1-L1] |
| `docs/plans/2026-05-15-issue-601-jumper-line-section-yaml-output.md` | [docs/plans/2026-05-15-issue-601-jumper-line-section-yaml-output.md:L1-L1] |
| `docs/plans/2026-05-15-issue-602-gom-tieback-jumper-projectinputspec-conversion.md` | [docs/plans/2026-05-15-issue-602-gom-tieback-jumper-projectinputspec-conversion.md:L1-L1] |
| `docs/plans/2026-05-15-issue-603-gom-tieback-jumper-pipeline-regression.md` | [docs/plans/2026-05-15-issue-603-gom-tieback-jumper-pipeline-regression.md:L1-L1] |
| `docs/plans/2026-05-15-issue-604-gom-tieback-licensed-orcaflex-validation.md` | [docs/plans/2026-05-15-issue-604-gom-tieback-licensed-orcaflex-validation.md:L1-L1] |
| `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` | [docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md:L1-L1] |
| `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md` | [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L1-L1] |
| `docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md` | [docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md:L1-L1] |
| `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | [docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md:L1-L1] |
| `docs/plans/2026-09-25-issue-2207-b401-f103-tables.md` | [docs/plans/2026-09-25-issue-2207-b401-f103-tables.md:L1-L1] |
| `docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md` | [docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md:L1-L1] |
| `docs/plans/2026-09-25-issue-2211-f103-module-kernel-design-loop.md` | [docs/plans/2026-09-25-issue-2211-f103-module-kernel-design-loop.md:L1-L1] |
| `docs/plans/2026-09-25-issue-2213-cp-test-hygiene.md` | [docs/plans/2026-09-25-issue-2213-cp-test-hygiene.md:L1-L1] |
| `docs/plans/2026-09-26-issue-2208-cp-editions.md` | [docs/plans/2026-09-26-issue-2208-cp-editions.md:L1-L1] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L1-L1] |
| `docs/session-handoffs/2026-10-12-atlas-visualization-import.md` | [docs/session-handoffs/2026-10-12-atlas-visualization-import.md:L1-L1] |
| `docs/small_operator/GROUNDING.md` | [docs/small_operator/GROUNDING.md:L1-L1] |
| `docs/vision/CALCULATIONS-VISION.md` | [docs/vision/CALCULATIONS-VISION.md:L1-L1] |
| `src/digitalmodel/marine_ops/artificial_lift/README.md` | [src/digitalmodel/marine_ops/artificial_lift/README.md:L1-L1] |
| `src/digitalmodel/marine_ops/artificial_lift/dynacard/README.md` | [src/digitalmodel/marine_ops/artificial_lift/dynacard/README.md:L1-L1] |
| `src/digitalmodel/marine_ops/artificial_lift/queue/prompts.md` | [src/digitalmodel/marine_ops/artificial_lift/queue/prompts.md:L1-L1] |
| `src/digitalmodel/marine_ops/installation/docs/agent-pack.md` | [src/digitalmodel/marine_ops/installation/docs/agent-pack.md:L1-L1] |
| `src/digitalmodel/marine_ops/marine_analysis/README.md` | [src/digitalmodel/marine_ops/marine_analysis/README.md:L1-L1] |
| `src/digitalmodel/marine_ops/marine_analysis/docs/agent-pack.md` | [src/digitalmodel/marine_ops/marine_analysis/docs/agent-pack.md:L1-L1] |
| `src/digitalmodel/marine_ops/reservoir/README.md` | [src/digitalmodel/marine_ops/reservoir/README.md:L1-L1] |
| 1 discovery file with identifying names | Locator withheld; [2770](https://github.com/vamseeachanta/digitalmodel/issues/2770) |

### Open-item register

Required accountable discipline: engineering calculations quality manager; maintainers shall resolve the filed follow-ups and arrange independent review before design qualification.

| Item | Evidence / unresolved limit | Issue |
|---|---|---|
| U1 / S01–S16: input semantics and negative-path coverage | Units, numeric validity, coverage and evaluated status are addressed by revision 1. Upstream conventions, empty mappings and plot fallbacks remain open. [src/digitalmodel/marine_ops/operation_envelope.py:L119-L288] | [2768](https://github.com/vamseeachanta/digitalmodel/issues/2768) |
| U2: standards provenance | The screening sidecar now exposes missing edition/clause/table qualification; standards provenance remains open. [src/digitalmodel/marine_ops/operation_envelope.py:L55-L112] [src/digitalmodel/hydrodynamics/wave_spectra.py:L46-L73] | [2769](https://github.com/vamseeachanta/digitalmodel/issues/2769) |
| Method-map follow-up and public documentation locators | Additional method-family mapping and codenaming remain required; inventory alone is not qualification. [src/digitalmodel/marine_ops/operation_envelope.py:L145-L165] [src/digitalmodel/marine_ops/operation_envelope.py:L190-L280] | [2770](https://github.com/vamseeachanta/digitalmodel/issues/2770) |
| U3: frequency-dependent independent benchmark | The dedicated tests include synthetic constant gains and an analytical low-pass integral; an independent physical benchmark remains open. [tests/marine_ops/test_operation_envelope.py:L24-L56] [tests/marine_ops/test_operation_envelope.py:L73-L148] | [2771](https://github.com/vamseeachanta/digitalmodel/issues/2771) |
| U4: deputy and shared lessons-log registration | Required assignments remain unverified, as stated in section 8 | [2771](https://github.com/vamseeachanta/digitalmodel/issues/2771) |
| QA appendix and input fingerprints | Output mapping has no input fingerprint, standard revision or signature. [src/digitalmodel/marine_ops/operation_envelope.py:L151-L165] | #1556 |
| Geometry-dependent operating criteria | Top-level path explicitly defers crane-tip and relative-motion refinements. [src/digitalmodel/marine_ops/operation_envelope.py:L21-L24] [docs/plans/2026-07-04-issue-1359-derived-quantities-go-no-go.md:L10-L23] | #1359 |
| Adjacent spectrum normalization review | The local envelope delegates to the normalized upstream spectrum; excluded installation-local normalization remains a distinct route. [src/digitalmodel/hydrodynamics/wave_spectra.py:L29-L44] [src/digitalmodel/marine_ops/installation/crane_tip_motion.py:L171-L201] | #2543 |
| Registry duplicate key outside this package | The solver domain has repeated `submodules` keys; strict YAML parsing rejects the baseline document, while ordinary parsing shadows its first mapping. [docs/registry/module-routing.yaml:L303-L325] | [2772](https://github.com/vamseeachanta/digitalmodel/issues/2772) |
