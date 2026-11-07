# Safety-case module knowledge pack

| Header | Value |
| --- | --- |
| Module | asset_integrity/safety_case |
| Repository | digitalmodel |
| Path | `src/digitalmodel/asset_integrity/safety_case` |
| Pack revision | 0 |
| Code revision | `aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3` |
| Analysis family | Engineering calculations (EC); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager; independent provider |
| Status | DRAFT |
| Evidence date | 2026-11-07 |

Citations resolve at the code revision unless an explicit `@revision` follows the path; numerical examples below are synthetic arithmetic assumptions. [docs/domains/articles/safety_case_mah_qra.md:L114-L115]

## 1. Purpose and scope

The package exports a validated major-accident-hazard inventory, individual-risk classification, a gross-disproportion cost-benefit check, and supplied-input mooring-break and passing-traffic scenario assessments. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L1-L13]

The routing registry marks the containing asset-integrity domain stable; this is domain metadata, rather than evidence of regulatory approval for this package. [docs/registry/module-routing.yaml:L15-L33]

Scenario expected fatalities/year are distinct from individual risk per annum; interaction forces and static tensions are context, not failure rates, collision damage or fatality models. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L6-L13]

Sensitivity, uncertainty propagation, environmental/property-loss consequences, fault/event-tree construction and regulatory-submission packaging are outside the documented implementation. [docs/domains/articles/safety_case_mah_qra.md:L170-L189]

## 2. Inputs and outputs

### Inputs

| Input / route | Unit, range and convention | Default / provenance requirement | Evidence |
| --- | --- | --- | --- |
| `MajorAccidentHazard` identifiers, description and bases | Nonblank trimmed text; identifiers unique within a register | Required, including bases for unknown quantities | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L47-L77] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L121-L126] |
| `frequency_per_year`, `fatalities_per_event` | Finite nonnegative events/year and conditional mean fatalities/event, or explicit `None`; consequence may exceed one | Required; blank numeric CSV cells mean unknown, not zero | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L20-L24] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L47-L77] |
| `barriers`, `responsible_roles` | Nonblank strings; barriers may be empty, roles require at least one element | Required; barrier names apply no automatic credit | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L61-L77] |
| `MAHRegister.from_csv(source)` | UTF-8 path or text stream; exact schema columns; arrays encoded as JSON; physical error-line numbering | String means path; UTF-8 BOM accepted; malformed input rejects the ingest | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L129-L204] |
| `classify_risk(risk, thresholds)` | Nonnegative individual annual fatality risk, per year; no coordinate system | Optional thresholds instantiate `ALARPThresholds`; code defaults are `1e-3` and `1e-6` per year, not a licensed-table reproduction | [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L85-L95] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L142-L171] |
| `gross_disproportion_check(cost, benefit, gdf)` | Nonnegative cost/benefit in the same currency; dimensionless factor at least one | Factor defaults to `3.0`; currency is caller-selected | [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L229-L250] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L264-L276] |
| `assess_mooring_failure(design, ...)` | Consumed top and horizontal tensions in kN; finite nonnegative, horizontal no greater than top | Hazard fields and data bases required; upstream `CatenaryResult` is evidence only | [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L37-L82] |
| `VesselCollisionInputs` | Finite nonnegative transits/year; collision probability per transit in [0, 1] or `None`; nonnegative fatalities/collision or `None` | All three bases required; rates/consequences have no empirical defaults | [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L45-L59] |
| Optional `passing_ship_forces`, `impact_screen` | Signed surge/sway in N, yaw in N*m; finite samples; impact-screen object retained | Both default to `None`; package defines no frame, origin, sign/phase transform or force-to-fatality conversion | [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L9-L19] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L62-L90] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L124-L129] |

### Outputs

| Output | Unit and interpretation | Evidence |
| --- | --- | --- |
| `expected_fatalities_per_year`, `status` | Product of event frequency and conditional mean fatalities; unknown factor returns `None` and `NOT_EVALUATED` | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L79-L98] |
| `MAHRegister.to_csv()` | CSV text; same model columns, JSON arrays; no automatic total or hazard-coverage conclusion | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L110-L119] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L207-L217] |
| `ALARPResult` | Risk, region, thresholds, demonstration flag and rationale; equality at upper/lower boundaries selects intolerable/broadly acceptable | [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L79-L105] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L173-L205] |
| `GrossDisproportionResult` | Currency cost/benefit, dimensionless factor and ratio, decision flags and rationale | [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L109-L139] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L269-L299] |
| `MooringFailureResult` | Validated hazard plus retained top/horizontal tension in kN | [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L37-L43] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L67-L82] |
| `VesselCollisionResult` | Hazard, structured inputs, optional force/impact evidence and missing-input tuple; annual event frequency may exceed one | [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L71-L100] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L104-L129] |

CSV combines collision traffic and probability provenance into a single frequency-basis string; preserve `result.inputs` to retain the separate structured bases. [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L98-L100] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L114-L129]

## 3. Method and governing standards

| Method | Implementing definition / equation | Reference posture |
| --- | --- | --- |
| Individual annual risk classification | `classify_risk`: upper comparison first, lower comparison second, otherwise tolerable/ALARP | HSE, *Reducing Risks, Protecting People* (2001), “The Tolerability of Risk framework”, as named by the code; implementation and code defaults are the evidence, not an independent conformance check. [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L8-L36] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L168-L205] |
| Likelihood/consequence product | `risk_from_likelihood_consequence = likelihood * consequence` | Unit depends on the supplied consequence; the helper does not perform an exposure conversion from aggregate fatalities/year to individual risk. [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L208-L226] [docs/domains/articles/safety_case_mah_qra.md:L170-L175] |
| Gross disproportion | `measure_required = cost < gdf * benefit`; ratio `cost / benefit`, special zero-benefit branch | Code-defined inequality and configurable factor; equality rules out the measure under this calculation. [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L229-L276] |
| Scenario PLL contribution | `MajorAccidentHazard.expected_fatalities_per_year = frequency_per_year * fatalities_per_event` | IMO MSC-MEPC.2/Circ.12/Rev.2 (2018), appendix 5, “Potential loss of life”, section 3.2.3, as cited in the implementation; rejects a non-finite product. [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L79-L98] |
| Collision frequency | `assess_vessel_collision`: transits/year times collision probability per counted transit, then hazard PLL product | IMO FSA (2018), appendix 3, “Event trees”, section 2; no annual-occurrence-probability conversion. [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L92-L119] |
| Mooring scenario boundary | `assess_mooring_failure` validates evidence and constructs the hazard; tension does not determine the supplied failure rate | UK SCR-2015, regulation 16(1)(d), and IMO FSA (2018), annex “Data basis” and appendix “Potential loss of life”, as named in its docstring. [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L1-L21] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L58-L82] |
| NORSOK context | NORSOK Z-013, *Risk and emergency-preparedness analysis*, 2010 revision 3 in the plan; code names risk acceptance criteria without an edition or clause | Edition omission is a traceability gap; no conflicting edition was found in the package. Clause text not checked against the licensed copy. [docs/plans/2026-05-05-issue-577-safety-case-mah-alarp.md:L8-L10] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L14-L18] |
| ISO context | ISO 17776:2016, *Major accident hazard management during the design of new installations*; register documentation names the edition, ALARP prose does not name an edition or clause | Contextual reference only; clause text not checked against the licensed copy. [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L11-L14] [docs/domains/articles/safety_case_mah_qra.md:L50-L56] |

UNVERIFIED: NORSOK clause-level conformance and applicability of its acceptance criteria; issue #2019. [docs/domains/articles/safety_case_mah_qra.md:L50-L56]

UNVERIFIED: ISO clause-level conformance; issue #2019. [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L11-L14]

These result types carry prose provenance and supplied bases, rather than an emitted `Citation` sidecar; retain that limit when using the calculations as evidence. [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L98-L139] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L70-L77] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L71-L79]

## 4. Validated worked examples

Validated here means independent closed-form arithmetic for synthetic inputs, not calibrated failure data or standards conformance; the existing tests spell out their hand calculations. [tests/asset_integrity/test_mah_register.py:L1-L5] [tests/asset_integrity/test_qra_vessel_collision.py:L1-L7] [tests/asset_integrity/test_alarp_demonstration.py:L106-L111]

| Kind / case | Source and inputs | Expected / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| Validated: inventory product | Independent hand multiplication in test; inline CSV fixture has 0.02 events/year and 0.5 fatalities/event | 0.01 fatalities/year; `pytest.approx` default relative tolerance `1e-6`, absolute `1e-12`, as used by the assertion | `tests/asset_integrity/test_mah_register.py::test_csv_ingest_and_loss` | PASS at code pin; [tests/asset_integrity/test_mah_register.py:L22-L53] |
| Validated: conditional collision product | Independent hand multiplication; inline input has 1000 transits/year, probability 0.0002/transit and 0.5 fatalities/collision | 0.2 collisions/year and 0.1 fatalities/year; same `pytest.approx` tolerances | `tests/asset_integrity/test_qra_vessel_collision.py::test_hand_calculated_frequency_and_consequence` | PASS at code pin; [tests/asset_integrity/test_qra_vessel_collision.py:L22-L60] |
| Validated: gross-disproportion inequality | Independent arithmetic: cost 2.0e6, benefit 1.0e6 currency units, factor 3.0 | Required flag true exactly, ratio 2.0 dimensionless; same `pytest.approx` tolerances | `tests/asset_integrity/test_alarp_demonstration.py::test_gdf_justifies_mitigation` | PASS at code pin; [tests/asset_integrity/test_alarp_demonstration.py:L106-L111] |
| Regression: mooring composition | Actual upstream catenary result; 100 m depth, 500 m line, 1000 N/m weight, 300 kN pretension | Retained 300 kN top / 200 kN horizontal, 0.01 fatalities/year; same `pytest.approx` tolerances; integration evidence only | `tests/asset_integrity/test_qra_mooring_failure.py::test_real_catenary_composition_and_register_round_trip` | PASS at code pin; [tests/asset_integrity/test_qra_mooring_failure.py:L25-L62] |
| Regression: executable example and CSV | Shipped synthetic script, not operating data or an independent benchmark dataset | First two PLL values 0.01 and 0.1 fatalities/year; same `pytest.approx` tolerances, unknown third result and CSV equality exact | `tests/asset_integrity/test_safety_case_example.py::test_executable_example_and_csv_exchange` | PASS at code pin; [tests/asset_integrity/test_safety_case_example.py:L17-L35] |

Offline evidence: all five module test files ran before and after the documentation change at the code pin, using `PYTHONPATH=src uv run --no-sources python -m pytest <five module test files> -q -rA -p no:cacheprovider --basetemp=<scratch directory>`; results were 89 run, 89 passed, 0 failed, 0 skipped and 0 NOT_RUN in each run. Test definitions and parametrizations are the durable case inventory. [tests/asset_integrity/test_alarp_demonstration.py:L24-L143] [tests/asset_integrity/test_mah_register.py:L47-L220] [tests/asset_integrity/test_qra_mooring_failure.py:L56-L104] [tests/asset_integrity/test_qra_vessel_collision.py:L53-L193] [tests/asset_integrity/test_safety_case_example.py:L17-L35]

The mooring case is not an independent hand-check reference for elastic stretch or fixture geometry; its assertions cover context retention and CSV composition. [tests/asset_integrity/test_qra_mooring_failure.py:L56-L62]

## 5. QA gates

Gate applicability follows AQG r0, section 10, “Python calculations”; G5 applies to every family (AQG r0, sections 9–10).

| Gate | Applicability | Module-specific evidence |
| --- | --- | --- |
| G0 inputs, units, citations | Applicable | Checks C0-CSV through C0-TENSION below; known non-finite ALARP gap remains in section 6. [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L47-L98] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L168-L171] |
| G1 imports and tests | Applicable | C1-SUITE imports the public package and runs its five test files; no target-program input deck is produced. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L16-L37] [tests/asset_integrity/test_safety_case_example.py:L17-L35] |
| G2 hand or published check | Applicable | C2-PLL, C2-COLLISION and C2-CBA; synthetic hand arithmetic, not external calibration. [tests/asset_integrity/test_mah_register.py:L1-L5] [tests/asset_integrity/test_qra_vessel_collision.py:L1-L7] [tests/asset_integrity/test_alarp_demonstration.py:L106-L111] |
| G3 short dynamics | NOT_APPLICABLE | Package methods evaluate supplied inputs rather than running time-domain dynamics. [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L9-L15] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L21-L30] |
| G4 diffraction | NOT_APPLICABLE | Outputs are inventory/scenario and classification result records, not diffraction bodies or meshes. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L6-L13] |
| G5 evidence appendix | Applicable | C5-CSV and C5-RECORDS; retain input fingerprints, offline node results, units and unresolved limitations with any downstream assessment; pack status remains DRAFT pending independent review. [tests/asset_integrity/test_safety_case_example.py:L17-L35] |

Check criteria below are reproducible acceptance instructions; equality and mismatch counts are exact unless a numerical tolerance is given.

| Check | Gate | Function, executable route and pass criterion | Evidence |
| --- | --- | --- | --- |
| C0-CSV | G0 | `MAHRegister.from_csv`: run `tests/asset_integrity/test_mah_register.py`; 0 accepted malformed rows (count), 0 physical-line/field mismatches (count), exact round-trip equality | [tests/asset_integrity/test_mah_register.py:L56-L178] |
| C0-UNKNOWN | G0 | `expected_fatalities_per_year` and QRA missing-input handling: run `tests/asset_integrity/test_mah_register.py::test_missing_inputs_are_not_zero_risk`, `tests/asset_integrity/test_qra_mooring_failure.py::test_unknown_frequency_is_not_evaluated` and `tests/asset_integrity/test_qra_vessel_collision.py::test_missing_external_models_are_explicit`; 0 numeric risk results for missing factors (count) | [tests/asset_integrity/test_mah_register.py:L181-L186] [tests/asset_integrity/test_qra_mooring_failure.py:L65-L68] [tests/asset_integrity/test_qra_vessel_collision.py:L63-L80] |
| C0-ZERO | G0 | Run `tests/asset_integrity/test_mah_register.py::test_true_zero_and_mean_greater_than_one_are_valid` and `tests/asset_integrity/test_mah_register.py::test_header_only_register_is_empty`; known zero is exactly 0 fatalities/year, empty-register round-trip has 0 mismatches (count) | [tests/asset_integrity/test_mah_register.py:L189-L210] |
| C0-DECISIONS | G0 | Run `tests/asset_integrity/test_alarp_demonstration.py`; 0 incorrect boundary/negative-input/default-factor/zero-benefit assertions (count); zero benefit with positive cost yields infinite dimensionless ratio, never a required flag | [tests/asset_integrity/test_alarp_demonstration.py:L24-L143] |
| C0-FORCES | G0 | Run `tests/asset_integrity/test_qra_vessel_collision.py`; 0 invalid force samples accepted (count), 0 probability-boundary errors (count); retained force dictionaries exactly equal supplied N, N, N*m values | [tests/asset_integrity/test_qra_vessel_collision.py:L83-L155] |
| C0-TENSION | G0 | Run `tests/asset_integrity/test_qra_mooring_failure.py`; 0 negative/non-finite/inverted tension inputs accepted (count), 0 change in fatalities/year when only retained tension doubles | [tests/asset_integrity/test_qra_mooring_failure.py:L71-L104] |
| C1-SUITE | G1 | Run the five module test files using the section-4 command; 89 passing collected cases, 0 failures, 0 skips and 0 NOT_RUN cases (counts) | [tests/asset_integrity/test_alarp_demonstration.py:L24-L143] [tests/asset_integrity/test_mah_register.py:L47-L220] [tests/asset_integrity/test_qra_mooring_failure.py:L56-L104] [tests/asset_integrity/test_qra_vessel_collision.py:L53-L193] [tests/asset_integrity/test_safety_case_example.py:L17-L35] |
| C2-PLL | G2 | Section-4 inventory node: compare 0.02 * 0.5 with returned 0.01 fatalities/year; absolute error at most `1e-8` fatalities/year (default relative tolerance at this expected value) | [tests/asset_integrity/test_mah_register.py:L22-L53] |
| C2-COLLISION | G2 | Section-4 collision node: expected 0.2 collisions/year and 0.1 fatalities/year; absolute errors at most `2e-7` collisions/year and `1e-7` fatalities/year | [tests/asset_integrity/test_qra_vessel_collision.py:L22-L60] |
| C2-CBA | G2 | Section-4 gross-disproportion node: 2.0 < 3.0, exact required flag; ratio error at most `2e-6` dimensionless | [tests/asset_integrity/test_alarp_demonstration.py:L106-L111] |
| C5-CSV | G5 | Run section-4 executable-example node: 0 status, output-string or CSV round-trip mismatches (count); retain synthetic-input labels and the unknown third scenario | [tests/asset_integrity/test_safety_case_example.py:L17-L35] |
| C5-RECORDS | G5 | Hash the five package Python files, five test files and shipped example at the code pin; retain node outcomes and this pack's limits with the evidence; pass at 11 input digests, 89 PASS nodes and 0 missing records (counts) | [src/digitalmodel/asset_integrity/safety_case/__init__.py:L16-L37] [examples/asset_integrity/example_safety_case.py:L1-L7] [tests/asset_integrity/test_safety_case_example.py:L17-L35] |

## 6. Known failure modes and pitfalls

sweep: 28 found

The 28 rows below are sweep dispositions, including intentional unknown/default/unit paths and guarded error paths, not 28 newly established defects. The sweep commands were:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%|int\(|float\(|default|units?|encoding|cache' src/digitalmodel/asset_integrity/safety_case --glob '*.py'
rg -n 'None|=\s*(0|0\.0)|1e-|3\.0|except|continue|return|float\(|int\(' src/digitalmodel/asset_integrity/safety_case --glob '*.py'
```

| ID | Symptom / cause at pin | Detection / disposition | Issue or lesson; evidence date |
| --- | --- | --- | --- |
| S01 | NaN threshold bypasses positivity/order comparisons; infinite thresholds are accepted | no detection check exists; #3279 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L85-L95] |
| S02 | NaN risk falls into the tolerable branch; infinity is not rejected as invalid | no detection check exists; #3279 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L168-L196] |
| S03 | Non-finite likelihood/consequence can propagate through the unguarded product | no detection check exists; #3279 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L208-L226] |
| S04 | Infinite cost can rule out a measure; non-finite benefit/factor can distort the comparison and ratio | no detection check exists; #3279 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L264-L288] |
| S05 | Code upper threshold default may be used without an applicable individual-risk basis | C0-DECISIONS checks boundary behaviour, not applicability; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L68-L86] |
| S06 | Code lower threshold default may be used without an applicable individual-risk basis | C0-DECISIONS checks boundary behaviour, not applicability; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L68-L86] |
| S07 | Omitted classifier thresholds instantiate the defaults | C0-DECISIONS; explicit convention, no hidden calibration; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L168-L171] |
| S08 | Omitted gross-disproportion factor becomes 3.0; site-specific justification is not checked | C0-DECISIONS verifies the default; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L229-L250] |
| S09 | Positive cost and zero benefit produce infinity and rule out the measure | C0-DECISIONS; explicit branch | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L269-L276] |
| S10 | Zero cost and zero benefit produce ratio 0.0 but still rule out the measure at equality | no detection check exists; issue pending (2) | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L269-L276] |
| S11 | Missing hazard factor returns `None`, not zero risk | C0-UNKNOWN; preserved `NOT_EVALUATED` | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L86-L98] |
| S12 | External numeric CSV text uses `float`; blank becomes `None`, underscores rejected, locale separators are not normalized | C0-CSV / C0-UNKNOWN cover malformed text and blank cells; separator-specific detection check does not exist; issue pending (2) | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L174-L184] |
| S13 | JSON array conversion catches `ValueError`/`TypeError` | C0-CSV; raises a located error, no swallowed row | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L164-L173] |
| S14 | Numeric conversion catches `ValueError` | C0-CSV; raises a located error, no zero fallback | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L174-L184] |
| S15 | Schema exception hides input payload details | C0-CSV; field retained and ingest rejected | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L185-L192] |
| S16 | CSV parser exception is translated | C0-CSV; physical parser line retained, no partial register returned | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L199-L205] |
| S17 | Empty accumulator permits a header-only register | C0-ZERO; exchange only, not hazard-coverage proof | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L110-L119] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L147-L150] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L205-L205] |
| S18 | UTF-8/BOM and exact-schema assumptions reject alternative encodings/headers | C0-CSV; first-line BOM handling is explicit | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L129-L159] |
| S19 | Fatality inputs are conditional means, not per-person probabilities; passing PLL to the classifier changes the risk measure | no detection check exists for cross-interface misuse; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L61-L65] [docs/domains/articles/safety_case_mah_qra.md:L170-L175] |
| S20 | Empty barrier tuple is valid; names never apply automatic risk reduction | C2-PLL with C0-CSV; bow-tie/LOPA detection check does not exist; #3841 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L61-L77] [tests/asset_integrity/test_mah_register.py:L201-L205] |
| S21 | Mooring context assumes upstream kN tensions and retains only top/horizontal values | C0-TENSION; no dynamic or failure-rate inference | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L37-L43] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L58-L82] |
| S22 | Collision frequency begins `None` when probability is missing | C0-UNKNOWN; missing factors remain explicit | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L104-L118] |
| S23 | Omitted passing-ship forces default to `None` | C0-FORCES; valid context does not complete missing risk inputs | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L82-L90] [tests/asset_integrity/test_qra_vessel_collision.py:L125-L155] |
| S24 | Omitted impact evidence defaults to `None` in result and call | C0-FORCES; actual impact-screen integration test also rejects a wrong object type | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L71-L103] [tests/asset_integrity/test_qra_vessel_collision.py:L179-L193] |
| S25 | Signed interaction loads are assumed N, N and N*m; no coordinate or phase conversion | C0-FORCES checks evidence retention; frame-specific detection check does not exist; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L9-L15] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L62-L68] |
| S26 | Collision CSV combines two bases into a string and cannot recover their structure | C5-CSV checks text round-trip; structured bases must be kept separately | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L98-L100] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L119-L129] |
| S27 | Annual collision event frequency may exceed one; it is not a probability | C0-FORCES checks probability endpoints; preserve events/year convention | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L92-L96] [tests/asset_integrity/test_qra_vessel_collision.py:L83-L91] |
| S28 | Cost and benefit rely on a common caller-chosen currency; factor is dimensionless | C2-CBA checks arithmetic; no detection check exists for mixed currencies; #2019 | 2026-11-07; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L113-L128] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L241-L250] |

The four exception handlers raise located errors rather than silently continuing; there is no package `continue`, error-path empty-list/dict return, epoch fallback or cache write in the reviewed definitions. [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L129-L217] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L142-L299] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L46-L82] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L82-L129]

Historical scope leads #577 and #2019 are not evidence that the three QRA/register files are still absent: they are exported at this pin and covered by executable tests; external failure/consequence models and clause-level conformance remain open. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L25-L37] [tests/asset_integrity/test_safety_case_example.py:L17-L35] [docs/domains/articles/safety_case_mah_qra.md:L177-L189]

## 7. Lessons log

| Date | Change reference | Lesson / evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Keep PLL, annual event frequency and individual annual risk distinct; passing-ship force is context, not collision consequence. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L6-L13] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L92-L100] | Recorded interface units, independent arithmetic cases and missing-model limits | Not assigned; #2019 |

## 8. Ownership

Reviewing role: engineering calculations quality manager. Required deputy role: independent engineering calculations reviewer. Producing role: engineering calculations contributor. These are role requirements for this draft, not claims of completed review.

Calculation ambiguity, missing applicability evidence and disputed implementation classifications go to that family reviewer; cross-family decisions go to the maintainers. The next action is second-provider review of this DRAFT pack against the acceptance checks.

## 9. Dependencies and implementation inventory

| Direction | Dependency / boundary | Evidence |
| --- | --- | --- |
| Runtime | Pydantic validates external inventory, QRA inputs and consumed static evidence; CSV/JSON exchange uses the standard library | [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L29-L45] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L129-L217] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L65-L82] |
| Upstream static evidence | `digitalmodel.orcaflex.mooring_design.CatenaryResult`; scenario method retains top/horizontal kN tensions, without launching a solver | [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L9-L15] [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L65-L82] |
| Optional upstream interaction evidence | `hydrodynamics.passing_ship.PassingShipCalculator.calculate_forces`; output retained verbatim as N, N, N*m; no collision model provided at this boundary | [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L9-L15] [tests/asset_integrity/test_qra_vessel_collision.py:L125-L155] |
| Optional upstream impact evidence | `technical_safety.accidental_loads.ShipImpactResult`; type checked and retained without supplying missing collision probability/consequence | [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L17-L25] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L102-L103] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L129-L129] |
| Downstream | `MAHRegister` exchange and shipped synthetic example; do not sum overlapping scenarios or feed aggregate PLL to the individual-risk classifier | [examples/asset_integrity/example_safety_case.py:L76-L85] [docs/domains/articles/safety_case_mah_qra.md:L164-L175] |

No solver or solver version is required by these scenario calculations; the mooring method uses solver-free evidence and the example consumes only the analytical catenary route. [src/digitalmodel/asset_integrity/safety_case/qra_mooring_failure.py:L9-L15] [examples/asset_integrity/example_safety_case.py:L17-L31]

### Map inventory and counted-family audit

Map ref: `codex/implmap-work` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; its named data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4]

Inventory result: **0 in-package rows; 0 module map families; inventory `[]`; 0 counted families.** Every implementation row of every family was filtered for the header package prefix; this package has no sub-package exclusion. The scanned map family inventory is the evidence for this empty result. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

The counted-family grep is **NOT_APPLICABLE: the map has no family with a row inside the package**; the empty inventory passes the inventory requirement. No counted-family search signal or path-signal exclusion is applicable. Map follow-up: add individual-risk classification, gross-disproportion CBA, scenario PLL and conditional collision-frequency multiplication before they become counted families; issue pending (1). [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L142-L276] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L86-L98] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L109-L119]

The additional whole-repository discovery search, including `scripts/`, ran at the code pin:

```sh
git grep -n -E 'ALARP|alarp|gross.disproportion|major.accident|expected_fatalities|collision_probability|assess_mooring_failure|assess_vessel_collision' aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 -- '*.py'
```

It found 143 matching lines in 11 files: the five package files, five test files and one example; definitions outside the package were test/fixture/example callers, with no additional computing definition found by those signals. This is a bounded search, not proof against differently worded implementations. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L16-L37] [tests/asset_integrity/test_alarp_demonstration.py:L24-L143] [tests/asset_integrity/test_mah_register.py:L22-L220] [tests/asset_integrity/test_qra_mooring_failure.py:L25-L104] [tests/asset_integrity/test_qra_vessel_collision.py:L22-L193] [tests/asset_integrity/test_safety_case_example.py:L17-L35] [examples/asset_integrity/example_safety_case.py:L21-L91]

### Local inventory reference

The external inventory's historical count was not reused: this pin contains the five module files and five associated test files identified above; the tests collect 89 cases, including parametrizations. The domain routing metadata is stable and its QA family for this pack is EC. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L16-L37] [docs/registry/module-routing.yaml:L15-L33] [tests/asset_integrity/test_mah_register.py:L56-L155] [tests/asset_integrity/test_qra_vessel_collision.py:L63-L120]

## 10. Coverage and open items

### Files read and test inventory

| Coverage | Files / evidence |
| --- | --- |
| Whole package, all Python files; no Python sub-folders | `__init__.py`, `alarp_demonstration.py`, `mah_register.py`, `qra_mooring_failure.py`, `qra_vessel_collision.py` under the header path; implementations are exported here. [src/digitalmodel/asset_integrity/safety_case/__init__.py:L16-L37] |
| All discovered module tests; no package-local test folder | `tests/asset_integrity/test_alarp_demonstration.py`, `tests/asset_integrity/test_mah_register.py`, `tests/asset_integrity/test_qra_mooring_failure.py`, `tests/asset_integrity/test_qra_vessel_collision.py`, `tests/asset_integrity/test_safety_case_example.py`; section 4 records the offline evidence. [tests/asset_integrity/test_alarp_demonstration.py:L24-L143] [tests/asset_integrity/test_mah_register.py:L47-L220] [tests/asset_integrity/test_qra_mooring_failure.py:L56-L104] [tests/asset_integrity/test_qra_vessel_collision.py:L53-L193] [tests/asset_integrity/test_safety_case_example.py:L17-L35] |
| Module documentation found by package/standard search | `docs/domains/articles/safety_case_mah_qra.md` and `docs/plans/2026-05-05-issue-577-safety-case-mah-alarp.md`; the article documents the current supplied-input boundaries, while the plan describes a broader intended scope. [docs/domains/articles/safety_case_mah_qra.md:L177-L189] [docs/plans/2026-05-05-issue-577-safety-case-mah-alarp.md:L12-L24] |
| Additional textual discovery | `docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md` uses the generic safety-case term but does not document this package API; retained as discovery context only. [docs/research/ffs-unbonded-flexible-pipe-integrity-screen.md:L100-L107] |
| Routing and method map | Existing asset-integrity routing entry and separately pinned implementation-map inventory. [docs/registry/module-routing.yaml:L15-L33] [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502] |

### Open items

These are maintainer follow-up assignments; each row has an existing issue or a numbered draft for issue creation before clearance.

| Item | Required follow-up / limit | Tracking |
| --- | --- | --- |
| NORSOK conformance (section-3 UNVERIFIED claim) | Resolve edition/clause applicability and check authorized clause text; code prose and plan supply no completed clause-conformance evidence | #2019; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L14-L18] [docs/domains/articles/safety_case_mah_qra.md:L50-L56] |
| ISO conformance (section-3 UNVERIFIED claim) | Contextual ISO 17776:2016 reference is not a demonstrated conformance result | #2019; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L11-L14] |
| Non-finite ALARP inputs | Guard and regression-test classification, thresholds, product and cost-benefit paths; existing tests check negatives but do not cover non-finite ALARP inputs | #3279; [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L88-L95] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L168-L171] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L224-L226] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L264-L276] [tests/asset_integrity/test_alarp_demonstration.py:L76-L100] |
| Degenerate arithmetic and numeric CSV conventions | Add zero/zero policy and separator-specific rejection tests; no such detection check exists in section 5 | issue pending (2); [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L269-L276] [tests/asset_integrity/test_alarp_demonstration.py:L135-L143] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L174-L184] |
| Method-map coverage | Inventory all four method families before applying a counted-family grep; current empty inventory is explicitly limited | issue pending (1); [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L142-L276] [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L86-L98] [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L109-L119] |
| Barrier modelling | Named barriers do not form a bow tie, LOPA or required-SIL calculation | #3841; [src/digitalmodel/asset_integrity/safety_case/mah_register.py:L61-L77] |
| Empirical inputs, applicability and independent datasets | Supply qualified failure frequencies, post-break response, traffic causation, collision damage/exposure and individual-risk models; validated cases here check synthetic arithmetic only | #2019; [docs/domains/articles/safety_case_mah_qra.md:L177-L189] |
| Structured provenance and output citations | Retain collision inputs separately from CSV bases; prose references and input bases are not emitted citation sidecars or a signed QA appendix | #2019; [src/digitalmodel/asset_integrity/safety_case/qra_vessel_collision.py:L98-L100] [src/digitalmodel/asset_integrity/safety_case/alarp_demonstration.py:L98-L139] |
