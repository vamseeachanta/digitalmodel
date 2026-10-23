# Marine engineering knowledge pack

| Header | Value |
| --- | --- |
| Module | marine_ops/marine_engineering |
| Repository | digitalmodel |
| Package | src/digitalmodel/marine_ops/marine_engineering |
| Pack revision | 0 |
| Code revision | 32fe066f387c13a66fd234eca2850a00c16e104a |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map revision | 3256bab95f84da3d9fadce8d5dcf8458d66d1eb4 |
| Implementation-map data revision | 270c8759c374b67a7f032953e7fcdb14e0224461 |

Source citations resolve at the code revision; citations to `docs/registry/implementation-map.yaml` resolve at the separately pinned implementation-map revision.

## 1. Purpose and scope

The package exports canonical catenary interfaces from `marine_analysis` and local environmental-loading and hydrodynamic-coefficient interfaces. [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L17-L44]

`FPSOMooringFullWorkflow.router` combines a wave spectrum, environmental forces, chain properties, line geometry, static equilibrium and output writing; the coefficient workflow loads CSV matrices, validates the database and writes query results. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L35-L52] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L35]

`OrcaFlexTensionReader` reads exported CSV/XLSX tables; `compute_fatigue_life` and `ScatterDiagramFatigue.compute` currently refuse damage calculation through the counting contract, so the diagnostic counter does not establish a usable fatigue assessment. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L115] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L388-L403] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L559-L569]

The domain registry records beta maturity; this describes the domain entry rather than numerical qualification of every local implementation. [docs/registry/module-routing.yaml:L162-L174]

Scope includes every local Python subpackage, the two registered offline workflows and the tests identified in section 10; the canonical catenary implementation remains an upstream dependency. [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L17-L25] [docs/registry/workflows.yaml:L686-L695] [docs/registry/workflows.yaml:L1090-L1098]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Range / validation | Default | Source and evidence |
| --- | --- | --- | --- | --- |
| Wind/current speed and direction; density | m/s, degrees, kg/m³ | Dataclass fields have no positivity or finite-value check | Density defaults are assumptions in code; explicit design-basis values are required for engineering use | `EnvironmentalConditions`; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L95-L103] |
| Length, beam, draft and projected areas | m, m² | Dataclass does not reject invalid dimensions | Missing areas use assumed freeboard or dimension products | `VesselGeometry`; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L106-L135] |
| OCIMF CSV | Heading degrees; displacement tonnes; coefficients dimensionless | Required columns checked; NaNs warn; query angles are reflected and clipped to the data domain | No CSV path default in `OCIMFDatabase` | Columns and validation; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L280-L305] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L59-L79] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] |
| Coefficient CSV / AQWA listing | Frequency rad/s; 6×6 coefficient matrices | Shape/type checked, but physical unit metadata not encoded | Empty stores before loading | `FrequencyDependentMatrix`; [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L28-L46] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L143-L150] |
| Coefficient workflow query | rad/s; named DOF | Positive finite frequency and nonempty query list required | Missing DOF selects Heave and the diagonal | Query helpers; [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L54-L77] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L111-L115] |
| Mooring workflow configuration | Geometry m; pretension N; chain diameter mm; chain database stiffness kN | Plain scalar conversions and database lookup, with formula fallback | Grade/link and fairlead-radius defaults in code | Workflow selection and conversions; [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L161-L195] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L323-L343] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L56] |
| Time-history export / scatter diagram | Time s; tension kN; probability dimensionless | Time-column presence checked; other columns converted to float | XLSX first sheet; no automatic tension-unit conversion | Reader and refusal; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L98] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L117-L172] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L388-L403] |
| Polar overlay DataFrame / silhouette | Angles degrees; values retain caller coefficient/force scale; silhouette fraction dimensionless | Schema and angle-range helpers are invoked; requested modes have section-6 limitations | Magnitude and lateral-only rendering | Interface and scaling; [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L326] [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L40-L75] |
| Fatigue report parameters | Design life years; inspection divisor dimensionless | Constructor stores parameters without positivity validation | Design life and inspection divisor are code assumptions | `FatigueLifeReport`; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L463-L485] |

### Outputs and conventions

The visualization code defines positive body X toward the bow and positive body Y toward port; flow angles increase anticlockwise viewed from above, while chart rendering remains the caller's responsibility. [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L21-L52]

The equilibrium route resolves its scalar offset into X/Y using the cosine and sine of the resultant-force heading. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L253]

| Output | Unit / representation | Evidence |
| --- | --- | --- |
| Wind/current/total force and yaw moment | N and N·m; separate wind and current components | Result dataclass and summation; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L138-L149] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L657-L691] |
| Coefficient query CSV and result mapping | `frequency_rad_s`, DOF names, added mass, damping; causality Boolean/error | Workflow result and CSV fields; [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L26-L35] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L101-L108] |
| Mooring summary JSON and line-tension CSV | Tension N/MN, offsets m, headings degrees and dimensionless safety-factor ratio; the nested chain mapping retains MBL/stiffness kN and weights kg/m | Result construction and CSV writer; [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L291-L321] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L369-L400] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L90-L103] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L56] |
| Rainflow ranges/counts and report objects | Tension range kN, count dimensionless, damage dimensionless, life years | Counter, damage and report helpers; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L225] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L487-L515] |
| Force-overlay chart | Body-frame flow direction and lateral-force sign; chart rendering is separate from data convention | Convention derivation and rendering caveat; [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L21-L52] |

UNVERIFIED (U1): A common dimensional convention for every translational/rotational coefficient entry, file origin and phase convention is not established by the local matrix container; issue pending (2). [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L28-L46]

## 3. Method and governing standards

| Method / definition | Implemented operation | Standard / qualification |
| --- | --- | --- |
| `EnvironmentalForces.calculate_wind_forces` | Dynamic pressure × projected area × interpolated coefficient; yaw moment additionally multiplies by length | OCIMF coefficient provenance requires qualification; sample generation is synthetic, not a licensed-table validation; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L607] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] |
| `EnvironmentalForces.calculate_current_forces` | Current pressure, frontal/lateral forces and yaw moment | Same qualification; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L609-L640] |
| `OCIMFDatabase.get_coefficients` / bounded interpolator | Heading reflection, bounded interpolation, nearest fallback and coefficient return | Vessel type is accepted but does not select separate interpolation data; [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L155-L269] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] |
| `CoefficientDatabase` / `KramersKronigValidator` | Frequency interpolation and Hilbert-based relative-error diagnostic | This numerical diagnostic alone does not establish physical causality or dimensional correctness; [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L90-L128] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L251] |
| `FPSOMooringFullWorkflow` | Canonical catenary calls, scalar offset equilibrium, wave-drift proxy and per-line safety-factor ratio | Demo workflow, not independent hand-check reference for the known catenary defect; [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L130-L140] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L291-L349] |
| `SNcurve.cycles_to_failure` | User-supplied power law for cycles to failure | API RP 2SK is named for built-in curves; no standard constants are reproduced here; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L81] |
| `RainflowFatigue._rainflow_algorithm` / `count_cycles` | Local stack algorithm and range aggregation | Diagnostic counter only; damage entry points refuse its result; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L297] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L388-L403] |
| `MinersRuleDamage.compute` / `FatigueLifeReport.build` | Sum of count/life terms; design-life/damage and inspection divisor | Scalar helper checks are separate from end-to-end fatigue validity; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L487-L515] |

| Named reference | Edition / clause available in repository | Disposition |
| --- | --- | --- |
| OCIMF, Mooring Equipment Guidelines | Third Edition (2008), Appendix A §A1 “Sign Convention and Coordinate System”; Fourth Edition (2018), Appendix A §A2 “Sign convention and coordinate system” | Both intentionally named; clause text not checked against the licensed copy; [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L10-L19] [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L71-L75] |
| API RP 2SK, Design and Analysis of Stationkeeping Systems | Edition and clause not specified by local curve declarations | Clause text not checked against the licensed copy; standards inventory #1805; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L69-L81] |
| DNV-RP-C205, Environmental conditions and environmental loads | Local fatigue reference uses the DNVGL designation; quick reference uses DNV, with no edition/clause | Clause text not checked against the licensed copy; edition reconciliation #1805; [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L10-L18] [docs/domains/marine-engineering/MARINE_QUICK_REFERENCE.md:L641-L646] |
| DNV-OS-E301, Position Mooring; ISO 19901-7, Stationkeeping | Documentation reference list only; edition/clause not specified there | No implementation compliance claim; clause text not checked against the licensed copy; #1805; [docs/domains/marine-engineering/MARINE_QUICK_REFERENCE.md:L641-L646] |
| ITTC | A historical RAO issue plan proposes a phase-convention search without establishing an edition/clause | No governing local method established; #1798; [docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md:L18-L22] |
| ASTM E1049-85 | Named by the fatigue module without a clause | Clause text not checked against the licensed copy; issue pending (6); [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L15-L18] |

UNVERIFIED (U2): Standards compliance, complete edition/clause reconciliation and standards-derived calculation sidecars remain unestablished for the local workflows; #1805 and #1823 cover the reference/provenance gaps. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L69-L81] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L26-L35] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L369-L400]

## 4. Validated worked examples

The following three scalar cases use independent closed-form arithmetic on test inputs; they qualify the named helper only, not the refused fatigue pipeline. [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L138-L143] [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L434-L458]

| Case | Source / input | Expected value and tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| Custom power-law life | Closed form with synthetic, user-defined curve and range in the test | `1e12 / (200 ** 3.5)` cycles; relative error ≤ 1e-6, dimensionless, as asserted | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestSNcurve::test_custom_sn_curve` | PASS at BASE and AFTER; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L138-L143] |
| Inverse-damage life | Closed form with synthetic damage 0.1 and design life 25 years | 250 years; relative error ≤ 1e-6, dimensionless, as asserted | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestFatigueLifeReport::test_fatigue_life_inverse_of_damage_per_year` | PASS at BASE and AFTER; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L434-L443] |
| Inspection divisor | Closed form with synthetic damage 0.2, design life 20 years and divisor 3 | `100 / 3` years; relative error ≤ 1e-6, dimensionless, as asserted | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestFatigueLifeReport::test_inspection_interval_is_fraction_of_fatigue_life` | PASS at BASE and AFTER; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L445-L458] |

### Regression examples

| Case | Input / expected comparison | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| Full mooring workflow | `examples/workflows/fpso-mooring-full/input.yml`; 8 output line rows; reported maximum equals maximum of returned tensions, with pytest default relative tolerance | `tests/workflows/test_durable_workflows.py::test_workflow_registry[fpso-mooring-full]` | PASS at BASE and AFTER; [tests/workflows/test_durable_workflows.py:L1052-L1076] |
| Coefficient workflow | `examples/workflows/hydro-coefficients/input.yml`; fixture-specific query values and CSV/result consistency, not an independent published benchmark | `tests/workflows/test_durable_workflows.py::test_workflow_registry[hydro-coefficients]` | PASS at BASE and AFTER; [tests/workflows/test_durable_workflows.py:L1439-L1448] [tests/workflows/hydrodynamics_assertions.py:L1-L60] |

`test_rao_verification_all_routes.py` defines a script class and `main`, rather than a pytest test function; its historical fixture routes therefore are not counted as a validated or regression test case here. [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L35-L68] [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L381-L400]

## 5. QA gates

The required family is AQG r0, section 10, Python calculations: G0, G1, G2 and G5 apply; G3 and G4 are not applicable to this offline calculation scope.

| Gate | Applicability | Module-specific check IDs |
| --- | --- | --- |
| G0 inputs, units and citations | Applicable | G0-1, G0-2 |
| G1 imports and tests | Applicable; Python test/import route, no solver load route | G1-1 |
| G2 hand or published comparison | Applicable to the scalar helpers; full physical workflow qualification remains open | G2-1, G2-2, G2-3 |
| G3 short dynamics | NOT_APPLICABLE: this scope runs no dynamic solver model | None |
| G4 diffraction | NOT_APPLICABLE: parsing/interpolation does not qualify a diffraction solve or mesh | None |
| G5 QA evidence | Applicable; pack/test evidence and workflow-output traceability require separate records | G5-1, G5-2 |

| Check | Executable check and evidence | Numeric pass criterion and unit | Result / limitation |
| --- | --- | --- | --- |
| G0-1 | Run `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestOrcaFlexTensionReader::test_read_csv_raises_on_missing_time_column` | 0 accepted malformed time-history inputs, count | PASS; this checks time-column rejection only, not units/separators; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L191-L195] |
| G0-2 | Resolve every source citation at its declared revision and compare the section-6 location inventory with the recorded source sweep | 0 unresolved ranges and 0 unaccounted sweep lines, count | Self-check evidence retained with the test records; source locations appear in section 6 |
| G1-1 | Run every discovered solver-free package test and both registered workflow nodes, with the declared test extra | 0 newly failing tests relative to BASE, count | Existing skips/expected failures are counted separately; [docs/registry/workflows.yaml:L686-L695] [docs/registry/workflows.yaml:L1090-L1098] |
| G2-1 | Custom power-law test in section 4 | Relative difference ≤ 1e-6, dimensionless; output cycles | PASS; no built-in standards-derived constant is used; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L138-L143] |
| G2-2 | Inverse-damage life test in section 4 | Relative difference ≤ 1e-6, dimensionless; output years | PASS; helper arithmetic only; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L434-L443] |
| G2-3 | Inspection-divisor test in section 4 | Relative difference ≤ 1e-6, dimensionless; output years | PASS; helper arithmetic only; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L445-L458] |
| G5-1 | Full-mooring workflow node reads its summary JSON and tension CSV back | 8 CSV rows and 8 summary lines, count; maximum-tension difference within pytest approximate comparison, N | PASS at BASE and AFTER; regression consistency only; [tests/workflows/test_durable_workflows.py:L1052-L1076] |
| G5-2 | Re-read scratch evidence manifest and require a code pin, source digests, commands and per-command results for the before/after runs | 0 missing required records, count; SHA-256 digest equality exact, dimensionless | Evidence-preservation check; a generated engineering QA appendix is not established by this package, issue pending (7) |

The known catenary defect is excluded as a hand-check authority; the independent reference test diagnoses it separately from the production implementation. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L121-L149] [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L211-L247]

## 6. Known failure modes and pitfalls

sweep: 261 found

The count denotes matching source lines, including deliberate sentinels, unit declarations and rendering defaults; it is not a count of confirmed numerical defects. Each matched line has an entry below, and additional reviewed control-flow behavior is recorded separately.

Exact discovery commands (text only, no solver imports):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|return np.array|return 0\.0|= 0(\.0)?\b|1970|0\.05|float\(|int\(|density|kg/|kN|tonnes|rad/s|mm\]|np.zeros|fill_value|errors=.*ignore|nan_to_num|cache|threshold|tolerance' src/digitalmodel/marine_ops/marine_engineering --glob '*.py'
rg -n -i 'OrcaFlex|OrcFxAPI|OrcaWave|AQWA|ANSYS' src/digitalmodel/marine_ops/marine_engineering --glob '*.py'
```

No numerical detector is claimed for an untested error branch. An issue reference on an intentional sentinel records the surrounding input/provenance audit rather than treating the sentinel itself as a defect.

| Exact source line | Symptom / disposition and supporting range | Detection | Tracking |
| --- | --- | --- | --- |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L59-L59] | Nonpositive tension range returns infinite life; negative input is not rejected. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L66] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L62-L62] | Nonpositive tension range returns infinite life; negative input is not rejected. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L66] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L64-L64] | Nonpositive tension range returns infinite life; negative input is not rejected. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L66] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L92-L92] | CSV/XLSX columns assume seconds and kN; non-time columns use ordinary float conversion without unit/separator validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L164] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L118-L118] | CSV/XLSX columns assume seconds and kN; non-time columns use ordinary float conversion without unit/separator validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L164] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L161-L161] | CSV/XLSX columns assume seconds and kN; non-time columns use ordinary float conversion without unit/separator validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L164] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L172-L172] | Missing-time None sentinel is checked by the caller and raises; intentional control flow. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L166-L172] | G0-1: missing-time rejection; numeric separator/unit checks absent | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L177-L177] | Line-name parser documentation assumes kN export naming; no unit conversion is performed. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L174-L200] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L209-L209] | Short/flat histories return no cycles; strict extrema can omit plateaux and nonfinite inputs have no explicit rejection. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L246] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L218-L218] | Short/flat histories return no cycles; strict extrema can omit plateaux and nonfinite inputs have no explicit rejection. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L246] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L222-L222] | Short/flat histories return no cycles; strict extrema can omit plateaux and nonfinite inputs have no explicit rejection. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L246] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L246-L246] | Short/flat histories return no cycles; strict extrema can omit plateaux and nonfinite inputs have no explicit rejection. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L203-L246] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L260-L260] | Stack counter and aggregation can omit full ranges; empty/zero ranges are retained as diagnostic empty output. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L249-L297] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L287-L287] | Stack counter and aggregation can omit full ranges; empty/zero ranges are retained as diagnostic empty output. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L249-L297] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L297-L297] | Stack counter and aggregation can omit full ranges; empty/zero ranges are retained as diagnostic empty output. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L249-L297] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L335-L335] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L336-L336] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L338-L338] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L340-L340] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L342-L342] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L343-L343] | Empty ranges return zero damage; zip truncates unequal ranges/counts and infinite lives are skipped. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L323-L344] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L361] | Probability tolerance, float conversions and zero accumulator are behind the unconditional damage refusal; they do not establish runnable validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L433] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L405-L405] | Probability tolerance, float conversions and zero accumulator are behind the unconditional damage refusal; they do not establish runnable validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L433] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L415-L415] | Probability tolerance, float conversions and zero accumulator are behind the unconditional damage refusal; they do not establish runnable validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L433] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L416-L416] | Probability tolerance, float conversions and zero accumulator are behind the unconditional damage refusal; they do not establish runnable validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L433] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L417-L417] | Probability tolerance, float conversions and zero accumulator are behind the unconditional damage refusal; they do not establish runnable validation. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L361-L433] | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L501-L501] | Design-life/divisor defaults are assumptions; nonpositive damage returns infinite life and inspection interval. [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L463-L515] | no detection check exists | issue pending (6) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L52-L52] | Optional plotting import warns and disables interactive output; intentional capability fallback. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L48-L54] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L70-L70] | Coefficient/displacement unit labels describe data or plots; they do not establish numerical provenance. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L59-L94] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L372-L426] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L457-L573] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L102-L102] | Density/freeboard/projected-area defaults are assumptions without numerical input validation. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L95-L135] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L103-L103] | Density/freeboard/projected-area defaults are assumptions without numerical input validation. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L95-L135] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L169-L169] | Constant/interpolation setup converts numeric fields and sets fill policy; domain fallback behavior is documented in adjacent rows. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L166-L204] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L200-L200] | Constant/interpolation setup converts numeric fields and sets fill policy; domain fallback behavior is documented in adjacent rows. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L166-L204] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L211-L211] | Broad interpolation exception warns and uses nearest fallback. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L205-L215] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L232-L232] | Incomplete rectangular grid returns None to choose scattered interpolation; intentional sentinel. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L217-L233] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L327-L327] | Vessel type is unused, headings are reflected, and invalid coefficient output can become zero with a warning. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L356-L356] | Vessel type is unused, headings are reflected, and invalid coefficient output can become zero with a warning. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L358-L358] | Vessel type is unused, headings are reflected, and invalid coefficient output can become zero with a warning. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L360-L360] | Vessel type is unused, headings are reflected, and invalid coefficient output can become zero with a warning. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L320-L371] | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L415-L415] | Coefficient/displacement unit labels describe data or plots; they do not establish numerical provenance. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L59-L94] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L372-L426] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L457-L573] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L442-L442] | Noninteractive render returns None intentionally; not a failed numeric result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L427-L456] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L456-L456] | Noninteractive render returns None intentionally; not a failed numeric result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L427-L456] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L464-L464] | Coefficient/displacement unit labels describe data or plots; they do not establish numerical provenance. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L59-L94] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L372-L426] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L457-L573] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L544-L544] | Coefficient/displacement unit labels describe data or plots; they do not establish numerical provenance. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L59-L94] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L372-L426] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L457-L573] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L596-L596] | Force/moment computations assume declared SI dimensions and coefficient provenance; total coefficients retain wind heading. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L691] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L599-L599] | Force/moment computations assume declared SI dimensions and coefficient provenance; total coefficients retain wind heading. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L691] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L600-L600] | Force/moment computations assume declared SI dimensions and coefficient provenance; total coefficients retain wind heading. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L691] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L631-L631] | Force/moment computations assume declared SI dimensions and coefficient provenance; total coefficients retain wind heading. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L691] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L707-L707] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L728-L728] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L729-L729] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L740-L740] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L746-L746] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L795-L795] | Missing comparison fields default zero; moment label scaling and near-zero-reference difference can misstate the comparison. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L790-L831] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L797-L797] | Missing comparison fields default zero; moment label scaling and near-zero-reference difference can misstate the comparison. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L790-L831] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L802-L802] | Missing comparison fields default zero; moment label scaling and near-zero-reference difference can misstate the comparison. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L790-L831] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L806-L806] | Missing comparison fields default zero; moment label scaling and near-zero-reference difference can misstate the comparison. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L790-L831] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L816-L816] | Missing comparison fields default zero; moment label scaling and near-zero-reference difference can misstate the comparison. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L790-L831] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L859-L859] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L860-L860] | Plot unit labels and display defaults are assumptions; they do not validate the numerical result. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L692-L789] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L832-L874] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L915-L915] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L916-L916] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L917-L917] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L920-L920] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L922-L922] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L942-L942] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L943-L943] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L944-L944] | Sample coefficients, dimensions and small offsets are synthetic demonstration assumptions, not qualified standard data. [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L875-L946] | no detection check exists | #1823 |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L29-L29] | Frequency-unit documentation and zero search-start default; actual missing-frequency handling is covered above. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L24-L38] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L105-L116] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L70-L70] | Frequency regex does not cover exponent notation fully; missing section/frequency uses None sentinel. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L39-L70] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L117-L125] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L88-L88] | Partial matrix entries remain zero; ordinary float parsing and fixed dimensions can discard unsupported or extra values. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L75-L104] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L89-L89] | Partial matrix entries remain zero; ordinary float parsing and fixed dimensions can discard unsupported or extra values. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L75-L104] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L98-L98] | Partial matrix entries remain zero; ordinary float parsing and fixed dimensions can discard unsupported or extra values. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L75-L104] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L107-L107] | Frequency-unit documentation and zero search-start default; actual missing-frequency handling is covered above. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L24-L38] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L105-L116] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L120-L120] | Frequency regex does not cover exponent notation fully; missing section/frequency uses None sentinel. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L39-L70] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L117-L125] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L121-L121] | Frequency regex does not cover exponent notation fully; missing section/frequency uses None sentinel. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L39-L70] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L117-L125] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L150-L150] | Decode errors ignored; duplicate frequency keys overwrite and empty parsing only warns. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L134-L190] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L183-L183] | Decode errors ignored; duplicate frequency keys overwrite and empty parsing only warns. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L134-L190] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L185-L185] | Decode errors ignored; duplicate frequency keys overwrite and empty parsing only warns. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L134-L190] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L197-L197] | Added-mass/damping sections without frequency are omitted; empty section lists returned; early-file context uses a backward slice. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L194-L284] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L245-L245] | Added-mass/damping sections without frequency are omitted; empty section lists returned; early-file context uses a backward slice. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L194-L284] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L304-L304] | Batch results overwrite duplicate frequencies and can export an empty database; numeric conversion assumes documented text format. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L287-L393] | no detection check exists | issue pending (1) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L33-L33] | Matrix shape/type is checked, but nonfinite values and full dimensional metadata are not rejected. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L28-L80] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L66-L66] | Matrix shape/type is checked, but nonfinite values and full dimensional metadata are not rejected. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L28-L80] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L69-L69] | Matrix shape/type is checked, but nonfinite values and full dimensional metadata are not rejected. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L28-L80] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L94-L94] | Causality uses default relative tolerance, warning suppression and a small denominator offset; NaN error entries are discarded and frequency spacing does not enter the Hilbert transformation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L90-L128] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L98-L98] | Causality uses default relative tolerance, warning suppression and a small denominator offset; NaN error entries are discarded and frequency spacing does not enter the Hilbert transformation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L90-L128] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L101-L101] | Causality uses default relative tolerance, warning suppression and a small denominator offset; NaN error entries are discarded and frequency spacing does not enter the Hilbert transformation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L90-L128] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L126-L126] | Causality uses default relative tolerance, warning suppression and a small denominator offset; NaN error entries are discarded and frequency spacing does not enter the Hilbert transformation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L90-L128] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L138-L138] | Empty dictionaries and interpolator caches are normal initialization; not an empty-result failure cache. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L134-L150] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L174-L174] | CSV frequencies use float parsing; no matching files yield empty data; frequency mismatch only warns. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L152-L213] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L192-L192] | CSV frequencies use float parsing; no matching files yield empty data; frequency mismatch only warns. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L152-L213] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L235-L235] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L248-L248] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L259-L259] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L271-L271] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L279-L279] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L291-L291] | Interpolation extrapolates outside data and reused caches are not invalidated on public dictionary mutation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L215-L291] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L297-L297] | Zero matrix buffers are fully populated by nested loops; intentional initialization. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L293-L327] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L304-L304] | Zero matrix buffers are fully populated by nested loops; intentional initialization. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L293-L327] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L315-L315] | Zero matrix buffers are fully populated by nested loops; intentional initialization. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L293-L327] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L322-L322] | Zero matrix buffers are fully populated by nested loops; intentional initialization. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L293-L327] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L338-L338] | Invalid/nonpositive critical damping can become zero damping ratio. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L329-L356] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L356-L356] | Invalid/nonpositive critical damping can become zero damping ratio. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L329-L356] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L360-L360] | Causality tolerance is assumed; export frequency rounding can collide and overwrite files. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L358-L456] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L366-L366] | Causality tolerance is assumed; export frequency rounding can collide and overwrite files. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L358-L456] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L385-L385] | Causality tolerance is assumed; export frequency rounding can collide and overwrite files. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L358-L456] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L394-L394] | Causality tolerance is assumed; export frequency rounding can collide and overwrite files. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L358-L456] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L456-L456] | Causality tolerance is assumed; export frequency rounding can collide and overwrite files. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L358-L456] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L83-L83] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L101-L101] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L128-L128] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L150-L150] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L161-L161] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L165-L165] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L234-L234] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L244-L244] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L248-L248] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L296-L296] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L299-L299] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L303-L303] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L317-L317] | Coupling display threshold suppresses plotted small coefficients; this is a display policy. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L315-L355] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L324-L324] | Coupling display threshold suppresses plotted small coefficients; this is a display policy. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L315-L355] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L351-L351] | Coupling display threshold suppresses plotted small coefficients; this is a display policy. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L315-L355] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L352-L352] | Coupling display threshold suppresses plotted small coefficients; this is a display policy. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L315-L355] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L357-L357] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L417-L417] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L473-L473] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L477-L477] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L499-L499] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L539-L539] | Undefined correlation NaN is displayed as zero. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L533-L540] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L603-L603] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L660-L660] | Axis coordinates, zero plotting planes and frequency-unit labels are rendering assumptions, not failed-data caches. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L314] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L356-L396] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L416-L532] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L541-L673] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L28-L28] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L46-L46] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L65-L65] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L84-L84] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L85-L85] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L89-L89] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L91-L91] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L111-L111] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L112-L112] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L113-L113] | Positive finite query frequency is checked; plain numeric conversions and causality tolerance lack a common physical unit contract. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L115] | no detection check exists | issue pending (2) |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L136-L136] | Display-path fallback returns resolved path when a relative path cannot be formed; intentional fallback with disclosure risk. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L117-L137] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L44-L44] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L50-L50] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L52-L52] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L54-L54] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L56-L56] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L95-L95] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L96-L96] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L98-L98] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L103-L103] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L117-L117] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L118-L118] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L122-L122] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L123-L123] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L126-L126] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L127-L127] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L140-L140] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L143-L143] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L145-L145] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L146-L146] | Historical chain formula, densities and bend-radius/unit conversions require independent source qualification. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L120-L177] | no detection check exists | #1805 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L188-L188] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L194-L194] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L196-L196] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L198-L198] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L200-L200] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L223-L223] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L236-L236] | Unknown wire construction receives default factors; density/fill assumptions and unit conversion are not caller-qualified. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L234-L259] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L243-L243] | Unknown wire construction receives default factors; density/fill assumptions and unit conversion are not caller-qualified. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L234-L259] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L247-L247] | Unknown wire construction receives default factors; density/fill assumptions and unit conversion are not caller-qualified. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L234-L259] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L256-L256] | Unknown wire construction receives default factors; density/fill assumptions and unit conversion are not caller-qualified. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L234-L259] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L258-L258] | Unknown wire construction receives default factors; density/fill assumptions and unit conversion are not caller-qualified. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L234-L259] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L331-L331] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L351-L351] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L365-L365] | Duplicate chain matches warn then select first. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L365-L375] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L372-L372] | Duplicate chain matches warn then select first. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L365-L375] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L390-L390] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L412-L412] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L428-L428] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L430-L430] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L431-L431] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L462-L462] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L484-L484] | Unit documentation and database lookup controls; empty eligible selections raise rather than return a fabricated chain. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L32-L119] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L180-L233] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L354] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L379-L462] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L479-L499] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L503-L503] | Duplicate wire matches select first without a warning. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L501-L509] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L25-L25] | A caught catenary ValueError breaks search and returns surviving default bounds. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L21-L37] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L30-L30] | A caught catenary ValueError breaks search and returns surviving default bounds. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L21-L37] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L49-L49] | Print statements are lexical int( matches, not conversions or error swallowing. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L45-L74] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L50-L50] | Print statements are lexical int( matches, not conversions or error swallowing. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L45-L74] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L55-L55] | Print statements are lexical int( matches, not conversions or error swallowing. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L45-L74] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L63-L63] | Print statements are lexical int( matches, not conversions or error swallowing. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L45-L74] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L69-L69] | Print statements are lexical int( matches, not conversions or error swallowing. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L45-L74] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L124-L124] | Relative-display failure falls back to resolved path; intentional fallback with disclosure risk. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L120-L125] | no detection check exists | issue pending (7) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L56-L56] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L57-L57] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L58-L58] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L59-L59] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L64-L64] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L65-L65] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L68-L68] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L73-L73] | Default spectrum shape and finite integration grid are workflow assumptions; external scalars use float conversion. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L87-L87] | Missing coefficient file triggers synthetic database creation; an existing sample is reused without provenance/freshness validation. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L113] | no detection check exists | #2442 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L88-L88] | Missing coefficient file triggers synthetic database creation; an existing sample is reused without provenance/freshness validation. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L113] | no detection check exists | #2442 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L89-L89] | Missing coefficient file triggers synthetic database creation; an existing sample is reused without provenance/freshness validation. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L113] | no detection check exists | #2442 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L90-L90] | Missing coefficient file triggers synthetic database creation; an existing sample is reused without provenance/freshness validation. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L113] | no detection check exists | #2442 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L96-L96] | Missing coefficient file triggers synthetic database creation; an existing sample is reused without provenance/freshness validation. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L77-L113] | no detection check exists | #2442 |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L116-L116] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L117-L117] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L118-L118] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L119-L119] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L131-L131] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L132-L132] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L133-L133] | Geometry, direction and wave-drift proxy defaults are unqualified assumptions. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L115-L140] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L162-L162] | Chain lookup/data errors fall back to historical formulas; unrecognized link text selects stud-link. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L161-L177] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L167-L167] | Chain lookup/data errors fall back to historical formulas; unrecognized link text selects stud-link. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L161-L177] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L180-L180] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L181-L181] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L182-L182] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L183-L183] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L185-L185] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L194-L194] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L195-L195] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L218-L218] | Line count can truncate fractional input; conversions, clipped geometry and fixed iterations have no common validation contract. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L179-L226] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L231] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L232-L232] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L235-L235] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L237-L237] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L243-L243] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L248-L248] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L249-L249] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L250-L250] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L251-L251] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L264-L264] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L277-L277] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L278-L278] | Bracket exhaustion returns its last bound, and catenary failure becomes negative infinity without a residual acceptance check. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L231-L278] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L280-L280] | Restoring dot-product conversion to float is a numeric serialization boundary. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L279-L281] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L304-L304] | Line serialization uses assumed units; zero restoring accumulation is intentional but direction norm can be zero. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L282-L400] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L341-L341] | Line serialization uses assumed units; zero restoring accumulation is intentional but direction norm can be zero. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L282-L400] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L393-L393] | Line serialization uses assumed units; zero restoring accumulation is intentional but direction norm can be zero. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L282-L400] | no detection check exists | issue pending (5) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L55-L55] | Sign routing, rendering defaults and heading-bin assumptions; no numerical solver result is created here. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L60] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L304] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L358-L372] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L59-L59] | Sign routing, rendering defaults and heading-bin assumptions; no numerical solver result is created here. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L60] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L304] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L358-L372] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L221-L221] | NaN/empty groups and zero-only components are omitted; signed radial mode still plots absolute values. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L202-L267] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L223-L223] | NaN/empty groups and zero-only components are omitted; signed radial mode still plots absolute values. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L202-L267] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L255-L255] | NaN/empty groups and zero-only components are omitted; signed radial mode still plots absolute values. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L202-L267] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L256-L256] | NaN/empty groups and zero-only components are omitted; signed radial mode still plots absolute values. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L202-L267] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L282-L282] | Sign routing, rendering defaults and heading-bin assumptions; no numerical solver result is created here. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L60] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L304] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L358-L372] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L307-L307] | Empty/nonpositive radius selects a unit display fallback. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L128-L134] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L305-L312] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L400] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L310-L310] | Empty/nonpositive radius selects a unit display fallback. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L128-L134] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L305-L312] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L400] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L311-L311] | Empty/nonpositive radius selects a unit display fallback. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L128-L134] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L305-L312] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L400] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L371-L371] | Sign routing, rendering defaults and heading-bin assumptions; no numerical solver result is created here. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L39-L60] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L304] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L358-L372] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L398] | Empty/nonpositive radius selects a unit display fallback. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L128-L134] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L305-L312] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L400] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L399-L399] | Empty/nonpositive radius selects a unit display fallback. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L128-L134] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L305-L312] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L398-L400] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L405-L405] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L406-L406] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L408-L408] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L416-L416] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L418-L418] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L419-L419] | First record per heading is retained; longitudinal arrows no-op and resultant mode uses lateral-only route. [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L373-L427] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L66-L66] | Silhouette size and opacity defaults are rendering assumptions; opacity/custom-path validation is explicit. [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L58-L75] | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L73-L73] | Silhouette size and opacity defaults are rendering assumptions; opacity/custom-path validation is explicit. [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L58-L75] | no detection check exists | issue pending (4) |

Additional reviewed behavior outside lexical matches:

| Location | Symptom | Detection | Tracking |
| --- | --- | --- | --- |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L522-L600] | Convenience damage route refuses before input validation; later conversions are unreachable. | no detection check exists | #1721 |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L397-L415] | Nonpositive mass/stiffness DOFs are omitted; default comparison line is not source-qualified. | no detection check exists | issue pending (4) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L155-L165] | Heading/displacement aggregation averages duplicate records without separating vessel types. | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L251-L269] | Scattered NaN uses nearest result; bounded queries clip to data extrema. | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L278-L305] | Required columns are checked; NaN values only produce a warning. | no detection check exists | issue pending (3) |
| [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L323-L349] | The catenary result is read without checking its convergence flag; this route does not require a successful convergence flag. | no detection check exists | issue pending (5) |

The production catenary remains outside the independent hand-check route because its reconciled-reference comparison is an existing strict expected failure; detection G1-1 records that status rather than counting it as a pass, #2050. [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L211-L247]

The local counter defect and refusal-gated damage consumers are described by the exposure record; the callable scalar damage/report helpers do not remove that limitation, #1721. [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L174-L243]

Historical documentation describes RBF interpolation and an inconsistent heading origin; the current interpolator and convention authority differ. Reference cleanup is tracked by #1823; no numerical detection check exists for documentation drift. [docs/domains/mooring/ocimf_module_readme.md:L317-L327] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L155-L269] [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L21-L38]

The ancestry-fixture test and optional workbook test are historical coverage pitfalls; their current run statuses are recorded in section 10, with #1993 and #2008 retained as follow-up context. [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L39-L68] [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L87-L96]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Shared-log disposition |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial pack | Separate scalar helper checks from refusal-gated fatigue workflows | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L388-L403] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L559-L569] | Entry not assigned; issue pending (7) |
| 2026-10-16 | Initial pack | Distinguish canonical imports from local duplicated implementations | [tests/marine_ops/marine_engineering/test_hydro_coefficients.py:L42-L51] [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L17-L44] | Entry not assigned; issue pending (7) |

## 8. Ownership

Required reviewer: engineering calculations quality manager on a provider independent of the producer. The deputy quality-manager assignment and shared-log registration require maintainer disposition, issue pending (7). Method classification disagreements go to the family quality manager; standards ambiguities and changes to acceptance criteria require that role's disposition.

## 9. Dependencies

The package delegates catenary analysis upstream, and its workflow imports the canonical catenary classes while retaining local environmental/coefficient implementations. [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L17-L44] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L10-L23]

The registered mooring and coefficient workflows are offline; this inventory does not establish a licensed program version or load result. [docs/registry/workflows.yaml:L686-L695] [docs/registry/workflows.yaml:L1090-L1098]

| Primary dependency / consumer | Role | Evidence |
| --- | --- | --- |
| WaveSpectra | Upstream spectrum and moment calculation used by the full-mooring workflow | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L54-L75] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L10-L21] |
| Counting contract | Upstream refusal gate for the two fatigue damage entry points | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L388-L403] [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L559-L569] |
| HullProfile | Upstream geometry object composed by the silhouette specification | [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L12-L16] [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L49-L75] |
| NumPy, Pandas, SciPy, Matplotlib and Seaborn | Numerical and plotting-library imports; separate from solver programs | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L13-L20] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L11-L22] |
| Component data directory and provenance | Local configured tables provide chain, wire and line data; missing directory raises, and legacy libraries without sidecars remain unverified | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L277-L317] |
| Engine dispatch | Downstream consumer routes both registered workflow names to this package | [src/digitalmodel/engine.py:L494-L501] [src/digitalmodel/engine.py:L656-L664] |

### Textual solver and format dependencies

Every matching source line is listed below: 63 lines and 69 textual occurrences. These are format readers, exports or documentation precedents; no licensed interface or solver was run.

| Source location | Dependency interpretation |
| --- | --- |
| [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L10-L10] | Public facade / text-only precedent |
| [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L40-L40] | Public facade / text-only precedent |
| [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L68-L68] | Public facade / text-only precedent |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L5-L5] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L8-L8] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L85-L85] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L88-L88] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L89-L89] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L91-L91] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L544-L544] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L589-L589] | Exported tension-table reader / documentation |
| [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L23-L23] | Public facade / text-only precedent |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L11-L11] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L41-L41] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L42-L42] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L43-L43] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L44-L44] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L55-L55] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L56-L56] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L57-L57] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L58-L58] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L2-L2] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L4-L4] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L8-L8] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L9-L9] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L24-L24] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L25-L25] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L39-L39] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L40-L40] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L42-L42] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L95-L95] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L99-L99] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L118-L118] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L124-L124] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L125-L125] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L134-L134] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L137-L137] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L141-L141] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L146-L146] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L154-L154] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L184-L184] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L186-L186] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L190-L190] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L194-L194] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L201-L201] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L203-L203] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L214-L214] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L220-L220] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L224-L224] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L238-L238] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L242-L242] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L249-L249] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L251-L251] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L262-L262] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L268-L268] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L272-L272] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L287-L287] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L317-L317] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L319-L319] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L322-L322] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L330-L330] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L331-L331] | AQWA listing format / public API documentation |
| [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L393-L393] | AQWA listing format / public API documentation |

### Implementation-map inventory and counted families

Each implementation entry below has its original sequence number, relation and exact pinned map locator. Outside-package locations are referenced through those locators so unrelated identifying discovery paths are not reproduced. Each locator contains the original path and symbol.

| Family | All map rows | In-package rows | Counted rank | Map source |
| --- | --- | --- | --- | --- |
| `wind-and-current-force-routes` | 135 | 7 | 1 | [docs/registry/implementation-map.yaml:L15393-L16806] |
| `sn-curves-and-miner-damage` | 392 | 5 | 2 | [docs/registry/implementation-map.yaml:L16807-L20963] |
| `effective-tension-riser-stack-up` | 264 | 4 | 3 | [docs/registry/implementation-map.yaml:L2139-L4932] |
| `catenary-mooring-line-statics` | 140 | 2 | Not counted | [docs/registry/implementation-map.yaml:L346-L2138] |
| `wave-spectra` | 141 | 1 | Not counted | [docs/registry/implementation-map.yaml:L13593-L15392] |

Counted-family signal patterns are used exactly as stored in the pinned map, with Python `re` and case-sensitive package-source searches. No counted signal matches the package path below `src/digitalmodel/` under a case-insensitive comparison, so none is excluded.

#### Signals: wind-and-current-force-routes

```text
algorithm: (?:0\.5|1\s*/\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient
vocabulary: OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile
constants_or_keys: Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity
```
[docs/registry/implementation-map.yaml:L15393-L15407]

#### Signals: sn-curves-and-miner-damage

```text
algorithm: rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life
vocabulary: S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve
constants_or_keys: thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges
```
[docs/registry/implementation-map.yaml:L16807-L16821]

#### Signals: effective-tension-riser-stack-up

```text
algorithm: effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
vocabulary: effective.tension|tensioner|stack.up
constants_or_keys: internal_pressure|external_pressure|fluid_density|buoyancy_module
```
[docs/registry/implementation-map.yaml:L2139-L2153]

#### Counted-family definition hits

| Family | Definition | Matching source lines | Disposition |
| --- | --- | --- | --- |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L9-L9], [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L27-L27], [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L29-L29], [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L60-L60] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L5-L5], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L8-L8], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L10-L10], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L17-L17], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L22-L22], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L30-L30], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L40-L40], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L41-L41], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L50-L50], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L51-L51] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L2-L2], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L4-L4], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L10-L10], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L11-L11], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L12-L12], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L14-L14], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L16-L16], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L17-L17], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L26-L26] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFCoefficients` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L58-L58], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L60-L60], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L63-L63], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L64-L64], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L66-L66], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L67-L67], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L72-L72], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L73-L73], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L75-L75], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L76-L76] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFCoefficients.to_dict` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L84-L84], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L85-L85], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L87-L87], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L88-L88] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalConditions` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L98-L98] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalForceResults` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L150-L150] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L272-L272], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L274-L274] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase.__init__` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L282-L282], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L285-L285], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L288-L288] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase._validate_data` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L297-L297], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L298-L298] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase._setup_interpolators` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L315-L315] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase.get_coefficients` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L321-L321], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L331-L331], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L362-L362], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L363-L363], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L364-L364], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L366-L366], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L367-L367] | Inventoried map definition |
| `wind-and-current-force-routes` | `OCIMFDatabase.plot_coefficient_surface` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L380-L380], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L412-L412], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L444-L444] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase.plot_polar_diagram` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L461-L461], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L470-L470], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L471-L471], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L475-L475], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L476-L476], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L485-L485], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L486-L486], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L487-L487], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L490-L490], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L494-L494], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L495-L495], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L496-L496], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L499-L499], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L503-L503] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `OCIMFDatabase.plot_interpolation_quality` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L512-L512], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L545-L545] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalForces` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L562-L562] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalForces.__init__` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L565-L565], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L570-L570] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalForces.calculate_wind_forces` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L574-L574], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L579-L579], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L596-L596], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L599-L599], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L600-L600], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L601-L601], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L602-L602] | Additional computation explicitly listed in section 3 |
| `wind-and-current-force-routes` | `EnvironmentalForces.calculate_current_forces` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L609-L609], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L614-L614], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L634-L634], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L635-L635] | Additional computation explicitly listed in section 3 |
| `wind-and-current-force-routes` | `EnvironmentalForces.calculate_total_forces` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L657-L657], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L658-L658], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L662-L662], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L663-L663] | Inventoried map definition |
| `wind-and-current-force-routes` | `EnvironmentalForces.plot_force_diagram` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L756-L756] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `EnvironmentalForces.plot_validation_chart` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L865-L865] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `create_sample_database` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952], [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L878-L952] | Inventoried map definition |
| `wind-and-current-force-routes` | `print_self_check` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L58-L58] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L14-L14] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `FPSOMooringFullWorkflow._environmental_forces` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L78-L102], [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L78-L102] | Inventoried map definition |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py:L3-L3], [src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py:L7-L7] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L1-L1], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L3-L3], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L10-L10], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L16-L16], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L29-L29], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L37-L37], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L48-L48], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L97-L97] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `_OcimfConventionAuthority` | [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L58-L58], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L72-L72], [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L73-L73] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `_OcimfConventionAuthority.positive_cy_arrow_at_starboard_incidence_deg` | [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L81-L81] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `_OcimfConventionAuthority.negative_cy_arrow_at_starboard_incidence_deg` | [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L89-L89] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L3-L3], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L6-L6], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L7-L7], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L21-L21] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `_resolve_arrow_direction_in_body_frame` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L46-L46], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L49-L49], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L56-L56], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L57-L57] | Inventoried map definition |
| `wind-and-current-force-routes` | `_add_silhouette_traces` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L116-L116] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `polar_force_overlay` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L290-L290], [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L353-L353] | Inventoried map definition |
| `wind-and-current-force-routes` | `_render_force_arrows` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L375-L375] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `_render_force_arrows._is_lateral` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L386-L386] | Helper, data, documentation or wrapper; no separate complete family result |
| `wind-and-current-force-routes` | `FrameConvention` | [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L22-L22], [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L27-L27], [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L32-L32] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `module-level statements` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L10-L10], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L69-L69], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L70-L70], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L76-L76], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L77-L77] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `SNcurve` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L40-L40], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L41-L41], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L46-L46] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `SNcurve.cycles_to_failure` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L53-L53], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L66-L66] | Inventoried map definition |
| `sn-curves-and-miner-damage` | `RainflowFatigue` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L197-L197] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `RainflowFatigue.count_cycles` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L206-L206], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L224-L224] | Inventoried map definition |
| `sn-curves-and-miner-damage` | `RainflowFatigue._rainflow_algorithm` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L249-L249] | Additional computation explicitly listed in section 3 |
| `sn-curves-and-miner-damage` | `MinersRuleDamage.__init__` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L315-L315], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L316-L316], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L319-L319], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L321-L321] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `MinersRuleDamage.compute` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L340-L340] | Inventoried map definition |
| `sn-curves-and-miner-damage` | `ScatterDiagramFatigue` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L352-L352] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `ScatterDiagramFatigue.__init__` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L363-L363], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L364-L364], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L367-L367], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L369-L369], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L371-L371] | Helper, data, documentation or wrapper; no separate complete family result |
| `sn-curves-and-miner-damage` | `ScatterDiagramFatigue.compute` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L428-L428] | Inventoried map definition |
| `sn-curves-and-miner-damage` | `compute_fatigue_life` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L524-L524], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L546-L546], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L584-L584], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L588-L588], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L593-L593], [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L598-L598] | Inventoried map definition |
| `effective-tension-riser-stack-up` | `_write_tensions_csv` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L95-L95] | Inventoried map definition |
| `effective-tension-riser-stack-up` | `FPSOMooringFullWorkflow._line_state` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L298-L328], [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L298-L328] | Inventoried map definition |
| `effective-tension-riser-stack-up` | `FPSOMooringFullWorkflow._solve_line` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L330-L356] | Inventoried map definition |
| `effective-tension-riser-stack-up` | `FPSOMooringFullWorkflow._result` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L376-L407], [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L376-L407] | Inventoried map definition |

The two force formulas and the rainflow stack algorithm found beyond the map inventory are explicitly listed in section 3; the remaining further unlisted complete computations total 0 across the three counted families, within the acceptance limit of 1. The quality manager determines any disputed helper classification.

#### Inventory: catenary-mooring-line-statics

| Map entry | Location / definition | Map relation | Pinned map locator |
| --- | --- | --- | --- |
| 1 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L353-L364] |
| 2 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L365-L376] |
| 3 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L377-L388] |
| 4 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L389-L400] |
| 5 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L401-L414] |
| 6 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L415-L426] |
| 7 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L427-L438] |
| 8 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L439-L450] |
| 9 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L451-L462] |
| 10 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L463-L474] |
| 11 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L475-L488] |
| 12 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L489-L504] |
| 13 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L505-L519] |
| 14 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L520-L531] |
| 15 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L532-L543] |
| 16 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L544-L555] |
| 17 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L556-L567] |
| 18 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L568-L579] |
| 19 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L580-L590] |
| 20 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L591-L602] |
| 21 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L603-L614] |
| 22 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L615-L626] |
| 23 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L627-L637] |
| 24 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L638-L649] |
| 25 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L650-L661] |
| 26 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L662-L673] |
| 27 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L674-L685] |
| 28 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L686-L697] |
| 29 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L698-L708] |
| 30 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L709-L722] |
| 31 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L723-L734] |
| 32 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L735-L746] |
| 33 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L747-L758] |
| 34 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L759-L770] |
| 35 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L771-L782] |
| 36 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L783-L794] |
| 37 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L795-L806] |
| 38 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L807-L820] |
| 39 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L821-L834] |
| 40 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L835-L846] |
| 41 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L847-L858] |
| 42 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L859-L870] |
| 43 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L871-L882] |
| 44 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L883-L894] |
| 45 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L895-L906] |
| 46 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L907-L918] |
| 47 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L919-L930] |
| 48 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L931-L941] |
| 49 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L942-L953] |
| 50 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L954-L964] |
| 51 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._baseline_span` | independent | [docs/registry/implementation-map.yaml:L965-L978] |
| 52 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._line_tensions` | independent | [docs/registry/implementation-map.yaml:L979-L992] |
| 53 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L989-L1002] |
| 54 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1003-L1018] |
| 55 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1019-L1030] |
| 56 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1031-L1042] |
| 57 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1043-L1054] |
| 58 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1055-L1066] |
| 59 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1067-L1078] |
| 60 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1079-L1090] |
| 61 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1091-L1102] |
| 62 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1103-L1114] |
| 63 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1115-L1126] |
| 64 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1127-L1138] |
| 65 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1139-L1150] |
| 66 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1151-L1165] |
| 67 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1166-L1179] |
| 68 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1180-L1191] |
| 69 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1192-L1208] |
| 70 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1209-L1222] |
| 71 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1223-L1234] |
| 72 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L1235-L1249] |
| 73 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1250-L1261] |
| 74 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1262-L1273] |
| 75 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1274-L1285] |
| 76 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1286-L1297] |
| 77 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1298-L1311] |
| 78 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1312-L1323] |
| 79 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1324-L1335] |
| 80 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1336-L1347] |
| 81 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1348-L1361] |
| 82 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1362-L1373] |
| 83 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1374-L1387] |
| 84 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1388-L1399] |
| 85 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1400-L1411] |
| 86 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1412-L1425] |
| 87 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1426-L1437] |
| 88 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L1438-L1453] |
| 89 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1454-L1465] |
| 90 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1466-L1477] |
| 91 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1478-L1489] |
| 92 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L1490-L1503] |
| 93 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1504-L1515] |
| 94 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1516-L1527] |
| 95 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1528-L1541] |
| 96 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1542-L1553] |
| 97 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1554-L1565] |
| 98 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1566-L1579] |
| 99 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1580-L1590] |
| 100 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1591-L1602] |
| 101 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1603-L1614] |
| 102 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1615-L1626] |
| 103 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1627-L1638] |
| 104 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1639-L1652] |
| 105 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1653-L1664] |
| 106 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1665-L1676] |
| 107 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1677-L1690] |
| 108 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1691-L1702] |
| 109 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1703-L1716] |
| 110 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1717-L1730] |
| 111 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1731-L1743] |
| 112 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1744-L1756] |
| 113 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1757-L1769] |
| 114 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1770-L1782] |
| 115 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1783-L1794] |
| 116 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1795-L1806] |
| 117 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1807-L1820] |
| 118 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1821-L1832] |
| 119 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1833-L1844] |
| 120 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1845-L1858] |
| 121 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1859-L1870] |
| 122 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1871-L1882] |
| 123 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1883-L1894] |
| 124 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1895-L1906] |
| 125 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1907-L1918] |
| 126 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1919-L1932] |
| 127 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1933-L1946] |
| 128 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1947-L1960] |
| 129 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1961-L1972] |
| 130 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1973-L1984] |
| 131 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1985-L1996] |
| 132 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L1997-L2008] |
| 133 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L2009-L2020] |
| 134 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L2021-L2032] |
| 135 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L2033-L2046] |
| 136 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L2047-L2060] |
| 137 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L2061-L2074] |
| 138 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2075-L2090] |
| 139 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2091-L2106] |
| 140 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2107-L2123] |

#### Inventory: effective-tension-riser-stack-up

| Map entry | Location / definition | Map relation | Pinned map locator |
| --- | --- | --- | --- |
| 1 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` — `_write_tensions_csv` | independent | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._line_state` | independent | [docs/registry/implementation-map.yaml:L3578-L3589] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._solve_line` | independent | [docs/registry/implementation-map.yaml:L3590-L3599] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._result` | independent | [docs/registry/implementation-map.yaml:L3600-L3609] |
| 139 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L4908-L4917] |

#### Inventory: wave-spectra

| Map entry | Location / definition | Map relation | Pinned map locator |
| --- | --- | --- | --- |
| 1 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13600-L13611] |
| 2 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13612-L13623] |
| 3 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L13624-L13635] |
| 4 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L13636-L13647] |
| 5 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L13648-L13662] |
| 6 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L13663-L13674] |
| 7 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13675-L13686] |
| 8 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13687-L13697] |
| 9 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13698-L13708] |
| 10 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13709-L13720] |
| 11 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13721-L13732] |
| 12 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13733-L13746] |
| 13 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13747-L13757] |
| 14 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13758-L13768] |
| 15 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13769-L13779] |
| 16 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13780-L13790] |
| 17 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13791-L13801] |
| 18 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13802-L13812] |
| 19 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13813-L13823] |
| 20 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13824-L13834] |
| 21 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13835-L13845] |
| 22 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13846-L13856] |
| 23 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13857-L13867] |
| 24 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13868-L13878] |
| 25 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13879-L13890] |
| 26 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13891-L13902] |
| 27 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13903-L13914] |
| 28 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13915-L13926] |
| 29 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13927-L13938] |
| 30 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13939-L13950] |
| 31 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13951-L13964] |
| 32 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13965-L13976] |
| 33 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13977-L13988] |
| 34 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L13989-L14004] |
| 35 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14005-L14017] |
| 36 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14018-L14030] |
| 37 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14031-L14043] |
| 38 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L14044-L14060] |
| 39 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14061-L14072] |
| 40 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14073-L14084] |
| 41 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14085-L14096] |
| 42 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14097-L14110] |
| 43 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14111-L14122] |
| 44 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14123-L14134] |
| 45 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14135-L14146] |
| 46 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14147-L14158] |
| 47 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14159-L14172] |
| 48 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14173-L14186] |
| 49 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14187-L14199] |
| 50 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14200-L14213] |
| 51 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L14214-L14225] |
| 52 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14226-L14237] |
| 53 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14238-L14249] |
| 54 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L14218-L14231] |
| 55 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14262-L14275] |
| 56 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14276-L14287] |
| 57 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14288-L14301] |
| 58 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14302-L14315] |
| 59 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14316-L14328] |
| 60 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14329-L14342] |
| 61 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14343-L14356] |
| 62 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14357-L14368] |
| 63 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14369-L14380] |
| 64 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14381-L14392] |
| 65 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14393-L14408] |
| 66 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L14409-L14421] |
| 67 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14422-L14433] |
| 68 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14434-L14445] |
| 69 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14446-L14459] |
| 70 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14460-L14477] |
| 71 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14478-L14489] |
| 72 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14490-L14501] |
| 73 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14502-L14513] |
| 74 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14514-L14525] |
| 75 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L14526-L14539] |
| 76 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14540-L14551] |
| 77 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L14552-L14563] |
| 78 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14564-L14577] |
| 79 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14578-L14591] |
| 80 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14592-L14605] |
| 81 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L14606-L14617] |
| 82 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L14618-L14629] |
| 83 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14630-L14641] |
| 84 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14642-L14655] |
| 85 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14656-L14667] |
| 86 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14668-L14679] |
| 87 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14680-L14691] |
| 88 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14692-L14703] |
| 89 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14704-L14715] |
| 90 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14716-L14727] |
| 91 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14728-L14739] |
| 92 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14740-L14751] |
| 93 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14752-L14763] |
| 94 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14764-L14775] |
| 95 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14776-L14791] |
| 96 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14792-L14806] |
| 97 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14807-L14818] |
| 98 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14819-L14830] |
| 99 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14831-L14842] |
| 100 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14843-L14854] |
| 101 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14855-L14866] |
| 102 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14867-L14878] |
| 103 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L14879-L14892] |
| 104 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14893-L14906] |
| 105 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14907-L14920] |
| 106 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14921-L14932] |
| 107 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14933-L14944] |
| 108 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14945-L14956] |
| 109 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14957-L14968] |
| 110 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14969-L14980] |
| 111 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14981-L14992] |
| 112 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L14993-L15004] |
| 113 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15005-L15016] |
| 114 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15017-L15028] |
| 115 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15029-L15040] |
| 116 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15041-L15052] |
| 117 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15053-L15064] |
| 118 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15065-L15076] |
| 119 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15077-L15088] |
| 120 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15089-L15100] |
| 121 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15101-L15112] |
| 122 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15113-L15124] |
| 123 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15125-L15136] |
| 124 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15137-L15148] |
| 125 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15149-L15160] |
| 126 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15161-L15172] |
| 127 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15173-L15184] |
| 128 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15185-L15198] |
| 129 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15199-L15210] |
| 130 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15211-L15222] |
| 131 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15223-L15234] |
| 132 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15235-L15246] |
| 133 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15247-L15258] |
| 134 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15259-L15270] |
| 135 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15271-L15284] |
| 136 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15285-L15298] |
| 137 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15299-L15314] |
| 138 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15315-L15328] |
| 139 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L15329-L15342] |
| 140 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L15343-L15361] |
| 141 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15362-L15377] |

#### Inventory: wind-and-current-force-routes

| Map entry | Location / definition | Map relation | Pinned map locator |
| --- | --- | --- | --- |
| 1 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` — `OCIMFDatabase.get_coefficients` | copy | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` — `EnvironmentalForces.calculate_total_forces` | copy | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` — `create_sample_database` | copy | [docs/registry/implementation-map.yaml:L16117-L16126] |
| 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._environmental_forces` | independent | [docs/registry/implementation-map.yaml:L16127-L16139] |
| 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._ocimf_database` | independent | [docs/registry/implementation-map.yaml:L16140-L16149] |
| 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` — `_resolve_arrow_direction_in_body_frame` | independent | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` — `polar_force_overlay` | independent | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16782-L16791] |

#### Inventory: sn-curves-and-miner-damage

| Map entry | Location / definition | Map relation | Pinned map locator |
| --- | --- | --- | --- |
| 1 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | Outside package; original path and symbol at pinned locator | example | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` — `SNcurve.cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` — `RainflowFatigue.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` — `MinersRuleDamage.compute` | independent | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` — `ScatterDiagramFatigue.compute` | independent | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` — `compute_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | Outside package; original path and symbol at pinned locator | copy | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | Outside package; original path and symbol at pinned locator | wrapper | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | Outside package; original path and symbol at pinned locator | caller | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | Outside package; original path and symbol at pinned locator | test_helper | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | Outside package; original path and symbol at pinned locator | independent | [docs/registry/implementation-map.yaml:L20939-L20948] |

### Map follow-up and whole-repository discovery

The following whole-repository Python searches include `scripts/`; their hits are discovery candidates, not counts of independent implementations.

| Family | Matching files | Matching script files | Matching lines |
| --- | --- | --- | --- |
| `catenary-mooring-line-statics` | 356 | 42 | 3327 |
| `effective-tension-riser-stack-up` | 390 | 20 | 2892 |
| `wave-spectra` | 494 | 60 | 3237 |
| `wind-and-current-force-routes` | 295 | 23 | 1985 |
| `sn-curves-and-miner-damage` | 317 | 30 | 4421 |

The external environmental force formulas and alternate rainflow/S-N paths remain map reconciliation candidates rather than changes to the counted-package result, issue pending (9). [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L626] [src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py:L128-L200] [src/digitalmodel/structural/fatigue_apps/rainflow_counter.py:L164-L220] [src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py:L106-L132]

Map follow-up shall add the disclosed local force/counter definitions and review families without in-package map rows: coefficient matrix interpolation/causality and damping ratios, AQWA matrix parsing, mooring-component selection/property formulas, the demo mean-wave-drift proxy and polar-force visualization. Those families are not counted until map rows are added; existing family-expansion issue #2620 and issue pending (9) cover reconciliation. [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L17-L35] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L130-L140] [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L134-L190] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L325-L375] [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L273-L357]

## 10. Coverage and open items

### Execution record

All runs use the code at `32fe066f387c13a66fd234eca2850a00c16e104a`; AFTER runs include only the knowledge-pack and routing edits in the working tree. The source-copy digest comparison covers all 20 local Python files.

Generic invocation (the full-path executable form and dedicated external environment were used):

```sh
PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<name>
```

No pytest addopts override, test deselection or new expected-failure marker is used. The measured unit-run seed is 3531540422; subsequent runs reuse it explicitly. Output-writing integration tests and both workflow cases run in a scratch copy, with inherited Git root bindings cleared.

| Command group | Collected | Passed | Failed | Skipped | Existing XFAIL | NOT_RUN |
| --- | --- | --- | --- | --- | --- | --- |
| base-unit | 410 | 400 | 0 | 1 | 9 | 0 |
| base-integration | 40 | 40 | 0 | 0 | 0 | 0 |
| base-extra | 82 | 77 | 0 | 1 | 5 | 0 |
| after-unit | 410 | 400 | 0 | 1 | 9 | 0 |
| after-integration | 40 | 40 | 0 | 0 | 0 | 0 |
| after-extra | 82 | 77 | 0 | 1 | 5 | 0 |
| contracts | 1114 | 1054 | 0 | 61 | 0 | 0 |
| contracts-final | 11 | 11 | 0 | 0 | 0 | 0 |

Unit groups select every `test_*.py` below the module test directory except its separately executed integration folder; integration groups select that folder plus the two exact workflow nodes in section 4. Extra groups select `tests/test_integration_phase1.py`, `tests/fatigue/test_rainflow_invariants.py` and `tests/marine_ops/marine_analysis/test_ocimf_charts.py`. The contracts group uses every path in the contracts row of `tests/DOMAINS.md`. [tests/DOMAINS.md:L13-L13]

A collection-level skip is reported separately by pytest and can make the sum of result statuses exceed the collected-item count; expected failures remain separate from passes.

| Existing non-pass | BASE result | AFTER result | Disposition / tracking |
| --- | --- | --- | --- |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestComputeFatigueLife::test_compute_fatigue_life_returns_report_dict` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestComputeFatigueLife::test_compute_requires_either_csv_or_scatter` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestComputeFatigueLife::test_higher_tension_amplitude_yields_higher_damage` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestComputeFatigueLife::test_compute_fatigue_life_with_scatter_diagram` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestScatterDiagramFatigue::test_single_seastate_with_probability_one` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestScatterDiagramFatigue::test_scatter_probabilities_must_sum_to_one` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestScatterDiagramFatigue::test_scatter_weighted_damage_is_probability_weighted_sum` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py::TestScatterDiagramFatigue::test_missing_seastate_tension_raises_keyerror` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L50] |
| `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py::test_solver_against_reconciled_fixture` | XFAIL | XFAIL | Existing strict marker; #2050; [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L1-L50] |
| `tests/fatigue/test_rainflow_invariants.py::test_7a_contract_fapps_counting_broadband` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/fatigue/test_rainflow_invariants.py:L1-L50] |
| `tests/fatigue/test_rainflow_invariants.py::test_6_contract_calm_buoy_narrow_band` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/fatigue/test_rainflow_invariants.py:L1-L50] |
| `tests/fatigue/test_rainflow_invariants.py::test_5_contract_sigproc_astm_broadband` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/fatigue/test_rainflow_invariants.py:L1-L50] |
| `tests/fatigue/test_rainflow_invariants.py::test_7b_contract_struct_fatigue_broadband` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/fatigue/test_rainflow_invariants.py:L1-L50] |
| `tests/fatigue/test_rainflow_invariants.py::test_12b_pypi_rainflow_two_point_signal` | XFAIL | XFAIL | Existing strict marker; #1721 and #1843; [tests/fatigue/test_rainflow_invariants.py:L1-L50] |
| Optional coefficient workbook test | SKIP | SKIP | Workbook unavailable; no private workbook is retrieved; #1823; [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L87-L96] |
| Phase-1 collection | SKIP | SKIP | Referenced local mooring module is absent; no missing extra can create it; #1795; [tests/test_integration_phase1.py:L19-L25] |

The all-routes RAO file contributes no pytest node; its script/fixture qualification remains open under #2008. [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L381-L400]

### Read coverage

All 20 local Python files are read; the package has 5,234 source lines. The module test tree has 36 Python files and 437 syntactic test-function definitions before pytest parametrization and collection rules. These are inventory counts rather than a validation claim.

| Local source | Line coverage read |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_engineering/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/__init__.py:L1-L73] |
| `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | [src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:L1-L600] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/__init__.py:L1-L59] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L1-L946] |
| `src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/__init__.py:L1-L70] |
| `src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py` | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/aqwa_parser.py:L1-L393] |
| `src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py` | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/coefficients.py:L1-L456] |
| `src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py` | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/plotting.py:L1-L673] |
| `src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py` | [src/digitalmodel/marine_ops/marine_engineering/hydrodynamic_coefficients/workflow.py:L1-L137] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring/__init__.py` | Git tree entry; no executable lines |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/__init__.py:L1-L20] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L1-L509] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_catenary.py:L1-L37] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py:L1-L125] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py:L1-L400] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py` | [src/digitalmodel/marine_ops/marine_engineering/visualization/__init__.py:L1-L24] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py` | [src/digitalmodel/marine_ops/marine_engineering/visualization/_convention.py:L1-L97] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | [src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py:L1-L427] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/types.py` | [src/digitalmodel/marine_ops/marine_engineering/visualization/types.py:L1-L76] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/vessel_silhouettes.py` | [src/digitalmodel/marine_ops/marine_engineering/visualization/vessel_silhouettes.py:L1-L112] |

| Test-tree source | Syntactic test definitions | Source locator |
| --- | --- | --- |
| `tests/marine_ops/marine_engineering/__init__.py` | 0 | [tests/marine_ops/marine_engineering/__init__.py:L1-L1] |
| `tests/marine_ops/marine_engineering/catenary/__init__.py` | 0 | [tests/marine_ops/marine_engineering/catenary/__init__.py:L1-L1] |
| `tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py` | 18 | [tests/marine_ops/marine_engineering/catenary/test_catenary_integration.py:L1-L384] |
| `tests/marine_ops/marine_engineering/catenary/test_catenary_perf.py` | 14 | [tests/marine_ops/marine_engineering/catenary/test_catenary_perf.py:L1-L397] |
| `tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py` | 6 | [tests/marine_ops/marine_engineering/catenary/test_catenary_reference.py:L1-L278] |
| `tests/marine_ops/marine_engineering/catenary/test_run_quick.py` | 0 | [tests/marine_ops/marine_engineering/catenary/test_run_quick.py:L1-L65] |
| `tests/marine_ops/marine_engineering/catenary/test_simplified.py` | 25 | [tests/marine_ops/marine_engineering/catenary/test_simplified.py:L1-L410] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | 26 | [tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py:L1-L526] |
| `tests/marine_ops/marine_engineering/integration/__init__.py` | 0 | [tests/marine_ops/marine_engineering/integration/__init__.py:L1-L10] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | 0 | [tests/marine_ops/marine_engineering/integration/integration_example.py:L1-L403] |
| `tests/marine_ops/marine_engineering/integration/test_end_to_end_workflow.py` | 4 | [tests/marine_ops/marine_engineering/integration/test_end_to_end_workflow.py:L1-L563] |
| `tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py` | 8 | [tests/marine_ops/marine_engineering/integration/test_hydro_rao_integration.py:L1-L435] |
| `tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py` | 6 | [tests/marine_ops/marine_engineering/integration/test_marine_eng_performance.py:L1-L454] |
| `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | 9 | [tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py:L1-L538] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | 11 | [tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py:L1-L335] |
| `tests/marine_ops/marine_engineering/legacy/test_component_database.py` | 18 | [tests/marine_ops/marine_engineering/legacy/test_component_database.py:L1-L703] |
| `tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py` | 11 | [tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py:L1-L557] |
| `tests/marine_ops/marine_engineering/legacy/test_validation.py` | 9 | [tests/marine_ops/marine_engineering/legacy/test_validation.py:L1-L114] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | 20 | [tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py:L1-L477] |
| `tests/marine_ops/marine_engineering/test_adapter_manual.py` | 5 | [tests/marine_ops/marine_engineering/test_adapter_manual.py:L1-L301] |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | 41 | [tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py:L1-L587] |
| `tests/marine_ops/marine_engineering/test_catenary_adapter.py` | 23 | [tests/marine_ops/marine_engineering/test_catenary_adapter.py:L1-L518] |
| `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` | 40 | [tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py:L1-L896] |
| `tests/marine_ops/marine_engineering/test_component_database.py` | 18 | [tests/marine_ops/marine_engineering/test_component_database.py:L1-L703] |
| `tests/marine_ops/marine_engineering/test_component_database_public.py` | 15 | [tests/marine_ops/marine_engineering/test_component_database_public.py:L1-L220] |
| `tests/marine_ops/marine_engineering/test_hydro_coefficients.py` | 31 | [tests/marine_ops/marine_engineering/test_hydro_coefficients.py:L1-L513] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | 14 | [tests/marine_ops/marine_engineering/test_marine_eng_perf.py:L1-L377] |
| `tests/marine_ops/marine_engineering/test_mooring_catenary.py` | 13 | [tests/marine_ops/marine_engineering/test_mooring_catenary.py:L1-L619] |
| `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` | 0 | [tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py:L1-L402] |
| `tests/marine_ops/marine_engineering/test_unified_rao_reader.py` | 12 | [tests/marine_ops/marine_engineering/test_unified_rao_reader.py:L1-L255] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | 20 | [tests/marine_ops/marine_engineering/test_wave_spectra.py:L1-L477] |
| `tests/marine_ops/marine_engineering/validation_script.py` | 0 | [tests/marine_ops/marine_engineering/validation_script.py:L1-L55] |
| `tests/marine_ops/marine_engineering/visualization/__init__.py` | 0 | Git tree entry; no executable lines |
| `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | 3 | [tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py:L1-L183] |
| `tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay.py` | 13 | [tests/marine_ops/marine_engineering/visualization/test_polar_force_overlay.py:L1-L351] |
| `tests/marine_ops/marine_engineering/visualization/test_vessel_silhouettes.py` | 4 | [tests/marine_ops/marine_engineering/visualization/test_vessel_silhouettes.py:L1-L64] |

The documentation search finds 103 neutrally named text documents: 30 are fully reviewed and 73 receive discovery scans only. One further discovery file with an identifying name is omitted from the public path list and remains unreviewed; issue pending (8).

| Document | Read depth | Discovery citation |
| --- | --- | --- |
| `docs/data/OCIMF_CORPUS_README.md` | Full text | [docs/data/OCIMF_CORPUS_README.md:L66-L66] |
| `docs/domains/aqwa/aqwa_parser_fix_complete.md` | Full text | [docs/domains/aqwa/aqwa_parser_fix_complete.md:L84-L84] |
| `docs/domains/aqwa/aqwa_parser_fix_summary.md` | Full text | [docs/domains/aqwa/aqwa_parser_fix_summary.md:L165-L165] |
| `docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md` | Full text | [docs/domains/fatigue/rainflow-path-exposure-2026-09-11.md:L176-L176] |
| `docs/domains/marine_ops/marine_engineering/README.md` | Full text | [docs/domains/marine_ops/marine_engineering/README.md:L1-L1] |
| `docs/domains/mooring/MOORING_LINE_TYPES.md` | Full text | [docs/domains/mooring/MOORING_LINE_TYPES.md:L13-L13] |
| `docs/domains/mooring/ocimf_implementation_summary.md` | Full text | [docs/domains/mooring/ocimf_implementation_summary.md:L9-L9] |
| `docs/domains/mooring/ocimf_module_readme.md` | Full text | [docs/domains/mooring/ocimf_module_readme.md:L21-L21] |
| `docs/domains/ship-design/stiff_vs_tender_ships.md` | Full text | [docs/domains/ship-design/stiff_vs_tender_ships.md:L67-L67] |
| `docs/plans/2026-05-05-issue-554-catenary-solver-bracketing-sinh-overflow.md` | Full text | [docs/plans/2026-05-05-issue-554-catenary-solver-bracketing-sinh-overflow.md:L11-L11] |
| `docs/plans/2026-05-05-issue-555-chain-database-diameters-slice.md` | Full text | [docs/plans/2026-05-05-issue-555-chain-database-diameters-slice.md:L8-L8] |
| `docs/plans/2026-05-05-issue-556-ocimf-coefficients-interpolation.md` | Full text | [docs/plans/2026-05-05-issue-556-ocimf-coefficients-interpolation.md:L8-L8] |
| `docs/plans/2026-05-05-issue-557-ocimf-boundary-warnings.md` | Full text | [docs/plans/2026-05-05-issue-557-ocimf-boundary-warnings.md:L8-L8] |
| `docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md` | Full text | [docs/plans/2026-05-05-issue-558-rao-damping-affects-phase.md:L8-L8] |
| `docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md` | Full text | [docs/plans/2026-05-05-issue-559-rao-full-matrix-interpolation.md:L14-L14] |
| `docs/plans/2026-05-05-issue-560-rao-coupling-terms-affect-response.md` | Full text | [docs/plans/2026-05-05-issue-560-rao-coupling-terms-affect-response.md:L8-L8] |
| `docs/plans/2026-05-05-issue-561-ocimf-mooring-combined-environmental.md` | Full text | [docs/plans/2026-05-05-issue-561-ocimf-mooring-combined-environmental.md:L8-L8] |
| `docs/plans/2026-05-05-issue-562-marine-eng-perf-numpy-bool-json.md` | Full text | [docs/plans/2026-05-05-issue-562-marine-eng-perf-numpy-bool-json.md:L8-L8] |
| `docs/plans/2026-05-05-issue-563-marine-eng-perf-ocimf-database.md` | Full text | [docs/plans/2026-05-05-issue-563-marine-eng-perf-ocimf-database.md:L8-L8] |
| `docs/plans/2026-05-05-issue-564-ocimf-mooring-environmental-forces-total.md` | Full text | [docs/plans/2026-05-05-issue-564-ocimf-mooring-environmental-forces-total.md:L8-L8] |
| `docs/plans/2026-05-05-issue-565-hydro-coefficients-csv-to-visualization.md` | Full text | [docs/plans/2026-05-05-issue-565-hydro-coefficients-csv-to-visualization.md:L8-L8] |
| `docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md` | Full text | [docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md:L17-L17] |
| `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md` | Full text | [docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md:L9-L9] |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | Full text | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L13-L13] |
| `docs/plans/2026-09-25-issue-2207-b401-f103-tables.md` | Full text | [docs/plans/2026-09-25-issue-2207-b401-f103-tables.md:L26-L26] |
| `docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md` | Full text | [docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md:L21-L21] |
| `docs/plans/2026-09-25-issue-2211-f103-module-kernel-design-loop.md` | Full text | [docs/plans/2026-09-25-issue-2211-f103-module-kernel-design-loop.md:L33-L33] |
| `docs/plans/2026-09-25-issue-2213-cp-test-hygiene.md` | Full text | [docs/plans/2026-09-25-issue-2213-cp-test-hygiene.md:L21-L21] |
| `docs/plans/2026-09-26-issue-2208-cp-editions.md` | Full text | [docs/plans/2026-09-26-issue-2208-cp-editions.md:L97-L97] |
| `docs/registry/implementation-map.md` | Full text | [docs/registry/implementation-map.md:L17-L17] |
| `docs/domains/blender/BLENDER_AUTOMATION_MODULE.md` | Discovery only; issue pending (8) | [docs/domains/blender/BLENDER_AUTOMATION_MODULE.md:L39-L39] |
| `docs/domains/catenary/CATENARY_CONSOLIDATION_COMPLETE.md` | Discovery only; issue pending (8) | [docs/domains/catenary/CATENARY_CONSOLIDATION_COMPLETE.md:L12-L12] |
| `docs/domains/catenary/CATENARY_CONSOLIDATION_EXECUTIVE_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/catenary/CATENARY_CONSOLIDATION_EXECUTIVE_SUMMARY.md:L24-L24] |
| `docs/domains/catenary/catenary_adapter_summary.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_adapter_summary.md:L15-L15] |
| `docs/domains/catenary/catenary_consolidation_plan.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_consolidation_plan.md:L13-L13] |
| `docs/domains/catenary/catenary_module_migration_status.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_module_migration_status.md:L9-L9] |
| `docs/domains/catenary/catenary_port_summary.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_port_summary.md:L9-L9] |
| `docs/domains/catenary/catenary_test_results.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_test_results.md:L5-L5] |
| `docs/domains/catenary/catenary_usage_analysis.md` | Discovery only; issue pending (8) | [docs/domains/catenary/catenary_usage_analysis.md:L123-L123] |
| `docs/domains/catenary/implementation_status_catenary_solver.md` | Discovery only; issue pending (8) | [docs/domains/catenary/implementation_status_catenary_solver.md:L3-L3] |
| `docs/domains/catenary/lazy_wave_migration_summary.md` | Discovery only; issue pending (8) | [docs/domains/catenary/lazy_wave_migration_summary.md:L5-L5] |
| `docs/domains/cathodic_protection/_index.md` | Discovery only; issue pending (8) | [docs/domains/cathodic_protection/_index.md:L230-L230] |
| `docs/domains/cathodic_protection/cp_bug_fixes_summary.md` | Discovery only; issue pending (8) | [docs/domains/cathodic_protection/cp_bug_fixes_summary.md:L289-L289] |
| `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` | Discovery only; issue pending (8) | [docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md:L97-L97] |
| `docs/domains/charts/phase2/README.md` | Discovery only; issue pending (8) | [docs/domains/charts/phase2/README.md:L5-L5] |
| `docs/domains/charts/phase2/validation/README.md` | Discovery only; issue pending (8) | [docs/domains/charts/phase2/validation/README.md:L91-L91] |
| `docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L770-L770] |
| `docs/domains/data/DATA_PROCUREMENT_STRATEGY.md` | Discovery only; issue pending (8) | [docs/domains/data/DATA_PROCUREMENT_STRATEGY.md:L5-L5] |
| `docs/domains/data/DATA_REORGANIZATION_PLAN.md` | Discovery only; issue pending (8) | [docs/domains/data/DATA_REORGANIZATION_PLAN.md:L27-L27] |
| `docs/domains/development/stability_check_results.md` | Discovery only; issue pending (8) | [docs/domains/development/stability_check_results.md:L113-L113] |
| `docs/domains/development/test_summary_with_validations.md` | Discovery only; issue pending (8) | [docs/domains/development/test_summary_with_validations.md:L107-L107] |
| `docs/domains/domains_index.md` | Discovery only; issue pending (8) | [docs/domains/domains_index.md:L9-L9] |
| `docs/domains/examples/propagation_setup_example.md` | Discovery only; issue pending (8) | [docs/domains/examples/propagation_setup_example.md:L17-L17] |
| `docs/domains/hydrodynamics/morison-elements-assessment.md` | Discovery only; issue pending (8) | [docs/domains/hydrodynamics/morison-elements-assessment.md:L117-L117] |
| `docs/domains/hydrodynamics/propeller-4-quadrant-data.md` | Discovery only; issue pending (8) | [docs/domains/hydrodynamics/propeller-4-quadrant-data.md:L503-L503] |
| `docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md:L90-L90] |
| `docs/domains/marine-engineering/MARINE_ENGINEERING_DOCUMENTATION_INDEX.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/MARINE_ENGINEERING_DOCUMENTATION_INDEX.md:L1-L1] |
| `docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md:L1-L1] |
| `docs/domains/marine-engineering/MARINE_QUICK_REFERENCE.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/MARINE_QUICK_REFERENCE.md:L1-L1] |
| `docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md:L211-L211] |
| `docs/domains/marine-engineering/marine-engineering-excel-mapping.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine-engineering-excel-mapping.md:L1-L1] |
| `docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md:L1-L1] |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine_3d_models_catalog.md:L1-L1] |
| `docs/domains/marine-engineering/marine_excel_analysis_report.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine_excel_analysis_report.md:L1-L1] |
| `docs/domains/marine-engineering/marine_excel_analysis_summary.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine_excel_analysis_summary.md:L1-L1] |
| `docs/domains/marine-engineering/marine_implementation_metrics.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/marine_implementation_metrics.md:L1-L1] |
| `docs/domains/marine-engineering/python-code-passing-ship/configuration_guide.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/python-code-passing-ship/configuration_guide.md:L589-L589] |
| `docs/domains/marine-engineering/unified_rao_reader_guide.md` | Discovery only; issue pending (8) | [docs/domains/marine-engineering/unified_rao_reader_guide.md:L5-L5] |
| `docs/domains/metocean/wave_spectra_implementation_summary.md` | Discovery only; issue pending (8) | [docs/domains/metocean/wave_spectra_implementation_summary.md:L6-L6] |
| `docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md` | Discovery only; issue pending (8) | [docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md:L583-L583] |
| `docs/domains/orcaflex/RAO_PARSER_FIX_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/orcaflex/RAO_PARSER_FIX_SUMMARY.md:L91-L91] |
| `docs/domains/project-docs/CROSS_REPO_PROPAGATION.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/CROSS_REPO_PROPAGATION.md:L77-L77] |
| `docs/domains/project-docs/EXTRACTION_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/EXTRACTION_SUMMARY.md:L11-L11] |
| `docs/domains/project-docs/FINAL_TEST_BASELINE.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/FINAL_TEST_BASELINE.md:L61-L61] |
| `docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md:L1-L1] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE1_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/IMPLEMENTATION_PHASE1_SUMMARY.md:L9-L9] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md:L1007-L1007] |
| `docs/domains/project-docs/MIGRATION_GUIDE.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/MIGRATION_GUIDE.md:L1-L1] |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L492-L492] |
| `docs/domains/project-docs/PHASE1_FINAL_STATUS.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/PHASE1_FINAL_STATUS.md:L2-L2] |
| `docs/domains/project-docs/PHASE3_PERFORMANCE_DELIVERABLES.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/PHASE3_PERFORMANCE_DELIVERABLES.md:L4-L4] |
| `docs/domains/project-docs/PHASE_2_IMPLEMENTATION_STRATEGY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/PHASE_2_IMPLEMENTATION_STRATEGY.md:L11-L11] |
| `docs/domains/project-docs/PHASE_2_TASK_SPECIFICATIONS.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/PHASE_2_TASK_SPECIFICATIONS.md:L17-L17] |
| `docs/domains/project-docs/PRODUCTION_READINESS_CHECKLIST.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/PRODUCTION_READINESS_CHECKLIST.md:L1-L1] |
| `docs/domains/project-docs/REORGANIZATION_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/REORGANIZATION_SUMMARY.md:L1-L1] |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L78-L78] |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L107-L107] |
| `docs/domains/project-docs/USER_DOCUMENTATION_COMPLETE.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/USER_DOCUMENTATION_COMPLETE.md:L1-L1] |
| `docs/domains/project-docs/performance_optimization_report.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/performance_optimization_report.md:L1-L1] |
| `docs/domains/project-docs/phase1-implementation-report.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase1-implementation-report.md:L12-L12] |
| `docs/domains/project-docs/phase1_action_plan.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase1_action_plan.md:L7-L7] |
| `docs/domains/project-docs/phase1_final_validation_report.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase1_final_validation_report.md:L49-L49] |
| `docs/domains/project-docs/phase1_test_report.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase1_test_report.md:L2-L2] |
| `docs/domains/project-docs/phase2_data_extraction_plan.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase2_data_extraction_plan.md:L12-L12] |
| `docs/domains/project-docs/phase2_implementation_plan.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase2_implementation_plan.md:L13-L13] |
| `docs/domains/project-docs/phase2_validation_summary.md` | Discovery only; issue pending (8) | [docs/domains/project-docs/phase2_validation_summary.md:L314-L314] |
| `docs/domains/reporting/standard-report-pack.md` | Discovery only; issue pending (8) | [docs/domains/reporting/standard-report-pack.md:L4-L4] |
| `docs/domains/risers/scr-slwr-coverage-2026-10.md` | Discovery only; issue pending (8) | [docs/domains/risers/scr-slwr-coverage-2026-10.md:L123-L123] |
| `docs/domains/standards/RAO_PHASE_CONVENTIONS.md` | Discovery only; issue pending (8) | [docs/domains/standards/RAO_PHASE_CONVENTIONS.md:L3-L3] |
| `docs/domains/test_automation/pipeline_mock_patterns.md` | Discovery only; issue pending (8) | [docs/domains/test_automation/pipeline_mock_patterns.md:L160-L160] |
| `docs/maps/digitalmodel-operator-map.md` | Discovery only; issue pending (8) | [docs/maps/digitalmodel-operator-map.md:L40-L40] |
| `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` | Discovery only; issue pending (8) | [docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md:L18-L18] |
| `docs/vision/CALCULATIONS-VISION.md` | Discovery only; issue pending (8) | [docs/vision/CALCULATIONS-VISION.md:L189-L189] |

### Open items

| Item | Limit / required next evidence | Responsible role | Issue |
| --- | --- | --- | --- |
| U1 | Full coefficient units, origin and phase metadata are not established | Maintainer | issue pending (2) |
| U2 | Standards compliance, complete editions/clauses and emitted calculation sidecars are not established | Maintainer | #1805, #1798 and #1823 |
| Independent workflow qualification | The two workflow cases are regressions; no independent full-mooring result is claimed | Maintainer | #1823; issue pending (5) |
| Parser/data/input failures | Local numerical input, truncation, zero-fill, fallback and cache branches in section 6 need direct failure-injection tests | Maintainer | issue pending (1), issue pending (2), issue pending (3), issue pending (5), issue pending (6) |
| Chart fidelity | Missing/undefined values, signed radii and arrow modes need direct numerical figure-data checks | Maintainer | issue pending (4) |
| QA evidence and direct local coverage | Generated appendix, input fingerprints, sidecars, independent role registration and shared-log entries are not established | Maintainer | issue pending (7) |
| Historical documentation coverage | Complete the 73 discovery-only documents plus 1 identifying-name file; reconcile stale statements through neutral locators | Maintainer | issue pending (8) |
| Map follow-up | Add disclosed definitions, reconcile external alternatives and introduce the missing in-package method families | Maintainer | issue pending (9) |
| Baseline routing duplicate | Strict YAML parsing rejects the existing duplicate solvers.submodules keys at pinned lines 305 and 313; the new module entry does not change them | Maintainer | issue pending (10) |
| RAO worked-example lead | Provide a real test node and available neutral reference fixture for the script-only lead | Maintainer | #2008 |
| Production catenary | Keep the independent reference separate from the known production defect | Maintainer | #2050 |
| Fatigue pipeline and degenerate counts | Refusal-gated damage and existing counting-contract expected failures remain visible | Maintainer | #1721 and #1843 |
| Optional reference workbook | Workbook-based trace parity is not established without the required coefficient workbook | Maintainer | #1823 |

Strict duplicate-key parsing fails on the pre-existing solvers mapping; syntax parsing and preservation of unrelated parsed entries pass, with zero new duplicate paths. [docs/registry/module-routing.yaml:L305-L313]

The pack remains DRAFT until the independent quality manager resolves the draft issue references, checks the cited evidence and records clearance. No private sidecar is written and no client-derived values are included.
