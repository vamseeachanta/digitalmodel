# Vertical riser knowledge pack

| Header | Value |
| --- | --- |
| Module | subsea/vertical_riser |
| Repository | digitalmodel |
| Package | src/digitalmodel/subsea/vertical_riser |
| Pack revision | 0 |
| Code revision | 444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55 |
| Analysis family | OrcaFlex riser and mooring; routing family MR |
| Quality role | OrcaFlex riser/mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`vertical_riser(cfg)` prepares spreadsheet-derived riser properties, assembles model data, saves plots and a stack-up table, and optionally prepares a second model for VIV. [src/digitalmodel/subsea/vertical_riser/vertical_riser.py:L12-L25]
`VerticalRiser` delegates stack-up force calculations to `TypicalRiserStackUpCalculations` and model writing to separate builders; these delegates are outside this package's implementation scope. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L331-L388]
The existing subsea domain has maturity `stable`; that registry value is a domain designation, not evidence that this module's complete workflow has been validated. [docs/registry/module-routing.yaml:L590-L595]
The riser-type selection accepts drilling, intervention and top-tensioned flags in that order, fixes the analysis type to fatigue and selects cyclic geotechnical data. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L59-L98]
UNVERIFIED: complete-workflow applicability bounds for water depth, geometry, loading and target solver versions are not established by the dedicated arithmetic tests; see issue pending (4).

## 2. Inputs and outputs

| Input | Unit / convention | Allowed range and default | Source and evidence |
| --- | --- | --- | --- |
| `cfg.stack_up.from_xlsx` | Spreadsheet tables assigned by configured label | No schema or range validation in this reader | `read_riser_input_data`; [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L41-L48] |
| Component elevations, length and water-depth marker | ft on input; converted to m | Required marker rows; no range checks here | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L64-L88]; [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L57-L94] |
| Pipe OD, wall thickness, drag diameter | inch on input; converted to m | Drag diameter falls back to buoyancy diameter on NaN; no positivity check | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L127]; [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L163-L166]; [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L224-L227] |
| Measured dry and wet weights | Implicit kip-equivalent mass converted with a pound-to-kilogram factor and divided by length in m | Required for measured branch; no unit tag validation | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L168-L181] |
| Material modulus `E` | psi to kPa, equivalent to kN/m² | Required key; conversion accepts zero in the synthetic probe | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L263-L291]; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| Section properties | in² to m²; in⁴ to m⁴; stiffness factors applied in place | Required dictionary keys; no range validation | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L271-L289] |
| Fluid and seawater `Rho` | Density consumed in the buoyancy expressions | Required keys; no range validation in these expressions | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L182-L209] |
| `Analysis.result_folder`, `Analysis.file_name`, `plot_settings` | Output path components and plotting settings | Required keys; no directory-creation contract here | [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L380-L407] |

| Output | Unit / convention | Destination and evidence |
| --- | --- | --- |
| Mutated section-property dictionary | m², m⁴, kPa and converted stiffnesses; neighboring mass field retained by this conversion | Same dictionary object; [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L271-L291] |
| `stack_up_properties_df` | Lengths/elevations in m; line weight kN/m; effective tension kN | Delegated table and `cfg.Analysis.result`; [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L12-L55]; [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L193-L209] |
| CSV and PNG | CSV of builder stack-up table; configured plot axes | Configured result destination; [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L380-L407] |
| Return value | Original configuration with delegated results | [src/digitalmodel/subsea/vertical_riser/vertical_riser.py:L12-L25] |

The delegated axial coordinate increases downward from end A, while elevation above mudline is propagated upward from the wellhead reference; elevation above MSL subtracts water depth. [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L57-L94] [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L108-L172]
UNVERIFIED: a complete horizontal frame, wave phase convention and versioned file-format unit contract are not established here; see issue pending (4).

## 3. Method and governing standards

| Method | Implementation and reference | Provenance limit |
| --- | --- | --- |
| Cross-section sizing | `get_fea_properties` constructs pipe input and calls `PipeSizing.get_pipe_system_properties`. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L147] | API 5L appears as a material-grade selector, not an implemented compliance check. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L128-L138] |
| Unit boundary | `update_property_dictionary_to_si_units` converts areas, inertias, stiffness, modulus and strengths in place. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L263-L291] | Strength conversion has a repeated assignment; do not use it as an independent reference. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L287-L289] |
| Weight branches | Measured mass per length derives from supplied dry/wet quantities; calculated branch subtracts a displaced-fluid expression from dry mass per length. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L168-L209] | Mixed mass and force dimensions in the calculated wet branch require correction or a documented basis; issue pending (2). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L196-L209] |
| Auxiliary internal area | Matching auxiliary components add the area at index zero, then feed an effective internal diameter. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L229-L236] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L336-L356] | Initialization precedes joint sizing; multi-line and ordering defects are tracked in issue pending (3). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L35-L37] |
| Suspended weight and effective tension | `evaluate_stack_up_properties` delegates to the shared calculation class. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L331-L334] | The independent test uses vertical force equilibrium and supplied wet mass; it is not a complete riser design assessment. [tests/subsea/vertical_riser/test_suspended_tension.py:L4-L26] |
| Model and VIV preparation | OrcaFlex builder delegation; optional uniform segment length for the VIV model. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L376] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L409-L413] | Solver execution and acceptance are separate QA gates. AQG r0, sections 5–7. |

UNVERIFIED: API 5L edition, clause/table and material-grade provenance are absent from the selector; clause text not checked against the licensed copy; see issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L128-L138]
The package's conversion and material-selection methods do not emit a `Citation` sidecar; provenance work is retained in issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L147] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L263-L291]

## 4. Validated worked examples

The following inputs are synthetic; the modulus reference is independent dimensional arithmetic using NIST SP 811, Appendix B.8, and the force reference is static vertical equilibrium using OpenStax, *University Physics Volume 1* (2016), section 12.1, equation 12.2, with BIPM, *SI Brochure*, ninth edition, Appendix 1, third CGPM declaration on weight. [tests/subsea/vertical_riser/test_component_units.py:L1-L27] [tests/subsea/vertical_riser/test_suspended_tension.py:L4-L39]
Reference pages were opened read only for this pack; the synthetic expected values and test tolerances are recorded below, without claiming design acceptance.

| Case | Input and independent expectation | Tolerance and basis | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| V1 | `E=0.0` psi; expected `E_kPa=E_psi*(4.448222/0.0254**2/1000)` | Relative `2e-7`, rounded force-reference precision; [tests/subsea/vertical_riser/test_component_units.py:L19-L27] | `tests/subsea/vertical_riser/test_component_units.py::test_component_modulus_units[0.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| V2 | `E=1.0` psi; expected `E_kPa=E_psi*(4.448222/0.0254**2/1000)` | Relative `2e-7`, rounded force-reference precision; [tests/subsea/vertical_riser/test_component_units.py:L19-L27] | `tests/subsea/vertical_riser/test_component_units.py::test_component_modulus_units[1.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| V3 | `E=1000.0` psi; expected `E_kPa=E_psi*(4.448222/0.0254**2/1000)` | Relative `2e-7`, rounded force-reference precision; [tests/subsea/vertical_riser/test_component_units.py:L19-L27] | `tests/subsea/vertical_riser/test_component_units.py::test_component_modulus_units[1000.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| V4 | `E=10000000.0` psi; expected `E_kPa=E_psi*(4.448222/0.0254**2/1000)` | Relative `2e-7`, rounded force-reference precision; [tests/subsea/vertical_riser/test_component_units.py:L19-L27] | `tests/subsea/vertical_riser/test_component_units.py::test_component_modulus_units[10000000.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| V5 | `E=30000000.0` psi; expected `E_kPa=E_psi*(4.448222/0.0254**2/1000)` | Relative `2e-7`, rounded force-reference precision; [tests/subsea/vertical_riser/test_component_units.py:L19-L27] | `tests/subsea/vertical_riser/test_component_units.py::test_component_modulus_units[30000000.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_component_units.py:L64-L77] |
| V6 | `100` ft × `1`, wet mass `10` kg/m, overpull `0` kip; expected length `30.48` m and top tension `2.98906692+0.0` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-1-30.48-2.98906692-0-0.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V7 | `100` ft × `1`, wet mass `10` kg/m, overpull `1` kip; expected length `30.48` m and top tension `2.98906692+4.448222` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-1-30.48-2.98906692-1-4.448222]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V8 | `100` ft × `1`, wet mass `10` kg/m, overpull `25` kip; expected length `30.48` m and top tension `2.98906692+111.20555` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-1-30.48-2.98906692-25-111.20555]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V9 | `100` ft × `2`, wet mass `10` kg/m, overpull `0` kip; expected length `60.96` m and top tension `5.97813384+0.0` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-2-60.96-5.97813384-0-0.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V10 | `100` ft × `2`, wet mass `10` kg/m, overpull `1` kip; expected length `60.96` m and top tension `5.97813384+4.448222` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-2-60.96-5.97813384-1-4.448222]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V11 | `100` ft × `2`, wet mass `10` kg/m, overpull `25` kip; expected length `60.96` m and top tension `5.97813384+111.20555` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[100-2-60.96-5.97813384-25-111.20555]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V12 | `40` ft × `3`, wet mass `10` kg/m, overpull `0` kip; expected length `36.576` m and top tension `3.586880304+0.0` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[40-3-36.576-3.586880304-0-0.0]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V13 | `40` ft × `3`, wet mass `10` kg/m, overpull `1` kip; expected length `36.576` m and top tension `3.586880304+4.448222` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[40-3-36.576-3.586880304-1-4.448222]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| V14 | `40` ft × `3`, wet mass `10` kg/m, overpull `25` kip; expected length `36.576` m and top tension `3.586880304+111.20555` kN | Length `1e-12` m absolute; force relative `5e-4` plus `1e-12` kN, bounded legacy conversions; [tests/subsea/vertical_riser/test_suspended_tension.py:L32-L52] | `tests/subsea/vertical_riser/test_suspended_tension.py::test_uniform_suspended_pipe_top_effective_tension[40-3-36.576-3.586880304-25-111.20555]` | PASS, BASE and AFTER; [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |

| Regression case | Expected behavior | Tolerance / existing test node ID | Offline result |
| --- | --- | --- | --- |
| R1 | Tripled input modulus triples output and preserves dimensional reference. [tests/subsea/vertical_riser/test_component_units.py:L80-L94] | Relative `2e-7`; `tests/subsea/vertical_riser/test_component_units.py::test_elastic_modulus_converts_linearly` | PASS, BASE and AFTER |
| R2 | Characterize all neighboring conversions, including the incorrect doubled strength conversion; this is not a validated strength result. [tests/subsea/vertical_riser/test_component_units.py:L97-L121] | Relative `2e-6` for legacy factors and `4e-6` for squared strength factor; `tests/subsea/vertical_riser/test_component_units.py::test_other_component_properties_keep_existing_behavior` | PASS, BASE and AFTER |

Run record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/vertical_riser -q -rA -p no:cacheprovider --basetemp=<scratch>` on code revision `444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55`; BASE and AFTER each collected 16, passed 16, failed 0, skipped 0, NOT_RUN 0.
This is 14 validated arithmetic cases and 2 regression cases; spreadsheet integration, solver loading, statics, dynamics and VIV response are not validated by these tests. [tests/subsea/vertical_riser/test_component_units.py:L31-L48] [tests/subsea/vertical_riser/test_suspended_tension.py:L71-L115]

## 5. QA gates

The family requires G0, G1, G2 and G3; G4 is conditional on new diffraction-derived vessel data, and G5 applies to reports. AQG r0, sections 3–10.
Checks below are acceptance procedures; a passing arithmetic subcheck does not clear the entire gate. AQG r0, sections 4–9.

| Gate / check | Module-specific procedure and numeric pass criterion | Evidence and present result |
| --- | --- | --- |
| G0 / C01 | Run section-4 unit tests on `update_property_dictionary_to_si_units`: modulus relative error ≤ `2e-7` dimensionless; linearity relative error ≤ `2e-7`; dictionary identity mismatch count `0`. | V1–V5, R1: PASS. [tests/subsea/vertical_riser/test_component_units.py:L65-L94] |
| G0 / C02 | Run the neighboring conversion characterization: area/inertia and legacy-factor relative errors ≤ `2e-6` dimensionless; squared-strength characterization ≤ `4e-6`. | R2: PASS as regression only; doubled `SMUS` prevents a physical strength approval. [tests/subsea/vertical_riser/test_component_units.py:L97-L121] |
| G0 / C03 | Before `prepare_riser_model`, require exactly `1` matching row for each required marker; missing keys, duplicate labels, unknown units, negative lengths and unmatched auxiliary components each `0` records; reconcile auxiliary area to the sum of matched areas with absolute error ≤ `1e-12` m². | NOT_RUN: no public synthetic workbook integration fixture; no automated detection check exists; issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L28-L48] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L336-L356] |
| G1 / C11 | Load every output of `prepare_fea_model` / `build_fea_model` in the exact target OrcaFlex version; errors `0`, warnings `0`, object-count difference `0` objects. Use matching demo version if available; otherwise the licensed `LoadData` route. | NOT_RUN: licensed interface and solver loading excluded; target version unspecified; issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L370] AQG r0, section 5. |
| G2 / C21 | Run `evaluate_stack_up_properties` through V6–V14: length absolute error ≤ `1e-12` m; top and bottom force relative error ≤ `5e-4` plus absolute `1e-12` kN; force-balance residual ≤ `1e-12` kN. | PASS, 9 cases; limited to supplied uniform wet mass and equal fluid densities. [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| G2 / C22 | Compare each generated line's static top tension with the independently assembled weight balance within `5.0 %`, angle within `1.0°`; sweep ≥ `6` offset steps with `0` convergence failures or unplanned compression and check `0` missing component rows. | NOT_RUN: licensed statics and complete design basis unavailable; issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L331-L334] AQG r0, section 6. |
| G3 / C31 | For the OrcaFlex model assembled by `prepare_fea_model`, run `1.0` s at production settings, require `0` warnings/errors and nonfinite results, and tension departure ≤ `25.0 %` from statics. | NOT_RUN: licensed dynamics excluded; issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L370] AQG r0, section 7. |
| G4 / C41 | Conditional check of the vessel data consumed by the builder: new diffraction inputs require a cited G4 record, weight/buoyancy error ≤ `1.0 %` and mesh/hydrostatic volume error ≤ `0.5 %`. | NOT_APPLICABLE to these arithmetic fixtures: no diffraction dataset is introduced; a future model must establish that condition before reuse. [tests/subsea/vertical_riser/test_suspended_tension.py:L71-L115] AQG r0, sections 8 and 10. |
| G5 / C51 | For a report using `save_data` outputs, require missing result fingerprints, missing applicable gate records and unchecked result rows each `0` records. | NOT_RUN: no engineering report or model QA record produced; issue pending (4). [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L380-L407] AQG r0, section 9. |
| G5 / C52 | Resolve every pack citation at its declared revision, require broken paths or out-of-range spans `0` citations; resolve every example node with missing IDs `0` tests. | Pack self-check record; acceptance evidence only, not a solver QA signature. |

## 6. Known failure modes and pitfalls

sweep: 35 found

The count covers 35 reviewed assumption, fallback, filtering or output sites, including ordinary initializers explicitly distinguished from defects below; no swallowed `except`, explicit `continue`, error `return None`, error empty-container return, external-text `int()`/`float()` conversion, epoch default or empty-result cache write was found in the package's Python files. [src/digitalmodel/subsea/vertical_riser/vertical_riser.py:L12-L25] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L20-L413] [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L1-L257] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure.py:L1-L54] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure_R1_D1.py:L1-L56]

Exact repeatable sweep commands:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(?:\.0)?\b|1970|\b0\.05\b|\b5\s*/\s*100|\b(?:int|float)\(|Q_\(|0\.4536|0\.0254|9\.81|6\.894745|open\(|to_csv|cache' src/digitalmodel/subsea/vertical_riser -g '*.py'
rg -n 'if|elif|else|pass|drop|zeros|zip|sort_values|save|write|min\(' src/digitalmodel/subsea/vertical_riser -g '*.py'
```

| Site | Symptom / cause at code pin | Detection / disposition |
| --- | --- | --- |
| S01: Loading index | The first loading is selected without a configurable index. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L89-L89] | no detection check exists; issue pending (4) |
| S02: Analysis mode | Fatigue is fixed, selecting cyclic soil behavior. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L88-L98] | no detection check exists; issue pending (4) |
| S03: Type and spring selection | First true riser flag wins; all false flags leave the type unset; support is enabled only for a count equal to one. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L59-L73] | no detection check exists; issue pending (4) |
| S04: Joint filtering | Two leading rows are dropped and only matching identifiers retained, without a discarded-row record. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L111-L118] | no detection check exists; issue pending (4) |
| S05: Corrosion default | Zero allowance is inserted rather than read from the joint row. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L138] | no detection check exists; issue pending (4) |
| S06: Measured dry quantity | Implicit mass convention and rounded conversion; no input unit tag. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L168-L175] | no detection check exists; issue pending (2) |
| S07: Measured wet quantity | The same implicit convention is applied independently to wet input. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L176-L181] | no detection check exists; issue pending (2) |
| S08: Calculated mass | Mass-per-length uses a fixed imperial conversion. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L194-L200] | no detection check exists; issue pending (2) |
| S09: Calculated wet quantity | A gravity-scaled displaced-fluid term is subtracted from mass-per-length; downstream code multiplies by gravity again. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L201-L209] [src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py:L193-L209] | no detection check exists; issue pending (2) |
| S10: Unknown weight method | Logs an unsupported method and then uses branch-local properties; the branch has no explicit failure return. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L218-L227] | no detection check exists; issue pending (4) |
| S11: Drag diameter | NaN silently selects buoyancy diameter. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L224-L227] | no detection check exists; issue pending (4) |
| S12: Auxiliary presence | Any positive auxiliary flag changes the current joint internal diameter without matching the current stack component. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L254-L258] | no detection check exists; issue pending (3) |
| S13: Area and inertia units | Input powers of inch are implicit in the in-place API. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L271-L282] | C02: regression PASS |
| S14: Stiffness units | Legacy pressure and length factors are fixed. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L283-L285] | C02: regression PASS |
| S15: Modulus units | psi is assumed; explicit conversion returns kPa. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L286-L286] | C01: PASS |
| S16: Yield units | A fixed pressure factor assumes psi input. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L287-L287] | C02: regression PASS |
| S17: Ultimate strength first write | Input dictionary is aliased before the strength mutation. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L271-L288] | C02: characterizes behavior; issue pending (1) |
| S18: Ultimate strength second write | Aliasing makes the second conversion multiply the already converted strength again. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L288-L289] | C02: characterizes defect; issue pending (1) |
| S19: Auxiliary area default | Area starts at zero, so no pre-existing sized properties gives zero area. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L336-L340] | no detection check exists; issue pending (3) |
| S20: Auxiliary accumulator | Accumulator starts at zero before matching; preparation calls this before sizing joints. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L342-L344] | no detection check exists; issue pending (3) |
| S21: Auxiliary matched index | Every match adds area from index zero rather than the matched index. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L345-L356] | no detection check exists; issue pending (3) |
| S22: Flexible-joint filter | An empty filtered table is retained without a completeness check. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L293-L301] | no detection check exists; issue pending (4) |
| S23: Tensioner filter | An empty filtered table is retained without a completeness check. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L303-L310] | no detection check exists; issue pending (4) |
| S24: Soil filter | An empty filtered and sorted table is retained without a completeness check. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L312-L326] | no detection check exists; issue pending (4) |
| S25: VIV segmentation | All segment lengths become the minimum excluding the first row; an empty selection can produce NaN. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L409-L413] | no detection check exists; issue pending (4) |
| S26: CSV write | Table is exported without checking completeness or emptiness. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L380-L388] | no detection check exists; issue pending (4) |
| S27: Legacy material assumptions | Fixed modulus and Poisson ratio have no source or unit tag beside the assignments. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L12-L13] | no detection check exists; issue pending (5) |
| S28: Legacy geometric assumptions | Fixed water-depth and floor-elevation values are separate from spreadsheet inputs. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L23-L24] | no detection check exists; issue pending (5) |
| S29: Legacy stack initializer | Zero is an accumulator origin, not an error fallback. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L62-L62] | reviewed initializer; no detection check exists; issue pending (5) |
| S30: Legacy zero arrays | Zero-filled length and elevation arrays may retain defaults for unmatched records. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L63-L64] | no detection check exists; issue pending (5) |
| S31: Legacy zip truncation | Unequal input lists stop at the shortest length without reporting omitted records. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L67-L68] | no detection check exists; issue pending (5) |
| S32: Legacy marker | Only a conductor match assigns the mudline reference; missing marker leaves it undefined. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L164-L171] | no detection check exists; issue pending (5) |
| S33: Legacy pictures | Both figure scripts and embedded legacy drawing use fixed coordinates, RGB values and GUI launch; zero colour channels and drawing flags are display constants, not engineering defaults. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L205-L257] | no detection check exists; issue pending (5) |
| S34: Plot configuration | An empty plot-settings sequence produces no plots and no missing-image record. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L390-L407] | no detection check exists; issue pending (4) |
| S35: Report table stub | The report-table method has no implementation. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L328-L329] | no detection check exists; issue pending (4) |

The auxiliary initialization order is explicit in `prepare_riser_model`; an empty `pipe_properties` list exists before the area calculation, while joint sizing follows it. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L20-L37]
The legacy image row also covers both standalone renderers. [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure.py:L12-L54] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure_R1_D1.py:L12-L56]

| Issue reference | Confirmed relevance and detection |
| --- | --- |
| #620 | Historical missing engine import: current code imports and dispatches the entry point; source guards exist, so the historical symptom is not asserted as current. [src/digitalmodel/engine.py:L48-L48] [src/digitalmodel/engine.py:L238-L239] [tests/security/test_review_20260523_remaining.py:L15-L28] |
| #756 | Historical deleted-namespace import: current component imports use the relocated sizing and builder modules; the dedicated suspended test imports the actual component class. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L8-L17] [tests/subsea/vertical_riser/test_suspended_tension.py:L26-L26] |
| #2676 | Historical value-check gap has a dedicated force-balance test at this pin; C21 detects changes to that bounded calculation. [tests/subsea/vertical_riser/test_suspended_tension.py:L42-L69] [tests/subsea/vertical_riser/test_suspended_tension.py:L117-L144] |
| #3320 | Broader worked-example and report work remains outside this pack; no detection check exists for complete workflow correctness. [docs/plans/vertical-riser-modulus.md:L22-L25] |
| Routing-test skip | An old test is still explicitly skipped on the historical missing-import premise despite the restored import; no detection check exists for stale skip metadata; tracked with #620. [tests/test_engine.py:L192-L210] [src/digitalmodel/engine.py:L48-L48] |

## 7. Lessons log

| Date | Entry | Lesson and pack change | Evidence | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-11-09 | Initial pack | Keep the strength characterization separate from validation; document the aliasing defect and unit boundary. | [tests/subsea/vertical_riser/test_component_units.py:L111-L121] | Not assigned; issue pending (4) |

## 8. Ownership

Review roles for this draft: OrcaFlex riser/mooring quality manager, with a riser/mooring deputy quality manager; engineering contributors maintain the source and evidence, and the maintainer routes unresolved questions.
These are review assignments for the pack, not recorded clearance or approval.

## 9. Dependencies and implementation inventory

| Dependency / source occurrence | Role and evidence |
| --- | --- |
| Shared unit registry, spreadsheet reader and sizing class | Unit conversion, table ingestion and sizing delegation. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L6-L8] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L41-L48] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L141-L147] |
| Shared stack-up calculator | Geometry, weight and tension calculation outside this package; module delegates rather than duplicating it. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L9-L11] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L331-L334] |
| OrcaFlex import | `OrcaflexModelComponents` is a text builder dependency, read as source; no licensed interface was loaded. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L12-L15] |
| OrcaFlex preparation calls | `prepare_fea_model` instantiates the builder and calls `orcaflex_model`; `build_fea_model` delegates saving. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L370] |
| SHEAR7 import | `Shear7ModelComponents` is a text builder dependency. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L16-L16] |
| SHEAR7 workflow occurrences | Optional VIV branch calls the property alteration, second FEA build with `shear7_flag=True`, and SHEAR7 preparation. [src/digitalmodel/subsea/vertical_riser/vertical_riser.py:L20-L24] |
| SHEAR7 preparation occurrences | `build_fea_model` accepts and forwards its flag; `prepare_shear7_model` prints its status, constructs the builder, prepares and saves it. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L369-L378] |
| SHEAR7 alteration occurrence | Segment lengths are replaced by the non-first-row minimum. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L409-L413] |
| Legacy OrcaFlex mention | The legacy generator's opening docstring names OrcaFlex; its executable route writes a picture script rather than establishing solver acceptance. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L1-L10] [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L189-L200] |
| Builder versions | SHEAR7 builder emits a `v4.6c` title; this is text provenance, not a tested solver compatibility claim. [src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py:L46-L52] |
| Visualization and legacy dependencies | Plotting delegates to `Visualization`; legacy modules depend on spreadsheet and GUI libraries and have import-time side effects. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L404-L407] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure.py:L45-L54] [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L6-L10] |

### Map basis and counted families

Map file: `docs/registry/implementation-map.yaml` from `codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`.
Citations containing `@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` resolve at that map commit; all other repository citations resolve at the code revision in the header.
The following inventory lists every implementation row of both module families; neutral inventory IDs identify exact map records without copying identifying discovery paths.
Each map-row citation includes the record's path, symbol and source-line fields, with YAML aliases resolved using the complete map.

| Rank / family | In-package rows | All implementation rows | Counted |
| --- | --- | --- | --- |
| 1 / `effective-tension-riser-stack-up` | 9 | 264 | Yes; [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2139-L4932] |
| 2 / `orcaflex-text-model-writers` | 1 | 440 | Yes; [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4933-L9508] |

The in-package records comprise legacy `StackUpDiagram`; `VerticalRiser.prepare_riser_model`, `read_riser_input_data`, `assign_key_properties_to_self`, `get_fea_properties`, `prepare_tensioner_data`, `evaluate_stack_up_properties`, `save_data`, `alter_properties_for_Shear7_software`; the text-writer family also records `get_fea_properties`. [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L27-L187] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L28-L105] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L261] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L303-L310] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L331-L334] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L380-L388] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L409-L413]

### Exact search signals and disposition

Reproduction procedure: load the map with a YAML parser; rank families by in-package row count preserving map order for ties; compile each `search_signals[].pattern` unchanged with Python `re`; exclude only signals matching `subsea/vertical_riser` with `re.IGNORECASE`; search all five package `.py` files at the header pin case-sensitively; assign each hit to the innermost enclosing function/class, or one module-level definition.
No signal matched the package path, so none was excluded.

Signals for `effective-tension-riser-stack-up`:

```text
algorithm: effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
vocabulary: effective.tension|tensioner|stack.up
constants_or_keys: internal_pressure|external_pressure|fluid_density|buoyancy_module
```

Signals for `orcaflex-text-model-writers`:

```text
algorithm: (?:yaml\.(?:dump|safe_dump)|write_text|open\(|\.write\(|save\(|SaveData|dump\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\.write\(|\.ya?ml|yaml\.safe_dump|shutil\.copy|\.write\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\.safe_dump|\.write_text
vocabulary: orcaflex|OrcaFlex|includefile|modular_generator
constants_or_keys: [\"\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\"\']|%YAML
```

| Family | Hit definitions and classification | Further second implementations |
| --- | --- | --- |
| Effective tension | 13 definitions: four module-level hits are imports/docstrings/data; seven methods already have map rows (`prepare_riser_model`, reader, assignment, property calculation, tensioner selection, stack-up delegation, CSV writer), as does segment alteration; `prepare_plots` only exports the listed table. | 0 |
| Text writers | 5 definitions: two module-level hits are imports or legacy picture output; `get_fea_properties` is listed; conversion hit is a docstring; `prepare_fea_model` delegates to the external listed builder. | 0 |

A8 search result: 0 further second implementations summed over the two counted families; the table inventory contains 704 map records.
Map follow-up: unit conversion, auxiliary-area accumulation and SHEAR7 segmentation/export require clearer family classification; map source locations also precede the header code revision. These are map follow-up findings, issue pending (6), rather than extra counted families.
Repository-wide discovery included tracked files under `scripts/` using the commands below; external implementations are inventory/map follow-up, not in-package misses.

```bash
git grep -n -I -E 'effective_tension|effectiveTension|top_tension|tensioner|stack_up|orcaRiserStackup|Shear7ModelComponents|update_property_dictionary_to_si_units' -- '*.py'
git grep -n -I -E 'vertical_riser|API.?5L' -- docs src/digitalmodel/subsea/vertical_riser
```

<details>
<summary>Complete map-row inventory</summary>

| Inventory ID | Exact implementation-map record | In package |
| --- | --- | --- |
| ET-001 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2146-L2155] | No |
| ET-002 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2156-L2165] | No |
| ET-003 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2166-L2175] | No |
| ET-004 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2176-L2185] | No |
| ET-005 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2186-L2195] | No |
| ET-006 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2196-L2205] | No |
| ET-007 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2206-L2215] | No |
| ET-008 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2216-L2225] | No |
| ET-009 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2226-L2235] | No |
| ET-010 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2236-L2247] | No |
| ET-011 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2248-L2259] | No |
| ET-012 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2260-L2269] | No |
| ET-013 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2270-L2282] | No |
| ET-014 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2283-L2291] | No |
| ET-015 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2292-L2301] | No |
| ET-016 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2302-L2311] | No |
| ET-017 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2312-L2324] | No |
| ET-018 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2325-L2334] | No |
| ET-019 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2335-L2344] | No |
| ET-020 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2345-L2356] | No |
| ET-021 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2357-L2365] | No |
| ET-022 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2366-L2375] | No |
| ET-023 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2376-L2387] | No |
| ET-024 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2388-L2397] | No |
| ET-025 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2398-L2407] | No |
| ET-026 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2408-L2417] | No |
| ET-027 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2418-L2427] | No |
| ET-028 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2428-L2437] | No |
| ET-029 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2438-L2447] | No |
| ET-030 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2448-L2457] | No |
| ET-031 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2458-L2470] | No |
| ET-032 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2471-L2484] | No |
| ET-033 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2485-L2494] | No |
| ET-034 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2495-L2504] | No |
| ET-035 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2505-L2514] | No |
| ET-036 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2515-L2524] | No |
| ET-037 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2525-L2534] | No |
| ET-038 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2535-L2544] | No |
| ET-039 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2545-L2556] | No |
| ET-040 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2557-L2566] | No |
| ET-041 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2567-L2576] | No |
| ET-042 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2577-L2586] | No |
| ET-043 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2587-L2595] | No |
| ET-044 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2596-L2605] | No |
| ET-045 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2606-L2615] | No |
| ET-046 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2616-L2625] | No |
| ET-047 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2626-L2635] | No |
| ET-048 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2636-L2645] | No |
| ET-049 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2646-L2655] | No |
| ET-050 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2656-L2667] | No |
| ET-051 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2668-L2679] | No |
| ET-052 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2680-L2689] | No |
| ET-053 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2690-L2701] | No |
| ET-054 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2702-L2715] | No |
| ET-055 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2716-L2726] | No |
| ET-056 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2727-L2740] | No |
| ET-057 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2741-L2750] | No |
| ET-058 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2751-L2760] | No |
| ET-059 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2761-L2774] | No |
| ET-060 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2775-L2787] | No |
| ET-061 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2788-L2797] | No |
| ET-062 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2798-L2807] | No |
| ET-063 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2808-L2817] | No |
| ET-064 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2818-L2827] | No |
| ET-065 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2828-L2839] | No |
| ET-066 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2840-L2852] | No |
| ET-067 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2853-L2862] | No |
| ET-068 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2863-L2874] | No |
| ET-069 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2875-L2884] | No |
| ET-070 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2885-L2896] | No |
| ET-071 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2897-L2907] | No |
| ET-072 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2908-L2917] | No |
| ET-073 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2918-L2927] | No |
| ET-074 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2928-L2938] | No |
| ET-075 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2939-L2948] | No |
| ET-076 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2949-L2960] | No |
| ET-077 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2961-L2970] | No |
| ET-078 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2971-L2980] | No |
| ET-079 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2981-L2990] | No |
| ET-080 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L2991-L3000] | No |
| ET-081 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3001-L3010] | No |
| ET-082 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3011-L3022] | No |
| ET-083 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3023-L3032] | No |
| ET-084 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3033-L3042] | No |
| ET-085 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3043-L3052] | No |
| ET-086 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3053-L3062] | No |
| ET-087 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3063-L3072] | No |
| ET-088 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3073-L3082] | No |
| ET-089 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3083-L3092] | No |
| ET-090 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3093-L3102] | No |
| ET-091 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3103-L3112] | No |
| ET-092 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3113-L3122] | No |
| ET-093 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3123-L3135] | No |
| ET-094 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3136-L3145] | No |
| ET-095 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3146-L3155] | No |
| ET-096 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3156-L3168] | No |
| ET-097 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3169-L3178] | No |
| ET-098 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3179-L3188] | No |
| ET-099 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3189-L3198] | No |
| ET-100 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3199-L3208] | No |
| ET-101 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3209-L3218] | No |
| ET-102 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3219-L3231] | No |
| ET-103 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3232-L3241] | No |
| ET-104 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3242-L3251] | No |
| ET-105 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3252-L3264] | No |
| ET-106 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3265-L3274] | No |
| ET-107 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3275-L3287] | No |
| ET-108 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3288-L3300] | No |
| ET-109 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3301-L3311] | No |
| ET-110 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3312-L3321] | No |
| ET-111 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3322-L3331] | No |
| ET-112 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3332-L3341] | No |
| ET-113 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3342-L3351] | No |
| ET-114 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3352-L3361] | No |
| ET-115 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3362-L3377] | No |
| ET-116 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3378-L3387] | No |
| ET-117 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3388-L3397] | No |
| ET-118 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3398-L3407] | No |
| ET-119 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3408-L3417] | No |
| ET-120 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3418-L3432] | No |
| ET-121 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3433-L3442] | No |
| ET-122 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3443-L3452] | No |
| ET-123 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3453-L3462] | No |
| ET-124 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3463-L3472] | No |
| ET-125 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3473-L3482] | No |
| ET-126 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3483-L3492] | No |
| ET-127 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3493-L3502] | No |
| ET-128 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3503-L3512] | No |
| ET-129 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3513-L3522] | No |
| ET-130 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3523-L3532] | No |
| ET-131 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3533-L3542] | No |
| ET-132 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3543-L3555] | No |
| ET-133 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3556-L3565] | No |
| ET-134 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3566-L3575] | No |
| ET-135 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3576-L3585] | No |
| ET-136 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3586-L3597] | No |
| ET-137 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3598-L3607] | No |
| ET-138 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3608-L3617] | No |
| ET-139 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3618-L3627] | No |
| ET-140 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3628-L3637] | No |
| ET-141 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3638-L3647] | No |
| ET-142 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3648-L3657] | No |
| ET-143 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3658-L3667] | No |
| ET-144 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3668-L3677] | No |
| ET-145 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3678-L3687] | No |
| ET-146 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3688-L3697] | No |
| ET-147 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3698-L3707] | No |
| ET-148 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3708-L3717] | No |
| ET-149 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3718-L3727] | No |
| ET-150 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3728-L3737] | No |
| ET-151 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3738-L3747] | No |
| ET-152 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3748-L3757] | No |
| ET-153 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3758-L3767] | No |
| ET-154 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3768-L3777] | No |
| ET-155 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3778-L3787] | No |
| ET-156 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3788-L3797] | No |
| ET-157 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3798-L3807] | No |
| ET-158 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3808-L3819] | No |
| ET-159 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3820-L3829] | No |
| ET-160 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3830-L3839] | No |
| ET-161 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3840-L3849] | No |
| ET-162 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3850-L3862] | No |
| ET-163 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3863-L3872] | No |
| ET-164 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3873-L3885] | No |
| ET-165 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3886-L3898] | No |
| ET-166 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3899-L3909] | No |
| ET-167 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3910-L3919] | No |
| ET-168 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3920-L3929] | No |
| ET-169 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3930-L3939] | No |
| ET-170 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3940-L3949] | No |
| ET-171 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3950-L3959] | No |
| ET-172 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3960-L3969] | No |
| ET-173 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3970-L3979] | No |
| ET-174 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3980-L3989] | No |
| ET-175 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L3990-L3999] | No |
| ET-176 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4000-L4009] | No |
| ET-177 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4010-L4019] | No |
| ET-178 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4020-L4029] | No |
| ET-179 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4030-L4039] | No |
| ET-180 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4040-L4049] | No |
| ET-181 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4050-L4059] | No |
| ET-182 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4060-L4069] | No |
| ET-183 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4070-L4079] | No |
| ET-184 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4080-L4089] | No |
| ET-185 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4090-L4099] | No |
| ET-186 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4100-L4109] | No |
| ET-187 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4110-L4119] | No |
| ET-188 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4120-L4129] | No |
| ET-189 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4130-L4139] | No |
| ET-190 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4140-L4149] | No |
| ET-191 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4150-L4159] | No |
| ET-192 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4160-L4169] | No |
| ET-193 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4170-L4179] | No |
| ET-194 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4180-L4189] | No |
| ET-195 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4190-L4199] | No |
| ET-196 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4200-L4209] | No |
| ET-197 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4210-L4222] | No |
| ET-198 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4223-L4232] | No |
| ET-199 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4233-L4242] | No |
| ET-200 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4243-L4252] | No |
| ET-201 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4253-L4262] | No |
| ET-202 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4263-L4272] | No |
| ET-203 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4273-L4285] | No |
| ET-204 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4286-L4295] | No |
| ET-205 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4296-L4307] | No |
| ET-206 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4308-L4317] | No |
| ET-207 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4318-L4327] | No |
| ET-208 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4328-L4337] | No |
| ET-209 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4338-L4347] | Yes |
| ET-210 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4348-L4362] | Yes |
| ET-211 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4363-L4372] | Yes |
| ET-212 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4373-L4382] | Yes |
| ET-213 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4383-L4392] | Yes |
| ET-214 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4393-L4402] | Yes |
| ET-215 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4403-L4412] | Yes |
| ET-216 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4413-L4422] | Yes |
| ET-217 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4423-L4432] | Yes |
| ET-218 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4433-L4444] | No |
| ET-219 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4445-L4454] | No |
| ET-220 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4455-L4464] | No |
| ET-221 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4465-L4476] | No |
| ET-222 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4477-L4486] | No |
| ET-223 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4487-L4496] | No |
| ET-224 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4497-L4506] | No |
| ET-225 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4507-L4516] | No |
| ET-226 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4517-L4526] | No |
| ET-227 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4527-L4536] | No |
| ET-228 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4537-L4545] | No |
| ET-229 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4546-L4557] | No |
| ET-230 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4558-L4567] | No |
| ET-231 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4568-L4578] | No |
| ET-232 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4579-L4589] | No |
| ET-233 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4590-L4599] | No |
| ET-234 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4600-L4609] | No |
| ET-235 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4610-L4623] | No |
| ET-236 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4624-L4634] | No |
| ET-237 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4635-L4644] | No |
| ET-238 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4645-L4654] | No |
| ET-239 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4655-L4666] | No |
| ET-240 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4667-L4676] | No |
| ET-241 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4677-L4686] | No |
| ET-242 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4687-L4696] | No |
| ET-243 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4697-L4706] | No |
| ET-244 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4707-L4716] | No |
| ET-245 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4717-L4726] | No |
| ET-246 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4727-L4736] | No |
| ET-247 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4737-L4746] | No |
| ET-248 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4747-L4756] | No |
| ET-249 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4757-L4766] | No |
| ET-250 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4767-L4776] | No |
| ET-251 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4777-L4786] | No |
| ET-252 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4787-L4796] | No |
| ET-253 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4797-L4806] | No |
| ET-254 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4807-L4816] | No |
| ET-255 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4817-L4826] | No |
| ET-256 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4827-L4836] | No |
| ET-257 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4837-L4846] | No |
| ET-258 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4847-L4856] | No |
| ET-259 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4857-L4865] | No |
| ET-260 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4866-L4875] | No |
| ET-261 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4876-L4887] | No |
| ET-262 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4888-L4897] | No |
| ET-263 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4898-L4907] | No |
| ET-264 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4908-L4917] | No |
| TW-001 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4940-L4949] | No |
| TW-002 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4950-L4958] | No |
| TW-003 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4959-L4968] | No |
| TW-004 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4969-L4978] | No |
| TW-005 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4979-L4988] | No |
| TW-006 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4989-L4998] | No |
| TW-007 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4999-L5008] | No |
| TW-008 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5009-L5020] | No |
| TW-009 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5021-L5030] | No |
| TW-010 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5031-L5040] | No |
| TW-011 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5041-L5050] | No |
| TW-012 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5051-L5060] | No |
| TW-013 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5061-L5070] | No |
| TW-014 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5071-L5080] | No |
| TW-015 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5081-L5090] | No |
| TW-016 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5091-L5100] | No |
| TW-017 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5101-L5110] | No |
| TW-018 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5111-L5120] | No |
| TW-019 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5121-L5132] | No |
| TW-020 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5133-L5142] | No |
| TW-021 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5143-L5151] | No |
| TW-022 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5152-L5161] | No |
| TW-023 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5162-L5171] | No |
| TW-024 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5172-L5181] | No |
| TW-025 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5182-L5191] | No |
| TW-026 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5192-L5201] | No |
| TW-027 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5202-L5211] | No |
| TW-028 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5212-L5223] | No |
| TW-029 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5224-L5233] | No |
| TW-030 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5234-L5243] | No |
| TW-031 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5244-L5253] | No |
| TW-032 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5254-L5263] | No |
| TW-033 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5264-L5273] | No |
| TW-034 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5274-L5283] | No |
| TW-035 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5284-L5293] | No |
| TW-036 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5294-L5303] | No |
| TW-037 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5304-L5313] | No |
| TW-038 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5314-L5323] | No |
| TW-039 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5324-L5333] | No |
| TW-040 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5334-L5343] | No |
| TW-041 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5344-L5353] | No |
| TW-042 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5354-L5363] | No |
| TW-043 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5364-L5372] | No |
| TW-044 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5373-L5382] | No |
| TW-045 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5383-L5392] | No |
| TW-046 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5393-L5402] | No |
| TW-047 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5403-L5415] | No |
| TW-048 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5416-L5425] | No |
| TW-049 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5426-L5437] | No |
| TW-050 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5438-L5447] | No |
| TW-051 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5448-L5457] | No |
| TW-052 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5458-L5467] | No |
| TW-053 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5468-L5477] | No |
| TW-054 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5478-L5487] | No |
| TW-055 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5488-L5497] | No |
| TW-056 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5498-L5507] | No |
| TW-057 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5508-L5519] | No |
| TW-058 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5520-L5531] | No |
| TW-059 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5532-L5540] | No |
| TW-060 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5541-L5550] | No |
| TW-061 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5551-L5560] | No |
| TW-062 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5561-L5570] | No |
| TW-063 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5571-L5580] | No |
| TW-064 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5581-L5593] | No |
| TW-065 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5594-L5603] | No |
| TW-066 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5604-L5615] | No |
| TW-067 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5616-L5628] | No |
| TW-068 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5629-L5638] | No |
| TW-069 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5639-L5648] | No |
| TW-070 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5649-L5658] | No |
| TW-071 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5659-L5667] | No |
| TW-072 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5668-L5677] | No |
| TW-073 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5678-L5687] | No |
| TW-074 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5688-L5697] | No |
| TW-075 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5698-L5707] | No |
| TW-076 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5708-L5717] | No |
| TW-077 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5718-L5727] | No |
| TW-078 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5728-L5737] | No |
| TW-079 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5738-L5749] | No |
| TW-080 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5750-L5759] | No |
| TW-081 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5760-L5769] | No |
| TW-082 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5770-L5781] | No |
| TW-083 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5782-L5791] | No |
| TW-084 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5792-L5801] | No |
| TW-085 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5802-L5811] | No |
| TW-086 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5812-L5823] | No |
| TW-087 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5824-L5833] | No |
| TW-088 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5834-L5843] | No |
| TW-089 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5844-L5853] | No |
| TW-090 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5854-L5863] | No |
| TW-091 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5864-L5872] | No |
| TW-092 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5873-L5882] | No |
| TW-093 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5883-L5891] | No |
| TW-094 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5892-L5901] | No |
| TW-095 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5902-L5911] | No |
| TW-096 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5912-L5921] | No |
| TW-097 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5922-L5933] | No |
| TW-098 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5934-L5943] | No |
| TW-099 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5944-L5953] | No |
| TW-100 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5954-L5965] | No |
| TW-101 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5966-L5975] | No |
| TW-102 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5976-L5985] | No |
| TW-103 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5986-L5994] | No |
| TW-104 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5995-L6004] | No |
| TW-105 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6005-L6014] | No |
| TW-106 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6015-L6024] | No |
| TW-107 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6025-L6034] | No |
| TW-108 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6035-L6044] | No |
| TW-109 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6045-L6054] | No |
| TW-110 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6055-L6064] | No |
| TW-111 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6065-L6074] | No |
| TW-112 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6075-L6084] | No |
| TW-113 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6085-L6096] | No |
| TW-114 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6097-L6109] | No |
| TW-115 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6110-L6119] | No |
| TW-116 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6120-L6129] | No |
| TW-117 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6130-L6141] | No |
| TW-118 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6142-L6153] | No |
| TW-119 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6154-L6163] | No |
| TW-120 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6164-L6173] | No |
| TW-121 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6174-L6183] | No |
| TW-122 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6184-L6193] | No |
| TW-123 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6194-L6203] | No |
| TW-124 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6204-L6213] | No |
| TW-125 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6214-L6223] | No |
| TW-126 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6224-L6233] | No |
| TW-127 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6234-L6243] | No |
| TW-128 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6244-L6253] | No |
| TW-129 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6254-L6263] | No |
| TW-130 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6264-L6273] | No |
| TW-131 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6274-L6283] | No |
| TW-132 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6284-L6293] | No |
| TW-133 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303] | No |
| TW-134 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313] | No |
| TW-135 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6314-L6323] | No |
| TW-136 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6324-L6333] | No |
| TW-137 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6334-L6343] | No |
| TW-138 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6344-L6353] | No |
| TW-139 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6354-L6363] | No |
| TW-140 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6364-L6373] | No |
| TW-141 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6374-L6383] | No |
| TW-142 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6384-L6393] | No |
| TW-143 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6394-L6403] | No |
| TW-144 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6404-L6413] | No |
| TW-145 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6414-L6423] | No |
| TW-146 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6424-L6436] | No |
| TW-147 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6437-L6448] | No |
| TW-148 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6449-L6458] | No |
| TW-149 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6459-L6468] | No |
| TW-150 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6469-L6478] | No |
| TW-151 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6479-L6490] | No |
| TW-152 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6491-L6500] | No |
| TW-153 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6501-L6510] | No |
| TW-154 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6511-L6520] | No |
| TW-155 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6521-L6530] | No |
| TW-156 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6531-L6540] | No |
| TW-157 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6541-L6550] | No |
| TW-158 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6551-L6560] | No |
| TW-159 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6561-L6570] | No |
| TW-160 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6571-L6580] | No |
| TW-161 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6581-L6590] | No |
| TW-162 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6591-L6600] | No |
| TW-163 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6601-L6610] | No |
| TW-164 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6611-L6620] | No |
| TW-165 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6621-L6630] | No |
| TW-166 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6631-L6640] | No |
| TW-167 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6641-L6650] | No |
| TW-168 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6651-L6659] | No |
| TW-169 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6660-L6672] | No |
| TW-170 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6673-L6682] | No |
| TW-171 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6683-L6692] | No |
| TW-172 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6693-L6704] | No |
| TW-173 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6705-L6714] | No |
| TW-174 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6715-L6724] | No |
| TW-175 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6725-L6734] | No |
| TW-176 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6735-L6744] | No |
| TW-177 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6745-L6754] | No |
| TW-178 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6755-L6764] | No |
| TW-179 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6765-L6774] | No |
| TW-180 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6775-L6784] | No |
| TW-181 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6785-L6794] | No |
| TW-182 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6795-L6804] | No |
| TW-183 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6805-L6814] | No |
| TW-184 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6815-L6824] | No |
| TW-185 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6825-L6834] | No |
| TW-186 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6835-L6844] | No |
| TW-187 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6845-L6854] | No |
| TW-188 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6855-L6864] | No |
| TW-189 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6865-L6874] | No |
| TW-190 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6875-L6884] | No |
| TW-191 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6885-L6894] | No |
| TW-192 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6895-L6904] | No |
| TW-193 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6905-L6914] | No |
| TW-194 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6915-L6927] | No |
| TW-195 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6928-L6937] | No |
| TW-196 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6938-L6947] | No |
| TW-197 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6948-L6957] | No |
| TW-198 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6958-L6967] | No |
| TW-199 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6968-L6977] | No |
| TW-200 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6978-L6987] | No |
| TW-201 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6988-L6997] | No |
| TW-202 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6998-L7007] | No |
| TW-203 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7008-L7017] | No |
| TW-204 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7018-L7031] | No |
| TW-205 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7032-L7041] | No |
| TW-206 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7042-L7051] | No |
| TW-207 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7052-L7061] | No |
| TW-208 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7062-L7071] | No |
| TW-209 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7072-L7081] | No |
| TW-210 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7082-L7091] | No |
| TW-211 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7092-L7101] | No |
| TW-212 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7102-L7111] | No |
| TW-213 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7112-L7121] | No |
| TW-214 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7122-L7131] | No |
| TW-215 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7132-L7141] | No |
| TW-216 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7142-L7153] | No |
| TW-217 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7154-L7163] | No |
| TW-218 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7164-L7176] | No |
| TW-219 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7177-L7186] | No |
| TW-220 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7187-L7196] | No |
| TW-221 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7197-L7207] | No |
| TW-222 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7208-L7217] | No |
| TW-223 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7218-L7227] | No |
| TW-224 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7228-L7237] | No |
| TW-225 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7238-L7247] | No |
| TW-226 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7248-L7257] | No |
| TW-227 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7258-L7269] | No |
| TW-228 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7270-L7279] | No |
| TW-229 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7280-L7289] | No |
| TW-230 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7290-L7299] | No |
| TW-231 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7300-L7309] | No |
| TW-232 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7310-L7319] | No |
| TW-233 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7320-L7329] | No |
| TW-234 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7330-L7339] | No |
| TW-235 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7340-L7349] | No |
| TW-236 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7350-L7359] | No |
| TW-237 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7360-L7369] | No |
| TW-238 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7370-L7379] | No |
| TW-239 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7380-L7389] | No |
| TW-240 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7390-L7399] | No |
| TW-241 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7400-L7409] | No |
| TW-242 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7410-L7419] | No |
| TW-243 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7420-L7429] | No |
| TW-244 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7430-L7439] | No |
| TW-245 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7440-L7449] | No |
| TW-246 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7450-L7459] | No |
| TW-247 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7460-L7469] | No |
| TW-248 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7470-L7479] | No |
| TW-249 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7480-L7489] | No |
| TW-250 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7490-L7499] | No |
| TW-251 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7500-L7509] | No |
| TW-252 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7510-L7519] | No |
| TW-253 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7520-L7529] | No |
| TW-254 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7530-L7539] | No |
| TW-255 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7540-L7551] | No |
| TW-256 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7552-L7561] | No |
| TW-257 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7562-L7571] | No |
| TW-258 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7572-L7581] | No |
| TW-259 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7582-L7591] | No |
| TW-260 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7592-L7603] | No |
| TW-261 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7604-L7613] | No |
| TW-262 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7614-L7626] | No |
| TW-263 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7627-L7636] | No |
| TW-264 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7637-L7646] | No |
| TW-265 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7647-L7656] | No |
| TW-266 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7657-L7666] | No |
| TW-267 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7667-L7676] | No |
| TW-268 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7677-L7686] | No |
| TW-269 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7687-L7696] | No |
| TW-270 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7697-L7706] | No |
| TW-271 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7707-L7716] | No |
| TW-272 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7717-L7729] | No |
| TW-273 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7730-L7739] | No |
| TW-274 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7740-L7749] | No |
| TW-275 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7750-L7762] | No |
| TW-276 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7763-L7774] | No |
| TW-277 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7775-L7784] | No |
| TW-278 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7785-L7794] | No |
| TW-279 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7795-L7804] | No |
| TW-280 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7805-L7816] | No |
| TW-281 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7817-L7826] | No |
| TW-282 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7827-L7836] | No |
| TW-283 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7837-L7846] | No |
| TW-284 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7847-L7856] | No |
| TW-285 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7857-L7866] | No |
| TW-286 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7867-L7878] | No |
| TW-287 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7879-L7888] | No |
| TW-288 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7889-L7898] | No |
| TW-289 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7899-L7908] | No |
| TW-290 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7909-L7921] | No |
| TW-291 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7922-L7931] | No |
| TW-292 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7932-L7941] | No |
| TW-293 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7942-L7951] | No |
| TW-294 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7952-L7961] | No |
| TW-295 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7962-L7971] | No |
| TW-296 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7972-L7981] | No |
| TW-297 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7982-L7991] | No |
| TW-298 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7992-L8001] | No |
| TW-299 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8002-L8011] | No |
| TW-300 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8012-L8021] | No |
| TW-301 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8022-L8031] | No |
| TW-302 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8032-L8043] | No |
| TW-303 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8044-L8053] | No |
| TW-304 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8054-L8063] | No |
| TW-305 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8064-L8073] | No |
| TW-306 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8074-L8083] | No |
| TW-307 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8084-L8093] | No |
| TW-308 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8094-L8103] | No |
| TW-309 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8104-L8113] | No |
| TW-310 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8114-L8123] | No |
| TW-311 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8124-L8133] | No |
| TW-312 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8134-L8143] | No |
| TW-313 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8144-L8155] | No |
| TW-314 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8156-L8165] | No |
| TW-315 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8166-L8175] | No |
| TW-316 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8176-L8185] | No |
| TW-317 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8186-L8195] | No |
| TW-318 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8205] | No |
| TW-319 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8206-L8215] | No |
| TW-320 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8216-L8227] | No |
| TW-321 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8228-L8237] | No |
| TW-322 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8238-L8247] | No |
| TW-323 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8248-L8257] | No |
| TW-324 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8258-L8267] | No |
| TW-325 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8268-L8279] | No |
| TW-326 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8280-L8290] | No |
| TW-327 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8291-L8300] | No |
| TW-328 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8301-L8310] | No |
| TW-329 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8311-L8323] | No |
| TW-330 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8324-L8333] | No |
| TW-331 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8334-L8343] | No |
| TW-332 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8344-L8353] | Yes |
| TW-333 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8354-L8363] | No |
| TW-334 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8364-L8373] | No |
| TW-335 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8374-L8383] | No |
| TW-336 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8384-L8393] | No |
| TW-337 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8394-L8403] | No |
| TW-338 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8404-L8413] | No |
| TW-339 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8414-L8423] | No |
| TW-340 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8424-L8435] | No |
| TW-341 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8436-L8445] | No |
| TW-342 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8446-L8457] | No |
| TW-343 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8458-L8470] | No |
| TW-344 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8471-L8480] | No |
| TW-345 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8481-L8492] | No |
| TW-346 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8493-L8505] | No |
| TW-347 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8506-L8517] | No |
| TW-348 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8518-L8527] | No |
| TW-349 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8528-L8537] | No |
| TW-350 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8538-L8547] | No |
| TW-351 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8548-L8560] | No |
| TW-352 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8561-L8572] | No |
| TW-353 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8573-L8582] | No |
| TW-354 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8583-L8596] | No |
| TW-355 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8597-L8608] | No |
| TW-356 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8609-L8619] | No |
| TW-357 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8620-L8632] | No |
| TW-358 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8633-L8647] | No |
| TW-359 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8648-L8657] | No |
| TW-360 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8658-L8667] | No |
| TW-361 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8668-L8677] | No |
| TW-362 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8678-L8687] | No |
| TW-363 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8688-L8701] | No |
| TW-364 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8702-L8714] | No |
| TW-365 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8715-L8724] | No |
| TW-366 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8725-L8739] | No |
| TW-367 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8740-L8749] | No |
| TW-368 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8750-L8759] | No |
| TW-369 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8760-L8773] | No |
| TW-370 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8774-L8783] | No |
| TW-371 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8784-L8793] | No |
| TW-372 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8794-L8803] | No |
| TW-373 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8804-L8813] | No |
| TW-374 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8814-L8823] | No |
| TW-375 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8824-L8833] | No |
| TW-376 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8834-L8843] | No |
| TW-377 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8844-L8856] | No |
| TW-378 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8857-L8866] | No |
| TW-379 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8867-L8876] | No |
| TW-380 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8877-L8886] | No |
| TW-381 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8887-L8896] | No |
| TW-382 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8897-L8908] | No |
| TW-383 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8909-L8918] | No |
| TW-384 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8919-L8928] | No |
| TW-385 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8929-L8938] | No |
| TW-386 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8939-L8948] | No |
| TW-387 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8949-L8958] | No |
| TW-388 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8959-L8970] | No |
| TW-389 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8971-L8980] | No |
| TW-390 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8981-L8990] | No |
| TW-391 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8991-L9000] | No |
| TW-392 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9001-L9010] | No |
| TW-393 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9011-L9020] | No |
| TW-394 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9021-L9030] | No |
| TW-395 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9031-L9040] | No |
| TW-396 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9041-L9050] | No |
| TW-397 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9051-L9060] | No |
| TW-398 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9061-L9070] | No |
| TW-399 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9071-L9080] | No |
| TW-400 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9081-L9090] | No |
| TW-401 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9091-L9100] | No |
| TW-402 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9101-L9110] | No |
| TW-403 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9111-L9120] | No |
| TW-404 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9121-L9130] | No |
| TW-405 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9131-L9140] | No |
| TW-406 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9141-L9152] | No |
| TW-407 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9153-L9162] | No |
| TW-408 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9163-L9172] | No |
| TW-409 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9173-L9182] | No |
| TW-410 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9183-L9192] | No |
| TW-411 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9193-L9202] | No |
| TW-412 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9203-L9212] | No |
| TW-413 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9213-L9222] | No |
| TW-414 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9223-L9232] | No |
| TW-415 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9233-L9242] | No |
| TW-416 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9243-L9252] | No |
| TW-417 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9253-L9262] | No |
| TW-418 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9263-L9272] | No |
| TW-419 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9273-L9282] | No |
| TW-420 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9283-L9292] | No |
| TW-421 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9293-L9302] | No |
| TW-422 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9303-L9312] | No |
| TW-423 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9313-L9322] | No |
| TW-424 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9323-L9332] | No |
| TW-425 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9333-L9342] | No |
| TW-426 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9343-L9352] | No |
| TW-427 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9353-L9362] | No |
| TW-428 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9363-L9372] | No |
| TW-429 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9373-L9382] | No |
| TW-430 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9383-L9392] | No |
| TW-431 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9393-L9402] | No |
| TW-432 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9403-L9412] | No |
| TW-433 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9413-L9422] | No |
| TW-434 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9423-L9432] | No |
| TW-435 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9433-L9442] | No |
| TW-436 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9443-L9452] | No |
| TW-437 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9453-L9462] | No |
| TW-438 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9463-L9472] | No |
| TW-439 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9473-L9482] | No |
| TW-440 | [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9483-L9493] | No |

</details>

## 10. Coverage and open items

| Item | Evidence and bounded coverage | Responsible role / tracking |
| --- | --- | --- |
| Package read set | Both current Python files, all three legacy Python files and legacy output text were read; the legacy output is generated drawing text. [src/digitalmodel/subsea/vertical_riser/vertical_riser.py:L1-L25] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L1-L413] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure.py:L1-L54] [src/digitalmodel/subsea/vertical_riser/legacy/RiserStackUp_Figure_R1_D1.py:L1-L56] [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py:L1-L257] [src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup_output.txt:L1-L49] | Engineering contributors; legacy handling issue pending (5) |
| Dedicated tests | Section 4 enumerates every collected dedicated node; seven conversion cases and nine suspended-force cases are bounded arithmetic checks. [tests/subsea/vertical_riser/test_component_units.py:L65-L121] [tests/subsea/vertical_riser/test_suspended_tension.py:L42-L144] | Quality manager; wider report coverage #3320 |
| Documentation discovery | Module README has TODOs; diagram description is under construction; modulus plan explicitly excludes full integration and reports. [docs/domains/subsea/vertical_riser/README.md:L5-L17] [docs/domains/diagrams/basename/vertical_riser/vertical_riser.md:L1-L16] [docs/plans/vertical-riser-modulus.md:L22-L25] | Engineering contributors; #3320 |
| Additional docs read | `docs/migration/legacy-batch-workflows.md`; `docs/field-development/subsea-production-systems-mapping.md`; `docs/domains/development/test_summary_with_validations.md`; `docs/domains/testing/engine_test_implementation_plan.md`; `docs/domains/testing/engine_test_strategy_summary.md`; `docs/domains/testing/engine_test_strategy.md`; `docs/domains/diagrams/tests/tests.md`; `docs/registry/implementation-map.md`; `docs/registry/implementation-map.yaml`. These are discovery/strategy material, not independent validation. [docs/migration/legacy-batch-workflows.md:L146-L153] [docs/field-development/subsea-production-systems-mapping.md:L77-L90] [docs/domains/development/test_summary_with_validations.md:L25-L30] [docs/domains/testing/engine_test_strategy_summary.md:L105-L109] [docs/domains/testing/engine_test_strategy.md:L80-L85] [docs/domains/testing/engine_test_implementation_plan.md:L270-L276] [docs/domains/diagrams/tests/tests.md:L5-L20] | Engineering contributors; reconcile claims with executable evidence, #3320 |
| Discovery outside this method | `docs/domains/risers/lift-riser-analysis.md` discusses slurry transport rather than this stack-up method; no values were reused. [docs/domains/risers/lift-riser-analysis.md:L41-L46] | Engineering contributors; scope remains #3320 |
| Discovery hygiene | 1 discovery file has an identifying directory name; archive material also contains identifying references; those paths and values are withheld and require codenaming before use; sidecar entry: to be created. | Maintainer; issue pending (5) |
| Strength defect | R2 characterizes the double strength conversion, not a correct strength. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L287-L289] | Engineering contributors; issue pending (1) |
| Weight-unit contract | Measured and calculated wet-property branches need an explicit common mass/force basis and branch tests. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L168-L209] | Engineering contributors; issue pending (2) |
| Auxiliary area | Initialization order, wrong matched index and global flag application require tests before relying on multi-line results. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L35-L37] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L254-L258] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L336-L356] | Engineering contributors; issue pending (3) |
| UNVERIFIED claim 1 | Complete-workflow geometry, depth, loading and solver-version bounds remain unestablished by the current examples. [tests/subsea/vertical_riser/test_suspended_tension.py:L71-L115] | Quality manager; issue pending (4) |
| UNVERIFIED claim 2 | Horizontal/phase conventions and all output file unit contracts require a complete model fixture and schema. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L388] | Quality manager; issue pending (4) |
| UNVERIFIED claim 3 | API 5L edition, clause/table and selector provenance are not recorded here; clause text not checked against the licensed copy. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L128-L138] | Quality manager; issue pending (4) |
| Input and QA evidence gaps | S01–S35 retain their checks or explicit lack of a detection check; C03, C11, C22, C31 and C51 need a public fixture or licensed execution. No complete-model validation is claimed. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L28-L48] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L358-L407] | Quality manager; issue pending (4) |
| Map follow-up | Unit-boundary, auxiliary-area and SHEAR7 methods need map classification and updated source locations; map gaps do not change the counted-family grep result. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L263-L291] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L336-L356] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L409-L413] | Engineering contributors; issue pending (6) |
| Historical routing checks | Current import guards exist, but the old routing skip still states the missing-import premise. [tests/security/test_review_20260523_remaining.py:L15-L28] [tests/test_engine.py:L192-L210] | Engineering contributors; #620; historical relocation #756 |
| External contract | The calculation-citation contract was readable; no new engineering constant or module code is introduced by this pack. The material selector and conversion methods have no emitted sidecar in their implementation. [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L120-L147] [src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py:L263-L291] | Engineering contributors; issue pending (4) |

Public reference readings: [NIST, Guide to the SI, Appendix B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8); [OpenStax, University Physics Volume 1, section 12.1](https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium); [BIPM, SI Brochure, ninth edition, Appendix 1](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf).
