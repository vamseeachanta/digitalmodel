# Solvers top-level knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers, direct Python files only |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `32fe066f387c13a66fd234eca2850a00c16e104a` |
| Analysis family | diffraction; registry code `DF` |
| Quality role | diffraction quality manager |
| Status | DRAFT; independent clearance pending |

## 1. Purpose and scope

`ParametricSpecGenerator` deep-copies a base dictionary, expands frequency, heading and nested-parameter dimensions, constructs `DiffractionSpec` objects and optionally serializes them to numbered YAML files. [src/digitalmodel/solvers/parametric_spec_generator.py:L99-L137] [src/digitalmodel/solvers/parametric_spec_generator.py:L141-L178]

This pack covers `parametric_spec_generator.py` and the direct `__init__.py`; the initializer also imports the separate `openfoam` package. [src/digitalmodel/solvers/parametric_spec_generator.py:L1-L34] [src/digitalmodel/solvers/__init__.py:L1-L3]

The registry assigns the encompassing `solvers` domain maturity `beta`; its listed test root is `tests/solvers/`, whereas the generator's direct tests reside in `tests/solver/`. [docs/registry/module-routing.yaml:L303-L312] [tests/solver/test_parametric_spec_generator.py:L24-L32]

Generation here ends at a canonical specification, with schema validation rather than a diffraction solution or native program load; the downstream schema describes inputs for both AQWA and OrcaWave. [src/digitalmodel/solvers/parametric_spec_generator.py:L113-L137] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L972-L977]

Subpackages have separate scope and are inventoried in section 9; applicability to real meshes, equilibrium, hydrodynamic responses and particular program versions is not established by the synthetic-fixture tests. [tests/solver/test_parametric_spec_generator.py:L40-L73] [tests/solver/test_parametric_spec_generator.py:L81-L253]

## 2. Inputs and outputs

| Input | Unit | Range or constraint | Default | Source |
| --- | --- | --- | --- | --- |
| `base_spec_data` | schema-dependent | dictionary; construction delegates validation to `DiffractionSpec` | required | [src/digitalmodel/solvers/parametric_spec_generator.py:L99-L124] |
| `frequency_sweeps` | rad/s, count | list of `FrequencySweep(start, end, steps)`; dataclass supplies no bounds checks | absent or empty means no expansion | [src/digitalmodel/solvers/parametric_spec_generator.py:L42-L66] [src/digitalmodel/solvers/parametric_spec_generator.py:L99-L109] |
| `heading_sweeps` | degrees | list of lists assigned to `wave_headings.values` | absent or empty means no expansion | [src/digitalmodel/solvers/parametric_spec_generator.py:L69-L76] [src/digitalmodel/solvers/parametric_spec_generator.py:L151-L155] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L442-L448] |
| `hull_sweeps` | target-field units | dot-separated dictionary path and values; not restricted to geometry | absent or empty means no expansion | [src/digitalmodel/solvers/parametric_spec_generator.py:L79-L88] [src/digitalmodel/solvers/parametric_spec_generator.py:L157-L160] [src/digitalmodel/solvers/parametric_spec_generator.py:L186-L191] |
| Vessel mass, CoG, gyration radii | kg, m, m | mass > 0; three-coordinate reference point and CoG | mass and CoG required; radii optional subject to inertia validation | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L171-L179] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L201-L265] |
| Environment | m, kg/m³, m/s² | positive numerical depth or deep-water alias; density screening envelope in schema | density 1025.0 kg/m³; gravity 9.80665 m/s² | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L301-L361] |
| Frequency representation | rad/s or s | exactly one of explicit values or range; conversion rejects nonpositive or nonfinite values | `frequency` | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L375-L431] |
| Mesh locator and reference point | file locator, m | relative to specification location; three coordinates | format auto; reference point [0, 0, 0] m; waterline z = 0 m; mesh length unit m | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L144-L179] |
| `output_dir` | directory | created before generation validates the base | required for serialization | [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L137] |

| Output | Unit / format | Behaviour | Source |
| --- | --- | --- | --- |
| `generate()` | list of canonical objects | ordered expansion, metadata tag insertion, then schema construction | [src/digitalmodel/solvers/parametric_spec_generator.py:L113-L125] |
| `generate_to_yaml()` | YAML, list of paths | `spec_sweep_000.yml` onward; existing names opened for writing; no mesh copied | [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L137] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1076-L1084] |
| Serialized fields | schema units | JSON-compatible model dump excludes `None`; YAML preserves model key order | [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1076-L1084] |

The generator does not transform coordinates, heading signs or phase; the schema labels coordinates in meters and headings in degrees, and the generator replaces field values directly. [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L158-L169] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L442-L448] [src/digitalmodel/solvers/parametric_spec_generator.py:L147-L160]

UNVERIFIED: a qualified coordinate-frame, origin, heading-sign and phase contract for use of these generated files in each native program is not established here; issue pending (3).

## 3. Method and governing standards

| Definition | Method and limitations | Reference |
| --- | --- | --- |
| `FrequencySweep.generate_ranges` | Centers use `linspace(start, end, steps)`; spacing = (end − start) / max(steps − 1, 1); half-band = max(0.4 × spacing, 0.05) rad/s; low endpoint = max(center − half-band, 0.01) rad/s; high endpoint = center + half-band; five samples per band, rounded to six decimals. The comment calls the band ±10.0% of spacing but the expression uses 40.0%. | [src/digitalmodel/solvers/parametric_spec_generator.py:L54-L66] |
| `HeadingSweep`, `HullParameterSweep` | Data holders for heading sets and arbitrary nested-field dimensions. | [src/digitalmodel/solvers/parametric_spec_generator.py:L69-L88] |
| `ParametricSpecGenerator.__init__` | Deep-copies the base; normalizes absent and empty sweep lists to `[]`. | [src/digitalmodel/solvers/parametric_spec_generator.py:L99-L109] |
| `_build_variants`, `_expand` | Applies frequency, heading, then hull dimensions; each expansion loops values outside existing variants. Counts multiply, and later dimensions on the same path overwrite earlier values without deduplication. | [src/digitalmodel/solvers/parametric_spec_generator.py:L141-L178] |
| `_set_nested` | Splits a dotted path, creates absent intermediate dictionaries and assigns the terminal value. | [src/digitalmodel/solvers/parametric_spec_generator.py:L186-L191] |
| `generate` | Adds a numbered tag only when no existing tag contains `sweep` or `parametric`, then constructs the schema. | [src/digitalmodel/solvers/parametric_spec_generator.py:L113-L125] |
| `generate_to_yaml` | Delegates serialization to `DiffractionSpec.to_yaml`; this is specification output rather than a solver deck. | [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L137] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1076-L1084] |

No governing engineering standard or standards-derived `Citation` sidecar is declared in the two direct source files; the frequency-band constants are implementation choices, not a cited physical validation reference. [src/digitalmodel/solvers/__init__.py:L1-L3] [src/digitalmodel/solvers/parametric_spec_generator.py:L1-L191]

## 4. Validated worked examples

Validated examples: **0**; the direct tests supply a synthetic schema fixture and hard-coded structural expectations, not an independent physical solution. [tests/solver/test_parametric_spec_generator.py:L40-L73] [tests/solver/test_parametric_spec_generator.py:L81-L253]

Regression examples: **14**; the table identifies every test in the direct suite, with its asserted expectation rather than an inferred hydrodynamic acceptance claim. [tests/solver/test_parametric_spec_generator.py:L81-L253]

The recorded offline executions use code revision `32fe066f387c13a66fd234eca2850a00c16e104a`; run form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/solver/test_parametric_spec_generator.py -q -rA -p no:cacheprovider --basetemp=<scratch>`, using the explicitly selected uv executable and dedicated environment; no addopts override or parallel workers. [tests/solver/test_parametric_spec_generator.py:L24-L32] [pytest.ini:L47-L51]

| Case / test node ID | Fixture and expected value | Tolerance basis | BASE / after |
| --- | --- | --- | --- |
| `tests/solver/test_parametric_spec_generator.py::TestSingleSpecGeneration::test_single_spec_from_base` | synthetic base; 1 schema object | exact count and type | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L84-L90] |
| `tests/solver/test_parametric_spec_generator.py::TestSingleSpecGeneration::test_single_spec_validates_against_schema` | density 1025.0 kg/m³; 3 frequencies | exact fixture equality | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L92-L100] |
| `tests/solver/test_parametric_spec_generator.py::TestSingleSpecGeneration::test_single_spec_to_yaml` | 1 existing file; restored synthetic name | exact count and string equality | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L102-L110] |
| `tests/solver/test_parametric_spec_generator.py::TestFrequencySweep::test_frequency_sweep_generates_multiple_specs` | start 0.1, end 2.0 rad/s; 3 steps → 3 specs | exact count | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L121-L130] |
| `tests/solver/test_parametric_spec_generator.py::TestFrequencySweep::test_frequency_sweep_range_values` | start 0.5, end 1.5 rad/s; 3 distinct bands | exact tuple-set count; no endpoint oracle | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L132-L142] |
| `tests/solver/test_parametric_spec_generator.py::TestFrequencySweep::test_frequency_sweep_schema_compliance` | start 0.1, end 3.0 rad/s; 5 steps | frequencies > 0 rad/s, schema type | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L144-L152] |
| `tests/solver/test_parametric_spec_generator.py::TestHeadingSweep::test_heading_sweep_generates_specs` | 3 heading sets → 3 specs | exact count | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L163-L168] |
| `tests/solver/test_parametric_spec_generator.py::TestHeadingSweep::test_heading_sweep_values_correct` | [0, 180]° and [0, 90, 180, 270]° | exact lists; 0° difference | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L170-L178] |
| `tests/solver/test_parametric_spec_generator.py::TestHeadingSweep::test_heading_sweep_schema_compliance` | [0, 45, 90, 135, 180]° | schema instance | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L180-L186] |
| `tests/solver/test_parametric_spec_generator.py::TestHullParameterSweep::test_hull_mass_sweep` | masses [500, 1000, 2000] kg | exact lists; 0 kg difference | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L197-L207] |
| `tests/solver/test_parametric_spec_generator.py::TestHullParameterSweep::test_hull_sweep_schema_compliance` | masses [800, 1200] kg | mass > 0 kg; schema instance | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L209-L216] |
| `tests/solver/test_parametric_spec_generator.py::TestSpecSchemaCompliance::test_generated_specs_have_required_fields` | vessel, environment, frequencies, headings | 0 missing objects | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L227-L235] |
| `tests/solver/test_parametric_spec_generator.py::TestSpecSchemaCompliance::test_generated_specs_round_trip_yaml` | 2 frequency variants; restore synthetic name | exact name; no full-field or mesh equality assertion | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L237-L244] |
| `tests/solver/test_parametric_spec_generator.py::TestSpecSchemaCompliance::test_metadata_includes_sweep_info` | 2 variants; each has a sweep-related tag | 0 objects without matching tag | PASS / PASS [tests/solver/test_parametric_spec_generator.py:L246-L253] |

## 5. QA gates

Gate applicability follows AQG r0, sections 8–10: G0, G4 and G5 describe generated-specification criteria; G1 native loading is NOT_APPLICABLE for this package and remains a downstream diffraction-backend gate; G2 and G3 are `NOT_APPLICABLE` because the diffraction family has no statics or short-dynamics gate. (AQG r0, revision 0, sections 8–10, OrcaWave and AQWA rows.)

The following are acceptance instructions; PASS of a schema check shall not be substituted for a native-load or physical gate record.

| Gate / check | Executable check and numeric pass criterion | Evidence / current coverage |
| --- | --- | --- |
| G0 / C01 | Run the three `TestSingleSpecGeneration` tests and `test_generated_specs_have_required_fields` in section 4: exactly 1 output spec for no sweeps and 0 missing required objects (count). | Regression node results; schema fields [tests/solver/test_parametric_spec_generator.py:L84-L110] [tests/solver/test_parametric_spec_generator.py:L227-L235] |
| G0 / C02 | Run the three `TestFrequencySweep` tests: 3 specs, 3 distinct frequency sets for the 3-step case; all converted frequencies > 0 rad/s in the 5-step case. | Regression node results; no independent band-width reference [tests/solver/test_parametric_spec_generator.py:L121-L152] |
| G0 / C03 | Run the three `TestHeadingSweep` tests: 3 outputs for 3 sets, and heading lists differ from the two fixture lists by exactly 0° per entry. | Regression node results [tests/solver/test_parametric_spec_generator.py:L163-L186] |
| G0 / C04 | Run both `TestHullParameterSweep` tests: 3 masses match [500, 1000, 2000] kg with 0 kg difference; 2-schema-object case has mass > 0 kg. | Regression node results [tests/solver/test_parametric_spec_generator.py:L197-L216] |
| G0 / C05 | Run `test_generated_specs_round_trip_yaml`: 2 specs reload with 0 synthetic-name differences (count). | Regression node results; other fields and mesh existence are not asserted [tests/solver/test_parametric_spec_generator.py:L237-L244] |
| G0 / C06 | Run `test_metadata_includes_sweep_info`: 0 generated specs without a sweep-related tag (count). | Regression node results; this does not identify every swept dimension [tests/solver/test_parametric_spec_generator.py:L246-L253] |
| G1 / C07 | `NOT_APPLICABLE`: ParametricSpecGenerator.generate_to_yaml writes canonical specifications only; native OrcaWave loading belongs to the diffraction backend gate. | No package-output acceptance is claimed by this check. |
| G1 / C08 | `NOT_APPLICABLE`: ParametricSpecGenerator.generate_to_yaml writes canonical specifications only; native AQWA loading belongs to the diffraction backend gate. | No package-output acceptance is claimed by this check. |
| G2 | `NOT_APPLICABLE`: diffraction family has no statics gate. | AQG r0 §10. |
| G3 | `NOT_APPLICABLE`: diffraction family has no short-dynamics gate. | AQG r0 §10. |
| G4 / C09 | For each free-floating generated body, compare `spec.environment.water_density × independently integrated mesh volume` against the declared mass, or the native displaced mass where free-floating mode derives it; require absolute mass difference / governing mass ≤ 1.0% (percent). State gravity consistently when comparing forces. | NOT_RUN: fixture has no mesh or native hydrostatics; do not use a changed schema mass as proof of changed OrcaWave free-floating mass. Units and mode [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L182-L215] [tests/solver/test_parametric_spec_generator.py:L49-L65]; criterion: AQG r0 §8 G4.1. |
| G4 / C10 | Resolve each generated `vessel.geometry.mesh_file` from the output specification directory, integrate its closed surface independently and compare volume with native hydrostatics; require 0 missing mesh files (count) and volume difference ≤ 0.5% (percent). | NOT_RUN: reference mesh absent; locator rule [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L144-L147]; criterion: AQG r0 §8 G4.2. |
| G4 / C11 | Compare each body's mesh waterline z with `vessel.geometry.waterline_z`: maximum discrepancy ≤ 0.001 m; compare draft with the declared design draft: discrepancy ≤ 0.010 m. | NOT_RUN: no mesh/design-draft record in fixture; schema field [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L158-L169]; criterion: AQG r0 §8 G4.5. |
| G5 / C12 | For each generated specification included in a report, count missing input digests, generator/base revisions, gate dispositions, native versions, physical evidence and independent sign-off fields; require 0 missing required fields (count). | NOT_RUN: no evidence-appendix generator or populated evidence manifest in this scope; implementation emits paths and optional tags only [src/digitalmodel/solvers/parametric_spec_generator.py:L113-L137]; criterion: AQG r0 §9. |

G0 C01–C06 are narrow regression checks; duplicate-key refusal, mesh resolution, checksums and native selector ordering are not established by them, and G4 additionally requires the remaining balance, centres, normals, irregular-frequency and response checks of AQG r0 before analysis acceptance. [tests/solver/test_parametric_spec_generator.py:L81-L253] (AQG r0, revision 0, §§4 and 8; issue pending (3).)

## 6. Known failure modes and pitfalls

First-priority selection defect: `tests/solver/conftest.py` marks all 14 offline generator regression nodes `solver`, despite their schema/Python-only operations. Any `-m "not solver"` invocation silently deselects all 14; it supplies no offline regression evidence for this package. The conftest docstring claiming a licensed dependency does not describe these nodes. The section-4 results apply only to the explicit run without that exclusion. Detection criterion: the offline selection shall retain 14 of 14 generator nodes (count); current `-m "not solver"` selection fails that criterion (0 retained). Marker repair remains issue pending (6); no marker or collection policy is changed by this pack. [tests/solver/conftest.py:L1-L13] [tests/solver/test_parametric_spec_generator.py:L81-L253]

sweep: 17 found

The count covers distinct fallback, empty-result, precision and unit/overwrite sites in the two direct Python files, including intended defaults; subpackage internals are excluded by scope. [src/digitalmodel/solvers/__init__.py:L1-L3] [src/digitalmodel/solvers/parametric_spec_generator.py:L42-L191]

Exact discovery commands:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|0\.4|0\.01|float\(|int\(|setdefault|or \[\]|get\(|round\(' src/digitalmodel/solvers/*.py
rg -n '\[\]|\{\}|return|linspace|frequencies\.values|wave_headings\.values|_expand|to_yaml' src/digitalmodel/solvers/*.py
```

| Site | Symptom and cause | Existing detection | Tracking / date |
| --- | --- | --- | --- |
| S01 | Band comment says ±10.0% while half-band uses 40.0% of spacing, bounded below by 0.05 rad/s; the spacing denominator is floored at 1 for at most 1 step. [src/digitalmodel/solvers/parametric_spec_generator.py:L57-L59] | no detection check exists for exact band width | issue pending (1); 2026-10-16 |
| S02 | Frequencies below 0.01 rad/s are clipped at the low endpoint, changing the requested band. [src/digitalmodel/solvers/parametric_spec_generator.py:L62-L64] | C02 checks positivity, not clipping magnitude | issue pending (1); 2026-10-16 |
| S03 | Six-decimal rounding can collapse very close samples. [src/digitalmodel/solvers/parametric_spec_generator.py:L64-L65] | no detection check exists for rounding collisions | issue pending (1); 2026-10-16 |
| S04 | `steps=0` produces no centers and returns an empty band list; no input guard precedes `linspace`. [src/digitalmodel/solvers/parametric_spec_generator.py:L50-L66] | no detection check exists for zero steps | #1711; 2026-10-16 |
| S05 | Missing or empty frequency sweep list becomes `[]`, retaining the base frequency definition. [src/digitalmodel/solvers/parametric_spec_generator.py:L102-L107] [src/digitalmodel/solvers/parametric_spec_generator.py:L147-L149] | C01 exercises absence; no detection check exists for explicit empty list | #1711; 2026-10-16 |
| S06 | Missing or empty heading sweep list becomes `[]`, retaining the base headings. [src/digitalmodel/solvers/parametric_spec_generator.py:L103-L108] [src/digitalmodel/solvers/parametric_spec_generator.py:L151-L155] | C01 exercises absence; no detection check exists for explicit empty list | #1711; 2026-10-16 |
| S07 | Missing or empty hull sweep list becomes `[]`, retaining the base properties. [src/digitalmodel/solvers/parametric_spec_generator.py:L104-L109] [src/digitalmodel/solvers/parametric_spec_generator.py:L157-L160] | C01 exercises absence; no detection check exists for explicit empty list | #1711; 2026-10-16 |
| S08 | Absent metadata becomes an empty dictionary before schema defaults apply. [src/digitalmodel/solvers/parametric_spec_generator.py:L119-L124] | C06 checks tag creation from metadata-free fixture | issue pending (2); 2026-10-16 |
| S09 | Absent tags become `[]`; a string passed through `list(...)` becomes individual character tags; no original-input digest is added. [src/digitalmodel/solvers/parametric_spec_generator.py:L120-L124] | C06 checks the default tag; no detection check exists for string splitting or missing fingerprint | issue pending (2); 2026-10-16 |
| S10 | Any existing tag containing either substring suppresses the numbered variant tag. [src/digitalmodel/solvers/parametric_spec_generator.py:L121-L123] | no detection check exists for suppression or unique variant provenance | issue pending (2); 2026-10-16 |
| S11 | `generate` returns `[]` when expansion removes all variants. [src/digitalmodel/solvers/parametric_spec_generator.py:L115-L125] | no detection check exists for empty generated results | #1711; 2026-10-16 |
| S12 | `_expand` returns its empty accumulator when given no values or no base variants. [src/digitalmodel/solvers/parametric_spec_generator.py:L172-L178] | no detection check exists for empty dimensions | #1711; 2026-10-16 |
| S13 | `generate_to_yaml` returns `[]` after directory creation when no specs exist. [src/digitalmodel/solvers/parametric_spec_generator.py:L129-L137] | no detection check exists for empty serialization output | #1711; 2026-10-16 |
| S14 | Missing intermediate dotted-path keys become dictionaries rather than rejected targets. [src/digitalmodel/solvers/parametric_spec_generator.py:L186-L191] | C04 covers a valid target; no detection check exists for invalid targets | issue pending (1); 2026-10-16 |
| S15 | Frequency numbers assume rad/s but replacing `frequencies.values` leaves `input_type` untouched; period inputs are therefore reinterpreted as seconds. [src/digitalmodel/solvers/parametric_spec_generator.py:L46-L48] [src/digitalmodel/solvers/parametric_spec_generator.py:L147-L149] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L424-L426] | no detection check exists for a period-based base | issue pending (1); 2026-10-16 |
| S16 | Heading numbers are assigned directly without conversion; the schema describes degrees. [src/digitalmodel/solvers/parametric_spec_generator.py:L151-L155] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L445-L448] | C03 checks degree-valued fixture; no detection check exists for a mismatched unit basis | issue pending (3); 2026-10-16 |
| S17 | Multiple dimensions targeting the same path overwrite earlier values while retaining multiplied variant counts. [src/digitalmodel/solvers/parametric_spec_generator.py:L147-L160] [src/digitalmodel/solvers/parametric_spec_generator.py:L173-L177] | no detection check exists for repeated dimensions | issue pending (1); 2026-10-16 |

There are no `except`, `continue`, explicit `return None`, external-text `int()`/`float()` conversions, epoch defaults or cache writes in these two files; empty accumulators and every discovered numeric floor/default are covered above. [src/digitalmodel/solvers/__init__.py:L1-L3] [src/digitalmodel/solvers/parametric_spec_generator.py:L1-L191]

| Additional pitfall | Detection and disposition | Tracking |
| --- | --- | --- |
| Expansion eagerly copies all combinations without a maximum; no CLI or parameter-derived output naming is implemented. [src/digitalmodel/solvers/parametric_spec_generator.py:L141-L178] [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L137] | no detection check exists; bound the requested product before generation | #1711 |
| Range-based frequency or heading bases retain `range` when explicit values are inserted, and the schema rejects both together. [src/digitalmodel/solvers/parametric_spec_generator.py:L147-L155] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L392-L401] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L461-L471] | no detection check exists for range-based bases | issue pending (1) |
| Mesh locators are relative to the saved spec, but serialization does not rebase or copy them; output directory creation precedes validation and numbered files can overwrite prior output. [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L137] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L144-L147] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1080-L1083] | no detection check exists in shipped tests; C10 defines the required mesh-location check | issue pending (2); #1711 |
| The fixture uses free-floating mode while changing schema mass; the schema states that OrcaWave ignores that mass in this mode. [tests/solver/test_parametric_spec_generator.py:L55-L59] [tests/solver/test_parametric_spec_generator.py:L197-L207] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L182-L190] | C04 establishes schema mutation only; C09 defines physical comparison; no native detection check exists in this suite | issue pending (3) |
| The enclosing registry has two `submodules` keys and discovery points outside the direct test directory. [docs/registry/module-routing.yaml:L303-L325] [tests/solver/test_parametric_spec_generator.py:L24-L32] | no detection check exists for duplicate keys in the current permissive routing loader [tests/docs/test_digitalmodel_routing_contract.py:L35-L37] | issue pending (5) |

The source and tests name #1596, whereas #1711 describes this generator's guardrails; incident ownership shall be checked before treating a docstring issue number as supporting evidence. [src/digitalmodel/solvers/parametric_spec_generator.py:L21-L21] [tests/solver/test_parametric_spec_generator.py:L6-L6]

## 7. Lessons log

| Date | Entry | Lesson and pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial pack | Separate specification regression from physical validation; record the free-floating mass caveat in sections 4–6. | [tests/solver/test_parametric_spec_generator.py:L197-L207] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L182-L190] | Not assigned; issue pending (3) |
| 2026-10-16 | Initial pack | Record empty expansion and input-representation pitfalls before reusing a sweep base. | [src/digitalmodel/solvers/parametric_spec_generator.py:L141-L178] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L392-L401] | Not assigned; issue pending (1) |

## 8. Ownership

Required review roles: diffraction quality manager, with diffraction deputy quality manager as deputy; module maintainers produce changes and evidence, and a different provider reviews the pack before the maintainer merges.

Escalate ambiguous units, missing reference data, failed gates and changes to tolerances or standard editions to the diffraction quality manager; cross-family effects require maintainer disposition. These are review responsibilities for this pack, not a record of completed review. (AQG r0, revision 0, §11, sign-off and independence.)

## 9. Dependencies and other implementations

| Boundary | Dependency or consumer | Evidence |
| --- | --- | --- |
| Python | NumPy supplies band interpolation; `copy` isolates variant dictionaries; schema construction is delegated. | [src/digitalmodel/solvers/parametric_spec_generator.py:L25-L34] [src/digitalmodel/solvers/parametric_spec_generator.py:L106-L124] [src/digitalmodel/solvers/parametric_spec_generator.py:L173-L176] |
| Upstream | Caller-owned base dictionary and sweep definitions; no file-reading API is implemented here. | [src/digitalmodel/solvers/parametric_spec_generator.py:L99-L109] [src/digitalmodel/solvers/parametric_spec_generator.py:L113-L137] |
| Downstream | Canonical `DiffractionSpec` feeds separate OrcaWave and AQWA backend generation. No native solver version is pinned by this generator. | [src/digitalmodel/solvers/parametric_spec_generator.py:L124-L136] [src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py:L787-L812] [src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py:L177-L202] |
| Import surface | Top-level `solvers` eagerly imports its OpenFOAM Python package; solver invocation is outside this pack's scope. | [src/digitalmodel/solvers/__init__.py:L1-L3] |

Excluded scopes for this pack: `benchmark/`, `blender_automation/`, `calculix/`, `fea_model/`, `ghs/`, `gmsh_meshing/`, `mystran/`, `openfoam/`, `orcaflex/`, `orcawave/`, `smoke/`; review their implementations under their own package scopes, rather than including recursive rows in the top-level inventory.

Implementation-map pin: `32fe066f387c13a66fd234eca2850a00c16e104a`; map data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; the map bytes at the code pin match the requested map revision. [docs/registry/implementation-map.yaml:L1-L3]

A8 inventory, computed over all implementation rows in that map: **0 top-level in-package rows, 0 module map families, empty section-9 implementation inventory**; subpackage rows are excluded. A8 counted-family ranking is empty; counted regex search is **NOT_APPLICABLE**, because no family has a row within this pack's direct-file scope. [docs/registry/implementation-map.yaml:L1-L3] [src/digitalmodel/solvers/parametric_spec_generator.py:L1-L191]

Reproduce the inventory at the map pin; for a nonempty result, rank by descending in-package count with map order breaking ties, then search the first three families using their unmodified Python regular expressions, omitting path-matching signals.

```python
import subprocess
import yaml

map_pin = "32fe066f387c13a66fd234eca2850a00c16e104a"
raw = subprocess.check_output([
    "git", "show", f"{map_pin}:docs/registry/implementation-map.yaml"
])
registry = yaml.safe_load(raw)
prefix = "src/digitalmodel/solvers/"
for family in registry["families"]:
    rows = [row for row in family["implementations"]
            if row["path"].startswith(prefix)
            and "/" not in row["path"][len(prefix):]]
    if rows:
        print(family["id"], len(rows), rows)
```

Whole-repository discovery, including `scripts/`, searches the generator symbols and distinctive field assignments; matching tests, imports and documentation are helpers or references, not additional implementations. [tests/solver/test_parametric_spec_generator.py:L24-L32] [src/digitalmodel/hydrodynamics/diffraction/resolver.py:L432-L436]

```sh
rg -n 'parametric_spec_generator|class ParametricSpecGenerator|class FrequencySweep|def generate_ranges' src scripts tests docs --glob '*.py' --glob '*.md'
rg -n 'frequencies\.values|wave_headings\.values|spec_sweep_|parametric-sweep-|def generate_batch_specs' src scripts tests --glob '*.py'
```

| Other implementation or helper | Classification and follow-up | Evidence |
| --- | --- | --- |
| `hydrodynamics/diffraction/parametric_spec_generator.py`, `generate_diffraction_spec` | Separate implementation: constructs a schema from a hull profile, generates and exports a mesh and estimates inertia; outside this package, hence a map follow-up rather than an A8 miss. | [src/digitalmodel/hydrodynamics/diffraction/parametric_spec_generator.py:L243-L285] |
| Same module, `generate_batch_specs` | Batch helper over a separate hull-parameter space; delegates profile variation and spec generation. | [src/digitalmodel/hydrodynamics/diffraction/parametric_spec_generator.py:L397-L425] |
| `hydrodynamics/diffraction/resolver.py` | Imports the separate hull-profile bridge; not a second implementation of the top-level sweep expander. | [src/digitalmodel/hydrodynamics/diffraction/resolver.py:L432-L436] |

Map follow-up: add canonical specification Cartesian expansion, frequency-band generation and nested-field assignment, including the definitions listed in section 3 and the external bridge above; no family counts toward A8 until its map revision contains an in-package implementation row; issue pending (4). [src/digitalmodel/solvers/parametric_spec_generator.py:L42-L191]

## 10. Coverage and open items

| Coverage | Reading and evidence |
| --- | --- |
| Direct source | Complete `solvers/__init__.py` and `solvers/parametric_spec_generator.py`. [src/digitalmodel/solvers/__init__.py:L1-L3] [src/digitalmodel/solvers/parametric_spec_generator.py:L1-L191] |
| Direct tests | All 14 test definitions and the synthetic base fixture; section 4 lists every node. The directory conftest adds a solver marker, but these tests only invoke schema and Python generator operations. [tests/solver/test_parametric_spec_generator.py:L24-L253] [tests/solver/conftest.py:L8-L13] |
| Schema interfaces | Geometry, inertia, environment, frequency, headings and serialization; no native program is needed to read these contracts. [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L132-L265] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L301-L484] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L972-L1088] |
| Docs naming this path | `docs/plans/evidence/issue-2082-ci-discovery.md` includes the direct test in a broader discovery command; it is not a physical validation source. [docs/plans/evidence/issue-2082-ci-discovery.md:L1-L10] |
| Related discovery docs | `docs/README.md`, `docs/maps/digitalmodel-operator-map.md` and the registry identify the solver domain; `docs/migration/legacy-batch-workflows.md` instead names the separate diffraction generator. [docs/README.md:L25-L27] [docs/maps/digitalmodel-operator-map.md:L65-L65] [docs/registry/module-routing.yaml:L303-L325] [docs/migration/legacy-batch-workflows.md:L93-L96] |
| Test ownership | The contracts domain includes routing and documentation tests, alongside compatibility and validation roots. [tests/DOMAINS.md:L13-L13] |

No independent physical worked example is established by the direct suite; no client-derived dataset or licensed-standard value is included. [tests/solver/test_parametric_spec_generator.py:L40-L253]

| Open item | Required next evidence | Responsible role | Tracking |
| --- | --- | --- | --- |
| Sweep input representations, numerical floors, precision and target validation | Negative tests for empty or reversed dimensions, period/range bases, invalid paths and repeated paths; reconcile the band comment with its expression. [src/digitalmodel/solvers/parametric_spec_generator.py:L54-L66] [src/digitalmodel/solvers/parametric_spec_generator.py:L141-L191] | Module maintainer | issue pending (1) |
| Bounded generation, CLI and output naming | Product-size guard and naming/CLI tests; current implementation has neither a guard nor command-line dispatch. [src/digitalmodel/solvers/parametric_spec_generator.py:L127-L191] | Module maintainer | #1711 |
| Mesh-location and variant provenance | Full-field round-trip, mesh digest and relocation checks; output collision policy and pre-validation filesystem checks. [src/digitalmodel/solvers/parametric_spec_generator.py:L119-L137] [src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:L1066-L1084] | Module maintainer | issue pending (2) |
| Qualified example, downstream native gates, conventions and appendix | Independently sourced reference and evidence for C09–C12; C07/C08 remain the downstream backend responsibility; full AQG G0/G4 coverage. This includes the UNVERIFIED coordinate-frame, origin, heading-sign and phase contract from section 2. [tests/solver/test_parametric_spec_generator.py:L40-L253] | Diffraction quality manager | issue pending (3) |
| Map coverage and source issue references | Add method families and reconcile the external bridge and ambiguous source/test issue references. [src/digitalmodel/solvers/parametric_spec_generator.py:L21-L21] [tests/solver/test_parametric_spec_generator.py:L6-L6] [src/digitalmodel/hydrodynamics/diffraction/parametric_spec_generator.py:L243-L280] | Module maintainer | issue pending (4) |
| Offline regression selection | Retain all 14 generator nodes in offline selections; replace the blanket solver classification for these Python-only regressions, with a collection-count check. [tests/solver/conftest.py:L1-L13] | Module maintainer | issue pending (6) |
| Excluded subpackage coverage | Establish registered packs for `benchmark`, `blender_automation`, `calculix`, `fea_model`, `ghs`, `gmsh_meshing`, `mystran`, `orcawave` and `smoke`; `benchmark`, `fea_model`, `orcawave` and `smoke` contain 23 implementation-map rows in total and remain outside this direct-file pack. [docs/registry/implementation-map.yaml:L1-L3] | Repository maintainer | issue pending (7) |
| Registry duplication and discovery pointers | Duplicate-key-rejecting contract and complete test/doc routing; preserve all existing submodule entries in the eventual repair. [docs/registry/module-routing.yaml:L303-L325] | Repository maintainer | issue pending (5) |

All pending references above denote numbered issue drafts rather than published issues; replace them with issue numbers before independent A7 clearance. No clearance or independent-provider verdict is recorded by this draft.
