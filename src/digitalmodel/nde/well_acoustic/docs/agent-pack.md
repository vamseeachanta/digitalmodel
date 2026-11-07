# Well acoustic loader knowledge pack

| Header | Value |
|---|---|
| Module | `nde/well_acoustic` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/nde/well_acoustic` |
| Pack revision | 0 |
| Code revision | `309d343e08a2f83aebc6ea5344e586407b80c1f8` |
| Analysis family | Engineering calculations; routing code `EC` |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`AcousticScatteringLoader` selects a dataset variant, constructs a backend for the training split, and returns collected items with metadata and attribution. It contains no acoustic propagation calculation, receiver extraction or inspection acceptance criterion. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L149]

The existing NDE routing entry records maturity `development`; this is routing metadata rather than evidence of physical validation. [docs/registry/module-routing.yaml:L335-L344]

Scope includes both Python files in this package; the parent pulse-timing function is an external consumer used by a synthetic integration test. [src/digitalmodel/nde/well_acoustic/__init__.py:L25-L35] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L32-L149] [tests/nde/test_pulse_timing.py:L34-L57]

## 2. Inputs and outputs

| Input | Unit, range and default | Source and enforced contract |
|---|---|---|
| `AcousticScatteringLoader(variant)` | Dimensionless selector; default `basic`; accepted keys `basic`, `maze`, `inclusions` | Unknown keys raise `ValueError`; no case normalization. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L100] |
| `stream_sample(n_steps)` | Item count; default 10; annotation `int`, no runtime type or positivity check | Collection appends before comparing its length with the request. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L112] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L128-L141] |
| Backend data | Iterable items; no local physical unit, coordinate frame, origin, sign or phase conversion | Backend receives `dataset_name` and `split="train"`; items pass through unchanged. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149] |

| Public output or symbol | Contract, unit and default |
|---|---|
| `acoustic_scattering_loader.AcousticScatteringLoader` | Instance stores the validated dimensionless variant selector. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L76-L100] |
| `acoustic_scattering_loader.AcousticScatteringLoader.stream_sample` | Returns a dictionary after collection; raises `RuntimeError` before constructing the backend when unavailable. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L149] |
| `frames` | List of backend iteration items, without shape, channel or timestamp validation; may be empty on exhaustion. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L149] |
| `variant`, `dataset_name` | Requested selector and mapped string: `basic` → `acoustic_scattering`, `maze` → `acoustic_scattering_maze`, `inclusions` → `acoustic_scattering_inclusions`. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L143-L149] |
| `n_steps` | Actual returned list length, in items, rather than a physical duration or guaranteed requested count. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L149] |
| `attribution`, `THE_WELL_ATTRIBUTION` | Returned constant string names The Well dataset, its CC BY 4.0 attribution and source; this records the local label rather than independently certifying every data release. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L53-L56] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L143-L149] |
| `WELL_AVAILABLE`, `WellDataset` | Successful import sets the flag true; `ImportError` sets it false and the alias to `None`. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L40-L46] |
| `VARIANT_DATASET_MAP`, `VALID_VARIANTS` | Mutable mapping and a frozen snapshot of its keys. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] |
| `__init__.AcousticScatteringLoader`, `__init__.WELL_AVAILABLE`, `__init__.THE_WELL_ATTRIBUTION` | Package re-exports of the defining symbols, with no additional conversion. [src/digitalmodel/nde/well_acoustic/__init__.py:L25-L35] |

UNVERIFIED: U1 — real-data item schema, physical scaling, frame/receiver interpretation and inspection applicability; mocks supply synthetic objects rather than a qualified dataset release; issue pending (1). [tests/test_acoustic_scattering_loader.py:L30-L58] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149]

## 3. Method and governing standards

| Operation | Algorithm, reference and qualification |
|---|---|
| Variant selection | Membership check against the frozen key set, then lookup in the mutable map. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L100] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L135] |
| Sample collection | Append each item, then stop when collected length is at least the request; iterator exhaustion also ends collection. This is bookkeeping, with no numerical wave solver or standards-derived coefficient. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149] |
| Numeric setting | The default 10 is an item-count setting, with no physical sampling criterion in the implementation; it is not a time step in seconds. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L112] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L149] |
| Synthetic hand reference | The integration fixture supplies a pressure proportional to `exp(-(6-2*(t-1))**2)` and checks a peak at 4.0 s; setting the squared argument to zero gives `t=1+6/2=4.0 s`. This validates synthetic timestamp preservation through a mocked loader, not acoustic scattering physics. [tests/nde/test_pulse_timing.py:L34-L57] |
| Governing standards and citation sidecar | No standard, standard-derived formula or `Citation` object occurs in the implementation; no licensed clause value is reproduced here. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L32-L149] |

The local backend call uses `dataset_name` and `split`. The opened upstream version 1.2.0 constructor instead accepts `path`, `well_base_path`, `well_dataset_name` and `well_split_name`, and requires a location; its signature has no `**kwargs` accepting the local names. This is a source-confirmed interface mismatch; no backend was imported to establish an installed-version runtime result; issue pending (1). [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L135] [The Well contributors, *WellDataset source*, version 1.2.0, constructor, accessed 2026-11-07](https://github.com/PolymathicAI/the_well/blob/v1.2.0/the_well/data/datasets.py)

## 4. Validated worked examples and regression examples

The following results were recorded at the code revision in the header, before and after this documentation change; backend mocks do not establish actual dataset compatibility. [tests/test_acoustic_scattering_loader.py:L30-L90] [tests/nde/test_pulse_timing.py:L34-L57]

| Kind and case | Independent source or fixture; inputs | Expected value, tolerance and basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| Validated synthetic integration V1 | Closed-form maximum of the fixture Gaussian: `6-2*(t-1)=0`; distance 6 m, speed 2 m/s, emission 1 s; synthetic timestamped dictionaries | 4.0 s; hand difference 0.0 s because the exact peak timestamp is sampled; shipped assertion uses `pytest.approx(4.0)` | `tests/nde/test_pulse_timing.py::test_streamed_synthetic_pulse_travel_time` | PASS at BASE and AFTER; validates timestamp transport to the parent peak picker only. [tests/nde/test_pulse_timing.py:L34-L57] |
| Regression R1 requested count | Fake backend supplies 20 items; request 5 | 5 items and reported count 5; 0-item discrepancy | `tests/test_acoustic_scattering_loader.py::TestStreamSample::test_stream_sample_n_steps_respected` | PASS at BASE and AFTER. [tests/test_acoustic_scattering_loader.py:L48-L52] [tests/test_acoustic_scattering_loader.py:L149-L153] |
| Regression R2 default count | Same fake backend; omitted count | 10 items; 0-item discrepancy; implementation default rather than physical reference | `tests/test_acoustic_scattering_loader.py::TestStreamSample::test_stream_sample_default_n_steps_is_10` | PASS at BASE and AFTER. [tests/test_acoustic_scattering_loader.py:L155-L159] |
| Regression R3 dataset mapping | One requested item for each selector; fixture repeats local mapping | 3 exact string matches, 0 mismatches | `tests/test_acoustic_scattering_loader.py::TestAllVariants::test_variant_dataset_name_correct[basic]`; `tests/test_acoustic_scattering_loader.py::TestAllVariants::test_variant_dataset_name_correct[maze]`; `tests/test_acoustic_scattering_loader.py::TestAllVariants::test_variant_dataset_name_correct[inclusions]` | All 3 PASS at BASE and AFTER. [tests/test_acoustic_scattering_loader.py:L187-L196] |
| Regression R4 dependency guard | Blocked optional import; request 5 | 1 `RuntimeError`, 0 successful samples | `tests/test_acoustic_scattering_loader.py::TestWellAvailableFlag::test_stream_sample_raises_when_well_unavailable` | PASS at BASE and AFTER. [tests/test_acoustic_scattering_loader.py:L276-L299] |

No real acoustic dataset worked example is claimed: the backend fixture generates mock frames and the integration fixture supplies its own receiver trace. [tests/test_acoustic_scattering_loader.py:L30-L58] [tests/nde/test_pulse_timing.py:L37-L51]

## 5. QA gates

Gate assignment follows AQG r0, section 10, Python calculations, with G5 applying to every family. [AQG r0, sections 9–11]

| Gate | Applicability and route | Module-specific checks |
|---|---|---|
| G0 inputs, units, citations | Applicable: Python constructor and metadata contract; physical units require separate data qualification. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L149] | C1, C2 |
| G1 imports and tests | Applicable: offline Python imports and mocks; no target solver file is written by this module. [src/digitalmodel/nde/well_acoustic/__init__.py:L25-L35] [tests/test_acoustic_scattering_loader.py:L66-L90] | C3, C4 |
| G2 hand or published check | Applicable: closed-form synthetic timestamp and independently counted collection; no acoustic physics qualification. [tests/nde/test_pulse_timing.py:L34-L57] [tests/test_acoustic_scattering_loader.py:L48-L52] | C5, C6 |
| G3 short dynamics | NOT_APPLICABLE: implementation only collects backend items, with no dynamics calculation. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L128-L149] | None |
| G4 diffraction | NOT_APPLICABLE: no mesh, hydrostatic or diffraction output is computed. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L128-L149] | None |
| G5 evidence | Applicable: retain counts, attribution, node results and the code pin alongside any use of these checks. [AQG r0, section 9] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L143-L149] | C7, C8 |

| Check | Executable procedure | Numeric pass criterion and unit | Evidence and gate |
|---|---|---|---|
| C1 variant validation | Run `tests/test_acoustic_scattering_loader.py::TestAcousticScatteringLoaderConstruction::test_invalid_variant_raises_value_error` | 1 exception for the invalid selector, 0 invalid instances; count | PASS; G0. [tests/test_acoustic_scattering_loader.py:L112-L114] |
| C2 variant map | Run all 3 R3 nodes | 3 expected dataset identifiers, 0 string mismatches; count | PASS; G0. [tests/test_acoustic_scattering_loader.py:L187-L196] |
| C3 unavailable backend | Run R4 and `tests/test_acoustic_scattering_loader.py::TestWellAvailableFlag::test_well_available_false_when_not_installed` | Flag false plus 1 expected exception, 0 successful samples; count | PASS; G1; only tests the simulated absence route. [tests/test_acoustic_scattering_loader.py:L250-L299] |
| C4 import and offline tests | Run `tests/test_acoustic_scattering_loader.py tests/nde` through `python -m pytest -q -rA -p no:cacheprovider` with scratch basetemp | 55 passed, 0 failed, 0 skipped, 0 NOT_RUN; tests | PASS; G1; includes 25 loader tests, 13 parent-package tests, 14 pulse tests and 3 literal timing tests. [tests/test_acoustic_scattering_loader.py:L97-L299] [tests/nde/test_nde.py:L20-L126] [tests/nde/test_pulse_timing.py:L21-L83] [tests/nde/test_agent_pack_values.py:L13-L25] |
| C5 synthetic hand check | Run V1; separately solve `6-2*(t-1)=0`, inspect the timestamp of the unique absolute peak | 4.0 s with hand residual 0.0 s; seconds | PASS; G2; sampled synthetic integration only. [tests/nde/test_pulse_timing.py:L34-L57] |
| C6 collection hand check | Count 20 available fake items; for positive integer requests 5 and 10, hand-compute `min(available, requested)`; run R1 and R2 | Expected 5 and 10 items; 0-item discrepancy | PASS; G2; no assurance for invalid requests or exhausted data. [tests/test_acoustic_scattering_loader.py:L48-L52] [tests/test_acoustic_scattering_loader.py:L149-L159] |
| C7 attribution | Run `tests/test_acoustic_scattering_loader.py::TestAttribution::test_attribution_contains_cc_by` and `tests/test_acoustic_scattering_loader.py::TestStreamSample::test_stream_sample_has_attribution_key` | 2 passing assertions, 0 missing labels/keys; count | PASS; G5; content presence, not release-specific rights validation. [tests/test_acoustic_scattering_loader.py:L166-L169] [tests/test_acoustic_scattering_loader.py:L215-L222] |
| C8 result traceability | Extract section-4 node IDs, match each to BASE and AFTER result logs and retain input/code digests | 7 node IDs with 2 PASS records each; 0 unmatched nodes; count | PASS; G5; synthetic/mock inputs are defined in the cited fixtures. [tests/test_acoustic_scattering_loader.py:L30-L58] [tests/test_acoustic_scattering_loader.py:L149-L196] [tests/test_acoustic_scattering_loader.py:L276-L299] [tests/nde/test_pulse_timing.py:L34-L57] |

These gates cover wrapper contracts and synthetic data transport; they do not clear an inspection analysis based on real acoustic data. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149] [tests/nde/test_pulse_timing.py:L37-L51]

## 6. Known failure modes and pitfalls

sweep: 6 found

The count includes the optional-import fallback, item-count default, unchecked request, exhausted accumulator, unqualified item units/schema and mutable-map snapshot; these six rows are S1–S6 below. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L40-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L149]

Reproduce the package sweep at the code pin with these exact commands; inspect executable context after matching, so docstring occurrences do not become error handlers. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L27-L46] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L149]

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5\s*%|int\(|float\(|unit|frame|n_steps|cache' src/digitalmodel/nde/well_acoustic --glob '*.py'
rg -n 'return|break|\[\]|\{\}|None|default|VARIANT|split|append|len' src/digitalmodel/nde/well_acoustic --glob '*.py'
```

| ID; location at code pin | Symptom and cause | Existing detection check | Follow-up; recorded date |
|---|---|---|---|
| S1; import fallback | An `ImportError`, including one raised within the optional package, sets `WELL_AVAILABLE=False` and `WellDataset=None`; later sampling reports the package as required without preserving the cause. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L40-L46] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L128-L132] | C3 detects the unavailable flag and guarded call; no detection check exists for distinguishing missing from broken backend imports. | issue pending (1); 2026-11-07 |
| S2; default count | Omitted count collects up to 10 items with no physical sampling-adequacy criterion. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L112] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L149] | C6 detects default-count drift; no detection check exists for physical sampling adequacy. | issue pending (1); 2026-11-07 |
| S3; request handling | A nonempty iterable yields one item even for a zero/negative request; fractional counts round up through the comparison, while incompatible types may fail after consuming an item. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L141] | no detection check exists | issue pending (2); 2026-11-07 |
| S4; exhausted accumulator | Short or empty data returns fewer items or an empty list with actual `n_steps`, without signalling under-delivery; `frames=[]` initializes this ordinary accumulator rather than an exception fallback. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L137-L149] | no detection check exists | issue pending (2); 2026-11-07 |
| S5; item interpretation | Unvalidated backend items are called frames; there is no local schema, unit, time-coordinate or receiver-channel check. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L121] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149] | C5 only detects synthetic timestamp transport; no detection check exists for real-data interpretation. | issue pending (1); 2026-11-07 |
| S6; mutable variant mapping | `VALID_VARIANTS` is frozen once; later mutation of the public mapping or stored selector can bypass their original correspondence or cause lookup failure. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L100] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L135] | C2 detects the initial mapping only; no detection check exists for post-construction mutation. | issue pending (2); 2026-11-07 |
| Additional interface defect | The constructor call is incompatible with the opened upstream version 1.2.0 signature documented in section 3; this error propagates rather than being swallowed. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L135] | no detection check exists; mock constructor accepts the local names. [tests/test_acoustic_scattering_loader.py:L44-L46] | issue pending (1); 2026-11-07 |
| Discovery example | Example exits with status 0 if the backend is absent, and otherwise indexes the first returned frame without an empty-data check. [docs/examples/acoustic_nde_validation.py:L46-L49] [docs/examples/acoustic_nde_validation.py:L68-L80] | no detection check exists | issue pending (2); 2026-11-07 |
| Related historical coverage issue #2673 | The present synthetic streamed-pulse test provides a closed-form peak timestamp; it does not validate a real-data first-arrival detector. [tests/nde/test_pulse_timing.py:L34-L57] | C5 | #2673; 2026-11-07 |

No executable `continue`, `return None`, error-path empty-dictionary return, external-text numeric conversion, epoch fallback, zero-valued numeric default, percentage constant or cache write occurs in the two-file package; the `continue` grep hit is docstring prose. [src/digitalmodel/nde/well_acoustic/__init__.py:L1-L35] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L27-L149]

## 7. Lessons log

| Date | Entry | Lesson and evidence | Pack change | Shared-log ID |
|---|---|---|---|---|
| 2026-11-07 | Initial documentation | A mock constructor with local keyword names cannot establish real-backend compatibility; distinguish wrapper regression from integration qualification. [tests/test_acoustic_scattering_loader.py:L44-L58] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L135] | Record interface defect and qualification gap in sections 3, 6 and 10 | Not assigned; issue pending (1) |
| 2026-11-07 | Initial documentation | Exact timestamp transport through a synthetic fixture supports a closed-form peak check, without establishing real-data first-arrival behavior. [tests/nde/test_pulse_timing.py:L34-L57] [src/digitalmodel/nde/pulse_timing.py:L10-L23] | Separate V1 from real-data validation in sections 4 and 5 | Not assigned; issue pending (1) |

## 8. Ownership

Review assignments for this draft: engineering calculations quality manager; deputy engineering calculations quality manager; producing role calculation/documentation maintainer. These are role assignments, not claims of completed clearance. Independent gate sign-off follows AQG r0, section 11; unresolved criteria or data interpretation go to the quality manager, with cross-family questions referred to the maintainers. [AQG r0, section 11]

The package is indexed within the existing NDE domain rather than a new top-level domain; its existing maturity remains `development`. [docs/registry/module-routing.yaml:L335-L344]

## 9. Dependencies and implementation inventory

| Relationship | Dependency or consumer | Boundary |
|---|---|---|
| Runtime | Standard-library `typing.Any` and optional `the_well.data.WellDataset` | Import availability flag is distinct from compatibility or data availability. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L32-L46] |
| Declared optional dependencies | Both `well` and `nde` extras declare `the_well>=1.2.0` | The repository declares no upper bound here; source comparison in section 3 is specifically version 1.2.0. [pyproject.toml:L248-L255] |
| Package interface | Three re-exports | No local file-format writer, solver input or dimensional conversion. [src/digitalmodel/nde/well_acoustic/__init__.py:L25-L35] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L134-L149] |
| Example consumer | `docs/examples/acoustic_nde_validation.py` | Selects all three variants and inspects returned items. [docs/examples/acoustic_nde_validation.py:L55-L80] |
| Synthetic consumer | Parent `pulse_peak_time` | Test supplies receiver timestamps and pressure values; parent picker is outside this package. [tests/nde/test_pulse_timing.py:L34-L57] [src/digitalmodel/nde/pulse_timing.py:L9-L51] |

Implementation map source: `docs/registry/implementation-map.yaml` on `origin/codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; map citations in this section resolve at that map commit, independently of the code pin. [docs/registry/implementation-map.yaml:L1-L4]

| Inventory requirement | Recorded result |
|---|---|
| All module map families | Empty inventory: no implementation row in any family has a path below `src/digitalmodel/nde/well_acoustic/`. [docs/registry/implementation-map.yaml:L345-L26502] |
| Ranked counted families | 0 families; no ranking ties or path-matching signals require exclusion. [docs/registry/implementation-map.yaml:L345-L26502] |
| Counted-family grep | NOT_APPLICABLE because the module has no map family; inventory passes with an empty list, and no counted-family further implementation is asserted. [docs/registry/implementation-map.yaml:L345-L26502] |
| Map follow-up | Add variant selection and attributed bounded acoustic dataset collection, the methods of section 3, before counting their family in a later map revision; issue pending (3). [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L149] |

The inventory search checks every implementation row against the package prefix; counted-family searches, when applicable, use unmodified `search_signals` as case-sensitive Python regular expressions and exclude signals matching the relative package path case-insensitively. No counted family exists at this map pin. [docs/registry/implementation-map.yaml:L345-L26502]

The additional whole-repository search, including `scripts/`, used `git grep -n -E 'AcousticScatteringLoader|WellDataset|acoustic_scattering' <code-pin> -- '*.py'`; symbol inspection finds the acoustic class here, re-exports, test doubles, test call sites and the demonstration, plus a separate shear-flow collector. The latter collects and transforms velocity/vorticity fields rather than computing an acoustic result; it is a related map-follow-up candidate outside the package, not a counted-family miss; issue pending (3). [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L76-L149] [src/digitalmodel/nde/well_acoustic/__init__.py:L25-L35] [tests/test_acoustic_scattering_loader.py:L30-L58] [tests/nde/test_nde.py:L102-L126] [docs/examples/acoustic_nde_validation.py:L68-L80] [src/digitalmodel/hydrodynamics/well_datasets/shear_flow_loader.py:L89-L164]

## 10. Coverage and open items

| Reading inventory | Coverage at the code pin |
|---|---|
| Entire package | `__init__.py` and `acoustic_scattering_loader.py`; no subpackage excluded. [src/digitalmodel/nde/well_acoustic/__init__.py:L1-L35] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L1-L149] |
| Tests | All of `tests/test_acoustic_scattering_loader.py`, `tests/nde/test_nde.py`, `tests/nde/test_pulse_timing.py`, `tests/nde/test_agent_pack_values.py` and the empty test-package initializer; 55 collected nodes, of which 25 are in the dedicated loader file. [tests/test_acoustic_scattering_loader.py:L1-L299] [tests/nde/test_nde.py:L1-L126] [tests/nde/test_pulse_timing.py:L1-L83] [tests/nde/test_agent_pack_values.py:L1-L25] |
| Parent and consumer code | Parent package exports, pulse timing, example and related shear-flow collector. [src/digitalmodel/nde/__init__.py:L1-L10] [src/digitalmodel/nde/pulse_timing.py:L1-L51] [docs/examples/acoustic_nde_validation.py:L1-L91] [src/digitalmodel/hydrodynamics/well_datasets/shear_flow_loader.py:L77-L164] |
| Documentation and routing | Existing parent pack, documentation index, operator-map NDE row, registry NDE entry and dependency extras. [src/digitalmodel/nde/docs/agent-pack.md:L1-L117] [docs/README.md:L9-L17] [docs/maps/digitalmodel-operator-map.md:L55-L55] [docs/registry/module-routing.yaml:L335-L344] [pyproject.toml:L248-L255] |
| Generated documentation discovery | The evidence indexes list the dedicated loader test file; their presence is discovery evidence rather than an adopted maturity or validation figure. [docs/audits/module-report-evidence.html:L695-L695] [docs/audits/module-report-evidence.json:L10261-L10261] |
| Additional documentation discovery | UNVERIFIED: U2 — a public locator for 1 discovery file with an identifying path is not supplied; issue pending (4). |

Offline execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/test_acoustic_scattering_loader.py tests/nde -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; ordinary repository addopts retained, serial execution, no override or test exclusion. BASE and AFTER each record 55 run / 55 passed / 0 failed / 0 skipped / 0 NOT_RUN at the header code pin. Test counts are re-counted from the collected nodes rather than the earlier inventory estimate. [tests/test_acoustic_scattering_loader.py:L97-L299] [tests/nde/test_nde.py:L20-L126] [tests/nde/test_pulse_timing.py:L21-L83] [tests/nde/test_agent_pack_values.py:L13-L25]

| Open item | Evidence and required next step | Responsible role | Tracking |
|---|---|---|---|
| U1; no real-data worked example | Qualify a versioned public data fixture, constructor, canonical identifiers, attribution and physical interpretation; retain synthetic-only limits until then. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L128-L149] [tests/test_acoustic_scattering_loader.py:L30-L58] | Module maintainer | issue pending (1) |
| Count and mutation contracts | Define and test positive-integer requests, exhausted/empty data, mutable selectors/mappings and demonstration status. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L62-L68] [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L149] [docs/examples/acoustic_nde_validation.py:L46-L49] [docs/examples/acoustic_nde_validation.py:L68-L80] | Module maintainer | issue pending (2) |
| Missing method-family inventory | Add the section-3 loader methods to the implementation map with row evidence and search signals. [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L94-L149] | Map maintainer | issue pending (3) |
| U2; discovery codenaming | Provide the missing neutral locator described above before publishing its reference. | Documentation maintainer | issue pending (4) |

The related parent-pack request is #3337; historical synthetic-value coverage is tracked by #2673. Neither issue establishes live-data qualification; C5 provides only the synthetic result described in section 4. [src/digitalmodel/nde/docs/agent-pack.md:L66-L87] [tests/nde/test_pulse_timing.py:L34-L57]
