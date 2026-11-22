# Hydrostatics module knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrostatics |
| Repository | digitalmodel |
| Package | `src/digitalmodel/hydrostatics` |
| Pack revision | 0 |
| Code revision | `df9787d346c226aad513cab9d248f7be841b37d5` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Family quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package calculates constant-density static fluid-column pressure rise, adds a supplied surface pressure, and converts density, length and pressure units; the routing registry records beta maturity. [src/digitalmodel/hydrostatics/fluid_column.py:L5-L20] [docs/registry/module-routing.yaml:L257-L262]

`hydrostatic_pressure` returns pressure relative to the column top; `seabed_pressure` returns absolute pressure only when its surface input is absolute, and adds no implicit atmospheric pressure. [src/digitalmodel/hydrostatics/fluid_column.py:L102-L121] [src/digitalmodel/hydrostatics/fluid_column.py:L132-L157]

The synthetic example excludes variable density or gravity, flow losses, transients, thermal effects, structural response and pipeline-code acceptance; the two calculation functions implement multiplication and surface-pressure addition only. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L120-L125] [src/digitalmodel/hydrostatics/fluid_column.py:L123-L159]

## 2. Inputs and outputs

Inputs are supplied directly by the caller; there is no parser or file writer in the implementation, and no range or finiteness validation before the arithmetic. [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159]

| Input | Unit | Allowed range / enforcement | Default | Source |
| --- | --- | --- | --- | --- |
| `density` | kg/m³ | Caller supplies physically justified density; no enforced range | Required | [src/digitalmodel/hydrostatics/fluid_column.py:L97-L123] |
| `height` | m | Vertical separation, positive downward from column top; no enforced range | Required | [src/digitalmodel/hydrostatics/fluid_column.py:L110-L123] |
| `gravity` | m/s² | Caller supplies acceleration; no enforced range | `G_STANDARD = 9.81` | [src/digitalmodel/hydrostatics/fluid_column.py:L54-L55] [src/digitalmodel/hydrostatics/fluid_column.py:L97-L123] |
| `surface_pressure` | Pa | Caller selects absolute or gauge reference; no enforced range | 0.0 Pa, column rise only | [src/digitalmodel/hydrostatics/fluid_column.py:L126-L159] |
| `density_ppg` | lb/US-gal | Multiplication only; no enforced range | Required | [src/digitalmodel/hydrostatics/fluid_column.py:L57-L69] |
| `depth_ft` | ft | Multiplication only; no enforced range | Required | [src/digitalmodel/hydrostatics/fluid_column.py:L72-L74] |
| `pressure_psi`, `pressure_pa`, `pressure_bar` | psi, Pa, bar respectively | Multiplication only; no enforced range | Required | [src/digitalmodel/hydrostatics/fluid_column.py:L77-L94] |

| Output | Unit | Meaning / evidence |
| --- | --- | --- |
| `hydrostatic_pressure(density, height, gravity)` | Pa | Column pressure rise; scalar or NumPy arithmetic result. [src/digitalmodel/hydrostatics/fluid_column.py:L97-L123] |
| `seabed_pressure(density, height, surface_pressure, gravity)` | Pa | Bottom pressure on the supplied reference. [src/digitalmodel/hydrostatics/fluid_column.py:L126-L159] |
| `ppg_to_kg_m3`, `ft_to_m` | kg/m³, m respectively | Converted value. [src/digitalmodel/hydrostatics/fluid_column.py:L67-L74] |
| `psi_to_pa`, `pa_to_psi`, `bar_to_psi`, `psi_to_bar` | Pa, psi, psi, bar respectively | Converted value. [src/digitalmodel/hydrostatics/fluid_column.py:L77-L94] |

With upward-positive elevations use `height = top_elevation - bottom_elevation`; shifting both elevations leaves the separation unchanged, and no phase convention enters the static example. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L40-L62]

NumPy-array multiplication is exercised, but the broad `ArrayLike` annotation does not itself enforce array type, compatible shape or units. [tests/hydrostatics/test_fluid_column.py:L66-L71] [src/digitalmodel/hydrostatics/fluid_column.py:L52-L52] [src/digitalmodel/hydrostatics/fluid_column.py:L123-L123]

## 3. Method and governing standards

| Method | Implementation | Basis and qualification |
| --- | --- | --- |
| `delta_p = density * gravity * height` | `hydrostatic_pressure` | Static column relation; independently derived by mass, weight and force over unit area in the synthetic note. [src/digitalmodel/hydrostatics/fluid_column.py:L97-L123] [docs/examples/pipeline-hydrostatic-pressure-basis.md:L40-L46] |
| `bottom = surface + delta_p` | `seabed_pressure` | Delegates the column calculation and adds the caller's pressure boundary; reference must be explicit. [src/digitalmodel/hydrostatics/fluid_column.py:L126-L159] |
| Density and length conversion | `ppg_to_kg_m3`, `ft_to_m` | Multiply by module constants; the density constant specifies U.S. gallons. [src/digitalmodel/hydrostatics/fluid_column.py:L57-L74] |
| Pressure conversion | `psi_to_pa`, `pa_to_psi`, `bar_to_psi`, `psi_to_bar` | Multiply by module constants; reciprocal constants couple the conversion directions. [src/digitalmodel/hydrostatics/fluid_column.py:L60-L94] |

The module docstring cites generic hydrostatics without an edition or clause; the example treats pipeline standards as context and makes no code acceptance claim. [src/digitalmodel/hydrostatics/fluid_column.py:L22-L27] [docs/examples/pipeline-hydrostatic-pressure-basis.md:L107-L125]

UNVERIFIED: authoritative provenance for the default gravity and the spreadsheet-derived conversion coefficients has not been established; numerical agreement with the existing worksheet assertions does not independently establish that provenance (issue pending (2)). [src/digitalmodel/hydrostatics/fluid_column.py:L54-L64] [tests/hydrostatics/test_fluid_column.py:L1-L11] [tests/hydrostatics/test_fluid_column.py:L31-L53]

The public API returns numeric arithmetic values and does not emit a `Citation` sidecar; resolving coefficient provenance and whether citation emission is required remains issue pending (2). [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159]

## 4. Validated worked examples

The validated cases below use the synthetic unit-area closed form, not a helper-generated oracle: volume 75.0 m³, mass 75000.0 kg, weight 750000.0 N and pressure rise 750000.0 Pa; adding 120000.0 Pa absolute gives 870000.0 Pa absolute. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L28-L46]

| Case | Inputs / source | Expected / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 synthetic column | Density 1000.0 kg/m³, gravity 10.0 m/s², height 75.0 m, surface 120000.0 Pa absolute | 750000.0 Pa rise, 870000.0 Pa absolute; exact equality (0 Pa error), synthetic exact arithmetic | `tests/hydrostatics/test_fluid_column.py::TestPipelinePressureBasis::test_synthetic_gauge_and_absolute_pressure` | PASS [tests/hydrostatics/test_fluid_column.py:L92-L107] |
| V2 original datum | Top 20.0 m, bottom -55.0 m; other V1 inputs | Height 75.0 m; same pressures, exact equality (0 Pa error) | `tests/hydrostatics/test_fluid_column.py::TestPipelinePressureBasis::test_height_datum_invariance[original-datum]` | PASS [tests/hydrostatics/test_fluid_column.py:L109-L123] |
| V3 raised datum | Both elevations shifted +250.0 m; other V1 inputs | Height 75.0 m; same pressures, exact equality (0 Pa error) | `tests/hydrostatics/test_fluid_column.py::TestPipelinePressureBasis::test_height_datum_invariance[raised-datum]` | PASS [tests/hydrostatics/test_fluid_column.py:L109-L123] |
| V4 lowered datum | Both elevations shifted -400.0 m; other V1 inputs | Height 75.0 m; same pressures, exact equality (0 Pa error) | `tests/hydrostatics/test_fluid_column.py::TestPipelinePressureBasis::test_height_datum_invariance[lowered-datum]` | PASS [tests/hydrostatics/test_fluid_column.py:L109-L123] |
| V5 executable note | Execute the single Python fence in the synthetic basis note | Height 75.0 m, pressures 750000.0 Pa and 870000.0 Pa; exact equality (0 Pa error) | `tests/hydrostatics/test_fluid_column.py::TestPipelinePressureBasis::test_documented_example_executes` | PASS [tests/hydrostatics/test_fluid_column.py:L125-L140] |

The older tests identify their oracle as a generic spreadsheet; vector and zero-surface checks reuse the formula or helper, so these are regression evidence rather than independent validation. [tests/hydrostatics/test_fluid_column.py:L1-L11] [tests/hydrostatics/test_fluid_column.py:L56-L89]

| Regression case | Expected value / unit and tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| R1 density conversion | Full-precision worksheet assertion in kg/m³, relative tolerance 1e-6 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_ppg_to_kg_m3` | PASS [tests/hydrostatics/test_fluid_column.py:L32-L33] |
| R2 length conversion | 10000.0 ft to 3048.0 m, relative tolerance 1e-9 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_ft_to_m` | PASS [tests/hydrostatics/test_fluid_column.py:L35-L36] |
| R3 psi conversion | Full-precision worksheet assertion in Pa, relative tolerance 1e-6 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_psi_to_pa` | PASS [tests/hydrostatics/test_fluid_column.py:L38-L39] |
| R4 Pa conversion | Worksheet Pa input to 20000.0 psi, relative tolerance 1e-6 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_pa_to_psi_roundtrip` | PASS [tests/hydrostatics/test_fluid_column.py:L41-L42] |
| R5 surface conversion | Full-precision worksheet assertion in psi, relative tolerance 1e-6 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_surface_pressure_pa_to_psi` | PASS [tests/hydrostatics/test_fluid_column.py:L44-L46] |
| R6 bar round trip | Stored psi assertion, relative tolerance 1e-5; return to 1.0 bar, relative tolerance 1e-12 | `tests/hydrostatics/test_fluid_column.py::TestUnitConversions::test_bar_psi_roundtrip` | PASS [tests/hydrostatics/test_fluid_column.py:L48-L53] |
| R7 column worksheet | Difference of stored bottom and surface pressures in Pa, relative tolerance 1e-4 | `tests/hydrostatics/test_fluid_column.py::TestHydrostaticPressure::test_rho_g_h` | PASS [tests/hydrostatics/test_fluid_column.py:L57-L64] |
| R8 vector arithmetic | Density 1025.0 kg/m³, heights 0.0, 100.0, 1000.0 m; expected `rho * 9.81 * h` Pa, relative tolerance 1e-12 | `tests/hydrostatics/test_fluid_column.py::TestHydrostaticPressure::test_vectorised` | PASS [tests/hydrostatics/test_fluid_column.py:L66-L71] |
| R9 bottom worksheet | Stored Pa target, relative tolerance 1e-4; 20000.0 psi target, relative tolerance 1e-3 | `tests/hydrostatics/test_fluid_column.py::TestSeabedPressure::test_matches_worksheet` | PASS [tests/hydrostatics/test_fluid_column.py:L75-L83] |
| R10 zero surface | Match column helper in Pa, relative tolerance 1e-12 | `tests/hydrostatics/test_fluid_column.py::TestSeabedPressure::test_zero_surface_is_gauge` | PASS [tests/hydrostatics/test_fluid_column.py:L85-L89] |

Verification record for this pack: BASE and AFTER each ran `PYTHONPATH=src uv run --no-sources python -m pytest tests/hydrostatics/ -q -rA -p no:cacheprovider --basetemp=<scratch>` at the header code revision, with 15 run, 15 passed, 0 failed, 0 skipped and 0 NOT_RUN; no addopts override or solver route was used.

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with section 9 for G5; check criteria below are pack requirements, not claims of module-enforced input rejection.

| Gate | Applicability / route | Check IDs |
| --- | --- | --- |
| G0 | APPLICABLE: explicit SI calculation inputs, unit conversion and elevation/reference conventions | H01, H02 |
| G1 | APPLICABLE: Python import through the offline test module; no generated solver file or target-program load route | H03 |
| G2 | APPLICABLE: independent unit-area force balance, datum invariance and executable note | H04 |
| G3 | NOT_APPLICABLE: constant-column arithmetic has no time integration. [src/digitalmodel/hydrostatics/fluid_column.py:L97-L159] | None |
| G4 | NOT_APPLICABLE: no mesh, displacement or diffraction calculation. [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159] | None |
| G5 | APPLICABLE: trace test outcomes and calculation basis to the pin; operational evidence packaging remains issue pending (4) | H05 |

| Check | Executable route and quantity | Numeric pass criterion and unit | Evidence / gate |
| --- | --- | --- | --- |
| H01 | Run R1–R6 nodes for all six unit converters; retain assertions at their full precision | 6 passing test cases (count); relative errors within each source assertion in section 4 (dimensionless) | Test outcome list; G0. [tests/hydrostatics/test_fluid_column.py:L31-L53] |
| H02 | Run V1–V4: `hydrostatic_pressure` and `seabed_pressure` with supplied SI density, gravity and pressure reference | 4 passing cases (count); 0 Pa pressure error and 0 m height error | Synthetic input basis and node outcomes; G0. [tests/hydrostatics/test_fluid_column.py:L92-L123] |
| H03 | Run the full section-4 command, importing `digitalmodel.hydrostatics.fluid_column` through the tests | 15 passed, 0 failures, 0 skips, 0 NOT_RUN (test counts) | Full offline test output; G1. [tests/hydrostatics/test_fluid_column.py:L19-L28] [tests/hydrostatics/test_fluid_column.py:L31-L140] |
| H04 | Run V1–V5 against the note's independent force/area arithmetic | 5 passed (count); 0 Pa error for 750000.0 Pa rise and 870000.0 Pa absolute | Node outcomes plus note execution output; G2. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L40-L46] [tests/hydrostatics/test_fluid_column.py:L92-L140] |
| H05 | Build a scratch evidence record from both test logs: pin, SHA-256 of implementation, test and basis note, unit/reference description and each node outcome; recompute input digests and compare node lists | 3 matching source digests (count), 15 identical PASS nodes in each run (count), 0 missing fields (count) | Scratch evidence JSON and digest comparison; G5; proposed pack-level check, executed for this pack |

H01 and H03 check regression compatibility, not physical validity of arbitrary inputs; no existing detection check rejects nonfinite, wrong-unit, signed or accidentally broadcast inputs (issue pending (1)). [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159] [tests/hydrostatics/test_fluid_column.py:L31-L140]

## 6. Known failure modes and pitfalls

sweep: 13 found

Sweep definition: the following rows enumerate arithmetic-boundary assumptions and default sites in both package Python files; the command below searches the complete package, followed by reading every definition. No swallowed exception, skipped record, error-path empty return, external-text numeric conversion or cache write occurs in these files. [src/digitalmodel/hydrostatics/__init__.py:L1-L42] [src/digitalmodel/hydrostatics/fluid_column.py:L1-L159]

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|cache|unit|gravity|ppg|psi|bar|feet' src/digitalmodel/hydrostatics --glob '*.py'
```

| ID / site at code pin | Symptom and cause | Detection / gap | Tracking |
| --- | --- | --- | --- |
| S01 `ppg_to_kg_m3` | Density assumes lb/US-gal, not another gallon definition; unchecked factor multiplication. [src/digitalmodel/hydrostatics/fluid_column.py:L57-L69] | H01 detects factor drift; no detection check exists for wrong-unit input | issue pending (1), issue pending (2) |
| S02 `ft_to_m` | Metre input supplied as feet silently rescales depth. [src/digitalmodel/hydrostatics/fluid_column.py:L72-L74] | H01 detects factor drift; no detection check exists for caller unit mismatch | issue pending (1) |
| S03 `psi_to_pa` | Non-psi input silently rescales pressure. [src/digitalmodel/hydrostatics/fluid_column.py:L77-L79] | H01 detects factor drift; no detection check exists for caller unit mismatch | issue pending (1) |
| S04 `pa_to_psi` | Non-Pa input silently rescales pressure. [src/digitalmodel/hydrostatics/fluid_column.py:L82-L84] | H01 detects factor drift; no detection check exists for caller unit mismatch | issue pending (1) |
| S05 `bar_to_psi` | Non-bar input silently rescales pressure. [src/digitalmodel/hydrostatics/fluid_column.py:L87-L89] | H01 detects factor drift; no detection check exists for caller unit mismatch | issue pending (1) |
| S06 `psi_to_bar` | Non-psi input silently rescales pressure. [src/digitalmodel/hydrostatics/fluid_column.py:L92-L94] | H01 detects factor drift; no detection check exists for caller unit mismatch | issue pending (1) |
| S07 `hydrostatic_pressure` density | Expects kg/m³; no dimensional guard. [src/digitalmodel/hydrostatics/fluid_column.py:L110-L123] | H02 checks stated synthetic basis; no detection check exists for caller unit mismatch | issue pending (1) |
| S08 `hydrostatic_pressure` height | Expects vertical separation in m, positive down, not route length or absolute elevation. [src/digitalmodel/hydrostatics/fluid_column.py:L112-L123] | H02 catches datum errors in the example; no detection check exists for arbitrary caller geometry | issue pending (1) |
| S09 `hydrostatic_pressure` gravity | Default uses 9.81 m/s² without recording the chosen basis; explicit gravity is unchecked. [src/digitalmodel/hydrostatics/fluid_column.py:L54-L55] [src/digitalmodel/hydrostatics/fluid_column.py:L97-L123] | H03 includes default-gravity regression; no detection check exists for project-specific acceleration basis | issue pending (1), issue pending (2) |
| S10 `seabed_pressure` surface | 0.0 Pa default returns only column rise; a caller may incorrectly label it absolute; surface input assumes Pa. [src/digitalmodel/hydrostatics/fluid_column.py:L126-L150] | H03 includes R10; H02 checks supplied absolute boundary; no detection check exists for caller reference labelling | issue pending (1) |
| S11 `seabed_pressure` gravity | Separate default site also supplies 9.81 m/s² to the delegated calculation. [src/digitalmodel/hydrostatics/fluid_column.py:L129-L130] [src/digitalmodel/hydrostatics/fluid_column.py:L151-L159] | H03 includes default-gravity regression; no detection check exists for caller acceleration basis | issue pending (1), issue pending (2) |
| S12 `seabed_pressure` density/height | Delegated inputs still assume kg/m³ and m positive down; no added guard. [src/digitalmodel/hydrostatics/fluid_column.py:L143-L159] | H02 checks the example; no detection check exists for arbitrary caller units | issue pending (1) |
| S13 arithmetic boundary, all helpers | Negative or nonfinite numeric values and compatible but unintended NumPy shapes are not rejected by these expressions. [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159] | No detection check exists for these rejection policies; H03 covers only established positive examples | issue pending (1) |

Further pitfalls: reciprocal-factor round trips cannot independently verify a pressure factor (H01); the vector test constructs its oracle with the same gravity expression (H03), so neither is the G2 hand reference. [src/digitalmodel/hydrostatics/fluid_column.py:L60-L64] [tests/hydrostatics/test_fluid_column.py:L48-L53] [tests/hydrostatics/test_fluid_column.py:L66-L71]

The header supplies #767 and #779 instead of a coefficient source title or clause; the source-reference audit has no detection check in the module suite (issue pending (2)). [src/digitalmodel/hydrostatics/fluid_column.py:L22-L27]

Issue #1813 concerns the broader pipeline standards inventory, not a confirmed incident in this helper; H04 detects synthetic pressure arithmetic errors only and cannot validate that inventory. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L127-L135]

## 7. Lessons log

| Entry | Date | Lesson / evidence | Pack change | Shared record |
| --- | --- | --- | --- | --- |
| hydrostatics-001 | 2026-11-04 | A zero surface default supplies pressure rise; absolute pressure requires an absolute boundary. [src/digitalmodel/hydrostatics/fluid_column.py:L132-L150] | Sections 2, 5 and 6 distinguish reference pressure | This pack entry |
| hydrostatics-002 | 2026-11-04 | Unit-area hand arithmetic supplies an independent oracle; matching the helper's vector expression does not. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L40-L46] [tests/hydrostatics/test_fluid_column.py:L66-L71] | Sections 4 and 5 separate validation from regression | This pack entry |

## 8. Ownership

Required review roles: engineering calculations quality manager, engineering calculations deputy quality manager, and engineering calculations producer; refer formula, unit, tolerance and provenance disagreements to the family quality manager, and unresolved cross-family scope questions to the maintainers. These are governance requirements for this draft, not recorded appointments or clearance.

## 9. Dependencies and implementation inventory

Runtime implementation uses the standard-library typing module and arithmetic on caller-supplied values; NumPy and pytest are used by the tests, while `__init__.py` re-exports the calculation and conversion API. [src/digitalmodel/hydrostatics/fluid_column.py:L30-L52] [src/digitalmodel/hydrostatics/fluid_column.py:L67-L159] [tests/hydrostatics/test_fluid_column.py:L14-L28] [src/digitalmodel/hydrostatics/__init__.py:L8-L42]

The executable basis note is a documented consumer of the two calculation functions; this package has no solver launch, solver version requirement, file format or external dataset read. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L66-L95] [src/digitalmodel/hydrostatics/fluid_column.py:L30-L159]

A8 inventory record: map ref `codex/implmap-work`, map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the code-pin copy names the same data revision. [docs/registry/implementation-map.yaml:L1-L3]

A8 scan result: 13 map families inspected, 0 families with implementation paths inside `src/digitalmodel/hydrostatics/`, therefore an empty required implementation inventory and 0 counted families. Counted-family grep: `NOT_APPLICABLE`, because no map family has an in-package row; no search signals or rank ties apply. This is an inventory result, not a completeness claim for the repository. Map follow-up: add constant-column pressure, surface-pressure addition, density/length conversion and pressure conversion before counting these families (issue pending (3)).

Whole-repository discovery used both commands below, including `scripts/`; selected formula hits were read without importing solver code; other matches remain candidates for the map follow-up. The following selected independent implementations are map-follow-up candidates, outside the package and outside the A8 counted result.

```bash
rg -n 'hydrostatic_pressure|seabed_pressure|density.*gravity.*height|0\.052.*(?:ppg|density)|def .*hydrostatic|def fluid_column_profile' . --glob '*.py'
rg -n 'rho.*g.*(?:h|depth)|density.*gravity|density.*9\.81|0\.052|6894\.|119\.826|0\.3048' . --glob '*.py'
```

| Definition | Classification / evidence | Follow-up |
| --- | --- | --- |
| `compute_hydrostatic_pressure` | Independent pressure formula with a negative-depth guard. [src/digitalmodel/structural/analysis/wall_thickness_phases.py:L56-L73] | issue pending (3) |
| `fluid_column_profile` | Independent density-gradient column plus surface pressure, in ppg/ft/psi. [src/digitalmodel/well/tubulars/casing_design.py:L108-L117] | issue pending (3) |
| `hydro_raw_numpy`, `hydro_pint_quantity`, `hydro_ureg_wraps` | Three independent pressure expressions, including demonstration/benchmark routes. [scripts/integrations/pint_benchmark.py:L40-L57] | issue pending (3) |
| `demo_offshore_hydrostatic` | Independent unit-bearing pressure arithmetic in a demonstration. [scripts/integrations/pint_poc.py:L102-L114] | issue pending (3) |
| `seabed_pressure`, package exports | Delegation plus boundary addition, and re-exports; not another implementation of the column formula. [src/digitalmodel/hydrostatics/fluid_column.py:L159-L159] [src/digitalmodel/hydrostatics/__init__.py:L8-L42] | Already listed in section 3 |

## 10. Coverage and open items

Read inventory for this draft: both Python files in the package, its complete 15-case test file, the pressure-basis note, operator-map row, routing registry entry and implementation map metadata; no package subfolder contains additional Python code at the pin. [src/digitalmodel/hydrostatics/__init__.py:L1-L42] [src/digitalmodel/hydrostatics/fluid_column.py:L1-L159] [tests/hydrostatics/test_fluid_column.py:L31-L140] [docs/examples/pipeline-hydrostatic-pressure-basis.md:L1-L135] [docs/maps/digitalmodel-operator-map.md:L50-L50] [docs/registry/module-routing.yaml:L257-L262] [docs/registry/implementation-map.yaml:L1-L3]

Documentation discovery searched package references and fluid-column terminology in `docs/` and repository READMEs; the module-specific Markdown sources found are `docs/examples/pipeline-hydrostatic-pressure-basis.md` and `docs/maps/digitalmodel-operator-map.md`, rather than the separate hull/diffraction hydrostatics documentation. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L3-L7] [docs/maps/digitalmodel-operator-map.md:L50-L50]

| Coverage item / limit | Responsible role | Tracking and completion evidence |
| --- | --- | --- |
| Input domain, finite-value, shape, unit and reference policies; S01–S13 missing rejection checks | Engineering calculations maintainer | issue pending (1); add documented contracts and rejection tests |
| The section-3 UNVERIFIED coefficient provenance claim; source-reference corrections and citation-sidecar applicability | Engineering calculations maintainer | issue pending (2); independent sources and conversion checks |
| Map lacks the section-3 method families; repository candidates are not an exhaustive inventory | Implementation-map maintainer | issue pending (3); map revision with in-package rows and classified repository candidates |
| No operational evidence/appendix pipeline in the arithmetic API; H05 records this pack's evidence only | Engineering calculations maintainer | issue pending (4); persistent input fingerprints and reviewed evidence |
| Pipeline standards qualification remains outside this pressure-input basis | Pipeline standards maintainer | #1813; qualified standards inventory, not helper compliance. [docs/examples/pipeline-hydrostatic-pressure-basis.md:L127-L135] |

The five validated cases establish the stated synthetic constant-column basis, not the provenance of legacy worksheet factors or acceptance under a pipeline standard. [tests/hydrostatics/test_fluid_column.py:L92-L140] [docs/examples/pipeline-hydrostatic-pressure-basis.md:L107-L125]
