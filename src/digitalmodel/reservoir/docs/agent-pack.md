# Reservoir module knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | reservoir / digitalmodel |
| Package | `src/digitalmodel/reservoir` |
| Pack revision / status | 0 / DRAFT |
| Code pin | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Map ref / commit | `origin/codex/implmap-work` / `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Repository citations resolve at the code pin; the map inventory in section 9 uses the separately pinned map revision. Gate requirements below are prescribed checks under AQG r0, section 10, Python calculations, and section 9, QA appendix.

## 1. Purpose and scope

The package implements original oil in place (OOIP), recovery-factor multiplication and incremental recovery in stock-tank barrels, together with multi-well gamma-ray, resistivity, density-neutron and facies plotting. [src/digitalmodel/reservoir/volumetrics.py:L4-L19] [src/digitalmodel/reservoir/stratigraphic.py:L6-L17]

The routing registry records development maturity and routes tests to `tests/reservoir/`; this pack covers all three package Python files, including the docstring-only initializer. [docs/registry/module-routing.yaml:L346-L351] [src/digitalmodel/reservoir/__init__.py:L1-L1] [src/digitalmodel/reservoir/volumetrics.py:L29-L64] [src/digitalmodel/reservoir/stratigraphic.py:L60-L371]

The volumetric model assumes uniformly saturated, fully drained area and net pay; it is a direct arithmetic calculation rather than a time-dependent reservoir simulation. [src/digitalmodel/reservoir/volumetrics.py:L18-L19] [src/digitalmodel/reservoir/volumetrics.py:L40-L64]

The separately packaged `marine_ops/reservoir` implementation is outside this pack's scope; its OOIP and plotting methods are recorded as map follow-up in section 9. [src/digitalmodel/marine_ops/reservoir/properties.py:L255-L270] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L166-L175]

## 2. Inputs and outputs

Input contract (the ranges described as preconditions are caller checks; the dataclass itself does not enforce them). [src/digitalmodel/reservoir/volumetrics.py:L29-L48]

| Input | Unit / precondition | Default / source | Evidence |
| --- | --- | --- | --- |
| `VolumetricInputs.area_acres`, `net_pay_ft` | acres, ft; require finite nonnegative values before use | Required arguments | [src/digitalmodel/reservoir/volumetrics.py:L29-L48] |
| `porosity`, `oil_saturation` | Fractions; comments specify 0–1 | Required arguments | [src/digitalmodel/reservoir/volumetrics.py:L35-L36] |
| `formation_volume_factor` | Reservoir bbl/STB; require finite value > 0 | Required argument; used as divisor | [src/digitalmodel/reservoir/volumetrics.py:L16-L16] [src/digitalmodel/reservoir/volumetrics.py:L37-L48] |
| `ooip`, `recovery_factor`, `rf_from`, `rf_to` | STB and fractions; require 0 ≤ factors ≤ 1 and `rf_to >= rf_from` for an uplift | Required arguments; no validation | [src/digitalmodel/reservoir/volumetrics.py:L52-L64] |
| `wells_list`, `df_logs` | Ordered UWI list; columns UWI, Depth, GR, RT, RHOB, PHIN, KMeans | No default data; only nonempty list and column presence checked | [src/digitalmodel/reservoir/stratigraphic.py:L52-L83] [src/digitalmodel/reservoir/stratigraphic.py:L239-L260] |
| `statdata` | UWI, TOP, BASE; use one top/base record per well | Caller DataFrame; dictionary indexed by UWI | [src/digitalmodel/reservoir/stratigraphic.py:L249-L251] [src/digitalmodel/reservoir/stratigraphic.py:L273-L278] |
| Track controls | GR cutoff 30.0, GR limits 0–75; RT limits 1–1000 on log axis | Function defaults, no geological qualification | [src/digitalmodel/reservoir/stratigraphic.py:L90-L105] [src/digitalmodel/reservoir/stratigraphic.py:L124-L152] |
| Density / neutron controls | RHOB limits 1.95–2.95, PHIN limits 0.45 to −0.15; matrix/fluid density 2.71/1.0 in mutually consistent density units | Function defaults | [src/digitalmodel/reservoir/stratigraphic.py:L156-L175] |
| `facies_colors`, `figsize` | Integer-code colors; figure width and height | Codes 0–5; `(18, 10)` | [src/digitalmodel/reservoir/stratigraphic.py:L43-L50] [src/digitalmodel/reservoir/stratigraphic.py:L239-L264] |

| Output | Unit / convention | Evidence |
| --- | --- | --- |
| `ooip_stb`, `recoverable_stb`, `incremental_recovery_stb` | STB scalar; no file serialization | [src/digitalmodel/reservoir/volumetrics.py:L40-L64] |
| `create_cross_section` | Matplotlib Figure; four primary axes per requested well, shared depth axis | [src/digitalmodel/reservoir/stratigraphic.py:L280-L291] [src/digitalmodel/reservoir/stratigraphic.py:L355-L360] |
| Adjusted depth | `Depth - TOP`; zero is each well's formation top; track helpers invert depth axes | [src/digitalmodel/reservoir/stratigraphic.py:L303-L319] [src/digitalmodel/reservoir/stratigraphic.py:L117-L120] [src/digitalmodel/reservoir/stratigraphic.py:L150-L152] |
| Density-neutron helper | Two secondary axes; `DPHI` is calculated on a copy | [src/digitalmodel/reservoir/stratigraphic.py:L174-L202] |

UNVERIFIED: physical units for Depth/TOP/BASE, GR, RT, RHOB and PHIN are not declared by the plotting API; caller-supplied unit consistency and depth type must be documented before interpretation (issue pending (1)). [src/digitalmodel/reservoir/stratigraphic.py:L99-L105] [src/digitalmodel/reservoir/stratigraphic.py:L131-L134] [src/digitalmodel/reservoir/stratigraphic.py:L166-L175] [src/digitalmodel/reservoir/stratigraphic.py:L249-L253]

No geographic coordinate, phase or horizontal-distance input is consumed by the plotter; ordering follows the requested UWI categories and it plots tracks rather than a spatially scaled section. [src/digitalmodel/reservoir/stratigraphic.py:L267-L290] [src/digitalmodel/reservoir/stratigraphic.py:L311-L319]

## 3. Method and governing standards

| Method | Implementation / reference basis | Limits |
| --- | --- | --- |
| OOIP | `ooip_stb`: `7758 * area_acres * net_pay_ft * porosity * oil_saturation / formation_volume_factor`; docstring gives the field-unit conversion basis | No named standard, edition, clause or emitted Citation sidecar in this module. [src/digitalmodel/reservoir/volumetrics.py:L10-L19] [src/digitalmodel/reservoir/volumetrics.py:L24-L48] |
| Recovery | `recoverable_stb = ooip * recovery_factor`; incremental recovery `= ooip * (rf_to - rf_from)` | No depletion, sweep-efficiency or uncertainty model. [src/digitalmodel/reservoir/volumetrics.py:L52-L64] |
| Density porosity | `plot_rhob_nphi_track`: `(matrix_density - RHOB) / (matrix_density - fluid_density)` | No zero-denominator or finite-value check before crossover shading. [src/digitalmodel/reservoir/stratigraphic.py:L156-L202] |
| Depth alignment / correlations | `create_cross_section` sorts by UWI and Depth, subtracts TOP, draws thickness and neighboring-base lines | No distance scaling or stratigraphic inference algorithm. [src/digitalmodel/reservoir/stratigraphic.py:L266-L278] [src/digitalmodel/reservoir/stratigraphic.py:L299-L357] |
| Facies display | `plot_facies_track` sorts color-map keys and passes raw class values to `imshow` | Does not compute KMeans classes or explicitly map class codes to fixed color bins. [src/digitalmodel/reservoir/stratigraphic.py:L205-L231] |

UNVERIFIED: independent provenance and precision of the barrel conversion and density/cutoff defaults are not established by an opened external reference; repository equations are implementation evidence only (issue pending (1)). [src/digitalmodel/reservoir/volumetrics.py:L10-L26] [src/digitalmodel/reservoir/stratigraphic.py:L90-L105] [src/digitalmodel/reservoir/stratigraphic.py:L156-L175]

No licensed standard is adopted by this pack; no licensed clause or tabulated value was checked or reproduced. The calculation-citation contract should be applied if a future change introduces a standards-derived constant; current methods return scalars or axes, without a Citation payload. [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/reservoir/stratigraphic.py:L174-L202]

## 4. Validated worked examples and regression examples

Validated examples: **0**. The numerical assertions below exercise the implementation equation or arithmetic identities; no independently opened benchmark establishes OOIP accuracy or geological applicability (issue pending (2)). [tests/reservoir/test_volumetrics.py:L16-L43]

Regression examples: **5**, all PASS in both offline runs at the code pin; fixtures are inline synthetic inputs, with no private data or solver required. The two plotting examples use mocked Matplotlib, so their results do not establish rendered appearance. [tests/reservoir/test_volumetrics.py:L19-L43] [tests/reservoir/test_stratigraphic.py:L24-L51] [tests/reservoir/test_stratigraphic.py:L118-L136] [tests/reservoir/test_stratigraphic.py:L177-L194]

| Case | Inline inputs / expected value / tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- |
| OOIP arithmetic | 1000 acres, 50 ft, porosity 0.20, So 0.75, Boi 1.2; 48,487,500.0 STB, relative tolerance 1e-6 from the assertion | `tests/reservoir/test_volumetrics.py::TestOoip::test_known_volumetric` | PASS / PASS. [tests/reservoir/test_volumetrics.py:L19-L28] |
| Recovery fraction | 100,000,000 STB × 0.25; 25,000,000 STB, pytest.approx default tolerance | `tests/reservoir/test_volumetrics.py::TestRecovery::test_recoverable_is_fraction_of_ooip` | PASS / PASS. [tests/reservoir/test_volumetrics.py:L36-L38] |
| Recovery uplift | 100,000,000 STB, factors 0.22 to 0.30; 8,000,000 STB, pytest.approx default tolerance | `tests/reservoir/test_volumetrics.py::TestRecovery::test_incremental_uplift` | PASS / PASS. [tests/reservoir/test_volumetrics.py:L40-L43] |
| Cross-section smoke | Two synthetic wells, four mocked primary axes per well; returned object is non-None, tolerance 0 missing objects | `tests/reservoir/test_stratigraphic.py::TestFunctionSignatures::test_create_cross_section_returns_figure` | PASS / PASS. [tests/reservoir/test_stratigraphic.py:L118-L136] |
| Empty-well rejection | Empty list; exactly one expected ValueError containing `wells_list` | `tests/reservoir/test_stratigraphic.py::TestErrorHandling::test_create_cross_section_empty_wells_list` | PASS / PASS. [tests/reservoir/test_stratigraphic.py:L177-L184] |

Offline execution record, 2026-10-18: `PYTHONPATH=src uv run --no-sources python -m pytest tests/reservoir tests/test_reservoir_framework.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; BASE and AFTER use this same form, without overriding addopts. Both runs: collected 53, executed 23, passed 23, failed 0, skipped 30, NOT_RUN 0; code pin unchanged. Test definitions comprise 8 package checks, 8 plotting checks, 7 volumetric checks and 30 legacy framework nodes. [tests/reservoir/test_reservoir.py:L21-L76] [tests/reservoir/test_stratigraphic.py:L93-L207] [tests/reservoir/test_volumetrics.py:L16-L74] [tests/test_reservoir_framework.py:L51-L802]

The 30 legacy nodes skip because their guarded import requests absent package exports, starting with `ReservoirProperties`; installing an extra cannot add these missing exports to the initializer (issue pending (4)). No solver-dependent test was withheld. [tests/test_reservoir_framework.py:L22-L57] [src/digitalmodel/reservoir/__init__.py:L1-L1]

## 5. QA gates and executable checks

The applicable set is G0, G1, G2 and G5; G3 and G4 are NOT_APPLICABLE for this Python calculation/plotting package (AQG r0, section 10, Python calculations; section 9, QA appendix).

| Gate | Applicability / route | Check IDs |
| --- | --- | --- |
| G0 | Applicable: caller input contract, units and provenance review before calculation | R-G0a, R-G0b, R-G0c |
| G1 | Applicable: Python imports and offline pytest; no target-program deck is written by these APIs | R-G1a, R-G1b |
| G2 | Applicable: arithmetic checks independent of the production function calls; external validation remains open | R-G2a, R-G2b |
| G3 | NOT_APPLICABLE: no dynamic time integration in the covered functions | None. [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/reservoir/stratigraphic.py:L239-L360] |
| G4 | NOT_APPLICABLE: no diffraction body or panel calculation in the covered functions | None. [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/reservoir/stratigraphic.py:L239-L360] |
| G5 | Applicable: retain test results, code pin, citations, input provenance and limits with any calculation report | R-G5a |

Checks defined: **8** (G0: 3, G1: 2, G2: 2, G3: 0, G4: 0, G5: 1). The following are repeatable checks, with acceptance criteria prescribed here; their passing results do not resolve the section-6 defects.

| ID | Executable check | Numeric pass criterion and unit | Evidence / current result |
| --- | --- | --- | --- |
| R-G0a | Run `tests/reservoir/test_stratigraphic.py::TestErrorHandling::test_create_cross_section_empty_wells_list` | 1 expected ValueError, 0 unexpected exceptions (counts) | PASS / PASS. [tests/reservoir/test_stratigraphic.py:L177-L184] |
| R-G0b | Run `tests/reservoir/test_stratigraphic.py::TestErrorHandling::test_create_cross_section_missing_columns` | 1 expected ValueError, 0 unexpected exceptions (counts) | PASS / PASS. [tests/reservoir/test_stratigraphic.py:L186-L194] |
| R-G0c | Run `tests/reservoir/test_volumetrics.py::TestOoip::test_higher_boi_lowers_ooip` | `ooip(higher Boi) - ooip(base) < 0 STB` for the stated two inputs | PASS / PASS. [tests/reservoir/test_volumetrics.py:L30-L33] |
| R-G1a | Run both `tests/reservoir/test_stratigraphic.py::TestModuleImport` nodes | 2 passes, 0 failed nodes (counts); functions callable/present | PASS / PASS; mocked imports only. [tests/reservoir/test_stratigraphic.py:L93-L108] |
| R-G1b | Run `tests/reservoir/test_stratigraphic.py::TestFunctionSignatures::test_create_cross_section_returns_figure` | 0 missing returned objects (count), 0 exceptions | PASS / PASS; mock figure smoke. [tests/reservoir/test_stratigraphic.py:L118-L136] |
| R-G2a | Run OOIP regression; independently evaluate the displayed product/division using decimal arithmetic and the inline test inputs | Absolute difference ≤ 48.5 STB (relative assertion 1e-6), arithmetic result 48,487,500.0 STB | PASS arithmetic and test; verifies algebra only. [tests/reservoir/test_volumetrics.py:L19-L28] |
| R-G2b | Run both `TestRecovery` nodes; independently multiply the inline volume by 0.25 and by `(0.30 - 0.22)` | Differences ≤ 25.0 STB and ≤ 8.0 STB respectively; expected 25,000,000 and 8,000,000 STB | PASS arithmetic and tests; engineering recovery factors remain caller assumptions. [tests/reservoir/test_volumetrics.py:L36-L43] |
| R-G5a | Resolve every pack `[path:Lstart-Lend]` reference with `git show <code-pin>:<path>`; retain counts, test-node outcomes and SHA-256 of the input source files in the evidence record | 0 unresolved citations, 0 missing outcomes for the 5 section-4 cases (counts) | Self-check evidence; does not constitute an independently signed report appendix (AQG r0, sections 9 and 11). |

## 6. Known failure modes and pitfalls

sweep: 19 found

The count below is an audit inventory of potential silent loss, assumptions and defaults, including documented display conventions; it is not a count of confirmed incidents. Each row gives the code-pin location, symptom, detection status and follow-up. The sweep used the following exact searches, followed by reading all 436 package Python lines. [src/digitalmodel/reservoir/__init__.py:L1-L1] [src/digitalmodel/reservoir/volumetrics.py:L1-L64] [src/digitalmodel/reservoir/stratigraphic.py:L1-L371]

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache|default' src/digitalmodel/reservoir --glob '*.py'
rg -n 'except|continue|return|=[[:space:]]*0|1970|int\(|float\(|cache|density|cutoff|xlim|figsize|units?' src/digitalmodel/reservoir --glob '*.py'
```

Audit date for all rows: 2026-10-18. There are no explicit swallowing `except` blocks, error-path `return None`/empty-container returns, external-text `int()`/`float()` conversions or cache writes in these package files. [src/digitalmodel/reservoir/volumetrics.py:L1-L64] [src/digitalmodel/reservoir/stratigraphic.py:L1-L371]

| ID | Symptom / cause at pin | Detection check / disposition |
| --- | --- | --- |
| S01 | A well with logs but no matching TOP/BASE is silently skipped by `continue`. [src/digitalmodel/reservoir/stratigraphic.py:L299-L302] | no detection check exists; issue pending (3) |
| S02 | Logs and tops outside `wells_list` are discarded by the two `isin` filters; omitted input is not reported. [src/digitalmodel/reservoir/stratigraphic.py:L266-L278] | no detection check exists; issue pending (3) |
| S03 | `plot_facies_track(facies_colors=None)` silently selects the code 0–5 palette. [src/digitalmodel/reservoir/stratigraphic.py:L43-L50] [src/digitalmodel/reservoir/stratigraphic.py:L219-L223] | no detection check exists; issue pending (3) |
| S04 | `create_cross_section(facies_colors=None)` selects the same palette before calling the helper. [src/digitalmodel/reservoir/stratigraphic.py:L263-L264] [src/digitalmodel/reservoir/stratigraphic.py:L319-L319] | no detection check exists; issue pending (3) |
| S05 | GR shading defaults to cutoff 30 without a rock-specific criterion. [src/digitalmodel/reservoir/stratigraphic.py:L90-L114] | no detection check exists; issue pending (1) |
| S06 | Default GR display limits 0–75 can hide values outside the visible range. [src/digitalmodel/reservoir/stratigraphic.py:L95-L95] [src/digitalmodel/reservoir/stratigraphic.py:L120-L120] | no detection check exists; issue pending (3) |
| S07 | RT defaults to limits 1–1000 on a logarithmic axis without a positivity check. [src/digitalmodel/reservoir/stratigraphic.py:L124-L153] | no detection check exists; issue pending (3) |
| S08 | RHOB display limits default to 1.95–2.95. [src/digitalmodel/reservoir/stratigraphic.py:L159-L159] [src/digitalmodel/reservoir/stratigraphic.py:L181-L181] | no detection check exists; issue pending (1) |
| S09 | PHIN display limits default to reversed 0.45 to −0.15. [src/digitalmodel/reservoir/stratigraphic.py:L160-L160] [src/digitalmodel/reservoir/stratigraphic.py:L187-L187] | no detection check exists; issue pending (1) |
| S10 | Matrix density defaults to 2.71; an unsuitable density changes DPHI without a warning. [src/digitalmodel/reservoir/stratigraphic.py:L161-L175] | no detection check exists; issue pending (1) |
| S11 | Fluid density defaults to 1.0; equal supplied matrix/fluid densities also leave a zero denominator unguarded. [src/digitalmodel/reservoir/stratigraphic.py:L162-L175] | no detection check exists; issue pending (1) |
| S12 | Depth/TOP/BASE are subtracted with no unit or measured/vertical-depth declaration. [src/digitalmodel/reservoir/stratigraphic.py:L249-L253] [src/digitalmodel/reservoir/stratigraphic.py:L303-L309] | no detection check exists; issue pending (1) |
| S13 | OOIP assumes field units, uniform saturation, complete drainage and the fixed conversion coefficient. [src/digitalmodel/reservoir/volumetrics.py:L10-L26] | R-G2a detects arithmetic drift only; provenance issue pending (1) |
| S14 | OOIP accepts negative or nonfinite quantities and out-of-range fractions; zero Boi reaches division directly. [src/digitalmodel/reservoir/volumetrics.py:L29-L48] | no detection check exists; issue pending (1) |
| S15 | Recovery functions accept unbounded factors and decreasing factors can return negative uplift. [src/digitalmodel/reservoir/volumetrics.py:L52-L64] | R-G2b covers nominal arithmetic only; no invalid-input detection check exists; issue pending (1) |
| S16 | `figsize=(18,10)`, zero subplot spacing, color opacity 0.5/0.7 and top datum zero are display defaults; they are not confidence factors or reservoir properties. [src/digitalmodel/reservoir/stratigraphic.py:L107-L115] [src/digitalmodel/reservoir/stratigraphic.py:L190-L200] [src/digitalmodel/reservoir/stratigraphic.py:L244-L244] [src/digitalmodel/reservoir/stratigraphic.py:L280-L290] [src/digitalmodel/reservoir/stratigraphic.py:L355-L357] | R-G1b is smoke only; no appearance detection check exists; issue pending (3) |
| S17 | Raw facies values go to `imshow` with a sorted color list and no fixed class normalization; code/color identity is not explicitly preserved across subsets. [src/digitalmodel/reservoir/stratigraphic.py:L222-L231] | no detection check exists; issue pending (3) |
| S18 | RT gradient normalization uses each well's own min/max, so color is not a shared resistivity scale across wells. [src/digitalmodel/reservoir/stratigraphic.py:L136-L147] | no detection check exists; issue pending (3) |
| S19 | A requested well without any log rows has no observed group; an empty filtered dataset can return a figure with blank tracks rather than a rejected input. [src/digitalmodel/reservoir/stratigraphic.py:L70-L83] [src/digitalmodel/reservoir/stratigraphic.py:L267-L290] [src/digitalmodel/reservoir/stratigraphic.py:L299-L302] [src/digitalmodel/reservoir/stratigraphic.py:L355-L360] | no detection check exists; issue pending (3) |

Additional confirmed coverage and traceability gaps:

| Finding | Evidence / detection | Follow-up |
| --- | --- | --- |
| Legacy framework suppresses all 30 tests when absent exports fail the guarded import | Offline run reports 30 skips; initializer exports none of the requested classes. [tests/test_reservoir_framework.py:L22-L57] [src/digitalmodel/reservoir/__init__.py:L1-L1] | issue pending (4); no active test fails on this gap |
| Plotter and plotting tests cite #1633, whose current issue describes a different validation problem | File references are at the pin; issue #1633 was read directly and is not evidence of a reservoir incident. [src/digitalmodel/reservoir/stratigraphic.py:L1-L4] [tests/reservoir/test_stratigraphic.py:L1-L3] | issue pending (4); no detection check exists |
| Plotting tests replace Matplotlib with mocks | R-G1a/R-G1b cannot detect renderer errors, depth-axis inversion interactions, or sparse-well connector placement. [tests/reservoir/test_stratigraphic.py:L24-L51] [tests/reservoir/test_stratigraphic.py:L124-L136] [src/digitalmodel/reservoir/stratigraphic.py:L339-L357] | issue pending (3); no rendered-geometry detection check exists |

No incident issue was specified for this package. The read issue #842 supplies a volumetrics problem lead, not an independent validation result; it is not treated as a confirmed defect. [tests/reservoir/test_volumetrics.py:L46-L74]

## 7. Lessons log

Append-only record; shared-log publication remains open as item C06.

| Date | Record | Lesson / evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Keep formula regression distinct from independent validation; the test computes the same equation. [tests/reservoir/test_volumetrics.py:L19-L28] | Sections 3–5 distinguish the evidence levels | issue pending (2) |
| 2026-10-18 | Initial pack | A green run may include absent in-repository APIs hidden behind skips. [tests/test_reservoir_framework.py:L22-L57] [src/digitalmodel/reservoir/__init__.py:L1-L1] | Sections 4 and 6 expose the skipped framework | issue pending (4) |
| 2026-10-18 | Initial pack | Require well/top completeness before interpreting cross sections; a missing top triggers `continue`. [src/digitalmodel/reservoir/stratigraphic.py:L299-L302] | Sections 2 and 6 record silent omission | issue pending (3) |

## 8. Ownership

Prescribed clearance role: engineering calculations quality manager; clearance requires a provider independent of the producer (AQG r0, section 11, Sign-off and independence).

UNVERIFIED: a named deputy and a durable shared-log record have not been established from the reviewed repository evidence; the maintainers must assign them before clearance (issue pending (6)).

Prescribed producing role: calculation/documentation maintainer. Route method classification and gate disputes to the engineering calculations quality manager; unresolved cross-family matters go to the maintainers. No clearance or approval is recorded by this DRAFT pack (AQG r0, section 11, Sign-off and independence).

## 9. Dependencies and other implementations

| Dependency / boundary | Observed use | Evidence |
| --- | --- | --- |
| Python dataclasses | Frozen `VolumetricInputs`; arithmetic functions return scalars | [src/digitalmodel/reservoir/volumetrics.py:L24-L64] |
| NumPy, pandas, Matplotlib | Imports and DataFrame/plot operations in the plotter; no solver launch in the covered module | [src/digitalmodel/reservoir/stratigraphic.py:L33-L36] [src/digitalmodel/reservoir/stratigraphic.py:L266-L360] |
| Caller-owned DataFrames | Log columns and top/base records supplied at API boundary; no file-reader API | [src/digitalmodel/reservoir/stratigraphic.py:L239-L260] |
| Offline consumers | Package, plot and volumetric test suites import the covered module | [tests/reservoir/test_reservoir.py:L21-L30] [tests/reservoir/test_stratigraphic.py:L96-L108] [tests/reservoir/test_volumetrics.py:L8-L13] |

### Implementation-map inventory

Map audit record: ref `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Every family implementation row was filtered on `path.startswith("src/digitalmodel/reservoir/")`; **0** families have an in-package row among **13** mapped families. The required inventory is therefore empty. Map citations in this section resolve at that map commit. [docs/registry/implementation-map.yaml:L1-L4] [docs/registry/implementation-map.yaml:L346-L26502]

Counted families: **0**. Ranking by in-package row count, with file-order ties, produces an empty list; the case-sensitive Python `re` grep is **NOT_APPLICABLE**, because there is no mapped family with an in-package implementation row. No path-matching signal needs exclusion, and no unlisted second implementation is counted. This is an inventory result, not a completeness claim for the map. [docs/registry/implementation-map.yaml:L346-L26502]

Map follow-up: add volumetric OOIP/recovery, density porosity and stratigraphic plotting/depth-alignment families, including the covered definitions below; these are the section-3 method families missing from the map (issue pending (5)). [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/reservoir/stratigraphic.py:L156-L231] [src/digitalmodel/reservoir/stratigraphic.py:L239-L360]

### Repository-wide further search

The following search was run across source, scripts and examples, then checked against the whole tracked repository, including tests and docs; findings below are map follow-up, without changing the empty counted-family result.

```bash
rg -n 'ooip|7758|DPHI|def create_cross_section|def plot_.*track|stratigraphic' src scripts examples --glob '*.py'
git grep -n -E 'ooip|7758|DPHI|create_cross_section|stratigraphic' <code-pin> -- '*.py'
```

| Family | Covered definitions | Further implementations / classification | Follow-up |
| --- | --- | --- | --- |
| OOIP and recovery | `ooip_stb`, `recoverable_stb`, `incremental_recovery_stb` | `ReservoirProperties.calculate_ooip` computes its own volume/saturation/conversion formula; it is an outside-package map gap. [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/marine_ops/reservoir/properties.py:L255-L270] | issue pending (5) |
| Density porosity and track plotting | `plot_gr_track`, `plot_rt_track`, `plot_rhob_nphi_track`, `plot_facies_track` | `_create_legacy_style_plot` independently computes DPHI and plots density/neutron crossover; outside-package map gap. [src/digitalmodel/reservoir/stratigraphic.py:L90-L231] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L166-L175] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L303-L330] | issue pending (5) |
| Depth alignment and cross-section construction | `create_cross_section`; `_validate_inputs` is a validation helper | `StratigraphicAnalysis.create_flattened_section` has its own datum selection/alignment; separate method and outside-package map gap, not a substitute reference for this plotter. [src/digitalmodel/reservoir/stratigraphic.py:L60-L83] [src/digitalmodel/reservoir/stratigraphic.py:L239-L360] [src/digitalmodel/marine_ops/reservoir/analysis.py:L452-L490] | issue pending (5) |


Search classification also excludes downstream OOIP delegates and exports, example callers and assertion-only test arithmetic from independent implementations. Material-balance OOIP estimation uses withdrawal/expansion rather than the volumetric product and is a different method; its depletion calculation is separate from fixed recovery-factor multiplication. These distinctions are map follow-up, not a claim of equivalence. [src/digitalmodel/marine_ops/reservoir/analysis.py:L1150-L1155] [src/digitalmodel/marine_ops/reservoir/modeling.py:L187-L209] [src/digitalmodel/marine_ops/reservoir/modeling.py:L390-L414] [examples/domains/reservoir/reservoir_analysis_examples.py:L89-L98] [tests/reservoir/test_volumetrics.py:L19-L43]

The package has no nested Python package to exclude; its initializer contains only its docstring, and all covered functions are inventoried in sections 2–3. [src/digitalmodel/reservoir/__init__.py:L1-L1] [src/digitalmodel/reservoir/volumetrics.py:L29-L64] [src/digitalmodel/reservoir/stratigraphic.py:L60-L360]

## 10. Coverage and open items

Code read: `src/digitalmodel/reservoir/__init__.py`, `volumetrics.py`, `stratigraphic.py`; test files read: `tests/reservoir/__init__.py`, `test_reservoir.py`, `test_stratigraphic.py`, `test_volumetrics.py`, and `tests/test_reservoir_framework.py`. The source inventory and offline collection are recorded in sections 1 and 4. [src/digitalmodel/reservoir/__init__.py:L1-L1] [src/digitalmodel/reservoir/volumetrics.py:L1-L64] [src/digitalmodel/reservoir/stratigraphic.py:L1-L371] [tests/reservoir/test_reservoir.py:L1-L76] [tests/reservoir/test_stratigraphic.py:L1-L207] [tests/reservoir/test_volumetrics.py:L1-L74] [tests/test_reservoir_framework.py:L1-L911]

Documentation discovery used `rg -l 'digitalmodel\.reservoir|src/digitalmodel/reservoir|reservoir\.volumetrics|stratigraphic\.py|tests/reservoir/' docs --glob '*.md' --glob '*README*'` and a broader reservoir/stratigraph/volumetric search. Module-specific documents read: `docs/maps/digitalmodel-operator-map.md`, `docs/collide_pe/README.md`, `docs/collide_pe/HANDOFF.md`; package-domain documents read: `docs/domains/reservoir/stratigraphy.md`, `docs/domains/reservoir/reservoir.md`, `docs/domains/reservoir/petroleum_economics.md`, plus **2 discovery files with identifying names**, whose paths are withheld for codenaming (issue pending (6)). The operator map routes this package; the stratigraphy note describes input DataFrames; the volumetric README accuracy wording is a **README claim, unverified**, not a validation source. [docs/maps/digitalmodel-operator-map.md:L61-L61] [docs/domains/reservoir/stratigraphy.md:L1-L5] [docs/collide_pe/README.md:L37-L45]

Search hits about reservoir pressure in other engineering domains, or the separate `marine_ops/reservoir` package, are discovery context rather than evidence that this package implements their methods. The separate OOIP implementation is retained in the map follow-up. [src/digitalmodel/marine_ops/reservoir/properties.py:L255-L270]

| Item | Open coverage / required action | Accountable role | Tracking |
| --- | --- | --- | --- |
| C01 | UNVERIFIED plotting units/depth convention and independent constant/default provenance (both section-2/3 statements); document caller bounds and invalid-input checks, including saturation, finite values and equal densities | Calculation maintainer | issue pending (1) |
| C02 | No independently validated worked example; qualify a public reference and test-node tolerance before describing OOIP or plotting as validated; problem #842 remains a lead | Calculation quality manager | issue pending (2); #842 |
| C03 | Silent omissions, plot defaults, sparse-well connectors, per-well RT scale and facies color normalization; add actual rendering and referential-integrity checks | Calculation maintainer | issue pending (3) |
| C04 | Thirty legacy framework skips and unrelated #1633 references; choose the correct API and repair test/incident traceability | Test maintainer | issue pending (4) |
| C05 | Add all section-3 method families to the map, including the outside-package implementations listed in section 9; no counted-family grep applies until there are in-package rows | Map maintainer | issue pending (5) |
| C06 | UNVERIFIED deputy/shared-log assignment; neutral indexing of the two discovery files, report appendix ownership and durable evidence publication remain open; no private values or sidecar entry are needed for the synthetic cases retained here | Documentation maintainer | issue pending (6) |

Acceptance record: A1–A6 and A8 are producer self-checks; A7 uses numbered issue drafts pending creation of permanent issue numbers. Independent clearance and any report-signature record remain outstanding (AQG r0, section 11, Sign-off and independence).
