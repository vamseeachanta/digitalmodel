# Wave-spectrum knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/wave_spectrum |
| Repository | digitalmodel |
| Package | src/digitalmodel/hydrodynamics/wave_spectrum |
| Pack revision | 0 |
| Code revision | b47e42fbe7208c69b28dd436aa91fb129bc385f7 |
| Analysis family | Engineering calculations (EC) |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The engine routes both `wave_spectrum` and `wave_spectra` to this package's `router`, which delegates JONSWAP and Pierson–Moskowitz generation to the sibling `WaveSpectra` class and emits a CSV plus a configuration summary. `[src/digitalmodel/engine.py:L493-L498]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L69]`

The routing registry gives the hydrodynamics domain maturity `production`; this is domain metadata, rather than independent qualification of this router. [docs/registry/module-routing.yaml:L181-L186]

The router accepts only the two named spectra; directional spreading, response calculations, time histories, geometry and solver execution have no branch in its dispatch. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L44]`

Scope is the two Python files in this package; the numerical sibling and other spectrum implementations remain dependencies, with their own applicability limits. `[src/digitalmodel/hydrodynamics/wave_spectrum/__init__.py:L1-L2]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L5-L10]`

## 2. Inputs and outputs

Input settings use `wave_spectra` when that key is present or the basename is `wave_spectra`; otherwise they use `wave_spectrum`. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L72-L75]`

| Input | Unit | Accepted range / default | Source and citation |
| --- | --- | --- | --- |
| `spectrum`, `spectrum_type`, `type` | text | First truthy key; case and hyphens normalized; aliases `pm`, `p_m`, `pierson_moskowitz`, `piersonmoskowitz`, `jonswap`; required | Settings; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L78-L96]` |
| `Hs` | m | Finite and greater than zero; required | Settings and delegate's unit contract; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L20-L21]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L153-L164]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L62-L71]` |
| `Tp` | s | Finite and greater than zero; required | Same validation and delegate contract; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L20-L21]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L153-L164]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L62-L71]` |
| `gamma` | dimensionless | Finite and greater than zero; default 3.3; validated even for Pierson–Moskowitz | Settings; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L22-L42]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L153-L164]` |
| `frequency_grid.start`, `.stop` | rad/s or Hz | Finite positive endpoints, stop greater than start; required | Settings; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L99-L120]` |
| `frequency_grid.num` | points | Converted by `int`; resulting count at least 2; missing count becomes zero and raises | Settings; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L104-L110]` |
| `frequency_grid.units` | text | Default `rad_s`; radian aliases and Hz aliases accepted; Hz endpoints multiplied by `2π` | Settings; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L112-L120]` |
| `output_dir` | path | Default `results`; relative to configuration directory, configuration-file parent, then current directory | Settings/configuration; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L123-L136]` |
| Configuration file / basename | text | Output stem uses input-file stem, otherwise basename, otherwise `wave_spectrum` | Configuration; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L139-L142]` |

| Output | Unit / representation | Behaviour and citation |
| --- | --- | --- |
| CSV `frequency`, `S` | rad/s, m²·s | Zipped arrays cast to floats; header and rows written to `<stem>_spectrum.csv`; the writer overwrites an existing file. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L46-L51]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L123-L128]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L70-L71]` |
| `Hs_input`, `Hs_check`, `Tp`, `gamma` | m, m, s, dimensionless | Summary replaces the input settings; gamma becomes `None` for Pierson–Moskowitz. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L57-L68]` |
| `m0`, `peak_frequency`, `frequency_units`, `points` | m², rad/s, text, points | Zeroth moment, discrete maximum's frequency, `rad_s`, row count. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L53-L68]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L266-L286]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L330-L346]` |
| `spectrum_csv` | path string | Relative to the calculated repository root when possible; absolute path on `ValueError`. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L13-L13]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L181-L186]` |

The CSV is scalar frequency-domain data; it contains no coordinate frame, origin, direction, sign or phase field, and no explicit unit column for `S`. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L46-L68]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]`

| Public symbol | Source | Behaviour |
| --- | --- | --- |
| `router` | `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L69]` | Generate the selected spectrum through the sibling, write CSV data and replace the settings with a summary. |

## 3. Method and governing standards

| Method | Implementation and reference status |
| --- | --- |
| Spectrum shape | `router` calls `WaveSpectra.jonswap` or `WaveSpectra.pierson_moskowitz`; the formula implementations reside outside this package. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L42]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L48-L139]` |
| Finite-grid energy normalization | Both delegated spectra call `_normalize_to_significant_height`, which rescales integrated energy to `Hs²/16` and returns zeros for nonpositive target or integrated energy. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L98-L98]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L139-L139]` |
| Summary calculation | `m0 = spectral_moment(..., n=0)` and `Hs_check = 4√m0`; the peak comes from the discrete maximum, without interpolation. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L53-L65]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L330-L346]` |
| Integration | `spectral_moment` integrates `ωⁿ S(ω)` by NumPy trapezoidal integration; the import chooses the available NumPy spelling. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L11-L11]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L266-L286]` |
| Provenance output | The router's CSV and summary contain no `Citation` sidecar. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L46-L69]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]` |

UNVERIFIED: the delegated docstrings associate these shapes with DNV-RP-C205, *Environmental conditions and environmental loads*, but do not establish an edition or clause title; clause text not checked against the licensed copy, and standards compliance is not established here (#1798). `[src/digitalmodel/hydrodynamics/wave_spectra.py:L24-L25]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L73-L75]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L123-L125]`

## 4. Validated worked examples and regression examples

Results below record offline execution on 2026-11-07 against the code revision in the header; the two validated cases check the numerical dependency, while the three router examples remain regression checks because the generator normalizes energy to the supplied height. `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]`

| Case / classification | Input and independent basis | Expected value / criterion | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 constant integral; validated dependency check | Synthetic nonuniform grid; constant density 2 m²·s on 1–4 rad/s; closed form integral `2ω` | `m0 = 6 m²`; scratch hand check absolute error at most `1e-12 m²`; shipped test uses `pytest.approx` | `tests/unit/hydrodynamics/test_wave_spectra_extended.py::TestSpectralMoment::test_moment_without_legacy_numpy_in_fresh_process[constant-integrand]` | PASS; `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]` `[tests/unit/hydrodynamics/_wave_spectra_numpy_fixture.py:L31-L36]` |
| V2 affine integral; validated dependency check | Same synthetic grid and density, first moment; closed form `ω²` | `m1 = 15 m²/s`; scratch hand check absolute error at most `1e-12 m²/s`; shipped test uses `pytest.approx` | `tests/unit/hydrodynamics/test_wave_spectra_extended.py::TestSpectralMoment::test_moment_without_legacy_numpy_in_fresh_process[affine-integrand]` | PASS; `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]` `[tests/unit/hydrodynamics/_wave_spectra_numpy_fixture.py:L31-L36]` |
| R1 JONSWAP; regression | `examples/workflows/wave-spectrum/input.yml` | Hs 3.0 m within 5.0%; peak `2π/Tp` within 0.02 rad/s; 181 rows; no negative density | `tests/workflows/test_durable_workflows.py::test_workflow_registry[wave-spectrum]` | PASS; [examples/workflows/wave-spectrum/input.yml:L1-L15] `[tests/workflows/test_durable_workflows.py:L565-L580]` |
| R2 Pierson–Moskowitz; regression | `examples/workflows/wave-spectra/input.yml` | Hs 2.5 m and m0 `2.5²/16 m²` within 5.0%; peak within 0.02 rad/s; 231 rows; no negative density | `tests/workflows/test_durable_workflows.py::test_workflow_registry[wave-spectra]` | PASS; [examples/workflows/wave-spectra/input.yml:L1-L19] `[tests/workflows/hydrodynamics_assertions.py:L51-L66]` |
| R3 engine Pierson–Moskowitz; regression | YAML constructed by test: Hs 2.5 m, Tp 8.0 s, grid 0.2–2.5 rad/s, 231 points | Hs within 5.0%; peak within 0.02 rad/s; CSV maximum frequency equals summary | `tests/workflows/test_durable_workflows.py::test_wave_spectrum_pierson_moskowitz_engine_textbook_check` | PASS; despite its name, no independently cited full-shape expected result; `[tests/workflows/test_durable_workflows.py:L1846-L1883]` |

Reproduction: run `PYTHONPATH=src uv run --no-sources --extra test python -m pytest` with the three router node IDs above and `tests/unit/hydrodynamics/test_wave_spectra_extended.py`, followed by `-q -rA -p no:cacheprovider --basetemp=<scratch-directory>`; use a temporary repository copy because the durable tests write into their example folders. `[tests/workflows/test_durable_workflows.py:L54-L100]` `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]`

Execution record: BASE and AFTER each passed 64 module/dependency tests, 11 supplemental `TestWaveSpectra` tests and 590 contract/routing tests (665 total per phase), with 0 failed, 0 skipped and 0 NOT_RUN; both phases used the code revision in the header and the uncommitted documentation tree for AFTER.

## 5. QA gates and executable checks

Gate applicability follows AQG r0, section 10, row “Python calculations”, and section 9 for G5; the following numeric criteria are pack checks rather than claims of standards qualification. (AQG r0, sections 9–11.)

| Gate | Applicability | Module-specific check IDs |
| --- | --- | --- |
| G0 | Applicable: configuration, units and input fingerprints | C01, C02 |
| G1 | Applicable: Python import, tests and CSV loading | C03, C04 |
| G2 | Applicable: independent integrals and regression quantities | C05, C06 |
| G3 | NOT_APPLICABLE: router has no time-domain simulation branch | `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L69]` |
| G4 | NOT_APPLICABLE: router produces no mesh or diffraction solution | `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L69]` |
| G5 | Applicable: retain check evidence, input fingerprints and limits | C07 |

| ID / gate | Executable procedure | Numeric pass criterion and unit | Evidence / last self-check |
| --- | --- | --- | --- |
| C01 / G0 | Call `_positive_float` and `_required_positive_float` on zero, negative, NaN, infinity, comma-formatted text and missing values; call `_frequency_grid` on missing/reversed endpoints, one point and unsupported units | 0 unexpected acceptances in 18 cases; values otherwise finite and greater than 0 in their declared unit | PASS; error guards exist, but no shipped router-negative test exists; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L99-L118]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L153-L178]` |
| C02 / G0 | Compare `_frequency_grid` for endpoints 0.2 and 2.0 Hz with explicitly converted rad/s endpoints, 11 points | Endpoint difference 0 rad/s; point-count difference 0 points | PASS; conversion uses `2π`; this checks conversion consistency, not intended units of unitless input; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L104-L120]` |
| C03 / G1 | Run section-4 reproduction command, including all 61 dependency tests | 64 passed tests, 0 failed tests, 0 skipped tests | PASS at BASE and AFTER; shipped coverage includes all three router nodes and dependency test file; `[tests/workflows/test_durable_workflows.py:L565-L580]` `[tests/workflows/test_durable_workflows.py:L1846-L1883]` `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L31-L594]` |
| C04 / G1 | Call `router` with the R3 parameters and a scratch output directory; load its CSV using NumPy `genfromtxt(..., delimiter=',', names=True)` | 231 rows; 0 negative or nonfinite density values; 2 named columns | PASS; CSV fields and summary point count are defined by the router; `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L46-L68]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]` |
| C05 / G2 | Call `WaveSpectra.spectral_moment` for V1 and V2 on the fixture's nonuniform grid and compare with their hand integrals | Absolute error at most `1e-12 m²` for m0 and `1e-12 m²/s` for m1 | PASS; both observed errors zero; `[tests/unit/hydrodynamics/_wave_spectra_numpy_fixture.py:L31-L36]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L266-L286]` |
| C06 / G2 | Check R1–R3 test assertions; additionally compute R3 `abs(Hs_check/Hs_input - 1)` and `abs(peak_frequency - 2π/Tp)` | Hs relative error at most 5.0%; peak-frequency absolute error at most 0.02 rad/s | PASS; finite-grid normalization makes the height test a regression criterion; `[tests/workflows/test_durable_workflows.py:L575-L580]` `[tests/workflows/test_durable_workflows.py:L1875-L1883]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]` |
| C07 / G5 | Retain machine-readable records for C01–C06, the code revision, run logs and SHA-256 fingerprints of workflow source and both durable inputs; recompute digests before reuse | 0 missing check records; 0 fingerprint mismatches; 64 hexadecimal characters per digest | PASS; evidence retained separately; inputs and workflow routes are pinned by [docs/registry/workflows.yaml:L346-L355] [docs/registry/workflows.yaml:L1124-L1134] |

The existing engine example checks no license-dependent solver result; the routed calculation uses Python and the sibling generator. `[src/digitalmodel/engine.py:L493-L498]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L69]`

## 6. Known failure modes and silent-failure sweep

sweep: 16 found

The table records every relevant default, external conversion, unit assumption and swallowed-error candidate found by the following commands; normal conventions and guarded errors are distinguished from confirmed defects. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L186]`

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|units|cache|\.get\(' src/digitalmodel/hydrodynamics/wave_spectrum --glob '*.py'
rg -n 'zip\(|open\(|Hs|Tp|gamma|default|encoding|write' src/digitalmodel/hydrodynamics/wave_spectrum --glob '*.py'
```

| ID | Symptom / cause at the pin | Detection and disposition | Date |
| --- | --- | --- | --- |
| S01 | Falsey settings become `{}`; missing spectrum subsequently raises rather than returning an empty result. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L17-L20]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L78-L85]` | No detection check exists for falsey nonmapping settings; input coverage issue pending (1) | 2026-11-07 |
| S02 | Gamma defaults to 3.3 and is validated even when the Pierson–Moskowitz branch does not use it. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L22-L42]` | C01 checks finite positivity; no detection check exists for an unintended default; issue pending (1) | 2026-11-07 |
| S03 | `zip` silently shortens CSV rows if delegated frequency and density lengths differ. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L46-L49]` | C04 detects wrong row count for its fixture; no detection check exists for unequal delegated arrays; issue pending (1) | 2026-11-07 |
| S04 | Summary gamma is deliberately `None` for Pierson–Moskowitz; consumers cannot assume all summary values are numeric. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L57-L68]` | No detection check exists for downstream numeric-only readers; issue pending (1) | 2026-11-07 |
| S05 | Presence of the plural settings key takes precedence over the singular key, including when both are supplied. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L72-L75]` | No detection check exists for ambiguous simultaneous settings; issue pending (1) | 2026-11-07 |
| S06 | Truthy alias fallback can mask an empty higher-priority spectrum field. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L78-L96]` | No detection check exists for conflicting aliases; issue pending (1) | 2026-11-07 |
| S07 | Confirmed: numeric `num=10.9` becomes 10 without an error; missing count defaults to zero and is rejected. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L106-L108]` | C01 checks missing/too-small counts; fractional-count probe reproduced truncation; no detection check exists for strict integer intent; issue pending (1) | 2026-11-07 |
| S08 | Missing units default to rad/s; unitless Hz input would be interpreted incorrectly. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L112-L118]` | C02 checks explicit conversion; no detection check exists for the user's omitted unit intent; issue pending (1) | 2026-11-07 |
| S09 | Hz endpoints are converted to rad/s; CSV density remains per angular-frequency contract. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L114-L120]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L70-L71]` | C02; C04; convention rather than an observed failure | 2026-11-07 |
| S10 | Output directory defaults to `results`; it is resolved relative to the configuration context. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L123-L128]` | No detection check exists for an unintended output directory; issue pending (1) | 2026-11-07 |
| S11 | Missing configuration context falls back to the current directory. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L131-L136]` | No detection check exists for unintended working-directory output; issue pending (1) | 2026-11-07 |
| S12 | Missing input file uses basename, then a fixed default stem; repeated runs can target the same filename. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L139-L142]` | No detection check exists for filename collisions; issue pending (1) | 2026-11-07 |
| S13 | Hs/Tp/gamma text passes through `float`; thousands and decimal commas raise instead of being interpreted; nonfinite and nonpositive values are rejected. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L153-L164]` | C01; guarded error, no silent separator normalization | 2026-11-07 |
| S14 | Grid endpoint text passes through `float` with the same finite-positive guards and comma rejection. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L167-L178]` | C01; guarded error | 2026-11-07 |
| S15 | `_display_path` catches `ValueError` and returns an absolute resolved path; the fallback was reproduced for scratch output. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L181-L186]` | No detection check exists for portable output manifests; issue pending (1) | 2026-11-07 |
| S16 | Height/period units depend on the delegate's m/s convention; CSV has no density-unit field. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L20-L21]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L62-L71]` | No detection check exists for upstream imperial inputs; issue pending (1) | 2026-11-07 |

Additional output hazard: the CSV writer opens with `w`, without an existence guard, explicit encoding or atomic replacement; no detection check exists for interrupted writes or unintended overwrites, issue pending (1). `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L145-L150]`

Dependency pitfall: a zero-energy delegated spectrum can become an all-zero array; recovery of Hs from a normalized spectrum does not independently establish the shape. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]`

The map associates #2543 with the two in-package definitions as `not_checked`; its reported normalization defect concerns other implementations, so it is not asserted as a confirmed router defect here; C06 detects energy disagreement but not all shape errors. [docs/registry/implementation-map.yaml:L14071-L14095] `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]`

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | A height recovered after finite-grid normalization is a regression check; use closed-form integrals to check the integration dependency independently. | `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]` `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]` | Separate dependency validation from router regression in section 4 | Not assigned; issue pending (3) |

## 8. Ownership and review assignment

Review assignment for this draft: engineering calculations quality manager; deputy role: alternate engineering calculations quality manager; producing role: calculation maintainer. Review independence and gate sign-off follow AQG r0, section 11. (AQG r0, section 11.)

Escalation policy: unresolved input intent, reference provenance, changed tolerances or implementation classifications go to the engineering calculations quality manager; cross-family scope goes to the maintainers. Must not merge before the second-provider review. (AQG r0, sections 10–11.)

## 9. Dependencies and implementation inventory

The numerical dependency is `digitalmodel.hydrodynamics.wave_spectra.WaveSpectra`; standard-library dependencies provide CSV, mathematics, paths and typing, while the engine and two workflow registry entries are callers. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L5-L10]` `[src/digitalmodel/engine.py:L493-L498]` [docs/registry/workflows.yaml:L346-L353] [docs/registry/workflows.yaml:L1124-L1132]

The NumPy-backed dependency implements spectrum generation and moments; no solver version or private reference dataset is a router parameter. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L7-L11]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L69]`

Inventory metadata: map source `codex/implmap-work` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the table records its 141 wave-spectra path/symbol pairs, which also occur in the map at the code pin, using citations to that pinned copy; coordinate, digest and defect metadata can differ between copies. [docs/registry/implementation-map.yaml:L1-L4] [docs/registry/implementation-map.yaml:L13567-L15368]

Only `wave-spectra` has rows inside this package: 2 rows, so it is the only counted family; the map labels `router` and `_spectrum_name` independent, while inspection shows spectrum delegation and alias handling respectively. [docs/registry/implementation-map.yaml:L14071-L14096] `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L25-L42]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L78-L96]`

Search record: Python `re`, case-sensitive, on both package `.py` files at the code pin; omit the vocabulary signal because its pattern matches `hydrodynamics/wave_spectrum` case-insensitively; retain algorithm and constants signals exactly as stored, without rewriting their escapes. The remaining 13 hit lines belong to `router` or `_spectrum_name`: 0 further second implementations; A8 passes. [docs/registry/implementation-map.yaml:L13567-L13572] `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L16-L96]` `[src/digitalmodel/hydrodynamics/wave_spectrum/__init__.py:L1-L2]`

Whole-repository search record: the same retained patterns searched tracked Python files, including `scripts/`; 451 files and 1,846 hit lines, including 31 script files. These broad candidates are discovery evidence rather than extra package misses; the inventory below includes script implementations. [docs/registry/implementation-map.yaml:L13567-L15368]

Map follow-up: classify router/delegation and alias helpers, and review unit-conversion, numeric-validation and output helpers as dependencies rather than additional shape implementations; these helper families have no in-package implementation rows under separate family IDs, issue pending (3). `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L72-L186]` [docs/registry/implementation-map.yaml:L14071-L14096]

The following table preserves map membership and relation labels; a relation is an inventory classification, not a new qualification of every listed method. [docs/registry/implementation-map.yaml:L13567-L15368]

| Path | Symbol | Map relation | Inventory evidence |
| --- | --- | --- | --- |
| `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` | independent | [docs/registry/implementation-map.yaml:L13574-L13581] |
| `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` | independent | [docs/registry/implementation-map.yaml:L13586-L13593] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` | example | [docs/registry/implementation-map.yaml:L13598-L13605] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `generate_visualizations` | example | [docs/registry/implementation-map.yaml:L13610-L13617] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | example | [docs/registry/implementation-map.yaml:L13622-L13629] |
| `examples/prototypes/orcaflex-agent/generate_test.py` | `main` | example | [docs/registry/implementation-map.yaml:L13637-L13644] |
| `scripts/_seed_eq_mock.py` | `mock_time_series` | independent | [docs/registry/implementation-map.yaml:L13649-L13656] |
| `scripts/benchmark/extract_pdf_plots.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13661-L13668] |
| `scripts/benchmark/validate_owd_vs_spec.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13672-L13679] |
| `scripts/extract_property_inventory.py` | `build_dormancy_rules` | independent | [docs/registry/implementation-map.yaml:L13683-L13690] |
| `scripts/generate_env_files.py` | `create_wave_file` | independent | [docs/registry/implementation-map.yaml:L13695-L13702] |
| `scripts/generate_env_files.py` | `main` | independent | [docs/registry/implementation-map.yaml:L13707-L13714] |
| `scripts/ingest_orcina_help.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13721-L13728] |
| `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13732-L13739] |
| `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13743-L13750] |
| `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13754-L13761] |
| `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13765-L13772] |
| `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13776-L13783] |
| `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13787-L13794] |
| `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13798-L13805] |
| `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13809-L13816] |
| `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13820-L13827] |
| `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13831-L13838] |
| `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `<module>` | independent | [docs/registry/implementation-map.yaml:L13842-L13849] |
| `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` | independent | [docs/registry/implementation-map.yaml:L13853-L13860] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L13865-L13872] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent | [docs/registry/implementation-map.yaml:L13877-L13884] |
| `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` | independent | [docs/registry/implementation-map.yaml:L13889-L13896] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` | independent | [docs/registry/implementation-map.yaml:L13901-L13908] |
| `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` | independent | [docs/registry/implementation-map.yaml:L13913-L13920] |
| `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` | independent | [docs/registry/implementation-map.yaml:L13925-L13932] |
| `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` | independent | [docs/registry/implementation-map.yaml:L13939-L13946] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | independent | [docs/registry/implementation-map.yaml:L13951-L13958] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | independent | [docs/registry/implementation-map.yaml:L13963-L13970] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.pierson_moskowitz` | independent | [docs/registry/implementation-map.yaml:L13979-L13986] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.bretschneider` | independent | [docs/registry/implementation-map.yaml:L13992-L13999] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.issc` | independent | [docs/registry/implementation-map.yaml:L14005-L14012] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.generate_spectrum` | wrapper | [docs/registry/implementation-map.yaml:L14018-L14025] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.significant_height_from_spectrum` | independent | [docs/registry/implementation-map.yaml:L14035-L14042] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.zero_crossing_period_from_spectrum` | independent | [docs/registry/implementation-map.yaml:L14047-L14054] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectrum_statistics` | independent | [docs/registry/implementation-map.yaml:L14059-L14066] |
| `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L14071-L14078] |
| `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `_spectrum_name` | independent | [docs/registry/implementation-map.yaml:L14085-L14092] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | independent | [docs/registry/implementation-map.yaml:L14097-L14104] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | independent | [docs/registry/implementation-map.yaml:L14109-L14116] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` | independent | [docs/registry/implementation-map.yaml:L14121-L14128] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | independent | [docs/registry/implementation-map.yaml:L14133-L14140] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_motion` | independent | [docs/registry/implementation-map.yaml:L14147-L14154] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_significant_velocity` | independent | [docs/registry/implementation-map.yaml:L14161-L14168] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14174-L14181] |
| `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` | copy | [docs/registry/implementation-map.yaml:L14188-L14195] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L14200-L14207] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum.run_spectrum` | independent | [docs/registry/implementation-map.yaml:L14212-L14219] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` | independent | [docs/registry/implementation-map.yaml:L14224-L14231] |
| `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` | independent | [docs/registry/implementation-map.yaml:L14238-L14245] |
| `src/digitalmodel/marine_ops/operation_envelope.py` | `_significant_response` | independent | [docs/registry/implementation-map.yaml:L14252-L14259] |
| `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` | independent | [docs/registry/implementation-map.yaml:L14264-L14271] |
| `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14278-L14285] |
| `src/digitalmodel/motion_forecast/wave_source.py` | `synthesize_forecast` | independent | [docs/registry/implementation-map.yaml:L14292-L14299] |
| `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` | independent | [docs/registry/implementation-map.yaml:L14305-L14312] |
| `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.spectral_density` | independent | [docs/registry/implementation-map.yaml:L14319-L14326] |
| `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.to_orcaflex_dict` | independent | [docs/registry/implementation-map.yaml:L14333-L14340] |
| `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` | independent | [docs/registry/implementation-map.yaml:L14345-L14352] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` | independent | [docs/registry/implementation-map.yaml:L14357-L14364] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `jonswap` | independent | [docs/registry/implementation-map.yaml:L14369-L14376] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `bretschneider` | wrapper | [docs/registry/implementation-map.yaml:L14385-L14392] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `issc_spectrum` | independent | [docs/registry/implementation-map.yaml:L14398-L14405] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `ochi_hubble._component` | independent | [docs/registry/implementation-map.yaml:L14410-L14417] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `torsethaugen` | independent | [docs/registry/implementation-map.yaml:L14422-L14429] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `generate_spectrum` | independent | [docs/registry/implementation-map.yaml:L14436-L14443] |
| `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` | independent | [docs/registry/implementation-map.yaml:L14454-L14461] |
| `src/digitalmodel/parametric/generate.py` | `_esp_pump_utilisation` | independent | [docs/registry/implementation-map.yaml:L14466-L14473] |
| `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` | independent | [docs/registry/implementation-map.yaml:L14478-L14485] |
| `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` | independent | [docs/registry/implementation-map.yaml:L14490-L14497] |
| `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` | wrapper | [docs/registry/implementation-map.yaml:L14502-L14509] |
| `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.getJONSWAPGamma` | independent | [docs/registry/implementation-map.yaml:L14516-L14523] |
| `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.RayleighDampingCoefficients` | copy | [docs/registry/implementation-map.yaml:L14528-L14535] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` | independent | [docs/registry/implementation-map.yaml:L14540-L14547] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._order_wave_train` | independent | [docs/registry/implementation-map.yaml:L14554-L14561] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` | independent | [docs/registry/implementation-map.yaml:L14568-L14575] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` | copy | [docs/registry/implementation-map.yaml:L14582-L14589] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.RayleighDampingCoefficients` | copy | [docs/registry/implementation-map.yaml:L14594-L14601] |
| `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` | independent | [docs/registry/implementation-map.yaml:L14606-L14613] |
| `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent | [docs/registry/implementation-map.yaml:L14618-L14625] |
| `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_frequency_validation` | independent | [docs/registry/implementation-map.yaml:L14632-L14639] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14644-L14651] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14656-L14663] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14668-L14675] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L14680-L14687] |
| `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` | independent | [docs/registry/implementation-map.yaml:L14692-L14699] |
| `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func._jonswap_wrapper` | independent | [docs/registry/implementation-map.yaml:L14704-L14711] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | independent | [docs/registry/implementation-map.yaml:L14716-L14723] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `wave` | independent | [docs/registry/implementation-map.yaml:L14728-L14735] |
| `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` | independent | [docs/registry/implementation-map.yaml:L14740-L14747] |
| `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_spectrum` | independent | [docs/registry/implementation-map.yaml:L14752-L14759] |
| `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `compute_wave_velocity` | independent | [docs/registry/implementation-map.yaml:L14768-L14775] |
| `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` | independent | [docs/registry/implementation-map.yaml:L14783-L14790] |
| `src/digitalmodel/workflows/installation_full_report.py` | `_design` | independent | [docs/registry/implementation-map.yaml:L14795-L14802] |
| `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` | independent | [docs/registry/implementation-map.yaml:L14807-L14814] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_configure` | independent | [docs/registry/implementation-map.yaml:L14819-L14826] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_verify` | independent | [docs/registry/implementation-map.yaml:L14831-L14838] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | independent | [docs/registry/implementation-map.yaml:L14843-L14850] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` | independent | [docs/registry/implementation-map.yaml:L14855-L14862] |
| `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14869-L14876] |
| `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14883-L14890] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L14897-L14904] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` | test_helper | [docs/registry/implementation-map.yaml:L14909-L14916] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` | test_helper | [docs/registry/implementation-map.yaml:L14921-L14928] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` | test_helper | [docs/registry/implementation-map.yaml:L14933-L14940] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_wave_energy_distribution` | test_helper | [docs/registry/implementation-map.yaml:L14945-L14952] |
| `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` | test_helper | [docs/registry/implementation-map.yaml:L14957-L14964] |
| `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_pierson_moskowitz_hs_recovery` | test_helper | [docs/registry/implementation-map.yaml:L14969-L14976] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper | [docs/registry/implementation-map.yaml:L14981-L14988] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper | [docs/registry/implementation-map.yaml:L14993-L15000] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper | [docs/registry/implementation-map.yaml:L15005-L15012] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper | [docs/registry/implementation-map.yaml:L15017-L15024] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper | [docs/registry/implementation-map.yaml:L15029-L15036] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` | test_helper | [docs/registry/implementation-map.yaml:L15041-L15048] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_hs_from_m0_relationship` | test_helper | [docs/registry/implementation-map.yaml:L15053-L15060] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` | test_helper | [docs/registry/implementation-map.yaml:L15065-L15072] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` | test_helper | [docs/registry/implementation-map.yaml:L15077-L15084] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestPublishedBenchmarks.test_jonswap_vs_published_curves` | test_helper | [docs/registry/implementation-map.yaml:L15089-L15096] |
| `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` | test_helper | [docs/registry/implementation-map.yaml:L15101-L15108] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | test_helper | [docs/registry/implementation-map.yaml:L15113-L15120] |
| `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` | test_helper | [docs/registry/implementation-map.yaml:L15125-L15132] |
| `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` | test_helper | [docs/registry/implementation-map.yaml:L15137-L15144] |
| `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` | test_helper | [docs/registry/implementation-map.yaml:L15149-L15156] |
| `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` | test_helper | [docs/registry/implementation-map.yaml:L15161-L15168] |
| `tests/unit/hydrodynamics/test_rao_analysis.py` | `TestWaveSpectra.test_jonswap_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15175-L15182] |
| `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15187-L15194] |
| `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestBretschneiderExtended.test_bretschneider_peak_near_tp` | test_helper | [docs/registry/implementation-map.yaml:L15199-L15206] |
| `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` | test_helper | [docs/registry/implementation-map.yaml:L15211-L15218] |
| `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` | test_helper | [docs/registry/implementation-map.yaml:L15223-L15230] |
| `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` | test_helper | [docs/registry/implementation-map.yaml:L15235-L15242] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L15247-L15254] |
| `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L15261-L15268] |
| `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper | [docs/registry/implementation-map.yaml:L15275-L15282] |
| `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | test_helper | [docs/registry/implementation-map.yaml:L15291-L15298] |
| `tests/workflows/test_durable_workflows.py` | `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L15305-L15312] |
| `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` | caller | [docs/registry/implementation-map.yaml:L15319-L15326] |
| `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range.ordinate` | independent | [docs/registry/implementation-map.yaml:L15338-L15345] |

## 10. Coverage and open items

Coverage record: both package files were read in full (`__init__.py`, `workflow.py`); source/delegate methods, engine route, workflow inputs, registry rows and tests cited above were read at the header's code pin. `[src/digitalmodel/hydrodynamics/wave_spectrum/__init__.py:L1-L2]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L1-L186]` `[src/digitalmodel/engine.py:L493-L498]`

The module's own initializer has only a docstring; indirect router coverage is in the three workflow nodes of section 4, supplemented by `tests/unit/hydrodynamics/test_wave_spectra_extended.py` and `tests/unit/hydrodynamics/test_rao_analysis.py` class `TestWaveSpectra`; similarly named marine-engineering and standalone spectrum implementations belong to other packages in section 9. `[src/digitalmodel/hydrodynamics/wave_spectrum/__init__.py:L1-L2]` `[tests/workflows/test_durable_workflows.py:L565-L580]` `[tests/workflows/test_durable_workflows.py:L1846-L1883]` `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L1-L24]` `[tests/unit/hydrodynamics/test_rao_analysis.py:L173-L273]` [docs/registry/implementation-map.yaml:L13567-L15368]

Documentation discovery list, including related implementations rather than treating their claims as router evidence:

- `docs/registry/implementation-map.md`: inventory method and qualification limits. [docs/registry/implementation-map.md:L1-L16]
- `docs/domains/metocean/wave_spectra_implementation_summary.md`: older marine-engineering implementation and examples. [docs/domains/metocean/wave_spectra_implementation_summary.md:L1-L36] [docs/domains/metocean/wave_spectra_implementation_summary.md:L182-L220]
- `docs/domains/orcawave/reference/INDEX.md`: links to a separate program's wave-spectrum documentation. [docs/domains/orcawave/reference/INDEX.md:L6-L21]
- `docs/domains/marine-engineering/marine_excel_analysis_report.md`: response-spectrum interface examples. [docs/domains/marine-engineering/marine_excel_analysis_report.md:L270-L281] [docs/domains/marine-engineering/marine_excel_analysis_report.md:L672-L680]
- `docs/domains/marine-engineering/marine-engineering-excel-mapping.md`: proposed wave-spectra structure. [docs/domains/marine-engineering/marine-engineering-excel-mapping.md:L122-L145]
- `docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md`: wave-spectra specification overview. [docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md:L80-L100]
- `docs/domains/project-docs/PHASE3_PERFORMANCE_DELIVERABLES.md`: profiling-output names rather than router acceptance evidence. [docs/domains/project-docs/PHASE3_PERFORMANCE_DELIVERABLES.md:L129-L139]

UNVERIFIED: legacy documentation's completion and benchmark-qualification claims are not an independently checked qualification of this router; no such claim is adopted in this pack (#1799). [docs/domains/metocean/wave_spectra_implementation_summary.md:L3-L6] [docs/domains/metocean/wave_spectra_implementation_summary.md:L269-L279]

UNVERIFIED: one discovery document with an identifying path is omitted from the public citation list until it has a neutral identifier; issue pending (2).

| Open item | Responsible role | Tracking |
| --- | --- | --- |
| Standards edition, clause title and licensed-clause qualification remain UNVERIFIED; no `Citation` sidecar is emitted. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L73-L75]` `[src/digitalmodel/hydrodynamics/wave_spectra.py:L123-L125]` `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L57-L68]` | Calculation maintainer | #1798 |
| Independent full-spectrum reference and legacy qualification claims remain UNVERIFIED; section-4 validation covers moments only. `[src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]` `[tests/unit/hydrodynamics/test_wave_spectra_extended.py:L262-L293]` | Calculation maintainer | #1799 |
| Fractional counts, ambiguous settings, unit intent, output portability and missing negative/router tests; section-6 candidates without detection checks. `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L72-L186]` | Calculation maintainer | issue pending (1) |
| One discovery file with identifying names; public path citation remains UNVERIFIED | Documentation maintainer | issue pending (2) |
| Map helper classifications, broad external candidates and shared-log indexing remain follow-up; no additional package miss was found. [docs/registry/implementation-map.yaml:L14071-L14096] `[src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py:L72-L186]` | Calculation maintainer | issue pending (3) |

The calculation-output citation contract was readable; private data was not used, and no private-sidecar value is included in this pack.
