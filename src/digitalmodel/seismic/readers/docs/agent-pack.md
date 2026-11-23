# Seismic readers knowledge pack

| Header | Value |
| --- | --- |
| Module | seismic/readers |
| Repository | digitalmodel |
| Package | `src/digitalmodel/seismic/readers` |
| Pack revision | 0 |
| Code revision | `df9787d346c226aad513cab9d248f7be841b37d5` |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public exports are `AccelerogramRecord` and `read_ascii_accelerogram`; the reader produces a single-component record from UTF-8 ASCII numeric rows, converting acceleration to SI and retaining a SHA-256 digest of the original bytes. [src/digitalmodel/seismic/readers/__init__.py:L1-L5] [src/digitalmodel/seismic/readers/ascii.py:L104-L127]

The supported layouts are one acceleration column with a time increment, or two columns of time and acceleration; other widths raise `ValueError`, and time must have at least two samples with a positive uniform increment. [src/digitalmodel/seismic/readers/ascii.py:L157-L180]

The enclosing seismic domain has maturity `development`; this pack covers the readers package only, excluding baseline correction, oscillator calculations, and the response-spectrum workflow that consumes it. [docs/registry/module-routing.yaml:L423-L428] [src/digitalmodel/seismic/response_spectrum/calculations.py:L17-L21] [src/digitalmodel/seismic/response_spectrum/workflow.py:L19-L31]

## 2. Inputs and outputs

### Public definition inventory

| Public symbol | Inputs | Output | Evidence |
| --- | --- | --- | --- |
| `AccelerogramRecord` | Time, acceleration, units and source identity | Immutable sample record | [src/digitalmodel/seismic/readers/ascii.py:L17-L38] |
| `AccelerogramRecord.npts` | Retained time array | Sample count | [src/digitalmodel/seismic/readers/ascii.py:L82-L83] |
| `AccelerogramRecord.dt_s` | First two retained times | Interval in s | [src/digitalmodel/seismic/readers/ascii.py:L86-L89] |
| `AccelerogramRecord.pga_m_s2` | Retained acceleration | Peak absolute value in m/s² | [src/digitalmodel/seismic/readers/ascii.py:L92-L93] |
| `AccelerogramRecord.pga_g` | Peak acceleration | Peak in g | [src/digitalmodel/seismic/readers/ascii.py:L96-L97] |
| `AccelerogramRecord.acceleration_g` | Retained acceleration | Derived array in g | [src/digitalmodel/seismic/readers/ascii.py:L100-L101] |
| `read_ascii_accelerogram` | Source path, optional units and interval | Accelerogram record | [src/digitalmodel/seismic/readers/ascii.py:L104-L127] |

### Inputs

| Name | Unit | Allowed range / interpretation | Default | Source and evidence |
| --- | --- | --- | --- | --- |
| `path` | File path | Readable bytes decoded as UTF-8; numeric tokens separated by whitespace | Required | Caller; [src/digitalmodel/seismic/readers/ascii.py:L104-L114] [src/digitalmodel/seismic/readers/ascii.py:L130-L143] |
| `units` | Acceleration unit token | `g`, `cm_s2`, `m_s2` and listed aliases; explicit truthy argument overrides header | Argument `None`; effective fallback `m_s2` | Caller or comment metadata; [src/digitalmodel/seismic/readers/ascii.py:L107-L117] [src/digitalmodel/seismic/readers/ascii.py:L183-L196] |
| `dt_s` | s | Required for one-column data unless header provides it; resulting time must pass uniformity validation; a parsed interval does not generate two-column times, but malformed interval metadata can still reject the file when no explicit interval is supplied | `None` | Caller or `dt_s`, `dt`, `delta_t`, `time_step_s` comment; [src/digitalmodel/seismic/readers/ascii.py:L116-L119] [src/digitalmodel/seismic/readers/ascii.py:L146-L180] |
| Numeric rows | s and declared acceleration unit | Uniform row widths of 1 or 2; time is first column for width 2 | No numeric rows raises | UTF-8 source; [src/digitalmodel/seismic/readers/ascii.py:L130-L143] [src/digitalmodel/seismic/readers/ascii.py:L157-L180] |

### Outputs

| Name | Unit | Range / representation | Default | Evidence |
| --- | --- | --- | --- | --- |
| `time_s`, `acceleration_m_s2` | s, m/s² | Immutable byte snapshots exposed through fresh ndarray headers | Reader-derived | [src/digitalmodel/seismic/readers/ascii.py:L16-L52] [src/digitalmodel/seismic/readers/ascii.py:L118-L127] |
| `units`, `source_path`, `source_digest` | Token, path, SHA-256 hex | Canonical input unit token; source path; hash of original bytes rather than converted samples | Reader-derived | [src/digitalmodel/seismic/readers/ascii.py:L115-L127] |
| `npts`, `dt_s` | Samples, s | Array size; difference of first two times | Derived properties | [src/digitalmodel/seismic/readers/ascii.py:L81-L89] |
| `pga_m_s2`, `pga_g`, `acceleration_g` | m/s², g, g | Maximum absolute acceleration; scalar and array division by `G0_M_S2` | Derived properties | [src/digitalmodel/seismic/readers/ascii.py:L91-L101] |

One-column time starts at zero; two-column time retains the supplied origin, and acceleration signs are retained through positive scale factors without coordinate rotation. [src/digitalmodel/seismic/readers/ascii.py:L161-L171] [src/digitalmodel/seismic/readers/ascii.py:L199-L205]

UNVERIFIED: physical axis orientation, geographic origin and phase convention cannot be recovered from this record interface; callers must document them separately, tracked in issue pending (2). [src/digitalmodel/seismic/readers/ascii.py:L20-L24]

## 3. Method and governing standards

| Definition | Method / reference | Limits and evidence |
| --- | --- | --- |
| `read_ascii_accelerogram` | Read bytes, parse, choose units and interval, validate time, convert, hash original bytes | No standards clause or `Citation` sidecar is emitted by this path; [src/digitalmodel/seismic/readers/ascii.py:L104-L127] |
| `_parse_rows`, `_metadata_from_comment` | Skip blanks and comments; extract `# key: value`; normalize selected interval aliases; convert whitespace tokens with Python `float` | Last matching metadata key wins; other comment text contributes no numeric samples; [src/digitalmodel/seismic/readers/ascii.py:L130-L154] |
| `_time_and_acceleration` | One column: `t[i] = i * dt_s`; two columns: retain supplied time and acceleration | Widths outside the two supported layouts raise; [src/digitalmodel/seismic/readers/ascii.py:L157-L172] |
| `_validate_uniform_time_step` | Positive adjacent differences; NumPy `allclose` against the first difference, relative tolerance `1e-6`, absolute tolerance `1e-12` s | These are implementation tolerances, not a cited standard limit; [src/digitalmodel/seismic/readers/ascii.py:L175-L180] |
| `_normalise_units`, `_to_si` | Alias lookup; g multiplied by `G0_M_S2`; cm/s² divided by 100; SI unchanged | Factors are implementation values; [src/digitalmodel/seismic/readers/ascii.py:L13-L13] [src/digitalmodel/seismic/readers/ascii.py:L183-L205] |
| `AccelerogramRecord` methods | Snapshot inputs; exact array/metadata equality; reconstruct through constructor for copying and pickle; normalize legacy state | Object-containing arrays and ndarray subclasses rejected; [src/digitalmodel/seismic/readers/ascii.py:L26-L79] |
| Record properties | Sample count, first interval, maximum absolute acceleration and g scaling | Direct construction does not call the reader's time validation; [src/digitalmodel/seismic/readers/ascii.py:L26-L38] [src/digitalmodel/seismic/readers/ascii.py:L81-L101] |

UNVERIFIED: the physical reference and citation-sidecar applicability for the implementation's gravity constant are not established by this package; issue pending (3) requests provenance rather than treating the conversion tests as an independent reference. [src/digitalmodel/seismic/readers/ascii.py:L13-L13] [src/digitalmodel/seismic/readers/ascii.py:L199-L205] [tests/seismic/test_readers.py:L10-L23]

## 4. Validated worked examples and regression examples

Results below were recorded on 2026-11-04 against the header code revision using `PYTHONPATH=src uv run --no-sources python -m pytest tests/seismic -q -rA -p no:cacheprovider --basetemp=<scratch>` with the repository's unchanged pytest options; BASE: 176 passed, 0 failed, 0 skipped, 0 NOT_RUN, including 4 reader and 50 integrity cases; AFTER: the same 176 node IDs passed, with 0 failed, 0 skipped and 0 NOT_RUN. The expected quantities below come from the cited test assertions and the independent synthetic hand example, rather than a generated snapshot. [tests/seismic/test_readers.py:L13-L51] [tests/seismic/test_record_integrity.py:L3-L8] [tests/seismic/test_record_integrity.py:L30-L47]

| Case / classification | Source and input | Expected value and tolerance basis | Existing test node ID | Offline result at BASE / after |
| --- | --- | --- | --- | --- |
| V1 validated: SI hand calculation and retained snapshots | Independent counting, adjacent subtraction and maximum absolute value; synthetic bytes written by fixture | 4 samples, 0.5 s, 2 m/s²; exact equality, zero error in each unit | `tests/seismic/test_record_integrity.py::test_constructor_snapshots_caller_arrays` | PASS / PASS; [tests/seismic/test_record_integrity.py:L3-L8] [tests/seismic/test_record_integrity.py:L22-L47] |
| R1 regression: g conversion | Synthetic `two_column_g.txt`; coefficient duplicated by test, not an independent gravity reference | 4 samples; 0.5 s; 0.2 g; scalar `pytest.approx` default relative `1e-6`, absolute `1e-12`; arrays `assert_allclose` default relative `1e-7`, absolute 0 | `tests/seismic/test_readers.py::test_reads_two_column_time_acceleration_and_converts_g_to_si` | PASS / PASS; [tests/seismic/fixtures/two_column_g.txt:L1-L4] [tests/seismic/test_readers.py:L10-L23] |
| R2 regression: one-column centimetre conversion | Synthetic `single_column_dt_header.asc` | 4 samples; 0.25 s; accelerations `[0, 0.1, -0.2, 0.05]` m/s²; scalar and array library defaults as R1 | `tests/seismic/test_readers.py::test_reads_single_column_dt_header_and_converts_cm_s2_to_si` | PASS / PASS; [tests/seismic/fixtures/single_column_dt_header.asc:L1-L6] [tests/seismic/test_readers.py:L26-L35] |
| R3 regression: irregular time rejection | Synthetic three-row file generated in test | 1 `ValueError` with uniform-time diagnostic; 0 accepted malformed records | `tests/seismic/test_readers.py::test_rejects_nonuniform_two_column_time_step` | PASS / PASS; [tests/seismic/test_readers.py:L38-L43] |
| R4 regression: missing interval rejection | Synthetic single-column file generated in test | 1 `ValueError` naming `dt_s`; 0 accepted records without interval | `tests/seismic/test_readers.py::test_requires_dt_for_single_column_without_header` | PASS / PASS; [tests/seismic/test_readers.py:L46-L51] |

The conversion cases remain regression examples because the g factor is repeated in the tests; the exact SI hand oracle qualifies only sample retention and derived count, interval and peak, not instrument accuracy or physical axis interpretation. [tests/seismic/test_readers.py:L10-L23] [tests/seismic/test_record_integrity.py:L3-L8]

## 5. QA gates

Gate applicability uses AQG r0, section 10, row “Python calculations”, and section 9 for G5. The following are executable acceptance instructions; a criterion is not a claim that untested malformed inputs are rejected. (AQG r0, sections 9–10.)

| Gate | Applicability | Module-specific check |
| --- | --- | --- |
| G0 | Applicable: inputs, units, citations | C01, C02, C03 |
| G1 | Applicable: Python import and tests; no program deck is written by the reader | C04; [src/digitalmodel/seismic/readers/ascii.py:L104-L127] |
| G2 | Applicable: independent hand calculation | C05 |
| G3 | NOT_APPLICABLE: reader returns a record without a dynamics integration | [src/digitalmodel/seismic/readers/ascii.py:L104-L127] |
| G4 | NOT_APPLICABLE: no diffraction body, mesh or hydrostatics output | [src/digitalmodel/seismic/readers/ascii.py:L104-L127] |
| G5 | Applicable: evidence completeness for reader results | C06 |

| Check ID | Executable check and evidence | Numeric pass criterion and unit | Gate |
| --- | --- | --- | --- |
| C01 | Run section-4 R3 and R4 node IDs; preserve pytest node results | 2 passing cases, 0 malformed records accepted; count | G0; [tests/seismic/test_readers.py:L38-L51] |
| C02 | Run section-4 R1 and R2 node IDs; compare `time_s`, converted acceleration, sample count and interval | 2 passing cases; expected 4 samples each, 0.5 s and 0.25 s; scalar relative error ≤ `1e-6` or absolute error ≤ `1e-12` in the quantity's unit; array error ≤ `1e-7 * abs(expected)` in s or m/s² | G0; [tests/seismic/test_readers.py:L13-L35] |
| C03 | Resolve every repository citation with `git show <pin>:<path>`; require nonempty inclusive ranges within the pinned file | 0 missing files, 0 invalid ranges; count | G0; acceptance procedure for citations supporting C01–C02 |
| C04 | Run `tests/seismic/test_readers.py` and `tests/seismic/test_record_integrity.py` as part of the full offline seismic command; retain per-node results | 54 passes, 0 failures, 0 skips; count | G1; [tests/seismic/test_readers.py:L13-L51] [tests/seismic/test_record_integrity.py:L40-L246] |
| C05 | Run V1; independently count four values, subtract adjacent times, and take `max(abs([0,1,-2,1]))`; verify after caller-array edits | Error = 0 samples, 0 s and 0 m/s²; 1 passing case | G2; [tests/seismic/test_record_integrity.py:L3-L8] [tests/seismic/test_record_integrity.py:L30-L47] |
| C06 | Run `tests/seismic/test_record_integrity.py::test_constructor_snapshots_caller_arrays` and `tests/seismic/test_response_spectrum_workflow.py::test_router_writes_timeseries_and_provenance_report`; retain source-byte hash assertion, emitted CSV/HTML checks, pin and all case statuses | 2 passing cases, 0 digest mismatches, 0 missing output files; count | G5; [tests/seismic/test_record_integrity.py:L30-L47] [tests/seismic/test_response_spectrum_workflow.py:L55-L92] |

## 6. Known failure modes and pitfalls

sweep: 15 found

Inspection date: 2026-11-04; code revision as recorded in the header.

The sweep groups occurrences into the 15 rows below, including intentional skips and unit assumptions; array-size/result casts are distinguished from conversions of external text. These commands cover every Python file in this package, followed by line-by-line examination of the complete implementation. [src/digitalmodel/seismic/readers/__init__.py:L1-L5] [src/digitalmodel/seismic/readers/ascii.py:L1-L205]

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%' src/digitalmodel/seismic/readers --glob '*.py'
rg -n 'int\(|float\(|units|G0_M_S2|allclose|metadata|cache|write' src/digitalmodel/seismic/readers --glob '*.py'
```

| ID | Symptom and cause at the pin | Detection / coverage | Tracking |
| --- | --- | --- | --- |
| S01 | Missing, empty or misspelled unit metadata selects SI silently; explicit truthy units take precedence over metadata | No detection check exists for missing-unit or conflicting-unit declarations | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L115-L115] |
| S02 | Header interval is converted with `float`; no locale separator handling | No detection check exists for locale-formatted interval text | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L116-L117] |
| S03 | Blank lines are intentionally skipped, losing any record-count meaning attached to blank rows | No detection check exists for unintended blank-row loss | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L133-L136] |
| S04 | Comment rows are skipped; duplicate normalized metadata overwrites earlier values | No detection check exists for duplicate or conflicting metadata | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L137-L139] |
| S05 | Numeric sample tokens use `float`; `nan`/`inf` acceleration is accepted; decimal-comma tokens raise instead of normalizing | No detection check exists for nonfinite acceleration or locale numbers | issue pending (1), issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L140-L140] [src/digitalmodel/seismic/readers/ascii.py:L199-L205] |
| S06 | Unmatched comment syntax returns an empty dictionary without warning | No detection check exists for malformed metadata syntax | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L146-L149] |
| S07 | Metadata keys are normalized and stored, but only interval and units affect reading; no component or coordinate interpretation | No detection check exists for unsupported metadata | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L114-L118] [src/digitalmodel/seismic/readers/ascii.py:L150-L154] |
| S08 | One-column time assumes a zero origin and converts the supplied interval to float | C01 detects missing interval; C02 detects ordinary interval generation; no detection check exists for semantic origin or numeric-type misuse | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L161-L167] |
| S09 | Two-column time assumes seconds and keeps its origin; a parsed interval does not generate these times, but malformed interval metadata can still reject the file when no explicit interval is supplied | C02 covers finite ordinary two-column time; no detection check exists for time-unit mismatch, unused-interval conflict or malformed interval metadata | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L116-L119] [src/digitalmodel/seismic/readers/ascii.py:L168-L171] |
| S10 | Time uniformity uses fixed relative/absolute tolerances and lacks an explicit finite-value test; a two-row `[0, inf]` time can pass | C01 rejects the existing finite nonuniform fixture; no detection check exists for infinite time | issue pending (1); [src/digitalmodel/seismic/readers/ascii.py:L175-L180] |
| S11 | Gravity constant and g/centimetre scale factors have no physical-source citation in this path | C02 covers regression conversion only; no detection check exists for citation provenance | issue pending (3); [src/digitalmodel/seismic/readers/ascii.py:L13-L13] [src/digitalmodel/seismic/readers/ascii.py:L199-L205] |
| S12 | Unit aliases normalize slash and hyphen spelling; unsupported units raise rather than infer | C02 covers `g` and `cm_s2`; no detection check exists for every alias or unsupported-unit token | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L183-L196] |
| S13 | Private `_to_si` returns unchanged values for any token other than g or centimetres; the public reader validates tokens first | No detection check exists for direct helper misuse | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L115-L120] [src/digitalmodel/seismic/readers/ascii.py:L199-L205] |
| S14 | Direct record construction snapshots arrays without checking matching lengths, dimensions, finite numeric values or time uniformity; supplied digest need not authenticate samples | C05 checks valid snapshots; no detection check exists for invalid direct-constructor values | issue pending (1); [src/digitalmodel/seismic/readers/ascii.py:L20-L38] [docs/domains/provenance-array-audit.md:L91-L94] |
| S15 | `npts`, `dt_s` and PGA cast array results to `int`/`float`; first interval and maximum absolute acceleration assume a valid record; g properties assume the implementation constant | C05 checks exact finite SI results; no detection check exists for inconsistent manually constructed records | issue pending (1); [src/digitalmodel/seismic/readers/ascii.py:L81-L101] |

No `except`, error-path `return None`, cache write, epoch default or percentage fallback appears in the complete reader implementation; empty metadata for unmatched comments is S06, and positive-time zero comparison is S10. [src/digitalmodel/seismic/readers/ascii.py:L1-L205]

The issue search identified #1182 as a capability/format-expansion lead and #3338 as a pack-coverage lead, rather than a confirmed reader incident; neither supplies a detection test for the defects above. Reader parsing and the existing test coverage, rather than their planning estimates, define this pack's current scope. [src/digitalmodel/seismic/readers/ascii.py:L104-L205] [tests/seismic/test_readers.py:L13-L51]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-04 | Initial reader pack | Immutable snapshots protect retained values, but direct constructor provenance is not authentication | [docs/domains/provenance-array-audit.md:L91-L94] [tests/seismic/test_record_integrity.py:L30-L47] | Separate C05 snapshot evidence from S14 input validity | Not recorded; coverage follow-up #3338 |

## 8. Ownership

The designated review role is the engineering calculations quality manager; the deputy role is the engineering calculations deputy quality manager. Use independent-provider review for gate clearance and refer disputed classification or acceptance criteria to the family quality manager. (AQG r0, section 11.)

UNVERIFIED: named appointments for the quality manager and deputy, and a shared-log registration, are not established by repository routing metadata; maintainers to assign and register them under #3338. [docs/registry/module-routing.yaml:L423-L428]

## 9. Dependencies and implementation inventory

The reader directly uses NumPy plus standard-library dataclasses, hashing, paths, regex and typing; `read_ascii_accelerogram` returns the record without launching an external solver. [src/digitalmodel/seismic/readers/ascii.py:L5-L11] [src/digitalmodel/seismic/readers/ascii.py:L104-L127]

| Direction | Dependency / consumer | Contract / evidence |
| --- | --- | --- |
| Upstream | UTF-8 numeric source and optional header | Original bytes are retained before parsing and hashed when constructing the record; [src/digitalmodel/seismic/readers/ascii.py:L112-L126] |
| Upstream | NumPy | Snapshots and exposed arrays; conversion and uniformity; [src/digitalmodel/seismic/readers/ascii.py:L26-L52] [src/digitalmodel/seismic/readers/ascii.py:L175-L205] |
| Downstream | `seismic` package export | Re-exports both public reader names; [src/digitalmodel/seismic/__init__.py:L1-L5] |
| Downstream | `seismic/response_spectrum` | Workflow delegates file, units and interval to reader; calculations consume arrays; [src/digitalmodel/seismic/response_spectrum/workflow.py:L19-L31] [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L63] |
| Test data | Two synthetic reader fixtures | [tests/seismic/fixtures/two_column_g.txt:L1-L4] [tests/seismic/fixtures/single_column_dt_header.asc:L1-L6] |

Implementation-map evidence is pinned separately: ref `origin/codex/implmap-work`, ref commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, file `docs/registry/implementation-map.yaml`. The inventory below is the result of parsing that exact map and selecting implementation rows whose `path` starts with `src/digitalmodel/seismic/readers/`.

| Map inventory result | Count / outcome |
| --- | --- |
| Families with at least one in-package row | 0 |
| Implementations in the required family inventory | 0 (empty inventory) |
| Counted families | 0 |
| Case-sensitive Python `re` package grep using counted-family signals | NOT_APPLICABLE: no map family has an in-package row |
| Further second implementations affecting acceptance | 0; no counted family |

The map follow-up is issue pending (4): introduce the section-3 method families for ASCII accelerogram parsing, unit conversion, uniform-time validation, PGA extraction and immutable record/provenance handling; these begin counting only after a map revision contains in-package rows. Their current implementations are the definitions in section 3. [src/digitalmodel/seismic/readers/ascii.py:L16-L205]

The additional whole-repository search below includes `scripts/` and is a discovery check independent of the counted-family result. Its reader hits identify this package, re-exports, test consumers and a workflow call; the workflow delegates to the listed reader, and the spectrum calculation consumes its arrays. [src/digitalmodel/seismic/readers/__init__.py:L1-L5] [src/digitalmodel/seismic/__init__.py:L1-L5] [src/digitalmodel/seismic/response_spectrum/workflow.py:L19-L31] [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L63] [tests/seismic/test_readers.py:L13-L51] [tests/seismic/test_record_integrity.py:L19-L27]

```bash
rg -n 'read_ascii_accelerogram|ASCII accelerogram|class AccelerogramRecord' . --glob '*.py'
```

## 10. Coverage and open items

### Read coverage

| Material | Scope and evidence |
| --- | --- |
| Package source, all files and subfolders | `__init__.py` and `ascii.py`; complete exports and implementation; [src/digitalmodel/seismic/readers/__init__.py:L1-L5] [src/digitalmodel/seismic/readers/ascii.py:L1-L205] |
| Module docs discovery | `docs/maps/digitalmodel-operator-map.md` and `docs/domains/provenance-array-audit.md`; [docs/maps/digitalmodel-operator-map.md:L65-L65] [docs/domains/provenance-array-audit.md:L51-L51] [docs/domains/provenance-array-audit.md:L61-L94] |
| Broad docs search false positive | `docs/domains/storage-tanks/api650-annex-e.md` describes a separate tank calculation, outside this reader package | [docs/domains/storage-tanks/api650-annex-e.md:L1-L12] |
| Routing | Existing seismic domain entry; registry addition in this change belongs under its `submodules.readers` mapping | [docs/registry/module-routing.yaml:L423-L428] |
| Reader tests | All 4 reader cases and 50 parametrized integrity cases | [tests/seismic/test_readers.py:L13-L51] [tests/seismic/test_record_integrity.py:L40-L246] |
| Consumer tests | All 4 response-spectrum workflow cases, including file/HTML output and engine dispatch | [tests/seismic/test_response_spectrum_workflow.py:L25-L127] |
| Additional seismic suite coverage | Baseline, elastic-layer and SDOF tests are run for neighboring-module regression coverage, outside this pack's method scope | [tests/seismic/test_baseline.py:L1-L20] [tests/seismic/test_elastic_layer.py:L1-L20] [tests/seismic/test_sdof.py:L1-L20] |

Documentation discovery command: `rg -l -i 'accelerogram|strong.motion|seismic.{0,20}reader|read_ascii_accelerogram|AccelerogramRecord' docs --glob '*.md'`; source and test discovery additionally searched the package and repository for the two exported names. [src/digitalmodel/seismic/readers/__init__.py:L3-L5]

The calculation-output citation contract was read; no new formula or coefficient is introduced here, and the existing gravity reference remains an explicit open item rather than an asserted standards-derived result. [src/digitalmodel/seismic/readers/ascii.py:L13-L13] [src/digitalmodel/seismic/readers/ascii.py:L199-L205]

### Open items (maintainers responsible for follow-up)

| Item | Limit / required action | Tracking |
| --- | --- | --- |
| O1 | Add rejection tests for nonfinite input, infinite time and invalid direct-constructor state; S05, S10, S14, S15 | issue pending (1); [src/digitalmodel/seismic/readers/ascii.py:L26-L38] [src/digitalmodel/seismic/readers/ascii.py:L140-L140] [src/digitalmodel/seismic/readers/ascii.py:L175-L180] |
| O2 | Define metadata, precedence, numeric-text, unit and coordinate contracts and tests; S01–S09, S12–S13; unresolved orientation/origin/phase claim from section 2 | issue pending (2); [src/digitalmodel/seismic/readers/ascii.py:L114-L117] [src/digitalmodel/seismic/readers/ascii.py:L130-L205] |
| O3 | Resolve physical gravity reference and citation sidecar applicability; add independent conversion evidence; S11 and unresolved reference claim from section 3 | issue pending (3); [src/digitalmodel/seismic/readers/ascii.py:L13-L13] [tests/seismic/test_readers.py:L10-L23] |
| O4 | Add missing section-3 method families and implementations to the designated map | issue pending (4); [src/digitalmodel/seismic/readers/ascii.py:L16-L205] |
| O5 | Confirm reviewer/deputy assignments and shared-log registration; unresolved assignment claim from section 8 | #3338; [docs/registry/module-routing.yaml:L423-L428] |
| O6 | Extended strong-motion formats and independently sourced physical-record examples remain outside the current simple ASCII layouts; retain capability follow-up without asserting those formats are supported | #1182; [src/digitalmodel/seismic/readers/ascii.py:L104-L110] [src/digitalmodel/seismic/readers/ascii.py:L157-L172] |

No private sidecar entry or client-derived values are used in this pack; the worked inputs are the repository's synthetic fixtures and hand example. [tests/seismic/fixtures/two_column_g.txt:L1-L4] [tests/seismic/fixtures/single_column_dt_header.asc:L1-L6] [tests/seismic/test_record_integrity.py:L3-L8]
