# Riser fatigue knowledge pack

| Header | Value |
|---|---|
| Module / repository | riser_fatigue / digitalmodel |
| Package | `src/digitalmodel/riser_fatigue` |
| Pack revision / status | 0 / DRAFT |
| Code revision | `b873edb364f0c55bf7789a7ae960ce9035233147` |
| Analysis family | Engineering calculations (EC), AQG r0 Python calculations |
| Quality roles | Engineering calculations quality manager; deputy calculations quality manager |

## 1. Purpose and scope

The touchdown API consumes a supplied stress-range histogram and produces bin damage, annual and design-life damage, unfactored fatigue life and a DFF verdict. [src/digitalmodel/riser_fatigue/touchdown.py:L157-L247]

The durable workflow evaluates supplied wave and VIV responses for each segment, sums their damages, and selects the maximum-damage segment for the screening verdict. [src/digitalmodel/riser_fatigue/workflow.py:L65-L144]

The routing registry records beta maturity; this is a registry classification rather than evidence of physical qualification. [docs/registry/module-routing.yaml:L490-L495]

The package does not extract dynamic stress histories: its documentation defers the OrcaFlex, Shear7 and VIVANA front ends to #810. [src/digitalmodel/riser_fatigue/workflow.py:L19-L23]

UNVERIFIED: qualified geometry, water-depth, loading and solver-version applicability limits have not been established; qualification remains under #810 and #812.

## 2. Inputs and outputs

### Inputs

| Input | Unit / convention | Range / default / origin | Evidence |
|---|---|---|---|
| `RiserSection.outer_diameter_mm`, `wall_thickness_mm` | mm | Caller-supplied; diameter is reported, wall thickness feeds correction | [src/digitalmodel/riser_fatigue/touchdown.py:L51-L67] [src/digitalmodel/riser_fatigue/touchdown.py:L187-L194] [src/digitalmodel/riser_fatigue/touchdown.py:L267-L269] |
| `stress_ranges_mpa`, `cycles` | MPa stress **range**, cycle count per histogram period | Equal array shapes enforced; neither finite nor nonnegative values checked locally | [src/digitalmodel/riser_fatigue/touchdown.py:L78-L81] [src/digitalmodel/riser_fatigue/touchdown.py:L165-L175] |
| `sn_class`, `environment` | Category / environment strings | Defaults in input dataclass; class uppercased before both lookups; environments validated by core | [src/digitalmodel/riser_fatigue/touchdown.py:L108-L115] [src/digitalmodel/riser_fatigue/touchdown.py:L177-L185] [src/digitalmodel/fatigue/sn_curves.py:L96-L124] |
| `scf`, `thickness_exponent`, `thickness_ref_mm` | Dimensionless, dimensionless, mm | SCF neutral default 1; class-derived exponent unless overridden; reference and class constants are not reproduced here | [src/digitalmodel/riser_fatigue/touchdown.py:L96-L115] [src/digitalmodel/riser_fatigue/touchdown.py:L181-L194] |
| `design_life_years`, `histogram_period_years`, `dff` | years, years, dimensionless | Direct API has defaults; period and DFF must exceed zero; direct API does not check design life | [src/digitalmodel/riser_fatigue/touchdown.py:L105-L115] [src/digitalmodel/riser_fatigue/touchdown.py:L165-L175] |
| Workflow `segments`, `wave` | List, mapping; wave keys use `stress_ranges_MPa` | Nonempty segments required; wave lists converted with Python `float`; no locale parsing | [src/digitalmodel/riser_fatigue/workflow.py:L147-L181] [src/digitalmodel/riser_fatigue/workflow.py:L248-L273] |
| VIV cases | Stress range MPa, frequency Hz, exposure fraction or hours/year | Positive stress/frequency; fraction in (0, 1]; fraction takes precedence over hours; aliases omit unit suffixes | [src/digitalmodel/riser_fatigue/workflow.py:L218-L224] [src/digitalmodel/riser_fatigue/workflow.py:L276-L308] |
| Workflow life / DFF | years / dimensionless | Life required and positive; explicit positive DFF wins, otherwise cited getter must resolve | [src/digitalmodel/riser_fatigue/workflow.py:L48-L69] [src/digitalmodel/riser_fatigue/workflow.py:L301-L308] |
| Output location | Filesystem path | Relative `output_dir` defaults to `results` below configuration directory, otherwise current directory | [src/digitalmodel/riser_fatigue/workflow.py:L323-L349] |

### Outputs

| Output | Unit / contract | Evidence |
|---|---|---|
| `BinResult` | Nominal/corrected stress MPa, applied/allowable cycles count, damage dimensionless | [src/digitalmodel/riser_fatigue/touchdown.py:L125-L133] [src/digitalmodel/riser_fatigue/touchdown.py:L203-L215] |
| `TouchdownFatigueResult` | Period/design-life/allowable damage and usage dimensionless; annual damage per year; fatigue life years; PASS or FAIL | [src/digitalmodel/riser_fatigue/touchdown.py:L136-L154] [src/digitalmodel/riser_fatigue/touchdown.py:L217-L247] |
| Router result mapping | Governing segment, wave/VIV/total damage, life years, DFF margin and screening status | [src/digitalmodel/riser_fatigue/workflow.py:L86-L103] |
| Bin CSV / summary CSV | Stress columns MPa, frequency Hz, cycles count; summary lives years and damages dimensionless; wave frequency is NaN | [src/digitalmodel/riser_fatigue/workflow.py:L187-L202] [src/digitalmodel/riser_fatigue/workflow.py:L230-L245] [src/digitalmodel/riser_fatigue/workflow.py:L133-L143] |
| Markdown string | Inputs, per-bin damage and verdict; no file write in `report_markdown` | [src/digitalmodel/riser_fatigue/touchdown.py:L250-L317] |

No spatial coordinate frame is consumed by these scalar histogram calculations; the VIV cycle conversion uses a fixed year duration defined in code. [src/digitalmodel/riser_fatigue/touchdown.py:L105-L115] [src/digitalmodel/riser_fatigue/workflow.py:L44-L45] [src/digitalmodel/riser_fatigue/workflow.py:L218-L224]

UNVERIFIED: a complete external file unit, sign, stress-amplitude/range and provenance validation contract is not enforced; issue pending (1).

## 3. Method and governing standards

| Method / standard | Implementation and limits | Evidence |
|---|---|---|
| SCF and thickness correction | `assess_touchdown_fatigue`: scales nominal range by SCF, calls `thickness_correction`, then S-N and Miner core; no correction below reference thickness | [src/digitalmodel/riser_fatigue/touchdown.py:L177-L201] [src/digitalmodel/fatigue/damage.py:L91-L125] |
| S-N and Miner | `get_sn_curve` and `miner_damage` supply allowable cycles and sum bin cycle ratios; wrappers do not independently qualify their tables | [src/digitalmodel/fatigue/sn_curves.py:L1-L21] [src/digitalmodel/fatigue/damage.py:L13-L39] |
| Period scaling and verdict | Annual damage = period damage / period years; design damage = annual damage × life; allowable = inverse DFF; PASS when usage ≤ 1 | [src/digitalmodel/riser_fatigue/touchdown.py:L217-L247] |
| Wave adapter | `_wave_damage` delegates to touchdown assessment, scales its bins to design-life cycles/damage, and exports them | [src/digitalmodel/riser_fatigue/workflow.py:L147-L202] |
| Combined screening | `_evaluate_segment` sums wave and VIV design-life damages, divides design life by total damage for life, and compares life with life × DFF | [src/digitalmodel/riser_fatigue/workflow.py:L106-L144] [src/digitalmodel/riser_fatigue/workflow.py:L311-L320] |
| VIV exposure | `_viv_damage` computes cycles = frequency × annual exposure fraction × seconds/year × design life, then delegates to shared Miner/S-N core; VIV ranges are passed without wave SCF/thickness correction | [src/digitalmodel/riser_fatigue/workflow.py:L205-L245] |
| DNV-RP-C203 | Source labels method/report 2021 but class exponent docstring names 2011; table numbering and edition mapping are a known basis gap under #1588 and #2161; clause text not checked against the licensed copy | [src/digitalmodel/riser_fatigue/touchdown.py:L14-L23] [src/digitalmodel/riser_fatigue/touchdown.py:L96-L102] [src/digitalmodel/riser_fatigue/touchdown.py:L148-L148] [src/digitalmodel/fatigue/sn_curves.py:L1-L10] |
| API 5L | Informational material designation only; the package does not implement a material grade qualification or cite an edition/clause; clause text not checked against the licensed copy | [src/digitalmodel/riser_fatigue/touchdown.py:L61-L67] [src/digitalmodel/riser_fatigue/workflow.py:L133-L143] |
| DNV-OS-F201 | DFF named in input documentation; getter cites design fatigue factors, Sec.5; no licensed values reproduced here; clause text not checked against the licensed copy | [src/digitalmodel/riser_fatigue/touchdown.py:L91-L92] [src/digitalmodel/riser_database/getters.py:L122-L141] |
| Citation sidecar | `resolve_dff` retains getter `.value` only, while getter creates and validates a `Citation`; router output does not include that citation | [src/digitalmodel/riser_fatigue/workflow.py:L48-L62] [src/digitalmodel/riser_fatigue/workflow.py:L86-L103] [src/digitalmodel/riser_database/getters.py:L130-L141] |

UNVERIFIED: exact governing standard editions, titles and clause-level applicability are unresolved under #1588 and #2161; result-side citation retention remains under #810 and #814; material input qualification is issue pending (1).

## 4. Validated worked examples

Classification: 0 independently validated examples; 3 regression examples below. The single-bin test computes an expected value from encoded standard parameters, without a separately opened independent benchmark; it is retained as a regression anchor under #1588. [tests/riser_fatigue/test_touchdown.py:L24-L52]

| Case | Source / input | Expected quantity and tolerance | Existing test node ID | Offline result at code pin |
|---|---|---|---|---|
| Single bin | Synthetic input embedded in test; code-based hand expression | Allowable cycles (count) and dimensionless damage equal test's explicit expression to relative 1e-9; no licensed coefficients reproduced | `tests/riser_fatigue/test_touchdown.py::test_single_bin_hand_verified` | PASS at BASE and AFTER; [tests/riser_fatigue/test_touchdown.py:L24-L52] |
| DFF verdict | Synthetic input embedded in test | Damage unchanged at relative 1e-12; usage crosses dimensionless threshold 1 when DFF changes | `tests/riser_fatigue/test_touchdown.py::test_dff_governs_verdict` | PASS at BASE and AFTER; [tests/riser_fatigue/test_touchdown.py:L93-L121] |
| Durable example | `examples/workflows/riser-fatigue/input.yml`; synthetic supplied responses | 2 segments; wave and VIV damage each > 0; governing total in (0, 1), sum within pytest relative 1e-6; life exceeds design life (years) | `tests/riser_fatigue/test_riser_fatigue_workflow.py::test_riser_fatigue_example_runs_offline` | PASS at BASE and AFTER; [tests/riser_fatigue/test_riser_fatigue_workflow.py:L18-L50] [examples/workflows/riser-fatigue/README.md:L17-L31] |

Run record: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/riser_fatigue -q -rA -p no:cacheprovider --basetemp=<scratch>`; unmodified test files and example run in a scratch copy because they write beside the input/test; BASE and AFTER each 21 run, 21 passed, 0 failed, 0 skipped, 0 NOT_RUN, both on the header code revision with documentation-only changes. [tests/riser_fatigue/test_riser_fatigue_workflow.py:L47-L50] [tests/riser_fatigue/test_riser_fatigue_workflow.py:L104-L108]

## 5. QA gates

Gate selection: AQG r0, section 10, Python calculations; G5 applies to every family. The checks below test the supplied-response calculation contract; independent engineering qualification remains under #810 and #1588. (AQG r0, sections 9–11, Python calculations.)

| Gate | Applicability / route | Check IDs |
|---|---|---|
| G0 inputs, units, citations | Applicable: typed scalar/histogram inputs and cited DFF resolution | C01–C03 |
| G1 imports and tests | Applicable: offline Python imports and tests; package emits CSV/Markdown rather than solver decks, so no target-program load route is applicable | C04 |
| G2 hand or published check | Applicable: encoded single-bin hand expression and damage additivity regression; independent source qualification remains open | C05–C06 |
| G3 short dynamics | NOT_APPLICABLE: supplied stress responses, no dynamics solve | [src/digitalmodel/riser_fatigue/workflow.py:L8-L23] |
| G4 diffraction | NOT_APPLICABLE: no body mesh or diffraction result is calculated | [src/digitalmodel/riser_fatigue/workflow.py:L8-L23] |
| G5 evidence appendix | Applicable: report fields and pinned test/citation evidence | C07–C09 |

| ID / gate | Executable check and numeric pass criterion | Evidence |
|---|---|---|
| C01 / G0 | `tests/riser_fatigue/test_touchdown.py::test_mismatched_lengths_raise`: 1 mismatched histogram raises ValueError; 0 accepted mismatches (count) | [tests/riser_fatigue/test_touchdown.py:L220-L228] |
| C02 / G0 | `tests/riser_fatigue/test_resolve_dff.py::test_explicit_dff_is_validated_positive`: 2 nonpositive DFF inputs raise ValueError; 0 accepted invalid values (count) | [tests/riser_fatigue/test_resolve_dff.py:L29-L33] |
| C03 / G0 | `tests/riser_fatigue/test_resolve_dff.py::test_absent_dff_standalone_raises`: 1 missing citation context raises CitationResolutionError; 0 silent DFF fallbacks (count) | [tests/riser_fatigue/test_resolve_dff.py:L40-L44] |
| C04 / G1 | Run `tests/riser_fatigue/`: assess histogram, import router, resolve DFF, render report; 21 passed and 0 failed tests (count) | [tests/riser_fatigue/test_touchdown.py:L12-L17] [tests/riser_fatigue/test_riser_fatigue_workflow.py:L14-L50] [tests/riser_fatigue/test_resolve_dff.py:L24-L44] |
| C05 / G2 | Single-bin section-4 node: allowable cycles and damage agree with its written expression within relative 1e-9 (dimensionless ratio) | [tests/riser_fatigue/test_touchdown.py:L44-L52] |
| C06 / G2 | `tests/riser_fatigue/test_riser_fatigue_workflow.py::test_riser_fatigue_router_sums_wave_and_viv`: omitted contribution exactly 0 and segment total agrees with sum at pytest relative 1e-6 (dimensionless damage ratio) | [tests/riser_fatigue/test_riser_fatigue_workflow.py:L91-L102] |
| C07 / G5 | `tests/riser_fatigue/test_touchdown.py::test_report_markdown_contains_key_fields`: 5 assertions on standard, location, verdict, category and usage labels; 0 missing fields (count) | [tests/riser_fatigue/test_touchdown.py:L231-L246] |
| C08 / G5 | `tests/riser_fatigue/test_touchdown.py::test_report_states_the_applied_thickness_exponent`: 1 applied-exponent string present; 0 missing strings (count) | [tests/riser_fatigue/test_touchdown.py:L214-L217] |
| C09 / G5 | Resolve every pack citation at its declared pin; require 0 missing files/ranges, 3 regression nodes with recorded results, and 0 lint findings (count); keep header code pin with results | [tests/riser_fatigue/test_touchdown.py:L24-L52] [tests/riser_fatigue/test_touchdown.py:L93-L121] [tests/riser_fatigue/test_riser_fatigue_workflow.py:L21-L50] |

## 6. Known failure modes and pitfalls

sweep: 40 found

The count below is 40 reviewed sites/groups, including intentional fallbacks and unit assumptions, not 40 confirmed defects; multiple casts of the same internal result are grouped within one row. [src/digitalmodel/riser_fatigue/touchdown.py:L105-L154] [src/digitalmodel/riser_fatigue/touchdown.py:L165-L247] [src/digitalmodel/riser_fatigue/workflow.py:L44-L367]

Exact sweep commands (repository root; text only):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|get\(|mm|MPa|seconds|hours|year|cache' src/digitalmodel/riser_fatigue --glob '*.py'
rg -n 'except|continue|return None|return \[\]|return \{\}|cache|1970|0\.05' src/digitalmodel/riser_fatigue --glob '*.py'
```

| Site | Symptom / cause and classification | Existing detection / tracking | Evidence at code pin |
|---|---|---|---|
| T01 | Input defaults include neutral SCF, period and design life; selected standard constants are implicit in direct API use | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L105-L115] |
| T02 | Unassessed result holds empty bins, seven zero-valued quantities, FAIL and standard label; zeros are placeholders, not computed evidence | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L136-L154] |
| T03 | External arrays converted by NumPy float coercion; locale-formatted text is not normalised | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L165-L166] |
| T04 | Only equal shapes checked; dimensionality, nonempty bins, nonnegative and finite data lack local checks | C01 checks unequal shapes only; no detection check exists for other cases; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L165-L171] |
| T05 | Period positivity comparison does not reject NaN | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L172-L173] |
| T06 | DFF positivity comparison does not reject NaN | C02 covers finite nonpositive workflow DFF only; no detection check exists for NaN; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L174-L175] |
| T07 | Exponent and SCF float conversion has no finite/domain validation | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L181-L188] |
| T08 | Wall/reference thickness assumed in mm; direct section dimensions have no local positivity or geometry check | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L189-L194] |
| T09 | NaN cycles can yield NaN bin damage but zero summed damage and PASS; local casts do not reject it | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L200-L214] [src/digitalmodel/fatigue/damage.py:L33-L38] |
| T10 | Empty input can produce empty bins and zero total damage; zip assumes core preserves rows | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L197-L215] |
| T11 | Nonpositive annual damage maps to infinite life; negative cycles may produce a passing negative usage | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L217-L230] |
| T12 | Nonpositive allowable damage maps usage to infinity; defensive branch after DFF positivity check | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L223-L230] |
| T13 | SCF, DFF and life recast into result without finiteness checks | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L241-L246] |
| T14 | Report rounds stress, thickness, cycles and results; rounded display is not a provenance record | C07–C08 check labels only; no detection check exists for precision loss; #814 | [src/digitalmodel/riser_fatigue/touchdown.py:L267-L307] |
| T15 | Any infinite life is displayed as inf; report does not diagnose invalid input that caused it | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/touchdown.py:L303-L305] |
| W01 | Fixed seconds/year and hours/year conversion assumes a uniform year, not calendar exposure | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L44-L45] |
| W02 | Absent or null DFF delegates to cited getter; explicit zero is rejected, not defaulted | C02–C03; #1246 | [src/digitalmodel/riser_fatigue/workflow.py:L48-L62] |
| W03 | Missing/false settings becomes empty mapping; required life check then raises | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L65-L69] |
| W04 | Missing id/name becomes segment; duplicate ids can obscure summary identity | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L113-L114] |
| W05 | Missing material becomes an informational designation without qualification | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L133-L143] |
| W06 | Absent wave returns empty rows and zero damage intentionally; omission may hide missing response | C06 covers intentional wave-only/VIV-only cases; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L155-L157] |
| W07 | Wave stress/cycles use float lists and assume MPa/count; wave section dimensions use mm and positive-only conversion | no detection check exists for unit errors; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L159-L169] |
| W08 | Wave SCF and histogram period default to neutral factor and annual period; external float conversion is locale-insensitive | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L171-L181] |
| W09 | Wave period-to-life scaling assumes stationary histogram over years | C06 covers additive output only; no detection check exists for stationarity; #810 | [src/digitalmodel/riser_fatigue/workflow.py:L183-L202] |
| W10 | Wave frequency column uses NaN sentinel rather than a measured frequency | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L187-L200] |
| W11 | Absent or false VIV cases returns empty rows and zero damage intentionally | C06 checks intentional omission; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L212-L214] |
| W12 | VIV cycles assume Hz, seconds and annual exposure; supplied stress is used without wave SCF/thickness correction | no detection check exists for input correction provenance; #812 | [src/digitalmodel/riser_fatigue/workflow.py:L218-L228] |
| W13 | Internal total cast to float; pandas summation can conceal nonfinite bin damage upstream | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L226-L228] |
| W14 | CSV index coerced to int; result columns recast to float; assumes integer positional index and matching metadata | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L230-L245] |
| W15 | Missing or nonmapping sn_curve silently returns default category/environment | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L255-L266] |
| W16 | List elements use Python float without finite/nonnegative/locale validation | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L269-L273] |
| W17 | Exposure fraction wins over hours even when both supplied; float parsing uses Python conventions | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L276-L278] |
| W18 | Hours converted to fraction using fixed year duration; no units metadata or consistency comparison | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L279-L288] |
| W19 | Stress/frequency aliases and positive comparison accept NaN or positive infinity | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L291-L298] |
| W20 | Required positive scalar float conversion rejects nonpositive finite values but accepts NaN/infinity | C02 covers nonpositive DFF only; no detection check exists for nonfinite inputs; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L301-L308] |
| W21 | Nonpositive total damage becomes infinite fatigue life | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L311-L314] |
| W22 | Infinite numerator short-circuits division to infinity, propagating a potentially misleading margin | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L317-L320] |
| W23 | Default results path and current-directory fallback can write outside the intended input folder | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L331-L349] |
| W24 | Empty CSV write raises rather than caching an empty result; file writes use default encoding | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L352-L359] |
| W25 | ValueError in relative path rendering is swallowed into an absolute path string; output may reveal local paths | no detection check exists; issue pending (1) | [src/digitalmodel/riser_fatigue/workflow.py:L362-L367] |

The only `except` in the package is the path-rendering fallback W25; the sweep found no `continue`, `return None`, empty-dict error return, epoch fallback or cache-write path in the three source files. [src/digitalmodel/riser_fatigue/__init__.py:L1-L25] [src/digitalmodel/riser_fatigue/touchdown.py:L1-L317] [src/digitalmodel/riser_fatigue/workflow.py:L1-L367]

| Incident / pitfall | Detection and status | Evidence |
|---|---|---|
| #2165 historical class-exponent and lowercase lookup defects | Class-specific and lowercase regression tests exist; C04 runs them; this does not independently qualify licensed tables | [tests/riser_fatigue/test_touchdown.py:L158-L211] |
| #1588 and #2161 mixed standard editions | no detection check exists for qualified governing-edition selection; C08 reports the applied exponent only; #2161 is closed history, #1588 remains the open qualification reference | [src/digitalmodel/riser_fatigue/touchdown.py:L96-L102] [src/digitalmodel/riser_fatigue/touchdown.py:L148-L148] [src/digitalmodel/fatigue/sn_curves.py:L1-L10] |
| #810 / #812 stress-response provenance and detailed response | no detection check exists for agreement with measured or independently qualified solver responses | [src/digitalmodel/riser_fatigue/workflow.py:L19-L23] |
| #814 evidence report completeness | C07–C08 check current Markdown labels; no detection check exists for a signed, fingerprinted QA appendix | [src/digitalmodel/riser_fatigue/touchdown.py:L250-L317] |
| #1350 atlas freshness is a downstream concern | no detection check exists in this package; atlas queries are separate registered workflows, not the supplied-response calculation | [docs/registry/workflows.yaml:L56-L69] |

Observation date for this initial log: 2026-10-16; rows with missing checks are assigned to the maintainer through the referenced issue or draft.

## 7. Lessons log

| Date | Change reference | Lesson / evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Initial pack | Keep class-specific exponent selection and lowercase normalization in the regression set after #2165 | Sections 3, 5 and 6 distinguish code regression from qualified standards evidence | Not assigned; #810; [tests/riser_fatigue/test_touchdown.py:L158-L211] |

## 8. Ownership

Proposed role assignment: engineering calculations quality manager, deputy calculations quality manager, and producing engineering calculations function. Technical classification goes to the family quality manager; unresolved cross-family scope goes to the maintainer. Clearance is pending independent review; this table assigns no completed approval.

## 9. Dependencies

| Dependency / consumer | Actual route / version limitation | Evidence |
|---|---|---|
| NumPy, pandas, shared fatigue core | Array coercion, bin dataframe, S-N and Miner evaluation; package versions come from dependency metadata, not a solver version | [src/digitalmodel/riser_fatigue/touchdown.py:L31-L39] [src/digitalmodel/riser_fatigue/touchdown.py:L165-L201] |
| Riser database citation getter | Missing workflow DFF resolves through `get_riser_dff`; its template names DNV-OS-F201 revision 2010 and its getter cites Sec.5 | [src/digitalmodel/riser_fatigue/workflow.py:L48-L62] [src/digitalmodel/riser_database/getters.py:L45-L50] [src/digitalmodel/riser_database/getters.py:L122-L141] |
| Durable workflow registry | `riser-fatigue` names example, three output files, offline runtime and a registry test; separate atlas/query entries are downstream | [docs/registry/workflows.yaml:L46-L69] |
| Markdown consumers | `report_markdown` returns text with bin data, scalar results and deferred extraction notice | [src/digitalmodel/riser_fatigue/touchdown.py:L250-L317] |

### Solver names: complete package text inventory

| Mention | Dependency meaning; text inspection only | Evidence |
|---|---|---|
| OrcaFlex, two prose mentions | Possible upstream histogram source; extraction deferred, no package interface call | [src/digitalmodel/riser_fatigue/touchdown.py:L7-L12] |
| OrcaFlex, generated report text | Deferred stress extraction and rainflow | [src/digitalmodel/riser_fatigue/touchdown.py:L310-L315] |
| OrcaFlex / Shear7 / VIVANA | Deferred front ends producing response inputs; no solver version specified | [src/digitalmodel/riser_fatigue/workflow.py:L19-L23] |

### Implementation-map inventory and counted-family search

Map ref: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Citations to `docs/registry/implementation-map.yaml` in this pack resolve at that map commit; all other repository citations resolve at the header code revision. [docs/registry/implementation-map.yaml:L1-L4]

Exactly one map family has rows in this package: `sn-curves-and-miner-damage`, with 4 in-package rows, ranked first; all 392 rows in that family are inventoried below, including scripts, docs, examples and tests. Map relations are retained as classifications from the map, rather than endorsements of independent computation. [docs/registry/implementation-map.yaml:L18494-L18534]

The four in-package map rows are `touchdown.assess_touchdown_fatigue`, `workflow.router`, `workflow._viv_damage` and `workflow._curve_settings`; section 3 additionally names `_evaluate_segment` and `_wave_damage` and explains their delegated operations. [docs/registry/implementation-map.yaml:L18494-L18534] [src/digitalmodel/riser_fatigue/workflow.py:L106-L202]

Counted-family search: Python `re`, case-sensitive, each map signal exactly as stored, over every package `.py` file at the code pin; signal exclusion compares against `riser_fatigue` case-insensitively and excludes 0 signals. The three patterns are:

```text
rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life
S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve
thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges
```

| Hit definition | Classification | Evidence |
|---|---|---|
| `__init__` module statements | Docstring and re-exports; helper | [src/digitalmodel/riser_fatigue/__init__.py:L1-L25] |
| `touchdown` module statements | Documentation and imports; helper | [src/digitalmodel/riser_fatigue/touchdown.py:L1-L39] |
| `TouchdownFatigueInput`, `TouchdownFatigueResult` | Data holders; helpers | [src/digitalmodel/riser_fatigue/touchdown.py:L70-L115] [src/digitalmodel/riser_fatigue/touchdown.py:L136-L154] |
| `assess_touchdown_fatigue` | Listed map row | [src/digitalmodel/riser_fatigue/touchdown.py:L157-L247] |
| `report_markdown` | Serialises listed result; helper | [src/digitalmodel/riser_fatigue/touchdown.py:L250-L317] |
| `workflow` module statements | Documentation and imports; helper | [src/digitalmodel/riser_fatigue/workflow.py:L1-L45] |
| `router`, `_viv_damage`, `_curve_settings` | Listed map rows | [src/digitalmodel/riser_fatigue/workflow.py:L65-L103] [src/digitalmodel/riser_fatigue/workflow.py:L205-L245] [src/digitalmodel/riser_fatigue/workflow.py:L255-L266] |
| `_wave_damage` | Delegates to listed touchdown assessment and scales/exports its rows; helper already described in section 3 | [src/digitalmodel/riser_fatigue/workflow.py:L147-L202] |

Search result: 11 hit definitions, 0 further second implementations; the inventory and counted-family criterion pass. Whole-repository text search includes `scripts/`; hits outside this package are map follow-up evidence and do not change the counted result. [src/digitalmodel/riser_fatigue/__init__.py:L1-L25] [src/digitalmodel/riser_fatigue/touchdown.py:L1-L317] [src/digitalmodel/riser_fatigue/workflow.py:L1-L367]

Map follow-up: the map contains the package's S-N/Miner family; exposure-time scaling and DFF decision/provenance boundaries described in section 3 are not separate families in the map, so they are not separately counted; maintainers can extend the map if separate method-family tracking is needed, issue pending (2). [src/digitalmodel/riser_fatigue/workflow.py:L128-L142] [src/digitalmodel/riser_fatigue/workflow.py:L218-L228]

### Complete family inventory

Each row below is one implementation row from the declared map revision; a blank map symbol is written `(module)`. The inventory includes helpers and tests because the map includes them. [docs/registry/implementation-map.yaml:L20030-L20039]

| Row | Repository-relative path | Symbol | Map relation | Map citation |
|---|---|---|---|---|
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | independent | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | independent | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | independent | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | independent | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | example | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | example | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | example | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | example | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | example | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | example | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | example | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | example | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | example | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | example | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | example | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | example | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | example | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | example | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | example | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | example | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | example | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | example | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | example | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | example | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | example | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | example | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | example | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | example | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | example | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | example | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | independent | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | independent | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | independent | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | independent | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | independent | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | independent | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | independent | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | independent | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | independent | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | independent | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | independent | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | independent | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | independent | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | independent | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | independent | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | independent | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | caller | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | independent | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | independent | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | independent | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | independent | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | independent | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | independent | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | independent | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | independent | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | independent | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | independent | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | independent | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | independent | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | independent | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | independent | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | independent | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | independent | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | independent | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | independent | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | independent | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | independent | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | caller | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | caller | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | wrapper | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | wrapper | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | wrapper | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | independent | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | caller | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | independent | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | independent | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | independent | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | independent | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | independent | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | independent | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | independent | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | caller | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | independent | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | independent | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | independent | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | caller | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | independent | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | independent | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | independent | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | independent | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | caller | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | independent | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | independent | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | independent | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | caller | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | independent | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | independent | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | wrapper | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | wrapper | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | independent | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | independent | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | independent | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | independent | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | independent | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | independent | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | independent | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | independent | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | independent | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | independent | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | independent | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | caller | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | independent | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | independent | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | wrapper | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | wrapper | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | independent | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | independent | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | independent | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | independent | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | independent | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | independent | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | independent | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | independent | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | independent | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | independent | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | independent | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | independent | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | independent | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | independent | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | independent | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | independent | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | independent | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | independent | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | independent | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | independent | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | independent | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | independent | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | independent | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | independent | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | independent | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | independent | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | independent | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | independent | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | independent | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | independent | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | independent | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | independent | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | independent | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | independent | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | independent | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | independent | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | independent | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | independent | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | independent | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | independent | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | independent | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | copy | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | independent | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | independent | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | caller | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | independent | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | caller | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | independent | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | wrapper | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | independent | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | independent | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | independent | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | independent | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | independent | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | independent | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | independent | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | independent | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | independent | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | independent | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | wrapper | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | independent | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | independent | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | independent | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | independent | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | independent | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | independent | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | independent | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | independent | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | independent | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | independent | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | independent | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | independent | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | caller | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | caller | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | independent | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | independent | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | independent | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | independent | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | independent | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | independent | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | independent | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | independent | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | independent | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | independent | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | independent | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | independent | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | independent | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | independent | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | independent | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | independent | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | independent | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | caller | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | caller | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | caller | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | independent | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | independent | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | independent | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | independent | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | independent | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | independent | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | independent | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | independent | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | independent | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `(module)` | independent | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | caller | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | independent | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | independent | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | independent | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | independent | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | independent | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | independent | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | independent | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | independent | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | independent | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | independent | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | independent | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | caller | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | independent | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | independent | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | caller | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | independent | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | independent | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | independent | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | caller | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | independent | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | independent | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | independent | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | independent | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | independent | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | test_helper | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | test_helper | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | test_helper | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | test_helper | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | test_helper | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | test_helper | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | test_helper | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | test_helper | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | test_helper | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | test_helper | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | test_helper | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | test_helper | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | test_helper | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | test_helper | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | test_helper | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | test_helper | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | test_helper | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | test_helper | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | test_helper | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | test_helper | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | test_helper | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | test_helper | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | test_helper | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | test_helper | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | test_helper | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | test_helper | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | test_helper | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | test_helper | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | test_helper | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | test_helper | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | test_helper | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `(module)` | test_helper | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | test_helper | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | test_helper | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | test_helper | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | test_helper | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | test_helper | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | test_helper | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | test_helper | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | test_helper | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | test_helper | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | test_helper | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | test_helper | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | test_helper | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | test_helper | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | test_helper | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | test_helper | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | test_helper | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | test_helper | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | test_helper | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | test_helper | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | test_helper | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | test_helper | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | test_helper | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | test_helper | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | test_helper | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | test_helper | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | test_helper | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | test_helper | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | test_helper | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | test_helper | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | test_helper | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | test_helper | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | test_helper | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | test_helper | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | test_helper | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | test_helper | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | test_helper | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | test_helper | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | test_helper | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | test_helper | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | test_helper | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | test_helper | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | test_helper | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | test_helper | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | independent | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | independent | [docs/registry/implementation-map.yaml:L20939-L20948] |

## 10. Coverage and open items

### Files inspected and discovery coverage

The complete package source read is `__init__.py`, `touchdown.py` and `workflow.py`; there are no excluded source subpackages in this pack scope. [src/digitalmodel/riser_fatigue/__init__.py:L1-L25] [src/digitalmodel/riser_fatigue/touchdown.py:L1-L317] [src/digitalmodel/riser_fatigue/workflow.py:L1-L367]

Tests read and executed: all three `tests/riser_fatigue/` files, with 15 touchdown cases including parameter expansion, 4 DFF-resolution cases and 2 workflow cases; the inventory lead's approximate test count is superseded by this 21-node collection. [tests/riser_fatigue/test_touchdown.py:L24-L246] [tests/riser_fatigue/test_resolve_dff.py:L24-L44] [tests/riser_fatigue/test_riser_fatigue_workflow.py:L21-L108]

The durable workflow example and its README were read; the registry additionally points to a shared workflow test. [examples/workflows/riser-fatigue/README.md:L1-L31] [examples/workflows/riser-fatigue/input.yml:L1-L53] [docs/registry/workflows.yaml:L46-L55]

Documentation discovery command:

```bash
rg -l 'riser_fatigue|riser-fatigue' docs --glob '*.md' --glob '*.yml' --glob '*.yaml' --glob '*.html'
```

The following discovery files were inspected as routing, method or documentation context; references to this module do not qualify other modules' computations.

- `docs/riser_database.md`: [docs/riser_database.md:L20-L20]
- `docs/capability-map/capabilities-added.yml`: [docs/capability-map/capabilities-added.yml:L67-L67]
- `docs/registry/workflows.yaml`: [docs/registry/workflows.yaml:L46-L46]
- `docs/registry/module-routing.yaml`: [docs/registry/module-routing.yaml:L490-L490]
- `docs/cad-inventory/ecosystem-and-automation-surface.md`: [docs/cad-inventory/ecosystem-and-automation-surface.md:L81-L81]
- `docs/maps/digitalmodel-operator-map.md`: [docs/maps/digitalmodel-operator-map.md:L99-L99]
- `docs/registry/implementation-map.yaml`: [docs/registry/implementation-map.yaml:L17438-L17438]
- `docs/registry/implementation-map.md`: [docs/registry/implementation-map.md:L24-L24]
- `docs/cad-inventory/project-domain-map.md`: [docs/cad-inventory/project-domain-map.md:L48-L48]
- `docs/api/ffs/build-wave-2026-07.html`: [docs/api/ffs/build-wave-2026-07.html:L130-L130]
- `docs/plans/parametric-refresh/adding-an-atlas.md`: [docs/plans/parametric-refresh/adding-an-atlas.md:L15-L15]
- `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`: [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21]
- `docs/domains/risers/scr-slwr-coverage-2026-10.md`: [docs/domains/risers/scr-slwr-coverage-2026-10.md:L178-L178]
- `docs/domains/asset-integrity/ffs-offering-catalog.md`: [docs/domains/asset-integrity/ffs-offering-catalog.md:L97-L97]
- `docs/domains/offshore-engineering/README.md`: [docs/domains/offshore-engineering/README.md:L6-L6]
- `docs/domains/offshore-engineering/scf-to-dnv-c203-fatigue-procedure.md`: [docs/domains/offshore-engineering/scf-to-dnv-c203-fatigue-procedure.md:L132-L132]

### Open-item register

All open items below are assigned to the maintainer for issue tracking and to the engineering calculations quality manager for technical disposition. Draft references await issue creation before clearance.

| Item | Coverage / remaining action | Issue |
|---|---|---|
| Applicability (UNVERIFIED, section 1) | Establish qualified response/geometry/loading range and independent solver or measured anchors | #810, #812 |
| External input contract (UNVERIFIED, section 2) | Finite, nonnegative, nonempty, scalar/array shape, units, stress-range and exposure provenance; cover all missing detections in sweep | issue pending (1) |
| Governing standards (UNVERIFIED, section 3) | Reconcile exact titles/editions/clauses with lawful source access; retain governing citations in results; distinguish informational material from qualification | #1588, #810; issue pending (1) |
| Independent example | Section 4 has regression anchors only; no independently qualified worked example claimed | #810, #1588 |
| VIV response | Establish whether supplied ranges already include required SCF/thickness effects and document the boundary | #812 |
| Evidence appendix | Add input fingerprint, source basis and independent QA evidence to the result/report contract | #814 |
| Cross-check maintenance | Keep registry, citation-consumer and material-reference results separate from the three-file module suite; 33 cross-checks passed before and after | issue pending (2); [tests/fatigue/test_c203_citations.py:L26-L55] [tests/riser_database/test_material_refs.py:L22-L55] [tests/workflows/test_durable_workflows.py:L165-L193] |
| Atlas freshness | This pack does not regenerate or qualify downstream atlases | #1350 |
| Map follow-up | Preserve complete family inventory and consider separate policy/exposure-family tracking before counting them | issue pending (2) |
| Standards access | No licensed clause text read; no tabulated licensed parameter reproduced; engineering qualification remains open | #1588 |

Publication scope: use public code and synthetic tests only; exclude private input values and unassigned sidecar identifiers.
