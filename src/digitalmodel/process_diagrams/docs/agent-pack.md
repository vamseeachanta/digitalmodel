# Process diagrams knowledge pack

| Header field | Value |
| --- | --- |
| Module | process_diagrams |
| Repository | digitalmodel |
| Package path | src/digitalmodel/process_diagrams |
| Pack revision | 0, 2026-11-07 |
| Code revision | `309d343e08a2f83aebc6ea5344e586407b80c1f8` |
| Analysis family | EC — Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map commit | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations below resolve at the code revision; the implementation-map inventory uses the separate map commit above.

## 1. Purpose and scope

The package assembles standalone SVG piping and instrumentation diagrams from symbol fragments, pipe paths, parsed instrument tags, line identifiers, sheet frames, notes and title blocks. [src/digitalmodel/process_diagrams/__init__.py:L1-L8]

The registry assigns beta maturity and directs module tests to `tests/process_diagrams/`; maturity is a routing label rather than evidence of standards conformance. [docs/registry/module-routing.yaml:L387-L392]

The public API exports sheet, tag, geometry and annotation helpers; drawing assembly operates on strings and in-memory lists rather than a process-flow network or a hydraulic solver. [src/digitalmodel/process_diagrams/__init__.py:L28-L39] [src/digitalmodel/process_diagrams/sheet.py:L48-L93]

The sheet defaults to a construction restriction stamp, and its scale field is descriptive text; the renderer does not establish an engineering length scale. [src/digitalmodel/process_diagrams/sheet.py:L23-L36] [src/digitalmodel/process_diagrams/sheet.py:L179-L209]

## 2. Inputs and outputs

Input contracts are the implemented signatures and guards, rather than additional range validation. [src/digitalmodel/process_diagrams/geometry.py:L33-L85] [src/digitalmodel/process_diagrams/sheet.py:L48-L88]

| Input | Unit and convention | Allowed range or implemented check | Default | Source |
| --- | --- | --- | --- | --- |
| Sheet width, height and symbol coordinates | SVG user units; positive y points downward | Numeric arguments; no positivity or finiteness guard | Width and height required | [src/digitalmodel/process_diagrams/geometry.py:L12-L12] [src/digitalmodel/process_diagrams/sheet.py:L48-L57] [src/digitalmodel/process_diagrams/sheet.py:L123-L147] |
| Coordinate origin and orientation | SVG viewBox origin `(0, 0)`; drawing coordinates are absolute | No physical coordinate transform or phase argument | Origin fixed in viewBox | [src/digitalmodel/process_diagrams/sheet.py:L123-L138] [src/digitalmodel/process_diagrams/symbols.py:L1-L5] |
| Zone columns, rows | Integer counts | No explicit guard; row labels index a fixed letter string | 8 columns, 4 rows | [src/digitalmodel/process_diagrams/sheet.py:L20-L20] [src/digitalmodel/process_diagrams/sheet.py:L48-L57] [src/digitalmodel/process_diagrams/sheet.py:L148-L162] |
| TitleBlock fields | Text, including drawing identifier, title, revision, date and status | Required drawing identifier and title; remaining fields have text defaults | See dataclass; scale `NONE`, status `NOT FOR CONSTRUCTION` | [src/digitalmodel/process_diagrams/sheet.py:L23-L36] |
| Raw fragments, notes and accessibility label | SVG markup for fragments; plain text for notes and label | Fragments appended unchanged; text labels escaped when rendered | Empty collections and empty label | [src/digitalmodel/process_diagrams/sheet.py:L48-L63] [src/digitalmodel/process_diagrams/sheet.py:L90-L93] [src/digitalmodel/process_diagrams/sheet.py:L123-L140] [src/digitalmodel/process_diagrams/sheet.py:L165-L177] |
| Instrument tag | Dimensionless identifier | Normalised uppercase grammar, vocabulary checks and disambiguation; malformed strings raise TagError | Required | [src/digitalmodel/process_diagrams/tags.py:L125-L187] |
| Pipe line identifier | Nominal size in inches; other fields are codes | Quoted whole, decimal, fraction or mixed-fraction size; service, sequence, class and optional insulation grammar | Insulation absent unless supplied | [src/digitalmodel/process_diagrams/tags.py:L59-L65] [src/digitalmodel/process_diagrams/tags.py:L190-L239] |
| Horizontal/vertical crossings and polyline points | SVG user units | Strictly internal crossings sorted in travel direction; polyline needs at least 2 points | Empty crossings | [src/digitalmodel/process_diagrams/geometry.py:L23-L85] |
| Instrument bubble kind and radius | Enumerated code option; radius in SVG user units | Literal type annotation; no runtime rejection of unknown kind | `field`, radius 21 user units | [src/digitalmodel/process_diagrams/symbols.py:L29-L30] [src/digitalmodel/process_diagrams/symbols.py:L42-L80] |
| Control-valve actuator, fail annotation and orientation | Code option, text and boolean | Type annotation only; false-valued fail annotation recorded by Sheet helper | Diaphragm, `FC`, horizontal orientation | [src/digitalmodel/process_diagrams/symbols.py:L116-L153] [src/digitalmodel/process_diagrams/sheet.py:L79-L88] |
| Continuation label lines, direction, width | Text list, direction option and SVG user units | Only first 2 labels rendered; any non-right direction takes left branch | Right, width 150 user units | [src/digitalmodel/process_diagrams/sheet.py:L213-L239] |

| Output | Unit and meaning | Consumer or limit | Source |
| --- | --- | --- | --- |
| `Sheet.render()` | Standalone SVG string with viewBox and accessibility attributes | Inherits current colour; embeds raw elements without XML validation | [src/digitalmodel/process_diagrams/sheet.py:L123-L140] |
| Geometry and symbol functions | SVG path data or fragment strings | Schematic geometry, no physical dimension conversion | [src/digitalmodel/process_diagrams/geometry.py:L33-L103] [src/digitalmodel/process_diagrams/symbols.py:L1-L5] |
| `run_length(points)` | Sum of segment lengths in supplied coordinate units | A drawing-space takeoff unless caller supplies an independent physical scale | [src/digitalmodel/process_diagrams/geometry.py:L98-L103] [src/digitalmodel/process_diagrams/sheet.py:L23-L36] |
| `Tag`, `LineNumber` | Frozen dataclasses; line size in inches | Tag description, loop comparison, safety/trip predicates and insulation predicate | [src/digitalmodel/process_diagrams/tags.py:L72-L122] [src/digitalmodel/process_diagrams/tags.py:L190-L206] |
| `Sheet.lint()`, `Sheet.loops()`, `Sheet.note()` | Defect strings, loop-to-tag mapping, 1-based note count | Lint checks missing fail annotations and exact raw duplicate tags only | [src/digitalmodel/process_diagrams/sheet.py:L90-L120] |
| Legend example | UTF-8 SVG file | Optional caller-supplied output path; default output in current directory | [examples/process_diagrams/legend_sheet.py:L98-L106] |

## 3. Method and governing standards

| Method | Implementation and reference | Boundary |
| --- | --- | --- |
| Instrument identification | `parse_tag` normalises input, applies anchored regex and vocabulary checks, removes modifiers, then creates Tag | Disambiguation is expressly described as grammar policy requiring a drawing legend, rather than a quoted standard rule. [src/digitalmodel/process_diagrams/tags.py:L125-L187] |
| Line identification | `parse_line_number` matches a compound identifier and computes mixed/fractional size using float conversion and division | No physical-size range or denominator check. [src/digitalmodel/process_diagrams/tags.py:L209-L239] |
| Line crossing | `_hops_between`, `horizontal`, `vertical` sort interior crossings and place SVG arcs either side | Radius is a code drafting constant; endpoints are excluded. [src/digitalmodel/process_diagrams/geometry.py:L19-L72] |
| Run length | `run_length` sums square roots of squared coordinate differences | Closed-form Euclidean segment length, without unit conversion or point-count validation. [src/digitalmodel/process_diagrams/geometry.py:L98-L103] |
| Symbol construction | `bubble`, valve and actuator helpers, equipment and electrical helpers produce positioned SVG fragments | Drawing geometry and options are implementation conventions; rendered XML alone does not prove conformity to a standard. [src/digitalmodel/process_diagrams/symbols.py:L42-L290] |
| Sheet assembly and drafting lint | `Sheet` appends fragments, tags and notes; renderer emits frame, content, notes and title block | Lint does not compare safety tag classification with bubble geometry. [src/digitalmodel/process_diagrams/sheet.py:L60-L140] |

The explicit edition in package prose, the symbol docstring, the tag docstring and the legend note is ISA-5.1-2009; the sheet placement docstring uses the unqualified ISA-5.1 designation. [src/digitalmodel/process_diagrams/__init__.py:L21-L23] [src/digitalmodel/process_diagrams/symbols.py:L7-L11] [src/digitalmodel/process_diagrams/tags.py:L14-L16] [src/digitalmodel/process_diagrams/sheet.py:L65-L70] [examples/process_diagrams/legend_sheet.py:L88-L94]

UNVERIFIED: ISA-5.1 conformance, the governing clause/table identifiers and standards-derived symbol/identification provenance are not established; clause text not checked against the licensed copy; issue pending (4).

The package's standard references are prose and code vocabulary, rather than emitted Citation sidecars; no differing explicit edition is identified in those references. [src/digitalmodel/process_diagrams/__init__.py:L21-L39] [src/digitalmodel/process_diagrams/tags.py:L24-L65] [src/digitalmodel/process_diagrams/symbols.py:L7-L30] [src/digitalmodel/process_diagrams/sheet.py:L123-L140]

## 4. Validated worked examples

Results below are offline results at the code pin, with the same tests repeated on the documentation changes; assertion expectations are visible in the cited tests. [tests/process_diagrams/test_geometry.py:L21-L70] [tests/process_diagrams/test_sheet.py:L31-L170] [tests/process_diagrams/test_tags.py:L15-L112]

| Case and kind | Independent basis or regression source | Input and expected result | Tolerance and basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- | --- |
| V1 — validated Euclidean length | Closed-form arithmetic: square root of `(3 squared + 4 squared)`, then add a vertical length of 4 | Inline points `(0,0),(3,4)` give 5 user units; adding `(3,8)` gives 9 user units | Additional hand check: absolute error at most 1e-12 user units, observed 0.0 for both; existing test uses pytest.approx | `tests/process_diagrams/test_geometry.py::TestRuns::test_run_length_is_euclidean` | PASS at BASE and AFTER; expectations [tests/process_diagrams/test_geometry.py:L64-L66] |
| R1 — straight and crossing paths, regression | Literal path and arc-count assertions | Horizontal and vertical straight paths; 2 interior crossings produce 2 arcs | Exact string/count equality; 0 mismatches | `tests/process_diagrams/test_geometry.py::TestHops::test_run_without_crossings_is_a_straight_line`; `tests/process_diagrams/test_geometry.py::TestHops::test_each_crossing_inserts_one_arc` | PASS at BASE and AFTER; expectations [tests/process_diagrams/test_geometry.py:L21-L28] |
| R2 — sheet scaffold, regression | In-memory Sheet fixture, XML parser and viewBox assertion | Render an 800 by 600 user-unit sheet with one equipment fragment | 0 XML parse errors; exact viewBox | `tests/process_diagrams/test_sheet.py::TestRendering::test_renders_well_formed_svg` | PASS at BASE and AFTER; expectations [tests/process_diagrams/test_sheet.py:L31-L36] |
| R3 — compound line parsing, regression | Inline parser fixture | Five parsed fields and insulation predicate | Exact dataclass equality; 0 field mismatches | `tests/process_diagrams/test_tags.py::TestLineNumbers::test_splits_all_five_fields` | PASS at BASE and AFTER; expectations [tests/process_diagrams/test_tags.py:L80-L87] |
| R4 — representative symbols, regression | Library-generated fragments and standard-library XML parser | Assemble the 21 fragments selected by the test | 0 XML parse errors; this is not a symbol-conformance criterion | `tests/process_diagrams/test_sheet.py::TestSymbols::test_every_symbol_is_well_formed_xml` | PASS at BASE and AFTER; expectations [tests/process_diagrams/test_sheet.py:L148-L167] |

The legend script is a worked drafting example, but has no dedicated test node in the three module test files; its separate build/render/XML/lint check is C04 and its missing regression node is an open item, rather than another validated case. [examples/process_diagrams/legend_sheet.py:L59-L106] [tests/process_diagrams/test_geometry.py:L21-L70] [tests/process_diagrams/test_sheet.py:L31-L170] [tests/process_diagrams/test_tags.py:L15-L112]

## 5. QA gates

Gate allocation follows AQG r0, section 10, Python calculations, with G5 from section 9; criteria below are pack checks rather than claims that the module enforces every criterion. (AQG r0, sections 9–10.)

| Gate | Applicability and route | Module checks |
| --- | --- | --- |
| G0 | Applicable: identifier inputs, SVG units and drafting checks | C01, C06, C07, C08 |
| G1 | Applicable: Python import through pytest; SVG parsed by ElementTree; no separate native CAD loader is implemented | C02, C04; rendering route [src/digitalmodel/process_diagrams/sheet.py:L123-L140] |
| G2 | Applicable: independent closed-form length arithmetic and literal crossing geometry | C03, C09 |
| G3 | NOT_APPLICABLE: no time integrator in the drawing/rendering path | [src/digitalmodel/process_diagrams/sheet.py:L123-L140] [src/digitalmodel/process_diagrams/geometry.py:L33-L103] |
| G4 | NOT_APPLICABLE: SVG drafting path has no diffraction mesh or hydrostatic computation | [src/digitalmodel/process_diagrams/sheet.py:L123-L140] [src/digitalmodel/process_diagrams/symbols.py:L177-L290] |
| G5 | Applicable: retain source pin, test-node results, criteria, evidence digests and open limits; review signature remains unestablished | C05; issue pending (6) |

| Check | Executable operation | Numeric pass criterion and unit | Evidence and gate |
| --- | --- | --- | --- |
| C01 | Run malformed-tag and malformed-line tests in `tests/process_diagrams/test_tags.py` | All 8 parametrised malformed strings raise TagError; 0 unexpected accepts, unit: cases | Pytest node results; G0. [tests/process_diagrams/test_tags.py:L67-L77] [tests/process_diagrams/test_tags.py:L109-L112] |
| C02 | Import the package through the complete `tests/process_diagrams/` pytest invocation | 67 passed, 0 failed, 0 skipped, 0 NOT_RUN, unit: collected cases | BASE/AFTER logs at code pin; G1. [tests/process_diagrams/test_geometry.py:L21-L70] [tests/process_diagrams/test_sheet.py:L31-L170] [tests/process_diagrams/test_tags.py:L15-L112] |
| C03 | Evaluate `run_length` on V1 points; independently compute square root of 25 and add 4 | Absolute residual at most 1e-12 user units for expected 5 and 9; observed 0.0 each | Hand-check record plus V1 node; G2. [tests/process_diagrams/test_geometry.py:L64-L66] |
| C04 | Load `examples/process_diagrams/legend_sheet.py` with importlib, call build, render, parse with ElementTree and call lint; output only to scratch | 0 XML errors, 0 lint messages; viewBox exactly `0 0 1120 720`, unit: SVG user units; 3 legend columns, unit: groups | Scratch SVG and check log; G1. [examples/process_diagrams/legend_sheet.py:L20-L56] [examples/process_diagrams/legend_sheet.py:L59-L95] |
| C05 | Build a scratch evidence manifest from pin, test logs, hand/legend log, sweep and citation results; recompute each recorded SHA-256 | 9 check records and 0 digest mismatches or missing evidence files, unit: records/files | Manifest plus self-check record; G5. Provenance requirement: AQG r0, sections 9 and 13. |
| C06 | Run `test_ampersands_and_quotes_in_labels_are_escaped` and `test_connector_carries_line_number_and_destination` in the sheet tests | 0 XML parse errors and 0 missing escaped labels, unit: cases | Pytest node results; G0. [tests/process_diagrams/test_sheet.py:L113-L125] |
| C07 | Run missing-fail, clean-sheet and fail-open annotation tests in the sheet tests | Missing fail gives exactly 1 lint message; explicit fail cases give 0; output retains the annotation, unit: messages | Pytest node results; G0. [tests/process_diagrams/test_sheet.py:L84-L103] |
| C08 | Run `test_duplicate_tags_are_reported` in the sheet tests | Exactly 1 defect for 2 identical raw identifiers, unit: messages/devices | Pytest node result; G0. [tests/process_diagrams/test_sheet.py:L93-L96] |
| C09 | Run the geometry tests for interior and endpoint crossings and reverse travel | 2 interior arcs, 0 endpoint/outside arcs; 0 ordering assertion failures, unit: arcs/cases | Pytest node results; G2. [tests/process_diagrams/test_geometry.py:L26-L49] |

These checks cover ordinary code behaviour; they do not detect every invalid drawing input, as section 6 identifies. [src/digitalmodel/process_diagrams/sheet.py:L96-L113] [src/digitalmodel/process_diagrams/symbols.py:L60-L80] [src/digitalmodel/process_diagrams/tags.py:L219-L231]

## 6. Known failure modes and pitfalls

sweep: 23 found

The sweep classifies 23 findings below, including intentional defaults and empty results; a finding is not automatically a defect. All rows were inspected at the code pin on 2026-11-07. The commands were:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}' src/digitalmodel/process_diagrams
rg -n -i 'except|continue|return None|return \[\]|return \{\}|return ""|= 0(\.0)?\b|1970|\b(int|float)\(|units?|default|\.get\(|else|\[:2\]|0\.0?5|0\.5' src/digitalmodel/process_diagrams
```

The first command produced 0 matches; the package has no swallowed exception, skipped-record continue, explicit None/empty-list/empty-dict return, 1970 fallback or cache write in the inspected functions. Empty collections returned through variables and omitted strings are classified below. [src/digitalmodel/process_diagrams/geometry.py:L23-L124] [src/digitalmodel/process_diagrams/sheet.py:L60-L252] [src/digitalmodel/process_diagrams/symbols.py:L33-L290] [src/digitalmodel/process_diagrams/tags.py:L84-L239]

| ID | Symptom and cause at the pin | Detection check or gap | Tracking |
| --- | --- | --- | --- |
| S01 | `run_length` starts from 0.0 and silently returns zero for empty or single-point input. [src/digitalmodel/process_diagrams/geometry.py:L98-L103] | C03 covers ordinary lengths; no detection check exists for short input | issue pending (1) |
| S02 | `_n` converts integral floats to int, otherwise uses general float formatting; coordinates can lose precision and nonfinite values are not rejected. [src/digitalmodel/process_diagrams/geometry.py:L120-L124] | no detection check exists | issue pending (1) |
| S03 | SVG units are assumed, with downward y; nominal pipe size instead uses inches, without a drawing-scale conversion. [src/digitalmodel/process_diagrams/geometry.py:L12-L20] [src/digitalmodel/process_diagrams/tags.py:L190-L199] [src/digitalmodel/process_diagrams/sheet.py:L23-L36] | C03 checks drawing-space length; no detection check exists for physical-scale misuse | issue pending (1) |
| S04 | Crossings at/beyond endpoints are intentionally filtered; omitted crossings give straight paths. [src/digitalmodel/process_diagrams/geometry.py:L23-L30] [src/digitalmodel/process_diagrams/geometry.py:L33-L72] | C09 detects the documented endpoint behaviour | issue pending (1) |
| S05 | Duplicate or closely spaced crossings are sorted but not deduplicated or checked against jump radius, so arcs can overlap. [src/digitalmodel/process_diagrams/geometry.py:L19-L30] [src/digitalmodel/process_diagrams/geometry.py:L48-L72] | no detection check exists | issue pending (1) |
| S06 | Empty fragment/tag lists yield empty lint/loop outputs; raw added symbols bypass tag/fail tracking, so a clean lint does not establish drawing completeness. [src/digitalmodel/process_diagrams/sheet.py:L54-L63] [src/digitalmodel/process_diagrams/sheet.py:L96-L120] | C07 covers tracked valves only; no detection check exists for raw-fragment completeness | issue pending (2) |
| S07 | Omitted valve fail annotation becomes the default text; false-valued explicit annotations are recorded, but arbitrary nonempty strings are accepted. [src/digitalmodel/process_diagrams/sheet.py:L79-L88] [src/digitalmodel/process_diagrams/symbols.py:L116-L137] | C07 detects missing annotation only when passed explicitly; no detection check exists for unspecified intent or invalid nonempty text | issue pending (2) |
| S08 | Duplicate counter defaults to zero and keys by raw spelling, so differently cased tags evade duplicate lint. [src/digitalmodel/process_diagrams/sheet.py:L106-L112] [src/digitalmodel/process_diagrams/tags.py:L147-L150] [src/digitalmodel/process_diagrams/tags.py:L179-L186] | C08 detects exact duplicates; no detection check exists for case variants | issue pending (2) |
| S09 | Safety-tag and bubble-kind correspondence is expressly not checked because geometry kind is not stored against the tag. [src/digitalmodel/process_diagrams/sheet.py:L101-L104] | no detection check exists | issue pending (2) |
| S10 | Empty notes intentionally omit the notes column; no warning requires a legend or design-basis note. [src/digitalmodel/process_diagrams/sheet.py:L165-L177] | C02 includes the no-notes regression; no detection check exists for required engineering notes | issue pending (6) |
| S11 | Zone defaults are 8 columns and 4 rows; no guard limits rows to available letters, and excessive rows can raise IndexError. [src/digitalmodel/process_diagrams/sheet.py:L20-L20] [src/digitalmodel/process_diagrams/sheet.py:L48-L57] [src/digitalmodel/process_diagrams/sheet.py:L148-L162] | no detection check exists for invalid grid settings | issue pending (1) |
| S12 | Default title metadata includes empty optional fields and a construction restriction; scale is documented as fixed but renderer uses caller text without validation. [src/digitalmodel/process_diagrams/sheet.py:L23-L36] [src/digitalmodel/process_diagrams/sheet.py:L196-L209] | C02 covers default stamp; no detection check exists for inappropriate overridden metadata | issue pending (2) |
| S13 | Any connector direction other than right takes the left branch. [src/digitalmodel/process_diagrams/sheet.py:L213-L230] | no detection check exists for invalid direction | issue pending (3) |
| S14 | Connector labels are silently truncated to 2; an empty list produces a shape without text. [src/digitalmodel/process_diagrams/sheet.py:L231-L239] | C06 covers 2 valid labels; no detection check exists for label loss | issue pending (3) |
| S15 | Unknown bubble kinds silently render the fallback circle; empty bottom text selects the single-label layout. [src/digitalmodel/process_diagrams/symbols.py:L58-L80] | no detection check exists for unknown kind; C02 covers selected declared kinds | issue pending (3) |
| S16 | `_actuator` falls back to the manual geometry for any unmatched kind. [src/digitalmodel/process_diagrams/symbols.py:L140-L153] | no detection check exists | issue pending (3) |
| S17 | Fixed drafting dimensions, default radii/orientations and default head depth proportions are code conventions rather than input-specific design calculations. [src/digitalmodel/process_diagrams/geometry.py:L19-L20] [src/digitalmodel/process_diagrams/symbols.py:L33-L80] [src/digitalmodel/process_diagrams/symbols.py:L86-L117] [src/digitalmodel/process_diagrams/symbols.py:L177-L204] [src/digitalmodel/process_diagrams/symbols.py:L216-L290] [src/digitalmodel/process_diagrams/sheet.py:L142-L210] [src/digitalmodel/process_diagrams/sheet.py:L213-L247] | C02/R4 parse representative output; no detection check exists for independent symbol fidelity | issue pending (4) |
| S18 | `packed_beds or []` draws no internals for missing/empty input, without a warning; vessel heads default from half-dimensions. [src/digitalmodel/process_diagrams/symbols.py:L177-L213] | C02/R4 includes one nonempty packed-bed case; no detection check exists for omitted required internals | issue pending (6) |
| S19 | `Tag.describe` uses unknown-variable/modifier/function fallbacks for directly constructed dataclasses; optional modifier strings collapse to empty parts. [src/digitalmodel/process_diagrams/tags.py:L84-L115] | no detection check exists for invalid direct construction | issue pending (6) |
| S20 | Tag parsing normalises grammar but retains raw spelling; raw tag strings are also split for rendering, so normalisation and displayed spelling differ. [src/digitalmodel/process_diagrams/tags.py:L147-L150] [src/digitalmodel/process_diagrams/tags.py:L179-L187] [src/digitalmodel/process_diagrams/sheet.py:L74-L77] | C02 includes normalised letters; no detection check exists for display/duplicate consistency | issue pending (2) |
| S21 | External size text uses float conversion at all 3 branches; regex rejects comma/grouped forms, but zero denominator escapes as ZeroDivisionError and zero size is accepted. [src/digitalmodel/process_diagrams/tags.py:L59-L65] [src/digitalmodel/process_diagrams/tags.py:L219-L239] | C01 checks malformed grammar, not numeric domain; no detection check exists for zero denominator/size | issue pending (1) |
| S22 | Modifier-only tags can pass the empty-function guard; absent modifiers and insulation use None/False defaults, not missing-input errors. [src/digitalmodel/process_diagrams/tags.py:L72-L82] [src/digitalmodel/process_diagrams/tags.py:L157-L187] [src/digitalmodel/process_diagrams/tags.py:L190-L203] | C02 covers intended safety/optional-insulation cases; no detection check exists for unintended modifier-only tags | issue pending (2) |
| S23 | Raw fragments and marker identifier are inserted unescaped; the render join omits empty parts without validating the assembled XML. [src/digitalmodel/process_diagrams/sheet.py:L60-L63] [src/digitalmodel/process_diagrams/sheet.py:L123-L140] | C06 checks selected escaped text fields only; no detection check exists for malformed raw fragments/marker identifiers | issue pending (2) |

No incident issue was named for this module; repository issue searches for the module, ISA designation and named parser/connector functions found no confirmed matching defect issue. The tracking references above are drafts for the maintainers, not existing issue numbers.

## 7. Lessons log

| Date | Entry | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | An empty drafting-lint result covers only tracked fail annotations and raw duplicates, not symbol correctness or complete XML. [src/digitalmodel/process_diagrams/sheet.py:L60-L88] [src/digitalmodel/process_diagrams/sheet.py:L96-L140] | Separate ordinary regression checks from uncovered drawing defects in sections 4–6 | Pending; issue pending (6) |

## 8. Ownership

| Responsibility | Review routing proposed by this pack |
| --- | --- |
| Family review | Engineering calculations quality manager, on a different provider from the producer |
| Deputy review | Engineering calculations deputy quality manager; assignment to be confirmed, issue pending (6) |
| Production | Module maintainers |
| Engineering ambiguity | Family quality manager for standards edition, symbol meaning, units or changed tolerance; maintainer for unresolved cross-module scope |
| Clearance | DRAFT until the independent review and issue references are complete |

These rows specify review responsibilities; they record no completed review or approval. (AQG r0, section 11.)

## 9. Dependencies and parallel implementations

The implementation-map inventory at the map commit in the header contains 13 families and 0 implementation rows whose path lies within this package. The module-family inventory is therefore empty; the counted-family search is `NOT_APPLICABLE` because no map family has an in-package row. These are computed inventory results at the named map revision, rather than a claim of uniqueness.

| Inventory item | Result at the named map revision |
| --- | --- |
| Module map families | 0 |
| Implementation inventory | Empty: 0 listed rows |
| Ranked counted families | Empty; no tie-breaking or signal exclusion required |
| Counted-family grep | NOT_APPLICABLE: no in-package rows |
| Further second implementations under counted families | NOT_APPLICABLE: no counted families |
| Map follow-up | Add tag/line parsing, crossing geometry/run length, SVG symbol generation and sheet assembly/lint before counting them; issue pending (5) |

The map follow-up methods are implemented by the parser functions, geometry functions, symbol helpers and Sheet methods recorded in section 3. [src/digitalmodel/process_diagrams/tags.py:L125-L239] [src/digitalmodel/process_diagrams/geometry.py:L23-L124] [src/digitalmodel/process_diagrams/symbols.py:L33-L290] [src/digitalmodel/process_diagrams/sheet.py:L60-L252]

The additional whole-repository search, including `scripts/`, used the following discovery command at the code pin; its matches outside this package, its tests and its example concerned unrelated waterline crossings, rather than these identifier or SVG drafting methods:

```bash
rg -n 'process_diagrams|ISA-5.1|line.crossing|parse_line_number|HOP_RADIUS|def bubble' src tests scripts examples --glob '*.py'
```

This vocabulary search is a discovery result, not proof that every algorithmic duplicate is found; broadening it and registering search signals remains issue pending (5).

| Dependency direction | Surface and contract | Evidence |
| --- | --- | --- |
| Internal upstream | Geometry formatting/escaping and tag parser feed symbol/sheet assembly | [src/digitalmodel/process_diagrams/symbols.py:L18-L18] [src/digitalmodel/process_diagrams/sheet.py:L17-L18] [src/digitalmodel/process_diagrams/sheet.py:L72-L88] |
| Runtime upstream | Standard-library dataclasses, re and collection/typing declarations; no CAD/plotting dependency in the five package files | [src/digitalmodel/process_diagrams/__init__.py:L7-L8] [src/digitalmodel/process_diagrams/geometry.py:L15-L17] [src/digitalmodel/process_diagrams/tags.py:L19-L22] [src/digitalmodel/process_diagrams/sheet.py:L13-L18] [src/digitalmodel/process_diagrams/symbols.py:L14-L18] |
| Downstream example | Legend build consumes symbols and Sheet, then main writes UTF-8 SVG | [examples/process_diagrams/legend_sheet.py:L17-L18] [examples/process_diagrams/legend_sheet.py:L59-L106] |
| Test tools | pytest assertions and ElementTree parsing; inline inputs rather than private data fixtures | [tests/process_diagrams/test_geometry.py:L5-L14] [tests/process_diagrams/test_tags.py:L5-L12] [tests/process_diagrams/test_sheet.py:L5-L15] [tests/process_diagrams/test_sheet.py:L148-L167] |
| Routing | Existing module registry and operator-map row point to the package, test directory and domain documentation | [docs/registry/module-routing.yaml:L387-L392] [docs/maps/digitalmodel-operator-map.md:L59-L59] |

## 10. Coverage and open items

Read coverage: all five package Python files, all four files of `tests/process_diagrams/` (including the empty initializer), and the legend example; the module has no package-local README, tests folder or pre-existing docs subtree at the code pin. Documentation discovery under `docs/` found only the routing registry and the operator-map row naming the module; their linked domain-layout README and the documentation entry point were also read. [src/digitalmodel/process_diagrams/__init__.py:L1-L39] [src/digitalmodel/process_diagrams/geometry.py:L1-L124] [src/digitalmodel/process_diagrams/sheet.py:L1-L256] [src/digitalmodel/process_diagrams/symbols.py:L1-L290] [src/digitalmodel/process_diagrams/tags.py:L1-L239] [docs/registry/module-routing.yaml:L387-L392] [docs/maps/digitalmodel-operator-map.md:L59-L59] [docs/domains/README.md:L39-L50] [docs/README.md:L7-L30]

| Test file | Collected cases | BASE result | AFTER result | Assertion coverage |
| --- | --- | --- | --- | --- |
| `tests/process_diagrams/test_geometry.py` | 14 | 14 PASS | 14 PASS | Paths, hops, run lengths and number formatting. [tests/process_diagrams/test_geometry.py:L21-L70] |
| `tests/process_diagrams/test_sheet.py` | 26 | 26 PASS | 26 PASS | Scaffold, lint, annotations and selected symbols. [tests/process_diagrams/test_sheet.py:L31-L170] |
| `tests/process_diagrams/test_tags.py` | 27 | 27 PASS | 27 PASS | Tag grammar, safety flags, loop grouping and line identifiers. [tests/process_diagrams/test_tags.py:L15-L112] |

The test command form is `PYTHONPATH=src uv run --no-sources python -m pytest tests/process_diagrams -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, solver selection or test exclusion was used. BASE ran on the code revision in the header; AFTER ran on its uncommitted documentation changes, before the runner creates the commit. No module test was skipped or NOT_RUN. These counts are observed execution records, not the earlier inventory estimate.

| Open item | Evidence and missing work | Responsible role | Tracking |
| --- | --- | --- | --- |
| Geometry and size validation | Cover S01–S05, S11 and S21; define coordinate precision, finite values, size and grid ranges. [src/digitalmodel/process_diagrams/geometry.py:L23-L124] [src/digitalmodel/process_diagrams/tags.py:L219-L239] [src/digitalmodel/process_diagrams/sheet.py:L148-L162] | Maintainer | issue pending (1) |
| Drafting-lint coverage | Cover raw content, safety classification, case variants, metadata and modifier-only inputs in S06–S09, S12, S20, S22–S23. [src/digitalmodel/process_diagrams/sheet.py:L60-L140] [src/digitalmodel/process_diagrams/tags.py:L157-L187] | Maintainer | issue pending (2) |
| Lossy connector and option fallbacks | Cover S13–S16 with explicit option and label-count policy. [src/digitalmodel/process_diagrams/sheet.py:L213-L239] [src/digitalmodel/process_diagrams/symbols.py:L58-L80] [src/digitalmodel/process_diagrams/symbols.py:L140-L153] | Maintainer | issue pending (3) |
| Standards provenance | U1, the single UNVERIFIED claim in section 3: establish ISA-5.1 edition applicability, clause/table locators, provenance and independent symbol checks; clause text not checked against the licensed copy. [src/digitalmodel/process_diagrams/symbols.py:L7-L11] [src/digitalmodel/process_diagrams/tags.py:L14-L16] | Family quality manager | issue pending (4) |
| Implementation-map coverage | Inventory is empty at the named map revision; register all section-3 method families and broaden repository discovery | Maintainer | issue pending (5) |
| Evidence and example coverage | Add a dedicated legend test node, broaden default/empty/option tests (S10, S18–S19), define durable evidence records and confirm review/deputy/shared-log assignments; no completed review is asserted. [examples/process_diagrams/legend_sheet.py:L59-L106] [tests/process_diagrams/test_sheet.py:L131-L170] | Maintainer and family quality manager | issue pending (6) |

The calculation-citation guidance was readable; this pack introduces no standards-derived numerical constant or code change. Standards conformance and output provenance remain in the standards item above. No private sidecar is needed for the public synthetic cases listed here. [tests/process_diagrams/test_geometry.py:L21-L70] [tests/process_diagrams/test_sheet.py:L148-L167]
