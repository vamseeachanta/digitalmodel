# OrcaFlex BOM utility knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/utils |
| Repository | digitalmodel |
| Package path | src/digitalmodel/solvers/orcaflex/utils |
| Pack revision | 0 |
| Code revision | b47e42fbe7208c69b28dd436aa91fb129bc385f7 |
| Analysis family | OrcaFlex riser and mooring |
| Quality role | Riser and mooring quality manager |
| Status | DRAFT |

Citations resolve at the code revision above unless an implementation-map revision is explicitly given.

## 1. Purpose and scope

`BOMValidator` checks leading bytes, adds a missing byte-order mark (BOM), and removes one leading BOM when two occur consecutively; the package exports that class. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L14-L101] [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L7-L9]

`validate_directory` prints per-file diagnostics and counts, optionally attempting repairs; `main` supplies the directory, pattern and repair flag. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L104-L188]

The checker reads six bytes and does not decode the remaining content, parse YAML, resolve references, select a solver version, or compute physical responses; a successful prefix check therefore establishes only its byte-prefix predicate. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43]

The containing `solvers` domain is registered as beta; this is domain metadata, rather than measured qualification of the utility. [docs/registry/module-routing.yaml:L476-L481]

UNVERIFIED: the repository's claim that every target-version OrcaFlex YAML file requires a BOM has not been checked against a vendor source or a target-version load; see section 10, item O3. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L11-L16]

## 2. Inputs and outputs

### Inputs

| Name | Unit | Allowed range and default | Source and convention |
| --- | --- | --- | --- |
| `check_file(filepath)` | filesystem path; content measured in bytes | Any path accepted by binary `open`; no argument default; reads at most 6 bytes | Caller; no text decoding or physical coordinate convention is applied. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L21-L43] |
| `fix_double_bom(filepath)` | filesystem path; bytes | No default; reads the entire file; changes it only when it starts with two BOMs | Caller; writes back to the same path. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L46-L72] |
| `add_bom(filepath)` | filesystem path; bytes | No default; reads the entire file; adds a prefix only if absent | Caller; writes back to the same path. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L75-L101] |
| `directory`, `pattern`, `fix` | path, glob string, Boolean | Pattern defaults to `*.yml`; `fix` defaults to `False`; an empty match returns after a message | `Path.glob(pattern)`; default pattern does not recurse or include `.yaml`; CLI rejects a directory that does not exist. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L104-L118] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L163-L184] |

### Outputs

| Name | Unit | Range and meaning | Destination and convention |
| --- | --- | --- | --- |
| `check_file` result | Boolean plus string | `True` for a single leading BOM unless the six-byte double prefix matches first; `False` for double/missing prefixes or read errors | Return tuple; missing-prefix messages expose the first three bytes in hexadecimal. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] |
| Repair result and content | Boolean; bytes | `True` when written; `False` for no change or error; add delta is +3 bytes, removal delta is -3 bytes | Original file overwritten; exceptions print to stderr. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L17-L18] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L56-L101] |
| Directory summary | file counts | Valid and invalid counts reflect the initial check; fixed counts successful write calls; no post-repair validation | stdout; function has no structured return value. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L123-L153] |
| CLI status | integer exit code | Explicit failure exit is 1 only for an invalid directory; invalid-file counts do not set failure status | stderr for invalid directory; otherwise directory diagnostics on stdout. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L180-L188] |

Units, origin, axes, sign and phase of model quantities remain uninterpreted because the implementation operates on file bytes rather than model fields. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L40] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L56-L97]

## 3. Method and governing standards

| Method | Implementation and result | Reference basis |
| --- | --- | --- |
| Prefix classification | Compare the first 6 bytes against `DOUBLE_BOM`, then test `SINGLE_BOM`; everything else is missing-BOM or read-error status. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L17-L43] | Repository byte predicate; not a YAML or program-load check. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] |
| Remove one prefix | For a double prefix, retain `content[3:]`; the suffix is copied without decoding. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L56-L66] | A documentation example implements the same slice and binary overwrite. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L104-L117] |
| Add one prefix | Prepend `SINGLE_BOM` when absent, otherwise return `False`; no duplicate-prefix normalization occurs. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L85-L101] | Repository implementation; no engineering equation or standards-derived coefficient is used. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L17-L101] |
| Directory orchestration | Sort glob matches, check each, select repair from the diagnostic string, count successful writes. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L113-L153] | Delegates byte decisions to the class methods. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L127-L146] |

No governing engineering standard, edition, clause or calculation Citation sidecar is named in the utility; its numeric constants describe bytes, byte counts and diagnostic layout. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188]

## 4. Validated worked examples

Validated examples: **0**; regression examples with existing test node IDs: **0**. A repository search for `validate_bom`, `BOMValidator`, `fix_double_bom`, and the package's dotted and slash paths found no matching test; the package itself consists of the export and utility source below. Temporary-file observations in section 5 are checks, not independently sourced worked examples or durable test cases; missing coverage is item O2. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188]

The existing routing test `tests/docs/test_digitalmodel_routing_contract.py::test_registry_preserves_solver_submodule_routes` checks registry metadata, not BOM behavior; it compares the complete solver mapping to a fixed inventory. [tests/docs/test_digitalmodel_routing_contract.py:L217-L254]

## 5. QA gates

The following are acceptance checks for this pack, not evidence of solver clearance. G0 through G3 and G5 remain applicable to a consuming riser/mooring analysis; G4 is conditional on new diffraction data. (AQG r0, sections 3, 10 and 11.)

| Gate | Applicability | Check IDs and current evidence |
| --- | --- | --- |
| G0 inputs | Applicable to byte preparation; complete model validation is a separate requirement | B1-B8 below; prefix success is narrower than YAML, reference, unit or schema validity. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] |
| G1 load | Applicable downstream; NOT_RUN, licensed program/version unavailable | L1; recheck the prepared bytes, then load the file in the target OrcaFlex version using its licensed `LoadData` route; retain version and diagnostic record. (AQG r0, section 5.) |
| G2 statics | Applicable downstream; NOT_RUN, requires licensed solver and a model | S1; recheck every prepared input and attach convergence, component and hand-check records; this utility supplies no statics reference. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L104-L153] (AQG r0, section 6.) |
| G3 short dynamics | Applicable downstream; NOT_RUN, requires licensed solver and converged model | D1; retain the same prepared-file fingerprint and short-run record. (AQG r0, section 7.) |
| G4 diffraction | NOT_APPLICABLE to this utility alone: it has no vessel-data or mesh operation; activate R1 when a consumer uses new diffraction data | R1 is a conditional record check; no diffraction result is asserted. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188] (AQG r0, sections 8 and 10.) |
| G5 appendix | Applicable to evidence accompanying downstream reports | E1; distinguish byte-check records from program qualification; no report appendix is generated by the utility. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L148-L153] (AQG r0, section 9.) |

| ID | Executable check and numeric pass criterion | Unit | Evidence or result |
| --- | --- | --- | --- |
| B1 | Call `check_file` on single, double, missing and empty prefixes; require 0 Boolean mismatches against `[True, False, False, False]`. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] | cases | PASS: 4/4 observed; checks current prefix behavior only. |
| B2 | Call `add_bom` on a missing prefix and a single prefix; require +3 bytes and 0 changed suffix bytes for the first, 0 changed bytes and a false return for the second. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L85-L101] | bytes | PASS: 2/2 observations; mutation confined to temporary files. |
| B3 | Call `fix_double_bom` on two prefixes; require -3 bytes, 0 changed suffix bytes and 1 successful subsequent `check_file`. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L56-L72] | bytes; checks | PASS: 1/1 observation. |
| B4 | Give all 3 methods a nonexistent path; require 3 false results, 1 read-error message and 2 stderr messages; separately distinguish no-change false returns by rereading bytes. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L42-L43] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L68-L72] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L97-L101] | outcomes; messages | PASS for diagnostic behavior; no distinct repair-error return code exists. |
| B5 | Place an invalid UTF-8 byte after a single BOM; compare `check_file` with strict `bytes.decode('utf-8-sig')`; require 0 malformed suffixes accepted before model use. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L40] | accepted files | FAIL: prefix check accepts 1 malformed file; issue pending (1). |
| B6 | Repair a triple prefix once, then call `check_file`; require 0 remaining duplicate prefixes before model use, irrespective of the repair Boolean. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L60-L68] | duplicate prefixes | FAIL: 1 duplicate prefix remains; issue pending (1). |
| B7 | Run the CLI on an invalid file and an empty match; require a nonzero exit code for invalid input and at least 1 checked file before accepting a directory record. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L113-L118] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L127-L153] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L180-L188] | exit code; files | FAIL: both processes exit 0; invalid-file failure is not propagated and an empty match checks 0 files; issue pending (1). |
| B8 | Reread every matched file after a repair; require 0 invalid files and equality between an independently counted expected manifest and the glob count, difference 0 files; require all 12 sweep sites in section 6 to be classified. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L113-L153] | files; sites | Manual control, not implemented by directory summary; no detection check exists for partial writes or unread files outside the glob; issue pending (1). |
| L1 | For every BOM-prepared model, require 1 valid prefix check, target-version load errors 0, warnings 0 and object-count difference 0 against the model manifest. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] (AQG r0, section 5.) | checks; messages; objects | NOT_RUN: licensed target-version load required. |
| S1 | For models using BOM-prepared inputs, require 0 changed input fingerprints, 0 unconverged models and 0 unchecked components; compare solver top tension to an independent hand reference within 5 %. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L127-L146] (AQG r0, section 6.) | fingerprints; models; objects; percent | NOT_RUN: licensed statics and independent physical input required. |
| D1 | With the same BOM-prepared inputs, run 1 s of dynamics with 0 errors, 0 warnings and 0 nonnumeric values; tension deviation from statics at most 25 %. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L127-L146] (AQG r0, section 7.) | seconds; messages; values; percent | NOT_RUN: licensed dynamics required. |
| R1 | If vessel data originate in new diffraction, require 1 linked G4 record for each imported vessel-data set and 0 unresolved record fingerprints. (AQG r0, sections 8 and 10.) | records; fingerprints | NOT_APPLICABLE to this byte-only utility; conditional downstream control. |
| E1 | In a consuming report, require 0 result claims supported only by a BOM prefix check and 0 missing applicable gate records; preserve the utility's before/after byte digests and repair outcomes. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L148-L153] (AQG r0, section 9.) | claims; records | NOT_RUN: no consuming report supplied. |

Reproduction recipe for B1-B7: load this source file directly with `importlib.util.spec_from_file_location`, not a solver package; use `tempfile.TemporaryDirectory` beneath the configured temporary root, write synthetic byte strings, capture diagnostics, and invoke the source CLI with the current Python executable. Never repair repository input files during a check. The tested methods use only binary file access, path globbing and standard-library CLI support. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L8-L11] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L101] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L156-L188]

Self-check observations at the code pin: 13 assertions reproduced current behavior (4 prefix classifications, 2 additions, 1 double repair, 1 combined error case, 1 malformed-suffix case, 1 triple-prefix case and 3 CLI cases); an assertion confirming a defect is evidence of that defect, not a PASS of B5-B7's acceptance criteria. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L101] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L113-L188]

## 6. Known failure modes and pitfalls

sweep: 12 found

The sweep counts distinct handling/default sites, including intentional counters and diagnostic false returns; it does not describe every matching diagnostic `print` as a failure. Commands run at the code pin:

```bash
rg -n -i 'except|continue|return None|return \[\]|return \{\}|return False|= 0(\.0)?\b|1970|0\.05|5 ?%|int\(|float\(|units?|cache' src/digitalmodel/solvers/orcaflex/utils --glob '*.py'
rg -n 'def |default=|return|open\(|read\(|write\(|glob\(' src/digitalmodel/solvers/orcaflex/utils --glob '*.py'
```

| Site | Symptom and classification | Detection and follow-up |
| --- | --- | --- |
| F1: read exception | Error becomes `(False, message)` rather than an exception; diagnostic is retained. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L42-L43] | B4; caller must inspect the message; issue pending (1). |
| F2: double-repair no-change | `False` means no matching double prefix. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L60-L68] | B3/B4; distinguish unchanged bytes from F3; issue pending (1). |
| F3: double-repair exception | Error prints to stderr and returns the same `False` as no change. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L70-L72] | B4 exercises missing input; no detection check exists for a partial write; issue pending (1). |
| F4: add no-change | Existing single or multiple BOM prefixes return `False`. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L89-L97] | B2/B6; do not interpret false as validated input; issue pending (1). |
| F5: add exception | Error prints to stderr and returns the same `False` as no change. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L99-L101] | B4 exercises missing input; no detection check exists for a partial write; issue pending (1). |
| F6: empty glob | No files prints a message and returns without a failure result. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L113-L118] | B7; require a nonempty external manifest; issue pending (1). |
| F7: valid counter default | Zero is an intentional initial count, not successful validation of an empty directory. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L123-L134] | B7/B8; issue pending (1). |
| F8: invalid counter default | Zero is incremented before any repair, so it does not report remaining invalid files. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L124-L144] | B6/B8; independently recheck bytes; issue pending (1). |
| F9: fixed counter default | Zero counts write calls returning true, not post-repair validity. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L125-L146] | B6/B8; issue pending (1). |
| F10: pattern default | `*.yml` omits `.yaml` and nested files unless the caller changes the pattern. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L104-L114] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L167-L170] | B8 specifies manifest reconciliation; no durable detection check exists for unscanned files; issue pending (2). |
| F11: repair default | `fix=False` and the opt-in CLI flag leave invalid bytes unchanged; intentional behavior. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L104-L111] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L138-L146] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L172-L175] | B7/B8; require rechecking rather than assuming repair; issue pending (2). |
| F12: summary/CLI implicit return | Printed invalid counts and failed repairs do not become a structured outcome or failing process status. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L148-L153] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L180-L188] | B7; issue pending (1). |

Additional defects and limits:

| Symptom | Cause and evidence | Detection and follow-up |
| --- | --- | --- |
| Invalid UTF-8 or malformed YAML accepted by prefix check | Only six bytes are read; no decoder or parser runs. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] | B5; broader input-validation follow-up #2519; utility defect issue pending (1). |
| Triple prefix remains invalid after successful repair | Exactly three bytes are removed, with no recheck. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L60-L66] | B6/B8; issue pending (1). |
| Original content may be lost if an in-place write fails | File is opened with `wb` before writing; exceptions do not restore the original bytes. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L64-L72] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L93-L101] | No detection check exists for interrupted writes; use copies and compare digests; issue pending (1). |
| Prefix evidence mistaken for target-version compatibility | Compatibility appears in docstrings but the code implements a byte predicate. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L3-L5] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L15-L43] | L1; issue pending (3). |

No external-text numeric conversion, cache write, skipped-record `continue`, physical unit assumption, explicit `return None`, empty-list/dict error return, epoch default or percentage fallback occurs in the two source files; the complete source was inspected after the lexical sweep. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-08 | Initial pack | Treat a prefix check, a successful repair write and a target-program load as separate outcomes. | [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L60-L72] | Defined B5-B8 and L1; recorded defects without changing the methods. | Not assigned; documentation follow-up issue pending (2). |

## 8. Ownership

Review assignment: riser and mooring quality manager; proposed deputy: installation quality manager. The maintainer assigns individuals, and review clearance must be supplied by a provider other than the producer. Escalate disputed classifications or gate evidence to the family quality manager; G4 evidence belongs to diffraction review. (AQG r0, sections 8 and 11.)

Maintenance responsibilities: maintainers update the pack when cited utility behavior changes; independent review resolves open qualification items before changing DRAFT status. These are proposed responsibilities, not a record of completed review.

## 9. Dependencies and implementation inventory

### Runtime and solver references

| Dependency or reference | Scope and evidence |
| --- | --- |
| Python standard library | Uses `os`, `sys`, `pathlib.Path`, typing annotations and CLI `argparse`; no licensed solver import or call is present. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L8-L11] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L156-L184] |
| Package export | `__init__` imports and exposes `BOMValidator`. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L7-L9] |
| Binary input and output files | Byte inputs come from caller paths; both repair methods overwrite that same path; directory processing delegates to the class methods. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L56-L101] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L127-L146] |
| OrcaFlex text reference 1 | Package title names OrcaFlex; documentation context only. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L2-L2] |
| OrcaFlex text reference 2 | Package description names OrcaFlex files; no API call. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L4-L4] |
| OrcaFlex text reference 3 | Utility title names OrcaFlex; documentation context only. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L3-L3] |
| OrcaFlex text reference 4 | Module docstring claims encoding compatibility; not load evidence. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L5-L5] |
| OrcaFlex text reference 5 | Class docstring claims compatibility; not a version specification. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L15-L15] |
| OrcaFlex text reference 6 | CLI description names OrcaFlex YAML; no version argument or program invocation. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L160-L184] |

The two-source-file inventory above covers every exact `OrcaFlex` occurrence in the package's Python source; no target version, external dataset ID or package-local upstream inventory row is declared there. The utility's inventory is therefore the two source files, rather than an invented solver-version or dataset inventory. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188]

### Counted implementation-map families

Map snapshot read: commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the data revision is retained in the map at the code pin. [docs/registry/implementation-map.yaml:L1-L3]

The inventory is **empty**: filtering each family's `implementations` rows by paths beneath this package yields 0 rows and hence 0 module map families. Counted-family grep: `NOT_APPLICABLE`, because no accepted family has an implementation row in this package. The map instead records this file as a rejected text-model-writer candidate and records the class and directory function as unresolved irregular-frequency name matches; these are not accepted implementations. [docs/registry/implementation-map.yaml:L32424-L32427] [docs/registry/implementation-map.yaml:L44855-L44868]

Reproduce the family selection by loading the map snapshot with `yaml.safe_load`, iterating `families` in file order, counting each family's `implementations` where `path.startswith('src/digitalmodel/solvers/orcaflex/utils/')`, sorting nonzero counts descending with stable ties, and taking at most 3 families. If a later map admits rows, apply its case-sensitive Python `re` search signals exactly, exclude signals matching `solvers/orcaflex/utils` case-insensitively, assign hits to the enclosing function/class (module-level statements as one definition), and classify independent computations against sections 3 and 9. This is the repeatable selection rule, not a claim that uncounted families passed a grep.

### Whole-repository follow-up

The whole-repository search included `scripts/`, Python source, tests and Markdown; the commands were:

```bash
rg -n 'validate_bom|BOMValidator|fix_double_bom|solvers/orcaflex/utils|solvers.orcaflex.utils' --glob '*.py' --glob '*.md' .
rg -n 'BOM|bom|utf-8-sig' src scripts tests docs --glob '*.py' --glob '*.md'
```

| Related implementation | Classification and map follow-up |
| --- | --- |
| Documentation `fix_double_bom` | Independently implements binary double-prefix detection and the same three-byte removal; external map gap, not an in-package miss. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L104-L117] |
| Documentation writer examples | Binary BOM prepend and text `utf-8-sig` writing are other routes to a BOM-bearing file; documentation examples, not additional in-package definitions. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L64-L83] |
| Template variation reader | Decodes `utf-8-sig` and delegates model construction; a reader/helper, not a second BOM repair. [src/digitalmodel/solvers/orcaflex/template_generator.py:L401-L414] |
| Script source-encoding checker | Uses `ast.parse` and `tokenize.detect_encoding` to check Python source; different input contract, not this YAML-prefix predicate. [scripts/ci/check_text_encoding.py:L205-L227] |

Map follow-up must name all section-3 methods: prefix classification, single-prefix insertion, duplicate-prefix removal and directory orchestration, and reconcile the documentation copies before any family becomes counted; issue pending (4). [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L14-L153] [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L64-L117]

## 10. Coverage and open items

Source coverage: both Python files were read in full, with no nested implementation file in this package; the export and CLI are included. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188]

Related engineering documentation read: `docs/domains/orcaflex/FILE_REQUIREMENTS.md`; its reusable encoding guidance and repair example were inspected, while configuration-specific values and identities are not repeated here. The capability report mentions extracting shared OrcaFlex utilities as a proposal, rather than a demonstrated method; it supplies no worked example for this package. [docs/capability-map/capability-report-2026-02-14.md:L209-L212] The routing registry and implementation-map candidate rows were also read. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L9-L125] [docs/registry/module-routing.yaml:L477-L530] [docs/registry/implementation-map.yaml:L32424-L32427] [docs/registry/implementation-map.yaml:L44855-L44868]

Test discovery used symbol names and both forms of the package path in `tests/` and package-local files; no utility test node was found. The relevant existing routing inventory test is listed in section 4; its duplicate-key checks cover registry parsing, not utility behavior. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188] [tests/docs/test_digitalmodel_routing_contract.py:L190-L254]

| Item | Coverage gap or action | Responsible role | Tracking |
| --- | --- | --- | --- |
| O1 | Prefix-only acceptance, ambiguous repair results, non-atomic writes, no recheck and CLI outcome propagation; section 6 gives individual source sites and checks. [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L153] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L180-L188] | Utility maintainer | issue pending (1); broader input contract #2519 |
| O2 | Add durable solver-free test nodes, glob/manifest and interrupted-write controls, and independently sourced worked examples; the current package has only implementation and export source. [src/digitalmodel/solvers/orcaflex/utils/__init__.py:L1-L9] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L1-L188] | Utility maintainer | issue pending (2) |
| O3 | Resolve the single UNVERIFIED compatibility claim in section 1 with public vendor evidence, a version declaration and G1 records; link G2/G3/G5 evidence for any consuming analysis. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L11-L16] [src/digitalmodel/solvers/orcaflex/utils/validate_bom.py:L31-L43] | Riser/mooring quality manager | issue pending (3) |
| O4 | Add the section-3 method families to the map before counting them; classify documentation copies and rejected candidates. [docs/domains/orcaflex/FILE_REQUIREMENTS.md:L64-L117] [docs/registry/implementation-map.yaml:L32424-L32427] [docs/registry/implementation-map.yaml:L44855-L44868] | Method-map maintainer | issue pending (4) |
| O5 | Extend the fixed route expectation when registering this utility, preserving all existing routes; the current exact inventory omits it. [tests/docs/test_digitalmodel_routing_contract.py:L217-L254] | Routing maintainer | issue pending (5) |

All open items carry draft references so they can receive issue numbers before review clearance; this draft does not assert completed independent review.
