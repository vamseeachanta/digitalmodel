# Analysis evidence boundary audit

## Summary

Refs #2120. This bounded audit inventories evidence consumers and adapters in
this repository and hardens the existing ANSYS exact-lookup boundary. It does
not establish ecosystem coverage or qualify an engineering response. The
implementation retains the current evidence schema and its external authority
model.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Record inspected boundaries and add exact comparison, checker identity and retained-example regressions. |

## Plan-lite

- Slice 1: remove ambient Decimal arithmetic from ANSYS qualification comparisons.
- Slice 2: reject the existing capture-author placeholders for checker identities.
- Slice 3: retain source-snapshot and committed-example manifest/CSV regressions.
- Production file: `src/digitalmodel/ansys/analysis_lookup.py`.
- Tests: `tests/ansys/test_analysis_evidence_boundaries.py` and existing analysis tests.
- Retained predicate: `abs(response - reference) <= absolute_tolerance`, with a
  nonnegative tolerance supplied by the verification register; no default limit.
- Basis: analysis criteria revision 1 (2026), AC-07; exact decimal-to-rational
  conversion follows the Python 3.11 Library Reference, `fractions.Fraction`.
- Expected comparison outcomes come from hand calculations in the test docstring.
- Snapshot expectations come from literal synthetic source bytes; export
  expectations come from the separately committed example manifest.
- Later slices: other adapter-specific gates, receipt metadata and digest mirrors.

## Inspected boundaries

Paths in this table are relative to `src/digitalmodel/`. Inspection means source
reading; unchanged modules were not executed or certified by this audit.

| Consumer or adapter | Boundary observed | Disposition |
| --- | --- | --- |
| `ansys/analysis_intake.py`, `analysis_records.py` | `_make_case` parses bytes returned by `verify_reference`, which checks their digest. `code_fingerprint` normalizes source newlines before hashing. | Preserve behavior; add a deterministic replacement-after-read test. Existing LF/CRLF regression remains in `test_analysis_review_regressions.py`. |
| `ansys/analysis_evidence.py` | Typed diagnostic records, row/package digests and response CSV export; import does not itself qualify a response. | Add a permanent committed manifest/CSV comparison, including every record and typed field. |
| `ansys/analysis_lookup.py` | External authority, review sources, withdrawal chain, per-finding evidence, author identity, native status and independent comparison gate ordinary lookup. | Replace Decimal arithmetic with exact rational arithmetic and reject placeholder checkers. |
| `ansys/cylinder_criteria.py` | Numerical assessment uses a fixed decimal context and explicitly returns no engineering authority. | Separate numerical evaluator; limits and qualification policy unchanged. |
| `solvers/openfoam/smoke_evidence.py` | Typed smoke evidence, duplicated artifact/bridge record checks and floating-point timing/motion comparisons. | Smoke validation is not an exact engineering-lookup authority. Adapter-specific numeric audit deferred. |
| `workflows/installation_added_mass_evidence.py` | Pinned JSON parses verified bytes; run, generation, trace and artifact dependencies are checked separately, with final evidence rechecks. | Snapshot/large-file consistency and imported authority audit deferred. |
| `hydrodynamics/diffraction/diagnostic_basis.py` | Decimal equality checks readback against a prescribed diagnostic basis; source comparison remains unestablished. | Diagnostic comparison is not source qualification; unchanged. |
| `cathodic_protection/report_adapters.py` | Report provenance hashes the current input file separately from the supplied results mapping. | Presentation provenance does not prove that the hashed file produced those results; computation-to-source binding audit deferred. |
| `reporting/adapters.py` | Registry converts a routed configuration/results mapping into a report specification. | Presentation adapter, not a qualification gate; unchanged. |
| `hydrodynamics/capytaine/mesh_adapter.py`, `motion_forecast/rao_adapter.py` | Geometry import and transfer-function interpolation respectively consume supplied configuration/data. | Transformation adapters; not equivalent to qualified exact lookup. No solver import or execution performed. |

This is a bounded inventory of the inspected paths, not a claim that every file
named adapter implements an evidence authority. Additional program adapters need
their own input, execution, response-readiness and live-authority mapping before
the ANSYS patterns can be described as general coverage.

## Defects and tests

Increasing Decimal precision alone leaves exponent bounds and signal traps
inherited from the caller. A caller context can therefore reject an exact
tolerance-boundary equality or raise a subnormal signal during comparison.
Conversion from validated decimal strings through `Decimal` to `Fraction`
preserves the exact rational value; subtraction, absolute value and comparison
then use integer arithmetic. The caller's Decimal context and flags remain
unchanged. Existing decimal input bounds remain in force.

The test docstring independently expands the acceptance arithmetic, including an
equality boundary, a strict exceedance, a small nonzero difference and a difference
that finite Decimal precision could round down to the supplied tolerance. These
are synthetic arithmetic inputs, not engineering acceptance values.

A checker named `unknown`, `unverified` or `historical-neutral-capture` previously
passed the non-author comparison. The existing capture-author sentinel policy
now applies to checkers after whitespace and case normalization. This rejects
known placeholders; it cannot authenticate a substantive identity or establish
competence, independence or execution by itself.

The source-snapshot test replaces the file immediately after its verified read.
The parser must use the bytes already checked, and must read that source only
once. Later changes still require live digest checks at publication or lookup.

The committed example regression validates the manifest's package and row hashes,
compares the full generated CSV to the committed CSV using the established LF
text policy, and independently constructs the expected parsed records from the
manifest. It does not compare historical source fingerprints against the current
implementation or require private source files. It proves export consistency,
not that the manifest's engineering values are correct or qualified.

Before production changes, the boundary tests reproduced comparison failures and
placeholder-checker acceptance. The existing snapshot and committed-export paths
passed; those behaviors are now retained as permanent regressions.

## Verification

The analysis test group passed with 430 tests, including the ten new boundary
cases. Two existing opt-in retained-input tests skipped because their private
fixture root was unavailable; the same skip condition is present on the base
revision. Ruff passed on both changed Python files. The engineering register,
repository structure and whitespace checks also passed. Checks ran on the
uncommitted tree.

## Sources

- [Analysis criteria revision 1 (2026)](../standards/analysis-parametric-lookup-criteria.html):
  AC-02 input authority, AC-07 independent verification, AC-08 response readiness,
  AC-09 machine-readable export, AC-10 identity/readback and AC-11 bounded lookup.
- [Python 3.11 Library Reference, fractions.Fraction](https://docs.python.org/3.11/library/fractions.html#fractions.Fraction):
  exact rational construction from Decimal; public documentation opened for this
  implementation. No new physical equation, material coefficient or acceptance
  limit is introduced.
- Existing source policies: `analysis_records.decimal_text`,
  `analysis_records.verify_reference` and `analysis_intake.code_fingerprint`.
- Example oracle: `examples/ansys/analysis-evidence/manifest.json`; corresponding
  export: `examples/ansys/analysis-evidence/responses.csv`.

## Later slices and known limits

Individual finding dispositions, supersession, typed intended-use lists, source
binding and null blocking-review refusal already have focused Stage A guards.
Their existence is not evidence of equivalent protection in other programs.
Shared-schema changes, digest-mirror equality rules, richer public execution
receipts and scanner path/digest maps remain deferred. No retained result,
source fingerprint or engineering qualification state is rewritten.

## Baseline

none

## Hooks that could not run

none; direct file checks apply here, with repository hooks handled at publication.

## Follow-up

Complete adapter-specific audits before generalizing the evidence policy beyond
the inspected ANSYS boundary. Independent review of the changed content remains
required before merge.
