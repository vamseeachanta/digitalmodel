# Solver-reference export contract

## Summary

`SolverReferenceExporter` preserves per-condition source evidence in JSON and
JSON-cell CSV. Both readers validate units and recompute export annotations.
Structural readback leaves engineering evaluation `NOT_EVALUATED`; computation,
applicability and a fresh run require independent evidence.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| A | 2026-10-22 | Document strict readback and an adapter audit to prevent warning and unknown-state loss. |

## Inputs and method

Use the existing
[exporter](../../../src/digitalmodel/data_systems/solver_reference_export.py).
Each row requires a nonempty `condition_id`, `quantity` and `source_unit`.
The caller supplies `supported_units`, a quantity-specific collection of exact
unit spellings from its existing source contract. No alias, unit conversion or
unit default is applied. Undeclared quantities and unsupported units raise
`ValueError` during construction and readback, including annotated exports.

All other source fields pass through unchanged: existing source and dataset
identities, revisions, provenance, status, execution evidence and limitations.
The exporter creates no catalog entry and does not resolve or authenticate these
fields. `warnings` may be absent, null, or a list of strings; these three forms
remain distinct. Warnings remain attached to their original condition.

`export_value_state` describes the supplied value evidence:

| Evidence | Export state |
|---|---|
| Value missing or null | `absent`, regardless of a source computation claim |
| Value is not an integer or float, including a boolean or source text | `invalid` |
| Numeric value with `computed`, `absent`, `invalid`, `uncomputed`, `cached_unqualified` or `unknown` | The supplied state |
| Numeric value without a state, or with an opaque vendor state | `unknown` |

The original `value` and `value_state` remain unchanged even when a computed
claim conflicts with missing or invalid data. A numeric zero never supplies a
missing computation state. Every row also has
`export_evaluation: NOT_EVALUATED`, even when its source state is `computed`.
Neither an empty warning list nor successful process exit establishes
applicability, current output or engineering acceptance. An opaque instance
handle remains an opaque source field.

`from_json(text, supported_units=...)` reads an array of source or exported rows.
Duplicate member names are rejected at every nesting depth before a decoder can
discard a warning or status. JSON numeric extensions and nonfinite numeric
storage are rejected. Invalid numeric tokens may instead be supplied as explicit
source strings, which remain evidence and export as `invalid`.

`from_csv(text, supported_units=...)` reads the existing JSON-cell CSV format.
Each present cell contains a JSON value, including quoted JSON strings and
arrays. An empty cell means a missing key; the text `null` means explicit null;
`[]` means an observed empty list. Column names must be unique, required columns
must be present, and each record must have the header's width. Malformed quoting,
duplicate JSON keys within cells, and non-JSON cells raise `ValueError`.
Empty input represents an empty export. The Python CSV reader's configured
field-size limit applies; exceeding it raises `ValueError` without dropping data.
Ordinary numeric-table CSV needs an explicit source adapter; passing it directly
does not infer units or qualification.

Exported rows may be read back only when both export annotations are present and
match independently recomputed annotations. Partial or inconsistent annotations
raise `ValueError`. This detects structural inconsistency, not forged evidence:
a consistently edited source row cannot be authenticated by readback alone.

## Worked example: inputs, method and answer

Run the [example](../../../examples/domains/data_systems/solver_reference_roundtrip.py)
from the repository root with the source package on the import path:

```sh
PYTHONPATH=src uv run --no-project python examples/domains/data_systems/solver_reference_roundtrip.py
```

The inputs are synthetic protocol tokens. The caller declares `force: {N}` as
its accepted spelling. No physical prediction, coefficient, acceptance limit or
unit conversion is used. The independent method is identity copying for every
source key, followed by the state rules above. CSV readback and JSON readback
then repeat those rules without replacing the source evidence.

| Condition | Raw input | Source state | Answer: export state | Warning evidence |
|---|---|---|---|---|
| computed | `0` | `computed` | `computed` | empty list |
| pending | `0` | `uncomputed` | `uncomputed` | null |
| cached | `0` | `cached_unqualified` | `cached_unqualified` | retained warning text |
| no-output | null | `computed` | `absent` | missing |
| invalid | string `NaN` | `computed` | `invalid` | missing |
| unknown | `0` | missing | `unknown` | missing |

Every answer retains `source_unit: N` and
`export_evaluation: NOT_EVALUATED`. The cached warning remains
`Applicability unknown, cached output`. The no-output row retains successful
exit and an opaque handle, while its output stays absent. These expected answers
are also enumerated independently in the
[tests](../../../tests/data_systems/test_solver_reference_export.py).

## Adapter audit and remaining scope

This is a static audit of the named paths, not an inventory of all adapters or a
solver run. It uses the existing dataset/source contract and introduces no result
registry. The generic export/readback path is implemented here; the other paths
need source-specific work before they can carry the same evidence.

| Existing surface | Observation from code | Way forward |
|---|---|---|
| `data_systems/solver_reference_export.py` | Retains arbitrary source fields and states; strict readers now reject ambiguous text and inconsistent annotations. | Use for evidence-preserving offline interchange; keep qualification separate. |
| `hydrodynamics/diffraction/wamit_reference_loader.py`, `_build_results` and `_build_matrix_list` | Missing response components are filled with zero arrays; missing matrices already carry `source: placeholder`. Response components have no equivalent source-state annotation and receive empty unit text. | Preserve component availability and declared source units upstream; never infer computation from filled zeros. |
| `hydrodynamics/diffraction/polars_exporter.py`, record builders | Response rows retain unit text; matrix rows omit the existing matrix `source` marker. Records have no warning or computation-state fields. | Carry existing matrix provenance and add source-backed condition evidence before using these tables as references. |
| `hydrodynamics/diffraction/orcaflex_exporter.py`, `export_raos_csv` | Numeric response tables are formatted for solver interchange, without per-condition warning/state fields. | Retain an evidence-bearing representation alongside solver-input tables; do not qualify numeric tables alone. |
| `hydrodynamics/diffraction/benchmark_runner.py`, YAML assembly | Comparison status and refusal reasons are exported; matrix numeric arrays do not retain their individual source markers in that assembly. | Keep comparison conclusions distinct from underlying reference qualification and preserve source markers in a later schema-compatible change. |

These remaining adapter changes require tests of their shared result schemas and
source-specific unit/evidence mappings. No qualified reference can be recovered
from a table after its warnings or state were discarded. Keep the authoritative
source record and existing identifiers; unavailable required evidence remains
`NOT_EVALUATED`. A fresh run-specific output and independently checked provenance
are required before a future readiness claim; exit status or handle creation
alone is insufficient. Execution identifiers are not reinterpreted as operating
system window handles.

## Sources and validation basis

- [RFC 8259, The JavaScript Object Notation (JSON) Data Interchange Format,
  December 2017](https://www.rfc-editor.org/rfc/rfc8259), section 4 (duplicate
  member interoperability) and section 6 (number grammar).
- [RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files,
  October 2005](https://www.rfc-editor.org/rfc/rfc4180), section 2 (record width,
  quoting, embedded commas and line breaks). Unique headers, JSON cells and
  blank/null semantics are the repository contract, rather than RFC requirements.
- The existing export module and its tests define the state vocabulary and
  `NOT_EVALUATED` policy. The audit observations above are bounded to their named
  code surfaces. Private source evidence is not reproduced.

Regression tests use literal evidence copies and independently enumerated state
answers. Cross-format tests exercise warning punctuation, missing/null/empty
values, cached and uncomputed zeros, unknown vendor states, strict unit rejection,
nonfinite numbers, duplicate keys/headers, malformed CSV and annotation conflicts.
No licensed solver is imported, queried or executed by this example or its tests.
