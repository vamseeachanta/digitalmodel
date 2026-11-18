# Comment-normalized duplicate inventory

The current inventory finds 1,834 files in 408 duplicate sets: 1,426 copies beyond one
proposed canonical location per set. No model is deleted, moved or renamed.
Equality under this rule does not establish engineering equivalence, provenance,
permission to redistribute, or successful solver execution.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-12 | The initial survey recorded duplicate families and location preferences without removing models. |
| 2 | 2026-10-20 | Proposal and reference outputs are separated to avoid repeated reference lists; a worked example and regression checks explain the join. |
| 3 | 2026-10-24 | Candidate and reference-document counts use the same regenerated inventory; table numbering follows document order and duplicate test licence text is removed. |

Table 1: document revisions; the original survey below remains a historical measurement.

## Current inventory (2026-10-24)

The tracked model tree at `807dd9b9` is read using the schema-version-2 script.
The scan selects 3,870 candidate YAML files. The family counts are reproduced
without deleting or editing any model input.

| Family | Sets | Files in sets | Redundant copies |
|---|---:|---:|---:|
| Buoy | 171 | 873 | 702 |
| Tier-two | 29 | 205 | 176 |
| Examples/model library | 62 | 165 | 103 |
| Other | 146 | 591 | 445 |
| Total | 408 | 1,834 | 1,426 |

Table 2: current disjoint family counts; redundant copies equal files minus sets.

The current tree contains 151 more files in duplicate sets and 23 more sets than
the historical inventory, giving 128 more redundant copies. This is a tree
comparison, not an engineering-equivalence claim. The original survey's
selection rules remain unavailable, so reconciliation to that survey is not
established. Generated JSON and CSV outputs are read back to check the row
counts, one canonical proposal per set, and all reference-count joins.

## Historical inventory (2026-10-12)

The earlier inventory found 1,683 files in 385 sets and 1,298 redundant copies.
Basis: the working tree based on `7299eb516e5953f89b415fa24ff178ef96589a52`,
with the bounded identifier and include-path edits for
[the cleanup issue](https://github.com/vamseeachanta/digitalmodel/issues/2425).
The inventory is dated 2026-10-12.

| Family | Sets | Files in sets | Redundant copies | Earlier survey copies |
|---|---:|---:|---:|---:|
| `mooring/buoy/` and `mooring/buoy_converted/` | 171 | 873 | 702 | 596 |
| `library/tier2_fast/` | 30 | 219 | 189 | 139 |
| `examples/raw/`, `examples/yml/`, `library/model_library/` | 62 | 168 | 106 | 91 |
| Other, including sets crossing the above families | 122 | 423 | 301 | not stated |
| Total | 385 | 1,683 | 1,298 | 996 |

Table 3: disjoint family counts. Paths in the first column are relative to
`docs/domains/orcaflex/`. Redundant copies equal files minus sets in every row.

The issue's earlier survey at `a8084d1` reports 1,308 files in 312 sets and 996
redundant copies. This inventory differs by 375 files, 73 sets and 302 redundant
copies. The earlier survey's executable normalization and selection rules are
not supplied in the issue; a like-for-like reconciliation is not established.
The figures have not been adjusted to match that survey.

## Reproduction and counting rule

Run the standard-library-only [inventory script](duplicate_inventory.py):

```text
python docs/domains/orcaflex/library/duplicate_inventory.py --output-dir <scratch-output>
```

The output directory must be outside the checkout. The script reads the tracked
file list with `git ls-files` and reads current file bytes. It selects `.yml` and
`.yaml` files under `docs/domains/orcaflex/` and `tests/solvers/orcaflex/`, plus
other tracked YAML containing a column-zero `General`, `Environment`, `LineTypes`,
`Vessels`, `BaseFile` or `IncludeFile` key. The 2026-10-24 scan selects
3,870 candidate files; it includes partial model and specification YAML and does
not claim that each candidate is independently loadable. Symlink files are
excluded.

For each file, `bytes.splitlines()` separates line terminators. Lines whose first
non-whitespace byte is `#` are removed. Remaining lines are joined with LF and
hashed with SHA-256. Blank lines, indentation and inline comments remain;
CRLF/LF differences and a single final terminator are normalized. A set contains
at least two matching digests. Each set is assigned once, to a named family only
when every member belongs to that family; otherwise it is assigned to Other.

The schema-version-2 proposal CSV contains one row per copy: set digest, family,
relative path, proposed-canonical flag, reference basename and reference count.
The separate reference CSV contains each distinct basename/location pair once.
A literal basename
search covers tracked Markdown, reStructuredText, HTML, text and Python files
under `docs/` and `tests/` (4,295 documents in the 2026-10-24 scan).
It reports file and line only, without copying source text. Basename matches are
explicitly ambiguous:
they are discovery candidates, not proof that a caller resolves to that copy.
Dynamic paths and references outside those text formats are not established.

The original combined table was 11,711,650 bytes. The script now writes
`duplicate_sets.csv`, `duplicate_references.csv` and `duplicate_counts.json` to
the requested external output directory. The summary records both table sizes
and the distinct reference-row count. The tables remain generated outputs;
this document and the script provide the reproducible report without committing
model paths or reference locations again.

Join `duplicate_sets.reference_basename` to `duplicate_references.basename` to
recover every candidate location for each copy. A zero `reference_count` means
no literal basename match was found in the scanned documents; it does not prove
that the model has no callers. A positive count does not resolve a reference to
a particular duplicate. The schema change replaces the former embedded
`references_by_basename_ambiguous` column; consumers of that column will need
to perform the join.

## Worked example: inputs, method and answer

Two synthetic inputs, `examples/raw/sample.yml` and `examples/yml/sample.yml`,
contain respectively `# first` and `# second` on their first lines, followed
by the identical data line `General: {}`. A third input has a different data
line and therefore does not belong to their set. Two documents mention
`sample.yml`, at `tests/a.py:3` and `docs/b.md:7`.

Removing only full comment lines leaves equal bytes for the first two inputs.
The hand calculation is two files minus one proposed canonical copy equals one
redundant copy, following the counting rule in the
[cleanup issue](https://github.com/vamseeachanta/digitalmodel/issues/2425).
The location preference selects `examples/raw/sample.yml`. The proposal table
has two rows, each with reference count two; the reference table has two rows.
Their join produces four candidate copy/location pairs. The separate tables
preserve all candidates without copying each reference list into every row.
This example establishes neither model validity nor permission to redistribute.

The executable hand-counted examples are in
`tests/solvers/orcaflex/test_duplicate_inventory.py`. Additional checks compare
the README counts with direct tree enumeration, resolve the repaired chain
include, and require a provenance note even where a group already has a README.

## Remaining decisions

The drawing and base-file references retained by the earlier cleanup remain
unchanged. Renaming referenced assets requires a separate coordinated change;
no engineering value is modified here. The absent `polyester_200mm.yml` line
type remains unresolved; no substitute is invented. The provenance notes record
unverified source and redistribution terms as `not stated`. Duplicate removal,
caller resolution and engineering qualification remain separate work.

## Proposed canonical-location rules

- Buoy family: prefer `mooring/buoy/` outside its `modular/` subtree, then use
  fewer path components, case-insensitive lexical order and exact lexical order.
- Examples/model-library family: prefer `examples/raw/`, then `examples/yml/`,
  then `library/model_library/`; use the same path-depth and lexical tie-breaks.
- Tier-two and Other families: use fewer path components, then the same lexical
  tie-breaks. No source-authority preference is established for these families.

These rules propose locations for later review, not deletions. Relative includes,
caller resolution, provenance and downstream tests must be assessed before any
copy is removed. The new guard tests also exercise the digest and proposal rules
with synthetic inputs; no solver is imported or called.
