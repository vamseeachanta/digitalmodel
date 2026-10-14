# Comment-normalized duplicate inventory

The inventory finds 1,683 files in 385 duplicate sets: 1,298 copies beyond one
proposed canonical location per set. No model is deleted, moved or renamed.
Equality under this rule does not establish engineering equivalence, provenance,
permission to redistribute, or successful solver execution.

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

Table 1: disjoint family counts. Paths in the first column are relative to
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
`Vessels`, `BaseFile` or `IncludeFile` key. This selects 3,734 candidate files;
it includes partial model and specification YAML and does not claim that each
candidate is independently loadable. Symlink files are excluded.

For each file, `bytes.splitlines()` separates line terminators. Lines whose first
non-whitespace byte is `#` are removed. Remaining lines are joined with LF and
hashed with SHA-256. Blank lines, indentation and inline comments remain;
CRLF/LF differences and a single final terminator are normalized. A set contains
at least two matching digests. Each set is assigned once, to a named family only
when every member belongs to that family; otherwise it is assigned to Other.

The generated CSV contains one row per copy: set digest, family, relative path,
proposed-canonical flag and candidate reference locations. A literal basename
search covers tracked Markdown, reStructuredText, HTML, text and Python files
under `docs/` and `tests/` (3,911 documents in this run). It reports file and line
only, without copying source text. Basename matches are explicitly ambiguous:
they are discovery candidates, not proof that a caller resolves to that copy.
Dynamic paths and references outside those text formats are not established.

The table is 11,711,650 bytes, exceeding the job's 1 MB artifact limit. Under the
authorized fallback, this summary and script are committed; the table is not.
The script writes `duplicate_sets.csv` and `duplicate_counts.json` to scratch
storage so that every proposal and reference candidate can be reproduced.

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
