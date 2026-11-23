# Fixture Provenance — `dnv-os-e301.md`

This directory contains a vendored copy of a wiki page from the workspace-hub
knowledge base. The vendored copy lets digitalmodel's citation tests
(`tests/citations/test_registry.py`, `tests/citations/test_schema.py`) pass on
a standalone digitalmodel CI checkout without requiring `knowledge/wikis/`
from workspace-hub. See workspace-hub issue
[#2580](https://github.com/vamseeachanta/workspace-hub/issues/2580) Defect 1
and the merged implementation in digitalmodel
[PR #542](https://github.com/vamseeachanta/digitalmodel/pull/542).

## Canonical source

- **Repo:** `vamseeachanta/workspace-hub`
- **Path:** `knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md`
- **Canonical SHA at vendoring time:** `bd11f33bfb358aec233ebaaf7da1c11479117575`
  (last commit touching the canonical file in workspace-hub as of vendoring)

## Vendored copy

- **Path in this repo:** `tests/citations/fixtures/knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md`
- **Vendored on:** 2026-05-02 (digitalmodel commit `b1346acb`, PR #542 merge)
- **Provenance pinned by:** workspace-hub commit landing this file.

## Freshness contract

Per the workspace-hub plan
`docs/plans/2026-05-02-issue-2580-digitalmodel-collect-ignore-test-fixes.md`
(r2 "Fixture Freshness — Mechanism A"):

1. **Cadence:** review monthly. Next review: **2026-06-02**.
2. **Procedure:** compare the vendored copy against the canonical workspace-hub
   path:
   ```bash
   diff -u \
     <workspace-hub>/knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md \
     tests/citations/fixtures/knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md
   ```
3. **If diverged:** re-vendor by copying the canonical file over the vendored
   copy, then update the *Canonical SHA at vendoring time* and *Vendored on*
   fields above. Re-run
   `PYTHONPATH=src uv run pytest tests/citations/ -v` to confirm the citation
   tests still match the new frontmatter (revision, publisher, etc.) before
   committing.
4. **If frontmatter changes:** the citation tests assert specific values
   (e.g. `revision == "2021-07"`). Update both the test expectations and this
   provenance record in the same commit; do NOT silently re-vendor.

## Mechanism B (CI staleness check) — follow-up

A CI hook that compares the two files when workspace-hub is checked out
alongside digitalmodel was deferred to a follow-up issue. See plan r2
"Fixture Freshness — Mechanism B" for the rationale.
