# Plan for #2214: Cathodic-protection docs — worked examples, index, brochure

> **Issue:** #2214 | **Epic:** #2206 | **Owner decisions:** D11, D12
> **Approved by:** owner instruction "continue with tasks", 2026-09-25
> **Status:** implemented (this branch) — awaiting review
> **Complexity:** T2 (documentation + one contract test; no solver changes)
> **Date:** 2026-09-25
> **Branch:** `docs/cp-2214-examples-index-brochure` (from `main`)
> **Source of findings:** `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` section 6

---

## Scope

Section 6 of the 2026-09-24 technical-quality review found that the cathodic-protection
documentation under `docs/domains/cathodic_protection/` was not reproducible from the code
it describes. This plan closes those findings without touching any solver.

Out of scope (owned elsewhere): the five analysis notes being redacted under #2155
(`saipem_cp_comparison_analysis.md`, `COATING_COMPARISON_ANALYSIS.md`,
`CP_ANALYSIS_FINDINGS.md`, `cp_bug_fixes_summary.md`, `standards-inventory.md`); the
edition-keyed DNV-RP-B401 / DNV-RP-F103 table modules (#2207); the F103 kernel (#2211).

### 1. Worked examples route through the real router

Ten of eleven `examples/calc-0xx*.md` Python snippets passed `calculation_type` keys that
`CathodicProtection.router()` rejects. The router accepts only `ABS_gn_ships_2018`,
`DNV_RP_F103_2010`, `ABS_gn_offshore_2018` and `DNV_RP_B401_offshore`.

Key mapping applied:

| Snippet key (before) | Router key (after) | Note recorded in the doc |
|---|---|---|
| `DNV_RP_B401_2021`, `DNVGL_RP_B401_2017`, `DNV_RP_B401_2005` | `DNV_RP_B401_offshore` + `inputs.design_data.edition` = `"2021"` / `"2017"` / `"2005"` | `edition` requires #2207 or later (merged into this branch as 36eddde4) |
| `ABS_CP_SHIPS_2017` | `ABS_gn_ships_2018` | router key is misnamed: the ABS Guidance Notes are dated December 2017; key not renamed here |
| `DNVGL_RP_F103_2016` | `DNV_RP_F103_2010` | 2016 tables are not yet in the repo |
| `depth_salinity_resistivity` | none | snippet tagged `# not-runnable: resistivity helper pending` |

Where a cfg dict needed router-schema keys (`structure.zones`, `pipeline`, `anode`) to run,
those keys were added from the values already tabulated in the same document; zones the
current route cannot represent (buried/sediment, insulation-coated, concrete-embedded, TSA)
are omitted and named in a "Reproduction note". Tabulated source numbers are never edited;
where the code's output differs, the Reproduction note states the code value and that it comes
from DNV-RP-B401 Tables 10-1 / 10-2 / 10-4 as of #2207 (follow-up after the #2207 merge:
every note re-run and rewritten with the merged-branch values).

New test `tests/cathodic_protection/test_worked_examples.py` extracts every ```python block
from `examples/calc-0xx*.md` and `examples/example-0*.md`, executes it in a sandbox namespace
with `src/` importable, asserts no exception, and — when the block leaves a `cfg` with an
`inputs.calculation_type` — asserts the key is one the router accepts. Blocks whose first
line is `# not-runnable: <reason>` are skipped.

### 2. Duplicate examples

Keep one example per standard, the edition-named file:

- `example-01-pipeline-dnv-f103-2010.md` absorbs the FBE / spacing-check case from
  `example-01-pipeline-dnv-f103.md` as "Variant B"; the latter is deleted.
- `example-02-ship-hull-abs-gn-ships-2017.md` absorbs the partially-coated (disbonding term)
  and 20-year cases from `example-02-ship-abs-2018.md` as "Variant B"; the latter is deleted.
  The "abs-2018" file name repeated the router-key misnomer; the surviving file says so.
- `example-03-offshore-platform-dnv-b401-placeholder.md` is deleted (WRK-272 closed; the
  `DNV_RP_B401_offshore` route exists). A real `example-03-platform-dnv-b401-offshore.md` is
  added (temperate 0–30 m jacket, Category III coating, 25 yr) with the numbers obtained by
  running the route on this branch, and `example-03-platform-abs-offshore-2018.md` links to it.

### 3. Index

`_index.md` is rewritten by hand: purpose, module map (`src/digitalmodel/cathodic_protection/`
one line per module, the legacy solver, the #2207 table modules), worked examples with
one-line descriptions, the analysis notes (the five #2155 files by filename only), standards
inventory link, reviews (`technical-quality-review-2026-09-24.md` and
`technical-quality-review-2026-09-25-human-decisions.html`), tests and fixtures. No
auto-generated "Total Files" stub.

### 4. Brochure (owner decision D11)

`docs/marketing/cathodic-protection-brochure.md` is deleted; every link to it is removed
(grep over `docs/`, `README.md`, `scripts/`).

### 5. Units in table cells

For every `.md` under `docs/domains/cathodic_protection/` this issue may edit, units are moved
from value cells into the column header per `.claude/standards/TABLE_FORMATTING_STANDARDS.md`.
No numeric value changes.

### 6. Housekeeping

- `literature.md`: broken links (`literature`, `sacrificial_anode.md`) fixed or removed.
- `docs/plans/2026-05-05-issue-573-dnv-rp-f103-cathodic-protection-calibration.md`: status
  marked done — "Closed 2026-09-25; fixed by f1a1b05f".

## Verification

```
PYTHONUTF8=1 <venv-python> <wt_pytest.py> tests/cathodic_protection/test_worked_examples.py tests/docs -q
```

plus the repo link validator (`scripts/python/digitalmodel/doc_tools/link_validator.py`)
run over `docs/domains/cathodic_protection/`.

## Constraints honoured

- No commit, stash, push or issue creation from this worktree.
- No edits to the five #2155 files.
- No client, operator, contractor, vessel or project identifiers and no private absolute
  paths introduced.
