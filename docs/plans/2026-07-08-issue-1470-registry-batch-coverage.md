# Plan for #1470: Registry Batch Coverage Wave 6

> **Status:** plan-review
> **Complexity:** T3
> **Date:** 2026-07-08
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1470
> **Client:** llm-wiki
> **Project:** riser-projects
> **Lane:** lane:codex
> **Review artifacts:** scripts/review/results/2026-07-08-plan-1470-provider-unavailable.md | scripts/review/results/2026-07-08-plan-1470-subagent-engineering.md | scripts/review/results/2026-07-08-plan-1470-subagent-governance.md | scripts/review/results/2026-07-08-plan-1470-r2-engineering.md | scripts/review/results/2026-07-08-plan-1470-r2-governance.md | scripts/review/results/2026-07-08-plan-1470-r3-governance.md

---

## Resource Intelligence Summary

### Existing Repo Code

- `examples/workflows/riser-stackup-registry-batch/run.py` iterates public `riser_stackup` handles with `stackup_type=as-planned-stackup` and `operation=drilling`, resolves wiki-side `16q-min-top-tension` calc contracts, and falls back to schedule assembly where `SCHEDULE_RSU_IDS` exposes a text-extractable schedule. The output CSV is deliberately gitignored because computed weights/tensions are wiki-side values and must not be committed to public `digitalmodel`.
- `src/digitalmodel/drilling_riser/schedule_assembly.py` currently exposes only `RSU-0019`, `RSU-0021`, and `RSU-0040` through `_SCHEDULE_PAGES`. `RSU-0038` is intentionally absent because the source carries raster-only joint counts.
- `tests/drilling_riser/test_schedule_assembly.py` pins the current pass/finding stance: `RSU-0019` passes within 3%; `RSU-0021` and `RSU-0040` remain documented findings; `RSU-0038` must fail closed.
- `tests/drilling_riser/test_assembly_golden.py` pins existing contract-chain goldens for `RSU-0010`, `RSU-0012`, `RSU-0014`, and `RSU-0023`, with values resolved from the private wiki at test time.
- `scripts/riser_database/sources.yml` already includes public handle rows for the missing as-planned drilling rows, including `RSU-0030..RSU-0033`, `RSU-0038`, `RSU-0041`, `RSU-0042`, `RSU-0050`, and `RSU-0077`.

### Standards

| Standard | Status | Source |
|---|---|---|
| API RP 16Q | Existing public engine and cited getter precedent | `docs/riser_database.md`, `src/digitalmodel/drilling_riser/assembly.py`, `src/digitalmodel/drilling_riser/stackup.py` |
| Project/source workbook chains | Private values, public handles only | `docs/riser_database.md` leak/provenance discipline |

This issue will not add standards-derived constants. It will extend source-derived calc contracts and parser coverage. If implementation discovers a standards-derived value, it will use the existing `Citation`/fail-closed getter pattern rather than embedding literals.

### LLM Wiki Pages Consulted

- `wikis/riser-projects/wiki/comparisons/31057-phase2-cal-triage.md` classifies the Phase-2 CAL folder: four CHECKED workbooks were extracted in wave 5 as `RSU-0081..RSU-0084`; fourteen CHECKED future-wave workbooks remain, grouped as model/motion generators, SHEAR7/VIV, structural checks, and wear. Weight/tension revs `1031-02` and `1030-03` are lineage-only relative to already extracted `RSU-0082` and `RSU-0083`; `1032-01` is DRAFT and cannot be promoted without comparison.
- `wikis/riser-projects/wiki/entities/31057-eni-devils-tower-ttr-fatigue.md` records that the Phase-2 CAL folder was fully triaged and that `RSU-0081..RSU-0084` are already extracted on the weight/tension source page. Remaining targets are not automatically top-tension datasets.
- `wikis/riser-projects/wiki/datasets/stackup-registry.md` shows `RSU-0030..RSU-0033` as X-Drill as-planned drilling stackups with joint schedules, and `RSU-0077` as an SBOP drilling shallow-water tension workbook.
- `wikis/riser-projects/wiki/entities/ri-xdrill-12000ft-riser-program.md` explicitly says the X-Drill `RSU-0030..RSU-0037` source family carries geometry and buoyancy-staging schedules only, with no weights or tensions in those sheets. Therefore `RSU-0030..RSU-0033` may remain non-runnable unless a paired source supplies weights.
- `wikis/riser-projects/wiki/sources/ri-sbop-weight-data-family.md` documents `RSU-0077` as a shallow-water tension-to-rotate workbook with four case sheets, a mini stack-up plus top-tension chain, and operating tension to apply. This is the wave-6 tracer-bullet target for lifting the batch count from 8/21 to at least 9/21.

### Documents Consulted

- GitHub issue `#1470` defines this as wave-6 extraction plus missing-input closure, with paired `llm-wiki` then `digitalmodel` PRs, the `riser-projects-ingest` claim protocol, and the de-identified source-share/sha256 extraction contract.
- `docs/riser_database.md` defines the public/private boundary: public `digitalmodel` carries handles, bands, parse shapes, and engine code; values, exact counts, and source-specific provenance tokens stay wiki-side; leak and provenance gates must run before first push.
- Drive-file index query `31057-CAL-1031-03 Weight Budget During Installation` found exact spreadsheet hits for CHECKED `1031-02`, CHECKED `1031-03`, and DRAFT `1032-01` in the Phase-2 CAL family. The implementation will cite only de-identified source-share notation and content hashes in wiki artifacts; public `digitalmodel` plan/review files, commits, PR text, and issue comments will not include raw machine-local source paths.
- Related completed issue/PR pattern: `digitalmodel#1468` and paired `llm-wiki#830`/`digitalmodel#1480` established the current paired-PR sequence, leak/provenance tripwires, and fail-closed getter/crosswalk discipline.

### Gaps Identified

- No wave-6 wiki extraction artifacts exist yet for the remaining Phase-2 classes or the registry page pending-extraction list.
- No `digitalmodel` parser path exists for new wave-6 contract dialects beyond the current `16q-min-top-tension` and `16q-min-tension-endpoints` shapes.
- No schedule-assembly support exists for `RSU-0030..RSU-0033`; source evidence says those rows have schedules but lack weights/tensions, so support must remain fail-closed unless a paired source provides weights.
- No `RSU-0077` contract/schema/golden test exists yet, even though wiki evidence identifies it as the clearest source-backed low-water-depth lift candidate.
- No batch regression test currently asserts the target post-wave runnable count or the preserved honest missing-input reason for rows that remain source-incomplete.
- `digitalmodel` currently has no `docs/plans/README.md` index. This plan will create a minimal active-plan index row for `#1470` without attempting a retrospective index of every historical plan.

### Evidence

**Issue status** (verified 2026-07-08T01:30Z via `gh issue view`):

- `#1470` — OPEN — "Registry batch coverage: lift 8/21 runnable via wave-6 extraction + missing-input closure"; labels: `cat:engineering`, `lane:codex`.

**File existence** (verified 2026-07-08T01:35Z):

- EXISTS: `examples/workflows/riser-stackup-registry-batch/run.py`
- EXISTS: `src/digitalmodel/drilling_riser/schedule_assembly.py`
- EXISTS: `tests/drilling_riser/test_schedule_assembly.py`
- EXISTS: `tests/drilling_riser/test_assembly_golden.py`
- EXISTS: `docs/riser_database.md`
- MISSING: `docs/plans/README.md` in `digitalmodel`

**Reproduction proofs** (verified 2026-07-08T01:38Z):

Command:

```bash
env LLM_WIKI_PATH=<local-llm-wiki-checkout> \
  PYTHONPATH=src \
  .venv/bin/python \
  examples/workflows/riser-stackup-registry-batch/run.py
```

Public-safe summary of output (numeric wiki-side values redacted by design):

- Runnable contract-chain rows: `RSU-0010`, `RSU-0012`, `RSU-0014`, `RSU-0023`, `RSU-0075`
- Runnable schedule-assembly rows: `RSU-0019` PASS, `RSU-0021` FINDING, `RSU-0040` FINDING
- Missing-input rows: `RSU-0001`, `RSU-0009`, `RSU-0027`, `RSU-0028`, `RSU-0030`, `RSU-0031`, `RSU-0032`, `RSU-0033`, `RSU-0038`, `RSU-0041`, `RSU-0042`, `RSU-0050`, `RSU-0077`
- Summary line: `8/21 as-planned drilling RSUs runnable (5 contract-chain + 3 schedule-assembly; 13 missing-inputs)`

The full command prints private wiki-side computed values. Those values, raw local checkout paths, and raw source-share paths must stay out of public `digitalmodel` plans, review artifacts, comments, PR bodies, and commits.

**Distinct source count:** 10+ sources across issue, repo code/tests/docs, wiki pages, and drive index.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md` |
| Plan index | `docs/plans/README.md` |
| Plan review — provider availability | `scripts/review/results/2026-07-08-plan-1470-provider-unavailable.md` |
| Plan review — engineering subagent | `scripts/review/results/2026-07-08-plan-1470-subagent-engineering.md` |
| Plan review — governance subagent | `scripts/review/results/2026-07-08-plan-1470-subagent-governance.md` |
| Focused re-review — engineering | `scripts/review/results/2026-07-08-plan-1470-r2-engineering.md` |
| Focused re-review — governance | `scripts/review/results/2026-07-08-plan-1470-r2-governance.md` |
| Final focused re-review — governance | `scripts/review/results/2026-07-08-plan-1470-r3-governance.md` |
| Wiki source/proposal/calc-contract updates | `llm-wiki/wikis/riser-projects/wiki/...` |
| dm batch workflow | `examples/workflows/riser-stackup-registry-batch/run.py` |
| dm parser | `src/digitalmodel/drilling_riser/schedule_assembly.py` and/or `src/digitalmodel/drilling_riser/calc_contracts.py` |
| dm parser tests | `tests/drilling_riser/test_schedule_assembly.py`, `tests/drilling_riser/test_assembly_golden.py` |
| dm batch tests | `tests/drilling_riser/` and `tests/riser_database/` focused batch/provenance tests |
| Public seed rows, if new handles are required | `scripts/riser_database/sources.yml` plus generated `data/riser_database/*` |

---

## Deliverable

A paired `llm-wiki` then `digitalmodel` implementation that first promotes `RSU-0077` into a source-backed runnable row, extends fail-closed parser coverage only where source evidence is complete, and raises the registry-batch runnable count from 8/21 to at least 9/21.

---

## Pseudocode

```text
claim riser-projects-ingest in llm-wiki
extract RSU-0077 first:
    create/update the SBOP-weight source page contract for the four shallow-water cases
    map the documented tension-to-rotate chain into an explicit calc dialect
    if the source cannot be mapped to the batch's top-tension semantics, stop for replan
for each wave-6 source candidate:
    classify status: current CHECKED, lineage-only, DRAFT, model-input, structural, wear, VIV, or tension-chain
    if source can support 16Q top-tension or schedule assembly:
        create/update source page with sha256 doc_key and reproduction block
        add proposal/calc_contracts entry with public-safe rsu_id and calc key
        avoid raw local paths; use de-identified source-share notation
    else:
        document why the source is out-of-scope for runnable count

in digitalmodel:
    write failing RSU-0077 contract/schema/golden tests first
    write failing in-context tests for each new contract/parser shape
    load calc contracts through existing bounded wiki readers
    add parser dialects only for source-backed text extraction
    add RSU to SCHEDULE_RSU_IDS only when both schedule and weight/library inputs exist
    keep X-Drill RSU-0030..0033 missing if only schedules exist
    run batch and assert count/status reasons
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create/modify | `llm-wiki/wikis/riser-projects/wiki/sources/<wave-6-pages>.md` | Source-page extraction with sha256 doc_keys, reproduction blocks, and no raw local paths |
| Modify | `llm-wiki/wikis/riser-projects/wiki/datasets/stackup-registry.md` | Add or update registry rows only if wave-6 extraction creates new RSU handles |
| Modify | `llm-wiki/wikis/riser-projects/wiki/data/calc_contracts*.yml` or established calc-contract location | Add new source-backed calc contracts for runnable candidates |
| Modify | `src/digitalmodel/drilling_riser/calc_contracts.py` | Recognize the `RSU-0077` shallow-water tension contract and any later fail-closed contract dialect needed by extracted sources |
| Modify | `src/digitalmodel/drilling_riser/schedule_assembly.py` | Add parser pages/parse shapes only for RSUs with complete text-extractable schedule plus weight/library evidence |
| Modify | `examples/workflows/riser-stackup-registry-batch/run.py` | Surface new runnable rows or more specific missing-input reasons |
| Modify | `tests/drilling_riser/test_assembly_golden.py` | Add contract-chain goldens for new runnable calc contracts |
| Modify | `tests/drilling_riser/test_schedule_assembly.py` | Add schedule-assembly tests or fail-closed tests for source-incomplete rows |
| Modify | `tests/riser_database/test_stackup_rig.py`, `tests/riser_database/test_provenance_tripwire.py` | Update public handle/provenance expectations only if new rows land |
| Modify/generated | `scripts/riser_database/sources.yml`, `data/riser_database/*` | Public handles only, if new RSU rows are required |
| Create | `docs/plans/README.md` | Minimal active-plan index row required by the issue-planning workflow |

---

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_registry_batch_current_baseline_before_wave6` | Characterization baseline captures current 8/21 state before implementation | Current `llm-wiki` checkout | 8 runnable rows and 13 missing-input rows, with values not committed |
| `test_rsu0077_shallow_water_contract_schema` | `RSU-0077` carries the complete source-backed four-case contract keys | `RSU-0077`, new calc dialect | All case rows and governing-operation fields present; missing key fails closed |
| `test_rsu0077_shallow_water_tension_golden` | `RSU-0077` reproduces the documented shallow-water tension-to-rotate workbook chain | `RSU-0077` contract | Computed governing top-tension value matches the documented contract tolerance |
| `test_wave6_contract_schema_<rsu>` | New wiki calc contract has required keys for a source-backed runnable candidate | New `rsu_id`, `calc` | Required keys present; missing key fails closed |
| `test_wave6_min_top_tension_golden_<rsu>` | New contract-chain row reproduces documented top-tension chain | New contract | Computed result matches contract tolerance used by existing goldens |
| `test_wave6_schedule_assembly_<rsu>` | New schedule parser assembles only complete schedule + weight/library inputs | New complete source page | Positive physical checks and documented endpoint comparison |
| `test_xdrill_schedule_only_rows_stay_missing` | `RSU-0030..RSU-0033` do not become runnable from schedule-only evidence | X-Drill rows | Fail-closed missing-input reason unless weight source is paired |
| `test_rsu0038_raster_only_still_fails_closed` | Existing raster-only guard remains intact | `RSU-0038` | `ScheduleAssemblyError` |
| `test_known_findings_not_silently_rebased` | `RSU-0021` and `RSU-0040` findings stay findings absent new source evidence | Existing endpoints | Finding bands remain pinned or require explicit source-backed update |
| `test_registry_batch_wave6_count` | Batch summary reflects the reviewed post-wave status | In-context wiki checkout | At least 9/21 runnable and `RSU-0077` no longer `missing-inputs` |
| `test_public_rows_no_values_or_private_tokens` | Public seed/table updates do not leak private values, paths, or source tokens | `scripts/riser_database/sources.yml`, generated tables | Leak/provenance tripwires pass |

---

## Acceptance Criteria

- [ ] `llm-wiki` claim `riser-projects-ingest` is acquired before wiki edits and released after push.
- [ ] Wiki PR lands first or remains the base branch for the `digitalmodel` PR; `digitalmodel` never references unpushed local wiki-only changes.
- [ ] Wave-6 extraction artifacts use the established source-page + proposal + calc-contract/reproduction pattern with sha256-pinned source document keys.
- [ ] `RSU-0077` is the first implementation tracer bullet. If extraction proves it cannot map to the batch's top-tension semantics, stop and revise this plan before any broader implementation.
- [ ] Public `digitalmodel` plan files, review artifacts, commits, PR bodies, and issue comments do not include raw machine-local paths, raw source-share paths, or wiki-side numeric values.
- [ ] Parser changes are fail-closed: no fuzzy matching, bounded reads, schema/key checks, and explicit errors for unsupported rows.
- [ ] `RSU-0030..RSU-0033` remain honestly unrunnable unless implementation finds a source-backed weight/library pair; schedule-only evidence is insufficient.
- [ ] Known documented inconsistencies (`RSU-0021` Nen-1 and `RSU-0040` Mad Dog) are not "fixed" by tolerance widening or undocumented loads.
- [ ] In-context batch run with `LLM_WIKI_PATH=<reviewed wiki checkout>` reports at least `9/21` runnable, with `RSU-0077` lifted from `missing-inputs`, and writes only gitignored local results.
- [ ] Focused tests pass: `tests/drilling_riser/` and `tests/riser_database/`.
- [ ] Before first push, implementation creates intent-to-add entries for new public artifacts (`git add -N <new-files>`) so untracked files are visible to diff-based scanners.
- [ ] Legal/provenance gates pass before first push: `tests/riser_database/test_leak_gate.py`, `tests/riser_database/test_provenance_tripwire.py`, `scripts/enforcement/check-no-abs-paths.sh <changed-public-files>`, and, from `workspace-hub`, `scripts/legal/legal-sanity-scan.sh --repo=../digitalmodel/.worktrees/<issue-1470-impl> --diff-only` while changes are uncommitted. If implementing in the canonical sibling checkout instead, use `--repo=../digitalmodel`.
- [ ] Implementation preflight verifies both user approval signals for the reviewed plan revision before code writes: live issue label `status:plan-approved` and `.planning/plan-approved/1470.md`.
- [ ] Adversarial code/artifact review runs after implementation; no self-merge.

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Provider CLI fanout | UNAVAILABLE | Sanitized availability record at `scripts/review/results/2026-07-08-plan-1470-provider-unavailable.md`; unavailable-provider records do not satisfy the review gate |
| Engineering subagent | MAJOR | RSU-0077 under-specified; runnable-count escape hatch too weak; legal scanner command invalid for worktree; review artifact status inconsistent |
| Governance subagent | MAJOR | Review artifacts false/incomplete; missing plan index; leakage controls under-scoped; legal scan topology and untracked-file blind spot; approval gate not locked |
| Focused engineering re-review | APPROVE | Prior engineering MAJORs resolved; legal scan topology verified |
| Focused governance re-review | MAJOR | Only remaining blocker was artifact governance drift after adding `r2-engineering`; fixed by listing the r2 artifacts in this revision |
| Final governance re-review | APPROVE | Artifact list, index, leakage controls, approval gate, `git add -N` scanner caveat, and worktree legal-scan topology verified |

**Overall result:** PASS for plan-review after provider-degraded review. Provider CLIs were unavailable in this runtime; two independent subagent reviews returned MAJOR, the plan was revised, and focused re-reviews returned APPROVE with no remaining blockers. Implementation remains blocked pending user approval.

Revisions made based on review:

- Made `RSU-0077` the explicit tracer bullet for the first runnable lift.
- Replaced the no-lift acceptance escape hatch with a minimum `9/21` runnable criterion.
- Added `RSU-0077`-specific schema/golden/batch tests.
- Corrected the legal-scan invocation for worktree-based implementation and required `git add -N` before diff-only scanning.
- Removed raw local paths from the tracked plan and replaced raw provider-failure transcripts with a sanitized availability artifact.
- Added an explicit plan index artifact and approval-marker preflight.
- Updated artifact references to reflect sanitized provider availability, `MAJOR` subagent reviews, and focused r2 review artifacts.
- Added final governance re-review after the r2 artifact-list blocker was patched.

---

## Risks and Open Questions

- **Risk:** The remaining fourteen CHECKED Phase-2 workbooks are mostly not top-tension chain sources; attempting to force them into runnable registry rows would create false coverage. Implementation must classify source utility before code changes.
- **Risk:** `RSU-0077` is source-backed but may not map cleanly to the existing batch's API RP 16Q semantics. If that happens, implementation must stop for plan revision rather than pass on a no-lift explanation.
- **Risk:** Two known source inconsistencies remain documented. Without new source evidence, they are findings, not implementation defects.
- **Risk:** This is a paired private/public workflow. Auto-sync or claim clobber in `llm-wiki` can overwrite wiki edits; implementation must follow the claim protocol and recover from remote state before continuing.

---

## Complexity: T3

T3 because the work spans paired repositories, private-source extraction, public leak constraints, engineering parser/tests, and review/merge ordering across `llm-wiki` and `digitalmodel`.
