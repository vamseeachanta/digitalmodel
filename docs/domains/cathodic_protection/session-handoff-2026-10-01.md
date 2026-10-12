# Cathodic protection: session handoff (2026-09-27 to 2026-10-01)

Continues `session-handoff-2026-09-27.md`. The prompt at the end is written for a Codex agent taking over the workstream.

## Delivered in this period (merged to main)

| Issue | Pull request(s) | Delivered |
|---|---|---|
| D1 revisited, F103 default | #2246 | Client use with engineer-of-record check for B401 offshore and F103 bracelet; F103 default edition 2019 |
| F103 route key | #2250 | `DNV_RP_F103`; `DNV_RP_F103_2010` alias pins edition 2010 |
| #2247 | #2248, #2251 | Stray current, galvanic and ICCP life re-modelled from open literature (provisional, experimental); fuel-system check removed |
| #2256 | #2258 | F103 2019 field-joint coating ids mapped through the 2019 table (`field_joint_infill`) |
| #2259 step 1 | #2265 | ABS ships route blocked until rebuilt |
| #2264 | llm-wiki #922, #2266 | Public-source evidence pages for EN 50162, ISO 18086, ISO 15589-1, NACE SP0169/SP0572, NORSOK M-503; `evidence_class` on every provisional value; EN 50162 band boundary corrected |
| #2262 | #2267 | Concrete-embedded and buried zones with per-zone anode families |
| #2260 | #2268 | Pipeline terminal anode banks with attenuation and far-end potential check |
| #2263 | #2269 | Riser bases, mudmats, hatch covers; temporary, wet-storage and retrofit phases |
| #2261 | #2270 | Multi-component risers with continuity and anode allocation |
| #2259 step 2 (wiki) | llm-wiki #926 | ABS Guidance Notes on CP of Ships (Dec 2017) citation page and datasets |

Benchmarks of 2026-09-27 against real CP design reports: de-identified summary on #1852; the private record stays on the analysis host.

## Open at handover

1. **PR #2271** (closes #2259): rebuilt ABS ships route; reproduces the benchmark hull design within 0.1% (old route −32%/−49%). Owner accepted the three interpretations on 2026-10-01; status client use with engineer-of-record check. Merge when all four workflows are green.
2. **Report-standard draft**: the owner asked for the shared engineering reporting standard (review draft, workspace-hub branch `chore/3925-engineering-reporting-standard`, revision `c955b86159acc4f9e9f5964690c24dd1411e5b48`) to be applied to one report, the CP anode-design report for the jacket regression case, as a commentable HTML draft. A run was in progress at handover; the engine and templates are not to change.
3. **Cleanup** of finished worktrees on the workstation and the analysis host.

## Handover prompt for a Codex agent

```markdown
# Handover: digitalmodel cathodic protection (CP) — continue from 2026-10-01

You are taking over an in-flight engineering workstream in vamseeachanta/digitalmodel (src/digitalmodel/cathodic_protection/) and the private wiki vamseeachanta/llm-wiki. Read first, in order: docs/domains/cathodic_protection/session-handoff-2026-10-01.md (this file), session-handoff-2026-09-27.md, _index.md (module map, Use status table), standards-inventory.md (provisional values and evidence), docs/domains/reporting/standard-report-engine.md.

## Open items
1. PR #2271 (closes #2259): merge once Quality Gates, Quality Gates by Domain, Build API Docs and Parametric atlas drift are all green and mergeability is CLEAN. If it conflicts with main, merge origin/main into feat/cp-2259-abs-rebuild, resolve keeping both sides, re-run tests.
2. Report-standard draft: apply the shared engineering reporting standard (workspace-hub branch chore/3925-engineering-reporting-standard, docs/standards/engineering-reporting-conventions-proposed.html and reformat-engineering-report-prompt.md, revision c955b86159acc4f9e9f5964690c24dd1411e5b48) to the CP anode-design report for tests/fixtures/cathodic_protection/workflow_inputs/jacket.yml. Produce a commentable HTML draft with a JSON comment sidecar, read-back verification, digest, change list and flagged conflicts, saved outside the repository. Do not change the report engine or templates (the standard is a review draft; one report is not adoption). The previous run may have left output in the orchestrator's scratch area; if you cannot find it, redo it.
3. Cleanup: remove finished worktrees with `git worktree remove` (never rm -rf on workspace roots). On the workstation: the wt-digitalmodel-cpreport and wt-digitalmodel-cphandover worktrees (the latter after its PR merges); wt-digitalmodel-cp2259 and cp2264 are already detached and may need manual deletion by the owner. On the analysis host: the cp-2259 to cp-2263 and llm-wiki-abs-ships worktrees (paths in the owner's private notes). Delete merged branches feat/cp-2261-rise and feat/cp-2262-conc only when no open PR uses them as base.

## State on main
Routes: B401 offshore and F103 bracelet (client use with engineer-of-record check); DNV_RP_F103_anode_bank; concrete and buried zones with anode families; inputs.components[] risers; inputs.riser_base_assessment phases (mutually exclusive with components[]); ABS offshore legacy. Defaults B401 2021, F103 2019. Experimental with provisional, evidence-classed values: stray current, galvanic, ICCP anode life (EN 50162, ISO 18086, ISO 15589-1, NACE SP0169/SP0572 are not owned).

## Next work (confirm with the owner first)
Benchmark #1852 on the first real CP job; re-check benchmark cases A, B, D against the new routes (private comparison on the analysis host, de-identified summary to #1852); #2261 deferrals (F103:2003 riser acceptance, field-joint/linepipe compatibility now NOT_EVALUATED); migrate worked examples off the old hydrodynamics router.

## Rules
- Merge authority belonged to the orchestrating Claude session; ask the owner before your first merge.
- Client documents stay on the analysis host. No client, operator, contractor, vessel or project names, document numbers, or /mnt or /home paths in any repository, issue or PR; fixtures de-identified with hand-derived expected values; CI absolute-path gate rejects /mnt paths.
- Licensed standards: numbers, table ids and captions only; no clause prose, no watermark lines; never download unauthorised copies.
- TDD; scoped tests (tests/cathodic_protection, tests/specialized/cathodic_protection, tests/reporting); ruff and mypy on touched files; CHANGELOG [Unreleased] line; a docs/plans plan per issue.
- No unrequested scope: no new agent-rule files, no issue comments unless asked. Never delete a stacked-PR base branch before retargeting. No auto-merge on this repository.

## Environment
- Analysis host: use ~/.local/bin/codex (the snap build cannot read ~/.codex); private reports in the benchmark folder recorded in the owner's private notes; per-worktree .venv; the host's main digitalmodel checkout belongs to another session, so work in your own worktrees.
- Workstation: the main checkout's editable install shadows worktree sources, so run pytest with the editable finder removed and the worktree src prepended. The local venv lacks bs4 and has a pyarrow/numpy mismatch, so test_engine_dispatches_cathodic_protection_to_adapter fails locally only; CI is authoritative. Codex sandboxes there cannot write worktree git metadata, so commit outside the sandbox. The host is short on memory.

Finish by reporting to the owner: what merged, the report draft location and verification, what remains, and any decision you need.
```
