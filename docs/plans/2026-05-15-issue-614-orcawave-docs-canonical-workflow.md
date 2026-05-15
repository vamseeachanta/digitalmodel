# Plan for #614: OrcaWave: update docs for canonical spec-to-run workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T1
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/614
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-614-claude.md | scripts/review/results/2026-05-15-plan-614-codex.md | scripts/review/results/2026-05-15-plan-614-gemini.md

---

## Scope

Documentation and docs-guard scope. Do not implement new user-facing CLI behavior here; link future commands as planned/dependent when not yet implemented. Documentation must distinguish current commands from future work so users do not follow non-existent workflows. In particular, #605 self-contained packaging is planned, not current behavior. The only code change in scope is lightweight docs-test coverage that guards canonical/current docs against stale legacy command examples.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#614` - OPEN - `OrcaWave: update docs for canonical spec-to-run workflow`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - direct `ls docs/plans` observation shows standalone plan files and no `docs/plans/README.md` index/template, so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `docs/domains/orcawave/README.md` contains OrcaWave domain documentation and older examples.
- `docs/domains/orcawave/examples/` contains parameter/reference material that should remain but can be cross-linked.
- `cli.py` is the source of truth for existing commands: `validate-spec`, `convert-spec`, and `run-orcawave`.
- #500 and #605-#613 define planned/current path-resolution, packaging, mesh preparation, given-mesh, mesh QA, auxiliary mesh, licensed acceptance, result contract, convergence, and doctor workflows.
- `docs/domains/orcawave/README.md` currently links to missing `diffraction/QUICK_START.md` and contains stale `src/digitalmodel/modules/...` and `digitalmodel.orcawave...` examples, including old OrcaWave module paths.
- `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` exists at HEAD as a parallel argparse entry point. #614 must not claim it is deleted; it should make current/canonical docs point to the Click CLI in `cli.py` and label `diffraction_cli.py` references as legacy/alternate argparse references when they remain.
- `docs/domains/orcawave/PHASE_1_COMPLETION.md`, `docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md`, `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md`, and `docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md` contain additional stale `diffraction_cli.py`, `src/digitalmodel/modules/...`, or old import references.

### Gaps identified

- Stale examples can point users at legacy scripts instead of the current Click CLI.
- No concise quickstart currently ties path resolution, dry-run behavior, and licensed execution together.
- Future commands need to be called out as planned work, not documented as available before implementation.

### Evidence

Commands and inspections used while drafting:

```text
sed -n '1,220p' AGENTS.md
ls -la docs/plans
rg -n "Plan Index|Adversarial Review Summary|Review artifacts" docs/plans -S
sed -n '400,590p' src/digitalmodel/hydrodynamics/diffraction/cli.py
sed -n '1,180p' src/digitalmodel/hydrodynamics/diffraction/spec_converter.py
sed -n '220,560p' src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py
sed -n '1,280p' src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py
rg -n 'diffraction_cli\\.py|src/digitalmodel/modules/|digitalmodel\\.orcawave|diffraction/QUICK_START' docs/domains/orcawave -g '*.md'
```

Reproduction proof: the `rg` command above found stale legacy CLI/module/import references in `docs/domains/orcawave/README.md`, `docs/domains/orcawave/PHASE_1_COMPLETION.md`, `docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md`, `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md`, and `docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md`, plus a broken README quickstart link to missing `diffraction/QUICK_START.md`. Additional audit targets required by issue #614 include `docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md`, `docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md`, `docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md`, and `docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md`, which contain legacy commands such as `run_orcawave_diffraction_improved.py`, `diagnose_orcawave_api.py`, and `run_orcawave_shell.py`.

## Deliverable

OrcaWave documentation reflects the current canonical `spec.yml -> validate -> convert YAML -> dry-run/licensed run` command workflow, labels #605 self-contained packaging as planned, and removes or labels stale legacy command examples.

## Proposed Tasks

1. Audit OrcaWave docs for stale legacy command/path/import examples, including `python diffraction_cli.py`, `src/digitalmodel/modules/`, `digitalmodel.orcawave...`, old OrcaWave module paths, broken quickstart links, and legacy script commands presented as current/recommended workflows (`run_orcawave_diffraction.py`, `run_orcawave_diffraction_improved.py`, `diagnose_orcawave_api.py`, `run_orcawave_benchmark.py`, `execute_orcawave_parallel.py`, `run_orcawave_shell.py`). Do not treat working example scripts as stale merely because they are scripts; they can remain only when clearly separated from canonical CLI docs and labeled historical/example/reference instead of recommended/current.
2. Replace current-command examples with `diffraction validate-spec`, `diffraction convert-spec --solver orcawave`, and `diffraction run-orcawave`, while stating that `convert-spec` at HEAD writes OrcaWave YAML and #605 will add self-contained packaging.
3. Add a minimal current quickstart: prepare spec, validate, convert YAML, dry-run, then run on licensed host. Add a separate "Planned workflow" callout for #500 and #605-#613.
4. Document mesh path-resolution/package behavior based on current #500/#605 state; label future behavior as planned if not implemented.
5. Link related issues explicitly: #500 and #605-#613, with planned/current wording.
6. Add required docs guard tests. The guard should fail stale canonical docs, allow working example-script references only when labeled as examples/reference/historical, and require any `diffraction_cli.py` or legacy script references to be under a heading or nearby label containing `legacy`, `alternate`, `argparse`, `reference`, `example`, or `historical` rather than quickstart/current/recommended workflow. Canonical/current sections are headings or nearby text containing `quickstart`, `current`, `canonical`, `recommended`, or `usage` in non-historical docs.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-614-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-614-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, optional best-effort artifact; `UNAVAILABLE` does not block if the review tool cannot complete) | `scripts/review/results/2026-05-15-plan-614-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-614-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `docs/domains/orcawave/README.md` | canonical quickstart and stale example cleanup |
| Modify | `docs/domains/orcawave/PHASE_1_COMPLETION.md` | label or update legacy CLI/module references |
| Modify | `docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md` | label or update stale module path/import references |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md` | label or update legacy CLI/module references |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md` | label or update stale module path references |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md` | remove/label recommended legacy API script workflows |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md` | label legacy investigation scripts as historical/reference |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/ROOT_CAUSE_ANALYSIS.md` | replace current/recommended legacy execution script guidance or label it historical |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/EXECUTION_TEST_RESULTS.md` | label shell/benchmark legacy execution options as historical/reference |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/THREAD_USAGE_SUMMARY.md` | label validated legacy execution commands as historical/reference or update to canonical CLI where applicable |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md` | label created diagnostic/execution scripts as historical context, not current workflow |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/AGENT_PROMPT.md` | label benchmark script prompt references as historical/reference |
| Modify | `docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md` | replace or mark stale example command as historical/reference |
| Modify | `docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md` | replace or mark stale shell-script command as historical/reference |
| Create/modify | `tests/docs/test_orcawave_docs.py` | required docs guard for canonical/current command references |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_docs_reference_existing_cli_commands` | docs command names match Click commands | README text | no legacy-only examples |
| `test_orcawave_docs_mark_future_commands_as_planned` | no premature docs | future workflow refs | issue links/planned wording |
| `test_orcawave_docs_current_quickstart_paths_present` | current quickstart complete | README text | validate/convert-YAML/dry-run/licensed-run path present without claiming package is current |
| `test_orcawave_docs_no_stale_module_paths_in_canonical_docs` | old module paths/imports removed from current docs | docs text with `src/digitalmodel/modules/` or `digitalmodel.orcawave` | references absent from canonical/current sections or explicitly labeled legacy/alternate reference |
| `test_orcawave_docs_legacy_cli_refs_labeled_noncanonical` | existing argparse entry point not presented as canonical | docs text with `diffraction_cli.py` | references are absent from quickstart/current docs or marked legacy/alternate argparse/reference |
| `test_orcawave_docs_legacy_script_refs_labeled_noncanonical` | old helper scripts are not presented as current/recommended commands | docs text with `run_orcawave_diffraction_improved.py`, `diagnose_orcawave_api.py`, `run_orcawave_benchmark.py`, `execute_orcawave_parallel.py`, or `run_orcawave_shell.py` | references are absent from current/recommended docs or marked legacy/example/reference/historical |
| `test_orcawave_docs_all_audit_matches_are_in_file_plan_or_historical` | audit scope cannot silently miss active legacy docs | grep results for stale command/path patterns under `docs/domains/orcawave` | every matched file is either in the #614 file-change list or has nearby historical/reference labeling enforced by the guard |
| `test_orcawave_docs_examples_scripts_allowed_only_as_examples` | working scripts not mislabeled stale | example-script references under `docs/domains/orcawave/examples/` | allowed only under headings/nearby text containing examples/reference/historical wording |
| `test_orcawave_docs_no_broken_quickstart_link` | README link valid | README links | no link to missing `diffraction/QUICK_START.md` |

## Acceptance Criteria

- [ ] Stale legacy command/path/import examples in all affected OrcaWave docs identified by the audit, including L01 benchmark investigation docs and `examples/L01_default_vessel`, are replaced or marked legacy/alternate/reference/historical.
- [ ] Docs include a minimal current `spec.yml -> validate -> convert YAML -> dry-run -> licensed solve` quickstart without claiming #605 packaging is current.
- [ ] Docs explain mesh path resolution and package layout according to implemented/current behavior.
- [ ] Docs link to related issues #500 and #605-#613 with planned/current wording.
- [ ] Commands in current quickstart docs match actual Click CLI names for the scoped canonical commands: `validate-spec`, `convert-spec`, and `run-orcawave`.
- [ ] The #614 implementation/plan-review issue comment links the final docs plan and summarizes which stale docs were updated.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for Claude and Codex; each non-empty artifact contains a `## Verdict` section. Gemini is best-effort for this T1 docs plan and an `UNAVAILABLE` artifact does not block if Claude and Codex have no unresolved `MAJOR`.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |
| Gemini | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** The docs can easily overstate planned #605/#607/#610/#613 behavior. The implementation must mark future workflows as planned until the corresponding issue lands.
- **Risk:** Existing OrcaWave example scripts and `diffraction_cli.py` may remain useful as reference/alternate material. The goal is to remove them from the canonical quickstart or label them non-canonical, not claim they are deleted.
- **Open:** This T1 docs plan uses Claude and Codex as the required review pair; Gemini is best-effort because unavailable Gemini artifacts have occurred in this review batch.

## Complexity: T1

T1 justification: T1 documentation-only scope with limited file surface.
