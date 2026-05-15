# Plan for #614: OrcaWave: update docs for canonical spec-to-run workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T1
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/614
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-614-claude.md | scripts/review/results/2026-05-15-plan-614-codex.md | scripts/review/results/2026-05-15-plan-614-gemini.md

---

## Scope

Documentation-only. Do not implement new CLI behavior here; link future commands as planned/dependent when not yet implemented. Documentation must distinguish current commands from future work so users do not follow non-existent workflows. In particular, #605 self-contained packaging is planned, not current behavior.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#614` - OPEN - `OrcaWave: update docs for canonical spec-to-run workflow`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `docs/domains/orcawave/README.md` contains OrcaWave domain documentation and older examples.
- `docs/domains/orcawave/examples/` contains parameter/reference material that should remain but can be cross-linked.
- `cli.py` is the source of truth for existing commands: `validate-spec`, `convert-spec`, and `run-orcawave`.
- #500, #605, #606, #607, #610, and #613 define planned/current path-resolution, packaging, mesh preparation, given-mesh, licensed acceptance, and doctor workflows.
- `docs/domains/orcawave/README.md` currently links to missing `diffraction/QUICK_START.md` and contains legacy `src/digitalmodel/modules/diffraction_cli.py` commands.
- `docs/domains/orcawave/PHASE_1_COMPLETION.md` and `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md` contain additional stale `diffraction_cli.py` / old `src/digitalmodel/modules/diffraction` references.

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
rg -n 'diffraction_cli\\.py|src/digitalmodel/modules/diffraction|diffraction/QUICK_START' docs/domains/orcawave -g '*.md'
```

Reproduction proof: the `rg` command above found stale legacy CLI/module references in `docs/domains/orcawave/README.md`, `docs/domains/orcawave/PHASE_1_COMPLETION.md`, and `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md`, plus a broken README quickstart link to missing `diffraction/QUICK_START.md`.

## Deliverable

OrcaWave documentation reflects the current canonical `spec.yml -> validate -> convert YAML -> dry-run/licensed run` command workflow, labels #605 self-contained packaging as planned, and removes or labels stale legacy command examples.

## Proposed Tasks

1. Audit OrcaWave docs for stale legacy command examples, including `python diffraction_cli.py`, `src/digitalmodel/modules/diffraction*`, and broken quickstart links. Do not treat working `docs/domains/orcawave/examples/Lxx/run_orcawave.py` scripts as stale merely because they are scripts; they can remain as example scripts when clearly separated from canonical CLI docs.
2. Replace current-command examples with `diffraction validate-spec`, `diffraction convert-spec --solver orcawave`, and `diffraction run-orcawave`, while stating that `convert-spec` at HEAD writes OrcaWave YAML and #605 will add self-contained packaging.
3. Add a minimal current quickstart: prepare spec, validate, convert YAML, dry-run, then run on licensed host. Add a separate "Planned workflow" callout for #605 packaging, #606 mesh preparation, #607 given-mesh, #610 licensed acceptance, and #613 doctor.
4. Document mesh path-resolution/package behavior based on current #500/#605 state; label future behavior as planned if not implemented.
5. Link related issues explicitly: #500, #605, #606, #607, #610, and #613.
6. Add required docs guard tests. The guard should fail stale canonical docs, allow working `examples/Lxx/run_orcawave.py` references when labeled as examples, and require any `diffraction_cli.py` references to be historical rather than quickstart/current workflow.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-614-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-614-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-614-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-614-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `docs/domains/orcawave/README.md` | canonical quickstart and stale example cleanup |
| Modify | `docs/domains/orcawave/PHASE_1_COMPLETION.md` | label or update legacy CLI/module references |
| Modify | `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md` | label or update legacy CLI/module references |
| Modify | `docs/domains/orcawave/examples/` | update cross-links only if stale examples live there |
| Create/modify | `tests/docs/test_orcawave_docs.py` | required docs guard for canonical/current command references |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_docs_reference_existing_cli_commands` | docs command names match Click commands | README text | no legacy-only examples |
| `test_orcawave_docs_mark_future_commands_as_planned` | no premature docs | future workflow refs | issue links/planned wording |
| `test_orcawave_docs_current_quickstart_paths_present` | current quickstart complete | README text | validate/convert-YAML/dry-run/licensed-run path present without claiming package is current |
| `test_orcawave_docs_legacy_cli_refs_labeled_historical` | stale CLI refs are not canonical | docs text with `diffraction_cli.py` or old module paths | references are absent from quickstart/current docs or marked historical |
| `test_orcawave_docs_examples_scripts_allowed_as_examples` | working scripts not mislabeled stale | `examples/Lxx/run_orcawave.py` references | allowed only under examples/reference wording |
| `test_orcawave_docs_no_broken_quickstart_link` | README link valid | README links | no link to missing `diffraction/QUICK_START.md` |

## Acceptance Criteria

- [ ] Stale legacy command examples in README, PHASE_1 completion notes, and L01 comparison docs are replaced or marked historical.
- [ ] Docs include a minimal current `spec.yml -> validate -> convert YAML -> dry-run -> licensed solve` quickstart without claiming #605 packaging is current.
- [ ] Docs explain mesh path resolution and package layout according to implemented/current behavior.
- [ ] Docs link to related issues #500, #605, #606, #607, #610, and #613 with planned/current wording.
- [ ] Commands in current quickstart docs match actual Click CLI names for the scoped canonical commands: `validate-spec`, `convert-spec`, and `run-orcawave`.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
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
- **Risk:** Existing OrcaWave example scripts may remain useful as references. The goal is to remove them from the canonical quickstart or label them historical/example-only, not delete working reference material blindly.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T1

T1 justification: T1 documentation-only scope with limited file surface.
