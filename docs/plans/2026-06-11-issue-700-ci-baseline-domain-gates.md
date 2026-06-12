# Plan: digitalmodel #700 - CI baseline domain-gate routing

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/700
**Status:** draft
**Tier:** T3 (CI workflow semantics plus cross-domain test routing)
**Client:** N/A
**Project:** N/A
**Lane:** codex

## Context

- `Quality Gates` on the latest observed `main` run is green, so this plan will not rework the non-domain gate unless fresh evidence shows a regression.
- `Quality Gates by Domain` will remain the active blocker: PRs and main pushes currently select broad domain shards for changes that should be domain-scoped.
- The domain detector will route most `src/digitalmodel/*` changes to the full matrix unless the prefix appears in `SOURCE_DOMAIN_PREFIXES`. `src/digitalmodel/orcaflex/` and many other first-class packages are currently unmapped.
- `pyproject.toml` changes will always select the full matrix, even when the diff only declares package data for one domain.
- Routing fixes alone will not unblock PR [#702](https://github.com/vamseeachanta/digitalmodel/pull/702), because corrected routing will still select OrcaFlex-family shards that are already red on `main`.
- The plan will therefore cover both the routing contract and the selected-domain OrcaFlex/OrcaFlex-solver failures needed for the corrected routing contract to pass. Other stale broad-matrix failures will be surfaced as child follow-ups.

## Resource Intelligence Summary

### GitHub evidence to cite

- PR [#702](https://github.com/vamseeachanta/digitalmodel/pull/702) run `27380515825` selects and fails the broad domain matrix while branch-local checks pass:
  - `docs`: success
  - `Run Quality Gates`: success
  - `Detect touched test domains`: success
  - GitGuardian: success
  - failing domain shards: `tests-contracts`, `tests-infrastructure-core`, `tests-marine-engineering`, `tests-marine-ops-other`, `tests-misc`, `tests-naval-architecture`, `tests-orcaflex`, `tests-orcaflex-solver`, `tests-solver-smoke`, `tests-specialized`, `tests-structural`, `tests-subsea`, `Domain test aggregate`
- Latest observed `main` domain-gate run `27375765025` fails the same shard set.
- Latest observed `main` non-domain `Quality Gates` run `27375765010` succeeds.

### Reproduction proofs

Timestamp: 2026-06-11 America/Chicago.

Detector full-matrix reproduction from PR [#702](https://github.com/vamseeachanta/digitalmodel/pull/702) branch:

```bash
python scripts/ci/detect_touched_domains.py \
  --mode touched \
  --base origin/main \
  --head HEAD \
  --domains-file tests/DOMAINS.md \
  --output-format list
```

The command returns every domain, including unrelated shards such as `asset-integrity`, `contracts`, `marine-ops-other`, `subsea`, and `misc`.

Trigger classification reproduction:

```bash
python - <<'PY'
from scripts.ci.detect_touched_domains import is_full_matrix_trigger
paths = [
    "pyproject.toml",
    "src/digitalmodel/orcaflex/mooring_design.py",
    "src/digitalmodel/orcaflex/synthetic_rope_design.py",
    "src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml",
    "tests/orcaflex/test_synthetic_rope_design.py",
]
for path in paths:
    print(f"{path}: {is_full_matrix_trigger(path)}")
PY
```

Observed routing:

```text
pyproject.toml: True
src/digitalmodel/orcaflex/mooring_design.py: True
src/digitalmodel/orcaflex/synthetic_rope_design.py: True
src/digitalmodel/orcaflex/data/synthetic_rope_profiles.yml: True
tests/orcaflex/test_synthetic_rope_design.py: False
```

Representative stale domain failures reproduce locally:

```bash
.venv/bin/python -m pytest \
  tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_with_pretension \
  -q -p no:randomly -p no:sugar --tb=short
```

Tail:

```text
src/digitalmodel/orcaflex/mooring_design.py:300: in solve_catenary
    h_calc = H / w_kn * (math.cosh(V / H) - 1.0) if H > 0 else 0
E   OverflowError: math range error
```

```bash
.venv/bin/python -m pytest \
  tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_var_data_file \
  -q -p no:randomly -p no:sugar --tb=short
```

Tail:

```text
tests/test_orcaflex_agent.py:56: in test_generate_var_data_file
    output_file = generator.generate_var_data(temp_output_dir)
E   AttributeError: 'BaseFileGenerator' object has no attribute 'generate_var_data'
```

### Artifact Map

- `.github/workflows/quality-gates-by-domain.yml` will own event semantics, but changes to this workflow will continue to be treated as full-matrix CI infrastructure changes unless the user explicitly approves a maintainer override:
  - PRs will use merge-base/head touched-mode detection so branches behind `main` do not inherit unrelated base-side changes.
  - Pushes to `main`/`develop` will use before/after touched-mode detection.
  - Touched-domain contexts will be `Detect touched test domains`, `tests-<domain>`, and `Domain touched aggregate`.
- `.github/workflows/domain-baseline-diagnostics.yml` will own scheduled/manual full sweeps:
  - Diagnostic full-sweep contexts will be `Detect baseline domains`, `baseline-tests-<domain>`, and `Domain baseline aggregate`.
  - Diagnostic baseline jobs will upload artifacts and summarize failures without sharing touched-domain check names.
- Current GitHub enforcement evidence: `main` has no classic branch protection and the active ruleset does not define required status checks. This plan will not configure dynamic `tests-<domain>` matrix jobs as required; if status-check enforcement is introduced, only stable aggregate contexts such as `Domain touched aggregate` should be required.
- `scripts/ci/detect_touched_domains.py` will own path and diff classification.
- `tests/scripts/test_detect_touched_domains.py` will own regression coverage for detector routing.
- `.claude/quality-gates.yaml` will remain the source of runnable per-domain commands.
- `tests/DOMAINS.md` will remain the source of domain roots.
- `src/digitalmodel/orcaflex/mooring_design.py`, `src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py`, and `tests/test_orcaflex_agent.py` may require selected-shard fixes so `tests-orcaflex` can pass after routing is corrected.
- `tests/solvers/orcaflex/` and `src/digitalmodel/solvers/orcaflex/` may require selected-shard fixes if conservative core routing selects `orcaflex-solver`.

## Scope

### In Scope

- Add source-prefix routing for the specific package domains required by this issue:
  - `src/digitalmodel/orcaflex/` -> `orcaflex` and `orcaflex-solver` until a narrower import-boundary test proves solver isolation
  - `src/digitalmodel/solvers/orcaflex/` -> `orcaflex-solver`
  - `src/digitalmodel/workflows/agents/orcaflex/` -> `workflows` and `orcaflex`
- Extend detector internals to allow one path to select multiple domains.
- Leave broader top-level package/domain mapping expansion to a follow-up unless additional prefixes are needed by this issue.
- Add a diff-aware `pyproject.toml` classifier so package-data-only declarations can route to the affected domain instead of forcing the full matrix.
- Keep dependency, build-system, pytest, and tool-configuration changes conservative: those will continue to select the full matrix.
- Update `quality-gates-by-domain.yml` so push events can use touched-mode detection via `github.event.before` and `github.sha`, rather than always using full mode.
- Define the required-vs-diagnostic split for full baseline sweeps in this plan rather than leaving it as an implementation-time decision:
  - touched-domain PR/push checks will remain in `quality-gates-by-domain.yml`;
  - scheduled/manual full sweeps will move to `domain-baseline-diagnostics.yml` with `baseline-tests-*` job names and a non-required diagnostic aggregate.
- Fix the selected `tests-orcaflex` and `tests-orcaflex-solver` baseline failures required to unblock OrcaFlex-scoped PRs after routing is corrected.
- Post or update follow-up GitHub issues for stale domain test failures that remain outside the corrected routing contract.

### Out of Scope

- Fixing every stale failure in `tests-contracts`, `tests-misc`, `tests-subsea`, `tests-structural`, and every other broad shard that the corrected [#702](https://github.com/vamseeachanta/digitalmodel/pull/702)-equivalent routing will not select.
- Changing individual domain commands in `.claude/quality-gates.yaml` unless needed to make the corrected routing contract executable.
- Weakening branch-relevant checks for touched domains.
- Marking full baseline failures as solved without child issue evidence.

## Deliverables

- Updated detector logic in `scripts/ci/detect_touched_domains.py`.
- Unit tests in `tests/scripts/test_detect_touched_domains.py` for source-prefix and `pyproject.toml` package-data routing.
- Workflow update in `.github/workflows/quality-gates-by-domain.yml` for PR/push touched-domain routing. This change will remain full-matrix-triggered by design and will need either a green full matrix or explicit maintainer override; do not hide workflow-infrastructure changes behind narrow routing.
- New diagnostic workflow `.github/workflows/domain-baseline-diagnostics.yml` for scheduled/manual full baseline sweeps with distinct `baseline-tests-*` contexts.
- OrcaFlex-family shard fixes sufficient for `tests-orcaflex` and `tests-orcaflex-solver` to pass when selected by an OrcaFlex-scoped PR.
- A concise issue comment on [#700](https://github.com/vamseeachanta/digitalmodel/issues/700) with the final routing contract, verification commands, and any child issues filed for stale full-suite failures.

## Approach

1. Land the detector bootstrap first:
   - Keep workflow-file changes as full-matrix triggers.
   - Keep this first PR detector-only so the current workflow selects the existing green `tests-workflows` shard via the existing `scripts/ci/detect_touched_domains.py` override.
   - Do not claim the workflow-file change can avoid full-matrix CI; that would reduce CI-infrastructure coverage.
2. Add red tests for source-prefix routing:
   - `src/digitalmodel/orcaflex/...` will select `orcaflex` and `orcaflex-solver`, not the full matrix.
   - `src/digitalmodel/solvers/orcaflex/...` will select `orcaflex-solver`, not the full matrix.
   - `src/digitalmodel/workflows/agents/orcaflex/...` will select `workflows` and `orcaflex`, not workflows alone.
   - Existing mapped prefixes such as `src/digitalmodel/citations/` will continue to select their current domain.
   - Unknown source prefixes will continue to select the full matrix until they have explicit domain routing.
3. Add red tests for `pyproject.toml` routing:
   - A package-data-only diff adding `orcaflex/data/*.yml` will select `orcaflex`.
   - A dependency or build-system diff in `pyproject.toml` will select the full matrix.
   - A mixed `pyproject.toml` diff with both package data and dependency changes will select the full matrix.
4. Refactor detector internals without changing the CLI contract:
   - Preserve `--mode touched`, `--mode full`, and `--output-format`.
   - Use union-of-all matching source prefixes so overlapping routes like `src/digitalmodel/workflows/agents/orcaflex/` select both `workflows` and `orcaflex`; do not rely on insertion-order first match.
   - For PR mode, compute `git merge-base <base> <head>` and diff `merge-base..head`, not raw `base..head`.
   - For push mode, diff `before..sha` only when `before` is non-zero and reachable; otherwise fall back to `--mode full`.
   - For `pyproject.toml`, structurally parse base and head TOML and route narrowly only when the sole semantic change is under `[tool.setuptools.package-data].digitalmodel`.
   - Replace the current unconditional `pyproject.toml` entry in `FULL_MATRIX_PATHS` with the structural classifier; invalid TOML, missing refs, or any non-package-data semantic change will fall back to full matrix.
   - Translate package-relative data globs such as `orcaflex/data/*.yml` to `src/digitalmodel/orcaflex/data/*.yml` before applying the source-prefix map.
   - Treat package-data-only diffs with unmapped package-relative prefixes as full-matrix triggers.
   - Keep failures fail-closed: parse errors, unreachable refs, or unclassified config diffs will return the full matrix instead of exiting with detector error when a conservative matrix can be produced.
5. Update `quality-gates-by-domain.yml` event logic after the detector bootstrap lands:
   - Pull requests will pass PR base/head refs to the detector, which will use merge-base semantics internally.
   - Push events will call touched mode with `github.event.before` and `github.sha`, with zero-SHA/unreachable fallback to full.
   - Remove `schedule` from `quality-gates-by-domain.yml`; keep this workflow for required PR/push touched-domain routing only.
   - Remove or redirect `workflow_dispatch` from `quality-gates-by-domain.yml`; manual full sweeps belong in `domain-baseline-diagnostics.yml`.
   - Rename the aggregate job to `Domain touched aggregate` so required touched routing does not share the diagnostic aggregate context.
6. Add `domain-baseline-diagnostics.yml` after the detector bootstrap lands:
   - Run on `schedule` and `workflow_dispatch`.
   - Use full-domain detection by design.
   - Name jobs `baseline-tests-<domain>` and `Domain baseline aggregate`.
   - Upload artifacts and summarize failures; do not use the required `tests-<domain>` or `Domain touched aggregate` context names.
7. Fix selected `tests-orcaflex` failures under TDD:
   - Reproduce each selected failure from the current CI/local baseline.
   - Add or adjust focused tests before implementation where behavior is under-specified.
   - Fix the catenary overflow/grounded-length/DAF failures in `tests/orcaflex/`.
   - Fix stale `BaseFileGenerator` expectations in `tests/test_orcaflex_agent.py` or update the tests to the current generator contract if the old API is intentionally retired.
8. Fix selected `tests-orcaflex-solver` failures under TDD:
   - Reproduce the current selected solver shard failures from CI/local baseline.
   - Fix stale modular-generator registry/order expectations, schema validation expectations, missing fixture paths, report snapshots, CLI help behavior without an OrcaFlex license, and converter batch/performance setup as scoped by current failures.
   - If any solver failure requires a licensed OrcaFlex runtime, convert it to an explicit license-gated skip with a tracked follow-up rather than a silent pass.
9. Re-run the detector against a PR-shaped diff equivalent to [#702](https://github.com/vamseeachanta/digitalmodel/pull/702):
   - Expected selected domains: `orcaflex` and `orcaflex-solver` unless additional touched files require more.
   - The corrected selector must not include unrelated shards such as `contracts`, `subsea`, `structural`, or `misc` for the OrcaFlex/API RP 2SM diff.
10. Run branch-relevant checks:
   - `python scripts/ci/detect_touched_domains.py --mode touched --base <base> --head <head> --domains-file tests/DOMAINS.md --output-format list`
   - `uv run --no-sources --with-editable . python -m pytest tests/scripts/test_detect_touched_domains.py -q -p no:randomly -p no:sugar`
   - `uv run --no-sources --with-editable . python -m pytest tests/orcaflex/ tests/test_orcaflex_agent.py -q -p no:randomly -p no:sugar --tb=short`
   - `uv run --no-sources --with-editable . python -m pytest tests/solvers/orcaflex/ -q -p no:randomly -p no:sugar --tb=short`
   - `uv run --no-sources --with-editable . python -m pytest tests/workflows/automation/test_quality_gates.py -q -p no:randomly -p no:sugar` if workflow/quality-gate config parsing changes.
11. File or update child follow-up issues for stale domain failures that still reproduce in full sweeps after routing is corrected.

## TDD Test List

- `tests/scripts/test_detect_touched_domains.py::test_orcaflex_source_change_selects_orcaflex_and_solver_domains`
- `tests/scripts/test_detect_touched_domains.py::test_orcaflex_solver_source_change_selects_orcaflex_solver_domain`
- `tests/scripts/test_detect_touched_domains.py::test_orcaflex_agent_source_change_selects_workflows_and_orcaflex_domains`
- `tests/scripts/test_detect_touched_domains.py::test_pyproject_package_data_only_change_selects_matching_domain`
- `tests/scripts/test_detect_touched_domains.py::test_pyproject_package_data_unmapped_prefix_selects_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_pyproject_dependency_change_still_selects_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_mixed_pyproject_change_fails_closed_to_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_invalid_pyproject_toml_falls_back_to_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_pr_diff_uses_merge_base_not_raw_base_head`
- `tests/scripts/test_detect_touched_domains.py::test_unreachable_ref_falls_back_to_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_push_zero_before_sha_falls_back_to_full_matrix`
- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_with_pretension`
- `tests/orcaflex/test_mooring_design.py::TestCatenary::test_catenary_grounded_length`
- `tests/orcaflex/test_installation_analysis.py::TestDAF::test_daf_increases_with_heave`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_general_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_var_data_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_vessel_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_lines_file`
- `tests/test_orcaflex_agent.py::TestBaseFileGenerator::test_generate_all_files`
- `tests/solvers/orcaflex/modular_generator/test_builder_registry.py::TestBuilderRegistry::test_all_builders_registered`
- `tests/solvers/orcaflex/modular_generator/test_builder_registry.py::TestBuilderRegistry::test_get_include_order`
- `tests/solvers/orcaflex/modular_generator/test_schema_compat.py::TestSchemaValidation::test_invalid_water_depth_too_deep`
- `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_mooring_tension_iteration`
- `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_single_line_iteration`
- `tests/solvers/orcaflex/mooring-tension-iteration/mooring_tension_iteration_test.py::test_multi_line_coupling`
- `tests/solvers/orcaflex/reporting/test_fixture_snapshot.py::test_minimal_fixture_report_matches_snapshot`
- `tests/solvers/orcaflex/test_orcaflex_cli.py::TestCLIDefaults::test_universal_defaults_shown_in_help`
- Existing detector tests will continue to pass.

## Acceptance Criteria

- [ ] A detector-only bootstrap PR can land without selecting the current full matrix.
- [ ] `.github/workflows/quality-gates-by-domain.yml` remains a full-matrix trigger unless the user explicitly approves a maintainer override for a CI-infrastructure change.
- [ ] A PR touching `src/digitalmodel/orcaflex/`, `tests/orcaflex/`, and an OrcaFlex package-data declaration in `pyproject.toml` selects `orcaflex` and `orcaflex-solver` only unless additional touched files require more domains.
- [ ] A PR touching `src/digitalmodel/workflows/agents/orcaflex/` selects both `workflows` and `orcaflex`.
- [ ] Dependency, pytest, build-system, or unknown global config changes still select the full matrix.
- [ ] PR detection uses merge-base/head semantics so branches behind `main` do not inherit unrelated base-side changes.
- [ ] Pushes to `main` use touched-domain detection where GitHub provides reachable before/after SHAs and fall back to full matrix for zero or unreachable `before` SHAs.
- [ ] Scheduled/manual full baseline sweeps run in `Domain Baseline Diagnostics` with `baseline-tests-*` contexts and do not share `tests-*` or `Domain touched aggregate` touched-domain contexts.
- [ ] `tests-orcaflex` and `tests-orcaflex-solver` pass locally and in CI when selected by an OrcaFlex-scoped PR.
- [ ] Stale full-suite failures outside the corrected selected domains are linked to child follow-up issues.
- [ ] `Quality Gates by Domain` no longer blocks mapped domain-scoped PRs with failures from untouched domains.

## Risks

- Over-specific source routing could hide necessary tests for shared modules. Mitigation: unknown or shared prefixes will continue to fail closed to the full matrix.
- A permissive `pyproject.toml` parser could under-test dependency changes. Mitigation: only package-data-only diffs will route narrowly; any other hunk will select the full matrix.
- The implementation PR can self-block if workflow-file routing is changed before the detector bootstrap lands. Mitigation: land detector routing first, then land workflow changes.
- Workflow-file changes are CI infrastructure changes; narrowing them to `workflows` only would reduce safety. Mitigation: keep workflow files as full-matrix triggers and require either a green full matrix or explicit maintainer override for workflow PRs.
- OrcaFlex and OrcaFlex-solver fixes can expand beyond CI routing scope. Mitigation: limit selected-shard repairs to failures that reproduce on current `main` and are required by corrected OrcaFlex-family routing; file child issues for broader domain failures.
- `src/digitalmodel/orcaflex/` may be less coupled to `src/digitalmodel/solvers/orcaflex/` than conservative routing assumes. Mitigation: start conservative, then narrow only after an import-boundary test proves solver isolation.
- OrcaFlex-agent code lives under `src/digitalmodel/workflows/agents/orcaflex/`, while its legacy tests live in `tests/test_orcaflex_agent.py` under the `orcaflex` domain. Mitigation: route that source prefix to both `workflows` and `orcaflex`.
- Scheduled full sweeps may keep showing red while stale domain failures are worked down. Mitigation: separate baseline diagnostics from required branch routing and keep child issues linked.
- Domain names in `tests/DOMAINS.md`, `.claude/quality-gates.yaml`, and source-prefix maps can drift. Mitigation: add tests that assert mapped domains exist in the parsed domain list and gate config.

## Adversarial Review Plan

- Plan review will ask reviewers to assume the detector change can under-test PRs until proven otherwise.
- Reviewers will verify:
  - source-prefix mappings do not bypass shared-module changes;
  - `pyproject.toml` package-data parsing fails closed on ambiguous diffs;
  - PR merge-base semantics and push-event fallback semantics are testable;
  - selected `tests-orcaflex` and `tests-orcaflex-solver` failures are actually in scope;
  - OrcaFlex-agent source routing covers both workflow and OrcaFlex tests;
  - stale full-suite failures remain visible and traced to child issues.

## Adversarial Review Summary

- Round 1 Claude: `MAJOR`. Blocking findings required a concrete required-vs-diagnostic split, an implementation landing sequence for workflow changes, selected-domain OrcaFlex fixes, corrected [#702](https://github.com/vamseeachanta/digitalmodel/pull/702) expected selection, push zero-SHA fallback, and a specified `pyproject.toml` parser.
- Round 1 Codex: `MAJOR`. Blocking findings required PR merge-base diff semantics, selected-domain OrcaFlex fixes, a concrete full-baseline check split, and narrower source-prefix scope.
- Round 1 Gemini: `UNAVAILABLE`; CLI returned an empty response / malformed tool call.
- Round 2 Codex: `MAJOR`. Blocking findings required exact diagnostic/required context names, the real OrcaFlex-agent source file path, and explicit invalid-TOML/unreachable-ref fallback tests.
- Round 2 Gemini: `MAJOR`. Blocking findings required routing `src/digitalmodel/workflows/agents/orcaflex/` to `orcaflex`, and called out OrcaFlex/orcaflex-solver coupling risk.
- Round 2 Claude: `UNAVAILABLE`; wrapper produced no usable review artifact.
- Round 3 Gemini: `MAJOR`. Blocking finding rejected narrow routing for `.github/workflows/quality-gates-by-domain.yml` because workflow-orchestration changes should remain full-matrix infrastructure changes.
- Round 3 Claude: `MAJOR`. Blocking finding showed there are currently no GitHub required status checks, so the prior required-context plan overstated enforcement and risked requiring dynamic per-domain jobs that may not report on every PR.
- Round 3 Codex: `UNAVAILABLE`; CLI timed out before a usable verdict.
- This draft will remain non-approval-ready until a revised review wave returns no `MAJOR` findings.

## Open Questions

- Should `tests/DOMAINS.md` become the canonical source for source-prefix routing, or should `scripts/ci/detect_touched_domains.py` keep an explicit conservative source map?
