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
- The plan will focus on the routing contract first. It will not attempt to repair every stale failing domain test in one branch; those failures will be surfaced as child follow-ups unless a failing shard is required by the corrected routing contract.

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

- `.github/workflows/quality-gates-by-domain.yml` will own event semantics:
  - PRs will use base/head touched-mode detection.
  - Pushes to `main`/`develop` will use before/after touched-mode detection.
  - Full baseline health sweeps will be scheduled or manually invoked without blocking unrelated PR work.
- `scripts/ci/detect_touched_domains.py` will own path and diff classification.
- `tests/scripts/test_detect_touched_domains.py` will own regression coverage for detector routing.
- `.claude/quality-gates.yaml` will remain the source of runnable per-domain commands.
- `tests/DOMAINS.md` will remain the source of domain roots.

## Scope

### In Scope

- Add source-prefix routing for first-class package domains that currently fall through to the full matrix, starting with the CI-blocking OrcaFlex paths and covering other top-level package/domain mappings where the domain command already exists.
- Add a diff-aware `pyproject.toml` classifier so package-data-only declarations can route to the affected domain instead of forcing the full matrix.
- Keep dependency, build-system, pytest, and tool-configuration changes conservative: those will continue to select the full matrix.
- Update `quality-gates-by-domain.yml` so push events can use touched-mode detection via `github.event.before` and `github.sha`, rather than always using full mode.
- Keep full-domain baseline sweeps visible as diagnostics, but separate them from required PR/domain routing if stale baseline shards remain red.
- Post or update follow-up GitHub issues for stale domain test failures that remain outside the corrected routing contract.

### Out of Scope

- Fixing every stale failure in `tests-contracts`, `tests-misc`, `tests-subsea`, `tests-structural`, and every other broad shard in this issue.
- Changing individual domain commands in `.claude/quality-gates.yaml` unless needed to make the corrected routing contract executable.
- Weakening branch-relevant checks for touched domains.
- Marking full baseline failures as solved without child issue evidence.

## Deliverables

- Updated detector logic in `scripts/ci/detect_touched_domains.py`.
- Unit tests in `tests/scripts/test_detect_touched_domains.py` for source-prefix and `pyproject.toml` package-data routing.
- Workflow update in `.github/workflows/quality-gates-by-domain.yml` for push touched-mode routing and non-blocking full baseline handling.
- A concise issue comment on [#700](https://github.com/vamseeachanta/digitalmodel/issues/700) with the final routing contract, verification commands, and any child issues filed for stale full-suite failures.

## Approach

1. Add red tests for source-prefix routing:
   - `src/digitalmodel/orcaflex/...` will select `orcaflex`, not the full matrix.
   - `src/digitalmodel/solvers/orcaflex/...` will select `orcaflex-solver`, not the full matrix.
   - Existing mapped prefixes such as `src/digitalmodel/citations/` will continue to select their current domain.
   - Unknown source prefixes will continue to select the full matrix until they have explicit domain routing.
2. Add red tests for `pyproject.toml` routing:
   - A package-data-only diff adding `orcaflex/data/*.yml` will select `orcaflex`.
   - A dependency or build-system diff in `pyproject.toml` will select the full matrix.
   - A mixed `pyproject.toml` diff with both package data and dependency changes will select the full matrix.
3. Refactor detector internals without changing the CLI contract:
   - Preserve `--mode touched`, `--mode full`, and `--output-format`.
   - Pass base/head into the full-matrix decision when diff-aware path classification is needed.
   - Keep failures fail-closed: parse errors or unclassified config diffs will return the full matrix.
4. Update `quality-gates-by-domain.yml` event logic:
   - Pull requests will keep current base/head detection.
   - Push events will call touched mode with `github.event.before` and `github.sha` when both SHAs are available.
   - Schedule/workflow-dispatch full sweeps will be explicitly documented as baseline diagnostics.
5. Re-run the detector against a PR-shaped diff equivalent to [#702](https://github.com/vamseeachanta/digitalmodel/pull/702):
   - Expected selected domains: `citations` and `orcaflex` unless additional touched files require more.
   - The corrected selector must not include unrelated shards such as `contracts`, `subsea`, `structural`, or `misc` for the OrcaFlex/API RP 2SM diff.
6. Run branch-relevant checks:
   - `python scripts/ci/detect_touched_domains.py --mode touched --base <base> --head <head> --domains-file tests/DOMAINS.md --output-format list`
   - `uv run --no-sources --with-editable . python -m pytest tests/scripts/test_detect_touched_domains.py -q -p no:randomly -p no:sugar`
   - `uv run --no-sources --with-editable . python -m pytest tests/workflows/automation/test_quality_gates.py -q -p no:randomly -p no:sugar` if workflow/quality-gate config parsing changes.
7. File or update child follow-up issues for stale domain failures that still reproduce in full sweeps after routing is corrected.

## TDD Test List

- `tests/scripts/test_detect_touched_domains.py::test_orcaflex_source_change_selects_orcaflex_domain`
- `tests/scripts/test_detect_touched_domains.py::test_orcaflex_solver_source_change_selects_orcaflex_solver_domain`
- `tests/scripts/test_detect_touched_domains.py::test_pyproject_package_data_only_change_selects_matching_domain`
- `tests/scripts/test_detect_touched_domains.py::test_pyproject_dependency_change_still_selects_full_matrix`
- `tests/scripts/test_detect_touched_domains.py::test_mixed_pyproject_change_fails_closed_to_full_matrix`
- Existing detector tests will continue to pass.

## Acceptance Criteria

- [ ] A PR touching `src/digitalmodel/orcaflex/`, `tests/orcaflex/`, and an OrcaFlex package-data declaration in `pyproject.toml` selects OrcaFlex/citation-relevant shards only, not the full domain matrix.
- [ ] Dependency, pytest, build-system, or unknown global config changes still select the full matrix.
- [ ] Pushes to `main` use touched-domain detection where GitHub provides before/after SHAs.
- [ ] Full baseline sweeps remain available as diagnostics without blocking unrelated domain-scoped PRs.
- [ ] Stale full-suite failures are either fixed if they are in a selected domain or linked to child follow-up issues if they are outside the corrected routing scope.
- [ ] `Quality Gates by Domain` no longer blocks unrelated feature PRs with failures from untouched domains.

## Risks

- Over-specific source routing could hide necessary tests for shared modules. Mitigation: unknown or shared prefixes will continue to fail closed to the full matrix.
- A permissive `pyproject.toml` parser could under-test dependency changes. Mitigation: only package-data-only diffs will route narrowly; any other hunk will select the full matrix.
- Scheduled full sweeps may keep showing red while stale domain failures are worked down. Mitigation: separate baseline diagnostics from required branch routing and keep child issues linked.
- Domain names in `tests/DOMAINS.md`, `.claude/quality-gates.yaml`, and source-prefix maps can drift. Mitigation: add tests that assert mapped domains exist in the parsed domain list and gate config.

## Adversarial Review Plan

- Plan review will ask reviewers to assume the detector change can under-test PRs until proven otherwise.
- Reviewers will verify:
  - source-prefix mappings do not bypass shared-module changes;
  - `pyproject.toml` package-data parsing fails closed on ambiguous diffs;
  - push-event workflow semantics do not break PR detection;
  - stale full-suite failures remain visible and traced to child issues.

## Open Questions

- Should scheduled full-domain sweeps remain in the same workflow name or move to a separate non-required workflow so branch protection can distinguish required routing checks from baseline health?
- Should `tests/DOMAINS.md` become the canonical source for source-prefix routing, or should `scripts/ci/detect_touched_domains.py` keep an explicit conservative source map?
