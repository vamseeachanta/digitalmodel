# Plan: digitalmodel #703 — OrcaFlex touched-domain detector routing

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/703
**Parent:** https://github.com/vamseeachanta/digitalmodel/issues/700
**Status:** draft
**Tier:** T2 (CI detector behavior; single script plus focused tests)
**Client:** N/A
**Project:** N/A
**Lane:** codex

## Resource Intelligence Summary

The issue will be handled as a detector-only change. The implementation will not
change `.github/workflows/quality-gates-by-domain.yml`, will not change domain
gate commands in `.claude/quality-gates.yaml`, and will not repair failing
`tests-orcaflex` or `tests-orcaflex-solver` baselines. Those remain under
https://github.com/vamseeachanta/digitalmodel/issues/704,
https://github.com/vamseeachanta/digitalmodel/issues/705, and
https://github.com/vamseeachanta/digitalmodel/issues/706.

Evidence gathered on 2026-06-12 from branch `chore/703-detector-routing-plan` at
`41289638e85572cb9397bd3c052418888ac8d492`:

- `scripts/ci/detect_touched_domains.py` currently treats every unmapped
  `src/digitalmodel/` change as a full-matrix trigger.
- Existing source-domain mappings cover `cathodic_protection`, `citations`, and
  `workflows`; they do not cover `orcaflex` or `solvers/orcaflex`.
- `pyproject.toml` is currently listed as an unconditional full-matrix path.
- `tests/scripts/test_detect_touched_domains.py` already uses temporary git
  repositories and direct detector invocations, so new cases can extend that
  style.
- `tests/DOMAINS.md` defines separate `orcaflex`, `orcaflex-solver`, and
  `workflows` domains. `.claude/quality-gates.yaml` maps those domains to
  separate test commands, so routing can target them independently.
- This checkout does not contain `docs/plans/README.md` or
  `docs/plans/_template-issue-plan.md`; the plan will follow the existing
  `digitalmodel` standalone plan-file pattern under `docs/plans/`.

### Reproduction proofs

Command:

```bash
python3 - <<'PY'
from scripts.ci.detect_touched_domains import is_full_matrix_trigger
for path in [
    "src/digitalmodel/orcaflex/mooring_design.py",
    "src/digitalmodel/solvers/orcaflex/core.py",
    "src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py",
    "pyproject.toml",
    ".github/workflows/quality-gates-by-domain.yml",
]:
    print(f"{path}: {is_full_matrix_trigger(path)}")
PY
```

Output:

```text
src/digitalmodel/orcaflex/mooring_design.py: True
src/digitalmodel/solvers/orcaflex/core.py: True
src/digitalmodel/workflows/agents/orcaflex/generators/base_files.py: False
pyproject.toml: True
.github/workflows/quality-gates-by-domain.yml: True
```

Scratch-repo command executed at `2026-06-12T06:43:50+00:00` with the current
detector:

```text
orcaflex_source=orcaflex,orcaflex-solver,workflows,structural,citations
orcaflex_solver_source=orcaflex,orcaflex-solver,workflows,structural,citations
workflow_orcaflex_agent=workflows
package_data_only_pyproject=orcaflex,orcaflex-solver,workflows,structural,citations
raw_base_head_diff=structural,citations
merge_base_head_diff=citations
```

The implementation will turn these into the narrower expected selections:

- `src/digitalmodel/orcaflex/...` -> `orcaflex`, `orcaflex-solver`
- `src/digitalmodel/solvers/orcaflex/...` -> `orcaflex-solver`
- `src/digitalmodel/workflows/agents/orcaflex/...` -> `workflows`, `orcaflex`
- package-data-only `pyproject.toml` OrcaFlex globs -> the same source-domain
  mapping used by concrete source paths
- PR comparisons -> merge-base/head changes only

## Artifact Map

- `scripts/ci/detect_touched_domains.py` — touched-domain detector to revise.
- `tests/scripts/test_detect_touched_domains.py` — focused detector regression
  tests to extend.
- `tests/DOMAINS.md` — source of domain names and test roots; read-only.
- `pyproject.toml` — package-data structure used as fixture input; production
  file should not be edited for this issue.
- `.github/workflows/quality-gates-by-domain.yml` — out of scope and should
  continue to trigger full matrix if changed.

## Deliverable

The implementation will make touched-domain detection route OrcaFlex-family
source and package-data changes to scoped domains while keeping unknown,
shared, invalid, or unavailable evidence fail-closed to the full domain matrix.

## Proposed Design

1. Replace the single-value `SOURCE_DOMAIN_PREFIXES` mapping with a multi-domain
   mapping, for example `SOURCE_DOMAIN_PREFIXES: tuple[tuple[str, tuple[str,
   ...]], ...]`.
2. Add a helper that returns the union of all matching source-domain prefixes for
   a normalized path. The helper will not stop after the first match.
3. Preserve existing mappings:
   - `src/digitalmodel/cathodic_protection/` -> `cathodic-protection`
   - `src/digitalmodel/citations/` -> `citations`
   - `src/digitalmodel/workflows/` -> `workflows`
4. Add OrcaFlex mappings:
   - `src/digitalmodel/orcaflex/` -> `orcaflex`, `orcaflex-solver`
   - `src/digitalmodel/solvers/orcaflex/` -> `orcaflex-solver`
   - `src/digitalmodel/workflows/agents/orcaflex/` -> `workflows`, `orcaflex`
5. Keep unmapped `src/digitalmodel/...` paths as full-matrix triggers.
6. Keep `.github/workflows/quality-gates-by-domain.yml`, `tests/DOMAINS.md`,
   `tests/conftest.py`, and `pytest.ini` as unconditional full-matrix triggers.
7. Remove `pyproject.toml` from unconditional full-matrix handling and classify
   it through a dedicated fail-closed helper.

### `pyproject.toml` classifier

The implementation will classify a `pyproject.toml` change as scoped only when
all of the following are true:

- both base and head TOML blobs can be read from git;
- both blobs parse with `tomllib`;
- the only semantic difference is under
  `[tool.setuptools.package-data].digitalmodel`;
- every changed package-data entry is a relative package glob without `..`,
  absolute-path syntax, or an empty prefix;
- every translated package path maps to at least one known source-domain prefix.

For scoped package-data-only diffs, each changed package-relative glob will be
translated into a source-style path under `src/digitalmodel/`, using the stable
prefix before the first glob metacharacter. Examples:

- `orcaflex/data/*.yml` -> `src/digitalmodel/orcaflex/data/` -> `orcaflex`,
  `orcaflex-solver`
- `solvers/orcaflex/**/*.yml` -> `src/digitalmodel/solvers/orcaflex/` ->
  `orcaflex-solver`
- `workflows/agents/orcaflex/templates/*.yml` ->
  `src/digitalmodel/workflows/agents/orcaflex/templates/` -> `workflows`,
  `orcaflex`

The helper will fail closed to full matrix for dependency changes, build-system
changes, tool config changes, mixed package-data and non-package-data changes,
invalid TOML, unreadable blobs, unreachable refs, and unmapped package-data
prefixes.

### Git diff semantics

`git_changed_files(base, head)` will use PR-style merge-base semantics:

```text
merge_base = git merge-base base head
changed = git diff --name-only merge_base head
```

If `merge-base` or `diff` fails, the detector will return the full matrix with
exit code `0`. That preserves fail-closed CI behavior for deleted refs,
unreachable refs, shallow checkout gaps, and other git-state problems.

## TDD Test List

The implementation will add failing tests first in
`tests/scripts/test_detect_touched_domains.py`, then make them pass:

- `test_orcaflex_source_change_outputs_orcaflex_and_solver_domains`
- `test_orcaflex_solver_source_change_outputs_solver_domain`
- `test_workflow_orcaflex_agent_source_change_outputs_workflows_and_orcaflex`
- `test_package_data_only_pyproject_change_routes_through_source_mapping`
- `test_unmapped_package_data_pyproject_change_escalates_to_full_matrix`
- `test_dependency_pyproject_change_escalates_to_full_matrix`
- `test_invalid_pyproject_toml_escalates_to_full_matrix`
- `test_unreachable_ref_escalates_to_full_matrix`
- `test_touched_mode_uses_merge_base_head_not_raw_base_head`

Existing detector tests will remain in place and should continue passing.

## Implementation Steps

1. Add the new tests and confirm the new [#703](https://github.com/vamseeachanta/digitalmodel/issues/703)
   tests fail for the current detector behavior.
2. Refactor source-domain prefix matching to return a union of all matching
   domain names.
3. Add OrcaFlex-specific source-domain mappings.
4. Change git diff collection to merge-base/head semantics with full-matrix
   fallback on git failures.
5. Add the `pyproject.toml` package-data classifier and wire it into touched
   selection before generic full-matrix path handling.
6. Run focused tests and detector smoke commands.
7. Run the repo legal sanity scan before final code-stage review.

## Acceptance Criteria

- `src/digitalmodel/orcaflex/` changes select `orcaflex` and
  `orcaflex-solver`, not the full matrix.
- `src/digitalmodel/solvers/orcaflex/` changes select `orcaflex-solver`, not
  the full matrix.
- `src/digitalmodel/workflows/agents/orcaflex/` changes select both
  `workflows` and `orcaflex`.
- Package-data-only `pyproject.toml` diffs under
  `[tool.setuptools.package-data].digitalmodel` route through package-relative
  glob translation to `src/digitalmodel/...` and then the source-domain map.
- Dependency/build/tool config changes in `pyproject.toml`, invalid TOML,
  unreachable refs, and unmapped package-data prefixes fail closed to the full
  matrix.
- PR diff detection uses merge-base/head semantics rather than raw base/head
  diff.
- Tests cover overlapping prefix union semantics, unmapped package-data
  fallback, invalid TOML fallback, and unreachable-ref fallback.
- `.github/workflows/quality-gates-by-domain.yml` changes still trigger full
  matrix; workflow orchestration remains unchanged.

## Verification Commands

After implementation and before code review, run:

```bash
python scripts/ci/detect_touched_domains.py --mode full --domains-file tests/DOMAINS.md --output-format list
uv run --no-sources --with-editable . python -m pytest tests/scripts/test_detect_touched_domains.py -q -p no:randomly -p no:sugar
python -m py_compile scripts/ci/detect_touched_domains.py
scripts/legal/legal-sanity-scan.sh
```

If `uv` mutates environment or lock artifacts, the closeout will clean or
restore unrelated residue before reporting completion.

## Risks

- Package-data glob parsing can accidentally under-select if wildcard prefixes
  are interpreted too narrowly. The implementation will use the stable
  non-glob prefix and fail closed when that prefix cannot be safely mapped.
- `pyproject.toml` comment-only or formatting-only edits are not required for
  optimization. They may remain full-matrix if the implementation cannot prove a
  scoped package-data semantic change.
- `src/digitalmodel/orcaflex/` intentionally selects both `orcaflex` and
  `orcaflex-solver`. That is conservative for solver consumers and matches the
  issue acceptance criteria.
- The broad CI baseline remains red outside this detector slice. The PR for
  this issue may still be affected by parent [#700](https://github.com/vamseeachanta/digitalmodel/issues/700)
  domain-gate failures until [#704](https://github.com/vamseeachanta/digitalmodel/issues/704),
  [#705](https://github.com/vamseeachanta/digitalmodel/issues/705), and
  [#706](https://github.com/vamseeachanta/digitalmodel/issues/706) land.

## Adversarial Review Plan

This draft will be reviewed before it is surfaced as approval-ready. Review
prompts will force an adversarial stance and will ask reviewers to look for
missed fail-closed cases, overbroad routing, under-selected domain shards,
workflow-scope creep, and test gaps.

Planned review artifacts:

- `scripts/review/results/2026-06-12-plan-703-claude.md`
- `scripts/review/results/2026-06-12-plan-703-codex.md`
- `scripts/review/results/2026-06-12-plan-703-gemini.md`

If any provider returns `MAJOR`, the plan will stay in draft/needs-revision
state and implementation will remain blocked.

## Approval Gate

Implementation must not start until the issue has user-provided
`status:plan-approved` evidence and the local approval marker exists. This plan
does not self-approve and does not authorize code edits.
