# Plan for #596: chore(repo-structure): normalize digitalmodel folder/file structure

> **Status:** plan-approved — approved by user on 2026-05-08; approval marker `.planning/plan-approved/596.md` records reviewed plan/review blob SHAs
> **Complexity:** T3
> **Date:** 2026-05-07
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/596
> **Initial review artifacts:** `scripts/review/results/2026-05-07-plan-596-claude.md` | `scripts/review/results/2026-05-07-plan-596-codex.md` | `scripts/review/results/2026-05-07-plan-596-gemini.md`
> **Re-review artifacts:** `scripts/review/results/2026-05-07-plan-596-rereview-claude.md` | `scripts/review/results/2026-05-07-plan-596-rereview-codex.md` | `scripts/review/results/2026-05-07-plan-596-rereview-gemini.md`
> **Parent anchors:** workspace-hub#1962, workspace-hub#2397

---

## Decision Summary

This is the **first repo-by-repo structure normalization** in the workspace-hub tier-1 ecosystem. The implementation must be conservative and template-quality.

The approved implementation scope is locked to **Phase 1: repo-structure contract, checker, enforcement wiring, root utility placement, and explicit exception metadata for known tracked generated evidence**.

No package-source, domain-methodology, broad docs routing, notebook-policy, or B1528 generated-evidence relocation is authorized by this plan.

### Locked structural choices

| Choice | Locked path | Rationale |
|---|---|---|
| Human-readable contract | `docs/standards/repo-structure.md` | Durable standard; avoids overloading `docs/README.md` routing entrypoint |
| Machine-readable contract | `config/repo_structure.yml` | Repo-local config at top-level `config/`; single source for checker/tests |
| Checker | `scripts/maintenance/verify_repo_structure.py` | Maintenance tooling, not product package code |
| Checker tests | `tests/repo_structure/test_repo_structure_contract.py` | Isolated TDD surface for structure rules |
| Routing tests | extend existing `tests/docs/test_digitalmodel_routing_contract.py` only if touched references require it | File exists and already validates routing contracts |
| Local invocation | `.pre-commit-config.yaml` local hook | Prevents new drift before commit |
| CI invocation | `.github/workflows/quality-gates.yml` or dedicated structure-check workflow row | Makes checker non-bypassable in PR/push validation |
| Docs CI path coverage | `.github/workflows/docs.yml` path filters include touched docs surfaces | Existing docs workflow does not trigger on `docs/standards/**` today |
| Plan index | no `docs/plans/README.md` in this issue | Absent today; separate repo-planning-contract issue if desired |

---

## Resource Intelligence Summary

### Existing repo code and structure

- `digitalmodel#596` is open with labels `enhancement`, `cat:engineering`, `priority:high`, `status:pending`.
- `AGENTS.md` defines `src/digitalmodel/` as source root and `PYTHONPATH=src uv run python -m pytest` as the base test pattern.
- `docs/README.md` defines the repo-vs-bulk-artifact-store boundary: source/tests/small curated docs/routing registries belong in repo; large/generated/binary/cache/raw/fast-growing artifacts belong outside the repo or under explicitly durable fixture/report locations.
- `docs/maps/digitalmodel-operator-map.md` and `docs/registry/module-routing.yaml` are durable routing surfaces and must remain aligned after touched path changes.
- `.gitignore` already ignores generated/transient roots, including `outputs/`. However, `outputs/b1528_sirocco/**` is already tracked, creating a pre-existing contradiction.
- `.github/workflows/*.yml` currently references CI artifacts: `coverage.xml`, `coverage.json`, `dist/`, `reports/quality_gates_results.json`, and `reports/quality-gates-pytest-full.log`.
- Current package source intentionally references `outputs/b1528_sirocco/**` for runtime output directories and traceability links in B1528 generators. Changing those source links is out of scope for this issue.

### Corrected tracked-state evidence

Verified with `git ls-files` on 2026-05-07:

```text
Tracked root Python files:
_count_tests.py
_coverage_map.py
orcaflex_maturity_analysis.py
vulture_whitelist.py

Tracked candidate generated/demo outputs:
outputs/b1528_sirocco/**  # 18 tracked report/result/provenance/manifest files

Tracked domain/runtime roots:
notebooks/gis/GIS_2013-12-16-UTM_conv.ipynb  # 1 tracked notebook
reports/      # zero tracked files; CI artifact landing only
memory/       # zero tracked files
specs/        # zero tracked files
benchmarks/   # zero tracked files
projects/     # zero tracked files
```

### Pre-existing inconsistency and Phase-1 resolution

`outputs/` is ignored by `.gitignore`, but 18 files under `outputs/b1528_sirocco/**` are tracked and linked from durable docs/source. The initial plan proposed moving them to `docs/examples/...`; second-pass re-review correctly found that move would require package-source link changes and per-file retention classification beyond this issue.

Locked Phase-1 action after approval:

```text
outputs/b1528_sirocco/**
  -> keep in place as a temporary durable exception in config/repo_structure.yml
  -> require owner/reason/category/reviewed_on/follow-up metadata
  -> do not move or rewrite B1528 source/docs links in this issue
```

Concrete follow-up issue: `digitalmodel#597` — https://github.com/vamseeachanta/digitalmodel/issues/597 — **B1528 generated evidence classification and relocation**. That follow-up must separately decide per-file retention for reports, result CSV/JSON, provenance, manifests, and runtime output link generation.

The checker/config must reject placeholder exception metadata. `follow_up_issue_or_permanent_justification` must be either a concrete GitHub issue URL/ID (for temporary exceptions) or a non-placeholder permanent justification that matches the checker’s documented permanent-exception schema.

### Reference evidence

- `docs/domains/marine-engineering/rudder-and-ship-force-calculation-review.md` links to the committed `outputs/b1528_sirocco/**` report files and embeds result path references.
- `docs/domains/marine-engineering/b1528-sirocco-yaw-moment-report.md` links to committed generated reports.
- B1528 package source currently emits or references `outputs/b1528_sirocco/**` runtime/link paths; this is explicitly left unchanged.
- No text references were found for `_count_tests.py`, `_coverage_map.py`, `orcaflex_maturity_analysis.py`, or `vulture_whitelist.py` in the quick repo scan; implementation must repeat the scoped reference scan before moving/removing any root script.

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-05-07-issue-596-repo-structure-normalization.md` |
| Human structure contract | `docs/standards/repo-structure.md` |
| Machine-readable rules | `config/repo_structure.yml` |
| Checker | `scripts/maintenance/verify_repo_structure.py` |
| Checker tests | `tests/repo_structure/test_repo_structure_contract.py` |
| Routing tests to extend if needed | `tests/docs/test_digitalmodel_routing_contract.py` |
| Local hook | `.pre-commit-config.yaml` |
| CI checker wiring | `.github/workflows/quality-gates.yml` or dedicated `.github/workflows/repo-structure.yml` |
| Docs CI path filter update | `.github/workflows/docs.yml` |
| Temporary durable exception | `outputs/b1528_sirocco/**` in `config/repo_structure.yml` metadata only; no file move |
| Root script destination | `scripts/maintenance/_count_tests.py`, `scripts/maintenance/_coverage_map.py`, `scripts/maintenance/orcaflex_maturity_analysis.py` |
| Root allowlist exception | `vulture_whitelist.py` |
| Plan review artifacts | `scripts/review/results/2026-05-07-plan-596-*review*.md` |
| Approval marker after user approval | `.planning/plan-approved/596.md` |
| Workspace-hub export note | this plan section “Workspace-hub upstream artifacts” |

---

## Deliverable

A tested `digitalmodel` repo-structure contract and verifier with local+CI enforcement, plus explicit metadata for the known tracked `outputs/b1528_sirocco/**` exception and low-risk root utility placement. This issue does **not** relocate B1528 generated evidence; it makes the inconsistency visible, testable, and impossible to expand silently.

---

## Classification Rules

### Durable repo content

- source: `src/digitalmodel/**`
- tests: `tests/**`
- docs and standards: `docs/**`
- examples and curated evidence: `examples/**`, `docs/examples/**`
- package/config/CI/control files explicitly allowlisted in `config/repo_structure.yml`
- one tracked notebook: `notebooks/gis/GIS_2013-12-16-UTM_conv.ipynb` retained until separate notebook policy issue
- temporary exception: tracked `outputs/b1528_sirocco/**` only while it has complete durable-exception metadata and a follow-up issue/permanent justification

### Generated or runtime content

Generated/runtime paths must be ignored and untracked unless they appear in `durable_exceptions` with owner, reason, category, reviewed-on, and follow-up metadata:

```text
outputs/**
build/**
dist/**
site/**
cache/**
logs/**
results/**
benchmark_output/**
test_output_ss/**
coverage.xml
coverage.json
htmlcov/**
.pytest_cache/**
.mypy_cache/**
.ruff_cache/**
.hypothesis/**
```

### CI artifact landing zones

- `reports/quality_gates_results.json` and `reports/quality-gates-pytest-full.log` are generated CI artifacts, not durable domain assets.
- `coverage.xml`, `coverage.json`, and `dist/**` are generated CI artifacts.
- They remain untracked and ignored; CI upload paths are allowed runtime outputs, not committed files.

---

## Draft `config/repo_structure.yml`

The implementation must start from this schema; material changes require plan update/re-review.

```yaml
version: 1
root_allowlist:
  files:
    - AGENTS.md
    - CHANGELOG.md
    - CLAUDE.md
    - .coveragerc
    - .editorconfig
    - .env.template
    - .gitattributes
    - .gitignore
    - .gitmodules
    - LICENSE
    - .license-header.txt
    - Makefile
    - mkdocs.yml
    - .mutmut_config
    - .pre-commit-config.yaml
    - pyproject.toml
    - pytest.ini
    - .radon.cfg
    - README.md
    - ROADMAP.md
    - uv.lock
    - uv.toml
    - vulture_whitelist.py
  directories:
    - .claude
    - .codex
    - .gemini
    - .github
    - .planning
    - assets
    - config
    - data
    - docs
    - examples
    - notebooks
    - scripts
    - src
    - tests
allowed_tracked_notebooks:
  - notebooks/gis/GIS_2013-12-16-UTM_conv.ipynb
generated_patterns:
  - outputs/**
  - build/**
  - dist/**
  - site/**
  - cache/**
  - logs/**
  - results/**
  - benchmark_output/**
  - test_output_ss/**
  - coverage.xml
  - coverage.json
  - htmlcov/**
  - .pytest_cache/**
  - .mypy_cache/**
  - .ruff_cache/**
  - .hypothesis/**
ci_artifact_patterns:
  - reports/quality_gates_results.json
  - reports/quality-gates-pytest-full.log
  - coverage.xml
  - coverage.json
  - dist/**
durable_exceptions:
  - pattern: outputs/b1528_sirocco/**
    owner: naval-architecture
    reason: "Pre-existing tracked generated evidence referenced by B1528 documentation and source traceability links; relocation requires separate per-file retention and source-link migration plan."
    category: temporary_durable_generated_evidence
    follow_up_issue_or_permanent_justification: "https://github.com/vamseeachanta/digitalmodel/issues/597"
    reviewed_on: "2026-05-07"
required_metadata_for_exceptions:
  - owner
  - reason
  - category
  - follow_up_issue_or_permanent_justification
  - reviewed_on
forbidden_patterns:
  - src/**/tests/**
```

---

## Path-Level Migration Matrix

| Row | Path / pattern | Current state | Approved first-pass action | Required reference proof | Verification |
|---:|---|---|---|---|---|
| 1 | `docs/standards/repo-structure.md` | absent | create contract | n/a | docs/routing/link checks |
| 2 | `config/repo_structure.yml` | absent | create from draft schema | n/a | YAML-load test |
| 3 | `scripts/maintenance/verify_repo_structure.py` | absent | create checker | n/a | checker CLI tests |
| 4 | `tests/repo_structure/test_repo_structure_contract.py` | absent | create tests first | n/a | RED→GREEN evidence |
| 5 | `tests/docs/test_digitalmodel_routing_contract.py` | tracked existing | extend only if touched docs/routing references require it | `git ls-files` proof | docs/routing tests pass |
| 6 | `.pre-commit-config.yaml` | tracked existing | add local repo-structure checker hook | n/a | `uv run pre-commit run verify-repo-structure --all-files` |
| 7 | `.github/workflows/quality-gates.yml` or new `repo-structure.yml` | tracked workflow surface | add mandatory CI execution for checker or pre-commit hook | workflow grep for `verify_repo_structure.py` or hook id | CI/workflow syntax plus checker command passes locally |
| 8 | `.github/workflows/docs.yml` | tracked existing; limited path filters and `mkdocs.yml` only covers `docs/api` | expand path filters to include touched docs surfaces and bind non-API docs validation to CI (`uv run pre-commit run --all-files` or at minimum markdown-link-check + `tests/docs/test_digitalmodel_routing_contract.py`) | workflow diff + workflow command grep | docs workflow triggers on touched docs paths and actually validates `docs/standards/**` / `docs/domains/**` surfaces |
| 9 | `outputs/b1528_sirocco/**` | tracked but under ignored root; linked from docs/source | keep in place as explicit temporary durable exception; do not move in this issue | `git ls-files outputs/b1528_sirocco/**` and config metadata | checker accepts only this exception; follow-up issue/link required before close |
| 10 | `_count_tests.py` | tracked root utility | `git mv` to `scripts/maintenance/_count_tests.py` if direct invocation works; otherwise remove only with explicit deprecation note | `rg -F '_count_tests' --hidden -g '!.git'` | direct invocation/help or deletion proof |
| 11 | `_coverage_map.py` | tracked root utility | `git mv` to `scripts/maintenance/_coverage_map.py` if direct invocation works; otherwise remove only with explicit deprecation note | `rg -F '_coverage_map' --hidden -g '!.git'` | direct invocation/help or deletion proof |
| 12 | `orcaflex_maturity_analysis.py` | tracked root utility | `git mv` to `scripts/maintenance/orcaflex_maturity_analysis.py` if still runnable; otherwise remove only with explicit deprecation note | `rg -F 'orcaflex_maturity_analysis' --hidden -g '!.git'` | direct invocation/help or deletion proof |
| 13 | `vulture_whitelist.py` | tracked root tool config | keep at root as explicit allowlist entry | `rg -F 'vulture_whitelist' --hidden -g '!.git'` | checker accepts allowlist |
| 14 | `reports/**` | zero tracked files; CI artifact landing | no tracked move; classify as CI artifact landing only | `git ls-files reports/` empty | checker allows untracked CI outputs but rejects tracked generated reports unless allowlisted |
| 15 | `coverage.xml`, `coverage.json`, `dist/**` | zero tracked files; CI artifacts | no committed move; keep generated/ignored and CI-upload-compatible | `git ls-files coverage.xml coverage.json dist/` empty | CI artifact contract test |
| 16 | `memory/`, `specs/`, `benchmarks/`, `projects/` | zero tracked files | no-op; leave local-only/untracked state outside commit | `git ls-files <dir>/` empty | checker reports no tracked files |
| 17 | `notebooks/gis/GIS_2013-12-16-UTM_conv.ipynb` | one tracked notebook | keep as explicit exception; open follow-up only if notebook policy desired | `git ls-files notebooks/` | checker accepts explicit notebook allowlist |
| 18 | `src/digitalmodel/**` | package source | no movement and no B1528 traceability-link edits in this issue | n/a | import smoke tests |
| 19 | `tests/**` | tests | no movement except new/extended tests above | n/a | pytest smoke tests |

---

## TDD Test List

Write tests before checker implementation.

| Test name | Purpose |
|---|---|
| `test_repo_structure_config_loads` | `config/repo_structure.yml` parses cleanly and contains required sections |
| `test_root_allowlist_accepts_current_control_files` | current root control/config files are explicit, including `vulture_whitelist.py` |
| `test_root_python_utilities_are_not_allowed_after_migration` | `_count_tests.py`, `_coverage_map.py`, `orcaflex_maturity_analysis.py` at root are violations |
| `test_generated_artifacts_are_not_tracked_unless_exception_has_metadata` | generated tracked paths fail unless exception metadata is complete |
| `test_outputs_b1528_exception_has_complete_metadata` | `outputs/b1528_sirocco/**` is accepted only because the exception is explicit, metadata-complete, and contains concrete follow-up issue URL `https://github.com/vamseeachanta/digitalmodel/issues/597` |
| `test_durable_exception_follow_up_rejects_placeholder_text` | temporary durable exceptions fail if follow-up metadata is placeholder prose rather than issue URL/ID or documented permanent-justification schema |
| `test_no_additional_outputs_paths_are_tracked` | any tracked `outputs/**` outside the B1528 exception fails |
| `test_src_tests_nested_paths_rejected` | rejects `src/**/tests/**` |
| `test_ci_artifacts_are_runtime_only` | `reports/quality_gates_results.json`, `coverage.xml`, `coverage.json`, `dist/**` are allowed runtime outputs but not tracked source |
| `test_ci_workflows_run_repo_structure_checker` | a named GitHub Actions workflow invokes the checker or hook |
| `test_docs_workflow_triggers_on_touched_docs_paths` | docs workflow path filters include touched docs surfaces |
| `test_docs_workflow_runs_non_api_docs_validation` | a named CI workflow runs full pre-commit or markdown-link/routing validation for non-API docs surfaces such as `docs/standards/**`, not just `mkdocs build --strict` |
| `test_ci_workflows_do_not_reference_missing_paths` | workflow paths are generated outputs or existing tracked paths; no deleted/moved path references |
| `test_module_routing_yaml_parses` | `docs/registry/module-routing.yaml` loads cleanly |
| `test_module_routing_paths_exist` | routing source/test/docs paths still exist or are explicitly optional |
| `test_docs_readme_curated_surfaces_exist` | docs entrypoint does not reference missing routing files |
| `test_checker_cli_returns_nonzero_for_bad_fixture_with_path_and_rule` | failures include path and rule name |
| `test_checker_cli_returns_zero_for_clean_fixture` | clean fixture passes |

---

## Exact Verification Commands

All commands are implementation gates after user approval.

```bash
# TDD / checker surface
PYTHONPATH=src uv run python -m pytest tests/repo_structure -v

# Existing docs/routing contract
PYTHONPATH=src uv run python -m pytest tests/docs/test_digitalmodel_routing_contract.py -v

# Structure checker on live repo
PYTHONPATH=src uv run python scripts/maintenance/verify_repo_structure.py --config config/repo_structure.yml --repo .

# No unauthorized tracked generated roots after approved Phase-1 work.
# outputs/b1528_sirocco/** is the only allowed tracked generated exception in this issue.
unauthorized=$(git ls-files 'outputs/**' 'build/**' 'dist/**' 'site/**' 'cache/**' 'logs/**' 'results/**' 'benchmark_output/**' 'test_output_ss/**' 'coverage.xml' 'coverage.json' | grep -v '^outputs/b1528_sirocco/' || true)
if [ -n "$unauthorized" ]; then
  printf '%s\n' "$unauthorized" >&2
  echo 'unauthorized tracked generated artifact violation' >&2
  exit 1
fi

# B1528 exception remains present and metadata-backed until follow-up issue resolves it.
git ls-files 'outputs/b1528_sirocco/**' | grep -q .
rg -F 'outputs/b1528_sirocco/**' config/repo_structure.yml
rg -F 'https://github.com/vamseeachanta/digitalmodel/issues/597' config/repo_structure.yml
if rg -F 'Create/link follow-up before closing' config/repo_structure.yml; then
  echo 'placeholder B1528 follow-up metadata violation' >&2
  exit 1
fi

# Root utilities are no longer tracked at repo root after migration rows.
if git ls-files '_count_tests.py' '_coverage_map.py' 'orcaflex_maturity_analysis.py' | grep -q .; then
  echo 'root utility migration violation' >&2
  exit 1
fi

# Import/package smoke
PYTHONPATH=src uv run python -c "import digitalmodel, digitalmodel.naval_architecture; print('digitalmodel import smoke OK')"

# Targeted naval architecture smoke to prove B1528 runtime/source paths were not broken.
PYTHONPATH=src uv run python -m pytest tests/naval_architecture/test_b1528_sirocco_yaw_moment.py tests/naval_architecture/test_b1528_sirocco_time_trace.py tests/naval_architecture/test_b1528_sirocco_moored_current.py -q -p no:randomly -p no:cov -p no:benchmark

# Docs build is a hard gate for MkDocs-covered paths; non-API docs validation is bound through CI/pre-commit below because mkdocs.yml currently uses docs_dir: docs/api.
uv run --group docs mkdocs build --strict

# Full pre-commit plus focused new hook; full pre-commit is also required in a named CI workflow (or equivalent markdown-link-check + routing tests) for non-API docs surfaces.
uv run pre-commit run verify-repo-structure --all-files
uv run pre-commit run --all-files

# CI semantics smoke: workflows must contain the new checker and non-API docs validation commands.
rg -F 'verify_repo_structure.py' .github/workflows || rg -F 'verify-repo-structure' .github/workflows
rg -F 'pre-commit run --all-files' .github/workflows || (rg -F 'markdown-link-check' .github/workflows && rg -F 'test_digitalmodel_routing_contract.py' .github/workflows)
```

If a local environment cannot run the docs build or pre-commit command because dependencies are unavailable, implementation must stop and either install the required dependencies with `uv` or use GitHub Actions evidence for the same gate. It cannot silently waive the gate.

---

## Branch, Commit, and Rollback Strategy

- Use branch `chore/596-repo-structure` for implementation unless user explicitly directs main-only execution at approval time.
- Keep commits atomic by migration-matrix row or tightly related rows:
  1. contract/config/checker tests RED
  2. checker/config/docs GREEN
  3. CI/pre-commit/docs workflow enforcement wiring
  4. root utility migration + references
  5. final verification/approval marker updates
- Do **not** move `outputs/b1528_sirocco/**` in this issue. Its only Phase-1 change is exception metadata and follow-up issue/link proof.
- Rollback for any failed row is `git revert <row-commit>` before closeout. If a CI/workflow path edit fails, revert only the workflow row and re-run checker/docs/routing tests.
- Rollback success proof requires clean `git status --short`, passing structure checker, and no unauthorized tracked generated roots beyond the explicit B1528 exception.

---

## Approval Gate

Implementation is blocked until all are true:

1. Revised plan has no unresolved CRITICAL/HIGH findings from fresh adversarial re-review.
2. Review artifacts are saved under `scripts/review/results/`.
3. The issue is labeled `status:plan-review` and the final issue comment requests user approval.
4. User approval explicitly cites the reviewed plan path and exact reviewed plan git SHA/blob SHA (a comment URL may be included as supporting evidence, but is not sufficient by itself) and either approves the locked migration matrix as written or states changes that require plan revision.
5. Only after approval, create and commit `.planning/plan-approved/596.md` with:
   - approval issue URL/comment URL
   - timestamp
   - reviewed plan path and git SHA/blob SHA
   - approved migration matrix rows
   - reviewer artifact paths
6. Before any implementation edit, run a preflight that compares the approved plan SHA/path in `.planning/plan-approved/596.md` against the current plan blob/commit SHA and aborts on mismatch.
7. Move issue to `status:plan-approved` before implementation.

A local marker alone never authorizes implementation.

---

## Implementation Scope Boundaries

### In scope after approval

- Create repo-structure contract, config, checker, tests, and pre-commit hook.
- Add mandatory CI enforcement for the checker/pre-commit gate.
- Expand docs CI path filters for touched docs surfaces and bind non-API docs validation to a named CI workflow (`uv run pre-commit run --all-files`, or markdown-link-check plus routing tests as the documented minimum).
- Add complete temporary durable-exception metadata for `outputs/b1528_sirocco/**` without moving it.
- Move or explicitly deprecate the three root utility scripts listed in the matrix.
- Keep `vulture_whitelist.py` at root as an explicit allowlist exception.
- Verify CI artifact landing paths remain generated/untracked.
- Create/link a follow-up issue for B1528 generated-evidence classification and relocation before closing #596.

### Out of scope

- No movement under `src/digitalmodel/**`.
- No edits to B1528 source traceability link generation.
- No movement of `outputs/b1528_sirocco/**` files.
- No broad movement under `tests/**` except new/extended tests.
- No movement of untracked `reports/`, `memory/`, `specs/`, `benchmarks/`, or `projects/` roots.
- No AI-agent artifact/session policy changes for `.codex`, `.gemini`, or `.claude` beyond respecting existing paths.
- No notebook policy beyond explicitly retaining the one tracked GIS notebook.
- No cross-repo implementation until this repo-specific plan is approved and closed.

---

## Adversarial Review Summary

Initial 3-provider review returned **MAJOR / not ready** across Claude, Codex, and Gemini. The first revision resolved most initial blockers by locking exact paths, embedding `config/repo_structure.yml`, enumerating root Python files, reclassifying CI artifacts, and adding rollback/approval metadata.

Second re-review again returned **MAJOR / not ready**. This revision addresses those findings by:

- removing the proposed B1528 file move from #596
- keeping `outputs/b1528_sirocco/**` as a metadata-complete temporary durable exception
- adding a required follow-up issue/link for B1528 per-file classification and relocation
- eliminating the over-broad `rg -F 'outputs/b1528_sirocco'` zero-match gate
- adding mandatory CI checker wiring
- adding docs workflow path-filter updates
- adding full pre-commit/docs-link coverage as a hard gate
- adding a plan-SHA approval preflight before implementation

| Provider / reviewer | Latest verdict | Artifact |
|---|---|---|
| Final reviewer 1 | APPROVE | captured in `scripts/review/results/2026-05-07-plan-596-final-rereview-synthesis.md` |
| Final reviewer 2 | APPROVE | captured in `scripts/review/results/2026-05-07-plan-596-final-rereview-synthesis.md` |
| Final reviewer 3 | APPROVE | captured in `scripts/review/results/2026-05-07-plan-596-final-rereview-synthesis.md` |

**Overall result:** approval-ready for `status:plan-review` / user approval request. No unresolved CRITICAL/HIGH/MAJOR findings remain. Implementation remains blocked until explicit user approval cites the reviewed plan path and exact reviewed git/blob SHA.

---

## Workspace-hub Upstream Artifacts

After `digitalmodel#596` is closed, export the reusable pattern back to workspace-hub before starting the next repo:

- `config/repo_structure.yml` schema as a tier-1 template candidate
- `scripts/maintenance/verify_repo_structure.py` checker pattern
- migration-matrix row taxonomy: root utility move, CI artifact landing, tracked generated exception metadata, tracked notebook exception, no-op untracked runtime roots
- review lessons from `scripts/review/results/2026-05-07-plan-596-*`

This export should become the seed for the next repo-specific plan rather than being applied blindly across repos.

---

## Complexity: T3

T3 because the work crosses repo policy, docs/routing contracts, generated-vs-durable evidence classification, pre-commit/CI enforcement, and ecosystem reuse.
