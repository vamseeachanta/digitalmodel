# Plan for [#1565](https://github.com/vamseeachanta/digitalmodel/issues/1565): External OpenFOAM batch work root

> **Status:** draft
> **Complexity:** T2
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1565
> **Client:** N/A
> **Project:** N/A
> **Lane:** lane:claude
> **Execution mode:** single-lane implementation after approval; read-only evidence gathering was serialized because every available agent slot was occupied
> **Review artifacts:** pending — this draft will require adversarial plan review before it can enter `status:plan-review`

---

## Resource Intelligence Summary

### Existing repository behavior

- `src/digitalmodel/workflows/openfoam_run_batch.py:104-148` resolves relative `output_dir` and `work_dir` against `_config_dir_path`, so the defaults create `results/` and `batch_runs/` beside the input.
- Lines 228-262 append the user-controlled case name without rejecting duplicates, absolute names, separators, `..`, or symlink escape.
- Lines 294-319 and 555-567 delete/rebuild cases and prune `processor*` without an explicit trusted-root containment check.
- Lines 585-609 atomically reuse completed `_result.json` checkpoints without binding them to the selecting configuration.
- Lines 612-666 place absolute `case_dir` values in returned CSV/in-memory data, which would expose an external host path.
- `tests/workflows/test_openfoam_run_batch.py:32-70,155-200` freezes the default layout, checkpoint reuse, and small-result allowlist for compatibility.
- `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml:24-33` has `output_dir: results` and `work_dir: batch_runs` but no neutral work-root setting.

### Precedent and related issues

- [#1560](https://github.com/vamseeachanta/digitalmodel/issues/1560) is closed; its contract keeps heavy cases/checkpoints under the work directory and only CSV/JSON under `results/`.
- [#1553](https://github.com/vamseeachanta/digitalmodel/issues/1553) remains open and requires deterministic script-only execution; no LLM, scheduler, or host-agent dependency will be added.
- [#1564](https://github.com/vamseeachanta/digitalmodel/issues/1564) remains open and concerns an OrcaFlex base-config gap, not work placement.
- `src/digitalmodel/workflows/orcaflex_run_batch.py:59-107` uses the same config-directory resolution; it is current-behavior precedent, not an external-root solution, and will remain unchanged.
- Squash commits `d70685e4`, `1a833df5`, and `38074785` establish the OpenFOAM workflow, engine config, and OrcaFlex batch shape.

### Resource and location intelligence

- The Drive query `openfoam batch work root` (`plan-resource-intel`, `2026-07-14T04:31:42Z`) returned no relevant design and no coverage gaps; three stale indexes make this negative evidence only. Irrelevant identifying paths are excluded.
- The online-resource registry has OpenFOAM upstream entries, but it, the standards/code registries, and the relevant CFD routing wiki define no scratch-root/return contract. No calculation standard applies.
- A live inventory found 62 first-level directories and 23 Git repositories within depth two. This is machine-local evidence only; no fleet claim or file movement will follow.
- Input/config and relative `output_dir` residency will remain unchanged; only heavy work will become overridable. The legacy data-mount alias is unrelated.

### Gaps identified

- No environment/config root or deterministic run namespace separates different batches with identical case names.
- Completed checkpoints are not bound to the effective configuration.
- Destructive cleanup lacks an explicit canonical-root containment contract.
- Case/path traversal, duplicate names, symlinks, absolute-path leakage, output residency, and legacy compatibility lack coverage.

### Embedded verification evidence

**Live issue state** (`gh issue view`, `2026-07-14T04:31:53Z`):

```text
#1553 OPEN   EPIC: Continuous batch mini-runs ...
#1560 CLOSED Land CFD execution surface on main + openfoam-run-batch saturating workflow
#1564 OPEN   orcaflex_run_batch: missing engine base-config yml ...
#1565 OPEN   openfoam_run_batch: default work/output dirs land inside the input's directory ...
```

Issue #1565 currently has no labels. This planning branch will not mutate
labels; the orchestrator will need to reconcile exactly one `lane:claude` label
before review-state promotion.

**Reproduction proof** (`2026-07-14T04:33:11Z`; exact helpers were extracted from the source AST because this isolated worktree cannot resolve its sibling editable dependency):

```text
$ python3 <AST harness selecting openfoam_run_batch._resolve_path/_resolve_dir>
work_parent_is_config=True
output_parent_is_config=True
inside_config=True
rc=0
```

The harness executed lines 670-680 against a synthetic `scope-repo/cases/openfoam-run-batch`; the placement matches the claim. Direct `uv run` stopped before import on the unavailable worktree-relative editable dependency and did not contradict this result.

Distinct sources: #1565; #1553/#1560/#1564; OpenFOAM and OrcaFlex code/tests; engine config; commits; registries/wiki; Drive index; live inventory.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-13-issue-1565-openfoam-external-work-root.md` |
| Plan index | `docs/plans/README.md` |
| Production workflow | `src/digitalmodel/workflows/openfoam_run_batch.py` |
| Engine defaults | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` |
| Focused tests | `tests/workflows/test_openfoam_run_batch.py` |
| Plan reviews | `scripts/review/results/2026-07-13-plan-1565-{claude,codex,gemini}.md` (future review outputs) |

No private input, host path, project identifier, native solver result, or
heavy work tree will become a repository artifact.

---

## Deliverable

`openfoam_run_batch` will support a validated external heavy-work root selected
by trusted host environment or explicit configuration, while preserving the
current layout when no override is supplied and keeping returned CSV/JSON
rollups beside the input.

## Normative Design Contract

### 1. Configuration and precedence

The implementation will add one neutral base-config key:

```yaml
openfoam_run_batch:
  run_batch:
    work_root: null
```

The effective heavy-work base will be selected in this exact order:

1. A non-empty `DIGITALMODEL_WORK_ROOT` environment value will win. This is the
   trusted dispatch-host policy and cannot be overridden by request YAML.
2. Otherwise, a non-empty `run_batch.work_root` value will apply.
3. Otherwise, legacy resolution will apply exactly: relative `work_dir` will be
   based at `_config_dir_path`, and an absolute `work_dir` will remain absolute.

An empty environment value and `null`/omitted config value will mean “unset.” A
non-string root will raise `ValueError`. An external root will be expanded for
the current user's home, canonicalized, required to exist as a writable
directory, and will never be created implicitly. Environment-variable
interpolation inside YAML values will not be added.

When an external root is active, `work_dir` will be a non-empty relative path.
Absolute paths, `.`/`..` components, NULs, and paths that resolve outside the
canonical root will fail before any case directory or result file is created.
The canonical external root will also be required to sit outside the nearest
Git checkout containing `_config_dir_path`; if no Git sentinel is present, it
will at minimum be outside the canonical config directory. A trusted root-level
symlink may resolve to its canonical target, but existing symlinks below that
target will fail closed.

`output_dir` will retain its current independent resolution against
`_config_dir_path`. The external work-root setting will never rebase or copy
`results/`.

### 2. Deterministic run isolation

External mode will create this logical layout:

```text
<canonical-external-root>/openfoam-run-<fingerprint>/<relative-work-dir>/<case-name>/
```

`fingerprint` will be the lowercase SHA-256 of canonical JSON encoded as UTF-8
with sorted keys and compact separators. The versioned payload will include the
resolved `base`, ordered resolved `cases`, `mapping`, mode, workers, mock flag,
reconstruct/resume flags, timeout, relative work directory, and a
`work_layout_version` constant. It will exclude timestamps, host paths,
environment-source labels, and output location. Non-JSON values will fail
closed rather than falling back to `repr()`.

The same effective batch input will therefore reuse the same namespace across
retries; any physics/execution-affecting configuration change will select a
different namespace. Every newly written checkpoint will carry the full
fingerprint. External-mode checkpoint reuse will require `status == completed`
and an exact fingerprint match. A missing or mismatched fingerprint will cause
a clean rerun inside the already selected safe namespace, never reuse.

Case names will be non-empty single path components with no `/`, `\\`, NUL,
`.` or `..`. Resolved case names will be unique. Validation of every case name,
the full matrix, the namespace, and both roots will complete before threads,
directory creation, cleanup, or result publication begins.

### 3. Destructive-operation boundary

Every cleanup/prune entrypoint will receive or derive the canonical run root
and will verify immediately before mutation that:

- the target is a strict descendant of that run root;
- the target and existing descendants being removed are not symlinks;
- the run root is itself a strict descendant of the canonical external root;
- the target is not the external root, run root, config directory, or Git root.

Failed containment, symlink, or type checks will raise a descriptive exception;
cleanup will not use `ignore_errors=True` to turn an unsafe or incomplete
deletion into a fresh solve. MPI resume will remain inside the same validated
case directory. This issue will not move, archive, or delete any pre-existing
legacy `batch_runs/` tree.

### 4. Public/small output boundary

Only `cases.csv` and `batch_summary.json` will be written beneath the resolved
`output_dir`, retaining the existing suffix/count/size contract. Heavy case
trees, checkpoints, solver logs, VTK output, and `processor*` will remain under
the external work namespace.

In external mode, returned/in-memory rows and errors will use a stable relative
work reference rooted at the opaque fingerprint namespace. They will not
contain the canonical external-root string, home directory, or another absolute
host path. `batch_summary.json` will add only non-sensitive fields such as
`external_work_root: true`, `work_root_source: environment|config`,
`work_layout_version`, and the opaque fingerprint. Legacy mode will preserve
the existing `case_dir` representation and summary shape except for additions
explicitly accepted during review.

### 5. Compatibility boundary

- With neither override present, default `batch_runs/`, explicit relative and
  absolute `work_dir`, relative/absolute `output_dir`, checkpoint behavior,
  pool/MPI execution, and returned paths will remain compatible with current
  tests.
- `work_root: null` in the packaged base config will not activate external
  mode or change deep-merge precedence.
- Existing external-mode-invalid inputs will fail before side effects with a
  stable `ValueError`; there will be no silent fallback into the checkout.
- OrcaFlex will remain unchanged. A shared cross-workflow root contract, if
  desired, will require a separate issue and review.

---

## Pseudocode

```text
resolve_work_layout(cfg_dir, run_settings, base, cases, mapping):
    root_value, source = first_nonempty(host_env, config_work_root)
    if no root_value:
        return legacy_resolve(work_dir, cfg_dir)
    canonical_root = validate_precreated_external_root(root_value, cfg_dir)
    relative_work_dir = validate_relative_descendant(run_settings.work_dir)
    fingerprint = sha256(canonical_json(versioned_effective_batch_payload))
    run_root = contained_child(canonical_root, "openfoam-run-" + fingerprint)
    work_dir = contained_child(run_root, relative_work_dir)
    validate_no_existing_symlink_components(canonical_root, work_dir)
    return WorkLayout(root, run_root, work_dir, fingerprint, source, external=True)

render_cases(..., layout):
    resolve every case name
    reject unsafe or duplicate case names before creating anything
    attach internal absolute path and stable public work reference separately

load_checkpoint(case_dir, expected_fingerprint, external):
    parse object
    return only completed rows
    if external, also require exact fingerprint

safe_clean_case(case_dir, layout):
    revalidate canonical containment and no symlink target
    remove; surface any incomplete deletion as failure

publish_rows(rows, layout):
    external mode strips/replaces absolute work-root prefixes
    assert serialized CSV/JSON does not contain the external-root bytes
    write only small rollups to independently resolved output_dir
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `tests/workflows/test_openfoam_run_batch.py` | Tests will be written first for precedence, isolation, containment, output residency, leakage, and compatibility |
| Modify | `src/digitalmodel/workflows/openfoam_run_batch.py` | The workflow will resolve and enforce the external work layout |
| Modify | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` | A neutral `work_root: null` key will expose the input contract without changing defaults |
| Keep | `src/digitalmodel/workflows/orcaflex_run_batch.py` | The sibling behavior will remain explicitly out of scope |
| Keep | `tests/workflows/test_orcaflex_run_batch.py` | Existing sibling tests will remain regression evidence only |

No implementation file will be edited until this plan receives adversarial
review and explicit user approval.

---

## TDD Test List

| Test | Contract frozen before implementation |
|---|---|
| `test_work_root_environment_wins_over_config` | A trusted non-empty host value will win over a different YAML root, and the summary will record `environment` without exposing the path |
| `test_work_root_config_used_when_environment_unset` | An absolute precreated config root will activate external mode when the environment key is absent/empty |
| `test_no_override_preserves_legacy_layout_and_outputs` | Default work/results paths and current row/checkpoint behavior will remain byte-compatible where currently asserted |
| `test_base_config_null_work_root_does_not_activate_external_mode` | Real engine deep merge will preserve legacy behavior |
| `test_external_root_requires_preexisting_writable_directory` | Missing, file, non-string, and non-writable roots will fail before side effects |
| `test_external_root_rejects_scope_git_checkout_descendant` | A root in the input checkout, including a sibling of the config directory, will fail |
| `test_external_work_dir_rejects_absolute_and_traversal` | Absolute, empty, `.`, `..`, separator escape, and NUL forms will fail |
| `test_external_root_canonicalizes_trusted_root_symlink` | A root-level symlink will be checked against its canonical target and will remain outside the checkout |
| `test_external_work_path_rejects_descendant_symlink` | A pre-existing symlink below the canonical root will fail before cleanup/write |
| `test_case_names_reject_traversal_and_separators` | Explicit and generated names cannot escape their work directory |
| `test_duplicate_case_names_fail_before_any_directory_creation` | Pool workers cannot race on one case/checkpoint directory |
| `test_fingerprint_is_stable_for_equivalent_mapping_order` | Canonical JSON key order will not change the run namespace |
| `test_fingerprint_changes_for_effective_case_or_run_setting` | Changes to resolved cases, base, mode, workers, mock, reconstruction/resume, timeout, or layout version will isolate the run |
| `test_external_retry_reuses_matching_completed_checkpoint` | The exact same effective input will reuse a completed checkpoint in the same namespace |
| `test_external_retry_rejects_missing_or_mismatched_fingerprint` | A stale/foreign checkpoint will never skip execution |
| `test_pool_retry_cleanup_is_contained` | A failed pool retry will remove only its validated case subtree |
| `test_mpi_resume_and_processor_prune_stay_in_run_root` | Resume/prune will preserve current semantics without touching another namespace |
| `test_cleanup_rejects_root_case_and_symlink_targets` | Destructive helpers will fail closed for the external root, run root, config/Git root, and symlink targets |
| `test_results_remain_beside_input_when_work_is_external` | CSV/JSON will remain under `output_dir`; heavy trees/checkpoints/logs will exist only under the external namespace |
| `test_external_outputs_do_not_contain_absolute_work_root` | CSV, JSON, returned settings, checkpoint-derived rows, and sanitized error text will contain no external-root bytes |
| `test_results_allowlist_still_rejects_heavy_artifacts` | Existing suffix, count, size, VTK, Foam, and processor exclusions will remain enforced |
| `test_external_pool_and_mpi_paths_are_deterministic` | Both execution modes will select the same layout algorithm and stable per-case paths |
| Existing focused suite | All current matrix, workers, fail-closed solver, checkpoint, MPI, and engine-path tests will continue to pass |

---

## Acceptance Criteria

- [ ] New tests will be committed before production changes and will demonstrate
      a red-to-green TDD sequence.
- [ ] `DIGITALMODEL_WORK_ROOT` will override `run_batch.work_root`; config will
      apply only when the environment is unset/empty; neither will preserve the
      current layout.
- [ ] External mode will create heavy work only under
      `<root>/openfoam-run-<sha256>/<work_dir>/<case>` with deterministic
      fingerprint-bound retries.
- [ ] A changed effective batch configuration will not reuse a prior completed
      checkpoint; an exact retry will.
- [ ] Traversal, absolute external-mode `work_dir`, duplicate/unsafe case names,
      descendant symlinks, and roots inside the scope Git checkout will fail
      before filesystem side effects.
- [ ] Cleanup and MPI pruning will revalidate containment immediately before
      deletion and will surface incomplete deletion instead of ignoring it.
- [ ] `results/cases.csv` and `results/batch_summary.json` will remain based at
      the input/config directory by default, and no heavy artifact will enter
      that output tree.
- [ ] No returned CSV/JSON/in-memory row or error will contain the canonical
      external-root string or an absolute heavy-work path.
- [ ] With no override, current relative/absolute work/output path behavior,
      pool/MPI behavior, and checkpoint semantics will remain compatible.
- [ ] OrcaFlex code and behavior will remain unchanged.
- [ ] `PYTHONPATH=src uv run pytest tests/workflows/test_openfoam_run_batch.py -q`
      will pass in a correctly linked repository checkout.
- [ ] `PYTHONPATH=src uv run pytest tests/workflows/test_orcaflex_run_batch.py -q`
      will pass as sibling regression evidence.
- [ ] `uv run ruff check src/digitalmodel/workflows/openfoam_run_batch.py tests/workflows/test_openfoam_run_batch.py`
      will pass.
- [ ] `uv run python -m compileall -q src/digitalmodel/workflows/openfoam_run_batch.py`
      will pass.
- [ ] The canonical repository test command will show zero new failures against
      a same-base baseline if unrelated failures remain.
- [ ] The workspace legal sanity scan will pass without publishing private
      paths, host identifiers, project names, or input/result content.
- [ ] Plan-stage and code-stage adversarial reviews will complete at the required
      tier before the issue can close.

---

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | No review has been requested by this drafting task |
| Codex | PENDING | No review has been requested by this drafting task |
| Gemini | PENDING | No review has been requested by this drafting task |

**Overall result:** PENDING — draft only; not approval-ready.

---

## Risks and Open Questions

- **Risk — fingerprint evolution:** A future execution-affecting field could be
  omitted from the canonical payload. The payload will carry an explicit layout
  version and tests will enumerate every current field; future changes will
  need to update both.
- **Risk — TOCTOU:** Path revalidation reduces but cannot eliminate a hostile
  same-user race between validation and filesystem mutation. The dispatch host
  and work root are assumed to be controlled by the same trusted account; a
  stronger dirfd/sandbox boundary would require a separate security issue.
- **Risk — output compatibility:** Replacing absolute `case_dir` in external
  mode may affect a consumer that treats returned rows as host-local paths.
  Legacy mode will stay unchanged, and external mode will expose a stable
  relative work reference instead of a misleading/unusable returned host path.
- **Risk — work retention:** This issue will relocate heavy work but will not
  define quotas, garbage collection, or migration. Retention/cleanup policy
  will require a separately approved transaction.
- **Open — environment provisioning:** The host operator will need to create and
  permission the external root before enabling the environment value. This plan
  will not change host state.
- **Governance gap:** Issue #1565 has no lane/status labels at drafting time.
  Label reconciliation remains an orchestrator action after review evidence;
  this branch will not perform it.

---

## Complexity: T2

The production change will remain localized to one workflow and its packaged
defaults, but filesystem deletion safety, deterministic checkpoint isolation,
host/config precedence, privacy-safe output, and legacy compatibility require a
substantial focused test matrix and independent adversarial review.
