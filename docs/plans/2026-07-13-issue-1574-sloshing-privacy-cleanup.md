# Plan for #1574: Source-Neutral Sloshing Privacy Cleanup

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1574
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1574-r{1,2}-consolidated.md`

---

## Resource Intelligence Summary

### Existing code and exposure

- `src/digitalmodel/solvers/openfoam/__init__.py` publicly imports and exports a
  project-coded pressure-tap helper from the reusable package surface.
- `src/digitalmodel/solvers/openfoam/pressure_taps.py` and its tests contain the
  corresponding helper, project-shaped defaults, labels, and examples.
- `src/digitalmodel/solvers/openfoam/sloshing_coupling.py`, validation modules,
  scripts, tests, generated documentation, and tracked plans are additional
  reachable surfaces requiring an inventory rather than a one-symbol rename.
- A repository search confirms the identifier class is already reachable from
  public imports. This plan deliberately does not repeat the sensitive literal
  spellings or private job context.
- Existing legal-scanning documentation allows narrowly justified forensic
  fixtures. A cleanup scanner must therefore use line-level sentinels and must
  not exempt whole files or silently skip its own test artifacts.

### Governing dependencies and boundaries

- Workspace-hub issue [#3522](https://github.com/vamseeachanta/workspace-hub/issues/3522)
  will own the authenticated private rule authority needed to compare sensitive
  values without committing those values to this public repository. #1574 will
  not invent a second secret registry or expose reversible encodings.
- Public source, fixtures, review artifacts, commit messages, issue comments,
  generated docs, and reports will contain neutral synthetic terminology only.
- Compatibility will preserve generic behavior, not a sensitive public symbol.
  Removing a project-coded import is an intentional privacy break; a deprecated
  alias would perpetuate the disclosure and is therefore out of scope.
- No private geometry, values, paths, queue records, or result excerpts will be
  copied into this repository or its GitHub metadata.

### Gap and reproduction

The following value-withholding probe reproduces the public-namespace defect at
`origin/main` without printing the coded symbol or using private data:

```bash
PYTHONPATH=src uv run python -c \
  "import digitalmodel.solvers.openfoam as m; print(sum(n.endswith('_default_taps') for n in m.__all__))"
# exact output: 1
```

Current tests do not enforce a neutral public namespace, generated-artifact
closure, or scanner self-coverage.

Distinct sources: issue #1574; package exports; pressure-tap module and tests;
coupling and validation modules; CFD scripts; legal-scanning documentation;
workspace-hub #3522; packaging metadata; and documentation generators (9+).

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1574-sloshing-privacy-cleanup.md` |
| Neutral tap API | `src/digitalmodel/solvers/openfoam/pressure_taps.py` |
| Public exports | `src/digitalmodel/solvers/openfoam/__init__.py` |
| Scope manifest | `scripts/legal/sloshing-public-surface-v1.json` |
| Reusable sloshing code | `src/digitalmodel/solvers/openfoam/validation/`; `sloshing_coupling*.py` |
| Scanner | `scripts/legal/check_sloshing_public_surface.py` |
| Scanner tests | `tests/scripts/test_check_sloshing_public_surface.py` |
| API/regression tests | `tests/solvers/openfoam/` |
| Review evidence | `scripts/review/results/2026-07-13-plan-1574-*.md` |

## Deliverable

The reusable sloshing/OpenFOAM surface will use source-neutral names and
synthetic defaults, while an authenticated fail-closed scanner will prove that
the protected identifier class is absent from source, tests, package exports,
generated documentation, and changed Git metadata.

## Privacy and Compatibility Contract

The cleanup will classify every finding before editing:

```text
public_api_symbol | default_value | docstring | fixture | output_label |
generated_artifact | git_metadata | forensic_reference
```

The public model will remain `PressureTap(name, location=None, patch=None,
fields=("p",), operation="areaAverage")` in `pressure_tap_models.py`, with the
same point/patch/surface validation and export from `openfoam.__init__`. The
coded factory will be replaced by
`rectangular_tank_wall_taps(*, tank_length_m, tank_width_m,
tap_elevations_m, fields=("p","p_rgh")) -> tuple[PressureTap,...]` in
`pressure_taps.py`; it validates finite positive dimensions, strictly interior
finite elevations, and emits deterministic neutral `wall_<n>` names. No defaults
encode a real geometry. #1575 will consume only this exact `PressureTap` model.
Sensitive aliases, re-exports, deprecation warnings, migration maps, and
changelog spellings will not be committed.

The scanner will accept an explicit repository root, a versioned public scope
manifest, and the authenticated CURRENT rule snapshot supplied by the approved
#3522 Phase B authority. Phase A synthetic codecs/tests are insufficient for a
production-clean assertion. The snapshot fields and verification CLI will be
pinned to the exact merged Phase B contract rather than extended locally.
It will:

1. validate snapshot schema, generation, CURRENT-slot authenticity, and
   anti-rollback state; missing/invalid authority returns a distinct nonzero code;
2. NUL-safely enumerate the complete raw tracked Git tree and require every path
   and extension to be classified by the scope manifest; new/unclassified paths
   fail rather than disappearing from coverage;
3. expose separate byte-oriented entry points for `--git-tree <oid>`, `--staged`
   (index blobs plus add/rename/delete paths), `--commit-message-file <path>`, and
   `--metadata-json <path>`; `--generated-root <dir> --generated-manifest <json>`
   scans every untracked generated byte and rejects missing/extra outputs; CI pins
   base/head OIDs and scans both raw object sets;
4. normalize Unicode and case exactly as the authority contract specifies,
   compare without logging sensitive rule values, and report only rule IDs,
   opaque path IDs, byte offsets, and match classes; the private workflow retains
   any reversible path map and never publishes it;
5. reject undecodable, oversized, symlink-escaping, or unenumerated generated
   artifacts rather than treating them as clean;
6. support only per-line forensic sentinels in scanner-owned tests and only for
   synthetic rules; no sensitive production match may be allowlisted;
7. scan its own implementation, tests, plan, and review artifacts so the check
   cannot block itself or hide behind a blanket path exemption.

Fork pull requests will run synthetic scanner tests without protected values.
The protected same-repository maintainer workflow will run the authenticated
production scan before merge; a fork result cannot satisfy that gate. Workflow,
environment, and ruleset changes remain in #3522 and require owner-controlled
external-state approval.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Split/modify | `src/digitalmodel/solvers/openfoam/pressure_taps.py`; new `pressure_tap_models.py`; new `pressure_tap_analysis.py` | neutral API and reduce current 629-line module |
| Split/modify | `src/digitalmodel/solvers/openfoam/sloshing_coupling.py`; new `sloshing_coupling_models.py` | neutralize reusable models and reduce current 682-line module before #1578 |
| Modify | `src/digitalmodel/solvers/openfoam/__init__.py`; `validation/sloshing_2d.py`; `validation/sloshing_sweep.py` | remove coded exports/defaults/prose |
| Modify | `scripts/capabilities/build_sloshing_explorer.py`; `scripts/cfd/run_sloshing_3d_benchmark.py`; `scripts/setup/provision-cfd-box.sh` | neutralize inventoried script surfaces |
| Split/modify | `tests/solvers/openfoam/test_pressure_taps.py`; `test_sloshing_coupling.py`; new focused test modules | synthetic fixtures; reduce current 410-line coupling test |
| Create | `scripts/legal/check_sloshing_public_surface.py`; `sloshing-public-surface-v1.json` | authenticated scanner and exhaustive classification |
| Create | `tests/scripts/test_check_sloshing_public_surface.py` | hostile Git/filesystem/self-block/authority cases |
| Regenerate | outputs named by `uv run python -m sphinx -W --keep-going docs docs/_build/html` | clean public API documentation |

Every modified implementation file will remain at or below 400 lines and every
function at or below 50 lines. Oversized touched modules will be split by
responsibility rather than grandfathered.

## TDD Test List

| Test | Verification |
|---|---|
| `test_coded_export_absent` | removed public symbol cannot be imported or found in `__all__` |
| `test_neutral_tap_api_preserves_geometry` | independent synthetic coordinates match expected engineering layout |
| `test_no_sensitive_compatibility_alias` | import, warning, mapping, and docs do not retain the removed spelling |
| `test_snapshot_required_and_authenticated` | missing, stale, malformed, or forged authority fails closed |
| `test_match_reports_rule_id_not_value` | diagnostics cannot disclose the protected value |
| `test_unicode_case_and_path_variants` | normalization catches hostile variants defined by authority |
| `test_rename_delete_and_commit_metadata` | staged rename/deletion and commit-message surfaces are covered |
| `test_unclassified_tracked_path_fails` | a new tracked path/extension cannot create an omission-based clean result |
| `test_worktree_differs_from_staged_blob` | staged scan uses exact index blobs, not mutable working-tree bytes |
| `test_symlink_binary_oversize_decode_fail_closed` | ambiguous filesystem/content cases cannot pass clean |
| `test_generated_docs_manifest_complete` | expected generated surfaces are enumerated and scanned |
| `test_scanner_scans_itself` | implementation, tests, plans, and reviews receive normal coverage |
| `test_synthetic_line_sentinel_only` | exact-line synthetic fixture works; file-wide exemption is rejected |
| `test_production_rule_cannot_be_allowlisted` | protected matches remain blocking in every public path |
| `test_fork_result_never_satisfies_protected_gate` | untrusted workflow cannot assert production-clean state |
| `test_module_and_function_size_limits` | all touched code satisfies universal limits |

## Implementation Sequence

1. Use merged #3522 Phase A only for synthetic RED tests. Wait for separately
   owner-approved Phase B migration/provision/CAS cutover; pin the exact workflow,
   CURRENT generation, environment/ruleset, and required-check readbacks.
2. Inventory all reachable public surfaces and store only neutral rule IDs,
   classifications, and file locations in the implementation evidence.
3. Add RED package-export and neutral pressure-tap behavior tests; replace the
   coded API and synthetic defaults without a sensitive compatibility alias.
4. Add RED authenticated-scanner, self-blocking, filesystem, staged-diff, and
   diagnostic-redaction tests; implement the bounded scanner adapter.
5. Neutralize remaining inventoried reusable modules, fixtures, scripts, and
   prose one surface at a time, running focused tests after every file.
6. Regenerate API docs from a clean tree, compare the manifest, and run the
   authenticated scan over source plus generated output and Git metadata.
7. Run legal/security, packaging, full OpenFOAM regressions, and T3 adversarial
   code/artifact review; resolve every MAJOR before requesting merge review.

## Acceptance Criteria

- [ ] #3522 Phase B is separately approved, merged, provisioned, CAS-promoted to
      CURRENT, and its exact workflow/generation/environment/ruleset readbacks are pinned.
- [ ] RED evidence precedes every implementation slice.
- [ ] Inventory covers source, tests, scripts, exports, fixtures, docs, generated
      artifacts, paths/renames/deletions, and changed commit metadata.
- [ ] Project-coded APIs/defaults are absent; generic replacement behavior uses
      only synthetic fixtures and has no sensitive compatibility alias.
- [ ] Authenticated production scan passes without printing protected values;
      missing or invalid authority fails closed with a distinct result.
- [ ] Scanner-owned plans/tests/artifacts pass their own enforcement, with only
      exact-line synthetic forensic sentinels and no blanket file exemptions.
- [ ] Fork CI cannot satisfy the protected production-clean merge requirement.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/scripts/test_check_sloshing_public_surface.py tests/solvers/openfoam/test_pressure_taps.py tests/solvers/openfoam/test_sloshing_coupling.py tests/solvers/openfoam/validation -q` passes.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam -q` passes.
- [ ] `uv run python -m sphinx -W --keep-going docs docs/_build/html` succeeds from
      a clean tree and the generated-output manifest matches exactly.
- [ ] `uv run ruff check scripts/legal/check_sloshing_public_surface.py src/digitalmodel/solvers/openfoam tests/scripts tests/solvers/openfoam` passes.
- [ ] `PYTHONPATH=src uv run python -m compileall -q src/digitalmodel/solvers/openfoam scripts/legal/check_sloshing_public_surface.py` passes.
- [ ] `uv run python scripts/legal/check_sloshing_public_surface.py --repo-root . --git-tree HEAD --generated-root docs/_build/html --generated-manifest docs/_build/manifest.json --authority "$PRIVATE_RULE_SNAPSHOT"` and the separately pinned staged/message/metadata invocations pass in the protected workflow.
- [ ] The SHA-verified cross-repository legal scan passes:
      `test -n "$WORKSPACE_HUB_ROOT" && test -n "$DIGITALMODEL_REL_FROM_HUB" && EXPECTED_SHA="$(git rev-parse HEAD)" && test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA" && (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)`; `git diff --check` passes.
- [ ] Modified files/functions satisfy 400/50-line limits.
- [ ] T3 code/artifact review reaches no-MAJOR consensus and the issue receives
      an implementation summary comment.
- [ ] No client data, queue execution, self-merge, self-close, or public result
      promotion occurs.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | MAJOR | Phase-B gate, separate Git entry points, exhaustive scope, exact commands |
| Codex | MAJOR | authority phase, opaque diagnostics, reproduction, executable inventory |
| Gemini | MAJOR | authority contract, path redaction, oversized-module split |

**Overall:** r1/r2 MAJOR findings are resolved inline in r3. Per the loop-break
rule, r3 is not redispatched; explicit user approval remains required. No agent
may apply `status:plan-approved` or create its marker.

## Risks and Open Questions

- #3522 Phase B is a hard dependency: Phase A can prove only synthetic mechanics;
  duplicating or weakening the production authority would be unverifiable.
- Removing an already-public sensitive symbol is intentionally breaking. Keeping
  an alias would preserve the defect; downstream callers must adopt the neutral
  API in the same approved change.
- Git history is not rewritten by this issue. Historical exposure and provider-
  side cache remediation require separate owner-authorized incident scope.
- Generated docs are acceptance evidence only when recreated from a clean tree;
  stale local output cannot establish closure.

## Complexity: T3

This changes a public API, privacy enforcement, protected-workflow evidence,
generated artifacts, and packaging behavior across multiple surfaces.
