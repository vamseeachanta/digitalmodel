# Plan for #1574: Source-Neutral Sloshing Privacy Cleanup

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1574
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1574-{claude,codex,gemini}.md`

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

Importing the reusable OpenFOAM package exposes a project-coded symbol through
`__all__`. Static inspection therefore reproduces the privacy defect without
running a solver or using private data. Current tests do not enforce a neutral
public namespace, generated-artifact closure, or scanner self-coverage.

Distinct sources: issue #1574; package exports; pressure-tap module and tests;
coupling and validation modules; CFD scripts; legal-scanning documentation;
workspace-hub #3522; packaging metadata; and documentation generators (9+).

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1574-sloshing-privacy-cleanup.md` |
| Neutral tap API | `src/digitalmodel/solvers/openfoam/pressure_taps.py` |
| Public exports | `src/digitalmodel/solvers/openfoam/__init__.py` |
| Reusable sloshing code | `src/digitalmodel/solvers/openfoam/validation/`; `sloshing_coupling.py` |
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

Each project-coded public API will be replaced by a generic equivalent whose
name describes engineering meaning (for example, tank-layout pressure taps)
rather than engagement identity. Generic function signatures and numerical
behavior will remain stable where possible. Sensitive aliases, re-exports,
deprecation warnings, migration maps, and changelog spellings will not be
committed. Release notes will describe only a privacy-driven removal and the
neutral replacement API.

The scanner will accept an explicit repository root, a versioned public scope
manifest, and an authenticated rule snapshot supplied by the #3522 authority.
It will:

1. validate the snapshot schema, generation, authenticity, and expiry before
   scanning; missing/invalid authority will return a distinct nonzero code;
2. scan raw bytes and decoded text in the declared source/test/script/doc scope,
   package export metadata, generated API artifacts, and the staged diff's path,
   content, rename, and commit-message surfaces;
3. normalize Unicode and case exactly as the authority contract specifies,
   compare without logging sensitive rule values, and report only rule IDs,
   paths, line numbers, and match classes;
4. reject undecodable, oversized, symlink-escaping, or unenumerated generated
   artifacts rather than treating them as clean;
5. support only per-line forensic sentinels in scanner-owned tests and only for
   synthetic rules; no sensitive production match may be allowlisted;
6. scan its own implementation, tests, plan, and review artifacts so the check
   cannot block itself or hide behind a blanket path exemption.

Fork pull requests will run synthetic scanner tests without protected values.
The protected same-repository maintainer workflow will run the authenticated
production scan before merge; a fork result cannot satisfy that gate. Workflow,
environment, and ruleset changes remain in #3522 and require owner-controlled
external-state approval.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `pressure_taps.py` | replace coded helper/defaults with neutral engineering API |
| Modify | OpenFOAM package exports | remove coded public namespace and export replacement |
| Modify | coupling/validation/scripts as inventoried | neutralize labels, defaults, and prose |
| Modify | focused OpenFOAM tests | synthetic fixtures and explicit removal/replacement checks |
| Create | public-surface scanner | authenticated fail-closed enforcement adapter |
| Create | scanner tests | hostile path/content/self-block/authority cases |
| Regenerate | public API documentation | remove stale generated exposure |

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
| `test_symlink_binary_oversize_decode_fail_closed` | ambiguous filesystem/content cases cannot pass clean |
| `test_generated_docs_manifest_complete` | expected generated surfaces are enumerated and scanned |
| `test_scanner_scans_itself` | implementation, tests, plans, and reviews receive normal coverage |
| `test_synthetic_line_sentinel_only` | exact-line synthetic fixture works; file-wide exemption is rejected |
| `test_production_rule_cannot_be_allowlisted` | protected matches remain blocking in every public path |
| `test_fork_result_never_satisfies_protected_gate` | untrusted workflow cannot assert production-clean state |
| `test_module_and_function_size_limits` | all touched code satisfies universal limits |

## Implementation Sequence

1. Wait for approved #3522 Phase A to merge; pin its exact authority/workflow SHA
   and verify the protected environment/ruleset readback before using it.
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

- [ ] #3522 Phase A is merged and its exact reusable-workflow/authority SHA is pinned.
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
- [ ] Focused and full `tests/solvers/openfoam/` suites pass.
- [ ] Generated API docs and packaging/import smoke tests pass from a clean tree.
- [ ] `scripts/legal/legal-sanity-scan.sh`, changed-path lint/compile checks, and
      `git diff --check` pass.
- [ ] Modified files/functions satisfy 400/50-line limits.
- [ ] T3 code/artifact review reaches no-MAJOR consensus and the issue receives
      an implementation summary comment.
- [ ] No client data, queue execution, self-merge, self-close, or public result
      promotion occurs.

## Adversarial Review Summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | pending | exact pushed draft required |
| Codex | pending | exact pushed draft required |
| Gemini | pending | exact pushed draft required |

**Overall:** draft; implementation requires adversarial review and explicit user
approval. No agent may apply `status:plan-approved` or create its marker.

## Risks and Open Questions

- #3522 is a hard dependency: duplicating or weakening its authority would make
  the public privacy assertion unverifiable.
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
