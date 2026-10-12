# Pipeline defect screen review and verification

Scope: [issue 2181](https://github.com/vamseeachanta/digitalmodel/issues/2181), refreshed Wave 1 plan-lite on [epic 1057](https://github.com/vamseeachanta/digitalmodel/issues/1057). Originating dispatch authorizes bounded implementation and one public draft PR; Claude reviews before owner merge. No main merge, force push, status label change or licensed-table transcription is included.

- Plan: Codex adversarial analysis and Claude R1/R2 MAJOR findings, closed by inline R3 definitions and regression checks.
- Code: Claude R1 MAJOR, R2 MINOR with no numerical/API defect found; actionable MINOR findings closed inline under the no-third-dispatch rule. Final source hashes are retained in `reviewed-sources.json`. Codex final inline review confirms the bounded adapter changes and their callers against those bytes.
- Gemini: UNAVAILABLE. Read-only-mode configuration failed first; the default CLI route then failed credential refresh with `invalid_grant`. No callable alternative review tool was present. This is an authentication failure, not an application-review verdict.
- Follow-ons: [generic decision/report semantics](https://github.com/vamseeachanta/digitalmodel/issues/2331), [area-weighted source-pit applicability](https://github.com/vamseeachanta/digitalmodel/issues/2335). The workflow uses MAX only; existing area-weighted sensitivity behavior is preserved and tested.

Final checks: **183 passed**, including **35 adapter tests**, **1 registry case**, **90 existing method tests**, **36 applicability tests**, and **21 catalog tests**. Both committed examples execute through the offline engine and retain separate reports and input echoes. Catalog render/check confirms 78 FFS rows and 3 design-screen rows; the offering Markdown remains byte-identical because its per-offering statuses did not change. The capability-map projection changes only the three named engine entries. Black, Ruff fatal/unused-name checks and whitespace checks pass; the new module is 399 lines and every new function is below 50 lines.

Tests use `uv run --no-sync` with a worktree-local environment and the canonical assetutilities source on PYTHONPATH. Default `uv run` dependency resolution from this worktree cannot find its relative editable sibling; dependency configuration was not changed. The full repository suite was not run.

Engineering limits: one caller-defined blunt-defect window, inches/psi, positive pressure and tensile membrane demand; boundary loss requires confirmation that the window captures the defect. Current-demand ACCEPT/DERATE/ESCALATE uses explicit capacity-to-demand criteria, not RSF severity bands. Combined loading, colony interaction, lifetime prediction and edition-matched API 579 Part 5 qualification are not established. Catalog notes identify the exercised entry points. The 1334 psi reference anchor is DNV-F101, not RSTRENG.

Public bundle review: code, synthetic examples, generated catalog projection, tests and review records are the outgoing set. Necessary method/standard/issue identifiers are retained; no measured/client data or physical hostnames are added. Licensed standards are linked by identifier and existing validation records; new numbers are regenerated. Secret scanning identifies review digests verified against source bytes and one unchanged public workflow fixture path; these are not credentials. No credential finding remains. The explicit identifier scan is run before publication. Generated result YAML/HTML and worktree-local environments/caches remain ignored and local, not part of the public bundle.

Cleanup classification: tracked changes are task-scoped; no stash, partial/trash artifact or sibling-repository modification is introduced. EXPECTED residue is the retained ignored example results, worktree-local virtual environment and task-scoped review/test/tool cache under temporary storage. No data is deleted.


## Fix-up fz2338 — 2026-10-10

The earlier parity-only factor disclosure is superseded by this numerical correction.
DNV-RP-F101 Part B applies modelling F1 = 0.9 times operational caller F2;
F2 = 0.72 gives F = 0.648 and DNV-RP-F101 Part B reference safe working pressure
864.5577179776003 psi. The computed factors are retained on the method row and
used by the HTML assessment-basis table. Both synthetic examples are regenerated
through the engine; generated YAML/HTML remains ignored local output.
The consolidated validation record gains a bounded Part B addendum, so the
catalog engine remains live for that method. The legacy total-F alias retains
its default 0.72 and unchanged 960.6196866417781 psi reference pressure.

TDD: first regression run 17 failed / 40 passed; review-delta regression run
3 failed / 63 passed. Final asset-integrity directory: 982 passed / 3 skipped;
selected durable workflow: 1 passed / 129 deselected. Catalog check:
78 FFS rows / 3 design rows, no drift. Black, Ruff F/E9 and whitespace pass.
Tests use uv run --no-sync, a temporary uv cache, and canonical assetutilities
source on PYTHONPATH because the worktree's relative editable dependency is not
installed by default synchronization. Existing pytest policy ignores deprecation
warnings; an explicit pytest.warns test pins the alias warning.

Claude R1 identifies silent F1 omission on the legacy path; the correction rejects
F1 without F2 and tests ambiguity, invalid F1 and report/compute basis agreement.
Frozen R2 byte hashes are retained in fz2338-reviewed-sources.json. Codex inline
review checks the decision caller, legacy interaction callers and report inputs.
Gemini is UNAVAILABLE: fresh credential refresh fails invalid_grant; no verdict
is inferred from that authentication failure. The earlier reviewed-sources.json
and review outputs remain historical; they do not certify this fix-up.

Public bundle: source, tests, synthetic input comments, catalog caveat and
regenerated validation numbers only; no standards PDF/table, client data, physical
hostname or credential is added. No main merge, rebase, force-push or data deletion.
Cleanup: CLEAN tracked task scope and no stash/partial/trash residue; EXPECTED
ignored example outputs, existing virtual environment and fz2338 temporary
review/test/cache artifacts are retained. No sibling checkout is modified.

Claude R2 closes the numerical/API MAJOR and reports secondary interface findings.
Inline R3 binds every report basis value to the saved assessment and warns only
on explicit legacy alias use (2 failed / 66 passed before these changes).
The requested screen-F2/engine-total-F compatibility contract is retained;
computed output carries separately named F1/F2/F. YAML-native finite numbers
are the supported input type; broad Decimal/numpy-scalar coercion is outside scope.
Legacy interacting callers explicitly use the deprecated total-F alias; their
existing numerical meaning is preserved and repository warning policy is verified.
Final acceptance is Codex inline against the R3 byte manifest, not an inferred
Claude APPROVE. Claude review remains required before owner merge.
