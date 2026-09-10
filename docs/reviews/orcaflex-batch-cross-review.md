# OrcaFlex batch reliability — review record

Scope: [1564](https://github.com/vamseeachanta/digitalmodel/issues/1564) and [2051](https://github.com/vamseeachanta/digitalmodel/issues/2051), 2026-09-09. T2, Claude and Codex. This records review and disposition, not unanimous approval or deployment authorization.

## Plan stage

- Codex independent plan review: APPROVE for the bounded repair after resource-thread enforcement was explicitly separated.
- Claude Sonnet, dispatched over authenticated SSH: r1 MAJOR requested exact central verdict constants and both licence/executor seams in offline real-mode tests; also requested explicit UTF-8/BOM and native opt-in boundaries. Plan/tests incorporated these. R2 MINOR confirmed the major concerns closed; actual returned verdict membership is tested, not only constant existence.
- Gemini remained unavailable in this environment. No third-provider consensus is claimed.

## Code stage

- Initial independent Codex review approved the bounded central verdict repair. Final independent review of the additional loader change is recorded in the sibling Codex review artifact.
- Claude r1: REQUEST CHANGES. Its four concerns were inferred availability, missing dictionary keys, legacy encoding compatibility and unversioned identity. Main-session source verification found the mandatory `_license_available()` refusal precedes real execution, with an existing unavailable-licence regression. `_manifest_rows` constructs both required keys internally. Those two hypothetical paths are not reachable defects in this patch. Explicit UTF-8/BOM support and uncertified legacy encoding are now documented. The sidecar identity remains unversioned; the native proof records the DLL version separately. That advisory limitation is retained rather than claiming the sidecar includes version provenance.
- Claude full-source r2 returned exit 1 without review output after bounded dispatch: UNAVAILABLE. It is not an approval. The earlier adverse verdict and main-session dispositions remain visible.
- The native run independently exposed a real blank-versus-tilde error. Three new failing regressions preceded the SafeLoader subclass repair. The final native engine run, statics, dynamics and saved-simulation reload passed. Final focused tests: 54 passed, one skipped.

## Artifact review and inline corrections

Claude reviewed the runbook/report and requested changes: distinguish overlapping test totals; explain Linux producer versus Windows execution; isolate the committing dispatch example; explain the duration override; narrow the offline-test licence statement. Main applied these corrections inline. Its unsupported-Windows concern was resolved by the actual local Windows evidence. Linked issue/source claims were locally verified; the no-tools review could not verify them. Codex's separate runbook review and local-link checks also supplied corrections and verification. No additional r3 dispatch was used.

## General findings and limits

The legal scanner's named-repository route printed an empty resolved path and falsely passed. Existing [workspace-hub 3804](https://github.com/vamseeachanta/workspace-hub/issues/3804) received the reproduction. The actual digitalmodel scan used explicit-root `--all --diff-only` in an isolated hub worktree without initialized submodules and printed the correct path before passing. This workaround is not valid for every workspace layout.

Includes, duplicate-key fidelity, same-stem collisions, authenticated evidence, global resource guards, versioned sidecar identity and production rollout remain outside this bounded acceptance. Review evidence supports a draft repair for integration review; it does not certify the broader execution plan.
