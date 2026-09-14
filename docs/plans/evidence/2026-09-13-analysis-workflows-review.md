# Analysis workflow definitions — review and verification

Scope: user-requested operational workflow definitions, tracked through [ecosystem routing issue 3853](https://github.com/vamseeachanta/workspace-hub/issues/3853), [ANSYS issue 2119](https://github.com/vamseeachanta/digitalmodel/issues/2119) and [AQWA issue 141](https://github.com/vamseeachanta/digitalmodel/issues/141). This is documentation delivery, not implementation or solve approval.

## Exact-current review

Claude reviewed the immutable Git tree `0f4aa5e6036114ca2c2d8ebcc3f1413376466f77` and returned MINOR. Independent non-author Codex review returned APPROVE on the same four file contents. The tree identifies staged content; it is not asserted to be a branch commit. The main session constructed the bundle from Git blobs, recomputed SHA-256 values and verified the returned path/hash map.

| File | SHA-256 of Git blob bytes |
|---|---|
| `docs/README.md` | `f4b06cfd0c8eb3b8c100aa58934d848d433fee4bb7c63c2f943765f2d5523588` |
| `docs/standards/engineering-analysis-workflow.html` | `10c4962998b9310f389cc219dd7d7f593e7d1fac867b3ea247cc7f9ff88dc753` |
| `docs/domains/ansys/analysis-workflow.html` | `a09976ebbfff14617cf63a7e69cfe208c11cd373aa765f14c54638e3b23b540a` |
| `docs/domains/aqwa/analysis-workflow.html` | `719710cb2d0e66d3f9ca53b9fc26d27c5dfc70d306f6ed407578ef16601a4c4e` |

Table 1. Exact reviewed operating definitions and routing content.

Earlier Claude rounds identified ambiguous Stage A naming, AQWA quantity labels and local/common stage numbering. Corrections bind Stage A to the existing pinned ANSYS plan, distinguish body mass from density, remove conflicting local numbering and explicitly map domain actions to common stages. Codex identified a native-only common execution gate; the corrected gate supports analytical calculations with native receipts required only for native solves.

## Final findings and disposition

- F1: the combined ANSYS prepare/execute row inherits both common stage gates; it does not waive preflight. Splitting the presentation is a retained editorial improvement.
- F2: issue 2119 plan findings/dispositions are recorded in `docs/plans/evidence/2026-09-13-issue-2119-review.md` at the cited baseline commit. Its MINOR verdict is not implementation approval.
- F3–F4: Stage A is explicitly tied to a pinned plan and refuses off-grid lookup. Interpolation capability is optional; its validation requirement is mandatory whenever used. No optional-validation interpretation is accepted. Future extension requires a reviewed scope/revision change.
- F5: this record supplies the AQWA revision-1 review locator, verdicts and hashes; the issue publication comments will link it.
- F6: attribution identifies the competent non-author checker and evidence. The shared contract does not universally require a cryptographic signature; any governing project signature requirement remains binding. Execution records retain both workings and results where the method produces them.
- F7–F8: eight routing tests pass against the final README. Browser checks confirm all new local HTML links resolve, with no page errors or horizontal overflow. Main and independent Codex local inspection verified cited criteria/plan revisions, source interfaces, AQWA fallback behavior and canonical mass/density/depth fields. Claude reviewed supplied content without filesystem access; independent recomputation by Claude is not claimed. The historical issue-141 body is source evidence of its stated basis, not independent native qualification.
- F9: the documentation-maintenance route starts at this README, whose required and curated lists link all new surfaces. No unreachable workflow was found. Repeating those links in every table cell is a navigational suggestion, not a missing gate.

The original neutral bulk-store example remains in the README because the existing routing contract tests require it; it is labelled an implementation example rather than an actual private evidence locator. One attempted editorial removal failed the existing test; restoration returned the final suite to eight passing tests. No test was weakened to obtain that result.

## Delivery boundaries and cleanup

The workflows define basis, study, preparation, execution, verification, saved-result, lookup and maintenance actions. No new software adapter, database service, numerical result, native solve or fleet-wide enforcement is delivered. ANSYS Stage A implementation approval remains pending; the AQWA unit-box basis is unresolved. Source ownership and native-output residence rules remain unchanged.

The four exact staged documents passed the scoped text legal scan, browser/local-link checks and whitespace checks. Private review bundles/results, verification receipts/scripts and screenshots are retained expected evidence. No task stash, root partial artifact or cleanup marker was observed. Existing sibling worktrees remain preserved. This record is factual review collation, not another independent review.
