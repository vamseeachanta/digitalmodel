# Issue 2082 — final code review disposition

Tracking: [issue 2082](https://github.com/vamseeachanta/digitalmodel/issues/2082), [PR 2084](https://github.com/vamseeachanta/digitalmodel/pull/2084).

The owner continued after the explicit bounded-plan approval request. Implementation followed failing tests; no plan-approved label was self-applied. Final runtime revision: `f61dedfd80ecd57feb346fe172d640cbe63c1155`.

| Review | Actual result | Main disposition |
|---|---|---|
| [Codex r1](issue-2082-codex-code-r1.md) | MINOR, no blocking probe/API defect | Missing mandatory input files now fail before child launch; exact manifest keys are tested. Deadline wording now distinguishes the 90-second child wait from revision/cleanup/setup overhead. |
| [Claude r1](issue-2082-claude-code-r1.md) | UNAVAILABLE, 180-second timeout with no output | Preserved; not counted as a review verdict. |
| [Claude r2](issue-2082-claude-code-r2.md) | MAJOR overall; probe APPROVE | Final termination wait was unchecked. Two RED tests reproduced false success for timeout and wait failure; cleanup now requires confirmed termination and closes handles even when cleanup raises. Patched inline; no further provider loop. |
| Gemini | UNAVAILABLE | Configured read-only review mode was unavailable during this issue; no credentials/settings were changed and no code verdict is claimed. |

Claude's separate claim that `finally` is skipped when ResumeThread raises is incorrect. The proposed raise-before-close repair was not adopted: it would create a handle leak. The final nested cleanup protects handle closure from exceptions in cleanup itself. Its optional request to include the invalid JSON token in a generic diagnostic is deferred; the message and failure stage already identify the refusal without exposing source content.

Actual provider verdicts remain as recorded, not retroactively relabeled APPROVE. Main verified the accepted fixes with 63 passing focused tests and repeated native acceptance after the last runtime change. Native 11.6c proof: PASS/exit zero, one observed thread per model, 81 finite solve samples and 81 finite reload samples, 122100-byte saved simulation. See [native proof](issue-2082-native-proof.json).

General findings are promoted to [mandatory proof coverage 2085](https://github.com/vamseeachanta/digitalmodel/issues/2085); CI omissions and Windows collection have [2083](https://github.com/vamseeachanta/digitalmodel/issues/2083) and [2086](https://github.com/vamseeachanta/digitalmodel/issues/2086). Hosted CI is independent evidence; it is not certified by these local tests or reviews. No deployment, merge or issue closure is implied.
