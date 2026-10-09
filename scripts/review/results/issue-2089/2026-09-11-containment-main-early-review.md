# Main-session early implementation review

This is an in-progress defect log, not a final review verdict or claim that the full implementation is frozen. No native test had run when these observations were sent to the sole writer.

- Native cleanup must attempt every owned handle even when a preceding wait/close fails; retain uncertain handle identity rather than clearing it before successful close. Independent ABI review found this call-site defect.
- A pending CreateProcess or unknown descendant census cannot become cleanup_confirmed through an empty resource list. Preserve creation/identity uncertainty and the stable interlock.
- Parent/child scenarios must require the child, except positively evidenced breakaway denial. Missing child data cannot satisfy timeout/census proof.
- Persisted private/public recovery state must agree with the returned result; do not publish failed then mutate only the returned state.
- Native evidence must preserve timing/wait/census observations, not just passed booleans.
- Normal-parent-exit must confirm exit code zero. Readiness must match the expected role. The unrelated control must be alive both before and after target termination.

The writer was asked for regression tests before fixes. Final review/test evidence will supersede these observations only after verifying the concrete corrections.
