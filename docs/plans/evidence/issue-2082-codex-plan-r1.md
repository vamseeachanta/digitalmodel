# Codex adversarial plan review — issue 2082

- Issue: [digitalmodel 2082](https://github.com/vamseeachanta/digitalmodel/issues/2082)
- Reviewed: 2026-09-10
- Artifact: [smoke plan](../2026-09-10-issue-2082-orcaflex-smoke.html)
- Plan SHA256: `88081ac454928c708718ae8333416eb919265fb75ea570ed7be6ce186a7ea10e`
- Verdict: **APPROVE, advisory plan review only.** No blocking defect established in the bounded proposed design. This is neither owner approval nor deployment approval, and does not represent another provider's review.

## Defect assessment

No MAJOR or MINOR plan defect established. Implementation and native acceptance remain pending; none of the proposed controls has been verified as implemented by this review.

## Checks performed

1. Read the complete plan, reproduction harness and recorded JSON, and compared the actual probe, engine workflow and CLI. A scoped `git diff --quiet` confirmed those three implementation files match revision `80da6e09b649707ef7bdb09c9d112f1650bf3d6a`.
2. Independently reran `issue-2082-reproduce.py` through `runpy` with the existing interpreter and bytecode writes disabled. All eight results exactly matched the recorded JSON: seven invalid scenarios returned success in the current probe. The temporary scratch directory was removed. The injected fake API acquired no native licence.
3. Checked **Bounded design** against those failures: finite scalar/history checks, completion plus stopped state, explicit one-thread construction and explicit saved-simulation loading address the demonstrated gaps. The official [OrcFxAPI Model reference](https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference,Model.htm) documents constructor `threadCount`, its default core-based selection, `simulationComplete`, and `LoadSimulation`; constructor filename loading can fall back from simulation to data. Explicit loading is therefore material to the proposed proof.
4. Checked **Artifact map and TDD sequence** against actual caller behavior. The workflow currently writes a report before its `require_all` failure; its explicit diagnostic opt-out and scratch-retention option are preserved. The proposed tests cover those branches, strict OrcaFlex JSON, mixed dispatch and hostname opt-in without claiming an AQWA repair.
5. Checked **Native acceptance and operational boundary** and **Approval, review and rollback** for scope escalation. The 90-second deadline belongs to a future manual subprocess harness; the in-process API receives no timeout guarantee. One-thread validation is probe-local. Queue activation, task cutover, host arbitration, neutral policy scope and Linux-origin qualification remain separate. Owner approval precedes implementation.

## Limits that code-stage review must retain

- The planning harness proves baseline false positives only. It does not implement `LoadSimulation`, completion properties or genuine persisted readback; its revision guard also intentionally prevents treating it as the repaired probe's acceptance suite. The separately proposed fake contract tests must supply that behavior.
- Sample-count agreement and finite readback do not establish value equality, convergence or engineering validity. The plan explicitly declines those claims.
- Native thread observations, reference release on exception paths, timeout process-tree ownership and retained failure diagnostics require implementation-stage evidence. This review ran no native solve, timeout exercise or deployment action.

## Write scope and residue

Only this review file was added by this reviewer. The plan, plan index, reproduction files and parallel Claude review are expected shared-task artifacts and were left untouched. Reproduction scratch removal was verified; no commits or external comments were made.
