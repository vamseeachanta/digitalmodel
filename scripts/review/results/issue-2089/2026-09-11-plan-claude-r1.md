# Actual Claude plan review r1

Requested model sonnet; inline tools-disabled review. Exit 0. Plan SHA256 ddccba2ffc1d53ba049e3d0e09e0d87d471c12c09758fe13d6de21aaf356c6fb

# Review verdict: MAJOR (not approved)

Verified independently: the box-hydrostatics oracle algebra (§"Neutral case...") is correct — I re-derived V, A, KB, BM_T=B²/(12T), BM_L=L²/(12T) from first principles and the numeric rows (200/400/600 t; BMT 25/3,25/6,25/9; BML 100/3,50/3,100/9) match. The cited SHA-256 (64 hex chars) and git baseline SHA (40 hex chars) are both structurally valid — not fabricated-length placeholders. That's real strength, not a defect. The gaps below are concrete and fixable, not nitpicks about missing live-host context (which is correctly and explicitly deferred throughout).

## 1. Staged authorization is asserted narratively, not architecturally (§Milestone 1, §Pseudocode)
`run_approved_capture(packet, local_execution_authority)` names an authorization parameter but the plan never defines the control that binds it: how is a specific reviewed packet hash tied to what actually executes? Nothing prevents "approve packet A conceptually, execute packet B with the same shape but different args." "User authorization for that packet will precede any solver launch" is a policy statement, not a mechanism (e.g., signed/expiring packet hash checked at launch, single-use token). Without this, the fail-closed claims in the runtime-contract section have no enforcement point.

## 2. Process termination and license-lock cleanup are under-specified for what is a legacy DOS/Windows executable (§Invocation, diagnostics and fail-closed runtime contract)
The `/R:`, `/L`, `/S`, `/D:`, `/T:` flag syntax and "Main Program 10-4" naming indicate a legacy DOS-lineage Windows tool. These are notorious for spawning helper processes, holding license-server leases, and leaving lock/temp files behind on a hard kill. The plan says "failure to establish bounded termination will block live approval" but gives no test methodology for *proving* the process tree is fully reaped, and never addresses whether a timeout-kill leaves the license seat/lease in a bad state for the next run. This is a real safety gap given a 120s wall-timeout policy that assumes clean termination is achievable.

## 3. Manual-derived command semantics aren't uniformly gated behind verification (§Documents and source routing table)
The plan correctly refuses to trust `/L` alone for suppressing the `OPEN-ext.RF` startup handler and requires live "disposition" before proceeding — good instinct. But it does *not* apply the same skepticism to the other manual citations used to build the fixed command set: `END` (p.161) for termination, `HS` argument semantics (pp.229–230), `REPORT OFF` (pp.357–358). These are treated as settled facts for constructing Milestone 1's "fixed reviewed commands," yet no reviewer/step is named to confirm them against the actually-installed version before Milestone 2 relies on them live. Extend the handler-disposition pattern to all command tokens, or explicitly flag this as a Milestone 2 blocking verification step.

## 4. Format qualification from a single capture is methodologically weak (§Milestone 2)
"A format-qualification addendum will then identify the exact supported version, encoding, headers, columns, units, pagination, number formatting, warning/error/completion markers and source-line mapping" — from one authorized capture, plus *synthetic derivatives* of that same capture (malformed/truncated variants). A single real sample cannot establish pagination behavior, warning-marker variability, or number-formatting edge cases; the "malformed derivatives" are hand-crafted mutations of one golden, not independent evidence. The plan should either require a second independent capture (e.g., different draft, forcing a different report layout) before qualification is considered complete, or explicitly scope down the qualification claim to "consistent with one observed capture" rather than implying general format coverage.

## 5. No sanitization step for captured artifacts before they become public fixtures (§Artifact Map, §Acceptance Criteria)
Native GHS report output typically includes a header block with license-holder/company name and possibly install metadata. The plan asserts the outcome ("Public artifacts will contain only neutral original code, permitted synthetic/native fixtures... private execution evidence will remain external") but never describes the control that gets there — no redaction/scrub step, no named reviewer for that scrub, before Milestone 2 capture bytes are promoted into the repo as "a legally permitted neutral golden." Given this is explicitly a data-residency-sensitive integration, that gap needs a concrete step, not an adjective ("legally permitted").

## 6. Two smaller spec gaps
- **Frame convention** (§Neutral case): "transverse sign explicitly fixed" is stated but no sign is actually given (port vs. starboard positive). Harmless for this symmetric canary (TCB/TCF=0) but leaves the spec incomplete for anyone extending it.
- **KM vs BM ambiguity** (§Manual table row "HS Rev E" vs. §Artifact Map `comparison.py`): the manual note clarifies draft-origin semantics but not whether GHS's default HS output reports BM directly or KM (=KB+BM). `comparison.py` is scheduled to be built in Milestone 1 with synthetic doubles, before Milestone 2's format qualification resolves this — the plan should flag that comparison logic may need rework once real columns are known, rather than implying it's final.

★ Insight ─────────────────────────────────────
The plan's `BM_T = B²/(12T)` and `BM_L = L²/(12T)` come from treating the waterplane as a rectangle and taking its second moment about the *axis it rotates around*, not the axis it's measured along — transverse (rolling) stability uses the moment about the longitudinal centerline (`L·B³/12`), longitudinal (pitching) stability uses the moment about the transverse centerline (`B·L³/12`). It's a common sign-of-confusion spot in stability code; this plan got it right, which is a useful ground-truth check for any future oracle work in this repo.
─────────────────────────────────────────────────

**Distinguishing absent context from defects:** T2 complexity labeling, "applicable completeness/cleanup gates," and the workspace issue-plan template reference can't be evaluated without org-specific rubrics/tooling I don't have — I'm not flagging those. Everything above is a specific, actionable gap in the plan text itself, independent of the (correctly) unresolved live host/license.

**Recommendation:** address items 1–5 with concrete mechanisms (not restated intent) before this goes back for approval; item 6 is minor polish.

## stderr
