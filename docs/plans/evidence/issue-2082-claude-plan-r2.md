# Issue 2082 — Claude Haiku adversarial plan review r2

Plan SHA-256: da142675d2753c781de39ec89c3b6f2a23e818bad1635bec46579ecd1afc7fc1
Model: haiku. Existing CLI authentication; no billing/settings change.
CLI version: 2.1.267. Deadline: 180 seconds. Tools disabled.
Exit: 0
Elapsed seconds: 140.57
Timed out: False
Private temporary directory removed: True

## Actual stdout (private host and scratch path redacted)

# ADVERSARIAL PLAN REVIEW — Issue 2082 r2 (amended)

**VERDICT: MAJOR** — One load-bearing gap remains; prior blockers adequately resolved.

---

## Prior Blockers Disposition

Items 1–12 from Haiku r1 are **all addressed** in the amended plan:

1. ✅ **Simulation reload proof** (§Additive report contract): exact field names now specified (`saved_sim_reloaded`, `reloaded_dynamic_samples`, `reloaded_dynamic_complete`, etc.); regression outcomes explicit.
2. ✅ **threadCount parameter** (§Design): Official Model API signature cited: `Model(filename=None, threadCount=None, handle=None)`. Three observations named: `solve`, `data_reader`, `simulation_reader`.
3. ✅ **LoadSimulation API** (§Design): Call pattern specified: `reader = OrcFxAPI.Model(threadCount=1)` → `reader.LoadSimulation(sim)` → `reader["SmokeTestLine"]`.
4. ✅ **Fake-API coverage gaps** (§Artifact map + TDD sequence): Test file allocated (`test_orcaflex_probe_contract.py`); TDD gate ensures scenarios are written before code.
5. ✅ **Proof field names** (§Additive report contract): All new fields named, typed, and conditional on observation (failures before construction don't fabricate counts).
6. ✅ **Regression claim clarity** (§Additive report contract): "On failure, proof fields will appear only after observation; import/constructor failure will never fabricate an observed thread count."
7. ✅ **Finiteness validation** (§Bounded design): Exact function specified: `math.isfinite(float(value))`, no magnitude threshold.
8. ✅ **require_all=false behavior** (§Bounded design): Both paths documented; no behavior change, just preservation of existing branching.
9. ✅ **Hostname opt-in** (§Bounded design): Parameter `run_probes(include_host=False)` specified; default False.
10. ✅ **Exception handling / cleanup** (§Proposed flow): Explicit `finally` block pattern: "Model and line references...initialized before guarded body and cleared in `finally`, with line references cleared first."
11. ✅ **Test discovery verification** (§Artifact map): Command specified: `pytest --collect-only -q tests/solvers/smoke` + contrast against legacy tests.
12. ✅ **Native acceptance harness owner** (§Native acceptance): "Implementing agent/operator will explicitly run `docs/plans/evidence/issue-2082-native.ps1`...it will invoke `scripts/solver_smoke_test.py --solver orcaflex`..."

---

## Critical New Issue: PowerShell Harness Platform Assumption

**§Native acceptance**: Plan specifies `.ps1` (Windows PowerShell) harness without explicitly stating platform scope.

```
docs/plans/evidence/issue-2082-native.ps1   ← PowerShell-only syntax
scripts/solver_smoke_test.py --solver orcaflex  ← Python, cross-platform
```

**The gap**:
- Fake-API tests (§TDD sequence) are Python and run on any platform (CI/Linux/Windows).
- Workflow code is Python; can run anywhere OrcaFlex bindings exist.
- Native acceptance harness is hardcoded `.ps1`, which assumes Windows + PowerShell 5.0+.

**Current context risk**:
- Current working directory: `[private-review-scratch]` (Linux).
- Memory indicates active execution hosts: gpu-claw (Linux), ace-linux-1 (Linux), ace-linux-2 (Linux).
- Retired: ace-win-1 (Windows, per `reference_ace_win_1_retired_use_[private-host-redacted].md`).
- OrcaFlex licensing suggests a specific Windows host ([private-host-redacted] per memory).

**If multi-platform implementation is in scope**, the `.ps1`-only harness is a blocker. If OrcaFlex is Windows-licensed-host-only, the platform assumption must be explicitly stated in the plan.

**No explicit statement exists** in the plan. The plan §Objective says "bounded prerequisite," §Design says "locally verified OrcaFlex 11.6c environment," but does NOT state "Windows-only" or "requires PowerShell host."

**Fix required** (pick one):

1. **Clarify Windows-only scope**: Add to §Native acceptance: *"This harness requires a Windows host with PowerShell 5.0+ and OrcaFlex 11.6c. Linux environments will complete offline (fake-API + code review) and defer native acceptance to Windows licensed host."*

2. **Provide cross-platform harness**: Supply both `issue-2082-native.ps1` and `issue-2082-native.sh` (bash) variants, OR a Python wrapper that detects platform and delegates appropriately.

3. **Defer native acceptance**: Move the `.ps1` harness to a follow-on issue; keep this plan to fake-API + code review only. Update §Approval/rollback to reflect offline-only verification.

**Severity: MAJOR** — The plan currently specifies a mandatory post-review step (`implementing agent/operator will explicitly run...`) that is not executable on Linux, and the working directory is Linux. This blocks implementability on the current platform.

---

## Secondary Observations (Low Risk, Code-Review Scope)

### A. Simulation Reader Thread Count Read-Point
**§Proposed flow**: When `reader.LoadSimulation(sim)` completes, how is the observed `simulation_reader` thread count obtained? Is it:
- `reader.threadCount` (post-load property check)?
- Hardcoded as 1 (since we passed it)?
- Extracted from .sim file metadata?

Plan doesn't specify *when* this is read. **Disposition**: Implementation detail; code review will verify. Not a blocker.

### B. Stopped-But-Incomplete State Regression Test
**§Regression**: Lists "Stopped but incomplete; running or paused model → ok=false" as a required failure mode.

How does the fake-API test trigger a `simulationComplete=False` but `stopped` state? The plan doesn't explain the mock scenario. **Disposition**: TDD will invent the scenario; not a blocker. If OrcaFlex doesn't have this state, test will be adapted or skipped with documentation.

### C. JSON Serialization Test Oracle
**§Proposed flow**: "Strict JSON serialization will be a test oracle for the OrcaFlex-only report."

Does this mean calling `json.dumps(report)` without `default=str` and asserting no exception? Plan doesn't spell out the test code. **Disposition**: Straightforward to implement; code review will verify.

---

## Remaining Strengths

- **Bounded, fail-closed design**: Clear regression table, exact validation thresholds, no invented tolerances.
- **TDD gate**: Fake-API tests written and run before probe repair.
- **Preserves existing contracts**: AQWA behavior, workflow require_all branching, scratch cleanup.
- **Testable without license**: Fake-API + conftest monkeypatch = CI-runnable.
- **Cleanup strategy explicit**: finally-block pattern, no undocumented close() assumptions.

---

## Recommendation

**Do not approve** pending resolution of the PowerShell platform issue (§Critical New Issue).

**After clarification**, re-submit for final sign-off. The remaining gaps (§Secondary Observations A–C) are code-review scope and do not block approval.

---

**DISPOSITION OF MAJOR**:
- If owner/implementer confirms **Windows-only scope** for native acceptance, APPROVE.
- If **cross-platform support** is intended, request `.sh` harness or Python wrapper.
- If **native acceptance is deferred**, move `.ps1` to follow-on issue and re-approve this plan as offline-only.


## Actual stderr

(empty)
