Plan SHA256: 95fb2c92e362d17413862a71882a6aa17554d7f121dbffbccd65750e057dca6d
Actual Claude exit: 0

# Verdict: MAJOR

I reviewed this as a design/spec document only — no code was run or written. Findings are grouped by the areas requested.

## Windows job-handle ownership & atomic process assignment

1. **No stated technical control preventing GHS launch via the private `_owned_process.py` module (MAJOR).** The plan restricts the *public* entry point (`qualification.py`) to fixed sentinel roles with no caller-supplied executable. But `_owned_process.py` is described only as generic "private process/job lifetime" logic. Python's leading-underscore convention is not an enforcement mechanism — nothing in the plan states that this module itself validates/allowlists the executable path it's given. As written, a future PR could import `_owned_process` directly and pass a GHS executable, satisfying every constraint in this plan while defeating its stated purpose ("Qualification must never enable GHS"). The allowlist/hash-check of permitted executables needs to live at the lowest-level primitive, not rely on caller discipline in `qualification.py`.

2. **Scenario 3 wording is ambiguous about what "controller death" mechanically means (MAJOR, since this is the exact orphan-window bug the plan exists to close).** "a separate observer will terminate only the controller's retained handle" could mean (a) `TerminateProcess` on a duplicated handle to the controller, or (b) merely `CloseHandle` on the observer's handle to the controller. Only (a) actually simulates a crash; (b) does nothing (the controller keeps running, keeps holding the non-inheritable job handle, and nothing gets killed). Given this scenario is the direct test of the devblog-cited orphan window, the plan should say explicitly: "the observer calls TerminateProcess via its retained handle to the controller, which triggers OS-driven closure of the controller's job handle, whose refcount reaching zero triggers KILL_ON_JOB_CLOSE."

3. **The job-handle exclusivity chain is never stated explicitly, only implied.** The correctness of scenarios 3/4 depends entirely on the job handle being held by exactly one process (the controller) with no duplication/inheritance anywhere else, so that killing the controller is *sufficient* to trigger `KILL_ON_JOB_CLOSE`. The plan states the job object is non-inheritable, which is consistent with this, but never states the invariant "exactly one live handle to the job object exists at any time, owned by the controller" as a checked property. Recommend adding it as an explicit invariant and a test that asserts handle count.

4. **Thread handle lifecycle for `CreateProcessW`/`ResumeThread` is unmentioned.** "Finally paths will release handles and attribute buffers" lists process handles and attribute buffers but not the thread handle returned by `CreateProcessW`. This is a minor resource leak risk, but leaked kernel handles in a qualification harness that's supposed to prove clean lifetime management is a credibility gap worth closing explicitly.

## PID identity

5. Creation-time FILETIME + job-membership verification before resume is the right defense against PID reuse — no defect found here. One gap: the plan never states what access rights the observer requests when it "opens and retains" handles (e.g., needs at least `PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE`). Worth pinning down so the implementer doesn't over-request rights (`PROCESS_ALL_ACCESS`) out of convenience, which would be an unnecessary privilege footprint for a "harmless" harness.

## Crash / readiness validity

6. **No scenario tests a genuine child crash, only controller crash.** The six scenarios cover controller death (pre- and post-resume) and parent/timeout/breakaway/unrelated-survival, but none dedicated to "the sentinel *child* itself terminates abnormally mid-run." "Unexpected early exit" is listed only as a generic failure condition that *fails qualification*, not as a scenario that verifies the harness correctly distinguishes an abnormal child exit from a normal one and reports it accurately rather than mis-attributing it. Given "crash/readiness validity" was explicitly flagged for review, this is a real gap — add a scenario where the child self-terminates with a non-zero/abnormal code and assert the harness classifies it correctly (not as a false pass).

7. **Readiness record write is not specified as atomic.** "A bounded readiness record will identify the child" via a fixed filename in an exclusive temp directory — but nothing states the record is written atomically (write-to-temp-then-rename) versus written in place, which risks the observer reading a partially-written record and either false-failing or, worse, misreading stale/garbage identity data. Should be pinned down given identity correctness is the crux of the whole exercise.

## Persistent interlock — behavior and scope

8. **Marker scope (global vs. per-`output_root`) is unspecified.** "the harness will exclusively create a persistent attempt marker outside disposable work" doesn't say whether this is keyed globally (one marker machine-wide) or per invocation/output_root. If global, a single crashed attempt on *any* output root blocks *all* future qualification attempts until manual clearing — this may be intentional (conservative, matches "never silently remove or age out the marker") but needs to be an explicit, reviewed decision, not an implicit consequence of underspecification. If it's meant to be scoped per output_root instead, that changes the blast radius significantly and should be stated.
9. **No stated behavior for concurrent legitimate attempts** (e.g., two developers, or a CI matrix, running qualification at the same time on the same or different machines). "Exclusively create" implies atomic creation prevents a second concurrent attempt from starting — good — but the plan should confirm this is desired (single-flight globally) vs. merely accidental serialization.
10. Good: no auto-expiry, no silent removal, manual disposition only, "Confirmed cleanup will atomically finalize" — these correctly prevent fail-open retry after ambiguous state. No defect found in the *fail-closed* direction itself, only in scope documentation (findings 8–9).

## Qualification must never enable GHS — additional check

11. **No explicit test case for "reject attempt to override sentinel role/executable."** The TDD list's "Unknown fields, boolean/nonfinite/negative/oversized timeouts | Reject before marker or process creation" row covers profile/timeout validation but does not explicitly test that supplying an executable, script path, or role override is rejected at the public boundary. Given this is the single most safety-critical property of the whole slice, it should be its own named test row, not implied by the "unknown fields" catch-all.
12. Good: `sentinel_containment_passed` / `licensed_execution_verified=false` / `ghs_launch_allowed=false`, non-reuse of `containment_qualified`, and preservation of `run_approved_capture`'s hard stop are all correctly designed to prevent this slice from being (mis)read as a GHS authorization. No defect found in that part of the design — it's the implementation-boundary enforcement (findings 1, 11) that's missing, not the evidence-flag design.

---

`★ Insight ─────────────────────────────────────`
The most interesting failure mode this plan risks isn't a Windows API bug — it's a **governance/enforcement-boundary gap**: restricting "what callers may pass" only at the public API (`qualification.py`) while leaving the private primitive (`_owned_process.py`) generically capable is a classic way a "safe by convention" module quietly becomes a general-purpose launcher a few PRs later. Python has no real access modifiers, so "private" module naming is documentation, not a control — the actual allowlist/hash check needs to be enforced at the lowest layer that touches `CreateProcessW`.
`─────────────────────────────────────────────────`

Given findings 1, 2, and 6 touch the core safety and correctness claims (GHS non-enablement enforcement, the exact orphan-window scenario this plan is built to prove, and crash/readiness completeness), this needs another revision pass before implementation approval.
Ignoring 19 permissions.allow entries from .claude/settings.json: this workspace has not been trusted. Run Claude Code interactively here once and accept the trust dialog, or set projects["/mnt/ace/ws/digitalmodel"].hasTrustDialogAccepted: true in /home/vamsee/.claude.json.
