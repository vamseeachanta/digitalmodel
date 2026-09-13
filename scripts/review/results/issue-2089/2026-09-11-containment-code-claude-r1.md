Actual Claude exit: 0
Reviewed hashes:
{
  "src/digitalmodel/solvers/ghs/_windows_job.py": "3254a9ade060ae2b05ef84482fcb041c0fe3ddfc200d46a4ee5ddc69e3fb725d",
  "src/digitalmodel/solvers/ghs/_owned_process.py": "4ec142b0bd4818269a04e323f54ab4063383acdea4787dcb999bc17e7a0c4e81",
  "src/digitalmodel/solvers/ghs/_sentinel.py": "749f28339d1f0a6f875c2efd13c016a1f28f6f4e6dbbc26070ce127e19d7b288",
  "src/digitalmodel/solvers/ghs/_qualification_state.py": "4e942b5995ad9175e5c8789d36465b8bcc6d7e5567279174b475ae63bde320be",
  "src/digitalmodel/solvers/ghs/_qualification_scenarios.py": "ea2d0ed7e906a6c0f49bbb9b9902aea8cc8c555977291dc5d4207e00ee0318e1",
  "src/digitalmodel/solvers/ghs/qualification.py": "ac165740ca76820e35a43db904502c8132e49aff7d5211223d3f012506a88267",
  "tests/solvers/ghs/test_windows_launch.py": "45ac3e5eb319631686cc2b7fe9402fff1151f7362a60f45f067cf5fd1e9a9d80",
  "tests/solvers/ghs/test_qualification.py": "f590c56b7af695c8f6721a73e9c68f8b2f6eedaf9f85567d91e572d233030080",
  "tests/solvers/ghs/test_windows_job.py": "c18747bd8babdc0767f0f82583fc2f3b61d8e3ac8b58e5bb78758ed47f7fb09b",
  "tests/solvers/ghs/test_windows_qualification.py": "6eb4c218200be8046a9b94310217264818e37fc45a7d7703e9a109930ad9cfce"
}

**Verdict: MAJOR**

This is a review of the plan/code as pasted — no tool use performed, per instructions.

## Confirmed defects (block approval)

**1. `crash()` has a `NameError` that makes scenario 4 unrunnable — `_qualification_scenarios.py`**
```python
if not before:
    (root/'resume_ack').touch(exist_ok=False)
    parent_record=ready(root/'parent.json',item.role)   # `item` is never defined in crash()
```
`crash()` never binds a name `item`; only `controller` and `target` exist in scope. `controller_after_readiness` will always raise `NameError` and get caught as an `ObservationFailure('NameError', …)`. It fails safe (no false pass), but the plan's acceptance criterion "every required native scenario will have identity-grounded observations" cannot be met — scenario 4 is dead code, not merely untested. This needs a real fix (should almost certainly be `controller.role`), not just a caught exception.

**2. The "closed profile" is decorative, not enforced — `qualification.py` / `_sentinel.py` / `_qualification_scenarios.py`**
`validate_profile()` only checks the caller passed back the exact literals `{30,5,10,2,5}`. Those numbers are never threaded to the child processes or to the timing logic — `_sentinel.py`'s `watchdog()` hardcodes `time.sleep(30)`, `await_file(..., 25)`/`await_file(..., 5)` are separately hardcoded, and `_qualification_scenarios.ready()` hardcodes `5` again, and `check_timing` hardcodes `10`/`30`/`7`. Nothing is derived from `PROFILE` at runtime. This means the "profile" is a no-op assertion, and if anyone edits `PROFILE` without touching four other files by hand, the validated value and the actual enforced timing silently diverge — exactly the kind of drift the interlock/timing design claims to prevent. This undercuts the "stable interlock" and natural-exit-false-positive guarantees the plan makes.

**3. Interlock fail-open gap on trivial input errors — `qualification.py`**
```python
attempt=storage.Attempt();attempt.reserve()
root.mkdir(parents=False)
```
`reserve()` (which persists the durable `active.json` marker) runs *before* `root.mkdir()`, and both statements sit outside the `try/except` block that later handles `ObservationFailure`/`OSError`/`ValueError`. If `output_root`'s parent doesn't exist (`FileNotFoundError`, a plain user typo, no native call has happened), the exception propagates uncaught: no `private-observations.json` is ever written, `attempt.finish()` is never reached, and the persistent marker is stuck at stage `reserved` forever. Per the plan's own recovery text, "manual recovery will independently establish the complete synthetic identity set" — but there is nothing to establish, since nothing was ever launched, and no evidence file exists to tell an operator that. This turns an ordinary usage mistake into a manual-recovery incident with zero diagnostic trail, which is a materially worse failure mode than the "ambiguous cleanup / observer crash" cases the design explicitly anticipates and journals. Fix: validate/create `root` before reserving the attempt, or wrap reserve+mkdir in the same exception-safe path that always persists a private-observations record.

## Minor / worth fixing before sign-off

- **Breakaway detection is coupled to a single magic error code.** `parent()`/scenario 5 treat only `winerror==5` (ERROR_ACCESS_DENIED) as proof that breakaway was blocked. If real hardware/OS combination surfaces a different code for this condition, scenario 5 will fail-closed (safe) but spuriously — brittle, should at least assert-and-log the actual code for triage rather than a bare mismatch.
- **Primary thread handle lifetime.** `Owned` keeps `self.thread` open for the full scenario duration instead of closing it immediately after a successful `ResumeThread` (standard hygiene once resume succeeds); not a leak (it's closed in `cleanup()`), just needlessly wide handle lifetime.
- **`Attributes.__enter__`'s `os.name=='nt'` branch for `get_last_error()`** only makes sense as test-mocking scaffolding — correctness on real Windows relies on `kernel_api()` gating earlier, but `Attributes` itself doesn't assert `os.name=='nt'`, so this is a latent trap if `Attributes` is ever exercised outside that guarantee.
- **`Attempt.stage()` doesn't enforce monotonic progression** — any accidental out-of-order call could regress the recorded stage, misleading a human doing manual recovery about how far an attempt actually got.
- **`public_summary()` doesn't "substitute role labels," it drops per-scenario detail entirely** (only `name`/`passed` survive). That's actually more conservative than the plan text ("substitute role labels for PIDs/accounts/paths") implies, so it's not a leak risk, but the plan's own wording overstates what's retained — worth aligning docs to what's implemented (allowlist reconstruction, not field-level redaction).
- **`census()`'s fixed 16-PID buffer** fails closed via `checked()`/count-mismatch if exceeded, which is fine for the current fixed 8-scenario/small-N harness, but flag as a scaling ceiling if scenario census requirements ever grow.

## What looked solid
Job-list ABI usage (`PROC_THREAD_ATTRIBUTE_JOB_LIST`, `JobObjectExtendedLimitInformation=9`, `JobObjectBasicProcessIdList=3`), assign-before-resume via `STARTUPINFOEX`+`CREATE_SUSPENDED`, post-create membership verification, the second-handle negative control, and the controller-kill-before-resume handle-duplication/access-mask logic (`PROCESS_DUP_HANDLE` correctly included only for the controller-kill path) are all correctly reasoned given Windows job-object semantics, contingent on real-machine verification.

`★ Insight ─────────────────────────────────────`
Two of these three MAJOR findings share a pattern worth naming: code that fails *safe* (raises rather than falsely reporting success) can still be a MAJOR defect if the failure destroys the evidence trail or permanently blocks recovery — "fail-closed" and "fail-diagnosable" are different properties, and a containment system needs both. The `crash()` NameError and the reserve-before-mkdir ordering are exactly this: neither one lets a process escape uncontained, but both leave an operator with no record of what happened.
`─────────────────────────────────────────────────`
Ignoring 19 permissions.allow entries from .claude/settings.json: this workspace has not been trusted. Run Claude Code interactively here once and accept the trust dialog, or set projects["/mnt/ace/ws/digitalmodel"].hasTrustDialogAccepted: true in /home/vamsee/.claude.json.
