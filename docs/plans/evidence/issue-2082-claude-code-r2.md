# Issue2082 — Claude Haiku code review r2

Model: haiku. Effort: low (CLI help verified). Existing auth; empty setting sources; no session persistence; tools disabled; packet-only system prompt.
Deadline: 180s. Exit: 0. Elapsed: 45.74s. Timed out: False.
Private temporary directory removed: True.
Raw stdout SHA256: aaf259ad5388831906fdfdb53c7b0f4ccf6f910aad767613bc81751c4aecd5ab
Only two current source files, compact contract, 61-test/native PASS summary supplied. No further retry.
Private context redacted if present; otherwise exact output.

## Reviewed file hashes

{
  "docs/plans/evidence/issue-2082-native.ps1": "864c05c561d6782022e61678748d1f2b7a6aa4510d39aac30e2ecaf0f1638946",
  "src/digitalmodel/solvers/smoke/probes.py": "074c57c7608db8162536a5dadb519c3cf9faf3308e25121474b78623006a0856"
}

## Actual stdout

# Adversarial Review: Solver Smoke Test Harness

**Files:** `probes.py` (Python probe module) + `issue-2082-native.ps1` (Windows PowerShell harness)

---

## Contract Compliance vs. Defects

### Probes.py: COMPLIANT

✓ **Thread enforcement**: threadCount=1 enforced via `_thread_proof()` across solve, data_reader, simulation_reader
✓ **Finite proof**: `_finite()` validates all static/dynamic tensions; history requires ≥2 samples
✓ **Completion**: `_completed()` verifies `simulationComplete=True` + `state==SimulationStopped`
✓ **Save/reload cycle**: `SaveSimulation()` → file-size check → `LoadSimulation()` → `SmokeTestLine` retrieval → history revalidation
✓ **Sample count matching**: reloaded_dynamic_samples == dynamic_samples enforced
✓ **Reference cleanup**: Finally blocks null all pointers (line, model, reader)

**No defects found.**

---

### PowerShell Harness: MAJOR ISSUES

#### 🔴 MAJOR: Thread Handle Leak in `run_owned()`
**Location**: Worker code, line ~135 (ResumeThread) / finally block
**Issue**: Thread handle obtained from `CreateProcess` is only closed after successful WaitForSingleObject. If any exception occurs between ResumeThread and thread close, the handle leaks.

```python
# DEFECT: thread closed only after full success path
if kernel.ResumeThread(thread) == 0xffffffff:
    raise c.WinError(...)  # thread handle never closed here
# ... later code can also raise before finally cleanup
finally:
    if thread is not None:
        _winapi.CloseHandle(thread)  # only reached if no exception
```

**Fix**: Separate try for ResumeThread; close thread before any raise:
```python
try:
    if kernel.ResumeThread(thread) == 0xffffffff:
        raise c.WinError(...)
finally:
    if thread is not None:
        _winapi.CloseHandle(thread)
```

---

#### 🔴 MAJOR: Process Termination Not Verified
**Location**: Worker finally block, line ~155
**Issue**: After `WaitForSingleObject(process, 5000)`, the code closes the handle regardless of whether the process actually terminated.

```python
if process is not None and _winapi.WaitForSingleObject(process, 0) == 258:
    _winapi.TerminateProcess(process, 1)
_winapi.WaitForSingleObject(process, 5000)  # ignores result (258 = timeout)
_winapi.CloseHandle(process)  # proceeds even if process didn't stop
```

**Fix**: Verify termination before close:
```python
wait_result = _winapi.WaitForSingleObject(process, 5000)
if wait_result == 258:  # timeout
    raise RuntimeError('process did not terminate within 5s')
_winapi.CloseHandle(process)
```

---

#### 🟡 MINOR: Missing Value in Error Message
**Location**: Worker code, `reject_constant()` function
**Issue**: Error message does not include the offending JSON constant.

```python
def reject_constant(value):
    raise ValueError('nonfinite JSON constant')  # value not logged
```

**Fix**: Include the value for diagnostics:
```python
def reject_constant(value):
    raise ValueError(f'nonfinite JSON constant: {value}')
```

---

### Verdict Summary

| Component | Status | Rationale |
|-----------|--------|-----------|
| **probes.py** | **APPROVE** | All contract clauses met: threadCount=1, finite static/dynamic ≥2, completion, save/reload/compare, cleanup |
| **issue-2082-native.ps1** | **MAJOR** | Two resource leaks (thread handle, unverified process termination) compromise cleanup guarantees in bounded-timeout subprocess contract |

---

## **FINAL VERDICT: MAJOR**

The Python probe module is solid. The PowerShell harness has **two blocking resource-management defects** that violate the "bounded timeout/tree cleanup" contract:
1. Thread handle leak on ResumeThread error
2. Process forced-close without termination confirmation

Fix both before merge. Once fixed, recommend APPROVE.


## Actual stderr
