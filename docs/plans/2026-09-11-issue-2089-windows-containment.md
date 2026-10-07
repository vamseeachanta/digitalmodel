# GHS next slice: Windows owned-process qualification

> Status: plan-review; revised addendum implementation will await explicit approval.
> Issue: https://github.com/vamseeachanta/digitalmodel/issues/2089
> Complexity: T2. Client: N/A. Date: 2026-09-11. Lane: lane:codex.
> Execution: parallel-readonly review; one implementation writer.
> Baseline: 6c89b4e30c9c224cccaef6cba4cc57885ba11153.

## Deliverable and authority

This slice will provide a local Windows process helper and harmless parent/child qualification harness. Qualification will cover only the exact helper, sentinels, interpreter, OS and account context observed. Approval will authorize TDD implementation, bounded Python sentinel execution on the current Windows machine, validation and PR delivery. It will not authorize GHS, Part Maker or another solver. The existing run_approved_capture hard stop and synthetic evidence labels will remain unchanged. Trusted receipt/DACL storage, approval nonces, vendor diagnostics, license recovery and the parent's concrete capture/format approval gates will remain separate.

## Resource Intelligence Summary

These verified sources will constrain implementation:

- Parent docs/plans/2026-09-11-issue-2089-ghs-canary.md will supply owned-tree timeout, creation-time identity and unrelated-process survival requirements.
- src/digitalmodel/solvers/ghs/runner.py will retain its unconditional live refusal and synthetic-only process seam.
- docs/plans/evidence/issue-2082-native.ps1 will supply generic ctypes/lifetime prior art: kernel_api, job_object, run_owned and cleanup_owned at lines 59,73,84,111. Its OrcaFlex entry point will not be imported or executed.
- tests/solvers/smoke/test_native_cleanup_contract.py will supply two mocked cleanup-failure cases; real descendant, crash and identity proofs will be added separately.
- tests/DOMAINS.md will route new tests through the existing solver-smoke root.
- The Microsoft references below will govern native semantics. The later GHS geometry packet will use separately qualified vendor sources.

Evidence: baseline runner.py will raise for every live call; no other open GHS PR was returned by the 2026-09-11 issue/PR inventory. The companion resource record will retain current observations. Drive-index availability/freshness limits will remain explicit. No indexed project file will become a neutral fixture. Standards-derived calculations will be out of scope. Failure reproduction will be N/A: this will add a missing qualification capability, not repair an alleged GHS runtime regression.

## Architecture and launch boundary

Private Windows primitives will use lazy ctypes DLL loading and pointer-width-correct types. Non-Windows imports/pure tests will remain supported. Unsupported APIs, architectures or nesting contexts will fail before any sentinel is resumed. No pywin32 dependency will be required.

The public entry point will accept only a fresh output root and closed finite timeout profile. It will derive the executable from the trusted running interpreter and select only fixed repository-owned sentinel roles. It will accept no caller command, shell, executable, arbitrary script, GHS profile or receipt. The lowest launch wrapper will also derive the interpreter and fixed sentinel path internally, validate the closed role enum and hashes, and accept no raw argv/executable/script override. No callable helper will provide a general launch interface; Python privacy will not be treated as enforcement. Direct-helper override tests will reject before CreateProcessW.

An unnamed non-inheritable Job Object will set KILL_ON_JOB_CLOSE and neither breakaway flag. STARTUPINFOEX with PROC_THREAD_ATTRIBUTE_JOB_LIST, EXTENDED_STARTUPINFO_PRESENT and CREATE_SUSPENDED will create the process in the job. There will be no fallback to create-then-assign: Microsoft identifies a controller-crash orphan window in that sequence. This will preserve assignment before resume. CREATE_NO_WINDOW and bInheritHandles=false will prevent visible windows and job-handle inheritance. The attribute buffer and job handle will remain alive through CreateProcessW.

Before ResumeThread, the helper will verify job membership and retain the process handle, PID and creation FILETIME. API failures, incompatible nesting, membership mismatch and resume failure will trigger owned cleanup or explicit uncertainty. All native calls will receive argtypes/restype and checked returns, including expected attribute-buffer sizing behavior. Finally paths will release the returned primary-thread handle, process/job handles and attribute buffers. Observer process handles will request QUERY_LIMITED_INFORMATION and SYNCHRONIZE; PROCESS_TERMINATE will be added only for owned cleanup. PROCESS_ALL_ACCESS will not be used.

## Real sentinel experiment

Original fixed sentinels will supply parent, child, unrelated control and crash-test controller roles. Once resumed, each will self-exit within 30 seconds, perform no network/vendor actions and accept no arbitrary executable. A never-resumed process will have no executing watchdog. The parent will launch its fixed child with no breakaway and close_fds. An atomically published bounded readiness record (exclusive temporary write, flush, then rename) will identify the child; the observer will retain each handle and verify creation time and membership while alive. Missing readiness will fail qualification, preventing vacuous absence proofs.

Each scenario will use an exclusive trusted local temporary directory and fixed filenames. Reparse checks will remain observational; hostile-writer storage containment will not be claimed. stdout/stderr will use null handles or bounded fixed records, never unbounded PIPE accumulation. Native tests will cover:

1. Normal parent exit while child lives: final job-handle closure will stop the child.
2. Timeout while parent/child live: closure will stop both under a bounded cleanup deadline.
3. Before-resume controller death will use controller-origin identity publication and an explicit observer barrier, not sentinel readiness. The observer will retain and verify the suspended process handle; ResumeThread will never be called for that process. The observer will call TerminateProcess on its retained controller handle and confirm death; OS-driven job-handle closure will terminate the suspended process.
4. Controller death after parent/child readiness will explicitly use TerminateProcess on the retained controller handle and confirm death, triggering OS-driven handle closure and termination of both contained identities.
5. Fixed child breakaway attempt: creation will fail or the child will remain a verified job member; escape will fail qualification.
6. The unrelated control will remain alive with matching retained identity through target-job cleanup; the harness will stop its own control separately afterward.
7. A deliberate second-job-handle negative control will show that closing the first handle does not terminate the sentinel; closing the retained second handle will then terminate it. This handle will be registered for finally cleanup and never retained in positive crash cases.
8. A fixed abnormal child-exit scenario will require the known nonzero code to produce the expected failed-run classification and complete cleanup. Its meta-test will pass only when that failure is correctly classified; it will not represent a successful workload.

The harness will test the current nesting context and, where possible, a deliberate compatible enclosing job. Injected tests will cover incompatible policies; unobserved policies will not become all-Windows claims. No taskkill/image-name sweep, PID-only kill or non-owned process manipulation will occur.

At each positive close/crash trigger, the controller will own the sole remaining target-job handle. Observer duplicates used for pre-trigger membership/census inspection will be closed and acknowledged before the barrier; no duplication/inheritance will occur afterward. Instrumented ownership tests and the second-handle control will verify this invariant without claiming a global system handle census. Membership/count checks will happen before closure. After last-handle closure, the observer will wait on retained process handles only, never query a closed unnamed job. Fixed sentinels will stop spawning after readiness; every expected member will be retained before triggering. Unexpected members will fail the pre-trigger census.

The closed profile will use integer seconds: watchdog=30, readiness_timeout=5, trigger_budget=10, cleanup_timeout=2 and margin=5. Monotonic time from earliest owned creation will require a trigger within 10 seconds and more than cleanup_timeout+margin of remaining watchdog life. Missing lifetime evidence or delayed scheduling will fail before triggering. An ineffective-kill test will allow only natural watchdog exits; qualification must fail at the earlier cleanup deadline. Scenario 1 will exclude its intentionally exited parent from live-at-trigger checks but require its child alive. The suspended scenario will use its separate barrier and no watchdog claim.

Termination proof will use retained-handle waits and the pre-close census. Wrong identity, WAIT_FAILED, unexpected pre-trigger count, early exit, unreadable evidence, leaked handles or incomplete readiness will fail qualification. Expected timeout may pass its experiment only after all required observations succeed; it will never imply production-run success.

## Recovery and evidence

Before starting any sentinel, the harness will exclusively create a persistent marker in one fixed per-user/per-machine namespace: resolved Windows LocalAppData plus digitalmodel/ghs-sentinel-qualification/active.json. This path will not depend on output_root, case_id or a caller override. Calls in the same account will be single-flight even across different output roots; different accounts/machines will have independent evidence, with no cross-account exclusion claim. Missing local state resolution will block execution.

After verified cleanup and durable outcome persistence, the active marker will be atomically moved without overwrite to an immutable completed-attempt record in the same state directory. This explicit completion will permit a later operator invocation; an unresolved marker will never be rotated this way. Concurrent creation will continue to use exclusive creation.

An unresolved marker will block another attempt. Confirmed cleanup will atomically finalize a bounded failed/passed outcome. Ambiguous cleanup, observer crash or failed persistence will preserve unresolved state and prohibit automatic retry. The harness will not silently remove or age out the marker. Durable stages will include reserved, creation_may_have_occurred, identities_recorded and cleanup_confirmed; the uncertainty stage will be flushed before process creation. Create-before-record crashes, empty identity sets and observer loss will remain unresolved. Missing identities will never prove that nothing launched.

This interlock will qualify synthetic attempts only; it will not act as a production nonce or license lock. Manual recovery will independently establish the complete synthetic identity set and termination. Elapsed watchdog time will not clear an incomplete registry or suspended process. If completeness cannot be established, the attempt will stay unresolved for operator investigation. No automatic clear command, vendor lock access or disk-full success fallback will exist.

Versioned evidence will bind helper/sentinel hashes, interpreter hash/version, Windows build/architecture, profile, UTC interval, enclosing-job context, scenario triggers, identity/wait observations, cleanup and limitations. Raw local identities will remain private; a public summary will substitute role labels for PIDs/accounts/paths and reference private evidence by opaque digest.

A passing result will be sentinel_containment_passed, with licensed_execution_verified=false and ghs_launch_allowed=false. It will not set the existing GHS containment_qualified flag. A changed helper, interpreter, sentinel or OS/account/job-policy context will require requalification. Saved success will not constitute an authorization credential.

## Pseudocode

```text
qualify(output_root, closed_profile):
    validate platform, fixed trusted identities and finite bounds
    exclusively persist unresolved attempt marker
    for each fixed scenario:
        create job; atomically create suspended sentinel inside it
        retain identities/membership; close observer job duplicates
        resume only scenarios that require running sentinels
        require readiness and fixed trigger; observe owned termination
        confirm unrelated survival; close handles; retain bounded evidence
    finalize only after confirmed cleanup
    otherwise preserve recovery_required; never automatically retry
```

## Artifact Map / Files to Change

| Action | Path | Purpose |
|---|---|---|
| Add | src/digitalmodel/solvers/ghs/_windows_job.py | Checked native bindings and structures |
| Add | src/digitalmodel/solvers/ghs/_owned_process.py | Private process/job lifetime |
| Add | src/digitalmodel/solvers/ghs/qualification.py | Fixed orchestration and evidence |
| Add | src/digitalmodel/solvers/ghs/_sentinel.py | Original finite sentinel roles |
| Add | tests/solvers/ghs/test_windows_job.py | Injected native failures |
| Add | tests/solvers/ghs/test_qualification.py | Evidence/interlock contracts |
| Add | tests/solvers/ghs/test_windows_qualification.py | Real Windows acceptance |
| Update | docs/domains/ghs/neutral-hydrostatics-canary.md | Invocation and limits |
| Add | scripts/review/results/issue-2089/ | Tests, observations and reviews |
| Update | docs/plans/README.md | Addendum status |

Files will remain below 400 lines and functions below 50; further splits will preserve these boundaries. Existing solver launchers and registry activation will remain outside the write set.

## TDD Test List

| Cases | Required outcome |
|---|---|
| Non-Windows import, unavailable API, unsupported architecture | No launch; truthful unqualified state |
| Unknown fields, boolean/nonfinite/negative/oversized timeouts | Reject before marker or process creation |
| Attribute sizing/init/update, limits/create/membership/resume failures | No uncontained execution; cleanup or uncertainty |
| Job-list flags, hidden launch, no shell/inherited handles/breakaway | Exact ABI configuration |
| Wrong creation time, PID reuse, failed wait, early exit | Failed observation; no identity-blind termination |
| Existing attempt, crash, failed finalization | Persistent unresolved interlock; no retry |
| Missing/oversized readiness, unexpected roles, reparse output | Bounded failure and owned cleanup |
| Eight real Windows observations | All observations required; skips will not qualify |
| Public/private executable, script or role overrides | Reject before CreateProcessW |
| Different output root after unresolved attempt; create-before-record crash | Stable interlock; persistent uncertainty |
| Ineffective kill with later natural watchdog exit | Cleanup deadline failure |
| Suspended barrier | No sentinel readiness or ResumeThread; retained identity required |
| Changed runtime/helper identity | No reuse as current evidence |
| Existing live API/synthetic comparison | Live refusal and synthetic labels preserved |

Generic Linux CI will run pure/injected tests and explicitly skip native Windows tests. No new hosted/licensed workflow will be added. Actual zero-skip Windows acceptance on the selected machine will be required separately.

## Acceptance Criteria

- New pure tests and the existing 86 GHS tests will pass.
- Every required native scenario will have identity-grounded observations; missing/blocked scenarios will prevent success.
- Confirmed cleanup will leave no owned live process, leaked handle or temporary work directory.
- Uncertain/crashed attempts will preserve a named unresolved interlock until manual disposition.
- Independent code review, legal scan and cleanup audit will precede delivery.
- The parent plan hash will remain unchanged, and issue 2089 will remain open for licensed work.

## Review, risks and decisions

Actual Claude r1 and independent Codex r1 verdicts will remain MAJOR against the original draft hash. This revised addendum will incorporate executable-boundary, controller-kill, handle-ownership, suspended-readiness, timing and interlock corrections. Main disposition will map findings to revised requirements/tests without claiming provider APPROVE or consensus. Explicit user approval will precede implementation.

Native vendor helpers, DACL-qualified receipt storage, approval nonces, diagnostics, geometry and entitlement will remain capture blockers. Host selection will affect later licensed qualification. A sentinel pass will cover observed process lifetime only, not vendor license recovery or filesystem isolation.

## Primary references

- [Microsoft Job Objects](https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects)
- [Microsoft Nested Jobs](https://learn.microsoft.com/en-us/windows/win32/procthread/nested-jobs)
- [UpdateProcThreadAttribute](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-updateprocthreadattribute)
- [Atomic process assignment and the orphan window](https://devblogs.microsoft.com/oldnewthing/20230209-00/?p=107812)
