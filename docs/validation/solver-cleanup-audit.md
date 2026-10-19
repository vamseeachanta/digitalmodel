# Local solver cleanup evidence audit

## Summary

This bounded source audit addresses issue #2095. The selected routes distinguish
termination requests, root-process exit, whole-owned-job drain and unresolved
cleanup differently. The smoke/model wrapper already contains the issue #2093
repair: it queries the retained Job handle before closing it. Other routes need
caller-specific review before their results can establish equivalent evidence.
Source inspection does not establish that any process leaked. Runtime cleanup,
licence release and solver reuse remain `NOT_EVALUATED` for every entry.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-19 | Record selected cleanup boundaries and source fingerprints so later reuse reviews cannot silently inherit stale findings. |

## Scope and method

The inventory in [solver-cleanup-inventory.json](solver-cleanup-inventory.json)
covers selected native wrappers, process supervisors and comparison routes.
Discovery used package searches for `cleanup_owned`, `JobObject`, `subprocess`,
`wait`, `killpg` and pool shutdown, followed by reading the callers and cleanup
functions. It includes local routes under `solvers`, `ansys`, `workflows` and
`hydrodynamics/aqwa`, plus the existing Windows smoke wrapper. It is not an
exhaustive repository inventory. Imported adapters, generated batch scripts,
remote execution, native library internals and other repositories are outside
this review.

The related production guard and watchdog issues identified by #2095 concern
the same evidence distinction. Their descriptions informed the questions below;
their code and production state were not audited or changed here.

The audit is offline. Inspected files are read as bytes, without importing their
modules or invoking executables. Each finding is bound to all listed source
dependencies by SHA-256. A missing, changed, unreadable, nonregular or symlinked
dependency invalidates that entry's classification and observations. Findings
must be re-reviewed before updating the fingerprints; blind rehashing is not a
review. Exact bytes are compared, so line-ending changes also invalidate a pin.
These are snapshot checks, not an atomic launch transaction or dispatch guard.

## Evidence basis

- Microsoft, [Job Objects](https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects),
  updated 2025-07-14, sections Managing Processes in Jobs, Job Termination,
  Managing Job Objects and Job Object Resource Accounting. Kill-on-last-close
  requests termination of associated processes. Process association and breakaway
  rules define the scope of that ownership; a root handle is not a membership
  census.
- Microsoft, [JOBOBJECT_BASIC_ACCOUNTING_INFORMATION](https://learn.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-jobobject_basic_accounting_information),
  page updated 2024-02-22, member `ActiveProcesses`. This field counts currently
  associated processes; it supplies job-level evidence when queried through a
  valid retained handle. It does not measure licence-server state.
- Both public pages were retrieved successfully on 2026-10-19. Their addresses,
  retrieval times, HTTP statuses and response bytes were retained with the audit
  working evidence.
- Issue #2095 supplies the audit objective. Issue #2093 supplies the repaired
  smoke/model contract. Repository function names below identify the inspected
  implementation basis; the JSON inventory records the exact bytes reviewed.

No physical equations, numerical coefficients or engineering acceptance limits
are introduced. Source equality is a logical freshness check. It does not test
an operating-system cleanup deadline or assign a numerical solver tolerance.

## Source findings

| Route | Source classification | Evidence boundary and next action |
| --- | --- | --- |
| Native smoke/model | `QUERY_BEFORE_CLOSE` | `drain_owned` queries active members and observes root exit before `cleanup_owned` closes handles. Errors propagate. Preserve this repaired contract; actual operating-system qualification is separate. |
| Cylinder on Windows | `QUERY_BEFORE_CLOSE` | `_settle` polls `WindowsJob.active` before close and retains uncertain supervisors in memory. Review restart persistence and downstream resource release. |
| Cylinder on POSIX | `GROUP_ONLY` | `PosixSupervisor.active` scans the original group and excludes zombies. Escaped descendants are not contained; the source explicitly marks containment unverified. |
| GHS sentinel qualification | `CALLER_CENSUS` | `Owned.cleanup` closes the Job and waits only for the root. `Resources.close_all` additionally waits retained identities and requires a complete census without pending creation. Review these together; live capture remains disabled. |
| OrcaFlex reproduction | `ROOT_ONLY` | `_terminate` requests tree/group termination, then waits for the root. `_solve` observes only root exit before readback. Plan retained containment and drain evidence before reuse qualification. |
| OrcaWave batch orchestration | `ROOT_ONLY` | `_phase2_orcawave_execution` checks the shell-launched batch return code without a whole-job drain observation or an explicit process timeout. Review the batch ownership boundary. |
| AQWA external-force execution | `ROOT_ONLY` | The utility waits for the direct child or returns a launch-only handle; the server caller waits or terminates that root before the next iteration. Plan server and solver cleanup together. |
| OrcaFlex worker pool | `IN_PROCESS` | `run_cases` observes futures and can replace a broken pool after nonwaiting shutdown. No job census is supplied here. Review worker/native-library ownership before scheduler changes. |
| Detached OpenFOAM comparison | `ROOT_ONLY` | `_terminate_group` signals the group but checks root liveness. `poll_detached_run` marks termination after the helper returns, including paths without confirmed drain. Plan identity-bound absence evidence. |
| CalculiX chain comparison | `ROOT_ONLY` | `solve` uses direct-child completion and timeout through `subprocess.run`. Descendant ownership is not established by the inspected method. Review spawning behavior before adding containment. |

`ROOT_ONLY` describes the observed exit boundary, including routes that request a
tree kill. It does not assert that descendants exist or survived. `CALLER_CENSUS`
records the additional caller obligations; it is not a statement that the helper
alone is safe for arbitrary solver reuse. The comparison routes do not consume
a licensed solver in this audit.

## Implemented slices

- Slice 1: a source inventory records termination requests, root exit, whole-job
  drain and unknown-cleanup handling separately for each selected route.
- Slice 2: the offline auditor validates inventory shape and exact source
  fingerprints. Stale entries expose `REVIEW_REQUIRED`; their classifications
  become `NOT_EVALUATED` and their observations are withheld.
- Slice 3: the command-line interface emits JSON with repository-relative source
  paths and constant error codes. Every runtime evidence field and solver reuse
  remain `NOT_EVALUATED`, even when all fingerprints match.

Run from the repository root:

```sh
python scripts/audit_solver_cleanup.py
```

Exit success means the listed source review is current. A changed source returns
a nonzero status; invalid inventory/root input returns a distinct error status.
Neither success nor failure authorizes execution. Recheck actual owned-process
absence, resource ownership and the applicable operational interlock separately.

## Verification

The focused test module is
[test_audit_solver_cleanup.py](../../tests/scripts/test_audit_solver_cleanup.py).
Expected outcomes are independent logical cases written in its docstrings:
matching bytes cannot prove runtime absence, while altered or missing bytes
invalidate source findings. Synthetic inspected source raises if executed,
ensuring that source audit does not depend on executing solver wrappers.

The cases cover combined-source invalidation, file and parent symlinks, unsafe
paths, duplicate keys/entries/dependencies, malformed records, JSON output,
exit-status semantics, read refusal and the checked-in inventory's freshness.
All 31 focused tests passed. Ruff, the engineering-register check, the identifier
check and the repository-structure check passed. No solver, process termination,
licence query or native API is exercised.

## Later slices and limits

Repository-wide discovery, additional language coverage and transitive caller
analysis remain future work. Any harness repair needs a concrete plan and
independent review before changing launch, cleanup or resource-release behavior.
Runtime qualification must include a root-exited/descendant-active case, failed
queries, failed termination, bounded drain, failed assignment, unrelated-process
survival and recovery after uncertain cleanup. Timeout values must come from the
applicable approved execution contract rather than this source audit.

No runtime observation receipt is accepted by this tool, no persistent quarantine
is implemented and no existing launch or cleanup behavior changes. Independent
review remains pending. Source matching cannot certify permission, licence
availability, absence of escaped processes or readiness to launch another solver.
