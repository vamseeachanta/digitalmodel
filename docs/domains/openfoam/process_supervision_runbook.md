# Process supervision and recorded-PID cleanup

## Summary

Use terminal markers to determine a run's outcome. Inspect an explicitly
recorded PID against its case directory and executable before treating it as a
candidate for cleanup. Full-command-line patterns can match the control shell
that carries the pattern, so they cannot establish process ownership. This
runbook accompanies issue #2053 and the existing CFD supervision scripts.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-19 | Added marker interpretation, recorded-PID inspection and descriptor-based cleanup to prevent wrapper matches. |
| 1 | 2026-10-25 | Added a test-interpreter preflight because Python builds can omit the required descriptor APIs. |

## Diagnosis

`pgrep -f` searches the full command line; a shell or SSH wrapper can contain
the same literal pattern as the intended program. The search process excludes
itself, but that exclusion does not remove its wrappers. `pkill -f` selects by
the same pattern and can therefore signal a control process. These behaviours
follow the OPTIONS and NOTES sections of [pgrep(1)][pgrep]. Changing the pattern
to a bracket expression is insufficient when another argument carries a
matching string.

The existing `cfd_solver_running` and `cfd_solver_ranks` helpers in
[`cfd_chain.sh`](../../../scripts/cfd/lib/cfd_chain.sh) use exact process-name
matching. That avoids command-line wrapper matches, but a name alone does not
identify a case, executable file or launch. Use name matching only as a broad
discovery or resource-availability check. A PID file alone and a successful
existence probe also do not establish ownership.

## Determine the outcome before cleanup

- Select the expected terminal marker from the launcher contract. For example,
  `cfd_marker_ok` and `cfd_marker_fail` write `OK` and `FAILED` to `CFD_MARKER`;
  continuation runs use `CONT_DONE` and `CONT_FAILED`. Marker formats differ
  between launchers; parse the selected launcher's format.
- Associate the marker and recorded PID with the current invocation. Use fresh
  per-run state, or verify the recorded launch identity before polling. An older
  success marker does not describe a restarted run.
- A missing, empty, unreadable or unrecognised marker is an unknown outcome.
  Do not infer success from the absence of a process, or infer a running process
  from the absence of a marker. If both success and failure markers exist for
  the same invocation, treat the evidence as inconsistent.
- Poll the explicit marker with an operator-supplied interval and deadline.
  Reaching the deadline without a current terminal marker leaves the outcome
  `NOT_EVALUATED`. These are required operational inputs; this runbook assigns
  no numerical polling or completion limits.
- If a recorded process is still expected to exist, validate it locally as
  below. Its disappearance or inaccessible metadata leaves the outcome unknown
  until the terminal marker and logs establish what happened. A successful
  process inspection means only that the candidate currently matches.

These marker conventions come from the repository's
[`cfd_chain.sh`](../../../scripts/cfd/lib/cfd_chain.sh) and
[`continue_case.sh`](../../../scripts/cfd/continue_case.sh). Marker freshness
and run association remain the launcher's responsibility.

## Inspect one recorded process

Run from the repository root, in the same PID namespace and filesystem view
as the candidate. Supply the launcher's recorded PID, the expected **exact**
working directory and the expected executable file. These values are inputs
from the launch record, not values discovered by command-line matching.

```bash
: "${RECORDED_PID:?set from the current launch record}"
: "${CASE_DIR:?set the expected exact working directory}"
: "${EXPECTED_EXECUTABLE:?set the expected executable file}"
python3 scripts/cfd/case_process.py \
  "$RECORDED_PID" "$CASE_DIR" "$EXPECTED_EXECUTABLE"
```

`VALIDATED` means that the PID is positive, is neither the helper nor one of
its controlling ancestors, and has the expected directory and executable
identity. Directory and executable comparisons follow filesystem identity,
so symlink aliases work; siblings and descendants are excluded. The helper
reads `/proc/<pid>/cwd`, `/proc/<pid>/exe` and ancestry through `PPid` in
`/proc/<pid>/status`, as defined in [proc_pid_cwd(5)][cwd],
[proc_pid_exe(5)][exe] and [proc_pid_status(5)][status]. For scripts, the
executable is their interpreter; an interpreter match alone does not identify
which script is running.

The helper opens a process descriptor before inspecting metadata. It then
uses a signal-zero probe on that descriptor to check existence and permission
without delivering a signal. Signal-zero semantics are defined in
[kill(2), DESCRIPTION][kill]. Read-only mode sends no terminating signal.

Missing processes, mismatched metadata, permission failures, unsupported
descriptor APIs and unavailable ancestry produce `NOT_EVALUATED` with an
unsuccessful exit status. There is no numeric-PID signalling fallback. A
restricted procfs view can prevent inspection of an otherwise live process.

## Request cleanup explicitly

After confirming the launch record and expected executable, request TERM:

```bash
python3 scripts/cfd/case_process.py \
  "$RECORDED_PID" "$CASE_DIR" "$EXPECTED_EXECUTABLE" --terminate
```

This repeats the checks and sends `SIGTERM` through the same descriptor opened
for that invocation. A descriptor refers to a particular process; if that
process exits, its recycled numeric PID cannot redirect the signal to another
process. This property is documented in [pidfd_send_signal(2), NOTES][pidfd]
and the Python [os.pidfd_open][python-os] and
[signal.pidfd_send_signal][python-signal] API references.

`TERM_SENT` confirms that the signal request succeeded. It does not confirm
termination, successful completion or a complete process-tree cleanup. Check
the terminal marker and logs again. Do not escalate by command-line pattern
or signal an unvalidated process group. Each additional candidate needs its
own launch evidence and validation.

## Limits and later work

This is a local operational check, not an ownership or containment boundary.
A stale PID could already refer to another process with the same cwd and
executable before inspection begins. The descriptor prevents retargeting
after it is opened; it does not authenticate historical launch identity.
Directory and executable checks are snapshots, and a process can change cwd
or execute another program after they are read. Confirm the current launch
record before permitting cleanup.

The helper requires Linux procfs access and Python support for both descriptor
APIs. It does not load a solver, launch a run, replace existing pollers, wait
for termination, authenticate terminal markers or manage descendants.
Persistent launch identity, automated marker freshness and integration into
existing launchers remain later work. No engineering equation, coefficient or
acceptance threshold is introduced.

## Validation and sources

Before running the recorded-PID tests, select a Python interpreter that exposes
both descriptor APIs. Python version alone does not establish their availability.
Use the same interpreter for the preflight and the test run:

```bash
: "${PROCESS_TEST_PYTHON:?set an interpreter with Linux pidfd support}"
"$PROCESS_TEST_PYTHON" -c \
  'from os import pidfd_open; from signal import pidfd_send_signal'
uv run --no-project --python "$PROCESS_TEST_PYTHON" --with pytest \
  python -m pytest tests/scripts/test_cfd_case_process.py -q -rA \
  -p no:cacheprovider --basetemp="${TMPDIR:?}/recorded-pid-tests"
```

Set `TMPDIR` to a scratch directory outside the repository. A missing API leaves
process inspection unavailable; the helper's refusal remains required.

[`test_cfd_case_process.py`](../../../tests/scripts/test_cfd_case_process.py)
uses ordinary Python children in temporary directories. Expected results
follow known cwd/interpreter identity, distinct filesystem objects, explicit
EOF and wait, and the [subprocess signal-return convention][python-subprocess]. It also reproduces
a full-command-line wrapper match without sending any signal. It exercises
ancestor rejection, stale handles, missing APIs and denied metadata. Timeouts
are test-harness budgets, not engineering acceptance limits.

The references below were read from public documentation on 2026-10-19.
Linux procfs and signal semantics use Linux man-pages 6.19 (2026);
`pgrep(1)` is the procps-ng manual, OPTIONS and NOTES. The Python references
are the Python 3 Library Reference, process-management API sections (2026).

[pgrep]: https://man7.org/linux/man-pages/man1/pgrep.1.html
[cwd]: https://man7.org/linux/man-pages/man5/proc_pid_cwd.5.html
[exe]: https://man7.org/linux/man-pages/man5/proc_pid_exe.5.html
[status]: https://man7.org/linux/man-pages/man5/proc_pid_status.5.html
[kill]: https://man7.org/linux/man-pages/man2/kill.2.html
[pidfd]: https://man7.org/linux/man-pages/man2/pidfd_send_signal.2.html
[python-os]: https://docs.python.org/3/library/os.html#os.pidfd_open
[python-signal]: https://docs.python.org/3/library/signal.html#signal.pidfd_send_signal
[python-subprocess]: https://docs.python.org/3/library/subprocess.html#subprocess.Popen.returncode
