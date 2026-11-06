# Finding a running shell script on Linux

## Summary

An empty `pgrep -x bash` result does not establish that no shell script is
running. A script launched through its shebang can have the script name in
`comm`. Use command-line arguments and the expected working directory to obtain
candidates for inspection. This procedure is read-only.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-19 | Document argument and directory matching because exact Bash-name matching misses shebang scripts. |

## Procedure

From the repository root, supply the actual script and its expected working
directory as quoted inputs:

```sh
python3 scripts/process/find_script_processes.py --script "$SCRIPT" --cwd "$WORK_DIR"
```

The helper enumerates numeric procfs entries without filtering `comm`, splits
`cmdline` at NUL boundaries, resolves relative arguments against the observed
working directory, and prints PIDs whose argument names the requested script
and whose working directory equals the supplied directory. Spaces in paths
remain part of the argument. A command containing the script name only as part
of a larger string does not qualify.

Inspect each candidate before acting. An exact argument can also belong to an
editor, a wrapper, or this lookup command itself: a candidate is not proof that
the script is executing. Check the executable, invocation, recorded launch
identity and supervision state through the relevant operational procedure.
Do not pipe this output into `kill`, `pkill`, or a deployment command.

## Limits and way forward

Processes may exit, change directory, or alter their arguments while being
read; PIDs may be reused. Unreadable and vanished entries are omitted. Empty
output therefore means no candidate was observed, not that the system is idle.
An unavailable procfs or invalid input path produces an error. Relative script
arguments are interpreted against the current cwd and may no longer resolve
correctly if a script has changed directory. Container PID namespaces and
procfs permissions also limit visibility.

The deployment script `scripts/cfd/deploy_lane.sh` already scans command lines
rather than only Bash names. Its broad driver-name scan is separate from this
diagnostic lookup. This runbook does not change deployment behavior or certify
that deployment is permitted. Automatic actions require a separately reviewed
process-identity and supervision design.

## Sources

- [Issue](https://github.com/vamseeachanta/digitalmodel/issues/2054): reported shebang-name failure and directory-validation workaround.
- Linux man-pages 6.19 (2026), [proc_pid_comm(5), DESCRIPTION](https://man7.org/linux/man-pages/man5/proc_pid_comm.5.html): command name is mutable and can be truncated.
- Linux man-pages 6.19 (2026), [proc_pid_cmdline(5), DESCRIPTION](https://man7.org/linux/man-pages/man5/proc_pid_cmdline.5.html): NUL-separated arguments, empty zombie command lines, and mutable contents.
- Linux man-pages 6.19 (2026), [proc_pid_cwd(5), DESCRIPTION](https://man7.org/linux/man-pages/man5/proc_pid_cwd.5.html): working-directory link and access restrictions.

No engineering equations, coefficients, or acceptance limits apply. Fixture
expectations are derived by explicit argument-and-directory set intersection
in `tests/scripts/test_find_script_processes.py`.
