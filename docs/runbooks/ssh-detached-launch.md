# Detached commands over SSH

A remote launch can leave SSH waiting when a background worker inherits the
connection's standard streams. Redirect standard input, standard output and
standard error explicitly. A new process session alone does not close inherited
descriptors. This runbook covers non-interactive POSIX-shell launches on Linux;
it does not establish worker completion or configure a service manager.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-19 | Document detached launches and test descriptor handling to prevent SSH hangs. |

## Linux launch

Set `REMOTE_TARGET` to an existing SSH configuration alias. Prepare the remote
working directory, an executable `./worker`, and a writable `./worker.log`.
Replace those relative paths with the intended command and log. Quote arguments
inside the remote command for the remote shell; do not interpolate untrusted text.
The worker must require no interactive input. Existing authentication is required.

<!-- recipe:setsid -->
```sh
ssh -nT "$REMOTE_TARGET" 'setsid -f ./worker </dev/null >>./worker.log 2>&1'
```

`setsid -f` always forks and starts the program in a new process session
[util-linux setsid(1), OPTIONS, May 2026](https://man7.org/linux/man-pages/man1/setsid.1.html).
Do not add `--wait` when launch return is required. This requires util-linux
`setsid`; the option is not assumed available on other operating systems.

`</dev/null` supplies EOF to the remote worker. `>>./worker.log` appends its
standard output; `2>&1` then sends standard error to the same destination.
The ordering matters: duplicating standard output before redirecting it leaves
standard error on the original connection. Descriptor syntax is defined in
[GNU Bash manual, REDIRECTION](https://man7.org/linux/man-pages/man1/bash.1.html).
The descriptor number in the recipe denotes standard error, not a numeric
engineering input or acceptance limit.

`ssh -n` disconnects the local client's standard input; `-T` disables terminal
allocation. Local input handling does not replace the remote redirections
[OpenSSH ssh(1), options -n and -T](https://man.openbsd.org/ssh.1).

## Fallback with nohup

Where `setsid -f` is unavailable, the following recipe ignores hangup signals
and backgrounds the worker. It does not establish a new process session.

<!-- recipe:nohup -->
```sh
ssh -nT "$REMOTE_TARGET" 'nohup ./worker </dev/null >>./worker.log 2>&1 &'
```

GNU `nohup` automatically redirects streams only under terminal-dependent
conditions; SSH pipes need explicit redirection. The shell's `&` backgrounds the
command; `nohup` alone does not background it
[GNU coreutils 9.11 nohup(1), DESCRIPTION, April 2026](https://man7.org/linux/man-pages/man1/nohup.1.html).

## Diagnose and confirm completion

If SSH still waits, check the complete remote command, including wrappers and
child processes, for inherited output descriptors or commands that wait for the
worker. Confirm redirections apply to the worker rather than only the local SSH
client. Check log permissions and utility availability before launch. Distinguish
connection or authentication failures from a worker launch failure.

A returned SSH command establishes neither successful worker startup nor a
completed result. A fork or background launch can return before an executable
fails. Read the remote log and require the worker's own startup acknowledgement,
completion record and expected output before reporting completion. Session
separation also does not override service policies that terminate login jobs.
For managed long-running work, use the deployment's existing service manager.

The regression tests execute the quoted remote commands locally with a synthetic
worker. They check EOF on input, both output streams in the log, and launcher
return while the worker is still blocked. They do not test SSH transport,
authentication, terminal hangups or login-service policies.

Related issue: [SSH detached launch quirk](https://github.com/vamseeachanta/digitalmodel/issues/2058).
