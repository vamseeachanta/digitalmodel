# Codex exec standard input in automation

For automation that supplies a prompt argument and no stdin context, disconnect
standard input explicitly. Issue #2060 reports that an inherited non-terminal
input stream can leave `codex exec` waiting for additional input.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-19 | Document explicit stdin handling to prevent inherited-input waits. |

## Symptom and basis

An automated launch can wait even though the complete prompt was passed as an
argument. A caller can retain an open pipe without sending more bytes or closing
it. A reader waiting for end-of-file can then remain blocked.

The [OpenAI non-interactive mode documentation](https://developers.openai.com/codex/noninteractive)
states that piped stdin accompanies a prompt argument as additional context.
Issue #2060 records the observed wait and the null-device workaround. The issue
does not establish that every CLI version exhibits the same wait.

## Shell: prompt argument without stdin context

```sh
# prompt argument, no stdin context
codex exec "Summarize the repository structure." < /dev/null
```

Keep the redirection on the `codex exec` command itself, including inside loops
or pipelines. This gives that command end-of-file without consuming the caller's
input. Quoting keeps the prompt together as an argument.

The [GNU Bash manual, edition 5.3 (2025), Redirections: Redirecting Input](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
defines `<` as replacing standard input when no descriptor is specified.
[POSIX.1-2024, Base Definitions, chapter 10: Directory Structure and Devices](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap10.html)
specifies that reading the null device returns end-of-file.

## Python: prompt argument without stdin context

```python
# Python prompt argument, no stdin context
import subprocess

subprocess.run(
    ["codex", "exec", "Summarize the repository structure."],
    stdin=subprocess.DEVNULL,
    check=True,
)
```

The [Python 3.11 subprocess documentation, DEVNULL and Popen](https://docs.python.org/3.11/library/subprocess.html#subprocess.DEVNULL)
defines `DEVNULL` as connecting the stream to the null device. Omitting `stdin`
inherits the caller's stream. Keep the prompt in an argument list and set
`check=True` so a command failure is raised to the caller. Standard output and
standard error remain available to the caller's normal logging.

## Shell: intentional stdin context

When stdin supplies context, use a producer that finishes and closes its output.
Do not add null-device redirection to this command:

```sh
# intentional finite stdin context
printf '%s\n' 'Selected repository notes.' |
    codex exec "Summarize the repository structure."
```

The [GNU Bash manual, edition 5.3 (2025), Pipelines](https://www.gnu.org/software/bash/manual/html_node/Pipelines.html)
defines how a producer's standard output becomes the next command's standard
input. Here `printf` writes the supplied text and newline, then exits. The OpenAI
documentation cited above describes the prompt argument as the instruction and
the piped text as additional context. A file redirected into the command is also
finite input. A producer that keeps its output open can reintroduce the wait.

## Diagnosis and limits

Check whether the caller intentionally supplies stdin context before adding the
redirection. Disconnecting stdin discards that context. If a wait persists with
stdin disconnected, investigate authentication, permissions, connectivity and
command diagnostics separately; this workaround addresses inherited input only.

No timeout or retry value is prescribed. Such limits depend on the caller's work
and execution policy. No engineering equations or numerical acceptance limits
apply to this input-routing procedure.

The examples are checked with a local stdin recorder in
`tests/docs/test_codex_exec_stdin_runbook.py`. Those checks establish shell and
Python input routing, without running an assistant CLI or asserting behavior of
a particular installed CLI version. Public documentation was accessed on
2026-10-19.
