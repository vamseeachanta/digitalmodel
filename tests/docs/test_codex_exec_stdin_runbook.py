# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise runbook examples with a local stdin recorder, never an assistant CLI."""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
RUNBOOK = ROOT / "docs" / "runbooks" / "codex-exec-stdin.md"
RECORDER = (
    "import json, sys; "
    "print(json.dumps({'args': sys.argv[1:], 'stdin': sys.stdin.read()}))"
)
PROMPT = "Summarize the repository structure."


def example(language, name):
    """Read the exact executable example following its identifying comment."""
    text = RUNBOOK.read_text(encoding="utf-8")
    pattern = rf"```{language}\n# {re.escape(name)}\n(.*?)\n```"
    match = re.search(pattern, text, re.DOTALL)
    assert match is not None, f"Missing example: {name}"
    return match.group(1)


@pytest.mark.parametrize("upstream", ["", "unrelated upstream data\n"])
def test_shell_prompt_ignores_inherited_stdin(upstream):
    """POSIX.1-2024, chapter 10: null-device reads yield EOF, hence empty text.

    GNU Bash manual, Redirections / Redirecting Input: the redirect replaces
    the command's standard input. Upstream text must therefore be absent.
    """
    script = example("sh", "prompt argument, no stdin context")
    # A shell function intercepts the command before any executable lookup.
    recorder_function = 'codex() { "$TEST_PYTHON" -c "$RECORDER" "$@"; }\n'
    result = subprocess.run(
        ["bash", "-c", recorder_function + script],
        input=upstream,
        text=True,
        capture_output=True,
        check=True,
        env={**os.environ, "TEST_PYTHON": sys.executable, "RECORDER": RECORDER},
    )
    assert json.loads(result.stdout) == {
        "args": ["exec", PROMPT],
        "stdin": "",
    }


@pytest.mark.parametrize("upstream", ["", "unrelated upstream data\n"])
def test_python_prompt_ignores_inherited_stdin(upstream):
    """Python 3.11 subprocess.DEVNULL uses the null device: expect empty input.

    The child interpreter inherits the supplied upstream text. Only the command
    executable is replaced, so removing DEVNULL exposes that text to the recorder.
    """
    script = example("python", "Python prompt argument, no stdin context")
    interception = f"""
import subprocess
import sys
original_run = subprocess.run
def record(command, **kwargs):
    assert command == ["codex", "exec", {PROMPT!r}]
    return original_run(
        [sys.executable, "-c", {RECORDER!r}, *command[1:]], **kwargs
    )
subprocess.run = record
"""
    result = subprocess.run(
        [sys.executable, "-c", interception + script],
        input=upstream,
        text=True,
        capture_output=True,
        check=True,
    )
    assert json.loads(result.stdout) == {
        "args": ["exec", PROMPT],
        "stdin": "",
    }


def test_shell_context_reaches_command():
    """Bash 5.3 Pipelines: printf's finite output becomes the command's stdin.

    Hand derivation: printf '%s\\n' writes the literal text followed by a newline.
    The recorder must receive exactly that text, with the prompt kept in argv.
    """
    script = example("sh", "intentional finite stdin context")
    recorder_function = 'codex() { "$TEST_PYTHON" -c "$RECORDER" "$@"; }\n'
    result = subprocess.run(
        ["bash", "-c", recorder_function + script],
        input="unrelated upstream data\n",
        text=True,
        capture_output=True,
        check=True,
        env={**os.environ, "TEST_PYTHON": sys.executable, "RECORDER": RECORDER},
    )
    assert json.loads(result.stdout) == {
        "args": ["exec", PROMPT],
        "stdin": "Selected repository notes.\n",
    }
