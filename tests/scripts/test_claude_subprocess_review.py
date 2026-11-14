"""Exercise the issue-522 runner with an inert executable, never an assistant."""

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / "scripts/automation/claude_subprocess_review.sh"
BASH = shutil.which("bash")


def run_runner(*arguments, env=None):
    environment = dict(os.environ if env is None else env)
    for key in ("BASH_ENV", "ENV", "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    return subprocess.run(
        [BASH, str(RUNNER), *map(str, arguments)],
        capture_output=True, text=True, env=environment, timeout=10,
    )


def test_help_succeeds_without_an_assistant():
    result = run_runner("--help")
    assert result.returncode == 0, result.stderr
    assert "Usage:" in result.stdout
    assert "PROMPT_FILE OUTPUT_DIR" in result.stdout


@pytest.fixture
def review_case(tmp_path):
    """Only this inert boundary executable is reachable as `claude`."""
    binary = tmp_path / "bin"
    binary.mkdir()
    for utility in ("mkdir", "grep"):
        (binary / utility).symlink_to(shutil.which(utility))
    fake = binary / "claude"
    fake.write_text(
        f"#!{sys.executable}\n"
        "import json, os, pathlib, sys\n"
        "record = pathlib.Path(os.environ['REVIEW_RECORD'])\n"
        "record.write_text(json.dumps({'argv': sys.argv[1:], "
        "'stdin': sys.stdin.read()}))\n"
        "print(os.environ.get('REVIEW_STDOUT', 'review complete'))\n"
        "print(os.environ.get('REVIEW_STDERR', ''), file=sys.stderr)\n"
        "sys.exit(int(os.environ.get('REVIEW_EXIT', '0')))\n"
    )
    fake.chmod(0o700)
    prompt = tmp_path / "prompt with spaces;literal.md"
    prompt.write_text("Review only the allowlisted files.\n$(exit 98)\n")
    output = tmp_path / "output with spaces"
    record = tmp_path / "call.json"
    env = dict(os.environ, PATH=str(binary),
               REVIEW_RECORD=str(record))
    return prompt, output, record, env


def test_prompt_uses_finite_stdin_and_records_invocation(review_case):
    prompt, output, record, env = review_case
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 0, result.stderr
    assert json.loads(record.read_text()) == {
        "argv": ["-p", "--max-turns", "3", "--output-format", "text"],
        "stdin": prompt.read_text(),
    }
    assert (output / "review.md").read_text() == "review complete\n"
    assert (output / "stderr.log").read_text() == "\n"
    manifest = (output / "manifest.txt").read_text()
    assert "command=claude -p --max-turns 3 --output-format text\n" in manifest
    assert "claude_exit_code=0\n" in manifest
    assert "exit_code=0\n" in manifest


@pytest.mark.parametrize("stream", ["REVIEW_STDOUT", "REVIEW_STDERR"])
def test_turn_exhaustion_fails_even_when_cli_exits_zero(review_case, stream):
    prompt, output, _, env = review_case
    env[stream] = "Error: Reached max turns (3)"
    result = run_runner(prompt, output, env=env)
    assert result.returncode != 0
    assert "Reached max turns" in (
        (output / "review.md").read_text() + (output / "stderr.log").read_text()
    )
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=0" in manifest
    assert f"exit_code={result.returncode}" in manifest


@pytest.mark.parametrize("text", ["", " \t\n"])
def test_empty_review_is_not_success(review_case, text):
    prompt, output, _, env = review_case
    env["REVIEW_STDOUT"] = text
    result = run_runner(prompt, output, env=env)
    assert result.returncode != 0
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=0" in manifest
    assert f"exit_code={result.returncode}" in manifest


@pytest.mark.parametrize("message", ["authentication rejected", "Reached max turns"])
def test_cli_failure_is_preserved_with_diagnostics(review_case, message):
    prompt, output, _, env = review_case
    env.update(REVIEW_EXIT="17", REVIEW_STDERR=message)
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 17
    assert (output / "stderr.log").read_text() == message + "\n"
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=17" in manifest
    assert "exit_code=17" in manifest


@pytest.mark.parametrize("kind", ["missing", "empty", "directory"])
def test_invalid_prompt_never_launches_cli(review_case, kind):
    prompt, output, record, env = review_case
    if kind == "missing":
        prompt.unlink()
    elif kind == "empty":
        prompt.write_text("")
    else:
        prompt.unlink()
        prompt.mkdir()
    result = run_runner(prompt, output, env=env)
    assert result.returncode != 0
    assert not record.exists()
    assert not output.exists()


@pytest.mark.parametrize("arguments", [[], ["prompt"], ["a", "b", "c"]])
def test_invalid_argument_count_fails(arguments):
    result = run_runner(*arguments)
    assert result.returncode == 2
    assert "Usage:" in result.stderr


def test_existing_output_is_preserved_without_launch(review_case):
    prompt, output, record, env = review_case
    output.mkdir()
    original = {"review.md": b"prior review", "manifest.txt": b"prior manifest"}
    for name, content in original.items():
        (output / name).write_bytes(content)
    result = run_runner(prompt, output, env=env)
    assert result.returncode != 0
    assert not record.exists()
    assert {path.name: path.read_bytes() for path in output.iterdir()} == original


def test_missing_cli_records_launch_failure_without_a_review(review_case):
    prompt, output, record, env = review_case
    (Path(env["PATH"]) / "claude").unlink()
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 127
    assert not record.exists()
    assert (output / "review.md").read_bytes() == b""
    assert "command not found" in (output / "stderr.log").read_text()
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=127" in manifest
    assert "exit_code=127" in manifest
