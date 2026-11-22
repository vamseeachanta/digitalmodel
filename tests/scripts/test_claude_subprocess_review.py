"""Exercise the issue-522 runner with an inert executable, never an assistant."""

import json
import os
import shlex
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone
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
    for utility in ("mkdir", "grep", "timeout", "date"):
        (binary / utility).symlink_to(shutil.which(utility))
    fake = binary / "claude"
    fake.write_text(
        f"#!{sys.executable}\n"
        "import json, os, pathlib, signal, sys, time\n"
        "if sys.argv[1:] == ['--version']:\n"
        "    if os.environ.get('REVIEW_VERSION_IGNORE_TERM') == '1':\n"
        "        signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
        "    time.sleep(float(os.environ.get('REVIEW_VERSION_SLEEP', '0')))\n"
        "    print('inert-review 1.2.3')\n"
        "    sys.exit(int(os.environ.get('REVIEW_VERSION_EXIT', '0')))\n"
        "record = pathlib.Path(os.environ['REVIEW_RECORD'])\n"
        "record.write_text(json.dumps({'argv': sys.argv[1:], "
        "'stdin': sys.stdin.read()}))\n"
        "if os.environ.get('REVIEW_IGNORE_TERM') == '1':\n"
        "    signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
        "print(os.environ.get('REVIEW_STDOUT', 'review complete'), flush=True)\n"
        "print(os.environ.get('REVIEW_STDERR', ''), file=sys.stderr, flush=True)\n"
        "time.sleep(float(os.environ.get('REVIEW_SLEEP', '0')))\n"
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


@pytest.mark.parametrize("ignore_term, wrapper_code", [(False, 124), (True, 137)])
def test_wall_clock_limit_retains_partial_review_and_manifest(
    review_case, ignore_term, wrapper_code,
):
    prompt, output, record, env = review_case
    env.update(REVIEW_TIMEOUT="1", REVIEW_SLEEP="4", REVIEW_STDOUT="partial review",
               REVIEW_IGNORE_TERM=str(int(ignore_term)))
    started = time.monotonic()
    result = run_runner(prompt, output, env=env)
    elapsed = time.monotonic() - started
    assert result.returncode == 5
    assert elapsed < 3.5, "The runner must also bound a child that ignores SIGTERM"
    assert record.exists()
    assert (output / "review.md").read_text() == "partial review\n"
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert f"claude_exit_code={wrapper_code}" in manifest
    assert "status=timeout" in manifest
    assert "exit_code=5" in manifest


@pytest.mark.parametrize("value", ["0", "-1", "invalid", "1.5", "1s"])
def test_invalid_timeout_never_launches_cli(review_case, value):
    prompt, output, record, env = review_case
    env["REVIEW_TIMEOUT"] = value
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 2
    assert not record.exists()
    assert not output.exists()


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


def test_manifest_records_prompt_utc_time_version_and_timeout(review_case):
    prompt, output, _, env = review_case
    env.pop("REVIEW_TIMEOUT", None)
    before = datetime.now(timezone.utc).replace(microsecond=0)
    result = run_runner(prompt, output, env=env)
    after = datetime.now(timezone.utc)
    assert result.returncode == 0, result.stderr
    manifest = dict(line.split("=", 1) for line in
                    (output / "manifest.txt").read_text().splitlines())
    assert shlex.split(manifest["prompt_file"]) == [str(prompt)]
    assert manifest["started_utc"].endswith("Z")
    started = datetime.fromisoformat(manifest["started_utc"].replace("Z", "+00:00"))
    assert before <= started <= after
    assert manifest["cli_version"] == "inert-review 1.2.3"
    assert manifest["review_timeout_seconds"] == "600"


@pytest.mark.parametrize("version_env", [
    {"REVIEW_VERSION_EXIT": "17"},
    {"REVIEW_VERSION_SLEEP": "4", "REVIEW_VERSION_IGNORE_TERM": "1"},
])
def test_version_probe_failure_is_bounded_and_cannot_discard_review(
    review_case, version_env,
):
    prompt, output, record, env = review_case
    env.update(version_env)
    started = time.monotonic()
    result = run_runner(prompt, output, env=env)
    assert time.monotonic() - started < 3.5
    assert result.returncode == 0
    assert json.loads(record.read_text())["stdin"] == prompt.read_text()
    assert (output / "review.md").read_text() == "review complete\n"
    assert "cli_version=unavailable" in (output / "manifest.txt").read_text().splitlines()


@pytest.mark.parametrize("stream", ["REVIEW_STDOUT", "REVIEW_STDERR"])
def test_turn_exhaustion_fails_even_when_cli_exits_zero(review_case, stream):
    prompt, output, _, env = review_case
    env[stream] = "Error: Reached max turns (3)"
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 3
    assert "Reached max turns" in (
        (output / "review.md").read_text() + (output / "stderr.log").read_text()
    )
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=0" in manifest
    assert f"exit_code={result.returncode}" in manifest
    assert "status=turn_exhausted" in manifest


@pytest.mark.parametrize("text", ["", " \t\n"])
def test_empty_review_is_not_success(review_case, text):
    prompt, output, _, env = review_case
    env["REVIEW_STDOUT"] = text
    result = run_runner(prompt, output, env=env)
    assert result.returncode == 4
    manifest = (output / "manifest.txt").read_text().splitlines()
    assert "claude_exit_code=0" in manifest
    assert f"exit_code={result.returncode}" in manifest
    assert "status=empty_review" in manifest


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
