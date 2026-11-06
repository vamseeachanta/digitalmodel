"""Execute runbook recipes locally; no remote service or solver is contacted."""

import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import time

import pytest


RUNBOOK = Path(__file__).resolve().parents[2] / "docs/runbooks/ssh-detached-launch.md"
# Harness timeout only, not an engineering acceptance limit.
TIMEOUT = 10


@pytest.mark.parametrize("recipe", ["setsid", "nohup"])
def test_documented_launch_disconnects_standard_streams(recipe, tmp_path):
    """Independent expectation from shell redirection and coreutils nohup(1).

    /dev/null yields EOF even when caller stdin carries data. stdout then stderr
    both append to worker.log, so its exact content must be 'stdout\nstderr\n'.
    The worker remains blocked on a gate until after the launcher returns;
    inherited output pipes would prevent communicate from returning.
    setsid(1), util-linux, OPTIONS --fork: the launcher must not wait.
    """
    if os.name != "posix" or not shutil.which(recipe):
        pytest.skip(f"requires POSIX shell and {recipe}")
    text = RUNBOOK.read_text()
    command = re.search(rf"<!-- recipe:{recipe} -->\n```sh\n(.*?)\n```", text, re.S)[1]
    # Exercise the exact remote shell command, excluding the SSH transport.
    args = shlex.split(command)
    assert args[:3] == ["ssh", "-nT", "$REMOTE_TARGET"]
    assert "</dev/null" in args[3]
    worker = tmp_path / "worker"
    worker.write_text(
        f"#!{sys.executable}\n"
        "import pathlib, sys, time\n"
        "assert sys.stdin.read() == ''\n"
        "print('stdout', flush=True)\n"
        "print('stderr', file=sys.stderr, flush=True)\n"
        "pathlib.Path('started').touch()\n"
        f"deadline = time.monotonic() + {TIMEOUT}\n"
        "while not pathlib.Path('release').exists() and time.monotonic() < deadline: time.sleep(0.01)\n"
        "pathlib.Path('finished').touch()\n"
    )
    worker.chmod(0o700)
    process = subprocess.Popen(
        ["sh", "-c", args[3]],
        cwd=tmp_path,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    try:
        stdout, stderr = process.communicate(
            input=b"inherited input\n", timeout=TIMEOUT
        )
        assert process.returncode == 0
        assert stdout == stderr == b""
        deadline = time.monotonic() + TIMEOUT
        while not (tmp_path / "started").exists() and time.monotonic() < deadline:
            time.sleep(0.01)
        assert (tmp_path / "started").exists()
        assert not (tmp_path / "finished").exists()
        assert (tmp_path / "worker.log").read_text() == "stdout\nstderr\n"
    finally:
        (tmp_path / "release").touch()
        if process.poll() is None:
            process.kill()
        process.communicate(timeout=TIMEOUT)
        deadline = time.monotonic() + TIMEOUT
        while not (tmp_path / "finished").exists() and time.monotonic() < deadline:
            time.sleep(0.01)
