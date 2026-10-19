"""Synthetic procfs expectations follow Linux man-pages 6.19 cmdline/cwd.

PIDs are fixture labels, not measured values. Each expected set is obtained
by hand: retain only records with an exact script argv entry AND equal cwd.
No solver or real process is queried by these fixture tests.
"""
import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/process/find_script_processes.py"


@pytest.fixture
def lookup():
    spec = importlib.util.spec_from_file_location("process_lookup", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.find_script_processes


def record(root, pid, argv, cwd):
    entry = root / str(pid)
    entry.mkdir()
    (entry / "cmdline").write_bytes(b"\0".join(arg.encode() for arg in argv) + b"\0")
    (entry / "cwd").symlink_to(cwd, target_is_directory=True)
    (entry / "comm").write_text("driver.sh\n")


@pytest.mark.parametrize("invocation", ["driver.sh", "./driver.sh", "absolute"])
def test_shebang_or_explicit_shell_without_comm_bash(tmp_path, lookup, invocation):
    """One exact argv/cwd pair qualifies regardless of script-valued comm."""
    cwd = tmp_path / "work with spaces"
    cwd.mkdir()
    script = cwd / "driver.sh"
    script.touch()
    argument = str(script) if invocation == "absolute" else invocation
    proc = tmp_path / "proc"
    proc.mkdir()
    record(proc, 101, ["/bin/bash", argument], cwd)
    assert lookup(script, cwd, proc) == [101]


def test_rejects_substrings_wrong_cwd_and_empty_cmdline(tmp_path, lookup):
    """Intersection by hand: only PID 101 has both exact argv and cwd."""
    cwd = tmp_path / "work"
    cwd.mkdir()
    other = tmp_path / "other"
    other.mkdir()
    script = cwd / "driver.sh"
    script.touch()
    proc = tmp_path / "proc"
    proc.mkdir()
    record(proc, 101, ["bash", str(script)], cwd)
    record(proc, 102, ["bash", str(script)], other)
    record(proc, 103, ["ssh", f"bash {script}"], cwd)
    record(proc, 104, ["bash", str(script) + ".bak"], cwd)
    record(proc, 105, [], cwd)
    (proc / "106").mkdir()  # Process vanished before cmdline could be read.
    (proc / "self").mkdir()
    assert lookup(script, cwd, proc) == [101]


def test_missing_proc_is_an_error(tmp_path, lookup):
    """An unavailable procfs cannot establish an empty candidate set."""
    script = tmp_path / "driver.sh"
    script.touch()
    with pytest.raises(OSError):
        lookup(script, tmp_path, tmp_path / "missing")


@pytest.mark.skipif(sys.platform != "linux", reason="Linux procfs integration")
@pytest.mark.parametrize("explicit_shell", [False, True])
def test_live_shell_script(tmp_path, lookup, explicit_shell):
    """Independent oracle: Popen supplies the PID of the blocked shell script.

    The readiness line proves startup; stdin holds the process until lookup
    finishes. Both exec-by-shebang and explicit Bash must return that PID.
    """
    script = tmp_path / "driver.sh"
    script.write_text('#!/bin/bash\nprintf "ready\\n"\nread -r release\n')
    script.chmod(0o700)  # POSIX owner read/write/execute mode, chmod(1).
    command = ["/bin/bash", str(script)] if explicit_shell else [str(script)]
    with subprocess.Popen(
        command, cwd=tmp_path, stdin=subprocess.PIPE,
        stdout=subprocess.PIPE, text=True,
    ) as process:
        try:
            assert process.stdout.readline() == "ready\n"
            assert process.pid in lookup(script, tmp_path)
        finally:
            process.communicate(input="release\n")


def test_cli_rejects_missing_script(tmp_path):
    """argparse reports invalid input rather than an empty observation."""
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--script", str(tmp_path / "missing"),
         "--cwd", str(tmp_path)], capture_output=True, text=True,
    )
    assert result.returncode != 0
    assert "error:" in result.stderr
    assert not result.stdout


def test_unresolvable_argument_does_not_abort_scan(tmp_path, lookup):
    """A cyclic path in one candidate must not conceal the valid record."""
    script = tmp_path / "driver.sh"
    script.touch()
    loop = tmp_path / "loop"
    loop.symlink_to(loop)
    proc = tmp_path / "proc"
    proc.mkdir()
    record(proc, 101, ["bash", str(loop)], tmp_path)
    record(proc, 102, ["bash", str(script)], tmp_path)
    assert lookup(script, tmp_path, proc) == [102]
