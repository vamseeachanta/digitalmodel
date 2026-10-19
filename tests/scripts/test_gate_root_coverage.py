"""Execution assignments must cover declared roots and every pytest file."""

import os
import re
import shlex
import subprocess
import sys
from fnmatch import fnmatch
from functools import lru_cache
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
GATES = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
SHARDS = {name: gate for name, gate in GATES.items() if gate.get("domain")}


@lru_cache(maxsize=None)
def pytest_paths(command):
    """Read positional paths, keeping --ignore arguments out of the roots."""
    args = shlex.split(command)
    args = args[args.index("pytest") + 1 :]
    roots, ignored = [], []
    while args:
        arg = args.pop(0)
        if arg == "--ignore":
            ignored.append(Path(args.pop(0)))
        elif arg.startswith("--ignore="):
            ignored.append(Path(arg.split("=", 1)[1]))
        elif arg.startswith("tests/") and "::" not in arg:
            roots.append(Path(arg))
    return roots, ignored


def covers(path, command):
    roots, ignored = pytest_paths(command)
    return any(path == root or root in path.parents for root in roots) and not any(
        path == root or root in path.parents for root in ignored
    )


def test_declared_test_roots_are_executed():
    """Existing routed tests need matching pytest arguments, not metadata alone."""
    missing = []
    for line in (ROOT / "tests/DOMAINS.md").read_text().splitlines():
        cells = line.split("|")
        if len(cells) < 4 or (name := "tests-" + cells[1].strip()) not in SHARDS:
            continue
        argv = shlex.split(SHARDS[name]["command"])
        for root in re.findall(r"`(tests/[^`]+)`", cells[2]):
            if (ROOT / root).exists() and root.endswith(("/", ".py")) and root not in argv:
                missing.append(f"{name}: {root}")
    assert not missing, f"{len(missing)} missing root assignments:\n" + "\n".join(missing)


def test_every_test_file_has_an_execution_assignment():
    """Exercise the sweep's real command reader, including sweep-only paths."""
    sweep = yaml.safe_load((ROOT / ".github/workflows/full-matrix-sweep.yml").read_text())
    steps = sweep["jobs"]["sweep-tests"]["steps"]
    reader = next(step for step in steps if step.get("id") == "gate")
    script = reader["run"].split("<<'PY'\n", 1)[1].split("\nPY\n", 1)[0]
    commands = []
    for name, gate in SHARDS.items():
        assert gate["enabled"], name
        env = {**os.environ, "GATE_NAME": name}
        result = subprocess.run(
            [sys.executable, "-c", script], cwd=ROOT, env=env,
            check=True, capture_output=True, text=True,
        )
        commands.extend((gate["command"], result.stdout.strip()))
    files = sorted(
        path.relative_to(ROOT) for path in (ROOT / "tests").rglob("*.py")
        if fnmatch(path.name, "test_*.py") or fnmatch(path.name, "*_test.py")
    )
    assert files, "No test files found"
    missing = [str(path) for path in files if not any(covers(path, cmd) for cmd in commands)]
    assert not missing, f"{len(missing)} unassigned test files:\n" + "\n".join(missing)


@pytest.mark.parametrize(
    ("path", "arguments", "expected"),
    [
        ("tests/a/test_one.py", "tests/a/", True),
        ("tests/ab/test_one.py", "tests/a/", False),
        ("tests/a/test_one.py", "--ignore tests/a/ tests/", False),
        ("tests/a/test_one.py", "--ignore=tests/a/test_one.py tests/a/", False),
        ("tests/a/test_one.py", "tests/a/test_one.py::test_one", False),
        ("tests/a/test_one.py", "tests/a/test_one.py", True),
    ],
)
def test_execution_path_matching(path, arguments, expected):
    assert covers(Path(path), f"python -m pytest {arguments} -q") is expected
