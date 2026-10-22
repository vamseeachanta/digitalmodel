# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Execution assignments must cover declared roots and every pytest file."""

import glob
import os
import re
import shlex
import importlib.util
from functools import lru_cache
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
GATES = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
SHARDS = {name: gate for name, gate in GATES.items() if gate.get("domain")}
spec = importlib.util.spec_from_file_location("domain_gate", ROOT / "scripts/ci/domain_gate.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


@lru_cache(maxsize=None)
def pytest_paths(command, root=ROOT):
    """Read whole-file selections; reject unsupported collection modifiers."""
    args = shlex.split(command)
    args = args[args.index("pytest") + 1 :]
    roots, ignored = [], []
    flags = {"-q", "-v", "--no-header", "--strict-markers", "--import-mode=importlib"}
    value_options = {"-p", "--tb", "--junitxml", "--basetemp"}
    while args:
        arg = args.pop(0)
        if arg == "--ignore":
            ignored.append(Path(args.pop(0)))
        elif arg.startswith("--ignore="):
            ignored.append(Path(arg.split("=", 1)[1]))
        elif arg == "-o":
            value = args.pop(0)
            if value != "junit_family=xunit2":
                raise ValueError(f"Unsupported pytest override: {value}")
        elif arg in value_options:
            value = args.pop(0)
            if arg == "-p" and value not in {"no:randomly", "no:sugar", "no:cacheprovider"}:
                raise ValueError(f"Unsupported pytest plugin: {value}")
        elif arg in flags or arg == "-rfE":
            continue
        elif any(arg.startswith(option + "=") for option in value_options if option != "-p"):
            continue
        elif arg.startswith("tests/"):
            # A node ID exercises only part of a file, so cannot assign that file.
            if "::" not in arg:
                paths = glob.glob(arg, root_dir=root) if glob.has_magic(arg) else [arg]
                roots.extend(Path(path) for path in paths)
        else:
            raise ValueError(f"Unsupported pytest selection option: {arg}")
    return roots, ignored


def covers(path, command):
    roots, ignored = pytest_paths(command)
    return any(path == root or root in path.parents for root in roots) and not any(
        path == root or root in path.parents for root in ignored
    )


def unassigned_files(files, commands, exceptions=()):
    """Return PR assignment gaps without accepting sweep-only coverage."""
    return [path for path in files if path not in exceptions and
            not any(covers(Path(path), command) for command in commands)]


def test_declared_test_roots_are_executed():
    """Existing routed tests need matching pytest arguments, not metadata alone."""
    missing = []
    for line in (ROOT / "tests/DOMAINS.md").read_text().splitlines():
        cells = line.split("|")
        if len(cells) < 4 or (name := "tests-" + cells[1].strip()) not in SHARDS:
            continue
        roots, _ = pytest_paths(gate.command_for(name, ROOT))
        for root in re.findall(r"`(tests/[^`]+)`", cells[2]):
            if (ROOT / root).exists() and root.endswith(("/", ".py")) and Path(root) not in roots:
                missing.append(f"{name}: {root}")
    assert not missing, f"{len(missing)} missing root assignments:\n" + "\n".join(missing)


def test_every_test_file_has_an_execution_assignment():
    """PR commands must cover every file or name a reviewed exception."""
    policy = gate.read_policy(ROOT)
    commands = []
    for name, shard in SHARDS.items():
        assert shard["enabled"], name
        commands.append(gate.command_for(name, ROOT))
    files = sorted(gate.test_files(ROOT))
    assert files, "No test files found"
    exceptions = set(policy["held"]) | set(policy["collection"])
    missing = unassigned_files(files, commands, exceptions)
    assert not missing, f"{len(missing)} unassigned PR test files:\n" + "\n".join(missing)
    for path, entry in policy["held"].items():
        assert not any(covers(Path(path), cmd) for cmd in commands), path
        assert covers(Path(path), gate.command_for(entry["gate"], ROOT, sweep=True)), path


def test_collection_exclusions_are_explicit_and_reasoned():
    policy = gate.read_policy(ROOT)
    actual = gate.collection_exclusions(ROOT)
    assert actual == set(policy["collection"]), (
        f"Unrecorded collection exclusions: {actual - set(policy['collection'])}; "
        f"stale exceptions: {set(policy['collection']) - actual}"
    )


def test_exception_registry_is_shrink_only_against_trusted_base():
    if ref := os.environ.get("GATE_POLICY_BASE"):
        gate.check_trusted_policy(ROOT, ref)


def test_cathodic_files_are_assigned_exactly_once():
    for path in (
        "tests/benchmarks/test_cp_benchmarks.py",
        "tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py",
    ):
        owners = [name for name in SHARDS if covers(Path(path), gate.command_for(name, ROOT))]
        assert owners == ["tests-cathodic-protection"]


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


def test_pytest_paths_expands_shell_globs(tmp_path):
    directory = tmp_path / "tests" / "module"
    directory.mkdir(parents=True)
    (directory / "test_one.py").write_text("")
    (directory / "helper.py").write_text("")
    roots, _ = pytest_paths("python -m pytest tests/module/test_*.py", root=tmp_path)
    assert roots == [Path("tests/module/test_one.py")]


@pytest.mark.parametrize("selection", [
    "--ignore-glob=tests/a/*", "--deselect=tests/a/test_one.py::test_one",
    "-k one", "-m unit", "--pyargs tests", "--unknown-option",
])
def test_unmodelled_selection_options_fail_closed(selection):
    with pytest.raises(ValueError, match="Unsupported"):
        pytest_paths(f"python -m pytest tests/ {selection}")


def test_new_unassigned_file_and_removed_root_fail_coverage():
    commands = [gate.command_for(name, ROOT) for name in SHARDS]
    assert unassigned_files(["tests/unassigned/test_new.py"], commands) == [
        "tests/unassigned/test_new.py"
    ]
    code_checks = sorted(path for path in gate.test_files(ROOT) if path.startswith("tests/code_checks/"))
    assert code_checks
    assert not unassigned_files(code_checks, commands)
    stripped = [command.replace("tests/code_checks/", "") for command in commands]
    assert unassigned_files(code_checks, stripped) == code_checks


def test_sweep_only_path_is_not_a_pr_assignment():
    path = "tests/a/test_one.py"
    assert unassigned_files([path], ["python -m pytest tests/b/"]) == [path]
    assert unassigned_files([path], ["python -m pytest tests/a/"]) == []


def test_junit_suffix_preserves_assignment():
    command = "python -m pytest tests/a/ --junitxml=reports/junit.xml -o junit_family=xunit2"
    assert covers(Path("tests/a/test_one.py"), command)


def test_pytest_configuration_does_not_hide_test_selections():
    import configparser

    config = configparser.ConfigParser(interpolation=None)
    config.read(ROOT / "pytest.ini")
    assert config["pytest"]["python_files"].split() == ["test_*.py", "*_test.py"]
    pytest_paths("python -m pytest tests/ " + config["pytest"]["addopts"])
