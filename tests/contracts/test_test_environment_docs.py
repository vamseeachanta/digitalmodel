# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep contributor test commands aligned with the declared test environment."""

import re
import shlex
import tomllib
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SMOKE_PATH = "tests/contracts/test_test_environment_docs.py"


def testing_block():
    """Read only the README section that supplies contributor test commands."""
    text = (ROOT / "README.md").read_text(encoding="utf-8")
    return text.split("## Testing\n", 1)[1].split("\n## ", 1)[0]


def pytest_commands(text):
    """Extract complete single-line pytest invocations without executing them."""
    return [
        shlex.split(line.strip().removeprefix("@"))
        for line in text.splitlines()
        if "python -m pytest" in line and not line.lstrip().startswith("#")
    ]


def assert_complete_extra(command):
    """Require the full extra and the same explicit source choice as CI."""
    assert command[:2] == ["uv", "run"]
    index = command.index("--with-editable")
    assert "--no-sources" in command[:index]
    assert command[index + 1] == ".[test]"
    return index


def assert_contributor_environment(command):
    """Keep contributor runs outside project resolution on the CI interpreter."""
    index = assert_complete_extra(command)
    uv_options = command[:index]
    assert "--no-project" in uv_options
    python_index = uv_options.index("--python")
    assert command[python_index + 1] == "3.11"
    return index


def assert_test_environment(command):
    """Require an isolated contributor environment running pytest."""
    index = assert_contributor_environment(command)
    assert command[index + 2 : index + 5] == ["python", "-m", "pytest"]


def recipe(target):
    """Read a Makefile target's recipes, stopping at the next non-recipe line."""
    text = (ROOT / "Makefile").read_text(encoding="utf-8")
    match = re.search(rf"^{re.escape(target)}:[^\n]*\n((?:\t[^\n]*\n)+)", text, re.M)
    assert match is not None, f"missing recipe: {target}"
    return match.group(1)


def test_readme_smoke_uses_complete_test_environment():
    commands = pytest_commands(testing_block())
    assert commands, "Testing must include a runnable pytest command"
    for command in commands:
        assert_test_environment(command)
    smoke = [command for command in commands if SMOKE_PATH in command]
    assert len(smoke) == 1
    assert "-q" in smoke[0]
    assert "no:cacheprovider" in smoke[0]


def test_readme_retains_ci_extra_and_source_choice():
    workflow = (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(
        encoding="utf-8"
    )
    match = re.search(r"^\s+(uv run[^\n]+python -m pytest) \\\s*$", workflow, re.M)
    assert match is not None, "CI must declare its pytest invocation"
    ci_command = shlex.split(match.group(1))
    index = assert_complete_extra(ci_command)
    assert ci_command[index + 2 :] == ["python", "-m", "pytest"]
    smoke = [
        command for command in pytest_commands(testing_block()) if SMOKE_PATH in command
    ]
    assert len(smoke) == 1
    assert_test_environment(smoke[0])
    contributor_prefix = smoke[0][: smoke[0].index("pytest") + 1]
    contributor_prefix.remove("--no-project")
    python_index = contributor_prefix.index("--python")
    del contributor_prefix[python_index : python_index + 2]
    assert contributor_prefix == ci_command


def test_agent_test_command_uses_complete_test_environment():
    text = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
    commands = re.findall(r"^test_command: (.+)$", text, re.M)
    assert len(commands) == 1
    assert_test_environment(shlex.split(commands[0]))
    assert len(text.splitlines()) <= 20


def test_make_test_installs_extra_and_accepts_bounded_arguments():
    commands = pytest_commands(recipe("test"))
    assert len(commands) == 1
    assert_test_environment(commands[0])
    assert "$(TEST_ARGS)" in commands[0]
    assert "auto" not in commands[0]
    assert re.search(
        r"^TEST_ARGS\s*\?=\s*tests/",
        (ROOT / "Makefile").read_text(encoding="utf-8"),
        re.M,
    )


def test_make_install_test_supplies_declared_extra():
    text = (ROOT / "Makefile").read_text(encoding="utf-8")
    phony = re.findall(r"^\.PHONY:\s*(.+)$", text, re.M)
    assert "install-test" in " ".join(phony).split()
    commands = [
        shlex.split(line.strip().removeprefix("@"))
        for line in recipe("install-test").splitlines()
        if "uv " in line
    ]
    assert len(commands) == 1
    index = assert_contributor_environment(commands[0])
    assert commands[0][index + 2 : index + 4] == ["python", "-c"]
    config = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))
    extra = config["project"]["optional-dependencies"]["test"]
    assert any(requirement.startswith("pytest>=") for requirement in extra)


@pytest.mark.parametrize("replacement", ["", "--with-editable '.[dev]'"])
def test_missing_or_dev_only_extra_is_rejected(replacement):
    command = (
        f"uv run --no-project --python 3.11 --no-sources {replacement} python -m pytest"
    )
    with pytest.raises((AssertionError, ValueError)):
        assert_test_environment(shlex.split(command))


def test_implicit_source_choice_is_rejected():
    with pytest.raises(AssertionError):
        assert_test_environment(
            shlex.split(
                "uv run --no-project --python 3.11 "
                "--with-editable '.[test]' python -m pytest"
            )
        )


def test_project_resolution_is_rejected():
    with pytest.raises(AssertionError):
        assert_test_environment(
            shlex.split(
                "uv run --python 3.11 --no-sources "
                "--with-editable '.[test]' python -m pytest"
            )
        )


@pytest.mark.parametrize("python_option", ["", "--python 3.12"])
def test_unpinned_or_different_python_is_rejected(python_option):
    with pytest.raises((AssertionError, ValueError)):
        assert_test_environment(
            shlex.split(
                f"uv run --no-project {python_option} --no-sources "
                "--with-editable '.[test]' python -m pytest"
            )
        )


@pytest.mark.parametrize(
    ("uv_options", "pytest_options"),
    [
        ("--python 3.11 --no-sources", "--no-project"),
        ("--no-project --no-sources", "--python 3.11"),
        ("--no-project --python 3.11", "--no-sources"),
    ],
)
def test_uv_options_passed_to_pytest_are_rejected(uv_options, pytest_options):
    command = (
        f"uv run {uv_options} --with-editable '.[test]' "
        f"python -m pytest {pytest_options}"
    )
    with pytest.raises((AssertionError, ValueError)):
        assert_test_environment(shlex.split(command))
