# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the module-report retention contract from issue #3357 using Git.

The disposable repository follows test_check_include_targets.py's fixture.
Git check-ignore documents exit status zero for ignored paths and one for none;
--no-index applies the policy even when the source inventory is tracked.
"""

import os
from pathlib import Path, PurePosixPath
import subprocess

import pytest


ROOT = Path(__file__).resolve().parents[2]
REPORT_ROOT = "docs/reports/modules"
BLOCK_START = "# BEGIN shared module-report retention rules"
BLOCK_END = "# END shared module-report retention rules"


def git(repo, *args, input=None, check=True):
    """Resolve only the fixture repository and its copied ignore policy."""
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    return subprocess.run(
        ["git", "-c", f"core.excludesFile={os.devnull}", *args],
        cwd=repo, env=env, input=input, capture_output=True, text=True,
        check=check,
    )


@pytest.fixture(scope="session")
def delivered_paths():
    """Retain the actual delivered inventory, independently of ignore matches."""
    result = git(ROOT, "ls-tree", "-r", "--name-only", "-z", "HEAD",
                 "--", REPORT_ROOT)
    paths = tuple(filter(None, result.stdout.split("\0")))
    assert paths, "The module-report inventory must not be empty"
    return paths


@pytest.fixture
def repo(tmp_path, delivered_paths):
    """Copy the real policy and create every report path without an index."""
    git(tmp_path, "init")
    (tmp_path / ".gitignore").write_bytes((ROOT / ".gitignore").read_bytes())
    for name in delivered_paths:
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.touch()
    return tmp_path


def ignored_paths(repo, paths):
    """Ask Git rather than interpreting its ignore pattern language."""
    result = git(repo, "check-ignore", "--no-index", "-z", "--stdin",
                 input="\0".join(paths) + "\0", check=False)
    assert result.returncode in (0, 1), result.stderr
    return set(filter(None, result.stdout.split("\0")))


def assert_retained(repo, paths):
    """Issue #3357 requires every delivered source and report to be includable."""
    suppressed = ignored_paths(repo, paths)
    assert not suppressed, f"Retained paths suppressed: {sorted(suppressed)}"


def test_all_tracked_module_report_paths_are_includable(repo, delivered_paths):
    assert_retained(repo, delivered_paths)


def test_unlisted_generated_files_and_new_modules_stay_ignored(repo, delivered_paths):
    """Retention applies to named files, never arbitrary siblings or modules."""
    modules = {PurePosixPath(name).parent for name in delivered_paths}
    new_module = PurePosixPath(REPORT_ROOT) / "unlisted_module"
    assert new_module not in modules
    controls = {str(module / "unlisted_generated_output.html")
                for module in modules | {new_module}}
    assert controls.isdisjoint(delivered_paths)
    for name in controls:
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.touch()
    assert ignored_paths(repo, controls) == controls


def test_module_report_parent_rules_are_unique():
    """Issue #3357 requires one shared pair before all module exceptions.

    With a slash inside the pattern, Git treats the optional leading root slash
    identically. Normalize only that spelling, without interpreting wildcards.
    """
    lines = (ROOT / ".gitignore").read_text(encoding="utf-8").splitlines()
    assert lines.count(BLOCK_START) == 1
    assert lines.count(BLOCK_END) == 1
    start, end = lines.index(BLOCK_START), lines.index(BLOCK_END)
    assert start < end
    positions = {}
    for expected in (f"!{REPORT_ROOT}/", f"{REPORT_ROOT}/*"):
        matches = []
        for index, line in enumerate(lines):
            prefix = "!" if line.startswith("!") else ""
            pattern = line.removeprefix("!").removeprefix("/")
            if prefix + pattern == expected:
                matches.append(index)
        assert len(matches) == 1, (expected, matches)
        position, = matches
        assert start < position < end
        positions[expected] = position
    opening, exclusion = positions.values()
    assert opening < exclusion
    exceptions = [index for index, line in enumerate(lines)
                  if line.removeprefix("!").removeprefix("/").startswith(
                      REPORT_ROOT + "/")
                  and line.startswith("!") and index != opening]
    assert exceptions
    assert all(exclusion < index < end for index in exceptions)


@pytest.mark.parametrize("root_prefix", ["", "/"], ids=["relative", "anchored"])
def test_later_broad_rule_calibration_detects_suppressed_reports(
    repo, delivered_paths, root_prefix,
):
    """A later broad rule must break the same retained-path assertion."""
    assert_retained(repo, delivered_paths)
    policy = repo / ".gitignore"
    with policy.open("a", encoding="utf-8") as stream:
        stream.write(f"\n{root_prefix}{REPORT_ROOT}/*\n")
    assert ignored_paths(repo, delivered_paths) == set(delivered_paths)
    with pytest.raises(AssertionError, match="Retained paths suppressed"):
        assert_retained(repo, delivered_paths)
