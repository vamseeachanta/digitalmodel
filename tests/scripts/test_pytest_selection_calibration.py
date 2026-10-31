# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent argv examples from issue 3449 for folder and file coverage.

Expected booleans follow positional selection and exclusion semantics: an
option value never selects tests, and removing any required path or node
invalidates full coverage. No CI result supplies these expectations.
"""

import os
import shlex
import subprocess
import sys

import pytest

from tests.scripts._pytest_selection import pytest_selects_target


@pytest.mark.parametrize("target,candidates", [
    ("tests/reporting/", [
        "tests/reporting/test_calc_report.py",
        "tests/reporting/test_engine_golden.py",
    ]),
    ("tests/reporting/test_calc_report.py", ["tests/reporting/test_calc_report.py"]),
], ids=["folder", "file"])
@pytest.mark.parametrize("template,expected", [
    # Explicit operands select the complete target, including normalized paths.
    ("{target}", True),
    ("./{target}", True),
    ("\\\n {target}", True),
    ("tests/./reporting/../{suffix}", True),
    ("{target} --ignore tests/unrelated/", True),
    ("{target} --ignore=tests/unrelated/", True),
    ("{target} --ignore-glob=tests/unrelated/*", True),
    ("{target} --deselect tests/unrelated/test_example.py::test_example", True),
    ("{target} --ignore tests/reporting_extra/", True),
    ("tests/reporting/", True),
    # The guarded path occurs only as an option value, not an operand.
    ("tests/unrelated/ --ignore {target}", False),
    ("tests/unrelated/ --ignore={target}", False),
    ("tests/unrelated/ --rootdir {target}", False),
    ("tests/unrelated/ --rootdir={target}", False),
    # Pytest requires a directory value here, even for a guarded file.
    ("tests/unrelated/ --confcutdir tests/reporting/", False),
    ("tests/unrelated/ --confcutdir=tests/reporting/", False),
    ("tests/reporting_extra/", False),
    # Parent, exact and child exclusions each remove required coverage.
    ("{target} --ignore tests/", False),
    ("{target} --ignore .", False),
    ("{target} --ignore=..", False),
    ("{target} --ignore {target}", False),
    ("{target} --ignore={target}", False),
    ("{target} --ignore ./tests/./reporting/", False),
    ("{target} --ignore=tests/reporting/test_calc_report.py", False),
    ("{target} --ignore-glob tests/*", False),
    ("{target} --ignore-glob=tests/*", False),
    ("tests/ --ignore-glob tests/reportin?", False),
    ("tests/ --ignore-glob=tests/reportin?", False),
    ("tests/ --ignore-glob=tests/reportin?_extra", True),
    ("{target} --ignore-glob *.py", False),
    ("{target} --ignore-glob=*.py", False),
    ("{target} --ignore-glob=./tests/reporting/test_*.py", False),
    ("{target} --deselect tests/reporting/test_calc_report.py", False),
    ("{target} --deselect=tests/reporting/test_calc_report.py::test_required", False),
    ("{target} --deselect ./tests/reporting/test_calc_report.py::test_required", False),
    ("{target} --deselect=tests/reporting/test_calc", False),
    # A single node cannot stand for an entire required file or folder.
    ("tests/reporting/test_calc_report.py::test_required", False),
    ("", False),
])
def test_positional_selection_and_exclusions(
    target, candidates, template, expected, pytestconfig,
):
    args = template.format(target=target, suffix=target.removeprefix("tests/"))
    assert pytest_selects_target(
        pytestconfig, f"python -m pytest {args}", target, candidates,
    ) is expected


def test_unknown_option_is_refused(pytestconfig):
    with pytest.raises(pytest.UsageError, match="unrecognized arguments"):
        pytest_selects_target(
            pytestconfig,
            "python -m pytest tests/reporting/ --unknown-selection-option tests/reporting/",
            "tests/reporting/", ["tests/reporting/test_calc_report.py"],
        )


@pytest.mark.parametrize("require_explicit,expected", [(False, True), (True, False)])
def test_explicit_registration_keeps_its_operand_requirement(
    pytestconfig, require_explicit, expected,
):
    # The parent covers the folder, but cannot replace an explicit registration.
    assert pytest_selects_target(
        pytestconfig, "python -m pytest tests/", "tests/reporting/",
        ["tests/reporting/test_calc_report.py"],
        require_explicit_target=require_explicit,
    ) is expected


@pytest.mark.parametrize("target", [
    "tests/reporting/",
    "tests/reporting/nested/test_example.py",
], ids=["folder", "file"])
@pytest.mark.parametrize("exclusion,expected_nodes", [
    ("", ["tests/reporting/nested/test_example.py::test_example"]),
    ("--ignore-glob tests/reportin?", []),
    ("--ignore-glob=tests/reportin?", []),
    ("--ignore-glob=tests/reporting/neste?", []),
    ("--ignore-glob=tests/reportin?_extra", [
        "tests/reporting/nested/test_example.py::test_example",
    ]),
])
def test_ancestor_globs_match_real_collection(
    tmp_path, pytestconfig, target, exclusion, expected_nodes,
):
    # Pruning either directory removes the sole test; unrelated globs keep it.
    candidate = "tests/reporting/nested/test_example.py"
    test_file = tmp_path / candidate
    test_file.parent.mkdir(parents=True)
    test_file.write_text(
        "def test_example():\n    assert True\n", encoding="utf-8", newline="\n",
    )
    (tmp_path / "pytest.ini").write_text(
        "[pytest]\n", encoding="utf-8", newline="\n",
    )
    env = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    args = ["tests/", *shlex.split(exclusion)]
    result = subprocess.run(
        [sys.executable, "-m", "pytest", *args, "--collect-only", "-q",
         "-p", "no:cacheprovider"],
        cwd=tmp_path, env=env, capture_output=True, text=True, encoding="utf-8",
        timeout=30,
    )
    assert result.returncode == (0 if expected_nodes else 5), result.stdout + result.stderr
    collected = [line for line in result.stdout.splitlines() if "::" in line]
    assert collected == expected_nodes
    assert pytest_selects_target(
        pytestconfig, f"python -m pytest tests/ {exclusion}", target, [candidate],
    ) is bool(expected_nodes)
