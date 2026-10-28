# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Test-only pytest selection contract, extracted from the corrosion guard."""

import fnmatch
import posixpath
import shlex
from collections.abc import Iterable

import pytest


def _covers(parent: str, path: str) -> bool:
    repository_ancestor = parent == "." or all(part == ".." for part in parent.split("/"))
    return repository_ancestor or path == parent or path.startswith(parent + "/")


def pytest_selects_target(
    pytestconfig: pytest.Config,
    command: str,
    target: str,
    candidate_test_files: Iterable[str],
    *,
    require_explicit_target: bool = False,
) -> bool:
    """Require complete positional coverage with no overlapping exclusions.

    Use the active pytest parser so registered options consume their values
    and unknown options remain errors. Paths are relative to the repository,
    as in the gate commands. A node operand cannot cover an entire target.
    Exact-registration callers retain their explicit operand requirement.
    Candidate files let globs such as ``*.py`` match inside a folder target.
    """
    command_tokens = shlex.split(command.replace("\\\n", ""))
    pytest_args = command_tokens[command_tokens.index("pytest") + 1:]
    parsed = pytestconfig._parser.parse(pytest_args)
    target = posixpath.normpath(target)
    operands = {
        posixpath.normpath(item) for item in parsed.file_or_dir if "::" not in item
    }
    if require_explicit_target:
        selected = target in operands
    else:
        selected = any(_covers(operand, target) for operand in operands)
    if not selected:
        return False

    required_paths = {target, *(posixpath.normpath(path) for path in candidate_test_files)}
    exclusions = (parsed.ignore or []) + (parsed.ignore_glob or []) + (parsed.deselect or [])
    for value in exclusions:
        excluded = posixpath.normpath(value.split("::", 1)[0])
        if (
            _covers(excluded, target) or _covers(target, excluded)
            or any(fnmatch.fnmatchcase(path, excluded) for path in required_paths)
        ):
            return False
    # Pytest deselects by node-id prefix, including a partial file name.
    for value in parsed.deselect or []:
        excluded = posixpath.normpath(value.split("::", 1)[0])
        if any(path.startswith(excluded) for path in required_paths):
            return False
    return True
