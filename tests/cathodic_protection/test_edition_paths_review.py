# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validate the frozen edition-path review without importing CP calculators."""

from __future__ import annotations

import ast
from functools import lru_cache
import json
import os
from pathlib import Path, PurePosixPath
import re
import subprocess

import pytest

ROOT = Path(__file__).resolve().parents[2]
REVIEW = ROOT / "docs/domains/cathodic_protection/reviews/edition-paths-review.json"


@lru_cache
def git_text(revision: str, path: str) -> str:
    """Read repository bytes without inherited repository bindings."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    return subprocess.run(
        ["git", "-C", str(ROOT), "show", f"{revision}:{path}"],
        check=True, capture_output=True, text=True, env=env,
    ).stdout


def no_duplicate_keys(pairs):
    """Reject duplicate object keys before JSON parsing discards evidence."""
    result = {}
    for key, value in pairs:
        assert key not in result, f"duplicate JSON key: {key}"
        result[key] = value
    return result


@pytest.fixture
def rows():
    return json.loads(REVIEW.read_text(encoding="utf-8"),
                      object_pairs_hook=no_duplicate_keys)


def check_location(location, commit):
    path = PurePosixPath(location["file"])
    assert not path.is_absolute() and ".." not in path.parts
    assert path.parts[:3] == ("src", "digitalmodel", "cathodic_protection")
    line = location["line"]
    assert type(line) is int and line > 0
    frozen = git_text(commit, str(path))
    assert frozen == git_text("HEAD", str(path)), f"review needs refresh: {path}"
    assert frozen == (ROOT / path).read_text(encoding="utf-8")
    assert frozen.splitlines()[line - 1].strip() == location["anchor"]
    assert location["anchor"]


def test_json_parses_and_has_review_rows(rows):
    assert isinstance(rows, list) and rows
    assert len({row["id"] for row in rows}) == len(rows)
    required = {"id", "code", "editions", "file", "line", "commit", "test",
                "label", "status", "kind", "difference", "missing", "anchor"}
    for row in rows:
        assert required <= row.keys()
        assert re.fullmatch(r"[0-9a-f]{40}", row["commit"])
        assert row["code"] in {"B401", "F103", "ABS"}
        assert row["kind"] in {"difference", "routing", "shared", "scope"}
        assert row["editions"] and all(isinstance(ed, str) for ed in row["editions"])
        assert row["difference"] and isinstance(row["test"], list)
        assert row["status"] in {"PASS", "FAIL"}
        if row["status"] == "PASS":
            assert row["test"] and row["label"] and not row["missing"]
        else:
            assert row["missing"], f"FAIL needs a missing item: {row['id']}"


def test_evidence_locations_match_pinned_commit_and_head(rows):
    for row in rows:
        check_location(row, row["commit"])
        for location in row.get("evidence", []):
            check_location(location, row["commit"])


def test_pass_rows_name_existing_tests(rows):
    for row in rows:
        for reference in row["test"]:
            filename, *names = reference.split("::")
            path = PurePosixPath(filename)
            assert not path.is_absolute() and ".." not in path.parts
            assert path.parts[:2] == ("tests", "cathodic_protection")
            assert names and names[-1].startswith("test_")
            for revision in (row["commit"], "HEAD"):
                tree = ast.parse(git_text(revision, filename))
                for name in names:
                    matches = [node for node in tree.body
                               if isinstance(node, (ast.ClassDef, ast.FunctionDef,
                                                    ast.AsyncFunctionDef))
                               and node.name == name]
                    assert len(matches) == 1, f"missing test: {reference}"
                    tree = matches[0]


def test_required_topics_are_recorded(rows):
    topics = {row["id"] for row in rows}
    assert {"b401-category-iv", "b401-anode-temperature", "b401-driving-voltage",
            "b401-retrofit", "b401-labels", "f103-linepipe", "f103-field-joints",
            "f103-temperature-bands", "f103-ambient", "f103-bank-equations",
            "b401-current-densities", "b401-utilisation", "b401-resistance",
            "b401-design-life", "abs-single-edition"} <= topics


def test_duplicate_keys_are_rejected():
    with pytest.raises(AssertionError, match="duplicate JSON key"):
        json.loads('{"status": "PASS", "status": "FAIL"}',
                   object_pairs_hook=no_duplicate_keys)
