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
        check=True, capture_output=True, text=True, encoding="utf-8", env=env,
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
    anchor = location["anchor"]
    assert anchor
    frozen = git_text(commit, path.as_posix()).splitlines()
    assert line <= len(frozen), f"pinned anchor out of bounds: {path}:{line}"
    assert frozen[line - 1].strip() == anchor, f"pinned anchor changed: {path}:{line}"
    # Line numbers describe the pin. Unrelated edits may move an intact anchor.
    for revision, text in (
        ("HEAD", git_text("HEAD", path.as_posix())),
        ("working tree", (ROOT / path).read_text(encoding="utf-8")),
    ):
        assert anchor in {item.strip() for item in text.splitlines()}, (
            f"review anchor missing at {revision}: {path}:{line}"
        )


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


def review_section(topic):
    text = REVIEW.with_suffix(".md").read_text(encoding="utf-8")
    return text.split(f"### {topic}\n", 1)[1].split("\n### ", 1)[0]


def test_f103_label_claim_names_field_joint_section_assertions(rows):
    row = next(row for row in rows if row["id"] == "f103-labels")
    for edition in ("2010", "2019"):
        reference = (
            "tests/cathodic_protection/test_f103_tables.py::"
            f"test_field_joint_coating_constants_match_table_a2_{edition}"
        )
        assert reference in row["test"]
        assert f"[`{reference}`]" in review_section(row["id"])


def test_retrofit_clause_gap_is_explicit_in_both_records(rows):
    row = next(row for row in rows if row["id"] == "b401-report-retrofit")
    assert row["status"] == "FAIL"
    assert any("retrofit clause" in item.lower() for item in row["missing"])
    section = review_section(row["id"])
    assert "**FAIL — B401;" in section
    assert row["coverage"] in section
    for item in row["missing"]:
        assert item in section


def test_markdown_dispositions_match_all_record_rows(rows):
    text = REVIEW.with_suffix(".md").read_text(encoding="utf-8")
    summary = re.search(
        r"contains (\d+) entries: (\d+) PASS and\s+(\d+) FAIL", text
    )
    assert summary is not None
    assert tuple(map(int, summary.groups())) == (
        len(rows),
        sum(row["status"] == "PASS" for row in rows),
        sum(row["status"] == "FAIL" for row in rows),
    )
    for row in rows:
        register = (
            f"| [{row['id']}](#{row['id']}) | {row['code']} | "
            f"{row['kind']} | {row['status']} |"
        )
        assert register in text
        assert f"**{row['status']} — {row['code']};" in review_section(row["id"])


@pytest.fixture
def location_snapshot(tmp_path, monkeypatch):
    """Provide independent pinned, committed and working source snapshots."""
    filename = "src/digitalmodel/cathodic_protection/reviewed_path.py"
    source = tmp_path / filename
    source.parent.mkdir(parents=True)
    frozen = "# context\ndef reviewed_path():\n    pass\n"
    source.write_text(frozen, encoding="utf-8")
    snapshots = {"pinned": frozen, "HEAD": frozen}
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    monkeypatch.setitem(globals(), "git_text", lambda revision, path: snapshots[revision])
    location = {"file": filename, "line": 2, "anchor": "def reviewed_path():"}
    return location, snapshots, source


@pytest.mark.parametrize("changed_snapshot", ["HEAD", "working", "both"])
def test_location_allows_unrelated_edits_and_line_shifts(location_snapshot, changed_snapshot):
    location, snapshots, source = location_snapshot
    changed = "# added context\n" + snapshots["pinned"] + "# unrelated addition\n"
    if changed_snapshot in {"HEAD", "both"}:
        snapshots["HEAD"] = changed
    if changed_snapshot in {"working", "both"}:
        source.write_text(changed, encoding="utf-8")
    check_location(location, "pinned")


@pytest.mark.parametrize("changed_snapshot", ["pinned", "HEAD", "working", "pinned-line"])
def test_location_rejects_changed_anchor_or_wrong_pinned_line(location_snapshot, changed_snapshot):
    location, snapshots, source = location_snapshot
    changed = snapshots["pinned"].replace("def reviewed_path():", "def other_path():")
    if changed_snapshot == "working":
        source.write_text(changed, encoding="utf-8")
    elif changed_snapshot == "pinned-line":
        location["line"] = 1
    else:
        snapshots[changed_snapshot] = changed
    with pytest.raises(AssertionError):
        check_location(location, "pinned")
