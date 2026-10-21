# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate the informational flaky register; it never selects tests."""

from datetime import date
from pathlib import Path
import re

import pytest
import yaml


def test_flaky_register_schema():
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    assert path.is_file()
    rows = yaml.safe_load(path.read_text())
    assert isinstance(rows, list)
    assert len({row["id"] for row in rows}) == len(rows)
    for row in rows:
        expected_keys = {"id", "kind", "first_seen", "last_seen", "evidence", "cause", "fix_pr", "status"}
        if row["status"] == "fixed":
            expected_keys.add("passing_evidence")
        assert set(row) == expected_keys
        assert isinstance(row["id"], str) and row["id"]
        assert row["kind"] in {"retry-pass", "same-tree-mismatch", "signal-exit", "repeated-failure"}
        assert row["status"] in {"open", "fixed"}
        assert date.fromisoformat(str(row["first_seen"])) <= date.fromisoformat(str(row["last_seen"]))
        assert isinstance(row["evidence"], list) and row["evidence"]
        assert all(url.startswith("https://github.com/") and "/actions/runs/" in url for url in row["evidence"])
        assert isinstance(row["cause"], str) and row["cause"]
        assert row["fix_pr"] == "pending" or (isinstance(row["fix_pr"], int) and row["fix_pr"] > 0)
        if row["status"] == "fixed":
            assert type(row["fix_pr"]) is int and row["fix_pr"] > 0
            assert isinstance(row["passing_evidence"], list) and row["passing_evidence"]
            assert all(isinstance(url, str) and re.fullmatch(
                r"https://api\.github\.com/repositories/[0-9]+/actions/runs/[0-9]+",
                url,
            ) for url in row["passing_evidence"])


def test_flaky_register_accepts_additional_entries(monkeypatch):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows.append(dict(rows[0], id="tests/test_example.py::test_new"))
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    test_flaky_register_schema()


@pytest.mark.parametrize("status", ["open", "fixed"])
def test_flaky_register_accepts_valid_statuses(monkeypatch, status):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows[0]["status"] = status
    if status == "fixed":
        rows[0]["fix_pr"] = 1
        rows[0]["passing_evidence"] = [
            "https://api.github.com/repositories/1/actions/runs/2"
        ]
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    test_flaky_register_schema()


def test_flaky_register_rejects_fixed_without_passing_evidence(monkeypatch):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows[0]["status"] = "fixed"
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    with pytest.raises(AssertionError):
        test_flaky_register_schema()


@pytest.mark.parametrize("passing_evidence", [
    [], None, "not-a-list", ["not-a-run-url"], [1],
    ["https://api.github.com/repositories/1/actions/jobs/2"],
])
def test_flaky_register_rejects_invalid_passing_evidence(monkeypatch, passing_evidence):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows[0].update(status="fixed", fix_pr=1, passing_evidence=passing_evidence)
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    with pytest.raises(AssertionError):
        test_flaky_register_schema()


@pytest.mark.parametrize("fix_pr", ["pending", "1", 0, -1, True])
def test_flaky_register_rejects_fixed_without_integer_fix(monkeypatch, fix_pr):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows[0].update(status="fixed", fix_pr=fix_pr, passing_evidence=[
        "https://api.github.com/repositories/1/actions/runs/2"
    ])
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    with pytest.raises(AssertionError):
        test_flaky_register_schema()


def test_repeated_failure_without_passing_evidence_remains_open():
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    row = yaml.safe_load(path.read_text())[0]
    assert row["kind"] == "repeated-failure"
    assert row["status"] == "open"
