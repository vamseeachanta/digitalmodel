# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate the informational flaky register; it never selects tests."""

from datetime import date
from pathlib import Path
import re

import yaml


def test_flaky_register_schema():
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    assert path.is_file()
    rows = yaml.safe_load(path.read_text())
    assert isinstance(rows, list) and rows
    assert len({row["id"] for row in rows}) == len(rows)
    for row in rows:
        assert set(row) == {"id", "kind", "first_seen", "last_seen", "evidence", "cause", "fix_issue", "status"}
        assert isinstance(row["id"], str) and row["id"]
        assert row["kind"] in {"retry-pass", "same-tree-mismatch", "signal-exit", "repeated-failure"}
        assert row["status"] in {"open", "fixed"}
        assert date.fromisoformat(str(row["first_seen"])) <= date.fromisoformat(str(row["last_seen"]))
        assert isinstance(row["evidence"], list) and row["evidence"]
        assert all(re.fullmatch(
            r"https://api\.github\.com/repositories/[0-9]+/actions/(?:runs|jobs)/[0-9]+",
            url,
        ) for url in row["evidence"])
        assert isinstance(row["cause"], str) and row["cause"]
        assert row["fix_issue"] == "pending" or (isinstance(row["fix_issue"], int) and row["fix_issue"] > 0)


def test_flaky_register_accepts_additional_entries(monkeypatch):
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    rows = yaml.safe_load(path.read_text())
    rows.append(dict(rows[0], id="tests/test_example.py::test_new"))
    monkeypatch.setattr(yaml, "safe_load", lambda text: rows)
    test_flaky_register_schema()


def test_repeated_failure_fix_is_recorded():
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    row = yaml.safe_load(path.read_text())[0]
    assert row["kind"] == "repeated-failure"
    assert row["status"] == "fixed"
    assert row["fix_issue"] == 2576
