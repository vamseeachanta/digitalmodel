# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate the informational flaky register; it never selects tests."""

from datetime import date
from pathlib import Path

import yaml


def test_flaky_register_schema():
    path = Path(__file__).resolve().parents[2] / ".github/ci/flaky-register.yml"
    assert path.is_file()
    rows = yaml.safe_load(path.read_text())
    assert isinstance(rows, list) and len(rows) == 2
    assert len({row["id"] for row in rows}) == len(rows)
    for row in rows:
        assert set(row) == {"id", "kind", "first_seen", "last_seen", "evidence", "cause", "fix_issue", "status"}
        assert isinstance(row["id"], str) and row["id"]
        assert row["kind"] in {"retry-pass", "same-tree-mismatch", "signal-exit"}
        assert row["status"] in {"open", "fixed"}
        assert date.fromisoformat(str(row["first_seen"])) <= date.fromisoformat(str(row["last_seen"]))
        assert isinstance(row["evidence"], list) and row["evidence"]
        assert all(url.startswith("https://github.com/") and "/actions/runs/" in url for url in row["evidence"])
        assert isinstance(row["cause"], str) and row["cause"]
        assert row["fix_issue"] == "pending" or (isinstance(row["fix_issue"], int) and row["fix_issue"] > 0)
