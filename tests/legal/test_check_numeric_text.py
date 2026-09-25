"""Review finding 4: an identifier cleanup must not change numeric data.

A JSON file re-serialised during the C11 cleanup changed two floats in their
last digit (``...212`` became ``...213``), and two timing artefacts were
rewritten by a test run. ``scripts/legal/check_numeric_text.py`` compares the
numeric tokens of each changed data file between two refs and fails on any
difference, so a redaction commit can prove it only touched text.
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "legal" / "check_numeric_text.py"

_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")


def _env():
    return {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}


def _git(repo: Path, *args: str) -> str:
    out = subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, env=_env()
    )
    assert out.returncode == 0, out.stderr
    return out.stdout.strip()


@pytest.fixture()
def repo(tmp_path):
    r = tmp_path / "r"
    r.mkdir()
    _git(r, "init", "-q")
    _git(r, "config", "user.email", "t@example.com")
    _git(r, "config", "user.name", "t")
    _git(r, "config", "core.autocrlf", "false")
    return r


def _commit(repo: Path, files: dict[str, str]) -> str:
    for name, text in files.items():
        p = repo / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8", newline="\n")
    _git(repo, "add", "-A")
    _git(repo, "commit", "-q", "-m", "c")
    return _git(repo, "rev-parse", "HEAD")


def _run(repo: Path, *args: str):
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args],
        cwd=repo,
        capture_output=True,
        text=True,
        env=_env(),
    )


def test_a_float_repr_change_fails(repo):
    base = _commit(repo, {"a.json": '{"x": [106.06601717798212, 2]}\n'})
    head = _commit(repo, {"a.json": '{"x": [106.06601717798213, 2]}\n'})
    out = _run(repo, base, head)
    assert out.returncode == 1, out.stdout
    assert "a.json" in out.stdout


def test_a_text_only_change_passes(repo):
    base = _commit(repo, {"m.yml": "# File: old/place/x.yml\nname: alpha\nL: 12.5\n"})
    head = _commit(repo, {"m.yml": "# File: new/x.yml\nname: vessel-a\nL: 12.5\n"})
    out = _run(repo, base, head)
    assert out.returncode == 0, out.stdout


def test_a_number_inside_a_redacted_path_is_not_data(repo):
    bs = "\\"
    old = "Include: " + "Q:" + bs + "9999 job" + bs + "run3.yml\nL: 1.5\n"
    base = _commit(repo, {"m.yml": old})
    head = _commit(repo, {"m.yml": "Include: <redacted>" + bs + "run.yml\nL: 1.5\n"})
    out = _run(repo, base, head)
    assert out.returncode == 0, out.stdout


def test_a_csv_value_change_fails(repo):
    base = _commit(repo, {"d.csv": "a,b\n1,2.25\n"})
    head = _commit(repo, {"d.csv": "a,b\n1,2.5\n"})
    out = _run(repo, base, head)
    assert out.returncode == 1, out.stdout


def test_removed_integers_can_be_allowed_for_a_redaction(repo):
    base = _commit(repo, {"c.json": '{"crane": "Lifter 5000", "swl": 12.5}\n'})
    head = _commit(repo, {"c.json": '{"crane": "Crane-A", "swl": 12.5}\n'})
    assert _run(repo, base, head).returncode == 1
    out = _run(repo, base, head, "--allow-removed-integers")
    assert out.returncode == 0, out.stdout


def test_allowing_removed_integers_still_catches_a_changed_float(repo):
    base = _commit(repo, {"c.json": '{"crane": "Lifter 5000", "swl": 12.5}\n'})
    head = _commit(repo, {"c.json": '{"crane": "Crane-A", "swl": 12.6}\n'})
    out = _run(repo, base, head, "--allow-removed-integers")
    assert out.returncode == 1, out.stdout


def test_files_other_than_data_are_ignored(repo):
    base = _commit(repo, {"n.md": "value 1.0\n"})
    head = _commit(repo, {"n.md": "value 2.0\n"})
    assert _run(repo, base, head).returncode == 0


def test_a_bad_ref_is_an_error(repo):
    _commit(repo, {"a.json": "{}\n"})
    out = _run(repo, "no-such-ref", "HEAD")
    assert out.returncode not in (0, 1)
