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


# Review r2 finding 1: a multiset of tokens loses which field a number
# belongs to, skips renamed files, excuses a deleted field as a "removed
# integer" and reads YAML's 12_000 as 1.


def test_swapped_json_values_fail(repo):
    base = _commit(repo, {"s.json": '{"x": 1, "y": 2}\n'})
    head = _commit(repo, {"s.json": '{"x": 2, "y": 1}\n'})
    out = _run(repo, base, head)
    assert out.returncode == 1, out.stdout
    assert "s.json" in out.stdout


def test_swapped_yaml_values_fail(repo):
    base = _commit(repo, {"s.yml": "x: 1.5\ny: 2.5\n"})
    head = _commit(repo, {"s.yml": "x: 2.5\ny: 1.5\n"})
    assert _run(repo, base, head).returncode == 1


def test_swapped_csv_cells_fail(repo):
    base = _commit(repo, {"s.csv": "a,b\n1,2\n3,4\n"})
    head = _commit(repo, {"s.csv": "a,b\n3,4\n1,2\n"})
    assert _run(repo, base, head).returncode == 1


def test_reordered_json_keys_with_the_same_values_pass(repo):
    base = _commit(repo, {"s.json": '{"x": 1, "y": 2.5}\n'})
    head = _commit(repo, {"s.json": '{"y": 2.5, "x": 1}\n'})
    out = _run(repo, base, head)
    assert out.returncode == 0, out.stdout


@pytest.mark.parametrize(
    "name,before,after",
    [
        ("d.json", '{"x": 10, "y": 20}\n', '{"x": 10}\n'),
        ("d.yml", "x: 10\ny: 20\n", "x: 10\n"),
        ("d.json", '{"x": 10, "ids": [1, 2]}\n', '{"x": 10, "ids": [1]}\n'),
        ("d.csv", "a,b\n1,2\n3,4\n", "a,b\n1,2\n"),
    ],
)
def test_a_deleted_numeric_field_is_never_excused(repo, name, before, after):
    base = _commit(repo, {name: before})
    head = _commit(repo, {name: after})
    out = _run(repo, base, head, "--allow-removed-integers")
    assert out.returncode == 1, out.stdout


def test_a_numeric_value_turned_into_text_is_never_excused(repo):
    base = _commit(repo, {"d.yml": "id: 4711\nL: 2.5\n"})
    head = _commit(repo, {"d.yml": "id: vessel-a\nL: 2.5\n"})
    out = _run(repo, base, head, "--allow-removed-integers")
    assert out.returncode == 1, out.stdout


def test_a_yaml_integer_removed_from_a_string_can_be_allowed(repo):
    base = _commit(repo, {"c.yml": "crane: Lifter 5000\nswl: 12.5\n"})
    head = _commit(repo, {"c.yml": "crane: Crane-A\nswl: 12.5\n"})
    assert _run(repo, base, head).returncode == 1
    out = _run(repo, base, head, "--allow-removed-integers")
    assert out.returncode == 0, out.stdout


def test_a_decimal_removed_from_a_string_is_not_excused(repo):
    base = _commit(repo, {"c.json": '{"note": "draft 12.5 m"}\n'})
    head = _commit(repo, {"c.json": '{"note": "draft"}\n'})
    assert _run(repo, base, head, "--allow-removed-integers").returncode == 1


def _body(value: str) -> str:
    rows = "".join(f'  "k{i}": "text line {i:03d} unchanged",\n' for i in range(20))
    return "{\n" + rows + f'  "v": {value}\n' + "}\n"


def test_a_renamed_file_is_compared(repo):
    base = _commit(repo, {"old/a.json": _body("1.25")})
    (repo / "old" / "a.json").unlink()
    head = _commit(repo, {"new/a.json": _body("1.5")})
    out = _run(repo, base, head)
    assert out.returncode == 1, out.stdout
    assert "new/a.json" in out.stdout


def test_a_pure_rename_passes_and_is_counted(repo):
    base = _commit(repo, {"old/a.json": _body("1.25")})
    (repo / "old" / "a.json").unlink()
    head = _commit(repo, {"new/a.json": _body("1.25")})
    out = _run(repo, base, head)
    assert out.returncode == 0, out.stdout
    assert "1 changed data file" in out.stdout


@pytest.mark.parametrize(
    "before,after,code",
    [
        ("a: 12_000\n", "a: 13_000\n", 1),
        ("a: 12_000\nb: 13_000\n", "a: 13_000\nb: 12_000\n", 1),
        ("a: 12_000\n", "a: 12000\n", 0),
        ("a: 1_000.5\n", "a: 1_000.25\n", 1),
    ],
)
def test_yaml_underscore_numbers_are_read_whole(repo, before, after, code):
    base = _commit(repo, {"u.yml": before})
    head = _commit(repo, {"u.yml": after})
    out = _run(repo, base, head)
    assert out.returncode == code, out.stdout


def test_an_unparseable_yaml_file_is_compared_in_order(repo):
    # A template that is not valid YAML falls back to the ordered token
    # sequence, which still catches a swap.
    base = _commit(repo, {"t.yml": "x: {{ a }} 1.5\ny: {{ b }} 2.5\n"})
    head = _commit(repo, {"t.yml": "x: {{ a }} 2.5\ny: {{ b }} 1.5\n"})
    out = _run(repo, base, head)
    assert out.returncode == 1, out.stdout


def test_files_other_than_data_are_ignored(repo):
    base = _commit(repo, {"n.md": "value 1.0\n"})
    head = _commit(repo, {"n.md": "value 2.0\n"})
    assert _run(repo, base, head).returncode == 0


def test_a_bad_ref_is_an_error(repo):
    _commit(repo, {"a.json": "{}\n"})
    out = _run(repo, "no-such-ref", "HEAD")
    assert out.returncode not in (0, 1)
