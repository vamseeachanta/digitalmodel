# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise encoding and newline findings on the lines a pull request changes."""

import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "check_text_encoding", ROOT / "scripts/ci/check_text_encoding.py"
)
checker = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(checker)


def check_source(tmp_path, source, lines=None, filename="example.py"):
    path = tmp_path / filename
    path.write_text(source, encoding="utf-8", newline="\n")
    return checker.check_files(tmp_path, {filename: lines})


@pytest.mark.parametrize(
    "expression, codes",
    [
        ("Path('data').read_text()", ["PLW1514"]),
        ("Path('data').write_text('value')", ["PLW1514", "TXT001"]),
        ("open('data')", ["PLW1514"]),
        ("open('data', 'rt')", ["PLW1514"]),
        ("open('data', mode='w')", ["PLW1514", "TXT001"]),
        ("open('data', 'a', encoding='utf-8')", ["TXT001"]),
        ("open('data', 'x', encoding='utf-8')", ["TXT001"]),
        ("open('data', 'r+', encoding='utf-8')", ["TXT001"]),
        ("Path('data').write_text('value', encoding='utf-8')", ["TXT001"]),
        ("Path('data').open('w', encoding='utf-8')", ["TXT001"]),
    ],
)
def test_text_calls_report_fix(tmp_path, expression, codes):
    findings = check_source(
        tmp_path, "from pathlib import Path\n" + expression + "\n", {2}
    )
    assert [item["code"] for item in findings] == codes
    assert all(item["path"] == "example.py" and item["line"] == 2 for item in findings)
    assert all(
        "encoding=" in item["fix"] or "newline=" in item["fix"] for item in findings
    )


@pytest.mark.parametrize(
    "expression",
    [
        "open('data', 'rb')",
        "open('data', mode='wb')",
        "open('data', 'ab')",
        "Path('data').open('wb')",
        "open('data', encoding='utf-8')",
        "Path('data').read_text(encoding='utf-8')",
        "Path('data').write_text('value', encoding='utf-8', newline='\\n')",
        "open('data', 'w', encoding='utf-8', newline='\\n')",
        "open('data', 'w', -1, 'utf-8', None, '\\n')",
        "Path('data').open('w', -1, 'utf-8', None, '\\n')",
        "Path('data').write_text('value', 'utf-8', None, '\\n')",
        "print('open(\"data\")')",
    ],
)
def test_explicit_text_and_binary_calls_pass(tmp_path, expression):
    assert not check_source(tmp_path, "from pathlib import Path\n" + expression + "\n")


def test_unchanged_calls_do_not_block(tmp_path):
    source = "from pathlib import Path\nPath('old').read_text()\nvalue = 1\n"
    assert not check_source(tmp_path, source, {3})


def test_changed_multiline_argument_is_checked(tmp_path):
    source = "from pathlib import Path\nPath('data').write_text(\n    'changed',\n)\n"
    findings = check_source(tmp_path, source, {3})
    assert {item["code"] for item in findings} == {"PLW1514", "TXT001"}
    assert all(item["line"] == 3 for item in findings)


def test_non_python_file_is_ignored(tmp_path):
    assert not check_source(tmp_path, "open('data')\n", {1}, filename="notes.md")


def test_hunks_include_only_new_side_lines():
    patch = "@@ -1,2 +1,3 @@\n@@ -9 +10 @@\n@@ -20,2 +21,0 @@\n"
    assert checker.added_lines(patch) == {1, 2, 3, 10}


def test_deleted_lines_do_not_trigger(tmp_path):
    assert not check_source(tmp_path, "open('old')\n", set())


def test_newline_unknown_mode_is_checked_conservatively(tmp_path):
    findings = check_source(tmp_path, "open('data', mode, encoding='utf-8')\n")
    assert [item["code"] for item in findings] == ["TXT001"]


def test_syntax_error_fails_closed(tmp_path):
    findings = check_source(tmp_path, "open(\n", {1})
    assert [item["code"] for item in findings] == ["TXT000"]


def test_git_scoping_handles_names_and_rename_additions(tmp_path, monkeypatch):
    calls = []

    def git(root, *args):
        calls.append(args)
        if "--name-only" in args:
            return "spaced name.py\0notes.md\0new.py\0"
        if args[-1] == "spaced name.py":
            return "@@ -3 +3 @@\n-old\n+new\n"
        return "@@ -0,0 +1,2 @@\n+one\n+two\n"

    monkeypatch.setattr(checker, "git", git)
    assert checker.diff_scope(tmp_path, "origin/main") == {
        "spaced name.py": {3},
        "new.py": {1, 2},
    }
    assert all("origin/main...HEAD" in call for call in calls)
    assert all("--no-renames" in call for call in calls)


def test_synthetic_bad_and_good_diff(tmp_path, monkeypatch, capsys):
    path = tmp_path / "example.py"
    monkeypatch.setattr(
        checker,
        "git",
        lambda root, *args: (
            "example.py\0" if "--name-only" in args else "@@ -0,0 +1 @@\n"
        ),
    )
    path.write_text("open('data', 'w')\n", encoding="utf-8", newline="\n")
    assert checker.main(["--root", str(tmp_path)]) == 1
    output = capsys.readouterr().out
    assert "example.py:1: PLW1514" in output
    assert "example.py:1: TXT001" in output
    path.write_text(
        "open('data', 'w', encoding='utf-8', newline='\\n')\n",
        encoding="utf-8",
        newline="\n",
    )
    assert checker.main(["--root", str(tmp_path)]) == 0
    assert "passed" in capsys.readouterr().out


def test_whole_tree_uses_tracked_python_files(tmp_path, monkeypatch, capsys):
    (tmp_path / "example.py").write_text(
        "open('data')\n", encoding="utf-8", newline="\n"
    )
    monkeypatch.setattr(checker, "git", lambda root, *args: "example.py\0notes.md\0")
    assert checker.main(["--root", str(tmp_path), "--whole-tree", "--json"]) == 1
    findings = json.loads(capsys.readouterr().out)
    assert len(findings) == 1 and findings[0]["code"] == "PLW1514"


def test_ruff_failure_fails_closed(tmp_path, monkeypatch):
    def fail(*args, **kwargs):
        raise OSError("tool unavailable")

    monkeypatch.setattr(checker.subprocess, "run", fail)
    with pytest.raises(RuntimeError, match="Ruff could not run"):
        check_source(tmp_path, "open('data')\n")


@pytest.mark.parametrize(
    "prefix, call",
    [
        ("import io", "io.open"),
        ("import io as streams", "streams.open"),
        ("import builtins", "builtins.open"),
        ("from io import open as text_open", "text_open"),
    ],
)
def test_standard_open_aliases(tmp_path, prefix, call):
    assert not check_source(tmp_path, f"{prefix}\n{call}('data', 'wb')\n")
    findings = check_source(
        tmp_path, f"{prefix}\n{call}('data', 'w', encoding='utf-8')\n"
    )
    assert [item["code"] for item in findings] == ["TXT001"]


def test_workflow_is_pull_request_only_and_pins_ruff():
    import tomllib
    import yaml

    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/text-encoding.yml").read_text(encoding="utf-8")
    )
    assert set(workflow["on"]) == {"pull_request"}
    assert workflow["permissions"] == {"contents": "read"}
    job = workflow["jobs"]["text-encoding"]
    assert not job.get("continue-on-error", False)
    checkout = job["steps"][0]["with"]
    assert checkout["ref"] == "${{ github.event.pull_request.head.sha }}"
    assert checkout["fetch-depth"] == 0
    steps = {step.get("name"): step for step in job["steps"]}
    check = steps["Check changed text calls"]
    assert check["env"]["PR_BASE_SHA"] == "${{ github.event.pull_request.base.sha }}"
    assert '--base "$PR_BASE_SHA"' in check["run"]
    assert not check.get("continue-on-error", False)
    lock = tomllib.loads((ROOT / "uv.lock").read_text(encoding="utf-8"))
    version = next(
        item["version"] for item in lock["package"] if item["name"] == "ruff"
    )
    assert f"--with ruff=={version}" in check["run"]
    assert f"--with ruff=={version}" in steps["Test the checker"]["run"]
    assert (
        "tests/scripts/test_check_text_encoding.py" in steps["Test the checker"]["run"]
    )


def test_unicode_prefix_and_multiline_changed_argument(tmp_path):
    findings = check_source(tmp_path, "value = 'é'; open(\n    'changed',\n)\n", {2})
    assert [item["code"] for item in findings] == ["PLW1514"]
    assert findings[0]["line"] == 2


def test_unchanged_writer_is_ignored(tmp_path):
    assert not check_source(tmp_path, "open('old', 'w')\nvalue = 1\n", {2})
