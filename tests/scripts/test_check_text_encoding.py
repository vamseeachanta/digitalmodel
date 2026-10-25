# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise encoding findings on the lines a pull request changes."""

import importlib.util
import json
import os
import subprocess
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
        ("Path('data').write_text('value')", ["PLW1514"]),
        ("open('data')", ["PLW1514"]),
        ("open('data', 'rt')", ["PLW1514"]),
        ("open('data', mode='w')", ["PLW1514"]),
    ],
)
def test_text_calls_report_fix(tmp_path, expression, codes):
    findings = check_source(
        tmp_path, "from pathlib import Path\n" + expression + "\n", {2}
    )
    assert [item["code"] for item in findings] == codes
    assert all(item["path"] == "example.py" and item["line"] == 2 for item in findings)
    assert all("encoding=" in item["fix"] for item in findings)


@pytest.mark.parametrize(
    "expression",
    [
        "open('data', 'rb')",
        "open('data', mode='wb')",
        "open('data', 'ab')",
        "Path('data').open('wb')",
        "open('data', encoding='utf-8')",
        "open('data', 'w', encoding='utf-8')",
        "open('data', 'a', encoding='utf-8')",
        "open('data', 'x', encoding='utf-8')",
        "open('data', 'r+', encoding='utf-8')",
        "Path('data').read_text(encoding='utf-8')",
        "Path('data').write_text('value', encoding='utf-8')",
        "Path('data').open('w', encoding='utf-8')",
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
    assert {item["code"] for item in findings} == {"PLW1514"}
    assert all(item["line"] == 3 for item in findings)


def test_non_python_file_is_ignored(tmp_path):
    assert not check_source(tmp_path, "open('data')\n", {1}, filename="notes.md")


def test_hunks_include_only_new_side_lines():
    patch = (
        "@@ -1,2 +1,3 @@\n-old\n-other\n+one\n+two\n+three\n"
        "@@ -9 +10 @@\n-old\n+new\n@@ -20,2 +21,0 @@\n-old\n-other\n"
    )
    assert checker.added_lines(patch) == {1, 2, 3, 10}


def test_deleted_lines_do_not_trigger(tmp_path):
    assert not check_source(tmp_path, "open('old')\n", set())


@pytest.mark.parametrize(
    "source",
    [
        "from PIL import Image\nImage.open(path)\n",
        "import webbrowser\nwebbrowser.open(url)\n",
        "import tarfile\ntarfile.open(path)\n",
        "import gzip\ngzip.open(path)\n",
        "import zipfile\nzipfile.ZipFile('archive').open(member)\n",
        "import codecs\ncodecs.open(path, 'w', 'utf-8')\n",
        "import os\nos.open(path, flags)\n",
        "open('data', mode, encoding='utf-8')\n",
    ],
)
def test_unrelated_open_apis_and_encoded_dynamic_mode_pass(tmp_path, source):
    assert not check_source(tmp_path, source)


def test_syntax_error_fails_closed(tmp_path):
    findings = check_source(tmp_path, "open(\n", {1})
    assert [item["code"] for item in findings] == ["TXT000"]


@pytest.fixture
def git_repo(tmp_path, monkeypatch):
    for key in tuple(os.environ):
        if key.startswith("GIT_"):
            monkeypatch.delenv(key)
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    return tmp_path


def snapshot(root, parent=None):
    """Create synthetic Git objects without reading or changing user identity."""
    checker.git(root, "add", ".")
    tree = checker.git(root, "write-tree").strip()
    ancestry = f"parent {parent}\n" if parent else ""
    result = subprocess.run(
        ["git", "hash-object", "-t", "commit", "-w", "--stdin"],
        cwd=root,
        input=(
            f"tree {tree}\n{ancestry}"
            "author Encoding test <> 1 +0000\n"
            "committer Encoding test <> 1 +0000\n\nTest input\n"
        ).encode("utf-8"),
        capture_output=True,
        check=True,
    )
    commit = result.stdout.decode("ascii").strip()
    checker.git(root, "update-ref", "HEAD", commit)
    return commit


def test_snapshot_preserves_commit_bytes_with_windows_text_translation(
    git_repo, monkeypatch
):
    real_run = subprocess.run

    def windows_run(args, **kwargs):
        # Windows text pipes translate LF to CRLF, invalidating Git object headers.
        if args[1:3] == ["hash-object", "-t"] and isinstance(kwargs.get("input"), str):
            kwargs["input"] = kwargs["input"].replace("\n", "\r\n")
        return real_run(args, **kwargs)

    monkeypatch.setattr(subprocess, "run", windows_run)
    commit = snapshot(git_repo)
    assert checker.git(git_repo, "cat-file", "-t", commit).strip() == "commit"


def test_rename_plus_comment_preserves_unchanged_calls(git_repo):
    old = git_repo / "spaced name.py"
    old.write_text("open('old')\n" * 8, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    new = old.rename(git_repo / "renamed.py")
    with new.open("a", encoding="utf-8", newline="\n") as stream:
        stream.write("# Changed comment\n")
    snapshot(git_repo, base)
    assert not checker.check_files(git_repo, checker.diff_scope(git_repo, base))


def test_renamed_file_checks_changed_calls(git_repo):
    old = git_repo / "before.py"
    old.write_text("# Context\n" * 8, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    new = old.rename(git_repo / "after.py")
    with new.open("a", encoding="utf-8", newline="\n") as stream:
        stream.write("open('new')\n")
    snapshot(git_repo, base)
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [(item["path"], item["line"], item["code"]) for item in findings] == [
        ("after.py", 9, "PLW1514")
    ]


def test_synthetic_bad_and_good_diff(tmp_path, monkeypatch, capsys):
    path = tmp_path / "example.py"
    monkeypatch.setattr(
        checker,
        "git",
        lambda root, *args: (
            "A\0example.py\0"
            if "--name-status" in args
            else "@@ -0,0 +1 @@\n+open('data')\n"
        ),
    )
    path.write_text("open('data', 'w')\n", encoding="utf-8", newline="\n")
    assert checker.main(["--root", str(tmp_path)]) == 1
    output = capsys.readouterr().out
    assert "example.py:1: PLW1514" in output
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
    assert checker.RUFF_VERSION == version
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


def test_editing_chain_suffix_does_not_change_read_call(git_repo):
    path = git_repo / "example.py"
    source = (
        "from pathlib import Path\nPath('data').read_text().replace(\n"
        "    'old',\n    'value',\n)\n"
    )
    path.write_text(source, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    path.write_text(source.replace("'old'", "'new'"), encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    assert not checker.check_files(git_repo, checker.diff_scope(git_repo, base))


def test_deleted_encoding_inside_call_is_checked(git_repo):
    path = git_repo / "example.py"
    path.write_text(
        "open(\n    'data',\n    encoding='utf-8',\n)\n", encoding="utf-8", newline="\n"
    )
    base = snapshot(git_repo)
    path.write_text("open(\n    'data',\n)\n", encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert len(findings) == 1 and findings[0]["code"] == "PLW1514"


@pytest.mark.parametrize("old_end, new_end", [("", "\n"), ("\n", "")])
@pytest.mark.parametrize("remove_encoding", [True, False])
def test_encoding_deletion_with_final_newline_change(
    git_repo, old_end, new_end, remove_encoding
):
    path = git_repo / "example.py"
    source = "open(\n    'data',\n    encoding='utf-8',\n).read()"
    path.write_text(source + old_end, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    if remove_encoding:
        source = source.replace("    encoding='utf-8',\n", "")
    path.write_text(source + new_end, encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [(item["line"], item["code"]) for item in findings] == (
        [(3, "PLW1514")] if remove_encoding else []
    )


def test_deleting_line_after_call_does_not_change_call(git_repo):
    path = git_repo / "example.py"
    path.write_text("open('old')\n# Remove comment\n", encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    path.write_text("open('old')\n", encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    assert not checker.check_files(git_repo, checker.diff_scope(git_repo, base))


@pytest.mark.parametrize(
    "appended, codes", [("# Comment\n", []), ("open('new')\n", ["PLW1514"])]
)
def test_append_without_final_newline_checks_only_added_call(git_repo, appended, codes):
    path = git_repo / "example.py"
    path.write_text("open('old')", encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    path.write_text("open('old')\n" + appended, encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [item["code"] for item in findings] == codes
    assert all(item["line"] == 2 for item in findings)


@pytest.mark.parametrize("config", ["color.ui=always", "diff.interHunkContext=5"])
def test_local_git_display_settings_do_not_change_scope(git_repo, monkeypatch, config):
    path = git_repo / "example.py"
    path.write_text(
        "value = 1\nopen('old')\nother = 1\n", encoding="utf-8", newline="\n"
    )
    base = snapshot(git_repo)
    path.write_text(
        "value = 2\nopen('old')\nother = 2\nopen('new')\n",
        encoding="utf-8",
        newline="\n",
    )
    snapshot(git_repo, base)
    real_git = checker.git
    monkeypatch.setattr(
        checker, "git", lambda root, *args: real_git(root, "-c", config, *args)
    )
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [(item["line"], item["code"]) for item in findings] == [(4, "PLW1514")]


def test_ruff_semantic_syntax_error_retains_message(tmp_path):
    findings = check_source(tmp_path, "def function(value, value): pass\n", {1})
    assert len(findings) == 1
    assert findings[0]["code"] == "TXT000"
    assert "Duplicate parameter" in findings[0]["fix"]


def test_git_error_includes_stderr_tail(git_repo):
    with pytest.raises(RuntimeError, match="single revision"):
        checker.git(git_repo, "rev-parse", "--verify", "refs/heads/absent")


def test_ruff_error_includes_stderr_tail(tmp_path, monkeypatch):
    real_run = subprocess.run

    def run(args, **kwargs):
        if args[:2] == ["ruff", "check"]:
            return real_run(["ruff", "check", "--not-a-real-option"], **kwargs)
        return real_run(args, **kwargs)

    monkeypatch.setattr(checker.subprocess, "run", run)
    with pytest.raises(RuntimeError, match="unexpected argument"):
        check_source(tmp_path, "open('data')\n")


def test_ruff_version_mismatch_fails_closed(tmp_path, monkeypatch):
    monkeypatch.setattr(checker, "RUFF_VERSION", "0.0.0", raising=False)
    with pytest.raises(RuntimeError, match="requires Ruff 0.0.0"):
        check_source(tmp_path, "open('data')\n")


def test_unannotated_parameter_read_is_a_known_limit(tmp_path):
    assert not check_source(tmp_path, "def read(path):\n    return path.read_text()\n")


def test_git_scope_clears_inherited_repository_bindings(git_repo, monkeypatch):
    path = git_repo / "example.py"
    path.write_text("value = 1\n", encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    path.write_text("open('new')\n", encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(name, str(git_repo / "absent"))
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [item["code"] for item in findings] == ["PLW1514"]
    assert path.read_text(encoding="utf-8") == "open('new')\n"


def test_hunk_function_context_does_not_shift_line_numbers():
    patch = "@@ -3 +3 @@ def function():\n-    old = 1\n+    new = 2\n"
    assert checker.added_lines(patch) == {3}


@pytest.mark.parametrize(
    "separator", ["\v", "\f", "\x1c", "\x1d", "\x1e", "\x85", "\u2028", "\u2029"]
)
def test_hunk_rows_ignore_nonphysical_separators(separator):
    patch = f"@@ -1 +1,2 @@\n-old\n+value = '{separator}+fragment'\n+open('new')\n"
    assert checker.added_lines(patch) == {1, 2}


@pytest.mark.parametrize(
    "separator", ["\v", "\f", "\x1c", "\x1d", "\x1e", "\x85", "\u2028", "\u2029"]
)
def test_source_rows_ignore_nonphysical_separators(git_repo, separator):
    path = git_repo / "example.py"
    source = (
        f"from pathlib import Path\n# Separator {separator}\n"
        "Path('data').write_text(\n    'old',\n)\n"
    )
    path.write_text(source, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    path.write_text(source.replace("'old'", "'new'"), encoding="utf-8", newline="\n")
    snapshot(git_repo, base)
    findings = checker.check_files(git_repo, checker.diff_scope(git_repo, base))
    assert [(item["line"], item["code"]) for item in findings] == [(4, "PLW1514")]


@pytest.mark.parametrize("line_ending", ["\n", "\r\n", "\r"])
def test_source_physical_line_endings(tmp_path, line_ending):
    source = "value = 'é'; open(\n    'changed',\n)\n".replace("\n", line_ending)
    findings = check_source(tmp_path, source, {2})
    assert [(item["line"], item["code"]) for item in findings] == [(2, "PLW1514")]


@pytest.mark.parametrize("line_ending", ["\n", "\r\n", "\r"])
def test_git_output_preserves_line_endings(git_repo, line_ending):
    source = "value = 1\n# Comment\n".replace("\n", line_ending)
    (git_repo / "example.py").write_bytes(source.encode("utf-8"))
    # Keep this assertion independent of Git checkout newline conversion settings.
    blob = checker.git(
        git_repo, "hash-object", "-w", "--no-filters", "example.py"
    ).strip()
    assert checker.git(git_repo, "cat-file", "-p", blob) == source


@pytest.mark.parametrize("line_ending", ["\n", "\r\n", "\r", "\r\n\r"])
def test_real_git_multiline_argument_line_endings(git_repo, line_ending, capsys):
    source = "from pathlib import Path\nPath('data').write_text(\n    'old',\n)\n"
    path = git_repo / "example.py"
    path.write_bytes(source.replace("\n", line_ending).encode("utf-8"))
    base = snapshot(git_repo)
    path.write_bytes(
        source.replace("'old'", "'new'").replace("\n", line_ending).encode("utf-8")
    )
    snapshot(git_repo, base)

    result = checker.main(["--root", str(git_repo), "--base", base, "--json"])
    output = capsys.readouterr()
    if line_ending in {"\r", "\r\n\r"}:
        assert result == 2
        assert "bare CR line endings" in output.err
        assert "example.py" in output.err
        assert not output.out
    else:
        assert result == 1
        findings = json.loads(output.out)
        assert [(item["line"], item["code"]) for item in findings] == [(3, "PLW1514")]


def test_literal_filename_does_not_include_other_file_hunks(git_repo):
    bracketed = git_repo / "sample[1].py"
    other = git_repo / "sample1.py"
    source = "value = 1\n# Original comment\n" + "# Context\n" * 10 + "open('old')\n"
    bracketed.write_text(source, encoding="utf-8", newline="\n")
    other.write_text(source, encoding="utf-8", newline="\n")
    base = snapshot(git_repo)
    bracketed.write_text(
        source.replace("Original", "Changed"), encoding="utf-8", newline="\n"
    )
    other.write_text(source.replace("'old'", "'new'"), encoding="utf-8", newline="\n")
    snapshot(git_repo, base)

    scope = checker.diff_scope(git_repo, base)
    assert scope == {"sample[1].py": {2}, "sample1.py": {13}}
    findings = checker.check_files(git_repo, scope)
    assert [(item["path"], item["line"], item["code"]) for item in findings] == [
        ("sample1.py", 13, "PLW1514")
    ]
