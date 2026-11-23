# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Prevent personal hash-tags in tracked files containing todo markers."""

import os
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
PERSONAL_TAG = re.compile(r"#[0-9]{1,3}_[a-z]+")
RULE = "todo-personal-tags"


def _tracked_todo_paths(root):
    """Select tracked text without inheriting another repository's bindings."""
    result = subprocess.run(
        ["git", "grep", "-I", "-l", "-z", "--", "#todo"],
        cwd=root,
        env={key: value for key, value in os.environ.items()
             if not key.startswith("GIT_")},
        capture_output=True,
        check=False,
    )
    if result.returncode not in (0, 1):
        pytest.fail("tracked-text: git-discovery", pytrace=False)
    return [os.fsdecode(path) for path in result.stdout.split(b"\0") if path]


def _findings(root, paths):
    """Report paths and the rule only, including tags on sibling lines."""
    findings = []
    for relative in paths:
        with (root / relative).open(
            encoding="utf-8", newline="\n", errors="surrogateescape"
        ) as stream:
            text = stream.read()
        if "\0" not in text and "#todo" in text and PERSONAL_TAG.search(text):
            findings.append(f"{relative}: {RULE}")
    return findings


def test_tracked_todo_files_have_no_personal_tags():
    findings = _findings(ROOT, _tracked_todo_paths(ROOT))
    if findings:
        pytest.fail("\n".join(findings), pytrace=False)


@pytest.mark.parametrize(
    ("layout", "expected"),
    [
        ("same-line", True),
        ("sibling-line", True),
        ("before-marker", True),
        ("no-marker", False),
        ("marker-only", False),
        ("binary", False),
        ("non-utf8", True),
    ],
)
def test_detector_scope_and_redacted_findings(tmp_path, layout, expected):
    tag = "#" + "7_person"
    text = {
        "same-line": f"#todo {tag} retain this prose\n",
        "sibling-line": f"#todo retain this prose\n# {tag}\n",
        "before-marker": f"# {tag}\n#todo retain this prose\n",
        "no-marker": f"# {tag}\n",
        "marker-only": "#todo retain this prose\n",
        "binary": f"\0#todo {tag}\n",
        "non-utf8": f"#todo {tag}\udcff\n",
    }[layout]
    with (tmp_path / "sample.txt").open(
        "w", encoding="utf-8", newline="\n", errors="surrogateescape"
    ) as stream:
        stream.write(text)
    findings = _findings(tmp_path, ["sample.txt"])
    assert findings == ([f"sample.txt: {RULE}"] if expected else [])


def test_git_discovery_ignores_inherited_repository_bindings(monkeypatch, tmp_path):
    expected = _tracked_todo_paths(ROOT)
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.setenv(key, str(tmp_path / "unrelated"))
    assert _tracked_todo_paths(ROOT) == expected
