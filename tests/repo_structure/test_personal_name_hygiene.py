# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Prevent personal references in tracked text without disclosing the word."""

import codecs
import hashlib
import os
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
PERSONAL_NAME_DIGEST = (
    "229198ac50189365a5dd6412995ab24b4126c777fd69673153f92a0b1ba46882"
)
RULE = "personal-name-hygiene"


def contains_personal_name(data, forbidden_digest=PERSONAL_NAME_DIGEST):
    """Match case-insensitive whole words in ordinary and BOM-marked text."""
    for marker, encoding in (
        (codecs.BOM_UTF32_LE, "utf-32"),
        (codecs.BOM_UTF32_BE, "utf-32"),
        (codecs.BOM_UTF16_LE, "utf-16"),
        (codecs.BOM_UTF16_BE, "utf-16"),
    ):
        if data.startswith(marker):
            text = data.decode(encoding, errors="replace")
            break
    else:
        if b"\0" in data:
            return False
        text = data.decode("utf-8", errors="replace")
    return any(
        hashlib.sha256(word.casefold().encode("utf-8")).hexdigest() == forbidden_digest
        for word in re.findall(r"\w+", text)
    )


def redacted_path(relative, forbidden_digest=PERSONAL_NAME_DIGEST):
    """Keep diagnostics relative and replace any matching path words."""

    def replace(match):
        word = match.group()
        digest = hashlib.sha256(word.casefold().encode("utf-8")).hexdigest()
        return "[redacted]" if digest == forbidden_digest else word

    return re.sub(r"\w+", replace, relative)


def has_symlink_ancestor(root, relative):
    """Check parent components in order before following any child path."""
    parent = root
    for component in Path(relative).parts[:-1]:
        parent = parent / component
        if parent.is_symlink():
            return True
    return False


def test_tracked_text_contains_no_personal_name():
    git_environment = os.environ.copy()
    for variable in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        git_environment.pop(variable, None)
    result = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=ROOT,
        env=git_environment,
        capture_output=True,
        check=False,
    )
    if result.returncode:
        pytest.fail(f".: {RULE}-enumeration", pytrace=False)
    failures = []
    for entry in result.stdout.split(b"\0"):
        if not entry:
            continue
        relative = os.fsdecode(entry)
        display = redacted_path(relative)
        path = ROOT / relative
        try:
            if has_symlink_ancestor(ROOT, relative):
                failures.append(f"{display}: {RULE}-symlink-ancestor")
                continue
            if path.is_symlink():
                data = os.fsencode(os.readlink(path))
            elif not path.exists() or path.is_dir():
                continue
            else:
                data = path.read_bytes()
        except OSError:
            failures.append(f"{display}: {RULE}-unreadable")
            continue
        if contains_personal_name(data):
            failures.append(f"{display}: {RULE}")
    if failures:
        pytest.fail("\n".join(failures), pytrace=False)


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (b"redacted", True),
        (b"REDACTED", True),
        (b"(Redacted),\n", True),
        (b"unredacted", False),
        (b"redacted_suffix", False),
        (b"redacted2", False),
        ("redacted\N{LATIN SMALL LETTER E WITH ACUTE}".encode(), False),
        (b"redacted\0binary", False),
        ("Redacted".encode("utf-16"), True),
        ("Redacted".encode("utf-32"), True),
        (codecs.BOM_UTF16_BE + "REDACTED".encode("utf-16-be"), True),
        (codecs.BOM_UTF32_BE + "REDACTED".encode("utf-32-be"), True),
        (b"\xff(redacted)\xfe", True),
    ],
    ids=[
        "whole-word",
        "uppercase",
        "punctuation",
        "prefix",
        "underscore",
        "digits",
        "unicode-boundary",
        "binary",
        "utf16",
        "utf32",
        "utf16-big-endian",
        "utf32-big-endian",
        "legacy-text",
    ],
)
def test_personal_name_word_matching(data, expected):
    digest = hashlib.sha256(b"redacted").hexdigest()
    assert contains_personal_name(data, digest) is expected


def test_symlink_ancestor_does_not_read_external_content(tmp_path, monkeypatch):
    root = tmp_path / "repository"
    external = tmp_path / "external"
    root.mkdir()
    external.mkdir()
    (external / "content.txt").write_text("fixture\n", encoding="utf-8", newline="\n")
    try:
        (root / "linked").symlink_to(external, target_is_directory=True)
    except (NotImplementedError, OSError):
        pytest.skip("Directory symlinks are unavailable")
    monkeypatch.setitem(globals(), "ROOT", root)
    enumeration = subprocess.CompletedProcess([], 0, stdout=b"linked/content.txt\0")
    monkeypatch.setattr(subprocess, "run", lambda *args, **kwargs: enumeration)

    def refuse_content_read(path):
        raise AssertionError("External content must not be read")

    monkeypatch.setattr(Path, "read_bytes", refuse_content_read)
    with pytest.raises(pytest.fail.Exception, match=rf"{RULE}-symlink-ancestor"):
        test_tracked_text_contains_no_personal_name()


def test_diagnostic_paths_redact_personal_word():
    digest = hashlib.sha256(b"redacted").hexdigest()
    assert redacted_path("folder/ReDaCtEd-notes.txt", digest) == (
        "folder/[redacted]-notes.txt"
    )
