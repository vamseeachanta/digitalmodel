# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reject client-derived names in tracked paths and small tracked text files.

Files over 1 MB, binary files and symlink or junction targets are not read.
Paths are always checked. Adjacent words are also joined for separator variants.
"""

from functools import cache
from hashlib import sha256
import os
from pathlib import Path
import re
import stat
import subprocess
from types import SimpleNamespace

import pytest


RESTRICTED_HASHES = frozenset({
    "d940b56413af584806504ea58f1f7c0b6da2a1ea2dc6733d32117427c7c83936",
    "0420b7df3fb6d4d29c6a5b99df8110dc39007c566b247ce57ccb2eab6c720653",
    "7aa839fe07ee6a8e105fd07006af2b8f7d62b952e01c60c2a70200a6f04a001d",
    "3f323aa417f00ccb884c01418841639a131d4b6cb129ee5831a406ef6057f8b8",
    "28e5a4ee543a9b61f2fd66b86982d883772021ee993211a145177a5e9b797f43",
    "283b78411df2113a33488285d63db0fe42e2048fdc57db3244112b72ecc79469",
    "59c28daef7df40a3ad997fe277e272b53bce9fd3086728a31a70c43c66e6ca37",
})
# Only compound names accept joining; unrelated symbol parts stay separate.
JOINED_HASHES = {
    "0420b7df3fb6d4d29c6a5b99df8110dc39007c566b247ce57ccb2eab6c720653": 8,
    "7aa839fe07ee6a8e105fd07006af2b8f7d62b952e01c60c2a70200a6f04a001d": 9,
}
MAX_TEXT_BYTES = 1_000_000
WORDS = re.compile(rb"[a-z0-9]+")


@cache
def _digest(token):
    return sha256(token).hexdigest()


def _matches(data, restricted=RESTRICTED_HASHES, joined=JOINED_HASHES):
    words = WORDS.findall(data.lower())
    tokens = set(words)
    matches = {_digest(token) for token in tokens} & restricted
    lengths = set(joined.values())
    compounds = {a + b for a, b in zip(words, words[1:])
                 if len(a) + len(b) in lengths}
    matches.update({_digest(token) for token in compounds} & joined.keys())
    return matches


def _is_link(path):
    return (
        path.is_symlink()
        or (hasattr(path, "is_junction") and path.is_junction())
        or (path.exists() and bool(
            getattr(path.lstat(), "st_file_attributes", 0) & stat.FILE_ATTRIBUTE_REPARSE_POINT
        ))
    )


def _scan(root, paths, restricted=RESTRICTED_HASHES, joined=JOINED_HASHES):
    findings = []
    for name in paths:
        matches = _matches(name.encode("utf-8"), restricted, joined)
        path = root / name
        parts = (path, *(root / parent for parent in Path(name).parents))
        if not any(_is_link(part) for part in parts) and path.is_file():
            if path.stat().st_size <= MAX_TEXT_BYTES:
                data = path.read_bytes()
                if b"\0" not in data:
                    # Byte tokenisation also covers text in legacy encodings.
                    matches.update(_matches(data, restricted, joined))
        findings.extend((name, digest) for digest in sorted(matches))
    return findings


def test_tracked_public_names():
    root = Path(__file__).resolve().parents[1]
    env = os.environ.copy()
    for binding in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(binding, None)
    names = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=root, env=env,
    ).decode("utf-8").split("\0")
    # Include pending additions and omit pending deletions before staging as well.
    paths = sorted({name for name in names if name and (
        (root / name).exists() or (root / name).is_symlink()
    )})
    findings = _scan(root, paths)
    assert not findings, "\n".join(f"{name}: {digest}" for name, digest in findings)


@pytest.mark.parametrize("binding", [
    "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE", "all", "none",
])
def test_guard_ignores_foreign_git_bindings(tmp_path, monkeypatch, binding):
    bindings = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")
    clean_env = {key: value for key, value in os.environ.items() if key not in bindings}
    local, foreign = tmp_path / "local", tmp_path / "foreign"
    for root in (local, foreign):
        root.mkdir()
        subprocess.run(["git", "init", "--quiet"], cwd=root, env=clean_env, check=True)
        (root / "shared.txt").write_text("neutral", encoding="utf-8", newline="\n")
        subprocess.run(["git", "add", "shared.txt"], cwd=root, env=clean_env, check=True)
    (local / "local.txt").write_text("samplelabel", encoding="utf-8", newline="\n")
    (local / ".gitignore").write_text("local.txt\n", encoding="utf-8", newline="\n")
    subprocess.run(["git", "add", "--force", "local.txt", ".gitignore"],
                   cwd=local, env=clean_env, check=True)
    (local / "pending.txt").write_text("neutral", encoding="utf-8", newline="\n")
    (foreign / ".git" / "info" / "exclude").write_text(
        "pending.txt\n", encoding="utf-8", newline="\n",
    )
    foreign_values = dict(zip(bindings, (
        foreign / ".git", foreign, foreign / ".git", foreign / ".git" / "index",
    )))
    for key, value in foreign_values.items():
        if binding in (key, "all"):
            monkeypatch.setenv(key, value.as_posix())
        else:
            monkeypatch.delenv(key, raising=False)
    caller_env = os.environ.copy()
    preserved = [local / "local.txt", local / "pending.txt"] + [
        root / ".git" / name for root in (local, foreign) for name in ("index", "config")
    ]
    before = {path: path.read_bytes() for path in preserved}
    inventories = []
    scan = _scan

    def scan_synthetic_names(root, paths):
        inventories.append(paths)
        return scan(root, paths, {_digest(b"samplelabel")}, {})

    monkeypatch.setitem(globals(), "__file__", str(local / "tests" / "guard.py"))
    monkeypatch.setitem(globals(), "_scan", scan_synthetic_names)
    with pytest.raises(AssertionError, match=r"local\.txt:"):
        test_tracked_public_names()
    assert inventories == [[".gitignore", "local.txt", "pending.txt", "shared.txt"]]
    assert os.environ == caller_env
    assert {path: path.read_bytes() for path in preserved} == before


def test_scanner_checks_paths_and_separator_variants(tmp_path):
    restricted = {_digest(b"samplelabel")}
    names = ["sample-label.txt", "words.txt", "substring.txt"]
    (tmp_path / names[0]).write_text("neutral", encoding="utf-8", newline="\n")
    (tmp_path / names[1]).write_bytes(b"\xff SAMPLE_label sample.label sample label")
    (tmp_path / names[2]).write_text("sampleslabel samplelabels", encoding="utf-8", newline="\n")
    assert _scan(tmp_path, names, restricted, {_digest(b"samplelabel"): 11}) == [
        (names[0], _digest(b"samplelabel")),
        (names[1], _digest(b"samplelabel")),
    ]


def test_scanner_does_not_join_unrelated_symbol_parts():
    assert _matches(b"E_cs specs enhanced records platform_builds") == set()


def test_scanner_omits_large_binary_and_symlink_contents(tmp_path):
    names = ["large.txt", "binary.dat", "link.txt"]
    (tmp_path / names[0]).write_bytes(b"samplelabel" + b" " * MAX_TEXT_BYTES)
    (tmp_path / names[1]).write_bytes(b"samplelabel\0")
    target = tmp_path / "target.txt"
    target.write_text("samplelabel", encoding="utf-8", newline="\n")
    (tmp_path / names[2]).symlink_to(target)
    assert _scan(tmp_path, names, {_digest(b"samplelabel")}) == []


@pytest.mark.parametrize("link_kind", ["symlink", "junction", "reparse"])
def test_scanner_omits_directory_link_contents(tmp_path, monkeypatch, link_kind):
    target = tmp_path / "target"
    target.mkdir()
    (target / "content.txt").write_text("samplelabel", encoding="utf-8", newline="\n")
    links = [tmp_path / "linked", tmp_path / "sample-label"]
    for link in links:
        if link_kind == "symlink":
            link.symlink_to(target, target_is_directory=True)
        else:
            link.mkdir()
            (link / "content.txt").write_text("samplelabel", encoding="utf-8", newline="\n")
    if link_kind == "junction":
        monkeypatch.setattr(Path, "is_junction", lambda path: path in links, raising=False)
    elif link_kind == "reparse":
        monkeypatch.delattr(Path, "is_junction", raising=False)
        lstat = Path.lstat

        def reparse_lstat(path, *args, **kwargs):
            result = lstat(path, *args, **kwargs)
            if path in links:
                return SimpleNamespace(st_mode=result.st_mode,
                                       st_file_attributes=stat.FILE_ATTRIBUTE_REPARSE_POINT)
            return result

        monkeypatch.setattr(Path, "lstat", reparse_lstat)
    paths = [(link / "content.txt").relative_to(tmp_path).as_posix() for link in links]
    digest = _digest(b"samplelabel")
    assert _scan(tmp_path, paths, {digest}, {digest: 11}) == [
        ("sample-label/content.txt", digest),
    ]
