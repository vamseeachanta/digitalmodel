# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reject client-derived names in tracked paths and small tracked text files.

Files over 1 MB, binary files and symlink targets are not read. Paths are always
checked. Adjacent words are also joined to cover separator variants.
"""

from functools import cache
from hashlib import sha256
from pathlib import Path
import re
import subprocess


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


def _scan(root, paths, restricted=RESTRICTED_HASHES, joined=JOINED_HASHES):
    findings = []
    for name in paths:
        matches = _matches(name.encode("utf-8"), restricted, joined)
        path = root / name
        if path.is_file() and not path.is_symlink():
            if path.stat().st_size <= MAX_TEXT_BYTES:
                data = path.read_bytes()
                if b"\0" not in data:
                    # Byte tokenisation also covers text in legacy encodings.
                    matches.update(_matches(data, restricted, joined))
        findings.extend((name, digest) for digest in sorted(matches))
    return findings


def test_tracked_public_names():
    root = Path(__file__).resolve().parents[1]
    names = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"], cwd=root
    ).decode("utf-8").split("\0")
    # Include pending additions and omit pending deletions before staging as well.
    paths = sorted({name for name in names if name and (
        (root / name).exists() or (root / name).is_symlink()
    )})
    findings = _scan(root, paths)
    assert not findings, "\n".join(f"{name}: {digest}" for name, digest in findings)


def test_scanner_checks_paths_and_separator_variants(tmp_path):
    restricted = {_digest(b"samplelabel")}
    names = ["sample-label.txt", "words.txt", "substring.txt"]
    (tmp_path / names[0]).write_text("neutral")
    (tmp_path / names[1]).write_bytes(b"\xff SAMPLE_label sample.label sample label")
    (tmp_path / names[2]).write_text("sampleslabel samplelabels")
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
    target.write_text("samplelabel")
    (tmp_path / names[2]).symlink_to(target)
    assert _scan(tmp_path, names, {_digest(b"samplelabel")}) == []
