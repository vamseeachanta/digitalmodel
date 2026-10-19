# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Byte-level oracles for the staged conflict-marker hook (#2310).

Expected results are hand classifications of the literal fixtures below using
the anchor, co-occurrence and forensic rules documented in
scripts/enforcement/check-no-conflict-markers.sh (repository source).
The seven-character anchors are inherited syntax, not an acceptance limit.
Temporary indexes are populated directly; no commit or identity is required.
"""

from __future__ import annotations

import io
import os
import runpy
import subprocess
from pathlib import Path

import pytest

HOOK = (
    Path(__file__).resolve().parents[2]
    / "scripts/enforcement/check-no-conflict-markers.sh"
)
HELPER = HOOK.with_name("check_no_conflict_markers.py")
PAIR = b"<<<<<<< left\n=======\n>>>>>>> right\n"
FILE_OK = b"<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->"


def git(repo: Path, *args: str, data: bytes | None = None) -> bytes:
    return subprocess.run(
        ["git", *args], cwd=repo, input=data, capture_output=True, check=True
    ).stdout


@pytest.fixture
def repo(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    # An inherited index or repository override must not affect the real tree.
    for name in tuple(os.environ):
        if name.startswith("GIT_"):
            monkeypatch.delenv(name)
    git(tmp_path, "init", "-q")
    return tmp_path


def stage(repo: Path, path: str, content: bytes) -> None:
    oid = git(repo, "hash-object", "-w", "--stdin", data=content).strip().decode()
    git(repo, "update-index", "--add", "--cacheinfo", "100644", oid, path)


def run_hook(repo: Path) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(["bash", str(HOOK)], cwd=repo, capture_output=True)


@pytest.mark.parametrize(
    ("content", "blocked"),
    [
        (b"", False),
        (b"plain\n=======\n", False),
        (b">>>>>>> quotation\n", False),
        (b"<<<<<<< alone\n", False),
        (b"<<<<<<<suffix\n>>>>>>>suffix\n", False),
        (b" <<<<<<< indented\n >>>>>>> indented\n", False),
        (PAIR, True),
        (b"<<<<<<<\n=======\n>>>>>>>", True),
        (b"<<<<<<<\tleft\r\n>>>>>>>\tright\r\n", True),
        (("<<<<<<<\u2003left\n>>>>>>>\u2003right").encode(), True),
        # Python 3.11 str.isspace() defines locale-independent whitespace.
        (("<<<<<<<\u00a0left\n>>>>>>>\u00a0right").encode(), True),
        (b"\x00\xffbinary\n" + PAIR, True),
        # NUL cannot be discarded to manufacture an anchor or exemption.
        (b"<<<\x00<<<< left\n>>>>\x00>>> right\n", False),
        (b"\x00\xffbinary without markers", False),
        (b"<<<<<<< left\n=======\n>>>>>>> right\x00", True),
    ],
)
def test_literal_blob_classification(repo: Path, content: bytes, blocked: bool):
    """Both literal anchors trigger; indentation/suffixes/NUL splits do not."""
    stage(repo, "fixture.dat", content)
    result = run_hook(repo)
    assert result.returncode == int(blocked)
    assert b"ignored null byte" not in result.stderr


@pytest.mark.parametrize(
    "prefix",
    [
        "docs/plans/",
        "docs/governance/",
        "docs/standards/",
        "scripts/review/results/",
        "scripts/review/prompts/",
        "src/",
        "docs/plans-suffix/",
    ],
)
def test_file_exemption_is_path_restricted(repo: Path, prefix: str):
    """Only the five documented directory prefixes permit a file exemption."""
    stage(repo, prefix + "fixture.md", PAIR + FILE_OK + b"\n")
    result = run_hook(repo)
    assert result.returncode == int(prefix in {"src/", "docs/plans-suffix/"})


@pytest.mark.parametrize(
    "sentinel",
    [
        b"# CONFLICT_MARKER_FORENSIC_OK",
        b"// CONFLICT_MARKER_FORENSIC_OK",
        b"<!-- CONFLICT_MARKER_FORENSIC_OK -->",
    ],
)
def test_line_exemption_is_exact_and_local(repo: Path, sentinel: bytes):
    """Suffix exempts its line; another line and a trailing space do not."""
    stage(repo, "fixture.txt", b"<<<<<<< left " + sentinel + b"\n>>>>>>> right\n")
    result = run_hook(repo)
    assert result.returncode == 1
    assert b"fixture.txt:1:" not in result.stderr
    assert b"fixture.txt:2:" in result.stderr
    stage(
        repo,
        "fixture.txt",
        b"<<<<<<< left " + sentinel + b"\n>>>>>>> right " + sentinel,
    )
    assert run_hook(repo).returncode == 0
    stage(repo, "fixture.txt", b"<<<<<<< left " + sentinel + b" \n>>>>>>> right\n")
    assert run_hook(repo).returncode == 1


def test_exempt_anchors_still_count_for_cooccurrence(repo: Path):
    """An exempt opening anchor still pairs with the unexempt closing anchor."""
    stage(repo, "fixture.txt", b"<<<<<<< # CONFLICT_MARKER_FORENSIC_OK\n>>>>>>>\n")
    assert run_hook(repo).returncode == 1


def test_index_is_the_only_content_source(repo: Path):
    """A working-tree file exemption cannot hide staged markers, or converse."""
    path = "docs/plans/fixture.md"
    worktree_file = repo / path
    worktree_file.parent.mkdir(parents=True)
    stage(repo, path, PAIR)
    worktree_file.write_bytes(FILE_OK)
    assert run_hook(repo).returncode == 1
    stage(repo, path, FILE_OK)
    worktree_file.write_bytes(PAIR)
    assert run_hook(repo).returncode == 0


def test_unusual_names_and_multiple_blobs(repo: Path):
    """NUL-delimited paths preserve spaces, newline, colon and non-ASCII bytes."""
    for path in [
        "space name.txt",
        "line\nbreak.txt",
        "colon:name.txt",
        "caf\u00e9.txt",
    ]:
        stage(repo, path, PAIR)
    stage(repo, "clean.bin", b"\x00\xffclean")
    result = run_hook(repo)
    assert result.returncode == 1
    assert b"ignored null byte" not in result.stderr
    assert b"space name.txt" in result.stderr
    assert b"colon:name.txt" in result.stderr


def test_cooccurrence_does_not_cross_blobs(repo: Path):
    """An opening in one blob and closing in another are each allowed alone."""
    stage(repo, "opening.txt", b"<<<<<<< left\n")
    stage(repo, "closing.txt", b">>>>>>> right\n")
    assert run_hook(repo).returncode == 0


def test_removed_index_entry_and_empty_index(repo: Path):
    """No staged blob remains after removal; there is nothing to inspect."""
    assert run_hook(repo).returncode == 0
    stage(repo, "removed.txt", PAIR)
    git(repo, "update-index", "--force-remove", "removed.txt")
    assert run_hook(repo).returncode == 0


def test_git_failure_is_not_a_clean_result(tmp_path: Path):
    """A directory without an index cannot certify the staged content."""
    assert run_hook(tmp_path).returncode != 0


def test_missing_staged_object_is_not_a_clean_result(repo: Path):
    """An index entry without its blob cannot certify staged content."""
    oid = git(repo, "hash-object", "--stdin", data=PAIR).strip().decode()
    git(
        repo,
        "update-index",
        "--add",
        "--info-only",
        "--cacheinfo",
        "100644",
        oid,
        "missing.txt",
    )
    result = run_hook(repo)
    assert result.returncode != 0
    assert b"Could not inspect staged content" in result.stderr


@pytest.mark.parametrize("chunk_bytes", [1, 7, 8, 39, 64])
@pytest.mark.parametrize(
    ("content", "path", "expected"),
    [
        (b"\x00\xffheader\n" + PAIR, b"blob.bin", [2, 4]),
        (b"x" + FILE_OK + b"y\n" + PAIR, b"docs/plans/blob.md", []),
        (PAIR + FILE_OK, b"src/blob.txt", [1, 3]),
        (b"<<<<<<< # CONFLICT_MARKER_FORENSIC_OK\n>>>>>>>", b"blob.txt", [2]),
        (b"<<<<<<<\x00\n>>>>>>>\n", b"blob.bin", []),
        (("<<<<<<<\u2003left\n>>>>>>>\u2003right").encode(), b"blob.txt", [1, 2]),
    ],
)
def test_chunk_boundaries_preserve_literal_rules(
    chunk_bytes: int, content: bytes, path: bytes, expected: list[int]
):
    """Hand counts: PAIR anchors are lines 1/3, shifted by one after a header.

    Tiny buffer sizes deliberately divide anchors and sentinel tokens. These
    sizes are synthetic boundary cases, not production acceptance thresholds.
    """
    namespace = runpy.run_path(str(HELPER))
    scanner = namespace["scan_blob"]
    scanner.__globals__["CHUNK_BYTES"] = chunk_bytes
    assert scanner(io.BytesIO(content), path) == expected


def test_newline_free_input_uses_bounded_reads():
    """Generated NUL bytes contain no anchors; no full line is materialized.

    The synthetic stream spans 32 production buffers and refuses unbounded
    reads. Retained line summaries contain only the prefix and suffix.
    """
    namespace = runpy.run_path(str(HELPER))
    chunk_bytes = namespace["CHUNK_BYTES"]

    class BinaryStream:
        remaining = 32 * chunk_bytes

        def read(self, size: int) -> bytes:
            assert size == chunk_bytes
            count = min(size, self.remaining)
            self.remaining -= count
            return b"\x00" * count

    summaries = list(namespace["line_summaries"](BinaryStream()))
    assert summaries == [
        (b"\x00" * namespace["PREFIX_BYTES"], b"\x00" * namespace["TAIL_BYTES"], False)
    ]


def test_nul_cannot_manufacture_file_exemption(repo: Path):
    """Splitting the literal sentinel with NUL leaves both anchors actionable."""
    forged = FILE_OK.replace(b"FILE", b"FI\x00LE")
    stage(repo, "docs/plans/fixture.md", forged + b"\n" + PAIR)
    assert run_hook(repo).returncode == 1


def test_binary_diagnostics_contain_locations_only(repo: Path):
    """The two anchor line numbers are reported without arbitrary blob bytes."""
    stage(repo, "fixture.bin", b"<<<<<<< \xff\x00payload\n>>>>>>> \xff\x00payload")
    result = run_hook(repo)
    assert result.returncode == 1
    assert b"fixture.bin:1:" in result.stderr
    assert b"fixture.bin:2:" in result.stderr
    assert b"payload" not in result.stderr
    assert b"\x00" not in result.stderr
