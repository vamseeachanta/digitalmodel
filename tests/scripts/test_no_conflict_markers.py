# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Staged-blob contracts for the conflict-marker hook (issue #2310).

Expected results are hand-derived from the policy in
scripts/enforcement/check-no-conflict-markers.sh: both column-zero anchors
must occur in one blob; a marker needs whitespace or end of line after its
seven delimiter bytes. Only the documented end-of-line and restricted-path
sentinels exempt markers. Fixture sizes are synthetic, not acceptance limits.
"""

import importlib.util
import io
import os
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
HOOK = ROOT / "scripts/enforcement/check-no-conflict-markers.sh"
OPEN = b"<<<<<<< left"
CLOSE = b">>>>>>> right"
PAIR = OPEN + b"\nbody\n" + CLOSE + b"\n"
FILE_OK = b"<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->"


@dataclass
class Repository:
    root: Path
    env: dict = field(repr=False)


@pytest.fixture
def repository(tmp_path):
    """Use an isolated index without changing repository metadata or identity."""
    env = os.environ.copy()
    for name in tuple(env):
        if name.startswith("GIT_"):
            env.pop(name)
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    return Repository(tmp_path, env)


def stage(repository, name, content):
    root, env = repository.root, repository.env
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)
    subprocess.run(["git", "add", "--", name], cwd=root, env=env, check=True)


def run_hook(repository):
    root, env = repository.root, repository.env
    return subprocess.run(
        ["bash", str(HOOK)], cwd=root, env=env, capture_output=True, check=False
    )


@pytest.mark.parametrize(
    ("content", "blocked"),
    [
        (b"image\x00payload\xff", False),
        (b"<<<<\x00<<< left\n" + CLOSE + b"\n", False),
        (OPEN + b"\x00\n" + CLOSE + b"\n", True),
        (b"binary\x00\xff\n" + PAIR, True),
        (b"\x00" + OPEN + b"\n" + CLOSE + b"\n", False),
        (b"<<<<<<<\x00 left\n" + CLOSE + b"\n", False),
    ],
    ids=[
        "binary",
        "split-anchor",
        "nul-in-label",
        "binary-and-text",
        "nul-prefix",
        "nul-boundary",
    ],
)
def test_binary_bytes_keep_their_meaning(repository, content, blocked):
    """NUL is neither an anchor byte, whitespace, nor a line boundary.

    Counting literal anchors gives the expected pass/block values above;
    deleting NUL would incorrectly create anchors in three passing cases.
    """
    stage(repository, "payload.bin", content)
    result = run_hook(repository)
    assert result.returncode == int(blocked)
    assert b"ignored null byte" not in result.stderr
    assert b"\x00" not in result.stderr


@pytest.mark.parametrize(
    ("content", "blocked"),
    [
        (b"", False),
        (b"=======\n", False),
        (OPEN + b"\n", False),
        (CLOSE + b"\n", False),
        (PAIR, True),
        (OPEN + b"\n" + CLOSE, True),
        (b"<<<<<<<\n>>>>>>>\n", True),
        (b"<<<<<<<\tleft\n>>>>>>>\rright\n", True),
        (b"<<<<<<<left\n>>>>>>>right\n", False),
        (b" " + OPEN + b"\n " + CLOSE + b"\n", False),
    ],
)
def test_text_policy(repository, content, blocked):
    """Manual column and suffix checks give the expected truth table."""
    stage(repository, "notes.txt", content)
    result = run_hook(repository)
    assert result.returncode == int(blocked)


@pytest.mark.parametrize(
    "sentinel",
    [
        b"# CONFLICT_MARKER_FORENSIC_OK",
        b"// CONFLICT_MARKER_FORENSIC_OK",
        b"<!-- CONFLICT_MARKER_FORENSIC_OK -->",
    ],
)
def test_line_exemptions(repository, sentinel):
    """Two end-of-line sentinels exempt both anchors; one leaves a violation."""
    stage(
        repository,
        "notes.txt",
        OPEN + b" " + sentinel + b"\n" + CLOSE + b" " + sentinel,
    )
    assert run_hook(repository).returncode == 0
    stage(repository, "notes.txt", OPEN + b" " + sentinel + b"\n" + CLOSE)
    result = run_hook(repository)
    assert result.returncode == 1
    assert b"notes.txt:2:>>>>>>> right" in result.stderr
    assert b"notes.txt:1:" not in result.stderr


@pytest.mark.parametrize(
    "prefix",
    [
        "docs/plans/",
        "docs/governance/",
        "docs/standards/",
        "scripts/review/results/",
        "scripts/review/prompts/",
    ],
)
def test_restricted_file_exemptions(repository, prefix):
    """Each existing allowed prefix exempts the literal whole-file sentinel."""
    stage(repository, prefix + "notes.md", PAIR + FILE_OK)
    assert run_hook(repository).returncode == 0


@pytest.mark.parametrize("name", ["src/notes.txt", "docs/plans-other/notes.md"])
def test_file_sentinel_cannot_exempt_other_paths(repository, name):
    """Neither path starts with an allowed prefix, so both anchors remain."""
    stage(repository, name, FILE_OK + b"\n" + PAIR)
    assert run_hook(repository).returncode == 1


def test_nul_cannot_create_file_sentinel(repository):
    """Inserting NUL into the sentinel destroys its literal byte match."""
    stage(
        repository,
        "docs/plans/notes.md",
        FILE_OK.replace(b"FILE", b"FI\x00LE") + b"\n" + PAIR,
    )
    assert run_hook(repository).returncode == 1


def test_index_is_the_only_content_source(repository):
    """A working-tree exemption cannot hide staged markers, or the reverse."""
    root = repository.root
    name = "docs/plans/notes.md"
    stage(repository, name, PAIR)
    (root / name).write_bytes(FILE_OK)
    assert run_hook(repository).returncode == 1
    stage(repository, name, FILE_OK + b"\n" + PAIR)
    (root / name).write_bytes(PAIR)
    assert run_hook(repository).returncode == 0


def test_cooccurrence_is_per_blob(repository):
    """An opening marker and closing marker in different files do not pair."""
    stage(repository, "opening.txt", OPEN)
    stage(repository, "closing.txt", CLOSE)
    assert run_hook(repository).returncode == 0


def test_unusual_filename_and_line_numbers(repository):
    """The opening and closing anchors are the second and fourth lines."""
    name = "-notes with spaces\nand-tab\t.txt"
    stage(repository, name, b"intro\n" + OPEN + b"\nbody\n" + CLOSE)
    result = run_hook(repository)
    assert result.returncode == 1
    assert b":2:<<<<<<< left" in result.stderr
    assert b":4:>>>>>>> right" in result.stderr


def test_empty_index(repository):
    """No staged paths means no violations."""
    result = run_hook(repository)
    assert result.returncode == 0
    assert result.stderr == b""


@pytest.mark.parametrize("suffix", [b" ", b"\r", b"\x00"])
def test_line_sentinel_must_be_the_literal_suffix(repository, suffix):
    """An extra suffix byte means the sentinel is no longer at end of line."""
    marker = OPEN + b" # CONFLICT_MARKER_FORENSIC_OK" + suffix
    stage(repository, "notes.txt", marker + b"\n" + CLOSE)
    result = run_hook(repository)
    assert result.returncode == 1
    assert b"notes.txt:1:" in result.stderr
    assert b"notes.txt:2:" in result.stderr


def test_index_read_failure_is_not_a_clean_scan(tmp_path):
    """A directory without an index cannot establish absence of violations."""
    env = {
        key: value for key, value in os.environ.items() if not key.startswith("GIT_")
    }
    # Prevent discovery of a parent repository if tests use an in-tree temp root.
    env["GIT_CEILING_DIRECTORIES"] = str(tmp_path)
    result = run_hook(Repository(tmp_path, env))
    assert result.returncode == 1
    assert b"scan incomplete" in result.stderr


def test_unreadable_staged_object_is_not_a_clean_scan(repository):
    """A staged object identifier with no stored blob must block the scan."""
    root, env = repository.root, repository.env
    oid = (
        subprocess.run(
            ["git", "hash-object", "--stdin"],
            input=b"object intentionally not written",
            cwd=root,
            env=env,
            capture_output=True,
            check=True,
        )
        .stdout.strip()
        .decode("ascii")
    )
    subprocess.run(
        ["git", "update-index", "--add", "--cacheinfo", f"100644,{oid},missing.txt"],
        cwd=root,
        env=env,
        check=True,
    )
    result = run_hook(repository)
    assert result.returncode == 1
    assert b"scan incomplete" in result.stderr


@pytest.fixture
def scanner():
    spec = importlib.util.spec_from_file_location(
        "conflict_scanner", ROOT / "scripts/enforcement/check_no_conflict_markers.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class BoundedStream:
    """Reject unbounded line reads, including the implicit iterator protocol."""

    def __init__(self, content):
        self.stream = io.BytesIO(content)

    def readline(self, size=-1):
        assert size > 0, "blob reads must have an explicit bound"
        return self.stream.readline(size)

    def __iter__(self):
        while line := self.readline():
            yield line


@pytest.mark.parametrize("offset", [-1, 0, 1])
def test_bounded_reads_and_sentinel_crossing(scanner, offset):
    """A literal sentinel exempts even when split across a read boundary.

    Prefix length places its midpoint on either side of the standard-library
    buffer boundary. Expected [] follows from the allowed path and exact
    sentinel bytes, independently of the scanner's chunking implementation.
    """
    prefix = b"x" * (io.DEFAULT_BUFFER_SIZE - len(PAIR) - len(FILE_OK) // 2 + offset)
    stream = BoundedStream(PAIR + prefix + FILE_OK + b"\n")
    assert scanner.scan_blob(stream, restricted=True) == []


def test_bounded_binary_reads_and_final_markers(scanner):
    """Three synthetic buffer lengths precede anchors on lines two and three.

    Literal anchors survive NUL-containing data. Byte counting gives the
    expected line numbers: the binary prefix is one LF-terminated line.
    """
    stream = BoundedStream(
        b"x\x00" * (io.DEFAULT_BUFFER_SIZE * 3) + b"\n" + OPEN + b"\n" + CLOSE
    )
    assert scanner.scan_blob(stream, restricted=False) == [(2, OPEN), (3, CLOSE)]


def test_marker_diagnostic_crosses_buffer_boundary(scanner):
    """A long anchor line ending in a line sentinel remains exempt."""
    opening = OPEN + b"x" * io.DEFAULT_BUFFER_SIZE + b" # CONFLICT_MARKER_FORENSIC_OK\n"
    stream = BoundedStream(opening + CLOSE)
    assert scanner.scan_blob(stream, restricted=False) == [(2, CLOSE)]


@pytest.mark.parametrize(
    ("name", "prefix", "suffix", "blocked"),
    [
        ("payload.bin", b"", b"", False),
        ("payload.bin", b"", b"\n" + PAIR, True),
        ("docs/plans/notes.md", b"", b"\n" + PAIR + FILE_OK, False),
        ("docs/plans/notes.md", FILE_OK + b"\n" + PAIR, b"", False),
    ],
    ids=[
        "binary-no-newlines",
        "late-markers",
        "late-file-exemption",
        "early-file-exemption",
    ],
)
def test_large_staged_blob(repository, name, prefix, suffix, blocked):
    """Synthetic 16 MiB binary body has no LF or marker bytes.

    Hand calculation: two bytes repeated 8 * 1024 * 1024 times = 16 MiB.
    Only the literal prefix or suffix can add anchors or a file exemption,
    giving the expected outcomes above. This exercises streaming and draining
    after early exemption; there is no execution time limit.
    """
    stage(repository, name, prefix + b"\x00\xff" * (8 * 1024 * 1024) + suffix)
    result = run_hook(repository)
    assert result.returncode == int(blocked)
    assert b"ignored null byte" not in result.stderr


def test_non_utf8_filename_and_diagnostic(repository):
    """Filesystem bytes survive path selection; labels are escaped for output."""
    name = os.fsdecode(b"notes-\xff.txt")
    stage(repository, name, OPEN + b"\xff\x00\n" + CLOSE)
    result = run_hook(repository)
    assert result.returncode == 1
    assert b"notes-\\xff.txt:1:<<<<<<< left\\xff\\x00" in result.stderr
    assert b"\x00" not in result.stderr
