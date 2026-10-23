# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the retrieval manifest without requiring a mounted store."""
import csv
from pathlib import Path, PurePosixPath
import re
import subprocess


ROOT = Path(__file__).resolve().parents[2]


def test_manifest_rows_are_well_formed_and_untracked():
    with (ROOT / "data/MANIFEST.tsv").open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        assert reader.fieldnames == ["path", "bytes", "sha256", "store_id"]
        rows = list(reader)
    assert rows
    tracked = set(subprocess.check_output(
        ["git", "ls-files", "-z"], cwd=ROOT,
    ).decode().split("\0"))
    assert len({r['path'] for r in rows}) == len(rows)
    for row in rows:
        path = PurePosixPath(row['path'])
        assert not path.is_absolute() and ".." not in path.parts
        assert row['path'] not in tracked
        assert int(row['bytes']) > 1048576
        digest = row['sha256']
        assert re.fullmatch(r"[0-9a-f]{64}", digest)
        assert re.fullmatch(rf"blobs/sha256/{digest[:2]}/{digest}\.[a-z0-9]+", row['store_id'])
        assert not row['path'].startswith(("tests/", "src/", "examples/"))


def test_trim_manifest_and_exact_ignore_entries_agree():
    """Documentation trim rows equal the explicit ignore block and Git's result.

    The set is derived independently from Git's treatment of absent paths and
    the contiguous content-store ignore block, not the retrieval implementation.
    """
    with (ROOT / "data/MANIFEST.tsv").open(encoding="utf-8", newline="") as stream:
        paths = {row["path"] for row in csv.DictReader(stream, delimiter="\t")}
    lines = (ROOT / ".gitignore").read_text(encoding="utf-8").splitlines()
    trim_paths = {path for path in paths if path.startswith("docs/")}
    start = lines.index("# fetched by scripts/data/fetch.py") + 1
    end = start
    while end < len(lines) and lines[end].startswith("/"):
        end += 1
    assert {line.removeprefix("/") for line in lines[start:end]} == trim_paths
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--stdin", "-z"],
        input="\0".join(sorted(trim_paths)) + "\0", cwd=ROOT,
        text=True, capture_output=True, check=True,
    )
    assert set(result.stdout.rstrip("\0").split("\0")) == trim_paths
