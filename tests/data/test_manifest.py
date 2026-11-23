# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the retrieval manifest without requiring a mounted store."""
import csv
from pathlib import Path, PurePosixPath
import re
import subprocess


ROOT = Path(__file__).resolve().parents[2]


def test_manifest_rows_are_well_formed_and_untracked():
    with (ROOT / "data/MANIFEST.tsv").open() as stream:
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
