# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Checkout conversion must preserve the manifest-bound database bytes."""

import hashlib
import subprocess
from pathlib import Path

import pytest
import yaml

from digitalmodel.riser_database.loader import RiserDatabase


def _git_bytes(*args: str) -> bytes:
    return subprocess.run(
        ["git", *args],
        cwd=Path(__file__).resolve().parents[2],
        check=True,
        capture_output=True,
    ).stdout


@pytest.mark.parametrize("autocrlf", ["true", "false", "input"])
def test_checkout_preserves_database_fingerprints(tmp_path, autocrlf):
    """Expected hashes are the committed manifest's independent byte digests.

    Git's gitattributes manual, "text" and "eol", specifies LF storage and
    checkout for text eol=lf, regardless of core.autocrlf. The git-cat-file
    manual, "--filters", specifies working-tree end-of-line conversion.
    Sources: https://git-scm.com/docs/gitattributes#_eol and
    https://git-scm.com/docs/git-cat-file (accessed 2026-10-19).
    No engineering coefficients or numerical acceptance tolerances apply.
    """
    manifest = yaml.safe_load(
        _git_bytes("show", "HEAD:data/riser_database/manifest.yaml")
    )
    filenames = [entry["file"] for entry in manifest["tables"].values()]
    for filename in ["manifest.yaml", *filenames]:
        relative = f"data/riser_database/{filename}"
        raw = _git_bytes(
            "-c", f"core.autocrlf={autocrlf}", "cat-file", "--filters", f"HEAD:{relative}"
        )
        assert b"\r\n" not in raw, relative
        (tmp_path / filename).write_bytes(raw)

    for entry in manifest["tables"].values():
        digest = hashlib.sha256((tmp_path / entry["file"]).read_bytes()).hexdigest()
        assert digest == entry["sha256"], entry["file"]
    RiserDatabase.load(root=tmp_path)
