# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Read-only integrity checks for tracked atlas default pointers (Refs #1350).

The layout is documented in docs/plans/parametric-refresh/atlas-spec.md,
Decision 2. The identifier contract comes from generate.py::_atlas_id and
generate.py::generate_atlas in src/digitalmodel/parametric: both retain the
first 12 lowercase hexadecimal characters of a SHA-256 digest. Library
identities use the same truncation in library.py::build_library_atlas.
Atlas.save in atlas.py writes that identity to default.txt and manifest.yaml.
"""

from __future__ import annotations

import os
import re
import subprocess
from pathlib import Path

import pytest
import yaml


@pytest.fixture(scope="module")
def tracked_default_pointers() -> tuple[Path, ...]:
    """Discover tracked pointers independently of the registry and loader."""
    root = Path(__file__).resolve().parent.parent.parent
    # Inherited Git bindings must not redirect discovery to another checkout.
    env = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    tracked = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "atlases/**/default.txt"],
        cwd=root,
        env=env,
    )
    pointers = tuple(root / os.fsdecode(name) for name in tracked.split(b"\0") if name)
    assert pointers, "No tracked atlas default pointers were found"
    return pointers


def test_default_pointers_have_single_identifiers(tracked_default_pointers):
    """Reject doubled identifiers, extra lines, whitespace and non-hex text."""
    for pointer in tracked_default_pointers:
        # The length is fixed by the identity definitions cited above, rather
        # than inferred from the current pointer contents. One final LF is OK.
        assert re.fullmatch(r"[0-9a-f]{12}\n?", pointer.read_text(encoding="utf-8")), (
            f"{pointer.parent.name}/default.txt must contain a single atlas identifier"
        )


def test_default_pointers_target_existing_manifests(tracked_default_pointers):
    """Every tracked default must resolve to a directory containing a manifest."""
    for pointer in tracked_default_pointers:
        atlas_id = pointer.read_text(encoding="utf-8").strip()
        target = pointer.parent / atlas_id
        assert target.is_dir(), f"{pointer.parent.name}: missing atlas directory"
        assert (target / "manifest.yaml").is_file(), (
            f"{pointer.parent.name}/{atlas_id}: missing manifest.yaml"
        )


def test_default_pointers_match_manifest_identity(tracked_default_pointers):
    """Check manifest data, including the basename, without loading a surrogate."""
    for pointer in tracked_default_pointers:
        atlas_id = pointer.read_text(encoding="utf-8").strip()
        manifest = yaml.safe_load(
            (pointer.parent / atlas_id / "manifest.yaml").read_text(encoding="utf-8")
        )
        assert isinstance(manifest, dict), f"{pointer.parent.name}: invalid manifest"
        assert manifest.get("atlas_id") == atlas_id, (
            f"{pointer.parent.name}: pointer and manifest atlas_id disagree"
        )
        assert manifest.get("basename") == pointer.parent.name, (
            f"{pointer.parent.name}: directory and manifest basename disagree"
        )
