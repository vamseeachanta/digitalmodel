# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic symlink boundary cases for clone validation (Refs #618)."""

import pytest

from digitalmodel.citations.resolver import _validate_clone


@pytest.fixture(params=["standalone", "overlay"])
def clone_link(tmp_path, request):
    """Provide a clone base and its layout-specific wiki link location."""
    base = tmp_path / "clone"
    wiki_parent = base if request.param == "standalone" else base / "knowledge"
    wiki_parent.mkdir(parents=True)
    return base, wiki_parent / "wikis"


def test_validate_clone_rejects_symlink_loop(clone_link):
    """A cyclic wiki link cannot supply a contained directory."""
    base, wiki_link = clone_link
    cycle_link = wiki_link.parent / "cycle"
    wiki_link.symlink_to(cycle_link.name, target_is_directory=True)
    cycle_link.symlink_to(wiki_link.name, target_is_directory=True)

    assert _validate_clone(base) is None


def test_validate_clone_accepts_retargeted_contained_link(tmp_path, clone_link):
    """Revalidate containment after each retarget, including escape and recovery."""
    base, wiki_link = clone_link
    initial_target = base / "initial"
    replacement_target = base / "replacement"
    outside_target = tmp_path / "clone-sibling"
    for target in (initial_target, replacement_target, outside_target):
        target.mkdir()

    # Expected acceptance follows the created topology: both targets are in base.
    wiki_link.symlink_to(initial_target, target_is_directory=True)
    assert _validate_clone(base) == base.resolve(strict=True)

    wiki_link.unlink()
    wiki_link.symlink_to(replacement_target, target_is_directory=True)
    assert _validate_clone(base) == base.resolve(strict=True)

    # The sibling shares the base name prefix but is outside the clone boundary.
    wiki_link.unlink()
    wiki_link.symlink_to(outside_target, target_is_directory=True)
    assert _validate_clone(base) is None

    wiki_link.unlink()
    wiki_link.symlink_to(initial_target, target_is_directory=True)
    assert _validate_clone(base) == base.resolve(strict=True)
