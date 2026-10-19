"""Canonical destination expectations derived by hand for issue #2315.

``child/../report`` and ``report`` address the same fixture file; a symlink
directory addresses its target directory. These are path-identity assertions,
not numerical acceptance tests. Source: Python 3.11 pathlib documentation,
Path.resolve and Path.samefile (linked in the helper's docstring).
"""

from hashlib import sha256

import pytest

from digitalmodel._output_paths import validate_output_paths


def test_normalized_nonexistent_destinations_collide_without_creating_directories(tmp_path):
    output = tmp_path / "new" / "report"
    alias = tmp_path / "new" / "child" / ".." / "report"
    with pytest.raises(ValueError, match="distinct"):
        validate_output_paths(output, alias)
    assert not output.parent.exists()


def test_directory_symlink_aliases_are_rejected(tmp_path):
    directory = tmp_path / "details"
    directory.mkdir()
    alias = tmp_path / "alias"
    alias.symlink_to(directory, target_is_directory=True)
    with pytest.raises(ValueError, match="distinct"):
        validate_output_paths(directory / "report", alias / "report")
    assert not list(directory.iterdir())


def test_distinct_existing_files_are_not_changed(tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    for path in (first, second):
        path.write_bytes(b"retained input")
    before = {path: sha256(path.read_bytes()).digest() for path in (first, second)}
    validate_output_paths(first, second)
    assert {path: sha256(path.read_bytes()).digest() for path in before} == before


def test_empty_output_set_is_valid():
    validate_output_paths()
