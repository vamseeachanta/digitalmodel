# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Repository CSV references remain usable from a separate working directory."""

from pathlib import Path


def test_reference_csv_paths(real_csv_paths, tmp_path, monkeypatch):
    """The three references are tracked CSV inputs, with no solver invocation."""
    monkeypatch.chdir(tmp_path)
    root = Path(__file__).resolve().parents[5]
    assert set(real_csv_paths) == {"pretension", "stiffness", "fender"}
    for path in real_csv_paths.values():
        assert path.is_relative_to(root)
        assert path.is_file()
        assert path.suffix == ".csv"
