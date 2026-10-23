"""Hand-enumerated relative paths using Python 3.11 pathlib and os.path.

Source URLs are in digitalmodel._output_paths.relative_artifact_path.
No path existence or engineering calculation is assumed by these examples.
"""

from pathlib import Path

import pytest

from digitalmodel._output_paths import relative_artifact_path


@pytest.mark.parametrize("artifact,document,expected", [
    ("report.json", "report.html", "report.json"),
    ("details/report.json", "pages/report", "../details/report.json"),
    ("details #data/report.json", "pages/report", "../details #data/report.json"),
])
def test_relative_links_without_creating_files(tmp_path, artifact, document, expected):
    assert relative_artifact_path(tmp_path / artifact, tmp_path / document) == expected
    assert not list(tmp_path.iterdir())


def test_link_uses_both_symlink_targets(tmp_path):
    document = tmp_path / "report.html"
    artifact = tmp_path / "report.json"
    document.symlink_to(tmp_path / "pages" / "report.html")
    artifact.symlink_to(tmp_path / "details" / "report.json")
    assert relative_artifact_path(artifact, document) == "../details/report.json"


def test_incompatible_relative_roots_are_reported(tmp_path, monkeypatch):
    import digitalmodel._output_paths as paths

    def incompatible(*args):
        raise ValueError("Synthetic incompatible roots")

    monkeypatch.setattr(paths.os.path, "relpath", incompatible)
    with pytest.raises(ValueError, match="incompatible roots"):
        relative_artifact_path(Path("details.json"), tmp_path / "report.html")
