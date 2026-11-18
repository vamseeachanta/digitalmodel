"""Packaged reference records fail closed without touching canonical sources."""

import hashlib
from pathlib import Path

import pytest

from digitalmodel.citations.schema import CitationResolutionError
from digitalmodel.process_piping import _basis


def test_every_record_resolves():
    for key in ("pressure", "hydro", "bolting", "hydraulics", "velocity", "erosion"):
        assert _basis.citations(key)[0].source_sibling == "generic"


def test_missing_and_mismatched_basis_fail_closed(tmp_path, monkeypatch):
    source = _basis._BASIS_ROOT / "wikis" / "asme-b31-3.md"
    before = hashlib.sha256(source.read_bytes()).hexdigest()
    isolated = tmp_path / "wikis"
    isolated.mkdir()
    monkeypatch.setattr(_basis, "_BASIS_ROOT", tmp_path)
    with pytest.raises(CitationResolutionError, match="page_missing"):
        _basis.citations("pressure")
    copy = isolated / source.name
    copy.write_text(
        source.read_text(encoding="utf-8").replace('"2024"', '"2008"'), encoding="utf-8"
    )
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        _basis.citations("hydro")
    copy.write_bytes(source.read_bytes())
    assert _basis.citations("hydro")[0].revision == "2024"
    assert hashlib.sha256(source.read_bytes()).hexdigest() == before


def test_public_records_are_declared_package_data():
    import tomllib

    root = Path(__file__).resolve().parents[2]
    project = tomllib.loads((root / "pyproject.toml").read_text(encoding="utf-8"))
    assert (
        "process_piping/basis/wikis/*.md"
        in project["tool"]["setuptools"]["package-data"]["digitalmodel"]
    )
