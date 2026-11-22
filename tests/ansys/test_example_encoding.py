"""Regress the existing example guards under an emulated cp1252 text default."""

from pathlib import Path
import runpy

import pytest


@pytest.fixture
def cp1252_default(monkeypatch):
    """Emulate Path.open's locale default while retaining explicit encodings."""
    original = Path.open

    def open_text(path, mode="r", buffering=-1, encoding=None, errors=None, newline=None):
        if "b" not in mode and encoding is None:
            encoding = "cp1252"
        if "b" not in mode and newline is None:
            newline = "\r\n"
        return original(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", open_text)


@pytest.mark.parametrize("name", ["padeye", "mudmat", "pressure_vessel_template"])
def test_existing_example_guard_with_cp1252_default(name, cp1252_default):
    """Existing guards must compare the UTF-8 deck to its generator without drift."""
    namespace = runpy.run_path(str(Path(__file__).with_name(f"test_{name}.py")))
    namespace["test_committed_example_inp_matches_generator"]()


@pytest.mark.parametrize("name", ["padeye", "mudmat"])
def test_generator_writer_with_cp1252_default(name, cp1252_default, tmp_path):
    """UTF-8 output bytes must equal independently encoded generated text."""
    namespace = runpy.run_path(str(Path(__file__).with_name(f"test_{name}.py")))
    geometry = namespace[f"{name.title()}Geometry"]()
    expected = namespace[f"generate_{name}_apdl"](geometry).encode("utf-8")
    result = namespace[f"write_{name}_inp"](geometry, tmp_path / f"{name}.inp")
    assert result.read_bytes() == expected
