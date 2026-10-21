# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Library metadata describes compute capacity without internal aliases."""

import importlib
from pathlib import Path

import pytest
import yaml

from digitalmodel.parametric.atlas import Atlas


ROOT = Path(__file__).resolve().parents[2]
INTERNAL_ALIAS = "-".join(("dev", "secondary"))
LIBRARIES = (
    ("build_diffraction_library", "diffraction_library", 120),
    ("build_drilling_riser_drift_off_library", "drilling_riser_drift_off", 60),
    ("build_drilling_riser_envelope_library", "drilling_riser_envelope", 900),
    ("build_orcaflex_library", "orcaflex_library", 15),
)


def assert_no_internal_alias(root):
    """Scan every emitted file, including binary output, without an empty pass."""
    files = sorted(path for path in root.rglob("*") if path.is_file())
    assert files, "No library output was scanned"
    offenders = [
        path.relative_to(root).as_posix()
        for path in files
        if INTERNAL_ALIAS.encode("ascii") in path.read_bytes().lower()
    ]
    assert not offenders, f"Internal workstation alias in: {offenders}"


@pytest.mark.parametrize("suffix", (".yaml", ".json", ".txt", ".parquet"))
@pytest.mark.parametrize("uppercase", (False, True))
def test_scan_rejects_planted_alias(tmp_path, suffix, uppercase):
    alias = INTERNAL_ALIAS.upper() if uppercase else INTERNAL_ALIAS
    output = tmp_path / "nested" / f"output{suffix}"
    output.parent.mkdir()
    output.write_bytes(b"\xff " + alias.encode("ascii"))
    with pytest.raises(AssertionError, match="nested/output"):
        assert_no_internal_alias(tmp_path)


def test_scan_accepts_workstation_capacity(tmp_path):
    (tmp_path / "manifest.yaml").write_text("note: 32-core workstation\n")
    assert_no_internal_alias(tmp_path)


def test_scan_rejects_empty_output(tmp_path):
    with pytest.raises(AssertionError, match="No library output"):
        assert_no_internal_alias(tmp_path)


def assert_library_loads(root, basename, rows, atlas_id=None):
    """Read the manifest with the repository loader and check its companions."""
    atlas = Atlas.load(root, basename, atlas_id)
    manifest_path = root / basename / atlas.atlas_id / "manifest.yaml"
    manifest = yaml.safe_load(manifest_path.read_text())
    spec = atlas.surrogate_spec()
    for key, value in spec.items():
        assert manifest[key] == value
    assert len(atlas.grid) == rows
    assert atlas.provenance["solver"]["licensed"] is False
    assert atlas.provenance["solver"]["version"] == "STUB"


@pytest.mark.parametrize("builder_name,basename,rows", LIBRARIES)
def test_generated_library_has_no_internal_alias(
    tmp_path, monkeypatch, capsys, builder_name, basename, rows,
):
    builder = importlib.import_module(f"scripts.parametric.{builder_name}")
    monkeypatch.setattr(builder, "REPO_ROOT", tmp_path)
    builder.main()
    assert INTERNAL_ALIAS not in capsys.readouterr().out.casefold()
    assert_no_internal_alias(tmp_path / "atlases" / basename)
    assert_library_loads(tmp_path / "atlases", basename, rows)
    generated = Atlas.load(tmp_path / "atlases", basename)
    committed = Atlas.load(ROOT / "atlases", basename)
    assert generated.surrogate_spec() == committed.surrogate_spec()
    assert generated.provenance == committed.provenance


@pytest.mark.parametrize("builder_name,basename,rows", LIBRARIES)
def test_committed_library_has_no_internal_alias(builder_name, basename, rows):
    assert_no_internal_alias(ROOT / "atlases" / basename)
    manifests = sorted((ROOT / "atlases" / basename).glob("*/manifest.yaml"))
    assert manifests, "No committed library manifest was scanned"
    for manifest in manifests:
        assert_library_loads(ROOT / "atlases", basename, rows, manifest.parent.name)
