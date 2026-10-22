# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Library metadata uses reproducible IDs and public workstation roles."""

import hashlib
import importlib
import json
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
    files = [root] if root.is_file() else sorted(
        path for path in root.rglob("*") if path.is_file()
    )
    assert files, "No library output was scanned"
    scan_root = root.parent if root.is_file() else root
    offenders = [
        path.relative_to(scan_root).as_posix()
        for path in files
        if INTERNAL_ALIAS.encode("ascii") in path.read_bytes().lower()
    ]
    assert not offenders, f"Internal workstation alias in: {offenders}"


@pytest.mark.parametrize("suffix", (".yaml", ".json", ".txt", ".parquet"))
@pytest.mark.parametrize("uppercase", (False, True))
@pytest.mark.parametrize("scan_file", (False, True))
def test_scan_rejects_planted_alias(tmp_path, suffix, uppercase, scan_file):
    alias = INTERNAL_ALIAS.upper() if uppercase else INTERNAL_ALIAS
    output = tmp_path / "nested" / f"output{suffix}"
    output.parent.mkdir()
    output.write_bytes(b"\xff " + alias.encode("ascii"))
    match = "output" if scan_file else "nested/output"
    with pytest.raises(AssertionError, match=match):
        assert_no_internal_alias(output if scan_file else tmp_path)


def test_scan_accepts_workstation_role(tmp_path):
    (tmp_path / "manifest.yaml").write_text("note: licensed-solver workstation\n")
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
    assert "licensed-solver workstation" in generated.provenance["solver"]["note"]
    committed = Atlas.load(ROOT / "atlases", basename)
    assert generated.surrogate_spec() == committed.surrogate_spec()
    assert generated.provenance == committed.provenance


@pytest.mark.parametrize("builder_name,basename,rows", LIBRARIES)
def test_committed_library_has_no_internal_alias(builder_name, basename, rows):
    assert_no_internal_alias(ROOT / "atlases" / basename)
    assert_no_internal_alias(
        ROOT / "docs/plans/parametric-refresh/licensed-libraries.md"
    )
    builders = sorted((ROOT / "scripts/parametric").glob("build_*library.py"))
    assert builders, "No library builder was scanned"
    for builder in builders:
        assert_no_internal_alias(builder)
    manifests = sorted((ROOT / "atlases" / basename).glob("*/manifest.yaml"))
    assert manifests, "No committed library manifest was scanned"
    for manifest in manifests:
        assert_library_loads(ROOT / "atlases", basename, rows, manifest.parent.name)


@pytest.mark.parametrize("builder_name,basename,rows", LIBRARIES)
def test_committed_library_directory_matches_manifest_hash(builder_name, basename, rows):
    directories = sorted(
        path for path in (ROOT / "atlases" / basename).iterdir() if path.is_dir()
    )
    assert directories, "No committed library directory was checked"
    for directory in directories:
        manifest = yaml.safe_load((directory / "manifest.yaml").read_text())
        provenance = manifest["provenance"]
        blob = json.dumps(
            {
                "axes": manifest["axes"],
                "solver": provenance["solver"],
                "cases": provenance["coverage"]["covered_cases"],
            },
            sort_keys=True,
        )
        expected_id = hashlib.sha256(blob.encode()).hexdigest()[:12]
        assert directory.name == manifest["atlas_id"] == expected_id
