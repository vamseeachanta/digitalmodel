"""Offline qualification packaging preserves inputs and reports reference differences."""
import importlib.util
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.smoke.model_manifest import verify_manifest

SCRIPT = Path(__file__).resolve().parents[3] / "scripts/prepare_mooring_qualification.py"


@pytest.fixture
def packaging(monkeypatch):
    monkeypatch.setenv("PYTHONHASHSEED", "0")
    import OrcFxAPI

    def prohibited(*args, **kwargs):
        pytest.fail("packaging attempted native construction")

    monkeypatch.setattr(OrcFxAPI, "Model", prohibited)
    spec = importlib.util.spec_from_file_location("qualification_packaging", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_existing_output_is_rejected_without_changes(packaging, tmp_path):
    marker = tmp_path / "keep.txt"
    marker.write_text("original")
    with pytest.raises((ValueError, FileExistsError)):
        packaging.prepare_bundle(tmp_path)
    assert marker.read_text() == "original"
    assert list(tmp_path.iterdir()) == [marker]


@pytest.mark.parametrize("seed", [None, "1"])
def test_seed_required_before_writing(packaging, tmp_path, monkeypatch, seed):
    monkeypatch.delenv("PYTHONHASHSEED", raising=False)
    if seed is not None:
        monkeypatch.setenv("PYTHONHASHSEED", seed)
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="seed"):
        packaging.prepare_bundle(output)
    assert not output.exists()


def test_complete_bundle_and_reference_differences(packaging, tmp_path):
    original = packaging.SOURCE.read_bytes()
    manifest_path = packaging.prepare_bundle(tmp_path / "output")
    data = verify_manifest(manifest_path)
    root = data["_root"]
    assert (root / "source/spec.yml").read_bytes() == original
    assert packaging.SOURCE.read_bytes() == original
    actual = {p.relative_to(root).as_posix() for p in (root / "bundle").rglob("*") if p.is_file()}
    listed = {item["path"] for item in data["files"]}
    assert actual | {"source/spec.yml"} == listed
    assert [x["path"] for x in data["files"]] == sorted(listed)
    assert data["master"] == "bundle/master.yml"
    differences = json.loads((root / "reference-differences.json").read_text())
    assert differences["reference_sha256"]
    assert differences["difference_count"] == len(differences["differences"])
    assert differences["difference_count"] > 0
    assert differences["reference_compatible"] is False
    assert all("path" in item for item in differences["differences"])


def test_existing_file_is_rejected(packaging, tmp_path):
    output = tmp_path / "file"
    output.write_text("preserve")
    with pytest.raises(FileExistsError):
        packaging.prepare_bundle(output)
    assert output.read_text() == "preserve"


def test_failed_generation_removes_only_owned_output(packaging, tmp_path, monkeypatch):
    def fail(*args):
        raise ValueError("injected generation failure")

    monkeypatch.setattr(packaging.ModularModelGenerator, "generate", fail)
    neighbor = tmp_path / "neighbor"
    neighbor.write_text("preserve")
    output = tmp_path / "output"
    with pytest.raises(ValueError, match="injected"):
        packaging.prepare_bundle(output)
    assert not output.exists()
    assert neighbor.read_text() == "preserve"


def test_reference_diff_preserves_nested_missing_and_typed_values(packaging):
    before = {"General": {"a": [False, 2], "gone": 3}}
    after = {"General": {"a": [0], "added": 4}}
    differences = list(packaging._differences(before, after))
    assert {x["path"] for x in differences} == {
        "/General/a/0", "/General/a/1", "/General/gone", "/General/added",
    }
    assert all(x["reference_value_sha256"] != x["candidate_value_sha256"]
               for x in differences)
