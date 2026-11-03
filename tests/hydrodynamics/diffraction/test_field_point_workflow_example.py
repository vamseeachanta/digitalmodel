# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline field-point input preparation; no solver results are exercised."""

from pathlib import Path
from shutil import copytree

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

REPO_ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = REPO_ROOT / "examples/workflows/orcawave-field-points"
MESH_SOURCE = Path(__file__).parent / "fixtures/acceptance_610/unit_box.gdf"
# Independent synthetic request, documented in the example README's Input basis.
EXPECTED_POINTS = [[2.0, 0.0, 0.5], [2.0, 0.0, 1.0], [-2.0, 0.0, 0.5]]


def test_field_point_example_converts(tmp_path):
    """Load the committed spec and package its exact reference mesh offline."""
    spec = DiffractionSpec.from_yaml(EXAMPLE / "spec.yml")
    assert spec.outputs.field_points[0].name == "airgap_splash"
    converter = SpecConverter(EXAMPLE / "spec.yml")
    assert converter.validate() == []
    generated = converter.convert(solver="orcawave", output_dir=tmp_path)
    assert generated.is_file()
    data = yaml.safe_load(generated.read_text())
    mesh = tmp_path / data["Bodies"][0]["BodyMeshFileName"]
    assert mesh.read_bytes() == MESH_SOURCE.read_bytes()
    assert data["DetectAndSkipFieldPointsInsideBodies"] is True
    assert "DetectAndSkipFieldPointsInsideBodies: Yes" in generated.read_text()


@pytest.mark.parametrize("detect_inside", [True, False], ids=["skip", "retain"])
def test_coordinates_preserved(tmp_path, detect_inside):
    """Keep point row order and use the global inside-body switch."""
    copied = copytree(EXAMPLE, tmp_path / "example")
    spec_path = copied / "spec.yml"
    raw = yaml.safe_load(spec_path.read_text())
    raw["outputs"]["detect_field_points_inside_bodies"] = detect_inside
    # The schema documents the group flag as informational only. Set it to the
    # opposite value to prove the global switch governs generated input.
    raw["outputs"]["field_points"][0]["detect_inside_bodies"] = not detect_inside
    spec_path.write_text(yaml.safe_dump(raw))
    spec = DiffractionSpec.from_yaml(spec_path)
    assert [
        list(point) for point in spec.outputs.field_points[0].points
    ] == EXPECTED_POINTS
    generated = SpecConverter(spec_path).convert(
        solver="orcawave", output_dir=tmp_path / "prepared"
    )
    data = yaml.safe_load(generated.read_text())
    assert data["FieldPointX, FieldPointY, FieldPointZ"] == EXPECTED_POINTS
    assert data["DetectAndSkipFieldPointsInsideBodies"] is detect_inside
    expected_switch = "Yes" if detect_inside else "No"
    assert (
        f"DetectAndSkipFieldPointsInsideBodies: {expected_switch}"
        in generated.read_text()
    )


def test_relocated_example_mesh_resolves(tmp_path, monkeypatch):
    """A copied example resolves from its spec, independently of the cwd."""
    copied = copytree(EXAMPLE, tmp_path / "relocated")
    unrelated = tmp_path / "unrelated"
    unrelated.mkdir()
    monkeypatch.chdir(unrelated)
    converter = SpecConverter(copied / "spec.yml")
    assert converter.validate() == []
    generated = converter.convert(
        solver="orcawave", output_dir=tmp_path / "prepared"
    )
    data = yaml.safe_load(generated.read_text())
    reference = Path(data["Bodies"][0]["BodyMeshFileName"])
    assert reference == Path("unit_box.gdf")
    assert (generated.parent / reference).read_bytes() == MESH_SOURCE.read_bytes()
    assert data["FieldPointX, FieldPointY, FieldPointZ"] == EXPECTED_POINTS


def test_example_dispatch_is_conversion_only():
    """Dispatch prepares input without invoking the diffraction runner."""
    workflow = yaml.safe_load((EXAMPLE / "input.yml").read_text())["diffraction"]
    assert workflow["operation"] == "convert_spec"
    assert workflow["solver"] == "orcawave"
    assert workflow["format"] == "single"
    assert (EXAMPLE / workflow["spec"]).resolve() == EXAMPLE / "spec.yml"
