"""Self-contained convert-spec packages (#605).

`SpecConverter.convert(solver="orcawave")` must emit a runnable directory:
every referenced mesh (body, damping lid, control surface, free-surface zone)
copied alongside the generated input, which references them by basename.
Missing auxiliary meshes must hard-fail like missing body meshes (#500).
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.mesh_packaging import (
    iter_mesh_references,
)
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

FIXTURES_DIR = Path(__file__).parent / "fixtures"
MESH_FIXTURE = (
    Path(__file__).parents[1] / "bemrosetta" / "fixtures" / "sample_box.gdf"
)

AUX_SECTIONS = """
damping_lid:
  mesh_file: "lid.gdf"
  damping_factor: 0.05

free_surface_zone:
  type: mesh
  mesh_file: "zone.fdf"
"""


def _spec_with_aux(tmp_path: Path, missing: str | None = None) -> Path:
    """Ship fixture spec + lid/control-surface/free-surface meshes in tmp_path.

    All mesh files are created except *missing* (one of 'body', 'lid',
    'control_surface', 'zone').
    """
    text = (FIXTURES_DIR / "spec_ship_raos.yml").read_text()
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.gdf"',
        'mesh_file: "body.gdf"',
    )
    text = text.replace(
        "  inertia:",
        '  control_surface:\n    type: mesh\n    mesh_file: "cs.gdf"\n  inertia:',
        1,
    )
    text += AUX_SECTIONS
    spec_path = tmp_path / "spec.yml"
    spec_path.write_text(text)

    names = {"body": "body.gdf", "lid": "lid.gdf",
             "control_surface": "cs.gdf", "zone": "zone.fdf"}
    for key, name in names.items():
        if key != missing:
            shutil.copy2(MESH_FIXTURE, tmp_path / name)
    return spec_path


class TestMeshReferenceCollection:
    def test_all_four_mesh_classes_collected(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path))
        files = [r.mesh_file for r in iter_mesh_references(converter.spec)]
        assert files == ["body.gdf", "lid.gdf", "cs.gdf", "zone.fdf"]


class TestPackageIsSelfContained:
    def test_all_meshes_copied_alongside_input(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path))
        out = tmp_path / "out"
        result = converter.convert(solver="orcawave", output_dir=out)
        assert result.parent == out
        for name in ("body.gdf", "lid.gdf", "cs.gdf", "zone.fdf"):
            assert (out / name).is_file(), f"{name} not packaged"

    def test_generated_yml_references_resolve_within_package(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path))
        out = tmp_path / "out"
        result = converter.convert(solver="orcawave", output_dir=out)
        documents = [d for d in yaml.safe_load_all(result.read_text()) if d]
        body = documents[0]["Bodies"][0]
        mesh_ref = Path(body["BodyMeshFileName"])
        assert not mesh_ref.is_absolute()
        assert (out / mesh_ref).is_file()

    def test_modular_format_packages_meshes_too(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path))
        out = tmp_path / "out"
        result = converter.convert(
            solver="orcawave", format="modular", output_dir=out
        )
        package_dir = result if result.is_dir() else result.parent
        assert (package_dir / "body.gdf").is_file()

    def test_convert_all_orcawave_subdir_self_contained(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path))
        out = tmp_path / "out"
        converter.convert_all(output_dir=out)
        assert (out / "orcawave" / "body.gdf").is_file()
        # AQWA decks embed geometry; no mesh copy expected there
        assert not (out / "aqwa" / "body.gdf").exists()

    def test_existing_fixture_packages_its_mesh(self, tmp_path):
        converter = SpecConverter(FIXTURES_DIR / "spec_ship_raos.yml")
        out = tmp_path / "out"
        converter.convert(solver="orcawave", output_dir=out)
        assert (out / "sample_box.gdf").is_file()


class TestAuxiliaryMeshPreflight:
    @pytest.mark.parametrize(
        ("missing", "name"),
        [
            ("lid", "lid.gdf"),
            ("control_surface", "cs.gdf"),
            ("zone", "zone.fdf"),
        ],
    )
    def test_missing_auxiliary_mesh_hard_fails(self, tmp_path, missing, name):
        converter = SpecConverter(_spec_with_aux(tmp_path, missing=missing))
        with pytest.raises(FileNotFoundError, match=name):
            converter.convert(solver="orcawave", output_dir=tmp_path / "out")

    def test_missing_auxiliary_mesh_is_validate_issue(self, tmp_path):
        converter = SpecConverter(_spec_with_aux(tmp_path, missing="lid"))
        issues = converter.validate()
        assert any("Damping lid" in issue and "lid.gdf" in issue for issue in issues)
