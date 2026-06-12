"""Mesh pre-flight validation in SpecConverter (#500).

Missing or unresolvable mesh references must surface as validate() issues and
hard-fail convert()/convert_all() with FileNotFoundError before any solver
input is generated — never as cryptic solver-time errors.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

FIXTURES_DIR = Path(__file__).parent / "fixtures"
MESH_FIXTURE = (
    Path(__file__).parents[1] / "bemrosetta" / "fixtures" / "sample_box.gdf"
)


def _spec_in_tmp(tmp_path: Path, mesh_file: str, with_mesh: bool = False) -> Path:
    """Copy the ship fixture spec to tmp_path with a rewritten mesh reference."""
    text = (FIXTURES_DIR / "spec_ship_raos.yml").read_text()
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.gdf"',
        f'mesh_file: "{mesh_file}"',
    )
    spec_path = tmp_path / "spec.yml"
    spec_path.write_text(text)
    if with_mesh:
        shutil.copy2(MESH_FIXTURE, tmp_path / Path(mesh_file).name)
    return spec_path


class TestValidateReportsMissingMesh:
    def test_missing_mesh_is_an_issue(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        issues = converter.validate()
        assert any("no_such_mesh.gdf" in issue for issue in issues)

    def test_issue_names_resolved_path_and_convention(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        issue = next(i for i in converter.validate() if "no_such_mesh.gdf" in i)
        assert str(tmp_path) in issue  # resolved location
        assert "spec file directory" in issue  # convention statement

    def test_present_mesh_yields_no_issues(self, tmp_path):
        converter = SpecConverter(
            _spec_in_tmp(tmp_path, "sample_box.gdf", with_mesh=True)
        )
        assert converter.validate() == []

    def test_original_fixture_still_validates_clean(self):
        assert SpecConverter(FIXTURES_DIR / "spec_ship_raos.yml").validate() == []


class TestConvertHardFails:
    def test_convert_raises_file_not_found(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        with pytest.raises(FileNotFoundError, match="no_such_mesh.gdf"):
            converter.convert(solver="orcawave", output_dir=tmp_path / "out")

    def test_convert_fails_before_writing_output(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        out_dir = tmp_path / "out"
        with pytest.raises(FileNotFoundError):
            converter.convert(solver="orcawave", output_dir=out_dir)
        assert not out_dir.exists() or not any(out_dir.iterdir())

    def test_convert_all_raises_too(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        with pytest.raises(FileNotFoundError):
            converter.convert_all(output_dir=tmp_path / "out")

    def test_aqwa_path_also_guarded(self, tmp_path):
        converter = SpecConverter(_spec_in_tmp(tmp_path, "no_such_mesh.gdf"))
        with pytest.raises(FileNotFoundError):
            converter.convert(solver="aqwa", output_dir=tmp_path / "out")

    def test_convert_succeeds_with_present_mesh(self, tmp_path):
        converter = SpecConverter(
            _spec_in_tmp(tmp_path, "sample_box.gdf", with_mesh=True)
        )
        result = converter.convert(solver="orcawave", output_dir=tmp_path / "out")
        assert result.is_file()


class TestPathResolution:
    def test_absolute_mesh_path_is_used_as_is(self, tmp_path):
        mesh_abs = tmp_path / "abs_box.gdf"
        shutil.copy2(MESH_FIXTURE, mesh_abs)
        converter = SpecConverter(
            _spec_in_tmp(tmp_path / "specs_elsewhere", str(mesh_abs))
        )
        assert converter.validate() == []

    def test_relative_path_resolves_from_spec_dir_not_cwd(self, tmp_path, monkeypatch):
        spec_path = _spec_in_tmp(tmp_path, "sample_box.gdf", with_mesh=True)
        monkeypatch.chdir(tmp_path.parent)  # cwd != spec dir
        assert SpecConverter(spec_path).validate() == []


@pytest.fixture(autouse=True)
def _mkdir_for_nested_spec(tmp_path):
    (tmp_path / "specs_elsewhere").mkdir(exist_ok=True)
