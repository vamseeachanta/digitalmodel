"""MeshPipeline integration into spec conversion and runner (#606).

Non-GDF meshes must be converted to OrcaWave's GDF target during packaging,
with the generated YAML referencing the converted filename; already-ready GDF
meshes copy unchanged (no conversion churn); unsupported formats fail with
the supported-format list before anything is written.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.mesh_packaging import (
    PROVENANCE_FILENAME,
    package_spec_meshes,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunStatus,
)
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

FIXTURES_DIR = Path(__file__).parent / "fixtures"
BEMROSETTA_FIXTURES = Path(__file__).parents[1] / "bemrosetta" / "fixtures"


def _spec_with_mesh(tmp_path: Path, mesh_name: str, source: str | None) -> Path:
    """Ship fixture spec in tmp_path referencing *mesh_name*.

    *source* names a bemrosetta fixture to copy in as the mesh (None = don't
    create the file).
    """
    text = (FIXTURES_DIR / "spec_ship_raos.yml").read_text()
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.gdf"',
        f'mesh_file: "{mesh_name}"',
    )
    fmt = Path(mesh_name).suffix.lstrip(".")
    if fmt in ("gdf", "dat", "stl"):
        text = text.replace("mesh_format: gdf", f"mesh_format: {fmt}")
    spec_path = tmp_path / "spec.yml"
    spec_path.write_text(text)
    if source is not None:
        shutil.copy2(BEMROSETTA_FIXTURES / source, tmp_path / mesh_name)
    return spec_path


def _body_mesh_name(input_file: Path) -> str:
    documents = [d for d in yaml.safe_load_all(input_file.read_text()) if d]
    return documents[0]["Bodies"][0]["BodyMeshFileName"]


class TestGdfPassthrough:
    def test_ready_gdf_copied_byte_identical(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.gdf", "sample_box.gdf")
        out = tmp_path / "out"
        SpecConverter(spec_path).convert(solver="orcawave", output_dir=out)
        assert (out / "body.gdf").read_bytes() == (tmp_path / "body.gdf").read_bytes()

    def test_no_provenance_file_without_conversion(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.gdf", "sample_box.gdf")
        out = tmp_path / "out"
        SpecConverter(spec_path).convert(solver="orcawave", output_dir=out)
        assert not (out / PROVENANCE_FILENAME).exists()


class TestDatConversion:
    @pytest.fixture()
    def converted(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.dat", "sample_box.dat")
        out = tmp_path / "out"
        result = SpecConverter(spec_path).convert(
            solver="orcawave", output_dir=out
        )
        return result, out

    def test_dat_mesh_converted_to_gdf(self, converted):
        _, out = converted
        assert (out / "body.gdf").is_file()

    def test_generated_yml_references_converted_name(self, converted):
        result, out = converted
        mesh_ref = _body_mesh_name(result)
        assert mesh_ref == "body.gdf"
        assert (out / mesh_ref).is_file()

    def test_provenance_recorded(self, converted):
        _, out = converted
        records = json.loads((out / PROVENANCE_FILENAME).read_text())
        assert records[0]["source_format"] == "dat"
        assert records[0]["packaged"] == "body.gdf"
        assert "length_units" in records[0]
        assert "symmetry" in records[0]

    def test_source_spec_not_mutated(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.dat", "sample_box.dat")
        converter = SpecConverter(spec_path)
        converter.convert(solver="orcawave", output_dir=tmp_path / "out")
        assert converter.spec.vessel.geometry.mesh_file == "body.dat"


class TestUnsupportedFormat:
    def test_fails_with_supported_format_list(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.obj", None)
        (tmp_path / "body.obj").write_text("o box\n")
        out = tmp_path / "out"
        with pytest.raises(ValueError, match=r"Unsupported mesh format"):
            SpecConverter(spec_path).convert(solver="orcawave", output_dir=out)

    def test_fails_before_writing_anything(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.obj", None)
        (tmp_path / "body.obj").write_text("o box\n")
        out = tmp_path / "out"
        with pytest.raises(ValueError):
            SpecConverter(spec_path).convert(solver="orcawave", output_dir=out)
        assert not out.exists() or not any(out.iterdir())

    def test_message_lists_supported_formats(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.obj", None)
        (tmp_path / "body.obj").write_text("o box\n")
        with pytest.raises(ValueError, match=r"\.gdf") as exc_info:
            package_spec_meshes(
                DiffractionSpec.from_yaml(spec_path),
                tmp_path / "out",
                spec_dir=tmp_path,
            )
        assert ".stl" in str(exc_info.value)


class TestRunnerIntegration:
    def test_dry_run_converts_dat_and_references_gdf(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.dat", "sample_box.dat")
        out = tmp_path / "out"
        runner = OrcaWaveRunner(RunConfig(output_dir=out, dry_run=True))
        result = runner.run(
            DiffractionSpec.from_yaml(spec_path), spec_path=spec_path
        )
        assert result.status == RunStatus.DRY_RUN
        assert any(p.name == "body.gdf" for p in result.mesh_files)
        assert _body_mesh_name(result.input_file) == "body.gdf"
        assert result.error_message is None

    def test_dry_run_gdf_passthrough_unchanged(self, tmp_path):
        spec_path = _spec_with_mesh(tmp_path, "body.gdf", "sample_box.gdf")
        out = tmp_path / "out"
        runner = OrcaWaveRunner(RunConfig(output_dir=out, dry_run=True))
        result = runner.run(
            DiffractionSpec.from_yaml(spec_path), spec_path=spec_path
        )
        assert result.status == RunStatus.DRY_RUN
        assert _body_mesh_name(result.input_file) == "body.gdf"
        assert not (out / PROVENANCE_FILENAME).exists()
