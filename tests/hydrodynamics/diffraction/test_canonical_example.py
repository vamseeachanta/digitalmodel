"""Regression guard for the canonical runnable diffraction example (#712).

Dry-runs ``examples/hydrodynamics/diffraction/unit_box_rao/`` through
``OrcaWaveRunner`` on unlicensed hosts so the shipped example cannot rot.
The licensed end-to-end path for the same chain is tracked in #610.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunStatus,
)

EXAMPLE_DIR = (
    Path(__file__).resolve().parents[3]
    / "examples"
    / "hydrodynamics"
    / "diffraction"
    / "unit_box_rao"
)


@pytest.fixture(scope="module")
def example_spec_path() -> Path:
    return EXAMPLE_DIR / "spec.yml"


class TestCanonicalExampleShape:
    """The example directory must stay complete and self-contained."""

    def test_example_files_present(self):
        assert (EXAMPLE_DIR / "spec.yml").is_file()
        assert (EXAMPLE_DIR / "unit_box.gdf").is_file()
        assert (EXAMPLE_DIR / "README.md").is_file()

    def test_spec_parses_as_diffraction_spec(self, example_spec_path):
        spec = DiffractionSpec.from_yaml(example_spec_path)
        assert spec.analysis_type == "diffraction"
        assert spec.vessel.geometry.mesh_file == "unit_box.gdf"

    def test_mesh_reference_is_relative(self, example_spec_path):
        spec = DiffractionSpec.from_yaml(example_spec_path)
        assert not Path(spec.vessel.geometry.mesh_file).is_absolute()


class TestCanonicalExampleDryRun:
    """Dry-run must produce a self-contained, portable solver package."""

    @pytest.fixture(scope="class")
    def dry_run_result(self, tmp_path_factory):
        output_dir = tmp_path_factory.mktemp("unit_box_rao_out")
        spec_path = EXAMPLE_DIR / "spec.yml"
        runner = OrcaWaveRunner(RunConfig(output_dir=output_dir, dry_run=True))
        spec = DiffractionSpec.from_yaml(spec_path)
        return runner.run(spec, spec_path=spec_path), output_dir

    def test_dry_run_completes(self, dry_run_result):
        result, _ = dry_run_result
        assert result.status == RunStatus.DRY_RUN
        assert result.error_message is None

    def test_solver_input_generated(self, dry_run_result):
        result, output_dir = dry_run_result
        assert result.input_file is not None
        assert result.input_file.is_file()
        assert result.input_file.parent == output_dir

    def test_mesh_copied_alongside(self, dry_run_result):
        result, output_dir = dry_run_result
        assert (output_dir / "unit_box.gdf").is_file()
        assert any(p.name == "unit_box.gdf" for p in result.mesh_files)

    def test_mesh_reference_stays_relative_in_package(self, dry_run_result):
        result, _ = dry_run_result
        text = result.input_file.read_text()
        documents = [d for d in yaml.safe_load_all(text) if d]
        bodies = documents[0]["Bodies"]
        assert bodies[0]["BodyMeshFileName"] == "unit_box.gdf"

    def test_modular_sections_generated(self, dry_run_result):
        _, output_dir = dry_run_result
        modular = output_dir / "modular"
        assert (modular / "master.yml").is_file()
        assert len(list(modular.glob("*.yml"))) >= 8
