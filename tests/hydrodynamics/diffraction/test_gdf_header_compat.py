"""OrcaWave-strict GDF header compatibility (#740).

The first licensed #610 run failed because the fixture mesh carried three
leading comment lines where the WAMIT GDF format allows exactly one header
line (line 2 must be ULEN GRAV). These tests pin the fixed fixtures, the
new blocking structural preflight, and the CLI exit-code contract.
"""

from __future__ import annotations

import shutil
from pathlib import Path
from unittest.mock import patch

import pytest
from click.testing import CliRunner

from digitalmodel.hydrodynamics.diffraction.cli import cli
from digitalmodel.hydrodynamics.diffraction.quality_gates import (
    run_mesh_quality_gate,
)

REPO_ROOT = Path(__file__).resolve().parents[3]
FIXTURE_MESHES = [
    REPO_ROOT
    / "tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf",
    REPO_ROOT
    / "examples/hydrodynamics/diffraction/unit_box_rao/unit_box.gdf",
]

BAD_HEADER = (
    "# comment line one\n"
    "# comment line two - OrcaWave reads this expecting ULEN GRAV\n"
    "1.0  9.81\n"
    "0  0\n"
    "1\n"
    "0 0 -1\n1 0 -1\n1 1 -1\n0 1 -1\n"
)


class TestFixtureMeshesAreOrcaWaveValid:
    @pytest.mark.parametrize(
        "mesh_path", FIXTURE_MESHES, ids=["acceptance_610", "unit_box_rao"]
    )
    def test_line2_is_ulen_grav(self, mesh_path):
        lines = mesh_path.read_text().splitlines()
        ulen, grav = lines[1].split()[:2]
        assert float(ulen) > 0 and float(grav) > 9
        assert not lines[1].startswith("#")

    @pytest.mark.parametrize(
        "mesh_path", FIXTURE_MESHES, ids=["acceptance_610", "unit_box_rao"]
    )
    def test_single_header_line_only(self, mesh_path):
        lines = mesh_path.read_text().splitlines()
        assert not any(line.startswith("#") for line in lines[1:])

    @pytest.mark.parametrize(
        "mesh_path", FIXTURE_MESHES, ids=["acceptance_610", "unit_box_rao"]
    )
    def test_fixture_passes_quality_gate_structure(self, mesh_path):
        gate = run_mesh_quality_gate(mesh_path, label="fixture")
        assert gate.status != "FAIL", gate.blocking

    def test_both_fixture_copies_identical(self):
        a, b = FIXTURE_MESHES
        assert a.read_bytes() == b.read_bytes()

    @pytest.mark.parametrize(
        "mesh_path", FIXTURE_MESHES, ids=["acceptance_610", "unit_box_rao"]
    )
    def test_panel_data_still_parses(self, mesh_path):
        from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import (
            MeshPipeline,
        )

        mesh = MeshPipeline().load(mesh_path)
        assert len(mesh.panels) == 5
        assert len(mesh.vertices) >= 8


class TestGdfStructuralPreflight:
    def test_multi_comment_header_blocks(self, tmp_path):
        bad = tmp_path / "bad.gdf"
        bad.write_text(BAD_HEADER)
        gate = run_mesh_quality_gate(bad, label="body")
        assert gate.status == "FAIL"
        assert any("ULEN GRAV" in issue for issue in gate.blocking)
        assert any("Unrecognised header" in issue for issue in gate.blocking)

    def test_truncated_file_blocks(self, tmp_path):
        bad = tmp_path / "tiny.gdf"
        bad.write_text("title\n1.0 9.81\n")
        gate = run_mesh_quality_gate(bad, label="body")
        assert gate.status == "FAIL"

    def test_bad_symmetry_line_blocks(self, tmp_path):
        bad = tmp_path / "sym.gdf"
        bad.write_text("title\n1.0 9.81\nnot ints\n5\n")
        gate = run_mesh_quality_gate(bad, label="body")
        assert gate.status == "FAIL"
        assert any("ISX ISY" in issue for issue in gate.blocking)

    def test_valid_header_proceeds_to_geometry_checks(self):
        gate = run_mesh_quality_gate(FIXTURE_MESHES[0], label="body")
        # geometry checks ran (report present), not a structural FAIL
        assert gate.report is not None


class TestRunCliExitCodes:
    def test_run_orcawave_failed_status_exits_nonzero(self, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
            OrcaWaveRunner,
            RunResult,
            RunStatus,
        )

        spec_src = (
            REPO_ROOT
            / "tests/hydrodynamics/diffraction/fixtures/acceptance_610"
        )
        spec_path = tmp_path / "spec.yml"
        shutil.copy2(spec_src / "spec.yml", spec_path)
        shutil.copy2(spec_src / "unit_box.gdf", tmp_path / "unit_box.gdf")

        failed = RunResult(
            status=RunStatus.FAILED,
            output_dir=tmp_path,
            error_message="Error code: 63",
        )
        with patch.object(OrcaWaveRunner, "run", return_value=failed):
            result = CliRunner().invoke(
                cli,
                ["run-orcawave", str(spec_path), "-o", str(tmp_path / "out")],
            )
        assert result.exit_code == 1, result.output

    def test_run_orcawave_dry_run_still_exits_zero(self, tmp_path):
        spec_src = (
            REPO_ROOT
            / "tests/hydrodynamics/diffraction/fixtures/acceptance_610"
        )
        spec_path = tmp_path / "spec.yml"
        shutil.copy2(spec_src / "spec.yml", spec_path)
        shutil.copy2(spec_src / "unit_box.gdf", tmp_path / "unit_box.gdf")
        result = CliRunner().invoke(
            cli,
            [
                "run-orcawave",
                str(spec_path),
                "--dry-run",
                "-o",
                str(tmp_path / "out"),
            ],
        )
        assert result.exit_code == 0, result.output
