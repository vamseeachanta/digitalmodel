"""Pre-solve mesh quality gates (#608).

FAIL verdicts block solve/package generation; WARNING verdicts are surfaced
but never block — calibrated so legitimate waterline-open hull meshes (which
fail the watertightness check by design) pass through with warnings.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.mesh_packaging import (
    package_spec_meshes,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunStatus,
)
from digitalmodel.hydrodynamics.diffraction.quality_gates import (
    QUALITY_REPORT_FILENAME,
    MeshQualityError,
    run_mesh_quality_gate,
)
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

FIXTURES_DIR = Path(__file__).parent / "fixtures"
SAMPLE_GDF = Path(__file__).parents[1] / "bemrosetta" / "fixtures" / "sample_box.gdf"

# Two disjoint panels: a sliver (aspect ratio ~10000) and a large quad —
# non-watertight, too few panels, terrible aspect ratio, wild element-size
# variance. Fails enough checks to score FAIL.
BAD_GDF = """\
deliberately bad mesh for quality gate tests
1.0  9.81
0  0
2
0.000000  0.000000  -1.000000
10.000000  0.000000  -1.000000
10.000000  0.001000  -1.000000
0.000000  0.001000  -1.000000
0.000000  0.000000  -2.000000
5.000000  0.000000  -2.000000
5.000000  5.000000  -2.000000
0.000000  5.000000  -2.000000
"""


def _spec_in_tmp(tmp_path: Path, mesh_name: str = "body.gdf") -> Path:
    text = (FIXTURES_DIR / "spec_ship_raos.yml").read_text()
    text = text.replace(
        'mesh_file: "../../bemrosetta/fixtures/sample_box.gdf"',
        f'mesh_file: "{mesh_name}"',
    )
    spec_path = tmp_path / "spec.yml"
    spec_path.write_text(text)
    return spec_path


@pytest.fixture
def good_mesh_spec(tmp_path):
    spec_path = _spec_in_tmp(tmp_path)
    shutil.copy2(SAMPLE_GDF, tmp_path / "body.gdf")
    return spec_path


@pytest.fixture
def bad_mesh_spec(tmp_path):
    spec_path = _spec_in_tmp(tmp_path)
    (tmp_path / "body.gdf").write_text(BAD_GDF)
    return spec_path


class TestGateVerdicts:
    def test_legit_open_waterline_mesh_warns_not_blocks(self):
        gate = run_mesh_quality_gate(SAMPLE_GDF, label="body")
        assert gate.status == "WARNING"
        assert gate.blocking == []
        assert gate.warnings  # watertightness/panel-count surfaced

    def test_bad_mesh_fails(self, tmp_path):
        bad = tmp_path / "bad.gdf"
        bad.write_text(BAD_GDF)
        gate = run_mesh_quality_gate(bad, label="body")
        assert gate.status == "FAIL"
        assert gate.blocking

    def test_fdf_skipped(self, tmp_path):
        zone = tmp_path / "zone.fdf"
        zone.write_text("not a panel mesh\n")
        gate = run_mesh_quality_gate(zone, label="free surface zone")
        assert gate.status == "SKIPPED"

    def test_aspect_ratio_no_crash_on_quads(self):
        """Regression: checker crashed on quad panels (nodes[panel[:3]])."""
        gate = run_mesh_quality_gate(SAMPLE_GDF)
        assert gate.report is not None
        assert gate.report.total_checks == gate.report.passed_checks + (
            gate.report.total_checks - gate.report.passed_checks
        )


class TestConvertGating:
    def test_good_mesh_converts_with_warnings(self, good_mesh_spec, tmp_path):
        converter = SpecConverter(good_mesh_spec)
        out = tmp_path / "out"
        result = converter.convert(solver="orcawave", output_dir=out)
        assert result.is_file()
        assert converter.quality_warnings
        assert (out / QUALITY_REPORT_FILENAME).is_file()

    def test_bad_mesh_blocks_conversion(self, bad_mesh_spec, tmp_path):
        converter = SpecConverter(bad_mesh_spec)
        with pytest.raises(MeshQualityError, match="quality gates failed"):
            converter.convert(solver="orcawave", output_dir=tmp_path / "out")

    def test_machine_readable_report_written_on_fail(self, bad_mesh_spec, tmp_path):
        out = tmp_path / "out"
        with pytest.raises(MeshQualityError):
            SpecConverter(bad_mesh_spec).convert(solver="orcawave", output_dir=out)
        records = json.loads((out / QUALITY_REPORT_FILENAME).read_text())
        assert records[0]["status"] == "FAIL"
        assert records[0]["blocking"]
        assert "report" in records[0]

    def test_no_quality_gates_escape_hatch(self, bad_mesh_spec, tmp_path):
        converter = SpecConverter(bad_mesh_spec)
        result = converter.convert(
            solver="orcawave",
            output_dir=tmp_path / "out",
            quality_gates=False,
        )
        assert result.is_file()
        assert converter.quality_warnings == []


class TestRunnerGating:
    def test_bad_mesh_blocks_run(self, bad_mesh_spec, tmp_path):
        runner = OrcaWaveRunner(
            RunConfig(output_dir=tmp_path / "out", dry_run=True)
        )
        spec = DiffractionSpec.from_yaml(bad_mesh_spec)
        with pytest.raises(MeshQualityError):
            runner.run(spec, spec_path=bad_mesh_spec)

    def test_quality_gates_off_allows_run(self, bad_mesh_spec, tmp_path):
        runner = OrcaWaveRunner(
            RunConfig(
                output_dir=tmp_path / "out", dry_run=True, quality_gates=False
            )
        )
        spec = DiffractionSpec.from_yaml(bad_mesh_spec)
        result = runner.run(spec, spec_path=bad_mesh_spec)
        assert result.status == RunStatus.DRY_RUN

    def test_warnings_surfaced_on_result(self, good_mesh_spec, tmp_path):
        runner = OrcaWaveRunner(
            RunConfig(output_dir=tmp_path / "out", dry_run=True)
        )
        spec = DiffractionSpec.from_yaml(good_mesh_spec)
        result = runner.run(spec, spec_path=good_mesh_spec)
        assert result.status == RunStatus.DRY_RUN
        assert result.quality_warnings


class TestValidateSpecQuality:
    def test_check_mesh_quality_reports_per_mesh(self, good_mesh_spec):
        results = SpecConverter(good_mesh_spec).check_mesh_quality()
        assert len(results) == 1
        assert results[0].status == "WARNING"

    def test_package_spec_meshes_returns_warnings(self, good_mesh_spec, tmp_path):
        spec = DiffractionSpec.from_yaml(good_mesh_spec)
        _, _, warnings = package_spec_meshes(
            spec,
            tmp_path / "out",
            spec_dir=good_mesh_spec.parent,
        )
        assert warnings
