"""Tests for batch_processor module.

ABOUTME: Tests for BatchConfiguration, BatchResult, BatchReport dataclasses,
BatchProcessor class, and process_batch_from_config_file function.
"""
from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import MagicMock, patch, PropertyMock

import pytest

from digitalmodel.hydrodynamics.diffraction.batch_processor import (
    BatchConfiguration,
    BatchProcessor,
    BatchReport,
    BatchResult,
    process_batch_from_config_file,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_config(
    tmp_path: Path,
    vessel_name: str = "TestVessel",
    source_type: str = "aqwa",
    water_depth: float = 100.0,
    export_formats: list | None = None,
    validate: bool = True,
) -> BatchConfiguration:
    """Create a BatchConfiguration pointing to tmp_path."""
    return BatchConfiguration(
        vessel_name=vessel_name,
        source_type=source_type,
        source_path=tmp_path / "source",
        water_depth=water_depth,
        output_dir=tmp_path / "output",
        export_formats=export_formats or ["all"],
        validate=validate,
    )


# ---------------------------------------------------------------------------
# Dataclass tests
# ---------------------------------------------------------------------------


class TestBatchConfiguration:
    """Tests for BatchConfiguration dataclass."""

    def test_creation_basic(self, tmp_path: Path):
        cfg = _make_config(tmp_path)
        assert cfg.vessel_name == "TestVessel"
        assert cfg.source_type == "aqwa"
        assert cfg.water_depth == 100.0
        assert cfg.validate is True

    def test_default_export_formats(self, tmp_path: Path):
        cfg = BatchConfiguration(
            vessel_name="V",
            source_type="aqwa",
            source_path=tmp_path,
            water_depth=50.0,
            output_dir=tmp_path,
        )
        assert cfg.export_formats == ["all"]

    def test_custom_export_formats(self, tmp_path: Path):
        cfg = _make_config(tmp_path, export_formats=["rao_csv", "excel"])
        assert cfg.export_formats == ["rao_csv", "excel"]

    def test_validate_default_true(self, tmp_path: Path):
        cfg = BatchConfiguration(
            vessel_name="V",
            source_type="orcawave",
            source_path=tmp_path,
            water_depth=50.0,
            output_dir=tmp_path,
        )
        assert cfg.validate is True


class TestBatchResult:
    """Tests for BatchResult dataclass."""

    def test_creation_defaults(self):
        r = BatchResult(
            vessel_name="V",
            source_type="aqwa",
            status="success",
            execution_time=1.5,
        )
        assert r.output_files == {}
        assert r.validation_status is None
        assert r.error_message is None

    def test_with_error(self):
        r = BatchResult(
            vessel_name="V",
            source_type="aqwa",
            status="error",
            execution_time=0.1,
            error_message="boom",
        )
        assert r.status == "error"
        assert r.error_message == "boom"


class TestBatchReport:
    """Tests for BatchReport dataclass."""

    def test_creation_defaults(self):
        rpt = BatchReport(
            start_time="2024-01-01 00:00:00",
            end_time="2024-01-01 00:01:00",
            total_duration=60.0,
            total_configurations=2,
            successful=1,
            failed=1,
            warnings=0,
        )
        assert rpt.results == []
        assert rpt.summary_statistics == {}
        assert rpt.total_configurations == 2


# ---------------------------------------------------------------------------
# BatchProcessor tests
# ---------------------------------------------------------------------------


class TestBatchProcessorInit:
    """Tests for BatchProcessor.__init__."""

    def test_single_config_max_workers(self, tmp_path: Path):
        cfgs = [_make_config(tmp_path)]
        bp = BatchProcessor(cfgs)
        assert bp.max_workers == 1  # min(4, 1)
        assert bp.report.total_configurations == 1

    def test_many_configs_max_workers_capped(self, tmp_path: Path):
        cfgs = [_make_config(tmp_path, vessel_name=f"V{i}") for i in range(10)]
        bp = BatchProcessor(cfgs)
        assert bp.max_workers == 4  # min(4, 10)

    def test_custom_max_workers(self, tmp_path: Path):
        cfgs = [_make_config(tmp_path)]
        bp = BatchProcessor(cfgs, max_workers=8)
        assert bp.max_workers == 8

    def test_report_initialised_to_zeros(self, tmp_path: Path):
        cfgs = [_make_config(tmp_path)]
        bp = BatchProcessor(cfgs)
        assert bp.report.successful == 0
        assert bp.report.failed == 0
        assert bp.report.warnings == 0


class TestProcessConfiguration:
    """Tests for BatchProcessor.process_configuration."""

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results"
    )
    def test_aqwa_success(self, mock_validate, mock_exporter_cls, tmp_path, mock_diffraction_results):
        """Successful AQWA conversion returns status='success'."""
        mock_validate.return_value = {"overall_status": "PASS"}

        mock_exporter = MagicMock()
        mock_exporter.export_all.return_value = {"vessel_type": tmp_path / "vt.yml"}
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(tmp_path, source_type="aqwa")
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        assert result.status == "success"
        assert result.vessel_name == "TestVessel"
        assert result.execution_time >= 0
        assert result.validation_status == "PASS"
        assert "vessel_type" in result.output_files

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results"
    )
    def test_orcawave_success(self, mock_validate, mock_exporter_cls, tmp_path, mock_diffraction_results):
        """Successful OrcaWave conversion returns status='success'."""
        mock_validate.return_value = {"overall_status": "PASS"}
        mock_exporter = MagicMock()
        mock_exporter.export_all.return_value = {}
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(tmp_path, source_type="orcawave")
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_orcawave", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        assert result.status == "success"

    def test_unknown_source_type_gives_error(self, tmp_path):
        """Unknown source_type should be caught and yield status='error'."""
        cfg = _make_config(tmp_path, source_type="unknown_solver")
        bp = BatchProcessor([cfg])
        result = bp.process_configuration(cfg)

        assert result.status == "error"
        assert "Unknown source type" in result.error_message

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results"
    )
    def test_validation_warning_sets_status_warning(
        self, mock_validate, mock_exporter_cls, tmp_path, mock_diffraction_results
    ):
        """If validation returns WARNING, result status should be 'warning'."""
        mock_validate.return_value = {"overall_status": "WARNING"}
        mock_exporter = MagicMock()
        mock_exporter.export_all.return_value = {}
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(tmp_path, source_type="aqwa")
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        assert result.status == "warning"
        assert result.validation_status == "WARNING"

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results"
    )
    def test_validation_fail_sets_status_warning(
        self, mock_validate, mock_exporter_cls, tmp_path, mock_diffraction_results
    ):
        """If validation returns FAIL, result status should be 'warning'."""
        mock_validate.return_value = {"overall_status": "FAIL"}
        mock_exporter = MagicMock()
        mock_exporter.export_all.return_value = {}
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(tmp_path, source_type="aqwa")
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        assert result.status == "warning"
        assert result.validation_status == "FAIL"

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    def test_skip_validation_when_flag_false(
        self, mock_exporter_cls, tmp_path, mock_diffraction_results
    ):
        """When validate=False, validation_status stays None."""
        mock_exporter = MagicMock()
        mock_exporter.export_all.return_value = {}
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(tmp_path, source_type="aqwa", validate=False)
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        assert result.status == "success"
        assert result.validation_status is None

    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter"
    )
    @patch(
        "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results"
    )
    def test_specific_export_formats(
        self, mock_validate, mock_exporter_cls, tmp_path, mock_diffraction_results
    ):
        """When export_formats lists specific formats, only those are called."""
        mock_validate.return_value = {"overall_status": "PASS"}
        mock_exporter = MagicMock()
        mock_exporter.export_vessel_type.return_value = tmp_path / "vt.yml"
        mock_exporter.export_raos_csv.return_value = tmp_path / "raos.csv"
        mock_exporter_cls.return_value = mock_exporter

        cfg = _make_config(
            tmp_path, source_type="aqwa", export_formats=["vessel_type", "rao_csv"]
        )
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", return_value=mock_diffraction_results):
            result = bp.process_configuration(cfg)

        mock_exporter.export_vessel_type.assert_called_once()
        mock_exporter.export_raos_csv.assert_called_once()
        mock_exporter.export_all.assert_not_called()
        assert result.status == "success"

    def test_exception_in_processing_gives_error(self, tmp_path):
        """If _process_aqwa raises, result status is 'error' with message."""
        cfg = _make_config(tmp_path, source_type="aqwa")
        bp = BatchProcessor([cfg])

        with patch.object(bp, "_process_aqwa", side_effect=RuntimeError("disk full")):
            result = bp.process_configuration(cfg)

        assert result.status == "error"
        assert "disk full" in result.error_message


class TestBatchProcessorRun:
    """Tests for BatchProcessor.run (sequential mode)."""

    def test_run_sequential_counts(self, tmp_path, mock_diffraction_results):
        """Run sequential with 2 configs: 1 success + 1 error."""
        cfg_ok = _make_config(tmp_path, vessel_name="GoodVessel", source_type="aqwa")
        cfg_bad = _make_config(tmp_path, vessel_name="BadVessel", source_type="bad_type")
        bp = BatchProcessor([cfg_ok, cfg_bad])

        with patch.object(
            bp,
            "_process_aqwa",
            return_value=mock_diffraction_results,
        ), patch(
            "digitalmodel.hydrodynamics.diffraction.batch_processor.validate_results",
            return_value={"overall_status": "PASS"},
        ), patch(
            "digitalmodel.hydrodynamics.diffraction.batch_processor.OrcaFlexExporter",
        ) as mock_exp_cls:
            mock_exp_cls.return_value.export_all.return_value = {}
            report = bp.run(parallel=False)

        assert report.total_configurations == 2
        assert report.successful == 1
        assert report.failed == 1
        assert len(report.results) == 2
        assert report.total_duration >= 0
        assert report.start_time != ""
        assert report.end_time != ""


class TestCalculateStatistics:
    """Tests for BatchProcessor._calculate_statistics."""

    def test_empty_results(self, tmp_path: Path):
        bp = BatchProcessor([_make_config(tmp_path)])
        bp._calculate_statistics()
        assert bp.report.summary_statistics == {}

    def test_statistics_computed(self, tmp_path: Path):
        bp = BatchProcessor([_make_config(tmp_path)])
        bp.report.results = [
            BatchResult("V1", "aqwa", "success", 2.0, output_files={"a": Path("a")}),
            BatchResult("V2", "aqwa", "success", 4.0, output_files={"b": Path("b"), "c": Path("c")}),
        ]
        bp.report.successful = 2
        bp.report.total_configurations = 2
        bp._calculate_statistics()

        stats = bp.report.summary_statistics
        assert stats["avg_execution_time"] == pytest.approx(3.0)
        assert stats["min_execution_time"] == pytest.approx(2.0)
        assert stats["max_execution_time"] == pytest.approx(4.0)
        assert stats["total_output_files"] == 3
        assert stats["success_rate"] == pytest.approx(100.0)

    def test_success_rate_partial(self, tmp_path: Path):
        bp = BatchProcessor([_make_config(tmp_path)])
        bp.report.results = [
            BatchResult("V1", "aqwa", "success", 1.0),
            BatchResult("V2", "aqwa", "error", 0.5, error_message="fail"),
        ]
        bp.report.successful = 1
        bp.report.total_configurations = 2
        bp._calculate_statistics()
        assert bp.report.summary_statistics["success_rate"] == pytest.approx(50.0)


class TestExportReport:
    """Tests for BatchProcessor.export_report."""

    def test_export_creates_json(self, tmp_path: Path):
        bp = BatchProcessor([_make_config(tmp_path)])
        bp.report.start_time = "2024-01-01 00:00:00"
        bp.report.end_time = "2024-01-01 00:01:00"
        bp.report.total_duration = 60.0
        bp.report.successful = 1
        bp.report.results = [
            BatchResult(
                vessel_name="V1",
                source_type="aqwa",
                status="success",
                execution_time=5.0,
                output_files={"vt": Path("/tmp/vt.yml")},
                validation_status="PASS",
            ),
        ]
        bp.report.summary_statistics = {"success_rate": 100.0}

        out_file = tmp_path / "report.json"
        bp.export_report(out_file)

        assert out_file.exists()
        data = json.loads(out_file.read_text())
        assert data["successful"] == 1
        assert len(data["results"]) == 1
        assert data["results"][0]["vessel_name"] == "V1"
        assert data["results"][0]["output_files"]["vt"] == "/tmp/vt.yml"

    def test_export_empty_report(self, tmp_path: Path):
        bp = BatchProcessor([_make_config(tmp_path)])
        bp.report.start_time = ""
        bp.report.end_time = ""
        out_file = tmp_path / "empty_report.json"
        bp.export_report(out_file)

        data = json.loads(out_file.read_text())
        assert data["results"] == []


# ---------------------------------------------------------------------------
# process_batch_from_config_file tests
# ---------------------------------------------------------------------------


class TestProcessBatchFromConfigFile:
    """Tests for the convenience function."""

    def test_reads_config_and_runs(self, tmp_path: Path):
        """Writes a JSON config, then mocks the processor run."""
        config_data = {
            "configurations": [
                {
                    "vessel_name": "Ship1",
                    "source_type": "aqwa",
                    "source_path": str(tmp_path / "aqwa_data"),
                    "water_depth": 200.0,
                    "output_dir": str(tmp_path / "out"),
                    "export_formats": ["summary"],
                    "validate": False,
                },
            ],
            "max_workers": 2,
            "parallel": False,
        }
        cfg_file = tmp_path / "batch.json"
        cfg_file.write_text(json.dumps(config_data))

        mock_report = BatchReport(
            start_time="t0", end_time="t1", total_duration=1.0,
            total_configurations=1, successful=1, failed=0, warnings=0,
        )

        with patch(
            "digitalmodel.hydrodynamics.diffraction.batch_processor.BatchProcessor"
        ) as MockBP:
            instance = MockBP.return_value
            instance.run.return_value = mock_report
            report = process_batch_from_config_file(cfg_file)

        MockBP.assert_called_once()
        args, kwargs = MockBP.call_args
        # configurations list passed
        cfgs = kwargs.get("configurations") or args[0]
        assert len(cfgs) == 1
        assert cfgs[0].vessel_name == "Ship1"
        assert cfgs[0].water_depth == 200.0
        assert cfgs[0].validate is False
        assert report.successful == 1

    def test_exports_report_when_specified(self, tmp_path: Path):
        """When 'report_output' key present, export_report is called."""
        report_path = tmp_path / "rpt.json"
        config_data = {
            "configurations": [
                {
                    "vessel_name": "V1",
                    "source_type": "aqwa",
                    "source_path": "/dummy",
                    "water_depth": 50.0,
                    "output_dir": "/dummy_out",
                },
            ],
            "report_output": str(report_path),
        }
        cfg_file = tmp_path / "batch.json"
        cfg_file.write_text(json.dumps(config_data))

        mock_report = BatchReport(
            start_time="t0", end_time="t1", total_duration=0.5,
            total_configurations=1, successful=1, failed=0, warnings=0,
        )

        with patch(
            "digitalmodel.hydrodynamics.diffraction.batch_processor.BatchProcessor"
        ) as MockBP:
            instance = MockBP.return_value
            instance.run.return_value = mock_report
            process_batch_from_config_file(cfg_file)

        instance.export_report.assert_called_once_with(report_path)

    def test_default_values_for_optional_fields(self, tmp_path: Path):
        """export_formats and validate get defaults when not in JSON."""
        config_data = {
            "configurations": [
                {
                    "vessel_name": "V1",
                    "source_type": "orcawave",
                    "source_path": "/data",
                    "water_depth": 80.0,
                    "output_dir": "/out",
                },
            ],
        }
        cfg_file = tmp_path / "batch.json"
        cfg_file.write_text(json.dumps(config_data))

        mock_report = BatchReport(
            start_time="", end_time="", total_duration=0,
            total_configurations=1, successful=0, failed=0, warnings=0,
        )

        with patch(
            "digitalmodel.hydrodynamics.diffraction.batch_processor.BatchProcessor"
        ) as MockBP:
            instance = MockBP.return_value
            instance.run.return_value = mock_report
            process_batch_from_config_file(cfg_file)

        args, kwargs = MockBP.call_args
        cfgs = kwargs.get("configurations") or args[0]
        assert cfgs[0].export_formats == ["all"]
        assert cfgs[0].validate is True
