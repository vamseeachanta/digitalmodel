"""Tests for AQWABatchRunner (WRK-028)."""
from __future__ import annotations

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.aqwa_runner import AQWARunConfig
from digitalmodel.hydrodynamics.diffraction.aqwa_batch_runner import (
    AQWABatchConfig,
    AQWABatchJobConfig,
    AQWABatchJobResult,
    AQWABatchReport,
    AQWABatchRunner,
    AQWAExecutionMode,
    run_aqwa_batch,
    run_aqwa_batch_from_specs,
)


# ---------------------------------------------------------------------------
# Enum
# ---------------------------------------------------------------------------


class TestAQWAExecutionMode:
    """Test AQWAExecutionMode enum values."""

    def test_values(self) -> None:
        assert AQWAExecutionMode.SEQUENTIAL == "sequential"
        assert AQWAExecutionMode.PARALLEL == "parallel"


# ---------------------------------------------------------------------------
# Config validation
# ---------------------------------------------------------------------------


class TestAQWABatchJobConfig:
    """Test AQWABatchJobConfig Pydantic model."""

    def test_defaults(self, ship_raos_spec_path: Path) -> None:
        cfg = AQWABatchJobConfig(spec_path=ship_raos_spec_path)
        assert cfg.dry_run is False
        assert cfg.validate is True
        assert cfg.generate_plots is True
        assert cfg.export_formats == ["csv"]

    def test_custom_values(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = AQWABatchJobConfig(
            spec_path=ship_raos_spec_path,
            job_name="aqwa_test_job",
            output_dir=tmp_path,
            dry_run=True,
            water_depth_override=150.0,
        )
        assert cfg.job_name == "aqwa_test_job"
        assert cfg.output_dir == tmp_path
        assert cfg.dry_run is True
        assert cfg.water_depth_override == 150.0

    def test_model_config_allows_arbitrary_types(
        self, ship_raos_spec_path: Path
    ) -> None:
        cfg = AQWABatchJobConfig(spec_path=ship_raos_spec_path)
        assert cfg.model_config.get("arbitrary_types_allowed") is True


class TestAQWABatchConfig:
    """Test AQWABatchConfig Pydantic model."""

    def test_defaults(self, ship_raos_spec_path: Path) -> None:
        cfg = AQWABatchConfig(
            jobs=[AQWABatchJobConfig(spec_path=ship_raos_spec_path)]
        )
        assert cfg.execution_mode == AQWAExecutionMode.SEQUENTIAL
        assert cfg.max_workers == 4

    def test_from_yaml(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        config_data = {
            "jobs": [
                {
                    "spec_path": str(ship_raos_spec_path),
                    "job_name": "yaml_job",
                    "dry_run": True,
                },
            ],
            "execution_mode": "sequential",
            "base_output_dir": str(tmp_path / "aqwa_batch"),
        }
        config_file = tmp_path / "aqwa_batch_config.yml"
        config_file.write_text(yaml.dump(config_data))

        cfg = AQWABatchConfig.from_yaml(config_file)
        assert len(cfg.jobs) == 1
        assert cfg.jobs[0].job_name == "yaml_job"
        assert cfg.jobs[0].dry_run is True
        assert cfg.execution_mode == AQWAExecutionMode.SEQUENTIAL

    def test_custom_values(self, ship_raos_spec_path: Path) -> None:
        cfg = AQWABatchConfig(
            jobs=[AQWABatchJobConfig(spec_path=ship_raos_spec_path)],
            execution_mode=AQWAExecutionMode.PARALLEL,
            max_workers=8,
            generate_summary_report=False,
        )
        assert cfg.execution_mode == AQWAExecutionMode.PARALLEL
        assert cfg.max_workers == 8
        assert cfg.generate_summary_report is False

    def test_run_config_default_is_aqwa_run_config(
        self, ship_raos_spec_path: Path
    ) -> None:
        cfg = AQWABatchConfig(
            jobs=[AQWABatchJobConfig(spec_path=ship_raos_spec_path)]
        )
        assert isinstance(cfg.run_config, AQWARunConfig)


# ---------------------------------------------------------------------------
# Result dataclasses
# ---------------------------------------------------------------------------


class TestAQWABatchJobResult:
    """Test AQWABatchJobResult dataclass."""

    def test_defaults(self) -> None:
        r = AQWABatchJobResult(job_name="aqwa_test", status="success")
        assert r.job_name == "aqwa_test"
        assert r.status == "success"
        assert r.run_result is None
        assert r.diffraction_results is None
        assert r.validation_report is None
        assert r.plot_files == []
        assert r.export_files == {}
        assert r.execution_time == 0.0
        assert r.error_message is None

    def test_custom_values(self) -> None:
        r = AQWABatchJobResult(
            job_name="custom_job",
            status="error",
            execution_time=12.5,
            error_message="AQWA solver failed",
        )
        assert r.job_name == "custom_job"
        assert r.status == "error"
        assert r.execution_time == 12.5
        assert r.error_message == "AQWA solver failed"


class TestAQWABatchReport:
    """Test AQWABatchReport dataclass."""

    def test_fields(self) -> None:
        report = AQWABatchReport(
            start_time="2026-01-01T00:00:00",
            end_time="2026-01-01T01:00:00",
            total_duration=3600.0,
            total_jobs=5,
            successful=3,
            failed=1,
            skipped=1,
            job_results=[],
        )
        assert report.start_time == "2026-01-01T00:00:00"
        assert report.end_time == "2026-01-01T01:00:00"
        assert report.total_duration == 3600.0
        assert report.total_jobs == 5
        assert report.successful == 3
        assert report.failed == 1
        assert report.skipped == 1
        assert report.job_results == []

    def test_defaults(self) -> None:
        report = AQWABatchReport(
            start_time="2026-01-01T00:00:00",
            end_time="2026-01-01T00:01:00",
            total_duration=60.0,
            total_jobs=0,
            successful=0,
            failed=0,
            skipped=0,
            job_results=[],
        )
        assert report.summary_statistics == {}


# ---------------------------------------------------------------------------
# Batch runner dry-run
# ---------------------------------------------------------------------------


class TestAQWABatchRunnerDryRun:
    """Test batch runner with dry_run jobs (no AQWA executable needed)."""

    def test_single_job_dry_run(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = AQWABatchConfig(
            jobs=[
                AQWABatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="aqwa_dry",
                    dry_run=True,
                )
            ],
            base_output_dir=tmp_path / "aqwa_batch",
            run_config=AQWARunConfig(dry_run=True),
        )
        runner = AQWABatchRunner(cfg)
        report = runner.run()
        assert isinstance(report, AQWABatchReport)
        assert report.total_jobs == 1
        assert report.successful + report.skipped + report.failed == 1

    def test_sequential_multi_job(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = AQWABatchConfig(
            jobs=[
                AQWABatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name=f"aqwa_seq_{i}",
                    dry_run=True,
                )
                for i in range(2)
            ],
            base_output_dir=tmp_path / "aqwa_batch",
            run_config=AQWARunConfig(dry_run=True),
        )
        runner = AQWABatchRunner(cfg)
        report = runner.run()
        assert report.total_jobs == 2

    def test_parallel_execution(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = AQWABatchConfig(
            jobs=[
                AQWABatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name=f"aqwa_par_{i}",
                    dry_run=True,
                )
                for i in range(2)
            ],
            execution_mode=AQWAExecutionMode.PARALLEL,
            max_workers=2,
            base_output_dir=tmp_path / "aqwa_batch",
            run_config=AQWARunConfig(dry_run=True),
        )
        runner = AQWABatchRunner(cfg)
        report = runner.run()
        assert report.total_jobs == 2

    def test_report_export(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = AQWABatchConfig(
            jobs=[
                AQWABatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="aqwa_export_test",
                    dry_run=True,
                )
            ],
            base_output_dir=tmp_path / "aqwa_batch",
            run_config=AQWARunConfig(dry_run=True),
        )
        runner = AQWABatchRunner(cfg)
        report = runner.run()
        out_file = tmp_path / "aqwa_report.json"
        runner.export_report(report, out_file)
        assert out_file.exists()
        data = json.loads(out_file.read_text())
        assert "total_jobs" in data
        assert "job_results" in data
        assert data["total_jobs"] == 1


# ---------------------------------------------------------------------------
# Convenience functions
# ---------------------------------------------------------------------------


class TestAQWABatchRunnerConvenience:
    """Test module-level convenience functions."""

    def test_run_aqwa_batch(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        config_data = {
            "jobs": [
                {
                    "spec_path": str(ship_raos_spec_path),
                    "job_name": "aqwa_conv",
                    "dry_run": True,
                },
            ],
            "execution_mode": "sequential",
            "base_output_dir": str(tmp_path / "aqwa_conv_output"),
        }
        config_file = tmp_path / "aqwa_conv_config.yml"
        config_file.write_text(yaml.dump(config_data))

        report = run_aqwa_batch(config_file)
        assert isinstance(report, AQWABatchReport)

    def test_run_aqwa_batch_from_specs(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        report = run_aqwa_batch_from_specs(
            spec_paths=[ship_raos_spec_path],
            output_dir=tmp_path / "aqwa_from_specs",
            dry_run=True,
            parallel=False,
        )
        assert isinstance(report, AQWABatchReport)
        assert report.total_jobs == 1
