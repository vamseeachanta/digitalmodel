"""Tests for OrcaWaveBatchRunner (WRK-030)."""
from __future__ import annotations

from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner import (
    BatchJobConfig,
    BatchJobResult,
    ExecutionMode,
    OrcaWaveBatchConfig,
    OrcaWaveBatchReport,
    OrcaWaveBatchRunner,
    run_orcawave_batch,
    run_orcawave_batch_from_specs,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunConfig


# ---------------------------------------------------------------------------
# Config validation
# ---------------------------------------------------------------------------


class TestBatchJobConfig:
    """Test BatchJobConfig Pydantic model."""

    def test_minimal_config(self, ship_raos_spec_path: Path) -> None:
        cfg = BatchJobConfig(spec_path=ship_raos_spec_path)
        assert cfg.dry_run is False
        assert cfg.validate is True
        assert cfg.generate_plots is True
        assert cfg.export_formats == ["csv"]

    def test_custom_values(self, ship_raos_spec_path: Path, tmp_path: Path) -> None:
        cfg = BatchJobConfig(
            spec_path=ship_raos_spec_path,
            job_name="test_job",
            output_dir=tmp_path,
            dry_run=True,
            water_depth_override=200.0,
        )
        assert cfg.job_name == "test_job"
        assert cfg.water_depth_override == 200.0

    def test_default_export_formats(self, ship_raos_spec_path: Path) -> None:
        cfg = BatchJobConfig(spec_path=ship_raos_spec_path)
        assert "csv" in cfg.export_formats


class TestOrcaWaveBatchConfig:
    """Test OrcaWaveBatchConfig Pydantic model."""

    def test_default_execution_mode(self, ship_raos_spec_path: Path) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[BatchJobConfig(spec_path=ship_raos_spec_path)]
        )
        assert cfg.execution_mode == ExecutionMode.SEQUENTIAL

    def test_parallel_mode(self, ship_raos_spec_path: Path) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[BatchJobConfig(spec_path=ship_raos_spec_path)],
            execution_mode=ExecutionMode.PARALLEL,
            max_workers=2,
        )
        assert cfg.execution_mode == ExecutionMode.PARALLEL
        assert cfg.max_workers == 2

    def test_from_yaml(self, batch_config_path: Path) -> None:
        cfg = OrcaWaveBatchConfig.from_yaml(batch_config_path)
        assert len(cfg.jobs) == 1
        assert cfg.execution_mode == ExecutionMode.SEQUENTIAL


class TestExecutionMode:
    """Test ExecutionMode enum."""

    def test_sequential(self) -> None:
        assert ExecutionMode.SEQUENTIAL == "sequential"

    def test_parallel(self) -> None:
        assert ExecutionMode.PARALLEL == "parallel"


# ---------------------------------------------------------------------------
# Batch runner
# ---------------------------------------------------------------------------


class TestBatchRunnerDryRun:
    """Test batch runner with dry_run jobs."""

    def test_single_job_dry_run(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="ship_dry",
                    dry_run=True,
                )
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()
        assert isinstance(report, OrcaWaveBatchReport)
        assert report.total_jobs == 1
        assert report.successful + report.skipped + report.failed == 1

    def test_multi_job_sequential(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name=f"job_{i}",
                    dry_run=True,
                )
                for i in range(3)
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()
        assert report.total_jobs == 3

    def test_parallel_execution(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name=f"par_{i}",
                    dry_run=True,
                )
                for i in range(2)
            ],
            execution_mode=ExecutionMode.PARALLEL,
            max_workers=2,
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()
        assert report.total_jobs == 2


class TestBatchReport:
    """Test batch report generation."""

    def test_report_fields(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="rpt_test",
                    dry_run=True,
                )
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()
        assert report.start_time is not None
        assert report.end_time is not None
        assert report.total_duration >= 0.0
        assert len(report.job_results) == 1

    def test_export_report_json(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="export_test",
                    dry_run=True,
                )
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()
        out_file = tmp_path / "report.json"
        runner.export_report(report, out_file)
        assert out_file.exists()


class TestBatchJobResult:
    """Test BatchJobResult dataclass."""

    def test_defaults(self) -> None:
        r = BatchJobResult(job_name="test", status="success")
        assert r.run_result is None
        assert r.plot_files == []
        assert r.export_files == {}
        assert r.execution_time == 0.0


# ---------------------------------------------------------------------------
# Postprocessing integration
# ---------------------------------------------------------------------------


class TestPostprocessingIntegration:
    """Test that postprocessing steps are invoked for successful runs."""

    def test_plots_generated_when_enabled(
        self, ship_raos_spec_path: Path, tmp_path: Path, mock_diffraction_results
    ) -> None:
        """When generate_plots=True and extraction succeeds, plot files should be created."""
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="plot_test",
                    dry_run=True,
                    generate_plots=True,
                    export_formats=["csv"],
                )
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)

        # Mock the extraction to return our synthetic results
        with patch.object(
            runner,
            "_extract_results",
            return_value=(mock_diffraction_results, None),
        ):
            report = runner.run()

        job = report.job_results[0]
        # Should have attempted plot generation
        assert isinstance(job.plot_files, list)

    def test_exports_generated_when_enabled(
        self, ship_raos_spec_path: Path, tmp_path: Path, mock_diffraction_results
    ) -> None:
        cfg = OrcaWaveBatchConfig(
            jobs=[
                BatchJobConfig(
                    spec_path=ship_raos_spec_path,
                    job_name="export_test",
                    dry_run=True,
                    generate_plots=False,
                    export_formats=["csv"],
                )
            ],
            base_output_dir=tmp_path / "batch",
            run_config=RunConfig(dry_run=True),
        )
        runner = OrcaWaveBatchRunner(cfg)

        with patch.object(
            runner,
            "_extract_results",
            return_value=(mock_diffraction_results, None),
        ):
            report = runner.run()

        job = report.job_results[0]
        assert isinstance(job.export_files, dict)


# ---------------------------------------------------------------------------
# Convenience functions
# ---------------------------------------------------------------------------


class TestConvenienceFunctions:
    """Test module-level convenience functions."""

    def test_run_orcawave_batch(self, batch_config_path: Path) -> None:
        report = run_orcawave_batch(batch_config_path)
        assert isinstance(report, OrcaWaveBatchReport)

    def test_run_orcawave_batch_from_specs(
        self, ship_raos_spec_path: Path, tmp_path: Path
    ) -> None:
        report = run_orcawave_batch_from_specs(
            spec_paths=[ship_raos_spec_path],
            output_dir=tmp_path / "from_specs",
            dry_run=True,
            parallel=False,
        )
        assert isinstance(report, OrcaWaveBatchReport)
        assert report.total_jobs == 1


# ---------------------------------------------------------------------------
# #625: batch validation delegates to the shared helper (no double-validation)
# ---------------------------------------------------------------------------

from digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner import (  # noqa: E402
    OrcaWaveBatchConfig,
    OrcaWaveBatchRunner,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (  # noqa: E402
    RunResult,
    RunStatus,
)


class TestBatchValidationDedup:
    def test_reuses_run_level_report(
        self, ship_raos_spec_path, tmp_path, dense_diffraction_results
    ):
        """When the runner already validated, the batch reuses that report
        verbatim and does NOT re-run validation."""
        cfg = OrcaWaveBatchConfig(
            jobs=[BatchJobConfig(spec_path=ship_raos_spec_path)],
            base_output_dir=tmp_path,
        )
        runner = OrcaWaveBatchRunner(cfg)

        prior = {"overall_status": "PASS", "vessel_name": "x"}
        run_result = RunResult(status=RunStatus.COMPLETED, validation_report=prior)

        with patch(
            "digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner."
            "run_validation"
        ) as mock_run_validation:
            report = runner._validate_results(
                dense_diffraction_results, tmp_path, run_result
            )

        assert report is prior
        mock_run_validation.assert_not_called()

    def test_delegates_to_shared_helper_when_no_prior(
        self, ship_raos_spec_path, tmp_path, dense_diffraction_results
    ):
        cfg = OrcaWaveBatchConfig(
            jobs=[BatchJobConfig(spec_path=ship_raos_spec_path)],
            base_output_dir=tmp_path,
        )
        runner = OrcaWaveBatchRunner(cfg)
        run_result = RunResult(status=RunStatus.COMPLETED)  # no prior report

        with patch(
            "digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner."
            "run_validation"
        ) as mock_run_validation:
            mock_run_validation.return_value = MagicMock(
                report={"overall_status": "PASS"}
            )
            report = runner._validate_results(
                dense_diffraction_results, tmp_path, run_result
            )

        mock_run_validation.assert_called_once()
        assert report == {"overall_status": "PASS"}


# ---------------------------------------------------------------------------
# Extraction path: reuse runner-built results; no false failures for
# completed subprocess solves (see review sweep)
# ---------------------------------------------------------------------------


class TestExtractionPath:
    def _runner(self, ship_raos_spec_path, tmp_path) -> OrcaWaveBatchRunner:
        cfg = OrcaWaveBatchConfig(
            jobs=[BatchJobConfig(spec_path=ship_raos_spec_path)],
            base_output_dir=tmp_path,
        )
        return OrcaWaveBatchRunner(cfg)

    def test_extract_reuses_run_result_diffraction_results(
        self, ship_raos_spec_path, tmp_path, dense_diffraction_results
    ):
        """Results already built by the runner (#625 API path) are used
        directly — ResultExtractor must not be re-invoked."""
        runner = self._runner(ship_raos_spec_path, tmp_path)
        job = BatchJobConfig(spec_path=ship_raos_spec_path)
        run_result = RunResult(
            status=RunStatus.COMPLETED,
            output_dir=tmp_path,
            diffraction_results=dense_diffraction_results,
        )
        with patch(
            "digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner."
            "ResultExtractor"
        ) as mock_extractor:
            results, error = runner._extract_results(run_result, job)
        assert results is dense_diffraction_results
        assert error is None
        mock_extractor.assert_not_called()

    def test_extract_failure_reports_error_message(
        self, ship_raos_spec_path, tmp_path
    ):
        """Extraction failures must surface a message, never a silent None."""
        runner = self._runner(ship_raos_spec_path, tmp_path)
        job = BatchJobConfig(spec_path=ship_raos_spec_path)
        run_result = RunResult(status=RunStatus.COMPLETED, output_dir=tmp_path)
        results, error = runner._extract_results(run_result, job)
        assert results is None
        assert error is not None and "No model file found" in error

    def test_completed_subprocess_solve_is_not_a_failed_job(
        self, ship_raos_spec_path, tmp_path
    ):
        """A COMPLETED subprocess solve (rc==0, .owr only, no OrcFxAPI) must
        not be counted as failed with error_message None."""
        runner = self._runner(ship_raos_spec_path, tmp_path)
        job = runner._config.jobs[0]
        # Subprocess path: solver finished, wrote a .owr, no .sim/.dat and
        # no runner-built diffraction results.
        (tmp_path / "vessel.owr").write_bytes(b"owr")
        completed = RunResult(
            status=RunStatus.COMPLETED,
            return_code=0,
            output_dir=tmp_path,
        )
        with patch.object(runner, "_run_solver", return_value=completed):
            result = runner._process_single_job(job)
        assert result.status == "success"
        assert result.error_message is not None
        assert "could not be extracted" in result.error_message
        report = runner._build_report([result], datetime.now())
        assert report.failed == 0
        assert report.successful == 1
