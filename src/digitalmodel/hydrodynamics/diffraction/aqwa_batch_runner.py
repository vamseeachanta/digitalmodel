"""AQWA batch runner with full postprocessing pipeline (WRK-027 + WRK-028).

Orchestrates: solver run -> result extraction -> validation -> plotting -> export.

Data flow::

    AQWABatchConfig (list of job specs)
      -> AQWABatchRunner.run()
         for each job:
           -> AQWARunner.run(spec)
           -> AQWAResultExtractor.extract(run_result)
           -> OutputValidator.run_all_validations()
           -> RAOPlotter.plot_all()
           -> PolarsExporter.export_all_csv()
      -> AQWABatchReport
"""
from __future__ import annotations

import json
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunner,
    AQWARunConfig,
    AQWARunResult,
    AQWARunStatus,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults
from digitalmodel.hydrodynamics.diffraction.output_validator import OutputValidator
from digitalmodel.hydrodynamics.diffraction.polars_exporter import PolarsExporter
from digitalmodel.hydrodynamics.diffraction.rao_plotter import RAOPlotter
from digitalmodel.hydrodynamics.diffraction.aqwa_result_extractor import AQWAResultExtractor


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class AQWAExecutionMode(str, Enum):
    SEQUENTIAL = "sequential"
    PARALLEL = "parallel"


# ---------------------------------------------------------------------------
# Config models
# ---------------------------------------------------------------------------


class AQWABatchJobConfig(BaseModel):
    """Configuration for a single AQWA batch job."""

    spec_path: Path
    job_name: str | None = None
    output_dir: Path | None = None
    dry_run: bool = False
    water_depth_override: float | None = None
    validate: bool = True
    generate_plots: bool = True
    export_formats: list[str] = Field(default_factory=lambda: ["csv"])

    model_config = {"arbitrary_types_allowed": True}


class AQWABatchConfig(BaseModel):
    """Top-level AQWA batch configuration."""

    jobs: list[AQWABatchJobConfig]
    execution_mode: AQWAExecutionMode = AQWAExecutionMode.SEQUENTIAL
    max_workers: int = 4
    base_output_dir: Path = Field(default=Path("batch_output"))
    run_config: AQWARunConfig = Field(default_factory=AQWARunConfig)
    generate_summary_report: bool = True

    model_config = {"arbitrary_types_allowed": True}

    @classmethod
    def from_yaml(cls, path: Path) -> AQWABatchConfig:
        with open(path) as f:
            raw = yaml.safe_load(f)
        jobs_raw = raw.pop("jobs", [])
        jobs = [AQWABatchJobConfig(**j) for j in jobs_raw]
        return cls(jobs=jobs, **raw)


# ---------------------------------------------------------------------------
# Result dataclasses
# ---------------------------------------------------------------------------


@dataclass
class AQWABatchJobResult:
    """Result of a single AQWA batch job."""

    job_name: str
    status: str  # success, error, skipped, dry_run
    run_result: AQWARunResult | None = None
    diffraction_results: DiffractionResults | None = None
    validation_report: dict[str, Any] | None = None
    plot_files: list[Path] = field(default_factory=list)
    export_files: dict[str, Path] = field(default_factory=dict)
    execution_time: float = 0.0
    error_message: str | None = None


@dataclass
class AQWABatchReport:
    """Aggregated AQWA batch execution report."""

    start_time: str
    end_time: str
    total_duration: float
    total_jobs: int
    successful: int
    failed: int
    skipped: int
    job_results: list[AQWABatchJobResult]
    summary_statistics: dict[str, Any] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------


class AQWABatchRunner:
    """Batch orchestrator for AQWA diffraction analyses."""

    def __init__(self, config: AQWABatchConfig) -> None:
        self._config = config

    def run(self) -> AQWABatchReport:
        start = datetime.now()
        if self._config.execution_mode == AQWAExecutionMode.PARALLEL:
            job_results = self._run_parallel()
        else:
            job_results = self._run_sequential()
        return self._build_report(job_results, start)

    # ----- execution modes -----

    def _run_sequential(self) -> list[AQWABatchJobResult]:
        results: list[AQWABatchJobResult] = []
        for job in self._config.jobs:
            results.append(self._process_single_job(job))
        return results

    def _run_parallel(self) -> list[AQWABatchJobResult]:
        results: list[AQWABatchJobResult] = []
        with ThreadPoolExecutor(max_workers=self._config.max_workers) as executor:
            futures = {
                executor.submit(self._process_single_job, job): job
                for job in self._config.jobs
            }
            for future in as_completed(futures):
                results.append(future.result())
        return results

    # ----- single job processing -----

    def _process_single_job(self, job: AQWABatchJobConfig) -> AQWABatchJobResult:
        job_start = time.monotonic()
        job_name = job.job_name or job.spec_path.stem

        output_dir = job.output_dir or (self._config.base_output_dir / job_name)
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        try:
            spec = DiffractionSpec.from_yaml(job.spec_path)
        except Exception as exc:
            return AQWABatchJobResult(
                job_name=job_name,
                status="error",
                execution_time=time.monotonic() - job_start,
                error_message=f"Failed to load spec: {exc}",
            )

        # Run solver
        run_result = self._run_solver(spec, job, output_dir)

        # Dry-run shortcut
        if job.dry_run or run_result.status == AQWARunStatus.DRY_RUN:
            # Attempt extraction even on dry-run if results available
            diff_results = self._extract_results(run_result, job)
            result = AQWABatchJobResult(
                job_name=job_name,
                status="dry_run",
                run_result=run_result,
                diffraction_results=diff_results,
                execution_time=time.monotonic() - job_start,
            )
            if diff_results is not None:
                self._postprocess(result, job, diff_results, output_dir)
            return result

        if run_result.status == AQWARunStatus.FAILED:
            return AQWABatchJobResult(
                job_name=job_name,
                status="error",
                run_result=run_result,
                execution_time=time.monotonic() - job_start,
                error_message=run_result.error_message,
            )

        # Extract results
        diff_results = self._extract_results(run_result, job)
        status = "success" if diff_results is not None else "error"
        result = AQWABatchJobResult(
            job_name=job_name,
            status=status,
            run_result=run_result,
            diffraction_results=diff_results,
            execution_time=time.monotonic() - job_start,
        )

        if diff_results is not None:
            self._postprocess(result, job, diff_results, output_dir)

        return result

    # ----- pipeline steps -----

    def _run_solver(
        self, spec: DiffractionSpec, job: AQWABatchJobConfig, output_dir: Path
    ) -> AQWARunResult:
        config = self._config.run_config.model_copy()
        config.output_dir = output_dir
        if job.dry_run:
            config.dry_run = True
        runner = AQWARunner(config)
        return runner.run(spec, spec_path=job.spec_path)

    def _extract_results(
        self, run_result: AQWARunResult, job: AQWABatchJobConfig
    ) -> DiffractionResults | None:
        extractor = AQWAResultExtractor(
            vessel_name=run_result.spec_name,
            water_depth=job.water_depth_override,
        )
        extraction = extractor.extract(run_result)
        if extraction.success:
            return extraction.results
        return None

    def _postprocess(
        self,
        result: AQWABatchJobResult,
        job: AQWABatchJobConfig,
        diff_results: DiffractionResults,
        output_dir: Path,
    ) -> None:
        if job.validate:
            result.validation_report = self._validate_results(diff_results, output_dir)
        if job.generate_plots:
            result.plot_files = self._generate_plots(diff_results, output_dir)
        if job.export_formats:
            result.export_files = self._export_results(
                diff_results, output_dir, job.export_formats
            )

    def _validate_results(
        self, results: DiffractionResults, output_dir: Path
    ) -> dict[str, Any]:
        validator = OutputValidator(results)
        report = validator.run_all_validations()
        report_path = output_dir / "validation_report.json"
        validator.export_report(report_path)
        return report

    def _generate_plots(
        self, results: DiffractionResults, output_dir: Path
    ) -> list[Path]:
        plotter = RAOPlotter(results, output_dir / "plots")
        return plotter.plot_all()

    def _export_results(
        self,
        results: DiffractionResults,
        output_dir: Path,
        formats: list[str],
    ) -> dict[str, Path]:
        files: dict[str, Path] = {}
        if "csv" in formats:
            exporter = PolarsExporter(results)
            files.update(exporter.export_all_csv(output_dir / "exports"))
        return files

    # ----- report -----

    def _build_report(
        self, job_results: list[AQWABatchJobResult], start: datetime
    ) -> AQWABatchReport:
        end = datetime.now()
        successful = sum(1 for r in job_results if r.status == "success")
        failed = sum(1 for r in job_results if r.status == "error")
        skipped = sum(
            1 for r in job_results if r.status in ("skipped", "dry_run")
        )
        return AQWABatchReport(
            start_time=start.isoformat(),
            end_time=end.isoformat(),
            total_duration=(end - start).total_seconds(),
            total_jobs=len(job_results),
            successful=successful,
            failed=failed,
            skipped=skipped,
            job_results=job_results,
            summary_statistics={
                "avg_execution_time": (
                    sum(r.execution_time for r in job_results) / len(job_results)
                    if job_results
                    else 0.0
                ),
            },
        )

    def export_report(self, report: AQWABatchReport, output_file: Path) -> None:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        serializable = {
            "start_time": report.start_time,
            "end_time": report.end_time,
            "total_duration": report.total_duration,
            "total_jobs": report.total_jobs,
            "successful": report.successful,
            "failed": report.failed,
            "skipped": report.skipped,
            "summary_statistics": report.summary_statistics,
            "job_results": [
                {
                    "job_name": r.job_name,
                    "status": r.status,
                    "execution_time": r.execution_time,
                    "error_message": r.error_message,
                    "plot_files": [str(p) for p in r.plot_files],
                    "export_files": {k: str(v) for k, v in r.export_files.items()},
                }
                for r in report.job_results
            ],
        }
        with open(output_file, "w") as f:
            json.dump(serializable, f, indent=2)


# ---------------------------------------------------------------------------
# Convenience functions
# ---------------------------------------------------------------------------


def run_aqwa_batch(config_path: Path) -> AQWABatchReport:
    """Run an AQWA batch from a YAML config file."""
    config = AQWABatchConfig.from_yaml(config_path)
    runner = AQWABatchRunner(config)
    return runner.run()


def run_aqwa_batch_from_specs(
    spec_paths: list[Path],
    output_dir: Path,
    dry_run: bool = False,
    parallel: bool = False,
) -> AQWABatchReport:
    """Run an AQWA batch from a list of spec paths."""
    jobs = [
        AQWABatchJobConfig(spec_path=p, dry_run=dry_run)
        for p in spec_paths
    ]
    config = AQWABatchConfig(
        jobs=jobs,
        execution_mode=AQWAExecutionMode.PARALLEL if parallel else AQWAExecutionMode.SEQUENTIAL,
        base_output_dir=output_dir,
        run_config=AQWARunConfig(dry_run=dry_run),
    )
    runner = AQWABatchRunner(config)
    return runner.run()
