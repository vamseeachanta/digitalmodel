"""Tests for BenchmarkRunner (end-to-end benchmark orchestration)."""
from __future__ import annotations

from pathlib import Path
from typing import Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
    BenchmarkRunResult,
    SolverType,
    run_benchmark,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


# ---------------------------------------------------------------------------
# 1. BenchmarkConfig defaults and custom values
# ---------------------------------------------------------------------------


class TestBenchmarkConfig:
    """Validate BenchmarkConfig default fields and custom overrides."""

    def test_config_default_solvers(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert len(config.solvers) == 3
        assert SolverType.AQWA in config.solvers
        assert SolverType.ORCAWAVE in config.solvers
        assert SolverType.BEMROSETTA in config.solvers

    def test_config_default_tolerance(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert config.tolerance == pytest.approx(0.05)

    def test_config_default_output_dir(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert config.output_dir == Path("benchmark_output")

    def test_config_custom_values(self) -> None:
        # Act
        config = BenchmarkConfig(
            tolerance=0.10,
            output_dir=Path("/tmp/custom_output"),
            headings=[0.0, 90.0, 180.0],
        )

        # Assert
        assert config.tolerance == pytest.approx(0.10)
        assert config.output_dir == Path("/tmp/custom_output")
        assert config.headings == [0.0, 90.0, 180.0]


# ---------------------------------------------------------------------------
# 2. BenchmarkRunResult defaults
# ---------------------------------------------------------------------------


class TestBenchmarkRunResult:
    """Validate BenchmarkRunResult default state."""

    def test_run_result_defaults_not_successful(self) -> None:
        # Act
        result = BenchmarkRunResult()

        # Assert
        assert result.success is False
        assert result.report is None
        assert result.error_message is None
        assert result.plot_paths == []
        assert result.solver_results == {}


# ---------------------------------------------------------------------------
# 3. BenchmarkRunner.run_from_results
# ---------------------------------------------------------------------------


class TestRunFromResults:
    """Verify BenchmarkRunner.run_from_results orchestration."""

    def test_run_from_results_success(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.success is True
        assert result.report is not None
        assert len(result.plot_paths) > 0

    def test_run_from_results_creates_json_report(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.report_json_path is not None
        assert result.report_json_path.exists()
        assert result.report_json_path.suffix == ".json"

    def test_run_from_results_creates_html_report(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.report_html_path is not None
        assert result.report_html_path.exists()
        assert result.report_html_path.suffix == ".html"

    def test_run_from_results_dry_run(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path, dry_run=True)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert - dry run still produces a report but no plot files
        assert result.success is True
        assert result.report is not None
        assert result.plot_paths == []


# ---------------------------------------------------------------------------
# 4. Convenience function
# ---------------------------------------------------------------------------


class TestRunBenchmarkConvenience:
    """Verify the run_benchmark top-level convenience function."""

    def test_run_benchmark_convenience_function(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Act
        result = run_benchmark(
            solver_results=three_solver_results,
            output_dir=tmp_path,
            tolerance=0.05,
        )

        # Assert
        assert result.success is True
        assert result.report is not None
