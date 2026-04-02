# ABOUTME: Tests for BatchRunner — batch APDL script generation with parameter sweeps
# ABOUTME: Verifies parameter matrix, script substitution, batch file generation

"""Tests for batch_runner — BatchRunner parametric study generation."""

import pytest

from digitalmodel.ansys.batch_runner import (
    BatchConfig,
    BatchRun,
    BatchRunner,
    ParameterSweep,
)


def _runner() -> BatchRunner:
    return BatchRunner()


# ---------------------------------------------------------------------------
# Parameter sweep creation
# ---------------------------------------------------------------------------

class TestParameterSweep:
    def test_from_range_creates_correct_count(self):
        sweep = ParameterSweep.from_range("thickness", 10.0, 50.0, num=5)
        assert len(sweep.values) == 5

    def test_from_range_includes_endpoints(self):
        sweep = ParameterSweep.from_range("pressure", 5.0, 15.0, num=3)
        assert pytest.approx(sweep.values[0]) == 5.0
        assert pytest.approx(sweep.values[-1]) == 15.0

    def test_from_list_preserves_values(self):
        sweep = ParameterSweep.from_list("temp", [100.0, 200.0, 300.0])
        assert sweep.values == [100.0, 200.0, 300.0]


# ---------------------------------------------------------------------------
# Run matrix generation
# ---------------------------------------------------------------------------

class TestRunMatrix:
    def test_combinatorial_product(self):
        config = BatchConfig(
            sweeps=[
                ParameterSweep.from_list("A", [1.0, 2.0]),
                ParameterSweep.from_list("B", [10.0, 20.0]),
            ],
            combinatorial=True,
        )
        matrix = _runner().generate_run_matrix(config)
        assert len(matrix) == 4  # 2 x 2

    def test_zip_mode(self):
        config = BatchConfig(
            sweeps=[
                ParameterSweep.from_list("A", [1.0, 2.0, 3.0]),
                ParameterSweep.from_list("B", [10.0, 20.0, 30.0]),
            ],
            combinatorial=False,
        )
        matrix = _runner().generate_run_matrix(config)
        assert len(matrix) == 3

    def test_empty_sweeps_returns_single_empty_dict(self):
        config = BatchConfig(sweeps=[])
        matrix = _runner().generate_run_matrix(config)
        assert len(matrix) == 1
        assert matrix[0] == {}


# ---------------------------------------------------------------------------
# Run generation
# ---------------------------------------------------------------------------

class TestGenerateRuns:
    def test_apdl_content_contains_parameter_values(self):
        config = BatchConfig(
            base_template="MP,EX,1,{E_MOD}\nSFA,ALL,1,PRES,{PRESSURE}",
            sweeps=[
                ParameterSweep.from_list("E_MOD", [200000.0]),
                ParameterSweep.from_list("PRESSURE", [10.0]),
            ],
        )
        runs = _runner().generate_runs(config)
        assert len(runs) == 1
        assert "200000.0" in runs[0].apdl_content
        assert "10.0" in runs[0].apdl_content

    def test_runs_have_unique_names(self):
        config = BatchConfig(
            sweeps=[ParameterSweep.from_list("P", [1.0, 2.0, 3.0])],
        )
        runs = _runner().generate_runs(config)
        names = [r.run_name for r in runs]
        assert len(set(names)) == 3

    def test_run_header_contains_project_name(self):
        config = BatchConfig(
            project_name="TestProject",
            sweeps=[ParameterSweep.from_list("P", [1.0])],
        )
        runs = _runner().generate_runs(config)
        assert "TestProject" in runs[0].apdl_content


# ---------------------------------------------------------------------------
# Batch script generation
# ---------------------------------------------------------------------------

class TestBatchScript:
    def _make_runs(self) -> tuple[BatchConfig, list[BatchRun]]:
        config = BatchConfig(
            project_name="Test",
            platform="linux",
            sweeps=[ParameterSweep.from_list("P", [1.0, 2.0])],
        )
        runs = _runner().generate_runs(config)
        return config, runs

    def test_linux_script_starts_with_shebang(self):
        config, runs = self._make_runs()
        script = _runner().generate_batch_script(config, runs)
        assert script.startswith("#!/bin/bash")

    def test_windows_script_starts_with_echo_off(self):
        config, runs = self._make_runs()
        config.platform = "windows"
        script = _runner().generate_batch_script(config, runs)
        assert script.startswith("@echo off")

    def test_batch_script_contains_all_runs(self):
        config, runs = self._make_runs()
        script = _runner().generate_batch_script(config, runs)
        assert "run_0000" in script
        assert "run_0001" in script


# ---------------------------------------------------------------------------
# CSV summary
# ---------------------------------------------------------------------------

class TestRunSummaryCSV:
    def test_csv_has_header(self):
        config = BatchConfig(
            sweeps=[ParameterSweep.from_list("pressure", [5.0, 10.0])],
        )
        runs = _runner().generate_runs(config)
        csv = _runner().generate_run_summary_csv(runs)
        assert "run_id" in csv
        assert "pressure" in csv

    def test_csv_has_correct_row_count(self):
        config = BatchConfig(
            sweeps=[ParameterSweep.from_list("P", [1.0, 2.0, 3.0])],
        )
        runs = _runner().generate_runs(config)
        csv = _runner().generate_run_summary_csv(runs)
        lines = [l for l in csv.strip().split("\n") if l]
        assert len(lines) == 4  # 1 header + 3 data rows

    def test_empty_runs_returns_header_only(self):
        csv = _runner().generate_run_summary_csv([])
        assert "run_id" in csv
