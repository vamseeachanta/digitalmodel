# ABOUTME: Tests for BatchRunner — batch APDL script generation with parameter sweeps
# ABOUTME: Verifies parameter matrix, load case parsing, results collection, summary reports

"""Tests for batch_runner — BatchRunner parametric study and load case management."""

import json
import pytest

from digitalmodel.ansys.batch_runner import (
    BatchConfig,
    BatchLoadConfig,
    BatchResults,
    BatchResultEntry,
    BatchRun,
    BatchRunner,
    LoadCase,
    ParameterSweep,
    RunStatus,
)


def _runner() -> BatchRunner:
    return BatchRunner()


# ---------------------------------------------------------------------------
# Parameter sweep creation (existing)
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
# Run matrix generation (existing)
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
# Run generation (existing)
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
# Batch script generation (existing)
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
# CSV summary (existing)
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


# ---------------------------------------------------------------------------
# Load case YAML parsing (NEW)
# ---------------------------------------------------------------------------

SAMPLE_YAML = """\
load_cases:
  - name: "Operating"
    description: "Normal operating conditions"
    loads:
      pressure_mpa: 10.0
      temperature_c: 200.0
    boundary_conditions:
      fixed_end: 0.0
  - name: "Hydrotest"
    description: "Hydrostatic test"
    loads:
      pressure_mpa: 13.0
      temperature_c: 25.0
    boundary_conditions:
      fixed_end: 0.0
  - name: "Shutdown"
    loads:
      pressure_mpa: 0.0
      temperature_c: 25.0
    boundary_conditions:
      fixed_end: 0.0
"""


class TestLoadCaseYAMLParsing:
    def test_parses_correct_count(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        assert len(cases) == 3

    def test_parses_load_case_names(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        names = [c.name for c in cases]
        assert "Operating" in names
        assert "Hydrotest" in names
        assert "Shutdown" in names

    def test_parses_loads(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        operating = [c for c in cases if c.name == "Operating"][0]
        assert operating.loads["pressure_mpa"] == 10.0
        assert operating.loads["temperature_c"] == 200.0

    def test_parses_boundary_conditions(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        operating = [c for c in cases if c.name == "Operating"][0]
        assert "fixed_end" in operating.boundary_conditions

    def test_parses_description(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        operating = [c for c in cases if c.name == "Operating"][0]
        assert "Normal operating" in operating.description

    def test_empty_yaml_returns_empty_list(self):
        cases = BatchRunner.parse_load_cases_yaml("")
        assert cases == []

    def test_case_without_description_gets_empty_string(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        shutdown = [c for c in cases if c.name == "Shutdown"][0]
        assert shutdown.description == ""

    def test_parses_all_load_values_as_float(self):
        cases = BatchRunner.parse_load_cases_yaml(SAMPLE_YAML)
        for case in cases:
            for val in case.loads.values():
                assert isinstance(val, float)


# ---------------------------------------------------------------------------
# APDL script generation for load cases (NEW)
# ---------------------------------------------------------------------------

class TestBatchScriptsFromLoadCases:
    def test_generates_script_per_load_case(self):
        config = BatchLoadConfig(
            project_name="PV_Test",
            base_model="SFA,ALL,1,PRES,{pressure_mpa}",
            load_cases=[
                LoadCase(name="Operating", loads={"pressure_mpa": 10.0}),
                LoadCase(name="Hydrotest", loads={"pressure_mpa": 13.0}),
            ],
        )
        scripts = _runner().generate_batch_scripts_from_load_cases(config)
        assert len(scripts) == 2

    def test_script_contains_substituted_values(self):
        config = BatchLoadConfig(
            base_model="SFA,ALL,1,PRES,{pressure_mpa}",
            load_cases=[
                LoadCase(name="Operating", loads={"pressure_mpa": 10.0}),
            ],
        )
        scripts = _runner().generate_batch_scripts_from_load_cases(config)
        path, content = scripts[0]
        assert "10.0" in content

    def test_script_path_uses_load_case_name(self):
        config = BatchLoadConfig(
            output_dir="results",
            load_cases=[
                LoadCase(name="My Case", loads={}),
            ],
        )
        scripts = _runner().generate_batch_scripts_from_load_cases(config)
        path, _ = scripts[0]
        assert "My_Case" in path


# ---------------------------------------------------------------------------
# Results collection from mock outputs (NEW)
# ---------------------------------------------------------------------------

MOCK_OUTPUT_OK = """\
 NUMBER OF NODES   =    5000
 SOLUTION IS CONVERGED
 MAXIMUM SEQV =  150.5  AT NODE=  1234
 MAXIMUM USUM =  0.350  AT NODE=  2345
"""

MOCK_OUTPUT_FAIL = """\
 NUMBER OF NODES   =    5000
 *** WARNING: SOLUTION DID NOT CONVERGE ***
"""


class TestCollectResults:
    def test_collects_converged_case(self):
        config = BatchLoadConfig(
            load_cases=[LoadCase(name="Operating")],
        )
        results = _runner().collect_results_from_outputs(
            config, {"Operating": MOCK_OUTPUT_OK}
        )
        assert results.completed_runs == 1
        assert results.entries[0].converged is True
        assert results.entries[0].max_stress_mpa == pytest.approx(150.5)

    def test_collects_failed_case(self):
        config = BatchLoadConfig(
            load_cases=[LoadCase(name="Bad")],
        )
        results = _runner().collect_results_from_outputs(
            config, {"Bad": MOCK_OUTPUT_FAIL}
        )
        assert results.failed_runs == 1
        assert results.entries[0].converged is False

    def test_missing_output_is_failure(self):
        config = BatchLoadConfig(
            load_cases=[LoadCase(name="Missing")],
        )
        results = _runner().collect_results_from_outputs(config, {})
        assert results.failed_runs == 1
        assert "No output" in results.entries[0].error_message

    def test_multi_case_results_count(self):
        config = BatchLoadConfig(
            load_cases=[
                LoadCase(name="A"),
                LoadCase(name="B"),
                LoadCase(name="C"),
            ],
        )
        outputs = {
            "A": MOCK_OUTPUT_OK,
            "B": MOCK_OUTPUT_OK,
            "C": MOCK_OUTPUT_FAIL,
        }
        results = _runner().collect_results_from_outputs(config, outputs)
        assert results.total_runs == 3
        assert results.completed_runs == 2
        assert results.failed_runs == 1


# ---------------------------------------------------------------------------
# Summary report generation (NEW)
# ---------------------------------------------------------------------------

class TestSummaryReport:
    def _sample_results(self) -> BatchResults:
        return BatchResults(
            project_name="Test",
            total_runs=2,
            completed_runs=1,
            failed_runs=1,
            entries=[
                BatchResultEntry(
                    load_case_name="Operating",
                    status=RunStatus.COMPLETED,
                    max_stress_mpa=150.0,
                    max_displacement_mm=0.5,
                    converged=True,
                ),
                BatchResultEntry(
                    load_case_name="Bad",
                    status=RunStatus.FAILED,
                    error_message="Did not converge",
                ),
            ],
        )

    def test_csv_report_has_header(self):
        report = _runner().generate_summary_report(self._sample_results(), format="csv")
        assert "load_case" in report
        assert "status" in report

    def test_csv_report_has_correct_rows(self):
        report = _runner().generate_summary_report(self._sample_results(), format="csv")
        lines = [l for l in report.strip().split("\n") if l]
        assert len(lines) == 3  # header + 2 data rows

    def test_json_report_is_valid_json(self):
        report = _runner().generate_summary_report(self._sample_results(), format="json")
        data = json.loads(report)
        assert data["project"] == "Test"
        assert len(data["cases"]) == 2

    def test_json_report_contains_status(self):
        report = _runner().generate_summary_report(self._sample_results(), format="json")
        data = json.loads(report)
        statuses = [c["status"] for c in data["cases"]]
        assert "completed" in statuses
        assert "failed" in statuses

    def test_csv_report_contains_stress_values(self):
        report = _runner().generate_summary_report(self._sample_results(), format="csv")
        assert "150.0" in report
