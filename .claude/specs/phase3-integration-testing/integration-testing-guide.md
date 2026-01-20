# Phase 3: Integration Testing Guide

> Comprehensive guide for writing, executing, and maintaining integration tests
> Version: 1.0.0
> Last Updated: 2025-01-09

## Table of Contents

1. [Real-World Integration Patterns](#real-world-integration-patterns)
2. [Writing Integration Tests](#writing-integration-tests)
3. [Testing ConfigManager](#testing-configmanager)
4. [Testing Solver Integration](#testing-solver-integration)
5. [Data Pipeline Testing](#data-pipeline-testing)
6. [Output Generation Testing](#output-generation-testing)
7. [Concurrent Operations Testing](#concurrent-operations-testing)
8. [Error Handling Testing](#error-handling-testing)
9. [Performance Testing Best Practices](#performance-testing-best-practices)
10. [Stress Testing Methodology](#stress-testing-methodology)
11. [Debugging Failed Integration Tests](#debugging-failed-integration-tests)
12. [Integration Testing Checklist](#integration-testing-checklist)

---

## Real-World Integration Patterns

### Pattern 1: ConfigManager → Solver Execution

**Real-world scenario:** User provides YAML config, system runs solver, returns results

```python
# tests/integration/test_config_to_solver.py
import pytest
from pathlib import Path
from src.config.manager import ConfigManager
from src.solvers.factory import SolverFactory

class TestConfigToSolverExecution:
    """Integration: Configuration loading → Solver execution"""

    @pytest.fixture
    def config_path(self, tmp_path):
        """Create test YAML configuration"""
        config_content = """
        solver: orcaflex
        mock_mode: true
        files:
          - tests/fixtures/data/orcaflex_sample.txt
        analysis:
          extract_time_series: true
          calculate_statistics: true
          time_window: [0, 100]
        output:
          format: csv
          path: {}/output.csv
        """.format(tmp_path)

        config_file = tmp_path / "config.yaml"
        config_file.write_text(config_content)
        return config_file

    def test_config_loads_successfully(self, config_path):
        """Verify YAML config loads without errors"""
        config = ConfigManager(str(config_path))

        assert config is not None
        assert config.solver_type == "orcaflex"
        assert len(config.files) == 1
        assert config.analysis["extract_time_series"] is True

    def test_solver_retrieved_from_config(self, config_path):
        """Verify correct solver type instantiated from config"""
        config = ConfigManager(str(config_path))
        solver = SolverFactory.create(config.solver_type)

        assert solver is not None
        assert solver.__class__.__name__ == "OrcaFlexSolver"

    def test_solver_accepts_config_parameters(self, config_path):
        """Verify solver accepts and uses config parameters"""
        config = ConfigManager(str(config_path))
        solver = SolverFactory.create(config.solver_type, mock_mode=True)

        # Pass config to solver
        results = solver.analyze(
            files=config.files,
            parameters=config.analysis
        )

        assert results is not None
        assert results.solver_type == "orcaflex"

    def test_results_match_config_output_format(self, config_path):
        """Verify results can be exported in configured format"""
        config = ConfigManager(str(config_path))
        solver = SolverFactory.create(config.solver_type, mock_mode=True)
        results = solver.analyze(files=config.files, parameters=config.analysis)

        # Export in configured format
        export_path = results.export(
            format=config.output["format"],
            path=config.output["path"]
        )

        assert export_path.exists()
        assert export_path.suffix == ".csv"
```

**Key Testing Points:**
1. Config loads and parses correctly
2. Correct solver instantiated from config
3. Solver receives config parameters
4. Results in expected format

### Pattern 2: Raw Data → Processing → Multiple Outputs

**Real-world scenario:** Load raw simulation results, process, export multiple formats

```python
# tests/integration/test_data_pipeline_outputs.py
import pytest
import pandas as pd
from pathlib import Path
from src.processing.pipeline import DataPipeline
from src.processing.validators import validate_csv, validate_html, validate_excel

class TestDataPipelineOutputs:
    """Integration: Raw data → Processing → Multiple output formats"""

    @pytest.fixture
    def raw_orcaflex_data(self):
        """Load sample raw OrcaFlex output"""
        # Format: Time, Tension, Velocity, Acceleration
        data = """
        Time,Tension,Velocity,Acceleration
        0.0,100.5,0.0,0.0
        0.1,101.2,1.5,15.0
        0.2,102.1,3.0,20.0
        0.3,103.5,4.2,18.0
        0.4,105.0,5.1,15.0
        """
        return pd.read_csv(pd.io.common.StringIO(data))

    @pytest.fixture
    def pipeline_config(self):
        """Configuration for data pipeline"""
        return {
            "statistics": ["mean", "max", "min", "std"],
            "time_window": [0, 0.4],
            "sampling_rate": 10,  # Hz
        }

    def test_pipeline_processes_data(self, raw_orcaflex_data, pipeline_config):
        """Verify pipeline processes raw data correctly"""
        pipeline = DataPipeline(pipeline_config)
        processed = pipeline.process(raw_orcaflex_data)

        assert processed is not None
        assert len(processed) == len(raw_orcaflex_data)
        assert "Tension" in processed.columns

    def test_pipeline_calculates_statistics(self, raw_orcaflex_data, pipeline_config):
        """Verify pipeline calculates configured statistics"""
        pipeline = DataPipeline(pipeline_config)
        processed = pipeline.process(raw_orcaflex_data)

        stats = pipeline.calculate_statistics(processed)

        assert stats["mean"] == pytest.approx(102.46, rel=0.01)
        assert stats["max"] == 105.0
        assert stats["min"] == 100.5
        assert stats["std"] > 0

    def test_export_to_csv(self, raw_orcaflex_data, pipeline_config, tmp_path):
        """Verify CSV export"""
        pipeline = DataPipeline(pipeline_config)
        processed = pipeline.process(raw_orcaflex_data)

        csv_file = tmp_path / "output.csv"
        pipeline.export_csv(processed, str(csv_file))

        # Validate CSV
        assert validate_csv(csv_file)
        df = pd.read_csv(csv_file)
        assert len(df) == len(raw_orcaflex_data)

    def test_export_to_excel(self, raw_orcaflex_data, pipeline_config, tmp_path):
        """Verify Excel export"""
        pipeline = DataPipeline(pipeline_config)
        processed = pipeline.process(raw_orcaflex_data)

        excel_file = tmp_path / "output.xlsx"
        pipeline.export_excel(processed, str(excel_file))

        # Validate Excel
        assert validate_excel(excel_file)
        df = pd.read_excel(excel_file)
        assert len(df) == len(raw_orcaflex_data)

    def test_export_to_html(self, raw_orcaflex_data, pipeline_config, tmp_path):
        """Verify HTML report generation"""
        pipeline = DataPipeline(pipeline_config)
        processed = pipeline.process(raw_orcaflex_data)
        stats = pipeline.calculate_statistics(processed)

        html_file = tmp_path / "report.html"
        pipeline.export_html(processed, stats, str(html_file))

        # Validate HTML
        assert validate_html(html_file)
        html_content = html_file.read_text()
        assert "<html" in html_content.lower()
        assert "plotly" in html_content.lower()  # Interactive plots
        assert str(stats["mean"]) in html_content
```

**Key Testing Points:**
1. Raw data loads and validates
2. Processing produces correct calculations
3. Each output format generates without errors
4. Output formats contain expected data

### Pattern 3: End-to-End Workflow

**Real-world scenario:** Full workflow from configuration to final report

```python
# tests/integration/test_end_to_end_workflow.py
import pytest
import os
from pathlib import Path
from src.workflow.engine import WorkflowEngine

class TestEndToEndWorkflow:
    """Integration: Complete workflow from config to report"""

    @pytest.fixture
    def workflow_config_file(self, tmp_path):
        """Create complete workflow configuration"""
        config = """
        version: "1.0"
        solver: orcaflex
        mock_mode: true

        input:
          files:
            - tests/fixtures/data/orcaflex_sample.txt
          format: orcaflex_sim

        analysis:
          extract_time_series: true
          calculate_statistics: true
          extreme_values: true

        processing:
          time_window: [0, 100]
          filtering: false
          decimation: 1

        output:
          formats: [csv, html, excel]
          path: {}/results
          report_title: "Integration Test Report"
        """
        config_file = tmp_path / "workflow.yaml"
        config_file.write_text(config.format(tmp_path=tmp_path))
        return config_file

    def test_workflow_executes_successfully(self, workflow_config_file, tmp_path):
        """Verify complete workflow executes without errors"""
        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        assert results is not None
        assert results.status == "completed"
        assert results.files_processed == 1

    def test_all_output_files_created(self, workflow_config_file, tmp_path):
        """Verify all configured output formats created"""
        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        output_dir = tmp_path / "results"
        assert (output_dir / "output.csv").exists()
        assert (output_dir / "report.html").exists()
        assert (output_dir / "output.xlsx").exists()

    def test_output_files_contain_valid_data(self, workflow_config_file, tmp_path):
        """Verify output files contain valid, expected data"""
        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        # Verify CSV
        csv_file = tmp_path / "results" / "output.csv"
        df_csv = pd.read_csv(csv_file)
        assert len(df_csv) > 0
        assert "Tension" in df_csv.columns

        # Verify Excel
        excel_file = tmp_path / "results" / "output.xlsx"
        df_excel = pd.read_excel(excel_file)
        assert len(df_excel) == len(df_csv)

        # Verify HTML
        html_file = tmp_path / "results" / "report.html"
        html_content = html_file.read_text()
        assert "Integration Test Report" in html_content
        assert "<table" in html_content.lower()

    def test_workflow_execution_time_logged(self, workflow_config_file):
        """Verify workflow logs execution time"""
        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        assert results.execution_time > 0
        assert results.execution_time < 5  # Should be quick with mock

    def test_workflow_cleanup_after_completion(self, workflow_config_file, tmp_path):
        """Verify temporary files cleaned up"""
        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        # Temp directory should be clean
        temp_files = list((tmp_path / "temp").glob("*"))
        assert len(temp_files) == 0

    def test_workflow_error_recovery(self, workflow_config_file, tmp_path):
        """Verify workflow handles errors gracefully"""
        # Modify config with missing file
        config_content = workflow_config_file.read_text()
        config_content = config_content.replace(
            "orcaflex_sample.txt",
            "nonexistent.txt"
        )
        workflow_config_file.write_text(config_content)

        engine = WorkflowEngine(str(workflow_config_file))
        results = engine.execute()

        # Should complete with warnings, not crash
        assert results.status in ["completed_with_warnings", "failed"]
        assert len(results.errors) > 0
```

**Key Testing Points:**
1. Workflow completes successfully
2. All output files created
3. Output files contain valid data
4. Error handling works

---

## Writing Integration Tests

### Test Structure Template

```python
# tests/integration/test_[module]_integration.py
"""
Integration tests for [module]

Tests the interaction between [component1] and [component2]
under realistic conditions.
"""

import pytest
from pathlib import Path
import tempfile
import logging

logger = logging.getLogger(__name__)


class Test[ModuleIntegration]:
    """Integration tests for [module]"""

    @pytest.fixture(scope="class")
    def shared_fixture(self):
        """Class-scoped fixture for expensive setup"""
        # Setup
        resource = expensive_setup()
        yield resource
        # Teardown
        resource.cleanup()

    @pytest.fixture
    def setup(self, tmp_path):
        """Test-scoped setup/teardown"""
        logger.info(f"Setting up test in {tmp_path}")
        yield tmp_path
        logger.info("Test cleanup completed")

    # ===== HAPPY PATH TESTS =====

    def test_basic_operation(self, setup):
        """Verify basic operation completes successfully"""
        # Arrange
        config = create_test_config()

        # Act
        result = execute_operation(config)

        # Assert
        assert result is not None
        assert result.status == "success"

    def test_operation_with_parameters(self, setup):
        """Verify operation accepts and uses parameters"""
        # Arrange
        config = create_test_config(param1="value1", param2="value2")

        # Act
        result = execute_operation(config)

        # Assert
        assert result.param1 == "value1"
        assert result.param2 == "value2"

    # ===== DATA FLOW TESTS =====

    def test_data_transformation_accuracy(self, setup):
        """Verify data transformations are accurate"""
        # Arrange
        input_data = create_test_data()
        expected_output = compute_expected_output(input_data)

        # Act
        result = transform_data(input_data)

        # Assert
        assert result == expected_output
        assert_data_integrity(result)

    # ===== ERROR HANDLING TESTS =====

    def test_handles_missing_input(self, setup):
        """Verify graceful handling of missing input"""
        # Arrange
        config = create_test_config(files=[])

        # Act
        result = execute_operation(config)

        # Assert
        assert result.status == "error"
        assert "missing" in result.error_message.lower()

    def test_handles_invalid_parameter(self, setup):
        """Verify error handling for invalid parameters"""
        # Arrange
        config = create_test_config(invalid_param="bad_value")

        # Act & Assert
        with pytest.raises(ValueError):
            execute_operation(config)

    # ===== OUTPUT VERIFICATION TESTS =====

    def test_output_format_valid(self, setup):
        """Verify output in correct format"""
        # Arrange
        config = create_test_config()

        # Act
        result = execute_operation(config)

        # Assert
        assert is_valid_output_format(result)
        assert result.has_required_fields()

    def test_output_file_created(self, setup):
        """Verify output files created successfully"""
        # Arrange
        output_file = setup / "output.csv"
        config = create_test_config(output_path=str(output_file))

        # Act
        execute_operation(config)

        # Assert
        assert output_file.exists()
        assert output_file.stat().st_size > 0
```

### Test Naming Convention

```python
# test_[component1]_with_[component2]_[scenario].py
test_config_manager_with_orcaflex_solver_valid_config()
test_config_manager_with_orcaflex_solver_missing_file()
test_data_pipeline_with_concurrent_workers_thread_safety()
test_output_generation_with_multiple_formats_file_creation()

# Clarity through naming:
# 1. What components interact (config_manager_with_orcaflex_solver)
# 2. What scenario is tested (valid_config)
```

---

## Testing ConfigManager

### ConfigManager Integration Tests

```python
# tests/integration/test_config_manager_integration.py
import pytest
import yaml
from pathlib import Path
from src.config.manager import ConfigManager

class TestConfigManagerIntegration:
    """Integration tests for ConfigManager"""

    @pytest.fixture
    def temp_config_dir(self, tmp_path):
        """Create temporary directory with test configurations"""
        # Create various test configs
        configs = {
            "valid_orcaflex.yaml": """
                solver: orcaflex
                files:
                  - sample1.txt
                  - sample2.txt
                analysis:
                  extract_time_series: true
            """,
            "valid_aqwa.yaml": """
                solver: aqwa
                files:
                  - sample.aqwa
                analysis:
                  calculate_rao: true
            """,
            "invalid.yaml": """
                solver: unknown_type
                files: []
            """,
        }

        for name, content in configs.items():
            config_file = tmp_path / name
            config_file.write_text(content)

        return tmp_path

    def test_load_valid_orcaflex_config(self, temp_config_dir):
        """Load and parse valid OrcaFlex config"""
        config_file = temp_config_dir / "valid_orcaflex.yaml"
        config = ConfigManager(str(config_file))

        assert config.solver_type == "orcaflex"
        assert len(config.files) == 2
        assert config.analysis["extract_time_series"] is True

    def test_load_valid_aqwa_config(self, temp_config_dir):
        """Load and parse valid AQWA config"""
        config_file = temp_config_dir / "valid_aqwa.yaml"
        config = ConfigManager(str(config_file))

        assert config.solver_type == "aqwa"
        assert len(config.files) == 1
        assert config.analysis["calculate_rao"] is True

    def test_validate_against_schema(self, temp_config_dir):
        """Verify config validation against schema"""
        config_file = temp_config_dir / "valid_orcaflex.yaml"
        config = ConfigManager(str(config_file))

        # Should validate successfully
        assert config.validate() is True

    def test_reject_invalid_solver_type(self, temp_config_dir):
        """Reject configuration with unknown solver type"""
        config_file = temp_config_dir / "invalid.yaml"

        with pytest.raises(ValueError) as exc_info:
            ConfigManager(str(config_file))

        assert "unknown_type" in str(exc_info.value).lower()

    def test_file_existence_checking(self, temp_config_dir):
        """Verify config checks for file existence"""
        config_file = temp_config_dir / "valid_orcaflex.yaml"

        # Config references nonexistent files
        with pytest.warns(UserWarning):
            config = ConfigManager(str(config_file))
            config.validate(check_file_existence=True)

    def test_config_parameter_interpolation(self, tmp_path):
        """Verify config supports parameter interpolation"""
        config_content = """
        base_dir: {base_dir}
        solver: orcaflex
        files:
          - ${{base_dir}}/sample1.txt
          - ${{base_dir}}/sample2.txt
        """

        config_file = tmp_path / "config.yaml"
        config_file.write_text(
            config_content.format(base_dir=tmp_path)
        )

        config = ConfigManager(str(config_file))

        # Files should be interpolated
        assert str(tmp_path) in str(config.files[0])

    def test_config_merging(self, tmp_path):
        """Verify ability to merge configs"""
        base_config = tmp_path / "base.yaml"
        base_config.write_text("""
            solver: orcaflex
            mock_mode: true
        """)

        override_config = tmp_path / "override.yaml"
        override_config.write_text("""
            files:
              - sample1.txt
        """)

        # Merge configs
        config = ConfigManager.merge(
            str(base_config),
            str(override_config)
        )

        assert config.solver_type == "orcaflex"
        assert config.mock_mode is True
        assert len(config.files) == 1
```

---

## Testing Solver Integration

### Solver Execution Integration Tests

```python
# tests/integration/test_solver_execution_integration.py
import pytest
from src.solvers.factory import SolverFactory
from src.config.manager import ConfigManager

class TestSolverExecutionIntegration:
    """Integration tests for solver execution"""

    @pytest.fixture
    def orcaflex_config(self, tmp_path):
        """Create OrcaFlex configuration"""
        config_file = tmp_path / "orcaflex.yaml"
        config_file.write_text("""
            solver: orcaflex
            mock_mode: true
            files:
              - tests/fixtures/data/orcaflex_sample.txt
            analysis:
              extract_time_series: true
              calculate_statistics: true
        """)
        return ConfigManager(str(config_file))

    def test_solver_factory_creates_correct_solver(self, orcaflex_config):
        """Verify factory creates correct solver type"""
        solver = SolverFactory.create(
            orcaflex_config.solver_type,
            mock_mode=True
        )

        assert solver is not None
        assert solver.__class__.__name__ == "OrcaFlexSolver"

    def test_solver_accepts_config_parameters(self, orcaflex_config):
        """Verify solver receives config parameters correctly"""
        solver = SolverFactory.create(
            orcaflex_config.solver_type,
            mock_mode=True
        )

        results = solver.analyze(
            files=orcaflex_config.files,
            parameters=orcaflex_config.analysis
        )

        assert results is not None
        assert results.time_series is not None

    def test_solver_error_handling(self, tmp_path):
        """Verify solver handles errors gracefully"""
        solver = SolverFactory.create("orcaflex", mock_mode=True)

        # Test with nonexistent file
        results = solver.analyze(
            files=["nonexistent.txt"],
            parameters={}
        )

        assert results.status == "error"
        assert results.errors is not None

    def test_multiple_solvers_independence(self, tmp_path):
        """Verify multiple solvers don't interfere"""
        solver1 = SolverFactory.create("orcaflex", mock_mode=True)
        solver2 = SolverFactory.create("aqwa", mock_mode=True)

        # Both should exist independently
        assert solver1 is not None
        assert solver2 is not None
        assert solver1.__class__ != solver2.__class__
```

---

## Data Pipeline Testing

### Pipeline Integration Tests

```python
# tests/integration/test_data_pipeline_integration.py
import pytest
import pandas as pd
from src.processing.pipeline import DataPipeline

class TestDataPipelineIntegration:
    """Integration tests for data processing pipeline"""

    @pytest.fixture
    def sample_data(self):
        """Create sample time series data"""
        return pd.DataFrame({
            "Time": [0.0, 0.1, 0.2, 0.3, 0.4],
            "Tension": [100, 101, 102, 103, 105],
            "Velocity": [0, 1.5, 3.0, 4.2, 5.1],
            "Acceleration": [0, 15, 20, 18, 15]
        })

    def test_pipeline_preserves_data_integrity(self, sample_data):
        """Verify pipeline doesn't lose or corrupt data"""
        pipeline = DataPipeline({})
        processed = pipeline.process(sample_data)

        assert len(processed) == len(sample_data)
        assert list(processed.columns) == list(sample_data.columns)

    def test_pipeline_calculations_accuracy(self, sample_data):
        """Verify statistical calculations are accurate"""
        pipeline = DataPipeline({})
        stats = pipeline.calculate_statistics(sample_data["Tension"])

        assert stats["mean"] == pytest.approx(102.2)
        assert stats["max"] == 105
        assert stats["min"] == 100
        assert stats["std"] > 0

    def test_pipeline_filtering(self, sample_data):
        """Verify data filtering functionality"""
        pipeline = DataPipeline({"time_window": [0.1, 0.3]})
        processed = pipeline.process(sample_data)

        # Should filter to time window
        assert processed["Time"].min() >= 0.1
        assert processed["Time"].max() <= 0.3

    def test_pipeline_aggregation(self, sample_data):
        """Verify data aggregation across multiple inputs"""
        pipeline = DataPipeline({})

        # Process multiple datasets
        results = []
        for _ in range(3):
            results.append(pipeline.process(sample_data.copy()))

        # Aggregate
        aggregated = pipeline.aggregate(results)

        assert aggregated is not None
        assert len(aggregated) > 0
```

---

## Output Generation Testing

### Output Format Validation

```python
# tests/integration/test_output_generation_integration.py
import pytest
import os
from pathlib import Path
from bs4 import BeautifulSoup
import openpyxl
from src.output.generator import OutputGenerator

class TestOutputGenerationIntegration:
    """Integration tests for output generation"""

    @pytest.fixture
    def sample_results(self):
        """Create sample analysis results"""
        return {
            "data": [
                {"time": 0.0, "tension": 100},
                {"time": 0.1, "tension": 101},
            ],
            "statistics": {
                "mean": 100.5,
                "max": 101,
                "min": 100,
            }
        }

    def test_generate_html_report(self, sample_results, tmp_path):
        """Verify HTML report generation"""
        output_file = tmp_path / "report.html"
        generator = OutputGenerator(sample_results)
        generator.export_html(str(output_file))

        assert output_file.exists()
        html_content = output_file.read_text()

        # Verify HTML structure
        soup = BeautifulSoup(html_content, "html.parser")
        assert soup.find("html") is not None
        assert soup.find("table") is not None

        # Verify interactive plots
        assert "plotly" in html_content.lower()

    def test_generate_csv_report(self, sample_results, tmp_path):
        """Verify CSV export"""
        output_file = tmp_path / "results.csv"
        generator = OutputGenerator(sample_results)
        generator.export_csv(str(output_file))

        assert output_file.exists()

        # Verify CSV content
        import csv
        with open(output_file) as f:
            reader = csv.DictReader(f)
            rows = list(reader)
            assert len(rows) == 2

    def test_generate_excel_report(self, sample_results, tmp_path):
        """Verify Excel export"""
        output_file = tmp_path / "results.xlsx"
        generator = OutputGenerator(sample_results)
        generator.export_excel(str(output_file))

        assert output_file.exists()

        # Verify Excel content
        workbook = openpyxl.load_workbook(output_file)
        assert "Data" in workbook.sheetnames
        assert "Statistics" in workbook.sheetnames

    def test_multiple_output_formats_consistency(self, sample_results, tmp_path):
        """Verify data consistency across output formats"""
        generator = OutputGenerator(sample_results)

        # Generate all formats
        csv_file = tmp_path / "results.csv"
        excel_file = tmp_path / "results.xlsx"
        html_file = tmp_path / "report.html"

        generator.export_csv(str(csv_file))
        generator.export_excel(str(excel_file))
        generator.export_html(str(html_file))

        # Verify all files created
        assert csv_file.exists()
        assert excel_file.exists()
        assert html_file.exists()

        # Verify data consistency
        import pandas as pd
        df_csv = pd.read_csv(csv_file)
        df_excel = pd.read_excel(excel_file)

        assert len(df_csv) == len(df_excel)
        assert list(df_csv.columns) == list(df_excel.columns)
```

---

## Concurrent Operations Testing

### Thread-Safe Execution Tests

```python
# tests/integration/test_concurrent_integration.py
import pytest
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
from src.workflow.engine import WorkflowEngine
from src.config.manager import ConfigManager

class TestConcurrentOperations:
    """Integration tests for concurrent operations"""

    def test_concurrent_analyses_thread_safe(self, tmp_path):
        """Verify thread-safe concurrent analysis execution"""
        from concurrent.futures import ThreadPoolExecutor

        # Create multiple test configurations
        configs = []
        for i in range(5):
            config_file = tmp_path / f"config_{i}.yaml"
            config_file.write_text(f"""
                solver: orcaflex
                mock_mode: true
                analysis_id: {i}
                files:
                  - tests/fixtures/data/orcaflex_sample.txt
            """)
            configs.append(str(config_file))

        # Execute analyses concurrently
        results = []
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(self._run_analysis, config)
                for config in configs
            ]
            results = [f.result() for f in futures]

        # Verify all completed
        assert len(results) == 5
        assert all(r is not None for r in results)

        # Verify no interference between runs
        for i, result in enumerate(results):
            assert result["analysis_id"] == i

    @staticmethod
    def _run_analysis(config_path):
        """Helper to run single analysis"""
        config = ConfigManager(config_path)
        engine = WorkflowEngine(config_path)
        return engine.execute().to_dict()

    def test_concurrent_file_processing(self, tmp_path):
        """Verify thread-safe file I/O"""
        from concurrent.futures import ThreadPoolExecutor

        # Create test files
        test_files = []
        for i in range(10):
            test_file = tmp_path / f"test_{i}.txt"
            test_file.write_text(f"Test data {i}")
            test_files.append(str(test_file))

        # Process files concurrently
        results = []
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(self._process_file, f)
                for f in test_files
            ]
            results = [f.result() for f in futures]

        # Verify all processed
        assert len(results) == 10
        assert all(len(r) > 0 for r in results)

    @staticmethod
    def _process_file(file_path):
        """Helper to process single file"""
        with open(file_path) as f:
            return f.read()

    def test_no_race_conditions_in_resource_access(self, tmp_path):
        """Verify no race conditions with shared resources"""
        from concurrent.futures import ThreadPoolExecutor
        import threading

        shared_resource = {"counter": 0}
        lock = threading.Lock()

        def increment():
            for _ in range(100):
                with lock:
                    shared_resource["counter"] += 1

        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = [executor.submit(increment) for _ in range(10)]
            for f in futures:
                f.result()

        # Should have exactly 1000 increments
        assert shared_resource["counter"] == 1000

    def test_concurrent_memory_usage(self, tmp_path):
        """Verify memory usage stays within limits with concurrent ops"""
        import psutil
        import os
        from concurrent.futures import ThreadPoolExecutor

        def memory_intensive_operation():
            # Allocate some memory
            data = [i for i in range(100000)]
            return len(data)

        process = psutil.Process(os.getpid())
        start_memory = process.memory_info().rss

        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(memory_intensive_operation)
                for _ in range(10)
            ]
            results = [f.result() for f in futures]

        end_memory = process.memory_info().rss
        memory_delta = (end_memory - start_memory) / 1_000_000  # MB

        # Memory increase should be reasonable
        assert memory_delta < 500  # Less than 500 MB increase
```

---

## Error Handling Testing

### Error Scenario Tests

```python
# tests/integration/test_error_handling_integration.py
import pytest
from src.config.manager import ConfigManager
from src.workflow.engine import WorkflowEngine

class TestErrorHandlingIntegration:
    """Integration tests for error handling"""

    def test_missing_config_file(self):
        """Verify error when config file doesn't exist"""
        with pytest.raises(FileNotFoundError):
            ConfigManager("nonexistent.yaml")

    def test_invalid_yaml_syntax(self, tmp_path):
        """Verify error when YAML syntax invalid"""
        config_file = tmp_path / "invalid.yaml"
        config_file.write_text("""
            invalid: yaml: syntax: here
        """)

        with pytest.raises(Exception):  # YAML parse error
            ConfigManager(str(config_file))

    def test_missing_required_fields(self, tmp_path):
        """Verify error when required fields missing"""
        config_file = tmp_path / "incomplete.yaml"
        config_file.write_text("""
            files: []
            # Missing 'solver' field
        """)

        with pytest.raises(ValueError):
            ConfigManager(str(config_file))

    def test_invalid_solver_type(self, tmp_path):
        """Verify error with unknown solver type"""
        config_file = tmp_path / "invalid_solver.yaml"
        config_file.write_text("""
            solver: unknown_solver
            files: []
        """)

        with pytest.raises(ValueError):
            ConfigManager(str(config_file))

    def test_partial_failure_handling(self, tmp_path):
        """Verify graceful handling of partial failures"""
        config_file = tmp_path / "partial.yaml"
        config_file.write_text("""
            solver: orcaflex
            mock_mode: true
            files:
              - nonexistent_file1.txt
              - tests/fixtures/data/orcaflex_sample.txt
              - nonexistent_file2.txt
        """)

        config = ConfigManager(str(config_file))
        engine = WorkflowEngine(str(config_file))
        results = engine.execute()

        # Should complete with warnings, process what's available
        assert results.status in ["completed_with_warnings", "completed"]
        assert results.files_processed >= 1
        assert len(results.warnings) > 0

    def test_error_message_clarity(self, tmp_path):
        """Verify error messages are clear and actionable"""
        config_file = tmp_path / "error.yaml"
        config_file.write_text("""
            solver: orcaflex
            files:
              - nonexistent.txt
        """)

        try:
            engine = WorkflowEngine(str(config_file))
            engine.execute()
        except Exception as e:
            error_msg = str(e).lower()
            # Error message should mention what's wrong
            assert "file" in error_msg or "missing" in error_msg

    def test_timeout_handling(self):
        """Verify proper timeout handling"""
        # This would require a long-running operation
        # For now, test that timeout mechanism exists
        from src.workflow.engine import WorkflowEngine
        assert hasattr(WorkflowEngine, "timeout")
```

---

## Performance Testing Best Practices

### Performance Test Template

```python
# tests/performance/test_performance_template.py
import pytest
import time
import psutil
import os
from src.workflow.engine import WorkflowEngine

class TestPerformanceTemplate:
    """Template for performance testing"""

    @pytest.fixture
    def benchmark_fixture(self):
        """Fixture for performance metrics"""
        class BenchmarkMetrics:
            def __init__(self):
                self.start_time = None
                self.start_memory = None
                self.metrics = {}

            def start(self):
                self.start_time = time.time()
                process = psutil.Process(os.getpid())
                self.start_memory = process.memory_info().rss

            def stop(self, test_name):
                elapsed = time.time() - self.start_time
                process = psutil.Process(os.getpid())
                end_memory = process.memory_info().rss
                memory_delta = (end_memory - self.start_memory) / 1_000_000

                self.metrics[test_name] = {
                    "elapsed_seconds": elapsed,
                    "memory_delta_mb": memory_delta,
                }
                return self.metrics[test_name]

        return BenchmarkMetrics()

    def test_performance_100_files(self, benchmark_fixture):
        """Test performance with 100 files"""
        benchmark_fixture.start()

        # Run operation on 100 files
        config = self._create_config(num_files=100)
        engine = WorkflowEngine(config)
        engine.execute()

        metrics = benchmark_fixture.stop("100_files")

        # Assertions
        assert metrics["elapsed_seconds"] < 2.0  # Should complete in <2 sec
        assert metrics["memory_delta_mb"] < 50  # <50 MB increase

    def test_performance_1000_files(self, benchmark_fixture):
        """Test performance with 1000 files"""
        benchmark_fixture.start()

        config = self._create_config(num_files=1000)
        engine = WorkflowEngine(config)
        engine.execute()

        metrics = benchmark_fixture.stop("1000_files")

        # Should handle 1000 files efficiently
        assert metrics["elapsed_seconds"] < 30  # <30 sec
        assert metrics["memory_delta_mb"] < 500  # <500 MB

    @staticmethod
    def _create_config(num_files):
        """Helper to create performance test config"""
        # Implementation depends on your test setup
        pass
```

---

## Stress Testing Methodology

### Stress Test Scenarios

```python
# tests/stress/test_stress_integration.py
import pytest
from concurrent.futures import ThreadPoolExecutor
import time

class TestStressIntegration:
    """Stress tests for system stability"""

    def test_stress_10000_files(self, tmp_path):
        """Stress test with 10,000 file batch"""
        # Create configuration for 10,000 files
        config = self._create_stress_config(file_count=10000)

        # Execute
        start_time = time.time()
        engine = WorkflowEngine(config)
        results = engine.execute()
        elapsed = time.time() - start_time

        # Verify completion and stability
        assert results.files_processed == 10000
        assert elapsed < 300  # Should complete within 5 minutes
        assert results.status == "completed"
        assert results.errors is None or len(results.errors) == 0

    def test_stress_concurrent_workflows(self, tmp_path):
        """Stress test with concurrent workflow execution"""
        configs = [
            self._create_stress_config(file_count=100)
            for _ in range(20)
        ]

        # Execute 20 workflows concurrently
        with ThreadPoolExecutor(max_workers=20) as executor:
            futures = [
                executor.submit(self._run_workflow, config)
                for config in configs
            ]
            results = [f.result() for f in futures]

        # All should complete
        assert len(results) == 20
        assert all(r.status == "completed" for r in results)

    def test_stress_sustained_load(self):
        """Stress test with sustained load over time"""
        # Run continuous operations for a duration
        duration_seconds = 60
        start_time = time.time()
        completed_ops = 0

        while time.time() - start_time < duration_seconds:
            config = self._create_stress_config(file_count=50)
            engine = WorkflowEngine(config)
            results = engine.execute()

            assert results.status == "completed"
            completed_ops += 1

        # Should handle sustained load
        assert completed_ops > 5  # At least 5 operations in 60 seconds

    @staticmethod
    def _create_stress_config(file_count):
        """Create configuration with specified file count"""
        pass

    @staticmethod
    def _run_workflow(config):
        """Helper to run workflow"""
        engine = WorkflowEngine(config)
        return engine.execute()
```

---

## Debugging Failed Integration Tests

### Debugging Strategies

**1. Enable Verbose Logging**
```bash
pytest -vv tests/integration -o log_cli=true --log-cli-level=DEBUG
```

**2. Capture Output**
```bash
pytest tests/integration -s  # Show print statements
pytest tests/integration --tb=long  # Full traceback
```

**3. Use Breakpoints**
```python
def test_failing_test():
    config = create_config()
    breakpoint()  # Debug here
    results = execute_config(config)
    assert results is not None
```

**4. Isolate the Issue**
```python
# Test individual components separately
def test_component_a():
    result_a = component_a()
    assert result_a is not None

def test_component_b():
    result_b = component_b()
    assert result_b is not None

def test_integration_a_b():
    result_a = component_a()
    result_b = component_b(result_a)
    assert result_b is not None
```

### Common Issues and Solutions

| Issue | Cause | Solution |
|-------|-------|----------|
| Flaky tests | Race conditions, timing issues | Add explicit waits, use locks |
| Temp file cleanup | Files not deleted | Use `with tempfile.TemporaryDirectory()` |
| Resource leaks | Files/connections not closed | Add fixture cleanup |
| Timeout failures | Operations too slow in CI | Increase timeout for CI, optimize code |
| Mock confusion | Wrong mock used | Verify mock is applied to correct import path |

---

## Integration Testing Checklist

Use this checklist when implementing integration tests:

### Before Writing Tests
- [ ] Understand the components being tested
- [ ] Identify integration points
- [ ] Determine data flow paths
- [ ] Plan test scenarios (happy path, error cases)

### While Writing Tests
- [ ] Use clear, descriptive test names
- [ ] Separate Arrange-Act-Assert sections
- [ ] Create reusable fixtures
- [ ] Test both success and failure cases
- [ ] Include docstrings explaining what's tested

### Test Structure
- [ ] Each test is independent
- [ ] Tests use proper setup/teardown
- [ ] Fixtures are appropriately scoped
- [ ] Temp files/directories are cleaned up
- [ ] Mock data is realistic

### Coverage
- [ ] Happy path tested
- [ ] Error cases tested
- [ ] Edge cases considered
- [ ] Concurrent access tested
- [ ] Resource cleanup verified

### Performance
- [ ] Performance targets identified
- [ ] Benchmarks established
- [ ] Stress tests implemented
- [ ] Memory usage monitored
- [ ] Timeout handling verified

### Documentation
- [ ] Tests have clear docstrings
- [ ] Test purposes documented
- [ ] Failure modes documented
- [ ] How to run tests documented
- [ ] How to add new tests documented

---

**End of Integration Testing Guide**
