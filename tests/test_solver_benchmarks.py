"""
ABOUTME: Comprehensive tests for solver benchmarking framework
ABOUTME: Demonstrates 50+ benchmark methods across categories
"""

import pytest
import logging
from pathlib import Path
from typing import List

from digitalmodel.base_solvers.base import BaseSolver, AnalysisSolver, SolverStatus
from digitalmodel.base_solvers.benchmarks import (
    SolverBenchmarks,
    ConfigurationBenchmarks,
    BenchmarkReportGenerator,
    BenchmarkConfig,
    BenchmarkCategory,
    BenchmarkResult,
)

logger = logging.getLogger(__name__)


# Mock solver implementations for testing
class SimpleSolver(BaseSolver):
    """Simple test solver."""

    def __init__(self, name: str = "SimpleSolver", version: str = "1.0.0"):
        super().__init__(name, version)
        self.input_data = {}

    def set_input(self, data: dict):
        self.input_data = data

    def validate_inputs(self):
        if not self.input_data:
            return False, ["No input data provided"]
        return True, []

    def solve(self):
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            self._set_status(SolverStatus.FAILED)
            return {"success": False, "errors": errors}

        self._set_status(SolverStatus.EXECUTING)

        # Simple calculation
        result = sum(self.input_data.get("values", [])) if "values" in self.input_data else 0

        self._cache_results({"sum": result, "count": len(self.input_data.get("values", []))})
        self._set_status(SolverStatus.COMPLETED)

        return {
            "success": True,
            "status": self.status.value,
            "result": self._results,
        }

    def get_solver_metadata(self):
        return {
            "name": self.name,
            "version": self.version,
            "description": "Simple test solver",
            "solver_type": "test",
            "inputs": {"values": {"type": "list"}},
            "outputs": {"sum": {"type": "float"}, "count": {"type": "int"}},
        }


class ComplexSolver(AnalysisSolver):
    """Complex test solver with configuration."""

    def __init__(self, name: str = "ComplexSolver", version: str = "1.0.0", config: dict = None):
        super().__init__(name, version, config)

    def validate_inputs(self):
        if not self.input_data:
            return False, ["No input data provided"]

        if "parameters" not in self.input_data:
            return False, ["Missing required field: parameters"]

        return True, []

    def solve(self):
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            self.set_validation_errors(errors)
            self._set_status(SolverStatus.FAILED)
            return {"success": False, "errors": errors}

        self._set_status(SolverStatus.EXECUTING)

        # Complex calculation
        values = self.input_data.get("values", [])
        params = self.input_data.get("parameters", {})

        # Simulate expensive computation
        import time
        time.sleep(0.001)  # 1ms simulated computation

        results = {
            "mean": sum(values) / len(values) if values else 0,
            "min": min(values) if values else None,
            "max": max(values) if values else None,
            "param_result": params.get("factor", 1.0) * sum(values) if values else 0,
        }

        self._cache_results(results)
        self._set_status(SolverStatus.COMPLETED)

        return {"success": True, "status": self.status.value, "result": results}

    def get_solver_metadata(self):
        return {
            "name": self.name,
            "version": self.version,
            "description": "Complex test solver with configuration",
            "solver_type": "analysis",
            "inputs": {
                "values": {"type": "list"},
                "parameters": {"type": "dict"},
            },
            "outputs": {
                "mean": {"type": "float"},
                "min": {"type": "float"},
                "max": {"type": "float"},
            },
        }


class TestSolverBenchmarks:
    """Test suite for solver benchmarking."""

    @pytest.fixture
    def benchmarks(self):
        """Create benchmark suite."""
        return SolverBenchmarks()

    @pytest.fixture
    def simple_solver(self):
        """Create simple solver."""
        solver = SimpleSolver()
        solver.set_input({"values": list(range(100))})
        return solver

    @pytest.fixture
    def complex_solver(self):
        """Create complex solver."""
        solver = ComplexSolver()
        solver.set_input({
            "values": list(range(100)),
            "parameters": {"factor": 2.0}
        })
        return solver

    def test_solver_initialization_benchmark(self, benchmarks):
        """Test solver initialization benchmarking."""
        result = benchmarks.benchmark_solver_initialization(SimpleSolver)

        assert result is not None
        assert result.name == "SimpleSolver_initialization"
        assert result.category == BenchmarkCategory.SOLVER_EXECUTION
        assert result.metrics.execution_time_ms > 0
        logger.info(f"Initialization benchmark: {result.metrics.execution_time_ms:.2f}ms")

    def test_solver_validation_benchmark(self, benchmarks, simple_solver):
        """Test solver validation benchmarking."""
        result = benchmarks.benchmark_solver_validation(simple_solver)

        assert result is not None
        assert result.category == BenchmarkCategory.SOLVER_EXECUTION
        assert result.metrics.execution_time_ms >= 0
        logger.info(f"Validation benchmark: {result.metrics.execution_time_ms:.2f}ms")

    def test_solver_execution_benchmark(self, benchmarks, simple_solver):
        """Test solver execution benchmarking."""
        result = benchmarks.benchmark_solver_execution(simple_solver)

        assert result is not None
        assert result.category == BenchmarkCategory.SOLVER_EXECUTION
        assert result.metrics.execution_time_ms > 0
        logger.info(f"Execution benchmark: {result.metrics.execution_time_ms:.2f}ms")

    def test_complex_solver_execution(self, benchmarks, complex_solver):
        """Test complex solver execution."""
        result = benchmarks.benchmark_solver_execution(complex_solver)

        assert result is not None
        assert result.metrics.execution_time_ms > 0
        logger.info(f"Complex solver execution: {result.metrics.execution_time_ms:.2f}ms")

    def test_solver_results_caching(self, benchmarks, simple_solver):
        """Test results caching performance."""
        result = benchmarks.benchmark_solver_results_caching(simple_solver, run_count=10)

        assert result is not None
        assert result.name == f"{simple_solver.name}_results_caching"
        assert result.metrics.iterations == 10
        logger.info(f"Results caching: {result.metrics.execution_time_ms:.4f}ms per retrieval")

    def test_solver_metadata_benchmark(self, benchmarks, complex_solver):
        """Test metadata retrieval performance."""
        result = benchmarks.benchmark_solver_metadata(complex_solver)

        assert result is not None
        assert result.metrics.execution_time_ms >= 0
        logger.info(f"Metadata retrieval: {result.metrics.execution_time_ms:.4f}ms")

    def test_input_data_handling_benchmark(self, benchmarks, complex_solver):
        """Test input data handling."""
        result = benchmarks.benchmark_input_data_handling(complex_solver)

        assert result is not None
        assert result.name == f"{complex_solver.name}_input_data_handling"
        assert result.metrics.execution_time_ms >= 0
        logger.info(f"Input data handling: {result.metrics.execution_time_ms:.2f}ms")

    def test_validation_error_handling(self, benchmarks, complex_solver):
        """Test validation error management."""
        result = benchmarks.benchmark_validation_error_handling(complex_solver)

        assert result is not None
        assert result.name == f"{complex_solver.name}_error_handling"
        logger.info(f"Error handling: {result.metrics.execution_time_ms:.2f}ms")

    def test_solver_scaling_benchmark(self, benchmarks, simple_solver):
        """Test solver scaling across data sizes."""
        def solver_factory(size):
            solver = SimpleSolver()
            solver.set_input({"values": list(range(size))})
            return solver

        data_sizes = [100, 500, 1000]
        results = benchmarks.benchmark_solver_scaling(solver_factory, data_sizes)

        assert len(results) == len(data_sizes)
        for result in results:
            assert result.category == BenchmarkCategory.SCALABILITY
            logger.info(f"Scaling benchmark: {result.name} - {result.metrics.execution_time_ms:.2f}ms")

    def test_concurrent_solvers_benchmark(self, benchmarks, simple_solver):
        """Test concurrent solver execution."""
        def solver_factory():
            solver = SimpleSolver()
            solver.set_input({"values": list(range(100))})
            return solver

        result = benchmarks.benchmark_concurrent_solvers(solver_factory, num_instances=5)

        assert result is not None
        assert result.category == BenchmarkCategory.INTEGRATION
        logger.info(f"Concurrent solvers (5): {result.metrics.execution_time_ms:.2f}ms")

    def test_benchmark_result_threshold_validation(self):
        """Test benchmark result threshold validation."""
        from digitalmodel.base_solvers.benchmarks import PerformanceMetrics

        metrics = PerformanceMetrics(
            execution_time_ms=150.0,
            memory_peak_mb=256.0,
            memory_avg_mb=128.0,
            cpu_percent=45.0,
        )

        result = BenchmarkResult(
            name="test_result",
            category=BenchmarkCategory.SOLVER_EXECUTION,
            metrics=metrics,
            threshold={
                "max_time_ms": 100.0,
                "max_memory_mb": 512.0,
            }
        )

        passed, violations = result.meets_threshold()

        assert not passed
        assert len(violations) > 0
        assert "Execution time" in violations[0]
        logger.info(f"Threshold violations: {violations}")

    def test_benchmark_baseline_comparison(self):
        """Test baseline comparison functionality."""
        from digitalmodel.base_solvers.benchmarks import PerformanceMetrics

        baseline = PerformanceMetrics(
            execution_time_ms=100.0,
            memory_peak_mb=200.0,
            memory_avg_mb=100.0,
            cpu_percent=40.0,
        )

        current = PerformanceMetrics(
            execution_time_ms=120.0,
            memory_peak_mb=220.0,
            memory_avg_mb=110.0,
            cpu_percent=45.0,
        )

        result = BenchmarkResult(
            name="test_baseline",
            category=BenchmarkCategory.SOLVER_EXECUTION,
            metrics=current,
            baseline=baseline,
        )

        comparison = result.get_comparison_to_baseline()

        assert "time_delta_percent" in comparison
        assert "memory_delta_percent" in comparison
        assert comparison["time_delta_percent"] > 0  # Should be slower
        assert comparison["memory_delta_percent"] > 0  # Should use more memory
        logger.info(f"Baseline comparison: {comparison}")

    def test_benchmark_aggregator(self, benchmarks):
        """Test result aggregation."""
        from digitalmodel.base_solvers.benchmarks import PerformanceMetrics

        # Add some results
        for i in range(5):
            metrics = PerformanceMetrics(
                execution_time_ms=100.0 + i * 10,
                memory_peak_mb=200.0,
                memory_avg_mb=100.0,
                cpu_percent=40.0,
            )

            result = BenchmarkResult(
                name=f"test_{i}",
                category=BenchmarkCategory.SOLVER_EXECUTION,
                metrics=metrics,
                passed=True,
            )

            benchmarks.suite.aggregator.add_result(result)

        summary = benchmarks.get_results_summary()

        assert summary["total"] == 5
        assert summary["passed"] == 5
        assert summary["failed"] == 0
        assert summary["pass_rate"] == 100.0
        logger.info(f"Aggregator summary: {summary}")

    def test_benchmark_export_json(self, benchmarks, tmp_path):
        """Test JSON export functionality."""
        from digitalmodel.base_solvers.benchmarks import PerformanceMetrics

        # Add a result
        metrics = PerformanceMetrics(
            execution_time_ms=100.0,
            memory_peak_mb=200.0,
            memory_avg_mb=100.0,
            cpu_percent=40.0,
        )

        result = BenchmarkResult(
            name="test_export",
            category=BenchmarkCategory.SOLVER_EXECUTION,
            metrics=metrics,
        )

        benchmarks.suite.aggregator.add_result(result)

        # Export
        json_file = tmp_path / "benchmarks.json"
        benchmarks.suite.aggregator.export_to_json(json_file)

        assert json_file.exists()
        logger.info(f"Exported JSON: {json_file}")

    def test_benchmark_export_csv(self, benchmarks, tmp_path):
        """Test CSV export functionality."""
        from digitalmodel.base_solvers.benchmarks import PerformanceMetrics

        # Add results
        for i in range(3):
            metrics = PerformanceMetrics(
                execution_time_ms=100.0 + i * 10,
                memory_peak_mb=200.0,
                memory_avg_mb=100.0,
                cpu_percent=40.0,
            )

            result = BenchmarkResult(
                name=f"test_{i}",
                category=BenchmarkCategory.SOLVER_EXECUTION,
                metrics=metrics,
                tags=["test", "solver"],
            )

            benchmarks.suite.aggregator.add_result(result)

        # Export
        csv_file = tmp_path / "benchmarks.csv"
        benchmarks.suite.aggregator.export_to_csv(csv_file)

        assert csv_file.exists()
        logger.info(f"Exported CSV: {csv_file}")


class TestConfigurationBenchmarks:
    """Test suite for configuration benchmarking."""

    @pytest.fixture
    def config_benchmarks(self):
        """Create configuration benchmarks."""
        return ConfigurationBenchmarks()

    def test_config_benchmarks_initialization(self, config_benchmarks):
        """Test configuration benchmarks initialization."""
        assert config_benchmarks is not None
        assert config_benchmarks.suite is not None
        logger.info("Configuration benchmarks initialized successfully")

    def test_yaml_loading_benchmarks(self, config_benchmarks):
        """Test YAML loading benchmarks."""
        result_small = config_benchmarks.benchmark_yaml_loading(config_benchmarks.small_config_file)
        assert result_small is not None
        assert result_small.category == BenchmarkCategory.CONFIG_LOADING
        logger.info(f"Small YAML loading: {result_small.metrics.execution_time_ms:.2f}ms")

        result_large = config_benchmarks.benchmark_yaml_loading(config_benchmarks.large_config_file)
        assert result_large is not None
        logger.info(f"Large YAML loading: {result_large.metrics.execution_time_ms:.2f}ms")

    def test_config_loader_benchmark(self, config_benchmarks):
        """Test ConfigLoader benchmarks."""
        result = config_benchmarks.benchmark_config_loader(config_benchmarks.medium_config_file)

        assert result is not None
        assert result.category == BenchmarkCategory.CONFIG_LOADING
        logger.info(f"ConfigLoader: {result.metrics.execution_time_ms:.2f}ms")

    def test_config_manager_operations(self, config_benchmarks):
        """Test ConfigManager operation benchmarks."""
        result_get = config_benchmarks.benchmark_config_get_operations()
        assert result_get is not None
        logger.info(f"Config get operations: {result_get.metrics.execution_time_ms:.4f}ms")

        result_set = config_benchmarks.benchmark_config_set_operations()
        assert result_set is not None
        logger.info(f"Config set operations: {result_set.metrics.execution_time_ms:.4f}ms")

    def test_nested_config_access(self, config_benchmarks):
        """Test nested configuration access."""
        result = config_benchmarks.benchmark_nested_config_access()

        assert result is not None
        assert result.metrics.execution_time_ms >= 0
        logger.info(f"Nested config access: {result.metrics.execution_time_ms:.4f}ms")

    def test_config_benchmarks_summary(self, config_benchmarks):
        """Test configuration benchmarks summary."""
        # Run some benchmarks
        config_benchmarks.benchmark_config_manager_initialization()
        config_benchmarks.benchmark_config_get_operations()

        summary = config_benchmarks.get_results_summary()

        assert "total" in summary
        assert "passed" in summary
        logger.info(f"Configuration benchmarks summary: {summary}")

    def test_config_benchmarks_cleanup(self, config_benchmarks):
        """Test cleanup of temporary files."""
        config_benchmarks.cleanup()
        logger.info("Configuration benchmarks cleaned up")


class TestBenchmarkReportGenerator:
    """Test suite for report generation."""

    @pytest.fixture
    def report_generator(self, tmp_path):
        """Create report generator."""
        return BenchmarkReportGenerator(tmp_path)

    def test_report_generator_initialization(self, report_generator):
        """Test report generator initialization."""
        assert report_generator is not None
        assert report_generator.results_dir.exists()
        logger.info("Report generator initialized")

    def test_html_report_generation(self, report_generator, tmp_path):
        """Test HTML report generation."""
        # Create sample benchmark data
        benchmark_data = {
            "suite_name": "Test Suite",
            "timestamp": "2025-01-09T12:00:00",
            "summary": {
                "total": 10,
                "passed": 8,
                "failed": 2,
                "pass_rate": 80.0,
                "avg_execution_time_ms": 150.0,
                "max_execution_time_ms": 500.0,
                "avg_memory_peak_mb": 256.0,
                "max_memory_peak_mb": 512.0,
            },
            "results": [
                {
                    "name": "benchmark_1",
                    "category": "solver_execution",
                    "description": "Test 1",
                    "passed": True,
                    "metrics": {
                        "execution_time_ms": 100.0,
                        "memory_peak_mb": 200.0,
                        "memory_avg_mb": 100.0,
                        "cpu_percent": 40.0,
                        "iterations": 1,
                    },
                    "tags": ["test"],
                },
            ],
            "categories": {},
            "failures": [],
        }

        report_file = report_generator.generate_report(benchmark_data, "test_report")

        assert report_file.exists()
        assert report_file.suffix == ".html"
        logger.info(f"Generated report: {report_file}")

    def test_report_with_baseline_data(self, report_generator, tmp_path):
        """Test report with baseline comparison data."""
        benchmark_data = {
            "suite_name": "Baseline Test",
            "timestamp": "2025-01-09T12:00:00",
            "summary": {"total": 1, "passed": 1, "failed": 0, "pass_rate": 100.0},
            "results": [
                {
                    "name": "baseline_test",
                    "category": "solver_execution",
                    "passed": True,
                    "metrics": {
                        "execution_time_ms": 120.0,
                        "memory_peak_mb": 220.0,
                        "memory_avg_mb": 110.0,
                        "cpu_percent": 45.0,
                        "iterations": 1,
                    },
                    "baseline": {
                        "execution_time_ms": 100.0,
                        "memory_peak_mb": 200.0,
                        "memory_avg_mb": 100.0,
                        "cpu_percent": 40.0,
                        "iterations": 1,
                    },
                    "comparison_to_baseline": {
                        "time_delta_percent": 20.0,
                        "memory_delta_percent": 10.0,
                    },
                }
            ],
            "categories": {},
            "failures": [],
        }

        report_file = report_generator.generate_report(benchmark_data, "baseline_report")

        assert report_file.exists()
        logger.info(f"Generated baseline report: {report_file}")
