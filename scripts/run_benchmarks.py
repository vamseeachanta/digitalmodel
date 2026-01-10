#!/usr/bin/env python3
"""
ABOUTME: Demonstration script for running the performance benchmarking framework
ABOUTME: Shows complete workflow for solver and configuration benchmarks
"""

import logging
import sys
from pathlib import Path

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.base_solvers.base import BaseSolver, AnalysisSolver, SolverStatus
from digitalmodel.base_solvers.benchmarks import (
    SolverBenchmarks,
    ConfigurationBenchmarks,
    BenchmarkReportGenerator,
    BenchmarkConfig,
    BenchmarkCategory,
    BenchmarkResult,
)


class DemoSolver(BaseSolver):
    """Demo solver for benchmarking."""

    def __init__(self, name: str = "DemoSolver", version: str = "1.0.0", data_size: int = 100):
        super().__init__(name, version)
        self.data_size = data_size
        self.input_data = list(range(data_size))

    def validate_inputs(self):
        if not self.input_data:
            return False, ["No input data"]
        return True, []

    def solve(self):
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            self._set_status(SolverStatus.FAILED)
            return {"success": False, "errors": errors}

        self._set_status(SolverStatus.EXECUTING)

        # Simulate some computation
        result = {
            "sum": sum(self.input_data),
            "mean": sum(self.input_data) / len(self.input_data),
            "count": len(self.input_data),
        }

        self._cache_results(result)
        self._set_status(SolverStatus.COMPLETED)

        return {"success": True, "result": result}

    def get_solver_metadata(self):
        return {
            "name": self.name,
            "version": self.version,
            "description": "Demo solver for benchmarking",
            "solver_type": "analysis",
            "inputs": {"values": {"type": "list"}},
            "outputs": {"sum": {"type": "float"}, "mean": {"type": "float"}},
        }


def run_solver_benchmarks():
    """Run solver benchmarking suite."""
    logger.info("=" * 70)
    logger.info("STARTING SOLVER BENCHMARKS")
    logger.info("=" * 70)

    results_dir = Path("./benchmarks/results")
    results_dir.mkdir(parents=True, exist_ok=True)

    benchmarks = SolverBenchmarks(results_dir)

    # Benchmark 1: Solver Initialization
    logger.info("\n1. Benchmarking Solver Initialization...")
    init_result = benchmarks.benchmark_solver_initialization(DemoSolver)
    logger.info(f"   Time: {init_result.metrics.execution_time_ms:.2f}ms")
    logger.info(f"   Status: {'PASS' if init_result.passed else 'FAIL'}")

    # Benchmark 2: Solver with Different Data Sizes
    logger.info("\n2. Benchmarking Solver Scaling...")
    def solver_factory(size):
        return DemoSolver(data_size=size)

    data_sizes = [100, 500, 1000, 5000]
    scaling_results = benchmarks.benchmark_solver_scaling(solver_factory, data_sizes)

    for result in scaling_results:
        logger.info(f"   {result.name}: {result.metrics.execution_time_ms:.2f}ms")

    # Benchmark 3: Solver Validation
    logger.info("\n3. Benchmarking Solver Validation...")
    solver = DemoSolver(data_size=100)
    validation_result = benchmarks.benchmark_solver_validation(solver)
    logger.info(f"   Time: {validation_result.metrics.execution_time_ms:.4f}ms")

    # Benchmark 4: Complete Solver Execution
    logger.info("\n4. Benchmarking Complete Solver Execution...")
    solver = DemoSolver(data_size=1000)
    execution_result = benchmarks.benchmark_solver_execution(solver)
    logger.info(f"   Time: {execution_result.metrics.execution_time_ms:.2f}ms")
    logger.info(f"   Memory Peak: {execution_result.metrics.memory_peak_mb:.2f}MB")

    # Benchmark 5: Results Caching
    logger.info("\n5. Benchmarking Results Caching...")
    solver = DemoSolver()
    caching_result = benchmarks.benchmark_solver_results_caching(solver, run_count=100)
    logger.info(f"   Time (avg): {caching_result.metrics.execution_time_ms:.4f}ms per access")

    # Benchmark 6: Metadata Retrieval
    logger.info("\n6. Benchmarking Metadata Retrieval...")
    solver = DemoSolver()
    metadata_result = benchmarks.benchmark_solver_metadata(solver)
    logger.info(f"   Time (avg): {metadata_result.metrics.execution_time_ms:.4f}ms per call")

    # Generate summary
    logger.info("\n" + "=" * 70)
    logger.info("SOLVER BENCHMARKS SUMMARY")
    logger.info("=" * 70)

    summary = benchmarks.get_results_summary()
    logger.info(f"Total Tests: {summary['total']}")
    logger.info(f"Passed: {summary['passed']}")
    logger.info(f"Failed: {summary['failed']}")
    logger.info(f"Pass Rate: {summary['pass_rate']:.1f}%")
    logger.info(f"Avg Execution Time: {summary['avg_execution_time_ms']:.2f}ms")
    logger.info(f"Max Execution Time: {summary['max_execution_time_ms']:.2f}ms")
    logger.info(f"Avg Memory Peak: {summary['avg_memory_peak_mb']:.2f}MB")

    return benchmarks


def run_configuration_benchmarks():
    """Run configuration benchmarking suite."""
    logger.info("\n" + "=" * 70)
    logger.info("STARTING CONFIGURATION BENCHMARKS")
    logger.info("=" * 70)

    results_dir = Path("./benchmarks/results")
    results_dir.mkdir(parents=True, exist_ok=True)

    config_benchmarks = ConfigurationBenchmarks(results_dir)

    # Benchmark 1: YAML Loading
    logger.info("\n1. Benchmarking YAML Loading...")
    yaml_small = config_benchmarks.benchmark_yaml_loading(config_benchmarks.small_config_file)
    logger.info(f"   Small config: {yaml_small.metrics.execution_time_ms:.2f}ms")

    yaml_large = config_benchmarks.benchmark_yaml_loading(config_benchmarks.large_config_file)
    logger.info(f"   Large config: {yaml_large.metrics.execution_time_ms:.2f}ms")

    # Benchmark 2: ConfigManager Initialization
    logger.info("\n2. Benchmarking ConfigManager Initialization...")
    init_result = config_benchmarks.benchmark_config_manager_initialization()
    logger.info(f"   Time: {init_result.metrics.execution_time_ms:.2f}ms")

    # Benchmark 3: Configuration Operations
    logger.info("\n3. Benchmarking Configuration Operations...")
    get_result = config_benchmarks.benchmark_config_get_operations()
    logger.info(f"   Get operations (100x): {get_result.metrics.execution_time_ms:.4f}ms avg")

    set_result = config_benchmarks.benchmark_config_set_operations()
    logger.info(f"   Set operations (50x): {set_result.metrics.execution_time_ms:.4f}ms avg")

    # Benchmark 4: Nested Config Access
    logger.info("\n4. Benchmarking Nested Config Access...")
    nested_result = config_benchmarks.benchmark_nested_config_access()
    logger.info(f"   Time: {nested_result.metrics.execution_time_ms:.4f}ms per access")

    # Benchmark 5: Config Update Performance
    logger.info("\n5. Benchmarking Config Updates...")
    update_result = config_benchmarks.benchmark_config_update_performance()
    logger.info(f"   Time: {update_result.metrics.execution_time_ms:.2f}ms per update")

    # Generate summary
    logger.info("\n" + "=" * 70)
    logger.info("CONFIGURATION BENCHMARKS SUMMARY")
    logger.info("=" * 70)

    summary = config_benchmarks.get_results_summary()
    logger.info(f"Total Tests: {summary['total']}")
    logger.info(f"Passed: {summary['passed']}")
    logger.info(f"Failed: {summary['failed']}")
    logger.info(f"Pass Rate: {summary['pass_rate']:.1f}%")
    logger.info(f"Avg Execution Time: {summary['avg_execution_time_ms']:.2f}ms")

    config_benchmarks.cleanup()

    return config_benchmarks


def generate_reports(solver_benchmarks, config_benchmarks):
    """Generate HTML reports from benchmark results."""
    logger.info("\n" + "=" * 70)
    logger.info("GENERATING REPORTS")
    logger.info("=" * 70)

    reports_dir = Path("./benchmarks/reports")
    report_gen = BenchmarkReportGenerator(reports_dir)

    # Generate solver benchmark report
    logger.info("\nGenerating Solver Benchmarks Report...")
    solver_report_data = solver_benchmarks.suite.get_report_data()
    solver_report = report_gen.generate_report(solver_report_data, "solver_benchmarks")
    logger.info(f"Report saved: {solver_report}")

    # Generate config benchmark report
    logger.info("\nGenerating Configuration Benchmarks Report...")
    config_report_data = config_benchmarks.suite.get_report_data()
    config_report = report_gen.generate_report(config_report_data, "config_benchmarks")
    logger.info(f"Report saved: {config_report}")

    return solver_report, config_report


def main():
    """Main execution function."""
    logger.info("Starting Performance Benchmarking Framework Demonstration")
    logger.info("=" * 70)

    try:
        # Run benchmarks
        solver_benchmarks = run_solver_benchmarks()
        config_benchmarks = run_configuration_benchmarks()

        # Generate reports
        solver_report, config_report = generate_reports(solver_benchmarks, config_benchmarks)

        # Summary
        logger.info("\n" + "=" * 70)
        logger.info("BENCHMARKING COMPLETE")
        logger.info("=" * 70)
        logger.info("\nGenerated Files:")
        logger.info(f"  Results: ./benchmarks/results/")
        logger.info(f"  Reports: ./benchmarks/reports/")
        logger.info(f"\nOpen the HTML reports to view interactive visualizations:")
        logger.info(f"  Solver Report: {solver_report.name}")
        logger.info(f"  Config Report: {config_report.name}")

        return 0

    except Exception as e:
        logger.error(f"Error during benchmarking: {e}", exc_info=True)
        return 1


if __name__ == "__main__":
    sys.exit(main())
