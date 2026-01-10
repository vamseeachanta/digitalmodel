"""
ABOUTME: Solver-specific performance benchmarks
ABOUTME: Tests for execution time, memory usage, and scalability
"""

import logging
import numpy as np
from typing import Dict, List, Any, Callable
from pathlib import Path

from digitalmodel.base_solvers.base import BaseSolver, AnalysisSolver
from .benchmark_suite import (
    BenchmarkConfig,
    BenchmarkResult,
    BenchmarkCategory,
    BenchmarkExecutor,
    BenchmarkSuite,
)

logger = logging.getLogger(__name__)


class SolverBenchmarks:
    """Benchmark suite for solver performance testing."""

    def __init__(self, results_dir: Path = None):
        """
        Initialize solver benchmarks.

        Args:
            results_dir: Directory for saving results
        """
        self.suite = BenchmarkSuite("Solver Benchmarks", results_dir)
        self._register_solver_benchmarks()

    def _register_solver_benchmarks(self) -> None:
        """Register all solver benchmark configurations."""
        # Execution time benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="solver_simple_execution",
                max_time_ms=100.0,
                iterations=5,
                tags=["execution", "speed"],
                description="Simple solver execution",
            ),
            BenchmarkConfig(
                name="solver_medium_execution",
                max_time_ms=500.0,
                iterations=3,
                tags=["execution", "speed"],
                description="Medium complexity solver",
            ),
            BenchmarkConfig(
                name="solver_complex_execution",
                max_time_ms=2000.0,
                iterations=1,
                tags=["execution", "speed"],
                description="Complex solver execution",
            ),
            BenchmarkConfig(
                name="solver_validation",
                max_time_ms=50.0,
                iterations=10,
                tags=["validation"],
                description="Input validation performance",
            ),
            BenchmarkConfig(
                name="solver_status_check",
                max_time_ms=10.0,
                iterations=100,
                tags=["status"],
                description="Status checking performance",
            ),
            BenchmarkConfig(
                name="solver_results_retrieval",
                max_time_ms=20.0,
                iterations=50,
                tags=["retrieval"],
                description="Results retrieval performance",
            ),
        ])

        # Memory benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="solver_memory_small_dataset",
                max_memory_mb=100.0,
                iterations=3,
                tags=["memory", "scalability"],
                description="Memory usage with small dataset",
            ),
            BenchmarkConfig(
                name="solver_memory_medium_dataset",
                max_memory_mb=256.0,
                iterations=2,
                tags=["memory", "scalability"],
                description="Memory usage with medium dataset",
            ),
            BenchmarkConfig(
                name="solver_memory_large_dataset",
                max_memory_mb=512.0,
                iterations=1,
                tags=["memory", "scalability"],
                description="Memory usage with large dataset",
            ),
        ])

        # Scalability benchmarks
        self.suite.register_benchmarks([
            BenchmarkConfig(
                name="solver_scalability_linear",
                max_time_ms=5000.0,
                iterations=1,
                tags=["scalability"],
                description="Linear scalability test",
            ),
            BenchmarkConfig(
                name="solver_scalability_quadratic",
                max_time_ms=10000.0,
                iterations=1,
                tags=["scalability"],
                description="Quadratic scalability test",
            ),
            BenchmarkConfig(
                name="solver_concurrent_execution",
                max_time_ms=3000.0,
                iterations=1,
                tags=["concurrency"],
                description="Concurrent solver execution",
            ),
        ])

    def benchmark_solver_initialization(self, solver_class: type) -> BenchmarkResult:
        """
        Benchmark solver initialization time.

        Args:
            solver_class: Solver class to benchmark

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name=f"{solver_class.__name__}_initialization",
            max_time_ms=50.0,
            iterations=10,
            tags=["initialization"],
        )

        def init_solver():
            solver_class(
                name="benchmark_solver",
                version="1.0.0"
            )

        executor = BenchmarkExecutor(config)
        return executor.execute(
            init_solver,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Initialization of {solver_class.__name__}",
        )

    def benchmark_solver_validation(self, solver: BaseSolver) -> BenchmarkResult:
        """
        Benchmark solver input validation.

        Args:
            solver: Solver instance to benchmark

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name=f"{solver.name}_validation",
            max_time_ms=50.0,
            iterations=10,
            tags=["validation"],
        )

        def validate():
            solver.validate_inputs()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            validate,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Validation of {solver.name}",
        )

    def benchmark_solver_execution(self, solver: BaseSolver) -> BenchmarkResult:
        """
        Benchmark complete solver execution.

        Args:
            solver: Solver instance to benchmark

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name=f"{solver.name}_execution",
            max_time_ms=1000.0,
            iterations=3,
            tags=["execution"],
        )

        def execute():
            solver.solve()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            execute,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Execution of {solver.name}",
        )

    def benchmark_solver_results_caching(self, solver: BaseSolver, run_count: int = 10) -> BenchmarkResult:
        """
        Benchmark results retrieval from cache.

        Args:
            solver: Solver instance to benchmark
            run_count: Number of retrievals to benchmark

        Returns:
            BenchmarkResult
        """
        # First execute to cache results
        solver.solve()

        config = BenchmarkConfig(
            name=f"{solver.name}_results_caching",
            max_time_ms=10.0,
            iterations=run_count,
            tags=["caching"],
        )

        def get_results():
            solver.get_results()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            get_results,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Results caching of {solver.name}",
        )

    def benchmark_solver_metadata(self, solver: BaseSolver) -> BenchmarkResult:
        """
        Benchmark metadata retrieval.

        Args:
            solver: Solver instance to benchmark

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name=f"{solver.name}_metadata",
            max_time_ms=10.0,
            iterations=50,
            tags=["metadata"],
        )

        def get_metadata():
            solver.get_solver_metadata()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            get_metadata,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Metadata retrieval of {solver.name}",
        )

    def benchmark_input_data_handling(self, solver: AnalysisSolver) -> BenchmarkResult:
        """
        Benchmark input data handling (set and get).

        Args:
            solver: AnalysisSolver instance to benchmark

        Returns:
            BenchmarkResult
        """
        test_data = {
            "values": np.random.randn(1000).tolist(),
            "parameters": {
                "param1": 1.5,
                "param2": 2.5,
                "param3": 3.5,
            },
        }

        config = BenchmarkConfig(
            name=f"{solver.name}_input_data_handling",
            max_time_ms=50.0,
            iterations=10,
            tags=["data_handling"],
        )

        def set_and_get_data():
            solver.set_input_data(test_data)
            solver.get_input_data()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            set_and_get_data,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Input data handling of {solver.name}",
        )

    def benchmark_validation_error_handling(self, solver: AnalysisSolver) -> BenchmarkResult:
        """
        Benchmark validation error management.

        Args:
            solver: AnalysisSolver instance to benchmark

        Returns:
            BenchmarkResult
        """
        errors = [f"Error {i}" for i in range(10)]

        config = BenchmarkConfig(
            name=f"{solver.name}_error_handling",
            max_time_ms=20.0,
            iterations=20,
            tags=["error_handling"],
        )

        def manage_errors():
            solver.set_validation_errors(errors)
            solver.get_validation_errors()
            solver.has_validation_errors()
            solver.clear_validation_errors()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            manage_errors,
            category=BenchmarkCategory.SOLVER_EXECUTION,
            description=f"Validation error handling of {solver.name}",
        )

    def benchmark_solver_scaling(
        self,
        solver_factory: Callable,
        data_sizes: List[int]
    ) -> List[BenchmarkResult]:
        """
        Benchmark solver performance across different data sizes.

        Args:
            solver_factory: Function that creates configured solver
            data_sizes: List of data sizes to test

        Returns:
            List of BenchmarkResults
        """
        results = []

        for size in data_sizes:
            config = BenchmarkConfig(
                name=f"solver_scaling_size_{size}",
                max_time_ms=5000.0,
                iterations=1,
                tags=["scalability", f"size_{size}"],
            )

            def solve_scaled():
                solver = solver_factory(size)
                solver.solve()

            executor = BenchmarkExecutor(config)
            result = executor.execute(
                solve_scaled,
                category=BenchmarkCategory.SCALABILITY,
                description=f"Solver execution with {size} data points",
            )
            results.append(result)

        return results

    def benchmark_concurrent_solvers(
        self,
        solver_factory: Callable,
        num_instances: int
    ) -> BenchmarkResult:
        """
        Benchmark concurrent solver execution.

        Args:
            solver_factory: Function that creates solver
            num_instances: Number of concurrent solvers

        Returns:
            BenchmarkResult
        """
        config = BenchmarkConfig(
            name=f"concurrent_solvers_{num_instances}",
            max_time_ms=10000.0,
            iterations=1,
            tags=["concurrency", f"instances_{num_instances}"],
        )

        def run_concurrent():
            import threading

            solvers = [solver_factory() for _ in range(num_instances)]
            threads = []

            for solver in solvers:
                thread = threading.Thread(target=solver.solve)
                threads.append(thread)
                thread.start()

            for thread in threads:
                thread.join()

        executor = BenchmarkExecutor(config)
        return executor.execute(
            run_concurrent,
            category=BenchmarkCategory.INTEGRATION,
            description=f"Concurrent execution of {num_instances} solvers",
        )

    def run_all_benchmarks(self) -> Dict[str, Any]:
        """
        Run all registered benchmarks.

        Returns:
            Summary of benchmark results
        """
        logger.info("Running all solver benchmarks")
        summary = self.suite.run_all()
        results = self.suite.save_results()
        logger.info(f"Benchmarks complete. Files: {results}")
        return summary

    def get_results_summary(self) -> Dict[str, Any]:
        """
        Get summary of all benchmark results.

        Returns:
            Summary dictionary
        """
        return self.suite.aggregator.get_summary()

    def get_results_by_category(self, category: BenchmarkCategory) -> List[BenchmarkResult]:
        """
        Get results filtered by category.

        Args:
            category: BenchmarkCategory to filter

        Returns:
            List of BenchmarkResults
        """
        return self.suite.aggregator.get_by_category(category)
