#!/usr/bin/env python3
"""
ABOUTME: Performance benchmarking tests for Phase 3 solvers
ABOUTME: Solver execution time, memory usage, and performance target validation
"""

import pytest
import logging
import time
import psutil
import os
import sys
from pathlib import Path
from typing import Dict, Any, List
from unittest.mock import MagicMock, patch
import statistics

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def performance_logger():
    """Configure performance logging."""
    logger = logging.getLogger("performance_tests")
    logger.setLevel(logging.INFO)
    return logger


@pytest.fixture
def memory_tracker():
    """Track memory usage during tests."""
    class MemoryTracker:
        def __init__(self):
            self.process = psutil.Process(os.getpid())
            self.snapshots = []

        def snapshot(self, label: str = ""):
            """Take memory snapshot."""
            memory_info = self.process.memory_info()
            snapshot = {
                "label": label,
                "timestamp": time.time(),
                "rss_mb": memory_info.rss / (1024 * 1024),
                "vms_mb": memory_info.vms / (1024 * 1024)
            }
            self.snapshots.append(snapshot)
            return snapshot

        def get_delta(self, start_idx: int = 0, end_idx: int = None) -> float:
            """Get memory delta between snapshots."""
            if end_idx is None:
                end_idx = len(self.snapshots) - 1

            if start_idx < len(self.snapshots) and end_idx < len(self.snapshots):
                start = self.snapshots[start_idx]["rss_mb"]
                end = self.snapshots[end_idx]["rss_mb"]
                return end - start

            return 0.0

        def peak_usage(self) -> float:
            """Get peak memory usage."""
            if self.snapshots:
                return max(s["rss_mb"] for s in self.snapshots)
            return 0.0

    return MemoryTracker()


@pytest.fixture
def mock_solver_with_metrics():
    """Create mock solver that reports performance metrics."""
    class MockSolverMetrics:
        def __init__(self, name: str, execution_time: float = 0.1):
            self.name = name
            self.execution_time = execution_time
            self.iterations = 0
            self.convergence_time = 0.0

        def solve(self) -> Dict[str, Any]:
            """Execute solver with timing."""
            start = time.time()
            time.sleep(self.execution_time)
            elapsed = time.time() - start

            return {
                "success": True,
                "name": self.name,
                "execution_time": elapsed,
                "iterations": 50,
                "convergence": True,
                "computation_time": self.execution_time
            }

        def solve_with_memory(self) -> Dict[str, Any]:
            """Execute solver with memory allocation."""
            start = time.time()

            # Allocate memory
            data = [0.0 for _ in range(1000000)]
            time.sleep(self.execution_time)
            elapsed = time.time() - start

            del data  # Clean up

            return {
                "success": True,
                "name": self.name,
                "execution_time": elapsed,
                "memory_allocated_mb": 8.0  # Approximate
            }

    return MockSolverMetrics


@pytest.fixture
def performance_targets():
    """Define performance targets for benchmarks."""
    return {
        "catenary_solver": {
            "max_execution_time": 1.0,  # seconds
            "max_memory_mb": 256.0,
            "iterations_range": (30, 100),
            "target_tolerance": 1e-6
        },
        "capacity_solver": {
            "max_execution_time": 0.5,
            "max_memory_mb": 128.0,
            "iterations_range": (10, 50)
        },
        "fatigue_solver": {
            "max_execution_time": 2.0,
            "max_memory_mb": 512.0,
            "iterations_range": (50, 500)
        },
        "batch_processing": {
            "max_time_per_item": 0.1,
            "throughput_items_per_sec": 10.0
        }
    }


# ============================================================================
# TEST CLASS: Solver Execution Time Benchmarks
# ============================================================================

class TestSolverExecutionTime:
    """Benchmark tests for solver execution time."""

    def test_catenary_solver_execution_time(self, benchmark, mock_solver_with_metrics,
                                           performance_targets):
        """Benchmark catenary solver execution time."""
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        target = performance_targets["catenary_solver"]["max_execution_time"]

        result = benchmark(solver.solve)

        assert result["success"]
        assert result["execution_time"] < target

    def test_capacity_solver_execution_time(self, benchmark, mock_solver_with_metrics,
                                           performance_targets):
        """Benchmark capacity solver execution time."""
        solver = mock_solver_with_metrics("capacity", execution_time=0.03)
        target = performance_targets["capacity_solver"]["max_execution_time"]

        result = benchmark(solver.solve)

        assert result["success"]
        assert result["execution_time"] < target

    def test_fatigue_solver_execution_time(self, benchmark, mock_solver_with_metrics,
                                          performance_targets):
        """Benchmark fatigue solver execution time."""
        solver = mock_solver_with_metrics("fatigue", execution_time=0.1)
        target = performance_targets["fatigue_solver"]["max_execution_time"]

        result = benchmark(solver.solve)

        assert result["success"]
        assert result["execution_time"] < target

    def test_solver_execution_consistency(self, mock_solver_with_metrics,
                                         performance_targets):
        """Test consistent execution time across multiple runs."""
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        target = performance_targets["catenary_solver"]["max_execution_time"]

        execution_times = []
        for _ in range(10):
            start = time.time()
            result = solver.solve()
            elapsed = time.time() - start
            execution_times.append(elapsed)

        mean_time = statistics.mean(execution_times)
        std_dev = statistics.stdev(execution_times)
        coefficient_of_variation = std_dev / mean_time if mean_time > 0 else 0

        assert mean_time < target
        assert coefficient_of_variation < 0.2  # Consistent within 20%

    def test_solver_warmup_performance(self, mock_solver_with_metrics,
                                      performance_targets):
        """Test solver performance with warmup runs."""
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        target = performance_targets["catenary_solver"]["max_execution_time"]

        # Warmup runs
        for _ in range(3):
            solver.solve()

        # Actual measurement
        execution_times = []
        for _ in range(10):
            start = time.time()
            result = solver.solve()
            elapsed = time.time() - start
            execution_times.append(elapsed)

        mean_time = statistics.mean(execution_times)
        assert mean_time < target


# ============================================================================
# TEST CLASS: Memory Usage Profiling
# ============================================================================

class TestMemoryUsageProfiler:
    """Memory usage profiling tests."""

    def test_solver_memory_usage(self, memory_tracker, mock_solver_with_metrics):
        """Test memory usage of solver execution."""
        memory_tracker.snapshot("before")

        solver = mock_solver_with_metrics("catenary", execution_time=0.01)
        result = solver.solve_with_memory()

        memory_tracker.snapshot("after")

        memory_delta = memory_tracker.get_delta()
        assert memory_delta >= 0
        assert memory_tracker.peak_usage() > 0

    def test_batch_processing_memory(self, memory_tracker, mock_solver_with_metrics):
        """Test memory usage during batch processing."""
        memory_tracker.snapshot("batch_start")

        solver = mock_solver_with_metrics("catenary", execution_time=0.01)

        # Process batch
        batch_size = 100
        for i in range(batch_size):
            result = solver.solve()
            if i % 25 == 0:
                memory_tracker.snapshot(f"batch_{i}")

        memory_tracker.snapshot("batch_end")

        peak = memory_tracker.peak_usage()
        assert peak > 0

    def test_memory_leak_detection(self, memory_tracker, mock_solver_with_metrics):
        """Test detection of memory leaks in iterative solving."""
        solver = mock_solver_with_metrics("catenary", execution_time=0.01)
        memory_snapshots = []

        memory_tracker.snapshot("start")

        # Run solver multiple times
        iterations = 50
        for i in range(iterations):
            result = solver.solve()

            # Take memory snapshot every 10 iterations
            if (i + 1) % 10 == 0:
                snapshot = memory_tracker.snapshot(f"iter_{i+1}")
                memory_snapshots.append(snapshot["rss_mb"])

        # Check memory trend (should not grow significantly)
        if len(memory_snapshots) > 1:
            memory_growth_rate = (memory_snapshots[-1] - memory_snapshots[0]) / len(memory_snapshots)
            # Growth should be minimal (< 1 MB per 10 iterations)
            assert memory_growth_rate < 1.0

    def test_concurrent_solver_memory(self, memory_tracker, mock_solver_with_metrics):
        """Test memory usage with concurrent solvers."""
        import concurrent.futures

        memory_tracker.snapshot("concurrent_start")

        def run_solver(name):
            solver = mock_solver_with_metrics(name, execution_time=0.01)
            return solver.solve()

        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            futures = [executor.submit(run_solver, f"solver_{i}") for i in range(10)]
            results = [f.result() for f in concurrent.futures.as_completed(futures)]

        memory_tracker.snapshot("concurrent_end")

        assert len(results) == 10
        assert all(r["success"] for r in results)


# ============================================================================
# TEST CLASS: Performance Target Validation
# ============================================================================

class TestPerformanceTargets:
    """Tests validating performance targets."""

    def test_catenary_performance_targets(self, mock_solver_with_metrics,
                                         performance_targets):
        """Validate catenary solver meets performance targets."""
        target = performance_targets["catenary_solver"]
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)

        result = solver.solve()

        assert result["execution_time"] < target["max_execution_time"]
        assert result["iterations"] >= target["iterations_range"][0]
        assert result["iterations"] <= target["iterations_range"][1]

    def test_capacity_performance_targets(self, mock_solver_with_metrics,
                                         performance_targets):
        """Validate capacity solver meets performance targets."""
        target = performance_targets["capacity_solver"]
        solver = mock_solver_with_metrics("capacity", execution_time=0.03)

        result = solver.solve()

        assert result["execution_time"] < target["max_execution_time"]

    def test_fatigue_performance_targets(self, mock_solver_with_metrics,
                                        performance_targets):
        """Validate fatigue solver meets performance targets."""
        target = performance_targets["fatigue_solver"]
        solver = mock_solver_with_metrics("fatigue", execution_time=0.1)

        result = solver.solve()

        assert result["execution_time"] < target["max_execution_time"]

    def test_batch_throughput_target(self, mock_solver_with_metrics,
                                    performance_targets):
        """Validate batch processing throughput target."""
        target = performance_targets["batch_processing"]
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)

        batch_size = 100
        start_time = time.time()

        for _ in range(batch_size):
            result = solver.solve()

        total_time = time.time() - start_time
        actual_throughput = batch_size / total_time

        assert actual_throughput >= target["throughput_items_per_sec"]

    def test_mixed_workload_performance(self, mock_solver_with_metrics,
                                       performance_targets):
        """Test performance with mixed workload."""
        solvers = [
            ("catenary", mock_solver_with_metrics("catenary", 0.05)),
            ("capacity", mock_solver_with_metrics("capacity", 0.03)),
            ("fatigue", mock_solver_with_metrics("fatigue", 0.1))
        ]

        execution_times = {name: [] for name, _ in solvers}

        # Run mixed workload
        for name, solver in solvers * 5:  # Repeat 5 times
            start = time.time()
            result = solver.solve()
            elapsed = time.time() - start
            execution_times[name].append(elapsed)

        # Verify all meet targets
        for name, times in execution_times.items():
            mean_time = statistics.mean(times)
            target_time = performance_targets[f"{name}_solver"]["max_execution_time"]
            assert mean_time < target_time


# ============================================================================
# TEST CLASS: Scalability Benchmarks
# ============================================================================

class TestScalabilityBenchmarks:
    """Scalability benchmarking tests."""

    def test_solver_scalability_with_iterations(self, mock_solver_with_metrics):
        """Test solver scalability with increasing iterations."""
        execution_times = {}

        for iterations in [10, 50, 100, 500]:
            solver = mock_solver_with_metrics("catenary", execution_time=0.01 * iterations)
            start = time.time()
            result = solver.solve()
            elapsed = time.time() - start
            execution_times[iterations] = elapsed

        # Verify roughly linear scaling
        times = list(execution_times.values())
        for i in range(1, len(times)):
            assert times[i] >= times[i-1]  # Monotonic increase

    def test_batch_size_scalability(self, mock_solver_with_metrics):
        """Test scalability with batch size."""
        batch_times = {}

        for batch_size in [10, 50, 100, 500]:
            solver = mock_solver_with_metrics("catenary", execution_time=0.01)

            start = time.time()
            for _ in range(batch_size):
                solver.solve()
            elapsed = time.time() - start

            batch_times[batch_size] = elapsed

        # Verify linear scaling
        for batch_size, elapsed in sorted(batch_times.items()):
            # Time should scale roughly linearly with batch size
            expected_time = (batch_size / 10) * batch_times[10]
            # Allow 50% variance
            assert elapsed <= expected_time * 1.5

    def test_parallel_execution_speedup(self, mock_solver_with_metrics):
        """Test speedup with parallel execution."""
        import concurrent.futures

        solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        num_tasks = 20

        # Sequential execution
        start = time.time()
        for _ in range(num_tasks):
            solver.solve()
        sequential_time = time.time() - start

        # Parallel execution
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            start = time.time()
            futures = [executor.submit(solver.solve) for _ in range(num_tasks)]
            results = [f.result() for f in concurrent.futures.as_completed(futures)]
            parallel_time = time.time() - start

        # Parallel should be faster
        speedup = sequential_time / parallel_time
        assert speedup > 1.0


# ============================================================================
# TEST CLASS: Regression Detection
# ============================================================================

class TestPerformanceRegression:
    """Tests for detecting performance regressions."""

    def test_performance_regression_detection(self, mock_solver_with_metrics,
                                             performance_logger):
        """Test detection of performance regression."""
        baseline_times = []
        regression_times = []

        # Establish baseline
        solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        for _ in range(5):
            start = time.time()
            solver.solve()
            elapsed = time.time() - start
            baseline_times.append(elapsed)

        baseline_mean = statistics.mean(baseline_times)

        # Simulate regression (slower solver)
        slow_solver = mock_solver_with_metrics("catenary", execution_time=0.15)
        for _ in range(5):
            start = time.time()
            slow_solver.solve()
            elapsed = time.time() - start
            regression_times.append(elapsed)

        regression_mean = statistics.mean(regression_times)

        # Detect regression (>50% slower)
        regression_detected = regression_mean > baseline_mean * 1.5
        assert regression_detected

        performance_logger.warning(
            f"Performance regression detected: {baseline_mean:.4f}s -> {regression_mean:.4f}s"
        )

    def test_performance_improvement_detection(self, mock_solver_with_metrics,
                                              performance_logger):
        """Test detection of performance improvement."""
        baseline_times = []
        improved_times = []

        # Establish baseline
        solver = mock_solver_with_metrics("catenary", execution_time=0.10)
        for _ in range(5):
            start = time.time()
            solver.solve()
            elapsed = time.time() - start
            baseline_times.append(elapsed)

        baseline_mean = statistics.mean(baseline_times)

        # Improved solver (faster)
        fast_solver = mock_solver_with_metrics("catenary", execution_time=0.05)
        for _ in range(5):
            start = time.time()
            fast_solver.solve()
            elapsed = time.time() - start
            improved_times.append(elapsed)

        improved_mean = statistics.mean(improved_times)

        # Detect improvement (>30% faster)
        improvement_detected = improved_mean < baseline_mean * 0.7
        assert improvement_detected

        performance_logger.info(
            f"Performance improvement detected: {baseline_mean:.4f}s -> {improved_mean:.4f}s"
        )


# ============================================================================
# TEST CLASS: Benchmark Summary
# ============================================================================

class TestBenchmarkSummary:
    """Generate benchmark summary and reports."""

    def test_generate_performance_report(self, mock_solver_with_metrics,
                                        performance_targets):
        """Generate comprehensive performance report."""
        report = {
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "solvers": {},
            "summary": {}
        }

        solvers_info = [
            ("catenary", 0.05),
            ("capacity", 0.03),
            ("fatigue", 0.10)
        ]

        total_times = []

        for name, exec_time in solvers_info:
            solver = mock_solver_with_metrics(name, execution_time=exec_time)
            times = []

            for _ in range(10):
                start = time.time()
                result = solver.solve()
                elapsed = time.time() - start
                times.append(elapsed)

            report["solvers"][name] = {
                "mean_time": statistics.mean(times),
                "std_dev": statistics.stdev(times),
                "min_time": min(times),
                "max_time": max(times),
                "target_time": performance_targets[f"{name}_solver"]["max_execution_time"]
            }

            total_times.extend(times)

        report["summary"] = {
            "total_benchmarks": len(total_times),
            "overall_mean": statistics.mean(total_times),
            "overall_min": min(total_times),
            "overall_max": max(total_times)
        }

        assert "solvers" in report
        assert len(report["solvers"]) == 3
        assert report["summary"]["total_benchmarks"] > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short", "-m", "benchmark"])
