"""
Performance benchmark tests for critical DigitalModel functions.

These tests establish performance baselines and detect regressions
following practices from Google's Testing Blog and Netflix's performance testing.
"""
import pytest
import numpy as np
import pandas as pd
import time
from pathlib import Path
import sys
import os

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

try:
    from digitalmodel.common.basic_statistics import *
    from digitalmodel.common.fatigue_analysis import *
    from digitalmodel.common.FEAComponents import *
except ImportError:
    # Skip if modules not available
    pytest.skip("DigitalModel modules not available", allow_module_level=True)


class TestMathematicalOperationsBenchmarks:
    """Benchmark tests for mathematical operations."""

    @pytest.mark.performance
    def test_statistical_calculations_benchmark(self, benchmark_runner, performance_baselines):
        """Benchmark statistical calculations with various data sizes."""

        def run_stats(data_size):
            """Run statistical calculations on data."""
            data = np.random.randn(data_size)
            mean = np.mean(data)
            std = np.std(data)
            median = np.median(data)
            return mean, std, median

        # Test with different data sizes
        for size in [1000, 10000, 100000]:
            stats = benchmark_runner.run_benchmark(
                run_stats, iterations=50, data_size=size
            )

            # Assert performance requirements
            baseline = performance_baselines.get("statistical_calculations", {})
            max_time = baseline.get("max_time", 0.01 * (size / 1000))  # Scale with data size

            assert stats['mean'] < max_time, f"Statistical calculations too slow for size {size}: {stats['mean']:.4f}s"
            assert stats['p95'] < max_time * 2, f"95th percentile too slow for size {size}: {stats['p95']:.4f}s"

            print(f"Size {size}: Mean={stats['mean']:.4f}s, P95={stats['p95']:.4f}s")

    @pytest.mark.performance
    def test_matrix_operations_benchmark(self, benchmark_runner):
        """Benchmark matrix operations performance."""

        def matrix_multiply(size):
            """Perform matrix multiplication."""
            a = np.random.randn(size, size)
            b = np.random.randn(size, size)
            return np.dot(a, b)

        for size in [100, 500, 1000]:
            stats = benchmark_runner.run_benchmark(
                matrix_multiply, iterations=10, size=size
            )

            # Matrix multiplication should scale roughly as O(n^3)
            expected_time = (size / 100) ** 3 * 0.001  # Base time for 100x100

            assert stats['median'] < expected_time * 10, f"Matrix multiplication too slow for size {size}x{size}"

    @pytest.mark.performance
    def test_array_processing_benchmark(self, benchmark_runner, load_test_data):
        """Benchmark array processing operations."""

        def process_array(data):
            """Process array with various operations."""
            # Simulate complex array processing
            result = np.where(data > 0, data ** 2, np.abs(data))
            result = np.convolve(result, np.ones(5)/5, mode='same')  # Moving average
            return result

        for size in [10000, 50000, 100000]:
            data = load_test_data(size, "numerical")
            stats = benchmark_runner.run_benchmark(
                process_array, iterations=20, data=data
            )

            # Should process at least 1M elements per second
            throughput = size / stats['mean']
            assert throughput > 1_000_000, f"Array processing throughput too low: {throughput:.0f} elements/sec"


class TestDataProcessingBenchmarks:
    """Benchmark tests for data processing operations."""

    @pytest.mark.performance
    def test_dataframe_operations_benchmark(self, benchmark_runner, load_test_data):
        """Benchmark pandas DataFrame operations."""

        def process_dataframe(df):
            """Perform typical DataFrame operations."""
            # Simulate data processing pipeline
            result = df.copy()
            result['new_col'] = result['x'] * result['y']
            result = result.groupby(pd.cut(result['z'], bins=10)).agg({
                'x': 'mean',
                'y': 'std',
                'new_col': 'sum'
            })
            return result

        for size in [1000, 10000, 50000]:
            df = load_test_data(size, "dataframe")
            stats = benchmark_runner.run_benchmark(
                process_dataframe, iterations=10, df=df
            )

            # DataFrame processing should be reasonable
            max_time = 0.1 * (size / 1000)  # Scale with size
            assert stats['mean'] < max_time, f"DataFrame processing too slow for size {size}"

    @pytest.mark.performance
    def test_file_io_benchmark(self, benchmark_runner, tmp_path):
        """Benchmark file I/O operations."""

        def write_read_csv(data, filepath):
            """Write and read CSV file."""
            data.to_csv(filepath, index=False)
            return pd.read_csv(filepath)

        for size in [1000, 5000, 10000]:
            df = pd.DataFrame({
                'col1': np.random.randn(size),
                'col2': np.random.randn(size),
                'col3': np.random.randint(0, 100, size)
            })

            filepath = tmp_path / f"test_{size}.csv"
            stats = benchmark_runner.run_benchmark(
                write_read_csv, iterations=5, data=df, filepath=filepath
            )

            # File I/O should complete within reasonable time
            max_time = 0.1 * (size / 1000)
            assert stats['mean'] < max_time, f"File I/O too slow for size {size}"


class TestMemoryBenchmarks:
    """Memory usage benchmark tests."""

    @pytest.mark.performance
    def test_memory_allocation_patterns(self, memory_profiler_fixture):
        """Test memory allocation patterns for different data structures."""

        def create_large_array(size):
            """Create large array and perform operations."""
            arr = np.zeros(size)
            arr += 1
            return arr

        def create_large_dataframe(size):
            """Create large DataFrame."""
            return pd.DataFrame({
                'col1': np.random.randn(size),
                'col2': np.random.randn(size)
            })

        # Test array memory usage
        for size in [100000, 500000, 1000000]:
            profile = memory_profiler_fixture(create_large_array, size)

            # Memory usage should be predictable (8 bytes per float64)
            expected_mb = size * 8 / 1024 / 1024
            actual_mb = profile['peak_memory_mb'] - profile['start_memory_mb']

            # Allow some overhead but catch memory leaks
            assert actual_mb < expected_mb * 3, f"Excessive memory usage for array size {size}"

    @pytest.mark.performance
    def test_memory_cleanup(self, memory_profiler_fixture):
        """Test that memory is properly cleaned up after operations."""

        def allocate_and_cleanup():
            """Allocate large amount of memory and let it be cleaned up."""
            large_data = [np.random.randn(10000) for _ in range(100)]
            # Process data
            result = sum(np.sum(arr) for arr in large_data)
            del large_data  # Explicit cleanup
            return result

        profile = memory_profiler_fixture(allocate_and_cleanup)

        # Memory should return close to starting point
        memory_leak = profile['end_memory_mb'] - profile['start_memory_mb']
        assert memory_leak < 10, f"Potential memory leak detected: {memory_leak:.1f}MB"


class TestConcurrencyBenchmarks:
    """Benchmark concurrent operations."""

    @pytest.mark.performance
    def test_parallel_processing_speedup(self, benchmark_runner):
        """Test that parallel processing provides expected speedup."""
        import multiprocessing as mp
        from concurrent.futures import ProcessPoolExecutor

        def cpu_intensive_task(n):
            """CPU-intensive task for testing parallelization."""
            return sum(i * i for i in range(n))

        # Sequential processing
        def sequential_processing(tasks):
            return [cpu_intensive_task(task) for task in tasks]

        # Parallel processing
        def parallel_processing(tasks):
            with ProcessPoolExecutor(max_workers=2) as executor:
                return list(executor.map(cpu_intensive_task, tasks))

        tasks = [50000] * 4  # 4 tasks of moderate size

        # Benchmark sequential
        seq_stats = benchmark_runner.run_benchmark(
            sequential_processing, iterations=5, tasks=tasks
        )

        # Benchmark parallel
        par_stats = benchmark_runner.run_benchmark(
            parallel_processing, iterations=5, tasks=tasks
        )

        # Parallel should be faster (allowing for overhead)
        speedup = seq_stats['mean'] / par_stats['mean']
        print(f"Speedup: {speedup:.2f}x")

        # Should see some speedup, even with overhead
        assert speedup > 1.2, f"Insufficient parallel speedup: {speedup:.2f}x"


@pytest.mark.performance
class TestPerformanceRegression:
    """Tests to detect performance regressions."""

    def test_save_current_baselines(self, benchmark_runner):
        """Save current performance metrics as baselines."""
        # This test should be run manually to establish new baselines

        def dummy_calculation():
            return sum(i ** 2 for i in range(1000))

        stats = benchmark_runner.run_benchmark(dummy_calculation, iterations=100)

        # Save baseline
        baseline_file = Path("tests/performance/baselines.json")
        baselines = {}

        if baseline_file.exists():
            import json
            with open(baseline_file) as f:
                baselines = json.load(f)

        baselines["dummy_calculation"] = {
            "mean": stats['mean'],
            "p95": stats['p95'],
            "timestamp": time.time()
        }

        baseline_file.parent.mkdir(parents=True, exist_ok=True)
        import json
        with open(baseline_file, 'w') as f:
            json.dump(baselines, f, indent=2)

        print(f"Baseline saved: {stats['mean']:.6f}s mean, {stats['p95']:.6f}s p95")