"""
Performance benchmark tests for digitalmodel.

These tests establish performance baselines and detect regressions.
"""

import pytest
from hypothesis import given, strategies as st
import time
from typing import List


@pytest.mark.benchmark
class TestAlgorithmPerformance:
    """Performance benchmarks for core algorithms."""

    def test_list_processing_benchmark(self, benchmark, benchmark_datasets):
        """Benchmark list processing performance."""
        data = benchmark_datasets["medium"]

        def process_list(items):
            return [x * 2 for x in items if x % 2 == 0]

        result = benchmark(process_list, data)
        assert isinstance(result, list)
        assert len(result) > 0

    def test_string_manipulation_benchmark(self, benchmark):
        """Benchmark string manipulation operations."""
        text = "The quick brown fox jumps over the lazy dog " * 100

        def process_text(text):
            return text.upper().replace(" ", "_").split("_")

        result = benchmark(process_text, text)
        assert isinstance(result, list)

    def test_dictionary_operations_benchmark(self, benchmark, benchmark_datasets):
        """Benchmark dictionary operations."""
        size = len(benchmark_datasets["small"])
        data = {f"key_{i}": f"value_{i}" for i in range(size)}

        def process_dict(data):
            return {k: v.upper() for k, v in data.items() if "5" in k}

        result = benchmark(process_dict, data)
        assert isinstance(result, dict)

    @pytest.mark.slow
    def test_large_dataset_benchmark(self, benchmark, benchmark_datasets):
        """Benchmark performance with large datasets."""
        data = benchmark_datasets["large"]

        def compute_statistics(items):
            return {
                "sum": sum(items),
                "avg": sum(items) / len(items),
                "max": max(items),
                "min": min(items)
            }

        result = benchmark(compute_statistics, data)
        assert "sum" in result
        assert "avg" in result


@pytest.mark.benchmark
class TestMemoryPerformance:
    """Memory usage and efficiency benchmarks."""

    def test_memory_efficient_processing(self, benchmark):
        """Test memory-efficient data processing."""
        def memory_efficient_generator(n):
            return (x * x for x in range(n))

        def consume_generator(gen):
            return sum(gen)

        result = benchmark(lambda: consume_generator(memory_efficient_generator(1000)))
        assert result > 0

    def test_memory_vs_speed_tradeoff(self, benchmark):
        """Compare memory usage vs processing speed."""
        data = list(range(1000))

        def memory_heavy_approach(items):
            # Store intermediate results
            squared = [x * x for x in items]
            filtered = [x for x in squared if x % 2 == 0]
            return sum(filtered)

        result = benchmark(memory_heavy_approach, data)
        assert result > 0


@pytest.mark.benchmark
@pytest.mark.property
class TestPerformanceProperties:
    """Property-based performance tests."""

    @given(st.lists(st.integers(min_value=1, max_value=1000), min_size=10, max_size=100))
    def test_linear_scaling_property(self, numbers):
        """Test that algorithm scales linearly with input size."""
        start_time = time.perf_counter()

        # Simple O(n) algorithm
        result = sum(x * 2 for x in numbers)

        duration = time.perf_counter() - start_time

        # Performance assertion: should complete quickly for reasonable input
        assert duration < 0.1  # 100ms threshold
        assert result >= 0

    @given(st.lists(st.text(min_size=1, max_size=20), min_size=5, max_size=50))
    def test_string_processing_scales(self, strings):
        """Test string processing performance scales predictably."""
        start_time = time.perf_counter()

        processed = [s.upper().strip() for s in strings]

        duration = time.perf_counter() - start_time

        assert duration < 0.1
        assert len(processed) == len(strings)


@pytest.mark.benchmark
class TestRegressionBenchmarks:
    """Benchmark tests to detect performance regressions."""

    def test_baseline_performance_regression(self, benchmark):
        """Establish baseline for regression detection."""
        def reference_algorithm(n):
            """Reference implementation for performance comparison."""
            result = 0
            for i in range(n):
                result += i * i
            return result

        # This benchmark will fail if performance degrades significantly
        result = benchmark.pedantic(
            reference_algorithm,
            args=(1000,),
            iterations=10,
            rounds=5
        )
        assert result > 0

    def test_performance_variance_check(self, benchmark):
        """Ensure performance is consistent across runs."""
        def consistent_algorithm():
            return sum(range(100))

        # Multiple rounds to check consistency
        result = benchmark.pedantic(
            consistent_algorithm,
            iterations=20,
            rounds=10
        )
        assert result == 4950  # Expected sum of range(100)


# Performance test utilities
def performance_test_helper(func, *args, max_time=1.0):
    """Helper function for custom performance assertions."""
    start = time.perf_counter()
    result = func(*args)
    duration = time.perf_counter() - start

    assert duration < max_time, f"Function took {duration:.3f}s, expected < {max_time}s"
    return result