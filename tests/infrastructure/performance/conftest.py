"""
Performance testing configuration and fixtures.
"""
import pytest
import time
import psutil
import memory_profiler
from typing import Dict, Any, Callable
from pathlib import Path
import json


@pytest.fixture
def performance_monitor():
    """Monitor system performance during tests."""
    process = psutil.Process()

    class PerformanceMonitor:
        def __init__(self):
            self.start_time = None
            self.start_memory = None
            self.start_cpu = None
            self.metrics = {}

        def start(self):
            self.start_time = time.perf_counter()
            self.start_memory = process.memory_info().rss / 1024 / 1024  # MB
            self.start_cpu = process.cpu_percent()

        def stop(self):
            end_time = time.perf_counter()
            end_memory = process.memory_info().rss / 1024 / 1024  # MB
            end_cpu = process.cpu_percent()

            self.metrics = {
                'execution_time': end_time - self.start_time,
                'memory_start_mb': self.start_memory,
                'memory_end_mb': end_memory,
                'memory_delta_mb': end_memory - self.start_memory,
                'cpu_start_percent': self.start_cpu,
                'cpu_end_percent': end_cpu,
                'peak_memory_mb': process.memory_info().peak_wss / 1024 / 1024 if hasattr(process.memory_info(), 'peak_wss') else end_memory
            }
            return self.metrics

        def assert_performance(self, max_time=None, max_memory_mb=None, max_cpu_percent=None):
            """Assert performance requirements are met."""
            if max_time and self.metrics['execution_time'] > max_time:
                pytest.fail(f"Execution time {self.metrics['execution_time']:.3f}s exceeded limit {max_time}s")
            if max_memory_mb and self.metrics['memory_delta_mb'] > max_memory_mb:
                pytest.fail(f"Memory usage {self.metrics['memory_delta_mb']:.1f}MB exceeded limit {max_memory_mb}MB")
            if max_cpu_percent and self.metrics['cpu_end_percent'] > max_cpu_percent:
                pytest.fail(f"CPU usage {self.metrics['cpu_end_percent']:.1f}% exceeded limit {max_cpu_percent}%")

    return PerformanceMonitor()


@pytest.fixture
def benchmark_runner():
    """Runner for benchmark tests with statistical analysis."""

    class BenchmarkRunner:
        def __init__(self):
            self.results = []

        def run_benchmark(self, func: Callable, iterations: int = 100, *args, **kwargs):
            """Run benchmark function multiple times and collect statistics."""
            times = []

            for _ in range(iterations):
                start = time.perf_counter()
                result = func(*args, **kwargs)
                end = time.perf_counter()
                times.append(end - start)

            import statistics
            stats = {
                'mean': statistics.mean(times),
                'median': statistics.median(times),
                'stdev': statistics.stdev(times) if len(times) > 1 else 0,
                'min': min(times),
                'max': max(times),
                'p95': sorted(times)[int(0.95 * len(times))],
                'p99': sorted(times)[int(0.99 * len(times))],
                'iterations': iterations
            }

            self.results.append({
                'function': func.__name__,
                'stats': stats,
                'times': times
            })

            return stats

        def save_results(self, filename: str):
            """Save benchmark results to file."""
            Path(filename).parent.mkdir(parents=True, exist_ok=True)
            with open(filename, 'w') as f:
                json.dump(self.results, f, indent=2)

    return BenchmarkRunner()


@pytest.fixture
def memory_profiler_fixture():
    """Memory profiling fixture using memory_profiler."""

    def profile_memory(func, *args, **kwargs):
        """Profile memory usage of a function."""
        usage = memory_profiler.memory_usage((func, args, kwargs), interval=0.1)
        return {
            'peak_memory_mb': max(usage),
            'start_memory_mb': usage[0],
            'end_memory_mb': usage[-1],
            'memory_profile': usage
        }

    return profile_memory


@pytest.fixture(scope="session")
def performance_baselines():
    """Load performance baselines from previous runs."""
    baselines_file = Path("tests/performance/baselines.json")

    if baselines_file.exists():
        with open(baselines_file) as f:
            return json.load(f)
    else:
        # Default baselines for first run
        return {
            "basic_calculation": {"max_time": 0.001, "max_memory_mb": 10},
            "data_processing": {"max_time": 0.1, "max_memory_mb": 50},
            "file_operations": {"max_time": 0.01, "max_memory_mb": 20},
            "api_calls": {"max_time": 1.0, "max_memory_mb": 30}
        }


@pytest.fixture
def load_test_data():
    """Generate test data for load testing."""

    def generate_data(size: int, data_type: str = "numerical"):
        """Generate test data of specified size and type."""
        import numpy as np
        import pandas as pd

        if data_type == "numerical":
            return np.random.randn(size)
        elif data_type == "dataframe":
            return pd.DataFrame({
                'x': np.random.randn(size),
                'y': np.random.randn(size),
                'z': np.random.randint(0, 100, size)
            })
        elif data_type == "strings":
            return [f"test_string_{i}" for i in range(size)]
        else:
            raise ValueError(f"Unknown data type: {data_type}")

    return generate_data