"""
Performance baseline validation tests.

This module establishes and validates performance baselines for critical
operations, ensuring consistent performance across different environments
and preventing performance regressions.
"""
import pytest
import json
import time
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, Any, List
import statistics
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


class PerformanceBaselineManager:
    """Manages performance baselines for critical operations."""

    def __init__(self, baseline_file: str = "tests/performance/baselines.json"):
        self.baseline_file = Path(baseline_file)
        self.baselines = self._load_baselines()

    def _load_baselines(self) -> Dict[str, Dict[str, float]]:
        """Load existing baselines or create default ones."""
        if self.baseline_file.exists():
            with open(self.baseline_file, 'r') as f:
                return json.load(f)
        else:
            # Default baselines for first run
            return self._get_default_baselines()

    def _get_default_baselines(self) -> Dict[str, Dict[str, float]]:
        """Get default performance baselines."""
        return {
            "basic_math": {
                "max_time_ms": 1.0,
                "max_memory_mb": 10.0,
                "target_throughput": 1000000.0  # operations per second
            },
            "array_operations": {
                "max_time_ms": 10.0,
                "max_memory_mb": 50.0,
                "target_throughput": 100000.0
            },
            "dataframe_operations": {
                "max_time_ms": 100.0,
                "max_memory_mb": 100.0,
                "target_throughput": 10000.0
            },
            "file_io": {
                "max_time_ms": 50.0,
                "max_memory_mb": 20.0,
                "target_throughput": 1000.0
            },
            "statistical_calculations": {
                "max_time_ms": 5.0,
                "max_memory_mb": 25.0,
                "target_throughput": 50000.0
            },
            "matrix_operations": {
                "max_time_ms": 20.0,
                "max_memory_mb": 75.0,
                "target_throughput": 5000.0
            }
        }

    def save_baselines(self):
        """Save current baselines to file."""
        self.baseline_file.parent.mkdir(parents=True, exist_ok=True)
        with open(self.baseline_file, 'w') as f:
            json.dump(self.baselines, f, indent=2)

    def update_baseline(self, operation_name: str, metrics: Dict[str, float]):
        """Update baseline for an operation."""
        if operation_name not in self.baselines:
            self.baselines[operation_name] = {}

        # Conservative update - only improve baselines if significantly better
        current = self.baselines[operation_name]

        for metric, value in metrics.items():
            if metric not in current or value < current[metric] * 0.9:  # 10% improvement
                current[metric] = value

    def validate_performance(self, operation_name: str, metrics: Dict[str, float]) -> Dict[str, bool]:
        """Validate performance against baselines."""
        if operation_name not in self.baselines:
            return {"status": False, "reason": "No baseline found"}

        baseline = self.baselines[operation_name]
        results = {"status": True, "violations": []}

        for metric, value in metrics.items():
            if metric in baseline:
                threshold = baseline[metric]

                # Different validation logic for different metrics
                if metric.startswith("max_"):
                    if value > threshold:
                        results["violations"].append(f"{metric}: {value:.3f} > {threshold:.3f}")
                        results["status"] = False
                elif metric.startswith("min_") or metric.startswith("target_"):
                    if value < threshold:
                        results["violations"].append(f"{metric}: {value:.3f} < {threshold:.3f}")
                        results["status"] = False

        return results


@pytest.fixture(scope="session")
def baseline_manager():
    """Session-scoped fixture providing performance baseline manager."""
    return PerformanceBaselineManager()


class TestPerformanceBaselines:
    """Test performance baselines for critical operations."""

    def test_basic_math_operations_baseline(self, baseline_manager):
        """Test basic mathematical operations performance baseline."""

        def basic_math_operations():
            """Basic mathematical operations."""
            result = 0
            for i in range(10000):
                result += i ** 2
                result -= i * 0.5
                result *= 1.1
                result /= 1.05
            return result

        # Measure performance
        start_time = time.perf_counter()
        result = basic_math_operations()
        end_time = time.perf_counter()

        execution_time_ms = (end_time - start_time) * 1000
        throughput = 10000 / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("basic_math", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"Basic math: {execution_time_ms:.3f}ms, {throughput:.0f} ops/sec")

    def test_array_operations_baseline(self, baseline_manager):
        """Test NumPy array operations performance baseline."""

        def array_operations():
            """NumPy array operations."""
            size = 100000
            arr1 = np.random.randn(size)
            arr2 = np.random.randn(size)

            # Various array operations
            result = arr1 + arr2
            result = np.multiply(result, 2.0)
            result = np.sqrt(np.abs(result))
            result = np.sum(result)

            return result

        # Measure performance
        import psutil
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024

        start_time = time.perf_counter()
        result = array_operations()
        end_time = time.perf_counter()

        end_memory = process.memory_info().rss / 1024 / 1024
        memory_used = end_memory - start_memory

        execution_time_ms = (end_time - start_time) * 1000
        throughput = 100000 / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "max_memory_mb": max(memory_used, 0),  # Ensure non-negative
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("array_operations", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"Array ops: {execution_time_ms:.3f}ms, {memory_used:.1f}MB, {throughput:.0f} ops/sec")

    def test_dataframe_operations_baseline(self, baseline_manager):
        """Test pandas DataFrame operations performance baseline."""

        def dataframe_operations():
            """Pandas DataFrame operations."""
            size = 10000
            df = pd.DataFrame({
                'A': np.random.randn(size),
                'B': np.random.randn(size),
                'C': np.random.randint(0, 100, size),
                'D': np.random.choice(['X', 'Y', 'Z'], size)
            })

            # Various DataFrame operations
            df['E'] = df['A'] * df['B']
            df_grouped = df.groupby('D').agg({
                'A': ['mean', 'std'],
                'B': 'sum',
                'C': 'max'
            })

            result = df_grouped.to_dict()
            return len(str(result))

        # Measure performance
        import psutil
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024

        start_time = time.perf_counter()
        result = dataframe_operations()
        end_time = time.perf_counter()

        end_memory = process.memory_info().rss / 1024 / 1024
        memory_used = end_memory - start_memory

        execution_time_ms = (end_time - start_time) * 1000
        throughput = 10000 / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "max_memory_mb": max(memory_used, 0),
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("dataframe_operations", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"DataFrame ops: {execution_time_ms:.3f}ms, {memory_used:.1f}MB, {throughput:.0f} ops/sec")

    def test_file_io_baseline(self, baseline_manager, tmp_path):
        """Test file I/O operations performance baseline."""

        def file_io_operations():
            """File I/O operations."""
            # Create test data
            data = pd.DataFrame({
                'values': np.random.randn(1000),
                'categories': np.random.choice(['A', 'B', 'C'], 1000)
            })

            # Write and read operations
            csv_file = tmp_path / "test_data.csv"
            data.to_csv(csv_file, index=False)

            # Read back
            read_data = pd.read_csv(csv_file)

            return len(read_data)

        # Measure performance
        start_time = time.perf_counter()
        result = file_io_operations()
        end_time = time.perf_counter()

        execution_time_ms = (end_time - start_time) * 1000
        throughput = 1000 / (end_time - start_time)  # rows per second

        metrics = {
            "max_time_ms": execution_time_ms,
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("file_io", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"File I/O: {execution_time_ms:.3f}ms, {throughput:.0f} rows/sec")

    def test_statistical_calculations_baseline(self, baseline_manager):
        """Test statistical calculations performance baseline."""

        def statistical_calculations():
            """Statistical calculations."""
            data_sizes = [1000, 5000, 10000]
            results = []

            for size in data_sizes:
                data = np.random.randn(size)

                # Statistical operations
                mean_val = np.mean(data)
                std_val = np.std(data)
                median_val = np.median(data)
                q25 = np.percentile(data, 25)
                q75 = np.percentile(data, 75)

                results.append({
                    'size': size,
                    'mean': mean_val,
                    'std': std_val,
                    'median': median_val,
                    'iqr': q75 - q25
                })

            return len(results)

        # Measure performance
        start_time = time.perf_counter()
        result = statistical_calculations()
        end_time = time.perf_counter()

        execution_time_ms = (end_time - start_time) * 1000
        total_operations = sum([1000, 5000, 10000])
        throughput = total_operations / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("statistical_calculations", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"Statistics: {execution_time_ms:.3f}ms, {throughput:.0f} vals/sec")

    def test_matrix_operations_baseline(self, baseline_manager):
        """Test matrix operations performance baseline."""

        def matrix_operations():
            """Matrix operations."""
            sizes = [100, 200, 300]
            results = []

            for size in sizes:
                # Create matrices
                A = np.random.randn(size, size)
                B = np.random.randn(size, size)

                # Matrix operations
                C = np.dot(A, B)
                eigenvals = np.linalg.eigvals(A)
                det_A = np.linalg.det(A)

                results.append({
                    'size': size,
                    'determinant': det_A,
                    'max_eigenval': np.max(np.real(eigenvals))
                })

            return len(results)

        # Measure performance
        import psutil
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024

        start_time = time.perf_counter()
        result = matrix_operations()
        end_time = time.perf_counter()

        end_memory = process.memory_info().rss / 1024 / 1024
        memory_used = end_memory - start_memory

        execution_time_ms = (end_time - start_time) * 1000
        total_elements = sum([size * size for size in [100, 200, 300]])
        throughput = total_elements / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "max_memory_mb": max(memory_used, 0),
            "target_throughput": throughput
        }

        # Validate against baseline
        validation = baseline_manager.validate_performance("matrix_operations", metrics)
        assert validation["status"], f"Performance regression: {validation.get('violations', [])}"

        print(f"Matrix ops: {execution_time_ms:.3f}ms, {memory_used:.1f}MB, {throughput:.0f} elements/sec")

    @pytest.mark.slow
    def test_comprehensive_performance_suite(self, baseline_manager):
        """Run comprehensive performance validation suite."""

        def comprehensive_operations():
            """Comprehensive operations combining multiple performance aspects."""
            # Data generation
            n_samples = 50000
            data = {
                'numerical': np.random.randn(n_samples),
                'categorical': np.random.choice(['A', 'B', 'C', 'D'], n_samples),
                'timestamps': pd.date_range('2023-01-01', periods=n_samples, freq='1min')
            }

            # Create DataFrame
            df = pd.DataFrame(data)

            # Complex operations
            df['rolling_mean'] = df['numerical'].rolling(window=100).mean()
            df['category_encoded'] = pd.Categorical(df['categorical']).codes

            # Statistical analysis
            grouped_stats = df.groupby('categorical').agg({
                'numerical': ['mean', 'std', 'min', 'max'],
                'category_encoded': 'count'
            })

            # Matrix operations on grouped results
            numeric_data = grouped_stats.select_dtypes(include=[np.number])
            correlation_matrix = numeric_data.corr()

            # File I/O simulation
            result_summary = {
                'total_samples': len(df),
                'groups': len(grouped_stats),
                'correlation_trace': np.trace(correlation_matrix.values)
            }

            return result_summary

        # Measure comprehensive performance
        import psutil
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024

        start_time = time.perf_counter()
        result = comprehensive_operations()
        end_time = time.perf_counter()

        end_memory = process.memory_info().rss / 1024 / 1024
        memory_used = end_memory - start_memory

        execution_time_ms = (end_time - start_time) * 1000
        throughput = result['total_samples'] / (end_time - start_time)

        metrics = {
            "max_time_ms": execution_time_ms,
            "max_memory_mb": max(memory_used, 0),
            "target_throughput": throughput
        }

        # More lenient thresholds for comprehensive test
        baseline = {
            "max_time_ms": 500.0,
            "max_memory_mb": 200.0,
            "target_throughput": 10000.0
        }

        # Validate each metric
        for metric, value in metrics.items():
            threshold = baseline[metric]
            if metric.startswith("max_"):
                assert value <= threshold, f"Comprehensive test {metric}: {value:.3f} > {threshold:.3f}"
            elif metric.startswith("target_"):
                assert value >= threshold, f"Comprehensive test {metric}: {value:.3f} < {threshold:.3f}"

        print(f"Comprehensive: {execution_time_ms:.3f}ms, {memory_used:.1f}MB, {throughput:.0f} samples/sec")

    def test_save_performance_baselines(self, baseline_manager):
        """Save current performance baselines for future comparisons."""

        # This test can be run manually to establish new baselines
        # It should not fail, but rather update the baseline file

        print("Current baselines:")
        for operation, metrics in baseline_manager.baselines.items():
            print(f"  {operation}:")
            for metric, value in metrics.items():
                print(f"    {metric}: {value}")

        # Save baselines
        baseline_manager.save_baselines()
        print(f"Baselines saved to: {baseline_manager.baseline_file}")

        # Verify file was created
        assert baseline_manager.baseline_file.exists()


class TestPerformanceRegression:
    """Tests specifically for performance regression detection."""

    def test_performance_trend_analysis(self):
        """Analyze performance trends over time."""

        # This would typically load historical performance data
        # and detect trends/regressions

        historical_data = {
            "2024-01-01": {"basic_math": 0.8, "array_ops": 8.2},
            "2024-01-15": {"basic_math": 0.9, "array_ops": 8.5},
            "2024-02-01": {"basic_math": 0.85, "array_ops": 8.1},
            "2024-02-15": {"basic_math": 0.82, "array_ops": 8.3}
        }

        # Simple trend analysis
        for operation in ["basic_math", "array_ops"]:
            values = [data[operation] for data in historical_data.values()]

            # Check for significant regression (>20% increase in time)
            if len(values) >= 2:
                recent_avg = statistics.mean(values[-2:])  # Last 2 measurements
                baseline_avg = statistics.mean(values[:2])  # First 2 measurements

                regression_ratio = recent_avg / baseline_avg
                assert regression_ratio < 1.2, f"Performance regression detected for {operation}: {regression_ratio:.2f}x slower"

                print(f"{operation}: {regression_ratio:.2f}x (baseline: {baseline_avg:.2f}, recent: {recent_avg:.2f})")

    def test_performance_variance_analysis(self):
        """Test that performance variance is within acceptable limits."""

        def measure_operation_variance():
            """Measure variance in operation performance."""
            times = []

            # Run same operation multiple times
            for _ in range(10):
                start = time.perf_counter()
                result = sum(i ** 2 for i in range(10000))
                end = time.perf_counter()
                times.append((end - start) * 1000)  # Convert to ms

            return times

        execution_times = measure_operation_variance()

        # Statistical analysis
        mean_time = statistics.mean(execution_times)
        std_time = statistics.stdev(execution_times)
        cv = std_time / mean_time  # Coefficient of variation

        # Performance should be consistent (CV < 0.3)
        assert cv < 0.3, f"High performance variance: CV={cv:.3f}, times={execution_times}"

        print(f"Performance variance: mean={mean_time:.3f}ms, std={std_time:.3f}ms, CV={cv:.3f}")


if __name__ == "__main__":
    # Demo performance baseline testing
    print("Running performance baseline validation...")

    manager = PerformanceBaselineManager()

    # Example operation measurement
    start = time.perf_counter()
    result = sum(i ** 2 for i in range(10000))
    end = time.perf_counter()

    execution_time = (end - start) * 1000
    throughput = 10000 / (end - start)

    print(f"Sample operation: {execution_time:.3f}ms, {throughput:.0f} ops/sec")

    # Validate against baseline
    metrics = {"max_time_ms": execution_time, "target_throughput": throughput}
    validation = manager.validate_performance("basic_math", metrics)

    print(f"Validation: {validation}")
    print("Performance baseline demo completed!")