"""
Load testing scenarios for DigitalModel components.

Tests system behavior under various load conditions using techniques
from Netflix's Chaos Engineering and Google's load testing practices.
"""
import pytest
import time
import threading
import queue
import concurrent.futures
from typing import List, Dict, Any
import numpy as np
import pandas as pd
from pathlib import Path
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class LoadTestRunner:
    """Runner for load testing scenarios."""

    def __init__(self):
        self.results = []
        self.errors = []

    def run_load_test(self, target_function, num_workers: int, duration_seconds: int,
                     requests_per_second: int = None, *args, **kwargs):
        """
        Run load test with specified parameters.

        Args:
            target_function: Function to test under load
            num_workers: Number of concurrent workers
            duration_seconds: How long to run the test
            requests_per_second: Target RPS (None for unlimited)
        """
        results = queue.Queue()
        errors = queue.Queue()
        stop_event = threading.Event()

        def worker():
            """Worker function that repeatedly calls target."""
            request_interval = 1.0 / requests_per_second if requests_per_second else 0
            last_request = time.time()

            while not stop_event.is_set():
                # Rate limiting
                if requests_per_second:
                    time_since_last = time.time() - last_request
                    if time_since_last < request_interval:
                        time.sleep(request_interval - time_since_last)

                try:
                    start_time = time.perf_counter()
                    result = target_function(*args, **kwargs)
                    end_time = time.perf_counter()

                    results.put({
                        'success': True,
                        'response_time': end_time - start_time,
                        'timestamp': time.time(),
                        'result': result
                    })
                    last_request = time.time()

                except Exception as e:
                    errors.put({
                        'error': str(e),
                        'timestamp': time.time(),
                        'type': type(e).__name__
                    })

        # Start workers
        workers = []
        for _ in range(num_workers):
            worker_thread = threading.Thread(target=worker)
            worker_thread.start()
            workers.append(worker_thread)

        # Run for specified duration
        time.sleep(duration_seconds)
        stop_event.set()

        # Wait for workers to finish
        for worker_thread in workers:
            worker_thread.join(timeout=1.0)

        # Collect results
        test_results = []
        test_errors = []

        while not results.empty():
            test_results.append(results.get())

        while not errors.empty():
            test_errors.append(errors.get())

        return self._analyze_results(test_results, test_errors, duration_seconds)

    def _analyze_results(self, results: List[Dict], errors: List[Dict], duration: float):
        """Analyze load test results."""
        if not results:
            return {
                'total_requests': 0,
                'successful_requests': 0,
                'failed_requests': len(errors),
                'error_rate': 1.0 if errors else 0.0,
                'throughput_rps': 0.0,
                'errors': errors
            }

        response_times = [r['response_time'] for r in results]
        response_times.sort()

        analysis = {
            'total_requests': len(results),
            'successful_requests': len(results),
            'failed_requests': len(errors),
            'error_rate': len(errors) / (len(results) + len(errors)),
            'throughput_rps': len(results) / duration,
            'response_time_stats': {
                'mean': np.mean(response_times),
                'median': np.median(response_times),
                'p95': response_times[int(0.95 * len(response_times))],
                'p99': response_times[int(0.99 * len(response_times))],
                'min': min(response_times),
                'max': max(response_times)
            },
            'errors': errors
        }

        return analysis


@pytest.fixture
def load_test_runner():
    """Fixture providing load test runner."""
    return LoadTestRunner()


class TestBasicLoadScenarios:
    """Basic load testing scenarios."""

    @pytest.mark.load_test
    def test_computational_load(self, load_test_runner):
        """Test system under computational load."""

        def cpu_intensive_calculation():
            """Simulate CPU-intensive calculation."""
            # Simulate complex mathematical operation
            data = np.random.randn(1000)
            result = np.fft.fft(data)
            return np.sum(np.abs(result))

        # Test with increasing load
        for workers in [1, 2, 4, 8]:
            results = load_test_runner.run_load_test(
                cpu_intensive_calculation,
                num_workers=workers,
                duration_seconds=5,
                requests_per_second=10  # Moderate rate
            )

            # Assert basic performance requirements
            assert results['error_rate'] < 0.01, f"High error rate with {workers} workers: {results['error_rate']:.2%}"
            assert results['throughput_rps'] > 5, f"Low throughput with {workers} workers: {results['throughput_rps']:.1f} RPS"

            # Response times should be reasonable
            assert results['response_time_stats']['p95'] < 1.0, f"High P95 response time: {results['response_time_stats']['p95']:.3f}s"

            print(f"Workers: {workers}, Throughput: {results['throughput_rps']:.1f} RPS, "
                  f"P95: {results['response_time_stats']['p95']:.3f}s")

    @pytest.mark.load_test
    def test_memory_intensive_load(self, load_test_runner):
        """Test system under memory-intensive load."""

        def memory_intensive_operation():
            """Simulate memory-intensive operation."""
            # Create and process large data structures
            data = pd.DataFrame({
                'col1': np.random.randn(10000),
                'col2': np.random.randn(10000),
                'col3': np.random.randint(0, 1000, 10000)
            })

            # Perform memory-intensive operations
            result = data.groupby('col3').agg({
                'col1': ['mean', 'std', 'min', 'max'],
                'col2': ['sum', 'count']
            })

            return len(result)

        results = load_test_runner.run_load_test(
            memory_intensive_operation,
            num_workers=4,
            duration_seconds=10,
            requests_per_second=2  # Lower rate for memory operations
        )

        # Memory operations should complete successfully
        assert results['error_rate'] < 0.05, f"High error rate in memory test: {results['error_rate']:.2%}"
        assert results['response_time_stats']['mean'] < 2.0, f"Slow memory operations: {results['response_time_stats']['mean']:.2f}s"

    @pytest.mark.load_test
    def test_io_intensive_load(self, load_test_runner, tmp_path):
        """Test system under I/O-intensive load."""

        def io_intensive_operation():
            """Simulate I/O-intensive operation."""
            # Create temporary file
            file_path = tmp_path / f"test_{threading.get_ident()}_{time.time()}.csv"

            # Generate and write data
            data = pd.DataFrame({
                'x': np.random.randn(1000),
                'y': np.random.randn(1000)
            })

            data.to_csv(file_path, index=False)

            # Read it back
            read_data = pd.read_csv(file_path)

            # Clean up
            file_path.unlink(missing_ok=True)

            return len(read_data)

        results = load_test_runner.run_load_test(
            io_intensive_operation,
            num_workers=6,
            duration_seconds=8,
            requests_per_second=5
        )

        # I/O operations should handle concurrent access
        assert results['error_rate'] < 0.02, f"High error rate in I/O test: {results['error_rate']:.2%}"
        assert results['throughput_rps'] > 2, f"Low I/O throughput: {results['throughput_rps']:.1f} RPS"


class TestStressTestScenarios:
    """Stress testing scenarios to find breaking points."""

    @pytest.mark.stress_test
    def test_increasing_load_until_failure(self, load_test_runner):
        """Gradually increase load until system shows stress."""

        def simple_calculation():
            """Simple calculation for stress testing."""
            return sum(i ** 2 for i in range(100))

        max_workers = 1
        max_rps = 0
        degradation_threshold = 0.5  # 500ms response time

        # Gradually increase load
        for workers in [1, 2, 4, 8, 16, 32]:
            results = load_test_runner.run_load_test(
                simple_calculation,
                num_workers=workers,
                duration_seconds=3,
                requests_per_second=None  # Unlimited
            )

            p95_response_time = results['response_time_stats']['p95']
            throughput = results['throughput_rps']

            print(f"Workers: {workers:2d}, RPS: {throughput:6.1f}, "
                  f"P95: {p95_response_time:.3f}s, Error rate: {results['error_rate']:.2%}")

            if (p95_response_time > degradation_threshold or
                results['error_rate'] > 0.01 or
                throughput < max_rps * 0.8):  # Throughput dropped significantly
                print(f"System degradation detected at {workers} workers")
                break

            max_workers = workers
            max_rps = max(max_rps, throughput)

        # System should handle at least 4 concurrent workers
        assert max_workers >= 4, f"System failed with only {max_workers} workers"

    @pytest.mark.stress_test
    def test_sustained_load(self, load_test_runner):
        """Test system under sustained load over longer period."""

        def moderate_calculation():
            """Moderate calculation for sustained testing."""
            data = np.random.randn(500)
            return np.sum(data ** 2)

        # Run sustained load for longer duration
        results = load_test_runner.run_load_test(
            moderate_calculation,
            num_workers=4,
            duration_seconds=30,  # Longer duration
            requests_per_second=10
        )

        # System should maintain performance over time
        assert results['error_rate'] < 0.01, f"High error rate in sustained test: {results['error_rate']:.2%}"
        assert results['throughput_rps'] > 8, f"Low sustained throughput: {results['throughput_rps']:.1f} RPS"

        # Check for performance degradation patterns
        response_times = [r['response_time'] for r in results['response_time_stats'] if 'response_time_stats' in results]

        # Response times should be consistent
        assert results['response_time_stats']['max'] < results['response_time_stats']['mean'] * 5, \
               "Response times show high variance, possible performance degradation"


class TestChaosEngineeringScenarios:
    """Chaos engineering tests inspired by Netflix practices."""

    @pytest.mark.chaos_test
    def test_random_failures(self, load_test_runner):
        """Test system resilience with random failures injected."""
        import random

        def unreliable_function():
            """Function that randomly fails to simulate real-world conditions."""
            # 5% chance of random failure
            if random.random() < 0.05:
                raise Exception("Random failure injected")

            # Normal operation
            return sum(i for i in range(100))

        results = load_test_runner.run_load_test(
            unreliable_function,
            num_workers=8,
            duration_seconds=10,
            requests_per_second=20
        )

        # System should handle some failures gracefully
        assert results['error_rate'] < 0.10, f"Too many errors: {results['error_rate']:.2%}"
        assert results['successful_requests'] > 150, f"Too few successful requests: {results['successful_requests']}"

        print(f"Chaos test results: {results['successful_requests']} successful, "
              f"{results['failed_requests']} failed, {results['error_rate']:.2%} error rate")

    @pytest.mark.chaos_test
    def test_resource_exhaustion_recovery(self, load_test_runner):
        """Test system recovery from resource exhaustion."""

        def resource_heavy_function():
            """Function that uses significant resources."""
            # Allocate memory
            data = np.random.randn(50000)  # ~400KB per call

            # CPU-intensive operation
            result = np.fft.fft(data)

            # Simulate cleanup
            del data

            return len(result)

        # First, run heavy load to exhaust resources
        heavy_results = load_test_runner.run_load_test(
            resource_heavy_function,
            num_workers=16,  # High worker count
            duration_seconds=5,
            requests_per_second=None
        )

        # Then test recovery with normal load
        time.sleep(2)  # Brief recovery period

        normal_results = load_test_runner.run_load_test(
            lambda: sum(range(100)),  # Light function
            num_workers=4,
            duration_seconds=5,
            requests_per_second=10
        )

        # System should recover and perform normally
        assert normal_results['error_rate'] < 0.02, f"Poor recovery: {normal_results['error_rate']:.2%}"
        assert normal_results['response_time_stats']['mean'] < 0.1, \
               f"Slow recovery: {normal_results['response_time_stats']['mean']:.3f}s"

        print(f"Recovery test: Heavy load error rate: {heavy_results['error_rate']:.2%}, "
              f"Recovery error rate: {normal_results['error_rate']:.2%}")


if __name__ == "__main__":
    # Run basic load test demonstration
    runner = LoadTestRunner()

    def demo_function():
        time.sleep(0.01)  # Simulate 10ms operation
        return "success"

    print("Running demo load test...")
    results = runner.run_load_test(
        demo_function,
        num_workers=4,
        duration_seconds=3,
        requests_per_second=50
    )

    print(f"Demo results: {results['throughput_rps']:.1f} RPS, "
          f"P95: {results['response_time_stats']['p95']:.3f}s")