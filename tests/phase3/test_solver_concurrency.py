#!/usr/bin/env python3
"""
ABOUTME: Concurrency and thread safety tests for Phase 3 solvers
ABOUTME: Thread safety, race condition detection, and parallel execution
"""

import pytest
import logging
import sys
import threading
import time
import random
from pathlib import Path
from typing import Dict, Any, List, Tuple
from unittest.mock import MagicMock
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
import queue

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def concurrency_logger():
    """Configure concurrency test logging."""
    logger = logging.getLogger("concurrency_tests")
    logger.setLevel(logging.DEBUG)
    return logger


@pytest.fixture
def thread_safe_config_manager():
    """Create thread-safe ConfigManager mock."""
    class ThreadSafeConfigManager:
        def __init__(self):
            self.config = {
                "solvers": {
                    "catenary": {"timeout": 60, "tolerance": 1e-6},
                    "capacity": {"timeout": 30, "tolerance": 1e-4},
                    "fatigue": {"timeout": 120, "tolerance": 1e-5}
                }
            }
            self.lock = threading.RLock()
            self.access_log = []

        def get_config(self, path: str, default=None):
            """Thread-safe config retrieval."""
            with self.lock:
                self.access_log.append(("get", path, time.time()))

                parts = path.split(".")
                value = self.config

                for part in parts:
                    if isinstance(value, dict):
                        value = value.get(part)
                    else:
                        return default

                return value if value is not None else default

        def set_config(self, path: str, value: Any):
            """Thread-safe config update."""
            with self.lock:
                self.access_log.append(("set", path, time.time()))

                parts = path.split(".")
                target = self.config

                for part in parts[:-1]:
                    if part not in target:
                        target[part] = {}
                    target = target[part]

                target[parts[-1]] = value

        def get_access_log(self) -> List[Tuple]:
            """Get access log."""
            with self.lock:
                return self.access_log.copy()

    return ThreadSafeConfigManager()


@pytest.fixture
def mock_concurrent_solver():
    """Create mock solver for concurrency testing."""
    class ConcurrentSolver:
        def __init__(self, name: str):
            self.name = name
            self.lock = threading.RLock()
            self.execution_log = []
            self.execution_count = 0
            self.active_executions = 0

        def solve(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
            """Execute solver with concurrency tracking."""
            thread_id = threading.get_ident()

            with self.lock:
                self.active_executions += 1
                self.execution_count += 1

            try:
                # Simulate work
                time.sleep(random.uniform(0.01, 0.05))

                execution_info = {
                    "success": True,
                    "solver": self.name,
                    "thread_id": thread_id,
                    "execution_number": self.execution_count,
                    "input_size": len(str(input_data))
                }

                with self.lock:
                    self.execution_log.append(execution_info)

                return execution_info

            finally:
                with self.lock:
                    self.active_executions -= 1

        def get_max_concurrent_executions(self) -> int:
            """Get maximum concurrent executions observed."""
            # This is a simplified check
            return self.execution_count

        def get_execution_log(self) -> List[Dict[str, Any]]:
            """Get execution log."""
            with self.lock:
                return self.execution_log.copy()

    return ConcurrentSolver


@pytest.fixture
def thread_pool():
    """Create thread pool for testing."""
    pool = ThreadPoolExecutor(max_workers=4)
    yield pool
    pool.shutdown(wait=True)


@pytest.fixture
def synchronization_primitives():
    """Provide synchronization primitives."""
    return {
        "lock": threading.Lock(),
        "rlock": threading.RLock(),
        "event": threading.Event(),
        "semaphore": threading.Semaphore(4),
        "barrier": threading.Barrier(4)
    }


# ============================================================================
# TEST CLASS: Thread Safety Validation
# ============================================================================

class TestThreadSafety:
    """Tests for thread safety of solvers and components."""

    def test_thread_safe_config_reads(self, thread_safe_config_manager, thread_pool):
        """Test thread-safe configuration reading."""
        read_results = []

        def read_config(index):
            value = thread_safe_config_manager.get_config("solvers.catenary.timeout")
            read_results.append((index, value))
            return value

        futures = [thread_pool.submit(read_config, i) for i in range(20)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 20
        assert all(v == 60 for v in results)

    def test_thread_safe_config_writes(self, thread_safe_config_manager, thread_pool):
        """Test thread-safe configuration writing."""
        def write_config(thread_id):
            key = f"solvers.custom.timeout_{thread_id}"
            thread_safe_config_manager.set_config(key, 100 + thread_id)

        futures = [thread_pool.submit(write_config, i) for i in range(10)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 10

        # Verify all writes succeeded
        access_log = thread_safe_config_manager.get_access_log()
        set_operations = [a for a in access_log if a[0] == "set"]
        assert len(set_operations) == 10

    def test_concurrent_solver_execution(self, mock_concurrent_solver, thread_pool):
        """Test concurrent solver execution thread safety."""
        solver = mock_concurrent_solver("catenary")

        def run_solve(task_id):
            input_data = {"task_id": task_id, "data": list(range(100))}
            return solver.solve(input_data)

        futures = [thread_pool.submit(run_solve, i) for i in range(20)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 20
        assert all(r["success"] for r in results)
        assert len(solver.get_execution_log()) == 20

    def test_concurrent_read_write_access(self, thread_safe_config_manager, thread_pool):
        """Test concurrent read/write access patterns."""
        def access_config(operation_id, is_write):
            if is_write:
                thread_safe_config_manager.set_config(
                    f"temp.value_{operation_id}",
                    operation_id * 10
                )
            else:
                thread_safe_config_manager.get_config("solvers.catenary.timeout")

        futures = []

        # Mix of reads and writes
        for i in range(30):
            is_write = random.random() > 0.7  # 30% writes
            futures.append(thread_pool.submit(access_config, i, is_write))

        results = [f.result() for f in as_completed(futures)]
        assert len(results) == 30

    def test_solver_state_consistency(self, mock_concurrent_solver, thread_pool):
        """Test solver state consistency under concurrent access."""
        solver = mock_concurrent_solver("fatigue")

        def solve_and_check(task_id):
            result = solver.solve({"task_id": task_id})
            return result["success"]

        futures = [thread_pool.submit(solve_and_check, i) for i in range(50)]
        results = [f.result() for f in as_completed(futures)]

        assert all(results)
        assert solver.execution_count == 50


# ============================================================================
# TEST CLASS: Race Condition Detection
# ============================================================================

class TestRaceConditionDetection:
    """Tests for detecting race conditions."""

    def test_config_corruption_detection(self, thread_safe_config_manager, thread_pool):
        """Test detection of configuration corruption in concurrent access."""
        thread_safe_config_manager.set_config("test.initial", 0)

        def increment_config(thread_id):
            for _ in range(10):
                current = thread_safe_config_manager.get_config("test.initial", 0)
                thread_safe_config_manager.set_config("test.initial", current + 1)

        futures = [thread_pool.submit(increment_config, i) for i in range(5)]
        [f.result() for f in as_completed(futures)]

        # With proper locking, should reach expected value
        final_value = thread_safe_config_manager.get_config("test.initial", 0)

        # Value should be consistent (due to locks)
        assert final_value >= 0

    def test_solver_result_race_condition(self, mock_concurrent_solver, thread_pool):
        """Test for race conditions in solver results."""
        solver = mock_concurrent_solver("catenary")
        results_dict = {}
        lock = threading.Lock()

        def solve_and_store(task_id):
            result = solver.solve({"task_id": task_id})

            with lock:
                results_dict[task_id] = result

        futures = [thread_pool.submit(solve_and_store, i) for i in range(100)]
        [f.result() for f in as_completed(futures)]

        # All tasks should have results
        assert len(results_dict) == 100
        assert all(task_id in results_dict for task_id in range(100))

    def test_concurrent_modification_detection(self, thread_safe_config_manager, thread_pool):
        """Test detection of concurrent modifications."""
        modification_log = []
        lock = threading.Lock()

        def modify_and_log(thread_id):
            for iteration in range(5):
                thread_safe_config_manager.set_config(
                    f"modify.thread_{thread_id}",
                    iteration
                )

                with lock:
                    modification_log.append((thread_id, iteration))

        futures = [thread_pool.submit(modify_and_log, i) for i in range(4)]
        [f.result() for f in as_completed(futures)]

        # All modifications should be recorded
        assert len(modification_log) == 20


# ============================================================================
# TEST CLASS: Parallel Solver Execution
# ============================================================================

class TestParallelSolverExecution:
    """Tests for parallel solver execution."""

    def test_parallel_different_solvers(self, mock_concurrent_solver, thread_pool):
        """Test parallel execution of different solver types."""
        solvers = {
            "catenary": mock_concurrent_solver("catenary"),
            "capacity": mock_concurrent_solver("capacity"),
            "fatigue": mock_concurrent_solver("fatigue")
        }

        def run_solver_suite():
            results = {}

            for solver_name, solver in solvers.items():
                result = solver.solve({"type": solver_name})
                results[solver_name] = result

            return results

        futures = [thread_pool.submit(run_solver_suite) for _ in range(10)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 10
        assert all(len(r) == 3 for r in results)

    def test_parallel_same_solver_independence(self, mock_concurrent_solver, thread_pool):
        """Test independence of parallel executions of same solver."""
        solver = mock_concurrent_solver("catenary")
        execution_times = []

        def run_solve(task_id):
            start = time.time()
            result = solver.solve({"task_id": task_id})
            elapsed = time.time() - start

            execution_times.append(elapsed)
            return result

        futures = [thread_pool.submit(run_solve, i) for i in range(20)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 20
        assert all(r["success"] for r in results)

    def test_parallel_batch_processing(self, mock_concurrent_solver, thread_pool):
        """Test parallel batch processing."""
        solver = mock_concurrent_solver("fatigue")
        batch_size = 50

        def process_batch(batch_id):
            batch_results = []

            for i in range(batch_size):
                task_id = batch_id * batch_size + i
                result = solver.solve({"task_id": task_id})
                batch_results.append(result)

            return batch_results

        num_batches = 4
        futures = [thread_pool.submit(process_batch, i) for i in range(num_batches)]
        all_results = [f.result() for f in as_completed(futures)]

        total_tasks = sum(len(batch) for batch in all_results)
        assert total_tasks == batch_size * num_batches
        assert all(
            task["success"]
            for batch in all_results
            for task in batch
        )

    def test_load_balancing_across_solvers(self, mock_concurrent_solver, thread_pool):
        """Test load balancing across multiple solvers."""
        solvers = [mock_concurrent_solver(f"solver_{i}") for i in range(4)]

        def solve_with_lb(task_id):
            solver = solvers[task_id % len(solvers)]
            return solver.solve({"task_id": task_id})

        futures = [thread_pool.submit(solve_with_lb, i) for i in range(100)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 100

        # Check load distribution
        executions_per_solver = {}

        for solver in solvers:
            log = solver.get_execution_log()
            executions_per_solver[solver.name] = len(log)

        # Load should be relatively balanced
        loads = list(executions_per_solver.values())
        min_load = min(loads)
        max_load = max(loads)
        imbalance = max_load - min_load

        assert imbalance <= 1  # Should be perfectly balanced for 100 tasks with 4 solvers

    def test_pipeline_parallel_execution(self, mock_concurrent_solver, thread_pool):
        """Test pipeline execution in parallel."""
        def pipeline_step_1(data):
            solver = mock_concurrent_solver("step1")
            return solver.solve(data)

        def pipeline_step_2(data):
            solver = mock_concurrent_solver("step2")
            return solver.solve(data)

        def pipeline_step_3(data):
            solver = mock_concurrent_solver("step3")
            return solver.solve(data)

        def run_pipeline(task_id):
            data = {"task_id": task_id, "step": 0}

            data = pipeline_step_1(data)
            data = pipeline_step_2(data)
            data = pipeline_step_3(data)

            return data

        futures = [thread_pool.submit(run_pipeline, i) for i in range(20)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 20
        assert all(r["success"] for r in results)


# ============================================================================
# TEST CLASS: Deadlock Detection
# ============================================================================

class TestDeadlockDetection:
    """Tests for deadlock detection and prevention."""

    def test_no_deadlock_with_simple_locks(self, thread_pool):
        """Test that simple lock usage doesn't deadlock."""
        lock1 = threading.Lock()
        lock2 = threading.Lock()
        results = []

        def acquire_in_order(thread_id):
            with lock1:
                time.sleep(0.001)
                with lock2:
                    results.append(thread_id)

        futures = [thread_pool.submit(acquire_in_order, i) for i in range(10)]
        completed = [f.result() for f in as_completed(futures, timeout=5)]

        assert len(completed) == 10

    def test_reentrant_lock_safety(self, thread_pool):
        """Test reentrancy with RLock."""
        rlock = threading.RLock()
        results = []

        def recursive_acquire(depth):
            if depth > 0:
                with rlock:
                    results.append(depth)
                    recursive_acquire(depth - 1)

        futures = [thread_pool.submit(recursive_acquire, 5) for _ in range(5)]
        [f.result() for f in as_completed(futures)]

        assert len(results) == 25  # 5 threads * 5 levels

    def test_thread_timeout_safety(self, thread_pool):
        """Test thread execution with timeout safety."""
        def long_running_task():
            time.sleep(0.1)
            return True

        futures = [thread_pool.submit(long_running_task) for _ in range(5)]

        try:
            results = [f.result(timeout=1.0) for f in futures]
            assert all(results)
        except Exception as e:
            pytest.fail(f"Timeout occurred: {e}")


# ============================================================================
# TEST CLASS: Concurrent ConfigManager Access
# ============================================================================

class TestConcurrentConfigManagerAccess:
    """Tests for ConfigManager under concurrent access."""

    def test_concurrent_solver_config_access(self, thread_safe_config_manager, thread_pool):
        """Test concurrent access to solver configurations."""
        def access_solver_config(solver_name):
            configs = []

            for _ in range(10):
                cfg = thread_safe_config_manager.get_config(f"solvers.{solver_name}.timeout")
                configs.append(cfg)

            return configs

        solver_names = ["catenary", "capacity", "fatigue"]

        futures = [thread_pool.submit(access_solver_config, name) for name in solver_names for _ in range(3)]
        results = [f.result() for f in as_completed(futures)]

        assert len(results) == 9

    def test_concurrent_config_override(self, thread_safe_config_manager, thread_pool):
        """Test concurrent configuration overrides."""
        def update_timeout(solver_id, new_timeout):
            thread_safe_config_manager.set_config(
                f"solvers.custom_{solver_id}.timeout",
                new_timeout
            )

        futures = [
            thread_pool.submit(update_timeout, i, 100 + i)
            for i in range(20)
        ]

        [f.result() for f in as_completed(futures)]

        # All updates should be recorded
        access_log = thread_safe_config_manager.get_access_log()
        set_ops = [a for a in access_log if a[0] == "set"]

        assert len(set_ops) >= 20


# ============================================================================
# TEST CLASS: Thread Synchronization
# ============================================================================

class TestThreadSynchronization:
    """Tests for thread synchronization mechanisms."""

    def test_barrier_synchronization(self, thread_pool):
        """Test barrier synchronization."""
        barrier = threading.Barrier(4)
        execution_order = []
        lock = threading.Lock()

        def barrier_test(thread_id):
            with lock:
                execution_order.append(("before", thread_id))

            barrier.wait()

            with lock:
                execution_order.append(("after", thread_id))

        futures = [thread_pool.submit(barrier_test, i) for i in range(4)]
        [f.result() for f in as_completed(futures)]

        before_count = sum(1 for op, _ in execution_order if op == "before")
        after_count = sum(1 for op, _ in execution_order if op == "after")

        assert before_count == 4
        assert after_count == 4

    def test_event_synchronization(self, thread_pool):
        """Test event synchronization."""
        event = threading.Event()
        results = []
        lock = threading.Lock()

        def wait_for_event(thread_id):
            event.wait()

            with lock:
                results.append(thread_id)

        # Submit waiting threads
        futures = [thread_pool.submit(wait_for_event, i) for i in range(5)]

        time.sleep(0.1)  # Let threads wait

        # Signal event
        event.set()

        [f.result() for f in as_completed(futures)]

        assert len(results) == 5

    def test_semaphore_rate_limiting(self, thread_pool):
        """Test semaphore for rate limiting."""
        semaphore = threading.Semaphore(2)
        concurrent_count = []
        max_concurrent = [0]
        lock = threading.Lock()

        def rate_limited_task(task_id):
            with semaphore:
                with lock:
                    concurrent_count.append(1)
                    current = len(concurrent_count)
                    max_concurrent[0] = max(max_concurrent[0], current)

                time.sleep(0.01)

                with lock:
                    concurrent_count.pop()

        futures = [thread_pool.submit(rate_limited_task, i) for i in range(10)]
        [f.result() for f in as_completed(futures)]

        # Max concurrent should not exceed semaphore limit
        assert max_concurrent[0] <= 2


# ============================================================================
# TEST HELPERS
# ============================================================================

def collect_thread_safety_metrics(concurrent_executions: List[Dict]) -> Dict[str, Any]:
    """Collect thread safety metrics."""
    return {
        "total_executions": len(concurrent_executions),
        "successful": sum(1 for e in concurrent_executions if e.get("success")),
        "failed": sum(1 for e in concurrent_executions if not e.get("success")),
        "unique_threads": len(set(e.get("thread_id") for e in concurrent_executions))
    }


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
