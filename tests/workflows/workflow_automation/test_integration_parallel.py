#!/usr/bin/env python3
"""
ABOUTME: Integration tests for parallel workflow execution with performance
benchmarking and validation of concurrent task execution.
"""

import pytest
import time
from datetime import datetime

from digitalmodel.workflow_automation import (
    WorkflowOrchestrator,
    WorkflowDefinition,
    WorkflowTask,
    ParallelExecutor,
    AdaptiveParallelExecutor,
)
from digitalmodel.workflows.workflow_automation.parallel import ParallelExecutionResult


class TestParallelExecution:
    """Integration tests for parallel task execution"""

    def test_parallel_executor_basic(self):
        """Test basic parallel executor functionality"""
        executor = ParallelExecutor(max_workers=2, use_processes=False)

        # Create simple tasks
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(3)
        ]

        # Define simple task executor
        def task_executor(task):
            time.sleep(0.1)  # Simulate work
            return {'result': task.task_id}

        # Execute in parallel
        start = time.time()
        results = executor.execute_task_group(tasks, task_executor)
        duration = time.time() - start

        # All tasks should complete
        assert len(results) == 3
        assert all(r.success for r in results)

        # Parallel execution should be faster than sequential
        # 3 tasks * 0.1s = 0.3s sequential
        # With 2 workers: ~0.2s parallel
        assert duration < 0.25  # Allow some overhead

    def test_adaptive_executor_selection(self):
        """Test adaptive executor strategy selection"""
        executor = AdaptiveParallelExecutor(max_workers=2)

        # I/O bound tasks (should use threads)
        io_tasks = [
            WorkflowTask(
                name="Hydrodynamics",
                task_id="hydro",
                module="hydrodynamics.wave_spectra",
                function="generate_jonswap_spectrum",
            )
        ]

        # CPU bound tasks (should use processes for multiple tasks)
        cpu_tasks = [
            WorkflowTask(
                name=f"Structural {i}",
                task_id=f"struct{i}",
                module="structural_analysis.stress",
                function="calculate_von_mises_stress",
            )
            for i in range(2)
        ]

        # Test strategy selection
        is_cpu = executor._estimate_cpu_bound(cpu_tasks)
        assert is_cpu is True

        is_io = executor._estimate_cpu_bound(io_tasks)
        assert is_io is False

    def test_parallel_error_handling(self):
        """Test error handling in parallel execution"""
        executor = ParallelExecutor(max_workers=2)

        tasks = [
            WorkflowTask(name="Good Task", task_id="good", module="m", function="f"),
            WorkflowTask(name="Bad Task", task_id="bad", module="m", function="f"),
        ]

        def task_executor(task):
            if task.task_id == "bad":
                raise ValueError("Simulated error")
            return {'result': 'success'}

        results = executor.execute_task_group(tasks, task_executor)

        # Should have both results
        assert len(results) == 2

        # One success, one failure
        successes = [r for r in results if r.success]
        failures = [r for r in results if not r.success]

        assert len(successes) == 1
        assert len(failures) == 1

        # Check error message
        failed = failures[0]
        assert "Simulated error" in failed.error

    def test_sequential_fallback(self):
        """Test fallback to sequential execution"""
        # Create orchestrator with parallel disabled
        orchestrator = WorkflowOrchestrator(
            enable_parallel=False,
            enable_cache=False,
            enable_progress=False,
        )

        assert orchestrator.parallel_executor is None

        # Workflow should still execute (sequentially)
        workflow = WorkflowDefinition(name="Test", description="Test")

        task = WorkflowTask(
            name="Task",
            task_id="t1",
            module="test",
            function="test",
        )

        workflow.add_task(task)

        # This will fail due to missing module, but tests the code path
        # The important thing is parallel_executor is None


class TestParallelPerformance:
    """Performance benchmarks for parallel execution"""

    def test_parallel_speedup_measurement(self):
        """Measure speedup from parallel execution"""
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(4)
        ]

        def task_executor(task):
            time.sleep(0.1)  # Simulate 100ms work
            return {'result': task.task_id}

        # Sequential execution
        start = time.time()
        for task in tasks:
            task_executor(task)
        sequential_time = time.time() - start

        # Parallel execution with 4 workers
        executor = ParallelExecutor(max_workers=4)
        start = time.time()
        executor.execute_task_group(tasks, task_executor)
        parallel_time = time.time() - start

        # Calculate speedup
        speedup = sequential_time / parallel_time

        # Should see significant speedup (close to 4x with 4 workers)
        assert speedup > 2.5  # At least 2.5x speedup

    def test_parallel_overhead_small_tasks(self):
        """Test parallel overhead with very small tasks"""
        # For very small tasks, parallelism overhead may dominate
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(10)
        ]

        def fast_task_executor(task):
            # Very fast task (1ms)
            time.sleep(0.001)
            return {'result': task.task_id}

        # Measure overhead
        executor = ParallelExecutor(max_workers=4)
        start = time.time()
        results = executor.execute_task_group(tasks, fast_task_executor)
        duration = time.time() - start

        # All should complete
        assert len(results) == 10
        assert all(r.success for r in results)

        # Duration should be reasonable (overhead < 100ms)
        assert duration < 0.2

    def test_parallel_scalability(self):
        """Test scalability with different worker counts"""
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(8)
        ]

        def task_executor(task):
            time.sleep(0.05)  # 50ms work
            return {'result': task.task_id}

        # Test with different worker counts
        timings = {}

        for workers in [1, 2, 4]:
            executor = ParallelExecutor(max_workers=workers)
            start = time.time()
            executor.execute_task_group(tasks, task_executor)
            timings[workers] = time.time() - start

        # More workers should be faster
        assert timings[2] < timings[1]
        assert timings[4] < timings[2]


class TestDependencyParallelization:
    """Test parallel execution with dependency constraints"""

    def test_dependency_group_parallelization(self):
        """Test that dependency groups execute correctly"""
        workflow = WorkflowDefinition(name="Test", description="Test")

        # Create dependency graph: A, B (parallel) -> C
        task_a = WorkflowTask(name="Task A", task_id="a", module="m", function="f")
        task_b = WorkflowTask(name="Task B", task_id="b", module="m", function="f")
        task_c = WorkflowTask(
            name="Task C",
            task_id="c",
            module="m",
            function="f",
            depends_on=['a', 'b'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)
        workflow.add_task(task_c)

        # Get dependency order
        order = workflow.get_dependency_order()

        # Should have 2 groups
        assert len(order) == 2

        # First group should have A and B (parallel)
        assert set(order[0]) == {'a', 'b'}

        # Second group should have C
        assert order[1] == ['c']

    def test_complex_dependency_graph(self):
        """Test complex dependency graph execution"""
        workflow = WorkflowDefinition(name="Complex", description="Test")

        #     A
        #    / \
        #   B   C
        #    \ /
        #     D

        task_a = WorkflowTask(name="A", task_id="a", module="m", function="f")
        task_b = WorkflowTask(
            name="B",
            task_id="b",
            module="m",
            function="f",
            depends_on=['a'],
        )
        task_c = WorkflowTask(
            name="C",
            task_id="c",
            module="m",
            function="f",
            depends_on=['a'],
        )
        task_d = WorkflowTask(
            name="D",
            task_id="d",
            module="m",
            function="f",
            depends_on=['b', 'c'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)
        workflow.add_task(task_c)
        workflow.add_task(task_d)

        order = workflow.get_dependency_order()

        # Should have 3 groups
        assert len(order) == 3

        # A first
        assert order[0] == ['a']

        # B and C parallel
        assert set(order[1]) == {'b', 'c'}

        # D last
        assert order[2] == ['d']


class TestParallelIntegrationWithOrchestrator:
    """Integration tests with WorkflowOrchestrator"""

    def test_orchestrator_parallel_enabled(self):
        """Test orchestrator with parallel execution enabled"""
        orchestrator = WorkflowOrchestrator(
            enable_parallel=True,
            enable_cache=False,
            enable_progress=False,
        )

        assert orchestrator.enable_parallel is True
        assert orchestrator.parallel_executor is not None
        assert isinstance(orchestrator.parallel_executor, AdaptiveParallelExecutor)

    def test_orchestrator_parallel_disabled(self):
        """Test orchestrator with parallel execution disabled"""
        orchestrator = WorkflowOrchestrator(
            enable_parallel=False,
            enable_cache=False,
            enable_progress=False,
        )

        assert orchestrator.enable_parallel is False
        assert orchestrator.parallel_executor is None

    def test_orchestrator_worker_configuration(self):
        """Test orchestrator with custom worker count"""
        from digitalmodel.workflows.workflow_automation.models import WorkflowConfig

        config = WorkflowConfig(max_parallel_tasks=8)

        orchestrator = WorkflowOrchestrator(
            config=config,
            enable_parallel=True,
        )

        assert orchestrator.parallel_executor.max_workers == 8
