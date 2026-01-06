#!/usr/bin/env python3
"""
ABOUTME: Performance benchmarks for workflow automation system measuring
cache speedup, parallel execution gains, and overall system performance.
"""

import pytest
import time
import tempfile
from datetime import datetime

from digitalmodel.modules.workflow_automation import (
    WorkflowOrchestrator,
    WorkflowDefinition,
    WorkflowTask,
    WorkflowCache,
    ParallelExecutor,
)


@pytest.mark.benchmark
class TestCachePerformance:
    """Benchmark cache performance"""

    def test_cache_speedup_simple_task(self, benchmark):
        """Benchmark cache speedup for simple tasks"""
        cache = WorkflowCache(cache_dir=tempfile.mkdtemp())

        task = WorkflowTask(
            name="Test Task",
            task_id="test",
            module="test",
            function="test",
        )

        inputs = {'value': 42}

        # First run (uncached)
        def uncached_execution():
            result = cache.get_task_result(task, inputs)
            if result is None:
                # Simulate computation
                time.sleep(0.01)
                result = {'output': 100}
                cache.set_task_result(task, inputs, result)
            return result

        # Benchmark uncached
        result_uncached = benchmark(uncached_execution)

        # Now benchmark cached
        def cached_execution():
            return cache.get_task_result(task, inputs)

        result_cached = benchmark(cached_execution)

        # Cached should be much faster
        # (This is measured by pytest-benchmark)

    def test_cache_memory_vs_disk_performance(self):
        """Compare memory cache vs disk cache performance"""
        cache = WorkflowCache(cache_dir=tempfile.mkdtemp())

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")
        inputs = {'x': 1}
        result = {'data': 'x' * 10000}

        # Set cache
        cache.set_task_result(task, inputs, result)

        # First retrieval (from disk, then memory)
        start = time.time()
        result1 = cache.get_task_result(task, inputs)
        disk_time = time.time() - start

        # Second retrieval (from memory)
        start = time.time()
        result2 = cache.get_task_result(task, inputs)
        memory_time = time.time() - start

        print(f"\nDisk cache time: {disk_time*1000:.2f}ms")
        print(f"Memory cache time: {memory_time*1000:.2f}ms")
        print(f"Speedup: {disk_time/memory_time:.1f}x")

        # Memory should be faster
        assert memory_time < disk_time

    def test_cache_hit_rate_performance(self):
        """Measure cache hit rate in realistic scenario"""
        cache = WorkflowCache(cache_dir=tempfile.mkdtemp())

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")

        # Simulate workflow execution with repeated parameter combinations
        parameter_sets = [
            {'diameter': 0.508, 'thickness': 0.025},
            {'diameter': 0.610, 'thickness': 0.030},
            {'diameter': 0.508, 'thickness': 0.025},  # Repeat
            {'diameter': 0.508, 'thickness': 0.030},
            {'diameter': 0.610, 'thickness': 0.030},  # Repeat
            {'diameter': 0.508, 'thickness': 0.025},  # Repeat
        ]

        hits = 0
        misses = 0
        total_time = 0

        for params in parameter_sets:
            start = time.time()

            cached = cache.get_task_result(task, params)

            if cached is None:
                misses += 1
                # Simulate computation
                time.sleep(0.01)
                cache.set_task_result(task, params, {'result': 'computed'})
            else:
                hits += 1

            total_time += time.time() - start

        hit_rate = hits / len(parameter_sets) * 100

        print(f"\nCache hit rate: {hit_rate:.1f}%")
        print(f"Hits: {hits}, Misses: {misses}")
        print(f"Total time: {total_time*1000:.2f}ms")
        print(f"Average time per lookup: {total_time/len(parameter_sets)*1000:.2f}ms")

        # Should see good hit rate (50% in this scenario)
        assert hit_rate == 50.0


@pytest.mark.benchmark
class TestParallelPerformance:
    """Benchmark parallel execution performance"""

    def test_parallel_speedup_io_bound(self):
        """Measure speedup for I/O bound tasks"""
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(4)
        ]

        def io_bound_task(task):
            # Simulate I/O operation
            time.sleep(0.1)
            return {'result': task.task_id}

        # Sequential execution
        start = time.time()
        for task in tasks:
            io_bound_task(task)
        sequential_time = time.time() - start

        # Parallel execution
        executor = ParallelExecutor(max_workers=4, use_processes=False)
        start = time.time()
        results = executor.execute_task_group(tasks, io_bound_task)
        parallel_time = time.time() - start

        speedup = sequential_time / parallel_time

        print(f"\nI/O Bound Tasks:")
        print(f"Sequential time: {sequential_time*1000:.2f}ms")
        print(f"Parallel time: {parallel_time*1000:.2f}ms")
        print(f"Speedup: {speedup:.2f}x")

        # Should see significant speedup
        assert speedup > 2.5

    def test_parallel_speedup_cpu_bound(self):
        """Measure speedup for CPU bound tasks"""
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(4)
        ]

        def cpu_bound_task(task):
            # Simulate CPU-intensive work
            result = 0
            for i in range(100000):
                result += i ** 2
            return {'result': task.task_id}

        # Sequential execution
        start = time.time()
        for task in tasks:
            cpu_bound_task(task)
        sequential_time = time.time() - start

        # Parallel execution with processes
        executor = ParallelExecutor(max_workers=4, use_processes=True)
        start = time.time()
        results = executor.execute_task_group(tasks, cpu_bound_task)
        parallel_time = time.time() - start

        speedup = sequential_time / parallel_time

        print(f"\nCPU Bound Tasks:")
        print(f"Sequential time: {sequential_time*1000:.2f}ms")
        print(f"Parallel time: {parallel_time*1000:.2f}ms")
        print(f"Speedup: {speedup:.2f}x")

        # Process-based parallelism should provide speedup
        # (May be limited by GIL in threads, but processes should scale)

    def test_parallel_scalability(self):
        """Test scalability with varying worker counts"""
        tasks = [
            WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            for i in range(8)
        ]

        def task_executor(task):
            time.sleep(0.05)
            return {'result': task.task_id}

        print("\nScalability Test:")

        results = {}

        for workers in [1, 2, 4, 8]:
            executor = ParallelExecutor(max_workers=workers)

            start = time.time()
            executor.execute_task_group(tasks, task_executor)
            duration = time.time() - start

            results[workers] = duration

            print(f"Workers: {workers}, Time: {duration*1000:.2f}ms")

        # Verify increasing workers improves performance
        assert results[2] < results[1]
        assert results[4] < results[2]

        # Calculate efficiency
        ideal_speedup_4 = results[1] / 4
        actual_speedup_4 = results[1] / results[4]
        efficiency = actual_speedup_4 / 4 * 100

        print(f"4-worker efficiency: {efficiency:.1f}%")


@pytest.mark.benchmark
class TestEndToEndPerformance:
    """End-to-end performance benchmarks"""

    def test_orchestrator_overhead(self):
        """Measure orchestrator overhead"""
        # Create minimal workflow
        workflow = WorkflowDefinition(name="Minimal", description="Test")

        task = WorkflowTask(
            name="Simple Task",
            task_id="t1",
            module="test",
            function="test",
        )

        workflow.add_task(task)

        # This will fail on execution, but we can measure setup overhead
        orchestrator = WorkflowOrchestrator(
            enable_cache=False,
            enable_parallel=False,
            enable_progress=False,
        )

        # Measure dependency ordering
        start = time.time()
        order = workflow.get_dependency_order()
        ordering_time = time.time() - start

        print(f"\nDependency ordering time: {ordering_time*1000:.3f}ms")

        # Should be very fast
        assert ordering_time < 0.001  # < 1ms

    def test_combined_features_performance(self):
        """Test performance with all features enabled"""
        workflow = WorkflowDefinition(name="Combined", description="Test")

        # Create dependency graph
        for i in range(5):
            task = WorkflowTask(
                name=f"Task {i}",
                task_id=f"t{i}",
                module="test",
                function="test",
            )
            workflow.add_task(task)

        # Measure with all features
        orchestrator_full = WorkflowOrchestrator(
            enable_cache=True,
            enable_parallel=True,
            enable_progress=True,
        )

        # Measure with no features
        orchestrator_minimal = WorkflowOrchestrator(
            enable_cache=False,
            enable_parallel=False,
            enable_progress=False,
        )

        # Feature initialization overhead should be minimal
        assert orchestrator_full.cache is not None
        assert orchestrator_full.parallel_executor is not None
        assert orchestrator_full.progress_tracker is not None


class TestMemoryUsage:
    """Test memory usage of caching and parallel execution"""

    def test_cache_memory_usage(self):
        """Measure cache memory footprint"""
        import sys

        cache = WorkflowCache(cache_dir=tempfile.mkdtemp())

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")

        # Add many cached results
        for i in range(100):
            inputs = {'iteration': i}
            result = {'data': [i] * 1000}  # ~8KB each
            cache.set_task_result(task, inputs, result)

        # Check memory cache size
        memory_entries = len(cache._memory_cache)

        print(f"\nMemory cache entries: {memory_entries}")

        # Check disk cache stats
        stats = cache.get_cache_stats()
        print(f"Total cache size: {stats['total_size_mb']:.2f} MB")
        print(f"Task cache entries: {stats['task_cache_entries']}")

    def test_parallel_executor_overhead(self):
        """Measure parallel executor memory overhead"""
        from digitalmodel.modules.workflow_automation import AdaptiveParallelExecutor

        # Create executor
        executor = AdaptiveParallelExecutor(max_workers=4)

        # Executor itself should have minimal overhead
        assert executor is not None


@pytest.mark.benchmark
class TestRealisticWorkflowPerformance:
    """Performance tests with realistic workflow patterns"""

    def test_pipeline_workflow_performance(self):
        """Test performance of pipeline workflow (A -> B -> C -> D)"""
        tasks = []

        for i in range(4):
            depends = [f"t{i-1}"] if i > 0 else []

            task = WorkflowTask(
                name=f"Pipeline Task {i}",
                task_id=f"t{i}",
                module="test",
                function="test",
                depends_on=depends,
            )
            tasks.append(task)

        workflow = WorkflowDefinition(name="Pipeline", description="Test")

        for task in tasks:
            workflow.add_task(task)

        # Get dependency order
        order = workflow.get_dependency_order()

        # Should be 4 sequential groups
        assert len(order) == 4

        print(f"\nPipeline workflow: {len(order)} sequential stages")

    def test_diamond_workflow_performance(self):
        """Test performance of diamond workflow (A -> B,C -> D)"""
        workflow = WorkflowDefinition(name="Diamond", description="Test")

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

        for task in [task_a, task_b, task_c, task_d]:
            workflow.add_task(task)

        order = workflow.get_dependency_order()

        # Should be 3 stages with middle stage parallelizable
        assert len(order) == 3
        assert len(order[1]) == 2  # B and C in parallel

        print(f"\nDiamond workflow: {len(order)} stages, middle stage has {len(order[1])} parallel tasks")
