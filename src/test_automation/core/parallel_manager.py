"""
Advanced Parallel Execution Manager for test automation.

This module provides intelligent work distribution, dependency management,
and resource-aware scheduling for optimal test execution performance.
"""

import time
import threading
import psutil
import queue
import concurrent.futures
from typing import Dict, List, Set, Optional, Any, Tuple
from dataclasses import dataclass, field
from concurrent.futures import ThreadPoolExecutor, Future, as_completed
from datetime import datetime, timezone
from collections import defaultdict, deque

from test_automation.config import config
from test_automation.logging_config import get_logger
from test_automation.core.discovery import ModuleInfo, TestDiscoveryEngine
from test_automation.core.runner import ModuleTestRunner, ModuleResult

logger = get_logger('test_automation.parallel_manager')


@dataclass
class WorkerStats:
    """Statistics for a worker thread."""
    worker_id: int
    active: bool = False
    current_module: Optional[str] = None
    completed_modules: int = 0
    total_tests_run: int = 0
    total_duration: float = 0.0
    avg_memory_mb: float = 0.0
    avg_cpu_percent: float = 0.0
    last_activity: Optional[datetime] = None


@dataclass 
class ExecutionTask:
    """Represents a test execution task with scheduling metadata."""
    module_name: str
    module_info: ModuleInfo
    priority: int = 0
    dependencies: Set[str] = field(default_factory=set)
    estimated_duration: float = 0.0
    requires_license: bool = False
    scheduled_time: Optional[datetime] = None
    worker_id: Optional[int] = None


class DependencyResolver:
    """Resolves module dependencies for optimal execution ordering."""
    
    def __init__(self, modules: Dict[str, ModuleInfo]):
        self.modules = modules
        self.dependency_graph = self._build_dependency_graph()
        self.resolved_order: List[str] = []
    
    def _build_dependency_graph(self) -> Dict[str, Set[str]]:
        """Build dependency graph from module information."""
        graph = {}
        
        for module_name, module_info in self.modules.items():
            # Add module dependencies
            deps = set(module_info.dependencies)
            
            # Remove self-dependencies
            deps.discard(module_name)
            
            # Only include dependencies that exist in our module set
            deps = deps.intersection(self.modules.keys())
            
            graph[module_name] = deps
            
        return graph
    
    def resolve_execution_order(self) -> List[str]:
        """Resolve optimal execution order using topological sort with priority."""
        if self.resolved_order:
            return self.resolved_order
        
        # Kahn's algorithm with priority scoring
        in_degree = defaultdict(int)
        for node in self.dependency_graph:
            for dep in self.dependency_graph[node]:
                in_degree[dep] += 1
        
        # Priority queue: modules with no dependencies and higher priority first
        ready_queue = []
        for module_name in self.modules:
            if in_degree[module_name] == 0:
                priority = self._calculate_priority(module_name)
                ready_queue.append((priority, module_name))
        
        ready_queue.sort(reverse=True)  # Higher priority first
        resolved = []
        
        while ready_queue:
            _, current = ready_queue.pop(0)
            resolved.append(current)
            
            # Update in-degrees for dependent modules
            for module_name, deps in self.dependency_graph.items():
                if current in deps:
                    in_degree[module_name] -= 1
                    if in_degree[module_name] == 0:
                        priority = self._calculate_priority(module_name)
                        ready_queue.append((priority, module_name))
                        ready_queue.sort(reverse=True)
        
        # Check for circular dependencies
        if len(resolved) != len(self.modules):
            remaining = set(self.modules.keys()) - set(resolved)
            logger.warning(f"Circular dependencies detected in modules: {remaining}")
            # Add remaining modules in estimated duration order
            remaining_sorted = sorted(remaining, 
                                    key=lambda m: self.modules[m].estimated_duration)
            resolved.extend(remaining_sorted)
        
        self.resolved_order = resolved
        return resolved
    
    def _calculate_priority(self, module_name: str) -> int:
        """Calculate execution priority for a module."""
        module_info = self.modules[module_name]
        priority = 100  # Base priority
        
        # Prefer modules with fewer dependencies (run foundational modules first)
        priority += max(0, 50 - len(module_info.dependencies) * 10)
        
        # Prefer shorter duration tests for quick feedback
        if module_info.estimated_duration < 30:
            priority += 30
        elif module_info.estimated_duration > 120:
            priority -= 20
        
        # Prefer modules with more runnable tests
        priority += min(module_info.runnable_tests * 2, 20)
        
        # Slightly prefer modules without license requirements for availability
        if not any(tf.requires_license for tf in module_info.test_files):
            priority += 5
        
        return priority


class ResourceMonitor:
    """Monitor system resources and worker performance."""
    
    def __init__(self):
        self.monitoring = False
        self.monitor_thread: Optional[threading.Thread] = None
        self.stats_lock = threading.Lock()
        self.system_stats = {
            'cpu_percent': 0.0,
            'memory_percent': 0.0,
            'available_memory_mb': 0.0,
            'load_average': 0.0
        }
        self.worker_stats: Dict[int, WorkerStats] = {}
    
    def start_monitoring(self):
        """Start resource monitoring thread."""
        if not self.monitoring:
            self.monitoring = True
            self.monitor_thread = threading.Thread(target=self._monitor_loop, daemon=True)
            self.monitor_thread.start()
            logger.info("Resource monitoring started")
    
    def stop_monitoring(self):
        """Stop resource monitoring."""
        if self.monitoring:
            self.monitoring = False
            if self.monitor_thread:
                self.monitor_thread.join(timeout=2.0)
            logger.info("Resource monitoring stopped")
    
    def _monitor_loop(self):
        """Main monitoring loop."""
        while self.monitoring:
            try:
                with self.stats_lock:
                    # Update system stats
                    self.system_stats['cpu_percent'] = psutil.cpu_percent(interval=1.0)
                    memory = psutil.virtual_memory()
                    self.system_stats['memory_percent'] = memory.percent
                    self.system_stats['available_memory_mb'] = memory.available / (1024 * 1024)
                    
                    # Update load average (simplified calculation)
                    try:
                        self.system_stats['load_average'] = psutil.getloadavg()[0]
                    except AttributeError:
                        # Windows doesn't have getloadavg
                        self.system_stats['load_average'] = self.system_stats['cpu_percent'] / 100.0
                
                time.sleep(2.0)  # Monitor every 2 seconds
                
            except Exception as e:
                logger.warning(f"Resource monitoring error: {e}")
                time.sleep(5.0)
    
    def get_system_stats(self) -> Dict[str, float]:
        """Get current system resource statistics."""
        with self.stats_lock:
            return self.system_stats.copy()
    
    def register_worker(self, worker_id: int):
        """Register a new worker for monitoring."""
        with self.stats_lock:
            self.worker_stats[worker_id] = WorkerStats(worker_id=worker_id)
    
    def update_worker_activity(self, worker_id: int, module_name: str, active: bool):
        """Update worker activity status."""
        with self.stats_lock:
            if worker_id in self.worker_stats:
                stats = self.worker_stats[worker_id]
                stats.active = active
                stats.current_module = module_name if active else None
                stats.last_activity = datetime.now(timezone.utc)
    
    def should_throttle_execution(self) -> bool:
        """Determine if execution should be throttled due to resource constraints."""
        stats = self.get_system_stats()
        
        # Throttle if CPU > 90% or available memory < 500MB
        if stats['cpu_percent'] > 90 or stats['available_memory_mb'] < 500:
            return True
        
        return False
    
    def get_optimal_worker_count(self) -> int:
        """Calculate optimal worker count based on current system state."""
        stats = self.get_system_stats()
        base_workers = config.execution.max_workers
        
        # Reduce workers if system is under stress
        if stats['cpu_percent'] > 80:
            return max(1, base_workers // 2)
        elif stats['available_memory_mb'] < 1000:
            return max(1, base_workers // 2)
        else:
            return base_workers


class ParallelExecutionManager:
    """
    Advanced parallel execution manager with intelligent scheduling,
    resource management, and progress tracking.
    """
    
    def __init__(self, max_workers: int = None):
        self.max_workers = max_workers or config.execution.max_workers
        self.discovery_engine = TestDiscoveryEngine()
        self.resource_monitor = ResourceMonitor()
        self.test_runner = ModuleTestRunner(parallel=False)  # We handle parallelism here
        
        # Execution state
        self.task_queue: queue.PriorityQueue = queue.PriorityQueue()
        self.completed_modules: Dict[str, ModuleResult] = {}
        self.failed_modules: Dict[str, ModuleResult] = {}
        self.in_progress: Dict[str, int] = {}  # module -> worker_id
        
        # Licensed software management
        self.licensed_slots = threading.Semaphore(config.execution.licensed_software_limit)
        self.licensed_modules: Set[str] = set()
        
        # Statistics and monitoring
        self.execution_start_time: Optional[datetime] = None
        self.execution_stats = {
            'total_modules': 0,
            'completed_modules': 0,
            'failed_modules': 0,
            'skipped_modules': 0,
            'total_duration': 0.0,
            'avg_module_duration': 0.0,
            'parallelization_efficiency': 0.0
        }
    
    def execute_all_modules(self, verbose: bool = False) -> Dict[str, ModuleResult]:
        """Execute all runnable modules with intelligent parallel scheduling."""
        logger.info("Starting advanced parallel test execution")
        
        # Discover modules
        modules = self.discovery_engine.discover_modules(force_refresh=True)
        runnable_modules = self.discovery_engine.get_runnable_tests()
        
        if not runnable_modules:
            logger.warning("No runnable modules found")
            return {}
        
        # Initialize execution state
        self.execution_start_time = datetime.now(timezone.utc)
        self.execution_stats['total_modules'] = len(runnable_modules)
        
        # Start resource monitoring
        self.resource_monitor.start_monitoring()
        
        try:
            # Resolve dependencies and create execution plan
            dependency_resolver = DependencyResolver(runnable_modules)
            execution_order = dependency_resolver.resolve_execution_order()
            
            logger.info(f"Execution plan created for {len(execution_order)} modules")
            logger.info(f"Execution order: {', '.join(execution_order[:10])}" + 
                       (f"... (+{len(execution_order)-10} more)" if len(execution_order) > 10 else ""))
            
            # Create execution tasks
            tasks = self._create_execution_tasks(runnable_modules, execution_order)
            
            # Execute tasks with intelligent scheduling
            results = self._execute_tasks_parallel(tasks, verbose)
            
            # Calculate final statistics
            self._calculate_execution_stats(results)
            
            return results
            
        finally:
            self.resource_monitor.stop_monitoring()
    
    def _create_execution_tasks(self, modules: Dict[str, ModuleInfo], 
                              execution_order: List[str]) -> List[ExecutionTask]:
        """Create execution tasks with priority and dependency information."""
        tasks = []
        
        for i, module_name in enumerate(execution_order):
            module_info = modules[module_name]
            
            # Calculate priority (earlier in order = higher priority)
            priority = len(execution_order) - i
            
            # Identify dependencies in runnable modules
            dependencies = module_info.dependencies.intersection(set(modules.keys()))
            
            # Check if requires licensed software
            requires_license = any(tf.requires_license for tf in module_info.test_files)
            
            task = ExecutionTask(
                module_name=module_name,
                module_info=module_info,
                priority=priority,
                dependencies=dependencies,
                estimated_duration=module_info.estimated_duration,
                requires_license=requires_license
            )
            
            tasks.append(task)
        
        return tasks
    
    def _execute_tasks_parallel(self, tasks: List[ExecutionTask], 
                              verbose: bool) -> Dict[str, ModuleResult]:
        """Execute tasks using intelligent parallel scheduling."""
        
        # Initialize task queue with dependency-free tasks
        ready_tasks = deque()
        waiting_tasks = deque()
        
        for task in tasks:
            if not task.dependencies:
                ready_tasks.append(task)
            else:
                waiting_tasks.append(task)
        
        # Adjust worker count based on system resources
        optimal_workers = self.resource_monitor.get_optimal_worker_count()
        actual_workers = min(optimal_workers, len(tasks))
        
        logger.info(f"Using {actual_workers} workers for parallel execution")
        
        results = {}
        
        with ThreadPoolExecutor(max_workers=actual_workers) as executor:
            # Register workers with resource monitor
            for i in range(actual_workers):
                self.resource_monitor.register_worker(i)
            
            active_futures: Dict[Future, ExecutionTask] = {}
            
            while ready_tasks or waiting_tasks or active_futures:
                # Submit ready tasks to executor
                while ready_tasks and len(active_futures) < actual_workers:
                    # Check for resource throttling
                    if self.resource_monitor.should_throttle_execution():
                        logger.info("Throttling execution due to resource constraints")
                        time.sleep(2.0)
                        continue
                    
                    task = ready_tasks.popleft()
                    
                    # Handle licensed software limits
                    if task.requires_license:
                        if not self.licensed_slots.acquire(blocking=False):
                            # Put back in queue and try later
                            ready_tasks.append(task)
                            continue
                    
                    # Submit task
                    future = executor.submit(self._execute_single_task, task, verbose)
                    active_futures[future] = task
                    
                    task.scheduled_time = datetime.now(timezone.utc)
                    logger.info(f"Scheduled module '{task.module_name}' for execution")
                
                # Wait for at least one task to complete
                if active_futures:
                    completed_futures = []
                    try:
                        for future in as_completed(active_futures, timeout=1.0):
                            completed_futures.append(future)
                            break  # Process one at a time for better scheduling
                    except concurrent.futures.TimeoutError:
                        # No futures completed in timeout window, continue loop
                        pass
                    
                    # Process completed tasks
                    for future in completed_futures:
                        task = active_futures.pop(future)
                        
                        try:
                            result = future.result()
                            results[task.module_name] = result
                            
                            if task.requires_license:
                                self.licensed_slots.release()
                            
                            logger.info(f"Completed module '{task.module_name}': {result.status}")
                            
                            # Move waiting tasks that are now ready
                            newly_ready = []
                            remaining_waiting = deque()
                            
                            for waiting_task in waiting_tasks:
                                # Check if all dependencies are completed
                                if waiting_task.dependencies.issubset(results.keys()):
                                    newly_ready.append(waiting_task)
                                else:
                                    remaining_waiting.append(waiting_task)
                            
                            ready_tasks.extend(newly_ready)
                            waiting_tasks = remaining_waiting
                            
                        except Exception as e:
                            logger.error(f"Error executing module '{task.module_name}': {e}")
                            
                            if task.requires_license:
                                self.licensed_slots.release()
                
                # Brief pause to prevent busy waiting
                time.sleep(0.1)
        
        return results
    
    def _execute_single_task(self, task: ExecutionTask, verbose: bool) -> ModuleResult:
        """Execute a single test task."""
        worker_id = threading.current_thread().ident
        
        # Update worker status
        self.resource_monitor.update_worker_activity(worker_id, task.module_name, True)
        
        try:
            # Execute the module tests
            result = self.test_runner.run_module_tests(task.module_name, verbose)
            
            # Update statistics
            self.execution_stats['completed_modules'] += 1
            
            return result
            
        except Exception as e:
            logger.error(f"Failed to execute module '{task.module_name}': {e}")
            
            # Create error result
            now = datetime.now(timezone.utc)
            error_result = ModuleResult(
                module_name=task.module_name,
                status='error',
                total_duration=0.0,
                test_results=[],
                error_tests=1,
                start_time=now.isoformat(),
                end_time=now.isoformat()
            )
            
            self.execution_stats['failed_modules'] += 1
            return error_result
            
        finally:
            # Update worker status
            self.resource_monitor.update_worker_activity(worker_id, task.module_name, False)
    
    def _calculate_execution_stats(self, results: Dict[str, ModuleResult]):
        """Calculate final execution statistics."""
        if not results:
            return
        
        total_duration = sum(r.total_duration for r in results.values())
        self.execution_stats['total_duration'] = total_duration
        self.execution_stats['avg_module_duration'] = total_duration / len(results)
        
        # Calculate parallelization efficiency
        sequential_estimate = sum(r.total_duration for r in results.values())
        actual_duration = time.time() - self.execution_start_time.timestamp() if self.execution_start_time else 0
        
        if actual_duration > 0:
            self.execution_stats['parallelization_efficiency'] = sequential_estimate / actual_duration
        
        logger.info(f"Execution completed - Total: {len(results)} modules, "
                   f"Duration: {actual_duration:.1f}s, "
                   f"Efficiency: {self.execution_stats['parallelization_efficiency']:.1f}x")
    
    def get_execution_summary(self) -> Dict[str, Any]:
        """Get comprehensive execution summary."""
        system_stats = self.resource_monitor.get_system_stats()
        
        return {
            'execution_stats': self.execution_stats.copy(),
            'system_stats': system_stats,
            'resource_usage': {
                'max_workers': self.max_workers,
                'licensed_software_limit': config.execution.licensed_software_limit
            },
            'timestamp': datetime.now(timezone.utc).isoformat()
        }