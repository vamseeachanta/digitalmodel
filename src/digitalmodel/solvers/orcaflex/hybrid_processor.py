"""
Hybrid Process/Thread Model for OrcaFlex
Task 3.1: Implement Hybrid Process/Thread Model
Combines process pool for I/O-bound tasks with thread pool for compute-bound tasks.
"""

import os
import time
import queue
import logging
import threading
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from typing import Dict, List, Any, Optional, Callable, Tuple
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass
from enum import Enum
import psutil

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - Process-%(process)d - Thread-%(thread)d - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class TaskType(Enum):
    """Task type enumeration."""
    IO_BOUND = "io_bound"
    CPU_BOUND = "cpu_bound"
    MEMORY_INTENSIVE = "memory_intensive"
    MIXED = "mixed"


@dataclass
class WorkItem:
    """Work item for hybrid processing."""
    id: str
    task_type: TaskType
    function: Callable
    args: tuple
    kwargs: dict
    priority: int = 0
    
    def __lt__(self, other):
        return self.priority > other.priority  # Higher priority first


class HybridExecutor:
    """
    Hybrid executor combining processes and threads for optimal performance.
    """
    
    def __init__(self, 
                 num_processes: int = None,
                 num_threads: int = None,
                 io_threads: int = 10,
                 compute_processes: int = None):
        """
        Initialize hybrid executor.
        
        Args:
            num_processes: Number of processes (None for auto)
            num_threads: Number of threads per process (None for auto)
            io_threads: Number of I/O threads
            compute_processes: Number of compute processes
        """
        # Auto-detect optimal configuration
        cpu_count = psutil.cpu_count(logical=False)
        logical_count = psutil.cpu_count(logical=True)
        
        self.num_processes = num_processes or min(cpu_count, 8)
        self.num_threads = num_threads or max(2, logical_count // self.num_processes)
        self.io_threads = io_threads
        self.compute_processes = compute_processes or self.num_processes
        
        # Create executors
        self.process_executor = None
        self.thread_executor = None
        self.io_executor = None
        
        # Work queues
        self.io_queue = queue.PriorityQueue()
        self.compute_queue = queue.PriorityQueue()
        
        # Statistics
        self.stats = {
            'io_tasks': 0,
            'compute_tasks': 0,
            'mixed_tasks': 0,
            'total_time': 0,
            'queue_wait_time': 0
        }
        
        logger.info(f"Hybrid Executor initialized:")
        logger.info(f"  Processes: {self.num_processes}")
        logger.info(f"  Threads per process: {self.num_threads}")
        logger.info(f"  I/O threads: {self.io_threads}")
        logger.info(f"  Compute processes: {self.compute_processes}")
    
    def __enter__(self):
        """Context manager entry."""
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.shutdown()
    
    def start(self):
        """Start the executor pools."""
        # Process pool for CPU-intensive tasks
        self.process_executor = ProcessPoolExecutor(max_workers=self.compute_processes)
        
        # Thread pool for I/O-bound tasks
        self.io_executor = ThreadPoolExecutor(max_workers=self.io_threads)
        
        # Thread pool for mixed tasks
        self.thread_executor = ThreadPoolExecutor(max_workers=self.num_threads)
        
        logger.info("Hybrid executor started")
    
    def submit(self, work_item: WorkItem) -> Any:
        """
        Submit work item to appropriate executor.
        
        Args:
            work_item: Work item to process
            
        Returns:
            Future object
        """
        # Route to appropriate executor based on task type
        if work_item.task_type == TaskType.IO_BOUND:
            self.stats['io_tasks'] += 1
            return self.io_executor.submit(
                work_item.function, *work_item.args, **work_item.kwargs
            )
        elif work_item.task_type == TaskType.CPU_BOUND:
            self.stats['compute_tasks'] += 1
            return self.process_executor.submit(
                work_item.function, *work_item.args, **work_item.kwargs
            )
        else:  # MIXED or MEMORY_INTENSIVE
            self.stats['mixed_tasks'] += 1
            return self.thread_executor.submit(
                work_item.function, *work_item.args, **work_item.kwargs
            )
    
    def map_async(self, function: Callable, items: List[Any], 
                  task_type: TaskType = TaskType.MIXED) -> List[Any]:
        """
        Map function over items asynchronously.
        
        Args:
            function: Function to apply
            items: List of items
            task_type: Type of task
            
        Returns:
            List of results
        """
        futures = []
        for i, item in enumerate(items):
            work_item = WorkItem(
                id=f"map_{i}",
                task_type=task_type,
                function=function,
                args=(item,),
                kwargs={},
                priority=0
            )
            futures.append(self.submit(work_item))
        
        # Collect results
        results = []
        for future in as_completed(futures):
            try:
                result = future.result()
                results.append(result)
            except Exception as e:
                logger.error(f"Task failed: {e}")
                results.append(None)
        
        return results
    
    def shutdown(self, wait: bool = True):
        """
        Shutdown executor pools.
        
        Args:
            wait: Wait for pending tasks to complete
        """
        if self.process_executor:
            self.process_executor.shutdown(wait=wait)
        if self.thread_executor:
            self.thread_executor.shutdown(wait=wait)
        if self.io_executor:
            self.io_executor.shutdown(wait=wait)
        
        logger.info("Hybrid executor shutdown")
        logger.info(f"Statistics: {self.stats}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get execution statistics."""
        return self.stats.copy()


class WorkDistributor:
    """
    Intelligent work distribution for hybrid processing.
    """
    
    def __init__(self, hybrid_executor: HybridExecutor):
        """
        Initialize work distributor.
        
        Args:
            hybrid_executor: Hybrid executor instance
        """
        self.executor = hybrid_executor
        self.task_history = []
        self.optimization_params = {
            'io_threshold_mb': 10,
            'compute_threshold_ops': 1000000,
            'memory_threshold_mb': 100
        }
    
    def classify_task(self, file_path: str = None, 
                     operation: str = None) -> TaskType:
        """
        Classify task type based on characteristics.
        
        Args:
            file_path: File being processed
            operation: Operation type
            
        Returns:
            Task type classification
        """
        # File size-based classification
        if file_path and os.path.exists(file_path):
            size_mb = os.path.getsize(file_path) / 1e6
            
            if size_mb > self.optimization_params['memory_threshold_mb']:
                return TaskType.MEMORY_INTENSIVE
            elif size_mb > self.optimization_params['io_threshold_mb']:
                return TaskType.IO_BOUND
        
        # Operation-based classification
        if operation:
            io_operations = ['read', 'write', 'load', 'save', 'download', 'upload']
            cpu_operations = ['calculate', 'process', 'analyze', 'simulate', 'optimize']
            
            if any(op in operation.lower() for op in io_operations):
                return TaskType.IO_BOUND
            elif any(op in operation.lower() for op in cpu_operations):
                return TaskType.CPU_BOUND
        
        # Default to mixed
        return TaskType.MIXED
    
    def distribute_work(self, tasks: List[Dict[str, Any]]) -> Dict[str, List[Any]]:
        """
        Distribute work items across executor pools.
        
        Args:
            tasks: List of task dictionaries
            
        Returns:
            Distribution map
        """
        distribution = {
            'io_bound': [],
            'cpu_bound': [],
            'memory_intensive': [],
            'mixed': []
        }
        
        for task in tasks:
            task_type = self.classify_task(
                file_path=task.get('file'),
                operation=task.get('operation')
            )
            
            work_item = WorkItem(
                id=task.get('id', str(time.time())),
                task_type=task_type,
                function=task.get('function'),
                args=task.get('args', ()),
                kwargs=task.get('kwargs', {}),
                priority=task.get('priority', 0)
            )
            
            distribution[task_type.value].append(work_item)
        
        logger.info(f"Work distribution:")
        for category, items in distribution.items():
            logger.info(f"  {category}: {len(items)} tasks")
        
        return distribution
    
    def optimize_distribution(self, metrics: Dict[str, float]) -> None:
        """
        Optimize distribution parameters based on performance metrics.
        
        Args:
            metrics: Performance metrics
        """
        # Adjust thresholds based on performance
        if metrics.get('io_wait_time', 0) > metrics.get('compute_time', 0):
            # I/O is bottleneck - route more to processes
            self.optimization_params['io_threshold_mb'] *= 0.9
        elif metrics.get('compute_time', 0) > metrics.get('io_wait_time', 0):
            # Compute is bottleneck - route more to threads
            self.optimization_params['io_threshold_mb'] *= 1.1
        
        logger.info(f"Optimized parameters: {self.optimization_params}")


class HybridOrcaFlexProcessor:
    """
    Hybrid processor for OrcaFlex files using process/thread model.
    """
    
    def __init__(self):
        """Initialize hybrid OrcaFlex processor."""
        self.executor = HybridExecutor()
        self.distributor = WorkDistributor(self.executor)
        
    def process_files(self, file_paths: List[str]) -> Dict[str, Any]:
        """
        Process OrcaFlex files using hybrid model.
        
        Args:
            file_paths: List of file paths
            
        Returns:
            Processing results
        """
        logger.info(f"Processing {len(file_paths)} files with hybrid model")
        
        start_time = time.time()
        
        # Create tasks
        tasks = []
        for i, file_path in enumerate(file_paths):
            # Determine task type based on file
            file_size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
            
            # Large files: I/O bound
            if file_size > 100 * 1024 * 1024:  # >100MB
                operation = "load_large_file"
                task_type = TaskType.IO_BOUND
            # Small files: CPU bound processing
            elif file_size < 10 * 1024 * 1024:  # <10MB
                operation = "process_small_file"
                task_type = TaskType.CPU_BOUND
            else:
                operation = "process_file"
                task_type = TaskType.MIXED
            
            tasks.append({
                'id': f"file_{i}",
                'file': file_path,
                'operation': operation,
                'function': self._process_single_file,
                'args': (file_path,),
                'kwargs': {'task_type': task_type.value},
                'priority': i
            })
        
        # Distribute work
        distribution = self.distributor.distribute_work(tasks)
        
        # Process with hybrid executor
        with self.executor:
            results = []
            
            # Submit all tasks
            futures = []
            for category, work_items in distribution.items():
                for work_item in work_items:
                    future = self.executor.submit(work_item)
                    futures.append((future, work_item.id))
            
            # Collect results
            for future, task_id in futures:
                try:
                    result = future.result(timeout=60)
                    results.append({
                        'id': task_id,
                        'status': 'success',
                        'result': result
                    })
                except Exception as e:
                    results.append({
                        'id': task_id,
                        'status': 'failed',
                        'error': str(e)
                    })
        
        duration = time.time() - start_time
        
        # Get statistics
        stats = self.executor.get_stats()
        
        return {
            'total_files': len(file_paths),
            'duration': duration,
            'throughput': len(file_paths) / duration if duration > 0 else 0,
            'results': results,
            'distribution': {k: len(v) for k, v in distribution.items()},
            'executor_stats': stats
        }
    
    def _process_single_file(self, file_path: str, task_type: str = "mixed") -> Dict[str, Any]:
        """
        Process a single file (mock implementation).
        
        Args:
            file_path: File path
            task_type: Task type
            
        Returns:
            Processing result
        """
        # Simulate different processing based on task type
        if task_type == "io_bound":
            # Simulate I/O operation
            time.sleep(0.1)
            return {'file': file_path, 'type': 'io', 'processed': True}
        elif task_type == "cpu_bound":
            # Simulate CPU-intensive operation
            result = sum(i**2 for i in range(10000))
            return {'file': file_path, 'type': 'cpu', 'result': result}
        else:
            # Mixed operation
            time.sleep(0.05)
            return {'file': file_path, 'type': 'mixed', 'processed': True}


def benchmark_hybrid_model():
    """Benchmark the hybrid process/thread model."""
    print("\n" + "="*60)
    print("HYBRID PROCESS/THREAD MODEL BENCHMARK")
    print("="*60)
    
    # Create test files
    test_dir = Path("./hybrid_test")
    test_dir.mkdir(exist_ok=True)
    
    test_files = []
    for i in range(20):
        file_path = test_dir / f"test_{i}.dat"
        # Create files of varying sizes
        if i < 5:
            size = 150 * 1024 * 1024  # Large files (150MB)
        elif i < 15:
            size = 5 * 1024 * 1024   # Small files (5MB)
        else:
            size = 50 * 1024 * 1024  # Medium files (50MB)
        
        # Create sparse file for testing
        file_path.write_bytes(b'\0' * min(size, 1024))  # Write minimal data
        test_files.append(str(file_path))
    
    print(f"Created {len(test_files)} test files")
    
    # Test hybrid processor
    processor = HybridOrcaFlexProcessor()
    
    print("\nProcessing with Hybrid Model...")
    results = processor.process_files(test_files)
    
    print(f"\nResults:")
    print(f"  Total files: {results['total_files']}")
    print(f"  Duration: {results['duration']:.2f}s")
    print(f"  Throughput: {results['throughput']:.1f} files/s")
    print(f"\nTask Distribution:")
    for task_type, count in results['distribution'].items():
        print(f"  {task_type}: {count} tasks")
    print(f"\nExecutor Statistics:")
    for key, value in results['executor_stats'].items():
        print(f"  {key}: {value}")
    
    # Cleanup
    import shutil
    shutil.rmtree(test_dir, ignore_errors=True)
    
    print("\n" + "="*60)
    print("Hybrid model benchmark completed successfully!")
    print("="*60)


if __name__ == "__main__":
    benchmark_hybrid_model()