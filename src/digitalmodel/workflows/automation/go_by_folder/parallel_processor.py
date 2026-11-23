"""
Parallel processing optimization for Create Go-By Folder Tool
"""

import os
import multiprocessing as mp
from multiprocessing import Pool, Queue, Manager
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
from typing import Dict, List, Any, Optional, Callable, Tuple
from pathlib import Path
from dataclasses import dataclass
import logging
import time
from queue import Empty
import threading

logger = logging.getLogger(__name__)


@dataclass
class ProcessingTask:
    """Task for parallel processing."""
    file_path: Path
    task_type: str
    parameters: Dict = None
    priority: int = 0
    
    def __lt__(self, other):
        """Compare by priority for queue ordering."""
        return self.priority < other.priority


class ParallelProcessor:
    """Handle parallel processing for go-by folder creation."""
    
    def __init__(self, 
                 max_workers: Optional[int] = None,
                 use_processes: bool = True,
                 chunk_size: int = 100):
        """
        Initialize parallel processor.
        
        Args:
            max_workers: Maximum number of workers (None for CPU count)
            use_processes: Use processes instead of threads
            chunk_size: Size of chunks for batch processing
        """
        self.max_workers = max_workers or mp.cpu_count()
        self.use_processes = use_processes
        self.chunk_size = chunk_size
        self.stats = {
            'tasks_completed': 0,
            'tasks_failed': 0,
            'processing_time': 0
        }
        self._lock = threading.Lock()
    
    def process_files_parallel(self, 
                              files: List[Dict],
                              process_func: Callable,
                              callback: Optional[Callable] = None,
                              error_callback: Optional[Callable] = None) -> Tuple[List, List]:
        """
        Process files in parallel.
        
        Args:
            files: List of file information dictionaries
            process_func: Function to process each file
            callback: Optional callback for successful processing
            error_callback: Optional callback for errors
            
        Returns:
            Tuple of (successful_results, failed_files)
        """
        successful = []
        failed = []
        start_time = time.time()
        
        # Choose executor based on configuration
        ExecutorClass = ProcessPoolExecutor if self.use_processes else ThreadPoolExecutor
        
        with ExecutorClass(max_workers=self.max_workers) as executor:
            # Submit all tasks
            future_to_file = {
                executor.submit(process_func, file_info): file_info 
                for file_info in files
            }
            
            # Process completed tasks
            for future in as_completed(future_to_file):
                file_info = future_to_file[future]
                
                try:
                    result = future.result(timeout=30)  # 30 second timeout per file
                    successful.append(result)
                    
                    with self._lock:
                        self.stats['tasks_completed'] += 1
                    
                    if callback:
                        callback(file_info, result)
                        
                except Exception as e:
                    failed.append((file_info, str(e)))
                    
                    with self._lock:
                        self.stats['tasks_failed'] += 1
                    
                    if error_callback:
                        error_callback(file_info, e)
                    
                    logger.warning(f"Failed to process {file_info.get('path')}: {e}")
        
        self.stats['processing_time'] = time.time() - start_time
        
        logger.info(
            f"Parallel processing complete: {len(successful)} successful, "
            f"{len(failed)} failed in {self.stats['processing_time']:.2f}s"
        )
        
        return successful, failed
    
    def scan_parallel(self, 
                     root_path: Path,
                     scanner_func: Callable,
                     num_workers: Optional[int] = None) -> List[Dict]:
        """
        Scan directory tree in parallel.
        
        Args:
            root_path: Root directory to scan
            scanner_func: Function to scan a directory
            num_workers: Number of workers for scanning
            
        Returns:
            List of all discovered files
        """
        all_files = []
        workers = num_workers or self.max_workers
        
        # Get initial directories to scan
        try:
            initial_dirs = [d for d in root_path.iterdir() if d.is_dir()]
            if not initial_dirs:
                # If no subdirectories, scan the root directly
                return scanner_func(root_path)
        except Exception as e:
            logger.error(f"Failed to list directories: {e}")
            return []
        
        # Add root directory for files in root
        initial_dirs.insert(0, root_path)
        
        with ThreadPoolExecutor(max_workers=workers) as executor:
            # Submit scanning tasks
            future_to_dir = {
                executor.submit(scanner_func, dir_path): dir_path
                for dir_path in initial_dirs
            }
            
            # Collect results
            for future in as_completed(future_to_dir):
                dir_path = future_to_dir[future]
                
                try:
                    files = future.result(timeout=60)  # 60 second timeout per directory
                    all_files.extend(files)
                    logger.debug(f"Scanned {dir_path}: {len(files)} files")
                    
                except Exception as e:
                    logger.warning(f"Failed to scan {dir_path}: {e}")
        
        return all_files
    
    def batch_process(self,
                     items: List[Any],
                     batch_func: Callable,
                     batch_size: Optional[int] = None) -> List[Any]:
        """
        Process items in batches.
        
        Args:
            items: Items to process
            batch_func: Function to process a batch
            batch_size: Size of each batch
            
        Returns:
            List of all results
        """
        batch_size = batch_size or self.chunk_size
        results = []
        
        # Create batches
        batches = [items[i:i + batch_size] for i in range(0, len(items), batch_size)]
        
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # Submit batch processing tasks
            futures = [executor.submit(batch_func, batch) for batch in batches]
            
            # Collect results
            for future in as_completed(futures):
                try:
                    batch_results = future.result(timeout=120)  # 2 minute timeout per batch
                    results.extend(batch_results)
                except Exception as e:
                    logger.error(f"Batch processing failed: {e}")
        
        return results
    
    def map_reduce(self,
                  items: List[Any],
                  map_func: Callable,
                  reduce_func: Callable,
                  initial_value: Any = None) -> Any:
        """
        Map-reduce pattern for data processing.
        
        Args:
            items: Items to process
            map_func: Function to map each item
            reduce_func: Function to reduce results
            initial_value: Initial value for reduction
            
        Returns:
            Reduced result
        """
        # Map phase
        with ProcessPoolExecutor(max_workers=self.max_workers) as executor:
            mapped_results = list(executor.map(map_func, items, chunksize=self.chunk_size))
        
        # Reduce phase
        result = initial_value
        for item in mapped_results:
            result = reduce_func(result, item)
        
        return result
    
    def pipeline_process(self,
                        items: List[Any],
                        pipeline_stages: List[Callable]) -> List[Any]:
        """
        Process items through a pipeline of stages.
        
        Args:
            items: Items to process
            pipeline_stages: List of processing functions
            
        Returns:
            Final processed items
        """
        current_items = items
        
        for stage_num, stage_func in enumerate(pipeline_stages):
            logger.info(f"Pipeline stage {stage_num + 1}/{len(pipeline_stages)}")
            
            with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
                current_items = list(executor.map(stage_func, current_items))
        
        return current_items
    
    def get_stats(self) -> Dict[str, Any]:
        """Get processing statistics."""
        with self._lock:
            return self.stats.copy()


class FileProcessorPool:
    """Pool of workers for file processing."""
    
    def __init__(self, num_workers: int = 4):
        """
        Initialize file processor pool.
        
        Args:
            num_workers: Number of worker processes
        """
        self.num_workers = num_workers
        self.manager = Manager()
        self.task_queue = self.manager.Queue()
        self.result_queue = self.manager.Queue()
        self.workers = []
        self.running = False
    
    def start(self) -> None:
        """Start worker processes."""
        self.running = True
        
        for i in range(self.num_workers):
            worker = mp.Process(
                target=self._worker_loop,
                args=(i, self.task_queue, self.result_queue)
            )
            worker.start()
            self.workers.append(worker)
        
        logger.info(f"Started {self.num_workers} worker processes")
    
    def stop(self) -> None:
        """Stop worker processes."""
        self.running = False
        
        # Send stop signals
        for _ in range(self.num_workers):
            self.task_queue.put(None)
        
        # Wait for workers to finish
        for worker in self.workers:
            worker.join(timeout=5)
            if worker.is_alive():
                worker.terminate()
        
        self.workers.clear()
        logger.info("Stopped all worker processes")
    
    def submit_task(self, task: ProcessingTask) -> None:
        """Submit a task for processing."""
        self.task_queue.put(task)
    
    def get_result(self, timeout: Optional[float] = None) -> Optional[Tuple[ProcessingTask, Any]]:
        """
        Get a processing result.
        
        Args:
            timeout: Timeout in seconds
            
        Returns:
            Tuple of (task, result) or None if timeout
        """
        try:
            return self.result_queue.get(timeout=timeout)
        except Empty:
            return None
    
    def _worker_loop(self, worker_id: int, task_queue: Queue, result_queue: Queue) -> None:
        """
        Worker process loop.
        
        Args:
            worker_id: Worker identifier
            task_queue: Queue to get tasks from
            result_queue: Queue to put results in
        """
        logger.debug(f"Worker {worker_id} started")
        
        while True:
            try:
                task = task_queue.get(timeout=1)
                
                if task is None:  # Stop signal
                    break
                
                # Process task
                try:
                    result = self._process_task(task)
                    result_queue.put((task, result))
                except Exception as e:
                    result_queue.put((task, {'error': str(e)}))
                    
            except Empty:
                continue
            except Exception as e:
                logger.error(f"Worker {worker_id} error: {e}")
        
        logger.debug(f"Worker {worker_id} stopped")
    
    def _process_task(self, task: ProcessingTask) -> Dict[str, Any]:
        """
        Process a single task.
        
        Args:
            task: Task to process
            
        Returns:
            Processing result
        """
        # This would be replaced with actual processing logic
        if task.task_type == 'scan':
            return {'files': [], 'size': 0}
        elif task.task_type == 'minimize':
            return {'minimized': True, 'new_size': 0}
        elif task.task_type == 'analyze':
            return {'patterns': {}}
        else:
            return {'processed': True}


def optimize_worker_count(test_func: Callable, 
                         test_data: List[Any],
                         max_workers_range: range = range(1, 9)) -> int:
    """
    Optimize worker count for best performance.
    
    Args:
        test_func: Function to test
        test_data: Sample data for testing
        max_workers_range: Range of worker counts to test
        
    Returns:
        Optimal number of workers
    """
    best_workers = 1
    best_time = float('inf')
    
    for num_workers in max_workers_range:
        start_time = time.time()
        
        with ThreadPoolExecutor(max_workers=num_workers) as executor:
            list(executor.map(test_func, test_data[:100]))  # Test with first 100 items
        
        elapsed = time.time() - start_time
        
        if elapsed < best_time:
            best_time = elapsed
            best_workers = num_workers
        
        logger.debug(f"Workers: {num_workers}, Time: {elapsed:.2f}s")
    
    logger.info(f"Optimal worker count: {best_workers}")
    return best_workers