#!/usr/bin/env python3
"""
MANDATORY Parallel Processing Utilities
All repos MUST use these utilities for parallel operations
"""

import asyncio
import concurrent.futures
from functools import wraps
from typing import Any, Callable, Iterable, List, Optional
import multiprocessing as mp
from pathlib import Path

# MANDATORY: Default to parallel processing
USE_PARALLEL = True
MAX_WORKERS = min(mp.cpu_count(), 8)

def mandatory_parallel(func: Callable) -> Callable:
    """
    Decorator that makes parallel processing mandatory
    Raises error if parallel processing is disabled
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        if not USE_PARALLEL:
            raise RuntimeError(
                "PARALLEL PROCESSING IS MANDATORY! "
                "Set USE_PARALLEL=True in environment"
            )
        return func(*args, **kwargs)
    return wrapper

@mandatory_parallel
def parallel_map(func: Callable, items: Iterable, 
                max_workers: Optional[int] = None) -> List[Any]:
    """
    MANDATORY parallel map function
    Uses multiprocessing for CPU-bound tasks
    """
    workers = max_workers or MAX_WORKERS
    with concurrent.futures.ProcessPoolExecutor(max_workers=workers) as executor:
        return list(executor.map(func, items))

@mandatory_parallel
async def async_parallel_map(func: Callable, items: Iterable) -> List[Any]:
    """
    MANDATORY async parallel map for I/O-bound tasks
    """
    tasks = [asyncio.create_task(func(item)) for item in items]
    return await asyncio.gather(*tasks)

@mandatory_parallel
def parallel_file_operations(file_paths: List[Path], 
                           operation: Callable) -> List[Any]:
    """
    MANDATORY parallel file operations
    Processes files in parallel batches
    """
    batch_size = max(1, len(file_paths) // MAX_WORKERS)
    
    def process_batch(batch):
        return [operation(fp) for fp in batch]
    
    batches = [file_paths[i:i+batch_size] 
               for i in range(0, len(file_paths), batch_size)]
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [executor.submit(process_batch, batch) for batch in batches]
        results = []
        for future in concurrent.futures.as_completed(futures):
            results.extend(future.result())
    return results

class ParallelProcessor:
    """
    MANDATORY parallel processor for all operations
    """
    
    def __init__(self, max_workers: Optional[int] = None):
        self.max_workers = max_workers or MAX_WORKERS
        if not USE_PARALLEL:
            raise RuntimeError("Parallel processing is MANDATORY!")
    
    def process_files(self, files: List[str], func: Callable) -> List[Any]:
        """Process files in parallel"""
        return parallel_map(func, files, self.max_workers)
    
    async def process_async(self, items: List[Any], 
                           func: Callable) -> List[Any]:
        """Process items asynchronously in parallel"""
        return await async_parallel_map(func, items)
    
    def batch_process(self, items: List[Any], func: Callable, 
                     batch_size: int = 10) -> List[Any]:
        """Process items in parallel batches"""
        results = []
        for i in range(0, len(items), batch_size):
            batch = items[i:i+batch_size]
            batch_results = parallel_map(func, batch, self.max_workers)
            results.extend(batch_results)
        return results

# Export mandatory parallel utilities
__all__ = [
    'mandatory_parallel',
    'parallel_map', 
    'async_parallel_map',
    'parallel_file_operations',
    'ParallelProcessor'
]

# Enforce parallel processing on import
if not USE_PARALLEL:
    raise ImportError(
        "PARALLEL PROCESSING IS MANDATORY! "
        "This module requires USE_PARALLEL=True"
    )
