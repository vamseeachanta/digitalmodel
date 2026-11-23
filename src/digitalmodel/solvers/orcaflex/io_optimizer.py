"""
OrcaFlex I/O Optimizer
Task 2.4: I/O Optimization
Implements async file operations, buffering, and batch writes for improved I/O performance.
"""

import os
import asyncio
import logging
import time
from pathlib import Path
from typing import List, Dict, Any, Optional, Callable
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
from functools import wraps
import json
import pickle
from datetime import datetime

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class IOOptimizer:
    """Optimize I/O operations for OrcaFlex file processing."""
    
    def __init__(self, buffer_size: int = 8192, max_workers: int = 4):
        """
        Initialize I/O optimizer.
        
        Args:
            buffer_size: Buffer size for file operations (default 8KB)
            max_workers: Max workers for async operations
        """
        self.buffer_size = buffer_size
        self.max_workers = max_workers
        self.io_stats = {
            'reads': 0,
            'writes': 0,
            'bytes_read': 0,
            'bytes_written': 0,
            'read_time': 0,
            'write_time': 0
        }
        
    async def async_read_file(self, file_path: str) -> bytes:
        """
        Read file asynchronously using thread pool.
        
        Args:
            file_path: Path to file
            
        Returns:
            File contents as bytes
        """
        start_time = time.time()
        
        def _read():
            with open(file_path, 'rb') as f:
                return f.read()
        
        try:
            loop = asyncio.get_event_loop()
            with ThreadPoolExecutor(max_workers=1) as executor:
                content = await loop.run_in_executor(executor, _read)
                
            self.io_stats['reads'] += 1
            self.io_stats['bytes_read'] += len(content)
            self.io_stats['read_time'] += time.time() - start_time
            
            logger.debug(f"Async read {len(content)} bytes from {file_path}")
            return content
            
        except Exception as e:
            logger.error(f"Error reading {file_path}: {e}")
            raise
    
    async def async_write_file(self, file_path: str, content: bytes) -> None:
        """
        Write file asynchronously using thread pool.
        
        Args:
            file_path: Path to file
            content: Content to write
        """
        start_time = time.time()
        
        def _write():
            # Ensure directory exists
            Path(file_path).parent.mkdir(parents=True, exist_ok=True)
            with open(file_path, 'wb') as f:
                f.write(content)
        
        try:
            loop = asyncio.get_event_loop()
            with ThreadPoolExecutor(max_workers=1) as executor:
                await loop.run_in_executor(executor, _write)
                
            self.io_stats['writes'] += 1
            self.io_stats['bytes_written'] += len(content)
            self.io_stats['write_time'] += time.time() - start_time
            
            logger.debug(f"Async wrote {len(content)} bytes to {file_path}")
            
        except Exception as e:
            logger.error(f"Error writing {file_path}: {e}")
            raise
    
    async def batch_read_files(self, file_paths: List[str]) -> Dict[str, bytes]:
        """
        Read multiple files asynchronously in batch.
        
        Args:
            file_paths: List of file paths
            
        Returns:
            Dictionary mapping file paths to contents
        """
        logger.info(f"Batch reading {len(file_paths)} files asynchronously")
        
        tasks = []
        for file_path in file_paths:
            task = self.async_read_file(file_path)
            tasks.append(task)
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        file_contents = {}
        for file_path, result in zip(file_paths, results):
            if isinstance(result, Exception):
                logger.error(f"Failed to read {file_path}: {result}")
                file_contents[file_path] = None
            else:
                file_contents[file_path] = result
        
        return file_contents
    
    async def batch_write_files(self, file_data: Dict[str, bytes]) -> List[str]:
        """
        Write multiple files asynchronously in batch.
        
        Args:
            file_data: Dictionary mapping file paths to contents
            
        Returns:
            List of successfully written file paths
        """
        logger.info(f"Batch writing {len(file_data)} files asynchronously")
        
        tasks = []
        for file_path, content in file_data.items():
            task = self.async_write_file(file_path, content)
            tasks.append(task)
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        successful = []
        for file_path, result in zip(file_data.keys(), results):
            if isinstance(result, Exception):
                logger.error(f"Failed to write {file_path}: {result}")
            else:
                successful.append(file_path)
        
        return successful
    
    def get_io_stats(self) -> Dict[str, Any]:
        """
        Get I/O statistics.
        
        Returns:
            I/O performance statistics
        """
        stats = self.io_stats.copy()
        
        # Calculate averages
        if stats['reads'] > 0:
            stats['avg_read_time'] = stats['read_time'] / stats['reads']
            stats['avg_read_size'] = stats['bytes_read'] / stats['reads']
            stats['read_throughput_mbps'] = (stats['bytes_read'] / 1e6) / stats['read_time'] if stats['read_time'] > 0 else 0
        
        if stats['writes'] > 0:
            stats['avg_write_time'] = stats['write_time'] / stats['writes']
            stats['avg_write_size'] = stats['bytes_written'] / stats['writes']
            stats['write_throughput_mbps'] = (stats['bytes_written'] / 1e6) / stats['write_time'] if stats['write_time'] > 0 else 0
        
        return stats


class BufferedFileWriter:
    """Buffered file writer for improved write performance."""
    
    def __init__(self, file_path: str, buffer_size: int = 65536):
        """
        Initialize buffered writer.
        
        Args:
            file_path: Output file path
            buffer_size: Buffer size in bytes (default 64KB)
        """
        self.file_path = file_path
        self.buffer_size = buffer_size
        self.buffer = []
        self.buffer_bytes = 0
        self.total_written = 0
        
        # Ensure directory exists
        Path(file_path).parent.mkdir(parents=True, exist_ok=True)
        
    def write(self, data: bytes) -> None:
        """
        Write data to buffer.
        
        Args:
            data: Data to write
        """
        self.buffer.append(data)
        self.buffer_bytes += len(data)
        
        # Flush if buffer is full
        if self.buffer_bytes >= self.buffer_size:
            self.flush()
    
    def flush(self) -> None:
        """Flush buffer to disk."""
        if not self.buffer:
            return
        
        with open(self.file_path, 'ab') as f:
            for data in self.buffer:
                f.write(data)
                self.total_written += len(data)
        
        logger.debug(f"Flushed {self.buffer_bytes} bytes to {self.file_path}")
        self.buffer = []
        self.buffer_bytes = 0
    
    def close(self) -> None:
        """Close writer and flush remaining data."""
        self.flush()
        logger.info(f"Closed buffered writer: {self.total_written} bytes written to {self.file_path}")
    
    def __enter__(self):
        """Context manager entry."""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()


class ParallelIOProcessor:
    """Process files with parallel I/O operations."""
    
    def __init__(self, num_workers: int = 4):
        """
        Initialize parallel I/O processor.
        
        Args:
            num_workers: Number of parallel workers
        """
        self.num_workers = num_workers
        self.io_optimizer = IOOptimizer()
        
    def process_files_parallel(self, 
                              file_paths: List[str],
                              process_func: Callable,
                              output_dir: str = "./output") -> Dict[str, Any]:
        """
        Process files with parallel I/O.
        
        Args:
            file_paths: List of input files
            process_func: Function to process each file
            output_dir: Output directory
            
        Returns:
            Processing results
        """
        logger.info(f"Processing {len(file_paths)} files with {self.num_workers} workers")
        
        start_time = time.time()
        results = []
        
        with ThreadPoolExecutor(max_workers=self.num_workers) as executor:
            # Submit all tasks
            futures = []
            for file_path in file_paths:
                future = executor.submit(self._process_single_file, 
                                       file_path, process_func, output_dir)
                futures.append(future)
            
            # Collect results
            for future in futures:
                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    logger.error(f"Processing failed: {e}")
                    results.append({'status': 'failed', 'error': str(e)})
        
        duration = time.time() - start_time
        
        # Get I/O statistics
        io_stats = self.io_optimizer.get_io_stats()
        
        return {
            'total_files': len(file_paths),
            'duration': duration,
            'results': results,
            'io_stats': io_stats,
            'throughput': len(file_paths) / duration if duration > 0 else 0
        }
    
    def _process_single_file(self, 
                           file_path: str,
                           process_func: Callable,
                           output_dir: str) -> Dict[str, Any]:
        """
        Process a single file with optimized I/O.
        
        Args:
            file_path: Input file path
            process_func: Processing function
            output_dir: Output directory
            
        Returns:
            Processing result
        """
        try:
            # Read with buffering
            with open(file_path, 'rb', buffering=65536) as f:
                content = f.read()
            
            # Process
            result = process_func(content)
            
            # Write with buffering
            output_path = Path(output_dir) / Path(file_path).name
            with BufferedFileWriter(str(output_path)) as writer:
                writer.write(result if isinstance(result, bytes) else str(result).encode())
            
            return {
                'file': file_path,
                'status': 'success',
                'output': str(output_path)
            }
            
        except Exception as e:
            return {
                'file': file_path,
                'status': 'failed',
                'error': str(e)
            }


class CachedFileReader:
    """Cached file reader to avoid redundant reads."""
    
    def __init__(self, cache_size: int = 100):
        """
        Initialize cached reader.
        
        Args:
            cache_size: Maximum number of files to cache
        """
        self.cache = {}
        self.cache_size = cache_size
        self.cache_hits = 0
        self.cache_misses = 0
        
    def read(self, file_path: str) -> bytes:
        """
        Read file with caching.
        
        Args:
            file_path: File path
            
        Returns:
            File contents
        """
        # Check cache
        if file_path in self.cache:
            self.cache_hits += 1
            logger.debug(f"Cache hit for {file_path}")
            return self.cache[file_path]
        
        # Cache miss - read from disk
        self.cache_misses += 1
        with open(file_path, 'rb') as f:
            content = f.read()
        
        # Add to cache (evict oldest if full)
        if len(self.cache) >= self.cache_size:
            # Simple FIFO eviction
            oldest = next(iter(self.cache))
            del self.cache[oldest]
        
        self.cache[file_path] = content
        return content
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        total_requests = self.cache_hits + self.cache_misses
        hit_rate = self.cache_hits / total_requests if total_requests > 0 else 0
        
        return {
            'cache_size': len(self.cache),
            'cache_hits': self.cache_hits,
            'cache_misses': self.cache_misses,
            'hit_rate': hit_rate,
            'total_cached_bytes': sum(len(v) for v in self.cache.values())
        }


def io_optimization_benchmark():
    """Run I/O optimization benchmark."""
    print("\n" + "="*60)
    print("I/O OPTIMIZATION BENCHMARK")
    print("="*60)
    
    # Create test data
    test_dir = Path("./io_test_data")
    test_dir.mkdir(exist_ok=True)
    
    # Create test files
    test_files = []
    for i in range(10):
        file_path = test_dir / f"test_{i}.dat"
        content = os.urandom(1024 * 1024)  # 1MB random data
        file_path.write_bytes(content)
        test_files.append(str(file_path))
    
    print(f"Created {len(test_files)} test files")
    
    # Test 1: Async I/O
    print("\n1. Testing Async I/O...")
    io_opt = IOOptimizer()
    
    async def test_async():
        start = time.time()
        contents = await io_opt.batch_read_files(test_files)
        read_time = time.time() - start
        
        output_files = {f"{f}.out": content for f, content in contents.items() if content}
        start = time.time()
        await io_opt.batch_write_files(output_files)
        write_time = time.time() - start
        
        return read_time, write_time
    
    read_time, write_time = asyncio.run(test_async())
    print(f"   Async Read: {read_time:.3f}s")
    print(f"   Async Write: {write_time:.3f}s")
    
    # Test 2: Buffered Writing
    print("\n2. Testing Buffered Writing...")
    start = time.time()
    with BufferedFileWriter("buffered_output.dat", buffer_size=65536) as writer:
        for _ in range(100):
            writer.write(os.urandom(10240))  # 10KB chunks
    buffered_time = time.time() - start
    print(f"   Buffered Write: {buffered_time:.3f}s")
    
    # Test 3: Cached Reading
    print("\n3. Testing Cached Reading...")
    cache = CachedFileReader(cache_size=5)
    
    start = time.time()
    # Read same files multiple times
    for _ in range(3):
        for file_path in test_files[:5]:
            _ = cache.read(file_path)
    cached_time = time.time() - start
    
    stats = cache.get_cache_stats()
    print(f"   Cached Read: {cached_time:.3f}s")
    print(f"   Cache Hit Rate: {stats['hit_rate']:.1%}")
    
    # Cleanup
    import shutil
    shutil.rmtree(test_dir, ignore_errors=True)
    for f in Path(".").glob("*.out"):
        f.unlink()
    Path("buffered_output.dat").unlink(missing_ok=True)
    
    print("\n" + "="*60)
    print("I/O optimization components tested successfully!")
    print("="*60)


if __name__ == "__main__":
    # Run benchmark
    io_optimization_benchmark()