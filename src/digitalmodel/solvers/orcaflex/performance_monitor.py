"""
OrcaFlex Performance Monitor and Optimizer
Implements dynamic resource allocation and performance monitoring.
"""

import os
import time
import logging
import psutil
import gc
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from functools import wraps
from datetime import datetime
import json

logger = logging.getLogger(__name__)


class PerformanceMonitor:
    """Monitor and track performance metrics for OrcaFlex operations."""
    
    def __init__(self):
        self.metrics = []
        self.process = psutil.Process()
        self.start_time = None
        
    def start_monitoring(self):
        """Start performance monitoring."""
        self.start_time = time.time()
        self.initial_memory = self.process.memory_info().rss / 1e9  # GB
        logger.info(f"Started monitoring - Initial memory: {self.initial_memory:.2f} GB")
        
    def record_metric(self, operation: str, file_path: str = None):
        """Record a performance metric."""
        if not self.start_time:
            self.start_monitoring()
            
        current_time = time.time()
        elapsed = current_time - self.start_time
        memory_usage = self.process.memory_info().rss / 1e9  # GB
        cpu_percent = self.process.cpu_percent()
        
        metric = {
            'timestamp': datetime.now().isoformat(),
            'operation': operation,
            'file_path': file_path,
            'elapsed_time': elapsed,
            'memory_gb': memory_usage,
            'cpu_percent': cpu_percent,
            'num_threads': len(self.process.threads())
        }
        
        self.metrics.append(metric)
        logger.debug(f"Metric: {operation} - Time: {elapsed:.2f}s, Memory: {memory_usage:.2f}GB")
        
        return metric
    
    def get_summary(self) -> Dict[str, Any]:
        """Get performance summary."""
        if not self.metrics:
            return {}
            
        total_time = time.time() - self.start_time if self.start_time else 0
        max_memory = max(m['memory_gb'] for m in self.metrics)
        avg_cpu = sum(m['cpu_percent'] for m in self.metrics) / len(self.metrics)
        
        return {
            'total_time': total_time,
            'max_memory_gb': max_memory,
            'avg_cpu_percent': avg_cpu,
            'num_operations': len(self.metrics),
            'metrics': self.metrics
        }
    
    def save_report(self, filepath: str):
        """Save performance report to file."""
        summary = self.get_summary()
        with open(filepath, 'w') as f:
            json.dump(summary, f, indent=2)
        logger.info(f"Performance report saved to {filepath}")


def performance_timer(func):
    """Decorator to time function execution."""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        duration = time.time() - start
        logger.debug(f"{func.__name__} took {duration:.2f}s")
        return result
    return wrapper


class ResourceManager:
    """Manage computational resources for OrcaFlex analysis."""
    
    def __init__(self):
        self.cpu_count = psutil.cpu_count()
        self.total_memory = psutil.virtual_memory().total / 1e9  # GB
        self.file_size_cache = {}
        
    def analyze_file_sizes(self, file_paths: List[str]) -> Dict[str, Any]:
        """Analyze file sizes for optimization."""
        sizes = []
        for file_path in file_paths:
            if file_path in self.file_size_cache:
                size = self.file_size_cache[file_path]
            else:
                try:
                    size = os.path.getsize(file_path)
                    self.file_size_cache[file_path] = size
                except OSError:
                    size = 0
            sizes.append(size)
        
        if not sizes:
            return {'avg_size': 0, 'max_size': 0, 'min_size': 0, 'total_size': 0}
            
        avg_size = sum(sizes) / len(sizes)
        
        return {
            'avg_size': avg_size,
            'max_size': max(sizes),
            'min_size': min(sizes),
            'total_size': sum(sizes),
            'count': len(sizes),
            'size_distribution': self._categorize_sizes(sizes)
        }
    
    def _categorize_sizes(self, sizes: List[int]) -> Dict[str, int]:
        """Categorize file sizes into buckets."""
        categories = {
            'small': 0,    # < 100MB
            'medium': 0,   # 100MB - 500MB  
            'large': 0,    # 500MB - 1GB
            'xlarge': 0    # > 1GB
        }
        
        for size in sizes:
            size_mb = size / 1e6
            if size_mb < 100:
                categories['small'] += 1
            elif size_mb < 500:
                categories['medium'] += 1
            elif size_mb < 1000:
                categories['large'] += 1
            else:
                categories['xlarge'] += 1
                
        return categories
    
    def calculate_optimal_threads(self, file_paths: List[str], 
                                 max_threads: int = 30) -> int:
        """
        Calculate optimal thread count based on file sizes.
        
        Quick Win #1: Dynamic thread adjustment based on file size
        Updated based on testing: Fewer threads for larger files (I/O bound)
        """
        file_stats = self.analyze_file_sizes(file_paths)
        avg_size = file_stats['avg_size']
        
        # Determine optimal threads based on average file size
        # UPDATED: Based on latest testing, aggressive thread reduction improves performance
        if avg_size < 100_000_000:  # < 100MB - CPU bound
            num_threads = min(10, max_threads)  # Reduced from 30
        elif avg_size < 500_000_000:  # 100-500MB - Mixed
            num_threads = min(8, max_threads)  # Reduced from 20
        elif avg_size < 1_000_000_000:  # 500MB-1GB - I/O bound
            num_threads = min(6, max_threads)  # Reduced from 15
        else:  # > 1GB - Heavily I/O bound
            num_threads = min(4, max_threads)  # Reduced from 10 - maximum I/O efficiency
        
        # Consider system resources
        available_memory = psutil.virtual_memory().available / 1e9  # GB
        memory_per_thread = 0.5  # Assume 0.5GB per thread
        max_threads_memory = int(available_memory / memory_per_thread)
        
        # Final decision
        optimal_threads = min(num_threads, max_threads_memory, self.cpu_count)
        
        logger.info(f"File size analysis: {file_stats['size_distribution']}")
        logger.info(f"Average file size: {avg_size/1e6:.1f}MB")
        logger.info(f"Optimal threads: {optimal_threads} (CPU: {self.cpu_count}, Memory allows: {max_threads_memory})")
        
        return optimal_threads
    
    def monitor_resources(self) -> Dict[str, float]:
        """Monitor current resource usage."""
        return {
            'cpu_percent': psutil.cpu_percent(interval=0.1),
            'memory_percent': psutil.virtual_memory().percent,
            'memory_available_gb': psutil.virtual_memory().available / 1e9,
            'disk_io_read_mb': psutil.disk_io_counters().read_bytes / 1e6 if hasattr(psutil.disk_io_counters(), 'read_bytes') else 0,
            'disk_io_write_mb': psutil.disk_io_counters().write_bytes / 1e6 if hasattr(psutil.disk_io_counters(), 'write_bytes') else 0
        }
    
    def should_adjust_threads(self, current_threads: int) -> Tuple[bool, int]:
        """
        Determine if thread count should be adjusted based on current resources.
        
        Returns:
            Tuple of (should_adjust, new_thread_count)
        """
        resources = self.monitor_resources()
        
        # High memory pressure - reduce threads
        if resources['memory_percent'] > 85:
            new_threads = max(1, int(current_threads * 0.75))
            return True, new_threads
            
        # Low resource usage - can increase threads
        if resources['memory_percent'] < 50 and resources['cpu_percent'] < 50:
            new_threads = min(45, int(current_threads * 1.25))
            return True, new_threads
            
        return False, current_threads


class MemoryOptimizer:
    """
    Optimize memory usage during OrcaFlex operations.
    
    Quick Win #2: Enable garbage collection
    """
    
    def __init__(self):
        self.gc_enabled = True
        self.gc_threshold = 100_000_000  # 100MB
        self.last_gc_time = time.time()
        self.gc_interval = 60  # seconds
        
    def optimize_memory(self):
        """Perform memory optimization."""
        if not self.gc_enabled:
            return
            
        current_time = time.time()
        if current_time - self.last_gc_time > self.gc_interval:
            collected = gc.collect()
            self.last_gc_time = current_time
            if collected > 0:
                logger.debug(f"Garbage collection freed {collected} objects")
    
    def cleanup_after_file(self):
        """
        Clean up memory after processing a file.
        Quick Win #2: Force garbage collection after each file
        """
        collected = gc.collect()
        logger.debug(f"Post-file cleanup: {collected} objects collected")
        
    def get_memory_stats(self) -> Dict[str, float]:
        """Get current memory statistics."""
        import sys
        return {
            'rss_gb': psutil.Process().memory_info().rss / 1e9,
            'vms_gb': psutil.Process().memory_info().vms / 1e9,
            'available_gb': psutil.virtual_memory().available / 1e9,
            'percent_used': psutil.virtual_memory().percent,
            'gc_stats': gc.get_stats() if hasattr(gc, 'get_stats') else []
        }


class BatchOptimizer:
    """Optimize batch processing of OrcaFlex files."""
    
    def __init__(self):
        self.resource_manager = ResourceManager()
        
    def group_files_by_size(self, file_paths: List[str]) -> Dict[str, List[str]]:
        """
        Group files by size for optimal processing.
        
        Returns:
            Dictionary with size categories as keys and file lists as values
        """
        groups = {
            'small': [],    # < 100MB
            'medium': [],   # 100MB - 500MB
            'large': [],    # 500MB - 1GB
            'xlarge': []    # > 1GB
        }
        
        for file_path in file_paths:
            try:
                size = os.path.getsize(file_path) / 1e6  # MB
                if size < 100:
                    groups['small'].append(file_path)
                elif size < 500:
                    groups['medium'].append(file_path)
                elif size < 1000:
                    groups['large'].append(file_path)
                else:
                    groups['xlarge'].append(file_path)
            except OSError:
                groups['small'].append(file_path)  # Default to small if can't determine
                
        return groups
    
    def get_optimal_batch_config(self, file_groups: Dict[str, List[str]]) -> List[Dict[str, Any]]:
        """
        Get optimal configuration for each batch.
        
        Returns:
            List of batch configurations
        """
        configs = []
        
        # Configure each group with optimal settings
        if file_groups['small']:
            configs.append({
                'files': file_groups['small'],
                'threads': 45,
                'category': 'small',
                'batch_size': 10
            })
            
        if file_groups['medium']:
            configs.append({
                'files': file_groups['medium'],
                'threads': 30,
                'category': 'medium',
                'batch_size': 5
            })
            
        if file_groups['large']:
            configs.append({
                'files': file_groups['large'],
                'threads': 15,
                'category': 'large',
                'batch_size': 3
            })
            
        if file_groups['xlarge']:
            configs.append({
                'files': file_groups['xlarge'],
                'threads': 8,
                'category': 'xlarge',
                'batch_size': 1
            })
            
        return configs


def create_performance_dashboard(metrics_file: str = "performance_metrics.json"):
    """
    Create a simple performance dashboard from collected metrics.
    Quick Win #3: Basic monitoring and reporting
    """
    if not os.path.exists(metrics_file):
        logger.warning(f"Metrics file not found: {metrics_file}")
        return
        
    with open(metrics_file, 'r') as f:
        data = json.load(f)
    
    print("\n" + "="*60)
    print("ORCAFLEX PERFORMANCE DASHBOARD")
    print("="*60)
    print(f"Total Runtime: {data.get('total_time', 0):.2f}s")
    print(f"Max Memory: {data.get('max_memory_gb', 0):.2f}GB")
    print(f"Avg CPU: {data.get('avg_cpu_percent', 0):.1f}%")
    print(f"Operations: {data.get('num_operations', 0)}")
    
    if 'metrics' in data and data['metrics']:
        print("\nTop 5 Longest Operations:")
        sorted_metrics = sorted(data['metrics'], key=lambda x: x['elapsed_time'], reverse=True)[:5]
        for i, metric in enumerate(sorted_metrics, 1):
            print(f"  {i}. {metric['operation']}: {metric['elapsed_time']:.2f}s")
    
    print("="*60 + "\n")


if __name__ == "__main__":
    # Example usage
    print("OrcaFlex Performance Monitor & Optimizer")
    print("="*60)
    
    # Test resource manager
    rm = ResourceManager()
    print(f"System Resources:")
    print(f"  CPUs: {rm.cpu_count}")
    print(f"  Total Memory: {rm.total_memory:.1f}GB")
    print(f"  Current Resources: {rm.monitor_resources()}")
    
    # Test memory optimizer
    mo = MemoryOptimizer()
    print(f"\nMemory Stats:")
    stats = mo.get_memory_stats()
    print(f"  RSS: {stats['rss_gb']:.2f}GB")
    print(f"  Available: {stats['available_gb']:.2f}GB")
    print(f"  % Used: {stats['percent_used']:.1f}%")
    
    # Test batch optimizer
    bo = BatchOptimizer()
    test_files = ['file1.dat', 'file2.dat', 'file3.dat']
    groups = bo.group_files_by_size(test_files)
    configs = bo.get_optimal_batch_config(groups)
    print(f"\nBatch Configurations: {len(configs)} groups")
    
    print("="*60)