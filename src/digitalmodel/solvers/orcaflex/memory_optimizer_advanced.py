"""
Advanced Memory Optimizer for OrcaFlex
Task 2.2: Complete memory pooling and data structure optimization
Implements memory pools, optimized data structures, and advanced memory management.
"""

import gc
import sys
import weakref
import array
import logging
import psutil
import numpy as np
from typing import Dict, List, Any, Optional, Union, TypeVar
from collections import deque, OrderedDict
from dataclasses import dataclass
from datetime import datetime
import pickle
import mmap
import tempfile
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

T = TypeVar('T')


class MemoryPool:
    """
    Memory pool for reusable objects to reduce allocation overhead.
    """
    
    def __init__(self, object_type: type, initial_size: int = 10, max_size: int = 100):
        """
        Initialize memory pool.
        
        Args:
            object_type: Type of objects to pool
            initial_size: Initial pool size
            max_size: Maximum pool size
        """
        self.object_type = object_type
        self.max_size = max_size
        self.pool = deque(maxlen=max_size)
        self.allocated = 0
        self.reused = 0
        
        # Pre-allocate initial objects
        for _ in range(initial_size):
            self.pool.append(object_type())
            self.allocated += 1
    
    def acquire(self) -> Any:
        """
        Acquire an object from the pool.
        
        Returns:
            Object from pool or newly created
        """
        if self.pool:
            obj = self.pool.popleft()
            self.reused += 1
            logger.debug(f"Reused {self.object_type.__name__} from pool")
            return obj
        else:
            self.allocated += 1
            logger.debug(f"Allocated new {self.object_type.__name__}")
            return self.object_type()
    
    def release(self, obj: Any) -> None:
        """
        Release an object back to the pool.
        
        Args:
            obj: Object to release
        """
        if len(self.pool) < self.max_size:
            # Reset object state if it has a reset method
            if hasattr(obj, 'reset'):
                obj.reset()
            elif hasattr(obj, 'clear'):
                obj.clear()
            
            self.pool.append(obj)
            logger.debug(f"Released {self.object_type.__name__} to pool")
    
    def get_stats(self) -> Dict[str, int]:
        """Get pool statistics."""
        return {
            'pool_size': len(self.pool),
            'allocated': self.allocated,
            'reused': self.reused,
            'reuse_rate': self.reused / (self.allocated + self.reused) if (self.allocated + self.reused) > 0 else 0
        }


class OptimizedDataStructures:
    """
    Optimized data structures for better memory efficiency.
    """
    
    @staticmethod
    def create_compact_array(data: List[float], dtype: str = 'f') -> array.array:
        """
        Create memory-efficient array.
        
        Args:
            data: List of values
            dtype: Array type code ('f' for float, 'd' for double)
            
        Returns:
            Compact array
        """
        return array.array(dtype, data)
    
    @staticmethod
    def create_numpy_array(data: List[float], dtype: np.dtype = np.float32) -> np.ndarray:
        """
        Create optimized numpy array.
        
        Args:
            data: List of values
            dtype: Numpy data type
            
        Returns:
            Numpy array with specified dtype
        """
        return np.array(data, dtype=dtype)
    
    @staticmethod
    def create_bounded_cache(max_size: int = 1000) -> OrderedDict:
        """
        Create size-bounded LRU cache.
        
        Args:
            max_size: Maximum cache size
            
        Returns:
            Bounded OrderedDict
        """
        class BoundedOrderedDict(OrderedDict):
            def __init__(self, max_size):
                super().__init__()
                self.max_size = max_size
            
            def __setitem__(self, key, value):
                if key in self:
                    del self[key]
                elif len(self) >= self.max_size:
                    self.popitem(last=False)
                super().__setitem__(key, value)
        
        return BoundedOrderedDict(max_size)
    
    @staticmethod
    def optimize_dict_memory(data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Optimize dictionary memory usage.
        
        Args:
            data: Dictionary to optimize
            
        Returns:
            Memory-optimized dictionary
        """
        # Use __slots__ for keys if possible
        optimized = {}
        
        for key, value in data.items():
            # Intern string keys for memory savings
            if isinstance(key, str):
                key = sys.intern(key)
            
            # Optimize numeric values
            if isinstance(value, float):
                # Use float32 if precision allows
                if abs(value) < 1e6 and abs(value) > 1e-6:
                    value = np.float32(value)
            elif isinstance(value, list) and len(value) > 100:
                # Convert large lists to arrays
                if all(isinstance(x, (int, float)) for x in value):
                    value = array.array('f', value)
            
            optimized[key] = value
        
        return optimized


@dataclass
class MemoryMappedBuffer:
    """
    Memory-mapped buffer for large data processing.
    """
    
    def __init__(self, size: int = 1024 * 1024 * 100):  # 100MB default
        """
        Initialize memory-mapped buffer.
        
        Args:
            size: Buffer size in bytes
        """
        self.size = size
        self.temp_file = tempfile.NamedTemporaryFile(delete=False)
        self.temp_file.write(b'\0' * size)
        self.temp_file.close()
        
        self.file = open(self.temp_file.name, 'r+b')
        self.mmap = mmap.mmap(self.file.fileno(), size)
        self.position = 0
    
    def write(self, data: bytes) -> int:
        """
        Write data to buffer.
        
        Args:
            data: Data to write
            
        Returns:
            Number of bytes written
        """
        data_len = len(data)
        if self.position + data_len > self.size:
            raise ValueError("Buffer overflow")
        
        self.mmap[self.position:self.position + data_len] = data
        self.position += data_len
        return data_len
    
    def read(self, size: int = -1) -> bytes:
        """
        Read data from buffer.
        
        Args:
            size: Number of bytes to read (-1 for all)
            
        Returns:
            Data bytes
        """
        if size == -1:
            size = self.position
        
        self.mmap.seek(0)
        return self.mmap.read(size)
    
    def reset(self):
        """Reset buffer position."""
        self.position = 0
    
    def close(self):
        """Close and cleanup buffer."""
        self.mmap.close()
        self.file.close()
        Path(self.temp_file.name).unlink(missing_ok=True)
    
    def __del__(self):
        """Cleanup on deletion."""
        try:
            self.close()
        except:
            pass


class WeakReferenceCache:
    """
    Cache using weak references to allow garbage collection.
    """
    
    def __init__(self):
        """Initialize weak reference cache."""
        self.cache = weakref.WeakValueDictionary()
        self.strong_refs = OrderedDict()
        self.max_strong_refs = 10
    
    def set(self, key: str, value: Any) -> None:
        """
        Add item to cache.
        
        Args:
            key: Cache key
            value: Value to cache
        """
        self.cache[key] = value
        
        # Keep strong reference for recently used items
        if key in self.strong_refs:
            del self.strong_refs[key]
        self.strong_refs[key] = value
        
        # Limit strong references
        if len(self.strong_refs) > self.max_strong_refs:
            self.strong_refs.popitem(last=False)
    
    def get(self, key: str) -> Optional[Any]:
        """
        Get item from cache.
        
        Args:
            key: Cache key
            
        Returns:
            Cached value or None
        """
        value = self.cache.get(key)
        if value is not None:
            # Refresh strong reference
            if key in self.strong_refs:
                del self.strong_refs[key]
            self.strong_refs[key] = value
        return value
    
    def clear_strong_refs(self):
        """Clear strong references to allow GC."""
        self.strong_refs.clear()


class AdvancedMemoryOptimizer:
    """
    Advanced memory optimization combining all techniques.
    """
    
    def __init__(self):
        """Initialize advanced memory optimizer."""
        self.pools = {}
        self.weak_cache = WeakReferenceCache()
        self.mmap_buffers = []
        self.optimization_stats = {
            'pools_created': 0,
            'objects_pooled': 0,
            'memory_saved_mb': 0,
            'gc_collections': 0
        }
        
        # Configure aggressive garbage collection
        gc.set_threshold(700, 10, 10)
    
    def create_pool(self, name: str, object_type: type, 
                   initial_size: int = 10, max_size: int = 100) -> MemoryPool:
        """
        Create a memory pool for a specific object type.
        
        Args:
            name: Pool name
            object_type: Type of objects to pool
            initial_size: Initial pool size
            max_size: Maximum pool size
            
        Returns:
            Created memory pool
        """
        pool = MemoryPool(object_type, initial_size, max_size)
        self.pools[name] = pool
        self.optimization_stats['pools_created'] += 1
        logger.info(f"Created memory pool '{name}' for {object_type.__name__}")
        return pool
    
    def get_from_pool(self, pool_name: str) -> Any:
        """
        Get object from named pool.
        
        Args:
            pool_name: Name of the pool
            
        Returns:
            Object from pool
        """
        if pool_name in self.pools:
            obj = self.pools[pool_name].acquire()
            self.optimization_stats['objects_pooled'] += 1
            return obj
        else:
            raise KeyError(f"Pool '{pool_name}' not found")
    
    def return_to_pool(self, pool_name: str, obj: Any) -> None:
        """
        Return object to pool.
        
        Args:
            pool_name: Name of the pool
            obj: Object to return
        """
        if pool_name in self.pools:
            self.pools[pool_name].release(obj)
    
    def optimize_large_data(self, data: Any, use_mmap: bool = False) -> Any:
        """
        Optimize large data structures.
        
        Args:
            data: Data to optimize
            use_mmap: Use memory mapping for very large data
            
        Returns:
            Optimized data
        """
        initial_memory = psutil.Process().memory_info().rss / 1e6  # MB
        
        if isinstance(data, list):
            # Convert to numpy array if numeric
            if all(isinstance(x, (int, float)) for x in data):
                optimized = OptimizedDataStructures.create_numpy_array(data)
            else:
                optimized = data
        elif isinstance(data, dict):
            optimized = OptimizedDataStructures.optimize_dict_memory(data)
        elif use_mmap and isinstance(data, bytes) and len(data) > 10 * 1024 * 1024:  # >10MB
            # Use memory mapping for very large binary data
            buffer = MemoryMappedBuffer(len(data))
            buffer.write(data)
            self.mmap_buffers.append(buffer)
            optimized = buffer
        else:
            optimized = data
        
        # Force garbage collection
        gc.collect()
        self.optimization_stats['gc_collections'] += 1
        
        final_memory = psutil.Process().memory_info().rss / 1e6  # MB
        saved = initial_memory - final_memory
        if saved > 0:
            self.optimization_stats['memory_saved_mb'] += saved
            logger.info(f"Memory optimization saved {saved:.2f}MB")
        
        return optimized
    
    def aggressive_cleanup(self) -> Dict[str, Any]:
        """
        Perform aggressive memory cleanup.
        
        Returns:
            Cleanup statistics
        """
        initial_memory = psutil.Process().memory_info().rss / 1e9  # GB
        
        # Clear caches
        self.weak_cache.clear_strong_refs()
        
        # Multiple GC passes
        collected = 0
        for i in range(3):
            collected += gc.collect(i)
        
        # Clear interpreter caches
        if hasattr(sys, 'intern'):
            # This would clear interned strings but it's not directly accessible
            pass
        
        final_memory = psutil.Process().memory_info().rss / 1e9  # GB
        
        stats = {
            'objects_collected': collected,
            'initial_memory_gb': initial_memory,
            'final_memory_gb': final_memory,
            'memory_freed_gb': initial_memory - final_memory,
            'pools_stats': {name: pool.get_stats() for name, pool in self.pools.items()}
        }
        
        logger.info(f"Aggressive cleanup freed {stats['memory_freed_gb']:.3f}GB")
        return stats
    
    def get_optimization_report(self) -> Dict[str, Any]:
        """
        Get comprehensive optimization report.
        
        Returns:
            Optimization statistics and recommendations
        """
        memory_info = psutil.virtual_memory()
        process_info = psutil.Process()
        
        report = {
            'timestamp': datetime.now().isoformat(),
            'system_memory': {
                'total_gb': memory_info.total / 1e9,
                'available_gb': memory_info.available / 1e9,
                'percent_used': memory_info.percent
            },
            'process_memory': {
                'rss_gb': process_info.memory_info().rss / 1e9,
                'vms_gb': process_info.memory_info().vms / 1e9
            },
            'optimization_stats': self.optimization_stats,
            'pools': {name: pool.get_stats() for name, pool in self.pools.items()},
            'gc_stats': gc.get_stats() if hasattr(gc, 'get_stats') else [],
            'recommendations': self._generate_recommendations()
        }
        
        return report
    
    def _generate_recommendations(self) -> List[str]:
        """Generate optimization recommendations."""
        recommendations = []
        
        memory_info = psutil.virtual_memory()
        if memory_info.percent > 80:
            recommendations.append("High memory usage - consider increasing system RAM or reducing batch size")
        
        for name, pool in self.pools.items():
            stats = pool.get_stats()
            if stats['reuse_rate'] < 0.5:
                recommendations.append(f"Low reuse rate for pool '{name}' - consider adjusting pool size")
        
        if self.optimization_stats['gc_collections'] > 100:
            recommendations.append("Frequent GC collections - consider optimizing object creation patterns")
        
        return recommendations


def benchmark_memory_optimizations():
    """Benchmark memory optimization techniques."""
    print("\n" + "="*60)
    print("MEMORY OPTIMIZATION BENCHMARK")
    print("="*60)
    
    optimizer = AdvancedMemoryOptimizer()
    
    # Test 1: Memory Pooling
    print("\n1. Testing Memory Pooling...")
    optimizer.create_pool('list_pool', list, initial_size=5, max_size=20)
    
    pooled_objects = []
    for i in range(10):
        obj = optimizer.get_from_pool('list_pool')
        obj.append(i)
        pooled_objects.append(obj)
    
    for obj in pooled_objects[:5]:
        obj.clear()
        optimizer.return_to_pool('list_pool', obj)
    
    pool_stats = optimizer.pools['list_pool'].get_stats()
    print(f"   Pool reuse rate: {pool_stats['reuse_rate']:.1%}")
    
    # Test 2: Data Structure Optimization
    print("\n2. Testing Data Structure Optimization...")
    
    # Large list to optimize
    large_list = [float(i) * 1.1 for i in range(100000)]
    initial_size = sys.getsizeof(large_list) / 1e6  # MB
    
    # Optimize to array
    optimized = OptimizedDataStructures.create_numpy_array(large_list, np.float32)
    optimized_size = optimized.nbytes / 1e6  # MB
    
    print(f"   Original size: {initial_size:.2f}MB")
    print(f"   Optimized size: {optimized_size:.2f}MB")
    print(f"   Savings: {(1 - optimized_size/initial_size)*100:.1f}%")
    
    # Test 3: Memory-Mapped Buffer
    print("\n3. Testing Memory-Mapped Buffer...")
    
    buffer = MemoryMappedBuffer(size=10 * 1024 * 1024)  # 10MB
    test_data = b"x" * (1024 * 1024)  # 1MB
    
    for _ in range(5):
        buffer.write(test_data)
    
    print(f"   Buffer used: {buffer.position / 1e6:.1f}MB")
    buffer.close()
    
    # Test 4: Aggressive Cleanup
    print("\n4. Testing Aggressive Cleanup...")
    
    # Create some garbage
    garbage = [list(range(1000)) for _ in range(1000)]
    del garbage
    
    cleanup_stats = optimizer.aggressive_cleanup()
    print(f"   Objects collected: {cleanup_stats['objects_collected']}")
    print(f"   Memory freed: {cleanup_stats['memory_freed_gb']:.3f}GB")
    
    # Final report
    print("\n5. Optimization Report:")
    report = optimizer.get_optimization_report()
    print(f"   Process memory: {report['process_memory']['rss_gb']:.2f}GB")
    print(f"   Pools created: {report['optimization_stats']['pools_created']}")
    print(f"   Objects pooled: {report['optimization_stats']['objects_pooled']}")
    
    print("\n" + "="*60)
    print("Memory optimization benchmark completed successfully!")
    print("="*60)


if __name__ == "__main__":
    benchmark_memory_optimizations()