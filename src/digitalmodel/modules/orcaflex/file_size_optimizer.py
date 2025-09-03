"""
OrcaFlex File Size-Based Thread Optimizer
Automatically determines optimal thread count based on .sim file sizes.

Key Finding: Large OrcaFlex files are I/O-bound and perform better with fewer threads.
"""

import os
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from statistics import median, mean

logger = logging.getLogger(__name__)


class FileSizeOptimizer:
    """
    Optimizes thread count based on OrcaFlex file sizes.
    Uses empirically-determined lookup table for best performance.
    """
    
    # Lookup table based on extensive testing
    # Format: (max_file_size_mb, optimal_threads)
    SIZE_THREAD_LOOKUP = [
        (50,    12),  # Very small files - more parallelism OK
        (100,   10),  # Small files - moderate parallelism
        (250,   8),   # Small-medium files
        (500,   8),   # Medium files - balanced
        (750,   6),   # Medium-large files
        (1000,  6),   # Large files - reduce contention
        (1500,  4),   # Very large files - minimize contention
        (2000,  4),   # Huge files
        (5000,  2),   # Extremely large files - nearly sequential
        (float('inf'), 2)  # Gigantic files - sequential processing
    ]
    
    # Alternative aggressive optimization (even fewer threads)
    AGGRESSIVE_LOOKUP = [
        (100,   8),
        (500,   6),
        (1000,  4),
        (2000,  2),
        (float('inf'), 2)
    ]
    
    def __init__(self, use_aggressive: bool = True):
        """
        Initialize the optimizer.
        
        Args:
            use_aggressive: If True, use more aggressive thread reduction
        """
        self.use_aggressive = use_aggressive
        self.lookup_table = self.AGGRESSIVE_LOOKUP if use_aggressive else self.SIZE_THREAD_LOOKUP
        self._file_size_cache = {}
        
    def get_file_size_mb(self, file_path: str) -> float:
        """
        Get file size in MB with caching.
        
        Args:
            file_path: Path to file
            
        Returns:
            File size in megabytes
        """
        if file_path in self._file_size_cache:
            return self._file_size_cache[file_path]
            
        try:
            size_bytes = os.path.getsize(file_path)
            size_mb = size_bytes / (1024 * 1024)
            self._file_size_cache[file_path] = size_mb
            return size_mb
        except (OSError, IOError) as e:
            logger.warning(f"Cannot get size of {file_path}: {e}")
            return 0.0
    
    def analyze_files(self, file_paths: List[str]) -> Dict[str, float]:
        """
        Analyze a list of files and return statistics.
        
        Args:
            file_paths: List of file paths to analyze
            
        Returns:
            Dictionary with file size statistics
        """
        sizes = []
        for file_path in file_paths:
            size = self.get_file_size_mb(file_path)
            if size > 0:
                sizes.append(size)
        
        if not sizes:
            return {
                'count': 0,
                'total_mb': 0,
                'avg_mb': 0,
                'median_mb': 0,
                'min_mb': 0,
                'max_mb': 0
            }
        
        return {
            'count': len(sizes),
            'total_mb': sum(sizes),
            'avg_mb': mean(sizes),
            'median_mb': median(sizes),
            'min_mb': min(sizes),
            'max_mb': max(sizes),
            'size_distribution': self._get_size_distribution(sizes)
        }
    
    def _get_size_distribution(self, sizes: List[float]) -> Dict[str, int]:
        """Get distribution of file sizes across categories."""
        distribution = {
            'tiny (<50MB)': 0,
            'small (50-100MB)': 0,
            'medium (100-500MB)': 0,
            'large (500MB-1GB)': 0,
            'xlarge (1-2GB)': 0,
            'huge (>2GB)': 0
        }
        
        for size in sizes:
            if size < 50:
                distribution['tiny (<50MB)'] += 1
            elif size < 100:
                distribution['small (50-100MB)'] += 1
            elif size < 500:
                distribution['medium (100-500MB)'] += 1
            elif size < 1000:
                distribution['large (500MB-1GB)'] += 1
            elif size < 2000:
                distribution['xlarge (1-2GB)'] += 1
            else:
                distribution['huge (>2GB)'] += 1
                
        return distribution
    
    def get_optimal_threads(self, file_paths: List[str], 
                          max_allowed: Optional[int] = None,
                          min_allowed: int = 1) -> Tuple[int, str]:
        """
        Determine optimal thread count based on file sizes.
        
        YML files have no file size dependency - they convert to SIM files
        and the input file size is small. Only SIM files need thread optimization
        based on their size.
        
        Args:
            file_paths: List of file paths to process
            max_allowed: Maximum threads allowed by system
            min_allowed: Minimum threads to use
            
        Returns:
            Tuple of (optimal_thread_count, reasoning_message)
        """
        # Check if all files are YML files (no size optimization needed)
        if file_paths and all(str(f).lower().endswith('.yml') for f in file_paths):
            # For YML files, use maximum available threads (up to max_allowed)
            # since these are small input files that generate SIM files
            cpu_count = os.cpu_count() or 1
            if max_allowed:
                optimal = min(max_allowed, cpu_count)
            else:
                optimal = cpu_count
            
            reason = (f"YML files: {len(file_paths)}, "
                     f"Using max threads: {optimal} (no file size dependency)")
            logger.info(f"Thread optimization: {reason}")
            return (optimal, reason)
        
        # Analyze files
        stats = self.analyze_files(file_paths)
        
        if stats['count'] == 0:
            return (min_allowed, "No valid files found - using minimum threads")
        
        # Use median size for robustness against outliers
        size_mb = stats['median_mb']
        
        # Find optimal threads from lookup table
        optimal = None
        for max_size, threads in self.lookup_table:
            if size_mb <= max_size:
                optimal = threads
                break
        
        if optimal is None:
            optimal = 2  # Fallback for extremely large files
        
        # Apply constraints
        if max_allowed:
            optimal = min(optimal, max_allowed)
        optimal = max(optimal, min_allowed)
        
        # Generate reasoning message
        size_desc = self._get_size_description(size_mb)
        mode = "aggressive" if self.use_aggressive else "standard"
        
        reason = (f"Files: {stats['count']}, "
                 f"Median size: {size_mb:.1f}MB ({size_desc}), "
                 f"Optimal threads: {optimal} ({mode} mode)")
        
        # Add distribution info if varied
        if stats['max_mb'] / stats['min_mb'] > 10:  # High variation
            reason += " - Note: File sizes vary significantly"
        
        logger.info(f"Thread optimization: {reason}")
        logger.debug(f"Size distribution: {stats['size_distribution']}")
        
        return (optimal, reason)
    
    def _get_size_description(self, size_mb: float) -> str:
        """Get human-readable size description."""
        if size_mb < 50:
            return "tiny"
        elif size_mb < 100:
            return "small"
        elif size_mb < 500:
            return "medium"
        elif size_mb < 1000:
            return "large"
        elif size_mb < 2000:
            return "very large"
        else:
            return "huge"
    
    def get_optimal_threads_for_single_file(self, file_path: str) -> int:
        """
        Get optimal thread count for a single file.
        
        Args:
            file_path: Path to file
            
        Returns:
            Optimal thread count
        """
        size_mb = self.get_file_size_mb(file_path)
        
        for max_size, threads in self.lookup_table:
            if size_mb <= max_size:
                return threads
        
        return 2  # Fallback
    
    def group_files_by_optimal_threads(self, file_paths: List[str]) -> Dict[int, List[str]]:
        """
        Group files by their optimal thread count.
        Useful for batch processing with different configurations.
        
        Args:
            file_paths: List of file paths
            
        Returns:
            Dictionary mapping thread count to list of files
        """
        groups = {}
        
        for file_path in file_paths:
            threads = self.get_optimal_threads_for_single_file(file_path)
            if threads not in groups:
                groups[threads] = []
            groups[threads].append(file_path)
        
        return groups
    
    def recommend_batch_strategy(self, file_paths: List[str]) -> List[Dict]:
        """
        Recommend a batch processing strategy.
        
        Args:
            file_paths: List of file paths
            
        Returns:
            List of batch configurations
        """
        groups = self.group_files_by_optimal_threads(file_paths)
        
        strategies = []
        for threads, files in sorted(groups.items()):
            # Analyze this group
            sizes = [self.get_file_size_mb(f) for f in files]
            avg_size = mean(sizes) if sizes else 0
            
            strategy = {
                'threads': threads,
                'files': files,
                'count': len(files),
                'avg_size_mb': avg_size,
                'total_size_mb': sum(sizes),
                'batch_size': self._get_optimal_batch_size(threads),
                'priority': self._get_priority(avg_size)
            }
            strategies.append(strategy)
        
        # Sort by priority (process smaller files first for quick wins)
        strategies.sort(key=lambda x: x['priority'])
        
        return strategies
    
    def _get_optimal_batch_size(self, threads: int) -> int:
        """Get optimal batch size for given thread count."""
        # More threads = larger batches
        # Fewer threads = smaller batches to avoid memory issues
        if threads >= 10:
            return threads * 2
        elif threads >= 6:
            return threads
        else:
            return max(1, threads // 2)
    
    def _get_priority(self, avg_size_mb: float) -> int:
        """Get processing priority (lower = higher priority)."""
        # Process smaller files first for quick wins
        if avg_size_mb < 100:
            return 1
        elif avg_size_mb < 500:
            return 2
        elif avg_size_mb < 1000:
            return 3
        else:
            return 4


def demonstrate_optimizer():
    """Demonstrate the file size optimizer."""
    
    print("OrcaFlex File Size Optimizer Demo")
    print("="*60)
    
    # Create test scenarios
    test_scenarios = [
        {
            'name': 'Small files',
            'files': [f'small_{i}.sim' for i in range(5)],
            'sizes': [50, 60, 70, 80, 90]  # MB
        },
        {
            'name': 'Medium files',
            'files': [f'medium_{i}.sim' for i in range(5)],
            'sizes': [200, 300, 400, 450, 500]  # MB
        },
        {
            'name': 'Large files',
            'files': [f'large_{i}.sim' for i in range(5)],
            'sizes': [600, 800, 900, 1000, 1200]  # MB
        },
        {
            'name': 'Mixed files',
            'files': [f'mixed_{i}.sim' for i in range(6)],
            'sizes': [50, 200, 500, 800, 1200, 2000]  # MB
        }
    ]
    
    optimizer = FileSizeOptimizer(use_aggressive=True)
    
    for scenario in test_scenarios:
        print(f"\nScenario: {scenario['name']}")
        print("-"*40)
        
        # Mock file sizes
        for file, size in zip(scenario['files'], scenario['sizes']):
            optimizer._file_size_cache[file] = size
        
        # Get optimal threads
        threads, reason = optimizer.get_optimal_threads(scenario['files'])
        print(f"Optimal threads: {threads}")
        print(f"Reasoning: {reason}")
        
        # Show batch strategy
        strategies = optimizer.recommend_batch_strategy(scenario['files'])
        if len(strategies) > 1:
            print("\nRecommended batch strategy:")
            for i, strategy in enumerate(strategies, 1):
                print(f"  Batch {i}: {strategy['count']} files, "
                      f"{strategy['threads']} threads, "
                      f"avg {strategy['avg_size_mb']:.0f}MB")
    
    print("\n" + "="*60)
    print("Optimization Guidelines:")
    print("  - Tiny files (<50MB): 12 threads")
    print("  - Small files (50-100MB): 10 threads")
    print("  - Medium files (100-500MB): 8 threads")
    print("  - Large files (500MB-1GB): 6 threads")
    print("  - Very large files (1-2GB): 4 threads")
    print("  - Huge files (>2GB): 2 threads")
    print("="*60)


if __name__ == "__main__":
    demonstrate_optimizer()