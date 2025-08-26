"""
OrcaFlex Baseline Performance Test
Establishes performance baseline for optimization comparison.
Task 1.1: Establish Current Performance Baseline
"""

import os
import time
import json
import argparse
import psutil
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional
import logging

# Import the optimized modules
from digitalmodel.modules.orcaflex.performance_monitor import (
    PerformanceMonitor,
    ResourceManager,
    create_performance_dashboard
)
from digitalmodel.modules.orcaflex.orcaflex_parallel_analysis import OrcaFlexParallelAnalysis
from digitalmodel.modules.orcaflex.orcaflex_optimized_parallel_v2 import OrcaFlexOptimizedParallelAnalysis

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class BaselinePerformanceTest:
    """Run baseline performance tests for OrcaFlex batch processing."""
    
    def __init__(self, test_directory: str = None):
        """
        Initialize baseline test.
        
        Args:
            test_directory: Directory containing test files
        """
        self.test_directory = test_directory
        self.results = {}
        self.performance_monitor = PerformanceMonitor()
        self.resource_manager = ResourceManager()
        
    def scan_test_files(self, pattern: str = "*.dat") -> List[str]:
        """
        Scan directory for test files.
        
        Args:
            pattern: File pattern to match
            
        Returns:
            List of file paths
        """
        if not self.test_directory:
            logger.warning("No test directory specified")
            return []
            
        test_path = Path(self.test_directory)
        if not test_path.exists():
            logger.error(f"Test directory not found: {self.test_directory}")
            return []
            
        files = list(test_path.glob(pattern))
        logger.info(f"Found {len(files)} files matching '{pattern}'")
        
        # Analyze file sizes
        file_stats = self.resource_manager.analyze_file_sizes([str(f) for f in files])
        logger.info(f"File size distribution: {file_stats['size_distribution']}")
        logger.info(f"Average size: {file_stats['avg_size']/1e6:.1f}MB")
        logger.info(f"Total size: {file_stats['total_size']/1e6:.1f}MB")
        
        return [str(f) for f in files]
    
    def run_baseline_test(self, files: List[str], num_threads: int = 30) -> Dict[str, Any]:
        """
        Run baseline test with original parallel processor.
        
        Args:
            files: List of files to process
            num_threads: Number of threads to use
            
        Returns:
            Test results dictionary
        """
        logger.info(f"Running BASELINE test with {num_threads} threads")
        logger.info("="*60)
        
        # Start monitoring
        self.performance_monitor.start_monitoring()
        process = psutil.Process()
        
        # Record initial state
        initial_memory = process.memory_info().rss / 1e9
        initial_cpu = psutil.cpu_percent(interval=0.1)
        
        # Run baseline processor
        analyzer = OrcaFlexParallelAnalysis(num_threads=num_threads)
        
        start_time = time.time()
        results = analyzer.process_files_parallel(
            files,
            config={
                'static': True,
                'dynamic': False,
                'save_sim': True,
                'output_dir': './baseline_results'
            }
        )
        end_time = time.time()
        
        # Record final state
        final_memory = process.memory_info().rss / 1e9
        final_cpu = psutil.cpu_percent(interval=0.1)
        
        # Calculate metrics
        duration = end_time - start_time
        memory_used = final_memory - initial_memory
        
        baseline_results = {
            'test_type': 'baseline',
            'num_threads': num_threads,
            'num_files': len(files),
            'duration': duration,
            'initial_memory_gb': initial_memory,
            'final_memory_gb': final_memory,
            'memory_used_gb': memory_used,
            'initial_cpu': initial_cpu,
            'final_cpu': final_cpu,
            'successful': results.get('successful', 0),
            'failed': results.get('failed', 0),
            'parallel_speedup': results.get('parallel_speedup', 1.0),
            'average_time_per_file': duration / len(files) if files else 0
        }
        
        logger.info(f"Baseline test completed in {duration:.2f}s")
        logger.info(f"Memory used: {memory_used:.2f}GB")
        logger.info(f"Files processed: {results.get('successful', 0)}/{len(files)}")
        
        return baseline_results
    
    def run_optimized_test(self, files: List[str], num_threads: int = None) -> Dict[str, Any]:
        """
        Run test with optimized processor.
        
        Args:
            files: List of files to process
            num_threads: Number of threads (None for auto-detect)
            
        Returns:
            Test results dictionary
        """
        logger.info(f"Running OPTIMIZED test with {'auto' if num_threads is None else num_threads} threads")
        logger.info("="*60)
        
        # Start monitoring
        process = psutil.Process()
        
        # Record initial state
        initial_memory = process.memory_info().rss / 1e9
        initial_cpu = psutil.cpu_percent(interval=0.1)
        
        # Run optimized processor
        analyzer = OrcaFlexOptimizedParallelAnalysis(num_threads=num_threads)
        
        start_time = time.time()
        results = analyzer.process_files_parallel(
            files,
            config={
                'static': True,
                'dynamic': False,
                'save_sim': True,
                'output_dir': './optimized_results',
                'performance_report': 'optimized_performance.json'
            }
        )
        end_time = time.time()
        
        # Record final state
        final_memory = process.memory_info().rss / 1e9
        final_cpu = psutil.cpu_percent(interval=0.1)
        
        # Calculate metrics
        duration = end_time - start_time
        memory_used = final_memory - initial_memory
        
        optimized_results = {
            'test_type': 'optimized',
            'num_threads': results.get('effective_threads', 'auto'),
            'num_files': len(files),
            'duration': duration,
            'initial_memory_gb': initial_memory,
            'final_memory_gb': final_memory,
            'memory_used_gb': memory_used,
            'initial_cpu': initial_cpu,
            'final_cpu': final_cpu,
            'successful': results.get('successful', 0),
            'failed': results.get('failed', 0),
            'parallel_speedup': results.get('parallel_speedup', 1.0),
            'average_time_per_file': duration / len(files) if files else 0,
            'dynamic_threads_used': results.get('performance_stats', {}).get('dynamic_threads_used', False)
        }
        
        logger.info(f"Optimized test completed in {duration:.2f}s")
        logger.info(f"Memory used: {memory_used:.2f}GB")
        logger.info(f"Files processed: {results.get('successful', 0)}/{len(files)}")
        
        return optimized_results
    
    def compare_results(self, baseline: Dict[str, Any], optimized: Dict[str, Any]) -> Dict[str, Any]:
        """
        Compare baseline and optimized results.
        
        Args:
            baseline: Baseline test results
            optimized: Optimized test results
            
        Returns:
            Comparison metrics
        """
        comparison = {
            'runtime_improvement': (baseline['duration'] - optimized['duration']) / baseline['duration'] * 100,
            'memory_improvement': (baseline['memory_used_gb'] - optimized['memory_used_gb']) / baseline['memory_used_gb'] * 100 if baseline['memory_used_gb'] > 0 else 0,
            'speedup_ratio': baseline['duration'] / optimized['duration'] if optimized['duration'] > 0 else 1,
            'baseline_duration': baseline['duration'],
            'optimized_duration': optimized['duration'],
            'baseline_memory': baseline['memory_used_gb'],
            'optimized_memory': optimized['memory_used_gb'],
            'baseline_threads': baseline['num_threads'],
            'optimized_threads': optimized['num_threads']
        }
        
        return comparison
    
    def run_full_baseline_test(self, pattern: str = "*.dat") -> Dict[str, Any]:
        """
        Run complete baseline test suite.
        
        Args:
            pattern: File pattern to match
            
        Returns:
            Complete test results
        """
        # Scan for test files
        files = self.scan_test_files(pattern)
        if not files:
            logger.error("No test files found")
            return {'error': 'No test files found'}
        
        # Limit to reasonable number for testing
        if len(files) > 10:
            logger.info(f"Limiting test to first 10 files (found {len(files)})")
            files = files[:10]
        
        logger.info("\n" + "="*60)
        logger.info("STARTING BASELINE PERFORMANCE TEST")
        logger.info("="*60 + "\n")
        
        # Run baseline test (using old suboptimal default for comparison)
        baseline_results = self.run_baseline_test(files, num_threads=30)
        
        # Clear memory between tests
        import gc
        gc.collect()
        time.sleep(2)  # Allow system to stabilize
        
        logger.info("\n" + "="*60)
        logger.info("STARTING OPTIMIZED PERFORMANCE TEST")
        logger.info("="*60 + "\n")
        
        # Run optimized test
        optimized_results = self.run_optimized_test(files, num_threads=None)
        
        # Compare results
        comparison = self.compare_results(baseline_results, optimized_results)
        
        # Compile full results
        full_results = {
            'test_date': datetime.now().isoformat(),
            'test_files': len(files),
            'baseline': baseline_results,
            'optimized': optimized_results,
            'comparison': comparison
        }
        
        # Save results
        self.save_results(full_results)
        self.print_summary(full_results)
        
        return full_results
    
    def save_results(self, results: Dict[str, Any]):
        """Save test results to file."""
        filename = f"baseline_test_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(filename, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        logger.info(f"Results saved to {filename}")
    
    def print_summary(self, results: Dict[str, Any]):
        """Print test summary."""
        print("\n" + "="*60)
        print("BASELINE PERFORMANCE TEST SUMMARY")
        print("="*60)
        print(f"Test Date: {results['test_date']}")
        print(f"Files Tested: {results['test_files']}")
        print("\nBASELINE RESULTS:")
        print(f"  Duration: {results['baseline']['duration']:.2f}s")
        print(f"  Memory Used: {results['baseline']['memory_used_gb']:.2f}GB")
        print(f"  Threads: {results['baseline']['num_threads']}")
        print(f"  Speedup: {results['baseline']['parallel_speedup']:.2f}x")
        print("\nOPTIMIZED RESULTS:")
        print(f"  Duration: {results['optimized']['duration']:.2f}s")
        print(f"  Memory Used: {results['optimized']['memory_used_gb']:.2f}GB")
        print(f"  Threads: {results['optimized']['num_threads']}")
        print(f"  Speedup: {results['optimized']['parallel_speedup']:.2f}x")
        print("\nIMPROVEMENTS:")
        print(f"  Runtime: {results['comparison']['runtime_improvement']:.1f}% faster")
        print(f"  Memory: {results['comparison']['memory_improvement']:.1f}% less")
        print(f"  Overall Speedup: {results['comparison']['speedup_ratio']:.2f}x")
        print("="*60 + "\n")


def main():
    """Main entry point for baseline testing."""
    parser = argparse.ArgumentParser(description='OrcaFlex Baseline Performance Test')
    parser.add_argument(
        '--directory', '-d',
        type=str,
        help='Directory containing OrcaFlex files to test'
    )
    parser.add_argument(
        '--pattern', '-p',
        type=str,
        default='*.dat',
        help='File pattern to match (default: *.dat)'
    )
    parser.add_argument(
        '--threads',
        type=int,
        default=30,
        help='Number of threads for baseline test (default: 30)'
    )
    parser.add_argument(
        '--quick',
        action='store_true',
        help='Run quick test with fewer files'
    )
    
    args = parser.parse_args()
    
    # Use provided directory or try to find test files
    test_dir = args.directory
    if not test_dir:
        # Try common locations
        possible_dirs = [
            './test_data',
            './orcaflex_files',
            './data',
            '../../../test_data',
            'D:/github/digitalmodel/test_data'
        ]
        for dir_path in possible_dirs:
            if Path(dir_path).exists():
                test_dir = dir_path
                logger.info(f"Using test directory: {test_dir}")
                break
    
    if not test_dir:
        logger.error("No test directory specified or found")
        logger.info("Please specify a directory with --directory or create ./test_data/")
        return 1
    
    # Run baseline test
    tester = BaselinePerformanceTest(test_directory=test_dir)
    results = tester.run_full_baseline_test(pattern=args.pattern)
    
    # Check if optimization was successful
    if 'comparison' in results:
        if results['comparison']['runtime_improvement'] > 0:
            logger.info("✅ Optimization successful! Performance improved.")
            return 0
        else:
            logger.warning("⚠️ Optimization did not improve performance")
            return 1
    
    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())