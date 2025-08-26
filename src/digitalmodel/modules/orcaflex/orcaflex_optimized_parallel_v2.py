"""
OrcaFlex Optimized Parallel Analysis Module V2
Enhanced version with performance optimizations including:
- Dynamic thread allocation based on file sizes
- Memory optimization with garbage collection
- Performance monitoring and reporting
- Fixed multiprocessing for Windows
"""

import os
import logging
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
import time
from datetime import datetime
import traceback
import gc

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available. Running in mock mode.")

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - Process-%(process)d - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


# Standalone function for multiprocessing (must be at module level)
def process_single_file_worker(file_info: Dict[str, Any]) -> Dict[str, Any]:
    """
    Process a single OrcaFlex file. Standalone function for multiprocessing.
    
    Args:
        file_info: Dictionary containing file path and configuration
        
    Returns:
        Dictionary with processing results
    """
    file_path = file_info['file_path']
    config = file_info.get('config', {})
    
    result = {
        'file_path': file_path,
        'status': 'pending',
        'start_time': datetime.now(),
        'end_time': None,
        'duration': None,
        'error': None,
        'output_files': [],
        'file_size_mb': 0
    }
    
    try:
        # Record file size for metrics
        file_size = os.path.getsize(file_path) / 1e6 if os.path.exists(file_path) else 0  # MB
        result['file_size_mb'] = file_size
        
        # Check if OrcFxAPI is available
        if not ORCAFLEX_AVAILABLE:
            # Mock mode for testing
            time.sleep(0.1)  # Simulate processing
            result['status'] = 'mock_success'
            result['output_files'] = ['mock_output.sim']
        else:
            # Create model
            model = OrcFxAPI.Model()
            
            # Load the file
            logger.info(f"Loading: {file_path} ({file_size:.1f}MB)")
            model.LoadData(str(file_path))
            
            # Determine what analyses to run
            run_static = config.get('static', True)
            run_dynamic = config.get('dynamic', False)
            save_sim = config.get('save_sim', True)
            save_dat = config.get('save_dat', False)
            
            # Get output file names
            input_path = Path(file_path)
            output_dir = Path(config.get('output_dir', input_path.parent))
            output_dir.mkdir(parents=True, exist_ok=True)
            
            base_name = input_path.stem
            
            # Run static analysis if requested
            if run_static:
                logger.info(f"Running static analysis: {file_path}")
                model.CalculateStatics()
            
            # Run dynamic analysis if requested
            if run_dynamic:
                logger.info(f"Running dynamic analysis: {file_path}")
                model.RunSimulation()
            
            # Save results
            if save_sim:
                sim_file = output_dir / f"{base_name}.sim"
                model.SaveSimulation(str(sim_file))
                result['output_files'].append(str(sim_file))
                logger.info(f"Saved: {sim_file}")
            
            if save_dat:
                dat_file = output_dir / f"{base_name}_processed.dat"
                model.SaveData(str(dat_file))
                result['output_files'].append(str(dat_file))
                logger.info(f"Saved: {dat_file}")
            
            result['status'] = 'success'
        
        # Quick Win #2: Clean up memory after processing
        gc.collect()
        
    except Exception as e:
        result['status'] = 'failed'
        result['error'] = str(e)
        logger.error(f"Failed to process {file_path}: {e}")
        logger.debug(traceback.format_exc())
    
    finally:
        result['end_time'] = datetime.now()
        result['duration'] = (result['end_time'] - result['start_time']).total_seconds()
        
    return result


class SimpleResourceManager:
    """Simplified resource management for optimization."""
    
    @staticmethod
    def calculate_optimal_threads(file_paths: List[str], max_threads: int = 45) -> int:
        """
        Calculate optimal thread count based on file sizes.
        Quick Win #1: Dynamic thread adjustment
        """
        import psutil
        
        # Calculate average file size
        sizes = []
        for file_path in file_paths:
            try:
                size = os.path.getsize(file_path) if os.path.exists(file_path) else 0
                sizes.append(size)
            except:
                sizes.append(0)
        
        avg_size = sum(sizes) / len(sizes) if sizes else 0
        
        # Determine optimal threads based on average file size
        # UPDATED: Aggressive thread reduction for better I/O performance
        if avg_size < 100_000_000:  # < 100MB
            num_threads = min(10, max_threads)  # Reduced from 45
        elif avg_size < 500_000_000:  # < 500MB
            num_threads = min(8, max_threads)  # Reduced from 30
        else:  # >= 500MB
            num_threads = min(6, max_threads)  # Reduced from 15
        
        # Consider system resources
        cpu_count = psutil.cpu_count()
        available_memory = psutil.virtual_memory().available / 1e9  # GB
        memory_per_thread = 0.5  # Assume 0.5GB per thread
        max_threads_memory = int(available_memory / memory_per_thread)
        
        # Final decision
        optimal_threads = min(num_threads, max_threads_memory, cpu_count)
        
        logger.info(f"Average file size: {avg_size/1e6:.1f}MB")
        logger.info(f"Optimal threads: {optimal_threads} (CPU: {cpu_count}, Memory allows: {max_threads_memory})")
        
        return optimal_threads


class OrcaFlexOptimizedParallelAnalysis:
    """
    Optimized parallel processing for OrcaFlex analysis files.
    Includes dynamic resource allocation and performance monitoring.
    """
    
    def __init__(self, num_threads: int = None, use_processes: bool = True):
        """
        Initialize optimized parallel analysis handler.
        
        Args:
            num_threads: Number of parallel threads/processes (None for auto-detection)
            use_processes: If True, use ProcessPoolExecutor; if False, use ThreadPoolExecutor
        """
        self.use_processes = use_processes
        self.results = []
        self.failed_files = []
        
        # Dynamic thread allocation if not specified
        self.num_threads = num_threads
        self.dynamic_threads = (num_threads is None)
        
        # Set up multiprocessing method for Windows
        if os.name == 'nt':  # Windows
            try:
                mp.set_start_method('spawn', force=True)
            except RuntimeError:
                pass  # Already set
        
        logger.info(f"Initialized OrcaFlexOptimizedParallelAnalysis")
        logger.info(f"Dynamic thread allocation: {self.dynamic_threads}")
        logger.info(f"Using {'processes' if use_processes else 'threads'} for parallelization")
    
    def process_files_parallel(self, 
                              file_list: List[str],
                              config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Process multiple files in parallel with optimizations.
        
        Args:
            file_list: List of file paths to process
            config: Configuration dictionary
            
        Returns:
            Processing summary with performance metrics
        """
        if not file_list:
            return {'status': 'no_files', 'results': []}
        
        config = config or {}
        
        # Quick Win #1: Dynamic thread allocation based on file sizes
        if self.dynamic_threads:
            self.num_threads = SimpleResourceManager.calculate_optimal_threads(file_list)
        else:
            # Still log the recommendation
            recommended = SimpleResourceManager.calculate_optimal_threads(file_list)
            logger.info(f"Using {self.num_threads} threads (recommended: {recommended})")
        
        # Use optimized default if still None
        if self.num_threads is None:
            self.num_threads = 8  # Further optimized - testing shows <10 threads provides best I/O performance
        
        logger.info(f"Processing {len(file_list)} files with {self.num_threads} threads")
        
        start_time = datetime.now()
        results = []
        
        # Prepare file info for processing
        file_infos = [{'file_path': f, 'config': config} for f in file_list]
        
        # Create executor
        ExecutorClass = ProcessPoolExecutor if self.use_processes else ThreadPoolExecutor
        
        # Quick Win #3: Add basic monitoring
        import psutil
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1e9  # GB
        logger.info(f"Initial memory: {initial_memory:.2f}GB")
        
        with ExecutorClass(max_workers=self.num_threads) as executor:
            # Submit all files
            future_to_file = {
                executor.submit(process_single_file_worker, file_info): file_info['file_path']
                for file_info in file_infos
            }
            
            # Process results as they complete
            completed = 0
            for future in as_completed(future_to_file):
                file_path = future_to_file[future]
                completed += 1
                
                try:
                    result = future.result()
                    results.append(result)
                    
                    # Progress update
                    status = result['status']
                    duration = result.get('duration', 0)
                    size_mb = result.get('file_size_mb', 0)
                    
                    # Quick Win #3: Monitor memory and CPU
                    current_memory = process.memory_info().rss / 1e9
                    cpu_percent = process.cpu_percent()
                    
                    logger.info(f"[{completed}/{len(file_list)}] {Path(file_path).name}: "
                              f"{status} ({duration:.2f}s, {size_mb:.1f}MB) "
                              f"Memory: {current_memory:.2f}GB, CPU: {cpu_percent:.1f}%")
                    
                    if status in ['failed', 'exception']:
                        self.failed_files.append(file_path)
                        
                except Exception as e:
                    logger.error(f"Exception processing {file_path}: {e}")
                    results.append({
                        'file_path': file_path,
                        'status': 'exception',
                        'error': str(e)
                    })
                    self.failed_files.append(file_path)
        
        # Quick Win #2: Final garbage collection
        gc.collect()
        
        end_time = datetime.now()
        total_duration = (end_time - start_time).total_seconds()
        
        # Calculate summary statistics
        successful = sum(1 for r in results if r.get('status') in ['success', 'mock_success'])
        failed = sum(1 for r in results if r.get('status') in ['failed', 'exception'])
        
        # Calculate performance metrics
        total_processing_time = sum(r.get('duration', 0) for r in results)
        parallel_speedup = total_processing_time / total_duration if total_duration > 0 else 1
        
        # Final memory stats
        final_memory = process.memory_info().rss / 1e9
        memory_increase = final_memory - initial_memory
        
        summary = {
            'total_files': len(file_list),
            'successful': successful,
            'failed': failed,
            'total_duration': total_duration,
            'average_duration': total_duration / len(file_list) if file_list else 0,
            'parallel_speedup': parallel_speedup,
            'effective_threads': self.num_threads,
            'results': results,
            'performance_stats': {
                'initial_memory_gb': initial_memory,
                'final_memory_gb': final_memory,
                'memory_increase_gb': memory_increase,
                'dynamic_threads_used': self.dynamic_threads
            }
        }
        
        logger.info(f"Completed processing {len(file_list)} files in {total_duration:.2f}s")
        logger.info(f"Success: {successful}, Failed: {failed}")
        logger.info(f"Parallel speedup: {parallel_speedup:.2f}x")
        logger.info(f"Memory usage: {initial_memory:.2f}GB -> {final_memory:.2f}GB (+{memory_increase:.2f}GB)")
        
        return summary
    
    def process_directory(self, 
                         directory: str,
                         pattern: str = "*.dat",
                         config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Process all matching files in a directory with optimizations.
        
        Args:
            directory: Directory path containing OrcaFlex files
            pattern: File pattern to match (default: "*.dat")
            config: Configuration dictionary
            
        Returns:
            Processing summary
        """
        dir_path = Path(directory)
        if not dir_path.exists():
            logger.error(f"Directory not found: {directory}")
            return {'status': 'directory_not_found', 'results': []}
        
        # Find all matching files
        file_list = list(dir_path.glob(pattern))
        file_paths = [str(f) for f in file_list]
        
        logger.info(f"Found {len(file_paths)} files matching '{pattern}' in {directory}")
        
        if not file_paths:
            return {'status': 'no_matching_files', 'results': []}
        
        return self.process_files_parallel(file_paths, config)


def run_optimized_parallel_analysis(file_list: List[str],
                                   num_threads: int = None,
                                   config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
    """
    Convenience function to run optimized parallel OrcaFlex analysis.
    
    Args:
        file_list: List of file paths to process
        num_threads: Number of parallel threads (None for auto-detection)
        config: Configuration dictionary
        
    Returns:
        Processing summary with performance metrics
    """
    analyzer = OrcaFlexOptimizedParallelAnalysis(num_threads=num_threads)
    return analyzer.process_files_parallel(file_list, config)


if __name__ == "__main__":
    # Example usage
    print("OrcaFlex Optimized Parallel Analysis Module V2")
    print("="*60)
    
    # Check environment
    if ORCAFLEX_AVAILABLE:
        print("[OK] OrcFxAPI available")
    else:
        print("[WARNING] OrcFxAPI not available - running in mock mode")
    
    # Example configuration
    example_config = {
        'static': True,
        'dynamic': False,
        'save_sim': True,
        'save_dat': False,
        'output_dir': './results_optimized'
    }
    
    # Test with mock files
    print("\nTesting optimizations with mock files...")
    test_files = [f"test_file_{i}.dat" for i in range(5)]
    
    # Create mock files of different sizes for testing
    import tempfile
    with tempfile.TemporaryDirectory() as tmpdir:
        mock_files = []
        for i, filename in enumerate(test_files):
            filepath = Path(tmpdir) / filename
            # Create files of varying sizes (just create small files for testing)
            filepath.write_text(f"Mock OrcaFlex file {i}")
            mock_files.append(str(filepath))
        
        # Run optimized analysis
        print(f"\nProcessing {len(mock_files)} mock files...")
        analyzer = OrcaFlexOptimizedParallelAnalysis()
        summary = analyzer.process_files_parallel(mock_files[:3], example_config)
        
        print(f"\nResults:")
        print(f"  Total files: {summary['total_files']}")
        print(f"  Successful: {summary['successful']}")
        print(f"  Failed: {summary['failed']}")
        print(f"  Total time: {summary['total_duration']:.2f}s")
        print(f"  Speedup: {summary['parallel_speedup']:.2f}x")
        print(f"  Threads used: {summary['effective_threads']}")
        print(f"  Memory change: +{summary['performance_stats']['memory_increase_gb']:.2f}GB")
    
    print("\n" + "="*60)
    print("Quick Win Optimizations Implemented:")
    print("  [OK] Dynamic thread allocation based on file size")
    print("  [OK] Memory optimization with garbage collection")
    print("  [OK] Basic performance monitoring (CPU, memory)")
    print("="*60)