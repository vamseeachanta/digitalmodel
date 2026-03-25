"""
OrcaFlex Optimized Parallel Analysis Module
Enhanced version with performance optimizations including:
- Dynamic thread allocation based on file sizes
- Memory optimization with garbage collection
- Performance monitoring and reporting
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

# Import performance optimization modules
from .performance_monitor import (
    PerformanceMonitor, 
    ResourceManager, 
    MemoryOptimizer,
    BatchOptimizer,
    performance_timer,
    create_performance_dashboard
)

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


class OrcaFlexOptimizedParallelAnalysis:
    """
    Optimized parallel processing for OrcaFlex analysis files.
    Includes dynamic resource allocation and performance monitoring.
    """
    
    def __init__(self, num_threads: int = None, use_processes: bool = True, 
                 enable_monitoring: bool = True):
        """
        Initialize optimized parallel analysis handler.
        
        Args:
            num_threads: Number of parallel threads/processes (None for auto-detection)
            use_processes: If True, use ProcessPoolExecutor; if False, use ThreadPoolExecutor
            enable_monitoring: Enable performance monitoring
        """
        self.use_processes = use_processes
        self.enable_monitoring = enable_monitoring
        self.results = []
        self.failed_files = []
        
        # Initialize optimization components
        self.resource_manager = ResourceManager()
        self.memory_optimizer = MemoryOptimizer()
        self.batch_optimizer = BatchOptimizer()
        self.performance_monitor = PerformanceMonitor() if enable_monitoring else None
        
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
    
    def process_single_file(self, file_info: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process a single OrcaFlex file with optimizations.
        
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
            file_size = os.path.getsize(file_path) / 1e6  # MB
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
            self.memory_optimizer.cleanup_after_file()
            
        except Exception as e:
            result['status'] = 'failed'
            result['error'] = str(e)
            logger.error(f"Failed to process {file_path}: {e}")
            logger.debug(traceback.format_exc())
        
        finally:
            result['end_time'] = datetime.now()
            result['duration'] = (result['end_time'] - result['start_time']).total_seconds()
            
        return result
    
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
        
        # Start performance monitoring
        if self.performance_monitor:
            self.performance_monitor.start_monitoring()
        
        # Quick Win #1: Dynamic thread allocation based on file sizes
        if self.dynamic_threads:
            self.num_threads = self.resource_manager.calculate_optimal_threads(file_list)
        else:
            # Still log the recommendation
            recommended = self.resource_manager.calculate_optimal_threads(file_list)
            logger.info(f"Using {self.num_threads} threads (recommended: {recommended})")
        
        # Group files by size for optimal processing
        file_groups = self.batch_optimizer.group_files_by_size(file_list)
        batch_configs = self.batch_optimizer.get_optimal_batch_config(file_groups)
        
        logger.info(f"Processing {len(file_list)} files with optimized batching")
        for batch_config in batch_configs:
            category = batch_config['category']
            count = len(batch_config['files'])
            threads = batch_config['threads']
            logger.info(f"  {category}: {count} files with {threads} threads")
        
        start_time = datetime.now()
        results = []
        
        # Process each batch with optimal settings
        for batch_config in batch_configs:
            batch_files = batch_config['files']
            batch_threads = min(batch_config['threads'], self.num_threads or 45)
            
            if not batch_files:
                continue
            
            # Prepare file info for processing
            file_infos = [{'file_path': f, 'config': config} for f in batch_files]
            
            # Create executor with optimal thread count for this batch
            ExecutorClass = ProcessPoolExecutor if self.use_processes else ThreadPoolExecutor
            
            with ExecutorClass(max_workers=batch_threads) as executor:
                # Submit all files in this batch
                future_to_file = {
                    executor.submit(self.process_single_file, file_info): file_info['file_path']
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
                        logger.info(f"[{completed}/{len(batch_files)}] {Path(file_path).name}: "
                                  f"{status} ({duration:.2f}s, {size_mb:.1f}MB)")
                        
                        if status in ['failed', 'exception']:
                            self.failed_files.append(file_path)
                        
                        # Record metric
                        if self.performance_monitor:
                            self.performance_monitor.record_metric(
                                f"process_file_{batch_config['category']}", 
                                file_path
                            )
                        
                        # Check if we should adjust resources
                        if completed % 5 == 0:  # Check every 5 files
                            should_adjust, new_threads = self.resource_manager.should_adjust_threads(batch_threads)
                            if should_adjust:
                                logger.info(f"Resource adjustment suggested: {batch_threads} -> {new_threads} threads")
                                
                    except Exception as e:
                        logger.error(f"Exception processing {file_path}: {e}")
                        results.append({
                            'file_path': file_path,
                            'status': 'exception',
                            'error': str(e)
                        })
                        self.failed_files.append(file_path)
            
            # Quick Win #2: Force garbage collection between batches
            gc.collect()
            logger.info(f"Completed {batch_config['category']} batch, memory cleaned")
        
        end_time = datetime.now()
        total_duration = (end_time - start_time).total_seconds()
        
        # Calculate summary statistics
        successful = sum(1 for r in results if r.get('status') in ['success', 'mock_success'])
        failed = sum(1 for r in results if r.get('status') in ['failed', 'exception'])
        
        # Calculate performance metrics
        total_processing_time = sum(r.get('duration', 0) for r in results)
        parallel_speedup = total_processing_time / total_duration if total_duration > 0 else 1
        
        summary = {
            'total_files': len(file_list),
            'successful': successful,
            'failed': failed,
            'total_duration': total_duration,
            'average_duration': total_duration / len(file_list) if file_list else 0,
            'parallel_speedup': parallel_speedup,
            'effective_threads': self.num_threads,
            'results': results,
            'optimization_stats': {
                'dynamic_threads_used': self.dynamic_threads,
                'memory_cleanups': len(batch_configs),
                'file_size_distribution': file_groups
            }
        }
        
        # Add performance report if monitoring enabled
        if self.performance_monitor:
            summary['performance_metrics'] = self.performance_monitor.get_summary()
            # Save report
            report_file = config.get('performance_report', 'performance_report.json')
            self.performance_monitor.save_report(report_file)
            create_performance_dashboard(report_file)
        
        logger.info(f"Completed processing {len(file_list)} files in {total_duration:.2f}s")
        logger.info(f"Success: {successful}, Failed: {failed}")
        logger.info(f"Parallel speedup: {parallel_speedup:.2f}x")
        logger.info(f"Effective threads used: {self.num_threads}")
        
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
                                   config: Optional[Dict[str, Any]] = None,
                                   enable_monitoring: bool = True) -> Dict[str, Any]:
    """
    Convenience function to run optimized parallel OrcaFlex analysis.
    
    Args:
        file_list: List of file paths to process
        num_threads: Number of parallel threads (None for auto-detection)
        config: Configuration dictionary
        enable_monitoring: Enable performance monitoring
        
    Returns:
        Processing summary with performance metrics
    """
    analyzer = OrcaFlexOptimizedParallelAnalysis(
        num_threads=num_threads,
        enable_monitoring=enable_monitoring
    )
    return analyzer.process_files_parallel(file_list, config)


if __name__ == "__main__":
    # Example usage
    print("OrcaFlex Optimized Parallel Analysis Module")
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
        'output_dir': './results_optimized',
        'performance_report': 'performance_metrics.json'
    }
    
    # Test with mock files
    print("\nTesting optimizations with mock files...")
    test_files = [f"test_file_{i}.dat" for i in range(10)]
    
    # Create mock files of different sizes for testing
    import tempfile
    with tempfile.TemporaryDirectory() as tmpdir:
        mock_files = []
        for i, filename in enumerate(test_files):
            filepath = Path(tmpdir) / filename
            # Create files of varying sizes
            size = (i % 3 + 1) * 50 * 1024 * 1024  # 50MB, 100MB, 150MB
            filepath.write_bytes(b'0' * min(size, 1024))  # Write small amount for testing
            mock_files.append(str(filepath))
        
        # Run optimized analysis
        print(f"\nProcessing {len(mock_files)} mock files...")
        analyzer = OrcaFlexOptimizedParallelAnalysis(enable_monitoring=True)
        summary = analyzer.process_files_parallel(mock_files[:3], example_config)
        
        print(f"\nResults:")
        print(f"  Total files: {summary['total_files']}")
        print(f"  Successful: {summary['successful']}")
        print(f"  Failed: {summary['failed']}")
        print(f"  Total time: {summary['total_duration']:.2f}s")
        print(f"  Speedup: {summary['parallel_speedup']:.2f}x")
        print(f"  Threads used: {summary['effective_threads']}")
    
    print("\n" + "="*60)
    print("Optimizations implemented:")
    print("  [OK] Dynamic thread allocation based on file size")
    print("  [OK] Memory optimization with garbage collection")
    print("  [OK] Performance monitoring and reporting")
    print("  [OK] Batch grouping by file size")
    print("="*60)