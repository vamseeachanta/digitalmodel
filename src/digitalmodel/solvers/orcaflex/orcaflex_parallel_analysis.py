"""
OrcaFlex Parallel Analysis Module
Handles multiple OrcaFlex files with multiprocessing support.
Default: 30 threads for parallel processing.
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

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available. Running in mock mode.")

# Configure logging for multiprocessing
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - Process-%(process)d - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaFlexParallelAnalysis:
    """
    Parallel processing for OrcaFlex analysis files.
    Handles multiple .dat/.yml files concurrently using multiprocessing.
    """
    
    def __init__(self, num_threads: int = 30, use_processes: bool = True):
        """
        Initialize parallel analysis handler.
        
        Args:
            num_threads: Number of parallel threads/processes (default: 30)
            use_processes: If True, use ProcessPoolExecutor; if False, use ThreadPoolExecutor
        """
        self.num_threads = num_threads
        self.use_processes = use_processes
        self.results = []
        self.failed_files = []
        
        # Set up multiprocessing method for Windows
        if os.name == 'nt':  # Windows
            mp.set_start_method('spawn', force=True)
        
        logger.info(f"Initialized OrcaFlexParallelAnalysis with {num_threads} threads")
        logger.info(f"Using {'processes' if use_processes else 'threads'} for parallelization")
    
    def process_single_file(self, file_info: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process a single OrcaFlex file.
        This method runs in a separate process/thread.
        
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
            'output_files': []
        }
        
        try:
            # Check if OrcFxAPI is available
            if not ORCAFLEX_AVAILABLE:
                raise ImportError("OrcFxAPI module not available")
            
            # Create model
            model = OrcFxAPI.Model()
            
            # Load the file
            logger.info(f"Loading: {file_path}")
            model.LoadData(str(file_path))
            
            # Determine what analyses to run
            run_static = config.get('static', True)
            run_dynamic = config.get('dynamic', False)
            save_sim = config.get('save_sim', True)
            save_dat = config.get('save_dat', False)
            
            # Get output file names
            file_stem = Path(file_path).stem
            output_dir = Path(config.get('output_dir', Path(file_path).parent))
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Run static analysis
            if run_static:
                logger.info(f"Running static analysis: {file_path}")
                model.CalculateStatics()
                result['static_complete'] = True
            
            # Run dynamic simulation
            if run_dynamic:
                logger.info(f"Running dynamic simulation: {file_path}")
                model.RunSimulation()
                result['dynamic_complete'] = True
            
            # Save SIM file
            if save_sim:
                sim_path = output_dir / f"{file_stem}.sim"
                logger.info(f"Saving SIM: {sim_path}")
                model.SaveSimulation(str(sim_path))
                result['output_files'].append(str(sim_path))
            
            # Save DAT file
            if save_dat:
                dat_path = output_dir / f"{file_stem}_processed.dat"
                logger.info(f"Saving DAT: {dat_path}")
                model.SaveData(str(dat_path))
                result['output_files'].append(str(dat_path))
            
            result['status'] = 'success'
            logger.info(f"Successfully processed: {file_path}")
            
        except Exception as e:
            result['status'] = 'failed'
            result['error'] = str(e)
            result['traceback'] = traceback.format_exc()
            logger.error(f"Failed to process {file_path}: {e}")
        
        finally:
            result['end_time'] = datetime.now()
            result['duration'] = (result['end_time'] - result['start_time']).total_seconds()
        
        return result
    
    def process_files_parallel(self, 
                             file_list: List[str], 
                             config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Process multiple OrcaFlex files in parallel.
        
        Args:
            file_list: List of file paths to process
            config: Configuration dictionary for all files
            
        Returns:
            Dictionary with processing summary and results
        """
        if not file_list:
            logger.warning("No files to process")
            return {'status': 'no_files', 'results': []}
        
        # Default configuration
        default_config = {
            'static': True,
            'dynamic': False,
            'save_sim': True,
            'save_dat': False,
            'output_dir': None
        }
        
        if config:
            default_config.update(config)
        
        # Prepare file info for processing
        file_infos = [
            {'file_path': file_path, 'config': default_config}
            for file_path in file_list
        ]
        
        logger.info(f"Starting parallel processing of {len(file_list)} files")
        logger.info(f"Using {self.num_threads} parallel {'processes' if self.use_processes else 'threads'}")
        
        start_time = datetime.now()
        results = []
        
        # Choose executor based on configuration
        ExecutorClass = ProcessPoolExecutor if self.use_processes else ThreadPoolExecutor
        
        # Process files in parallel
        with ExecutorClass(max_workers=self.num_threads) as executor:
            # Submit all tasks
            future_to_file = {
                executor.submit(self.process_single_file, file_info): file_info['file_path']
                for file_info in file_infos
            }
            
            # Collect results as they complete
            completed = 0
            for future in as_completed(future_to_file):
                file_path = future_to_file[future]
                completed += 1
                
                try:
                    result = future.result()
                    results.append(result)
                    
                    # Progress update
                    status = result['status']
                    duration = result['duration']
                    logger.info(f"[{completed}/{len(file_list)}] {file_path}: {status} ({duration:.2f}s)")
                    
                    if status == 'failed':
                        self.failed_files.append(file_path)
                        
                except Exception as e:
                    logger.error(f"Exception processing {file_path}: {e}")
                    results.append({
                        'file_path': file_path,
                        'status': 'exception',
                        'error': str(e)
                    })
                    self.failed_files.append(file_path)
        
        end_time = datetime.now()
        total_duration = (end_time - start_time).total_seconds()
        
        # Calculate summary statistics
        successful = sum(1 for r in results if r.get('status') == 'success')
        failed = sum(1 for r in results if r.get('status') in ['failed', 'exception'])
        
        summary = {
            'total_files': len(file_list),
            'successful': successful,
            'failed': failed,
            'total_duration': total_duration,
            'average_duration': total_duration / len(file_list) if file_list else 0,
            'parallel_speedup': (sum(r.get('duration', 0) for r in results) / total_duration) if total_duration > 0 else 1,
            'num_threads': self.num_threads,
            'results': results
        }
        
        logger.info(f"Completed processing {len(file_list)} files in {total_duration:.2f}s")
        logger.info(f"Success: {successful}, Failed: {failed}")
        logger.info(f"Parallel speedup: {summary['parallel_speedup']:.2f}x")
        
        return summary
    
    def process_directory(self, 
                         directory: str,
                         pattern: str = "*.dat",
                         config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Process all matching files in a directory.
        
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
    
    def get_optimal_thread_count(self) -> int:
        """
        Determine optimal thread count based on system and license constraints.
        
        Returns:
            Recommended number of threads
        """
        # Get CPU count
        cpu_count = mp.cpu_count()
        
        # Check OrcaFlex license limits (if available)
        max_licenses = 30  # Default assumption
        
        try:
            if ORCAFLEX_AVAILABLE:
                # Try to determine actual license limit
                # This would need to be implemented based on your license manager
                pass
        except:
            pass
        
        # Recommend minimum of CPU count and license limit
        recommended = min(cpu_count, max_licenses)
        
        logger.info(f"System CPUs: {cpu_count}, License limit: {max_licenses}")
        logger.info(f"Recommended thread count: {recommended}")
        
        return recommended


def run_parallel_analysis(file_list: List[str],
                         num_threads: int = 30,
                         config: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
    """
    Convenience function to run parallel OrcaFlex analysis.
    
    Args:
        file_list: List of file paths to process
        num_threads: Number of parallel threads (default: 30)
        config: Configuration dictionary
        
    Returns:
        Processing summary
    """
    analyzer = OrcaFlexParallelAnalysis(num_threads=num_threads)
    return analyzer.process_files_parallel(file_list, config)


if __name__ == "__main__":
    # Example usage
    print("OrcaFlex Parallel Analysis Module")
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
        'output_dir': './results_parallel'
    }
    
    # Example file list
    example_files = [
        "orcaflex_test1.dat",
        "orcaflex_test2.dat",
        "orcaflex_test3.dat"
    ]
    
    print(f"\nExample usage:")
    print(f"  Files: {example_files}")
    print(f"  Threads: 30")
    print(f"  Config: {example_config}")
    
    # Determine optimal thread count
    analyzer = OrcaFlexParallelAnalysis()
    optimal = analyzer.get_optimal_thread_count()
    print(f"\nOptimal thread count for this system: {optimal}")