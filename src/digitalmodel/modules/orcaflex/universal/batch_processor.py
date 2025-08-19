"""
Batch Processing System
=======================

Manage batch processing with resource optimization and parallel execution.
"""

import os
import time
import logging
from pathlib import Path
from typing import List, Dict, Optional, Callable, Any
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock
import psutil

logger = logging.getLogger(__name__)


class BatchProcessor:
    """
    Manage batch processing with resource optimization.
    
    Features:
    - Parallel processing with ThreadPoolExecutor
    - Adaptive worker scaling based on system resources
    - Chunk-based processing for large batches
    - Progress tracking and reporting
    - Resource monitoring
    """
    
    def __init__(self, 
                 max_workers: int = 30,
                 mock_mode: bool = False,
                 chunk_size: Optional[int] = None):
        """
        Initialize the batch processor.
        
        Args:
            max_workers: Maximum number of parallel workers
            mock_mode: If True, simulate processing without OrcaFlex
            chunk_size: Size of chunks for batch processing (None for auto)
        """
        self.max_workers = max_workers
        self.mock_mode = mock_mode
        self.chunk_size = chunk_size
        self.lock = Lock()
        
        # Performance tracking
        self.start_time = None
        self.completed_count = 0
        self.total_count = 0
        
        # Results storage
        self.results = []
        self.successful = 0
        self.failed = 0
        
        # System resource monitoring
        self.cpu_count = os.cpu_count() or 1
        self.memory_gb = psutil.virtual_memory().total / (1024**3)
        
        logger.info(f"BatchProcessor initialized")
        logger.info(f"System: {self.cpu_count} CPUs, {self.memory_gb:.1f}GB RAM")
        
    def process_batch(self,
                     models: List[Path],
                     output_directory: Path,
                     processor_func: Optional[Callable] = None,
                     status_reporter: Optional[Any] = None,
                     **kwargs) -> List[Dict]:
        """
        Process models in optimized batches.
        
        Args:
            models: List of model paths to process
            output_directory: Directory for output files
            processor_func: Custom processing function (optional)
            status_reporter: Status reporter for progress updates
            **kwargs: Additional arguments for processing
        
        Returns:
            List of result dictionaries
        """
        if not models:
            logger.warning("No models to process")
            return []
        
        self.start_time = time.time()
        self.total_count = len(models)
        self.completed_count = 0
        self.results = []
        self.successful = 0
        self.failed = 0
        
        # Determine optimal worker count
        optimal_workers = self._calculate_optimal_workers(len(models))
        
        logger.info("=" * 80)
        logger.info("BATCH PROCESSING - STARTING")
        logger.info(f"Total models: {len(models)}")
        logger.info(f"Parallel workers: {optimal_workers}")
        logger.info(f"Output directory: {output_directory}")
        logger.info(f"Mode: {'MOCK' if self.mock_mode else 'REAL'}")
        logger.info("=" * 80)
        
        # Initialize status reporter if provided
        if status_reporter:
            status_reporter.total = len(models)
            status_reporter.start_time = self.start_time
        
        # Determine processing function
        if processor_func is None:
            processor_func = self._default_processor
        
        # Process in chunks if specified
        if self.chunk_size:
            results = self._process_in_chunks(
                models, output_directory, processor_func, 
                optimal_workers, status_reporter, **kwargs
            )
        else:
            results = self._process_parallel(
                models, output_directory, processor_func,
                optimal_workers, status_reporter, **kwargs
            )
        
        # Final summary
        total_time = time.time() - self.start_time
        self._display_summary(total_time)
        
        return results
    
    def _calculate_optimal_workers(self, model_count: int) -> int:
        """
        Calculate optimal worker count based on system resources.
        
        Args:
            model_count: Number of models to process
        
        Returns:
            Optimal number of workers
        """
        # Start with configured maximum
        workers = self.max_workers
        
        # Limit by model count (no point having more workers than models)
        workers = min(workers, model_count)
        
        # Limit by CPU count (typically 2x CPU count is good for I/O bound tasks)
        workers = min(workers, self.cpu_count * 2)
        
        # Limit by memory (assume ~2GB per OrcaFlex instance)
        if not self.mock_mode:
            memory_limited_workers = int(self.memory_gb / 2)
            workers = min(workers, max(1, memory_limited_workers))
        
        # Check current system load
        try:
            cpu_percent = psutil.cpu_percent(interval=0.1)
            if cpu_percent > 80:
                # System is already loaded, reduce workers
                workers = max(1, workers // 2)
                logger.info(f"High CPU load detected ({cpu_percent}%), reducing workers")
        except:
            pass
        
        return max(1, workers)
    
    def _process_parallel(self,
                         models: List[Path],
                         output_directory: Path,
                         processor_func: Callable,
                         workers: int,
                         status_reporter: Optional[Any],
                         **kwargs) -> List[Dict]:
        """Process models in parallel."""
        results = []
        
        with ThreadPoolExecutor(max_workers=workers) as executor:
            # Submit all tasks
            future_to_model = {
                executor.submit(
                    processor_func, 
                    model, 
                    output_directory,
                    **kwargs
                ): model
                for model in models
            }
            
            # Process completed tasks
            for future in as_completed(future_to_model):
                model = future_to_model[future]
                
                try:
                    result = future.result()
                    self._handle_result(result, model, status_reporter)
                    results.append(result)
                    
                except Exception as e:
                    error_result = self._create_error_result(model, str(e))
                    self._handle_result(error_result, model, status_reporter)
                    results.append(error_result)
        
        return results
    
    def _process_in_chunks(self,
                          models: List[Path],
                          output_directory: Path,
                          processor_func: Callable,
                          workers: int,
                          status_reporter: Optional[Any],
                          **kwargs) -> List[Dict]:
        """Process models in chunks."""
        results = []
        chunk_size = self.chunk_size or workers * 2
        
        for i in range(0, len(models), chunk_size):
            chunk = models[i:i + chunk_size]
            logger.info(f"Processing chunk {i//chunk_size + 1}: {len(chunk)} models")
            
            chunk_results = self._process_parallel(
                chunk, output_directory, processor_func,
                workers, status_reporter, **kwargs
            )
            results.extend(chunk_results)
            
            # Brief pause between chunks to avoid overwhelming the system
            if i + chunk_size < len(models):
                time.sleep(1)
        
        return results
    
    def _default_processor(self, model_path: Path, output_directory: Path, **kwargs) -> Dict:
        """Default model processor function."""
        result = {
            'model': model_path.name,
            'input': str(model_path),
            'success': False,
            'sim_file': None,
            'error': None,
            'duration': 0,
            'mock': self.mock_mode
        }
        
        start_time = time.time()
        
        try:
            if self.mock_mode:
                # Simulate processing
                time.sleep(0.1)
                sim_file = output_directory / f"{model_path.stem}.sim"
                sim_file.write_text(f"Mock simulation for {model_path.name}")
                result['sim_file'] = str(sim_file)
                result['success'] = True
            else:
                # Real OrcaFlex processing
                import OrcFxAPI
                
                model = OrcFxAPI.Model()
                
                # Load based on file type
                if model_path.suffix.lower() in ['.yml', '.yaml']:
                    model.LoadData(str(model_path))
                elif model_path.suffix.lower() == '.dat':
                    model.LoadDataFile(str(model_path))
                else:
                    raise ValueError(f"Unsupported file type: {model_path.suffix}")
                
                # Run static analysis
                model.CalculateStatics()
                
                # Save simulation
                sim_file = output_directory / f"{model_path.stem}.sim"
                model.SaveSimulation(str(sim_file))
                
                result['sim_file'] = str(sim_file)
                result['success'] = True
                
        except Exception as e:
            result['error'] = str(e)
        
        result['duration'] = time.time() - start_time
        return result
    
    def _handle_result(self, result: Dict, model: Path, status_reporter: Optional[Any]):
        """Handle a processing result."""
        with self.lock:
            self.completed_count += 1
            
            if result.get('success'):
                self.successful += 1
                status = "OK"
            else:
                self.failed += 1
                status = "FAIL"
            
            # Log progress
            progress = self.completed_count / self.total_count * 100
            elapsed = time.time() - self.start_time
            rate = self.completed_count / elapsed if elapsed > 0 else 0
            
            logger.info(
                f"[{self.completed_count}/{self.total_count}] "
                f"{status} {model.name} "
                f"({result.get('duration', 0):.2f}s) "
                f"[{progress:.1f}% @ {rate:.1f}/s]"
            )
            
            # Update status reporter
            if status_reporter:
                status_reporter.completed = self.completed_count
                status_reporter.success = self.successful
                status_reporter.failed = self.failed
                status_reporter.current_model = model.name
                
                if hasattr(status_reporter, 'update_terminal_title'):
                    status_reporter.update_terminal_title()
                if hasattr(status_reporter, 'display_progress'):
                    status_reporter.display_progress()
    
    def _create_error_result(self, model: Path, error: str) -> Dict:
        """Create an error result dictionary."""
        return {
            'model': model.name,
            'input': str(model),
            'success': False,
            'sim_file': None,
            'error': error,
            'duration': 0
        }
    
    def _display_summary(self, total_time: float):
        """Display batch processing summary."""
        logger.info("\n" + "=" * 80)
        logger.info("BATCH PROCESSING - COMPLETE")
        logger.info(f"Total: {self.total_count}")
        logger.info(f"Successful: {self.successful}")
        logger.info(f"Failed: {self.failed}")
        
        if self.total_count > 0:
            success_rate = self.successful / self.total_count * 100
            avg_time = total_time / self.total_count
            logger.info(f"Success Rate: {success_rate:.1f}%")
            logger.info(f"Total Time: {total_time:.2f} seconds")
            logger.info(f"Average Time per Model: {avg_time:.2f} seconds")
            logger.info(f"Processing Rate: {self.total_count/total_time:.1f} models/second")
        
        logger.info("=" * 80)
    
    def get_performance_stats(self) -> Dict:
        """Get performance statistics from the last batch run."""
        if not self.start_time:
            return {}
        
        elapsed = time.time() - self.start_time
        
        return {
            'total_models': self.total_count,
            'completed': self.completed_count,
            'successful': self.successful,
            'failed': self.failed,
            'success_rate': (self.successful / self.total_count * 100) if self.total_count > 0 else 0,
            'total_time': elapsed,
            'avg_time_per_model': elapsed / self.total_count if self.total_count > 0 else 0,
            'processing_rate': self.total_count / elapsed if elapsed > 0 else 0,
        }