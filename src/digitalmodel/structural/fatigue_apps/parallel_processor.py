#!/usr/bin/env python3
"""
Parallel Processing Module for Fatigue Analysis

This module provides parallel processing capabilities for the fatigue analysis pipeline,
enabling concurrent processing of multiple struts, configurations, and conditions.
"""

import os
import sys
import time
import logging
import multiprocessing as mp
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any, Callable
from dataclasses import dataclass, asdict
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
import pandas as pd
import numpy as np
from tqdm import tqdm
import json

# Import fatigue analysis components
from .reference_seastate_processor import (
    ReferenceSeaStateProcessor,
    FatigueCondition,
    FatigueResult
)

logger = logging.getLogger(__name__)


@dataclass
class ProcessingTask:
    """Represents a single processing task"""
    config_name: str
    strut_num: int
    condition: FatigueCondition
    task_id: str
    
    def __hash__(self):
        return hash(self.task_id)


class ParallelFatigueProcessor:
    """
    Parallel processor for fatigue analysis with progress tracking
    """
    
    def __init__(self, 
                 base_processor: ReferenceSeaStateProcessor,
                 num_workers: Optional[int] = None,
                 use_threading: bool = False,
                 show_progress: bool = True):
        """
        Initialize parallel processor
        
        Args:
            base_processor: Base reference seastate processor
            num_workers: Number of parallel workers (None = CPU count)
            use_threading: Use threads instead of processes
            show_progress: Show progress bars
        """
        self.base_processor = base_processor
        self.num_workers = num_workers or mp.cpu_count()
        self.use_threading = use_threading
        self.show_progress = show_progress
        
        # Results storage
        self.results: List[FatigueResult] = []
        self.failed_tasks: List[ProcessingTask] = []
        self.processing_times: Dict[str, float] = {}
        
        logger.info(f"Initialized parallel processor with {self.num_workers} workers")
        logger.info(f"Using {'threads' if use_threading else 'processes'} for parallelization")
    
    def create_tasks(self,
                    config_names: List[str],
                    strut_nums: List[int],
                    conditions: List[FatigueCondition]) -> List[ProcessingTask]:
        """
        Create processing tasks for parallel execution
        
        Args:
            config_names: List of configuration names
            strut_nums: List of strut numbers
            conditions: List of fatigue conditions
            
        Returns:
            List of processing tasks
        """
        tasks = []
        task_counter = 0
        
        for config in config_names:
            for strut in strut_nums:
                for condition in conditions:
                    task = ProcessingTask(
                        config_name=config,
                        strut_num=strut,
                        condition=condition,
                        task_id=f"{config}_S{strut}_FC{condition.id:03d}"
                    )
                    tasks.append(task)
                    task_counter += 1
        
        logger.info(f"Created {len(tasks)} processing tasks")
        logger.info(f"  Configurations: {len(config_names)}")
        logger.info(f"  Struts: {len(strut_nums)}")
        logger.info(f"  Conditions: {len(conditions)}")
        
        return tasks
    
    def process_single_task(self, task: ProcessingTask) -> Tuple[Optional[FatigueResult], float]:
        """
        Process a single task
        
        Args:
            task: Processing task
            
        Returns:
            Tuple of (result, processing_time)
        """
        start_time = time.time()
        
        try:
            result = self.base_processor.process_fatigue_condition(
                config_name=task.config_name,
                condition=task.condition,
                strut_num=task.strut_num
            )
            
            processing_time = time.time() - start_time
            
            if result:
                logger.debug(f"Task {task.task_id} completed in {processing_time:.2f}s")
            else:
                logger.warning(f"Task {task.task_id} returned no result")
            
            return result, processing_time
            
        except Exception as e:
            logger.error(f"Task {task.task_id} failed: {str(e)}")
            return None, time.time() - start_time
    
    def process_batch_parallel(self,
                              tasks: List[ProcessingTask],
                              batch_size: Optional[int] = None) -> List[FatigueResult]:
        """
        Process tasks in parallel batches
        
        Args:
            tasks: List of processing tasks
            batch_size: Optional batch size for chunking
            
        Returns:
            List of fatigue results
        """
        results = []
        failed_tasks = []
        
        # Choose executor type
        executor_class = ThreadPoolExecutor if self.use_threading else ProcessPoolExecutor
        
        # Calculate optimal batch size
        if batch_size is None:
            batch_size = max(1, len(tasks) // (self.num_workers * 4))
        
        # Create progress bar
        if self.show_progress:
            pbar = tqdm(total=len(tasks), desc="Processing tasks", unit="task")
        
        # Process tasks in parallel
        with executor_class(max_workers=self.num_workers) as executor:
            # Submit tasks
            future_to_task = {
                executor.submit(self.process_single_task, task): task
                for task in tasks
            }
            
            # Collect results as they complete
            for future in as_completed(future_to_task):
                task = future_to_task[future]
                
                try:
                    result, proc_time = future.result(timeout=60)
                    
                    if result:
                        results.append(result)
                        self.processing_times[task.task_id] = proc_time
                    else:
                        failed_tasks.append(task)
                    
                except Exception as e:
                    logger.error(f"Task {task.task_id} exception: {str(e)}")
                    failed_tasks.append(task)
                
                if self.show_progress:
                    pbar.update(1)
        
        if self.show_progress:
            pbar.close()
        
        # Store results
        self.results = results
        self.failed_tasks = failed_tasks
        
        # Log summary
        logger.info(f"Parallel processing complete:")
        logger.info(f"  Successful: {len(results)}")
        logger.info(f"  Failed: {len(failed_tasks)}")
        logger.info(f"  Total time: {sum(self.processing_times.values()):.2f}s")
        logger.info(f"  Average time: {np.mean(list(self.processing_times.values())):.2f}s")
        
        return results
    
    def process_by_configuration(self,
                                config_names: List[str],
                                strut_nums: List[int],
                                conditions: List[FatigueCondition]) -> Dict[str, List[FatigueResult]]:
        """
        Process tasks grouped by configuration
        
        Args:
            config_names: List of configuration names
            strut_nums: List of strut numbers
            conditions: List of fatigue conditions
            
        Returns:
            Dictionary of results by configuration
        """
        config_results = {}
        
        for config in config_names:
            logger.info(f"Processing configuration: {config}")
            
            # Create tasks for this configuration
            config_tasks = []
            for strut in strut_nums:
                for condition in conditions:
                    task = ProcessingTask(
                        config_name=config,
                        strut_num=strut,
                        condition=condition,
                        task_id=f"{config}_S{strut}_FC{condition.id:03d}"
                    )
                    config_tasks.append(task)
            
            # Process configuration in parallel
            config_results[config] = self.process_batch_parallel(config_tasks)
        
        return config_results
    
    def process_by_strut(self,
                        config_names: List[str],
                        strut_nums: List[int],
                        conditions: List[FatigueCondition]) -> Dict[int, List[FatigueResult]]:
        """
        Process tasks grouped by strut
        
        Args:
            config_names: List of configuration names
            strut_nums: List of strut numbers
            conditions: List of fatigue conditions
            
        Returns:
            Dictionary of results by strut number
        """
        strut_results = {}
        
        for strut in strut_nums:
            logger.info(f"Processing strut {strut}")
            
            # Create tasks for this strut
            strut_tasks = []
            for config in config_names:
                for condition in conditions:
                    task = ProcessingTask(
                        config_name=config,
                        strut_num=strut,
                        condition=condition,
                        task_id=f"{config}_S{strut}_FC{condition.id:03d}"
                    )
                    strut_tasks.append(task)
            
            # Process strut in parallel
            strut_results[strut] = self.process_batch_parallel(strut_tasks)
        
        return strut_results
    
    def get_performance_metrics(self) -> Dict[str, Any]:
        """
        Get performance metrics from parallel processing
        
        Returns:
            Dictionary of performance metrics
        """
        if not self.processing_times:
            return {}
        
        times = list(self.processing_times.values())
        
        metrics = {
            'total_tasks': len(self.results) + len(self.failed_tasks),
            'successful_tasks': len(self.results),
            'failed_tasks': len(self.failed_tasks),
            'total_time': sum(times),
            'average_time': np.mean(times),
            'median_time': np.median(times),
            'min_time': min(times),
            'max_time': max(times),
            'std_time': np.std(times),
            'num_workers': self.num_workers,
            'parallelization_efficiency': self._calculate_efficiency(times)
        }
        
        return metrics
    
    def _calculate_efficiency(self, times: List[float]) -> float:
        """
        Calculate parallelization efficiency
        
        Args:
            times: List of processing times
            
        Returns:
            Efficiency percentage (0-100)
        """
        if not times:
            return 0.0
        
        # Sequential time would be sum of all times
        sequential_time = sum(times)
        
        # Parallel time is the maximum time (assuming perfect distribution)
        parallel_time_ideal = sequential_time / self.num_workers
        
        # Actual parallel time (rough estimate based on completion)
        parallel_time_actual = max(times) * len(times) / self.num_workers
        
        efficiency = (parallel_time_ideal / parallel_time_actual) * 100
        
        return min(100.0, max(0.0, efficiency))
    
    def save_performance_report(self, filename: str = "parallel_performance.json"):
        """
        Save performance metrics to file
        
        Args:
            filename: Output filename
        """
        metrics = self.get_performance_metrics()
        
        output_path = Path(self.base_processor.output_path) / filename
        
        with open(output_path, 'w') as f:
            json.dump(metrics, f, indent=2, default=str)
        
        logger.info(f"Performance report saved to {output_path}")


class AdaptiveParallelProcessor(ParallelFatigueProcessor):
    """
    Adaptive parallel processor that adjusts worker count based on system load
    """
    
    def __init__(self, base_processor: ReferenceSeaStateProcessor, **kwargs):
        """Initialize adaptive processor"""
        super().__init__(base_processor, **kwargs)
        
        # Monitor system resources
        self.cpu_threshold = 80  # CPU usage threshold
        self.memory_threshold = 80  # Memory usage threshold
        self.adaptive_enabled = True
    
    def get_system_load(self) -> Tuple[float, float]:
        """
        Get current system CPU and memory usage
        
        Returns:
            Tuple of (cpu_percent, memory_percent)
        """
        try:
            import psutil
            cpu_percent = psutil.cpu_percent(interval=1)
            memory_percent = psutil.virtual_memory().percent
            return cpu_percent, memory_percent
        except ImportError:
            logger.warning("psutil not available, using fixed worker count")
            return 50.0, 50.0  # Default moderate load
    
    def adjust_workers(self) -> int:
        """
        Adjust number of workers based on system load
        
        Returns:
            Adjusted number of workers
        """
        if not self.adaptive_enabled:
            return self.num_workers
        
        cpu_load, mem_load = self.get_system_load()
        
        # Calculate available capacity
        cpu_available = max(0, self.cpu_threshold - cpu_load)
        mem_available = max(0, self.memory_threshold - mem_load)
        
        # Scale workers based on available resources
        capacity_factor = min(cpu_available, mem_available) / 100
        
        # Adjust worker count
        adjusted_workers = max(1, int(self.num_workers * capacity_factor))
        
        if adjusted_workers != self.num_workers:
            logger.info(f"Adjusting workers from {self.num_workers} to {adjusted_workers}")
            logger.info(f"  CPU load: {cpu_load:.1f}%, Memory: {mem_load:.1f}%")
        
        return adjusted_workers
    
    def process_batch_parallel(self,
                              tasks: List[ProcessingTask],
                              batch_size: Optional[int] = None) -> List[FatigueResult]:
        """
        Process with adaptive worker adjustment
        
        Args:
            tasks: List of processing tasks
            batch_size: Optional batch size
            
        Returns:
            List of fatigue results
        """
        # Adjust workers before processing
        original_workers = self.num_workers
        self.num_workers = self.adjust_workers()
        
        # Process with adjusted workers
        results = super().process_batch_parallel(tasks, batch_size)
        
        # Restore original worker count
        self.num_workers = original_workers
        
        return results


def parallel_fatigue_analysis(data_path: str,
                             output_path: str = "output",
                             num_workers: Optional[int] = None,
                             configs: Optional[List[str]] = None,
                             struts: Optional[List[int]] = None,
                             adaptive: bool = False,
                             show_progress: bool = True) -> Dict[str, Any]:
    """
    Convenience function for parallel fatigue analysis
    
    Args:
        data_path: Path to data directory
        output_path: Output directory path
        num_workers: Number of parallel workers
        configs: List of configurations to process
        struts: List of strut numbers to process
        adaptive: Use adaptive worker adjustment
        show_progress: Show progress bars
        
    Returns:
        Dictionary with results and metrics
    """
    # Create base processor
    base_processor = ReferenceSeaStateProcessor(
        data_path=data_path,
        output_path=output_path
    )
    
    # Load conditions
    conditions = base_processor.load_fatigue_conditions()
    
    # Default configurations and struts
    if configs is None:
        configs = list(base_processor.configurations.keys())
    
    if struts is None:
        struts = list(range(1, 9))
    
    # Create parallel processor
    if adaptive:
        processor = AdaptiveParallelProcessor(
            base_processor=base_processor,
            num_workers=num_workers,
            show_progress=show_progress
        )
    else:
        processor = ParallelFatigueProcessor(
            base_processor=base_processor,
            num_workers=num_workers,
            show_progress=show_progress
        )
    
    # Create and process tasks
    tasks = processor.create_tasks(configs, struts, conditions)
    results = processor.process_batch_parallel(tasks)
    
    # Get metrics
    metrics = processor.get_performance_metrics()
    
    # Save results
    if results:
        results_df = pd.DataFrame([asdict(r) for r in results])
        results_file = Path(output_path) / "parallel_fatigue_results.csv"
        results_df.to_csv(results_file, index=False)
        logger.info(f"Results saved to {results_file}")
    
    # Save performance report
    processor.save_performance_report()
    
    return {
        'results': results,
        'metrics': metrics,
        'failed_tasks': processor.failed_tasks
    }


if __name__ == "__main__":
    # Example usage
    import argparse
    
    parser = argparse.ArgumentParser(description='Parallel Fatigue Analysis')
    parser.add_argument('--data-path', required=True, help='Path to data directory')
    parser.add_argument('--output-path', default='output', help='Output directory')
    parser.add_argument('--workers', type=int, help='Number of workers')
    parser.add_argument('--adaptive', action='store_true', help='Use adaptive workers')
    parser.add_argument('--configs', nargs='+', help='Configurations to process')
    parser.add_argument('--struts', nargs='+', type=int, help='Strut numbers')
    
    args = parser.parse_args()
    
    results = parallel_fatigue_analysis(
        data_path=args.data_path,
        output_path=args.output_path,
        num_workers=args.workers,
        configs=args.configs,
        struts=args.struts,
        adaptive=args.adaptive
    )
    
    print(f"Processed {len(results['results'])} tasks")
    print(f"Performance: {results['metrics']}")