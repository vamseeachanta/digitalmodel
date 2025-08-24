"""
Batch Processing Utilities for GMSH Agent
Process multiple geometries in parallel with progress tracking
"""

import json
import logging
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Callable
import time
import yaml
from queue import Queue
import threading

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False
    gmsh = None

logger = logging.getLogger(__name__)


@dataclass
class BatchJob:
    """Represents a single batch processing job"""
    job_id: str
    input_file: str
    output_file: str
    config: Dict[str, Any]
    status: str = "pending"
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    error: Optional[str] = None
    result: Optional[Dict] = None
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        data = asdict(self)
        if self.start_time:
            data['start_time'] = self.start_time.isoformat()
        if self.end_time:
            data['end_time'] = self.end_time.isoformat()
        return data


class ProgressTracker:
    """Track and report batch processing progress"""
    
    def __init__(self, total_jobs: int):
        """
        Initialize progress tracker
        
        Args:
            total_jobs: Total number of jobs to process
        """
        self.total_jobs = total_jobs
        self.completed_jobs = 0
        self.failed_jobs = 0
        self.current_job = None
        self.start_time = time.time()
        self.lock = threading.Lock()
        self.callbacks = []
    
    def update(self, job_id: str, status: str, message: Optional[str] = None):
        """
        Update progress for a job
        
        Args:
            job_id: Job identifier
            status: Job status (processing, completed, failed)
            message: Optional status message
        """
        with self.lock:
            if status == "processing":
                self.current_job = job_id
            elif status == "completed":
                self.completed_jobs += 1
                if self.current_job == job_id:
                    self.current_job = None
            elif status == "failed":
                self.failed_jobs += 1
                if self.current_job == job_id:
                    self.current_job = None
            
            # Call registered callbacks
            for callback in self.callbacks:
                try:
                    callback(self.get_status())
                except Exception as e:
                    logger.error(f"Progress callback error: {e}")
    
    def get_status(self) -> Dict:
        """Get current progress status"""
        with self.lock:
            elapsed_time = time.time() - self.start_time
            jobs_processed = self.completed_jobs + self.failed_jobs
            
            if jobs_processed > 0:
                avg_time = elapsed_time / jobs_processed
                eta = avg_time * (self.total_jobs - jobs_processed)
            else:
                eta = 0
            
            return {
                "total": self.total_jobs,
                "completed": self.completed_jobs,
                "failed": self.failed_jobs,
                "pending": self.total_jobs - jobs_processed,
                "current_job": self.current_job,
                "progress_percent": (jobs_processed / self.total_jobs * 100) if self.total_jobs > 0 else 0,
                "elapsed_time": elapsed_time,
                "estimated_remaining": eta
            }
    
    def register_callback(self, callback: Callable):
        """Register a callback for progress updates"""
        self.callbacks.append(callback)
    
    def print_progress(self):
        """Print progress to console"""
        status = self.get_status()
        print(f"\rProgress: {status['progress_percent']:.1f}% "
              f"({status['completed']}/{status['total']} completed, "
              f"{status['failed']} failed) "
              f"ETA: {status['estimated_remaining']:.0f}s", end="")


class BatchProcessor:
    """Process multiple mesh generation jobs in parallel"""
    
    def __init__(self, config: Optional[Dict] = None):
        """
        Initialize batch processor
        
        Args:
            config: Configuration dictionary
        """
        self.config = config or {}
        self.batch_config = self.config.get("batch", {
            "max_workers": mp.cpu_count(),
            "chunk_size": 10,
            "error_recovery": True,
            "save_intermediate": True,
            "progress_reporting": True
        })
        self.jobs = []
        self.results = {}
        self.progress_tracker = None
    
    def load_batch_config(self, config_file: str) -> Dict:
        """
        Load batch configuration from file
        
        Args:
            config_file: Path to configuration file
            
        Returns:
            Batch configuration dictionary
        """
        config_path = Path(config_file)
        if not config_path.exists():
            raise FileNotFoundError(f"Batch config not found: {config_file}")
        
        with open(config_path, 'r') as f:
            if config_path.suffix in ['.yml', '.yaml']:
                config = yaml.safe_load(f)
            else:
                config = json.load(f)
        
        logger.info(f"Loaded batch configuration from {config_file}")
        
        # Validate configuration schema
        self._validate_batch_config(config)
        
        return config
    
    def _validate_batch_config(self, config: Dict):
        """Validate batch configuration schema"""
        required_fields = ["input_directory", "output_directory", "mesh_config"]
        
        for field in required_fields:
            if field not in config:
                raise ValueError(f"Missing required field in batch config: {field}")
        
        # Validate mesh configuration
        mesh_config = config.get("mesh_config", {})
        if "algorithm" in mesh_config:
            valid_algorithms = ["frontal-delaunay", "delaunay", "frontal", "meshadapt"]
            if mesh_config["algorithm"] not in valid_algorithms:
                raise ValueError(f"Invalid algorithm: {mesh_config['algorithm']}")
    
    def create_jobs(self, 
                   input_files: List[str],
                   output_directory: str,
                   mesh_config: Dict) -> List[BatchJob]:
        """
        Create batch jobs from input files
        
        Args:
            input_files: List of input geometry files
            output_directory: Output directory for meshes
            mesh_config: Mesh generation configuration
            
        Returns:
            List of batch jobs
        """
        jobs = []
        output_path = Path(output_directory)
        output_path.mkdir(parents=True, exist_ok=True)
        
        for i, input_file in enumerate(input_files):
            input_path = Path(input_file)
            output_file = output_path / f"{input_path.stem}_mesh.msh"
            
            job = BatchJob(
                job_id=f"job_{i:04d}_{input_path.stem}",
                input_file=str(input_file),
                output_file=str(output_file),
                config=mesh_config
            )
            jobs.append(job)
        
        logger.info(f"Created {len(jobs)} batch jobs")
        return jobs
    
    def process_single_job(self, job: BatchJob) -> BatchJob:
        """
        Process a single mesh generation job
        
        Args:
            job: Batch job to process
            
        Returns:
            Processed job with results
        """
        job.status = "processing"
        job.start_time = datetime.now()
        
        try:
            # Import utilities here to avoid pickling issues in multiprocessing
            from .mesh_generator import MeshGenerator
            from .mesh_metrics import MeshQualityAnalyzer
            
            # Generate mesh
            generator = MeshGenerator(job.config)
            
            # Determine mesh dimension from file
            if job.config.get("dimension", 3) == 1:
                # For 1D, need points (simplified for now)
                mesh_data = {"status": "1D mesh placeholder"}
            elif job.config.get("dimension", 3) == 2:
                # For 2D, need boundary points (simplified)
                mesh_data = {"status": "2D mesh placeholder"}
            else:
                # 3D mesh from geometry file
                mesh_data = generator.generate_3d_mesh(
                    job.input_file,
                    algorithm=job.config.get("algorithm", "frontal-delaunay"),
                    element_type=job.config.get("element_type", "tetrahedron"),
                    element_size=job.config.get("element_size", 1.0)
                )
            
            # Save mesh
            if "num_elements" in mesh_data:
                generator.save_mesh(job.output_file)
            
            # Assess quality if requested
            if job.config.get("assess_quality", True):
                analyzer = MeshQualityAnalyzer()
                quality = analyzer.assess_mesh_quality(job.output_file)
                mesh_data["quality"] = quality
            
            job.result = mesh_data
            job.status = "completed"
            
        except Exception as e:
            logger.error(f"Job {job.job_id} failed: {e}")
            job.status = "failed"
            job.error = str(e)
            
            # Error recovery if enabled
            if self.batch_config.get("error_recovery", True):
                job = self._attempt_recovery(job)
        
        finally:
            job.end_time = datetime.now()
        
        return job
    
    def _attempt_recovery(self, job: BatchJob) -> BatchJob:
        """
        Attempt to recover from job failure
        
        Args:
            job: Failed job
            
        Returns:
            Recovered job or original failed job
        """
        logger.info(f"Attempting recovery for job {job.job_id}")
        
        # Try with more conservative settings
        recovery_config = job.config.copy()
        recovery_config["element_size"] = recovery_config.get("element_size", 1.0) * 2
        recovery_config["algorithm"] = "delaunay"  # More robust algorithm
        
        recovery_job = BatchJob(
            job_id=f"{job.job_id}_recovery",
            input_file=job.input_file,
            output_file=job.output_file,
            config=recovery_config
        )
        
        try:
            return self.process_single_job(recovery_job)
        except Exception as e:
            logger.error(f"Recovery failed for job {job.job_id}: {e}")
            return job
    
    def process_batch(self,
                     jobs: List[BatchJob],
                     max_workers: Optional[int] = None,
                     use_threads: bool = False) -> Dict[str, Any]:
        """
        Process batch of jobs in parallel
        
        Args:
            jobs: List of batch jobs
            max_workers: Maximum parallel workers
            use_threads: Use threads instead of processes
            
        Returns:
            Batch processing results
        """
        max_workers = max_workers or self.batch_config.get("max_workers", mp.cpu_count())
        self.jobs = jobs
        self.progress_tracker = ProgressTracker(len(jobs))
        
        if self.batch_config.get("progress_reporting", True):
            # Start progress reporting thread
            progress_thread = threading.Thread(target=self._report_progress)
            progress_thread.daemon = True
            progress_thread.start()
        
        logger.info(f"Starting batch processing: {len(jobs)} jobs with {max_workers} workers")
        
        # Choose executor based on configuration
        executor_class = ThreadPoolExecutor if use_threads else ProcessPoolExecutor
        
        completed_jobs = []
        failed_jobs = []
        
        with executor_class(max_workers=max_workers) as executor:
            # Submit all jobs
            future_to_job = {
                executor.submit(self.process_single_job, job): job 
                for job in jobs
            }
            
            # Process completed jobs
            for future in as_completed(future_to_job):
                job = future_to_job[future]
                
                try:
                    result_job = future.result()
                    
                    if result_job.status == "completed":
                        completed_jobs.append(result_job)
                        self.progress_tracker.update(result_job.job_id, "completed")
                    else:
                        failed_jobs.append(result_job)
                        self.progress_tracker.update(result_job.job_id, "failed")
                    
                    # Save intermediate results if configured
                    if self.batch_config.get("save_intermediate", True):
                        self._save_intermediate_results(result_job)
                    
                except Exception as e:
                    logger.error(f"Unexpected error processing job {job.job_id}: {e}")
                    job.status = "failed"
                    job.error = str(e)
                    failed_jobs.append(job)
                    self.progress_tracker.update(job.job_id, "failed")
        
        # Compile final results
        results = self._aggregate_results(completed_jobs, failed_jobs)
        
        # Save final report
        self._save_batch_report(results)
        
        logger.info(f"Batch processing complete: {len(completed_jobs)} successful, {len(failed_jobs)} failed")
        
        return results
    
    def _report_progress(self):
        """Report progress periodically"""
        while True:
            status = self.progress_tracker.get_status()
            if status["pending"] == 0:
                break
            
            self.progress_tracker.print_progress()
            time.sleep(1)
        
        # Final progress report
        self.progress_tracker.print_progress()
        print()  # New line after progress
    
    def _save_intermediate_results(self, job: BatchJob):
        """Save intermediate results for a job"""
        results_dir = Path(self.batch_config.get("results_directory", "./batch_results"))
        results_dir.mkdir(parents=True, exist_ok=True)
        
        result_file = results_dir / f"{job.job_id}_result.json"
        with open(result_file, 'w') as f:
            json.dump(job.to_dict(), f, indent=2, default=str)
    
    def _aggregate_results(self, 
                         completed_jobs: List[BatchJob],
                         failed_jobs: List[BatchJob]) -> Dict[str, Any]:
        """
        Aggregate batch processing results
        
        Args:
            completed_jobs: List of completed jobs
            failed_jobs: List of failed jobs
            
        Returns:
            Aggregated results dictionary
        """
        total_jobs = len(completed_jobs) + len(failed_jobs)
        
        # Calculate statistics
        processing_times = []
        quality_scores = []
        element_counts = []
        
        for job in completed_jobs:
            if job.start_time and job.end_time:
                processing_time = (job.end_time - job.start_time).total_seconds()
                processing_times.append(processing_time)
            
            if job.result:
                if "quality" in job.result and "overall_score" in job.result["quality"]:
                    quality_scores.append(job.result["quality"]["overall_score"])
                if "num_elements" in job.result:
                    element_counts.append(job.result["num_elements"])
        
        results = {
            "summary": {
                "total_jobs": total_jobs,
                "completed": len(completed_jobs),
                "failed": len(failed_jobs),
                "success_rate": (len(completed_jobs) / total_jobs * 100) if total_jobs > 0 else 0
            },
            "performance": {
                "total_time": sum(processing_times) if processing_times else 0,
                "average_time": sum(processing_times) / len(processing_times) if processing_times else 0,
                "min_time": min(processing_times) if processing_times else 0,
                "max_time": max(processing_times) if processing_times else 0
            },
            "quality": {
                "average_score": sum(quality_scores) / len(quality_scores) if quality_scores else 0,
                "min_score": min(quality_scores) if quality_scores else 0,
                "max_score": max(quality_scores) if quality_scores else 0
            },
            "mesh_statistics": {
                "total_elements": sum(element_counts) if element_counts else 0,
                "average_elements": sum(element_counts) / len(element_counts) if element_counts else 0
            },
            "completed_jobs": [job.to_dict() for job in completed_jobs],
            "failed_jobs": [job.to_dict() for job in failed_jobs]
        }
        
        return results
    
    def _save_batch_report(self, results: Dict[str, Any]):
        """Save batch processing report"""
        report_dir = Path(self.batch_config.get("report_directory", "./batch_reports"))
        report_dir.mkdir(parents=True, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = report_dir / f"batch_report_{timestamp}.json"
        
        with open(report_file, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        
        logger.info(f"Batch report saved to {report_file}")
        
        # Also create summary report
        summary_file = report_dir / f"batch_summary_{timestamp}.txt"
        with open(summary_file, 'w') as f:
            f.write("Batch Processing Summary\n")
            f.write("=" * 50 + "\n\n")
            f.write(f"Total Jobs: {results['summary']['total_jobs']}\n")
            f.write(f"Completed: {results['summary']['completed']}\n")
            f.write(f"Failed: {results['summary']['failed']}\n")
            f.write(f"Success Rate: {results['summary']['success_rate']:.1f}%\n\n")
            
            f.write("Performance Metrics\n")
            f.write("-" * 30 + "\n")
            f.write(f"Total Time: {results['performance']['total_time']:.2f}s\n")
            f.write(f"Average Time: {results['performance']['average_time']:.2f}s\n")
            f.write(f"Min Time: {results['performance']['min_time']:.2f}s\n")
            f.write(f"Max Time: {results['performance']['max_time']:.2f}s\n\n")
            
            if results['quality']['average_score'] > 0:
                f.write("Quality Metrics\n")
                f.write("-" * 30 + "\n")
                f.write(f"Average Score: {results['quality']['average_score']:.1f}\n")
                f.write(f"Min Score: {results['quality']['min_score']:.1f}\n")
                f.write(f"Max Score: {results['quality']['max_score']:.1f}\n")
        
        logger.info(f"Batch summary saved to {summary_file}")


def create_batch_config_template(output_file: str = "batch_config.yml"):
    """
    Create a template batch configuration file
    
    Args:
        output_file: Output file path
    """
    template = {
        "input_directory": "./geometries",
        "output_directory": "./meshes",
        "file_pattern": "*.step",
        "mesh_config": {
            "algorithm": "frontal-delaunay",
            "element_size": 1.0,
            "element_type": "tetrahedron",
            "dimension": 3,
            "assess_quality": True,
            "optimize": False
        },
        "batch_settings": {
            "max_workers": 4,
            "error_recovery": True,
            "save_intermediate": True,
            "progress_reporting": True,
            "results_directory": "./batch_results",
            "report_directory": "./batch_reports"
        },
        "quality_targets": {
            "min_jacobian": 0.3,
            "max_aspect_ratio": 5.0,
            "max_skewness": 0.7
        }
    }
    
    with open(output_file, 'w') as f:
        yaml.dump(template, f, default_flow_style=False, sort_keys=False)
    
    logger.info(f"Created batch configuration template: {output_file}")
    return output_file