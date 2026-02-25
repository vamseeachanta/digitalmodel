"""
Batch Processing System for multiple file analysis

Supports parallel processing, progress tracking, and result aggregation.
"""

import logging
import time
from pathlib import Path
from typing import List, Dict, Optional, Union, Any, Callable
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from datetime import datetime
import pandas as pd
import numpy as np
try:
    from tqdm import tqdm
    HAS_TQDM = True
except ImportError:
    HAS_TQDM = False
    tqdm = lambda x, **kwargs: x

logger = logging.getLogger(__name__)


class BatchProcessor:
    """Batch processing system for time series analysis"""
    
    def __init__(self, 
                 analyzer: Any,
                 config: Optional[Dict] = None,
                 progress_callback: Optional[Callable] = None):
        """
        Initialize batch processor
        
        Args:
            analyzer: TimeSeriesAnalyzer instance
            config: Configuration dictionary
            progress_callback: Optional callback for progress updates
        """
        self.analyzer = analyzer
        self.config = config or {}
        self.progress_callback = progress_callback
        
        # Batch configuration
        self.parallel_config = self.config.get('batch', {}).get('parallel', {})
        self.max_workers = self.parallel_config.get('max_workers', 4)
        self.use_threads = self.parallel_config.get('use_threads', False)
        self.chunk_size = self.parallel_config.get('chunk_size', 1)
        
        # Error handling
        self.continue_on_error = self.config.get('batch', {}).get('continue_on_error', True)
        self.retry_failed = self.config.get('batch', {}).get('retry_failed', False)
        self.max_retries = self.config.get('batch', {}).get('max_retries', 3)
        
        # Results tracking
        self.results = {}
        self.errors = {}
        self.processing_times = {}
        
    def process_files(self,
                     files: List[Union[str, Path]],
                     output_dir: Optional[Union[str, Path]] = None,
                     parallel: bool = True) -> Dict:
        """
        Process multiple files with optional parallelization
        
        Args:
            files: List of file paths
            output_dir: Output directory for results
            parallel: Whether to use parallel processing
            
        Returns:
            Dictionary of results keyed by file path
        """
        files = [Path(f) for f in files]
        total_files = len(files)
        
        logger.info(f"Starting batch processing of {total_files} files")
        start_time = time.time()
        
        if parallel and total_files > 1:
            results = self._process_parallel(files, output_dir)
        else:
            results = self._process_sequential(files, output_dir)
        
        # Calculate processing time
        total_time = time.time() - start_time
        self.processing_times['total'] = total_time
        self.processing_times['average'] = total_time / total_files if total_files > 0 else 0
        
        # Generate batch report
        if self.config.get('batch', {}).get('aggregate', {}).get('create_summary', True):
            self._generate_batch_report(results, output_dir)
        
        logger.info(f"Batch processing complete: {len(results)} succeeded, {len(self.errors)} failed")
        logger.info(f"Total time: {total_time:.2f}s, Average: {self.processing_times['average']:.2f}s per file")
        
        return results
    
    def _process_sequential(self,
                           files: List[Path],
                           output_dir: Optional[Path]) -> Dict:
        """Process files sequentially with progress tracking"""
        results = {}
        
        # Use tqdm for progress bar if available
        try:
            progress_bar = tqdm(files, desc="Processing files", unit="file")
        except:
            progress_bar = files
        
        for file in progress_bar:
            try:
                file_start = time.time()
                
                # Process file with retries
                result = self._process_with_retry(file, output_dir)
                
                results[str(file)] = result
                self.processing_times[str(file)] = time.time() - file_start
                
                # Update progress
                if self.progress_callback:
                    self.progress_callback(len(results), len(files))
                    
            except Exception as e:
                logger.error(f"Failed to process {file}: {e}")
                self.errors[str(file)] = str(e)
                
                if not self.continue_on_error:
                    raise
        
        return results
    
    def _process_parallel(self,
                         files: List[Path],
                         output_dir: Optional[Path]) -> Dict:
        """Process files in parallel"""
        results = {}
        
        # Choose executor type
        if self.use_threads:
            ExecutorClass = ThreadPoolExecutor
        else:
            ExecutorClass = ProcessPoolExecutor
        
        # Process in chunks if specified
        if self.chunk_size > 1:
            file_chunks = [files[i:i+self.chunk_size] 
                          for i in range(0, len(files), self.chunk_size)]
        else:
            file_chunks = [[f] for f in files]
        
        with ExecutorClass(max_workers=self.max_workers) as executor:
            # Submit all tasks
            futures = {}
            for chunk in file_chunks:
                if len(chunk) == 1:
                    # Single file
                    future = executor.submit(self._process_with_retry, chunk[0], output_dir)
                    futures[future] = chunk[0]
                else:
                    # Multiple files in chunk
                    future = executor.submit(self._process_chunk, chunk, output_dir)
                    futures[future] = chunk
            
            # Process completed futures
            try:
                progress_bar = tqdm(as_completed(futures), total=len(futures), 
                                  desc="Processing", unit="task")
            except:
                progress_bar = as_completed(futures)
            
            for future in progress_bar:
                file_or_chunk = futures[future]
                
                try:
                    result = future.result()
                    
                    if isinstance(file_or_chunk, list):
                        # Chunk result
                        results.update(result)
                    else:
                        # Single file result
                        results[str(file_or_chunk)] = result
                    
                    # Update progress
                    if self.progress_callback:
                        self.progress_callback(len(results), len(files))
                        
                except Exception as e:
                    if isinstance(file_or_chunk, list):
                        for file in file_or_chunk:
                            logger.error(f"Failed to process {file}: {e}")
                            self.errors[str(file)] = str(e)
                    else:
                        logger.error(f"Failed to process {file_or_chunk}: {e}")
                        self.errors[str(file_or_chunk)] = str(e)
                    
                    if not self.continue_on_error:
                        # Cancel remaining futures
                        for f in futures:
                            f.cancel()
                        raise
        
        return results
    
    def _process_chunk(self, files: List[Path], output_dir: Optional[Path]) -> Dict:
        """Process a chunk of files"""
        results = {}
        
        for file in files:
            try:
                result = self._process_with_retry(file, output_dir)
                results[str(file)] = result
            except Exception as e:
                logger.error(f"Failed to process {file}: {e}")
                if not self.continue_on_error:
                    raise
        
        return results
    
    def _process_with_retry(self, file: Path, output_dir: Optional[Path]) -> Dict:
        """Process a file with retry logic"""
        last_error = None
        
        for attempt in range(self.max_retries if self.retry_failed else 1):
            try:
                if attempt > 0:
                    logger.info(f"Retry {attempt}/{self.max_retries} for {file}")
                
                result = self.analyzer.process_file(file, output_dir=output_dir)
                return result
                
            except Exception as e:
                last_error = e
                if attempt < self.max_retries - 1 and self.retry_failed:
                    time.sleep(2 ** attempt)  # Exponential backoff
                else:
                    raise
        
        raise last_error
    
    def _generate_batch_report(self, results: Dict, output_dir: Optional[Path]):
        """Generate comprehensive batch processing report"""
        if not output_dir:
            return
        
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create summary data
        summary_data = []
        
        for file_path, file_results in results.items():
            file_name = Path(file_path).name
            
            # Add file-level summary
            file_summary = {
                'file': file_name,
                'processing_time': self.processing_times.get(str(file_path), 0),
                'columns_analyzed': len(file_results.get('columns', {})),
                'validation_warnings': len(file_results.get('validation', {}).get('warnings', []))
            }
            
            # Add column-level summaries
            for col_name, col_results in file_results.get('columns', {}).items():
                col_summary = file_summary.copy()
                col_summary.update({
                    'column': col_name,
                    'mean': col_results.get('statistics', {}).get('mean'),
                    'std': col_results.get('statistics', {}).get('std'),
                    'max': col_results.get('statistics', {}).get('max'),
                    'min': col_results.get('statistics', {}).get('min'),
                    'rms': col_results.get('statistics', {}).get('rms'),
                    'rainflow_cycles': col_results.get('rainflow', {}).get('statistics', {}).get('total_cycles'),
                    'max_range': col_results.get('rainflow', {}).get('statistics', {}).get('max_range'),
                    'mean_range': col_results.get('rainflow', {}).get('statistics', {}).get('mean_range')
                })
                
                # Add dominant frequencies
                peaks = col_results.get('spectral', {}).get('peaks')
                if isinstance(peaks, pd.DataFrame) and len(peaks) > 0:
                    col_summary['dominant_freq_1'] = peaks.iloc[0]['frequency'] if len(peaks) > 0 else None
                    col_summary['dominant_freq_2'] = peaks.iloc[1]['frequency'] if len(peaks) > 1 else None
                    col_summary['dominant_freq_3'] = peaks.iloc[2]['frequency'] if len(peaks) > 2 else None
                
                summary_data.append(col_summary)
        
        # Create summary DataFrame
        summary_df = pd.DataFrame(summary_data)
        
        # Save summary CSV
        summary_csv = output_dir / 'batch_summary.csv'
        summary_df.to_csv(summary_csv, index=False)
        logger.info(f"Saved batch summary: {summary_csv}")
        
        # Generate HTML report if configured
        if self.config.get('output', {}).get('summary', {}).get('format') == 'html':
            self._generate_html_report(summary_df, results, output_dir)
        
        # Save error log
        if self.errors:
            error_log = output_dir / 'batch_errors.txt'
            with open(error_log, 'w') as f:
                f.write(f"Batch Processing Errors - {datetime.now().isoformat()}\n")
                f.write("="*60 + "\n\n")
                
                for file_path, error in self.errors.items():
                    f.write(f"File: {file_path}\n")
                    f.write(f"Error: {error}\n")
                    f.write("-"*40 + "\n")
            
            logger.info(f"Saved error log: {error_log}")
    
    def _generate_html_report(self, summary_df: pd.DataFrame, results: Dict, output_dir: Path):
        """Generate HTML report with interactive tables and charts"""
        html_content = f"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>Batch Processing Report</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                h1 {{ color: #333; }}
                h2 {{ color: #666; }}
                table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
                th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
                th {{ background-color: #f2f2f2; }}
                tr:nth-child(even) {{ background-color: #f9f9f9; }}
                .summary {{ background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }}
                .error {{ color: red; }}
                .success {{ color: green; }}
            </style>
        </head>
        <body>
            <h1>Time Series Analysis - Batch Processing Report</h1>
            <div class="summary">
                <h2>Summary</h2>
                <p><strong>Processing Date:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
                <p><strong>Files Processed:</strong> {len(results)}</p>
                <p><strong>Files Failed:</strong> {len(self.errors)}</p>
                <p><strong>Total Processing Time:</strong> {self.processing_times.get('total', 0):.2f} seconds</p>
                <p><strong>Average Time per File:</strong> {self.processing_times.get('average', 0):.2f} seconds</p>
            </div>
            
            <h2>Analysis Results</h2>
            {summary_df.to_html(index=False, classes='results-table')}
            
            <h2>Processing Statistics</h2>
            <table>
                <tr>
                    <th>Metric</th>
                    <th>Value</th>
                </tr>
                <tr>
                    <td>Total Files</td>
                    <td>{len(results) + len(self.errors)}</td>
                </tr>
                <tr>
                    <td>Successful</td>
                    <td class="success">{len(results)}</td>
                </tr>
                <tr>
                    <td>Failed</td>
                    <td class="error">{len(self.errors)}</td>
                </tr>
                <tr>
                    <td>Success Rate</td>
                    <td>{100 * len(results) / (len(results) + len(self.errors)) if results or self.errors else 0:.1f}%</td>
                </tr>
            </table>
        """
        
        # Add error section if there are errors
        if self.errors:
            html_content += """
            <h2>Processing Errors</h2>
            <table>
                <tr>
                    <th>File</th>
                    <th>Error</th>
                </tr>
            """
            for file_path, error in self.errors.items():
                html_content += f"""
                <tr>
                    <td>{Path(file_path).name}</td>
                    <td class="error">{error}</td>
                </tr>
                """
            html_content += "</table>"
        
        html_content += """
        </body>
        </html>
        """
        
        # Save HTML report
        html_report = output_dir / 'batch_report.html'
        with open(html_report, 'w') as f:
            f.write(html_content)
        
        logger.info(f"Saved HTML report: {html_report}")