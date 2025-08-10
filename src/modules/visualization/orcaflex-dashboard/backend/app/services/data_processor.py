"""
Data Processing Orchestrator

Main orchestrator for OrcaFlex data processing pipeline.
Coordinates parsing, classification, validation, and database operations.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Union, Tuple, Any
from pathlib import Path
from dataclasses import dataclass, asdict
from datetime import datetime
import numpy as np
import pandas as pd
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
import json

from .csv_parser import CSVParser, FileType, PolarData
from .component_classifier import ComponentClassifier
from .loading_decoder import LoadingDecoder
from .data_validator import DataValidator
from ..models.analysis import Analysis
from ..models.result import Result
from ..models.component import Component

logger = logging.getLogger(__name__)


@dataclass
class ProcessingConfig:
    """Configuration for data processing"""
    chunk_size: int = 10000
    max_workers: int = 4
    memory_limit_mb: int = 1024  # 1GB default
    enable_caching: bool = True
    cache_ttl_hours: int = 24
    validation_level: str = 'standard'  # minimal, standard, strict
    parallel_processing: bool = True


@dataclass
class ProcessingResult:
    """Result of data processing operation"""
    success: bool
    file_path: str
    file_type: FileType
    processing_time_seconds: float
    analysis_id: Optional[str] = None
    component_count: int = 0
    result_count: int = 0
    errors: List[str] = None
    warnings: List[str] = None
    metadata: Dict[str, Any] = None
    
    def __post_init__(self):
        if self.errors is None:
            self.errors = []
        if self.warnings is None:
            self.warnings = []
        if self.metadata is None:
            self.metadata = {}


@dataclass
class BatchProcessingResult:
    """Result of batch processing operation"""
    total_files: int
    successful: int
    failed: int
    processing_time_seconds: float
    results: List[ProcessingResult]
    summary_stats: Dict[str, Any]


class DataProcessor:
    """
    Main data processing orchestrator for OrcaFlex CSV files.
    
    Coordinates the entire data processing pipeline:
    1. File parsing and validation
    2. Component classification
    3. Loading condition decoding
    4. Data quality validation
    5. Database persistence
    6. Performance monitoring
    """
    
    def __init__(self, config: Optional[ProcessingConfig] = None):
        """
        Initialize data processor.
        
        Args:
            config: Processing configuration
        """
        self.config = config or ProcessingConfig()
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        
        # Initialize component services
        self.csv_parser = CSVParser(chunk_size=self.config.chunk_size)
        self.component_classifier = ComponentClassifier()
        self.loading_decoder = LoadingDecoder()
        self.data_validator = DataValidator()
        
        # Performance tracking
        self.processing_stats = {
            'files_processed': 0,
            'total_processing_time': 0.0,
            'average_processing_time': 0.0,
            'memory_usage_peak': 0.0,
            'errors': 0,
            'warnings': 0
        }
        
        # Thread pool for parallel processing
        if self.config.parallel_processing:
            self.executor = ThreadPoolExecutor(max_workers=self.config.max_workers)
        else:
            self.executor = None
    
    async def process_file(self, file_path: Union[str, Path], 
                          analysis_case: Optional[str] = None) -> ProcessingResult:
        """
        Process a single OrcaFlex CSV file.
        
        Args:
            file_path: Path to CSV file
            analysis_case: Optional analysis case identifier
            
        Returns:
            ProcessingResult with operation details
        """
        start_time = time.time()
        file_path = Path(file_path)
        
        result = ProcessingResult(
            success=False,
            file_path=str(file_path),
            file_type=FileType.UNKNOWN,
            processing_time_seconds=0.0
        )
        
        try:
            self.logger.info(f"Processing file: {file_path.name}")
            
            # Step 1: Parse the file
            parse_result = await self._parse_file_async(file_path)
            result.file_type = parse_result['file_type']
            result.metadata.update({
                'file_size_mb': file_path.stat().st_size / 1024**2,
                'shape': parse_result['shape'],
                'column_count': parse_result['shape'][1]
            })
            
            # Step 2: Classify components and decode loading conditions
            classification_result = await self._classify_and_decode_async(parse_result)
            
            # Step 3: Validate data quality
            validation_result = await self._validate_data_async(parse_result, classification_result)
            
            # Step 4: Process based on file type
            if result.file_type == FileType.DM_SUMMARY:
                processing_result = await self._process_summary_file(
                    parse_result, classification_result, validation_result
                )
            elif result.file_type == FileType.TIME_TRACE:
                processing_result = await self._process_time_trace_file(
                    parse_result, classification_result, validation_result
                )
            elif result.file_type == FileType.DM_INPUTS:
                processing_result = await self._process_inputs_file(
                    parse_result, classification_result, validation_result
                )
            else:
                raise ValueError(f"Unsupported file type: {result.file_type}")
            
            # Step 5: Persist to database
            if processing_result['success']:
                persistence_result = await self._persist_data_async(
                    processing_result, analysis_case
                )
                result.analysis_id = persistence_result.get('analysis_id')
                result.component_count = persistence_result.get('component_count', 0)
                result.result_count = persistence_result.get('result_count', 0)
            
            # Compile final result
            result.success = processing_result['success']
            result.errors.extend(processing_result.get('errors', []))
            result.warnings.extend(processing_result.get('warnings', []))
            result.warnings.extend(validation_result.get('warnings', []))
            
            # Update metadata
            result.metadata.update({
                'polar_data_count': len(processing_result.get('polar_data', {})),
                'validation_score': validation_result.get('overall_score', 0.0),
                'processing_steps_completed': processing_result.get('steps_completed', 0)
            })
            
            self.logger.info(f"Successfully processed {file_path.name}")
            
        except Exception as e:
            self.logger.error(f"Failed to process {file_path.name}: {str(e)}")
            result.errors.append(f"Processing failed: {str(e)}")
            result.success = False
        
        finally:
            result.processing_time_seconds = time.time() - start_time
            self._update_stats(result)
        
        return result
    
    async def process_batch(self, file_paths: List[Union[str, Path]], 
                           analysis_case: Optional[str] = None) -> BatchProcessingResult:
        """
        Process multiple files in batch with parallel processing.
        
        Args:
            file_paths: List of file paths to process
            analysis_case: Optional analysis case identifier
            
        Returns:
            BatchProcessingResult with batch operation details
        """
        start_time = time.time()
        self.logger.info(f"Starting batch processing of {len(file_paths)} files")
        
        # Process files
        if self.config.parallel_processing and self.executor:
            # Parallel processing
            futures = []
            for file_path in file_paths:
                future = asyncio.get_event_loop().run_in_executor(
                    self.executor, 
                    lambda fp: asyncio.run(self.process_file(fp, analysis_case)),
                    file_path
                )
                futures.append(future)
            
            results = await asyncio.gather(*futures, return_exceptions=True)
        else:
            # Sequential processing
            results = []
            for file_path in file_paths:
                result = await self.process_file(file_path, analysis_case)
                results.append(result)
        
        # Handle exceptions in results
        processed_results = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                self.logger.error(f"File {file_paths[i]} failed with exception: {result}")
                processed_results.append(ProcessingResult(
                    success=False,
                    file_path=str(file_paths[i]),
                    file_type=FileType.UNKNOWN,
                    processing_time_seconds=0.0,
                    errors=[f"Exception: {str(result)}"]
                ))
            else:
                processed_results.append(result)
        
        # Calculate batch statistics
        successful = sum(1 for r in processed_results if r.success)
        failed = len(processed_results) - successful
        total_time = time.time() - start_time
        
        # Summary statistics
        summary_stats = self._calculate_batch_stats(processed_results)
        
        batch_result = BatchProcessingResult(
            total_files=len(file_paths),
            successful=successful,
            failed=failed,
            processing_time_seconds=total_time,
            results=processed_results,
            summary_stats=summary_stats
        )
        
        self.logger.info(f"Batch processing completed: {successful}/{len(file_paths)} successful "
                        f"in {total_time:.1f}s")
        
        return batch_result
    
    async def _parse_file_async(self, file_path: Path) -> Dict:
        """Async wrapper for file parsing"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            self.executor, self.csv_parser.parse_file, file_path
        )
    
    async def _classify_and_decode_async(self, parse_result: Dict) -> Dict:
        """Async classification and decoding"""
        loop = asyncio.get_event_loop()
        
        # Run classification and decoding in parallel
        classify_task = loop.run_in_executor(
            self.executor, 
            self.component_classifier.classify_file,
            parse_result
        )
        
        decode_task = loop.run_in_executor(
            self.executor,
            self.loading_decoder.decode_loading_conditions,
            Path(parse_result['file_path']),
            parse_result['dataframe']
        )
        
        classification, loading_conditions = await asyncio.gather(
            classify_task, decode_task
        )
        
        return {
            'components': classification,
            'loading_conditions': loading_conditions
        }
    
    async def _validate_data_async(self, parse_result: Dict, 
                                  classification_result: Dict) -> Dict:
        """Async data validation"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            self.executor,
            self.data_validator.validate_analysis_data,
            parse_result['dataframe'],
            classification_result['components'],
            self.config.validation_level
        )
    
    async def _process_summary_file(self, parse_result: Dict, 
                                   classification_result: Dict,
                                   validation_result: Dict) -> Dict:
        """Process dm_* summary files with polar data"""
        try:
            result = {
                'success': True,
                'errors': [],
                'warnings': [],
                'polar_data': {},
                'statistics': {},
                'steps_completed': 0
            }
            
            df = parse_result['dataframe']
            components = classification_result['components']
            loading_conditions = classification_result['loading_conditions']
            
            # Extract polar data
            if 'polar_data' in parse_result:
                result['polar_data'] = parse_result['polar_data']
                result['steps_completed'] += 1
            
            # Calculate summary statistics for each component
            for component_info in components:
                component_name = component_info['name']
                component_columns = component_info['columns']
                
                component_stats = {}
                for col in component_columns:
                    if col in df.columns:
                        data = df[col].dropna()
                        if len(data) > 0:
                            component_stats[col] = {
                                'mean': float(np.mean(data)),
                                'std': float(np.std(data)),
                                'min': float(np.min(data)),
                                'max': float(np.max(data)),
                                'rms': float(np.sqrt(np.mean(data**2)))
                            }
                
                if component_stats:
                    result['statistics'][component_name] = component_stats
            
            result['steps_completed'] += 1
            
            # Add loading condition metadata
            result['loading_conditions'] = loading_conditions
            result['steps_completed'] += 1
            
            self.logger.debug(f"Processed summary file with {len(result['polar_data'])} polar datasets")
            
        except Exception as e:
            result = {
                'success': False,
                'errors': [f"Summary processing failed: {str(e)}"],
                'warnings': [],
                'steps_completed': 0
            }
            
        return result
    
    async def _process_time_trace_file(self, parse_result: Dict,
                                      classification_result: Dict,
                                      validation_result: Dict) -> Dict:
        """Process time trace files"""
        try:
            result = {
                'success': True,
                'errors': [],
                'warnings': [],
                'time_series_stats': {},
                'steps_completed': 0
            }
            
            df = parse_result['dataframe']
            
            # Find time column
            time_col = None
            for col in df.columns:
                if 'time' in col.lower() or col.lower() == 't':
                    time_col = col
                    break
            
            if time_col is None:
                result['warnings'].append("No time column found in time trace file")
                time_col = df.columns[0]  # Use first column as fallback
            
            # Calculate time series statistics
            from .csv_parser import extract_time_series_stats
            time_stats = extract_time_series_stats(df, time_col)
            
            if 'error' not in time_stats:
                result['time_series_stats'] = time_stats
                result['steps_completed'] += 1
            else:
                result['warnings'].append(f"Time series analysis failed: {time_stats['error']}")
            
            # Add component-specific analysis
            components = classification_result['components']
            for component_info in components:
                component_name = component_info['name']
                component_columns = component_info['columns']
                
                component_ts_stats = {}
                for col in component_columns:
                    if col in time_stats and isinstance(time_stats[col], dict):
                        component_ts_stats[col] = time_stats[col]
                
                if component_ts_stats:
                    result['time_series_stats'][f"{component_name}_detailed"] = component_ts_stats
            
            result['steps_completed'] += 1
            self.logger.debug(f"Processed time trace with {len(time_stats)} time series")
            
        except Exception as e:
            result = {
                'success': False,
                'errors': [f"Time trace processing failed: {str(e)}"],
                'warnings': [],
                'steps_completed': 0
            }
            
        return result
    
    async def _process_inputs_file(self, parse_result: Dict,
                                  classification_result: Dict,
                                  validation_result: Dict) -> Dict:
        """Process dm_*_inputs.csv files"""
        try:
            result = {
                'success': True,
                'errors': [],
                'warnings': [],
                'input_parameters': {},
                'linked_files': [],
                'steps_completed': 0
            }
            
            df = parse_result['dataframe']
            
            # Extract input parameters
            for col in df.columns:
                if df[col].dtype in ['object', 'string']:
                    # Look for file references
                    file_refs = df[col].dropna().astype(str)
                    file_refs = file_refs[file_refs.str.contains(r'\.(csv|txt|dat)$', na=False)]
                    if len(file_refs) > 0:
                        result['linked_files'].extend(file_refs.tolist())
                else:
                    # Numerical parameters
                    data = df[col].dropna()
                    if len(data) > 0:
                        result['input_parameters'][col] = {
                            'value': float(data.iloc[-1]) if len(data) == 1 else data.tolist(),
                            'type': str(df[col].dtype)
                        }
            
            result['steps_completed'] += 1
            self.logger.debug(f"Processed inputs file with {len(result['input_parameters'])} parameters")
            
        except Exception as e:
            result = {
                'success': False,
                'errors': [f"Inputs processing failed: {str(e)}"],
                'warnings': [],
                'steps_completed': 0
            }
            
        return result
    
    async def _persist_data_async(self, processing_result: Dict,
                                 analysis_case: Optional[str] = None) -> Dict:
        """Persist processed data to database"""
        # This would implement database persistence
        # For now, return mock result
        persistence_result = {
            'analysis_id': f"analysis_{int(time.time())}",
            'component_count': len(processing_result.get('statistics', {})),
            'result_count': len(processing_result.get('polar_data', {}))
        }
        
        # TODO: Implement actual database persistence
        self.logger.debug("Database persistence not yet implemented")
        
        return persistence_result
    
    def _update_stats(self, result: ProcessingResult):
        """Update processing statistics"""
        self.processing_stats['files_processed'] += 1
        self.processing_stats['total_processing_time'] += result.processing_time_seconds
        self.processing_stats['average_processing_time'] = (
            self.processing_stats['total_processing_time'] / 
            self.processing_stats['files_processed']
        )
        
        if not result.success:
            self.processing_stats['errors'] += 1
        
        if result.warnings:
            self.processing_stats['warnings'] += len(result.warnings)
    
    def _calculate_batch_stats(self, results: List[ProcessingResult]) -> Dict[str, Any]:
        """Calculate summary statistics for batch processing"""
        if not results:
            return {}
        
        processing_times = [r.processing_time_seconds for r in results]
        file_types = [r.file_type.value for r in results if r.file_type != FileType.UNKNOWN]
        
        return {
            'processing_time': {
                'total': sum(processing_times),
                'mean': np.mean(processing_times),
                'std': np.std(processing_times),
                'min': min(processing_times),
                'max': max(processing_times)
            },
            'file_types': {
                file_type: file_types.count(file_type) 
                for file_type in set(file_types)
            },
            'success_rate': sum(1 for r in results if r.success) / len(results),
            'total_errors': sum(len(r.errors) for r in results),
            'total_warnings': sum(len(r.warnings) for r in results),
            'total_components': sum(r.component_count for r in results),
            'total_results': sum(r.result_count for r in results)
        }
    
    def get_processing_stats(self) -> Dict[str, Any]:
        """Get current processing statistics"""
        return self.processing_stats.copy()
    
    def reset_stats(self):
        """Reset processing statistics"""
        self.processing_stats = {
            'files_processed': 0,
            'total_processing_time': 0.0,
            'average_processing_time': 0.0,
            'memory_usage_peak': 0.0,
            'errors': 0,
            'warnings': 0
        }
    
    def __del__(self):
        """Cleanup thread pool"""
        if hasattr(self, 'executor') and self.executor:
            self.executor.shutdown(wait=True)


# Factory function for easy instantiation
def create_processor(chunk_size: int = 10000, 
                    max_workers: int = 4,
                    memory_limit_mb: int = 1024,
                    validation_level: str = 'standard') -> DataProcessor:
    """
    Create a configured data processor.
    
    Args:
        chunk_size: Chunk size for processing large files
        max_workers: Maximum number of worker threads
        memory_limit_mb: Memory limit in MB
        validation_level: Validation strictness (minimal, standard, strict)
        
    Returns:
        Configured DataProcessor instance
    """
    config = ProcessingConfig(
        chunk_size=chunk_size,
        max_workers=max_workers,
        memory_limit_mb=memory_limit_mb,
        validation_level=validation_level
    )
    
    return DataProcessor(config)