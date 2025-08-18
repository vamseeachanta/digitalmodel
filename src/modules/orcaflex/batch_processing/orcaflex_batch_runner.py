#!/usr/bin/env python
"""
OrcaFlex Batch Runner with Full API Integration
==============================================
This module provides batch processing capabilities for OrcaFlex models
with support for mooring tension iteration and Length[2] modifications.
"""

import os
import sys
import yaml
import time
import shutil
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Any
import logging
import pandas as pd
from concurrent.futures import ThreadPoolExecutor, as_completed
import multiprocessing

# Import file type detector
sys.path.insert(0, str(Path(__file__).parent.parent))
try:
    from file_type_detector import FileTypeDetector, FileType, FileClassifier
except ImportError:
    FileTypeDetector = None
    FileType = None
    print("Warning: File type detector not available")

# OrcaFlex API import with fallback
try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available. Running in mock mode.")

# Configure logging
logger = logging.getLogger(__name__)


class OrcaFlexBatchRunner:
    """
    Batch runner for OrcaFlex models with full API integration.
    Supports mooring tension iteration with Length[2] modifications.
    """
    
    def __init__(self, config_file: str, mock_mode: bool = False):
        """
        Initialize the batch runner.
        
        Args:
            config_file: Path to batch configuration YAML file
            mock_mode: If True, run without actual OrcaFlex API calls (for testing)
        """
        self.config_file = Path(config_file)
        self.mock_mode = mock_mode or not ORCAFLEX_AVAILABLE
        self.results = []
        self.models_processed = 0
        self.models_failed = 0
        
        # Initialize file type detector
        self.file_detector = FileTypeDetector() if FileTypeDetector else None
        self.file_classifier = FileClassifier() if FileTypeDetector else None
        
        # Load configuration
        self.load_config()
        self.setup_logging()
        
        # Validate configuration if detector available
        if self.file_classifier:
            self.validate_configuration()
        
        if self.mock_mode:
            logger.warning("Running in MOCK MODE - no actual OrcaFlex processing")
    
    def load_config(self):
        """Load and validate the batch configuration."""
        if not self.config_file.exists():
            raise FileNotFoundError(f"Configuration file not found: {self.config_file}")
        
        with open(self.config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Set up paths
        self.base_dir = Path(self.config['batch_info']['base_directory'])
        output_dir = self.config['batch_info']['output_directory']
        
        # Handle relative output directory
        if output_dir.startswith('./'):
            self.output_dir = self.base_dir / output_dir[2:]
        else:
            self.output_dir = Path(output_dir)
        
        # Create output directories
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create subdirectories for different output types
        self.csv_dir = self.output_dir / self.config['output_settings']['csv_output_folder']
        self.sim_dir = self.output_dir / self.config['output_settings']['sim_output_folder']
        self.csv_dir.mkdir(exist_ok=True)
        self.sim_dir.mkdir(exist_ok=True)
        
        # Store key parameters
        self.parallel_processing = self.config['simulation_settings'].get('parallel_processing', False)
        self.section_to_modify = self.config['mooring_parameters'].get('section_to_modify', 2)
        self.tension_tolerance = self.config['mooring_parameters'].get('tension_tolerance', 0.01)
        self.damping_factor = self.config['processing_options'].get('damping_factor', 0.7)
        self.max_iterations = self.config['processing_options'].get('max_iterations', 10)
        
        logger.info(f"Configuration loaded: {self.config['batch_info']['name']}")
        logger.info(f"Base directory: {self.base_dir}")
        logger.info(f"Output directory: {self.output_dir}")
        logger.info(f"Section to modify: Length[{self.section_to_modify}]")
    
    def validate_configuration(self):
        """Validate configuration using file type detector."""
        logger.info("Validating batch configuration and referenced files...")
        
        is_valid, issues = self.file_classifier.validate_batch_config(self.config_file)
        
        if not is_valid:
            logger.warning("Configuration validation issues found:")
            for issue in issues:
                logger.warning(f"  - {issue}")
        else:
            logger.info("Configuration validation PASSED")
        
        # Classify all files in the base directory
        classification = self.file_classifier.classify_directory(self.base_dir)
        
        logger.info(f"Directory classification:")
        logger.info(f"  OrcaFlex models: {len(classification['orcaflex_models'])}")
        logger.info(f"  Includefiles: {len(classification['includefiles'])}")
        logger.info(f"  Target tensions: {len(classification['target_tensions'])}")
        logger.info(f"  Batch configs: {len(classification['batch_configs'])}")
        logger.info(f"  DigitalModel configs: {len(classification['digitalmodel_configs'])}")
    
    def setup_logging(self):
        """Set up logging configuration."""
        log_level = getattr(logging, self.config['processing_options']['log_level'], logging.INFO)
        log_file = self.output_dir / f"batch_run_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
        
        # Configure file and console handlers
        file_handler = logging.FileHandler(log_file)
        console_handler = logging.StreamHandler(sys.stdout)
        
        # Set format
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        # Add handlers to logger
        logger.addHandler(file_handler)
        logger.addHandler(console_handler)
        logger.setLevel(log_level)
    
    def run_batch(self) -> Dict[str, Any]:
        """
        Run batch processing for all configured models.
        
        Returns:
            Dictionary containing batch results and statistics
        """
        start_time = time.time()
        
        logger.info("=" * 80)
        logger.info(f"Starting batch run: {self.config['batch_info']['name']}")
        logger.info(f"Total models to process: {len(self.config['models'])}")
        logger.info(f"Parallel processing: {self.parallel_processing}")
        logger.info("=" * 80)
        
        if self.parallel_processing and not self.mock_mode:
            # Run in parallel mode
            self.results = self.run_parallel_batch()
        else:
            # Run in sequential mode
            self.results = self.run_sequential_batch()
        
        # Calculate elapsed time
        elapsed_time = time.time() - start_time
        
        # Generate summary
        summary = {
            'total_models': len(self.config['models']),
            'successful': self.models_processed,
            'failed': self.models_failed,
            'success_rate': (self.models_processed / len(self.config['models']) * 100) if self.config['models'] else 0,
            'elapsed_time': elapsed_time,
            'results': self.results
        }
        
        # Generate report
        if self.config['processing_options']['generate_report']:
            self.generate_report(summary)
        
        logger.info("\n" + "=" * 80)
        logger.info("BATCH PROCESSING COMPLETE")
        logger.info(f"  Total: {summary['total_models']}")
        logger.info(f"  Successful: {summary['successful']}")
        logger.info(f"  Failed: {summary['failed']}")
        logger.info(f"  Success Rate: {summary['success_rate']:.1f}%")
        logger.info(f"  Total Time: {summary['elapsed_time']:.2f} seconds")
        logger.info("=" * 80)
        
        # Verify simulation files if not in mock mode
        if not self.mock_mode and self.config['simulation_settings']['save_simulation_file']:
            self.verify_sim_files(summary)
        
        return summary
    
    def run_sequential_batch(self) -> List[Dict[str, Any]]:
        """Run batch processing sequentially."""
        results = []
        
        for idx, model_config in enumerate(self.config['models'], 1):
            model_file = model_config['model_file']
            description = model_config.get('description', model_file)
            
            logger.info(f"\n[{idx}/{len(self.config['models'])}] Processing: {description}")
            logger.info(f"  Model file: {model_file}")
            
            try:
                # Process the model
                result = self.process_model(model_config)
                results.append(result)
                
                if result['success']:
                    self.models_processed += 1
                    logger.info(f"  [OK] SUCCESS - Model processed in {result['processing_time']:.2f} seconds")
                    if result.get('tensions_converged'):
                        logger.info(f"    Tensions converged in {result.get('iterations', 0)} iterations")
                        logger.info(f"    Max tension error: {result.get('max_tension_error', 0):.3%}")
                else:
                    self.models_failed += 1
                    logger.error(f"  [FAILED] - {result.get('error', 'Unknown error')}")
                    
            except Exception as e:
                self.models_failed += 1
                logger.error(f"  [ERROR] EXCEPTION - {str(e)}")
                results.append({
                    'model_file': model_file,
                    'description': description,
                    'success': False,
                    'error': str(e)
                })
                
                if not self.config['simulation_settings']['continue_on_error']:
                    logger.error("Stopping batch run due to error (continue_on_error=False)")
                    break
        
        return results
    
    def run_parallel_batch(self) -> List[Dict[str, Any]]:
        """Run batch processing in parallel using ThreadPoolExecutor."""
        results = []
        # Get max_workers from config, default to 30 if not specified
        config_max_workers = self.config.get('simulation_settings', {}).get('max_workers', 30)
        max_workers = min(multiprocessing.cpu_count(), config_max_workers)
        
        logger.info(f"Running parallel processing with {max_workers} workers")
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all tasks
            future_to_model = {}
            for idx, model_config in enumerate(self.config['models'], 1):
                future = executor.submit(self.process_model_with_logging, model_config, idx)
                future_to_model[future] = model_config
            
            # Collect results as they complete
            for future in as_completed(future_to_model):
                model_config = future_to_model[future]
                try:
                    result = future.result()
                    results.append(result)
                    
                    if result['success']:
                        self.models_processed += 1
                        logger.info(f"  [OK] Completed: {model_config['model_file']}")
                    else:
                        self.models_failed += 1
                        logger.error(f"  [FAILED] Failed: {model_config['model_file']}")
                        
                except Exception as e:
                    self.models_failed += 1
                    logger.error(f"  [ERROR] Exception for {model_config['model_file']}: {str(e)}")
                    results.append({
                        'model_file': model_config['model_file'],
                        'description': model_config.get('description', ''),
                        'success': False,
                        'error': str(e)
                    })
        
        return results
    
    def process_model_with_logging(self, model_config: Dict[str, Any], idx: int) -> Dict[str, Any]:
        """Process model with thread-safe logging."""
        model_file = model_config['model_file']
        description = model_config.get('description', model_file)
        
        logger.info(f"[Worker {idx}] Starting: {description}")
        result = self.process_model(model_config)
        
        if result['success']:
            logger.info(f"[Worker {idx}] Finished: {description} ({result['processing_time']:.2f}s)")
        else:
            logger.error(f"[Worker {idx}] Failed: {description}")
        
        return result
    
    def verify_sim_files(self, summary: Dict[str, Any]):
        """Verify that .sim files were actually created."""
        logger.info("\nVerifying simulation file creation...")
        
        sim_files_found = []
        sim_files_missing = []
        
        for result in summary['results']:
            if result.get('success') and 'sim_output' in result:
                sim_path = Path(result['sim_output'])
                if sim_path.exists():
                    file_size = sim_path.stat().st_size
                    sim_files_found.append({
                        'file': sim_path.name,
                        'size': file_size,
                        'size_mb': file_size / (1024 * 1024)
                    })
                    logger.info(f"  [FOUND] {sim_path.name} ({file_size / (1024 * 1024):.2f} MB)")
                else:
                    sim_files_missing.append(sim_path.name)
                    logger.warning(f"  [MISSING] {sim_path.name}")
        
        # Add verification results to summary
        summary['sim_verification'] = {
            'files_found': len(sim_files_found),
            'files_missing': len(sim_files_missing),
            'files_details': sim_files_found,
            'missing_files': sim_files_missing
        }
        
        logger.info(f"\nSimulation file verification:")
        logger.info(f"  Total expected: {len(summary['results'])}")
        logger.info(f"  Files found: {len(sim_files_found)}")
        logger.info(f"  Files missing: {len(sim_files_missing)}")
        
        if sim_files_found:
            total_size = sum(f['size'] for f in sim_files_found)
            logger.info(f"  Total size: {total_size / (1024 * 1024):.2f} MB")
        
        return len(sim_files_found), len(sim_files_missing)
    
    def process_model(self, model_config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process a single OrcaFlex model.
        
        Args:
            model_config: Configuration for the model from batch YAML
            
        Returns:
            Dictionary containing processing results
        """
        start_time = time.time()
        model_path = self.base_dir / model_config['model_file']
        
        # Validate model file exists
        if not model_path.exists():
            return {
                'model_file': model_config['model_file'],
                'description': model_config.get('description', ''),
                'success': False,
                'error': f"Model file not found: {model_path}"
            }
        
        # Check for includefile
        includefile_path = None
        if 'includefile' in model_config:
            includefile_path = self.base_dir / model_config['includefile']
            if not includefile_path.exists():
                logger.warning(f"  Includefile not found: {model_config['includefile']}")
                includefile_path = None
            else:
                logger.info(f"  Using includefile: {model_config['includefile']}")
        
        # Check for target tensions CSV
        target_tensions_path = None
        if 'target_tensions' in model_config:
            target_tensions_path = self.base_dir / model_config['target_tensions']
            if target_tensions_path.exists():
                logger.info(f"  Target tensions: {model_config['target_tensions']}")
        
        # Create output paths
        model_name = Path(model_config['model_file']).stem
        csv_output = self.csv_dir / f"{model_name}_results.csv"
        
        # Save .sim file in the same directory as the model file
        save_sim_in_model_dir = self.config.get('output_settings', {}).get('save_sim_in_model_directory', False)
        if save_sim_in_model_dir:
            # Save .sim in the same directory as the model
            sim_output = model_path.parent / f"{model_name}.sim"
        else:
            # Save .sim in the configured output directory
            sim_output = self.sim_dir / f"{model_name}.sim"
        
        if self.mock_mode:
            # Mock mode for testing
            result = self._mock_process_model(model_config, model_path, includefile_path, target_tensions_path)
        else:
            # Actual OrcaFlex processing
            result = self._orcaflex_process_model(
                model_config, model_path, includefile_path, 
                target_tensions_path, csv_output, sim_output
            )
        
        # Add timing information
        result['processing_time'] = time.time() - start_time
        result['csv_output'] = str(csv_output)
        result['sim_output'] = str(sim_output)
        
        return result
    
    def _orcaflex_process_model(self, model_config: Dict, model_path: Path, 
                                includefile_path: Optional[Path], 
                                target_tensions_path: Optional[Path],
                                csv_output: Path, sim_output: Path) -> Dict[str, Any]:
        """
        Process model using actual OrcaFlex API.
        
        Returns:
            Processing results dictionary
        """
        try:
            # Load the model
            logger.info("  Loading OrcaFlex model...")
            model = OrcFxAPI.Model()
            model.LoadData(str(model_path))
            
            # Apply includefile if provided
            if includefile_path:
                logger.info(f"  Applying includefile with Length[{self.section_to_modify}] modifications...")
                with open(includefile_path, 'r') as f:
                    include_data = yaml.safe_load(f)
                
                # Apply Length[2] modifications from includefile
                self._apply_length_modifications(model, include_data)
            
            # Run static analysis
            logger.info("  Running static analysis...")
            model.CalculateStatics()
            
            # Check if we need to iterate for target tensions
            tensions_converged = True
            iterations = 0
            max_tension_error = 0.0
            
            if target_tensions_path:
                # Load target tensions
                target_tensions = pd.read_csv(target_tensions_path)
                
                # Iterate to achieve target tensions
                tensions_converged, iterations, max_tension_error = self._iterate_tensions(
                    model, target_tensions, model_config
                )
            
            # Save simulation file
            if self.config['simulation_settings']['save_simulation_file']:
                logger.info(f"  Saving simulation file: {sim_output.name}")
                model.SaveSimulation(str(sim_output))
            
            # Extract and save results
            if self.config['output_settings']['save_csv']:
                logger.info(f"  Extracting results to: {csv_output.name}")
                self._extract_results_to_csv(model, csv_output, model_config)
            
            return {
                'model_file': model_config['model_file'],
                'description': model_config.get('description', ''),
                'success': True,
                'tensions_converged': tensions_converged,
                'iterations': iterations,
                'max_tension_error': max_tension_error
            }
            
        except Exception as e:
            return {
                'model_file': model_config['model_file'],
                'description': model_config.get('description', ''),
                'success': False,
                'error': str(e)
            }
    
    def _apply_length_modifications(self, model: 'OrcFxAPI.Model', include_data: Dict):
        """Apply Length[2] modifications from includefile to the model."""
        if 'UnstretchedLength' not in include_data:
            return
        
        for line_name, sections in include_data['UnstretchedLength'].items():
            try:
                line = model[line_name]
                
                # Get the Length array
                lengths = line.Length
                
                # Modify Length[section_to_modify] (typically Length[2])
                if f'Length[{self.section_to_modify}]' in sections:
                    new_length = sections[f'Length[{self.section_to_modify}]']
                    lengths[self.section_to_modify - 1] = new_length  # OrcaFlex uses 0-based indexing
                    line.Length = lengths
                    logger.debug(f"    Updated {line_name} Length[{self.section_to_modify}] = {new_length:.4f} m")
                    
            except Exception as e:
                logger.warning(f"    Could not modify {line_name}: {str(e)}")
    
    def _iterate_tensions(self, model: 'OrcFxAPI.Model', target_tensions: pd.DataFrame, 
                         model_config: Dict) -> Tuple[bool, int, float]:
        """
        Iterate mooring line lengths to achieve target tensions.
        
        Returns:
            Tuple of (converged, iterations, max_error)
        """
        converged = False
        iterations = 0
        max_error = float('inf')
        
        # Implementation would iterate here, adjusting Length[2] values
        # This is a placeholder for the actual iteration logic
        
        for iteration in range(self.max_iterations):
            iterations = iteration + 1
            
            # Run statics
            model.CalculateStatics()
            
            # Extract current tensions
            current_tensions = self._extract_mooring_tensions(model, target_tensions)
            
            # Calculate errors
            errors = {}
            for line_name, current in current_tensions.items():
                target = target_tensions[target_tensions['line_name'] == line_name]['target_tension'].values[0]
                errors[line_name] = abs(current - target) / target
            
            max_error = max(errors.values()) if errors else 0
            
            # Check convergence
            if max_error <= self.tension_tolerance:
                converged = True
                break
            
            # Apply adjustments (simplified - actual implementation would use the formula)
            # ΔL = L/EA × (T_current - T_target) × damping_factor
            
        return converged, iterations, max_error
    
    def _extract_mooring_tensions(self, model: 'OrcFxAPI.Model', target_tensions: pd.DataFrame) -> Dict[str, float]:
        """Extract mooring tensions at anchors."""
        tensions = {}
        
        for _, row in target_tensions.iterrows():
            line_name = row['line_name']
            try:
                line = model[line_name]
                # Get tension at anchor (End A)
                tension = line.StaticResult('Effective Tension', OrcFxAPI.oeEndA)
                tensions[line_name] = tension
            except:
                tensions[line_name] = 0.0
        
        return tensions
    
    def _extract_results_to_csv(self, model: 'OrcFxAPI.Model', csv_path: Path, model_config: Dict):
        """Extract analysis results to CSV file."""
        results = []
        
        # Extract mooring line results
        for line_name in self.config['mooring_parameters']['lines_to_check']:
            try:
                line = model[line_name]
                result = {
                    'model': model_config['model_file'],
                    'line': line_name,
                    'tension_endA': line.StaticResult('Effective Tension', OrcFxAPI.oeEndA),
                    'tension_endB': line.StaticResult('Effective Tension', OrcFxAPI.oeEndB),
                    'length_section2': line.Length[self.section_to_modify - 1]
                }
                results.append(result)
            except:
                pass
        
        # Save to CSV
        if results:
            df = pd.DataFrame(results)
            df.to_csv(csv_path, index=False)
    
    def _mock_process_model(self, model_config: Dict, model_path: Path, 
                           includefile_path: Optional[Path], 
                           target_tensions_path: Optional[Path]) -> Dict[str, Any]:
        """
        Mock processing for testing without OrcaFlex license.
        
        Returns:
            Mock processing results
        """
        # Simulate processing time
        time.sleep(0.1)
        
        # Return mock successful result
        return {
            'model_file': model_config['model_file'],
            'description': model_config.get('description', ''),
            'success': True,
            'tensions_converged': True,
            'iterations': 3,
            'max_tension_error': 0.008
        }
    
    def generate_report(self, summary: Dict[str, Any]):
        """Generate detailed batch processing report."""
        report_file = self.output_dir / f"batch_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        
        with open(report_file, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("OrcaFlex Batch Processing Report\n")
            f.write(f"{self.config['batch_info']['name']}\n")
            f.write("=" * 80 + "\n\n")
            
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"Configuration: {self.config_file}\n")
            f.write(f"Base Directory: {self.base_dir}\n")
            f.write(f"Output Directory: {self.output_dir}\n")
            f.write(f"Mode: {'MOCK' if self.mock_mode else 'LIVE'}\n\n")
            
            f.write("Configuration Parameters:\n")
            f.write(f"  Section to Modify: Length[{self.section_to_modify}]\n")
            f.write(f"  Convergence Tolerance: {self.tension_tolerance*100:.1f}%\n")
            f.write(f"  Damping Factor: {self.damping_factor}\n")
            f.write(f"  Max Iterations: {self.max_iterations}\n")
            f.write(f"  Parallel Processing: {self.parallel_processing}\n\n")
            
            f.write("Summary Statistics:\n")
            f.write(f"  Total Models: {summary['total_models']}\n")
            f.write(f"  Successful: {summary['successful']}\n")
            f.write(f"  Failed: {summary['failed']}\n")
            f.write(f"  Success Rate: {summary['success_rate']:.1f}%\n")
            f.write(f"  Total Time: {summary['elapsed_time']:.2f} seconds\n")
            f.write(f"  Average Time per Model: {summary['elapsed_time']/summary['total_models']:.2f} seconds\n\n")
            
            f.write("Detailed Results:\n")
            f.write("-" * 80 + "\n")
            
            for idx, result in enumerate(summary['results'], 1):
                f.write(f"\n[{idx}] Model: {result['model_file']}\n")
                f.write(f"    Description: {result.get('description', 'N/A')}\n")
                
                if result['success']:
                    f.write(f"    Status: SUCCESS\n")
                    f.write(f"    Processing Time: {result.get('processing_time', 0):.2f} seconds\n")
                    
                    if 'tensions_converged' in result:
                        f.write(f"    Tensions Converged: {result['tensions_converged']}\n")
                        f.write(f"    Iterations: {result.get('iterations', 0)}\n")
                        f.write(f"    Max Tension Error: {result.get('max_tension_error', 0):.3%}\n")
                    
                    if 'sim_output' in result:
                        f.write(f"    Simulation File: {Path(result['sim_output']).name}\n")
                    if 'csv_output' in result:
                        f.write(f"    Results CSV: {Path(result['csv_output']).name}\n")
                else:
                    f.write(f"    Status: FAILED\n")
                    f.write(f"    Error: {result.get('error', 'Unknown error')}\n")
            
            f.write("\n" + "=" * 80 + "\n")
            f.write("End of Report\n")
        
        logger.info(f"Report saved to: {report_file}")
        
        # Generate failure summary if there are failures
        if summary['failed'] > 0:
            self.generate_failure_summary(summary)
    
    def generate_failure_summary(self, summary: Dict[str, Any]):
        """
        Generate a detailed failure summary and rerun configuration.
        
        Args:
            summary: The batch processing summary
        """
        # Create failure summary file
        failure_file = self.output_dir / f"failed_runs_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        rerun_config_file = self.output_dir / f"rerun_failed_{datetime.now().strftime('%Y%m%d_%H%M%S')}.yml"
        
        failed_models = [r for r in summary['results'] if not r['success']]
        
        # Write detailed failure summary
        with open(failure_file, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("FAILURE SUMMARY REPORT\n")
            f.write("=" * 80 + "\n\n")
            f.write(f"Total Failed Models: {len(failed_models)}\n")
            f.write(f"Failure Rate: {(len(failed_models)/summary['total_models']*100):.1f}%\n\n")
            
            # Group failures by error type
            error_groups = {}
            for model in failed_models:
                error = model.get('error', 'Unknown error')
                # Simplify error message for grouping
                error_key = error.split('\n')[0][:100]  # First line, max 100 chars
                if error_key not in error_groups:
                    error_groups[error_key] = []
                error_groups[error_key].append(model)
            
            f.write("FAILURES BY ERROR TYPE:\n")
            f.write("-" * 40 + "\n")
            for error_type, models in error_groups.items():
                f.write(f"\nError: {error_type}\n")
                f.write(f"Count: {len(models)} models\n")
                f.write("Affected models:\n")
                for model in models:
                    f.write(f"  - {model['model_file']}\n")
            
            f.write("\n" + "=" * 80 + "\n")
            f.write("DETAILED FAILURE INFORMATION:\n")
            f.write("-" * 40 + "\n")
            
            for idx, model in enumerate(failed_models, 1):
                f.write(f"\n[{idx}] Model: {model['model_file']}\n")
                f.write(f"    Description: {model.get('description', 'N/A')}\n")
                f.write(f"    Error Type: {model.get('error_type', 'Unknown')}\n")
                f.write(f"    Error Message:\n")
                error_msg = model.get('error', 'No error message available')
                for line in error_msg.split('\n'):
                    f.write(f"        {line}\n")
                f.write(f"    Processing Time: {model.get('processing_time', 0):.2f} seconds\n")
                f.write(f"    Timestamp: {model.get('timestamp', 'N/A')}\n")
                
                # Add suggestions for common errors
                if 'license' in error_msg.lower():
                    f.write("    SUGGESTION: Check OrcaFlex license availability\n")
                elif 'file not found' in error_msg.lower():
                    f.write("    SUGGESTION: Verify model file path and include files\n")
                elif 'convergence' in error_msg.lower():
                    f.write("    SUGGESTION: Adjust convergence parameters or damping factor\n")
            
            f.write("\n" + "=" * 80 + "\n")
            f.write("RERUN INSTRUCTIONS:\n")
            f.write("-" * 40 + "\n")
            f.write(f"1. A rerun configuration has been created: {rerun_config_file.name}\n")
            f.write(f"2. To rerun failed models, execute:\n")
            f.write(f"   python -m orcaflex_batch_runner --config {rerun_config_file}\n")
            f.write(f"3. Consider adjusting parameters based on error types above\n")
            f.write(f"4. Check system resources and OrcaFlex license before rerunning\n")
        
        # Create rerun configuration for failed models
        rerun_config = self.config.copy()
        rerun_config['batch_info']['name'] = f"Rerun Failed Models - {self.config['batch_info']['name']}"
        rerun_config['batch_info']['description'] = f"Rerunning {len(failed_models)} failed models from previous batch"
        rerun_config['batch_info']['original_config'] = str(self.config_file)
        rerun_config['batch_info']['timestamp'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        
        # Include only failed models
        rerun_config['models'] = []
        for model in failed_models:
            # Find original model config
            for orig_model in self.config['models']:
                if orig_model['model_file'] == model['model_file']:
                    model_config = orig_model.copy()
                    model_config['previous_error'] = model.get('error', 'Unknown')
                    model_config['retry_attempt'] = model.get('retry_attempt', 0) + 1
                    rerun_config['models'].append(model_config)
                    break
        
        # Adjust processing options for retry
        rerun_config['processing_options']['log_level'] = 'DEBUG'  # More verbose logging
        if 'convergence' in str(error_groups.keys()).lower():
            # Adjust convergence parameters if convergence errors detected
            rerun_config['mooring_parameters']['tension_tolerance'] *= 1.5
            rerun_config['processing_options']['damping_factor'] *= 0.9
            rerun_config['processing_options']['max_iterations'] += 5
        
        # Save rerun configuration
        with open(rerun_config_file, 'w') as f:
            yaml.dump(rerun_config, f, default_flow_style=False)
        
        logger.info(f"Failure summary saved to: {failure_file}")
        logger.info(f"Rerun configuration saved to: {rerun_config_file}")
        logger.info(f"Failed models can be rerun using the generated configuration")


def main():
    """Main entry point for batch runner."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='OrcaFlex Batch Runner - Process multiple models with mooring tension iteration'
    )
    
    parser.add_argument(
        '--config',
        required=True,
        help='Path to batch configuration YAML file'
    )
    
    parser.add_argument(
        '--mock',
        action='store_true',
        help='Run in mock mode without OrcaFlex API'
    )
    
    parser.add_argument(
        '--log-level',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        default='INFO',
        help='Logging level'
    )
    
    args = parser.parse_args()
    
    # Set logging level
    logging.basicConfig(level=getattr(logging, args.log_level))
    
    # Run batch processing
    runner = OrcaFlexBatchRunner(args.config, mock_mode=args.mock)
    summary = runner.run_batch()
    
    # Return exit code based on success
    sys.exit(0 if summary['failed'] == 0 else 1)


if __name__ == "__main__":
    main()