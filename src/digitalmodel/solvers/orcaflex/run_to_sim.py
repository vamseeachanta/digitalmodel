#!/usr/bin/env python
"""
OrcaFlex Model Runner - Generate .sim Files
===========================================
This module provides functionality to run OrcaFlex models (.yml files) 
and save them as simulation files (.sim).
"""

import logging
import time
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock
from typing import List, Dict, Optional, Any

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False

logger = logging.getLogger(__name__)


class OrcaFlexModelRunner:
    """Class to handle running OrcaFlex models and generating .sim files."""
    
    def __init__(self, mock_mode: bool = False):
        """
        Initialize the OrcaFlex model runner.
        
        Args:
            mock_mode: If True, simulate without OrcaFlex license
        """
        self.mock_mode = mock_mode
        self.lock = Lock()
        
    def run_single_model(self, 
                        model_file: Path, 
                        output_dir: Optional[Path] = None) -> Dict[str, Any]:
        """
        Run a single OrcaFlex model and save as .sim file.
        
        Args:
            model_file: Path to the .yml model file
            output_dir: Directory to save .sim files (if None, saves in same directory as model)
        
        Returns:
            dict: Result with success status and output path
        """
        model_path = Path(model_file)
        
        # If no output_dir specified, use the same directory as the model file
        if output_dir is None:
            output_path = model_path.parent
        else:
            output_path = Path(output_dir)
            output_path.mkdir(parents=True, exist_ok=True)
        
        # Generate output filename
        sim_file = output_path / f"{model_path.stem}.sim"
        
        result = {
            'model': model_path.name,
            'sim_output': str(sim_file),
            'success': False,
            'error': None,
            'time': 0
        }
        
        start_time = time.time()
        
        try:
            if self.mock_mode or not ORCAFLEX_AVAILABLE:
                # Mock mode - simulate success
                logger.info(f"[MOCK] Processing: {model_path.name}")
                time.sleep(0.1)  # Simulate processing time
                logger.info(f"[MOCK] Would save to: {sim_file}")
                result['success'] = True
                result['mock'] = True
            else:
                # Real mode - use OrcaFlex
                logger.info(f"Loading model: {model_path.name}")
                
                # Load the model
                model = OrcFxAPI.Model()
                model.LoadData(str(model_path))
                
                # Run static analysis
                logger.info(f"Running static analysis for {model_path.name}...")
                model.CalculateStatics()
                
                # Save as .sim file
                logger.info(f"Saving to: {sim_file}")
                model.SaveSimulation(str(sim_file))
                
                result['success'] = True
                logger.info(f"✓ Successfully created: {sim_file.name}")
                
        except Exception as e:
            error_msg = str(e)
            logger.error(f"✗ Failed processing {model_path.name}: {error_msg}")
            result['error'] = error_msg
        
        result['time'] = time.time() - start_time
        return result
    
    def run_batch(self, 
                  model_list: List[Path], 
                  output_dir: Optional[Path] = None, 
                  max_workers: int = 30) -> Dict[str, Any]:
        """
        Run multiple OrcaFlex models in parallel.
        
        Args:
            model_list: List of model file paths
            output_dir: Directory to save .sim files (if None, saves in same directory as each model)
            max_workers: Maximum number of parallel threads (default: 30)
        
        Returns:
            dict: Summary of batch processing results
        """
        logger.info("=" * 80)
        logger.info("Starting Parallel Batch Processing")
        logger.info(f"Total models: {len(model_list)}")
        logger.info(f"Parallel threads: {max_workers}")
        logger.info(f"Output: {'Same directory as models' if output_dir is None else output_dir}")
        logger.info(f"Mode: {'MOCK' if self.mock_mode else 'REAL'}")
        logger.info("=" * 80)
        
        results = []
        successful = 0
        failed = 0
        completed_count = 0
        
        start_batch_time = time.time()
        
        # Use ThreadPoolExecutor for parallel processing
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all tasks
            future_to_model = {
                executor.submit(self.run_single_model, model_file, output_dir): model_file 
                for model_file in model_list
            }
            
            # Process completed tasks as they finish
            for future in as_completed(future_to_model):
                model_file = future_to_model[future]
                
                try:
                    result = future.result()
                    
                    with self.lock:
                        completed_count += 1
                        results.append(result)
                        
                        if result['success']:
                            successful += 1
                            logger.info(f"[{completed_count}/{len(model_list)}] ✓ Completed: {Path(model_file).name} ({result['time']:.2f}s)")
                        else:
                            failed += 1
                            logger.error(f"[{completed_count}/{len(model_list)}] ✗ Failed: {Path(model_file).name}")
                            
                except Exception as e:
                    with self.lock:
                        completed_count += 1
                        failed += 1
                        logger.error(f"[{completed_count}/{len(model_list)}] ✗ Exception for {Path(model_file).name}: {str(e)}")
                        results.append({
                            'model': Path(model_file).name,
                            'sim_output': None,
                            'success': False,
                            'error': str(e),
                            'time': 0
                        })
        
        total_time = time.time() - start_batch_time
        
        # Summary
        logger.info("\n" + "=" * 80)
        logger.info("PARALLEL BATCH PROCESSING COMPLETE")
        logger.info(f"Total: {len(model_list)}")
        logger.info(f"Successful: {successful}")
        logger.info(f"Failed: {failed}")
        logger.info(f"Success Rate: {successful/len(model_list)*100:.1f}%")
        logger.info(f"Total time: {total_time:.2f} seconds")
        logger.info(f"Average time per model: {total_time/len(model_list):.2f} seconds")
        logger.info("=" * 80)
        
        return {
            'total': len(model_list),
            'successful': successful,
            'failed': failed,
            'results': results,
            'total_time': total_time
        }


def find_model_files(directory: Path = Path("."), 
                     pattern: str = "*.yml",
                     exclude_patterns: Optional[List[str]] = None) -> List[Path]:
    """
    Find all OrcaFlex model files in directory.
    
    Args:
        directory: Directory to search
        pattern: Glob pattern for finding files (default: "*.yml")
        exclude_patterns: List of patterns to exclude from results
    
    Returns:
        list: List of model file paths
    """
    directory = Path(directory)
    model_files = []
    
    if exclude_patterns is None:
        exclude_patterns = ["includefile", "_output", "_backup", "_old"]
    
    # Find all files matching the pattern
    for model_file in directory.glob(pattern):
        # Skip files matching exclude patterns
        skip = False
        for exclude in exclude_patterns:
            if exclude.lower() in str(model_file).lower():
                skip = True
                break
        
        if not skip and model_file.suffix == ".yml":
            model_files.append(model_file)
    
    return sorted(model_files)


def run_models(models: Optional[List[str]] = None,
               directory: Optional[str] = None,
               pattern: str = "*.yml",
               output_dir: Optional[str] = None,
               mock: bool = False,
               threads: int = 30,
               all_models: bool = False) -> Dict[str, Any]:
    """
    Main function to run OrcaFlex models to generate .sim files.
    
    Args:
        models: List of specific model files to run
        directory: Directory to search for models (default: current directory)
        pattern: Glob pattern for finding model files
        output_dir: Output directory for .sim files
        mock: Run in mock mode without OrcaFlex license
        threads: Number of parallel threads
        all_models: Run all models found in directory
    
    Returns:
        dict: Summary of processing results
    """
    # Setup logging if not already configured
    if not logger.handlers:
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
    
    # Determine working directory
    work_dir = Path(directory) if directory else Path.cwd()
    
    # Determine which models to run
    if models:
        # Run specific models
        model_list = [Path(m) if Path(m).is_absolute() else work_dir / m for m in models]
        logger.info(f"Running specified models: {len(model_list)} files")
    elif all_models:
        # Find and run all models
        model_list = find_model_files(work_dir, pattern)
        logger.info(f"Found {len(model_list)} model files in {work_dir}")
        for model in model_list[:5]:  # Show first 5
            logger.info(f"  - {model.name}")
        if len(model_list) > 5:
            logger.info(f"  ... and {len(model_list) - 5} more")
    else:
        # Run a test subset
        all_found = find_model_files(work_dir, pattern)
        model_list = all_found[:3]  # Just run first 3 as test
        logger.info(f"Running test subset: {len(model_list)} models from {work_dir}")
        for model in model_list:
            logger.info(f"  - {model.name}")
    
    if not model_list:
        logger.error("No models to run!")
        return {'total': 0, 'successful': 0, 'failed': 0, 'results': []}
    
    # Create runner and process batch
    runner = OrcaFlexModelRunner(mock_mode=mock)
    output_path = Path(output_dir) if output_dir else None
    
    results = runner.run_batch(model_list, output_path, max_workers=threads)
    
    # List created .sim files
    if results['successful'] > 0:
        logger.info("\nCreated .sim files:")
        for result in results['results']:
            if result['success']:
                logger.info(f"  ✓ {Path(result['sim_output']).name}")
    
    return results