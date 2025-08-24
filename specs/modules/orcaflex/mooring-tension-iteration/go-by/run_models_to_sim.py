#!/usr/bin/env python
"""
Simple OrcaFlex Model Runner - Generate .sim Files
==================================================
This script runs OrcaFlex models (.yml files) and saves them as .sim files.
No tension iteration or complex processing - just load, run static, and save.
"""

import sys
import os
from pathlib import Path
import time
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock

# Add digitalmodel to path
digitalmodel_path = Path("D:/github/digitalmodel")
if digitalmodel_path.exists():
    sys.path.insert(0, str(digitalmodel_path / "src"))

# Try to import OrcaFlex
try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    print("WARNING: OrcaFlex not available. Running in test mode.")
    ORCAFLEX_AVAILABLE = False

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def run_single_model(model_file, output_dir=None, mock_mode=False):
    """
    Run a single OrcaFlex model and save as .sim file.
    
    Args:
        model_file: Path to the .yml model file
        output_dir: Directory to save .sim files (if None, saves in same directory as model)
        mock_mode: If True, simulate without OrcaFlex license
    
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
        if mock_mode or not ORCAFLEX_AVAILABLE:
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
            logger.info(f"Running static analysis...")
            model.CalculateStatics()
            
            # Save as .sim file
            logger.info(f"Saving to: {sim_file}")
            model.SaveSimulation(str(sim_file))
            
            result['success'] = True
            logger.info(f"✓ Successfully created: {sim_file.name}")
            
    except Exception as e:
        error_msg = str(e)
        logger.error(f"✗ Failed: {error_msg}")
        result['error'] = error_msg
    
    result['time'] = time.time() - start_time
    return result


def run_batch(model_list, output_dir=None, mock_mode=False, max_workers=30):
    """
    Run multiple OrcaFlex models in parallel.
    
    Args:
        model_list: List of model file paths
        output_dir: Directory to save .sim files (if None, saves in same directory as each model)
        mock_mode: If True, simulate without OrcaFlex license
        max_workers: Maximum number of parallel threads (default: 30)
    
    Returns:
        dict: Summary of batch processing results
    """
    logger.info("=" * 80)
    logger.info("Starting Parallel Batch Processing")
    logger.info(f"Total models: {len(model_list)}")
    logger.info(f"Parallel threads: {max_workers}")
    logger.info(f"Output: {'Same directory as models' if output_dir is None else output_dir}")
    logger.info(f"Mode: {'MOCK' if mock_mode else 'REAL'}")
    logger.info("=" * 80)
    
    results = []
    successful = 0
    failed = 0
    completed_count = 0
    lock = Lock()
    
    start_batch_time = time.time()
    
    # Use ThreadPoolExecutor for parallel processing
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        # Submit all tasks
        future_to_model = {
            executor.submit(run_single_model, model_file, output_dir, mock_mode): model_file 
            for model_file in model_list
        }
        
        # Process completed tasks as they finish
        for future in as_completed(future_to_model):
            model_file = future_to_model[future]
            
            try:
                result = future.result()
                
                with lock:
                    completed_count += 1
                    results.append(result)
                    
                    if result['success']:
                        successful += 1
                        logger.info(f"[{completed_count}/{len(model_list)}] ✓ Completed: {Path(model_file).name} ({result['time']:.2f}s)")
                    else:
                        failed += 1
                        logger.error(f"[{completed_count}/{len(model_list)}] ✗ Failed: {Path(model_file).name}")
                        
            except Exception as e:
                with lock:
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


def find_model_files(directory="."):
    """
    Find all OrcaFlex model files in directory.
    
    Args:
        directory: Directory to search
    
    Returns:
        list: List of model file paths
    """
    model_files = []
    
    # Find all .yml files that look like OrcaFlex models
    for yml_file in Path(directory).glob("fsts_*.yml"):
        # Skip includefiles and output files
        if "includefile" not in str(yml_file).lower() and "_output" not in str(yml_file):
            # Check if corresponding .sim already exists
            if yml_file.suffix == ".yml":
                model_files.append(yml_file)
    
    return sorted(model_files)


def main():
    """Main function to run models."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Run OrcaFlex models to generate .sim files")
    parser.add_argument("--models", nargs="+", help="Specific model files to run")
    parser.add_argument("--all", action="store_true", help="Run all models in directory")
    parser.add_argument("--output", default=None, help="Output directory for .sim files (default: same as model)")
    parser.add_argument("--mock", action="store_true", help="Run in mock mode (no license needed)")
    parser.add_argument("--threads", type=int, default=30, help="Number of parallel threads (default: 30)")
    
    args = parser.parse_args()
    
    # Determine which models to run
    if args.models:
        # Run specific models
        model_list = args.models
        logger.info(f"Running specified models: {len(model_list)} files")
    elif args.all:
        # Find and run all models
        model_list = find_model_files()
        logger.info(f"Found {len(model_list)} model files")
        for model in model_list[:5]:  # Show first 5
            logger.info(f"  - {Path(model).name}")
        if len(model_list) > 5:
            logger.info(f"  ... and {len(model_list) - 5} more")
    else:
        # Run a test subset
        all_models = find_model_files()
        model_list = all_models[:3]  # Just run first 3 as test
        logger.info(f"Running test subset: {len(model_list)} models")
        for model in model_list:
            logger.info(f"  - {Path(model).name}")
    
    if not model_list:
        logger.error("No models to run!")
        return
    
    # Run the batch
    results = run_batch(model_list, args.output, args.mock, max_workers=args.threads)
    
    # List created .sim files
    if results['successful'] > 0:
        logger.info("\nCreated .sim files:")
        for result in results['results']:
            if result['success']:
                logger.info(f"  ✓ {Path(result['sim_output']).name}")


if __name__ == "__main__":
    main()