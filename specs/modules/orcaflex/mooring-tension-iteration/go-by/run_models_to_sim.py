#!/usr/bin/env python
"""
Simple OrcaFlex Model Runner - Generate .sim Files
==================================================
This script runs OrcaFlex models (.yml and .dat files) and saves them as .sim files.
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
        model_file: Path to the .yml or .dat model file
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


def find_model_files(directory=".", include_dat=False, dat_only=False, pattern="fsts_*"):
    """
    Find all OrcaFlex model files in directory.
    
    Args:
        directory: Directory to search
        include_dat: If True, also include .dat files (default: False for backwards compatibility)
        dat_only: If True, only include .dat files, no .yml files
        pattern: File pattern to match (default: "fsts_*")
    
    Returns:
        list: List of model file paths
    """
    model_files = []
    directory = Path(directory)
    
    # Find .yml files unless dat_only is True
    if not dat_only:
        for yml_file in directory.glob(f"{pattern}.yml"):
            # Skip includefiles and output files
            if "includefile" not in str(yml_file).lower() and "_output" not in str(yml_file):
                model_files.append(yml_file)
    
    # Find .dat files if include_dat or dat_only is True
    if include_dat or dat_only:
        for dat_file in directory.glob(f"{pattern}.dat"):
            # Skip includefiles and temporary files
            if "includefile" not in str(dat_file).lower() and "_temp" not in str(dat_file).lower():
                model_files.append(dat_file)
    
    return sorted(model_files)


def main():
    """Main function to run models."""
    import argparse
    import sys
    
    # Support both keyword arguments and traditional flags
    # Convert keyword arguments to flags for argparse
    converted_args = []
    for arg in sys.argv[1:]:
        if '=' in arg and not arg.startswith('-'):
            # Convert keyword argument to flag format
            key, value = arg.split('=', 1)
            # Handle boolean values
            if value.lower() in ['true', '1', 'yes']:
                converted_args.append(f'--{key.replace("_", "-")}')
            elif value.lower() in ['false', '0', 'no']:
                # Don't add the flag for false boolean values
                pass
            else:
                # For non-boolean values, add both flag and value
                converted_args.append(f'--{key.replace("_", "-")}')
                converted_args.append(value)
        else:
            # Keep original argument
            converted_args.append(arg)
    
    # Replace sys.argv for argparse
    sys.argv = [sys.argv[0]] + converted_args
    
    parser = argparse.ArgumentParser(description="Run OrcaFlex models to generate .sim files")
    parser.add_argument("--models", nargs="+", help="Specific model files to run")
    parser.add_argument("--all", action="store_true", help="[DEPRECATED - use pattern instead] Run all models in directory")
    parser.add_argument("--include-dat", action="store_true", help="Include .dat files in addition to .yml files")
    parser.add_argument("--dat", action="store_true", help="Process only .dat files (no .yml files)")
    parser.add_argument("--pattern", default="*", help="File pattern to match (default: * = all files)")
    parser.add_argument("--output", default=None, help="Output directory for .sim files (default: same as model)")
    parser.add_argument("--mock", action="store_true", help="Run in mock mode (no license needed)")
    parser.add_argument("--threads", type=int, default=30, help="Number of parallel threads (default: 30)")
    
    args = parser.parse_args()
    
    # Determine which models to run
    if args.models:
        # Run specific models
        model_list = [Path(m) for m in args.models]
        logger.info(f"Running specified models: {len(model_list)} files")
    else:
        # Find models based on pattern (default: all files with pattern="*")
        # If --all is used for backwards compatibility, treat it as pattern="*"
        if args.all and args.pattern == "*":
            logger.info("[INFO] --all flag is deprecated. Using default pattern='*' instead")
        
        model_list = find_model_files(
            directory=".", 
            include_dat=args.include_dat,
            dat_only=args.dat,
            pattern=args.pattern
        )
        
        # Log what types of files we're looking for
        if args.dat:
            file_types = [".dat"]
        elif args.include_dat:
            file_types = [".yml", ".dat"]
        else:
            file_types = [".yml"]
        
        # Show search criteria
        pattern_desc = "all files" if args.pattern == "*" else f"pattern: {args.pattern}"
        logger.info(f"Searching for {' and '.join(file_types)} files ({pattern_desc})")
        
        logger.info(f"Found {len(model_list)} model files")
        for model in model_list[:5]:  # Show first 5
            logger.info(f"  - {Path(model).name}")
        if len(model_list) > 5:
            logger.info(f"  ... and {len(model_list) - 5} more")
    
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