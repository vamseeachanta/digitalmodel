#!/usr/bin/env python
"""
Test script for running OrcaFlex analyses using configuration files.
Demonstrates how to use the standardized YAML input files.
"""

import os
import yaml
import logging
from pathlib import Path
from typing import Dict, Any, List

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


def load_config(config_path: str) -> Dict[str, Any]:
    """Load and validate YAML configuration file."""
    try:
        with open(config_path, 'r') as f:
            config = yaml.safe_load(f)
        
        # Validate required sections
        required_sections = ['meta', 'default', 'orcaflex', 'file_management']
        for section in required_sections:
            if section not in config:
                raise ValueError(f"Missing required section: {section}")
        
        # Validate meta section
        if config['meta'].get('library') != 'digitalmodel':
            raise ValueError("Invalid library in meta section")
        
        logger.info(f"Successfully loaded config: {config['meta'].get('description')}")
        return config
    
    except Exception as e:
        logger.error(f"Failed to load configuration: {e}")
        raise


def get_input_files(config: Dict[str, Any]) -> List[str]:
    """Extract list of input files from configuration."""
    files = []
    file_config = config.get('file_management', {})
    
    if not file_config.get('flag'):
        return files
    
    input_dir = Path(file_config.get('input_directory', '.'))
    
    # Check for specific files
    input_files = file_config.get('input_files', {})
    if 'specific' in input_files:
        for file_type, file_list in input_files['specific'].items():
            for filename in file_list:
                files.append(input_dir / filename)
    elif input_files:
        # Direct file specification
        for file_type, file_list in input_files.items():
            if isinstance(file_list, list):
                for filename in file_list:
                    files.append(input_dir / filename)
    
    # Check for pattern-based selection
    filename_config = file_config.get('filename', {})
    if filename_config:
        extensions = filename_config.get('extension', [])
        pattern = filename_config.get('pattern', '*')
        filters = filename_config.get('filters', {})
        
        for ext in extensions:
            for file_path in input_dir.glob(f"{pattern}.{ext}"):
                # Apply filters
                filename = file_path.name
                
                # Check contains filters
                contains = filters.get('contains', [])
                if contains and not any(c in filename for c in contains):
                    continue
                
                # Check not_contains filters
                not_contains = filters.get('not_contains', [])
                if not_contains and any(nc in filename for nc in not_contains):
                    continue
                
                files.append(file_path)
    
    return files


def run_orcaflex_analysis(config: Dict[str, Any], input_file: Path) -> bool:
    """
    Run OrcaFlex analysis for a single input file.
    
    This is a mock implementation. In production, this would:
    1. Import OrcFxAPI
    2. Load the model
    3. Run static/dynamic analysis based on config
    4. Extract results
    5. Save outputs
    """
    logger.info(f"Processing: {input_file.name}")
    
    analysis_config = config.get('orcaflex', {}).get('analysis', {})
    
    # Check file type and existence
    if not input_file.exists():
        logger.warning(f"File not found: {input_file}")
        return False
    
    file_ext = input_file.suffix.lower()
    
    # Mock analysis steps
    if analysis_config.get('static'):
        logger.info(f"  Running static analysis on {file_ext} file...")
        # In production: model.CalculateStatics()
    
    if analysis_config.get('dynamic'):
        duration = analysis_config.get('simulation_duration', 3600)
        logger.info(f"  Running dynamic analysis for {duration}s...")
        # In production: model.RunSimulation()
    
    # Mock post-processing
    postprocess_config = config.get('orcaflex', {}).get('postprocess', {})
    
    if postprocess_config.get('summary', {}).get('flag'):
        logger.info("  Generating summary statistics...")
    
    if postprocess_config.get('visualization', {}).get('flag'):
        logger.info("  Creating visualizations...")
    
    if postprocess_config.get('time_series', {}).get('flag'):
        logger.info("  Extracting time series data...")
    
    logger.info(f"  ✓ Completed: {input_file.name}")
    return True


def run_batch_analysis(config_path: str):
    """Run batch analysis using configuration file."""
    logger.info("="*60)
    logger.info("OrcaFlex Batch Analysis Runner")
    logger.info("="*60)
    
    # Load configuration
    config = load_config(config_path)
    
    # Get input files
    input_files = get_input_files(config)
    
    if not input_files:
        logger.warning("No input files found matching criteria")
        return
    
    logger.info(f"Found {len(input_files)} files to process:")
    for f in input_files:
        logger.info(f"  - {f.name}")
    
    # Process files
    batch_config = config.get('file_management', {}).get('batch_processing', {})
    continue_on_error = batch_config.get('continue_on_error', True)
    
    successful = 0
    failed = 0
    
    logger.info("\nStarting analysis...")
    logger.info("-"*40)
    
    for input_file in input_files:
        try:
            if run_orcaflex_analysis(config, input_file):
                successful += 1
            else:
                failed += 1
                if not continue_on_error:
                    break
        except Exception as e:
            logger.error(f"Error processing {input_file.name}: {e}")
            failed += 1
            if not continue_on_error:
                break
    
    # Summary
    logger.info("-"*40)
    logger.info("Batch Analysis Complete!")
    logger.info(f"  Successful: {successful}")
    logger.info(f"  Failed: {failed}")
    logger.info(f"  Total: {successful + failed}")
    logger.info("="*60)


def test_configurations():
    """Test all configuration files."""
    config_files = [
        "run_orcaflex_analysis.yml",
        "run_orcaflex_simple.yml",
        "run_orcaflex_batch.yml"
    ]
    
    for config_file in config_files:
        config_path = Path(__file__).parent / config_file
        if config_path.exists():
            logger.info(f"\nTesting configuration: {config_file}")
            logger.info("-"*40)
            try:
                config = load_config(str(config_path))
                logger.info(f"✓ Valid configuration: {config['meta'].get('description')}")
                
                # Show what would be processed
                files = get_input_files(config)
                logger.info(f"  Would process {len(files)} files")
                
            except Exception as e:
                logger.error(f"✗ Invalid configuration: {e}")
        else:
            logger.warning(f"Configuration file not found: {config_file}")


if __name__ == "__main__":
    # Test configurations
    test_configurations()
    
    # Example: Run simple analysis
    simple_config = Path(__file__).parent / "run_orcaflex_simple.yml"
    if simple_config.exists():
        logger.info("\n" + "="*60)
        logger.info("Running Simple Analysis Example")
        run_batch_analysis(str(simple_config))
    
    # Example: Run batch analysis
    # batch_config = Path(__file__).parent / "run_orcaflex_batch.yml"
    # if batch_config.exists():
    #     run_batch_analysis(str(batch_config))