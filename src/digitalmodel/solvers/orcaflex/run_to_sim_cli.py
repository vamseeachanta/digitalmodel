#!/usr/bin/env python
"""
Command-line interface for OrcaFlex Model Runner.
This provides a CLI wrapper around the run_to_sim module functionality.
"""

import argparse
import sys
import logging
from pathlib import Path
from typing import Optional

from digitalmodel.solvers.orcaflex.run_to_sim import run_models, ORCAFLEX_AVAILABLE


def setup_logging(verbose: bool = False):
    """Setup logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )


def main():
    """Main CLI entry point for run-to-sim command."""
    parser = argparse.ArgumentParser(
        prog='run-to-sim',
        description='Run OrcaFlex models to generate .sim files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run all models in current directory
  run-to-sim --all
  
  # Run specific models
  run-to-sim --models model1.yml model2.yml
  
  # Run models with custom pattern in specific directory
  run-to-sim --directory /path/to/models --pattern "fsts_*.yml" --all
  
  # Run with custom output directory and thread count
  run-to-sim --all --output ./sim_files --threads 10
  
  # Run in mock mode (no OrcaFlex license required)
  run-to-sim --all --mock
        """
    )
    
    # Model selection arguments
    model_group = parser.add_argument_group('Model Selection')
    model_group.add_argument(
        '--models', 
        nargs='+', 
        help='Specific model files to run'
    )
    model_group.add_argument(
        '--all', 
        action='store_true',
        help='Run all models found in directory'
    )
    model_group.add_argument(
        '--pattern',
        default='*.yml',
        help='Glob pattern for finding model files (default: *.yml)'
    )
    model_group.add_argument(
        '--directory',
        help='Directory to search for models (default: current directory)'
    )
    
    # Processing options
    process_group = parser.add_argument_group('Processing Options')
    process_group.add_argument(
        '--output',
        help='Output directory for .sim files (default: same as model)'
    )
    process_group.add_argument(
        '--threads',
        type=int,
        default=30,
        help='Number of parallel threads (default: 30)'
    )
    process_group.add_argument(
        '--mock',
        action='store_true',
        help='Run in mock mode (no OrcaFlex license needed)'
    )
    
    # General options
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )
    parser.add_argument(
        '--version',
        action='version',
        version='%(prog)s 1.0.0'
    )
    
    args = parser.parse_args()
    
    # Setup logging
    setup_logging(args.verbose)
    logger = logging.getLogger(__name__)
    
    # Check OrcaFlex availability
    if not args.mock and not ORCAFLEX_AVAILABLE:
        logger.warning("OrcaFlex API not available. Running in mock mode.")
        args.mock = True
    
    # Validate arguments
    if not args.models and not args.all:
        logger.info("No models specified. Running test subset (first 3 models).")
        logger.info("Use --all to run all models or --models to specify specific files.")
    
    # Run the models
    try:
        results = run_models(
            models=args.models,
            directory=args.directory,
            pattern=args.pattern,
            output_dir=args.output,
            mock=args.mock,
            threads=args.threads,
            all_models=args.all
        )
        
        # Exit with appropriate code
        if results['failed'] > 0:
            sys.exit(1)
        else:
            sys.exit(0)
            
    except KeyboardInterrupt:
        logger.info("\nProcess interrupted by user")
        sys.exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}", exc_info=args.verbose)
        sys.exit(1)


if __name__ == '__main__':
    main()