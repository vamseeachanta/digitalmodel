#!/usr/bin/env python
"""
Command-line interface for OrcaFlex Time Series Analysis

Usage:
    python -m digitalmodel.signal_processing.signal_analysis.orcaflex [options]
    
Examples:
    # Single file analysis
    python -m digitalmodel.signal_processing.signal_analysis.orcaflex --file data.csv
    
    # Pattern-based processing
    python -m digitalmodel.signal_processing.signal_analysis.orcaflex --pattern "*.csv" --directory ./data
    
    # Batch processing with parallel execution
    python -m digitalmodel.signal_processing.signal_analysis.orcaflex --directory ./data --recursive --parallel 4
"""

import sys
import argparse
import logging
from pathlib import Path
from typing import Optional
import json

from .config import ConfigurationManager
from .analyzer import TimeSeriesAnalyzer
from .batch import BatchProcessor
from .reader import GenericTimeSeriesReader


def setup_logging(level: str = "INFO", log_file: Optional[str] = None):
    """Setup logging configuration"""
    log_level = getattr(logging, level.upper(), logging.INFO)
    
    # Configure format
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    
    # Configure handlers
    handlers = [logging.StreamHandler()]
    if log_file:
        handlers.append(logging.FileHandler(log_file))
    
    logging.basicConfig(
        level=log_level,
        format=log_format,
        handlers=handlers
    )
    
    # Set specific loggers
    logging.getLogger('matplotlib').setLevel(logging.WARNING)
    logging.getLogger('PIL').setLevel(logging.WARNING)


def create_parser():
    """Create command-line argument parser"""
    parser = argparse.ArgumentParser(
        description='OrcaFlex Time Series Analysis - Rainflow and FFT Processing',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single file with auto-detection
  %(prog)s --file simulation.csv --auto-detect
  
  # Pattern matching with specific columns
  %(prog)s --pattern "fat*.csv" --input-directory ./data --time-column "Time (s)"
  
  # Batch processing with configuration
  %(prog)s --config analysis_config.yml --parallel 4
  
  # Directory processing with profile
  %(prog)s --input-directory ./orcaflex_output --output-directory ./results --profile orcaflex_standard
        """
    )
    
    # Input options
    input_group = parser.add_argument_group('Input Options')
    input_group.add_argument(
        '--file', '-f',
        type=str,
        help='Single file to process'
    )
    input_group.add_argument(
        '--pattern', '-p',
        type=str,
        help='File pattern (e.g., "*.csv", "fat*.csv")'
    )
    input_group.add_argument(
        '--input-directory', '--directory', '-d',
        type=str,
        dest='directory',
        help='Input directory to search for files'
    )
    input_group.add_argument(
        '--recursive', '-r',
        action='store_true',
        help='Search directory recursively'
    )
    input_group.add_argument(
        '--file-list',
        type=str,
        nargs='+',
        help='List of files to process'
    )
    
    # Column mapping options
    column_group = parser.add_argument_group('Column Mapping')
    column_group.add_argument(
        '--auto-detect',
        action='store_true',
        help='Automatically detect time and data columns'
    )
    column_group.add_argument(
        '--time-column',
        type=str,
        help='Name of time column'
    )
    column_group.add_argument(
        '--data-columns',
        type=str,
        nargs='+',
        help='Names of data columns to analyze'
    )
    column_group.add_argument(
        '--data-pattern',
        type=str,
        help='Pattern for data columns (e.g., "*Tension*")'
    )
    column_group.add_argument(
        '--profile',
        type=str,
        help='Column mapping profile (e.g., orcaflex_standard)'
    )
    
    # Analysis options
    analysis_group = parser.add_argument_group('Analysis Options')
    analysis_group.add_argument(
        '--no-rainflow',
        action='store_true',
        help='Skip rainflow analysis'
    )
    analysis_group.add_argument(
        '--no-fft',
        action='store_true',
        help='Skip FFT analysis'
    )
    analysis_group.add_argument(
        '--window-size',
        type=int,
        default=4096,
        help='FFT window size (default: 4096)'
    )
    analysis_group.add_argument(
        '--bin-count',
        type=int,
        default=50,
        help='Number of bins for rainflow histogram (default: 50)'
    )
    
    # Output options
    output_group = parser.add_argument_group('Output Options')
    output_group.add_argument(
        '--output-directory', '--output', '-o',
        type=str,
        dest='output',
        default='output/analysis',
        help='Output directory (default: output/analysis)'
    )
    output_group.add_argument(
        '--no-plots',
        action='store_true',
        help='Skip plot generation'
    )
    output_group.add_argument(
        '--formats',
        type=str,
        nargs='+',
        default=['csv', 'json'],
        choices=['csv', 'json', 'excel', 'mat'],
        help='Output formats (default: csv json)'
    )
    
    # Processing options
    processing_group = parser.add_argument_group('Processing Options')
    processing_group.add_argument(
        '--parallel',
        type=int,
        metavar='N',
        help='Number of parallel workers'
    )
    processing_group.add_argument(
        '--continue-on-error',
        action='store_true',
        default=True,
        help='Continue processing if a file fails'
    )
    processing_group.add_argument(
        '--retry-failed',
        action='store_true',
        help='Retry failed files'
    )
    
    # Configuration
    config_group = parser.add_argument_group('Configuration')
    config_group.add_argument(
        '--config', '-c',
        type=str,
        help='Configuration file (YAML or JSON)'
    )
    config_group.add_argument(
        '--save-config',
        type=str,
        help='Save current configuration to file'
    )
    
    # Logging options
    logging_group = parser.add_argument_group('Logging')
    logging_group.add_argument(
        '--log-level',
        type=str,
        default='INFO',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        help='Logging level (default: INFO)'
    )
    logging_group.add_argument(
        '--log-file',
        type=str,
        help='Log file path'
    )
    
    # Other options
    parser.add_argument(
        '--version',
        action='version',
        version='%(prog)s 1.0.0'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be processed without actually processing'
    )
    parser.add_argument(
        '--list-profiles',
        action='store_true',
        help='List available column mapping profiles'
    )
    
    return parser


def build_config_from_args(args):
    """Build configuration dictionary from command-line arguments"""
    config = {}
    
    # Input configuration
    if args.file:
        config['input'] = {'mode': 'single', 'file_path': args.file}
    elif args.pattern:
        config['input'] = {
            'mode': 'pattern',
            'pattern': args.pattern,
            'directory': args.directory or '.',
            'recursive': args.recursive
        }
    elif args.directory:
        config['input'] = {
            'mode': 'directory',
            'directory': args.directory,
            'recursive': args.recursive
        }
    elif args.file_list:
        config['input'] = {
            'mode': 'list',
            'file_list': args.file_list
        }
    
    # Column mapping
    if args.time_column or args.data_columns:
        config['column_mapping'] = {
            'strategy': 'manual',
            'manual': {
                'time': args.time_column,
                'data_columns': args.data_columns or []
            }
        }
    elif args.auto_detect:
        config['column_mapping'] = {'strategy': 'auto'}
    elif args.profile:
        config['column_mapping'] = {
            'strategy': 'profile',
            'active_profile': args.profile
        }
    
    # Analysis configuration
    config['analysis'] = {
        'rainflow': {
            'enable': not args.no_rainflow,
            'bin_count': args.bin_count
        },
        'fft': {
            'enable': not args.no_fft,
            'window_size': args.window_size
        }
    }
    
    # Output configuration
    config['output'] = {
        'directory': args.output,
        'plots': {'enable': not args.no_plots},
        'formats': {fmt: True for fmt in args.formats}
    }
    
    # Batch configuration
    if args.parallel:
        config['batch'] = {
            'parallel': {
                'enable': True,
                'max_workers': args.parallel
            },
            'continue_on_error': args.continue_on_error,
            'retry_failed': args.retry_failed
        }
    
    return config


def main():
    """Main CLI entry point"""
    parser = create_parser()
    args = parser.parse_args()
    
    # Setup logging
    setup_logging(args.log_level, args.log_file)
    logger = logging.getLogger(__name__)
    
    # Handle special commands
    if args.list_profiles:
        config_mgr = ConfigurationManager()
        profiles = config_mgr.list_profiles()
        print("Available column mapping profiles:")
        for profile in profiles:
            print(f"  - {profile}")
        return 0
    
    # Load or build configuration
    if args.config:
        logger.info(f"Loading configuration from {args.config}")
        config_mgr = ConfigurationManager(args.config)
        config = config_mgr.to_dict()
    else:
        config = build_config_from_args(args)
        config_mgr = ConfigurationManager()
        config_mgr.config = config
    
    # Save configuration if requested
    if args.save_config:
        config_mgr.save_config(args.save_config)
        logger.info(f"Configuration saved to {args.save_config}")
    
    # Validate input
    if not any([args.file, args.pattern, args.directory, args.file_list, args.config]):
        parser.error("No input specified. Use --file, --pattern, --directory, or --config")
    
    # Dry run mode
    if args.dry_run:
        reader = GenericTimeSeriesReader(config)
        
        # Discover files
        if args.file:
            files = [Path(args.file)]
        elif args.file_list:
            files = [Path(f) for f in args.file_list]
        else:
            files = reader.discover_files(
                pattern=args.pattern,
                directory=args.directory,
                recursive=args.recursive
            )
        
        print(f"Would process {len(files)} files:")
        for f in files[:10]:  # Show first 10
            print(f"  - {f}")
        if len(files) > 10:
            print(f"  ... and {len(files) - 10} more")
        return 0
    
    # Create analyzer
    analyzer = TimeSeriesAnalyzer(
        config=config,
        auto_detect_columns=args.auto_detect,
        profile=args.profile
    )
    
    # Process files
    try:
        if args.file:
            # Single file processing
            logger.info(f"Processing single file: {args.file}")
            results = analyzer.process_file(
                args.file,
                output_dir=args.output
            )
            
            # Print summary
            print("\nAnalysis Complete:")
            for col_name, col_results in results.get('columns', {}).items():
                stats = col_results.get('statistics', {})
                rainflow = col_results.get('rainflow', {}).get('statistics', {})
                print(f"\n  {col_name}:")
                print(f"    Mean: {stats.get('mean', 0):.2f}")
                print(f"    Std: {stats.get('std', 0):.2f}")
                print(f"    Cycles: {rainflow.get('total_cycles', 0)}")
                
        elif args.pattern:
            # Pattern-based processing
            logger.info(f"Processing files matching pattern: {args.pattern}")
            results = analyzer.process_pattern(
                pattern=args.pattern,
                directory=args.directory or '.',
                output_dir=args.output,
                parallel=bool(args.parallel)
            )
            
            print(f"\nProcessed {len(results)} files successfully")
            
        elif args.directory:
            # Directory processing
            logger.info(f"Processing directory: {args.directory}")
            reader = GenericTimeSeriesReader(config)
            files = reader.discover_files(directory=args.directory, recursive=args.recursive)
            
            if args.parallel:
                batch_processor = BatchProcessor(analyzer, config)
                results = batch_processor.process_files(
                    files,
                    output_dir=args.output,
                    parallel=True
                )
            else:
                results = {}
                for file in files:
                    try:
                        results[str(file)] = analyzer.process_file(file, output_dir=args.output)
                    except Exception as e:
                        logger.error(f"Failed to process {file}: {e}")
                        if not args.continue_on_error:
                            raise
            
            print(f"\nProcessed {len(results)} files successfully")
            
        elif args.file_list:
            # File list processing
            logger.info(f"Processing {len(args.file_list)} files")
            
            if args.parallel:
                batch_processor = BatchProcessor(analyzer, config)
                results = batch_processor.process_files(
                    args.file_list,
                    output_dir=args.output,
                    parallel=True
                )
            else:
                results = analyzer.batch_process(
                    args.file_list,
                    output_dir=args.output,
                    parallel=False
                )
            
            print(f"\nProcessed {len(results)} files successfully")
        
        print(f"\nResults saved to: {args.output}")
        return 0
        
    except KeyboardInterrupt:
        logger.info("Processing interrupted by user")
        return 1
    except Exception as e:
        logger.error(f"Processing failed: {e}", exc_info=args.log_level == 'DEBUG')
        return 1


if __name__ == '__main__':
    sys.exit(main())