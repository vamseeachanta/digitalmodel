#!/usr/bin/env python
"""
Load Scaling Program Runner
============================
Command-line script to run load scaling analysis with specified configuration.

Usage:
    python run_load_scaling.py <config_file>
    python run_load_scaling.py input/load_scaling_config.yml
    python run_load_scaling.py --help
"""

import sys
import os
import argparse
from pathlib import Path

# Add src directory to path
project_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(project_root / 'src'))

from digitalmodel.fatigue_analysis.load_scaling import LoadScalingProcessor


def create_parser():
    """Create argument parser following repository conventions"""
    parser = argparse.ArgumentParser(
        description='Run load scaling analysis for fatigue assessment',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Run with default configuration
    %(prog)s input/load_scaling_config.yml
    
    # Run with custom configuration
    %(prog)s /path/to/custom_config.yml
    
    # Run with verbose output
    %(prog)s input/load_scaling_config.yml --verbose
    
    # Dry run to validate configuration
    %(prog)s input/load_scaling_config.yml --dry-run
        """
    )
    
    # Positional argument for configuration file
    parser.add_argument(
        'config_file',
        type=str,
        nargs='?',
        default='input/load_scaling_config.yml',
        help='Path to YAML configuration file (default: input/load_scaling_config.yml)'
    )
    
    # Optional arguments
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose output'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Validate configuration without processing'
    )
    
    parser.add_argument(
        '-o', '--output-dir',
        type=str,
        help='Override output directory from configuration'
    )
    
    return parser


def main(args=None):
    """Main entry point for load scaling analysis"""
    
    # Parse arguments
    parser = create_parser()
    args = parser.parse_args(args)
    
    # Resolve configuration file path
    config_file = Path(args.config_file)
    
    # If relative path, make it relative to script directory
    if not config_file.is_absolute():
        config_file = Path(__file__).parent / config_file
    
    # Validate configuration file exists
    if not config_file.exists():
        print(f"Error: Configuration file not found: {config_file}")
        print("\nAvailable configuration files:")
        input_dir = Path(__file__).parent / 'input'
        if input_dir.exists():
            for f in input_dir.glob('*.yml'):
                print(f"  - {f.relative_to(Path(__file__).parent)}")
        return 1
    
    # Print header
    print("="*60)
    print("LOAD SCALING ANALYSIS")
    print("="*60)
    print(f"Configuration: {config_file}")
    if args.verbose:
        print(f"Verbose mode: ENABLED")
    if args.dry_run:
        print(f"Mode: DRY RUN (validation only)")
    if args.output_dir:
        print(f"Output override: {args.output_dir}")
    print()
    
    try:
        # Initialize processor
        processor = LoadScalingProcessor(str(config_file))
        
        # Override output directory if specified
        if args.output_dir:
            processor.config['output']['base_folder'] = args.output_dir
            if args.verbose:
                print(f"Output directory overridden to: {args.output_dir}")
        
        # Dry run - validate only
        if args.dry_run:
            print("Validating configuration...")
            processor.load_input_data()
            print("[OK] Configuration valid")
            print("[OK] Input files accessible")
            print(f"[OK] Would process {len(processor.fatigue_seastates)} fatigue conditions")
            print(f"[OK] Would generate {len(processor.config['input_data']['vessel_configurations']['configs']) * len(processor.fatigue_seastates) * len(processor.config['input_data']['vessel_configurations']['struts'])} output files")
            return 0
        
        # Run the analysis
        processor.run()
        
        print()
        print("="*60)
        print("Analysis completed successfully!")
        print(f"  Output folder: {processor.config['output']['base_folder']}")
        print(f"  Cases processed: {len(processor.results)}")
        print("="*60)
        
        return 0
        
    except KeyboardInterrupt:
        print("\nAnalysis interrupted by user")
        return 130
        
    except Exception as e:
        print(f"Analysis failed: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())