"""
CLI entry point for OrcaFlex mooring comparative analysis.

This module provides command-line interface for running comparative analysis
across multiple mooring configurations, generating reports and visualizations.

Example:
    # Basic usage with default settings
    python -m digitalmodel.solvers.orcaflex.analysis --directory ./output/.csv
    
    # Generate report at specific location
    python -m digitalmodel.solvers.orcaflex.analysis \
        --directory ./output/.csv \
        --report ./output/report/analysis.md
    
    # With custom visualizations output
    python -m digitalmodel.solvers.orcaflex.analysis \
        --directory ./output/.csv \
        --visualizations ./output/visual \
        --report ./output/report/analysis.md
    
    # Using configuration file
    python -m digitalmodel.solvers.orcaflex.analysis \
        --config analysis_config.yml
"""

import argparse
import sys
import yaml
from pathlib import Path
from typing import Optional, Dict, Any
from .comparative import MooringComparativeAnalysis


def load_config(config_path: Path) -> Dict[str, Any]:
    """Load configuration from YAML file.
    
    Args:
        config_path: Path to configuration file
        
    Returns:
        Configuration dictionary
    """
    with open(config_path, 'r') as f:
        config = yaml.safe_load(f)
    
    # Resolve relative paths based on config file location
    config_dir = config_path.parent
    
    # Handle file_management block (compatible with OrcaFlex mooring config format)
    if 'file_management' in config:
        fm = config['file_management']
        if 'input_directory' in fm and fm['input_directory']:
            # Convert relative path to absolute based on config location
            input_path = Path(fm['input_directory'])
            if not input_path.is_absolute():
                config['input_directory'] = str(config_dir / input_path)
            else:
                config['input_directory'] = str(input_path)
        
        if 'output_directory' in fm and fm['output_directory']:
            # Convert relative path to absolute based on config location
            output_path = Path(fm['output_directory'])
            if not output_path.is_absolute():
                config['output_directory'] = str(config_dir / output_path)
            else:
                config['output_directory'] = str(output_path)
    
    # Also support direct input_directory/output_directory keys
    if 'input_directory' in config and config['input_directory']:
        input_path = Path(config['input_directory'])
        if not input_path.is_absolute():
            config['input_directory'] = str(config_dir / input_path)
        else:
            config['input_directory'] = str(input_path)
    
    if 'output_directory' in config and config['output_directory']:
        output_path = Path(config['output_directory'])
        if not output_path.is_absolute():
            config['output_directory'] = str(config_dir / output_path)
        else:
            config['output_directory'] = str(output_path)
    
    # Handle output paths for report and visualizations
    if 'output' in config:
        if 'report' in config['output'] and config['output']['report']:
            report_path = Path(config['output']['report'])
            if not report_path.is_absolute():
                config['output']['report'] = str(config_dir / report_path)
            else:
                config['output']['report'] = str(report_path)
        if 'visualizations' in config['output'] and config['output']['visualizations']:
            vis_path = Path(config['output']['visualizations'])
            if not vis_path.is_absolute():
                config['output']['visualizations'] = str(config_dir / vis_path)
            else:
                config['output']['visualizations'] = str(vis_path)
    
    return config


def main(argv: Optional[list] = None) -> int:
    """Main entry point for comparative analysis CLI.
    
    Args:
        argv: Command line arguments (for testing)
        
    Returns:
        Exit code (0 for success)
    """
    parser = argparse.ArgumentParser(
        description='OrcaFlex Mooring Comparative Analysis Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    
    # Input/output arguments
    parser.add_argument(
        '--input-directory', '--directory', '-d',
        type=str,
        dest='input_directory',
        help='Directory containing CSV analysis files'
    )
    
    parser.add_argument(
        '--output-directory', '--output', '-o',
        type=str,
        dest='output_directory',
        help='Base output directory for report and visualizations'
    )
    
    parser.add_argument(
        '--report', '--output-report', '-r',
        type=str,
        help='Output path for markdown report (default: {output_directory}/report/analysis_report.md)'
    )
    
    parser.add_argument(
        '--visualizations', '--output-visual', '-v',
        type=str,
        help='Output directory for visualizations (default: {output_directory}/comparative_analysis)'
    )
    
    # Configuration file
    parser.add_argument(
        '--config', '-c',
        type=str,
        help='Configuration file path (YAML format)'
    )
    
    # Analysis options
    parser.add_argument(
        '--pattern',
        type=str,
        default='*.csv',
        help='File pattern for CSV files (default: *.csv)'
    )
    
    parser.add_argument(
        '--no-visualizations',
        action='store_true',
        help='Skip visualization generation'
    )
    
    parser.add_argument(
        '--no-report',
        action='store_true',
        help='Skip report generation'
    )
    
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose output'
    )
    
    args = parser.parse_args(argv)
    
    # Load configuration if provided
    if args.config:
        config_path = Path(args.config)
        if not config_path.exists():
            print(f"Error: Configuration file not found: {args.config}")
            return 1
        
        config = load_config(config_path)
        
        # Override with command line arguments if provided
        if not args.input_directory and 'input_directory' in config:
            args.input_directory = config['input_directory']
        if not args.output_directory and 'output_directory' in config:
            args.output_directory = config['output_directory']
        if not args.report and 'output' in config and 'report' in config['output']:
            args.report = config['output']['report']
        if not args.visualizations and 'output' in config and 'visualizations' in config['output']:
            args.visualizations = config['output']['visualizations']
        if 'pattern' in config:
            args.pattern = config.get('pattern', args.pattern)
    
    # Validate required arguments
    if not args.input_directory:
        parser.error("--input-directory is required (or use --config with input_directory)")
    
    csv_directory = Path(args.input_directory)
    if not csv_directory.exists():
        print(f"Error: Input directory not found: {csv_directory}")
        return 1
    
    # Determine output directory
    if args.output_directory:
        output_directory = Path(args.output_directory)
    else:
        # Default to parent of input directory
        output_directory = csv_directory.parent
    
    # Set default output paths if not specified
    if not args.report and not args.no_report:
        args.report = output_directory / 'report' / 'analysis_report.md'
    
    if not args.visualizations and not args.no_visualizations:
        args.visualizations = output_directory / 'comparative_analysis'
    
    try:
        # Initialize analyzer
        if args.verbose:
            print(f"Loading CSV files from: {csv_directory}")
            print(f"Pattern: {args.pattern}")
        
        analyzer = MooringComparativeAnalysis(str(csv_directory))
        
        # Perform analysis
        if args.verbose:
            print("\nPerforming comparative analysis...")
        
        # Analyze pretension
        pretension_df = analyzer.analyze_pretension()
        if pretension_df is not None and args.verbose:
            print(f"  - Analyzed pretension for {len(pretension_df)} configurations")
        
        # Analyze stiffness
        stiffness_df = analyzer.analyze_stiffness()
        if stiffness_df is not None and args.verbose:
            print(f"  - Analyzed stiffness for {len(stiffness_df)} configurations")
        
        # Analyze line stiffness distribution
        line_stiffness_df = analyzer.analyze_line_stiffness_distribution()
        if line_stiffness_df is not None and args.verbose:
            print(f"  - Analyzed line stiffness distribution")
        
        # Analyze fender forces
        fender_df = analyzer.analyze_fender_forces()
        if fender_df is not None and args.verbose:
            print(f"  - Analyzed fender forces for {len(fender_df)} configurations")
        
        # Generate visualizations
        if not args.no_visualizations:
            if args.verbose:
                print(f"\nGenerating visualizations in: {args.visualizations}")
            
            vis_dir = analyzer.create_visualizations(args.visualizations)
            
            if args.verbose:
                print(f"  - Visualizations saved to: {vis_dir}")
        
        # Generate report
        if not args.no_report:
            if args.verbose:
                print(f"\nGenerating report: {args.report}")
            
            report_content = analyzer.generate_report(args.report)
            
            if args.verbose:
                print(f"  - Report saved to: {args.report}")
                print(f"  - Report size: {len(report_content)} characters")
        
        if args.verbose:
            print("\nAnalysis completed successfully!")
        
        return 0
        
    except Exception as e:
        print(f"Error during analysis: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())