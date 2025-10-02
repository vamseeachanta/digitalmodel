"""
Command-line interface for the Passing Ship Forces calculation module.

This module provides a CLI interface following repository standards for
calculating hydrodynamic forces between passing vessels.
"""

import argparse
import json
import csv
import sys
import os
from pathlib import Path
from typing import Dict, Any, Optional, List
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
from datetime import datetime

from .calculator import PassingShipCalculator
from .visualization import (
    plot_forces,
    create_parametric_study,
    create_comparison_plots
)


def create_parser() -> argparse.ArgumentParser:
    """
    Create argument parser with standard repository parameter naming.
    
    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(
        prog='passing-ship-forces',
        description='Passing Ship Forces Calculator - Calculate hydrodynamic interaction forces',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single calculation with config file
  %(prog)s --config vessel_config.yaml
  
  # Batch processing with parallel execution
  %(prog)s --batch --input-directory configs --parallel 4
  
  # Generate plots and export results
  %(prog)s --config config.yaml --plot --export-json results.json
  
  # Dry run to preview operations
  %(prog)s --batch --input-directory configs --dry-run
        """
    )
    
    # Input/Output arguments (standard repository naming)
    parser.add_argument(
        '--input-directory', '--directory', '-d',
        type=str,
        dest='input_directory',
        help='Input directory to search for configuration files'
    )
    
    parser.add_argument(
        '--output-directory', '--output', '-o',
        type=str,
        dest='output_directory',
        default='results',
        help='Output directory for results (default: results)'
    )
    
    # File selection
    parser.add_argument(
        '--pattern',
        type=str,
        default='*.yaml',
        help='File pattern for batch processing (default: *.yaml)'
    )
    
    parser.add_argument(
        '--recursive', '-r',
        action='store_true',
        help='Recursively search directories'
    )
    
    # Processing options
    parser.add_argument(
        '--config', '-c',
        type=str,
        help='Configuration file for single calculation'
    )
    
    parser.add_argument(
        '--batch', '-b',
        action='store_true',
        help='Enable batch processing mode'
    )
    
    parser.add_argument(
        '--parallel', '-p',
        type=int,
        default=1,
        choices=range(1, 101),  # Limit to 1-100
        metavar='N',
        help='Number of parallel workers (default: 1, max: 100)'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Preview operations without executing'
    )
    
    # Export options
    parser.add_argument(
        '--export-json',
        type=str,
        help='Export results to JSON file'
    )
    
    parser.add_argument(
        '--export-csv',
        type=str,
        help='Export results to CSV file'
    )
    
    # Visualization options
    parser.add_argument(
        '--plot',
        action='store_true',
        help='Generate force distribution plots'
    )
    
    parser.add_argument(
        '--plot-format',
        type=str,
        choices=['png', 'pdf', 'svg'],
        default='png',
        help='Plot output format (default: png)'
    )
    
    parser.add_argument(
        '--interactive',
        action='store_true',
        help='Show interactive plots'
    )
    
    # Calculation options
    parser.add_argument(
        '--stagger-range',
        type=str,
        help='Stagger distance range as "min,max,step" (e.g., "-500,500,50")'
    )
    
    parser.add_argument(
        '--cache',
        action='store_true',
        default=True,
        help='Enable result caching (default: enabled)'
    )
    
    parser.add_argument(
        '--no-cache',
        action='store_false',
        dest='cache',
        help='Disable result caching'
    )
    
    # Logging options
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose output'
    )
    
    parser.add_argument(
        '--quiet', '-q',
        action='store_true',
        help='Suppress non-error output'
    )
    
    # Validation
    def validate_args(args):
        """Validate argument combinations."""
        if args.parallel < 1:
            parser.error("--parallel must be >= 1")
        
        if args.batch and not args.input_directory:
            parser.error("--batch requires --input-directory")
        
        if not args.batch and not args.config:
            parser.error("Either --config or --batch mode required")
        
        return args
    
    parser.validate_args = validate_args
    
    return parser


def process_single(
    config_path: str,
    output_dir: Optional[str] = None,
    stagger_range: Optional[str] = None,
    cache: bool = True,
    verbose: bool = False,
    dry_run: bool = False
) -> Optional[Dict[str, Any]]:
    """
    Process a single configuration file.
    
    Args:
        config_path: Path to configuration file
        output_dir: Output directory for results
        stagger_range: Stagger distance range specification
        cache: Enable caching
        verbose: Enable verbose output
        dry_run: Preview without execution
        
    Returns:
        Calculation results or None if dry run
    """
    if dry_run:
        print(f"DRY RUN: Would process {config_path}")
        return None
    
    if verbose:
        print(f"Processing: {config_path}")
        print(f"  Output: {output_dir or 'console only'}")
        if stagger_range:
            print(f"  Stagger range: {stagger_range}")
    
    try:
        # Create calculator from config
        calculator = PassingShipCalculator.from_config(config_path)
        
        # Parse stagger range if provided
        if stagger_range:
            parts = stagger_range.split(',')
            if len(parts) != 3:
                raise ValueError("Stagger range must be 'min,max,step'")
            
            min_s, max_s, step = map(float, parts)
            stagger_distances = list(range(int(min_s), int(max_s) + 1, int(step)))
            
            # Calculate for multiple stagger distances
            results = []
            for stagger in stagger_distances:
                if verbose:
                    print(f"  Calculating for stagger distance: {stagger} m")
                
                result = calculator.calculate(stagger_distance=stagger)
                result['stagger_distance'] = stagger
                results.append(result)
            
            return {'multiple': True, 'results': results}
        else:
            # Single calculation
            result = calculator.calculate()
            
            if verbose:
                print(f"  Results: Surge={result['surge_force']:.1f} N, "
                      f"Sway={result['sway_force']:.1f} N, "
                      f"Yaw={result['yaw_moment']:.1f} N·m")
            
            return result
            
    except Exception as e:
        print(f"Error processing {config_path}: {e}", file=sys.stderr)
        if verbose:
            import traceback
            traceback.print_exc()
        return None


def process_batch(
    input_directory: str,
    pattern: str = '*.yaml',
    recursive: bool = False,
    parallel: int = 1,
    output_dir: Optional[str] = None,
    cache: bool = True,
    verbose: bool = False,
    dry_run: bool = False
) -> Dict[str, Any]:
    """
    Process multiple configuration files in batch mode.
    
    Args:
        input_directory: Directory containing config files
        pattern: File pattern to match
        recursive: Search recursively
        parallel: Number of parallel workers
        output_dir: Output directory for results
        cache: Enable caching
        verbose: Enable verbose output
        dry_run: Preview without execution
        
    Returns:
        Dictionary mapping config files to results
    """
    # Find configuration files
    input_path = Path(input_directory)
    
    if recursive:
        config_files = list(input_path.rglob(pattern))
    else:
        config_files = list(input_path.glob(pattern))
    
    if not config_files:
        print(f"No files matching '{pattern}' found in {input_directory}")
        return {}
    
    print(f"Found {len(config_files)} configuration file(s)")
    
    if dry_run:
        print("DRY RUN: Would process:")
        for cf in config_files:
            print(f"  - {cf}")
        return {}
    
    # Process files
    results = {}
    
    if parallel > 1:
        # Parallel processing
        print(f"Processing with {parallel} workers...")
        
        with ThreadPoolExecutor(max_workers=parallel) as executor:
            # Submit all tasks
            future_to_file = {
                executor.submit(
                    process_single,
                    str(cf),
                    output_dir,
                    None,
                    cache,
                    verbose,
                    dry_run
                ): cf
                for cf in config_files
            }
            
            # Collect results
            completed = 0
            for future in as_completed(future_to_file):
                config_file = future_to_file[future]
                completed += 1
                
                try:
                    result = future.result()
                    if result:
                        results[str(config_file)] = result
                        
                    if not verbose:
                        print(f"Progress: {completed}/{len(config_files)}", end='\r')
                        
                except Exception as e:
                    print(f"\nError processing {config_file}: {e}", file=sys.stderr)
    else:
        # Sequential processing
        for i, config_file in enumerate(config_files, 1):
            if not verbose:
                print(f"Progress: {i}/{len(config_files)}", end='\r')
            
            result = process_single(
                str(config_file),
                output_dir,
                None,
                cache,
                verbose,
                dry_run
            )
            
            if result:
                results[str(config_file)] = result
    
    print(f"\nCompleted: {len(results)} successful, "
          f"{len(config_files) - len(results)} failed")
    
    return results


def export_results(
    results: Dict[str, Any],
    format: str = 'json',
    file: Any = None,
    verbose: bool = False
) -> None:
    """
    Export results to specified format.
    
    Args:
        results: Calculation results
        format: Export format ('json' or 'csv')
        file: File object or path
        verbose: Enable verbose output
    """
    if format == 'json':
        # JSON export
        if isinstance(file, str):
            with open(file, 'w') as f:
                json.dump(results, f, indent=2, default=str)
            if verbose:
                print(f"Exported results to {file}")
        else:
            json.dump(results, file, indent=2, default=str)
    
    elif format == 'csv':
        # CSV export
        if isinstance(results, dict) and 'multiple' in results:
            # Multiple results (stagger range)
            rows = results['results']
        else:
            # Single result
            rows = [results] if isinstance(results, dict) else results
        
        if rows:
            if isinstance(file, str):
                with open(file, 'w', newline='') as f:
                    writer = csv.DictWriter(f, fieldnames=rows[0].keys())
                    writer.writeheader()
                    writer.writerows(rows)
                if verbose:
                    print(f"Exported results to {file}")
            else:
                writer = csv.DictWriter(file, fieldnames=rows[0].keys())
                writer.writeheader()
                writer.writerows(rows)


def main() -> int:
    """
    Main entry point for CLI.
    
    Returns:
        Exit code (0 for success, 1 for error)
    """
    # Parse arguments
    parser = create_parser()
    args = parser.parse_args()
    
    # Validate arguments
    try:
        args = parser.validate_args(args)
    except SystemExit:
        return 1
    
    # Set up output directory
    if args.output_directory:
        output_path = Path(args.output_directory)
        if not args.dry_run:
            output_path.mkdir(parents=True, exist_ok=True)
    
    try:
        # Process based on mode
        if args.batch:
            # Batch processing
            results = process_batch(
                args.input_directory,
                args.pattern,
                args.recursive,
                args.parallel,
                args.output_directory,
                args.cache,
                args.verbose,
                args.dry_run
            )
        else:
            # Single calculation
            results = process_single(
                args.config,
                args.output_directory,
                args.stagger_range if hasattr(args, 'stagger_range') else None,
                args.cache,
                args.verbose,
                args.dry_run
            )
        
        # Export results if requested
        if results and not args.dry_run:
            if args.export_json:
                export_results(results, 'json', args.export_json, args.verbose)
            
            if args.export_csv:
                export_results(results, 'csv', args.export_csv, args.verbose)
            
            # Generate plots if requested
            if args.plot:
                if args.verbose:
                    print("Generating plots...")
                
                if isinstance(results, dict) and 'multiple' in results:
                    # Plot stagger range results
                    stagger_data = results['results']
                    plot_forces(
                        stagger_distances=[r['stagger_distance'] for r in stagger_data],
                        surge_forces=[r['surge_force'] for r in stagger_data],
                        sway_forces=[r['sway_force'] for r in stagger_data],
                        yaw_moments=[r['yaw_moment'] for r in stagger_data],
                        save_path=Path(args.output_directory) / f"forces.{args.plot_format}"
                        if args.output_directory else None,
                        show=args.interactive
                    )
                elif isinstance(results, dict) and len(results) > 1:
                    # Multiple configs - create comparison
                    create_comparison_plots(
                        results,
                        save_path=Path(args.output_directory) / f"comparison.{args.plot_format}"
                        if args.output_directory else None,
                        show=args.interactive
                    )
        
        return 0
        
    except KeyboardInterrupt:
        print("\nOperation cancelled by user")
        return 1
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())