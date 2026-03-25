#!/usr/bin/env python
"""
Module entry point for Universal OrcaFlex Runner.

Allows running as: python -m digitalmodel.solvers.orcaflex.universal

Examples:
    # Basic usage
    python -m digitalmodel.solvers.orcaflex.universal --all
    
    # With options
    python -m digitalmodel.solvers.orcaflex.universal --pattern "*.yml" --mock
    
    # With keyword arguments
    python -m digitalmodel.solvers.orcaflex.universal \
        pattern="fsts_*.yml" \
        input_directory="./models" \
        output_directory="./sim"
"""

import sys
import argparse
from pathlib import Path
from typing import Dict, Any

from . import UniversalOrcaFlexRunner, StatusReporter


def parse_kwargs(args_list):
    """Parse keyword arguments from command line."""
    kwargs = {}
    for arg in args_list:
        if '=' in arg:
            key, value = arg.split('=', 1)
            # Special handling for models parameter (should be a list)
            if key == 'models':
                # If it contains commas, split it
                if ',' in value:
                    kwargs[key] = [v.strip() for v in value.split(',')]
                else:
                    # Single model, still make it a list
                    kwargs[key] = [value]
            # Convert to appropriate type
            elif value.lower() == 'true':
                kwargs[key] = True
            elif value.lower() == 'false':
                kwargs[key] = False
            elif value.isdigit():
                kwargs[key] = int(value)
            else:
                kwargs[key] = value
    return kwargs


def main():
    """Main entry point for module execution."""
    parser = argparse.ArgumentParser(
        prog='python -m digitalmodel.solvers.orcaflex.universal',
        description='Universal OrcaFlex Simulation Runner - Module Interface',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Keyword Arguments:
    You can pass any keyword argument supported by the runner:
    
    pattern="*.yml"              # File pattern to match
    input_directory="./models"   # Input directory
    output_directory="./sim"     # Output directory  
    recursive=true               # Recursive search
    parallel=true                # Parallel processing
    max_workers=20               # Concurrent workers (also accepts 'workers')
    mock=true                    # Mock mode
    analysis_type="dynamic"      # Analysis type (static/dynamic/both)
    simulation_time=200.0        # Dynamic simulation duration (seconds)
    
Examples:
    # Pattern matching with static analysis (default)
    python -m digitalmodel.solvers.orcaflex.universal pattern="fsts_*.yml"
    
    # Dynamic analysis only
    python -m digitalmodel.solvers.orcaflex.universal --dynamic pattern="*.yml" simulation_time=200
    
    # Both static and dynamic
    python -m digitalmodel.solvers.orcaflex.universal --both pattern="*.dat"
    
    # Multiple arguments
    python -m digitalmodel.solvers.orcaflex.universal \\
        pattern="*.dat" \\
        input_directory="/path/to/models" \\
        output_directory="/path/to/sim" \\
        analysis_type="dynamic" \\
        simulation_time=150.0 \\
        parallel=true \\
        max_workers=10
    
    # Configuration file
    python -m digitalmodel.solvers.orcaflex.universal config_file="batch.yml"
        """
    )
    
    # Add basic options
    parser.add_argument('--all', action='store_true',
                       help='Process all matching files')
    parser.add_argument('--test', action='store_true',
                       help='Test mode (first 3 files)')
    parser.add_argument('--mock', action='store_true',
                       help='Mock mode (no license)')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')
    parser.add_argument('--report', help='Save JSON report')
    parser.add_argument('--static', action='store_true',
                       help='Run static analysis only')
    parser.add_argument('--dynamic', action='store_true',
                       help='Run dynamic analysis only')
    parser.add_argument('--both', action='store_true',
                       help='Run both static and dynamic analysis')
    
    # Parse known args and collect remaining as kwargs
    args, unknown = parser.parse_known_args()
    
    # Parse keyword arguments
    kwargs = parse_kwargs(unknown)
    
    # Handle standard arguments
    if args.mock:
        kwargs['mock_mode'] = True
    if args.verbose:
        kwargs['verbose'] = True
    
    # Handle analysis type flags
    if args.static:
        kwargs['analysis_type'] = 'static'
    elif args.dynamic:
        kwargs['analysis_type'] = 'dynamic'
    elif args.both:
        kwargs['analysis_type'] = 'both'
    
    # Special handling for common patterns
    if args.all and 'models' not in kwargs:
        # Process all files in current directory
        if 'pattern' not in kwargs:
            kwargs['pattern'] = '*.yml'
        if 'input_directory' not in kwargs:
            kwargs['input_directory'] = '.'
    
    if args.test:
        # Limit to first 3 files for testing
        kwargs['test_mode'] = True
    
    # Initialize runner
    print("="*80)
    print("UNIVERSAL ORCAFLEX RUNNER - MODULE INTERFACE")
    print("="*80)
    
    try:
        # Extract runner-specific kwargs
        runner_kwargs = {}
        if 'mock_mode' in kwargs:
            runner_kwargs['mock_mode'] = kwargs.pop('mock_mode')
        if 'verbose' in kwargs:
            runner_kwargs['verbose'] = kwargs.pop('verbose')
        
        # Handle both 'workers' and 'max_workers' for consistency
        if 'max_workers' in kwargs:
            runner_kwargs['max_workers'] = kwargs.pop('max_workers')
        elif 'workers' in kwargs:
            # Support 'workers' for backward compatibility
            runner_kwargs['max_workers'] = kwargs.pop('workers')
            print(f"  Note: 'workers' parameter mapped to 'max_workers'")
        
        # Create runner
        runner = UniversalOrcaFlexRunner(**runner_kwargs)
        
        # Create status reporter
        status_reporter = StatusReporter(enable_colors=True)
        # Only add status_reporter if not already in kwargs
        if 'status_reporter' not in kwargs:
            kwargs['status_reporter'] = status_reporter
        
        # Run with kwargs
        print(f"\nRunning with arguments:")
        for key, value in kwargs.items():
            if key != 'status_reporter':
                print(f"  {key}: {value}")
        print()
        
        results = runner.run(**kwargs)
        
        # Display summary
        status_reporter.display_summary()
        
        # Save report if requested
        if args.report:
            report_path = Path(args.report)
            status_reporter.save_report(report_path)
            print(f"\nReport saved to: {report_path}")
        
        # Set final status
        status_reporter.set_final_status()
        
        # Return appropriate exit code
        if results.failed > 0:
            print(f"\nWARNING: {results.failed} models failed")
            return 1
        else:
            print(f"\nSUCCESS: All {results.successful} models processed successfully")
            return 0
            
    except KeyboardInterrupt:
        print("\n\nInterrupted by user")
        return 130
    except Exception as e:
        print(f"\nError: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())